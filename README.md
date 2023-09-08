# cabal-solver-sat

Recently I wrote [a blog post](https://oleg.fi/gists/posts/2023-08-30-using-cabal-install-solver-as-sat-solver.html) about using `cabal-install` solver as SAT solver.
Natural question, which I had in the back of my mind for quite long time, is whether we can use off-the-shelf SAT solver as `cabal-install` dependency solver?
I always assumed that we can, but I never bothered to try out myself.
So here we go.

Encoding
--------

We need to encode the dependency resolution problem as SAT problem.
To encode any problem you need to understand the problem first.
Dependency resolution problem is not that complicated, at least if we simplify it a bit:

1. Input: Collection of packages: which have name, version and dependencies.
2. Output: A set of package-versions.
3. Constraint: The resulting set must be coherent: each package in the set has its dependencies also in the set.
4. Constraint: Requested (targets) must also be in the set.

An example of package is

```cabal
name: my-package
version: 1.2

library
  build-depends: base >=4.16 && <4.19, containers ^>=0.6.7
```

Obviously `.cabal` files have more information, but to the first approaximation the above is all we need to resolve dependencies.

One wisdom of SAT encoding is apparently "try the most obvious encoding first", and that is what I did.

The idea is to encode each package-version as own literal. Then dependencies can be encoded as implication clauses (recall: `x → y` is `¬x ∨ y`).
Let's assume there are `base-4.16.0.0`, `base-4.17.0.0`, etc packages, then the package description above will be encoded as

```text
¬ my-package-1.2 ∨ base-4.16.0.0 ∨ base-4.16.1.0 ∨ base-4.16.2.0 ∨ ... ∨ base-4.18.0.0
¬ my-package-1.2 ∨ containers-0.6.7
```

If our goal is to build `my-package`, then `my-package-1.2` will be a single literal clause.

An important bit, somewhat specific to Haskell, is that we must require that *at most one package version per package* is selected.
So we need to add a constraint like `assertAtMostOne (base-3.0.3.1, base-3.0.3.2, ..., 4.18.0.0)` which can be encoded in linear amount of clauses.

If we could select multiple package versions, the dependency resolution problem would become [trivial](https://en.wikipedia.org/wiki/Horn-satisfiability). Intuitively: we could simply select all packages (or greedily select a version when required by `build-depends`).
The at most one restriction is the constraint which makes the problem hard.

Another Cabal specific aspect are *automatic* flags, e.g. 

```cabal
name:    tar
version: 0.5.1.1

flag old-bytestring
  default: False
  manual: False

library
  ...

  if flag(old-bytestring)
    build-depends: bytestring-builder >= 0.10.4.0.2 && < 0.11, bytestring == 0.9.*
  else
    build-depends: bytestring >= 0.10 && < 0.12
```

We will encode each flag selection as own SAT variable. I used fresh variables for each package-version (i.e. `tar-0.5.1.1-flag-old-bytestring`),
instead of sharing variables across package (i.e. just `tar--flag-old-bytestring`), which is also possible as at most one version is selected.
Latter would result in less variables, and probably work well, as flag selection is usually consistent across different package versions:
if `old-bytestring` was off in `tar-0.5.1.0` it probably have to be off in `tar-0.5.1.1` too.
But the former option was the one simpler to implement, so I stick with it.

Then the conditional dependencies can be encoded as

```text
(tar-0.5.1.1 ∧ tar-0.5.1.1-flag-old-bytestring) → (bytestring-builder-0.10.4.0.2 ∨ ... ∨ bytestring-builder-0.10.8.2.0)
(tar-0.5.1.1 ∧ tar-0.5.1.1-flag-old-bytestring) → (bytestring-0.9 ∨ ... ∨ bytestring-0.9.2.1)
(tar-0.5.1.1 ∧ ¬ tar-0.5.1.1-flag-old-bytestring) → (bytestring-0.10.0.0 ∨ ... ∨ bytestring-0.11.5.1)
```

i.e.

```text
¬ tar-0.5.1.1 ∨ ¬ tar-0.5.1.1-flag-old-bytestring ∨ bytestring-builder-0.10.4.0.2 ∨ ... ∨ bytestring-builder-0.10.8.2.0
¬ tar-0.5.1.1 ∨ ¬ tar-0.5.1.1-flag-old-bytestring ∨ bytestring-0.9 ∨ ... ∨ bytestring-0.9.2.1
¬ tar-0.5.1.1 ∨ tar-0.5.1.1-flag-old-bytestring ∨ bytestring-0.10.0.0 ∨ ... ∨ bytestring-0.11.5.1
```

More complicated conditions then just single flag, e.g. something like

```cabal
if flag(old-bytestring) || flag (old-time)
  build-depends: ...
```

can be encoded using an auxiliary variable:

```text
condition-aux ↔ (tar-0.5.1.1-flag-old-bytestring ∨ tar-0.5.1.1-flag-old-time)
(tar-0.5.1.1 ∧ condition-aux) → ...
```

Turns out SAT encoding of the dependency resolution problem is not very complicated.

Problem size problem
--------------------

At the time of writing, there are around 17500 packages and 135000 package-versions (i.e. about 7-8 versions per package in average) on Hackage.
That's a lot.
Modern SAT solvers probably can handle this just fine, but we can do better.

There are *incremental* SAT solvers.
Which means that we create a solver instance, add variables and clauses, ask for solution, and then we can  continue adding variables and clauses and solve again.
This is in fact quite efficient, as SAT solvers "learn" (quite literally) about the problem, and reuse the knowledge in the consequtive searches. 

We can use incrementality to only encode parts of search space we need.
1. We start by encoding that we need target package. Ask for solution.
2. Look through solution, find selected package-versions which don't have their dependencies expanded yet, expand them.
3. If there weren't any package-versions we need to expand, we are done. Otherwise ask for a new solution and go to step two.

Unfortunately [ersatz](https://hackage.haskell.org/package/ersatz) (I'd say go to package for dealing with SAT problems) doesn't support incremental mode of operation so I made a small package of my own: [sat-simple](https://hackage.haskell.org/package/sat-simple).

Real solver
-----------

`cabal-install` supports having multiple dependency solvers, at least in theory. The dependency solver is "just" a function:

```haskell
type DependencyResolver loc =
    Platform ->
    CompilerInfo ->
    InstalledPackageIndex ->
    PackageIndex (SourcePackage loc) ->
    PkgConfigDb ->
    (PackageName -> PackagePreferences) ->
    [LabeledPackageConstraint] ->
    Set PackageName ->
    Progress String String [ResolverPackage loc]
```

The `Platform` and `CompilerInfo` data is needed to evaluate `if arch(...)`, `if os(...)` and `if impl(..)` conditionals.
Also `CompilerInfo` contains the set of known extensions, so package-versions which specify `default-extensions` or `other-extensions` can be
ruled out as well.

The `InstalledPackageIndex` is the installed packages. Currently these contain the package (versions) bundled with GHC: `base`, `ghc-prim`, `template-haskell` but also a version of `containers`, `transformers` etc.

The `PackageIndex (SourcePackage loc)` are the source packages, i.e. usually Hackage, but also (local) packages in the project.
The `loc` argument is an extra bits for each package (usually a location), which solver doesn't care about, but threads through into result.

The `PkgConfigDb` are the native libraries which build info is provided by `pkg-config`, i.e. solver takes into account `pkgconfig-depends` definitions.
In other words, it can backtrack if `pkgconfig-depends` are not satisfied. Yet, the `extra-libraries` are always assumed to be satifiable.
(I think it's mainly because the solver interface is *pure*, i.e. no IO. The `pkg-config` database can be preread, but you cannot prepare all libraries that could exist in `extra-libraries`. But if solver interface allowed IO, then solver could also try `extra-libraries`, which would allow to fail early, and not in the building phase if libraries are missing).

The `(PackageName -> PackagePreferences)` tells which versions for packages to prefer. These are soft-constraints: we can tell solver to avoid using some version.

The `[LabeledPackageConstraint]` are on the other hand the hard constraints. For example you can have

```cabal
constraint: transformers installed
constraint: lens ^>=5.2.3
```

as additional constraints in your `cabal.project`. These are not problematic for SAT encoding.
We could either encode them directly, or apply them even before encoding to SAT.
(`cabal-install`s solver uses them similarly to other constraints, which makes the error reporting more uniform. E.g. you see that it rulled all older `lens` versions due the constraint, they don't simply disapppear).

The `Set PackageName` ...
