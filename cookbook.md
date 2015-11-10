# Cookbook for common build-failures

This document gives recipes for addressing common compile errors

----

**Problem:**

    Implicit import declaration:
        Ambiguous module name `Prelude':
          it was found in multiple packages: base haskell98-2.0.0.0

**Fix:** Add constraint `haskell98 < 2`

**Explanation:** Starting with version 2.0.0.0, `haskell98` started including the `Prelude` module.

----

**Problem:**

```
Could not find module `MyModule'
Use -v to see a list of the files searched for.
```

**Fix:** Add two lines right under `library` keyword

```
-- some comment explaining why this package ver is marked bad
build-depends: base<0
```

**Explanation:** package is non-buildable and non-fixable by any metadata
change: missing file etc. By adding non-solvable constraint, we tell Cabal to
never pick this version.

----
