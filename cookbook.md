# Cookbook for common build-failures

This document gives recipes for addressing common compile errors and
other meta-data issues, as well as best practices for performing
meta-data revisions.

## Compile errors/symptoms and their solutions

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

**Problem:**

```
Ambiguous occurrence ‘defaultTimeLocale’
It could refer to either ‘System.Locale.defaultTimeLocale’, imported from ‘System.Locale’
or ‘Data.Time.defaultTimeLocale’, imported from  ‘Data.Time(.Format.Locale)’
```

**Metadata fix:** Add a `time < 1.5` constraint.

**Source fix:** Import `defaultTimeLocale` from `time-locale-compat:Data.Time.Locale.Compat`

**Explanation:** `time-1.5` added `defaultTimeLocale` obsoleting `old-locale`.

---

**Problem**

```
Could not deduce (Eq a) arising from a use of ‘==’ from the context: Num a
```

```
Could not deduce (Show a) arising from a use of ‘show’ from the context: Num a
```

**Metadata fix:** Add a `base < 4.5` constraint.

**Explaination:** `Num` lost its Haskell2010-compliant `Eq`/`Show` superclasses in `base-4.5`

---

**Problem**

```
    Ambiguous occurrence `catch'
    It could refer to either `Prelude.catch',
                             imported from `Prelude' at Network/HTTP/Client/TLS.hs:4:8-30
                             (and originally defined in `System.IO.Error')
                          or `Control.Exception.catch',
```

**Metadata fix:** Add a `base >=4.6` constraint.

**Explanation:** `catch` was removed from `Prelude` in `base-4.6`

---

**Problem:**

Several different errors: Missing modules or identifiers, name clashes.
This is a quick reference for adding bounds for common errors.

**Fix:** If the identifier is missing/clashes add the constraint on the RHS.

```
base upper bounds:
Illegal bang-pattern (use -XBangP..)        => base < 4.4
Prelude.catch                               => base < 4.6

base lower bounds:
Data.Monoid.<>                              => base >= 4.5
Data.Bits.zeroBits                          => base >= 4.7
Prelude.&                                   => base >= 4.8
Control.Concurrent.forkFinally              => base >= 4.6
atomicModifyIORef'                          => base >= 4.6

bytestring:
Data.ByteString.Builder                     => bytestring >= 0.10.2
Data.ByteString.Lazy.Builder                => bytestring >= 0.10
Data.ByteString.Lazy.toStrict               => bytestring >= 0.10

containers:
Data.IntMap.Strict                          => containers >= 0.5
Data.Map.Strict                             => containers >= 0.5

getModificationTime :: _ -> IO UTCTime      => directory >= 1.2

Attoparsec.Done/Fail/Partial                => attoparsec < 0.10
Scientific vs Number                        => aeson < 0.7

Control.Monad.Trans.Resource & conduit      => conduit < 0.3

MonadThrow.monadThrow doesn't exist         => exceptions < 0.4, resourcet < 1.1

Data.Serialize.Builder                      => cereal < 0.5

crypto packages:
Crypto.Cipher.AES & depends on cryptocipher => cryptocipher < 0.5
Crypto.Cipher.RSA & depends on cryptocipher => cryptocipher < 0.5
Crypto.Cipher.AES.Key/keyOfCtx              => cipher-aes < 0.2
Crypto.Modes.IV                             => crypto-api < 0.11
Crypto.Random.AESCtr.genRandomBytes         => cprng-aes < 0.5
Couldn't match `Int' with `AESRNG'          => cprng-aes < 0.3
```

## Best practice for managing meta-data

Below, multiple common schemes used to ensure accurate dependency
meta-data are described, together their respective trade-offs and
associated costs.

The architecture of the Hackage/Cabal ecosystem make it necessary to
optimise the way meta-data is managed in order to keep the overall
system sustainable.  Specifically, the meta-data management protocols
needs to ensure that the amount of meta-data updates are minimised and
that the configuration space is kept reasonably compact.

In general, new package releases ought to be avoided when a meta-data
would suffice.

In the descriptions below, the term *active releases* is to denote
releases which are deemed the most recent ones and are considered
actively supported (this is usually only one: the most recent
version).

### The *correct* way

 - Upload releases with proper [PVP](http://pvp.haskell.org)-style version bounds
 - Relax version bounds for *active releases* via revisions as needed

*Static cost: 1 meta-data revision per compatible-dependency-upgrade-event*

It has the benefit of reducing the overall risk of compile failures or
compilable unsound configurations to a minimum. And by the more
conservative approach to opt-in to extend the solution space rather
than opt-out of incorrect parts of the solution space, don't put as
much pressure on the maintainer to step in.

Moreover, this keeps the configuration space small which is beneficial
for the cabal solver as it allows it to operate more efficiently and
discard less interesting parts of the space sooner. This typically
results in a configuration space which gets larger towards newer
versions.

If a package has frequent releases (in relation to its dependencies),
this scheme is very economical versions can often be relaxed as part
of a new release.

From the correctness point of view as well as the overall cost, this
represents the optimal scheme. This results in packages that from
experience cause the least problems on Hackage, and therefore the
least amount of work from all parties involved.

Moreover, this can be complemented with automated systems (such as
Stackage or via `packdeps` + Travis CI) which can notify package
authors when new package releases of dependencies become available
which happen to be out of bounds.

Because __this scheme is the overall optimal one__, this is also the
only scheme that's actually in full compliance with the stated Hackage
guidelines.

### The *lagging* way

 - Upload new releases without upper bounds (assuming they're compatible w/ most recent depending package versions on Hackage), and...
 - ... add upper bounds to previous releases that have become non-*active* by the new release
 - When unsound/bad install-plans are detected, add upper bounds ASAP

*Static cost: 1 meta-data revision per release __plus__ 1 meta-data revision per incompatible-dependency-upgrade-event*

In this scheme, only *active* releases can have upper bounds omitted.

The problem with this scheme is that there is a constant risk that a
new major release of a dependency allowed by the lack of an upper
bound may result in an unsound configuration. However, since it's not
possible to instantly revalidate each depending package when a new
major release of a dependency occurs, this introduces a dangerous
"lag" between Hackage's meta-data regressing into an unsound
state and recovering again.

This "lag" is made up of the time from when the breaking dependency
gets uploaded to the time when the unsoundness is detected plus the
time needed for a maintainer stepping in and recovering correctness by
adding the necessary upper bound.

This scheme results in the same meta-data during "sound" phases as the
"correct" way described in the previous section; however, this scheme
has scalability issues as the more Hackage grows, the more likelihood
of at one or more packages becoming unsound increases as well. If this
increases too much, a point would be reached at which there is always
multiple unsound packages.

Another problem with this scheme is that by the time the problem has
been detected, install-plans using unsound configurations (which
happen to compile) may have started to become used by users (and which
may not even exhibit the incorrectness as they may not use the broken
codepaths). If such install-plans are retroactively prohibited, this
results in a bad user-experience, and results in violation of the
invariant that a meta-data revision shall not result in existing
install-plans becoming illegal.

So this scheme is very sensitive to being able to detect & resolve
meta-data regressions in a *very* timely matter, as otherwise the
overall system collapses due to a build-up of not-yet-fixed unsound
meta-data.

### The *sloppy* way

 - Upload new releases without (upper) bounds (-"-)
 - When unsound/bad install-plans are detected,
    - add (all) upper bounds to all affected releases

*Static cost: 1 meta-data revision per release.*

This is similiar to the *lagging* way, typically increases the
risk-exposure to more releases than just the next-to-last one, and
thereby also misses the goal to keep the configuration space
reasonably (and therefore results in a different meta-data situation
than the other workflows!) compact to aid constraint solving.

Due to the increased configuration space, the combinatorics involved
make it even more prohibitive than the *laggy* scheme (if not
impossible) to cover validating new install-plans becoming possible
due to the appearance of new major versions.

The revision-cost is actually lower than for the *lagging* scheme; for
packages with a small number of total releases combined with a high
amount of direct frequently releasing dependencies, this scheme may
also result in a smaller static cost. However, the aforesaid mentioned
disadvantages make this scheme still less preferable to the *correct*
and *lagging* schemes.

### The *worst* way

 - Upload new releases without (upper) bounds (-"-)
 - When unsound/bad install-plans are detected,
    - add the minimal amount of necessary upper bounds to all affected releases

*Static cost: 1 meta-data revision per incompatible-dependency-upgrade-event __times__ number of affected releases*

This has by far the worst overall cost-model of all schemes listed
here and asymptotically leads
to the very harmful situation where the number of meta-data revisions
will eventually outnumber the amount of actual package releases by
orders of magnitudes, while also amplifying the downsides already
described for the *laggy* and *sloppy* schemes, and last but not least
also being the most laborious workflow for both maintainers and
trustees.

In short, __this scheme is maximally detrimental to the Hackage/Cabal ecosystem__
and therefore ought to be avoided at any price as it isn't sustainable
due to the prohibitive computational complexities involved.
