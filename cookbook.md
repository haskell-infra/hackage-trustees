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
