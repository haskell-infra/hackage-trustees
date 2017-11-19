# Hackage Metadata Revisions -- What They Are, How They Work

## What are revisions?

Package maintainers (as well as Hackage trustees) may provide metadata-revisions to existing package versions. These revisions can be thought of as "updating" the cabal file of a package with new or more current information. However, they do not _actually_ update the file. Tarballs retrieved from hackage are always as the authors uploaded them. Revision information is tracked explicitly in the Hackage index, and `cabal-install` will chose to prefer the latest revision of a cabal file to the original one when downloading or installing a package.

## Why do we have revisions?

Revisions are used to provide better metadata, typically in the case when a build-plan turns out to be incorrect, and we wish to tighten bounds to exclude incompatible versions of dependencies, or when a new version of a package is released and loosening dependencies will allow existing packages to work with it. Strategies for managing revisions and their costs are enumerated at: https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md#best-practice-for-managing-meta-data

Revisions can also be used when a package homepage or maintainer or the like changes, to keep information displayed current.

## Why not just keep uploading new versions?

First, uploading a whole bunch of code when only metadata changes leads to an unnecessary growth in versions. Second, often revisions need to be applied not only to the most recent version of a package, but prior versions as well. In particular, if a package at a given version has a bad install plan, then you do not want to let some tool continue to think this is a good plan, even if that package is not the latest version,.

## Where can I see revisions?

Packages with revisions will tell you so on their cabal page. You can always access the revisions of a package by appending `/revisions` to its url, like so: http://hackage.haskell.org/package/lens-3.5.1/revisions/

## How can I download a package without revisions applied?

`cabal get --pristine`

## How are revisions numbered?

Revisions are numbered by natural numbers, starting at zero, and increase in increments of 1.

## What can revisions change?

Revisions are intended to only update metadata used by build tooling, or that is used purely for display purposes. The source of a package is never changed, only specified fields of cabal files. Given the same fixed dependencies and flag configuration, as long as a metadata change hasn't disallowed them, the build product from a compiler should be the same regardless of revision changes.

Specifically, revisions can alter the following:

* Dependency bounds (tightening or loosening)
* Cabal spec version (only between the interval 1.10 and 1.21)
* Custom-setup dependencies (adding, removing or altering bounds)
* Flag defaults
* Flag type (automatic to manual only)
* Flag descriptions
* Dependencies (adding, but only in very special cases, see below).
* Copyright attribution
* Maintainer attribution
* Author attribution
* Tested-with information
* Bug report URL
* Homepage URL
* Source repositories
* Package synopsis
* Package description
* Package category

Revisions can also add custom-setup stanzas. This is necessary because newer cabal library versions make explicit custom-setup depends, while older versions simply shared dependencies with the package proper.

Other exceptions for adding dependencies all fall in the camp of needing to add things which had been implicitly assumed in the past, from a small whitelist. Package dependencies which may be added are "base" and "base-orphans". Build tool dependencies which may be added are "alex", "c2hs", "cpphs", "greencard", "happy" and "hsc2hs".


## What can't revisions change?

Revisions can't change anything else. In particular, they cannot alter code, and cannot alter anything besides the cabal file. Among some specific things revisions _cannot_ alter are:

* Dependencies (removing)
* Dependencies (adding, except as above)
* Package name
* Package version
* Package license
* Build-type
* Modules
* Flags (adding or removing)
* library, sub-library, executable, test-suite, or foreign-library stanzas (adding or removing)

## Why can't I revise licenses?

Because once code is released under a license, it remains under that license
