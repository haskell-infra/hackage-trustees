# Hackage Metadata Revisions — What They Are, How They Work

## What are revisions?

Package maintainers (as well as Hackage trustees) may provide metadata-revisions to existing package versions. These revisions can be thought of as "updating" the cabal file of a package with new or more current information. However, they do not _actually_ update the file. Tarballs retrieved from Hackage are always as the authors uploaded them. Revision information is tracked explicitly in the Hackage index, and `cabal-install` will chose to prefer the latest revision of a cabal file to the original one when downloading or installing a package.

## Why do we have revisions?

Revisions are used to provide better metadata, typically in the case when a build-plan turns out to be incorrect, and we wish to tighten bounds to exclude incompatible versions of dependencies, or when a new version of a package is released and loosening dependencies will allow existing packages to work with it. Strategies for managing revisions and their costs are enumerated in [the hackage trustee cookbook](https://github.com/haskell-infra/hackage-trustees/blob/master/cookbook.md#best-practice-for-managing-meta-data)

Revisions can also be used when a package homepage or maintainer or the like changes, to keep information displayed current.

## Why not just keep uploading new versions?

First, uploading a whole bunch of code when only metadata changes leads to an unnecessary growth in versions. Second, often revisions need to be applied not only to the most recent version of a package, but prior versions as well. In particular, if a package at a given version has a bad install plan, then you do not want to let some tool continue to think this is a good plan, even if that package is not the latest version.

## Where can I see revisions?

Packages with revisions will tell you so on their Hackage page. You can always access the revisions of a package by appending `/revisions` to its url, like so: http://hackage.haskell.org/package/lens-3.5.1/revisions/

## How can I download a package without revisions applied?

`cabal get --pristine` or simply get `https://hackage.haskell.org/package/foo-1.6.0.0/foo-1.6.0.0.tar.gz`

## How can I fix dependencies in the face of potential future revisions?

If you produced something at a particular point in time and want to pin it down so that no future revisions can alter anything about it, you can specify the `index-state` parameter (either in a configuration file or on the command line) as documented in [the cabal manual: index-state configuration field](http://cabal.readthedocs.io/en/latest/nix-local-build.html?highlight=index-state#cfg-field-index-state)

This pins down the view of the package universe to use revisions as of any particular timestamp and so allows you to `cabal freeze` not only versions, but the exact revisions of them.

## How are revisions numbered?

The initial upload of a package has the implicit revision zero. Every successive revision increments that by one. 

## What can revisions change?

Revisions are intended to only update metadata used by build tooling, or that is used purely for display purposes. The source code of a package is never changed, only specified fields of cabal files. Given the same fixed dependencies and flag configuration, as long as a metadata change hasn't disallowed them, the build product from a compiler in the same environment is intended to be the same regardless of revision changes. Put another way, it is intended revisions do not alter the semantic meaning of the version assigned to a package. 

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

Revisions can also add custom-setup stanzas. This is necessary because newer cabal library versions make explicit custom-setup depends, while in older versions `Setup.hs` had access to whatever happened to be registered in the global and user package databases. This was fragile and created a situation with untracked dependencies (cf: https://www.well-typed.com/blog/2015/07/cabal-setup-deps/)

Other exceptions for adding dependencies all fall in the camp of needing to add things which had been implicitly assumed in the past, from a small whitelist. Package dependencies which may be added are "base" and "base-orphans". Build tool dependencies which may be added are "alex", "c2hs", "cpphs", "greencard", "happy" and "hsc2hs". This can be seen as improving compatibility with newer cabal library versions which require that past implicit assumptions be made more explicit (cf: https://www.well-typed.com/blog/2015/03/qualified-goals/).

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

## Will these rules for what can and can't be revised ever change?

It is expected that the overriding goal that revisions do not alter the semantic meaning of the version assigned to a package should always hold. However, it is possible that specific allowances may be made for particular other things to be changed by revisions, in order to keep things building under new cabal library versions. However, as cabal has increasingly made more dependencies explicit (and particular enabled the tracking of cabal specification versioning information itself), then it is hoped that this will occur less frequently going forward, and it is hoped that it should not occur at all. Additionally, as new stanzas and features are added to cabal files over time, their interactions with revisions will also need to be specified.

## Why can't I revise licenses?

Because once code is released under a license, it remains under that license. Additionally a `LICENSE` file is included in the tarball, and revisions can only alter cabal metadata, so the file itself would remain, regardless.

## When will Hackage Trustees make revisions?

In general, it is better if authors make revisions themselves to packages. Trustees are expected to step in to help out when maintainers aren't proactive about these changes, or otherwise when they can offer particular help with regards to improving the health of the package collection as a whole, and they are expected to do so in a way which fosters a good relationship between authors/maintainers and trustees. More specific details are provided in the "metadata-only" sections of the trustee policies: https://github.com/haskell-infra/hackage-trustees/blob/master/policy.md
