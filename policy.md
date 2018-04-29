# Hackage trustee policy and procedures

The Hackage trustees are a group of volunteers who are interested in
the health of the package collection as a whole. This is different to
the role of maintainers of individual packages.

The trustees role is in many ways similar to that of the individuals
who maintain the Haskell packages for the various Linux distributions.
In Linux distributions the distribution maintainers seek to have their
set of packages build and work together. The kinds of problems they
deal with tend to be similar across many packages, and so they can
become adept at spotting and solving them, where a maintainer of any
individual package is likely to only encounter such problems once. In
particular distribution maintainers are often involved in making minor
fixes to packages and pushing those fixes back upstream.

The Hackage trustees role is much like that of distribution maintainers
but closer to upstream, and more centralised rather than duplicated
across numerous Linux distributions.


## Mission statement

The goal of the trustees is to make it easy for anyone to use packages
that are hosted on Hackage, and maintain the overall health of Hackage
infrastructure. Ideally users will always be able to find a suitable
combination of packages and versions that work well together.

Trustees strive to involve maintainers in this process as much as
possible while being friendly, helpful, and respectful. In general,
trustees aim to empower and educate maintainers about the tools at
their disposal to better manage their own packages. We realize that
not everyone shares our priorities and we do not want to take up
anyone's time unnecessarily. For these cases we would like to find an
arrangement that suits the maintainers wishes, while also not unduly
burdening resources of trustees. Being a part of the hackage curation
process is entirely optional.

Trustee policies are intended to maximise the useful improvements
trustees can make (or encourage others to make) to the collection
while making sure the relationship with authors/maintainers is
healthy. We do not want to discourage maintainers from using Hackage.
We want processes (and if necessary automation) for each case that
balances the ability of trustees to make useful improvements, with the
legitimate concerns of authors/maintainers to maintain control and
quality.

Common trustee operations involve specifying or correcting metadata
using revisions, but there are also uncommon operations involving
uploads of new versions. The rest of this document contains details on
which changes can be made in which circumstances, and with what
necessary prior and subsequent communication.


## 1. Metadata-only changes: tightening constraints

The trustees' view is that tightening constraints is very useful,
and is pretty safe and obvious. It is safe in the sense that it
does not make possible any configurations that were not previously
possible. It's pretty obvious when it needs to be done because there
is concrete evidence that certain combinations of versions do not in
fact compile.

(The only plausible danger is in accidentally over-constraining and
excluding build configurations that did actually work. This is easily
correctable.)

Making these changes is also likely to be the bulk of the trustees'
work and so the trustees want a process with low overheads.

### Policy/Procedure

* trustees can edit .cabal file to constrain dependencies
* maintainers should be notified via e-mail, by filing a bug tracker issue, or
  by sending pull request (in the future this step may be automated by hackage)

## 2. Metadata-only changes: relaxing constraints

The trustees' view is that relaxing constraints should be done
carefully, and ideally done by or checked by the package maintainers.
Nevertheless there are cases where it is helpful.

### Policy/Procedure

* trustees can edit .cabal file to relax dependencies

* maintainers should be notified via e-mail, by filing a bug tracker
  issue, or by sending a pull request (in the future this step may be
  automated by hackage)

* the default is to be opted in to allowing trustees to make these edits

* (in the future) maintainers should be able to explicitly indicate
  whether trustees are able to relax constraints for their package

* (in the future) if not, trustee's proposed edits are sent to the
  maintainers but not applied

Trustees are expected to use this power judiciously and make sure they
understand the packages involved and their APIs. In the first instance
they should get maintainers to make changes. If maintainers are not
available then they should use their judgement and consult with each
other when in doubt.


## 3. Source changes: "simple" patches

There are cases where maintainers are unavailable indefinitely or for
long periods. This is not usually an urgent problem, but sometimes when
a package has lots of others that depend on it, those other packages
can be blocked from working with newer dependencies. For example a
package that is not updated for a long period may block all packages
that depend on it from working with a newer compiler or base library
release.

In such cases trustees should in the first instance work with the
maintainers to make them aware and help them to make appropriate
updates.

Of course when maintainers are not available indefinitely, there is a
procedure to allow a new volunteer to take over the package. Stopping
short of that, when only very minor updates are needed it may be
desirable to allow the trustees to make the update without securing a
new volunteer maintainer. Similarly, if the maintainer(s) are not
available for a prolonged period but do eventually return, it may be
desirable in the meantime for the trustees to make simple updates when
it affects important packages (directly or indirectly).

Making source changes without the involvement of the maintainer (a
so-called NMU -- non-maintainer upload) is certainly not ideal and so
a careful procedure is called for. It should balance the benefits to
the health of the package collection with the control of package
author.

### Policy/Procedure

 1. (Anyone) Send a pull request or open an issue to the offending
    packages source repository. If no source repository is
    available, send an e-mail.
 2. (Anyone) After a while if there is no response, [open a ticket in
    the trustees issue tracker](https://github.com/haskell-infra/hackage-trustees/issues/new),
    linking to the pull request / issue,
    or describing/embedding the e-mail. Alternatively if problems
    are anticipated from the outset then file the issue immediately.
 3. (Trustee) When the issue is opened, a trustee must try to
    contact the maintainer(s), primarily by e-mail to try and
    resolve things without needing to force anything. They must
    record in the ticket when they first tried to contact the
    maintainer(s). This documents the start of the 2-week deadline.
    (Many users do not have github notifications set up.)
 4. (Trustee) After a week if there's still no contact with any of
    the maintainers then a trustee should again email the
    maintainers to inform them of the plan to do a NMU, what, why
    and when. The when is at earliest in a week. The trustee must
    record in the ticket when again tried to contact the
    maintainer(s). This documents the start of the 1-week deadline.
 5. (Anyone) Discuss exactly what code needs to be changed.
 6. (Trustee) Fork the upstream repo under the hackage-trustee
    organisation on github and prepare the patches (perhaps by
    pulling in changes from somewhere else), including a tag.
 7. (Trustee) Get at least a second trustee to look at and agree to
    the changes for the proposed NMU.
 8. (Admin) After the deadline (no sooner than 2 weeks from the
    original contact attempt and no sooner than 1 week from the
    contact attempt where the intention to do a NMU is explained),
    and having got signoff from two trustees, an admin may grant the
    trustees authority to make uploads for the package in question.
 9. (Trustee) Tag the version to be uploaded. Do the NMU. Again
    inform the maintainers about what has happened and where they
    can see the changes that have been made (ie the forked repo and
    the ticket tracking the issue).

## 4. Full transfer of ownership

The policy for this is already established, but the trustees can help
facilitate it, e.g. by acting as a point of contact for the procedure.

This is detailed on the [haskell wiki](https://wiki.haskell.org/Taking_over_a_package).

## Anticipated Hackage enhancements for automation

Edits to .cabal metadata should be emailed to the members of the
maintainer group. Ideally this should include:

* an English language description of the change, much like the
  revisions page on the website
* a patch attachment or link to a patch in a suitable format (e.g.
  git patch format)
* instructions for applying the patch (e.g. git commands)
