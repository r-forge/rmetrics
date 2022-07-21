# fBasics 4021.92

- new maintainer: Georgi N. Boshnakov.

- updated DESCRIPTION with links.

- in NAMESPACE, now export selectively rather than with a generic pattern.
  No longer export symbols starting with a dot.
  
- tidied up the documentation somewhat. There is now a `_pkgdown` file with the
  functions in the package organised by topic. Run `pkgdown::build_site()` on
  the source directory (or unpacked tarball) to build the site locally.

- removed `.HedgeFund1` and `.HedgeFund2` - they were just used to build the
  dataset `HedgeFund`.

# fBasics 3042.89.2 and older versions

  See file `ChangeLog` for changes before 4021.92.

