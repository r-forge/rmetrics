# timeDate 4021.104

- new maintainer: Georgi N. Boshnakov.

- updated DESCRIPTION with links and moved all `Depends:` to `Imports:`.

- removed the line `LazyData: yes` from DESCRIPTION to fix the NOTE on CRAN.

- added the new US holiday, Juneteenth National Independence Day. Fixes #6755 by
  Ian E (ene100).

- `holidayTSX()` now includes the Labour Day. Fixes part (1) of issue #1288
  reported by Stefan Wilhelm.

- created a first version of `_pkgdown.yml` for more organised view of the large
  number of objects in the package. Unpack the tarball and run
  `pkgdown::build_site()` to build the site locally. Don't know if this could
  work directly off the R-forge repository.
  

# timeDate 3043.102 and older versions

  See file `ChangeLog` for changes before 4021.104.
