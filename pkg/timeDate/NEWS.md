# timeDate 4021.105

- the list returned by `holidaysNYSE()` was missing the special closing days of
  the New York stock exchange (NYSE). Now it should be complete (though there
  may be ommissions after 2011). This fixes issue #1356 reported by Corwin
  Joy. Thanks to him and Ian E for the insigthful discussion and useful links.

  See also below. Contributions for the other exchanges and corrections are
  welcome. 

- `holidaysNYSE()` gets a new argument, `type`, to select what type of the
  exchange's closing days to return. The default is to return all days in the
  requested years when NYSE was closed for whatever reason. Use `type = "standard"`
  and `type = `special` to get the standard holidays and the special closings,
  respectively.

  Returning any closing day by default might be considered a breaking
  change. However, not returning all closing days was perceived as erroneous by
  users (eg issue #1356). In fact, the package itself calculates business days
  by dropping weekends and days returned by `holidayXXXX`. 

  Note that `holiday()` returns the actual dates of the public holidays, while
  the corresponding days returned by `holidayXXXX` are the resulting non-weekend
  closing days, if any.

- now `holiday()` accepts also a function or a list of functions for argument
  'Holiday'.

- removed `.holidayList()` which had been replaced by `listHolidays()` a long
  time ago and was not exported in recent versions of `timeDate`.

- updated documentation files.


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
