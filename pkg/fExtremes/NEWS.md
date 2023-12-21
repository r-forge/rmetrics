# fExtremes 4032.84: revisions 6462-6463 on R-Forge

* Call to `fBasics::.distCheck()` have been replaced by calls to (the identical) `fBasics::distCheck()` because the former will be deprecated. Thank you to Georgi Boshnakov for this.
* `NAMESPACE` edited so that user-level functions and those needed for unit test are exported explicitly, rather than exporting everything.
* Functions intended for internal use only are documented in `fExtremes.Rd`.
* Minor cosmetic changes to documentation, e.g., correction of typos.

# fExtremes 4021.83: revisions 6272-6275 on R-Forge

* New maintainer: Paul Northrop
* Updated DESCRIPTION with links and moved timeDate, timeSeries, fBasics and fGarch from Depends: to Imports:.
* Corrected some minor typos.
* Added documentation in `TimeSeriesData` to describe the `bmwRet` and `danishClaims` datasets.

# fExtremes 3042.82 and older versions

See file ChangeLog on [the CRAN fExtremes page](https://CRAN.R-project.org/package=fExtremes) for changes before 4021.83.
