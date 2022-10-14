
## timeSeries 4021.105

- class `timeSeries` now has a dedicated summary method. Previously it was
  flling back to the method for matrices.

- corrected USDCHF dataset. The year information was wrong (the data started
  from year 8295). The bug had been introduced in version 2100.84 when the
  dataset file was converted from a `usdchf.csv` to
  `USDCHF.rda`. `USDCHF@documentation` contains a short note about this change.
  Also changed the FinCenter to Zurich (neither the documentation nor the csv
  file does't contain FinCenter information).

- the original source file `msft.dat.csv` of the `MSFT` data is included now as
  `inst/extdata/msft.csv` (note the different name). The file had been removed
  in v2100.84. Note that there is a file ``msft.dat.csv` in `test/` but it is a
  modified and abbreviated version of the original file.
  
- the additional arguments of the S3 `timeSeries` method for `diff()` are now in
  its signature, which previously was `diff(x, ...)`.  An intermediate function,
  `.diff.timeSeries`, was eliminated in the process.

- eliminated `.merge.timeSeries` and other redundancy in the implementation of
  the `c("timeSeries", "timeSeries")` method.

- eliminated `.rev.timeSeries` in the definition of the `rev` method.

- eliminated `.scale.timeSeries` in the definition of the `scale` timeSeries
  method.

- same as above for `.sort.timeSeries`.

- eliminated `.start.timeSeries`and redundancy in the implementation of the
  `timeSeries` method.

- eliminated `.end.timeSeries`and redundancy in the implementation of the
  `timeSeries` method.

- stopped exporting (almost) all functions whose names start with a
  '.'. Historically, the package was exporting all functions, including those
  start with a '.'.

- The functions `returnSeries` and `getReturns` are no longer exported and will
  be removed in the near future. They are synonyms for the function
  \code{returns} and their use was discouraged for many years. Just use
  `returns`.

- The functions `spreadSeries` and `midquoteSeries` are no longer exported and
  will be removed in the near future. Just use their synonyms `spreads` and
  `midquotes`, respectively.

- `colCumsums`, `colCummaxs`, `colCummins`, and `colCumprods` no longer throw
  error for `timeSeries` objects when called with `na.rm = TRUE`. Fixes bug
  #2121 reported by Shane Haas.

- updated and significantly improved the documentation.

- function `cut` is now formally deprecated. Use `window` instead.

- deprecated function `seriesData` is now defunct. Use `as.matrix()` instead.

- deprecated function `seriesPositions` is now defunct. Use `time()` instead.

- deprecated function `newPositions<-` is now defunct. Use `time<-` instead.

- deprecated function `colAvgs` is now defunct. Use `colMeans()` instead.

- deprecated function `colStdevs` is now defunct. Use `colSds()` instead.

- the function `.applySeries` is now defunct. It was obsoleted long time ago and
  was exported for historical reasons only. Use `applySeries()` instead.


### Technical changes


- the bodies of the methods of `series<-()` and `coredata<-` for signature
  `"matrix"` of value were identical. Now the body is a separate, unexported
  function and is used as the definition of both of these methods.


## timeSeries 4021.104

- new maintainer: Georgi Boshnakov.

- moved package `methods` to `Imports`.

- fixed CRAN NOTE `Escaped LaTeX specials: \_ \_` in `methods-plot.Rd`.


## timeSeries 3062.100 and older

  See file `ChangeLog`.
