
## timeSeries 4021.105

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


## timeSeries 4021.104

- new maintainer: Georgi Boshnakov.

- moved package `methods` to `Imports`.

- fixed CRAN NOTE `Escaped LaTeX specials: \_ \_` in `methods-plot.Rd`.


## timeSeries 3062.100 and older

  See file `ChangeLog`.
