# fUnitRoots 4052.82

- added to Suggests the missing 'interp'.

- fixed links and clarified documentation.


# fUnitRoots 4040.81

- fixed warnings about ancient Fortran features. Now all `DO` loops are terminated
  with `END DO` and do not share termination statements.


# fUnitRoots 4021.80

- new maintainer: Georgi N. Boshnakov.

- removed `timeDate` from (direct) dependencies. Moved `timeSeries` and
  `fBasics` from `Depends:` to `Imports:`. Users who relied on
  `library(fUnitRoots)` to put all these packages on the search path will need
  to load them explicitly, if needed.

- updated DESCRIPTION, in particular removed redundant `LazyData` line (this
  removed a NOTE to that effect from CRAN).

- set up the Fortran code to use ‘R_registerRoutines’, etc. (this
  removed a NOTE to that effect from CRAN)

- replaced `class(x) == "timeSeries"` with `is(x, "timeSeries")` to cancel a
  NOTE from R's checks.

- in NAMESPACE, now export selectively rather than with the pattern `"."`.

- import `plot` from `timeSeries`. Without this, S4 methods for `plot` are not
  necessarilly visible from `fUnitRoots`.  E.g., the examples in
  "UnitrootUrcaInterface.Rd" failed (after stopping importing `timeDate`)
  because `plot(urca)` was calling the default plot method. (TODO: this probably
  could be avoided by defining also some S3 methods, as recommended in
  `methods`.)


# fUnitRoots 3042.79 and older versions

  See file `ChangeLog` for changes before 4021.80.

