# fUnitRoots 4021.80

- new maintainer: Georgi N. Boshnakov.

- updated DESCRIPTION, in particular removed redundant `LazyData` line.

- replaced `class(x) == "timeSeries"` with `is(x, "timeSeries")` to cancel a
  NOTE from R's checks.

- in NAMESPACE, now export selectively rather than with the pattern `"."`.

- set up the Fortran code to use ‘R_registerRoutines’, etc.
  

# fUnitRoots 3042.79 and older versions

  See file `ChangeLog` for changes before 4021.80.

