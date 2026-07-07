# araponga 1.1.0

* `download.simdata()`: regenerated simulation dataset v1.1.0, now with fixed floating-point issue.
* `download.simdata()`: updated to download dataset v1.1.0 and warn users that might have the older version installed.
* `download.simdata()`: added codes for generating dataset to `data-raw/simdata`
* `pitch2d.from.3d()`: fixed floating-point handling for degenerate projections (#4).
* `find.3d()`: fixed bug that dropped column name in the output.
* `find.3d()` and others: minor clarification in documentation for `label_error` argument.
* `plot.angles()`: fixed bug that plotted full circle when `facing = "left"`.

# araponga 1.0.1

* Fixed plotting in `trim.yaws()` when retained or excluded yaw sets are empty (#2).
* CRAN resubmission.

# araponga 1.0.0

* Initial CRAN submission.
