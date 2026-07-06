# araponga (development version)

* Fixed floating-point handling for degenerate projections in `pitch2d.from.3d()` (#4).
* Regenerated simulation dataset v1.1.0, now with fixed floating-point issue.
* Updated `download.simdata()` to download dataset v1.1.0.
* Added codes for generating dataset to `data-raw/simdata`
* Fixed bug that dropped column name in the output of `find.3d()`.
* Minor clarification to `label_error` argument.
* Fixed bug that plotted full circle when `facing = "left"` in `plot.angles()`.

# araponga 1.0.1

* Fixed plotting in `trim.yaws()` when retained or excluded yaw sets are empty (#2).
* CRAN resubmission.

# araponga 1.0.0

* Initial CRAN submission.
