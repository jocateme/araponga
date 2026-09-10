# araponga (development version)

* `find.3d()`: redesigned angle recovery to calculate projected 2D pitch directly with `pitch2d.from.3d()`, removing the dependency on the precomputed simulation lookup table. Candidate angles supplied explicitly are now evaluated at their exact values rather than rounded to integer degrees.
* `find.3d()`: added `default_step` to control the resolution of default candidate-angle grids and `max_combinations` as a safeguard against excessively large searches. Candidate combinations are evaluated in chunks to reduce peak memory use.
* `find.3d()` and `pitch2d.from.3d()`: extended supported 3D pitch values to (-180, 180]. The default candidate pitch range in `find.3d()` remains [-90, 90]; values outside this range can be supplied explicitly when an extended pitch representation is intended.
* `pitch2d.from.3d()`: vectorized calculation of projected 2D pitch for multiple 3D orientations. Input angle vectors must have equal lengths. Geometrically undefined zero-length projections now return `NA`.
* `download.simdata()`: removed. The precomputed simulation dataset is no longer required for package use.
* `pitch2d.from.xy()`: zero-length vectors produced by coincident base and tip landmarks now return `NA`.
* `pitch2d.w.error()`: undefined projected pitches generated during landmark-error propagation are excluded.
* Updated tests, documentation, README, and vignette for the redesigned angle-recovery workflow.

## Compatibility notes

Results from `find.3d()` may differ slightly from versions <= 1.1.0 when non-integer candidate angles are supplied. Previous versions rounded candidate angles to integer degrees before querying the simulation dataset; version 2.0.0 evaluates supplied values exactly.

Orientations whose projected object axis has zero length are now treated as geometrically undefined and excluded from compatible `find.3d()` results rather than being represented as a projected pitch of 0 degrees.

# araponga 1.1.0

* `download.simdata()`: regenerated simulation dataset v1.1.0, now with fixed floating-point issue.
* `download.simdata()` & `find.3d()`: updated to download dataset v1.1.0 and warn users that might have the older version installed.
* `download.simdata()`: added codes for generating dataset to `data-raw/simdata`.
* `pitch2d.from.3d()`: fixed floating-point handling for degenerate projections (#4).
* `find.3d()`: fixed bug that dropped column name in the output.
* `find.3d()` and others: minor clarification in documentation for `label_error` argument.
* `plot.angles()`: fixed bug that plotted full circle when `facing = "left"`.

# araponga 1.0.1

* Fixed plotting in `trim.yaws()` when retained or excluded yaw sets are empty (#2).
* CRAN resubmission.

# araponga 1.0.0

* Initial CRAN submission.
