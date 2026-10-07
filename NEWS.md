# araponga (development version)

## Major changes

* `find.3d()`, `find.pitch()`, and `find.yaw()` now calculate projected geometry directly with `project2d.from.3d()` rather than querying a precomputed simulation dataset. The external simulation dataset and `download.simdata()` are therefore no longer required and `download.simdata()` has been removed.
* `find.3d()` now supports additional information derived from 2D landmarks. An `araponga2d` observation can be combined with landmark-labeling uncertainty through `label_error` and with the object's unprojected length through `full_length`. For workflows in which projected measurements have already been calculated, `pitch2d` and/or `length2d` constraints can alternatively be supplied directly.
* Added `full_length.from.camera()` and `full_length.from.reference()` to estimate the unprojected image length required by `full_length` from camera geometry or a reference segment, respectively. Input ranges can be used to propagate uncertainty in physical measurements, distance, camera parameters, and reference measurements.
* `pitch2d.from.3d()` has been renamed to `project2d.from.3d()` and substantially extended. The function is now vectorized and returns projected 2D pitch, projection factor, and projected x-y component factors. If `full_length` is supplied, absolute projected length and x-y components are also returned. Supported 3D pitch values now span (-180, 180]. Geometrically undefined projected pitches return `NA`.
* `pitch2d.from.xy()` has been renamed to `project2d.from.xy()`. It now returns an `araponga2d` data frame containing the landmark coordinates, projected 2D pitch, and projected length rather than a numeric pitch value with landmark coordinates stored as an attribute. Coincident landmarks produce a projected length of zero and an undefined (`NA`) projected pitch.
* `pitch2d.w.error()` has been redesigned to calculate the exact continuous interval of projected 2D pitches compatible with landmark-labeling uncertainty. It now accepts an `araponga2d` object and returns an angular-interval summary rather than a sampled vector of possible pitches. The previous `label_nsamp` and `add_boundaries` arguments have been removed.

## Search and performance

* Candidate pitch, yaw, and view-elevation values supplied explicitly to `find.3d()` and its wrappers are now evaluated at their exact values rather than rounded to integer degrees.
* Added `default_step` to control the resolution of automatically generated candidate-angle grids.
* Added `max_combinations` as a safeguard against excessively large searches. Candidate combinations are evaluated in chunks to reduce peak memory use.
* Explicit candidate 3D pitches can now span (-180, 180]. The default candidate pitch range remains [-90, 90]; values outside this range can be supplied when an extended pitch representation is intended.

## Compatibility notes

Version 2.0.0 introduces breaking API changes relative to araponga 1.x.

* `pitch2d.from.xy()` and `pitch2d.from.3d()` have been replaced by `project2d.from.xy()` and `project2d.from.3d()`, respectively. Their return values have also changed, so code relying on the previous numeric outputs or `"xy"` attribute must be updated.
* The first argument to `find.3d()`, `find.pitch()`, and `find.yaw()` is now `observed2d`. Code that previously supplied a numeric projected pitch positionally should instead use the direct constraint explicitly, for example `find.pitch(pitch2d = x, ...)`.
* The `find` argument to `find.3d()` now refers only to 3D quantities (`"pitch"`, `"yaw"`, and `"view_elevation"`); `"pitch2d"` is no longer a return option.
* `pitch2d.w.error()` now operates directly on an `araponga2d` observation and returns a continuous angular interval. The previous sampled-output interface and the `label_nsamp` and `add_boundaries` arguments have been removed.
* Because candidate angles are now evaluated directly rather than looked up in an integer-resolution simulation dataset, results may differ slightly from versions <= 1.1.0 when non-integer candidate angles are supplied.
* A projection with zero projected length has undefined projected pitch and is represented by `pitch2d = NA`. Such orientations cannot satisfy a projected-pitch constraint, but may still be compatible with a search based only on projected length.

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
