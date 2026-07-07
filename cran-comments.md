## Resubmission

This is a resubmission shortly after the previous CRAN release.

This update fixes a numerical edge case affecting the simulation-based lookup table used by `find.3d()`. Near-degenerate projected vectors could previously inherit arbitrary two-dimensional pitch values from floating-point residuals. The relevant functions and external simulation dataset have therefore been updated so that these edge cases are handled consistently.

This is a resubmission.

* regenerated the external simulation dataset used by `find.3d()`;
* updated `download.simdata()` and `find.3d()` to retrieve the corrected, versioned Zenodo dataset and warn users who may have an older cached version installed;
* added the code used to generate the external simulation dataset to `data-raw/simdata`;
* fixed floating-point handling for degenerate projections in `pitch2d.from.3d()`;
* fixed a `find.3d()` bug that could drop the intended column name when returning a single selected column;
* clarified documentation for the `label_error` argument;
* fixed a plotting bug in `plot.angles()` when `facing = "left"`.

## Resubmission timing

This update is being submitted shortly after the previous CRAN release because it corrects package behavior involving the external simulation dataset used by `find.3d()`.

I understand that frequent CRAN updates should be avoided. However, I believe this early update is warranted because it fixes a numerical correctness issue in the simulation-based lookup procedure, rather than making cosmetic or documentation-only changes.

## R CMD check results

0 errors | 0 warnings | 1 note

* Days since last update: 5

This note is expected because the package was recently accepted on CRAN. The short interval reflects the correctness fix described above.

## Downstream dependencies

There are currently no downstream dependencies for this package.