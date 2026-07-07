## Resubmission

This update follows shortly after initial CRAN acceptance because it fixes a numerical edge case in the simulation-based lookup used by `find.3d()`.

Near-degenerate projected vectors could previously inherit arbitrary two-dimensional pitch values from floating-point residuals. The package now handles these cases consistently, and the external simulation dataset used by `find.3d()` has been regenerated and versioned accordingly.

`download.simdata()` now uses a versioned cache directory and records/checks dataset metadata so that outdated cached simulation datasets are not silently reused.

## R CMD check results

0 errors | 0 warnings | 0 notes
