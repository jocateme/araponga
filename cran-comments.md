## Release summary

This is an update of `araponga` from version 1.1.0 to 2.0.0.

Version 2.0.0 introduces a substantial redesign of the package's orientation-estimation workflow, including breaking changes to the public API.

The main changes are:

- Replaced the precomputed simulation lookup table with direct calculations of projected 2D geometry. This removes the need for external simulation data downloads and the associated dependencies.
- Extended orientation searches to incorporate projected length as well as projected direction, including their joint uncertainty when calculated from labeled landmarks.
- Added `full_length.from.camera()` and `full_length.from.reference()` to estimate unprojected image length from camera geometry or reference measurements.
- Renamed and extended the projection functions, updated their return structures, and introduced the `araponga2d` observation format.
- Added controls for candidate-angle resolution and search size.

These changes improve flexibility, numerical precision, and independence from external data. The breaking changes and migration guidance are documented in `NEWS.md`.

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

There are no reverse dependencies on CRAN.