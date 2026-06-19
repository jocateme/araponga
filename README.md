
<!-- README.md is generated from README.Rmd. Please edit that file -->

# araponga: estimating 3D angles from 2D landmarks in R

`araponga` estimates possible 3D orientations of objects from 2D
landmarks in images or videos.

The central problem is that a 2D projection does not uniquely determine
a 3D angle: the same apparent orientation in an image can be produced by
many combinations of true orientation, camera position, and viewing
angle. Instead of returning a single potentially misleading estimate,
araponga returns the set of 3D angles compatible with the observed 2D
projection and any user-provided constraints.

The package was originally developed to estimate beak gape angles in
singing birds, but the workflow is general: if an object can be
represented by a base landmark and a tip landmark, araponga can help
explore which 3D pitch, yaw, and view elevation angles are compatible
with its 2D projection.

## Installation

You can install the development version of `araponga` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jocateme/araponga")
```

Then load the package:

``` r
library(araponga)
```

## Simulation data

The main angle-recovery functions use a precomputed simulation dataset.
Download it once before using `find.3d()`, `find.pitch()`, or
`find.yaw()`:

``` r
download.simdata()
```

The dataset is cached locally and reused in future sessions.
Alternatively, `find.3d()`, `find.pitch()`, and `find.yaw()` can attempt
to download it automatically if called with `sim_download = TRUE`.

## Basic workflow

The general workflow is:

`2D landmarks -> projected 2D pitch -> constrained 3D search -> compatible angles`

First, use two landmarks — a base and a tip — to calculate the projected
2D pitch:

``` r
p2d <- pitch2d.from.xy(
  x_tip = 1,
  y_tip = 1,
  x_base = 0,
  y_base = 0
)
```

If the object was photographed perfectly side-on and at eye level, this
2D angle would already equal its 3D pitch. In real images, however, many
3D orientations can produce the same 2D projection, which is why
`araponga` exists.

The output from `pitch2d.from.xy()` can then be used to ask which 3D
pitches (up-down orientation) are compatible with the image, given
assumptions about view elevation, 3D orientations, and landmark error:

``` r
possible_pitches <- find.pitch(
  p2d,
  candidate_view_elevations = -35:-25,
  candidate_yaws = -30:0,
  label_error = 1
)
```

Using the same 2D projection, we can ask which yaws (left-right
orientation) are compatible with the image:

``` r
possible_yaws <- find.yaw(
  p2d,
  candidate_view_elevations = -35:-25,
  candidate_pitches = 0:90,
  label_error = 1
)
```

The important point is that the`candidate_...` arguments define which 3D
configurations are considered plausible before compatible angles are
returned.

## Main functions

`araponga` includes functions for:

- extracting projected 2D pitch from landmarks:
  - `pitch2d.from.xy()`
  - `pitch2d.w.error()`
- finding compatible 3D angles:
  - `find.3d()`
  - `find.pitch()`
  - `find.yaw()`
- visualizing angles:
  - `plot.angles()`
- working with yaw sets:
  - `trim.yaws()`
  - `summarize.yaws()`
- simulating and rotating 3D coordinates:
  - `rotate3d()`, `Rx()`, `Ry()`, `Rz()`
  - `pitch2d.from.3d()`

## Learn more

For a full worked example, see the package vignette:

``` r
vignette("araponga")
```
