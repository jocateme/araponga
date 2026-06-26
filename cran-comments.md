## Ressubmission

This is a resubmission. In this version I have:

* Replace `if(interactive()){}` wrappers in examples with `\donttest{}` where the examples require downloading the external simulation dataset.

* Kept small unwrapped tow examples for functions that can be demonstrated without the external dataset.

## Reviewer comments

CRAN requested that references describing the methods be added to the DESCRIPTION file if available. The method implemented in araponga is an original geometric/simulation lookup approach developed for this package, and I am not aware of a published reference that specifically describes it. I have therefore not added a reference to the DESCRIPTION field. The method is described in the package documentation and vignette.

CRAN also requested that examples wrapped in `if(interactive())` be unwrapped where feasible or replaced with small toy examples. I have replaced these wrappers with `\donttest{}` for examples involving `find.3d()`, `find.pitch()`, and `find.yaw()`, because these examples require a precomputed simulation dataset hosted externally. The dataset is approximately 207 MB, so downloading it automatically during CRAN checks is not appropriate. Other functions include small examples that can be run without this external dataset.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
