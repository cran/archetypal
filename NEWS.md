# archetypal 1.3.0 (2022-07-04) - major update

## New S3 classes

* `print.archetypal()` It prints the output of archetypal()
* `summary.archetypal()` It gives a summary for the output of archetypal()
* `plot.archetypal()` It makes a plot of the archetypes creating after using archetypal()
* `plot.kappa_tools()` It makes a plot of the results created after using kappa_tools()
* `plot.study_AAconvergence()` It makes a plot of the results created after using 
study_AAconvergence()

## New functions

* `plot_archs()` plots a matrix or data frame of archetypes

# archetypal 1.2.1 (2021-11-19)

## New functions

* `kappa_tools()` computes a set of proxies for the dimensionality 

# archetypal 1.1.1 (2020-10-09)

* fix a bug for OS r-patched-solaris-x86


# archetypal 1.1.0 (2020-01-27) - major update

## New functions

* `find_pcha_optimal_parameters()` finds the optimal parameters to be used for PCHA algorithm. 
* `find_closer_points()` finds the data points that are closer to the archetypes
during all iterations of algorithm PCHA.
* `study_AAconvergence()` studies the convergence of Archetypal Analysis when
using algorithm PCHA.
* `grouped_resample()` performs simple or Dirichlet resampling.
* `dirichlet_sample()` performs Dirichlet sampling.

## New data sets

* `Absolute Temperature` the Global Absolute Temperature data set for Northern Hemisphere 1969-
2013.

* `gallupGPS6` the Gallup Global Preferences Study processed data set of six variables.

* `wd25` a 2D data set created by 5 points for demonstration purposes.

## Minor changes

* `find_outmost_projected_convexhull_points` changes its `n` argument to `npr` and has new arguments `rseed`, `doparallel`, `nworkers` and 
`uniquerows`.
* `check_Bmatrix` changes its `print.details` argument to `verbose`.
* `find_furthestsum_points` changes its `nworkers = 10` argument to `nworkers = NULL` and has new argument  `doparallel`.
* `align_archetypes_from_list` has a new argument `verbose`.
