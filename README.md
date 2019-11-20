
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtapas <img src="man/figures/logo.svg" align="right" alt="" width="120" />

[![Build
Status](https://travis-ci.org/avalcarcel9/rtapas.svg?branch=master)](https://travis-ci.org/avalcarcel9/rtapas)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/avalcarcel9/rtapas?branch=master&svg=true)](https://ci.appveyor.com/project/avalcarcel9/rtapas)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rtapas)](https://cran.r-project.org/package=rtapas)
[![Coverage
Status](https://img.shields.io/coveralls/avalcarcel9/rtapas.svg)](https://coveralls.io/r/avalcarcel9/rtapas?branch=master)

The goal of `rtapas` is to determine a subject-specific threshold to
apply to multiple sclerosis lesion probability maps for automatic
segmentation. This R package is based on the Thresholding Approach for
Probability Map Automatic Segmentation (TAPAS) method. The methods are
in progress for publication. This package creates data structures
necessary for training the TAPAS model. After training, the model can be
used to predict subject-specific thresholds to use on probability maps
for automatic lesion segmentation. Methods can be extended outside of
multiple sclerosis lesion segmentation but have not been validated.

## Installation

To install the package from [Neuroconductor](www.neuroconductor.org),
type:

``` r
source("https://neuroconductor.org/neurocLite.R")
neuro_install("rtapas")
```

To get the latest development version from GitHub:

``` r
# install.packages('remotes')
library(remotes)
remotes::install_github('avalcarcel9/rtapas')
```

## Tutorial

This package is documented using `pkgdown`. You can find the general
`rtapas` `pkgdown` site [here](https://avalcarcel9.github.io/rtapas/)
and the tutorial/vignette
[here](https://avalcarcel9.github.io/rtapas/articles/tapas-vignette.html).
The GitHub tutorial/vignette is available
[here](https://github.com/avalcarcel9/rtapas/blob/master/vignettes/tapas-vignette.md).
