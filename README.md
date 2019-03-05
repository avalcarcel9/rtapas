
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rtapas

[![Build
Status](https://travis-ci.org/avalcarcel9/rtapas.svg?branch=master)](https://travis-ci.org/avalcarcel9/rtapas)[![AppVeyor
Build
Status](https://ci.appveyor.com/api/projects/status/github/avalcarcel9/rtapas?branch=master&svg=true)](https://ci.appveyor.com/project/avalcarcel9/rtapas)[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rtapas)](https://cran.r-project.org/package=rtapas)

The goal of `rtapas` is to determine a subject-specific threshold to
apply to multiple sclerosis lesion probability maps for automatic
segmentation. This R package is based on the Thresholding Approach for
Probability Map Automatic Segmentation (TAPAS) method. The methods are
in progress for publication. This package creates data structures
necessary for training the TAPAS model. After training, the model can be
used to predict subject-specific threhsolds to use on probability maps
for automatic lesion segmentation.

## Installation

This packages is still under construction and development. Please check
back when the builds are stable.

To get the latest development version from GitHub:

``` r
# install.packages('remotes')
library(remotes)
remotes::install_github('avalcarcel9/rtapas')
```

We are currently working to get the package on
[Neuroconductor](www.neuroconductor.org).

## Function Documentation

The TAPAS package contains a number of functions to generate data
required for training, train the model, and predict subject-specific
thresholds based on the trained model. The functions, defaults, and
usage are provided below.

### `tapas_data`

``` r
tapas_data(thresholds = seq(from = 0, to = 1, by = 0.01),
           pmap,
           gold_standard,
           mask,
           k = 8,
           subject_id = NULL,
           verbose = TRUE)
```

**Description**:

This function creates the training vectors for a single subject from a
probability map, a gold standard mask (normally a manual segmentation),
and a brain mask. For a grid of thresholds provided and applied to the
probability map the function calculates Sørensen’s–Dice coefficient
(DSC) between the automatic volume and the gold standard volume. The
function also calculates the automatic volume associated with
thresholding at each respective threshold.

**Usage**:

  - `thresholds` A `vector` of thresholds to apply to the probability
    map. The default `vector` applied is 0 to 1 by 0.01 increments which
    matches the published work. Threshold values must be between 0 and
    1.  
  - `pmap` A `character` file path to probability map images or an
    object of class `nifti`.
  - `gold_standard` A `character` file path to a gold standard image
    (normally a manual segmentation) or an object of class `nifti`. The
    gold standard segmentation is used to compare the thresholded
    probability map image using Sørensen’s–Dice coefficient (DSC).  
  - `mask` A `character` file path to a brain mask image or an object of
    class `nifti`.  
  - `k` The minimum number of voxels for a cluster/component. Passed to
    `label_mask`. Segmentation clusters of size less than k are removed
    from the mask, volume estimation, the and Sørensen’s–Dice
    coefficient (DSC) calculation.  
  - `subject_id` A subject ID of class `character`. By default this is
    set to `NULL` but users must provide an ID.  
  - `verbose` A `logical` argument to print messages. Set to `TRUE` by
    default.

**Return**:

A `tibble` containing the training data. The data contains columns
`threshold`, Sørensen’s–Dice coefficient (`dsc`), and `volume`.

### `tapas_data_par`

``` r
tapas_data_par(cores = 1,
               thresholds = seq(from = 0, to = 1, by = 0.01),
               pmap,
               gold_standard,
               mask,
               k = 8,
               subject_id = NULL,
               ret = FALSE,
               outfile = NULL,
               verbose = TRUE)
```

**Description**:

This function wraps `tapas_data` to run in parallel. This function
creates the training vectors for a single subject from a probability
map, a gold standard mask (normally a manual segmentation), and a brain
mask. For a grid of thresholds provided and applied to the probability
map the function calculates Sørensen’s–Dice coefficient (DSC) between
the automatic image and the gold standard image. The function also
calculates the volume associated with thresholding at each respective
threshold.

**Usage**:

  - `cores` The number of cores to use. This argument controls at most
    how many child processes will be run simultaneously. The default is
    set to 1.  
  - `thresholds` A `vector` of thresholds to apply to the probability
    maps. The default `vector` applied is 0 to 1 by 0.01 increments
    which matches the published work. Threshold values must be between 0
    and 1.  
  - `pmap` A `vector` of `character` file paths to probability map
    images or a `list` object with elements of class `nifti`.  
  - `gold_standard` A `vector` of `character` file paths to gold
    standard images (normally a manual segmentation) or a `list` object
    with elements of class `nifti`. The gold standard segmentation is
    used to compare the thresholded probability map image using
    Sørensen’s–Dice coefficient (DSC).
  - `mask` A `vector` of `character` file paths to brain mask images or
    a `list` object with elements of class `nifti`.  
  - `k` The minimum number of voxels for a cluster/component. Passed to
    `label_mask`. Segmentation clusters of size less than k are removed
    from the mask, volume estimation, the and Sørensen’s–Dice
    coefficient (DSC) calculation.  
  - `subject_id` A subject ID of class `character`. By default this is
    set to `NULL` but users must provide an ID.  
  - `ret` A `logical` argument set to `TRUE` by default. Return the
    `tibble` objects from the function as a `list` in the local R
    environment. If `FALSE` then `outfile` must be specified so subject
    data is saved.  
  - `outfile` Is set to `NULL` by default which only Return the
    subject-level `tibble` as a list in the local R environment. To save
    each subject-level `tibble` as an R object specify a `list` or
    `vector` of file paths to save with either .rds or .RData extensions
    included.  
  - `verbose` A `logical` argument to print messages. Set to `TRUE` by
    default.

**Return**:

A list of the `tibble` object returned from `tapas_data` for each
subject.

### `tapas_train`

``` r
tapas_train(data, 
            dsc_cutoff = 0.03, 
            verbose = TRUE)
```

**Description**:

This function trains the TAPAS model from a `tibble` or `data.frame`
produced from the `tapas_data` function. The TAPAS model is fit and
clamp data is calculated. The clamp data contains the predicted
threshold when using the 10th and 90th percentile volume from training
data.

**Usage**:

  - `data` Data resulting from `tapas_data`. The `data` should be a
    `tibble` or `data.frame` containing binded subject data or a `list`
    object with all subject data. Data from these subjects will be used
    for model training.  
  - `dsc_cutoff` The Sørensen’s–Dice coefficient (DSC) value to use as a
    cutoff for training inclusion. By default 0.03 is used. This must be
    a single value between 0 and 1. Only training subjects with a
    subject-specific threshold estimate resulting in Sørensen’s–Dice
    coefficient (DSC) greater than or equal to the `dsc_cutoff` will be
    included in training the TAPAS model.  
  - `verbose` A `logical` argument to print messages. Set to `TRUE` by
    default.

**Return**:

A `list` with the TAPAS model (`tapas_model`) of class `gam` and the a
`tibble` with the clamp information (`clamp_data`). The clamp
information contains the TAPAS-predicted smallest and largest threshold
to be applied by using estimates related to the volume at the 10th and
90th percentile.

### `tapas_predict`

``` r
tapas_predict(pmap, 
              model, 
              clamp = TRUE, 
              k = 8, 
              verbose = TRUE){
```

**Description**:

This function takes a probability map for a single subject and predicts
the subject specific threshold to apply based on the TAPAS model
generated from \`tapas\_train\`\`. The function will return a list of
objects including the TAPAS predicted subject-specific threshold, the
lesion mask produced from applying this threshold, as well as the lesion
mask produced from using the group threshold.

**Usage**:

  - `pmap` A `character` file path to probability map images or an
    object of class `nifti`.
  - `model` The TAPAS model fit from `tapas_train`` of class`gam\`. This
    model will be used to make subject-specific threshold predictions.
  - `clamp` A `logical` object that is `TRUE` by default. This setting
    uses the clamped subject-specific threshold prediction rather than
    the prediction fit by the TAPAS `model`. This only applied to
    volumes exceeding those at the 10th and 90th percentile calculated
    using the training data. Using the clamp data avoids extrapolation
    when the naive volume estimate falls in the tails of the TAPAS
    model. If `FALSE` then the the TAPAS `model` predicted threshold
    will be used for segmentation rather than the clamped threshold. The
    clamping method was used in published work.
  - `k` The minimum number of voxels for a cluster/component. Passed to
    `label_mask`. Segmentation clusters of size less than k are removed
    from the mask, volume estimation, the and Sørensen’s–Dice
    coefficient (DSC) calculation.
  - `verbose` A `logical` argument to print messages. Set to `TRUE` by
    default.

**Return**:

A `list` containing the TAPAS predicted subject-specific threshold
(`subject_threshold`), the lesion segmentation mask obtained using the
TAPAS predicted subject-specific threshold (`tapas_binary_mask`), and
the lesion segmentation mask obtained using the group threshold
(`group_binary_mask`).

### `tapas_predict_par`

``` r
tapas_predict_par(cores = 1,
                  pmap,
                  subject_id,
                  model,
                  clamp = TRUE,
                  k = 8,
                  ret = FALSE,
                  outfile = NULL,
                  verbose = TRUE)
```

**Description**:

This function wraps `tapas_predict` to run in parallel. This function
takes probability maps across subjects and predicts the subject specific
threshold to apply based on the TAPAS model generated from
`tapas_train`. The function will return or save a list of objects for
each subject including the TAPAS predicted subject-specific threshold,
the lesion mask produced from applying this threshold, as well as the
lesion mask produced from using the group threshold.

**Usage**:

  - `cores` The number of cores to use. This argument controls at most
    how many child processes will be run simultaneously. The default is
    set to 1.
  - `pmap` A `vector` of `character` file paths to probability map
    images or a `list` object with elements of class `nifti`.
  - `subject_id` A  of subject IDs of class . By default this is set to 
    but users must provide an ID.
  - `model` The TAPAS model fit from `tapas_train` of class `gam`. This
    model will be used to make subject-specific threshold predictions.
  - `clamp` A `logical` object that is `TRUE` by default. This setting
    uses the clamped subject-specific threshold prediction rather than
    the prediction fit by the TAPAS `model`. This only applied to
    volumes exceeding those at the 10th and 90th percentile calculated
    using the training data. Using the clamp data avoids extrapolation
    when the naive volume estimate falls in the tails of the TAPAS
    model. If `FALSE` then the the TAPAS `model` predicted threshold
    will be used for segmentation rather than the clamped threshold. The
    clamping method was used in published work.
  - `k` The minimum number of voxels for a cluster/component. Passed to
    `\link[extrantsr]{label_mask``. Segmentation clusters of size less
    than k are removed from the mask, volume estimation, the and
    Sørensen's–Dice coefficient (DSC) calculation. @param ret
    A`logical`argument set to`TRUE`by default. Returns a nested`list`of
    objects from the function to the local R environement.
    If`FALSE`then`outfile\` must be specified so subject data is saved.
  - `outfile` Is set to `NULL` by default which only returns the
    subject-level `tibble` as a list in the local R environment. To save
    each subject-level `tibble` as an R object specify a `list` or
    `vector` of file paths to save with either .rds or .RData extensions
    included.
  - `verbose` A `logical` argument to print messages. Set to `TRUE` by
    default.

**Return**:

A nested `list`. Each element in the list contains data from a subject.
The subject data is a `list` object containing the objects returned from
`tapas_predict`.
