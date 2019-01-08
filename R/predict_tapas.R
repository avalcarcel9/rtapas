#' @title TAPAS Prediction
#' @description This function takes a probability map for a single subject and predicts the subject
#' specific threshold to apply based on the TAPAS model generated from \code{\link{train_tapas}}.
#' The function will return a list of objects including the TAPAS predicted subject-specific threshold,
#' the lesion mask produced from applying this threshold, as well as the lesion mask
#' produced from using the group threshold.
#' @param pmap A \code{character} file path to probability map images or an object of
#' class \code{nifti}.
#' @param model The TAPAS model fit from \code{\link{train_tapas}} of class \code{gam}. This model will be
#' used to make subject-specific threshold predictions.
#' @param clamp A \code{logical} object that is \code{TRUE} by default. This setting uses the clamped
#' subject-specific threshold prediction rather than the prediction fit by the
#' TAPAS \code{model}. This only applied to volumes exceeding those at the 10th and 90th percentile
#' calculated using the training data. Using the clamp data avoids extrapolation when the naive volume estimate
#' falls in the tails of the TAPAS model. If \code{FALSE} then the the TAPAS \code{model} predicted threshold
#' will be used for segmentation rather than the clamped threshold. The clamping method was
#' used in published work.
#' @param k The minimum number of voxels for a cluster/component. Passed to \code{\link[extrantsr]{label_mask}}.
#' Segmentation clusters of size less than k are removed from the mask, volume estimation, the and
#' Sørensen's–Dice coefficient (DSC) calculation.
#' @param verbose A \code{logical} argument to print messages. Set to \code{TRUE} by default.
#' @export
#' @importFrom dplyr filter
#' @importFrom extrantsr label_mask
#' @importFrom gtools inv.logit
#' @importFrom mgcv predict.gam
#' @importFrom neurobase check_nifti
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @return A \code{list} containing the TAPAS predicted subject-specific threshold (\code{subject_threshold}), the lesion
#' segmentation mask obtained using the TAPAS predicted subject-specific threshold (\code{tapas_binary_mask}), and the
#' lesion segmentation mask obtained using the group threshold (\code{group_binary_mask}).
#' @examples \dontrun{
#' # Put the code from other examples in here
#' predict_tapas(pmap, model, clamp = TRUE, k = 8, verbose = TRUE)
#'
#' }

predict_tapas <- function(pmap, model, clamp = TRUE, k = 8, verbose = TRUE){

  # Check that verbose is TRUE or FALSE
  if(base::is.logical(verbose) == FALSE){
    base::stop('# verbose must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }
  # Check clamp is TRUE or FALSE
  if(base::is.logical(clamp) == FALSE){
    base::stop('# clamp must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }

  if(verbose == TRUE){
    base::message('# Validating parameter inputs.')
  }

  # Verify inputs
  pmap = neurobase::check_nifti(pmap)

  if(verbose == TRUE){
    base::message('# Estimating naive volume estimate.')
  }

  # Threshold pmap at the group threshold to obtain a naive volume estimate
  group_binary_mask = pmap
  group_binary_mask[pmap > model$group_threshold] = 1
  group_binary_mask[pmap <= model$group_threshold] = 0

  # Remove connected components less than k then return mask to binary 0/1 after counted components
  group_binary_mask = extrantsr::label_mask(group_binary_mask, k = k)
  group_binary_mask[group_binary_mask > 0] = 1

  # Obtain a naive estimate of the volume using the group thresholded binary mask
  naive_volume = tibble::tibble(volume = base::sum(group_binary_mask))

  if(verbose == TRUE){
    base::message('# Estimating subject-specific threshold.')
  }

  if(naive_volume$volume < dplyr::filter(model$clamp_data, .data$bound == 'lower')$volume & clamp == TRUE){
    # Use lower bound threshold prediction
    if(verbose == TRUE){
      base::message('# Subject is below clamp volume. Using the threshold associated with the volume at the 10th precentile.')
    }
    # Make threshold object
    subject_threshold = dplyr::filter(model$clamp_data, .data$bound == 'lower')$pred_threshold
  } else if (naive_volume$volume > dplyr::filter(model$clamp_data, .data$bound == 'upper')$volume & clamp == TRUE){
    # Use upper bound threshold prediction
    if(verbose == TRUE){
      base::message('# Subject is above clamp volume. Using the threshold associated with the 10th precentile volume estimate.')
    }
    # Make threshold object
    subject_threshold = dplyr::filter(model$clamp_data, .data$bound == 'upper')$pred_threshold
  } else if (naive_volume$volume >= dplyr::filter(model$clamp_data, .data$bound == 'lower')$volume &
             naive_volume$volume <= dplyr::filter(model$clamp_data, .data$bound == 'upper')$volume | clamp == FALSE){
    # Subject fell outside clamp bounds but clamp == FALSE
    if(naive_volume$volume < dplyr::filter(model$clamp_data, .data$bound == 'lower')$volume |
       naive_volume$volume > dplyr::filter(model$clamp_data, .data$bound == 'upper')$volume){
      base::message('# Subject is above clamp volume. Using the threshold associated with the 90th precentile volume estimate.')
    }
    # Use the subject-specific threshold fit from the TAPAS model
    subject_threshold = base::unname(gtools::inv.logit(mgcv::predict.gam(model$tapas_model, naive_volume, type = "response")))
  }

  # Update the binary mask using the subject-specific threshold
  tapas_binary_mask = pmap
  tapas_binary_mask[pmap > model$subject_threshold] = 1
  tapas_binary_mask[pmap <= model$subject_threshold] = 0

  # Remove connected components less than k then return mask to binary 0/1 after counted components
  tapas_binary_mask = extrantsr::label_mask(tapas_binary_mask, k = k)
  tapas_binary_mask[tapas_binary_mask > 0] = 1

  # Return subject-specific threshold, tapas lesion mask, group lesion mask
  base::return(base::list(subject_threshold = subject_threshold,
                          tapas_binary_mask = tapas_binary_mask,
                          group_binary_mask = group_binary_mask))
}
