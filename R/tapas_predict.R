#' @title TAPAS Prediction
#' @description This function takes a probability map for a single subject and predicts the subject-specific
#' threshold to apply based on the TAPAS model generated from \code{\link{tapas_train}}.
#' The function will return a \code{list} of objects including the TAPAS predicted subject-specific threshold,
#' the lesion mask produced from applying this threshold, and the lesion mask
#' produced from using the group threshold.
#' @param pmap A \code{character} file path to a probability map image or an object of
#' class \code{nifti}.
#' @param model The TAPAS model fit from \code{\link{tapas_train}} of class \code{gam}. This model will be
#' used to make subject-specific threshold predictions.
#' @param clamp A \code{logical} object set to \code{TRUE} by default. This setting uses the clamped
#' subject-specific threshold prediction rather than the prediction fit by the
#' TAPAS \code{model}. This only applies to volumes exceeding those at the 10th and 90th percentile
#' calculated using the training data. Using the clamp data avoids extrapolation when the naive volume estimate
#' falls in the tails of the TAPAS model. If \code{FALSE} then the the TAPAS model predicted threshold
#' will be used for segmentation rather than the clamped threshold.
#' @param k The minimum number of voxels for a cluster/component. Passed to \code{\link[extrantsr]{label_mask}}.
#' Segmentation clusters of size less than k are removed from the mask, volume estimation, and the
#' Sørensen's–Dice coefficient (DSC) calculation.
#' @param verbose A \code{logical} argument to print messages. Set to \code{TRUE} by default.
#' @export
#' @importFrom dplyr filter
#' @importFrom gtools inv.logit
#' @importFrom methods as
#' @importFrom mgcv predict.gam
#' @importFrom neurobase check_nifti niftiarr
#' @importFrom neuroim connComp3D
#' @importFrom oro.nifti is.nifti
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @return A \code{list} containing the TAPAS predicted subject-specific threshold (\code{subject_threshold}), the lesion
#' segmentation mask obtained using the TAPAS predicted subject-specific threshold (\code{tapas_binary_mask}), and the
#' lesion segmentation mask obtained using the group threshold (\code{group_binary_mask}).
#' @examples \dontrun{
#' # Data is provided in the rtapas package as arrays. Below we will convert them to nifti objects.
#' # Before we can implement the train_tapas function we have to generate the training data
#' library(oro.nifti)
#' # Create a list of gold standard manual segmentation
#' train_gold_standard_masks = list(gs1 = gs1,
#'                                  gs2 = gs2,
#'                                  gs3 = gs3,
#'                                  gs4 = gs4,
#'                                  gs5 = gs5,
#'                                  gs6 = gs6,
#'                                  gs7 = gs7,
#'                                  gs8 = gs8,
#'                                  gs9 = gs9,
#'                                  gs10 = gs10)
#' # Convert the gold standard masks to nifti objects
#' train_gold_standard_masks = lapply(train_gold_standard_masks, oro.nifti::nifti)
#'
#' # Make a list of the training probability maps
#' train_probability_maps = list(pmap1 = pmap1,
#'                              pmap2 = pmap2,
#'                              pmap3 = pmap3,
#'                              pmap4 = pmap4,
#'                              pmap5 = pmap5,
#'                              pmap6 = pmap6,
#'                              pmap7 = pmap7,
#'                              pmap8 = pmap8,
#'                              pmap9 = pmap9,
#'                              pmap10 = pmap10)
#'
#' # Convert the probability maps to nifti objects
#' train_probability_maps = lapply(train_probability_maps, oro.nifti::nifti)
#' # Make a list of the brain masks
#' train_brain_masks = list(brain_mask1 = brain_mask,
#'                          brain_mask2 = brain_mask,
#'                          brain_mask3 = brain_mask,
#'                          brain_mask4 = brain_mask,
#'                          brain_mask5 = brain_mask,
#'                          brain_mask6 = brain_mask,
#'                          brain_mask7 = brain_mask,
#'                          brain_mask8 = brain_mask,
#'                          brain_mask9 = brain_mask,
#'                          brain_mask10 = brain_mask)
#'
#' # Convert the brain masks to nifti objects
#' train_brain_masks = lapply(train_brain_masks, oro.nifti::nifti)
#'
#' # Specify training IDs
#' train_ids = paste0('subject_', 1:length(train_gold_standard_masks))
#'
#' # The function below runs on 2 cores. Be sure your machine has 2 cores available or switch to 1.
#' # Run tapas_data_par function
#' # You can also use the tapas_data function and generate each subjects data
#' data = tapas_data_par(cores = 2,
#'                       thresholds = seq(from = 0, to = 1, by = 0.01),
#'                       pmap = train_probability_maps,
#'                       gold_standard = train_gold_standard_masks,
#'                       mask = train_brain_masks,
#'                       k = 0,
#'                       subject_id = train_ids,
#'                       ret = TRUE,
#'                       outfile = NULL,
#'                       verbose = TRUE)
#'
#' # We can now implement the train_tapas function using the data from tapas_data_par
#' tapas_model = tapas_train(data = train_data1,
#'                           dsc_cutoff = 0.03,
#'                           verbose = TRUE)
#'
#' # Load a subject reserved for testing and convert to a nifti
#' # Probability map
#' pmap11 = oro.nifti::nifti(pmap11)
#' # Brain mask
#' brain_mask = oro.nifti::nifti(brain_mask)
#'
#' # Use TAPAS to predict on a new subject
#' test_subject_prediction = tapas_predict(pmap = pmap11,
#'                                         model = tapas_model,
#'                                         clamp = TRUE,
#'                                         k = 0,
#'                                         verbose = TRUE)
#' # Show subject-specific TAPAS threshold
#' test_subject_prediction$subject_threshold
#' # Look at TAPAS binary segmentation from applying the TAPAS threshold
#' oro.nifti::image(test_subject_prediction$tapas_binary_mask)
#' # Look at group threshold binary segmentation from applying the group threshold
#' oro.nifti::image(test_subject_prediction$group_binary_threshold)
#' }

tapas_predict <- function(pmap, model, clamp = TRUE, k = 8, verbose = TRUE){

  # Check that verbose is TRUE or FALSE
  if(base::is.logical(verbose) == FALSE){
    base::stop('# ERROR: verbose must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }
  # Check clamp is TRUE or FALSE
  if(base::is.logical(clamp) == FALSE){
    base::stop('# ERROR: clamp must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
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
  group_binary_mask[pmap >= model$group_threshold] = 1
  group_binary_mask[pmap < model$group_threshold] = 0

  # Function comes from extrantsr::label_mask
  # Copy and pasted to stabilize package while extrantsr and ANTsX content is being worked on
  # https://github.com/muschellij2/extrantsr/blob/master/R/label_mask.R
  label_mask = function(img, k = k){
    bin_mask = methods::as(img > 0, "array")
    cc = neuroim::connComp3D(bin_mask)
    cc$index[cc$size < k] = 0
    les_xyz = cc$index
    if (oro.nifti::is.nifti(img)) {
      les_xyz = neurobase::niftiarr(img, les_xyz)
    }
    return(les_xyz)
  }

  # Remove connected components less than k then return mask to binary 0/1 after counted components
  if(sum(group_binary_mask) > 0){
    group_binary_mask = label_mask(group_binary_mask, k = k)
    group_binary_mask[group_binary_mask > 0] = 1
  }

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
  tapas_binary_mask[pmap >= subject_threshold[1]] = 1
  tapas_binary_mask[pmap < subject_threshold[1]] = 0

  # Remove connected components less than k then return mask to binary 0/1 after counted components
  if(sum(tapas_binary_mask) > 0){
    tapas_binary_mask = label_mask(tapas_binary_mask, k = k)
    tapas_binary_mask[tapas_binary_mask > 0] = 1
  }

  # Return subject-specific threshold, tapas lesion mask, group lesion mask
  base::return(base::list(subject_threshold = subject_threshold,
                          tapas_binary_mask = tapas_binary_mask,
                          group_binary_mask = group_binary_mask))
}
