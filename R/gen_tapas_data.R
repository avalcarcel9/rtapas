#' @title Calculate Threshold Grid
#' @description This function creates the training data from a single probability map generated using an
#' automatic segmentation method in order generate data necessary to train the TAPAS model.
#' For a grid of threhsolds provided it calculates Sørensen's–Dice coefficient and automatic volume
#' estimation for a grid of thresholds.
#' @param thresholds A \code{vector} of thresholds to use for calculation of Sørensen's–Dice coefficient (DSC)
#' and automatic volume. The default and grid applied in the published work is 0 to 1. Threshold
#' values must be between 0 and 1.
#' @param pmap A file path to a \code{nifti} probability map generated from an automatic segmentation
#' approach or the name of a probability map locally available
#' @param gold_standard A file path to the \code{nifti} gold standard segmentation approach corresponding
#' with the probability map provided or the name of the gold standard segmentation that has been
#' locally loaded. The gold standard segmentation is used to compare the thresholded probability map
#' image with using Sørensen's–Dice coefficient.
#' @param mask A file path to a \code{nifti} brain mask corresponding with the probability map provided
#' or the name of a mask that has been locally loaded.
#' @param k Minimum number of voxels for a cluster/component. Passed to \code{\link[extrantsr]{label_mask}}.
#' @param subject_id is \code{NULL} by default. Must be a \code{character} \code{vector} with the unique subject ID for
#' which the threshold data is being generated.
#' @param verbose is \code{TRUE} by default. \code{TRUE} returns messages throughout the generating data function.
#' \code{FALSE} will silence comment returns.
#' @export
#' @importFrom aliviateR dsc
#' @importFrom dplyr bind_rows
#' @importFrom extrantsr label_mask
#' @importFrom magrittr "%>%"
#' @importFrom neurobase check_nifti
#' @importFrom tibble tibble
#' @return A tibble with threshold, Sørensen's–Dice coefficient (dsc), and volume
#' @examples \dontrun{
#' gen_tapas_data(thresholds = seq(from = 0, to = 1, by = 0.01),
#' pmap = 'probability_map_subject_1.nii.gz',
#' gold_standard = 'manual_segmentation_subject_1.nii.gz',
#' mask = 'brain_mask_subject_1.nii.gz',
#' k = 8,
#' subject_id = 'subject_1')
#' }

gen_tapas_data <- function(thresholds = seq(from = 0, to = 1, by = 0.01), pmap, gold_standard, mask, k = 8, subject_id = NULL, verbose = TRUE){

  # Check that verbose is TRUE or FALSE
  if(base::is.logical(verbose) == FALSE){
    base::stop('# verbose must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }

  if(verbose == TRUE){
    base::message('# Validating parameter inputs.')
  }
  # Verify inputs are NIFTI objects
  pmap = neurobase::check_nifti(pmap)
  gold_standard = neurobase::check_nifti(gold_standard)
  mask = neurobase::check_nifti(mask)

  # Check that grid is a vector from 0 to 1
  if(base::is.numeric(thresholds) == FALSE | base::any(thresholds < 0) == TRUE | base::any(thresholds > 1) == TRUE){
    stop('# thresholds must be a vector between 0 and 1.')
  }

  # Make pmap a vector to sapply over
  pmap = c(pmap[mask == 1])

  # Obtain a matrix of 0/1 after threhsolding at each threshold value
  pred_lesion = base::sapply(thresholds, function(x) {base::ifelse(pmap > x, 1, 0)})

  # initialize a results tibble
  results = tibble::tibble(threshold = thresholds,
                           dsc = thresholds,
                           volume =  thresholds,
                           subject_id = subject_id)

  if(verbose == TRUE){
    base::message('# Obtaining threshold level information.')
  }

  # function to calculate the volume and DSC at each threshold
  calc_dv <- function(j, temp_lmask = mask, subject_id = subject_id){
    # Fill in the predicted lesion values from threshold j to a temporary mask
    temp_lmask[mask == 1] = pred_lesion[,j]

    if(base::sum(temp_lmask) != 0){
      # Label the lesion connected components
      # Remove any lesion smaller than k connected components
      temp_lmask = extrantsr::label_mask(temp_lmask, k = k)
    }
    # Return temp_lmask to binary 0/1
    temp_lmask[temp_lmask > 0] = 1

    results = tibble::tibble(threshold = thresholds[j],
                             dsc = aliviateR::dsc(gold_standard = gold_standard, comp_method = temp_lmask),
                             volume =  sum(temp_lmask),
                             subject_id = subject_id)

  }

  results = base::lapply(1:length(thresholds), calc_dv, temp_lmask = mask, subject_id = subject_id) %>%
    dplyr::bind_rows()

  base::return(results)
}
