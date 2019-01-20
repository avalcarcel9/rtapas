#' @title Generates the TAPAS Training Data
#' @description This function creates the training vectors for a single subject from a probability map,
#' a gold standard mask (normally a manual segmentation), and a brain mask. For a grid of thresholds provided
#' and applied to the probability map the function calculates Sørensen's–Dice coefficient (DSC) between the automatic
#' volume and the gold standard volume. The function also calculates the automatic volume associated with thresholding
#' at each respective threshold.
#' @param thresholds A \code{vector} of thresholds to apply to the probability map. The default
#' \code{vector} applied is 0 to 1 by 0.01 increments which matches the published work. Threshold values must be
#' between 0 and 1.
#' @param pmap A \code{character} file path to probability map images or an object of
#' class \code{nifti}.
#' @param gold_standard A \code{character} file path to a gold standard image (normally a manual
#' segmentation) or an object of class \code{nifti}. The gold standard segmentation is used to compare the
#' thresholded probability map image using Sørensen's–Dice coefficient (DSC).
#' @param mask A \code{character} file path to a brain mask image or an object of class \code{nifti}.
#' @param k The minimum number of voxels for a cluster/component. Passed to \code{\link[extrantsr]{label_mask}}.
#' Segmentation clusters of size less than k are removed from the mask, volume estimation, the and
#' Sørensen's–Dice coefficient (DSC) calculation.
#' @param subject_id A subject ID of class \code{character}. By default this is set to \code{NULL} but users must
#' provide an ID.
#' @param verbose A \code{logical} argument to print messages. Set to \code{TRUE} by default.
#' @export
#' @importFrom aliviateR dsc
#' @importFrom dplyr bind_rows
#' @importFrom extrantsr label_mask
#' @importFrom magrittr "%>%"
#' @importFrom neurobase check_nifti check_mask
#' @importFrom tibble tibble
#' @return A \code{tibble} containing the training data. The data contains columns \code{threshold},
#' Sørensen's–Dice coefficient (\code{dsc}), and \code{volume}.
#' @examples \dontrun{
#' tapas_data(thresholds = seq(from = 0, to = 1, by = 0.01),
#' pmap = 'probability_map_subject_1.nii.gz',
#' gold_standard = 'manual_segmentation_subject_1.nii.gz',
#' mask = 'brain_mask_subject_1.nii.gz',
#' k = 8,
#' subject_id = 'subject_1')
#' }

tapas_data <- function(thresholds = seq(from = 0, to = 1, by = 0.01),
                       pmap,
                       gold_standard,
                       mask,
                       k = 8,
                       subject_id = NULL,
                       verbose = TRUE){

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

  # Check gold_standard and mask are both binary 0/1
  if(neurobase::check_mask(gold_standard) == FALSE | neurobase::check_mask(mask) == FALSE){
    stop('# gold_standard or mask is not binary.')
  }

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
