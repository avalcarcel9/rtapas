#' @title Calculate Threshold Grid
#' @description Calculates Sørensen's–Dice coefficient and automatic volume
#'  estimation for a grid of thresholds.
#' @param thresholds A vector of thresholds to use for calculation of Sørensen's–Dice coefficient
#' and automatic volume. The default and grid applied in the published work is 0 to 1. Threshold
#' values must be between 0 and 1.
#' @param pmap A file path to a NIFTI probability map generated from an automatic segmentation
#' approach or the name of a probability map that has been locally loaded
#' @param gold_standard A file path to the NIFTI gold standard segmentation approach corresponding
#' with the probability map provided or the name of the gold standard segmentation that has been
#' locally loaded. The gold standard segmentation is used to compare the thresholded probability map
#' image with using Sørensen's–Dice coefficient.
#' @param mask A file path to a NIFTI brain mask corresponding with the probability map provided
#' or the name of a mask that has been locally loaded.
#' @param k A file path to a NIFTI brain mask corresponding with the probability map provided
#' or the name of a mask that has been locally loaded.
#' @export
#' @importFrom neurobase check_nifti
#' @importFrom tibble tibble
#' @importFrom extrantsr label_mask
#' @importFrom aliviateR dsc
#' @return A tibble with threshold, dsc, and volume columns
#' @examples \dontrun{
#' gen_thresh_data(thresholds = seq(from = 0, to = 1, by = 0.01),
#' pmap = 'probability_map_subject_1.nii.gz',
#' gold_standard = 'manual_segmentation_subject_1.nii.gz',
#' mask = 'brain_mask_subject_1.nii.gz',
#' k = 8)
#' }


gen_thresh_data <- function(thresholds = seq(from = 0, to = 1, by = 0.01), pmap, gold_standard, mask, k = 8){
  # Verify inputs are NIFTI objects
  pmap = neurobase::check_nifti(pmap)
  gold_standard = neurobase::check_nifti(gold_standard)
  mask = neurobase::check_nifti(mask)

  # Check that grid is a vector from 0 to 1
  if(is.numeric(thresholds) == FALSE | any(thresholds < 0) == TRUE | any(thresholds > 1) == TRUE){
    return('thresholds must be a vector between 0 and 1.')
  }

  # Make pmap and gold_standard vectors to sapply over
  pmap = c(pmap[mask == 1])
  gold_standard = c(gold_standard[mask == 1])

  # Obtain a matrix of 0/1 after threhsolding at each threshold value
  pred_lesion = sapply(thresholds, function(x) {ifelse(pmap > x, 1, 0)})

  # initialize a results tibble
  results = tibble::tibble(threshold = thresholds, dsc = thresholds, volume =  thresholds)

  # function to calculate the volume and DSC at each threshold
  calc_dv <- function(j, temp_lmask = mask){
    # Fill in the predicted lesion values from threshold j to a temporary mask
    temp_lmask[mask == 1] = pred_lesion[,j]
    # Label the lesion connected components
    # Remove any lesion smaller than k connected components
    temp_lmask = extrantsr::label_mask(temp_lmask, k = k)
    # Return temp_lmask to binary 0/1
    temp_lmask[temp_lmask > 0] = 1
    # Obtain the volume for threshold j
    results$volume[j] = sum(temp_lmask)
    # Calculate DSC between the temporary mask and the gold standard
    results$dsc[j] = aliviateR::dsc(gold_standard = gold_standard, comp_method = temp_lmask)
  }

  lapply(1:length(thresholds), calc_dv)

  return(results)

}
