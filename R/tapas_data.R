#' @title Generates the TAPAS Training Data
#' @description This function creates the training vectors for a single subject from a probability map,
#' a gold standard mask (normally a manual segmentation), and a brain mask. For a grid of thresholds provided
#' and applied to the probability map the function calculates Sørensen's–Dice coefficient (DSC) between the automatic
#' image and the gold standard image. The function also calculates the volume associated with thresholding
#' at each respective threshold.
#' @param thresholds A \code{vector} of thresholds to apply to the probability map. The default
#' \code{vector} applied is 0 to 1 by 0.01 increments. Threshold values must be
#' between 0 and 1.
#' @param pmap A \code{character} file path to a probability map image or an object of
#' class \code{nifti}.
#' @param gold_standard A \code{character} file path to a gold standard image (normally a manual
#' segmentation) or an object of class \code{nifti}. The gold standard segmentation is used to compare the
#' thresholded probability map image using Sørensen's–Dice coefficient (DSC).
#' @param mask A \code{character} file path to a brain mask image or an object of class \code{nifti}.
#' @param k The minimum number of voxels for a cluster/component.
#' Segmentation clusters of size less than k are removed from the mask, volume estimation, and the
#' Sørensen's–Dice coefficient (DSC) calculation.
#' @param subject_id A subject ID of class \code{character}. By default this is set to \code{NULL} but users must
#' provide an ID.
#' @param verbose A \code{logical} argument to print messages. Set to \code{TRUE} by default.
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom magrittr "%>%"
#' @importFrom methods as
#' @importFrom neurobase check_nifti check_mask niftiarr fast_dice
#' @importFrom neuroim connComp3D
#' @importFrom oro.nifti is.nifti
#' @importFrom tibble tibble
#' @return A \code{tibble} containing the training data for a single subject. The data contains columns \code{threshold},
#' Sørensen's–Dice coefficient (\code{dsc}), and \code{volume}.
#' @examples \dontrun{
#' # Data is provided in the rtapas package as arrays. Below we will convert them to nifti objects.
#' library(oro.nifti)
#' # Gold standard manual segmentation
#' gs1 = oro.nifti::nifti(gs1)
#' # Probability map for subject 1
#' pmap1 = oro.nifti::nifti(pmap1)
#' # Brain mask
#' brain_mask = oro.nifti::nifti(brain_mask)
#'
#' # Run the tapas_data function for subject 1
#' data = tapas_data(thresholds = seq(from = 0, to = 1, by = 0.01),
#'                   pmap = pmap1,
#'                   gold_standard = gs1,
#'                   mask = brain_mask,
#'                   k = 8,
#'                   subject_id = "subject_1",
#'                   verbose = TRUE)
#' # Visualize data returned from the tapas_data function
#' head(data)
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
    base::stop('# ERROR: verbose must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
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
    stop('# ERROR: gold_standard or mask is not binary.')
  }

  # Check that grid is a vector from 0 to 1
  if(base::is.numeric(thresholds) == FALSE | base::any(thresholds < 0) == TRUE | base::any(thresholds > 1) == TRUE){
    stop('# ERROR: thresholds must be a vector between 0 and 1.')
  }

  # Make pmap a vector to sapply over
  pmap = c(pmap[mask == 1])

  # Obtain a matrix of 0/1 after threhsolding at each threshold value
  # pred_lesion = base::sapply(thresholds, function(x) {base::ifelse(pmap > x, 1, 0)})
  pred_lesion = base::sapply(thresholds, function(x) {
    x = as.numeric(pmap > x)
    x[!is.na(x)] = 0
    x
  })

  # initialize a results tibble
  results = tibble::tibble(threshold = thresholds,
                           dsc = thresholds,
                           volume =  thresholds,
                           subject_id = subject_id)

  if (verbose == TRUE){
    base::message('# Obtaining threshold level information.')
  }

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

  # function to calculate the volume and DSC at each threshold
  calc_dv <- function(j, temp_lmask = mask, subject_id = subject_id){
    # Fill in the predicted lesion values from threshold j to a temporary mask
    temp_lmask[mask == 1] = pred_lesion[,j]

    if(base::sum(temp_lmask, na.rm = TRUE) != 0){
      # Label the lesion connected components
      # Remove any lesion smaller than k connected components
      temp_lmask = label_mask(temp_lmask, k = k)
    }
    # Return temp_lmask to binary 0/1
    temp_lmask[temp_lmask > 0] = 1

    if (requireNamespace("aliviateR", quietly = TRUE)) {
      dice_value = aliviateR::dsc(gold_standard = gold_standard, comp_method = temp_lmask)
    } else {
      dice_value = neurobase::fast_dice(gold_standard,
                                        temp_lmask,
                                        verbose = verbose)
    }

    results = tibble::tibble(threshold = thresholds[j],
                             dsc = dice_value,
                             volume =  sum(temp_lmask),
                             subject_id = subject_id)

  }

  results = base::lapply(
    1:length(thresholds),
    calc_dv, temp_lmask = mask,
    subject_id = subject_id) %>%
    dplyr::bind_rows()

  base::return(results)
}
