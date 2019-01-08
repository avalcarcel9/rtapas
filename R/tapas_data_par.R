#' @title Generates The TAPAS Training Data in Parallel
#' @description This function wraps the \code{\link{gen_tapas_data}} to run in parallel. We create the
#' training vectors for subjects from a probability map, gold standard mask (normally
#' manual segmentation), and brain mask. For a grid of thresholds provided and applied to the
#' probability map it calculates Sørensen's–Dice coefficient between the automatic volume and the
#' gold standard volume as well as the automatic volume estimation for each threshold.
#' @param thresholds A \code{vector} of thresholds to apply to the probability maps. The default
#' \code{vector} applied is 0 to 1 by 0.01 increments which matches the published work. Threshold values must be
#' between 0 and 1.
#' @param cores The number of cores to use. This argument controls at most how many child processes will
#' be run simultaneously. The default is set to 1.
#' @param pmap A \code{vector} of \code{character} file paths to probability map images or a
#' \code{list} object with elements of class \code{nifti}.
#' @param gold_standard A \code{vector} of \code{character} file paths to gold standard images (normally
#' a manual segmentation) or a \code{list} object with elements of class \code{nifti}. The gold
#' standard segmentation is used to compare the thresholded probability map image using Sørensen's–Dice
#' coefficient (DSC).
#' @param mask A \code{vector} of \code{character} file paths to brain mask images or a \code{list} object
#' with elements of class \code{nifti}.
#' @param k The minimum number of voxels for a cluster/component. Passed to \code{\link[extrantsr]{label_mask}}.
#' Segmentation clusters of size less than k are removed from the mask, volume estimation, the and
#' Sørensen's–Dice coefficient (DSC) calculation.
#' @param subject_id A subject ID of class \code{character}. By default this is set to \code{NULL} but users must
#' provide an ID.
#' @param verbose A \code{logical} argument to print messages. Set to \code{TRUE} by default.
#' @export
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster mclapply
#' @return A list of the \code{tibble} object returned from \code{\link{gen_tapas_data}} for each subject.
#' @examples \dontrun{
#' tapas_data_par(cores = 1,
#' thresholds = seq(from = 0, to = 1, by = 0.01),
#' pmap, gold_standard,
#' mask,
#' k = 8,
#' subject_id = NULL,
#' verbose = TRUE)
#' }

### ADD A PARAMETER FOR FILEPATHS TO SAVE
### ADD A PARAMETER TO RETURN OBJECTS OR NOT

# outfile = NULL,
# retimg = TRUE,

tapas_data_par <- function(cores = 1,
                           thresholds = seq(from = 0, to = 1, by = 0.01),
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

  # Check that files exists for pmap
  if(base::is.list(pmap) == FALSE){
    if(base::any(base::file.exists(pmap)) == FALSE){
      base::stop('# At least one pmap file path does not exist.')
    }
  }

  # Check that files exists for gold_standard
  if(base::is.list(gold_standard) == FALSE){
    if(base::any(base::file.exists(gold_standard)) == FALSE){
      base::stop('# At least one gold_standard file path does not exist.')
    }
  }

  # Check that files exists for mask
  if(base::is.list(mask) == FALSE){
    if(base::any(base::file.exists(mask)) == FALSE){
      base::stop('# At least one mask file path does not exist.')
    }
  }

  # Check that pmap, gold_standard, mask, and subject_id are all the same length
  if(base::length(pmap) != base::length(gold_standard) | base::length(pmap) == base::length(mask) | base::length(pmap) == base::length(subject_id)){
    base::stop("# pmap, gold_standard, mask, or subject_id are not of equal lengths.")
  }

  tapas_parallel <- function(i){
    subject_data = gen_tapas_data(thresholds = seq(from = 0, to = 1, by = 0.01),
                                  pmap = pmap[[i]],
                                  gold_standard = gold_standard[[i]],
                                  mask = mask[[i]],
                                  k = 8,
                                  subject_id = subject_id[[i]],
                                  verbose = TRUE)
    base::return(subject_data)
  }

  if(Sys.info()["sysname"] == "Windows"){
    cl = parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    results = foreach::foreach(i = 1:length(pmap)) %dopar% tapas_parallel(i)
    stopCluster(cl)
  } else if (Sys.info()["sysname"] != "Windows"){
    results = parallel::mclapply(1:length(pmap), tapas_parallel, mc.cores = cores)
  }

  return(results)
}
