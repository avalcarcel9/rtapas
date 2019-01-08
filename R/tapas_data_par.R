#' @title Generate TAPAS Data in Parallel
#' @description This function wraps the \code{\link{gen_tapas_data}} to run in parallel and create the
#' training data from probability maps generated using an automatic segmentation method in order generate
#' data necessary to train the TAPAS model. For a grid of threhsolds provided it calculates Sørensen's–Dice coefficient and automatic volume
#' estimation for a grid of thresholds.
#' @param thresholds A \code{vector} of thresholds to use for calculation of Sørensen's–Dice coefficient (DSC)
#' and automatic volume. The default and grid applied in the published work is 0 to 1. Threshold
#' values must be between 0 and 1.
#' @param cores The number of cores to use, i.e. at most how many child processes will be run
#' simultaneously. Default is set to 1.
#' @param pmap A file path to a \code{nifti} probability map generated from an automatic segmentation
#' approach or the name of a list of probability map locallys available.
#' @param gold_standard A vector of file paths to the \code{nifti} gold standard segmentation approach corresponding
#' with the probability map provided or the name of a list of gold standard segmentations that has been
#' locally loaded. The gold standard segmentation is used to compare the thresholded probability map
#' image with using Sørensen's–Dice coefficient.
#' @param mask A vector file paths to a \code{nifti} brain mask corresponding with the probability map provided
#' or the name of a list of masks that has been locally loaded.
#' @param k Minimum number of voxels for a cluster/component. Passed to \code{\link[extrantsr]{label_mask}}.
#' @param subject_id is \code{NULL} by default. Must be a \code{vector} of \code{character} strings unique subject IDs for
#' which the threshold data is being generated.
#' @param verbose is \code{TRUE} by default. \code{TRUE} returns messages throughout the generating data function.
#' \code{FALSE} will silence comment returns.
#' @export
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster mclapply
#' @return A list of the \code{tibble}s returned from \code{\link{gen_tapas_data}} for each subject.
#' @examples \dontrun{
#' tapas_data_par(cores = 1,
#' thresholds = seq(from = 0, to = 1, by = 0.01),
#' pmap, gold_standard,
#' mask,
#' k = 8,
#' subject_id = NULL,
#' verbose = TRUE)
#' }

tapas_data_par <- function(cores = 1, thresholds = seq(from = 0, to = 1, by = 0.01), pmap, gold_standard, mask, k = 8, subject_id = NULL, verbose = TRUE){

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
