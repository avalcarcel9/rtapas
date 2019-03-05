#' @title Generates The TAPAS Training Data in Parallel
#' @description This function wraps \code{\link{tapas_data}} to run in parallel. This function creates the training vectors for a single subject from a probability map,
#' a gold standard mask (normally a manual segmentation), and a brain mask. For a grid of thresholds provided
#' and applied to the probability map the function calculates Sørensen's–Dice coefficient (DSC) between the automatic
#' image and the gold standard image. The function also calculates the volume associated with thresholding
#' at each respective threshold.
#' @param cores The number of cores to use. This argument controls at most how many child processes will
#' be run simultaneously. The default is set to 1.
#' @param thresholds A \code{vector} of thresholds to apply to the probability maps. The default
#' \code{vector} applied is 0 to 1 by 0.01 increments which matches the published work. Threshold values must be
#' between 0 and 1.
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
#' @param subject_id A \code{vector} of subject IDs of class \code{character}. By default this is set to \code{NULL} but users must
#' provide an ID.
#' @param ret A \code{logical} argument set to \code{TRUE} by default. Returns the \code{tibble} objects from the
#' function as a \code{list} in the local R environement. If \code{FALSE} then \code{outfile} must be specified so subject data is
#' saved.
#' @param outfile Is set to \code{NULL} by default which only returns the subject-level \code{tibble} as a list
#' in the local R environment. To save each subject-level \code{tibble} as an R object
#' specify a \code{list} or \code{vector} of file paths to save with either .rds or .RData extensions included.
#' @param verbose A \code{logical} argument to print messages. Set to \code{TRUE} by default.
#' @export
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster mclapply stopCluster
#' @importFrom stringr str_detect
#' @return A list of the \code{tibble} object returned from \code{\link{tapas_data}} for each subject.
#' @examples \dontrun{
#' tapas_data_par(cores = 1,
#' thresholds = seq(from = 0, to = 1, by = 0.01),
#' pmap, gold_standard,
#' mask,
#' k = 8,
#' subject_id = NULL,
#' verbose = TRUE)
#' }

tapas_data_par <- function(cores = 1,
                           thresholds = seq(from = 0, to = 1, by = 0.01),
                           pmap,
                           gold_standard,
                           mask,
                           k = 8,
                           subject_id = NULL,
                           ret = FALSE,
                           outfile = NULL,
                           verbose = TRUE){

  # Check that verbose is TRUE or FALSE
  if(base::is.logical(verbose) == FALSE){
    base::stop('# verbose must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }

  # Check that ret is TRUE or FALSE
  if(base::is.logical(ret) == FALSE){
    base::stop('# ret must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
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

  # Check that outfile is character or NULL
  if(base::any(base::is.character(outfile)) == FALSE & base::is.null(outfile) == FALSE){
    base::stop('# At least one outfile is not character. Must be character or NULL.')
  }

  # Check that pmap, gold_standard, mask, and subject_id are all the same length
  if(base::length(pmap) != base::length(gold_standard) | base::length(pmap) != base::length(mask) | base::length(pmap) != base::length(subject_id)){
    base::stop("# pmap, gold_standard, mask, or subject_id are not of equal lengths.")
  }

  data_parallel <- function(i){
    subject_data = tapas_data(thresholds = seq(from = 0, to = 1, by = 0.01),
                              pmap = pmap[[i]],
                              gold_standard = gold_standard[[i]],
                              mask = mask[[i]],
                              k = k,
                              subject_id = subject_id[[i]],
                              verbose = verbose)

    # Don't return the tibble and just save to the outfile
    if(ret == FALSE & base::is.null(outfile) == FALSE){
      if(stringr::str_detect(outfile[[i]], '.rds') == TRUE){
        base::saveRDS(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.RData') == TRUE){
        base::save(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.rds|.RData') == FALSE){
        base::stop('# outfile must have .rds or .RData extension.')
      }
    }
    # Return the tibble and save to the outfile
    if(ret == TRUE & base::is.null(outfile) == FALSE){
      if(stringr::str_detect(outfile[[i]], '.rds') == TRUE){
        base::saveRDS(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.RData') == TRUE){
        base::save(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.rds|.RData') == FALSE){
        base::stop('# outfile must have .rds or .RData extension.')
      }
      base::return(subject_data)
    }
    # Return the tibble and do not save outfile
    if(ret == TRUE & base::is.null(outfile) == TRUE){
      base::return(subject_data)
      message('ret == TRUE & base::is.null(outfile) == TRUE')
    }
  }

  if(Sys.info()["sysname"] == "Windows"){
    cl = parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    results = foreach::foreach(i = 1:length(pmap)) %dopar% data_parallel(i)
    stopCluster(cl)
  } else if (Sys.info()["sysname"] != "Windows"){
    results = parallel::mclapply(1:length(pmap), data_parallel, mc.cores = cores)
  }

  if(ret == TRUE){
    return(results)
  }
}
