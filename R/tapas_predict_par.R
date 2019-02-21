#' @title TAPAS Prediction in Parallel
#' @description This function wraps \code{\link{tapas_predict}} to run in parallel. This function takes
#' probability maps across subjects and predicts the subject specific threshold to apply based on the
#' TAPAS model generated from \code{\link{tapas_train}}. The function will return or save a list of objects
#' for each subject including the TAPAS predicted subject-specific threshold, the lesion mask produced from
#' applying this threshold, as well as the lesion mask produced from using the group threshold.
#' @param cores The number of cores to use. This argument controls at most how many child processes will
#' be run simultaneously. The default is set to 1.
#' @param pmap A \code{vector} of \code{character} file paths to probability map images or a
#' \code{list} object with elements of class \code{nifti}.
#' @param subject_id A \code{vector} of subject IDs of class \code{character}. By default this is set to \code{NULL} but users must
#' provide an ID.
#' @param model The TAPAS model fit from \code{\link{tapas_train}} of class \code{gam}. This model will be
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
#' @param ret A \code{logical} argument set to \code{TRUE} by default. Returns a nested \code{list} of objects from the
#' function to the local R environement. If \code{FALSE} then \code{outfile} must be specified so subject data is
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
#' @return A nested \code{list}. Each element in the list contains data from a subject. The subject data
#' is a \code{list} object containing the objects returned from \code{\link{tapas_predict}}.
#' @examples \dontrun{
#' tapas_data_par(cores = 1,
#' thresholds = seq(from = 0, to = 1, by = 0.01),
#' pmap, gold_standard,
#' mask,
#' k = 8,
#' subject_id = NULL,
#' verbose = TRUE)
#' }

tapas_predict_par <- function(cores = 1,
                              pmap,
                              subject_id,
                              model,
                              clamp = TRUE,
                              k = 8,
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

  # Check that outfile is character
  if(base::any(base::is.character(outfile)) == FALSE & base::is.null(outfile) == FALSE){
    base::stop('# At least one outfile is not character. Must be character or NULL.')
  }

  predict_parallel <- function(i){
    subject_data = tapas_predict(pmap = pmap[[i]],
                                 model = model,
                                 clamp = clamp,
                                 k = k,
                                 verbose = verbose)

    subject_data = base::list(subject_id = subject_id[[i]],
                              subject_threshold = subject_data$subject_threshold,
                              tapas_binary_mask = subject_data$tapas_binary_mask,
                              group_binary_mask = subject_data$group_binary_mask)


    # Don't return the list and just save to the outfile
    if(ret == FALSE & base::is.null(outfile) == FALSE){
      if(stringr::str_detect(outfile[[i]], '.rds') == TRUE){
        base::saveRDS(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.RData') == TRUE){
        base::save(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.rds|.RData') == FALSE){
        base::stop('# outfile must have .rds or .RData extension.')
      }
    }

    # Return the list and save to the outfile
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

    # Return the list and do not save outfile
    if(ret == TRUE & base::is.null(outfile) == TRUE){
      base::return(subject_data)
    }


  }

  if(Sys.info()["sysname"] == "Windows"){
    cl = parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    results = foreach::foreach(i = 1:length(pmap)) %dopar% predict_parallel(i)
    parallel::stopCluster(cl)
  } else if (Sys.info()["sysname"] != "Windows"){
    results = parallel::mclapply(1:length(pmap), predict_parallel, mc.cores = cores)
  }

  if(ret == TRUE){
    return(results)
  }
}
