#' @title TAPAS Prediction in Parallel
#' @description This function wraps \code{\link{tapas_predict}} to run in parallel. This function takes
#' probability maps across subjects and predicts the subject-specific threshold to apply based on the
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
#' @param clamp  A \code{logical} object set to \code{TRUE} by default. This setting uses the clamped
#' subject-specific threshold prediction rather than the prediction fit by the
#' TAPAS model. This only applies to volumes exceeding those at the 10th and 90th percentile
#' calculated using the training data. Using the clamp data avoids extrapolation when the naive volume estimate
#' falls in the tails of the TAPAS model. If \code{FALSE} then the TAPAS \code{model} predicted threshold
#' will be used for segmentation rather than the clamped threshold.
#' @param k The minimum number of voxels for a cluster/component.
#' Segmentation clusters of size less than k are removed from the mask, volume estimation, and the
#' Sørensen's–Dice coefficient (DSC) calculation.
#' @param ret A \code{logical} argument set to \code{TRUE} by default. Returns a nested \code{list} of objects from the
#' function to the local R environment. If \code{FALSE} then \code{outfile} must be specified so subject data is
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
#' @return A nested \code{list}. Each element in the list contains subject-level data returned from \code{\link{tapas_predict}}.
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
#' # Obtain 2 test subject probability maps
#' test_probability_maps = list(pmap11 = pmap11,
#'                              pmap12 = pmap12)
#'
#' # Make array objects niftis
#' test_probability_maps = lapply(test_probability_maps, oro.nifti::nifti)
#'
#' # Create a list of testing brain masks
#' test_brain_masks = list(brain_mask11 = brain_mask,
#'                         brain_mask12 = brain_mask)
#'
#' # Make array objects niftis
#' test_brain_masks = lapply(test_brain_masks, oro.nifti::nifti)
#'
#' # Create a vector of IDs
#' test_ids = paste0('subject_', (10 + 1:length(test_gold_standard_masks)))
#'
#' # Run tapas_predict_par function
#' test_subject_prediction2 = tapas_predict_par(cores = 2,
#'                                              pmap = test_probability_maps,
#'                                              subject_id = test_ids,
#'                                              model = tapas_model,
#'                                              clamp = TRUE,
#'                                              k = 0,
#'                                              ret = TRUE,
#'                                              outfile = NULL,
#'                                              verbose = TRUE)
#'
#'
#' names(test_subject_prediction2)
#' names(test_subject_prediction2[[1]])
#' test_subject_prediction2[[1]]$subject_threshold
#' #' # Look at TAPAS binary segmentation from applying the TAPAS threshold
#' oro.nifti::image(test_subject_prediction2[[1]]$tapas_binary_mask)
#' # Look at group threshold binary segmentation from applying the group threshold
#' oro.nifti::image(test_subject_prediction2[[1]]$group_binary_threshold)
#'
#' # You can compare with subject 2 by replacing [[1]] with [[2]]
#' }

tapas_predict_par <- function(cores = 1,
                              pmap,
                              subject_id,
                              model,
                              clamp = TRUE,
                              k = 0,
                              ret = FALSE,
                              outfile = NULL,
                              verbose = TRUE){

  if(verbose == TRUE){
    base::message('# Validating parameter inputs.')
  }

  # Check that verbose is TRUE or FALSE
  if(base::is.logical(verbose) == FALSE){
    base::stop('# ERROR: verbose must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }

  # Check that ret is TRUE or FALSE
  if(base::is.logical(ret) == FALSE){
    base::stop('# ERROR: ret must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }

  # Check that files exists for pmap
  if(base::is.list(pmap) == FALSE){
    if(base::any(base::file.exists(pmap)) == FALSE){
      base::stop('# ERROR: At least one pmap file path does not exist.')
    }
  }

  # Check that outfile is character
  if(base::any(base::is.character(outfile)) == FALSE & base::is.null(outfile) == FALSE){
    base::stop('# ERROR: At least one outfile is not character. Must be character or NULL.')
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
        base::stop('# ERROR: outfile must have .rds or .RData extension.')
      }
    }

    # Return the list and save to the outfile
    if(ret == TRUE & base::is.null(outfile) == FALSE){
      if(stringr::str_detect(outfile[[i]], '.rds') == TRUE){
        base::saveRDS(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.RData') == TRUE){
        base::save(subject_data, file = outfile[[i]])
      } else if(stringr::str_detect(outfile[[i]], '.rds|.RData') == FALSE){
        base::stop('# ERROR: outfile must have .rds or .RData extension.')
      }
      base::return(subject_data)
    }

    # Return the list and do not save outfile
    if(ret == TRUE & base::is.null(outfile) == TRUE){
      base::return(subject_data)
    }


  }

  if(verbose == TRUE){
    base::message('# Running tapas_predict in parallel.')
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
