#' @title Generates The TAPAS Training Data in Parallel
#' @description This function wraps [tapas_data()] to run in parallel. This function creates
#' the training vectors for all subjects from a probability map,
#' a gold standard mask (normally a manual segmentation), and a brain mask. For a grid of thresholds provided
#' and applied to the probability map the function calculates Sørensen's–Dice coefficient (DSC) between the automatic
#' image and the gold standard image. The function also calculates the volume associated with thresholding
#' at each respective threshold.
#' @param cores The number of cores to use. This argument controls at most how many child processes will
#' be run simultaneously. The default is set to 1.
#' @param thresholds A `vector` of thresholds to apply to the probability maps. The default
#' `vector` applied is 0 to 1 by 0.01 increments. Threshold values must be
#' between 0 and 1.
#' @param pmap A `vector` of `character` file paths to probability map images or a
#' `list` object with elements of class `nifti`.
#' @param gold_standard A `vector` of `character` file paths to gold standard images (normally
#' a manual segmentation) or a `list` object with elements of class `nifti`. The gold
#' standard segmentation is used to compare the thresholded probability map image using Sørensen's–Dice
#' coefficient (DSC).
#' @param mask A `vector` of `character` file paths to brain mask images or a `list` object
#' with elements of class `nifti`.
#' @param k The minimum number of voxels for a cluster/component.
#' Segmentation clusters of size less than k are removed from the mask, volume estimation, and the
#' Sørensen's–Dice coefficient (DSC) calculation.
#' @param subject_id A `vector` of subject IDs of class `character`. By default this is set to `NULL` but users must
#' provide an ID `vector`.
#' @param ret A `logical` argument set to `TRUE` by default. Returns the `tibble` objects from the
#' function as a `list` in the local R environment. If `FALSE` then `outfile` must be specified so subject data is
#' saved.
#' @param outfile Is set to `NULL` by default which only returns the subject-level `tibble` as a list
#' in the local R environment. To save each subject-level `tibble` as an R object
#' specify a `vector` or `list` of file paths to save with either .rds or .RData extensions included.
#' @param verbose A `logical` argument to print messages. Set to `TRUE` by default.
#' @export
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster mclapply stopCluster
#' @importFrom stringr str_detect
#' @return A `list` with the subject-level `tibble` object in each element returned from [tapas_data()] for each
#' subject. `ret` must be `TRUE` to return objects locally. To save objects a `vector` of code{outfile}
#' file paths must be provided.
#' @examples \dontrun{
#' # Data is provided in the rtapas package
#' library(oro.nifti)
#' # Data is provided in the rtapas package as arrays. Below we will convert them to nifti objects.
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
#' }

tapas_data_par <- function(cores = 1,
                           thresholds = seq(from = 0, to = 1, by = 0.01),
                           pmap,
                           gold_standard,
                           mask,
                           k = 0,
                           subject_id = NULL,
                           ret = FALSE,
                           outfile = NULL,
                           verbose = TRUE){

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

  # Check that files exists for gold_standard
  if(base::is.list(gold_standard) == FALSE){
    if(base::any(base::file.exists(gold_standard)) == FALSE){
      base::stop('# ERROR: At least one gold_standard file path does not exist.')
    }
  }

  # Check that files exists for mask
  if(base::is.list(mask) == FALSE){
    if(base::any(base::file.exists(mask)) == FALSE){
      base::stop('# ERROR: At least one mask file path does not exist.')
    }
  }

  # Check that outfile is character or NULL
  if(base::any(base::is.character(outfile)) == FALSE & base::is.null(outfile) == FALSE){
    base::stop('# ERROR: At least one outfile is not character. Must be character or NULL.')
  }

  # Check that pmap, gold_standard, mask, and subject_id are all the same length
  if(base::length(pmap) != base::length(gold_standard) | base::length(pmap) != base::length(mask) | base::length(pmap) != base::length(subject_id)){
    base::stop("# ERROR: pmap, gold_standard, mask, or subject_id are not of equal lengths.")
  }

  data_parallel <- function(i){
    subject_data = tapas_data(thresholds = thresholds,
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
        base::stop('# ERROR: outfile must have .rds or .RData extension.')
      }
    }
    # Return the tibble and save to the outfile
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
    # Return the tibble and do not save outfile
    if(ret == TRUE & base::is.null(outfile) == TRUE){
      base::return(subject_data)
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
