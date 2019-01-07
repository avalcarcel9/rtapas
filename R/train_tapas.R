#' @title TAPAS Model Training
#' @description  This function trains the TAPAS model as a \code{tibble} or \code{data.frame}
#' produced by an element from the output of the function \code{\link{gen_tapas_data}}.
#' @param data Data resulting from \code{\link{gen_tapas_data}}. May be in a binded
#' \code{tibble} or \code{data.frame} from all subjects to include for
#' training or a \code{list} with all subject data in unique elements.
#' @param dsc_cutoff is \code{0.03} by default. Must be a single value between 0 and 1.
#' This is the Sørensen's–Dice coefficient (DSC) value that will be used as a cutoff.
#' Only subjects with Sørensen's–Dice coefficient (DSC) greater than or equal to \code{dsc_cutoff}
#' will be included in training the TAPAS model.
#' @param verbose is \code{TRUE} by default. \code{TRUE} returns messages throughout the generating data
#' function. \code{FALSE} will silence comment returns.
#' @export
#' @importFrom dplyr bind_rows filter group_by inner_join ungroup mutate select slice summarize
#' @importFrom gtools inv.logit logit
#' @importFrom magrittr "%>%"
#' @importFrom mgcv gam predict.gam s
#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom tibble tibble is_tibble
#' @return A \code{list} with the TAPAS model (\code{tapas_model}) in the first element of the list of class \code{gam} and the
#' second element is a \code{tibble} with the clamp volume and threshold information (\code{clamp_data}).
#' @examples \dontrun{
#' # Put gen_tapas_data function here first then run train_tapas. Fix later.
#' train_tapas(data = data, dsc_cutoff = 0.03, verbose = TRUE)
#' }

train_tapas <- function(data, dsc_cutoff = 0.03, verbose = TRUE){

  # Check that verbose is TRUE or FALSE
  if(is.logical(verbose) == FALSE){
    base::stop('# verbose must be logical TRUE to return comments throughout the function or FALSE to silence comments.')
  }

  # Check that dsc_cutoff is between 0 and 1
  if(dsc_cutoff < 0 | dsc_cutoff > 1){
    base::stop('# dsc_cutoff must be a single value between 0 and 1.')
  }

  if(verbose == TRUE){
    base::message('# Validating data input.')
  }

  # Create full subject tibble by binding the list rows or verify the data provided is a data.frame or tibble
  if(base::is.list(data) == TRUE){
    data = dplyr::bind_rows(data)
  } else if (base::is.data.frame(data) == FALSE & tibble::is_tibble(data) == FALSE){
    base::stop('# data must be a list, data.frame, or tibble of stacked subject data objects from rtapas::gen_tapas_data(). \n
          # The rtapas::gen_tapas_data() function returns a tibble by default.')
  }

  if(verbose == TRUE){
    base::message('# Calculating group and subject-specific threshold and volume values.')
  }

  # Calculate subject level threshold that produces maximum DSC
  subject_thresholds = data %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::slice(which.max(dsc)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$volume)

  # Calculate the threshold that maximizes group level average DSC
  group_threshold = data %>%
    dplyr::group_by(.data$threshold) %>%
    dplyr::summarize(mean_dsc = mean(dsc)) %>%
    dplyr::slice(base::which.max(.data$mean_dsc)) %>%
    dplyr::select(.data$threshold)

  # Obtain the group level volume from using the group_threshold
  group_volumes = data %>%
    dplyr::group_by(.data$subject_id) %>%
    dplyr::filter(.data$threshold == group_threshold$threshold) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dsc, -.data$threshold)

  # Merge the group volume with the best thresholds
  ## This contains the subject specific threshold that maximized DSC
  ## The dsc value produced using the subject specific threshold
  ## The volume produced using the group threshold
  ## The unique subject_id
  data = dplyr::inner_join(x = subject_thresholds, y = group_volumes,
                           by = c(".data$subject_id" = ".data$subject_id"))

  # Check for subjects with DSC less than the dsc_cutoff
  if(base::any(data$dsc < dsc_cutoff)){

    if(verbose == TRUE){
      base::message('# Poor DSC detected excluding subject(s) from training the TAPAS model.')
    }

    # Remove subjects from training with poor DSC
    data = data %>%
      dplyr::filter(dsc >= dsc_cutoff)
  }

  # logit transformation cannot handle 0 or 1 exact values so add a check to make these values
  # e^(-16) or  0.999999999 so logit will not error
  if(base::any(data$threshold == 0) | base::any(data$threshold == 1)){
    data = data %>%
      dplyr::mutate(threshold = base::replace(.data$threshold, .data$threshold == 0, exp(-16)),
                    threshold = base::replace(.data$threshold, .data$threshold == 1, 0.999999999))
  }

  if(verbose == TRUE){
    base::message('# Fitting the TAPAS model.')
  }

  # Fit the TAPAS model
  tapas_model = mgcv::gam(formula = gtools::logit(threshold) ~ mgcv::s(volume),
                          data = data)

  if(verbose == TRUE){
    base::message('# Calculating lower and upper bound clamps.')
  }

  # Based on the training data obtain the volume associated with the 10th and 90th percentile
  # Use these volumes to predict a threshold
  clamp_data = tibble::tibble(volume = c(stats::quantile(data$volume, .1), stats::quantile(data$volume, .9))) %>%
    dplyr::mutate(pred_threshold = gtools::inv.logit(mgcv::predict.gam(tapas_model, .data$., type = "response")),
                  bound = c('lower', 'upper')) %>%
    dplyr::select(.data$bound, .data$volume, .data$pred_threshold)

  base::return(base::list(tapas_model = tapas_model, group_threshold = group_threshold$threshold, clamp_data = clamp_data))

}
