#' Make a Scatterplot of Subject-Level Thresholds with Marginal Histogram
#' @description Code to help visualize the subject-specific thresholds selected
#'   using both a scatter plot of the subject-specific best threshold from
#'   training data on the subject ID with a marginal histogram of the
#'   subject-specific thresholds. We call this plot a "scattergram". This plot
#'   will help users re-fine the TAPAS threshold grid applied in `tapas_data`.
#' @param tapas_model The object returned from `tapas_train`.
#'
#' @return An object of class `ggExtraPlot`. This object can be printed to show
#'   the plots or saved using any of the typical image-saving functions (for
#'   example, using `png()` or `pdf()`).
#' @export
#' @importFrom dplyr arrange mutate n
#' @importFrom ggplot2 aes element_text geom_hline geom_point ggplot labs scale_x_continuous theme theme_minimal
#' @importFrom ggExtra ggMarginal
#' @importFrom magrittr "%>%"
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
#' tapas_model = tapas_train(data = data,
#'                           dsc_cutoff = 0.03,
#'                           verbose = TRUE)
#' # Make scatter plot with marginal histogram of subject-specific thresholds
#' make_scattergram(tapas_model = tapas_model)
#' }

make_scattergram <- function(tapas_model){
  # Add a dummy column to plot as x
  data = tapas_model$train_data %>%
    dplyr::mutate(n = 1:dplyr::n()) %>%
    dplyr::arrange(threshold)

  # Threshold scatter plot for subject-level threshold selected
  scatter = ggplot2::ggplot(data, ggplot2::aes(x = n, y = threshold)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(breaks = data$n,
                                label = data$subject_id) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::geom_hline(yintercept = tapas_model$group_threshold, color = 'red', linetype = 'dashed') +
    ggplot2::labs(y = 'Threshold', x = 'Subject ID',
         caption = paste0("Group Threshold (dashed line) = ", tapas_model$group_threshold))
  # Add marginal histogram for thresholds
  scattergram = ggExtra::ggMarginal(scatter, margins = "y", type = "histogram", size = 3)

  return(scattergram)
}
