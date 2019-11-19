#' Make a Scatterplot of Subject-Level Thresholds with Marginal Histogram
#' @description Code to help visualize the subject-specific thresholds selected
#'   using both a scatter plot of the subject-specific best threshold from
#'   training data on the subject ID with a marginal histogram of the
#'   subject-specific thresholds. This function will help users re-fine the
#'   TAPAS threshold grid applied in `tapas_data`.
#' @param tapas_model The object returned from `tapas_train`.
#'
#' @return An object of class `ggExtraPlot`. This object can be printed to show
#'   the plots or saved using any of the typical image-saving functions (for
#'   example, using `png()`` or `pdf()``).
#' @export
#' @importFrom dplyr arrange mutate n
#' @importFrom ggplot2 aes element_text geom_hline geom_point ggplot labs scale_x_continuous theme
#' @importFrom ggExtra ggMarginal
#' @examples #TODO

make_scattergrams<- function(tapas_model){
  # Add a dummy column to plot as x
  data = tapas_model$train_data %>%
    dplyr::mutate(n = 1:dplyr::n()) %>%
    dplyr::arrange(threshold)

  # Threshold scatter plot for subject-level threshold selected
  scatter = ggplot2::ggplot(tapas_model$train_data, ggplot2::aes(x = n, y = threshold)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = tapas_model$train_data$n,
                                label = tapas_model$train_data$subject_id) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::geom_hline(yintercept = tapas_model$group_threshold, color = 'red', linetype = 'dashed') +
    ggplot2::labs(y = 'Threshold', x = 'Subject ID',
         caption = paste0("Group Threshold (dashed line) = ", tapas_model$group_threshold))
  # Add marginal histogram for thresholds
  scattergram = ggExtra::ggMarginal(scatter, margins = "y", type = "histogram", size = 3)

  return(scattergram)
}
