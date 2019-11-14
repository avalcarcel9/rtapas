###########################
# Alessandra Valcarcel
# Tests for adjacent threshold detection in training
# Created: November 14, 2019
# Updated: November 14, 2019
###########################

# Non-adjacent thresholds are detected and error out
testthat::test_that("Test non-adjacent tied thresholds during tapas_train errors.", {

  # Non-adjacent ties
  data = tibble::tibble(threshold = rep(c(.1,.2, .3), 50),
                        dsc = runif(150, min = 0, max = 1),
                        volume = rnorm(150, 25, 20),
                        subject_id = rep(1:50, 3)) %>%
    dplyr::arrange(subject_id, threshold)
  data$dsc[data$subject_id == 1] = c(.3, .1, .3)

  # We can now implement the train_tapas function using the data from tapas_data_par
  testthat::expect_error(rtapas::tapas_train(data = data,
                                    dsc_cutoff = 0.03,
                                    verbose = TRUE))
})

# Adjacent thresholds are detected and returns the median
testthat::test_that("Test adjacent tied thresholds during tapas_train runs and returns a message about tie.", {
# Adjacent ties
data = tibble::tibble(threshold = rep(c(.1,.2, .3), 50),
                      dsc = runif(150, min = 0, max = 1),
                      volume = rnorm(150, 25, 20),
                      subject_id = rep(1:50, 3)) %>%
  dplyr::arrange(subject_id, threshold)
data$dsc[data$subject_id == 1] = c(.3, .3, .1)

testthat::expect_message(rtapas::tapas_train(data = data,
                    dsc_cutoff = 0.03,
                    verbose = TRUE), "Subject ties are adjacent using the median value for training.")

})
