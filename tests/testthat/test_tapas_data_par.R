###########################
# Alessandra Valcarcel
# Tests for rtapas::tapas_data_par
# Created: March 6, 2019
# Updated: March 6, 2019
###########################

testthat::context("Test that tapas_data_par produces correct output compared to previous sample data runs.")

testthat::test_that("Test tapas_data_par run on first subject sample data matches original validated value.", {

  tmp <- tempfile()

  # Load saved data
  gs1 = oro.nifti::nifti(gs1)
  pmap1 = oro.nifti::nifti(pmap1)
  brain_mask = oro.nifti::nifti(brain_mask)

  train_data_par = rtapas::tapas_data_par(cores = 1,
                                          thresholds = seq(from = 0, to = 1, by = 0.01),
                                          pmap = list(pmap1),
                                          gold_standard = list(gs1),
                                          mask = list(brain_mask),
                                          k = 0,
                                          subject_id = list('subject_1'),
                                          verbose = FALSE)

  # The first run always succeeds
  expect_known_output(train_data_par, tmp, print = TRUE, update = FALSE)

  # Subsequent runs will suceed only if the file is unchanged
  # This will succeed:
  expect_known_output(train_data_par, tmp, print = TRUE, update = FALSE)
})
