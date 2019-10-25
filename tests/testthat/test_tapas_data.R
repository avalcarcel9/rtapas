###########################
# Alessandra Valcarcel
# Tests for rtapas::train_data
# Created: March 6, 2019
# Updated: March 6, 2019
###########################

testthat::context("Test that tapas_data produces correct output compared to previous sample data runs.")

testthat::test_that("Test tapas_data run on first subject sample data matches original validated value.", {

  tmp <- tempfile()

  # Load saved data
  gs1 = oro.nifti::nifti(gs1)
  pmap1 = oro.nifti::nifti(pmap1)
  brain_mask = oro.nifti::nifti(brain_mask)

  train_data = rtapas::tapas_data(
    thresholds = seq(from = 0, to = 1, by = grid),
    pmap = pmap1,
    gold_standard = gs1,
    mask = brain_mask,
    k = 0,
    subject_id = 'subject_1',
    verbose = FALSE)

  # The first run always succeeds
  expect_known_output(train_data, tmp, print = TRUE, update = FALSE)

  # Subsequent runs will suceed only if the file is unchanged
  # This will succeed:
  expect_known_output(train_data, tmp, print = TRUE, update = FALSE)
})
