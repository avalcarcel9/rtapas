###########################
# Alessandra Valcarcel
# Tests for rtapas::tapas_predict_par
# Created: March 6, 2019
# Updated: March 6, 2019
###########################

testthat::context("Test that tapas_predict_par produces correct output compared to previous sample data runs.")

testthat::test_that("Test tapas_predict_par run on sample training data matches original validated value.", {

  tmp <- tempfile()

  # Run tapas_data_par function
  # You can also use the tapas_data function and generate each subjects data
  data = rtapas::tapas_data_par(
    cores = 1,
    thresholds = seq(from = 0, to = 1, by = grid),
    pmap = train_probability_maps,
    gold_standard = train_gold_standard_masks,
    mask = train_brain_masks,
    k = 0,
    subject_id = train_ids,
    ret = TRUE,
    outfile = NULL,
    verbose = FALSE)

  # We can now implement the train_tapas function using the data from tapas_data_par
  tapas_model = rtapas::tapas_train(data = data,
                                    dsc_cutoff = 0.03,
                                    verbose = TRUE)

  # Load test data
  pmap11 = list(oro.nifti::nifti(pmap11))
  brain_mask = list(oro.nifti::nifti(brain_mask))

  # Predict for subject 11
  subj11_par = rtapas::tapas_predict_par(cores = 1,
                                         pmap = pmap11,
                                         subject_id = c('subject_11'),
                                         model = tapas_model,
                                         clamp = TRUE,
                                         k = 0,
                                         ret = TRUE,
                                         outfile = NULL,
                                         verbose = FALSE)

  # The first run always succeeds
  expect_known_output(subj11_par, tmp, print = TRUE, update = FALSE)

  # Subsequent runs will suceed only if the file is unchanged
  # This will succeed:
  expect_known_output(subj11_par, tmp, print = TRUE, update = FALSE)
})
