###########################
# Alessandra Valcarcel
# Tests for rtapas::tapas_train
# Created: March 6, 2019
# Updated: March 6, 2019
###########################

testthat::context("Test that tapas_train produces correct output compared to previous sample data runs.")

testthat::test_that("Test tapas_train run on sample training data matches original validated value.", {

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

  # The first run always succeeds
  expect_known_output(tapas_model, tmp, print = TRUE, update = FALSE)

  # Subsequent runs will suceed only if the file is unchanged
  # This will succeed:
  expect_known_output(tapas_model, tmp, print = TRUE, update = FALSE)
})
