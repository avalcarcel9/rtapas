###########################
# Alessandra Valcarcel
# Tests to make sure the parallel wrappers for tapas_data and
# tapas_predict (tapas_data_par and tapas_predict_par) result in the same thing
# Created: March 6, 2019
# Updated: March 6, 2019
###########################

testthat::context("1. Test to ensure that tapas_data and tapas_data_par produce the same output.
                  2. Test to ensure that tapas_predict and tapas_predict_par produce the same output.")

testthat::test_that("1. Test tapas_data and tapas_data_par produce the same output.", {

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

  # Make data into a list for parallel version
  gs1 = list(gs1)
  pmap1 = list(pmap1)
  brain_mask = list(brain_mask)

  train_data_par = tapas_data_par(
    cores = 1,
    thresholds = seq(from = 0, to = 1, by = grid),
    pmap = pmap1,
    gold_standard = gs1,
    mask = brain_mask,
    k = 0,
    subject_id = list('subject_1'),
    ret = TRUE,
    outfile = NULL,
    verbose = FALSE)


  # Test for equality
  expect_equal(train_data, train_data_par)
})

testthat::test_that("2. Test tapas_predict and tapas_predict_par produce the same output.", {

  # Obtain training data
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
                                    verbose = FALSE)


  # Obtain the test subject probability maps
  test_probability_maps = list(pmap11 = pmap11,
                               pmap12 = pmap12,
                               pmap13 = pmap13,
                               pmap14 = pmap14,
                               pmap15 = pmap15)

  # Make array objects niftis
  test_probability_maps = lapply(test_probability_maps, oro.nifti::nifti)

  # Create a list of testing brain masks
  test_brain_masks = list(brain_mask11 = brain_mask,
                          brain_mask12 = brain_mask,
                          brain_mask13 = brain_mask,
                          brain_mask14 = brain_mask,
                          brain_mask15 = brain_mask)

  # Test IDS
  test_ids = paste0('subject_', (10 + 1:length(test_brain_masks)))

  # tapas_predict
  # Initialize empty data list
  predict_data = list()
  for(i in 1:length(test_ids)){
    # Obtain subject level data
    subject_data = rtapas::tapas_predict(pmap = test_probability_maps[[i]],
                                         model = tapas_model,
                                         clamp = TRUE,
                                         k = 0,
                                         verbose = FALSE)
    # Add list element with subject ID
    predict_data[[i]] = list(subject_id = test_ids[[i]],
                             subject_threshold = subject_data$subject_threshold,
                             tapas_binary_mask = subject_data$tapas_binary_mask,
                             group_binary_mask = subject_data$group_binary_mask)
  }

  predict_data_par = rtapas::tapas_predict_par(cores = 1,
                                               pmap = test_probability_maps,
                                               subject_id = test_ids,
                                               model = tapas_model,
                                               k = 0,
                                               ret = TRUE,
                                               outfile = NULL,
                                               verbose = FALSE)

  # Test for equality
  expect_equal(predict_data, predict_data_par)
})
