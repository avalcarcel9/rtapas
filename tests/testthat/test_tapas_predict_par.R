###########################
# Alessandra Valcarcel
# Tests for rtapas::tapas_predict_par
# Created: March 6, 2019
# Updated: March 6, 2019
###########################

testthat::context("Test that tapas_predict_par produces correct output compared to previous sample data runs.")

testthat::test_that("Test tapas_predict_par run on sample training data matches original validated value.", {

  tmp <- tempfile()

  # Data is provided in the rtapas package as arrays. Below we will convert them to nifti objects.
  # Create a list of gold standard manual segmentation
  train_gold_standard_masks = list(gs1 = gs1,
                                   gs2 = gs2,
                                   gs3 = gs3,
                                   gs4 = gs4,
                                   gs5 = gs5,
                                   gs6 = gs6,
                                   gs7 = gs7,
                                   gs8 = gs8,
                                   gs9 = gs9,
                                   gs10 = gs10)
  # Convert the gold standard masks to nifti objects
  train_gold_standard_masks = lapply(train_gold_standard_masks, oro.nifti::nifti)

  # Make a list of the training probability maps
  train_probability_maps = list(pmap1 = pmap1,
                                pmap2 = pmap2,
                                pmap3 = pmap3,
                                pmap4 = pmap4,
                                pmap5 = pmap5,
                                pmap6 = pmap6,
                                pmap7 = pmap7,
                                pmap8 = pmap8,
                                pmap9 = pmap9,
                                pmap10 = pmap10)

  # Convert the probability maps to nifti objects
  train_probability_maps = lapply(train_probability_maps, oro.nifti::nifti)
  # Make a list of the brain masks
  train_brain_masks = list(brain_mask1 = brain_mask,
                           brain_mask2 = brain_mask,
                           brain_mask3 = brain_mask,
                           brain_mask4 = brain_mask,
                           brain_mask5 = brain_mask,
                           brain_mask6 = brain_mask,
                           brain_mask7 = brain_mask,
                           brain_mask8 = brain_mask,
                           brain_mask9 = brain_mask,
                           brain_mask10 = brain_mask)

  # Convert the brain masks to nifti objects
  train_brain_masks = lapply(train_brain_masks, oro.nifti::nifti)

  # Specify training IDs
  train_ids = paste0('subject_', 1:length(train_gold_standard_masks))

  # Run tapas_data_par function
  # You can also use the tapas_data function and generate each subjects data
  data = rtapas::tapas_data_par(cores = 1,
                                thresholds = seq(from = 0, to = 1, by = 0.01),
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
