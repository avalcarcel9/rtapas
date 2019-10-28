library(rtapas)

in_ci <- function() {
  nzchar(Sys.getenv("CI"))
}

grid = 0.01
if (in_ci()) {
  grid = 0.05
}


# Data is provided in the rtapas package as arrays. Below we will convert them to nifti objects.
# Create a list of gold standard manual segmentation
train_gold_standard_masks = list(gs1 = rtapas::gs1,
                                 gs2 = rtapas::gs2,
                                 gs3 = rtapas::gs3,
                                 gs4 = rtapas::gs4,
                                 gs5 = rtapas::gs5,
                                 gs6 = rtapas::gs6,
                                 gs7 = rtapas::gs7,
                                 gs8 = rtapas::gs8,
                                 gs9 = rtapas::gs9,
                                 gs10 = rtapas::gs10)
# Convert the gold standard masks to nifti objects
train_gold_standard_masks = lapply(train_gold_standard_masks, oro.nifti::nifti)

# Make a list of the training probability maps
train_probability_maps = list(pmap1 = rtapas::pmap1,
                              pmap2 = rtapas::pmap2,
                              pmap3 = rtapas::pmap3,
                              pmap4 = rtapas::pmap4,
                              pmap5 = rtapas::pmap5,
                              pmap6 = rtapas::pmap6,
                              pmap7 = rtapas::pmap7,
                              pmap8 = rtapas::pmap8,
                              pmap9 = rtapas::pmap9,
                              pmap10 = rtapas::pmap10)

# Convert the probability maps to nifti objects
train_probability_maps = lapply(train_probability_maps, oro.nifti::nifti)
# Make a list of the brain masks
train_brain_masks = list(brain_mask1 = rtapas::brain_mask,
                         brain_mask2 = rtapas::brain_mask,
                         brain_mask3 = rtapas::brain_mask,
                         brain_mask4 = rtapas::brain_mask,
                         brain_mask5 = rtapas::brain_mask,
                         brain_mask6 = rtapas::brain_mask,
                         brain_mask7 = rtapas::brain_mask,
                         brain_mask8 = rtapas::brain_mask,
                         brain_mask9 = rtapas::brain_mask,
                         brain_mask10 = rtapas::brain_mask)

# Convert the brain masks to nifti objects
train_brain_masks = lapply(train_brain_masks, oro.nifti::nifti)

# Specify training IDs
train_ids = paste0('subject_', 1:length(train_gold_standard_masks))
