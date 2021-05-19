# test-gw-model.R
library(units)

params_row <- tibble::tibble(
  z1 = set_units(10, "ft"),
  z2 = set_units(20, "ft"),
  area = set_units(64, "acre"),
  theta_range = list(c(0, pi/4)), # this will be unlisted in the function
  alpha_range = list(c(0, 20))) # this will be unlisted in the function

# Add varying parameters
params_df <- params_row %>%
  tidyr::crossing(density = set_units(c(0, 0.5), "1/acre"),
                  rs = set_units(c(10, 20),"ft"),
                  self_treat = c(TRUE, FALSE)) # if self_treat is TRUE, a household cannot contaminate its own well

# Get probabilities
probs <- get_contamination_probabilities(params_df)

probs_expected <- c(0.05,0,0.1,0,0.0613,0.0113,0.1226,0.0226)

test_that('get_contamination_probabilities works for simple cases', {
  expect_equal(round(probs, 4), probs_expected)
})

