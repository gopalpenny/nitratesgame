## code to prepare `hh_grid_example` dataset goes here


density <- units::set_units(0.25,"1/acre")
area <- units::set_units(64,"acre")
hh_grid_example <- nitratesgame::get_hh_grid(density,area)
hh_grid_example$z1 <- units::set_units(10,"ft")
hh_grid_example$z2 <- units::set_units(18,"ft")
hh_grid_example$rs <- units::set_units(10,"ft")
hh_grid_example$id <- 1:nrow(hh_grid_example)

usethis::use_data(hh_grid_example, overwrite = TRUE)
