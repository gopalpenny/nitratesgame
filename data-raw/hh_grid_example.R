## code to prepare `hh_grid_example` dataset goes here

density <- units::set_units(0.25,"1/acre")
area <- units::set_units(64,"acre")
hh_grid_example <- nitratesgame::get_hh_grid(density,area)

usethis::use_data(hh_grid_example, overwrite = TRUE)
