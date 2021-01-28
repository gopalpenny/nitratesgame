## code to prepare `game_2x2_structures` dataset goes here

game_2x2_structures <- tibble::tribble(~order, ~name, ~abbrev, ~r00, ~rself, ~ropp, ~r11,
                                       1, "Concord", "Nc", 1, 2, 3, 4,
                                       2, "Harmony", "Ha", 1, 3, 2, 4,
                                       3, "Peace", "Pc", 2, 3, 1, 4,
                                       4, "Coordination", "Co", 3, 2, 1, 4,
                                       5, "Assurance", "As", 3, 1, 2, 4,
                                       6, "Stag Hunt", "Sh", 2, 1, 3, 4,
                                       7, "Prisoner's Dilemma", "Pd", 2, 1, 4, 3,
                                       8, "Deadlock", "Dl", 3, 1, 4, 2,
                                       9, "Compromise", "Cm", 3, 2, 4, 1,
                                       10, "Hero", "Hr", 2, 3, 4, 1,
                                       11, "Battle", "Ba", 1, 3, 4, 2,
                                       12, "Chicken", "Ch", 1, 2, 4, 3,
                                       0, "Mid Battle", "Mb", 1, 3, 4, 3,
                                       0, "Mid Compromise", "Mc", 3, 3, 4, 1,
                                       0, "Midlock", "Mk", 3, 1, 4, 3,
                                       0, "Mid Hunt", "Mu", 3, 1, 3, 4,
                                       0, "Mid Peace", "Mp", 3, 3, 1, 4,
                                       0, "Mid Harmony", "Mh", 1, 3, 3, 4)

usethis::use_data(game_2x2_structures, overwrite = TRUE)
