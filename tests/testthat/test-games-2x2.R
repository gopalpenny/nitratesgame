# test-games-2x2

# get_2x2_ggplot
payouts_i <- get_2x2_payouts(3, 3, Cs = 2, Cd = 3, T)
payouts_ii <- get_2x2_payouts(3, 1, Cs = 2, Cd = 3, T)

weights <- c(0.25, 0.75)
weighted_payouts_00_100 <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = c(0,1))
weighted_payouts_25_75 <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = c(0.25,0.75))
weighted_payouts_50_50 <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = c(0.5,0.5))
weighted_payouts_75_25 <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = c(0.75,0.25))
weighted_payouts_100_00 <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = c(1,0))
# weighted_payouts_50_50 %>% ggp::print_data_frame_for_entry(single_line = T)

weighted_payouts_00_100_output <- tibble::tibble(A=c(0, 0, 1, 1), B=c(0, 1, 0, 1), tA=c("3 (0), 3 (1)", "3 (0), 3 (1)", "3 (0), 3 (1)", "3 (0), 3 (1)"), tB=c("3 (0), 1 (1)", "3 (0), 1 (1)", "3 (0), 1 (1)", "3 (0), 1 (1)"), Cs_A=c(0, 0, -2, -2), Cs_B=c(0, -2, 0, -2), Cd_A=c(0, 0, 0, 0), Cd_B=c(-3, -3, 0, 0), UA=c(5, 5, 3, 3), UB=c(2, 0, 5, 3), payouts=c("5, 2", "5, 0", "3, 5", "3, 3"))
weighted_payouts_25_75_output <- tibble::tibble(A=c(0, 0, 1, 1), B=c(0, 1, 0, 1), tA=c("3 (0.25), 3 (0.75)", "3 (0.25), 3 (0.75)", "3 (0.25), 3 (0.75)", "3 (0.25), 3 (0.75)"), tB=c("3 (0.25), 1 (0.75)", "3 (0.25), 1 (0.75)", "3 (0.25), 1 (0.75)", "3 (0.25), 1 (0.75)"), Cs_A=c(0, 0, -2, -2), Cs_B=c(0, -2, 0, -2), Cd_A=c(-0.75, 0, -0.75, 0), Cd_B=c(-3, -3, 0, 0), UA=c(4.25, 5, 2.25, 3), UB=c(2, 0, 5, 3), payouts=c("4.25, 2", "5, 0", "2.25, 5", "3, 3"))
weighted_payouts_50_50_output <- tibble::tibble(A=c(0, 0, 1, 1), B=c(0, 1, 0, 1), tA=c("3 (0.5), 3 (0.5)", "3 (0.5), 3 (0.5)", "3 (0.5), 3 (0.5)", "3 (0.5), 3 (0.5)"), tB=c("3 (0.5), 1 (0.5)", "3 (0.5), 1 (0.5)", "3 (0.5), 1 (0.5)", "3 (0.5), 1 (0.5)"), Cs_A=c(0, 0, -2, -2), Cs_B=c(0, -2, 0, -2), Cd_A=c(-1.5, 0, -1.5, 0), Cd_B=c(-3, -3, 0, 0), UA=c(3.5, 5, 1.5, 3), UB=c(2, 0, 5, 3), payouts=c("3.5, 2", "5, 0", "1.5, 5", "3, 3"))
weighted_payouts_75_25_output <- tibble::tibble(A=c(0, 0, 1, 1), B=c(0, 1, 0, 1), tA=c("3 (0.75), 3 (0.25)", "3 (0.75), 3 (0.25)", "3 (0.75), 3 (0.25)", "3 (0.75), 3 (0.25)"), tB=c("3 (0.75), 1 (0.25)", "3 (0.75), 1 (0.25)", "3 (0.75), 1 (0.25)", "3 (0.75), 1 (0.25)"), Cs_A=c(0, 0, -2, -2), Cs_B=c(0, -2, 0, -2), Cd_A=c(-2.25, 0, -2.25, 0), Cd_B=c(-3, -3, 0, 0), UA=c(2.75, 5, 0.75, 3), UB=c(2, 0, 5, 3), payouts=c("2.75, 2", "5, 0", "0.75, 5", "3, 3"))
weighted_payouts_100_00_output <- tibble::tibble(A=c(0, 0, 1, 1), B=c(0, 1, 0, 1), tA=c("3 (1), 3 (0)", "3 (1), 3 (0)", "3 (1), 3 (0)", "3 (1), 3 (0)"), tB=c("3 (1), 1 (0)", "3 (1), 1 (0)", "3 (1), 1 (0)", "3 (1), 1 (0)"), Cs_A=c(0, 0, -2, -2), Cs_B=c(0, -2, 0, -2), Cd_A=c(-3, 0, -3, 0), Cd_B=c(-3, -3, 0, 0), UA=c(2, 5, 0, 3), UB=c(2, 0, 5, 3), payouts=c("2, 2", "5, 0", "0, 5", "3, 3"))

test_that("get_2x2_weighted_payouts works for multiple probabilities", {
  expect_equal(weighted_payouts_00_100, weighted_payouts_00_100_output)
  expect_equal(weighted_payouts_25_75, weighted_payouts_25_75_output)
  expect_equal(weighted_payouts_50_50, weighted_payouts_50_50_output)
  expect_equal(weighted_payouts_75_25, weighted_payouts_75_25_output)
  expect_equal(weighted_payouts_100_00, weighted_payouts_100_00_output)
})


# payouts_gg <- get_2x2_payouts(3, 3, Cs = 1, Cd = 2, TRUE)

test_that("get_2x2_ggplot returns ggplot object",{
  expect_equal(class(get_2x2_ggplot(payouts_i)), c("gg","ggplot"))
  expect_equal(class(get_2x2_ggplot(payouts_i, TRUE)), c("gg","ggplot"))
})


payouts_iii <- get_2x2_payouts(3, 3, Cs = 2, Cd = 3)
payouts_iv <- get_2x2_payouts(3, 1, Cs = 2, Cd = 3, TRUE)

weights <- c(0.5, 0.5)
weighted_payouts <- get_2x2_weighted_payouts(list(payouts_iii, payouts_iv), weights = weights)

test_that("get_2x2_game_solutions works for NE, FB, type, dilemma",{
  expect_equal(get_2x2_game_solutions(weighted_payouts, "NE"),"A0B0")
  expect_equal(get_2x2_game_solutions(weighted_payouts, "FB"),"A1B0")
  expect_equal(get_2x2_game_solutions(weighted_payouts, "type"),"Dl-Pd")
  expect_equal(get_2x2_game_solutions(weighted_payouts, "dilemma"),"social dilemma")
})
