# games-2x2.R

#' Get payouts for 2x2 games
#'
#' @param tA Player A type as 1, 2, 3, or 4
#' @param tB Player B type as 1, 2, 3, or 4
#' @param Cs Cost of upgrading septic system (positive)
#' @param Cd Cost of contamination of domestic well (positive)
#' @export
#' @description
#' This function generates a 2x2 payout matrix for players A and B
#' given their player type, decision space (upgrade septic or not),
#' and associated costs.
#' @return
#' The function returns a tibble containing the following columns:
#' \itemize{
#' \item A, B: player decision as either 1 (Upgrade) or 0 (Neglect)
#' \item tA, tB: Type of each player
#' \item Cs_i: Expected utility (i.e., cost) of upgrading septic system
#' \item Cd_i: Expected utility (i.e., cost) of contaminated domestic well
#' \item UA, UB: Expected utility of both costs
#' \item payouts: Payout structure for each cell for A, B
#' }
get_payouts_2x2 <- function(tA, tB, Cs, Cd, pos = FALSE) {
  if(pos) { # adj adjusts the utility by Cs + Cd to obtain positive utilities
    adj <- Cs + Cd
  } else{
    adj <- 0
  }
  # tA <- 3; tB <- 3; Cs <- 1; Cd <- 2
  C_A <- get_2x2_contamination_vector(tA, "A")
  C_B <- get_2x2_contamination_vector(tB, "B")

  df <- tidyr::crossing(A = c(1,0), B = c(1, 0)) %>% # 1 -> upgrade, 0 -> neglect
    dplyr::mutate(
      tA = tA,
      tB = tB,
      Cs_A = -A*Cs,
      Cs_B = -B*Cs,
      Cd_A = -pmax(C_A[1]*(1-A), C_B[1]*(1-B))*Cd,
      Cd_B = -pmax(C_A[2]*(1-A), C_B[2]*(1-B))*Cd,
      UA = Cs_A + Cd_A + adj,
      UB = Cs_B + Cd_B + adj,
      payouts = paste(UA,UB,sep=", "))
  return(df)
}


#' ggplot Payouts
#'
#' Generate ggplot of 2x2 payouts
#' @param payouts Tibble of payouts from get_payouts_2x2
#' @export
get_2x2_ggplot <- function(payouts, equilibria = FALSE) {
  df_lines <- tibble::tribble(~x1, ~x2, ~y1, ~y2,
                      -1, 1.5, -0.5, -0.5,
                      -1, 1.5, 0.5, 0.5,
                      -1, 1.5, 1.5, 1.5,
                      -0.5, -0.5, -0.5, 2,
                      0.5, 0.5, -0.5, 2,
                      1.5, 1.5, -0.5, 2)
  df_headers <- tibble::tribble(~label, ~x, ~y,
                        "U", -0.75, 1,
                        "N", -0.75, 0,
                        "U", 0, 1.75,
                        "N", 1, 1.75)

  gg_payouts_prep <- ggplot2::ggplot() +
    # geom_text(aes(A,B,label=payouts)) +
    ggplot2::geom_text(data=df_headers,ggplot2::aes(x,y,label=label)) +
    ggplot2::geom_segment(data=df_lines,ggplot2::aes(x1, y1, xend = x2, yend = y2)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(color=NA,fill=NA),
          panel.border = ggplot2::element_rect(color=NA,fill=NA),
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank())
  plot_title <- paste0("A: ",payouts$tA[1],"\nB: ",payouts$tB[1])
  gg_payouts_prep <- gg_payouts_prep  +
    ggplot2::geom_text(data=payouts, ggplot2::aes(1-B,A,label=payouts)) +
    ggplot2::annotate("text", x = -0.75, 1.75, label = plot_title)
  if(equilibria) {
    payouts <- get_2x2_game_solutions(payouts)
    p_payouts_prep <- gg_payouts_prep +
      ggplot2::geom_rect(data= payouts[payouts$FB,],
                         ggplot2::aes(xmin=0.5-B, xmax=1.5-B, ymin= A-0.5, ymax = A+0.5),fill='blue',alpha=0.3) +
      ggplot2::geom_rect(data= payouts[payouts$NE %in% c(0,1),],
                         ggplot2::aes(xmin=0.5-B, xmax=1.5-B, ymin= A-0.5, ymax = A+0.5),fill=NA, color = "red", size = 1.5)
  } else {
    p_payouts_prep <- gg_payouts_prep
  }

  p_payouts <- p_payouts_prep #+
  #   ggplot2::geom_text(data=payouts, ggplot2::aes(1-B,A,label=payouts)) +
  #   ggplot2::annotate("text", x = -0.75, 1.75, label = plot_title)
  # p_payouts
  return(p_payouts)
}


#' Get 2x2 contamination vector
#' @param type Player type as 1, 2, 3, or 4
#' @param player Player A or player B
#' @details
#' The player types are:
#' \enumerate{
#' \item Contaminate no one
#' \item Contaminate self
#' \item Contaminate other player
#' \item Contaminate both
#' }
#' Contamination structure C_A, C_B: vector indicating cost to each player
#' \itemize{
#' \item C_A1: c(0,0)
#' \item C_A2: c(1,0)
#' \item C_A3: c(0,1)
#' \item C_A2: c(1,1)
#' \item C_B1: c(0,0)
#' \item C_B2: c(0,1)
#' \item C_B3: c(1,0)
#' \item C_B2: c(1,1)
#' }
#' @keywords internal
#' @examples
#' payouts <- get_payouts_2x2(3, 3, Cs = 1, Cd = 2, T)
#' get_2x2_ggplot(payouts)
#'
#' payouts <- get_payouts_2x2(3, 3, Cs = 2, Cd = 1, T)
#' get_2x2_ggplot(payouts)
#'
#' payouts <- get_payouts_2x2(4, 4, Cs = 1, Cd = 2, T)
#' get_2x2_ggplot(payouts)
get_2x2_contamination_vector <- function(type, player) {
  if (player == "A") {
    C_vect <- switch(type, c(0,0), c(1,0), c(0,1), c(1,1))
  } else if (player == "B") {
    C_vect <- switch(type, c(0,0), c(0,1), c(1,0), c(1,1))
  } else {
    stop("player must be A or B")
  }

  return(C_vect)
}

#' Get 2x2 game solutions
#'
#' Get 2x2 Nash equilibrium and first best
#' @param payouts List of payouts from get_payouts_2x2
#' @description
#' This function evaluates Nash stability by checking if
#' @export
#' @examples
#' payouts <- get_payouts_2x2(3, 3, Cs = 2, Cd = 3)
#' get_2x2_game_solutions(payouts)
#' get_2x2_ggplot(payouts, equilibria = TRUE)
#' payouts_ii <- get_payouts_2x2(3, 1, Cs = 2, Cd = 3, T)
#'
#' weights <- c(0.5, 0.5)
#' weighted_payouts <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = weights)
#' get_2x2_ggplot(weighted_payouts)
get_2x2_game_solutions <- function(payouts) {
  # get_2x2_cell_NE <- function()

  payouts$NE <- sapply(1:4, get_2x2_nash_stability,payouts = payouts)
  payouts$FB <- payouts$UA + payouts$UB == max(payouts$UA + payouts$UB)

  return(payouts)
}

#' Get 2x2 Nash stability
#'
#' Get 2x2 Nash equilibrium and first best
#' @param payouts List of payouts from get_payouts_2x2
#' @param i Index at which to evaluate Nash stability
#' @description
#' This function evaluates Nash stability by checking if either player
#' would benefit by switching their action.
#' @return
#' Returns a value of 1 (Nash equilibrium), 0 (stable), or -1 (not NE)
#' @examples
#' payouts <- get_payouts_2x2(3, 3, Cs = 2, Cd = 3, T)
#' payouts <- get_payouts_2x2(3, 3, Cs = 2, Cd = 3)
#' get_2x2_nash_stability(payouts, 4)
#' get_2x2_nash_stability(payouts, 1)
get_2x2_nash_stability <- function(payouts, i) {
  # get_2x2_cell_NE <- function()
  # i <- 1

  A <- payouts$A[i]
  B <- payouts$B[i]
  UA <- payouts$UA[i]
  UB <- payouts$UB[i]
  A_switch <- 1 - A
  B_switch <- 1 - B

  # perspective of A: compare UA with UA after switching A
  UA_switch <- payouts$UA[payouts$A == A_switch & payouts$B == B]
  A_stable <- sign(UA - UA_switch)

  # perspective of B: compare UB with UB after switching B
  UB_switch <- payouts$UB[payouts$A == A & payouts$B == B_switch]
  B_stable <- sign(UB - UB_switch)

  stability <- pmin(A_stable, B_stable)

  return(stability)
}

#' Get 2x2 weighted payouts
#'
#' @param payouts_list List of payouts from get_payouts_2x2
#' @param weights Vector used to weight payouts, in order of \code{...}
#' @export
#' @examples
#' payouts_i <- get_payouts_2x2(3, 3, Cs = 2, Cd = 3, T)
#' payouts_ii <- get_payouts_2x2(3, 1, Cs = 2, Cd = 3, T)
#'
#' weights <- c(0.5, 0.5)
#' weighted_payouts <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = weights)
#' get_2x2_ggplot(weighted_payouts)
get_2x2_weighted_payouts <- function(payouts_list, weights) {
  # payouts_list <- list(...)

  if (length(payouts_list) != length(weights)) {
    stop("The number of payout tibbles (",length(payouts_list),") must be the same as the length of weights (",length(weights),").")
  }
  payouts_w_weights_list <- mapply(function(li, weight) dplyr::mutate(li, weight = weight),
                              li = payouts_list, weight = weights, SIMPLIFY = FALSE)
  payouts_w_weights <- do.call(rbind,payouts_w_weights_list)

  weighted_types <- payouts_w_weights %>%
    dplyr::select(c("tA","tB","weight")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(tAx = paste0(tA," (",weight,")"),
                     tBx = paste0(tB," (",weight,")")) %>%
    dplyr::summarize(tA = paste(tAx, collapse = ", "),
                     tB = paste(tBx, collapse = ", "))

  weighted_payouts <- payouts_w_weights %>%
    dplyr::mutate(tA = weighted_types$tA,
                  tB = weighted_types$tB) %>%
    dplyr::group_by(A, B, tA, tB) %>%
    dplyr::summarize(Cs_A = weighted.mean(Cs_A, weight),
                     Cs_B = weighted.mean(Cs_B, weight),
                     Cd_A = weighted.mean(Cd_A, weight),
                     Cd_B = weighted.mean(Cd_B, weight),
                     UA = weighted.mean(UA, weight),
                     UB = weighted.mean(UB, weight), .groups = "drop") %>%
    dplyr::mutate(payouts = paste(UA,UB,sep=", "))

  return(weighted_payouts)
}
