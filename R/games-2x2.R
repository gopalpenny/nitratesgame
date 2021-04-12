# games-2x2.R

#' Get payouts for 2x2 games
#'
#' @param tA Player A type as 1, 2, 3, or 4
#' @param tB Player B type as 1, 2, 3, or 4
#' @param Cs Cost of upgrading septic system (positive)
#' @param Cd Cost of contamination of domestic well (positive)
#' @param pos If \code{TRUE}, all payouts are shifted by Cs + Cd (prior to any rescaling)
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
#' @examples
#' payouts_pos <- get_2x2_payouts(3, 3, Cs = 1, Cd = 2, T)
#' payouts_neg <- get_2x2_payouts(3, 3, Cs = 1, Cd = 2, F)
#'
#' get_2x2_ggplot(payouts_pos, T)
get_2x2_payouts <- function(tA, tB, Cs, Cd, pos = FALSE) {
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
      UB = Cs_B + Cd_B + adj)
  return(df)
}


#' ggplot Payouts
#'
#' Generate ggplot of 2x2 payouts
#' @param payouts Tibble of payouts from get_2x2_payouts
#' @param equilibria If \code{TRUE}, highlight equilibria
#' @param nsmall Used for rounding payouts
#' @export
#' @examples
#' payouts <- get_2x2_payouts(3, 3, Cs = 1, Cd = 2, T)
#' get_2x2_ggplot(payouts)
#'
#' payouts <- get_2x2_payouts(3, 3, Cs = 2, Cd = 1, T)
#' get_2x2_ggplot(payouts, TRUE)
#'
#' payouts <- get_2x2_payouts(4, 4, Cs = 1, Cd = 2, T)
#' get_2x2_ggplot(payouts, TRUE)
get_2x2_ggplot <- function(payouts, equilibria = FALSE, nsmall = 2) {
  payouts <- payouts %>%
    dplyr::mutate(payouts = paste(round(UA,nsmall),round(UB,nsmall),sep=", "))
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
          # axis.text.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(size = 11, color = "black"),
          axis.ticks.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          # axis.text.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 11, color = "black", angle = 90, hjust = 0.5),
          axis.ticks.y = ggplot2::element_blank())
  plot_title <- paste0("A: ",payouts$tA[1],"\nB: ",payouts$tB[1])
  y_title <- paste0("A: ",payouts$tA[1])
  x_title <- paste0("\nB: ",payouts$tB[1])
  gg_payouts_prep <- gg_payouts_prep  +
    ggplot2::geom_text(data=payouts, ggplot2::aes(1-B,A,label=payouts)) +
    # ggplot2::annotate("text", x = -0.75, 1.75, label = plot_title) +
    ggplot2::scale_x_continuous(breaks = 0.5, labels = x_title, position = "top")+
    ggplot2::scale_y_continuous(breaks = 0.5, labels = y_title)
  if(equilibria) {
    payouts <- get_2x2_game_solutions(payouts)
    p_payouts_prep <- gg_payouts_prep +
      ggplot2::geom_rect(data= payouts[payouts$FB == 1,],
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
#' payouts <- get_2x2_payouts(3, 3, Cs = 1, Cd = 2, T)
#' get_2x2_ggplot(payouts)
#'
#' payouts <- get_2x2_payouts(3, 3, Cs = 2, Cd = 1, T)
#' get_2x2_ggplot(payouts)
#'
#' payouts <- get_2x2_payouts(4, 4, Cs = 1, Cd = 2, T)
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
#' @param payouts List of payouts from get_2x2_payouts
#' @param output Either "solution", "NE", "FB", or "dilemma", or "type". See Value section.
#' @details
#' This function evaluates Nash stability by checking, for each combination of
#' pure strategies, if any player is incentivized to change their strategy. The set of
#' pure strategies can be stable (NE set to 1), neutral (NE set to 0), or unstable
#' (NE set to -1). The first two (0 and 1) represent Nash equilibria. The first best
#' solution is calculated as the set of pure strategies that product the highest net
#' payout among all players (i.e., the sum for all players). A value of 1 indicates
#' the set of pure strategies is a joint optimum, and 0 indicates it is not.
#' @return
#' The \code{output} variable determines the type of output. The options are:
#' \itemize{
#' \item \code{"solution"}: a payout tibble is returned that contains additional columns NE and FB
#' columns containing the solution.
#' \item \code{"NE"}: the pure strategies where NE == 1 or 0.
#' \item \code{"FB"}: the pure strategies where FB == 1.
#' \item \code{"dilemma"}: whether the game is a "social dilemma" or "agreement".
#' \item \code{"type"}: the payout structure for each player.
#' }
#' @export
#' @examples
#' payouts <- get_2x2_payouts(3, 3, Cs = 2, Cd = 3)
#' get_2x2_game_solutions(payouts)
#'
#' # Note that this function is contained in get_2x2_ggplot, if desired
#' get_2x2_ggplot(payouts, equilibria = TRUE)
#' payouts_ii <- get_2x2_payouts(3, 1, Cs = 2, Cd = 3, T)
#'
#' weights <- c(0.5, 0.5)
#' weighted_payouts <- get_2x2_weighted_payouts(list(payouts, payouts_ii), weights = weights)
#' get_2x2_game_solutions(weighted_payouts, "NE")
#' get_2x2_game_solutions(weighted_payouts, "FB")
#' get_2x2_game_solutions(weighted_payouts, "type")
#' get_2x2_game_solutions(weighted_payouts, "dilemma")
get_2x2_game_solutions <- function(payouts, output = "solution") {
  # get_2x2_cell_NE <- function()

  payouts$NE <- sapply(1:4, get_2x2_nash_stability,payouts = payouts)
  payouts$FB <- ifelse(payouts$UA + payouts$UB == max(payouts$UA + payouts$UB), 1, 0)

  nash_strategies_vec <- payouts %>% dplyr::rowwise() %>% dplyr::filter(NE >= 0) %>%
    dplyr::mutate(pure_strategy = paste0("A",A,"B",B)) %>%
    dplyr::pull(pure_strategy)
  nash_strategies <- nash_strategies_vec %>% paste(collapse = ", ")
  fb_strategies_vec <- payouts %>% rowwise() %>% filter(FB == 1) %>%
    dplyr::mutate(pure_strategy = paste0("A",A,"B",B)) %>%
    dplyr::pull(pure_strategy)
  fb_strategies <- fb_strategies_vec %>% paste(collapse = ", ")

  if (output == "solution") {
    out_var <- payouts
  } else if (output == "NE") {
    out_var <- nash_strategies
  } else if (output == "FB") {
    out_var <- fb_strategies
  } else if (output == "dilemma") {
    out_var <- dplyr::case_when(
      all(nash_strategies_vec %in% fb_strategies_vec) ~ "agreement",
      !all(nash_strategies_vec %in% fb_strategies_vec) ~ "social dilemma",
      TRUE ~ "something unexpected - investigate further"
    )
  } else if (output == "type") {
    # Get A game type

    # get ranks
    A_ranks <- payouts %>% dplyr::arrange(A, B) %>% dplyr::pull(UA) %>% rank(ties.method = "min") %>%
      tibble::tibble(rank = ., cell = c("r00", "ropp", "rself", "r11")) %>%
      tidyr::pivot_wider(names_from = "cell", values_from = "rank")

    # get row from nitratesgame::game_2x2_structures
    A_gametype_df <- A_ranks %>%
      dplyr::inner_join(game_2x2_structures, by = c("r00", "rself", "ropp", "r11"))

    # if row exists, take game abbrev
    if (nrow(A_gametype_df) == 1) {
      A_gametype <- A_gametype_df$abbrev

    # if row does not exist, flip the ordinal payouts vector and try again
    } else { # check to see if game parameters are reversed
      A_gametype_df <- A_ranks %>% dplyr::rename(r00=r11, rself = ropp, ropp = rself, r11 = r00) %>%
        dplyr::inner_join(game_2x2_structures, by = c("r00", "rself", "ropp", "r11"))
      if (nrow(A_gametype_df) == 1) {
        A_gametype <- paste0("r",A_gametype_df$abbrev)

        # if still can't find row, return
      } else {
        A_gametype <- "X"
      }
    }

    # Get B game type
    B_ranks <- payouts %>% dplyr::arrange(B, A) %>% dplyr::pull(UB) %>% rank(ties.method = "min") %>%
      tibble::tibble(rank = ., cell = c("r00", "ropp", "rself", "r11")) %>%
      tidyr::pivot_wider(names_from = "cell", values_from = "rank")
    B_gametype_df <- B_ranks %>%
      dplyr::inner_join(game_2x2_structures, by = c("r00", "rself", "ropp", "r11"))
    if (nrow(B_gametype_df) == 1) {
      B_gametype <- B_gametype_df$abbrev
    } else { # check to see if game parameters are reversed
      B_gametype_df <- B_ranks %>% dplyr::rename(r00=r11, rself = ropp, ropp = rself, r11 = r00) %>%
        dplyr::inner_join(game_2x2_structures, by = c("r00", "rself", "ropp", "r11"))
      if (nrow(B_gametype_df) == 1) {
        B_gametype <- paste0("r",B_gametype_df$abbrev)
      } else {
        B_gametype <- "X"
      }
    }
    out_var <- paste0(A_gametype,"-",B_gametype)
  } else {
    stop("output must be one of solution, NE, FB, or type. Instead it is: ",output,".\n")
  }

  return(out_var)
}

#' Get 2x2 Nash stability
#'
#' Get 2x2 Nash equilibrium and first best
#' @param payouts List of payouts from get_2x2_payouts
#' @param i Index at which to evaluate Nash stability
#' @description
#' This function evaluates Nash stability by checking if either player
#' would benefit by switching their action.
#' @return
#' Returns a value of 1 (Nash equilibrium), 0 (stable), or -1 (not NE)
#' @examples
#' payouts <- get_2x2_payouts(3, 3, Cs = 2, Cd = 3, T)
#' payouts <- get_2x2_payouts(3, 3, Cs = 2, Cd = 3)
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
#' @param payouts_list List of payouts from get_2x2_payouts
#' @param weights Vector used to weight payouts, in order of \code{...}
#' @export
#' @examples
#' payouts_i <- get_2x2_payouts(3, 3, Cs = 2, Cd = 3, T)
#' payouts_ii <- get_2x2_payouts(3, 1, Cs = 2, Cd = 3, T)
#'
#' weights <- c(0.5, 0.5)
#' weighted_payouts <- get_2x2_weighted_payouts(list(payouts_i, payouts_ii), weights = weights)
#'
#' library(gridExtra)
#' grid.arrange(get_2x2_ggplot(payouts_i, equilibria = TRUE),
#'   get_2x2_ggplot(payouts_ii, equilibria = TRUE),
#'   get_2x2_ggplot(weighted_payouts, equilibria = TRUE))
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
