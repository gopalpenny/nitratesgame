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
gg_payouts <- function(payouts) {
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
  plot_title <- paste0("A",payouts$tA[1],", B",payouts$tB[1])
  p_payouts <- gg_payouts_prep +
    ggplot2::geom_text(data=payouts, ggplot2::aes(1-B,A,label=payouts)) +
    ggplot2::annotate("text", x = -0.75, 1.75, label = plot_title)
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
#' gg_payouts(payouts)
#'
#' payouts <- get_payouts_2x2(3, 3, Cs = 2, Cd = 1, T)
#' gg_payouts(payouts)
#'
#' payouts <- get_payouts_2x2(4, 4, Cs = 1, Cd = 2, T)
#' gg_payouts(payouts)
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
