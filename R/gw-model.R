# gw-model.R


# nitrate_gw_model.R

require(tidyverse)
require(units)
require(sf)
library(ggforce)

#' OLD & OUTDATED #' Get probability of contamination
#' #'
#' #' Get probability of contamination of well i by septic system j
#' #' @param df a data.frame containing the columns: rij, rs, alpha, z1, z2
#' #' @description
#' #' Works when all variables have the same unit [L] or with these variables
#' #' defined with units::set_units()
#' #' @examples
#' #' df <- data.frame(rij = seq(20,200, by = 10), rs = 10, alpha = 20, z1 = 3, z2 = 20)
#' #' df$pij <- get_pij(df)
#' #' df
#' get_pij <- function(df) {
#'   case_1 <- with(df, rs / (pi * alpha) * (1/z1 + 1/z2))
#'   case_2 <- with(df, rs / (pi) * (1/rij + 1/(alpha*z2)))
#'   pij <- ifelse(df$rij < df$z1 * df$alpha, case_1, case_2)
#'   pij <- ifelse(df$z2 * df$alpha <= df$rij, 0, pij)
#'   return(pij)
#' }



# theta1 <- 0; theta2 <- 0.04
# phi1 <- 0; phi2 <- 0.04

#' Get rectangle polygon from coordinates
#'
#' Get rectangle polygon in theta-phi space from coordinates
#' @param theta1
#' @param theta2
#' @param phi1
#' @param phi2
#' @details
#' Returns a rectangle as an sf polygon in theta-phi space
#' @example rectangle(0,pi/2,0,pi/6)
get_well_rectangle <- function(theta1, theta2, phi1, phi2) {
  corners <- data.frame(theta=c(theta1, theta1, theta2, theta2),
                        phi = c(phi1, phi2, phi2, phi1))
  well_rectangle_sf <- st_as_sf(corners, coords = c("theta","phi")) %>%
    summarize(do_union=FALSE) %>% st_cast("LINESTRING") %>% st_cast("POLYGON")
  return(st_geometry(well_rectangle_sf))
  # st_pi <- st_as_sf(data.frame(theta=pi,phi=0), coords =  c("theta","phi"))
}

#' Get household grid
#'
#' @param density housing density as a units object with units [1/acre]
#' @param z1_ft depth from the water table surface to the top of the well in ft
#' @param z2_ft depth from the water table surface to the bottom of the well in ft
#' @param rs radius of the well source area
#' @details
# Create an mxm grid of septic fields around a well at m/2,m/2.
get_hh_grid <- function(density, z1_ft, z2_ft, rs) {
  # Create mxm grid of septic fields around a well at m/2,m/2.
  # The total number of wells (N) determined by the number of houses in 9 connected pixels
  N <- as.numeric(density * units::set_units(16, "acre") * 9)
  m <- round(sqrt(N))

  l <- (1/density) %>% set_units("ft^2") %>% sqrt() # l is length of square of each property

  hh_grid <- expand.grid(col=seq(-ceiling(m/2)+1,m/2),row=seq(-ceiling(m/2)+1,m/2)) %>%
    mutate(x = col * l,
           y = row * l,
           rij = sqrt(x^2 + y^2),
           rs = rs,
           z1 = set_units(z1_ft,"ft"),
           z2 = set_units(z2_ft,"ft"),
           theta = as.numeric(atan2(y, x)),
           dtheta = atan(as.numeric(rs/rij)),
           origin = ifelse(as.numeric(x) == 0, as.numeric(y) == 0, FALSE),
           theta1 = ifelse(origin, -pi, theta - dtheta),
           theta2 = ifelse(origin, pi, theta + dtheta),
           phi1 = ifelse(origin, as.numeric(atan(z1 / rs)), as.numeric(atan(z1 / rij))),
           phi2 = ifelse(origin, pi/2, as.numeric(atan(z2 / rij)))) %>% as_tibble()

  septic_grid <- hh_grid %>%
    rowwise() %>%
    mutate(well_rect = get_well_rectangle(theta1, theta2, phi1, phi2))

  return(septic_grid)

}


# hh_grid %>% filter(phi1 > 1 | phi2 > 1)

#' Shift rectangle polygons to fall within [-pi, pi]
#'
#' @param septic_grid septic_grid is a data.freame containing well_rect column populated by get_well_rectangle
#' @param theta_min minimum theta over which to clip output
#' @param theta_max maximum theta over which to clip output
#' @details
#' Shift all well rectangles by ±2pi and clip to the range \code{[theta_min, theta_max]}. These theta values
#' don't necessarily need to be the extremes [-pi, pi] and can be any subset of that range.
shift_hh_grid_pi <- function(septic_grid, theta_min = -pi, theta_max = pi) {
  st_pi <- st_as_sf(data.frame(theta=pi,phi=0), coords =  c("theta","phi")) %>% st_geometry() # sf object representing pi

  wells_rectangles <- c(septic_grid$well_rect,
                        septic_grid$well_rect[septic_grid$theta2 > pi] - st_pi*2, # shift any object that cross pi or -pi by 2 pi
                        septic_grid$well_rect[septic_grid$theta1 < -pi] + st_pi*2)

  domain <- get_well_rectangle(theta_min, theta_max, 0, pi/2)
  wells <- st_intersection(wells_rectangles, domain)

  return(wells)
}



#' Get probability of contamination
#'
#' @param wells
#' @param theta_min
#' @param theta_max
#' @param alpha_min
#' @param alpha_max
#' CAREFUL WON'T WORK IF [theta_min, theta_max] CROSSES OVER -pi or pi
get_union_probability <- function(wells, theta_min = -pi, theta_max = pi, alpha_min = 0, alpha_max = 100, show_progress = TRUE) {
  # theta_min <- theta_range[1]
  # theta_min <- theta_range[2]
  wells_union <- st_union(wells)

  # this line removes points where more than 2 points make a line. Points are removed
  # from the polygon (including desired vertices) but it's okay because the unique values
  # of X are preserved. The unique values of Y are also preserved. But some unique combinations are lost.
  well_union_pts <- st_coordinates(wells_union) %>% as_tibble() %>% dplyr::select(X,Y) %>%
    ungroup() %>%
    mutate(remove = (X == lag(X,1) & X == lag(X,2)) | (Y == lag(Y,1) & Y == lag(Y,2)) ,
           remove = ifelse(is.na(remove),FALSE,remove)) %>%
    filter(!remove)
  theta_breaks <- sort(unique(well_union_pts$X)) # all breaks long theta axis

  # get each polygon within theta_breaks[i], theta_breaks[i+1]
  if (show_progress) {pb <- txtProgressBar(min = 0, max = length(theta_breaks) - 1, style = 3)}
  for (i in 1:(length(theta_breaks)-1)) {
    if(show_progress) {setTxtProgressBar(pb, i)}
    theta_domain <- get_well_rectangle(theta_breaks[i], theta_breaks[i+1], 0, pi/2)
    wells_theta_break <- st_intersection(wells_union, theta_domain)
    if (any(grepl("GEOMETRYCOLLECTION",class(wells_theta_break)))) {
      wells_theta_break <- st_collection_extract(wells_theta_break)
    }
    wells_theta_break_pts <- st_coordinates(wells_theta_break) %>% as_tibble() %>% dplyr::select(X,Y) %>%
      ungroup() %>%
      mutate(remove = (X == lag(X,1) & X == lag(X,2)) | (Y == lag(Y,1) & Y == lag(Y,2)) ,
             remove = ifelse(is.na(remove),FALSE,remove)) %>%
      filter(!remove)

    # ggplot() + geom_sf(data=wells_union,aes(),fill="gray") +
    #   ylim(c(0,0.1)) + xlim(c(-pi,-pi+0.2))

    # ggplot() + geom_sf(data=wells_theta_break,aes(),fill="gray") + ylim(c(0,0.1))

    phi_breaks <- sort(unique(wells_theta_break_pts %>% pull("Y"))) # all breaks long theta axis

    for (j in 1:(length(phi_breaks)-1)) {
      phi_domain <- get_well_rectangle(theta_breaks[i], theta_breaks[i+1], phi_breaks[j], phi_breaks[j+1])
      wells_phi_break <- st_intersection(wells_theta_break, phi_domain)
      if (!any(grepl("sfc_POLYGON",class(wells_phi_break)))) { # in some caes there may be just a line or points which are class: LINESTRING or sfc_GEOMETRYCOLLECTION
        # note that the intersection will return a linestring or sfc_GEOMETRYCOLLECTION (points) if the area is empty
        combined_rectangles <- combined_rectangles
      } else {
        wells_phi_break_pts <- st_coordinates(wells_phi_break) %>% as_tibble() %>% dplyr::select(X,Y) %>%
          ungroup() %>%
          mutate(remove = (X == lag(X,1) & X == lag(X,2)) | (Y == lag(Y,1) & Y == lag(Y,2)) ,
                 remove = ifelse(is.na(remove),FALSE,remove)) %>%
          filter(!remove)
        if (i == 1 & j == 1) {
          combined_rectangles <-
            with(wells_phi_break_pts,
                 data.frame(theta1 = min(X), theta2 = max(X), phi1 = min(Y), phi2 = max(Y)))
        } else {
          combined_rectangles <- combined_rectangles %>%
            rbind(with(wells_phi_break_pts,
                       data.frame(theta1 = min(X), theta2 = max(X), phi1 = min(Y), phi2 = max(Y))))
        }

        # ggplot() + geom_sf(data=wells_phi_break,aes(),fill="gray") + ylim(c(0,0.1))
      }
    }
    # ggplot() + geom_sf(data=wells_theta_break,aes(),fill="gray") + ylim(c(0,0.1))
  }

  # convert polygons to probabilities
  # aquifer <- data.frame(theta_min = theta_min, theta_max = theta_max, alpha_min = alpha_min, alpha_max = alpha_max)
  probs <- combined_rectangles %>% as_tibble() %>%
    mutate(alpha1_orig = 1/tan(phi2),
           alpha1_adj = case_when(
             alpha1_orig < alpha_min~alpha_min,
             alpha1_orig > alpha_max~alpha_max,
             TRUE~alpha1_orig),
           alpha2_orig = 1/tan(phi1),
           alpha2_adj = case_when(
             alpha2_orig < alpha_min~alpha_min,
             alpha2_orig > alpha_max~alpha_max,
             TRUE~alpha2_orig),
           d_alpha = alpha2_adj - alpha1_adj,
           d_theta = theta2 - theta1,
           p_theta = d_theta / (theta_max - theta_min),
           p_z = d_alpha / (alpha_max - alpha_min),
           p = p_theta * p_z) ## NEED TO FIX THIS LINE -- E.G., THERE WILL BE AN PROBLEM if THETA2 IS GREATER THAN THETA_MAX
  # sum(probs$p)

  return(probs)

}


get_intersection_probability <- function(density, z1_ft, z2_ft, rs, theta_min = -pi, theta_max = pi, alpha_min = 0, alpha_max = 100, self_treat = FALSE, show_progress = TRUE) {
  septic_grid <- get_hh_grid(density, z1_ft, z2_ft, rs)
  if (self_treat) {
    septic_grid <- septic_grid %>% filter(as.numeric(rij) > 0)
  }
  wells <- shift_hh_grid_pi(septic_grid, theta_min, theta_max = theta_max)
  probs <- get_union_probability(wells, theta_min = theta_min, theta_max = theta_max, alpha_min = alpha_min, alpha_max = alpha_max, show_progress = show_progress)
  return(list(septic_grid=septic_grid, probs=probs, wells = wells))
}



# well_rectangle(theta1 = 0, theta2 = 0.04,
#                phi1 = 0, phi2 = 0.04) %>% st_geometry()
#
#
#
# hh_grid$pij <- get_pij(hh_grid)
# p_i <- sum(hh_grid$pij)
# p_i
#
# ggplot(hh_grid) + geom_point(aes(x,y,color=pij)) +
#   scale_color_viridis_c()
#
# df <- tibble(rij = seq(20,200, by = 10), rs = 10, alpha = 100, z1 = 3, z2 = 20)
# df$pij <- get_pij(df)
# df
