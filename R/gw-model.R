# gw-model.R

#' Get rectangle polygon from coordinates
#'
#' Get rectangle polygon in theta-phi space from coordinates
#' @param theta1 Lower limit theta
#' @param theta2 Upper limit theta
#' @param phi1 Lower limit phi
#' @param phi2 Upper limit phi
#' @keywords internal
#' @details
#' Returns a rectangle as an sf polygon in theta-phi space
#' @examples
#' \dontrun{
#' get_well_rectangle(0,pi/2,0,pi/6)
#' }
get_well_rectangle <- function(theta1, theta2, phi1, phi2) {
  corners <- data.frame(theta=c(theta1, theta1, theta2, theta2),
                        phi = c(phi1, phi2, phi2, phi1))
  well_rectangle_sf <- sf::st_as_sf(corners, coords = c("theta","phi")) %>%
    dplyr::summarize(do_union=FALSE) %>% sf::st_cast("LINESTRING") %>% sf::st_cast("POLYGON")
  return(sf::st_geometry(well_rectangle_sf))
  # st_pi <- st_as_sf(data.frame(theta=pi,phi=0), coords =  c("theta","phi"))
}

#' Get household grid
#'
#' @param density housing density as a units object with units [1/area]
#' @param area total area of area as an object with units [acre]
#' @export
#' @details
#' Create a grid of septic fields around a well at (x = 0, y = 0). If the grid
#' length is even-numbered, the final row and column are place at positive x and y.
#' @examples
#' library(units)
#' density <- set_units(0.25,"1/acre")
#' area <- set_units(64,"acre")
#' hh_grid <- get_hh_grid(density,area)
#'
#' library(ggplot2)
#' library(ggforce) # ggforce allows units objects for the axes
#' ggplot(hh_grid) +
#'   geom_point(aes(x, y)) +
#'   geom_point(aes(0, 0), color = "red", size = 3, shape = 1)
get_hh_grid <- function(density, area) {
  # Create mxm grid of septic fields around a well at m/2,m/2.
  # The total number of wells (N) determined by the number of houses in 9 connected pixels
  density_acre <- units::set_units(density, "1/acre")
  area_acre <- units::set_units(area, "acre")
  N <- max(c(1, as.numeric(density_acre * area_acre))) # total number of wells (1 minimum)
  m <- round(sqrt(N)) # m x m wells

  if (1/density_acre > area_acre) {
    density_acre <- 1 / area_acre
  }

  l <- (1/density_acre) %>% units::set_units("ft^2") %>% sqrt() # %>% as.numeric() # l is length of square of each property

  hh_grid <- expand.grid(col=seq(-ceiling(m/2)+1,m/2),row=seq(-ceiling(m/2)+1,m/2))
  hh_grid$x = hh_grid$col * l
  hh_grid$y = hh_grid$row * l

  return(hh_grid)
}

#' Get septic well array
#'
#' @param hh_array array of septic or well systems with coordinates [x, y] as
#'   units objects and (optionally) z1, z2, rs
#' @param hh_array_type either "septic" or "well" describing the
#'   \code{hh_array}. See Details below.
#' @param ... See details for optional inputs
#' @importFrom rlang .data
#' @export
#' @description This function prepares an array of septic systems or private
#' wells for the groundwater model function
#' \code{get_intersection_probability()}. This function takes as input an array
#' of septic systems or wells with (x,y) coordinates where (0,0) is the location
#' of the domestic well.
#'
#' Option 1:
#'
#' If the goal is to calculate the probability that a single septic system at
#' (0,0) contaminates any well within a set of wells at (x, y), then
#' \code{hh_array} should represent a set of wells and \code{hh_array_type}
#' should be set to "wells". In this case, the x-y locations in \code{hh_array}
#' are kept and each well is parameterized with with a \code{well_rect}.
#'
#' Option 2:
#'
#' Alternatively, if the goal is to calculate the probability that a single well
#' at (0,0) is contaminated by any of a set of septic systems at (x, y), then
#' \code{hh_array} represents septic systems and \code{hh_array_type} should be
#' set to "septic". In this case, the function translates a set of point-source
#' septic fields that might contaminate the well into a set of virtual wells to
#' use with \code{get_intersection_probability()}. Furthermore, each virtual
#' well is parameterized with with a \code{well_rect}. In order to calculate the
#' probability of a septic system contaminating a well, the septic system is
#' treated as a virtual well identical to the actual well and located directly
#' opposite original septic system at (-xi, -yi). In other words, the septic
#' array is translated to a well array rotated 180 degrees around the domestic
#' well. The problem of determining contamination can then be treated by
#' considering the probability that a particle introduced at (0,0) will
#' intersect the virtual wells at (-xi, -yi).
#' @details Optial input in \code{...} can include: \itemize{ \item
#' code{z_range}: units vector of length 2 representing depth from the water
#' table surface to the top and bottom of the well \item \code{rs}: units object
#' representing the radius of the well source area }
#' @examples
#' # Generate household array
#' library(units)
#'
#' # Option 1: Prepare array of wells -- see Details
#' wells <- get_septic_well_array(hh_grid_example, "well")
#'
#' # plot the array of wells
#' library(ggplot2)
#' library(ggforce) # needed to plot axes using units objects
#' ggplot(mapping = aes(x, y, color = id)) +
#'   geom_point(data = hh_grid_example, aes(shape = "wells from\nhh_grid_example"),
#'       size = 4, stroke = 1) +
#'   geom_point(data = wells, aes(shape = "wells"), size = 2) +
#'   scale_shape_manual(values = c(16, 1)) +
#'   scale_color_viridis_c(option = "B") + coord_equal()
#'
#' # Option 2: Prepare array of virtual wells -- see Details
#' virtual_well_array <- get_septic_well_array(hh_grid_example, "septic")
#'
#' # plot the flipped array of virtual wells
#' library(ggplot2)
#' library(ggforce) # needed to plot axes using units objects
#' ggplot(mapping = aes(x, y, color = id)) +
#'   geom_point(data = hh_grid_example, aes(shape = "septic systems\nfrom hh_grid_example"),
#'       size = 4, stroke = 1) +
#'   geom_point(data = virtual_well_array, aes(shape = "virtual wells"), size = 2) +
#'   scale_shape_manual(values = c(1, 16)) +
#'   scale_color_viridis_c(option = "B") + coord_equal()
get_septic_well_array <- function(hh_array, hh_array_type, ...) {
  params <- list(...)

  if (all(grepl("[sS]eptic",hh_array_type))) {
    coord_flip <- -1
  } else if (all(grepl("[wE]ell",hh_array_type))) {
    coord_flip <- 1
  } else {
    stop("hh_array_type is ",hh_array_type," but must be either \"septic\" or \"well\"")
  }

  if ("z_range" %in% names(params)) {
    hh_array$z1 <- params$z_range[1]
    hh_array$z2 <- params$z_range[2]
  } else if (!all(c("z1","z2") %in% names(hh_array))) {
    stop("z1 and z2 must be columns in hh_array or specified in ... by z_range")
  }
  if ("rs" %in% names(params)) {
    hh_array$rs <- params$rs
  } else if (!("rs" %in% names(hh_array))) {
    stop("rs must be a column in hh_array or specified in ... by rs")
  }

  wells <- tibble::as_tibble(hh_array)
  wells$x <- coord_flip * wells$x
  wells$y <- coord_flip * wells$y
  wells$rij <- sqrt(wells$x^2 + wells$y^2)
  # wells$theta <- as.numeric(atan2(wells$y, wells$x))
  # wells$dtheta <- atan(as.numeric(wells$rs/wells$rij)) # this is the width of the well
  # wells$origin <- ifelse(as.numeric(wells$x) == 0, as.numeric(wells$y) == 0, FALSE)
  # wells$theta1 <- ifelse(wells$origin, -pi, wells$theta - wells$dtheta)
  # wells$theta2 <- ifelse(wells$origin, pi, wells$theta + wells$dtheta)
  # wells$phi1 <- ifelse(wells$origin, as.numeric(atan(wells$z1 / wells$rs)), as.numeric(atan(wells$z1 / wells$rij)))
  # wells$phi2 <- ifelse(wells$origin, pi/2, as.numeric(atan(wells$z2 / wells$rij)))
  # wells$well_rect <- mapply(get_well_rectangle,theta1 = wells$theta1, theta2 = wells$theta2, phi1 = wells$phi1, phi2 = wells$phi2)

  wells$theta <- as.numeric(atan2(wells$y, wells$x))
  wells$dtheta <- atan(as.numeric(wells$rs/wells$rij)) # this is the width of the well
  wells$origin <- ifelse(as.numeric(wells$x) == 0, as.numeric(wells$y) == 0, FALSE)
  wells$theta1 <- ifelse(wells$origin, -pi, wells$theta - wells$dtheta)
  wells$theta2 <- ifelse(wells$origin, pi, wells$theta + wells$dtheta)
  wells$phi1 <- ifelse(wells$origin, as.numeric(atan(wells$z1 / wells$rs)), as.numeric(atan(wells$z1 / wells$rij)))
  wells$phi2 <- ifelse(wells$origin, pi/2, as.numeric(atan(wells$z2 / wells$rij)))
  wells <- wells %>% tibble::as_tibble() %>% dplyr::rowwise() %>%
    dplyr::mutate(well_rect = get_well_rectangle(.data$theta1, .data$theta2, .data$phi1, .data$phi2))

  return(wells)
}


#' Shift rectangle polygons to fall within [-pi, pi]
#'
#' @inheritParams get_intersection_probability
#' @keywords internal
#' @details
#' Shift all well rectangles by ±2pi and clip to the range \code{[theta_min, theta_max]}. These theta values
#' don't necessarily need to be the extremes [-pi, pi] and can be any subset of that range.
shift_hh_grid_pi <- function(wells_array, theta_range = c(-pi, pi)) {
  theta_min <- theta_range[1]
  theta_max <- theta_range[2]
  st_pi <- sf::st_as_sf(data.frame(theta=pi,phi=0), coords =  c("theta","phi")) %>% sf::st_geometry() # sf object representing pi

  wells_rectangles <- c(wells_array$well_rect,
                        wells_array$well_rect[wells_array$theta2 > pi] - st_pi*2, # shift any object that cross pi or -pi by 2 pi
                        wells_array$well_rect[wells_array$theta1 < -pi] + st_pi*2)

  domain <- get_well_rectangle(theta_min, theta_max, 0, pi/2)
  wells <- sf::st_intersection(wells_rectangles, domain)

  return(wells)
}


#' Get probability of contamination
#'
#' @inheritParams get_intersection_probability
#' @importFrom rlang .data
#' @keywords internal
#' @details
#' Careful: this won't work if theta_range cross over -pi or pi
get_union_probability <- function(wells_array, theta_range = c(-pi, pi), alpha_range = c(0, 100), show_progress = TRUE) {
  theta_min <- theta_range[1]
  theta_max <- theta_range[2]
  alpha_min <- alpha_range[1]
  alpha_max <- alpha_range[2]
  wells_union <- sf::st_union(wells_array)

  # this line removes points where more than 2 points make a line. Points are removed
  # from the polygon (including desired vertices) but it's okay because the unique values
  # of X are preserved. The unique values of Y are also preserved. But some unique combinations are lost.
  well_union_pts <- sf::st_coordinates(wells_union) %>% tibble::as_tibble() %>% dplyr::select(c("X","Y")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(remove = (.data$X == dplyr::lag(.data$X,1) & .data$X == dplyr::lag(.data$X,2)) | (.data$Y == dplyr::lag(.data$Y,1) & .data$Y == dplyr::lag(.data$Y,2)) ,
           remove = ifelse(is.na(.data$remove),FALSE,.data$remove)) %>%
    dplyr::filter(!.data$remove)
  theta_breaks <- sort(unique(well_union_pts$X)) # all breaks long theta axis

  # get each polygon within theta_breaks[i], theta_breaks[i+1]
  if (show_progress) {pb <- utils::txtProgressBar(min = 0, max = length(theta_breaks) - 1, style = 3)}
  for (i in 1:(length(theta_breaks)-1)) {
    if(show_progress) {utils::setTxtProgressBar(pb, i)}
    theta_domain <- get_well_rectangle(theta_breaks[i], theta_breaks[i+1], 0, pi/2)
    wells_theta_break <- sf::st_intersection(wells_union, theta_domain)
    if (any(grepl("GEOMETRYCOLLECTION",class(wells_theta_break)))) {
      wells_theta_break <- sf::st_collection_extract(wells_theta_break)
    }
    wells_theta_break_pts <- sf::st_coordinates(wells_theta_break) %>% tibble::as_tibble() %>% dplyr::select(c("X","Y")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(remove = (.data$X == dplyr::lag(.data$X,1) & .data$X == dplyr::lag(.data$X,2)) | (.data$Y == dplyr::lag(.data$Y,1) & .data$Y == dplyr::lag(.data$Y,2)) ,
             remove = ifelse(is.na(.data$remove),FALSE,.data$remove)) %>%
      dplyr::filter(!.data$remove)

    # ggplot() + geom_sf(data=wells_union,aes(),fill="gray") +
    #   ylim(c(0,0.1)) + xlim(c(-pi,-pi+0.2))

    # ggplot() + geom_sf(data=wells_theta_break,aes(),fill="gray") + ylim(c(0,0.1))

    phi_breaks <- sort(unique(wells_theta_break_pts %>% dplyr::pull("Y"))) # all breaks long theta axis

    for (j in 1:(length(phi_breaks)-1)) {
      phi_domain <- get_well_rectangle(theta_breaks[i], theta_breaks[i+1], phi_breaks[j], phi_breaks[j+1])
      wells_phi_break <- sf::st_intersection(wells_theta_break, phi_domain)
      if (!any(grepl("sfc_POLYGON",class(wells_phi_break)))) { # in some caes there may be just a line or points which are class: LINESTRING or sfc_GEOMETRYCOLLECTION
        # note that the intersection will return a linestring or sfc_GEOMETRYCOLLECTION (points) if the area is empty
        combined_rectangles <- combined_rectangles
      } else {
        wells_phi_break_pts <- sf::st_coordinates(wells_phi_break) %>% tibble::as_tibble() %>% dplyr::select(c("X","Y")) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(remove = (.data$X == dplyr::lag(.data$X,1) & .data$X == dplyr::lag(.data$X,2)) | (.data$Y == dplyr::lag(.data$Y,1) & .data$Y == dplyr::lag(.data$Y,2)) ,
                 remove = ifelse(is.na(.data$remove),FALSE,.data$remove)) %>%
          dplyr::filter(!remove)
        if (i == 1 & j == 1) {
          combined_rectangles <- data.frame(theta1 = min(wells_phi_break_pts$X), theta2 = max(wells_phi_break_pts$X),
                                            phi1 = min(wells_phi_break_pts$Y), phi2 = max(wells_phi_break_pts$Y))
        } else {
          combined_rectangles <- combined_rectangles %>%
            rbind(data.frame(theta1 = min(wells_phi_break_pts$X), theta2 = max(wells_phi_break_pts$X),
                             phi1 = min(wells_phi_break_pts$Y), phi2 = max(wells_phi_break_pts$Y)))
        }

        # ggplot() + geom_sf(data=wells_phi_break,aes(),fill="gray") + ylim(c(0,0.1))
      }
    }
    # ggplot() + geom_sf(data=wells_theta_break,aes(),fill="gray") + ylim(c(0,0.1))
  }

  # convert polygons to probabilities
  # aquifer <- data.frame(theta_min = theta_min, theta_max = theta_max, alpha_min = alpha_min, alpha_max = alpha_max)
  probs <- combined_rectangles %>% tibble::as_tibble() %>%
    dplyr::mutate(alpha1_orig = 1/tan(.data$phi2),
                  alpha1_adj = dplyr::case_when(
                    .data$alpha1_orig < alpha_min~alpha_min,
                    .data$alpha1_orig > alpha_max~alpha_max,
                    TRUE~.data$alpha1_orig),
                  alpha2_orig = 1/tan(.data$phi1),
                  alpha2_adj = dplyr::case_when(
                    .data$alpha2_orig < alpha_min~alpha_min,
                    .data$alpha2_orig > alpha_max~alpha_max,
                    TRUE~alpha2_orig),
                  d_alpha = .data$alpha2_adj - .data$alpha1_adj,
                  d_theta = .data$theta2 - .data$theta1,
                  p_theta = .data$d_theta / (theta_max - theta_min),
                  p_z = .data$d_alpha / (alpha_max - alpha_min),
                  p = .data$p_theta * .data$p_z) ## NEED TO FIX THIS LINE -- E.G., THERE WILL BE AN PROBLEM if THETA2 IS GREATER THAN THETA_MAX
  # sum(probs$p)

  return(probs)

}

#' Get intersection probability
#'
#' @param wells_array Wells object prepared with \code{get_septic_well_array}
#' @param theta_range Vector describing the min and max of the uniform distribution for the mean lateral direction of flow
#' @param alpha_range Vector describing the min and max of distance to the groundwater divide
#' @param self_treat Logical. If \code{TRUE}, any well at (x = 0, y = 0) will be removed
#' @param return_option Either 1 or 2. See Return, below
#' @param show_progress Logical that determines if progress bar is shown
#' @importFrom rlang .data
#' @export
#' @details
#' Note that theta_range must not cross -pi or pi.
#' @return
#' If return_option = 1: Probability of contamination
#' If return_option = 2: Returns a list containing the well array after clipping and shifting and probabilities of contamination
#' @examples
#' library(units)
#' wells_array <- get_septic_well_array(hh_grid_example, "septic")
#' prob <- get_intersection_probability(wells_array, show_progress = FALSE)
get_intersection_probability <- function(wells_array, theta_range = c(-pi, pi), alpha_range = c(0, 100), self_treat = FALSE, return_option = 1, show_progress = TRUE) {
  if (max(theta_range) > pi | min(theta_range) < -pi) {
    stop("theta_range must not cross -pi or pi")
  }

  if (self_treat) {
    wells_array <- wells_array %>% dplyr::filter(as.numeric(.data$rij) > 0)
  }

  # if there are no wells left, set probs = 0
  if (nrow(wells_array) == 0) {
    probs <- list(p=0)
  } else {
    wells_array <- shift_hh_grid_pi(wells_array, theta_range = theta_range)
    probs <- get_union_probability(wells_array, theta_range = theta_range, alpha_range = alpha_range, show_progress = show_progress)
  }

  if (return_option == 1) {
    return(sum(probs$p))
  } else if (return_option == 2) {
    return(list(wells_array=wells_array, probs=probs))
  }
}



#' Get probability of contamination of row
#' @param params_row \code{list} or \code{tibble} row with appropriate names variables (see details)
#' @keywords internal
#' @details
#' \code{params_row} must contain the following named variables:
#' \itemize{
#' \item \code{z1}, \code{z2},  \code{rs}, \code{density}, \code{area} must be \code{units} objects.
#' \item \code{self_treat} is a boolean
#' \item \code{theta_range} should be in radians as \code{c(theta_min, theta_max)}
#' \item \code{alpha_range} should be in radians as \code{c(alpha_min, alpha_max)}
#' }
#' @examples
#' \dontrun{
#' library(units)
#' params_row <- tibble(
#'   z1 = set_units(10, "ft"),
#'   z2 = set_units(20, "ft"),
#'   rs = set_units(15, "ft"),
#'   density = set_units(1, "1/acre"),
#'   area = set_units(64, "acre"),
#'   self_treat = TRUE,
#'   theta_range = list(c(0, pi/4)), # this will be unlisted in the function
#'   alpha_range = list(c(0, 20))) # this will be unlisted in the function
#' get_row_contamination_probability(params_row)
#'
#' # This function allows multiple rows to be calculated at once
#' params_df <- params_row %>% dplyr::select(-density, -rs, -self_treat) %>%
#'   tidyr::crossing(density = set_units(c(0, 1), "1/acre"), rs = set_units(c(10, 20),"ft"), self_treat = c(TRUE, FALSE))
#'
#' # use sapply to get all probabilities at once
#' params_df$probs <- sapply(split(params_df, 1:nrow(params_df)), get_row_contamination_probability)
#' params_df
#' }
get_row_contamination_probability <- function(params_row) {
  # define parameters
  if (!all(c("z1", "z2", "rs", "density", "area", "self_treat", "theta_range", "alpha_range") %in%
      names(params_row))) {
    stop("params_row must contain all names elements z1, z2, rs, density, area, self_treat, theta_range, alpha_range")
  }
  z1 <- params_row$z1
  z2 <- params_row$z2
  density <- params_row$density
  area <- params_row$area
  rs <- params_row$rs
  self_treat <- params_row$self_treat
  theta_range <- params_row$theta_range
  alpha_range <- params_row$alpha_range

  if (!all(sapply(params_row[c("z1", "z2", "rs", "density", "area")],class) == "units")) {
    stop("All of z1, z2, rs, density, and area must be specified as units objects class")
  }

  # unless theta_range and alpha_range, if necessary
  if (class(theta_range) == "list") {
    theta_range <- theta_range[[1]]
  }
  if (class(alpha_range) == "list") {
    alpha_range <- alpha_range[[1]]
  }

  hh_array <- get_hh_grid(density, area)
  virtual_well_array <- get_septic_well_array(hh_array, "septic", z_range = c(z1, z2), rs = rs)
  prob <- get_intersection_probability(virtual_well_array,
                                       theta_range = theta_range, alpha_range = alpha_range,
                                       self_treat = self_treat, show_progress = FALSE)
  return(prob)
}



#' Get probabilities of contamination
#' @param params_df \code{tibble} with appropriate columns (see details)
#' @param show_progress boolean, whether or not to show progress bar
#' @export
#' @details
#' \code{params_df} must contain the following named columns:
#' \itemize{
#' \item \code{z1}, \code{z2},  \code{rs}, \code{density}, \code{area} must be \code{units} objects.
#' \item \code{self_treat} is a boolean
#' \item \code{theta_range} should be in radians as \code{c(theta_min, theta_max)}
#' \item \code{alpha_range} should be in radians as \code{c(alpha_min, alpha_max)}
#' }
#' @examples
#' library(units)
#' library(tidyr)
#' params_row <- tibble(
#'   z1 = set_units(10, "ft"),
#'   z2 = set_units(20, "ft"),
#'   area = set_units(64, "acre"),
#'   theta_range = list(c(0, pi/4)), # this will be unlisted in the function
#'   alpha_range = list(c(0, 20))) # this will be unlisted in the function
#'
#' # This function allows multiple rows to be calculated at once
#' params_df <- params_row %>%
#'   crossing(density = set_units(c(0, 0.5), "1/acre"), rs = set_units(c(10, 20),"ft"), self_treat = c(TRUE, FALSE))
#'
#' params_df$probs <- get_contamination_probabilities(params_df)
#' params_df
get_contamination_probabilities <- function(params_df, show_progress = FALSE) {

  if (show_progress) {pb <- utils::txtProgressBar(min = 1, max = nrow(params_df), style = 3)}

  probs <- NULL
  for (i in 1:nrow(params_df)) {
    if(show_progress) {utils::setTxtProgressBar(pb, i)}
    probs[i] <- get_row_contamination_probability(params_df[i,])
  }
  return(probs)
}
