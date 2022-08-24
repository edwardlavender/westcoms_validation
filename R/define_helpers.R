################################
################################
#### helpers.R

#### This code:
# 1) Contains helper functions for other scripts. 


################################
################################
#### get_season()

#### Define a function to assign the 'season' given an integer month 
# 'Seasons' are defined to facilitate description of the raw data
# ... and thus are defined according to the distribution of raw data
# ... rather than 'formal' definitions (e.g. as in lunar::terrestrial.season)

get_season <- 
  function(month){
    if(month >= 1 & month < 3){
      season <- "Winter"
    } else if(month >= 3 & month < 6) {
      season <- "Spring"
    } else if(month >= 6 & month < 8){
      season <- "Summer"
    } else {
      season <- "Autumn/Winter"
    }
    return(season)
  }
get_season <- Vectorize(get_season)


################################
################################
#### calc_skill_metrics()

#### Define a metric to calculate model skill metrics 
# This function is designed for the bottom temperature validation dataset
# ... in analyse_bottom_temp.R. Using the validation dataset inputted, 
# ... the function calculates a set of standard model skill metrics
# ... either across the whole time series or each grouping level 
# ... (e.g., month/year category or node) if 'group' 
# ... (a character variable of a column in 'validation' used for grouping) 
# ... is specified. 

calc_skill_metrics <- function(validation, group = NULL){
  stopifnot(c("wc", "obs") %in% colnames(validation))
  if(!is.null(group)){
    validation <- 
      validation %>% 
      dplyr::group_by(.data[[group]]) 
  }
  validation %>%
    dplyr::mutate(error = wc - obs, 
                  error_sq = error^2) %>%
    dplyr::mutate() %>%
    dplyr::summarise(
      # n 
      n    = dplyr::n(),
      # mean obs and wc
      o_hat = mean(obs),
      m_hat = mean(wc),
      # sigma obs and wc, 
      sigma_o = sd(obs),
      sigma_m = sd(wc),
      # correlation coefficient (Pearson's r: Aleynik, Pablo)
      r    = cor(wc, obs),
      # mean difference ('mean bias': Pablo)
      mb   = mean(error), 
      # mean absolute difference ('mean error')
      me = mean(abs(error)),
      # rms difference ('rmse': Aleynik, Pablo)
      rmse = sqrt(mean(error_sq)), 
      # normalised mean difference ('normalised mean bias')
      nmb = mb/mean(obs),
      # normalised mean absolute difference ('normalised mean error')
      nme = mean(me)/mean(obs),
      # normalised RMSE
      nrmse = rmse/mean(obs),
      # Index of Agreement
      d = 1 - (sum(error_sq)/(sum((abs(wc - mean(obs)) + abs(obs - mean(obs)))^2))),
    ) %>% data.frame()
}


################################
################################
#### get_mvt_rate() and get_depth_limits_for_temp()

#### Define a function to calculate the movement rate
# This function takes in a start depth and and end depth 
# ... and calculates the rate of ascent or descent
# ... in units of 12 s, assuming an average speed of ascent 
# ... or descent. 
get_mvt_rate <- function(start_depth, 
                         end_depth, 
                         cast = c("capture", "release")){
  cast <- match.arg(cast)
  if(cast == "capture"){
    start <- start_depth + 4.77
    end   <- end_depth - 4.77 
  } else if(cast == "release"){
    start <- start_depth - 4.77 
    end   <- end_depth + 4.77
  }
  rate <- (abs(end - start)/120)*12
  return(rate)
}

#### Define a function to calculate depth (temperature limits)
# This function calculates, for a given depth/temperature observation, 
# ... the possible depth range to which the temperature observation
# ... corresponds. This depends on the uncertainty in depth (± 4.77 m)
# ... the response time of the temperature sensor (12 s) and how far
# ... the individual could move in that time. 
get_depth_limits_for_temp <- function(start_depth = 143, 
                                      end_depth = 133, 
                                      cast = c("capture", "release")){
  cast <- match.arg(cast)
  if(is.na(start_depth) | is.na(end_depth)) {
    if(cast == "capture"){
      limits <- c(end_depth - 4.77, end_depth + 4.77)
    } else if(cast == "release"){
      limits <- c(0, end_depth + 4.77)
    }
  } else {
    start_depths <- seq(start_depth - 4.77, start_depth + 4.77, by = 0.01)
    end_depths   <- seq(end_depth - 4.77, end_depth + 4.77, by = 0.01)
    possibilities <- 
      expand.grid(start_depth = start_depths, end_depth = end_depths)
    possibilities$adj <- 
      (abs(possibilities$end_depth - possibilities$start_depth)/120)*12
    if(start_depth >= end_depth) type <- "ascent" else type <- "descent"
    if(type == "ascent"){
      possibilities$depth_temp <- possibilities$end_depth + possibilities$adj
      limits <- range(c(possibilities$depth_temp, end_depth - 4.77))
    } else if(type == "descent"){
      possibilities$depth_temp <- possibilities$end_depth - possibilities$adj
      limits <- range(c(possibilities$depth_temp, end_depth + 4.77))
    }
  }
  # limits[limits < 0] <- 0
  return(limits)
}


#### End of code. 
################################
################################