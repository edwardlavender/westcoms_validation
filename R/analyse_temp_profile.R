################################
################################
#### analyse_temp_profile.R

#### This script: 
# 1) Analyses observed versus modelled temperature-depth profiles 
# ... Both ascending and descending profiles are considered
# ... But initial analysis shows that descending profiles
# ... are of limited utility. 

#### Steps preceding this script: 
# 1) Assemble observed/modelled datasets (assemble_temp_profile.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")
source("./R/define_helpers.R")

#### Load data
obs_all   <- readRDS("./data/wc/val_temp_profile_obs.rds")
preds_all <- readRDS("./data/wc/val_temp_profile_pred.rds")


################################
################################
#### Examine data for ascending vs. descending profiles 

#### Define type (capture, release)
cast <- "capture"
cast <- "release"

#### Get data 
## Define observations 
obs   <- 
  obs_all %>% 
  dplyr::filter(sample_type == cast) %>%
  dplyr::arrange(as.Date(date_time), event_id, date_time) %>%
  dplyr::mutate(event_id = factor(event_id)) %>%
  dplyr::group_by(event_id) %>%
  dplyr::mutate(
    # Depth limits
    depth_start   = dplyr::lag(depth),
    depth_shallow = depth - 4.77, 
    depth_deep    = depth + 4.77,
    # The approx distance between sequential observations
    # ... if the individual travelled from between two observed depths 
    dist_in_2_mins_approx = abs(depth - depth_start),
    # The corresponding distance travelled in 12 s
    dist_in_12_secs_approx = (dist_in_2_mins_approx/120) * 12,
    # The maximum distance travelled in 12 s 
    # ... if the id travelled from the lower depth bound (given 4.77 m error) at t = 1
    # ... to the upper depth bound (given 4.77 m error) at t = 2 
    dist_in_12_secs_max = get_mvt_rate(start_depth = depth, 
                                       end_depth = depth_start, 
                                       cast = cast)
  ) %>% 
  # Drop any observations that we cannot guarantee were 'below' the surface
  # ... (or flag them on the figure appropriately)
  dplyr::filter(depth_shallow >= 0) %>%
  dplyr::mutate(index = dplyr::row_number())
# Check the shallowest reading
obs %>%
  dplyr::group_by(event_id) %>%
  dplyr::summarise(min_depth = min(depth)) %>%
  dplyr::pull(min_depth) %>%
  utils.add::basic_stats()

## Process observations 
# For release events, drop the first sub-surface observation
# ... The temperature at this observation could be influenced by the 
# ... temperature at the surface, as the individual may have been
# ... released in the last ~ 12 s (the temperature sensor response time). 
# ... We know individuals can descend rapidly (e.g., individual 1522
# ... descended following angling on 2016-08-27 to > 100 m in less than
# ... two minutes, so we cannot use a simple depth threshold here instead. 
# ... (There is evidence of this lag effect if you set run <- FALSE.)
run <- TRUE
if(run && cast == "release"){
  obs <- 
    obs %>% 
    dplyr::filter(index >= 2)
}

## Define observational uncertainties in depth 
obs <- pbapply::pblapply(split(obs, obs$event_id), function(obs_for_event){
  # obs_for_event <- split(obs, obs$event_id)[[1]]
  obs_for_event <- as.data.frame(obs_for_event)
  for(i in 1:nrow(obs_for_event)){
    # print(i)
    obs_for_event[i, c("depth_for_temp_shallow", "depth_for_temp_deep")] <- 
      get_depth_limits_for_temp(start_depth = obs_for_event$depth_start[i], 
                                end_depth = obs_for_event$depth[i], 
                                cast = cast)
  }
  return(obs_for_event)
}) %>% dplyr::bind_rows()

## Define corresponding predictions (uncertainties are negligible)
preds <- 
  preds_all %>% 
  dplyr::filter(event_id %in% obs$event_id) %>% 
  dplyr::mutate(event_id = factor(event_id, levels = levels(obs$event_id))) %>%
  dplyr::arrange(event_id)

#### Examine movement rates
## View data 
# For ascending profiles:
# ... deep depth limits for T are deeper than deep depth limit (due to temp lag)
# ... ... (but probably only marginally so given small temp gradient)
# ... and shallow depth limits could be deeper than the shallow limit too (again due to lag)
# ... ... (but since the lag is affected by the temperature gradient, 
# ... ... the shallowest possible depth limits is just the obs - 4.77 
# For descending profiles:
# ... shallow depth limits are also shallower than the deep depth limit (due to temp lag)
# ... deep depth limits for T could also shallower than shallow depth limit (due to temp lag) 
# ... ... (but may be as deep as the depth limit obs + 4.77 if tag response time is fast) 
view <- FALSE
if(view){
  View(obs[, c("event_id", "date_time", 
               "depth_start", "depth", "dist_in_2_mins_approx", "depth_shallow", "depth_deep", 
               "depth_for_temp_shallow", "depth_for_temp_deep", 
               "temp")])
}
## Examine summary statistics for ascent/descent rates
# Note that for descending profiles these are affected by the group of 
# ... observations close in depth at the bottom of most profiles
# ... on account of the fact that we have included a set number of 
# ... profiles from individuals post-release.
utils.add::basic_stats(obs$dist_in_2_mins_approx, na.rm = TRUE)
utils.add::basic_stats(obs$dist_in_12_secs_approx, na.rm = TRUE)
utils.add::basic_stats(obs$dist_in_12_secs_max, na.rm = TRUE)
utils.add::basic_stats(obs$depth_for_temp_deep - obs$depth_for_temp_shallow, na.rm = TRUE) 
## Check the shallowest reading
obs %>%
  dplyr::group_by(event_id) %>%
  # dplyr::arrange(date_time, .by_group = TRUE) %>%
  # dplyr::slice(2L) %>%
  dplyr::summarise(depth = min(depth)) %>%
  dplyr::pull(depth) %>%
  utils.add::basic_stats()
# Results for cast == "capture"
# ... min mean median  max   sd   IQR  MAD
# ... 5.27 19.1  17.48 34.1 9.94 10.75 9.84
# Results for cast == "release"
# ... min  mean median    max    sd   IQR   MAD
# ... 16.51 64.43  56.51 144.13 34.03 33.67 25.48
## Check the distance moved in the first movement (from the shallowest reading to the next one)
## Check the number of depth 'bins' with observations 
obs %>%
  dplyr::group_by(event_id) %>%
  dplyr::mutate(depth_bin = cut(depth, breaks = seq(0, 250, by = 10))) %>%
  dplyr::summarise(n_bins = length(unique(depth_bin)), 
                   n_obs = dplyr::n()) %>%
  as.data.frame() %>%
  dplyr::pull(n_bins) %>%
  utils.add::basic_stats()
# capture 
# min mean median max   sd IQR  MAD
#   4  6.5    6.5   9 1.85 2.5 2.22 
# release
# min mean median max   sd IQR  MAD
#   2 4.14      4   7 1.39   2 1.48


################################
################################
#### Plot temperature-depth time series 

#### Plotting parameters for all plots
cex      <- 1.3
cex.axis <- 1.3
cex.pch  <- 0.2

#### Negate depths for visualisation 
obs$depth_neg         <- obs$depth*-1
preds$depth_layer_neg <- preds$depth_layer*-1

#### Plot observed/predicted temperature depth profiles for each event 
# Plotting param across all events 
x_rng <- range(c(obs$temp, preds$wc))
y_rng <- c(-200, 0)
# Split lists
obs      <- obs %>% dplyr::arrange(date_time, .by_group = TRUE)
obs_ls   <- split(obs, f = factor(obs$event_id, levels = unique(obs$event_id)))
preds_ls <- split(preds, f = preds$event_id)
preds_ls <- preds_ls[names(obs_ls)]
names(obs_ls)
names(preds_ls)
# Define colours for observed and predicted depth-temperature profiles
colo <- scales::alpha("red", 0.9)
colp <- scales::alpha("black", 0.9)
pt.cex <- 0.75
# Error bar param
eb_length <- 0.01
# Set plotting window
save <- TRUE
if(save){
  if(cast == "capture"){
    png(paste0("./fig/val_temp_profile_results_profiles_", cast, "_with_errors.png"), 
        height = 7, width = 9, units = "in", res = 600)
  } else if(cast == "release"){
    png(paste0("./fig/val_temp_profile_results_profiles_", cast, "_with_errors.png"), 
        height = 9, width = 12, units = "in", res = 600)
  }
}
if(cast == "capture"){
  pp <- par(mfrow = c(2, 4), 
            oma = c(1, 5, 5, 1), mar = c(1, 3.5, 4.75, 2))
} else if(cast == "release"){
  pp <- par(mfrow = par_mf(length(unique(obs$event_id))), 
            oma = c(1, 5, 5, 1), mar = c(1, 3.5, 4.75, 2))
}

# Loop over each recapture event
mapply(obs_ls, preds_ls, letters[1:length(obs_ls)], FUN = function(o, p, main){
  
  #### Ensure we are dealing with the same event
  # o <- obs_ls[[19]]
  # p <- preds_ls[[19]]
  # main = letters[19]
  stopifnot(all(o$event_id %in% p$event_id) & all(p$event_id %in% o$event_id))
  
  #### Define axes properties 
  temp_lwr <- floor(min(o$temp) - 0.1)
  if(cast == "capture"){
    lim <- list(c(temp_lwr, temp_lwr+2), c(-200, 0)) 
  } else {
    preds$temp <- preds$wc
    diff_1 <- 
      rbind(obs[, c("event_id", "temp")], preds[, c("event_id", "temp")]) %>% 
      dplyr::group_by(event_id) %>% 
      dplyr::summarise(diff = max(temp) - floor(min(temp))) %>%
      dplyr::pull(diff) %>%
      max() %>%
      ceiling()
    lim <- list(c(temp_lwr, temp_lwr+diff_1), c(-200, 0))
  }

  axis_ls <- pretty_axis(side = c(3, 2), 
                         lim = lim,
                         axis = list(las = TRUE, cex.axis = cex.axis)
                         )
  
  #### Define plot  
  # Plot observed depth-temperature profile 
  pretty_plot(o$temp, o$depth_neg, 
              pretty_axis_args = list(axis_ls = axis_ls),
              xlab = "", ylab = "",
              type = "n")
  add_error_bars(o$temp, 
                 fit = o$depth_neg, 
                 lwr = o$depth_for_temp_deep * -1, 
                 upr = o$depth_for_temp_shallow * -1, 
                 length = eb_length, col = "darkred",
                 add_fit = NULL)
  add_error_bars(o$depth_neg,
                 fit = o$temp,
                 lwr = o$temp - 0.1, 
                 upr = o$temp + 0.1, 
                 length = eb_length, col = "darkred", lwd = 1.5,
                 add_fit = NULL, 
                 horiz = TRUE)
  points(o$temp, o$depth_neg, pch = 21, col = colo, bg = colo, cex = pt.cex)
  lines(o$temp, o$depth_neg, col = colo, lty = 3)
  # Add predicted profile 
  points(p$wc, p$depth_layer_neg, type = "p", pch = 21, col = colp, bg = colp, cex = pt.cex)
  lines(p$wc, p$depth_layer_neg, type = "l", col = colp, bg = colp, lty = 3)
  
  #### Add titles 
  day <- as.Date(o$date_time[1])
  day <- format(day, "%Y-%m-%d")
  title <- main
  use_ast <- FALSE
  if(use_ast){
    if(cast == "release"){
      obs_cap <- obs_all %>% dplyr::filter(sample_type == "capture" & event_type == "angling")
      if(paste0(o$dst_id[1], "-", as.Date(o$date_time)[1]) %in% 
         paste0(obs_cap$dst_id, "-", as.Date(obs_cap$date_time))){
        title <- paste0(main, "*")
      }
    }
  }
  mtext(side = 3, text = title, cex = cex.axis-0.2, adj = -0.02, line = 2.75, font = 2)
  mtext(side = 3, text = paste0("   (", day, ")"), cex = cex.axis-0.2, adj = -0.02, line = 2.75)
  
  #### Add legend
  if(main == "a"){
    if(cast == "capture") {
      legend.x <- 13.75 
      legend.y <- -150
    } else if(cast == "release") {
      legend.x <- 11
      legend.y <- -100
    }
    legend(x = legend.x, y = legend.y,
           xjust = 1,
           pch = c(21, 21),
           col = c("black", "red"),
           pt.bg = c("black", "red"),
           pt.cex = pt.cex,
           legend = c(expression(T[M]), expression(T[O])),
           cex = cex.axis,
           bty = "n",
           x.intersp = 1
    )
  }
  
  #### Add box
  box("figure", lwd = 0.25, col = "dimgrey")
  
}) %>% invisible()

#### Add global axes titles 
mtext(side = 3, expression(paste("Temperature (T,", degree, "C)")), 
      cex = cex, line = 1.5, outer = TRUE)
mtext(side = 2, "Depth (m)", cex = cex, outer = TRUE, line = 2)
par(pp)
if(save) dev.off()

#### Comments:
# Recapture event 8, individual 39, dst 1511, starting 13:39:00
# ... stops before the surface. This is because any data after this are missing 
# ... in the raw data (see 1511_A.xlxs).
# For individual 1522, following capture & release by recreational anglers
# ... off Kerrera on 2016-08-27 15:04:00, the individual
# ... descended to 125.93 m within two minutes, so the vertical profile
# ... is 'missing' for this individual. 


#### End of code. 
################################
################################