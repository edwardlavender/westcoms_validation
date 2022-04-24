################################
################################
#### analyse_temp_profile.R

#### This script: 
# 1) Implements the temperature-depth profile validation
# ... A) Assemble observational dataset 
# ... ... (i.e., temperature-depth profile for recapture events)
# ... B) Assemble corresponding validation dataset
# ... C) Overlay observed and predicted profiles for validation 

#### Steps preceding this script: 
# 1) Define global parameters     (define_global_param.R)
# 2) Obtain and process raw data  (process_data_raw.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data 
# Spatial fields 
mesh      <- readRDS("./data/spatial/mesh/mesh_around_nodes.rds")
h         <- readRDS("./data/spatial/mesh/mesh_h.rds")
coast     <- readRDS("./data/spatial/coast/coast.rds")
bathy     <- raster::raster("./data/spatial/bathy/bathy.tif")
# Movement time series 
archival_in_final_recap  <- readRDS("./data/skate/archival_during_final_recap.rds")
archival_in_middle_recap <- readRDS("./data/skate/archival_in_middle_recap.rds")
recaptures               <- readRDS("./data/skate/recaptures_processed.rds")


################################
################################
#### Define observational dataset 

run <- FALSE
if(run){
  
  #### Define a single dataframe with depth/temperature observations during capture
  # Define event IDs in archival_in_final_recap 
  # ... by continuing archival_in_middle_recap
  event_ids <- 
    seq(max(archival_in_middle_recap$event_id)+1, 
        length.out = length(unique(archival_in_final_recap$dst_id)))
  archival_in_final_recap$event_id <- 
    event_ids[match(archival_in_final_recap$dst_id, 
                    unique(archival_in_final_recap$dst_id))]
  # Distinguish sample types 
  archival_in_middle_recap$event_type  <- "angling"
  archival_in_middle_recap$sample_type <- "capture"
  archival_in_final_recap$event_type   <- "tagging"
  archival_in_final_recap$sample_type  <- "capture"
  # Define columns to retain in each dataframe 
  cols <- c("event_id", "individual", "dst_id", "event_type", 
            "sample_type", "date_time", "depth", "temp")
  archival_in_middle_recap <- archival_in_middle_recap[, cols]
  archival_in_final_recap <- archival_in_final_recap[, cols]
  # Define validation dataset
  obs <- rbind(archival_in_middle_recap, archival_in_final_recap)
  # Define data name 
  obs$date_name <- date_name(obs$date_time)
  
  #### Examine 'raw' captures dataset
  ## Summaries
  head(obs)
  tail(obs)
  ## Number of recapture events
  # 26 events = 21 captures and 5 recreational events:
  length(unique(obs$event_id))
  table(sapply(split(obs, obs$event_id), function(d) d$sample_type[1]))
  table(sapply(split(obs, obs$event_id), function(d) d$event_type[1]))
  ## Visualise capture events
  pp <- par(mfrow = c(5, 6), oma = c(2, 2, 2, 2), mar = c(2, 2, 2, 2))
  lapply(split(obs, f = obs$event_id), function(d){
    pretty_plot(d$date_time, d$depth*-1, 
                pretty_axis_args = list(side = c(3, 2), lim = list(NULL, c(-200, 0))), 
                main = d$event_id[1], 
                type = "b", 
                col = "blue")
  }) %>% invisible()
  par(pp)
  
  #### Implement data processing 
  ## Define locations 
  # Select relevant recaptures (i.e., those with coordinates)
  length(which(!is.na(recaptures$lat)))
  obs$match_recaptures <- paste0(as.Date(obs$date_time), "_", obs$dst_id)
  recaptures <- recaptures[!is.na(recaptures$lat), ]
  # Add locations to dataframe 
  recaptures$match_recaptures <- paste0(recaptures$date, "_", recaptures$dst_id)
  obs$lat <- recaptures$lat[match(obs$match_recaptures, recaptures$match_recaptures)]
  obs$long <- recaptures$long[match(obs$match_recaptures, recaptures$match_recaptures)]
  # Exclude any events for which locations are unavailable:
  obs <- obs[!is.na(obs$lat), ]
  ## Validate recorded locations based on bathymetry versus WeStCOMS data
  # Define bathymetric depths
  obs$bathy <- abs(raster::extract(bathy, obs[, c("long", "lat")]))
  unique(obs$bathy) 
  # 152.80074  27.93853 135.51212 118.01100 158.41701 157.54199 156.57088 134.48238 137.89963
  # Flag any capture events for which the depth is >= 25 m than the observed depth  
  obs$loc <- paste0(obs$lat, "_", obs$long)
  shift   <- 25
  obs <- lapply(split(obs, obs$event_id), function(d){
    too_deep <- max(d$depth) > (d$bathy[1] + shift)
    if(too_deep){
      d$max_depth <- max(d$depth)
      d$bathy <- d$bathy 
      print(d[1, ])
      return(NULL)
    } else{
      return(d)
    }
  })
  obs <- plyr::compact(obs)
  obs <- dplyr::bind_rows(obs)
  ## One event has been excluded (Loch Sween)
  
  #### Define westcoms nodes
  obs$mesh_ID <- find_cells(obs$lat, obs$long, 
                            mesh = mesh, 
                            proj = raster::crs(mesh), 
                            return = 4)
  obs$mesh_ID <- as.integer(as.character(obs$mesh_ID))
  
  #### Summary of capture events with locational information
  # All 21 archival tag deployment events have coordinates
  # Of the 5 recreational captures, only two are associated with coordinates
  # ... (1522 on 2016-08-27 and 1523 on 2016-05-16)
  # Of the tag recovery events, 
  # ... 7 have associated coordinates
  # So we have a total of nine events with locational information,
  # ... of which 8 have valid coordinates. 
  # ... (out of 26 events)
  
  #### Properties of observation dataset after processing
  # n events (i.e., observation profiles) remaining:
  length(unique(obs$event_id)) # 8 observations remaining
  # n different hours of the day:
  length(unique(Tools4ETS::hour_nearest(obs$date_time))) # 4 hours
  # across a depth range of:
  range(obs$depth) # 0.00 163.36
  # and a temperature range of
  range(obs$temp) # 7.81 14.05
  # n different dates:
  length(unique(as.Date(obs$date_time))) # 8
  # ... from end of April 2016 to end of April 2017
  range(as.Date(obs$date_time))
  # Examine distribution of dates
  as.Date(sort(tapply(as.Date(obs$date_time), obs$event_id, min)), origin = "1970-01-01")
  # all locations unique (8 locations):
  length(unique(paste0(obs$lat, "_", obs$long)))
  # although these only lie in 5 different nodes:
  length(unique(obs$mesh_ID))
  # Events lasted between 8 to 20 minutes 
  obs %>% 
    dplyr::group_by(event_id) %>%
    dplyr::summarise(duration = as.numeric(difftime(max(date_time), min(date_time)))) %>%
    dplyr::arrange(duration) %>%
    dplyr::pull(duration) %>%
    utils.add::basic_stats()
  
  #### Visualise validation depth-temperature profiles
  obs_ls <- split(obs, f = obs$event_id)
  pp <- par(mfrow = par_mf(length(obs_ls)), 
            oma = c(3, 3, 3, 3), 
            mar = c(3, 3, 3, 3))
  lapply(obs_ls, function(d){
    pretty_ts(d$date_time,
              d$depth *-1,
              d$temp,
              y2_method = "by_new_axis",
              add_lines_args = list(type = "b"),
              add_lines_args_y2 = list(col = "blue", type = "b"),
              pretty_axis_args = list(side = c(3, 2), 
                                      lim = list(NULL, list(-200, 0)), 
                                      axis = list(las = TRUE)),
              pretty_axis_args_y2 = list(side = 4, axis = list(las = TRUE))
    )
  }) %>% invisible()
  par(pp)
  saveRDS(obs, "./data/wc/val_temp_profile_obs.rds")
  
} else {
  obs <- readRDS("./data/wc/val_temp_profile_obs.rds")
}


################################
################################
#### Define predictions dataframe 

run <- FALSE
if(run){
  
  #### Define a dataframe for prediction extraction
  # ... This contains one row for each layer
  preds <- lapply(obs_ls, function(d){
    # Extract first row from observation dataframe and select necessary columns
    dsbt <- d[1, c("event_id", "date_time", "date_name", "mesh_ID")]
    # Add hour (needed to extract predictions)
    dsbt$hour <- round(lubridate::hour(dsbt$date_time) + 
                         lubridate::minute(dsbt$date_time)/60 + 
                         lubridate::second(dsbt$date_time)/3600)
    # Duplicate rows 
    dsbt <- lapply(1:10, function(i) dsbt) %>% dplyr::bind_rows()
    dsbt$layer <- 1:10
    # Order columns intuitively 
    dsbt <- 
      dsbt %>% 
      dplyr::select(event_id, date_time, date_name, hour, layer, mesh_ID)
    return(dsbt)
  }) %>% dplyr::bind_rows()
  head(preds, 15)
  
  #### Extract temperature predictions (~10 s)
  preds <- fvcom.tbx::extract(dat = preds, 
                              dir2load = wc_con, 
                              cl = parallel::makeCluster(9L))
  
  #### Compute the depth of each layer 
  # Get depths 
  preds <- 
    preds %>% 
    dplyr::arrange(date_time, event_id)
  preds_layer_depths <-
    depth_from_unknown(dat = preds, 
                       h = h, 
                       siglev = dat_siglev, 
                       dir2load = "/Volumes/Lacie_Share/Dima/FVCOM_variable_outputs/tidal_elevation/")
  # Extract depths for layers of interest
  preds_layer_depths$depth_layer <- NA
  for(i in 1:nrow(preds_layer_depths)){
    c <- paste0("l", preds_layer_depths$layer[i])
    preds_layer_depths$depth_layer[i] <- preds_layer_depths[i, c]
  }
  preds$depth_layer <- preds_layer_depths$depth_layer
  saveRDS(preds, "./data/wc/val_temp_profile_pred.rds")
  
} else {
  preds <- readRDS("./data/wc/val_temp_profile_pred.rds")
}


################################
################################
#### Compare observations and predictions 

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
obs      <- obs %>% dplyr::arrange(date_time)
obs_ls   <- split(obs, f = factor(obs$event_id, levels = unique(obs$event_id)))
preds_ls <- split(preds, f = preds$event_id)
preds_ls <- preds_ls[names(obs_ls)]
names(obs_ls)
names(preds_ls)
# Define colours for observed and predicted depth-temperature profiles
colo <- scales::alpha("red", 0.9)
colp <- scales::alpha("black", 0.9)
# Set plotting window
save <- TRUE
if(save) png("./fig/val_temp_profile_results_profiles.png", 
             height = 7, width = 9, units = "in", res = 600)
pp <- par(mfrow = c(2, 4), oma = c(1, 5, 5, 1), mar = c(1, 3.5, 4.75, 2))
# Loop over each recapture event
mapply(obs_ls, preds_ls, letters[1:length(obs_ls)], FUN = function(o, p, main){
  
  #### Ensure we are dealing with the same event
  stopifnot(all(o$event_id %in% p$event_id) & all(p$event_id %in% o$event_id))
  
  #### Define axes properties 
  temp_lwr <- floor(min(o$temp))
  lim <- list(c(temp_lwr, temp_lwr+2), c(-200, 0)) 
  axis_ls <- pretty_axis(side = c(3, 2), 
                         lim = lim,
                         axis = list(las = TRUE, cex.axis = cex.axis)
                         )
  
  #### Define plot  
  # Plot observed depth-temperature profile 
  pretty_plot(o$temp, o$depth_neg, 
              pretty_axis_args = list(axis_ls = axis_ls),
              xlab = "", ylab = "",
              type = "l", col = colo)
  points(o$temp, o$depth_neg, pch = 21, col = colo, bg = colo)
  # Add predicted profile 
  points(p$wc, p$depth_layer_neg, type = "p", pch = 21, col = colp, bg = colp)
  lines(p$wc, p$depth_layer_neg, type = "l", col = colp, bg = colp)
  
  #### Add titles 
  day <- as.Date(o$date_time[1])
  day <- format(day, "%Y-%m-%d")
  mtext(side = 3, text = main, cex = cex.axis-0.2, adj = -0.02, line = 2.75, font = 2)
  mtext(side = 3, text = paste0("   (", day, ")"), cex = cex.axis-0.2, adj = -0.02, line = 2.75)
  
  #### Add legend
  if(main == "a"){
    legend(x = 13.75, -150,
           xjust = 1,
           pch = c(21, 21),
           col = c("red", "black"),
           pt.bg = c("red", "black"),
           legend = c(expression(T[o]), expression(T[p])),
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


#### End of code. 
################################
################################