################################
################################
#### assemble_temp_profile.R

#### This script: 
# 1) Assembles the temperature-depth profile validation dataset
# ... A) Assemble observational dataset 
# ... ... (i.e., temperature-depth profile for recapture events)
# ... B) Assemble corresponding validation dataset

#### Steps preceding this script: 
# 1) Define global parameters     (define_global_param.R)
# 2) Obtain and process raw data  (process_data_raw.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data 
## Spatial fields 
mesh      <- readRDS("./data/spatial/mesh/mesh_around_nodes.rds")
h         <- readRDS("./data/spatial/mesh/mesh_h.rds")
coast     <- readRDS("./data/spatial/coast/coast.rds")
bathy     <- raster::raster("./data/spatial/bathy/bathy.tif")
## Movement time series 
# archival raw contains raw time series
# archival_following_tag_dep contains time series following tag deployment 
# archival_in_middle_recap contains time series preceding capture during individuals' time at liberty
# ... and we need to extract corresponding descending profiles using the times of events
# ... recorded in 'recapture_events'
# archival_in_final_recap contains time series preceding tag retrieval 
archival_raw                <- readRDS("./data/skate/archival_raw.rds")
archival_following_tag_dep  <- readRDS("./data/skate/archival_following_tag_dep.rds")
archival_in_middle_recap    <- readRDS("./data/skate/archival_in_middle_recap.rds")
archival_in_final_recap     <- readRDS("./data/skate/archival_during_final_recap.rds")
recaptures                  <- readRDS("./data/skate/recaptures_processed.rds")
recapture_events            <- readRDS("./data/skate/recapture_events.rds")
skateids                    <- readRDS("./data/skate/skateids.rds")


################################
################################
#### Define observational dataset 

run <- FALSE
if(run){
  
  #### Define archival time series following recapture events that occurred during time at liberty 
  archival_following_middle_recap <- 
    lapply(split(archival_in_middle_recap, archival_in_middle_recap$event_id), function(obs){
      # obs <- split(archival_in_middle_recap, archival_in_middle_recap$event_id)[[2]]
      time <- 
        recapture_events %>% 
        dplyr::filter(dst_id == obs$dst_id[1] & date_recap %in% as.Date(obs$date_time)) %>%
        dplyr::filter(recap_event == "end_recap") %>%
        dplyr::mutate(timestamp = timestamp + 60*2) %>%
        dplyr::pull(timestamp)
      out <- 
        archival_raw %>% 
        dplyr::filter(dst_id == obs$dst_id[1]) %>%
        dplyr::filter(date_time %in% seq(time, time + 9 * 2*60, by = 2*60)) %>%
        dplyr::select(individual, dst_id, date_time, temp, depth)
      out$category <- obs$event_id[1]
      return(out)
  }) %>% dplyr::bind_rows()
  length(unique(archival_following_middle_recap$category))
  
  #### Visualise the time series for each data source 
  lapply(
    list(archival_following_tag_dep, 
         archival_in_middle_recap,
         archival_following_middle_recap,
         archival_in_final_recap), 
    function(obs){
      pp <- par(mfrow = c(5, 6), oma = c(2, 2, 2, 2), mar = c(2, 2, 2, 2))
      lapply(split(obs, f = obs$dst_id), function(d){
        pretty_plot(d$date_time, d$depth*-1, 
                    pretty_axis_args = list(side = c(3, 2), lim = list(NULL, c(-200, 0))), 
                    main = d$dst_id[1], 
                    type = "b", 
                    col = "blue")
      }) %>% invisible()
      par(pp)
      # readline("Press [Enter] to continue...")
    })
  
  #### Define a single dataframe with depth/temperature observations during capture/release
  ## Define event IDs 
  # archival_in_middle_recap
  unique(archival_in_middle_recap$event_id)
  # archival_in_final_recap
  event_ids <- 
    seq(max(archival_in_middle_recap$event_id)+1, 
        length.out = length(unique(archival_in_final_recap$dst_id)))
  archival_in_final_recap$event_id <- 
    event_ids[match(archival_in_final_recap$dst_id, 
                    unique(archival_in_final_recap$dst_id))]
  # archival_following_tag_dep
  event_ids <- 
    seq(max(archival_in_final_recap$event_id)+1, 
        length.out = length(unique(archival_following_tag_dep$dst_id)))
  archival_following_tag_dep$event_id <- 
    event_ids[match(archival_following_tag_dep$dst_id, 
                    unique(archival_following_tag_dep$dst_id))]
  # archival_following_middle_recap 
  event_ids <- 
    seq(max(archival_following_tag_dep$event_id)+1, 
        length.out = length(unique(archival_following_middle_recap$category)))
  archival_following_middle_recap$event_id <- 
    event_ids[match(archival_following_middle_recap$category, 
                    unique(archival_following_middle_recap$category))]
  
  ## Distinguish sample types 
  # capture events
  archival_in_middle_recap$event_type  <- "angling"
  archival_in_middle_recap$sample_type <- "capture"
  archival_in_final_recap$event_type   <- "tagging"
  archival_in_final_recap$sample_type  <- "capture"
  # release events
  archival_following_tag_dep$event_type       <- "tagging"
  archival_following_tag_dep$sample_type      <- "release"
  archival_following_middle_recap$event_type  <- "angling"
  archival_following_middle_recap$sample_type <- "release"
  
  # Define columns to retain in each dataframe 
  cols <- c("event_id", "individual", "dst_id", "event_type", 
            "sample_type", "date_time", "depth", "temp")
  archival_in_middle_recap        <- archival_in_middle_recap[, cols]
  archival_in_final_recap         <- archival_in_final_recap[, cols]
  archival_following_tag_dep      <- archival_following_tag_dep[, cols]
  archival_following_middle_recap <- archival_following_middle_recap[, cols]
  
  # Define validation dataset
  obs <- rbind(archival_in_middle_recap, archival_in_final_recap, 
               archival_following_tag_dep, archival_following_middle_recap)
  # Show that 'release' events all comprise 10 observations
  obs %>% 
    dplyr::filter(sample_type == "release") %>% 
    dplyr::group_by(event_id) %>% 
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull(n) %>%
    table()
  
  # Define data name 
  obs$date_name <- date_name(obs$date_time)
  
  #### Examine 'raw' captures dataset
  ## Summaries
  head(obs)
  tail(obs)
  ## Number of recapture events
  # 26 capture events = 21 captures and 5 recreational events
  # 26 release events = 21 captures and 5 recreational events
  length(unique(obs$event_id))
  obs %>% 
    dplyr::group_by(sample_type, event_type) %>% 
    dplyr::summarise(length(unique(event_id)))
  ## Visualise capture/release events
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
  # Add tag deployment locations
  recaptures_ls <- split(recaptures, recaptures$code == "dep")
  recaptures_ls$`TRUE`$lat <- 
    skateids$lat_tag_capture[match(recaptures_ls$`TRUE`$dst_id, skateids$dst_id)]
  recaptures_ls$`TRUE`$long <- 
    skateids$long_tag_capture[match(recaptures_ls$`TRUE`$dst_id, skateids$dst_id)]
  recaptures <- do.call(rbind, recaptures_ls)
  length(which(!is.na(recaptures$lat)))
  obs$match_recaptures <- paste0(as.Date(obs$date_time), "_", obs$dst_id)
  # Select relevant recaptures (i.e., those with coordinates)
  recaptures <- recaptures[!is.na(recaptures$lat), ]
  # Add locations to dataframe 
  recaptures$match_recaptures <- paste0(recaptures$date, "_", recaptures$dst_id)
  obs$lat <- recaptures$lat[match(obs$match_recaptures, recaptures$match_recaptures)]
  obs$long <- recaptures$long[match(obs$match_recaptures, recaptures$match_recaptures)]
  # Exclude any events for which locations are unavailable:
  # ... We have locations for:
  # ... ... 21 tag deployment events 
  # ... ... 2 angling events that occurred during individuals' time at liberty
  # ... ... 7 tag retrieval events (though one of these is questionable--see below) 
  obs <- obs[!is.na(obs$lat), ]
  obs %>% 
    dplyr::group_by(sample_type, event_type) %>% 
    dplyr::summarise(length(unique(event_id)))
  ## Validate recorded locations based on bathymetry versus WeStCOMS data
  # Define bathymetric depths
  obs$bathy <- abs(raster::extract(bathy, obs[, c("long", "lat")]))
  unique(obs$bathy) 
  # 152.80074  27.93853 135.51212 118.01100 158.41701 157.54199 156.57088 134.48238 137.89963
  # Flag any capture events for which the depth is >= 25 m than the observed depth  
  obs$loc <- paste0(obs$lat, "_", obs$long)
  shift   <- 25
  obs_ls <- split(obs, obs$sample_type)
  obs_ls$capture <- 
    lapply(split(obs_ls$capture, obs_ls$capture$event_id), function(d){
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
  obs_ls$capture <- plyr::compact(obs_ls$capture)
  obs_ls$capture <- dplyr::bind_rows(obs_ls$capture)
  obs            <- dplyr::bind_rows(obs_ls)
  ## One event has been excluded (Loch Sween) & 
  # ... we will drop the release ts for this individual as well, 
  # ... leaving 21 tag deployment events, 
  # ... 1 angling event that occurred during individuals' time at liberty (up and down profiles)
  # ... 7 tag capture events 
  obs <- obs[-which(obs$lat == 56.008 & obs$long == -5.615), ]
  obs %>% 
    dplyr::group_by(sample_type, event_type) %>% 
    dplyr::summarise(length(unique(event_id)))
  
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
  # So we have a total of nine capture events with locational information,
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


#### End of code. 
################################
################################