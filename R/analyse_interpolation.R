################################
################################
#### analyse_interpolation.R

#### This script: 
# 1) Analyses the suitability of nearest neighbour interpolation in the 
# ... assembly for validation datasets. 
# ... A) For bottom temperatures:
# ... ... i) The differences between focal nodes and surrounding nodes are estimated
# ... ... ii) The differences for focal nodes between hourly predictions are estimated
# ... B) For temperature-depth profiles:
# ... ... i) The change in depth due to tidal elevation during angling is estimated 

#### Steps preceding this script: 
# 1) Define global parameters     (define_global_param.R)
# 2) Obtain and process raw data  (process_data_raw.R)
# 3) Implement validation         (analyse_temp_bottom.R & analyse_temp_profile.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data 
# Spatial fields 
mesh      <- readRDS("./data/spatial/mesh/mesh_around_nodes_in_mpa.rds")
val_tb    <- readRDS("./data/wc/val_temp_bottom.rds")
val_tp    <- readRDS("./data/wc/val_temp_profile_pred.rds")


################################
################################
#### Analysis for bottom temperature

#### For the focal nodes (nearest to receiver locations), identify surrounding nodes:
nb <- rgeos::gTouches(mesh, byid = TRUE)
nb <- nb[, colnames(nb) %in% val_tb$mesh_ID]
nb <- nb[rowSums(nb) > 0, ]

### Define data for extraction of bottom temperatures 
# For each focal node and surrounding nodes, we will extract
# ... bottom temperatures over the period of observations
# ... and examine the distribution of differences
# ... between neighbouring nodes 
dates       <- utils.add::seq_range(range(val_tb$date), 1)
date_names  <- date_name(dates)
hours       <- 0:23
layers      <- 10
mesh_IDs    <- unique(as.integer(as.character(c(colnames(nb), rownames(nb)))))
wc <- 
  expand.grid(date_name = date_names, 
              hour = hours, 
              layer = layers, 
              mesh_ID = mesh_IDs) %>%
  dplyr::arrange(date_name, 
                 hour, 
                 mesh_ID)
str(wc)

#### Implement extraction (~10 mins)
run <- FALSE
if(run){
  t1 <- Sys.time()
  wc <- fvcom.tbx::extract(dat = wc,
                           dir2load = wc_con, 
                           extension = ".mat", 
                           cl = parallel::makeCluster(10L)
  )
  t2 <- Sys.time()
  saveRDS(wc, "./data/wc/var_temp_bottom.rds")
}

#### Calculate the differences in bottom temperatures between focal and neighbouring nodes [30 s]
# To do this, we loop over each focal node, 
# ... identify surrounding nodes
# ... and for each surrounding node,
# ... we calculate the differences in predictions compared to the focal node.
# ... We then convert this to a vector of differences and summarise it. 
wc_var <- 
  pbapply::pblapply(colnames(nb), function(mesh_id_focal){
    # mesh_id_focal <- colnames(nb)[1]
    nb_for_focal <- nb[, mesh_id_focal]
    mesh_id_nbs  <- names(which(nb_for_focal))
    wc_for_focal <- wc %>% dplyr::filter(mesh_ID %in% mesh_id_focal)
    wc_for_nbs   <- wc %>% dplyr::filter(mesh_ID %in% mesh_id_nbs)
    wc_for_nbs_by_nb <- split(wc_for_nbs, wc_for_nbs$mesh_ID)
    diff <- lapply(wc_for_nbs_by_nb, function(wc_for_nb){
      return(wc_for_focal$wc - wc_for_nb$wc)
    }) %>% unlist()
    return(diff_by_nb)
  }) %>% unlist()

#### Summarise the (absolute) differences in bottom temperatures 
# ... between focal and neighbouring nodes
utils.add::basic_stats(abs(wc_var), p = NULL)

#### Summarise the distribution of (absolute) hourly differences in predictions 
# ... for focal nodes 
wc %>% 
  dplyr::filter(mesh_ID %in% colnames(nb)) %>%
  dplyr::group_by(mesh_ID) %>%
  dplyr::mutate(diff = Tools4ETS::serial_difference(wc)) %>%
  dplyr::mutate(diff = abs(diff)) %>%
  dplyr::filter(!is.na(diff)) %>%
  dplyr::pull(diff) %>%
  utils.add::basic_stats(p = NULL)


################################
################################
#### Analysis for temperature-depth profiles

#### Define data for extraction 
# For each capture event, we will extract the tidal elevation values
# ... at that node for all the hours on the day of capture
# ... and then summarise the distribution of sequential differences 
wc <- 
  expand.grid(date_name = unique(val_tp$date_name), 
              hour      = 0:23,
              mesh_ID   = unique(val_tp$mesh_ID)) %>%
  dplyr::arrange(date_name, 
                 hour, 
                 mesh_ID)

#### Implement extraction (~3 s)
run <- FALSE
if(run){
  t1 <- Sys.time()
  wc <- fvcom.tbx::extract(dat = wc,
                           dir2load = "/Volumes/Lacie_Share/Dima/FVCOM_variable_outputs/tidal_elevation/", 
                           extension = ".mat"
  )
  t2 <- Sys.time()
  saveRDS(wc, "./data/wc/var_temp_profile.rds")
}

#### Summarise the distribution of (absolute) hourly differences in tidal elevation
# ... for each capture event
wc %>% 
  dplyr::group_by(mesh_ID) %>%
  dplyr::mutate(diff = Tools4ETS::serial_difference(wc)) %>%
  dplyr::mutate(diff = abs(diff)) %>%
  dplyr::filter(!is.na(diff)) %>%
  dplyr::pull(diff) %>%
  utils.add::basic_stats(p = NULL)


#### End of code. 
################################
################################