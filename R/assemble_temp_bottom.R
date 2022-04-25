################################
################################
#### assemble_temp_bottom.R

#### This script: 
# 1) Assembles the bottom-temperature validation dataset

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
# Movement time series 
acoustics <- readRDS("./data/skate/acoustics.rds")
archival  <- readRDS("./data/skate/archival.rds")


################################
################################
#### Data processing 

#### Define the locational dataset
dat_obs1           <- acoustics
dat_obs1           <- dat_obs1[, c("date_time", "lat_receiver", "long_receiver", "dst_id")]
colnames(dat_obs1) <- c("timestamp", "lat", "long", "key")
dat_obs1$layer     <- 10
dat_obs1$key       <- as.character(dat_obs1$key)
dat_obs1           <- dat_obs1 %>% dplyr::arrange(key, timestamp)
head(dat_obs1, 20)
str(dat_obs1)

#### Define the observational dataset
dat_obs2           <- archival[, c("date_time", "dst_id", "temp")]
colnames(dat_obs2) <- c("timestamp", "key", "obs")
dat_obs2$key       <- as.character(dat_obs2$key)
dat_obs2           <- dat_obs2 %>% dplyr::arrange(key, timestamp)
head(dat_obs2, 3)
str(dat_obs2)

#### Filter the locational dataset by individuals with associated data
dat_obs1 <- 
  dat_obs1 %>% dplyr::filter(key %in% unique(dat_obs2$key))


################################
################################
#### Assemble dataset

#### Implement validate() (~15 minutes)
implement_validate <- FALSE
if(implement_validate){
  ## Implement validation 
  validation <-
    validate(dat_obs1            = dat_obs1,
             dat_obs2            = dat_obs2,
             threshold_match_gap = 120,
             mesh                = mesh,
             dir2load            = wc_con,
             cl                  = parallel::makeCluster(10L),
             pass2varlist        = "mesh"
    )
  ## Process validation dataset 
  # Define individuals ('key')
  dat_obs1$match   <- paste0(dat_obs1$timestamp, "-", dat_obs1$lat, "-", dat_obs1$long)
  validation$match <- paste0(validation$timestamp, "-", validation$lat, "-", validation$long)
  validation$key   <- dat_obs1$key[match(validation$match, dat_obs1$match)]
  # Fix validation differences (model - observation)
  validation$diff <- validation$wc - validation$obs
  # Define month/year categories 
  validation$mm_yy <- Tools4ETS::mmyy(validation$date)
  ## Save dataframe 
  saveRDS(validation, "./data/wc/val_temp_bottom.rds")
} else{
  validation <- readRDS("./data/wc/val_temp_bottom.rds")
}

#### Standard checks 
check <- FALSE
if(check){
  ## (A) Confirm that time series look reasonable 
  nrow(validation)
  range(difftime(validation$timestamp, validation$timestamp_obs, units = "secs"))
  # pretty_plot(validation$timestamp, validation$obs, col = "red", cex = 0.5)
  # points(validation$timestamp, validation$wc, col = "black", cex = 0.5)
  ## (B) Confirm that extraction has been implemented correctly 
  ck_ls <- lapply(1:2, function(i){
    set.seed(i)
    dn <- sample(unique(validation$date_name), 1)
    ck <- validation[validation$date_name == dn, ]
    ck$wc_frm_validate <- ck$wc
    ck$wc <- NULL
    ck <- extract(ck, 
                  dir2load = "/Volumes/Lacie_Share/Dima/FVCOM_variable_outputs/temp/",
                  extension = ".mat", 
                  verbose = FALSE)
    ck$identical <- ck$wc_frm_validate == ck$wc
    print(table(ck$identical))
    return(ck)
  })
}


#### End of code.
################################
################################