################################
################################
#### define_global_param.R

#### This code: 
# 1) Wipes the workspace, loads essential packages
# ... and defines parameters used by multiple scripts. 
# ... This is designed to be called at the start of every script. 

#### Steps preceding this code: 
# 1) WeStCOMS predictions have been generated.
# 2) WestCOMS predictions have been processed 
# ... in line with the requirements of the fvcom.tbx package:
# ... ... one folder for each variable 
# ... ... files labelled by date name 
# ... ... processed fields, such as current_speed, have been defined via fvcom.tbx. 


################################
################################
#### Global set up 

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(prettyGraphics)
library(magrittr)
library(fvcom.tbx)


################################
################################
#### Define global parameters 

#### Define the root directory containing FVCOM predictions
# These have been processed in line with fvcom.tbx requirements. 
wc_con <- "/Volumes/Lacie_Share/Dima/FVCOM_variable_outputs/temp/"
list.files(wc_con)

#### Projections
wgs84  <- sp::CRS(SRS_string = "EPSG:4326")
bng    <- sp::CRS(SRS_string = "EPSG:27700") 

#### Study site limits (BNG)
xlim <- c(130000.000, 200000.000)
ylim <- c(680000.000, 774469.749)
ext  <- raster::extent(c(xlim, ylim))
col_land   <- "#f1f4c7"
col_border <- "#bdbf97" 


#### End of code. 
################################
################################