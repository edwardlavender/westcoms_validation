################################
################################
#### process_data_raw.R

#### This script: 
# 1) Processes raw data for use in this project:
# ... Defines MPA boundaries
# ... Defines coastline in area
# ... Builds WeStCOMS meshes for the area 

#### Steps preceding this script: 
# 1) Obtain raw data          (see README)


################################
################################
#### Set up

#### Wipe workspace
rm(list = ls())

#### Essential packages
library(magrittr)
library(fvcom.tbx)

#### Load data
## Bathymetry
bathy <- raster::raster("./data-raw/spatial/bathy/digmap_bathym_merged_1arcsec_res/digimap_bathy_merge_1arcsec_res.tif")
## MPA boundaries
mpa <- rgdal::readOGR("./data-raw/spatial/mpa/mpa_polygon.shp")
## WeStCOMS mesh
# mesh coordinates (nodes)
nodexy <- read.csv("./data-raw/spatial/mesh/mesh_x.csv")
str(nodexy)
## mesh depths 
h <- readxl::read_xlsx("./data-raw/spatial/mesh/h.xlsx", 
                       sheet = "depth_below_mean_sea_level")
# trinodes
trinodes <- read.csv("./data-raw/spatial/mesh/mesh_trinodes.csv")
str(trinodes)

#### Define global parameters
wgs84  <- sp::CRS(SRS_string = "EPSG:4326")
bng    <- sp::CRS(SRS_string = "EPSG:27700") 


################################
################################
#### Bathymetry, coastline and MPA boundaries

#### Define bathymetry
raster::writeRaster(bathy, "./data/spatial/bathy/bathy.tif")
bathy_bng <- raster::projectRaster(bathy, crs = bng)
raster::writeRaster(bathy_bng, "./data/spatial/bathy/bathy_bng.tif")

#### Define MPA boundaries
mpa <- sp::spTransform(mpa, wgs84)
raster::crs(mpa)
raster::plot(mpa)
saveRDS(mpa, "./data/spatial/mpa/mpa.rds")
mpa_bng <- sp::spTransform(mpa, bng)
saveRDS(mpa_bng, "./data/spatial/mpa/mpa_bng.rds")

#### Define coastline
# Download a SpatialPolygonsDataFrame defining the administrative areas of the UK:
download <- FALSE
if(download){
  download.file("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_GBR_0_sp.rds",
                destfile = "./data-raw/spatial/coast/GBR_adm0.rds")
}
coast <- readRDS("./data-raw/spatial/coast/GBR_adm0.rds")
coast <- sp::spTransform(coast, wgs84)
ire   <- readRDS("./data-raw/spatial/coast/gadm36_IRL_0_sp.rds")
coast <- raster::bind(coast, ire)
saveRDS(coast, "./data/spatial/coast/coast.rds")
coast_bng <- sp::spTransform(coast, bng)
saveRDS(coast_bng, "./data/spatial/coast/coast_bng.rds")


################################
################################
#### Build mesh 

#### Data processing
## Node coordinates 
colnames(nodexy) <- c("x", "y", "z")
nodexy$node_id   <- 1:nrow(nodexy)
nodexy           <- dplyr::select(nodexy, node_id, x, y, z)
nrow(nodexy)
## Node depths 
h <- data.frame(ID = 1:nrow(h), h = h$h)
## Save dataframes
saveRDS(nodexy, 
        "./data/spatial/mesh/mesh_nodexy.rds")
saveRDS(h, 
        "./data/spatial/mesh/mesh_h.rds")

#### Build mesh
run <- FALSE
if(run){
  
  ## Mesh around nodes
  mesh_around_nodes <- build_mesh(nodexy = nodexy,
                                  trinodes = trinodes,
                                  mesh_type = "element")
  raster::crs(mesh_around_nodes) <- wgs84
  
  ## Save mesh
  saveRDS(mesh_around_nodes, 
          "./data/spatial/mesh/mesh_around_nodes.rds")
  mesh_around_nodes_bng  <- sp::spTransform(mesh_around_nodes, bng)
  saveRDS(mesh_around_nodes_bng, 
          "./data/spatial/mesh/mesh_around_nodes_bng.rds")
  
} else {
  mesh_around_nodes <- 
    readRDS("./data/spatial/mesh/mesh_around_nodes.rds")
}

#### Crop mesh within the MPA
run <- FALSE
if(run){
  
  ## Mesh around nodes
  mesh_around_nodes_in_mpa <- raster::crop(mesh_around_nodes, mpa)
  # raster::plot(mesh_around_nodes_in_mpa, col = "royalblue")
  
  ## Summary statistics for the mesh in the MPA
  # Because this has been cut at the edges, the lower bound for the cell area
  # ... is an underestimate, but the median value should be stable. 
  utils.add::basic_stats(raster::area(mesh_around_nodes_in_mpa)/1e6)
  # min mean median  max  sd  IQR  MAD
  #   0 0.14   0.13 0.66 0.1 0.15 0.11
  length(unique(mesh_around_nodes_in_mpa$ID))
  # 5055
  
  ## Save meshes
  saveRDS(mesh_around_nodes_in_mpa, 
          "./data/spatial/mesh/mesh_around_nodes_in_mpa.rds")
  
} else {
  mesh_around_nodes_in_mpa <- 
    readRDS("./data/spatial/mesh/mesh_around_nodes_in_mpa.rds")
}


#### End of code. 
################################
################################