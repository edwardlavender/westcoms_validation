################################
################################
#### analyse_study_site.R

#### This script: 
# 1) Maps the study site 

#### Steps preceding this script: 
# 1) Define global parameters     (define_global_param.R)
# 2) Obtain and process raw data  (process_data_raw.R)
# 3) Implement validation         (analyse_temp_bottom.R and analyse_temp_profile.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data 
# Spatial fields 
mesh      <- readRDS("./data/spatial/mesh/mesh_around_nodes.rds")
coast     <- readRDS("./data/spatial/coast/coast.rds")
mpa       <- readRDS("./data/spatial/mpa/mpa.rds")
# Validation datasets
val_tb    <- readRDS("./data/wc/val_temp_bottom.rds")
val_tp    <- readRDS("./data/wc/val_temp_profile_obs.rds")

################################
################################
#### Data processing

#### Define spatial fields
# Project spatial fields 
coast <- sp::spTransform(coast, bng)
mesh  <- sp::spTransform(mesh, bng)
mpa   <- sp::spTransform(mpa, bng)
# Crop spatial fields
coast_uk   <- rgeos::gSimplify(coast, 100)
coast_site <- raster::crop(coast, ext)
mesh_site  <- raster::crop(mesh, ext)


################################
################################
#### Map study site 

#### Define graphical parameters
cex      <- 1.5
cex.axis <- cex - 0.2
quick    <- FALSE

#### Set up figure 
save <- TRUE
if(save) png("./fig/study_site.png",
             height = 8, width = 8, units = "in",  res = 600)
pp <- par(oma = c(1, 1, 1, 1))

#### Define pretty axes 
xat <- c(xlim[1], 140000, 160000, 180000, 200000)
xlb <- c("", "140000", "160000", "180000", "200000")
yat <- c(ylim[1], 700000, 720000, 740000, 760000, ylim[2])
ylb <- c("", "700000", "720000", "740000", "760000", "")
axis_ls <- 
  pretty_axis(side = 1:4,
              x = list(xat, yat),
              axis = list(list(at = xat, labels = xlb),
                          list(at = yat, labels = ylb),
                          list(at = xat, labels = FALSE),
                          list(at = yat, labels = FALSE)), 
              control_axis = list(las = TRUE, cex.axis = cex.axis))

#### Plot and add pretty axes
raster::plot(coast_site, 
             col = col_land, 
             border = col_border, 
             xlim = axis_ls[[1]]$lim,
             ylim = axis_ls[[2]]$lim, 
             lwd = 0.25)

#### Add spatial fields 
if(!quick) raster::lines(mesh_site, col = "royalblue", lwd = 0.25)
raster::lines(mpa, lwd = 1.75)

#### Add validation locations
# Bottom temperature validation
val_tb_sp <- sp::SpatialPoints(val_tb[, c("long", "lat")], wgs84)
val_tb_sp <- sp::spTransform(val_tb_sp, bng)
points(val_tb_sp, pch = 4, cex = 0.75)
# Temperature-depth profile validation 
val_tp_sp <- 
  val_tp %>% 
  dplyr::group_by(event_id) %>% 
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::select(x = long, y = lat) %>%
  sp::SpatialPoints(proj = wgs84) %>%
  sp::spTransform(CRSobj = bng)
raster::coordinates(val_tp_sp)
points(val_tp_sp, pch = 3,  cex = 0.75, lwd = 1.5, col = "grey50")
legend(182000, 775000,
       pch = c(4, 3), 
       pt.cex = c(0.75, 0.75), 
       lwd = c(1, 1.5), 
       lty = c(0, 0),
       col = c("black", "grey50"), 
       legend = c("Bottom", "Profile"),
       x.intersp = 0.1,
       bty = "n"
       )

#### Add essential labels
labels <- data.frame(x = c(161025.3, 171000, 190000, 160000), 
                     y = c(733265.5, 750740.6, 719216.9, 687693.1), 
                     text = c("Mull", "Morvern", "Mainland", "Jura"))
text(labels$x, labels$y, labels$text, font = 2)
add_tagging_sites <- FALSE
if(add_tagging_sites){
  labels <- data.frame(x = c(180956.3, 169000, 189000), 
                       y = c( 693175.5, 717503.6, 731552.3), 
                       text = c("Crinan", "Insh", "Kerrera"))
  text(labels$x, labels$y, labels$text)
}

#### Add north arrow and scale 
add_north_arrow(135000, 772000, 
                width = 5000, 
                height = 8000)
raster::scalebar(10000, xy = c(180000, 681000), 
                 type = "line", 
                 label = c("10 km"))

#### Add axes titles 
pretty_axis(axis_ls = axis_ls, add = TRUE)
mtext(side = 1, "Easting", cex = cex, line = 2)
mtext(side = 2, "Northing", cex = cex, line = 0)

#### Add map of the UK
if(!quick){
  TeachingDemos::subplot(
    { 
      raster::plot(coast_uk, 
                   col = scales::alpha(col_land, 0.7), 
                   border = scales::alpha(col_border, 0.8), lwd = 0.5,
                   # xlim = c(0, 6e5), ylim = c(6e5, 1.2e6),
                   axes = FALSE)
      raster::lines(mesh, col = "royalblue", lwd = 0.1)
      # raster::plot(mpa, add = TRUE, col = "black", border = "black", lwd = 0.1)
      raster::lines(raster::extent(mpa), lwd = 2)
      raster::lines(flapper::update_extent(raster::extent(mpa), 1e5), lwd = 3)
      box()
    },
    x = xlim[1], y = ylim[1],
    size = c(1.5, 1.5),
    vadj = 0, hadj = 0
  )
}

#### Save
par(pp)
if(save) dev.off()


#### End of code. 
################################
################################