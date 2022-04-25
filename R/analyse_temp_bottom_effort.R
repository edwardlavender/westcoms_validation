################################
################################
#### analyse_temp_bottom_effort.R

#### This script: 
# 1) Analyse validation 'effort' for bottom temperature 

#### Steps preceding this script: 
# 1) Assemble validation dataset (assemble_temp_bottom.R)


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
# Validation
validation <- readRDS("./data/wc/val_temp_bottom.rds")

#### Global parameters
cex      <- 1.5
cex.axis <- cex - 0.2
cex.pch  <- 0.2


################################
################################
#### Summary statistics 

#### Number of observations
nrow(validation)

#### Number of individuals contributing data (14): 
length(unique(validation$key))

#### Total time window
# 15th March 2016 to 1st June 2017:
range(validation$timestamp) 
# 443 days:
difftime(max(validation$timestamp), min(validation$timestamp), units = "days") 
# 72 days without detections during this time window:
which(!(
  seq(as.Date(min(validation$timestamp)), 
      as.Date(max(validation$timestamp)), 
      by = 1) %in% 
    validation$date)) %>% 
  length()

#### Total number of unique mesh nodes
# 37 locations:
length(unique(validation$mesh_ID)) 
sort(table(validation$mesh_ID)/nrow(validation)*100)

#### Total number of hours
# All hours of the day:
length(unique(validation$index_hour))

#### Total number of validation observations
# >100,000 validation observations (102,630):
nrow(validation)
# Proportion of observations contributed by each individual:
sort(table(validation$key)/nrow(validation) * 100 %>% round(digits = 1)) 
# 14 individuals contribute to validation, 
# ... but 5 individuals account for most validation (10-20 % each)


################################
################################
#### Temporal validation effort

#### Define dataframe of temporal validation effort 
temporal_effort <- table(validation$date)
temporal_effort <- data.frame(date = names(temporal_effort), 
                              count = as.numeric(temporal_effort))
temporal_effort$date <- as.POSIXct(temporal_effort$date, tz = "UTC")

#### Plot figure
save <- TRUE
if(save) png("./fig/val_temp_bottom_effort_temporal.png",
             height = 8, width = 8, units = "in", res = 600)
pp <- par(oma = c(1, 1, 1, 1))
pretty_plot(temporal_effort$date,
            temporal_effort$count,
            pretty_axis_args = list(side = 1:2, 
                                    pretty = list(n = 5),
                                    axis = list(list(format = "%b-%y"), list()),
                                    control_axis = list(las = TRUE, cex.axis = cex.axis)
            ),
            type = "l",
            lwd = 1,
            xlab = "Time (months)",
            ylab = "Daily number of observations",
            cex.lab = cex)
par(pp)
if(save) dev.off()


################################
################################
#### Spatial effort

#### Define dataframe with number of observations per location:
validation$loc    <- paste0("(", validation$long, ",", validation$lat, ")")
validation_by_loc <- split(validation, f = validation$loc)
spatial_effort <- lapply(validation_by_loc, function(df) {
  dat <- df[1, c("lat", "long")]
  dat$nobs <- nrow(df)
  return(dat)
}) %>% dplyr::bind_rows()

#### Define spatial fields
# Project spatial fields 
coast <- sp::spTransform(coast, bng)
mesh  <- sp::spTransform(mesh, bng)
mpa   <- sp::spTransform(mpa, bng)
spatial_effort_sp <- 
  sp::SpatialPoints(spatial_effort[, c("long", "lat")], wgs84)
spatial_effort_sp <- 
  sp::spTransform(spatial_effort_sp, bng)
# Crop spatial fields
coast_site <- raster::crop(coast, ext)
mesh_site  <- raster::crop(mesh, ext)

#### Define figure
# Set up to save
save <- TRUE
if(save) png("./fig/val_temp_bottom_effort_spatial.png",
             height = 8, width = 8, units = "in",  res = 600)
pp <- par(oma = c(1, 1, 1, 10))
# Define pretty axes 
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
# Plot and add pretty axes
raster::plot(coast_site, 
             col = col_land, 
             border = col_border, 
             xlim = axis_ls[[1]]$lim,
             ylim = axis_ls[[2]]$lim, 
             lwd = 0.25)
# Add spatial fields 
raster::lines(mesh_site, col = "royalblue", lwd = 0.25)
raster::lines(mpa, lwd = 1.75)
pretty_axis(axis_ls = axis_ls, add = TRUE)
mtext(side = 1, "Easting", cex = cex, line = 2)
mtext(side = 2, "Northing", cex = cex, line = 4)
# Add spatial effort 
adj <- 1.25e3
points(spatial_effort_sp,
       cex = spatial_effort$nobs/adj,
       pch = 21,
       col = scales::alpha("grey40", 0.95),
       bg = scales::alpha("grey", 0.5))
# check range nobs to inform legend
range(spatial_effort$nobs)
leg <- c(500, 1000, 5000, 10000, 20000)
# Add legend
par(xpd = NA)
legend(215000, 780000,
       legend = leg,
       cex = cex.axis,
       pch = rep(21, 5),
       pt.cex = leg/adj,
       col = scales::alpha("grey40", 0.95),
       pt.bg = scales::alpha("grey", 0.5),
       bty = "n",
       y.intersp = 3.75,
       x.intersp = 4)
# Add north arrow and scale 
add_north_arrow(135000, 772000, 
                width = 5000, 
                height = 8000)
raster::scalebar(10000, xy = c(180000, 681000), 
                 type = "line", 
                 label = c("10 km"))
rect(205000, ylim[1], 245000, ylim[2], lty = 3, lwd = 0.75)
par(xpd = TRUE)
if(save) dev.off()
par(pp)


################################
################################
#### Spatial and temporal effort
# I.e., The no. of nodes with observations per day through time

#### Define dataframe 
space_and_time <- 
  rowSums((table(validation$date, validation$mesh_ID) > 0) + 0)
space_and_time <- 
  data.frame(date = names(space_and_time), 
             count = as.numeric(space_and_time))
space_and_time$date <- 
  as.POSIXct(space_and_time$date, tz = "UTC")

#### Make plot
save <- TRUE
if(save) png("./fig/val_temp_bottom_effort_spatiotemporal.png",
             height = 8, width = 8, units = "in", res = 600)
pp <- par(oma = c(1, 1, 1, 1))
pretty_plot(space_and_time$date,
            space_and_time$count,
            pretty_axis_args = list(side = 1:2, 
                                    pretty = list(n = 5),
                                    axis = list(list(format = "%b-%y"), list()),
                                    control_axis = list(las = TRUE, cex.axis = cex.axis)
            ),
            type = "p",
            pch = 21,
            col = scales::alpha("grey", 0.9),
            bg = scales::alpha("grey", 0.9),
            xlab = "Time (months)",
            ylab = "Daily number of nodes",
            cex.lab = cex
)
# Add weekly average to elucidate trends
summary <- 
  summarise_in_bins(space_and_time$date,
                    space_and_time$count,
                    bin = "weeks",
                    fun = list(mean = mean))
lines(summary$mean$bin, summary$mean$stat, type = "b")
if(save) dev.off()


#### End of code. 
################################
################################