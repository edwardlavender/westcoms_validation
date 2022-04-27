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
mesh      <- readRDS("./data/spatial/mesh/mesh_around_nodes_bng.rds")
h         <- readRDS("./data/spatial/mesh/mesh_h.rds")
bathy     <- raster::raster("./data/spatial/bathy/bathy_bng.tif")
coast     <- readRDS("./data/spatial/coast/coast_bng.rds")
mpa       <- readRDS("./data/spatial/mpa/mpa_bng.rds")
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
# Depths
utils.add::basic_stats(node_IDs$depth)
utils.add::basic_stats(node_IDs$depth[4:nrow(node_IDs)])

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


################################
################################
#### Depth map 

#### Define node IDs 
node_IDs <- 
  validation %>%
  dplyr::group_by(mesh_ID) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::mutate(ID = (1:dplyr::n()) - 1, 
                depth = h$h[match(mesh_ID, h$ID)]) %>%
  dplyr::select(ID, mesh_ID, n, depth)
# Define tidy table
node_IDs %>%
  tidy_numbers(digits = 2) %>%
  dplyr::select(`Node ID` = ID, 
                Node = mesh_ID,
                n = n, 
                `Depth (m)` = depth
                ) %>%
  tidy_write("./fig/val_temp_bottom_effort_node_ids.txt")

#### Define spatial layers 
# Define boundaries of focal area of interest
mesh_focal    <- mesh[mesh$ID %in% node_IDs$mesh_ID, ]
ext <- raster::extent(mesh_focal)
ext <- flapper::update_extent(ext, 1000)
# Define mesh cell coordinates and IDs 
mesh_focal_xy    <- find_xy(node_IDs$mesh_ID, mesh_focal)
mesh_focal_xy$ID <- node_IDs$ID[match(mesh_focal_xy$mesh_ID, node_IDs$mesh_ID)]
# Crop spatial layers to region of interest
mesh_focal  <- raster::crop(mesh, ext)
bathy_focal <- raster::crop(bathy, ext)
bathy_focal <- raster::mask(bathy_focal, mesh_focal)
coast_focal <- raster::crop(coast, ext)

#### Define figure
# Set up to save
save <- TRUE
if(save) png("./fig/val_temp_bottom_effort_depth_map.png",
             height = 8, width = 6, units = "in",  res = 600)
pp <- par(oma = c(1, 1, 1, 1))
# Define pretty axes 
xlim <- ext[1:2]
ylim <- ext[3:4]
axis_ls <- 
  pretty_axis(side = 1:4,
              lim = list(xlim, ylim),
              pretty = list(list(n = 3), list(n = 5)),
              axis = list(list(),
                          list(),
                          list(labels = FALSE),
                          list(labels = FALSE)), 
              control_axis = list(las = TRUE, cex.axis = cex.axis), 
              control_sci_notation = list(magnitude = 16L, digits = 0))
# Plot and add pretty axes

raster::plot(coast_focal, 
             xlim = xlim, ylim = ylim,
             col = col_land, 
             border = col_border, 
             lwd = 0.25)
# Add spatial fields 
raster::plot(bathy_focal, 
             col = scales::alpha(viridis::viridis(100), 0.75), 
             add = TRUE)
raster::plot(coast_focal, 
             col = col_land, 
             border = col_border, 
             lwd = 0.25, 
             add = TRUE)
raster::lines(mesh_focal, col = "royalblue", lwd = 0.5)
lapply(split(node_IDs, 1:nrow(node_IDs)), function(d){
  m <- mesh[mesh$ID == d$mesh_ID, ]
  raster::lines(m, col = "royalblue", lwd = 2)
})
basicPlotteR::addTextLabels(mesh_focal_xy$x, mesh_focal_xy$y, 
                            mesh_focal_xy$ID, 
                            col.label = "black", 
                            cex.pt = 1, lwd = 1.5)

# Add axes and labels 
pretty_axis(axis_ls = axis_ls, add = TRUE)
mtext(side = 1, "Easting", cex = cex, line = 2)
mtext(side = 2, "Northing", cex = cex, line = 3)
mtext(side = 4, "Depth (m)", cex = cex, line = 2)
# Add north arrow and scale 
add_north_arrow(170097.7, 741356, 
                width = 5000/5, 
                height = 8000/5)
raster::scalebar(5000, xy = c(175000, 723000), 
                 type = "line", 
                 label = c("5 km"))
if(save) dev.off()


################################
################################
#### Depth time series

#### Calculate the number of observations in each depth bin through time 
# Define a series of depth bins  
breaks <- seq(0, 200, by = 25)
# Calculate the number of observations in each bin 
node_ts <- 
  validation %>% 
  dplyr::mutate(depth = h$h[match(mesh_ID, h$ID)]) %>%
  dplyr::mutate(bin = cut(depth, breaks, labels = FALSE)) %>%
  dplyr::group_by(bin, mm_yy) %>%
  dplyr::summarise(n = dplyr::n(), n_node = length(unique(mesh_ID))) %>%
  dplyr::mutate(mm_yy      = paste0(substr(mm_yy, 4, 8), "-", substr(mm_yy, 1, 2)), 
                date       = as.Date(paste0(mm_yy, "-01")),
                timestamp  = as.POSIXct(paste(date, "00:00:00"), tz = "UTC")) %>%
  dplyr::arrange(bin, mm_yy)
# Update bin labels to use the midpoint of each bin
node_ts$bin <- paste0(breaks[node_ts$bin + 1], " m")
node_ts$bin <- factor(node_ts$bin, levels = unique(node_ts$bin))

#### Define graphical parameters for plot 
zlim <- c(min(node_ts$n_node), max(node_ts$n_node) + 1)
col_param     <- 
  pretty_cols_brewer(zlim = zlim,
                     n_breaks = zlim[2],
                     pal = function(...) viridis::viridis(..., direction = -1))
node_ts$col     <- col_param$col[findInterval(node_ts$n_node, col_param$breaks)]

#### Define plot of number of observations through time for each depth bin
## Set up plot to save 
save <- TRUE
if(save) png("./fig/val_temp_bottom_effort_depth_ts.png",
             height = 5, width = 8, units = "in",  res = 600)
pp <- par(mfrow = par_mf(length(unique(node_ts$bin))), 
          oma = c(4, 4, 2, 5), 
          mar = c(2, 2, 2, 2))
## Make a plot for each depth bin 
lapply(1:nrow(node_ts), function(i){
  # Isolate data
  d <- split(node_ts, node_ts$bin)[[i]]
  # Define blank plot 
  pretty_plot(d$timestamp, d$n, 
              pretty_axis_args = list(
                # pretty = list(n = 12),
                x = list( x = range(node_ts$timestamp), 
                          y = range(d$n)),
                axis = list(list(format = "%b-%y"), list())),
              xlab = "", ylab = "",
              type = "n")
  # Add time series, coloured by the number of nodes with observations in each month
  nrw <- nrow(d)
  l0 <- 1:(nrw-1)
  l1 <- 2:nrw
  arrows(x0 = d$timestamp[l0], 
         y0 = d$n[l0], 
         x1 = d$timestamp[l1], 
         y1 = d$n[l1], 
         col = d$col[l0], 
         lwd = 1.5,
         length = 0)
  points(d$timestamp, d$n, 
         pch = 21, bg = d$col, col = d$col)
  # Add title defining the panel label, depth bin and number of nodes 
  nn_1 <- min(d$n_node)
  nn_2 <- max(d$n_node)
  if(nn_1 == nn_2) nn <- nn_1 else nn <- paste0(nn_1, "–", nn_2)
  nn <- paste0("[", nn, " nodes]")
  mtext(side = 3, 
        bquote(bold(.(letters[i])) ~ "(" * .(as.character(d$bin[1])) ~ .(nn) * ")"
        ), 
        adj = 0.7, 
        line = 0.3)
})
mtext(side = 1, "Time (months)", line = 3)
mtext(side = 2, "Number of observations", outer = TRUE, line = 2)
par(pp)
## Add colour bar
col_param_paa <- pretty_axis(side = 4,
                             lim = list(x = col_param$zlim),
                             axis = list(at = zoo::rollmean(col_param$breaks, 2), 
                                         labels = floor(zoo::rollmean(col_param$breaks, 2))),
                             control_axis = list(las = TRUE, cex.axis = 0.75),
                             add = FALSE)
TeachingDemos::subplot(
  add_colour_bar(data.frame(x = col_param$breaks,
                            col = c(col_param$col, NA)),
                 pretty_axis_args = col_param_paa,
                 mtext_args = list(side = 4, 
                                   "Number of nodes", 
                                   line = 2)
  ), 
  x = c(quantile(node_ts$timestamp, 1) + 70*24*60*60, 
        quantile(node_ts$timestamp, 1) + 75*24*60*60), 
  y = c(45, 215),
)
## Save
if(save) dev.off()


#### End of code. 
################################
################################