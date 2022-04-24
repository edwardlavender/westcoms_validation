################################
################################
#### analyse_temp_bottom.R

#### This script: 
# 1) Implements the bottom-temperature validation

#### Steps preceding this script: 
# 1) Define global parameters     (define_global_param.R)
# 2) Obtain and process raw data  (process_data_raw.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")
source("R/define_helpers.R")

#### Load data 
# Spatial fields 
mesh      <- readRDS("./data/spatial/mesh/mesh_around_nodes.rds")
h         <- readRDS("./data/spatial/mesh/mesh_h.rds")
coast     <- readRDS("./data/spatial/coast/coast.rds")
mpa       <- readRDS("./data/spatial/mpa/mpa.rds")
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
#### Implement validation 

#### Implement valiation (~15 minutes)
implement_validate <- FALSE
if(implement_validate){
  validation <-
    validate(dat_obs1            = dat_obs1,
             dat_obs2            = dat_obs2,
             threshold_match_gap = 120,
             mesh                = mesh,
             dir2load            = wc_con,
             cl                  = parallel::makeCluster(10L),
             pass2varlist        = "mesh"
             )
  saveRDS(validation, "./data/wc/val_temp_bottom.rds")
} else{
  validation <- readRDS("./data/wc/val_temp_bottom.rds")
}

#### Process validation dataset 
# Fix validation differences (model - observation)
validation$diff <- validation$wc - validation$obs
# Define month/year categories 
validation$mm_yy <- Tools4ETS::mmyy(validation$date)

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


################################
################################
#### Validation effort 

################################
#### Summary statistics 

#### Number of observations
nrow(validation)

#### Number of individuals contributing data (14): 
dat_obs1$match   <- paste0(dat_obs1$timestamp, "-", dat_obs1$lat, "-", dat_obs1$long)
validation$match <- paste0(validation$timestamp, "-", validation$lat, "-", validation$long)
validation$key  <- dat_obs1$key[match(validation$match, dat_obs1$match)]
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
#### Temporal validation effort

#### Local parameters
cex      <- 1.5
cex.axis <- cex - 0.2
cex.pch  <- 0.2

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
#### Validation results 

################################
#### Visualisation 

#### Local parameters
cex.label <- 1.7
cex       <- 1.5
cex.axis  <- cex - 0.2
cex.pch   <- 0.2
line.ylab <- 2.5
adj.label <- 0
add_label <- 
  function(x) mtext(side = 3, x, font = 2, cex = cex.label, adj = adj.label)

#### Set up figure 
if(save) png("./fig/val_temp_bottom_results.png",
             height = 8, width = 8.5, units = "in",  res = 600)
pp <- par(mfrow = c(2, 2), 
          oma = c(2, 2, 1, 1), 
          mar = c(3, 3, 3, 3))

#### Histogram of differences
save <- TRUE
pscale <- 1e4
pretty_hist(validation$diff,
            xn = 3,
            xlab = "",
            ylab = "",
            xlim = c(-1, 2),
            cex.lab = cex,
            xaxis = list(at = seq(-1, 2, by = 0.5), cex.axis = cex.axis),
            yaxis = list(at = seq(0, 35000, by = 5000), 
                         labels = add_lagging_point_zero(seq(0, 35000, by = 5000)/pscale, 1),
                         cex.axis = cex.axis, las = TRUE),
            col = scales::alpha("grey", 0.5)
            )
mtext_args <- 
  list(list(side = 1, 
            text = expression(paste(T[M] - T[O], " (", degree, "C)")), 
            cex = cex, 
            line = 2.5),
       list(side = 2, 
            text = expression(paste("Frequency (x", 10^4, ")")), 
            cex = cex, 
            line = line.ylab)
  )
lapply(mtext_args, function(elm) do.call(mtext, elm))
lines(x = c(0, 0), y = c(0, 3.5 * pscale), lty = 3, lwd = 2)
# lines(rep(mean(validation$diff), 2), c(0, 3.5 * pscale), lty = 2)
add_label("A")


#### Time series of observations/predictions 

## Define graphical properties 
paa <- list(side = 1:2,
            pretty = list(n = 5),
            axis = list(list(format = "%b-%y"), list()),
            control_axis = list(cex.axis = cex.axis, las = TRUE)
            )
mta <- 
  list(list(side = 1, "Time (months)", line = 2.5, cex = cex),
       list(side = 2, 
            text = expression(paste("Temperature (T,", degree, "C)")), 
            line = line.ylab, cex = cex)
  )

## Plot observed and expected time series 
axis_ls <-
  pretty_plot(validation$timestamp, validation$obs,
              pretty_axis_args = paa,
              xlab = "", ylab = "",
              mtext_args = mta,
              type = "p", pch = 21,  col = "red", bg = "red", cex = cex.pch,
              )
points(validation$timestamp, validation$wc,  
       pch = 21, col = "black", bg = "black", cex = 0.1)
rug(validation$timestamp, pos = axis_ls[[2]]$lim[1], 
    ticksize = 0.02, lwd = 0.1)

## Add legend
px <- par(xpd = NA)
legend(x = axis_ls[[1]]$lim[1] + (axis_ls[[1]]$lim[2] - axis_ls[[1]]$lim[1])/4, 
       y = axis_ls[[2]]$lim[2],
       xjust = 1,
       pch = c(21, 21),
       col = c("black", "red"),
       pt.bg = c("black", "red"),
       legend = c(expression(T[M]), expression(T[O])),
       cex = cex.axis,
       bty = "n",
       x.intersp = 1
)
par(px)
add_label("B")

#### Plot difference through time
mta[[2]]$text <- expression(paste(T[M]- T[O], " (", degree, "C)"))
axis_ls <-
  pretty_plot(validation$timestamp, validation$diff,
              pretty_axis_args = paa,
              xlab = "", ylab = "",
              mtext_args = mta, 
              type = "p", pch = 21,  col = "black", bg = "black", cex = cex.pch,
  )
lines(axis_ls[[1]]$lim, c(0, 0), lwd = 2, lty = 3, col = "black")
rug(validation$timestamp, pos = axis_ls[[2]]$lim[1], ticksize = 0.02, lwd = 0.1)
add_label("C")


#### Relationship with depth 

## Define dataframe that relates differences to depth 
# Identify the subset of nodes at which there were detections:
nodes_with_detections <- unique(validation$mesh_ID)
# Determine the depth of each node below sea level 
node_depth_mean      <- data.frame(node = nodes_with_detections)
node_depth_mean$mean <- h$h[match(node_depth_mean$node, h$ID)]
node_depth_mean$mean <- abs(node_depth_mean$mean) *-1
table(is.na(node_depth_mean$mean)) 

## Add depths to dataframe
sort(unique(node_depth_mean$node))
sort(unique(validation$mesh_ID))
validation$node_depth_mean <- 
  node_depth_mean$mean[match(validation$mesh_ID, node_depth_mean$node)]

## Plot figure 
yat <- seq(-1, 2, by = 0.5)
ylim <- range(yat)
xat <- seq(0, 150, by = 30)
xlabels <- xat
xlim <- range(xat)
plot(abs(validation$node_depth_mean), validation$diff, 
     axes = FALSE, xlab = "", ylab = "",
     xlim = xlim,
     ylim = ylim,
     type = "p", pch = 21,  col = "black", bg = "black", cex = cex.pch)
lines(xlim, c(0, 0), lwd = 2, lty = 3, col = "black")
axis(side = 1, xat, xlabels, cex.axis = cex.axis, pos = ylim[1])
axis(side = 2, yat, cex.axis = cex.axis, pos = xlim[1], las = TRUE)
mtext(side = 1, "Mean depth (m)", cex = cex, line = 2)
mtext(side = 2, 
      expression(paste(T[M] - T[O], " (", degree, "C)")), 
      cex = cex, line = line.ylab)
add_label("D")


#### Save figures 
par(pp)
if(save) dev.off()


################################
#### Model performance metrics 

#### Distribution of differences
utils.add::basic_stats(validation$diff)

#### Define metric names and tidy table names
metrics <- c("MB", "NMB", "ME", "NME", "RMSE", "NRMSE", "R", "d")
cols    <- 
  data.frame(
    raw = factor(c("n", "o_hat", "m_hat", 
                   "sigma_o", "sigma_m", "r", 
                   "mb", "me", "rmse", "nmb", 
                   "nme", "nrmse", "d"),
                 levels = c("n", "m_hat", "o_hat", 
                            "sigma_m", "sigma_o", 
                            "mb", "nmb", "me", "nme", 
                            "rmse", "nrmse", "r", "d"
                 )),
    pro = c("n", "hat O", "hat M",
            "Sigma O", "Sigma M", 
            "R", "MB", "ME", "RMSE", 
            "NMB", "NME", "NRMSE", "d")) %>%
  dplyr::arrange(raw) %>%
  dplyr::mutate(raw = as.character(raw))

#### Analyse overall model skill
skill <- calc_skill_metrics(validation = validation)
skill <- dplyr::as_tibble(skill)
skill <- skill[, cols$raw]
for(i in 2:ncol(skill)) skill[, i] <- tidy_numbers(skill[, i], digits = 2, ignore = FALSE) 
tidy_write(skill, file = "./fig/val_temp_bottom_results_metrics_overall.txt")

#### Analyse skill by month
## Get skill
skill           <- calc_skill_metrics(validation = validation, "mm_yy")
skill$mm_yy     <- paste0(substr(skill$mm_yy, 4, 8), "-", substr(skill$mm_yy, 1, 2))
skill$date      <- as.Date(paste0(skill$mm_yy, "-01"))
skill$timestamp <- as.POSIXct(paste(skill$date, "00:00:00"), tz = "UTC")
## Write tidy table
skill_tidy           <- dplyr::as_tibble(skill)
skill_tidy           <- skill_tidy[, c("mm_yy", cols$raw)]
colnames(skill_tidy) <- c("Time (months)", cols$pro)
for(i in 3:ncol(skill_tidy)) 
  skill_tidy[, i] <- tidy_numbers(skill_tidy[, i], digits = 2, ignore = FALSE) 
tidy_write(skill_tidy, 
           file = "./fig/val_temp_bottom_results_metrics_by_month.txt")
## Visualise skill
# Set up graphical param 
skill$n_scale <- log10(skill$n)
col_param     <- 
  pretty_cols_brewer(zlim = c(2, 4.5), #range(skill$n_scale), 
                     pal = function(...) viridis::viridis(..., direction = -1))
skill$col     <- col_param$col[findInterval(skill$n_scale, col_param$breaks)]
# Make plot
png("./fig/val_temp_bottom_metrics_by_month.png", 
    height = 6, width = 7, units = "in", res = 300)
pp <- par(mfrow = c(4, 2), oma = c(3, 3, 1, 5.5), mar = c(2, 2, 2, 2))
lapply(1:length(metrics), function(i){
  # i = 1
  pretty_plot(skill$timestamp, skill[, tolower(metrics[i])], 
              pretty = list(list(n = 5), list(n = 4)), 
              axis = list(list(format = "%b-%y"), 
                          list()),
              xlab = "", ylab = "", 
              type = "n"
  )
  nrw <- nrow(skill)
  l0 <- 1:(nrw-1)
  l1 <- 2:nrw
  arrows(x0 = skill$timestamp[l0], 
         y0 = skill[l0, tolower(metrics[i])], 
         x1 = skill$timestamp[l1], 
         y1 = skill[l1, tolower(metrics[i])], 
         col = skill$col[l0], 
         length = 0)
  points(skill$timestamp, skill[, tolower(metrics[i])], 
         pch = 21, col = skill$col, bg = skill$col)
  mtext(side = 2, metrics[i], line = 2.5)
  mtext(side = 3, LETTERS[i], font = 2, adj = 0)
}) %>% invisible()
mtext(side = 1, "Time (months)", outer = TRUE, line = 1)
par(pp)
# Add colour bar (log scale)
xlim <- range(col_param$breaks)
at   <- log10(c(100, 1000, 10000))
xlb  <- c(100, 1000, 10000)
col_param_paa <- pretty_axis(side = 4,
                             lim = list(xlim),
                             axis = list(at = at, labels = xlb, cex.axis = 0.75),
                             control_axis = list(las = TRUE),
                             add = FALSE)
TeachingDemos::subplot(
  add_colour_bar(data.frame(x = col_param$breaks,
                            col = c(col_param$col, NA)),
                 pretty_axis_args = col_param_paa,
                 mtext_args = list(side = 4, 
                                   expression("Number of observations (" * italic(n) * ")"), 
                                   line = 3.1)
  ), 
  x = c(max(skill$timestamp) + 32*24*60*60, 
        max(skill$timestamp)+ 37*24*60*60), 
  y = c(0, 1))
dev.off()

#### Analyse skill by node 
## Get skill
skill <- 
  calc_skill_metrics(validation = validation, "mesh_ID") %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(ID = (1:dplyr::n())-1)
## Write tidy table
skill_tidy           <- dplyr::as_tibble(skill)
skill_tidy           <- skill_tidy[, c("ID", "mesh_ID", cols$raw)]
colnames(skill_tidy) <- c("ID", "Node", cols$pro)
for(i in 4:ncol(skill_tidy)) 
  skill_tidy[, i] <- tidy_numbers(skill_tidy[, i], digits = 2, ignore = FALSE) 
tidy_write(skill_tidy, 
           file = "./fig/val_temp_bottom_results_metrics_by_node.txt")
## Visualise skill
# Set up graphical param 
skill$ID      <- factor(skill$ID)
skill$n_scale <- log10(skill$n)
col_param     <- 
  pretty_cols_brewer(zlim = c(0, 5), #range(skill$n_scale), 
                     pal = function(...) viridis::viridis(..., direction = -1))
skill$col     <- col_param$col[findInterval(skill$n_scale, col_param$breaks)]
# Make plot
png("./fig/val_temp_bottom_metrics_by_node.png", 
    height = 6, width = 7, units = "in", res = 300)
pp <- par(mfrow = c(4, 2), oma = c(3, 3, 1, 5), mar = c(2, 2, 2, 2))
lapply(1:length(metrics), function(i){
  # i = 1
  paa <- list(pretty = list(list(n = 20), list(n = 4)), 
              control_axis = list(las = TRUE))
  skill_for_metric <- skill[, c("ID", tolower(metrics[i]), "col")]
  skill_for_metric <- skill_for_metric[complete.cases(skill_for_metric), ]
  pretty_plot(skill_for_metric$ID, 
              skill_for_metric[, tolower(metrics[i])], 
              pretty_axis_args = paa,
              xlab = "", ylab = "", 
              type = "n")
  points(skill_for_metric$ID, 
         skill_for_metric[, tolower(metrics[i])], 
         pch = 21, 
         col = skill_for_metric$col, 
         bg = skill_for_metric$col)
  mtext(side = 2, metrics[i], line = 2.5)
  mtext(side = 3, LETTERS[i], font = 2, adj = 0)
}) %>% invisible()
mtext(side = 1, "Node ID", outer = TRUE, line = 1)
par(pp)
# Add colour bar (log scale)
xlim <- range(col_param$breaks)
at   <- log10(c(1, 10, 100, 1000, 10000))
xlb  <- c(1, 10, 100, 1000, 10000)
col_param_paa <- pretty_axis(side = 4,
                             lim = list(xlim),
                             axis = list(at = at, labels = xlb, cex.axis = 0.75),
                             control_axis = list(las = TRUE),
                             add = FALSE)
TeachingDemos::subplot(
  add_colour_bar(data.frame(x = col_param$breaks,
                            col = c(col_param$col, NA)),
                 pretty_axis_args = col_param_paa,
                 mtext_args = list(side = 4, 
                                   expression("Number of observations (" * italic(n) * ")"), 
                                   line = 3.25)
  ), 
  x = c(37.25, 37.75), y = c(0, 1))
dev.off()

#### Monthly RMSE
## Get monthly RMSE scores 
rmse <- 
  validation %>%
  dplyr::group_by(mm_yy) %>%
  dplyr::mutate(se = (wc - obs)^2) %>%
  dplyr::summarise(rmse = sqrt(mean(se)))
## Examine ordered RMSE
rmse %>% dplyr::arrange()
## Examine the % improvement from 2016 - 17 
improvement <- (((rmse$rmse[1:4] - rmse$rmse[13:16])/rmse$rmse[1:4]) * 100) %>% sort() 
# 77.09852 81.08708 88.16038 90.68037
mean(improvement)
## Write tidy table with RMSEs to file 
rmse %>% dplyr::select(Month = mm_yy, Score = rmse) %>%
  dplyr::mutate(Score = round(Score, 2), 
                Score = add_lagging_point_zero(Score, 2)) %>%
  tidy_write(file = "./fig/val_temp_bottom_results_rmse_monthly.txt")


#### End of code. 
################################
################################