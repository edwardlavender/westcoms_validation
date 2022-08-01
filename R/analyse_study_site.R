################################
################################
#### analyse_study_site.R

#### This script: 
# 1) Maps the study site 
# 2) Analyses spatial variation in bottom temperatures across the study site. 

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
mesh_mpa  <- readRDS("./data/spatial/mesh/mesh_around_nodes_in_mpa.rds")
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
pp <- par(oma = c(1, 1, 1, 1), mgp = c(3, 0.5, 0.5))

#### Define pretty axes 
xat <- c(xlim[1], 140000, 160000, 180000, 200000)
xlb <- c("", "140000", "160000", "180000", "200000")
yat <- c(ylim[1], 700000, 720000, 740000, 760000, ylim[2])
ylb <- c("", "700000", "720000", "740000", "760000", "")
axis_ls <- 
  pretty_axis(side = 1:4,
              x = list(xat, yat),
              axis = list(list(at = xat),
                          list(at = yat),
                          list(at = xat),
                          list(at = yat)), 
              control_axis = list(las = TRUE, cex.axis = cex.axis, lwd.ticks = 0, labels = FALSE))

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
add_sp_grid_ll(coast_site, 
               ext = raster::extent(axis_ls[[1]]$lim, axis_ls[[2]]$lim)
               )

#### Add validation locations
# Bottom temperature validation
length(unique(paste0(val_tb$long, "-", val_tb$lat)))
val_tb_sp <- sp::SpatialPoints(val_tb[, c("long", "lat")], wgs84)
val_tb_sp <- sp::spTransform(val_tb_sp, bng)
points(val_tb_sp, pch = 4, cex = 0.75)
# Temperature-depth profile validation
length(unique(val_tp$event_id))
val_tp_sp <- 
  val_tp %>% 
  dplyr::group_by(event_id) %>% 
  dplyr::slice(1L) %>%
  dplyr::ungroup() %>%
  dplyr::select(x = long, y = lat) %>%
  sp::SpatialPoints(proj = wgs84) %>%
  sp::spTransform(CRSobj = bng)
raster::coordinates(val_tp_sp)
points(val_tp_sp, pch = 3,  cex = 0.75, lwd = 1.5, col = "red")
legend(182000, 775000,
       pch = c(4, 3), 
       pt.cex = c(0.75, 0.75), 
       lwd = c(1, 1.5), 
       lty = c(0, 0),
       col = c("black", "red"), 
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


################################
################################
#### Temperature variation across the MPA 

################################
#### Extract temperatures 

#### Define data for extraction 
dates       <- utils.add::seq_range(range(val_tb$date), "days")
date_names  <- date_name(dates)
hours       <- 0:23
layers      <- 10
mesh_IDs    <- as.integer(as.character(mesh_mpa$ID))
wc <- 
  expand.grid(date_name = date_names, 
              hour = hours, 
              layer = layers, 
              mesh_ID = mesh_IDs) %>%
  dplyr::arrange(date_name, 
                 hour, 
                 mesh_ID)
str(wc)

#### Implement extraction (~3.28 hrs)
run <- FALSE
if(run){
  # Implement extraction
  t1 <- Sys.time()
  wc <- fvcom.tbx::extract(dat = wc, # [1:10, ], 
                           dir2load = wc_con, 
                           extension = ".mat", 
                           cl = parallel::makeCluster(10L)
                           )
  t2 <- Sys.time()
  # Define time stamps 
  wc$date           <- date_name(wc$date_name, define = "date")
  wc$hour_char      <- as.character(wc$hour)
  pos               <- wc$hour < 10
  wc$hour_char[pos] <- paste0("0", wc$hour_char[pos])
  wc$timestamp <- fasttime::fastPOSIXct(paste0(wc$date, " ", wc$hour_char, ":00:00"), tz = "UTC")
  # Save 
  saveRDS(wc, "./data/wc/ss_temp_bottom.rds")
} else {
  wc <- readRDS("./data/wc/ss_temp_bottom.rds")
}


################################
#### Spatial variation 

#### Generate summary statistics (~10 s)
run <- FALSE
if(run){
  t1 <- Sys.time()
  wc_stats <-
    wc %>% 
    dplyr::group_by(timestamp) %>%
    dplyr::summarise(median = median(wc),
                     mad    = mad(wc), 
                     sd     = sd(wc),
                     iqr    = IQR(wc))
  t2 <- Sys.time()
  difftime(t2, t1)
  saveRDS(wc_stats, "./data/wc/ss_temp_bottom_stats.rds")
} else {
  wc_stats <- readRDS("./data/wc/ss_temp_bottom_stats.rds")
}

#### Summarise summary statistics
median(wc_stats$mad)
median(wc_stats$sd)
median(wc_stats$iqr)
# 0.3900328

#### Visualise summary statistics
png("./fig/ss_temp_bottom_iqr.png", 
    height = 4, width = 6, units = "in", res = 600)
pretty_plot(wc_stats$timestamp, wc_stats$iqr, 
            pretty_axis_args = list(axis = list(list(format = "%b-%y"), 
                                                list())),
            xlab = "", ylab = "",
            type = "l")
mtext(side = 1, "Time (months)", line = 2.4)
mtext(side = 2, expression("IQR (" * degree * "C)"), line = 2)
dev.off()


################################
#### Temporal variation (daily ranges)

#### Calculate daily ranges through time (~10 s)
t1 <- Sys.time()
wc_stats <- 
  wc %>%
  dplyr::group_by(date, mesh_ID) %>%
  dplyr::summarise(rng = max(wc) - min(wc))
t2 <- Sys.time()
difftime(t2, t1)

#### Summarise daily ranges at each node across nodes 
wc_stats_avg <- 
  wc_stats %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(min    = min(rng), 
                   q25    = quantile(rng, 0.25),
                   median = median(rng),
                   q75    = quantile(rng, 0.75),
                   max    = max(rng)) %>%
  dplyr::mutate(date = as.Date(date))

#### Visualise daily ranges through time 
## Set up plot
save <- TRUE
if(save) png("./fig/ss_temp_bottom_daily_ranges.png", 
             height = 4, width = 6, units = "in", res = 600)
## Define blank plot 
pretty_plot(wc_stats_avg$date, wc_stats_avg$median, 
            pretty_axis_args = list(
              x = list(x = range(wc_stats_avg$date), 
                       y = range(c(wc_stats_avg$min, wc_stats_avg$max))), 
              axis = list(list(format = "%b-%y"), 
                          list())
            ),
            xlab = "", ylab = "",
            type = "n")
## Add summary statistics 
# Add range 
polygon(x = c(wc_stats_avg$date, rev(wc_stats_avg$date)), 
        y = c(wc_stats_avg$min, rev(wc_stats_avg$max)), 
        col = scales::alpha("grey", 0.5), border = FALSE)
# Add IQR
polygon(x = c(wc_stats_avg$date, rev(wc_stats_avg$date)), 
        y = c(wc_stats_avg$q25, rev(wc_stats_avg$q75)), 
        col = scales::alpha("dimgrey", 0.5), border = FALSE)
lines(wc_stats_avg$date, wc_stats_avg$median, lwd = 2)
## Add monthly ME score for comparison 
skill <- readRDS("./data/wc/val_temp_bottom_sim_scores_by_month_summary.rds")
skill$date <- as.Date(skill$date)
lines(zoo::rollmean(skill$date, 2), skill$me[1:(nrow(skill)-1)], 
      type = "b",
      pch = 21, col = "red4", bg = "red4", cex = 0.5,
      lwd = 2, lty = 3)
## Add legend
legend(as.Date("2016-01-20"), 6, 
       pch = c(22, 22, NA, 21), 
       col = c(NA, NA, "black", "red4"), 
       pt.bg = c(scales::alpha("grey", 0.5), scales::alpha("dimgrey", 0.5), NA, "red4"),
       lty = c(0, 0, 1, 3), 
       lwd = c(1, 1, 2, 2),
       legend = c("R", "IQR", "Median", "ME"), 
       bty = "n")
## Add axis titles 
mtext(side = 1, "Time (months)", line = 2)
mtext(side = 2, expression(Delta * T ~ "(" * degree * "C)"), line = 1.5)
if(save) dev.off()


#### End of code. 
################################
################################