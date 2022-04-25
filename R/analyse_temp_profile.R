################################
################################
#### analyse_temp_profile.R

#### This script: 
# 1) Analyses observed versus modelled temperature-depth profiles 

#### Steps preceding this script: 
# 1) Assemble observed/modelled datasets (assemble_temp_profile.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")

#### Load data
obs   <- readRDS("./data/wc/val_temp_profile_obs.rds")
preds <- readRDS("./data/wc/val_temp_profile_pred.rds")


################################
################################
#### Compare observations and predictions 

#### Plotting parameters for all plots
cex      <- 1.3
cex.axis <- 1.3
cex.pch  <- 0.2

#### Negate depths for visualisation 
obs$depth_neg         <- obs$depth*-1
preds$depth_layer_neg <- preds$depth_layer*-1

#### Plot observed/predicted temperature depth profiles for each event 
# Plotting param across all events 
x_rng <- range(c(obs$temp, preds$wc))
y_rng <- c(-200, 0)
# Split lists
obs      <- obs %>% dplyr::arrange(date_time)
obs_ls   <- split(obs, f = factor(obs$event_id, levels = unique(obs$event_id)))
preds_ls <- split(preds, f = preds$event_id)
preds_ls <- preds_ls[names(obs_ls)]
names(obs_ls)
names(preds_ls)
# Define colours for observed and predicted depth-temperature profiles
colo <- scales::alpha("red", 0.9)
colp <- scales::alpha("black", 0.9)
# Set plotting window
save <- TRUE
if(save) png("./fig/val_temp_profile_results_profiles.png", 
             height = 7, width = 9, units = "in", res = 600)
pp <- par(mfrow = c(2, 4), oma = c(1, 5, 5, 1), mar = c(1, 3.5, 4.75, 2))
# Loop over each recapture event
mapply(obs_ls, preds_ls, letters[1:length(obs_ls)], FUN = function(o, p, main){
  
  #### Ensure we are dealing with the same event
  stopifnot(all(o$event_id %in% p$event_id) & all(p$event_id %in% o$event_id))
  
  #### Define axes properties 
  temp_lwr <- floor(min(o$temp))
  lim <- list(c(temp_lwr, temp_lwr+2), c(-200, 0)) 
  axis_ls <- pretty_axis(side = c(3, 2), 
                         lim = lim,
                         axis = list(las = TRUE, cex.axis = cex.axis)
                         )
  
  #### Define plot  
  # Plot observed depth-temperature profile 
  pretty_plot(o$temp, o$depth_neg, 
              pretty_axis_args = list(axis_ls = axis_ls),
              xlab = "", ylab = "",
              type = "l", col = colo)
  points(o$temp, o$depth_neg, pch = 21, col = colo, bg = colo)
  # Add predicted profile 
  points(p$wc, p$depth_layer_neg, type = "p", pch = 21, col = colp, bg = colp)
  lines(p$wc, p$depth_layer_neg, type = "l", col = colp, bg = colp)
  
  #### Add titles 
  day <- as.Date(o$date_time[1])
  day <- format(day, "%Y-%m-%d")
  mtext(side = 3, text = main, cex = cex.axis-0.2, adj = -0.02, line = 2.75, font = 2)
  mtext(side = 3, text = paste0("   (", day, ")"), cex = cex.axis-0.2, adj = -0.02, line = 2.75)
  
  #### Add legend
  if(main == "a"){
    legend(x = 13.75, -150,
           xjust = 1,
           pch = c(21, 21),
           col = c("red", "black"),
           pt.bg = c("red", "black"),
           legend = c(expression(T[o]), expression(T[p])),
           cex = cex.axis,
           bty = "n",
           x.intersp = 1
    )
  }
  
  #### Add box
  box("figure", lwd = 0.25, col = "dimgrey")
  
}) %>% invisible()

#### Add global axes titles 
mtext(side = 3, expression(paste("Temperature (T,", degree, "C)")), 
      cex = cex, line = 1.5, outer = TRUE)
mtext(side = 2, "Depth (m)", cex = cex, outer = TRUE, line = 2)
par(pp)
if(save) dev.off()

#### Comments:
# Recapture event 8, individual 39, dst 1511, starting 13:39:00
# ... stops before the surface. This is because any data after this are missing 
# ... in the raw data (see 1511_A.xlxs).


#### End of code. 
################################
################################