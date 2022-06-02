################################
################################
#### analyse_temp_bottom.R

#### This script: 
# 1) Analyses model skill for bottom temperatures 
# ... A) Visualises modelled/observed temperatures
# ... B) Analyses trends and patterns in model skill metrics

#### Steps preceding this script: 
# 1) Assemble validation dataset (assemble_temp_bottom.R)


################################
################################
#### Set up 

#### Wipe workspace and source essential packages and variables
source("./R/define_global_param.R")
source("R/define_helpers.R")

#### Load data 
# Spatial fields 
mesh      <- readRDS("./data/spatial/mesh/mesh_around_nodes_bng.rds")
h         <- readRDS("./data/spatial/mesh/mesh_h.rds")
coast     <- readRDS("./data/spatial/coast/coast_bng.rds")
# Validation data 
validation <- readRDS("./data/wc/val_temp_bottom.rds")


################################
################################
#### Data processing

#### Define depth 

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

## Define colour scheme for node depth
# ... Either use split colour scheme or 
# ... follow the scheme in 'val_temp_bottom_effort_depth_map'
use_scheme_split <- FALSE
if(use_scheme_split){
  zlim <- utils.add::round_range(range(abs(validation$node_depth_mean)), digits = 1)
  col_param <- 
    pretty_cols_split_heat(zlim = zlim,
                           scheme_cold = "Reds",
                           scheme_hot = "Blues",
                           select_cold = 2:8,
                           select_hot = 3:8,
                           split = 50)
} else {
  col_param <- 
    pretty_cols_brewer(zlim = c(0, 209.654099),
                       pal = function(x,...) scales::alpha(rev(viridis::viridis(x,...)), 0.75))
}
validation$col_by_depth <- 
  col_param$col[findInterval(abs(validation$node_depth_mean), col_param$breaks)]


################################
################################
#### Visualisation 

#### Local parameters
cex.label <- 1.7
cex       <- 1.5
cex.axis  <- 1.7
cex.pch   <- 0.2
line.ylab <- 2.9
line.xlab <- 3
line.main <- 0.5
adj.label <- 0
add_label <- 
  function(x) mtext(side = 3, x, font = 2, cex = cex.label, adj = adj.label, line = line.main)

#### Set up figure 
save <- TRUE
if(save) png("./fig/val_temp_bottom_results.png",
             height = 10, width = 9, units = "in",  res = 600)
mat <- matrix(c(1, 2, 
                3, 4, 
                5, 5), ncol = 2, byrow = TRUE)
layout(mat)
pp <- par(oma = c(2.5, 2.5, 1, 1), 
          mar = c(3, 3, 3, 3))

#### Histogram of differences
mx <- 1500
pretty_hist(validation$diff,
            xn = 3,
            xlab = "",
            ylab = "",
            xlim = c(-1, 2),
            cex.lab = cex,
            xaxis = list(at = seq(-1, 2, by = 0.5), cex.axis = cex.axis),
            yaxis = list(at = seq(0, mx, by = 300),
                         cex.axis = cex.axis, las = TRUE),
            col = scales::alpha("grey", 0.5)
)
mtext_args <- 
  list(list(side = 1, 
            text = expression(paste(T[M] - T[O], " (", degree, "C)")), 
            cex = cex, 
            line = line.xlab),
       list(side = 2, 
            text = "Frequency", 
            cex = cex, 
            line = line.ylab + 1)
  )
lapply(mtext_args, function(elm) do.call(mtext, elm))
lines(x = c(0, 0), y = c(0, mx), lty = 3, lwd = 2)
# lines(rep(mean(validation$diff), 2), c(0, 3.5 * pscale), lty = 2)
add_label("a")


#### Time series of observations/predictions 

## Define graphical properties 
paa <- list(side = 1:2,
            pretty = list(n = 5),
            axis = list(list(format = "%b-%y"), list()),
            control_axis = list(cex.axis = cex.axis, las = TRUE)
)
mta <- 
  list(list(side = 1, "Time (months)", line = line.xlab, cex = cex),
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
add_label("b")


#### Plot difference through time
mta[[2]]$text <- expression(paste(T[M]- T[O], " (", degree, "C)"))
axis_ls <-
  pretty_plot(validation$timestamp, validation$diff,
              pretty_axis_args = paa,
              xlab = "", ylab = "",
              mtext_args = mta, 
              type = "p", pch = 21,  
              col = validation$col_by_depth, bg = validation$col_by_depth,
              # col = "black", bg = "black", 
              cex = cex.pch)
lines(axis_ls[[1]]$lim, c(0, 0), lwd = 2, lty = 3, col = "black")
rug(validation$timestamp, pos = axis_ls[[2]]$lim[1], ticksize = 0.02, lwd = 0.1)
add_label("c")


#### Relationship with depth 
yat <- seq(-1, 2, by = 0.5)
ylim <- range(yat)
xat <- seq(0, 150, by = 30)
xlabels <- xat
xlim <- range(xat)
plot(abs(validation$node_depth_mean), validation$diff, 
     axes = FALSE, xlab = "", ylab = "",
     xlim = xlim,
     ylim = ylim,
     type = "p", pch = 21,  
     col = validation$col_by_depth, bg = validation$col_by_depth,
     # col = "black", bg = "black", 
     cex = cex.pch)
lines(xlim, c(0, 0), lwd = 2, lty = 3, col = "black")
axis(side = 1, xat, xlabels, cex.axis = cex.axis, pos = ylim[1])
axis(side = 2, yat, cex.axis = cex.axis, pos = xlim[1], las = TRUE)
mtext(side = 1, "Depth (m)", cex = cex, line = line.xlab)
mtext(side = 2, 
      expression(paste(T[M] - T[O], " (", degree, "C)")), 
      cex = cex, line = line.ylab - 0.5)
add_label("d")


#### Plot differences by node 
# Define node IDs (0,...,36)
node_IDs <- 
  validation %>%
  dplyr::group_by(mesh_ID) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::mutate(ID = (1:dplyr::n()) - 1, 
                depth = h$h[match(mesh_ID, h$ID)])
validation$ID <- node_IDs$ID[match(validation$mesh_ID, node_IDs$mesh_ID)]
validation$ID <- factor(validation$ID, levels = node_IDs$ID)
validation    <- validation %>% dplyr::arrange(ID)
# pretty_plot(validation$ID, validation$node_depth_mean, col = validation$col_by_depth)
node_IDs$col_by_depth <- 
  col_param$col[findInterval(abs(node_IDs$depth), col_param$breaks)]

# Make plot 
paa <- list(pretty = list(list(n = 20), list(n = 4)), 
            control_axis = list(las = TRUE, cex.axis = cex.axis))
pretty_boxplot(validation$ID, validation$diff, 
               pretty_axis_args = paa,
               col = node_IDs$col_by_depth,
               xlab = "", ylab = "",
               pch = ".", varwidth = TRUE)
mtext(side = 1, "Node ID", cex = cex, line = line.xlab)
mtext(side = 2, 
      expression(paste(T[M] - T[O], " (", degree, "C)")), 
      cex = cex, line = line.ylab - 1)
add_label("e")


#### Add (depth) legend
fields::image.plot(zlim = sort(col_param$zlim * -1), 
                   breaks = sort(col_param$breaks * -1),
                   col = rev(col_param$col),
                   smallplot = c(0.92, 0.93, 0.2, 0.78),
                   axis.args = list(at = c(0, -50, -100, -150, -200), 
                                    labels = c(0, 50, 100, 150, 200),
                                    cex.axis = 2),
                   legend.only = TRUE)
px <- par(xpd = NA)
text(81, 2, "Depth (m)", cex = 2)
par(px)

#### Save figures 
par(pp)
if(save) dev.off()


################################
################################
#### Summary of 'raw' differences

#### Distribution of differences
utils.add::basic_stats(validation$diff)

#### Distribution of differences by season 
validation %>%
  dplyr::arrange(date) %>%
  dplyr::mutate(year = lubridate::year(date), 
                season = get_season(lubridate::month(date)), 
                season = paste(season, year), 
                season = factor(season, levels = unique(season))) %>%
  dplyr::group_by(season) %>%
  dplyr::summarise(n = dplyr::n(), 
                   min = min(diff), 
                   median = median(diff),
                   max = max(diff), 
  ) %>%
  tidy_numbers(digits = 2) %>%
  tidy_write(file = "./fig/val_temp_bottom_results_summary.txt")

#### Examine correlations between monthly variables (e.g., mean and variance)
monthly_variation <- 
  validation %>%
  dplyr::group_by(mm_yy) %>%
  dplyr::summarise(mu_m = mean(wc), 
                   mu_o = mean(obs),
                   mu_diff = mean(diff),
                   sd_m = sd(wc),
                   sd_o = sd(obs), 
                   n = dplyr::n()
                   )
cor(monthly_variation$sd_o, monthly_variation$n)
cor(monthly_variation$sd_m, monthly_variation$n)
cor(monthly_variation$sd_o, monthly_variation$mu_o)
cor(monthly_variation$sd_m, monthly_variation$mu_m)
cor(monthly_variation$mu_diff, monthly_variation$n)
cor(monthly_variation$mu_diff, monthly_variation$mu_o)
cor(monthly_variation$mu_diff, monthly_variation$mu_m)

#### Examine correlations between nodes 
nodely_variation <- 
  validation %>%
  dplyr::group_by(mesh_ID) %>%
  dplyr::summarise(mu_m = mean(wc), 
                   mu_o = mean(obs),
                   mu_diff = mean(diff),
                   sd_m = sd(wc),
                   sd_o = sd(obs), 
                   n = dplyr::n(),
                   depth = node_depth_mean[1]
  )
cor(nodely_variation$sd_o, nodely_variation$n, use = "pairwise.complete.obs")
cor(nodely_variation$sd_m, nodely_variation$n, use = "pairwise.complete.obs")
cor(nodely_variation$sd_o, nodely_variation$mu_o, use = "pairwise.complete.obs")
cor(nodely_variation$sd_m, nodely_variation$mu_m, use = "pairwise.complete.obs")
cor(nodely_variation$mu_diff, nodely_variation$n, use = "pairwise.complete.obs")
cor(nodely_variation$mu_diff, nodely_variation$mu_o, use = "pairwise.complete.obs")
cor(nodely_variation$mu_diff, nodely_variation$mu_m, use = "pairwise.complete.obs")
cor(nodely_variation$mu_diff, nodely_variation$depth, use = "pairwise.complete.obs")


################################
################################
#### Model skill metrics (overall)

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


################################
################################
#### Model skill metrics (by month)

################################
#### Simulation parameters 

#### Define simulation parameters
## Set seed
set.seed(1)
## Check number of observations available per month 
n_chk <- 
  validation %>%
  dplyr::group_by(mesh_ID, mm_yy) %>%
  dplyr::summarise(n = dplyr::n())
pretty_hist(n_chk$n)
quantile(n_chk$n, seq(0, 1, by = 0.05))
utils.add::basic_stats(n_chk$n)
## Define sample sizes
n_min    <- 5   # minimum n. of observations required to include a node/month combination in the analysis 
n_sim    <- 1000  # n. of iterations 
n_sample <- 5   # n. of samples drawn on each iteration 

#### Define focal node/month combinations for the analysis
pretty_plot(node_IDs$ID, node_IDs$depth)
focals <- 
  validation %>%
  dplyr::group_by(mm_yy, mesh_ID) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::filter(n >= n_min) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(depth = h$h[match(mesh_ID, h$ID)])


################################
#### Analyse skill by month

run <- FALSE
if(run){
  
  #### Generate scores
  ## For each node, for each month, for each simulation, we generate
  # ... calculate model skill metrics.
  sim_scores <- 
    pbapply::pblapply(split(focals, 1:nrow(focals)), cl = 11L, function(focal){
      # focal <- focals[1, , drop = FALSE]
      node  <- focal$mesh_ID
      month <- focal$mm_yy
      skill_by_sim <- 
        lapply(1:n_sim, function(i){
          # Simulate validation dataset
          val <- 
            validation %>% 
            dplyr::filter(mesh_ID %in% node & mm_yy %in% month) %>%
            dplyr::slice_sample(n = n_sample, replace = FALSE)
          stopifnot(nrow(val) == n_sample)
          # Calculate skill metrics for sampled dataset
          skill <- 
            calc_skill_metrics(val) %>%
            dplyr::mutate(node = node, mm_yy = month, sim = i) %>%
            dplyr::select(-n) 
          return(skill)
        }) %>% dplyr::bind_rows()
    }) %>% 
    dplyr::bind_rows() %>%
    dplyr::arrange(node, mm_yy, sim)
  
  #### Save dataframe 
  saveRDS(sim_scores, "./data/wc/val_temp_bottom_sim_scores_by_month.rds")
  
} else {
  sim_scores <- readRDS("./data/wc/val_temp_bottom_sim_scores_by_month.rds")
}

#### Summarise simulated scores for each node
# (via the median score across all simulations for each node/month)
head(sim_scores)
sim_stats <- 
  sim_scores %>% 
  dplyr::group_by(node, mm_yy) %>%
  dplyr::summarise(o_hat   = median(o_hat, na.rm = TRUE), 
                   m_hat   = median(m_hat, na.rm = TRUE), 
                   sigma_o = median(sigma_o, na.rm = TRUE), 
                   sigma_m = median(sigma_m, na.rm = TRUE), 
                   mb      = median(mb, na.rm = TRUE), 
                   me      = median(me, na.rm = TRUE), 
                   rmse    = median(rmse, na.rm = TRUE), 
                   nmb     = median(nmb, na.rm = TRUE), 
                   nme     = median(nme, na.rm = TRUE), 
                   nrmse   = median(nrmse, na.rm = TRUE), 
                   r       = median(r, na.rm = TRUE), 
                   d       = median(d, na.rm = TRUE)) %>%
  dplyr::mutate(mm_yy      = paste0(substr(mm_yy, 4, 8), "-", substr(mm_yy, 1, 2)), 
                date       = as.Date(paste0(mm_yy, "-01")),
                timestamp  = as.POSIXct(paste(date, "00:00:00"), tz = "UTC"), 
                ID         = node_IDs$ID[match(node, node_IDs$mesh_ID)], 
                depth      = node_IDs$depth[match(node, node_IDs$mesh_ID)]) %>%
  data.frame()

#### Summarise simulated scores across all nodes 
sim_stats_avg <- 
  sim_stats %>% 
  dplyr::group_by(mm_yy) %>%
  dplyr::mutate(
    o_hat = mean(o_hat), m_hat = mean(m_hat), 
    sigma_o = mean(sigma_o), sigma_m = mean(sigma_m),
    dplyr::across(all_of(tolower(metrics)), mean, na.rm = TRUE), 
                n_node = length(unique(node))) %>%
  dplyr::select(-node) %>%
  dplyr::slice(1L)
saveRDS(sim_stats_avg, "./data/wc/val_temp_bottom_sim_scores_by_month_summary.rds")
# Save tidy dataframe 
skill_tidy <- 
  sim_stats_avg %>%
  dplyr::select(`Time (months)` = mm_yy, 
                n_node          = n_node, 
                cols$raw[cols$raw %in% colnames(sim_stats_avg)])
colnames(skill_tidy) <- 
  c("Time (months)", "n_node", cols$pro[cols$raw %in% colnames(sim_stats_avg)])
for(i in 3:ncol(skill_tidy)) 
  skill_tidy[, i] <- tidy_numbers(skill_tidy[, i], digits = 2, ignore = FALSE) 
tidy_write(skill_tidy, 
           file = "./fig/val_temp_bottom_results_metrics_by_month.txt")

##### Define depth threshold (optional)
# If defined, update figure names below. 
# shallow < 50; deep >= 50 m
# sim_stats <- sim_stats[sim_stats$depth < 50, ]
# sim_stats <- sim_stats[sim_stats$depth >= 50, ]

#### Visualise simulated scores through time 

## Define graphical parameters
# Colours for node-specific results
col_node <- scales::alpha("dimgrey", 0.25)
# Colours for overall results based on the number of nodes 
nodes_per_month <- 
  sim_stats %>%
  dplyr::group_by(mm_yy) %>%
  dplyr::summarise(nodes = unique(node)) %>%
  dplyr::summarise(n = dplyr::n())
sim_stats$n <- 
  nodes_per_month$n[match(sim_stats$mm_yy, nodes_per_month$mm_yy)]
zlim <- c(min(sim_stats$n), max(sim_stats$n) + 1)
col_param     <- 
  pretty_cols_brewer(zlim = zlim,
                     n_breaks = zlim[2],
                     pal = function(...) viridis::viridis(..., direction = -1))
sim_stats$col     <- col_param$col[findInterval(sim_stats$n, col_param$breaks)]

## Set up figure to save 
save <- TRUE
if(save) png("./fig/val_temp_bottom_metrics_by_month.png", 
             height = 6, width = 7, units = "in", res = 300)
pp <- par(mfrow = c(4, 2), oma = c(3, 3, 1, 5), mar = c(2, 2, 2, 2))

## Loop over metrics and make plot 
lapply(1:length(metrics), function(i){
  ## Define data/parameters for plotting 
  # i = 1
  x <- sim_stats$timestamp
  y <- sim_stats[, tolower(metrics[i])]
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  paa <- list(pretty = list(list(n = 5), list(n = 4)), 
              axis = list(list(format = "%b-%y"), 
                          list()))
  ## Define blank plot 
  pretty_plot(x, y,
              pretty_axis_args = paa,
              xlab = "", ylab = "", 
              type = "n"
  )
  ## Add node-specific time series 
  depth_scale <- 50
  lapply(split(sim_stats, sim_stats$node), function(d) {
    lines(d$timestamp, d[, tolower(metrics[i])], 
          type = "l", 
          lwd = d$depth/depth_scale, col = col_node)
    points(d$timestamp, d[, tolower(metrics[i])],
           cex = 0.5, col = col_node)
  })
  if(i == 1){
    legend("topright", 
           lty = c(1, 1),
           legend = c("50 m", "150 m"), 
           lwd = c(50, 150)/depth_scale, 
           col = col_node,
           cex = 0.75,
           
           box.lty = 3,
           box.col = "grey", 
           box.lwd = 0.5,
           horiz = TRUE
    )
  }
  ## Add the mean score across all nodes, coloured by the number of nodes 
  col <- tolower(metrics[i])
  sim_stats_avg <- 
    sim_stats %>% 
    dplyr::group_by(mm_yy) %>%
    dplyr::mutate(mean = mean(.data[[col]], na.rm = TRUE), 
                  lower = quantile(.data[[col]], 0.25, na.rm = TRUE), 
                  upper = quantile(.data[[col]], 0.75, na.rm = TRUE)) %>%
    dplyr::slice(1L)
  nrw <- nrow(sim_stats_avg)
  l0 <- 1:(nrw-1)
  l1 <- 2:nrw
  arrows(x0 = sim_stats_avg$timestamp[l0], 
         y0 = sim_stats_avg$mean[l0], 
         x1 = sim_stats_avg$timestamp[l1], 
         y1 = sim_stats_avg$mean[l1], 
         col = sim_stats_avg$col[l0], 
         lwd = 1.5,
         length = 0)
  add_error_bars(x = sim_stats_avg$timestamp, 
                 fit = sim_stats_avg$mean, 
                 lwr = sim_stats_avg$lower, 
                 upr = sim_stats_avg$upper, 
                 add_fit = list(pch = 21, col = sim_stats_avg$col, bg = sim_stats_avg$col), 
                 col = sim_stats_avg$col, bg = sim_stats_avg$col, length = 0, 
                 lwd = 1.5,
  )
  ## Add titles 
  mtext(side = 2, metrics[i], line = 2.5)
  mtext(side = 3, letters[i], font = 2, adj = 0, line = 0.5)
  # mtext(side = 3, LETTERS[i], font = 2, adj = 0)
}) %>% invisible()
mtext(side = 1, "Time (months)", outer = TRUE, line = 1)
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
                                   text = "Number of nodes", 
                                   line = 2) # 2 for shallow
  ), 
  x = c(max(sim_stats$timestamp) + 35*24*60*60, # 70*24*60*60 for shallow
        max(sim_stats$timestamp)+ 40*24*60*60), # 75*24*60*60 for shallow
  y = c(0, 1))

if(save) dev.off()

#### Examine the influence of shallow-water nodes on the seasonal pattern 
# ... by comparing ME estimates for the whole dataset versus a subsetted dataset
# ... without shallow-water receivers
# Number of shallow-water nodes over period of interes
sim_stats %>% 
  dplyr::filter(mm_yy %in% c("2016-03", "2016-04","2016-05", "2016-06")) %>%
  dplyr::filter(node %in% node_IDs$mesh_ID[node_IDs$depth < 50]) %>%
  dplyr::group_by(mm_yy) %>%
  dplyr::summarise(unique(node))
# Summary statistics for period of interest across all nodes 
skill_wi_shallow <- 
  sim_stats_avg %>% 
  dplyr::filter(mm_yy %in% c("2016-03", "2016-04","2016-05", "2016-06")) %>%
  dplyr::select(mm_yy, me)
# Summary statistics for period of interest excluding shallow nodes 
skill_wo_shallow <- 
  sim_stats %>% 
  dplyr::filter(mm_yy %in% c("2016-03", "2016-04","2016-05", "2016-06")) %>% 
  dplyr::filter(!(node %in% node_IDs$mesh_ID[node_IDs$depth < 50])) %>%
  dplyr::group_by(mm_yy) %>%
  dplyr::mutate(dplyr::across(all_of(tolower(metrics)), mean)) %>%
  dplyr::select(-node) %>%
  dplyr::slice(1L) %>%
  dplyr::select(mm_yy, me)
(skill_wi_shallow$me - skill_wo_shallow$me) %>% sort()


################################
################################
#### Model skill metrics (improvement)

#### Estimate improvement in model skill 
# Pull out relevant data 
skill_change <- 
  sim_stats_avg %>%
  dplyr::filter(mm_yy %in% c("2016-03", "2016-04", "2016-05", 
                             "2017-03", "2017-04", "2017-05")) %>%
  dplyr::select(mm_yy,n_node, mb, me, rmse, r, d) %>%
  dplyr::arrange(mm_yy) %>%
  data.frame()
# Define a function to calculate improvement 
calc_improvement <- 
  function(x){
    p16 <- 1:3
    p17 <- 4:6
    metric    <- skill_change[, x]
    metric_s <- as.numeric(scale(metric))
    scores <- 
      dplyr::tibble(Metric = c(toupper(x), NA, NA),
                    Period = c("March", "April", "May"), 
                    x_2016 = metric[p16], 
                    x_2017 = metric[p17],
                    s_2016 = metric_s[p16], 
                    s_2017 = metric_s[p17],
                    change = metric[p17] - metric[p16],
                    change_pc = (change/metric[p16]) * 100,
                    change_s = (metric_s[p17] - metric_s[p16]),
                    change_s_pc = (change_s/abs(metric_s[p16])) * 100
                    )
    if(x %in% c("mb", "me", "rmse")){
      scores$change      <-  (metric[p17] - metric[p16]) * -1
      scores$change_pc   <- (scores$change/metric[p16]) * 100
      scores$change_s    <- scores$change_s * -1 
      scores$change_s_pc <- (scores$change_s/metric_s[p16]) * 100
    }
    scores <- rbind(scores, 
                    c(NA_integer_, NA_integer_, 
                      apply(scores[3:ncol(scores)], 2, mean)))
    scores$Period[4] <- "Average"
    return(scores)
  }
# Define a dataframe with improvement scores
metrics_for_improve <- c("mb", "me", "rmse", "r", "d")
improvement <- 
  lapply(metrics_for_improve, function(metric) calc_improvement(metric)) %>%
  dplyr::bind_rows() 
improvement
# Check the average (standardised) improvement across all scores
improvement %>%
  dplyr::filter(!(Period %in% "Average")) %>%
  dplyr::summarise(mean(change_s), mean(change_s_pc)) %>%
  as.numeric()
improvement %>%
  dplyr::slice(1:12) %>%
  dplyr::filter(!(Period %in% "Average")) %>%
  dplyr::summarise(mean(change_s), mean(change_s_pc)) %>%
  as.numeric()
improvement %>%
  dplyr::slice(13:20) %>%
  dplyr::filter(!(Period %in% "Average")) %>%
  dplyr::summarise(mean(change_s), mean(change_s_pc)) %>%
  as.numeric()
# Save tidy file 
for(i in c('x_2016', "x_2017", 
           "s_2016", "s_2017",
           "change", "change_pc", 
           "change_s", "change_s_pc")){
  improvement[, i] <- tidy_numbers(improvement[, i], 2)
}
tidy_write(improvement, 
           "./fig/val_temp_bottom_results_metrics_improvement.txt", 
           na = "")


################################
################################
#### Model skill metrics (by node)

#### Summarise simulated scores across all months for each node  
sim_stats_avg <- 
  sim_stats %>% 
  dplyr::group_by(node) %>%
  dplyr::mutate(    o_hat = mean(o_hat), m_hat = mean(m_hat), 
                    sigma_o = mean(sigma_o), sigma_m = mean(sigma_m),
                    dplyr::across(c(dplyr::everything(), -mm_yy), mean, na.rm = TRUE), 
                    n_mm_yy = length(unique(mm_yy))) %>%
  dplyr::select(-mm_yy, -n) %>%
  dplyr::slice(1L) %>%
  dplyr::ungroup()
# Save tidy dataframe 
skill_tidy <- 
  sim_stats_avg %>%
  dplyr::select(`Node ID` = ID, 
                n_mm_yy          = n_mm_yy, 
                cols$raw[cols$raw %in% colnames(sim_stats_avg)]) %>%
  dplyr::arrange(`Node ID`)
colnames(skill_tidy) <- 
  c("Node ID", "n_time", cols$pro[cols$raw %in% colnames(sim_stats_avg)])
for(i in 3:ncol(skill_tidy)) 
  skill_tidy[, i] <- tidy_numbers(skill_tidy[, i], digits = 2, ignore = FALSE) 
tidy_write(skill_tidy, 
           file = "./fig/val_temp_bottom_results_metrics_by_node.txt")

#### Visualise simulated scores by nodes 

## Define graphical parameters
# Colours for node-specific results
col_node <- scales::alpha("dimgrey", 0.5)
# Colours for overall results based on the number of months  
months_per_node <- 
  sim_stats %>%
  dplyr::group_by(node) %>%
  dplyr::summarise(months = unique(mm_yy)) %>%
  dplyr::summarise(n = dplyr::n())
sim_stats$n <- 
  months_per_node$n[match(sim_stats$node, months_per_node$node)]
zlim <- c(min(sim_stats$n), max(sim_stats$n) + 1)
col_param     <- 
  pretty_cols_brewer(zlim = zlim,
                     n_breaks = zlim[2],
                     pal = function(...) viridis::viridis(..., direction = -1))
sim_stats$col     <- col_param$col[findInterval(sim_stats$n, col_param$breaks)]

## Set up figure to save 
save <- TRUE
if(save) png("./fig/val_temp_bottom_metrics_by_node.png", 
             height = 6, width = 7, units = "in", res = 300)
pp <- par(mfrow = c(4, 2), oma = c(3, 3, 1, 5), mar = c(2, 2, 2, 2))

## Loop over metrics and make plot 
lapply(1:length(metrics), function(i){
  ## Define data/parameters for plotting 
  # i = 1
  x <- sim_stats$ID
  y <- sim_stats[, tolower(metrics[i])]
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  paa <- list(pretty = list(list(NULL), list(n = 4)), 
              units = list(4, NULL))
  ## Define blank plot 
  pretty_plot(x, y,
              pretty_axis_args = paa,
              xlab = "", ylab = "", 
              type = "n")
  ## Add month-specific results 
  points(sim_stats$ID, sim_stats[, tolower(metrics[i])],
         cex = 0.5, col = col_node)
  ## Add the mean score across all months, coloured by the number of months 
  col <- tolower(metrics[i])
  sim_stats_avg <- 
    sim_stats %>% 
    dplyr::group_by(node) %>%
    dplyr::mutate(mean = mean(.data[[col]]), na.rm = TRUE, 
                  lower = quantile(.data[[col]], 0.25, na.rm = TRUE), 
                  upper = quantile(.data[[col]], 0.75, na.rm = TRUE)) %>%
    dplyr::slice(1L)
  cex_scale <- 75
  add_error_bars(x = sim_stats_avg$ID, 
                 fit = sim_stats_avg$mean, 
                 lwr = sim_stats_avg$lower, 
                 upr = sim_stats_avg$upper, 
                 add_fit = list(pch = 21, col = sim_stats_avg$col, bg = sim_stats_avg$col, 
                                cex = sim_stats_avg$depth/cex_scale), 
                 col = sim_stats_avg$col, bg = sim_stats_avg$col, length = 0, 
                 lwd = 1.5,
  )
  if(i == 1){
    legend("topright", 
           legend = c("50 m", "150 m"), 
           pt.cex = c(50, 150)/cex_scale, 
           pch = 21, 
           pt.bg = "black",
           cex = 0.75,
           box.lty = 3,
           box.col = "grey", 
           box.lwd = 0.5,
           horiz = TRUE
           )
  }
  ## Add titles 
  mtext(side = 2, metrics[i], line = 2.5)
  mtext(side = 3, letters[i], font = 2, adj = 0, line = 0.5)
  # mtext(side = 3, LETTERS[i], font = 2, adj = 0)
}) %>% invisible()
mtext(side = 1, "Node ID", outer = TRUE, line = 1)
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
                                   "Number of months", 
                                   line = 2.5)
  ), 
  x = c(max(sim_stats$ID)+0.5, max(sim_stats$ID) + 0.75), y = c(0, 1))

if(save) dev.off()

#### Examine spatial clustering of high skill nodes

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
coast_focal <- raster::crop(coast, ext)

#### Define figure
# Set up to save
save <- TRUE
if(save) png("./fig/val_temp_bottom_metrics_by_node_map.png",
             height = 8, width = 6.5, units = "in",  res = 600)
pp <- par(oma = c(1, 1, 1, 2))
# Define colour bar 
mesh_focal$mb <- sim_stats_avg$mb[match(mesh_focal$ID, as.character(sim_stats_avg$node))]
zlim <- c(-0.5, 1.5) # range(mesh_focal$mb, na.rm = TRUE)
col_param     <- 
  pretty_cols_brewer(zlim = zlim,
                     n_breaks = 100,
                     pal = function(...) viridis::viridis(..., direction = -1))
mesh_focal$col     <- col_param$col[findInterval(mesh_focal$mb, col_param$breaks)]

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
              control_axis = list(las = TRUE, cex.axis = cex.axis - 0.5), 
              control_sci_notation = list(magnitude = 16L, digits = 0))
# Plot and add pretty axes
raster::plot(coast_focal, 
             xlim = xlim, ylim = ylim,
             col = col_land, 
             border = col_border, 
             lwd = 0.25)
# Add spatial fields 
raster::plot(mesh_focal, col = mesh_focal$col, 
             border = "royalblue", lwd = 0.5,
             add = TRUE)
basicPlotteR::addTextLabels(mesh_focal_xy$x, mesh_focal_xy$y, 
                            mesh_focal_xy$ID, 
                            col.label = "black", 
                            cex.pt = 1, lwd = 1.5)

# Add axes and labels 
pretty_axis(axis_ls = axis_ls, add = TRUE)
mtext(side = 1, "Easting", cex = cex, line = 2)
mtext(side = 2, "Northing", cex = cex, line = 2)
# Add north arrow and scale 
add_north_arrow(170097.7, 741356, 
                width = 5000/5, 
                height = 8000/5)
raster::scalebar(5000, xy = c(175000, 723000), 
                 type = "line", 
                 label = c("5 km"))

col_param_paa <- pretty_axis(side = 4,
                             lim = list(x = col_param$zlim),
                             control_axis = list(las = TRUE, cex.axis = cex.axis - 0.5),
                             add = FALSE)
TeachingDemos::subplot(
  add_colour_bar(data.frame(x = col_param$breaks,
                            col = c(col_param$col, NA)),
                 pretty_axis_args = col_param_paa,
                 mtext_args = list(side = 4, 
                                   expression("MB (" * degree * "C)"), 
                                   line = 3.75, cex = cex)
  ), 
  x = c(181500, 182000), y = c(725000, 740000))

if(save) dev.off()

#### Examine ensemble-average skill metrics in relation to depth 
mod <- lm(mb ~ depth, data = sim_stats_avg)
summary(mod)
png("./fig/val_temp_bottom_metrics_by_node_with_depth.png", 
    height = 5, width = 5, units = "in", res = 600)
sim_stats_avg$col     <- col_param$col[findInterval(sim_stats_avg$mb, col_param$breaks)]
pretty_predictions_1d(mod, 
                      add_xlab = list(text = "Depth (m)", line = 2), 
                      add_ylab = list(text = expression("MB (" * degree * "C)"), line = 2),
                      add_main = NULL,
                      add_points = list(pch = 21,
                                        bg = sim_stats_avg$col, 
                                        col = sim_stats_avg$col
                                        ))
sim_stats_avg$id <- node_IDs$ID[match(sim_stats_avg$node, node_IDs$mesh_ID)]
basicPlotteR::addTextLabels(sim_stats_avg$depth, sim_stats_avg$mb, 
                            sim_stats_avg$id, 
                            col.label = "black", 
                            cex.pt = 1.25)
dev.off()


#### End of code. 
################################
################################