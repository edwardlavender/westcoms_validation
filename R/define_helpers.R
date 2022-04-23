################################
################################
#### helpers.R

#### This code:
# 1) Contains helper functions for other scripts. 


################################
################################
#### calc_skill_metrics()

#### Define a metric to calculate model skill metrics 
# This function is designed for the bottom temperature validation dataset
# ... in analyse_bottom_temp.R. Using the validation dataset inputted, 
# ... the function calculates a set of standard model skill metrics
# ... either across the whole time series or each grouping level 
# ... (e.g., month/year category or node) if 'group' 
# ... (a character variable of a column in 'validation' used for grouping) 
# ... is specified. 

calc_skill_metrics <- function(validation, group = NULL){
  stopifnot(c("wc", "obs") %in% colnames(validation))
  if(!is.null(group)){
    validation <- 
      validation %>% 
      dplyr::group_by(.data[[group]]) 
  }
  validation %>%
    dplyr::mutate(error = wc - obs, 
                  error_sq = error^2) %>%
    dplyr::mutate() %>%
    dplyr::summarise(
      # n 
      n    = dplyr::n(),
      # mean obs and wc
      o_hat = mean(obs),
      m_hat = mean(wc),
      # sigma obs and wc, 
      sigma_o = sd(obs),
      sigma_m = sd(wc),
      # correlation coefficient (Pearson's r: Aleynik, Pablo)
      r    = cor(wc, obs),
      # mean difference ('mean bias': Pablo)
      mb   = mean(error), 
      # mean absolute difference ('mean error')
      me = mean(abs(error)),
      # rms difference ('rmse': Aleynik, Pablo)
      rmse = sqrt(mean(error_sq)), 
      # normalised mean difference ('normalised mean bias')
      nmb = mb/mean(obs),
      # normalised mean absolute difference ('normalised mean error')
      nme = mean(me)/mean(obs),
      # normalised RMSE
      nrmse = rmse/mean(obs),
      # Index of Agreement
      d = 1 - (sum(error_sq)/(sum((abs(wc - mean(obs)) + abs(obs - mean(obs)))^2))),
    ) %>% data.frame()
}


#### End of code. 
################################
################################