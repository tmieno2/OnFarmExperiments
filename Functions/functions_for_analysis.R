
get_ttest_text <- function(test_results, zone){

  t <- test_results[zone_txt == paste0("Zone ", zone), t]

  if (t < 1.30){
    temp_text <- "The data and model provide negligible evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K does not provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else if (1.30 <= t & t < 1.64){
    temp_text <- "The data and model provide only limited evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else if (1.64 <= t & t < 1.96){
    temp_text <- "The data and model provide moderate evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)."
  } else {
    temp_text <- "The data and model provide strong evidence that the estimated optimal rate of `r get_seed(\"Optimal\", zone)`K did indeed provide greater profits than the grower-chosen rate of grower_chosen_rate_hereK (t-value of `r get_t_value(zone)`)." 
  }

  return(gsub("zone", zone, temp_text))
}

#/*=================================================*/
#' # Yield and Profit Predictions
#/*=================================================*/

# est <- data_analysis_s$gam_res[[1]]
# data <- data_analysis_s$data[[1]]
# var_name <- "seed_rate"

# data_dt[, n_rate := seq(1:nrow(data_dt))]

# all_var_names_ls_ls <- c(all_var_names_ls_ls, "n_rate")
# est$model$n_rate <- 1

make_data_for_eval <- function(data, est) {

# data <- analysis_res_gcr$data[[1]]
# est <- analysis_res_gcr$gam_res[[1]]

  data_dt <- data.table(data)

  var_names_ls <- est$model %>% 
    data.table() %>% 
    dplyr::select(- any_of(c("input_rate", "yield"))) %>%
    names() 

  data_for_eval <- data_dt[, ..var_names_ls] %>% 
    .[, lapply(.SD, mean), by = zone_txt]

  return(data_for_eval)

}

make_eval_data_gc <- function(data, data_for_eval, gc_type, w_gc_rate) {

  if (gc_type == "Rx") {
    #=== individual plot data ===#
    data_for_eval <- data %>% 
      .[, input_rate := NULL] %>% 
      #=== designate gc_rate as input_rate for prediction ===#
      .[, input_rate := gc_rate]  
  } else {
    #=== data by zone ===#
    data_for_eval <- data_for_eval %>% 
      #=== designate gc_rate as input_rate for prediction ===#
      .[, gc_rate := w_gc_rate]
  }

  return(data_for_eval)
}

# input_rate_seq <- analysis_res_p$input_rate_seq[[1]]
# data_for_eval <- analysis_res_p$data[[1]]

predict_yield_range <- function(data_for_eval, input_rate_seq, est) {

  eval_data <- data_for_eval[input_rate_seq, on = "zone_txt"]  

  #--- predict yield ---#
  yield_prediction <- predict(est, newdata = eval_data, se = TRUE)

  eval_data <- eval_data %>% 
    .[, `:=`(
      yield_hat = yield_prediction$fit,
      yield_hat_se = yield_prediction$se.fit
    )] 

  return(eval_data)

}

predict_yield <- function(data, est, var_name) {

  eval_data <- data.table::copy(data) %>% 
    dplyr::select(-input_rate) %>% 
    setnames(var_name, "input_rate")

  yield_prediction <- predict(est, newdata = eval_data, se = TRUE)

  #--- predict yield ---#
  eval_data <- eval_data %>% 
  mutate(
    yield_hat = yield_prediction$fit,
    yield_hat_se = yield_prediction$se.fit
  )
   
  return(eval_data)

}

#/*=================================================*/
#' # Profit-differential test 
#/*=================================================*/
# Notes: test if the profit associated with the optimal and grower-chosen
# rates are statistically significantly different from zero 

get_dif_stat <- function(data, test_var, opt_var, gc_var, gam_res, crop_price, input_price){
    
  if ("scam" %in% class(gam_res)) {
    gam_coef <- gam_res$coefficients.t
    gam_V <- gam_res$Ve.t
  } else {
    gam_coef <- gam_res$coefficients
    gam_V <- gam_res$Ve
  }

  base_data <- data.table::copy(data) %>% 
    .[, (test_var) := get(gc_var)]

  comp_data <- data.table::copy(data) %>% 
    .[, (test_var) := get(opt_var)]

  #/*----------------------------------*/
  #' ## Profit (gc)
  #/*----------------------------------*/
  Xmat_base <- predict(gam_res, newdata = base_data, type = "lpmatrix") 
  # predict(gam_res, newdata = base_data) %>% mean

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(Xmat_base)[1], 1, dim(Xmat_base)[1])

  #--- average yield ---#
  yhat_base <- ones %*% Xmat_base %*% gam_coef

  #--- point estimate of profit differential ---#
  pi_gc <- crop_price * yhat_base - (input_price * ones %*% base_data$input_rate)  

  big_mat_base <- ones %*% Xmat_base

  #--- se of the profit differential  ---# 
  pi_gc_se <- crop_price * sqrt(big_mat_base %*% gam_V %*% t(big_mat_base))

  #/*----------------------------------*/
  #' ## Profit (optimal)
  #/*----------------------------------*/
  Xmat_comp <- predict(gam_res, newdata = comp_data, type = "lpmatrix") 

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(Xmat_comp)[1], 1, dim(Xmat_comp)[1])

  #--- average yield ---#
  yhat_comp <- ones %*% Xmat_comp %*% gam_coef

  #--- point estimate of profit differential ---#
  pi_opt <- crop_price * yhat_comp - (input_price * ones %*% comp_data$input_rate)  

  big_mat_comp <- ones %*% Xmat_comp

  #--- se of the profit differential  ---# 
  pi_opt_se <- crop_price * sqrt(big_mat_comp %*% gam_V %*% t(big_mat_comp))

  #/*----------------------------------*/
  #' ## Profit differential
  #/*----------------------------------*/
  #--- difference in X mat ---#
  X_dif_mat <- Xmat_comp - Xmat_base

  #--- vector of 1s for summation divided by the number of observations for averaging ---#
  ones <- matrix(1 / dim(X_dif_mat)[1], 1, dim(X_dif_mat)[1])

  #--- X_dif_mat summed ---#
  big_mat_dif <- ones %*% X_dif_mat

  #--- point estimate of profit differential ---#
  pi_dif <- ones %*% ((crop_price * X_dif_mat %*% gam_coef) - input_price * (comp_data$input_rate - base_data$input_rate))  

  #--- se of the profit differential  ---# 
  pi_dif_se <- crop_price * sqrt(big_mat_dif %*% gam_V %*% t(big_mat_dif))

  #--- t-stat ---#
  t_stat <- (pi_dif/pi_dif_se) %>% round(digits = 2)  

  return_data <- data.table(
    yhat_est_gc = yhat_base[1, 1],
    point_est_gc = pi_gc[1, 1],
    point_est_gc_se = pi_gc_se[1, 1],
    yhat_est_opt = yhat_comp[1, 1],
    point_est_opt = pi_opt[1, 1],
    point_est_opt_se = pi_opt_se[1, 1],
    point_est_dif = pi_dif[1, 1],
    point_est_dif_se = pi_dif_se[1, 1],
    t = t_stat[1, 1]
  )

  return(return_data)

}

# data = analysis_res_o$data[[1]] %>% data.table()
# gam_res = analysis_res_o$gam_res[[1]]
# input_price = analysis_res_o$price[[1]]

get_dif_stat_zone <- function(data, test_var, opt_var, gc_var, gam_res, by_var, crop_price, input_price){

  zone_ls <- data[, ..by_var] %>% 
    unique() %>% 
    unlist()

  temp_data <- setnames(data.table::copy(data), by_var, "by_var")

  # y <- zone_ls[1]

  return_data <- lapply(
    zone_ls,
    function(y) 
    get_dif_stat(
      data = temp_data[by_var == y, ] %>% 
        setnames("by_var", by_var),
      test_var,
      opt_var,
      gc_var,
      gam_res,
      crop_price,
      input_price
    ) %>% 
    mutate(by_var = y)
  ) %>% 
  rbindlist() %>% 
  setnames("by_var", by_var)

  return(return_data)

}

# data <- analysis_res_o$data[[1]]
# gc_type <- analysis_res_o$gc_type[[1]]
# gam_res <- analysis_res_o$gam_res[[1]]
# crop_price <- analysis_res_o$crop_price[[1]]
# input_price <- analysis_res_o$price[[1]]

get_pi_dif_test_zone <- function(data, gc_type, gam_res, crop_price, input_price) {

  if (gc_type == "uniform") {

    data_for_test <- data.table(data)

    pi_dif_test_zone <- get_dif_stat_zone(
      data = data_for_test, 
      test_var = "input_rate", 
      opt_var = "opt_input",
      gc_var = "gc_rate",
      gam_res = gam_res,
      by_var = "zone_txt",
      crop_price = crop_price,
      input_price = input_price
    )

  } else {

    pi_dif_test_zone <- get_dif_stat_zone(
      data = data.table(data), 
      test_var = "input_rate", 
      opt_var = "opt_input",
      gc_var = "gc_rate",
      gam_res = gam_res,
      by_var = "zone_txt",
      crop_price = crop_price,
      input_price = input_price
    )

  }

  return(pi_dif_test_zone)
}

find_opt_u <- function(data, gam_res, crop_price, input_price) {

  data_dt <- data.table(data)

  input_ls <- seq(
    quantile(data_dt$input_rate, prob = 0.025), 
    quantile(data_dt$input_rate, prob = 0.975), 
    length = 100
  )

  opt_input_u <- data_dt %>% 
  # this is okay because each zone has the same
  # number of observations
  unique(by = "zone_txt") %>% 
  .[rep(1:nrow(.), length(input_ls)), ] %>% 
  .[, input_rate := rep(input_ls, each = nrow(.)/length(input_ls))] %>% 
  .[, yield_hat := predict(gam_res, newdata = .)] %>% 
  .[, profit_hat := crop_price * yield_hat - input_price * input_rate] %>% 
  .[, .(profit_hat = mean(profit_hat)), by = input_rate] %>% 
  .[order(profit_hat), ] %>% 
  .[.N, input_rate]

  return(opt_input_u)

} 

#/*=================================================*/
#' # filter data 
#/*=================================================*/

find_field_vars <- function(data_sf) {

  #/*----------------------------------*/
  #' ## pick field vars
  #/*----------------------------------*/
  #=== keep only the ones that are available ===#
  field_var_ls <- c(
    #=== topography ===#
    # "twi", 
    "tpi", "elevation", "slope", 
    #=== ssurgo ===#
    "clay", "sand", "water_storage",
    #=== ec ===#
    "ecs", "om", "cec", "ec02"
  ) %>% 
  .[. %in% names(data_sf)]

  #=== find variables to keep ===#
  keep_vars <- data_sf[, field_var_ls] %>% 
    st_drop_geometry() %>% 
    #=== if missing for more than 10%, then drop ===#
    data.table() %>% 
    .[, lapply(
      .SD, 
      function(x) {
        (sum(is.na(x))/nrow(data_sf)) < 0.1
      }
    )] %>% 
    as.matrix() %>% 
    as.vector()

  field_var_ls <- field_var_ls[keep_vars]

  return(field_var_ls)
}

# data_sf <- analysis_res$data[[1]]
# field_var_ls <- analysis_res$field_vars[[1]]

gen_y_res <- function (data_sf, field_var_ls){

  if (length(field_var_ls) == 0) {
    data_sf <- mutate(data_sf, res_y = yield)
    return(data_sf)
  } else {
    #/*----------------------------------*/
    #' ## Regress yield on characteristics 
    #/*----------------------------------*/
    field_vars_f <- field_var_ls %>% 
    .[. %in% names(data_sf)] %>% 
    paste(., collapse = " + ")

    #--- regression formula ---#
    y_field_formula <- formula(
      paste(
        #=== main ===#
        "yield ~ ",
        #=== field/soil char ===#
        field_vars_f
      )
    )

    data_sf <- data_sf %>% 
      .[, c("obs_id", "yield", "input_rate", field_var_ls, "X", "Y")] %>% 
      .[complete.cases(st_drop_geometry(.)), ] %>% 
      mutate(
        res_y = lm(y_field_formula, data = data_sf)$res
      )

    return(data_sf)
  }
}

#/*=================================================*/
#' # Run scam or gam
#/*=================================================*/
# data <- analysis_res_g$data[[1]]
# field_vars <- analysis_res_m$field_vars[[1]]

run_scam_gam <- function(data, field_vars){

  results <- NULL
  
  # results <- tryCatch(
  #     {
  #       withTimeout(
  #         {
  #           formula <- paste0(
  #             "yield ~ s(input_rate, bs = \"cv\", by = zone_txt) + s(X, k = 5) + s(Y, k = 5) + te(X, Y, k = c(5, 5))",
  #             ifelse(
  #               length(field_vars) > 0,
  #               paste0(" + ", paste0(field_vars, collapse = " + ")),
  #               ""
  #             )
  #           ) %>% formula()

  #           scam(formula, data = data)
  #         },
  #         timeout = 20, # 20 seconds,
  #         onTimeout = "silent"
  #       )
  #     }, 
  #     error = function(cond){
  #       return(NULL)
  #     }
  #   )

  # #=== start with 6 knots ===#
  # start_k <- 6

  # while (is.null(results) & start_k >= 5) {

  #   results <- tryCatch(
  #     {
  #       withTimeout(
  #         {
  #           formula <- paste0(
  #             "yield ~ s(input_rate, k = ",
  #             start_k,
  #             ", bs = \"cv\", by = zone_txt) + s(X, k = 5) + s(Y, k = 5) + te(X, Y, k = c(5, 5))",
  #             ifelse(
  #               length(field_vars) > 0,
  #               paste0(" + ", paste0(field_vars, collapse = " + ")),
  #               ""
  #             )
  #           ) %>% formula()

  #           scam(formula, data = data)
  #         },
  #         timeout = 20, # 20 seconds,
  #         onTimeout = "silent"
  #       )
  #     }, 
  #     error = function(cond){
  #       return(NULL)
  #     }
  #   )

  #   start_k <- start_k - 1 

  # }

  if (is.null(results)) {

    formula <- paste0(
      "yield ~ s(input_rate, k = 4, by = zone_txt) + s(X, k = 5) + s(Y, k = 5) + te(X, Y, k = c(5, 5))",
      ifelse(
        length(field_vars) > 0,
        paste0(" + ", paste0(field_vars, collapse = " + ")),
        ""
      )
    ) %>% formula()

    results <- gam(formula, data = data)

  }

 return(results) 

}

#/*=================================================*/
#' # GWR-analysis
#/*=================================================*/

# var_name <- "seed_rate"
# data_sf <- rename(data_sf, input_rate = s_rate)

run_gwr <- function(data_sf, var_name) {

  reg_data_sp <- data_sf %>%
    as("Spatial")

  # library(matrixcalc)
  # is.singular.matrix(dMat)

  #--- find optimal bandwidth ---#
  dMat <- data_sf %>% 
    st_centroid() %>% 
    st_coordinates() %>% 
    as.matrix() %>% 
    gw.dist()

  reg_formula <- formula("res_y ~ log(input_rate)")

  # obw <- bw.gwr(
  #   reg_formula,
  #   data = reg_data_sp,
  #   approach = "AICc",
  #   kernel = "gaussian",
  #   adaptive = T,
  #   dMat = dMat
  # )

  #--- gwr estimation with optimal bw ---#
  gwr_est <- gwr.basic(
    reg_formula,
    data = reg_data_sp,
    bw = 100,
    # bw = obw,
    kernel = "gaussian",
    adaptive = T
  )

  #--- join the coefficients to the sf ---#
  data_sf <- data.table(
    obs_id = data_sf$obs_id,
    b_int = gwr_est$SDF$Intercept,
    b_slope = gwr_est$SDF@data[, paste0("log(", var_name, ")")]
  ) %>%
  left_join(data_sf, ., by = "obs_id") 

  # ggplot(data = data_sf) +
  #   geom_histogram(aes(x = b_slope))

  return(data_sf)

}



define_mz <- function(data_sf, max_num_zones, min_obs) {

  #--- number of zones ---#
  num_zones <- min(floor(nrow(data_sf) / min_obs), max_num_zones)

  #--- grouping the data into 5 groups based on beta ---#
  data_sf <- data_sf %>% 
  mutate(
    zone = cut(
      b_slope, 
      breaks = quantile(b_slope, prob = seq(0, 1, length = num_zones + 1)),
      include.lowest = TRUE
    )
  ) %>% 
  mutate(zone_txt = factor(paste0("Zone ", as.numeric(zone))))

  return(data_sf)

}

#/*=================================================*/
#' # Create yield response functions by characteristic
#/*=================================================*/

# data_sf <- analysis_res$data[[1]]
# field_vars <- analysis_res$data[[1]]


make_ys_by_chars <- function(data_sf){

  #/*----------------------------------*/
  #' ## Get correlation table
  #/*----------------------------------*/
  vars_all <- names(data_sf) %>% 
    .[!str_detect(., "id")] %>% 
    .[!str_detect(., "yield")] %>% 
    .[!str_detect(., "ID")] %>% 
    .[!str_detect(., "geometry")] %>% 
    .[!str_detect(., "b_int")] %>% 
    .[!str_detect(., "gc_rate")] %>% 
    .[!str_detect(., "zone")] %>% 
    .[!str_detect(., "_rate")] %>% 
    .[!str_detect(., "opt_")] %>% 
    .[!str_detect(., "x")] %>% 
    .[. != "X"] %>% 
    .[. != "Y"]  

  drop_vars <- data_sf[, vars_all] %>% 
    st_drop_geometry() %>% 
    .[, lapply(.SD, function(x) all(is.na(x)))] %>% 
    as.matrix() %>% 
    which()

  cor_data <- data_sf[, c(vars_all, "input_rate")] %>% 
    st_drop_geometry() %>% 
    tibble() %>% 
    .[, - drop_vars] %>% 
    dplyr::select(where(is.numeric)) 

  if(ncol(cor_data) <= 1) {
    return(NULL)
  }

  cor_tab <- cor_data %>% 
    cor(use = "complete.obs") %>% 
    .[, "b_slope", drop = FALSE] %>% 
    .[!(rownames(.) %in% c("b_slope")), , drop = FALSE]

  # g_cor <- cor_tab %>%  
  #   ggcorrplot(
  #     lab = TRUE,
  #     lab_size = 7
  #   ) +
  #   theme(
  #     axis.text.x = element_text(size = 16),
  #     axis.text.y = element_text(size = 16),
  #     legend.text = element_text(size = 16)
  #   )

  #/*----------------------------------*/
  #' ## Create maps and yield response figures
  #/*----------------------------------*/
  vars_plot_ls <- rownames(cor_tab)[abs(cor_tab) >= 0.2] %>% 
    .[!str_detect(., "yield")]

  vars_plot_ls_len <- length(vars_plot_ls)

  if (vars_plot_ls_len > 0) { # if any variable > 0.2
    
    plot_ls <- list()
    map_ls <- list()

    # var_p <- "elevation"

    for (var_p in vars_plot_ls){

      temp_data <- data_sf[, c("yield", "input_rate", var_p)] %>% 
        setnames(var_p, "var_temp") %>% 
        filter(!is.na(var_temp))

      if (!str_detect(var_p, "ss")){

        temp_data <- temp_data %>% 
          mutate(
            temp_cat = cut(
              var_temp, 
              breaks = quantile(
                var_temp, 
                prob = seq(0, 1, length = 4)
              ),
              include.lowest = TRUE
            )
          )

        g_map <- ggplot(data_sf) +
          geom_sf(aes_string(fill = var_p), color = NA) +
          theme_void() +
          scale_fill_distiller(
            palette = "YlOrRd",
            direction = -1
          ) + 
          theme(
            legend.position = "bottom",
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 12),
            legend.key.width =  unit(1, "cm"),
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            plot.margin = unit(c(0, 2, 0, 0), "cm") 
          )

        map_ls[[var_p]] <- g_map

      } else {

        temp_data <- temp_data %>% 
          mutate(
            temp_cat = ifelse(var_temp < 0.5, 0, 1)
          )

      }

      plot_ls[[var_p]] <- ggplot(data = temp_data) +
        geom_point(aes(y = yield, x = input_rate, color = factor(temp_cat)), size = 0.3) +
        geom_smooth(
          aes(
            y = yield, 
            x = input_rate, 
            color = factor(temp_cat)
          ),
          method = "gam",
          formula = y ~ s(x, k = 3)
        ) +
        theme_bw() +
        scale_color_discrete(name = var_p) +
        ylab("Yield (bu/acre)") +
        xlab("Seed Rate (1000 seeds)") +
        theme_bw() +
        theme(
          legend.position = c(0.3, 0.2),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 9)
        ) 

    }

    if(str_detect(var_p, "ss")){
      ssurgo_data <- readRDS("Intermediate/ssurgo.rds") %>%
        setnames(names(.), tolower(names(.)))

      ss_var <- gsub("ss_", "", var_p)

      g_map <- ggplot() +
        geom_sf(
          data = ssurgo_data,
          aes(fill = musym)
        ) + 
        theme_void()

      map_ls[["ssurgo"]] <- g_map
    }
    

    if (vars_plot_ls_len == 1){
      g_all <- plot_ls[[1]]
    } else if (vars_plot_ls_len == 2){
      g_all <- plot_ls[[1]] | plot_ls[[2]]
    } else if (vars_plot_ls_len == 3){
      g_all <- (plot_ls[[1]] | plot_ls[[2]]) / (plot_ls[[3]] | plot_spacer())
    } else if (vars_plot_ls_len >= 4){
      g_all <- (plot_ls[[1]] | plot_ls[[2]]) / (plot_ls[[3]] | plot_ls[[4]])
    }   

    if (length(map_ls) == 1){
      g_all_map <- map_ls[[1]]
    } else if (length(map_ls) == 2){
      g_all_map <- map_ls[[1]] | map_ls[[2]]
    } else if (length(map_ls) == 3){
      g_all_map <- (map_ls[[1]] | map_ls[[2]]) / (map_ls[[3]] | (ggplot() + theme_void()))
    } else if (length(map_ls) >= 4){
      g_all_map <- (map_ls[[1]] | map_ls[[2]]) / (map_ls[[3]] | map_ls[[4]])
    } 


  } else {
    g_all <- NULL
    g_all_map <- NULL
  }

  return(list(g_all, g_all_map))

}

expand_grid_df <- function(data_1, data_2) {

  expanded_data <- expand.grid(
    index_1 = seq_len(nrow(data_1)),
    index_2 = seq_len(nrow(data_2))
  ) %>% 
  tibble() %>% 
  rowwise() %>% 
  mutate(
    data = list(
      cbind(
        slice(data.table(data_1), index_1),
        slice(data.table(data_2), index_2)
      )
    )
  ) %>% 
  dplyr::select(data) %>% 
  ungroup() %>% 
  .$data %>% 
  rbindlist() %>% 
  tibble()

  return(expanded_data)

}

get_seed <- function(opt_data, c_type, w_zone){
  opt_data[type == c_type & zone_txt == paste0("Zone ", w_zone), seed_rate] %>% round(digits = 0)
}

get_pi <- function(opt_data, c_type, w_zone){
  opt_data[type == c_type & zone_txt == paste0("Zone ", w_zone), profit_hat] %>% round(digits = 2)
}

get_t_value <- function(test_data, w_zone){
  test_data[zone_txt == paste0("Zone ", w_zone), t] %>% 
    round(digits = 2)
}  

#/*=================================================*/
#' # Assign grower-chosen rate
#/*=================================================*/ 
# data <-  analysis_res$data[[1]]
# input_type <-  analysis_res$input_type[[1]]
# gc_type <-  analysis_res$gc_type[[1]]
# gc_rate <-  analysis_res$gc_rate[[1]]

assign_gc_rate <- function(data, input_type, gc_type, gc_rate) {

  if (gc_type == "uniform") {

    data$gc_rate <- gc_rate 

  } else if (gc_type == "Rx") {

    #--------------------------
    # Read Rx data
    #--------------------------
    Rx <- st_read(gc_rate) %>% 
      st_set_crs(4326) %>% 
      st_transform(st_crs(data)) %>%
      st_make_valid() %>%
      setnames(names(.), tolower(names(.)))
    

    dict_input <- dictionary[type == paste0("Rx-", tolower(input_type)), ]
    col_list <- dict_input[, column]

    Rx <- make_var_name_consistent(
      Rx, 
      dict_input 
    )

    #/*----------------------------------*/
    #' ## Unit conversion
    #/*----------------------------------*/
    if (input_type == "N") {
      Rx <- mutate(Rx, 
        tgti = convert_N_unit(
          input_data_n$form, 
          input_data_n$unit, 
          tgti, 
          field_data$reporting_unit
        ) 
        # + n_base_rate # add base N rate
      )
    } else if (input_type == "S") {
      #--- seed rate conversion ---#
      if (any(Rx$tgti > 10000)){
        #--- convert to K ---#
        Rx <- mutate(Rx, tgti = tgti / 1000)
      }
    }

    #=== map ===#
    # tm_shape(Rx) +
    #   tm_fill(col = "tgti")

    #--------------------------
    # Identify grower-chosen rate by observation
    #--------------------------
    obs_tgti <- st_intersection(data, Rx) %>% 
      mutate(area = as.numeric(st_area(.))) %>% 
      data.table() %>% 
      .[, .SD[area == max(area)], by = obs_id] %>% 
      .[, num_obs_per_zone := .N, tgti] %>% 
      .[, analyze := FALSE] %>% 
      .[num_obs_per_zone >= 200, analyze := TRUE] %>% 
      .[, .(obs_id, tgti, analyze)] 

    data <- left_join(data, obs_tgti, by = "obs_id") %>% 
      rename(gc_rate = tgti)

  }

  return(data)

}

#/*=================================================*/
#' # optimal-grower-chosen data
#/*=================================================*/ 
# data <-  analysis_res_o$data[[1]]
# pi_dif_test_zone <-  analysis_res_o$pi_dif_test_zone[[1]]
# opt_input_data <-  analysis_res_o$opt_input_data[[1]]

get_opt_gc_data <- function(data, pi_dif_test_zone, opt_input_data) {

  mean_gc_rate_by_zone <- data.table(data) %>% 
    .[, .(input_rate = mean(gc_rate)), by = zone_txt]

  gc_data <- pi_dif_test_zone %>% 
    .[, .(yhat_est_gc, point_est_gc, point_est_gc_se, zone_txt)] %>% 
    mean_gc_rate_by_zone[., on = "zone_txt"] %>% 
    setnames(
      c("yhat_est_gc", "point_est_gc", "point_est_gc_se"), 
      c("yield_hat", "profit_hat", "profit_hat_se")
    ) %>% 
    .[, `:=`(
      pi_upper = profit_hat + 1.96 * profit_hat_se,
      pi_lower = profit_hat - 1.96 * profit_hat_se
    )] %>% 
    .[, type := "gc"]

  opt_data <- pi_dif_test_zone %>% 
    .[, .(yhat_est_opt, point_est_opt, point_est_opt_se, zone_txt)] %>% 
    opt_input_data[, .(zone_txt, opt_input)][., on = "zone_txt"] %>% 
    setnames(
      c("yhat_est_opt", "point_est_opt", "point_est_opt_se", "opt_input"), 
      c("yield_hat", "profit_hat", "profit_hat_se", "input_rate")
    ) %>% 
    .[, `:=`(
      pi_upper = profit_hat + 1.96 * profit_hat_se,
      pi_lower = profit_hat - 1.96 * profit_hat_se
    )] %>% 
    .[, type := "opt_v"]

  opt_gc_data <- rbind(opt_data, gc_data)

  return(opt_gc_data)

}
    
get_whole_pi_test <- function(data, gam_res, crop_price, input_price) {

  test_data <- data.table(data) 

  whole_profits_test <- rbind(
    #=== opt (V) vs gc ===#
    get_dif_stat(
      test_data, 
      "input_rate", 
      "opt_input", 
      "gc_rate",
      gam_res,
      crop_price,
      input_price = input_price 
    ) %>% 
    .[, type := "optimal site-specific rate strategy \n vs \n grower-chosen strategy"] %>% 
    .[, type_short := "ovg"],

    #=== opt (u) vs gc ===#
    get_dif_stat(
      test_data, 
      "input_rate", 
      "opt_input", 
      "opt_input_u",
      gam_res,
      crop_price,
      input_price = input_price 
    ) %>% 
    .[, type := "optimal site-specific rate strategy \n vs \n optimal uniform rate strategy"] %>% 
    .[, type_short := "ovou"],

    #=== opt (u) vs gc ===#
    get_dif_stat(
      test_data, 
      "input_rate", 
      "opt_input_u", 
      "gc_rate",
      gam_res,
      crop_price,
      input_price = input_price 
    ) %>% 
    .[, type := "optimal uniform rate strategy \n vs \n grower-chosen strategy"] %>% 
    .[, type_short := "oug"]
  )

  return(whole_profits_test)

}

#/*=================================================*/
#' # Interactions
#/*=================================================*/

get_field_int <- function(data_sf, field_vars) {

  #=== find correlation coefs of b_slope and field vars ===#
  cor_tab <- data_sf[, c("b_slope", field_vars)] %>% 
    st_drop_geometry() %>% 
    cor(use = "complete.obs") %>% 
    .[, "b_slope", drop = FALSE] %>% 
    .[!(rownames(.) %in% c("b_slope")), , drop = FALSE] %>% 
    data.frame() %>% 
    arrange(desc(abs(b_slope)))

  #=== find variables that are correlated with b_slope higher than 0.2 ===#
  interacting_vars <- rownames(cor_tab)[abs(cor_tab) >= 0.2] %>% 
    .[!str_detect(., "yield")]

  return(list(
    cor_tab = cor_tab, 
    interacting_vars = interacting_vars
  ))

}

#/*=================================================*/
#' # Interactions and illustrative figures
#/*=================================================*/
#=== find correlation coefs of b_slope and field vars ===#

# get_inteactions_maps_ys(data, input_type, field_interactions)$g_ys_char

get_inteactions_maps_ys <- function(data, input_type, field_interactions) {

# field_interactions <- analysis_res_w$field_interactions[[1]]
# data <- analysis_res_w$data[[1]]

  cor_tab <- field_interactions$cor_tab
  interacting_vars <- field_interactions$interacting_vars

  unit_txt = case_when(
    input_type == "S" ~ "K seeds",
    input_type == "N" ~ "lbs",
    input_type == "K" ~ "lbs"
  )

  input_full_name = case_when(
    input_type == "S" ~ "Seed",
    input_type == "N" ~ "Nitrogen",
    input_type == "K" ~ "Potassium"
  )

  if (length(interacting_vars) == 0) {
    field_plots <- NULL
  } else {

# field_plots$data_plot_dt[[4]]$temp_cat %>% unique()
# field_plots$g_ys_char[[4]]

    field_plots <- tibble(
      ch_var = interacting_vars,
      data_plot = list(data)
    ) %>% 
    left_join(., field_var_data, by = c("ch_var" = "field_vars")) %>% 
    rowwise() %>% 
    mutate(g_map =
      list(
        ggplot(data_plot) +
        geom_sf(aes_string(fill = ch_var), color = NA) +
        theme_void() +
        scale_fill_distiller(
          palette = "YlGn",
          direction = -1,
          name = ""
        ) + 
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 12),
          legend.key.width =  unit(1, "cm"),
          plot.title = element_text(size = 12, hjust = 0.5),
          plot.margin = unit(c(0, 2, 0, 0), "cm") 
        ) +
        ggtitle(str_to_title(var_txt_in_report))
      )
    ) %>% 
    mutate(data_plot_dt = 
      list(
        data_plot[, c("yield", "input_rate", ch_var)] %>% 
          st_drop_geometry() %>% 
          setnames(ch_var, "temp_var") %>% 
          data.table()  
      )
    ) %>% 
    mutate(breaks = list(
      quantile(
        data_plot_dt$temp_var, 
        prob = seq(0, 1, length = 4)
      ) %>% 
      data.table(breaks = .) %>% 
      .[1, breaks := floor(breaks)] %>% 
      .[.N, breaks := ceiling(breaks)] %>% 
      .[, breaks] %>% 
      unique()
    )) %>% 
    mutate(data_plot_dt = 
      list(
        mutate(
          data_plot_dt,
          temp_cat = cut(
            data_plot_dt$temp_var, 
            breaks = breaks,
            include.lowest = TRUE
          )
        )
      )
    ) %>% 
    mutate(g_ys_char = list(
      ggplot(data = data_plot_dt) +
      geom_point(aes(y = yield, x = input_rate, color = factor(temp_cat)), size = 0.3) +
      geom_smooth(
        aes(
          y = yield, 
          x = input_rate, 
          color = factor(temp_cat)
        ),
        method = "gam",
        formula = y ~ s(x, k = 3)
      ) +
      theme_bw() +
      scale_color_discrete(name = str_to_title(var_txt_in_report)) +
      ylab("Yield (bu/acre)") +
      xlab(
        paste0(
          input_full_name, 
          " Rate (",
          unit_txt,
          ")"
        )
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9)
      )
    )) %>% 
    dplyr::select(- data_plot, - data_plot_dt, - breaks)

  }

  return(field_plots)

}

#/*=================================================*/
#' # Field variable name in the report  
#/*=================================================*/
field_var_data <- c(
    #=== topography ===#
    "twi", "tpi", "elevation", "slope", 
    #=== ssurgo ===#
    "clay", "sand", "water_storage",
    #=== ec ===#
    "ecs", "om", "cec", "ec02"
) %>% 
data.table(field_vars = .) %>% 
.[, var_txt_in_report := c(
  "topological wetness index", "topological position index", "elevation", "slope",
  "clay content (%)", "sand content (%)", "water storage",
  "EC", "organic matter", "CEC", "EC"
)] 
