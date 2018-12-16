####Functons for wine quality predictions


#Rename
rename_wine <- function(data) {
  data %>%
    rename(fixed_acidity = `fixed acidity`, volatile_acidity = `volatile acidity`,
           citric_acid = `citric acid`, 
           residual_sugar = `residual sugar`,
           free_sulfur_dioxide = `free sulfur dioxide`, 
           total_sulfur_dioxide = `total sulfur dioxide`)
}

# Correlation ----
get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)
  
  if (fct_reorder) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }
  
  if (fct_rev) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }
  
  return(data_cor)
  
}


# Correlation plot----
plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1, 
                     color_pos = palette_light()[[1]], 
                     color_neg = palette_light()[[2]]) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())
  
  g <- data_cor %>%
    ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    theme_tq() +
    scale_color_manual(values = c(color_neg, color_pos)) 
  
  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  
  return(g)
  
}


#histogram ----
plot_hist_facet <- function(data, bins = 10, ncol = 5,
                            fct_reorder = FALSE, fct_rev = FALSE, 
                            fill = palette_light()[[3]], 
                            color = "white", scale = "free") {
  
  data_factored <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = key, value = value, factor_key = TRUE) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale) + 
    theme_tq()
  
  return(g)
}


#For GLM model
get_metrics_lm <- function(model){ 

glue::glue(
"Lambda: {model@parameters$lambda}
Alpha: {model@parameters$alpha}

Training MSE: {round(h2o.mse(model, train = T), 5)}
Training R_Squared: {round(h2o.r2(model, train = T), 5)}
-----------------------------------
Validation MSE: {round(h2o.mse(model, valid = T),5)}
Validation R_Squared: {round(h2o.r2(model, valid = T),5)}")
}


#For tree based model
get_metrics_tree <- function(model){

glue::glue(
"ntrees: {model@allparameters$ntrees}
max_depth: {model@allparameters$max_depth}
             
Training MSE: {round(h2o.mse(model, train = T), 5)}
Training R_Squared: {round(h2o.r2(model, train = T), 5)}
------------------------------------------
Validation MSE: {round(h2o.mse(model, valid = T),5)}
Validation R_Squared: {round(h2o.r2(model, valid = T),5)}")
}
