#' fit_nb
#'
#' fit Naive Bayes
#'
#' @param container ...
#' @return model
#'
#' @export
fit_nb <- function(container){
  
  params <- list(
    laplace = 1
  ) %>%
    check_list(container$params)
  
  train_dtm <- container$data$train_input %>%
    h2o::as.h2o()
  x <- colnames(train_dtm)
  
  y_col <- container$data$train[[container$params$target]] %>%
    as.factor() %>%
    as.h2o()
  y <- colnames(y_col)
  
  h2o_train_dtm <- h2o::h2o.cbind(train_dtm, y_col)
  
  
  nb <- h2o.naiveBayes(
    training_frame = h2o_train_dtm,
    x = x,                    
    y = y,
    laplace = params$laplace,# > 1 integer Specify the Laplace smoothing parameter. 
    ### Global options
    nfolds = 4,
    fold_assignment = "Modulo",
    score_each_iteration = T,
    seed = 2018
    #verbose = F
  )
  
  return(nb)
}


#' fit_xgboost
#'
#' fit XGBoost
#'
#' @param container ...
#' @return model
#'
#' @export
fit_xgboost <- function(container){
  
  params <- list(
    ntrees = 50, # 20-80
    max_depth = 6,
    min_rows = 2, # 1-5
    learn_rate = 0.1, #
    sample_rate = 0.7, #
    col_sample_rate = 0.9 #
  ) %>%
    check_list(container$params)
  
  train_dtm <- container$data$train_input %>%
    h2o::as.h2o()
  x <- colnames(train_dtm)
  
  y_col <- container$data$train[[container$params$target]] %>%
    as.factor() %>%
    as.h2o()
  y <- colnames(y_col)
  
  h2o_train_dtm <- h2o::h2o.cbind(train_dtm, y_col)
  
  
  xgboost <- h2o.xgboost(
    training_frame = h2o_train_dtm,
    x = x,                    
    y = y,
    ntrees = params$ntrees,
    max_depth = params$max_depth,
    min_rows = params$min_rows,
    learn_rate = params$learn_rate,
    sample_rate = params$sample_rate,
    col_sample_rate = params$col_sample_rate,
    ### Global options
    nfolds = 4,
    fold_assignment = "Modulo",
    score_each_iteration = T,
    seed = 2018, 
    verbose = F
  )
  
  return(xgboost)
}


#' fit_gbm
#'
#' fit Gradient Boosting Machine
#'
#' @param container ...
#' @return model
#'
#' @export
fit_gbm <- function(container){
  
  params <- list(
    ntrees = 30,
    max_depth = 3,
    learn_rate = .3,
    sample_rate = .7,
    stop_round = 2,
    stop_tol = .5,
    nbins = 10,
    balance = F
  ) %>%
    check_list(container$params)
  
  train_dtm <- container$data$train_input %>%
    h2o::as.h2o()
  
  x <- colnames(train_dtm)
  
  y_col <- container$data$train[[container$params$target]] %>%
    as.factor() %>%
    as.h2o()
  
  y <- colnames(y_col)
  
  h2o_train_dtm <- h2o::h2o.cbind(train_dtm, y_col)
  

  gbm <- h2o.gbm(
    training_frame = h2o_train_dtm,
    x = x,                    
    y = y,    
    ntrees = params$ntress,
    max_depth = params$max_depth,
    learn_rate = params$learn_rate,
    sample_rate = params$sample_rate,
    stopping_rounds = params$stop_round,
    stopping_tolerance = params$stop_tol,
    nbins = params$nbins,
    ## Global options
    balance_classes = params$balance,
    #distribution = "bernoulli",
    keep_cross_validation_predictions = T,
    nfolds = 4,
    fold_assignment = "Modulo",
    score_each_iteration = T,
    seed = 2018,
    verbose = F
  )
  
  return(gbm)
}


#' fit_dnn
#'
#' fit Deep Neural Networks
#'
#' @param container ...
#' @return model
#'
#' @export
# container <- list(data = final, params = params)
#container <- text_to_matrix(container, "text_lemma")
fit_dnn <- function(container){
  
  params <- list(
    hidden1 = 80,
    hidden2 = 30,
    epochs = 10,
    #rho = .99,
    epsilon = 1e-08,
    #rate = 0.005,
    rate_annealing = 1e-06,
    #rate_decay = 1,
    #momentum_start = 0, # #0.5
    #input_dropout_ratio = 0,# 0.1 or 0.2
    #hidden_dropout1 = 0,
    #hidden_dropout2 = 0, 
    balance = F,
    l1 = 0, #force to 0
    l2 = 0, #  force small weights high spikes
    activation = "Rectifier"# c("Tanh", "TanhWithDropout", "Rectifier","RectifierWithDropout", "Maxout", "MaxoutWithDropout")
  ) %>%
  check_list(container$params)
  
  train_dtm <- container$data$train_input %>%
    #as.matrix() %>%
    #as.tibble() %>%
    h2o::as.h2o()
  
  x <- colnames(train_dtm)
  
  y_col <- container$data$train[[container$params$target]] %>%
    as.factor() %>%
    as.h2o()
  
  y <- colnames(y_col)
  
  h2o_train_dtm <- h2o::h2o.cbind(train_dtm, y_col)
  
  dnn <- h2o::h2o.deeplearning(
    training_frame = h2o_train_dtm,
    x = x,                    
    y = y,    
    hidden = c(params$hidden1, params$hidden2),
    #hidden_dropout_ = c(params$hidden_dropout1, params$hidden_dropout2),# .5 default
    #rho = params$rho,
    epsilon = params$epsilon,
    #rate = params$rate,
    rate_annealing = params$rate_annealing,
    #rate_decay = params$rate_decay,
    #momentum_start = params$momentum_start, # #0.5
    #input_dropout_ratio = params$input_dropout_ratio,# 0.1 or 0.2
    #hidden_dropout_ratios = c(params$hidden_dropout1, params$hidden_dropout2),
    l1 = params$l1, #force to 0
    l2 = params$l2, #  force small weights high spikes
    activation = as.character(params$activation),
    epochs = params$epochs, 
    ### Global options
    balance_classes = params$balance,
    nfolds = 4,
    fold_assignment = "Modulo",
    #balance_classes = T, 
    score_each_iteration = T,
    keep_cross_validation_predictions = T,
    seed = 2018, 
    verbose = F
  )
  
  return(dnn)
}

#' learn_h2o_model
#'
#' text pre-processing
#'
#' @param container ...
#' @return list(metric = accuracy, params = params)
#'
#' @export
learn_h2o_model <- function(container, reconstruct = F){

  if(container$params$arch == "gbm"){
    model <- fit_gbm(container)
  }
  if(container$params$arch == "dnn"){
    model <- fit_dnn(container)
  }
  if(container$params$arch == "xgboost"){
    model <- fit_xgboost(container)
  }
  if(container$params$arch == "nb"){
    model <- fit_nb(container)
  }
  
  test_dtm <- container$data$test_input %>%
    h2o::as.h2o()
  
  preds <- h2o::h2o.predict(model, newdata = test_dtm) %>%
    tibble::as_tibble()
  
  if(reconstruct){
    perform <- preds$predict
  } else {
    perform <- get_perform(container$data$test[[container$params$target]], preds$predict)
  }
  
  h2o::h2o.removeAll() 
  
  return(list(perform = perform, params = container$params, data = container$data))
}

