#' corpus_description
#'
#' corpus_description
#'
#' @param data data
#' @param text text variable
#' @return out ...
#'
#' @export
corpus_description <- function(data, text){
  dat <- data %>%
    dplyr::rename_("text" = text) %>%
    dplyr::mutate(nchar = text %>% nchar())  %>%
    dplyr::mutate(ntok = tidyTX::tx_n_tokens(text))
  
  tc <- dat %>%
    dplyr::select(text) %>%
    tidytext::unnest_tokens(word, text, token = "words") %>% 
    dplyr::count(word) %>% 
    dplyr::arrange(desc(n)) 
  
  out <- list(
    char = list(
      mean = mean(dat$nchar, na.rm = T) %>% floor(),
      med = median(dat$nchar, na.rm = T) 
    ),
    token = list(
      mean = mean(dat$ntok, na.rm = T) %>% floor(),
      med = median(dat$ntok, na.rm = T),
      quant = quantile(dat$ntok),
      denc = quantile(dat$ntok, probs = seq(.1:1, by = .1)),
      n_5 = tc %>%
        filter(n > 5) %>%
        nrow(),
      n_3 = tc %>%
        filter(n > 3) %>% 
        nrow(),
      n_all = tc %>%
        nrow(),
      tokens = tc
    )
  )
  return(out)
}

#' check_list
#'
#' check_list compares two lists and augments missing entries.
#'
#' @param a a list of default parameters
#' @param b a list of input parameters
#' @return out ...
#'
#' @export
check_list <- function(a, b){
  cols <- names(a) %in% names(b)
  out <- c(b, a[!cols]) # append lists
  return(out)
}


#' compile_keras_model
#'
#' Compile Keras graph
#'
#' @param container ...
#' @param text ...
#' @return list(model = model, params = params, data = container$data)
#'
#' @export
compile_keras_model <- function(container, target){

  params <- list(
    max_features = 2000,
    output_dim = 128,
    maxlen = 30,
    output_fun = "softmax",
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics  = "accuracy",
    arch = "fasttext",
    lstm_units = 64,
    dropout = .2,
    recurrent_dropout = .2
  ) %>%
    check_list(container$params)

  ### Model init and word embedding
  model <- keras::keras_model_sequential()
  
  model %>%
    keras::layer_embedding(
      input_dim = params$max_features,
      output_dim = params$output_dim,
      input_length = params$maxlen
    )

  ### Architecture
  if(params$arch == "fasttext"){
    model %<>% keras::layer_global_average_pooling_1d()
  }
  if(params$arch == "lstm"){
    model %<>% keras::layer_lstm(units = params$lstm_units, dropout = params$dropout, recurrent_dropout = params$recurrent_dropout)
  }
  if(params$arch == "bilstm"){
    model %<>% keras::bidirectional(keras::layer_lstm(units = params$lstm_units, dropout = params$dropout, recurrent_dropout = params$recurrent_dropout))
  }
  if(params$arch == "mlp"){
    model %<>% 
      keras::layer_activation(activation = 'relu') %>% 
      keras::layer_dropout(rate = 0.5)
  }

  ### Output function
  if(params$output_fun == "softmax"){
    model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "softmax")
  }
  if(params$output_fun == "sigmoid"){
    model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "sigmoid")
  }
  # if(params$output_fun == "relu"){
  #   model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "relu")
  # }

  ### Model Compilation
  model %<>%
    keras::compile(
      loss = params$loss,
      optimizer = params$optimizer,
      metrics = params$metrics
    )

  return(list(model = model, params = params, data = container$data))
}

#' keras_predict
#'
#' Convienience function for keras predictions
#'
#' @param model ...
#' @param test_input ...
#' @param index correction
#' @return preds
#'
#' @export
keras_predict <- function(model, test_input, index = 1){
  preds <- keras::predict_classes(model, x = test_input) + index %>% as.vector()
  return(preds)
}

#' get_perform
#'
#' get performance measures
#'
#' @param actual ...
#' @param predicted ...
#' @return list(metrics)
#'
#' @export
get_perform <- function(actual, predicted){
  list(
    accuracy = Metrics::accuracy(actual, predicted),
    auc = Metrics::auc(actual, predicted),
    f1 = Metrics::f1(actual, predicted),
    ce = Metrics::ce(actual, predicted)
    #logLoss = Metrics::logLoss(actual, predicted),
    #ll = Metrics::ll(actual, predicted)
  )
}

#' learn_keras_model
#'
#' text pre-processing
#'
#' @param container ...
#' @return list(metric = accuracy, params = params)
#'
#' @export
learn_keras_model <- function(container, target, reconstruct = F){

  params <- list(
    batch_size = 32,
    epochs = 2,
    val_split = .2
  ) %>%
    check_list(container$params)

  history <- container$model %>%
    keras::fit(
      x = container$data$train_input,
      y = tidyTX::tx_onehot(container$data$train[[target]]),
      batch_size = params$batch_size,
      epochs = params$epochs,
      validation_split = params$val_split, 
      verbose = F
    )

  preds <- container$model %>%
    #keras::predict_classes(model, x = test_input) + 1 %>%
    keras_predict(container$data$test_input, 1) %>%
    as.vector()

  perform <- get_perform(container$data$test[[target]], preds)
  if(reconstruct) perform <- preds

  return(list(perform = perform, params = params))
}

#' learn_h2o_model
#'
#' text pre-processing
#'
#' @param container ...
#' @return list(metric = accuracy, params = params)
#'
#' @export
learn_h2o_model <- function(container, target, reconstruct = F){

  if(container$params$arch == "gbm"){

    #params <- container$params
    # target <- "party_id"
    # text <- "text_lemma"
    # container <- list(data = dt, params = NULL)
    
    params <- list(
      ntrees = NULL,
      max_depth = NULL,
      learn_rate = NULL,
      sample_rate = NULL,
      col_sample_rate = NULL,
      stopping_rounds = NULL,
      stopping_tolerance = NULL,
      col_sample_rate = NULL,
      nbins = NULL
    ) %>%
      check_list(container$params)
    
    container <- text_to_matrix(container, text = "text_lemma")
    
    train_dtm <- container$data$train_input %>% h2o::as.h2o()
    x <- colnames(train_dtm)
    y_col <- container$data$train[[target]] %>% as.factor() %>% as.h2o()
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
      col_sample_rate = params$col_sample_rate,
      stopping_rounds = params$stopping_rounds,
      stopping_tolerance = params$stopping_tolerance,
      #col_sample_rate = params$col_sample_rate,
      nbins = params$nbins, 
      score_each_iteration = T,
      seed = 2018, 
      verbose = F
    )
    
    test_dtm <- container$data$test_input %>%
      h2o::as.h2o()
    
    preds <- h2o::h2o.predict(gbm, newdata = test_dtm) %>%
      as_tibble()
    
    perform <- get_perform(container$data$test[[target]], preds$predict)
    if(reconstruct) perform <- preds
    
    return(list(perform = perform, params = params))
  } else {
    cat("Stopp no correct model selected")
    stop()
  }
}
