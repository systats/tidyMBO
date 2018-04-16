#' check_list
#'
#' check_list compares two lists and augments missing entries.
#'
#' @param a a list of default parameters
#' @param b a list of input parameters
#' @return out
#'
#' @export
check_list <- function(a, b){
  cols <- names(a) %in% names(b)
  out <- c(b, a[!cols]) # append lists
  return(out)
}

#' split_data
#'
#' split_data splits the data into train and test set
#'
#' @param data input data
#' @param p propability of the train set
#' @return out
#'
#' @export
split_data <- function(data, p){

  train_id  <- caret::createDataPartition(y = data$index, p = p, list = F)
  train <- data[train_id,]
  test  <- data[-train_id,]

  return(list(train = train, test = test))
}

#' tidy
#'
#' convienence function for extracting the parameter
#'
#' @param run
#' @return perform
#'
#' @export
tidy <- function(run, metric){
  perform <- run$opt.path$env$path %>% 
    cbind(
      exec.time = run$opt.path$env$exec.time,
      step = run$opt.path$env$dob
    ) %>% 
    #dplyr::arrange(desc(y)) %>% # not reasonable for multi target functions
    tibble::as_tibble()
  
  colnames(perform)[stringr::str_detect(colnames(perform), "^y")] <- metric
  return(perform)
}


#' text_to_seq
#'
#' seq tokenizer
#'
#' @param container
#' @param text
#' @return list(data = data, params = params)
#'
#' @export
text_to_seq <- function(container, text){

  params <- list(
    max_features = 2000,
    batch_size = 40,
    maxlen = 30
  ) %>%
    check_list(container$params)

  tokenizer <- keras::text_tokenizer(num_words = params$max_features)
  keras::fit_text_tokenizer(tokenizer, x = container$data$train[[text]])

  train_seq <- tokenizer %>%
    keras::texts_to_sequences(container$data$train[[text]]) %>%
    keras::pad_sequences(maxlen = params$maxlen, value = 0)

  test_seq <- tokenizer %>%
    keras::texts_to_sequences(container$data$test[[text]]) %>%
    keras::pad_sequences(maxlen = params$maxlen, value = 0)

  data <- c(container$data, list(train_seq = train_seq, test_seq = test_seq))

  return(list(data = data, params = params))
}

#' compile_keras_model
#'
#' Compile Keras graph
#'
#' @param container
#' @param text
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
  model <- keras::keras_model_sequential() %>%
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
    model %<>% layer_lstm(units = params$lstm_units, dropout = params$dropout, recurrent_dropout = params$recurrent_dropout)
  }
  if(params$arch == "bilstm"){
    model %<>% bidirectional(layer_lstm(units = params$lstm_units, dropout = params$dropout, recurrent_dropout = params$recurrent_dropout))
  }
  if(params$arch == "mlp"){
    model %<>% 
      layer_activation(activation = 'relu') %>% 
      layer_dropout(rate = 0.5)
  }

  
  
  ### Output function
  if(params$output_fun == "softmax"){
    model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "softmax")
  }
  if(params$output_fun == "sigmoid"){
    model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "sigmoid")
  }
  if(params$output_fun == "relu"){
    model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "relu")
  }

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
#' @param model
#' @param seq
#' @param index correction
#' @return preds
#'
#' @export
keras_predict <- function(model, seq, index = 1){
  preds <- keras::predict_classes(model, x = seq) + index %>% as.vector()
  return(preds)
}

#' get_perform
#'
#' get performance measures
#'
#' @param actual
#' @param predicted
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
#' @param container
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
      x = container$data$train_seq,
      y = tidyTX::tx_onehot(container$data$train[[target]]),
      batch_size = params$batch_size,
      epochs = params$epochs,
      validation_split = params$val_split, 
      verbose = F
    )

  preds <- container$model %>%
    #keras::predict_classes(model, x = seq) + 1 %>%
    keras_predict(container$data$test_seq, 1) %>%
    as.vector()

  perform <- get_perform(container$data$test[[target]], preds)
  if(reconstruct) perform <- preds

  return(list(perform = perform, params = params))
}
