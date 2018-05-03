#' fit_glove
#'
#' Fit Global average pooling operation for temporal data (Keras)
#'
#' @param container ...
#' @return list(model = model, params = params, data = container$data)
#'
#' @export
fit_glove <- function(container){
  
  params <- list(
    max_vocab = 2000,
    out_dim = 128,
    maxlen = 30,
    out_fun = "softmax",
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics  = "accuracy",
    batch_size = 32,
    epochs = 2,
    val_split = .2
  ) %>%
    check_list(container$params)
  

  
  ### Model init and word embedding
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = params$max_vocab,
      output_dim = params$out_dim,
      input_length = params$maxlen
    ) %>% 
    keras::layer_global_average_pooling_1d() %>%
    keras::layer_dense(
      units = length(unique(container$data$train[[params$target]])), 
      activation = params$activation
    ) %>%
    keras::compile(
      loss = params$loss,
      optimizer = params$optimizer,
      metrics = params$metrics
    )

  history <- model %>%
    keras::fit(
      x = container$data$train_input,
      y = tidyTX::tx_onehot(container$data$train[[params$target]]),
      batch_size = params$batch_size,
      epochs = params$epochs,
      validation_split = params$val_split, 
      verbose = F
    )
  
  return(model)
}


#' fit_lstm
#'
#' Fit Long Short-Term Memory Network (Keras)
#'
#' @param container ...
#' @param target ...
#' @return list(model = model, params = params, data = container$data)
#'
#' @export
fit_lstm <- function(container){
  
  params <- list(
    max_vocab = 2000,
    out_dim = 128,
    maxlen = 30,
    out_fun = "softmax",
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics  = "accuracy",
    batch_size = 32,
    epochs = 2,
    val_split = .2,
    lstm_dim = 64,
    lstm_drop = .1,
    rnn_drop = .1
  ) %>%
    check_list(container$params)
  
  ### Model init and word embedding
  model <- keras::keras_model_sequential() %>%
    keras::layer_embedding(
      input_dim = params$max_vocab,
      output_dim = params$out_dim,
      input_length = params$maxlen
    ) %>% 
    keras::layer_lstm(units = params$lstm_dim, dropout = params$lstm_drop, recurrent_dropout = params$rnn_drop) %>%
    keras::layer_dense(
      units = length(unique(container$data$train[[params$target]])), 
      activation = params$activation
    ) %>%
    keras::compile(
      loss = params$loss,
      optimizer = params$optimizer,
      metrics = params$metrics
    )
  
  history <- model %>%
    keras::fit(
      x = container$data$train_input,
      y = tidyTX::tx_onehot(container$data$train[[params$target]]),
      batch_size = params$batch_size,
      epochs = params$epochs,
      validation_split = params$val_split, 
      verbose = F
    )
  
  return(model)
}

#' learn_keras_model
#'
#' text pre-processing
#'
#' @param container ...
#' @return list(metric = accuracy, params = params)
#'
#' @export
learn_keras_model <- function(container, reconstruct){
  
  
  if(container$params$arch == "lstm"){
    model <- fit_lstm(container)
  }
  if(container$params$arch == "rnn"){
    model <- fit_(container)
  }
  if(container$params$arch == "glove"){
    model <- fit_glove(container)
  }
  
  preds <- model %>%
    #keras::predict_classes(model, x = test_input) + 1 %>%
    keras_predict(container$data$test_input, 1) %>%
    as.vector()
  
  perform <- get_perform(container$data$test[[container$params$target]], preds)
  if(reconstruct) perform <- preds
  
  return(list(perform = perform, params = container$params, data = container$data))
}

# compile_keras_model <- function(container, target){
#   
#   params <- list(
#     max_features = 2000,
#     out_dim = 128,
#     maxlen = 30,
#     out_fun = "softmax",
#     loss = "binary_crossentropy",
#     optimizer = "adam",
#     metrics  = "accuracy",
#     arch = "fasttext",
#     lstm_units = 64,
#     dropout = .2,
#     recurrent_dropout = .2
#   ) %>%
#     check_list(container$params)
#   
#   ### Model init and word embedding
#   model <- keras::keras_model_sequential()
#   
#   model %>%
#     keras::layer_embedding(
#       input_dim = params$max_features,
#       out_dim = params$out_dim,
#       input_length = params$maxlen
#     )
#   
#   ### Architecture
#   if(params$arch == "fasttext"){
#     model %<>% keras::layer_global_average_pooling_1d()
#   }
#   if(params$arch == "lstm"){
#     model %<>% keras::layer_lstm(units = params$lstm_units, dropout = params$dropout, recurrent_dropout = params$recurrent_dropout)
#   }
#   if(params$arch == "bilstm"){
#     model %<>% keras::bidirectional(keras::layer_lstm(units = params$lstm_units, dropout = params$dropout, recurrent_dropout = params$recurrent_dropout))
#   }
#   if(params$arch == "mlp"){
#     model %<>% 
#       keras::layer_activation(activation = 'relu') %>% 
#       keras::layer_dropout(rate = 0.5)
#   }
#   
#   ### Output function
#   if(params$out_fun == "softmax"){
#     model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "softmax")
#   }
#   if(params$out_fun == "sigmoid"){
#     model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "sigmoid")
#   }
#   # if(params$out_fun == "relu"){
#   #   model %<>% keras::layer_dense(length(unique(container$data$train[[target]])), activation = "relu")
#   # }
#   
#   ### Model Compilation
#   model %<>%
#     keras::compile(
#       loss = params$loss,
#       optimizer = params$optimizer,
#       metrics = params$metrics
#     )
#   
#   return(list(model = model, params = params, data = container$data))
# }

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

# learn_keras_model <- function(container, target, reconstruct = F){
#   
#   params <- list(
#     batch_size = 32,
#     epochs = 2,
#     val_split = .2
#   ) %>%
#     check_list(container$params)
#   
#   history <- container$model %>%
#     keras::fit(
#       x = container$data$train_input,
#       y = tidyTX::tx_onehot(container$data$train[[target]]),
#       batch_size = params$batch_size,
#       epochs = params$epochs,
#       validation_split = params$val_split, 
#       verbose = F
#     )
#   
#   preds <- container$model %>%
#     #keras::predict_classes(model, x = test_input) + 1 %>%
#     keras_predict(container$data$test_input, 1) %>%
#     as.vector()
#   
#   perform <- get_perform(container$data$test[[target]], preds)
#   if(reconstruct) perform <- preds
#   
#   return(list(perform = perform, params = params))
# }