#' build_vocab
#' 
#' create vectorizer 
#'
#' @param text text vector
#' @param id id vector
#' @return vectorizer
#'
#' @export
build_vectorizer <- function(text, id, max_vocab = NULL, ngram = "unigram"){
  
  it_train <- text2vec::itoken(
    text, 
    ids = id,
    progressbar = F
  )
  
  if(ngram == "unigram"){
    vocab <- it_train %>%
      text2vec::create_vocabulary() 
  } else if(ngram == "bigram"){
    vocab <- it_train %>%
      text2vec::create_vocabulary(ngram = c(1L, 2L)) 
  }
  
  
  vocab %<>%
    text2vec::prune_vocabulary(
      term_count_min = 3, 
      vocab_term_max = max_vocab
    )
  
  vectorizer <- vocab %>% 
    text2vec::vocab_vectorizer()
  
  # train_input <- it_train %>% 
  #   text2vec::create_dtm(vectorizer)

   
  return(vectorizer)
}

#' build_dtm
#' 
#' build bag of words matrix
#'
#' @param container ...
#' @param text ...
#' @return list(data = data, params = params)
#'
#' @export
build_dtm <- function(vectorizer, text, id){
  it <- text2vec::itoken(
    text, 
    ids = id,
    progressbar = F
  )
  
  dtm <- it %>% 
    text2vec::create_dtm(vectorizer)
  
  return(dtm)
}

#' h2o_text_to_matrix
#' 
#' matrix tokenizer
#'
#' @param container ...
#' @return list(data = data, params = params)
#'
#' @export
text_to_matrix <- function(container){
  
  #container <- list(data=dt, params = params)
  #text <- "text_lemma"
  
  params <- list(
    max_vocab = 2000,
    ngram = "unigram"
  ) %>%
    check_list(container$params)
  
  
  vec <- build_vectorizer(
    text = container$data$train[[container$params$text]], 
    id = container$data$train$id,
    max_vocab = params$max_vocab,
    ngram = params$ngram
  )
  
  train_input <- vec %>% 
    build_dtm(
      text = container$data$train[[container$params$text]],
      id = container$data$train$id
    )
  
  test_input <- vec %>% 
    build_dtm(
      text = container$data$test[[container$params$text]],
      id = container$data$test$id
    )
  
  data <- c(container$data, list(train_input = train_input, test_input = test_input))
  
  return(list(data = data, params = params))
}


#' text_to_seq
#'
#' seq tokenizer
#'
#' @param container ...
#' @param text ...
#' @return list(data = data, params = params)
#'
#' @export
text_to_seq <- function(container){
  
  params <- list(
    max_vocab = 2000,
    batch_size = 40,
    maxlen = 30
  ) %>%
    check_list(container$params)
  
  tokenizer <- keras::text_tokenizer(num_words = params$max_vocab)
  keras::fit_text_tokenizer(tokenizer, x = container$data$train[[params$text]])
  
  train_input <- tokenizer %>%
    keras::texts_to_sequences(container$data$train[[params$text]]) %>%
    keras::pad_sequences(maxlen = params$maxlen, value = 0)
  
  test_input <- tokenizer %>%
    keras::texts_to_sequences(container$data$test[[params$text]]) %>%
    keras::pad_sequences(maxlen = params$maxlen, value = 0)
  
  data <- c(container$data, list(train_input = train_input, test_input = test_input))
  
  return(list(data = data, params = params))
}


#' text_to_matrix_keras
#'
#' matrix tokenizer
#'
#' @param container ...
#' @param text ...
#' @return list(data = data, params = params)
#'
#' @export
text_to_matrix_keras <- function(container){
  
  params <- list(
    max_features = 2000,
    batch_size = 40,
    maxlen = 30
  ) %>%
    check_list(container$params)
  
  tokenizer <- keras::text_tokenizer(num_words = params$max_features)
  keras::fit_text_tokenizer(tokenizer, x = container$data$train[[params$text]])
  
  train_input <- sequences_to_matrix(tokenizer, container$data$train[[params$text]], mode = 'binary')
  test_input <- sequences_to_matrix(tokenizer, container$data$test[[params$text]], mode = 'binary')
  
  data <- c(container$data, list(train_input = train_input, test_input = test_input))
  
  return(list(data = data, params = params))
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