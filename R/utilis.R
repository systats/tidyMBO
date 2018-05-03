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

#' tidy
#'
#' convienence function for extracting the parameter
#'
#' @param run ...
#' @return perform ...
#'
#' @export
tidy <- function(run, const, data, metric){
  perform <- run$opt.path$env$path %>% 
    cbind(
      exec.time = run$opt.path$env$exec.time,
      step = run$opt.path$env$dob
    ) %>% 
    tibble::as_tibble()
  
  const_expand <- const %>% 
    as_tibble() %>% 
    sample_n(size = nrow(perform), replace = T)
  
  combined <- cbind(perform, const_expand)
  
  colnames(combined)[stringr::str_detect(colnames(combined), "^y|^y_")] <- metric
  
  return(list(data = data, params = combined))
}
