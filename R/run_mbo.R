#' run_keras_steps
#'
#' run all tuning processes
#'
#' @param params params
#' @param target target variable
#' @param text text variable
#' @return metric
#'
#' @export
run_keras_steps <- function(params, data, target, text, metric, reconstruct = F){

  out <- list(data = data, params = params) %>%
    text_to_seq(text) %>%
    compile_keras_model(target) %>%
    learn_keras_model(target, reconstruct = reconstruct)

  if(reconstruct){
    return(out) 
  } else {
    metric <- metric %>% 
      purrr::map_dbl(~out$perform[[.x]])
    
    return(metric) 
  }
}




#' start_parallel_core
#'
#' start parallel processing
#'
#' @export
start_parallel_core <- function(parallel, cores = 2){
  if(!parallel) return()
  parallelMap::parallelStartMulticore(cpus = cores)
  #cat(crayon::blue("\n>>>") %+% crayon::green("Parallelization started") %+% crayon::blue("<<<\n"))
  
}

#' kill_parallel_core
#'
#' stop parallel processing
#'
#' @export
kill_parallel_core <- function(parallel){
  if(!parallel) return()
  parallelMap::parallelStop()
  #cat(crayon::blue("\n>>>") %+% crayon::green("Parallelization stopped") %+% crayon::blue("<<<\n"))
}

#' progressively
#'
#' gives progressbar capabilities to purrr
#'
#' @export
progressively <- function(.f, .n, ...) {
  pb <- progress::progress_bar$new(total = .n, ...)
  function(...) {
    pb$tick()
    .f(...)
  }
}

#' run_mbo
#'
#' tidy MBO
#'
#' @param params params
#' @param target target variable
#' @param name the name
#' @param n_init burn in iterations
#' @param n_main main iterations
#' @return list(data = data, params = params)
#'
#' @export
run_mbo <- function(data, params, target, text, n_init = 5, n_main = 30, name = "", metric, parallel){

  n_obj <- length(metric)
  
  list_metrics <- list(
    accuracy = F, 
    ce = T, 
    auc = F, 
    f1 = F
    #logLoss = T, 
    #ll = T
  ) # Minimize?
  
  minimize <- list_metrics[metric] %>% unlist %>% as.logical()
  #class(minimize)
  cat(crayon::blue(":::::::::::::::::::MBO:::::::::::::::::\n")) 
  ### Main Definition Function
  if(n_obj == 1) {
    constructor <- smoof::makeSingleObjectiveFunction(
      name = name,
      fn = function(x) {
        perform <- run_keras_steps(x, data = data, target = target, text = text, metric = metric)
        return(perform)
      },
      par.set = params,
      has.simple.signature = F, # function expects a named list of parameter values
      minimize = minimize # to increase accuracy
    )
    crayon::blue("[1] ") %+% 
      crayon::green("Single Objective Function") %+% 
      crayon::red(paste0(" (", paste(metric, collapse = ", ") ,")\n")) %>%
      cat() 
  } else {
    constructor <- smoof::makeMultiObjectiveFunction(
      name = name,
      fn = function(x) {
        perform <- run_keras_steps(x, data = data, target = target, text = text, metric = metric)
        return(perform)
      },
      par.set = params,
      n.objectives = n_obj,
      has.simple.signature = F, # function expects a named list of parameter values
      minimize = minimize # to increase accuracy
    )
    crayon::blue("[1] ") %+% 
      crayon::green("Multi Objective Function") %+% 
      crayon::red(paste0(" (", paste(metric, collapse = ", ") ,")\n")) %>%
      cat() 
  }
  
  init <- ParamHelpers::generateDesign(
    n = n_init,
    par.set = ParamHelpers::getParamSet(constructor),
    fun = lhs::randomLHS
  )

  crayon::blue("[2] ") %+% 
    crayon::green("Burning in Random Design\n") %>% 
    cat()
  
  progress_fun <- progressively(.f = constructor, .n = nrow(init))

  if(n_obj == 1){
    var_names <- "y"
  } else { 
    var_names <- paste0("y_", 1:n_obj)
  }
  
  init <- init %>%
    split(seq_row(init)) %>%
    purrr::map(progress_fun) %>%
    purrr::reduce(rbind) %>%
    as_tibble() %>%
    set_colnames(value = var_names) %>%
    cbind(init, .)

  type <- init %>%
    purrr::map(class) %>%
    purrr::map(~.x == "factor") %>%
    unlist %>%
    any() %>%
    ifelse(., "factor", "integer")

  if(type == "integer"){
    
    crayon::blue("[3] ") %+% 
      crayon::green("Continous Search Space\n") %>% 
      cat()
    
    crayon::blue("[4] ") %+% 
      crayon::green("Surrogate Model: ") %+% 
      crayon::red("Bayesian Optimization\n") %>% 
      cat()
    
    surrogate <- mlr::makeLearner(
      cl = "regr.km",
      predict.type = "se",
      covtype = "matern3_2",
      control = list(trace = F)
    )
    
    control <- mlrMBO::makeMBOControl() %>%
      mlrMBO::setMBOControlTermination(iters = n_main) %>%
      mlrMBO::setMBOControlInfill(crit = makeMBOInfillCritEI())
  }

  if(type == "factor"){
    
    crayon::blue("[3] ") %+% 
      crayon::green("Discrete Search Space\n") %>% 
      cat()
    
    crayon::blue("[4] ") %+% 
      crayon::green("Surrogate Model: ") %+% 
      crayon::red("Random Forest Regression\n") %>% 
      cat()
    
    surrogate <- makeLearner("regr.randomForest", predict.type = "se")
    
    control <- makeMBOControl() %>%
      setMBOControlInfill(
        crit = makeMBOInfillCritCB(cb.lambda = 5),
        opt.focussearch.points = 500
      ) %>%
      setMBOControlTermination(
        iters = n_main
      )
  }
  
  if(n_obj > 1){
    control <- makeMBOControl(n.objectives = n_obj) %>%
      setMBOControlTermination(iters = n_main) %>%
      setMBOControlInfill(crit = makeMBOInfillCritDIB())
  }
  
  start_parallel_core(parallel, cores = 2)
  
  run <- mlrMBO::mbo(
    constructor,
    design = init,
    learner = surrogate,
    control = control,
    show.info = T
  )
    
  kill_parallel_core(parallel)
  
  return(list(obj = run, df = tidy(run, metric)))
}