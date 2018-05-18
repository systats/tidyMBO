round_3 <- function(x){
  round(x, 3)
}

get_load <- function(x){
  params <- get(load(x)) %>%
    .$params %>%
    dplyr::select(accuracy, step, dplyr::everything()) %>%
    dplyr::mutate_if(is.numeric, round_3) %>%
    dplyr::arrange(dplyr::desc(accuracy)) 
  return(params)
}

get_data <- function(x){
  data <- get(load(x)) %>%
    .$data 
  return(data)
}

get_top_accuracy <- function(x){
  x %>%
    dplyr::arrange(desc(accuracy)) %>%
    dplyr::slice(1) %>%
    .$accuracy
}

get_top_step <- function(x){
  x %>%
    dplyr::arrange(desc(accuracy)) %>%
    dplyr::slice(1) %>%
    .$step
}

get_target <- function(x){
  x %>%
    dplyr::arrange(desc(accuracy)) %>%
    dplyr::slice(1) %>%
    .$target
}

get_text <- function(x){
  x %>%
    dplyr::arrange(desc(accuracy)) %>%
    dplyr::slice(1) %>%
    .$text
}

get_arch <- function(x){
  x %>%
    dplyr::arrange(desc(accuracy)) %>%
    dplyr::slice(1) %>%
    .$arch
}

get_type <- function(x){
  #x = "lstm_boy"
  val <- case_when(
    stringr::str_detect(x, "gbm") ~ "gbm",
    stringr::str_detect(x, "dnn") ~ "dnn",
    stringr::str_detect(x, "nb") ~ "nb",
    stringr::str_detect(x, "lstm") ~ "lstm",
    stringr::str_detect(x, "rnn") ~ "rnn",
    stringr::str_detect(x, "glove") ~ "glove",
    stringr::str_detect(x, "mlp") ~ "mlp"
  )
  return(val)
}


table_mod_UI <- function(id){
  ns <- NS(id)
  htmltools::tagList(
    br(),
    div(class = "ui yellow ribbon label", "Select Cluster"),
    DT::dataTableOutput(ns("results")),
    br(),
    br(),
    div(class = "ui yellow ribbon label", "Select Run"),
    DT::dataTableOutput(ns("runs")),
    br(),
    br(),
    div(class = "ui yellow ribbon label", "Store Models for Reconstruction"),
    DT::dataTableOutput(ns("stored"))
  )
}

table_mod <- function(input, output, session){
  
  #get_top_accuracy(table$data[[3]])
  
  table <- reactive({
    table <- tibble(
      file = dir("results/"), 
      data = paste0("results/", file) %>%
        purrr::map(~ get_load(.x)),
      top_acc = data %>%
        purrr::map_dbl(~ get_top_accuracy(.x)),
      target = data %>%
        purrr::map_chr(~ get_target(.x)),
      text = data %>%
        purrr::map_chr(~ get_text(.x)),
      arch = data %>%
        purrr::map_chr(~ get_arch(.x)),
      top_step = data %>%
        purrr::map_dbl(~ get_top_step(.x))
      # arch = paste0("results/", file) %>%
      #   as.list() %>%
      #   purrr::map_chr(get_type)
    ) %>%
    arrange(desc(top_acc))
    
    return(table)
    #dat <- get(load("results/gbm_1.Rdata"))
  })
  
  
  output$results <- DT::renderDataTable({
    
    meta <- table() %>%
      dplyr::select(-data)
    
    meta %>%
      datatable(
        select = "single", rownames = F
      ) %>%
      formatStyle(
        names(meta),
        #names(meta %>% select(-top_acc)),
        backgroundColor = 'black'
      ) %>%
      formatStyle(
        "top_acc",
        background = styleColorBar(range(meta %>% select(top_acc)), 'yellow'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  
  unnest_stored <- reactive({
    if(is.null(input$results_cell_clicked$row)) return()
    
    stored <- table() %>%
      .[input$results_cell_clicked$row, ] %>%
      tidyr::unnest()
    
    return(stored)
  })

  output$runs <- DT::renderDataTable({
    if(is.null(unnest_stored())) return()

    unnest_stored() %>%
      DT::datatable(select = "multiple", rownames = F) %>%
      formatStyle(
        names(unnest_stored()),
        backgroundColor = 'black'
      ) %>%
      formatStyle(
        "accuracy",
        background = styleColorBar(range(unnest_stored() %>% select(accuracy)), 'yellow'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  selection <- reactive({
    if(is.null(input$runs_cell_clicked$row)) return(NULL)
    unnest_stored() %>%
      .[input$runs_rows_selected, ]
  })

  
  output$stored <- DT::renderDataTable({
    if(is.null(selection())) return()
    
    selection() %>%
      DT::datatable(rownames = F) %>%
      formatStyle(
        names(selection()),
        backgroundColor = 'black'
      ) %>%
      formatStyle(
        "accuracy",
        background = styleColorBar(range(selection() %>% select(accuracy)), 'yellow'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  return(selection)
}