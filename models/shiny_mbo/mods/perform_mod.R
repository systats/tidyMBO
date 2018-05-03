perform_mod_UI <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("memory")),
    # shiny.semantic::tabset(
    #   tabs = list(
    #     list(menu = "Memory", content = ),
    #     list(menu = "History", content = DT::dataTableOutput(ns("history")))
    #   )
    # ),
    br(),
    div(class = "four wide column",
      actionButton(ns("start"), class = "ui blue inverted button", label = "Start Performance")# icon = uiicon("exit")  
    ),  
    div(class = "ten wide column",
      shiny.semantic::tabset(
        tabs = list(
          list(menu = "Confusion", content = DT::dataTableOutput(ns("confusion"))),
          list(menu = "Params", content = verbatimTextOutput(ns("params"))),
          list(menu = "Data", content = verbatimTextOutput(ns("data")))
        )
      )
      # div(class = "ui accordion",
      #   div(class = "active title",
      #       uiicon("dropdown"),
      #       "Confusion Matrix"
      #   ),
      #   div(class="active content",
      #       
      #   ),
      #   div(class = "title",
      #     uiicon("dropdown"),
      #     "Input Information"
      #   ),
      #   div(class="content", 
      #     verbatimTextOutput(ns("dev"))
      #   )
      # ),
      # shiny::tags$script("$('.ui.accordion').accordion();")
      #DT::dataTableOutput(ns("calc"))
    )
  )
}

perform_mod <- function(input, output, session, data){
  
  output$memory <- renderDataTable({
    if(is.null(data())) return()
    data() %>%
      dplyr::select(accuracy, dplyr::everything()) %>%
      DT::datatable(select = "single", rownames = F) %>%
      formatStyle(
        names(data()),
        backgroundColor = 'black'
      ) %>%
      formatStyle(
        "accuracy",
        background = styleColorBar(range(data() %>% select(accuracy)), 'yellow'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  history <- reactive({ 
    dir("perform") %>% 
      paste0("perform/", .) %>% 
      as.list() %>% 
      purrr::map(~{get(load(.x)) %>% .$params}) %>%
      purrr::map(as_tibble) %>%
      purrr::reduce(bind_rows)
  })
  
  
  # output$history <- renderDataTable({
  #   if(is.null(history())) return()
  #   
  #   history() %>%
  #     DT::datatable(select = "single", rownames = F) %>%
  #     formatStyle(
  #       names(history()),
  #       backgroundColor = 'black'
  #     ) %>%
  #     formatStyle(
  #       "accuracy",
  #       background = styleColorBar(range(history() %>% select(accuracy)), 'yellow'),
  #       backgroundSize = '98% 88%',
  #       backgroundRepeat = 'no-repeat',
  #       backgroundPosition = 'center'
  #     )
  # })
  
  pre <- eventReactive(input$start, {
    if(is.null(input$memory_rows_selected)){
      return()
    } else {
      data() %>%
        .[input$memory_rows_selected, ] %>%
        dplyr::mutate(run_id = file %>% str_replace(".Rdata", "") %>% paste0(. , "_", step)) %>%
        dplyr::select(run_id, dplyr::everything()) 
    }
  })
  
  # output$calc <- renderDataTable({
  #   if(is.null(pre())) return()
  #   pre() %>%
  #     DT::datatable(select = "multiple", rownames = F) %>%
  #     formatStyle(
  #       names(pre()),
  #       backgroundColor = 'black'
  #     )
  # })
  
  recon <- reactive({
    if(is.null(pre())) return()

    dat <- list(params = as.list(pre()), data = get_data(paste0("results/", pre()$file)))
    
    return(dat)
  })
  
  output$params <- renderPrint({ recon()$params })
  output$data <- renderPrint({ recon()$data })
  
  
  final <- reactive({
    if(is.null(recon())) return()

    con <- dir("perform/") %>%
      str_detect(recon()$params$run_id) %>%
      any()

    if(con){
      final <- get(load(paste0("perform/", recon()$params$run_id, ".Rdata")))
    } else {
      is_h2o_up <- purrr::possibly(h2o::h2o.clusterIsUp, F)
      if(recon()$params$arch %in% c("gbm", "dnn", "nb") & !is_h2o_up()) h2o::h2o.init()
        
      #launch_mbo <- function(x) x %>% run_mbo_steps(reconstruct = T)
      #safe_mbo <- purrr::possibly(launch_mbo, NULL)
      
      final <- recon() %>% run_mbo_steps(reconstruct = T)
        
      save(final, file = paste0("perform/", recon()$params$run_id, ".Rdata"))
    }
    return(final)
  })

  output$confusion <- renderDataTable({
    
    if(is.null(final())) return(NULL)
    if(is.null(input$memory_rows_selected)) return(NULL)
    
    predicted <- final()$perform
    actual <- final()$data$test[[final()$params$target]]
    
    mat <- table(predicted, actual)
    df <- mat %>%
      tibble::as_tibble()
      
    if(!is_tibble(df) | nrow(df) == 0) return(NULL)
    
    df <- df %>%
      tidyr::spread(actual, n)

    brks <- quantile(as.matrix(mat), probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}

    library(DT)
    DT::datatable(df, rownames = F) %>%
      formatStyle(names(df %>% dplyr::select(-predicted)),
                  backgroundColor = styleInterval(brks, clrs))
      # formatStyle(
      #   names(df),
      #   backgroundColor = 'black'
      # )
  })
}