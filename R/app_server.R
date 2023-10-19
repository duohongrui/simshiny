#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom stringr str_detect
#' @noRd
app_server <- function(input, output, session) {

  output$home_img <- renderImage({
    list(src = "inst/app/www/simsite_logo.png",
         width = 120,
         height = 140,
         align = "center")
  })

  autoInvalidate <- reactiveTimer(100000)
  observe({
    autoInvalidate()
    cat(".")
  })

  ### cell number
  observe({
    updateSliderInput(
      session = session,
      inputId ="cell_num",
      value = input$cell_num_slide
    )
  })
  observe({
    updateNumericInput(
      session = session,
      inputId ="cell_num_slide",
      value = input$cell_num
    )
  })
  ### gene number
  observe({
    updateSliderInput(
      session = session,
      inputId ="gene_num",
      value = input$gene_num_slide
    )
  })
  observe({
    updateNumericInput(
      session = session,
      inputId = "gene_num_slide",
      value = input$gene_num
    )
  })

  ### group and DEGs functionality and prior info
  observeEvent(input$determine_which_function, {
    selected_function <- input$determine_which_function
    if(any(stringr::str_detect(selected_function, "DEGs"))){
      if(!any(stringr::str_detect(selected_function, "group"))){
        selected_function <- append(selected_function, "group")
        updateCheckboxGroupInput(session,
                                 inputId = "determine_which_function",
                                 choices = c("Cell groups" = "group",
                                             "DEGs" = "DEGs",
                                             "Cell batches" = "batch",
                                             "Trajectory" = "trajectory"),
                                 selected = selected_function)
      }
    }
    if(length(selected_function) != 0){
      prior_info <- selected_function
      prior_info[which(prior_info == "group")] <- "group lables of cells"
      prior_info[which(prior_info == "batch")] <- "batch lables of cells"
      if(any(stringr::str_detect(prior_info, "DEGs"))){
        prior_info <- prior_info[-which(prior_info == "DEGs")]
      }
      if(any(stringr::str_detect(prior_info, "trajectory"))){
        prior_info <- prior_info[-which(prior_info == "trajectory")]
      }
      # prior_info[which(prior_info == "trajectory")] <- "the real data with pre-defined trajectory"
      if(!"none" %in% input$determine_which_prior){
        updateCheckboxGroupInput(session,
                                 inputId = "determine_which_prior",
                                 choices = c(prior_info))
      }else if("none" %in% input$determine_which_prior & length(selected_function) > 2){
        updateCheckboxGroupInput(session,
                                 inputId = "determine_which_prior",
                                 choices = c(prior_info))
      }else{
        updateCheckboxGroupInput(session,
                                 inputId = "determine_which_prior",
                                 choices = c("none"))
      }
    }
  })

  ### method choice (Output)
  overall_filter <- get_responses(input)
  output$overall_recommendation <- formattable::renderFormattable(render_funkyheatmap(overall_filter()))

  output$accuracy_recommendation <- formattable::renderFormattable(render_funkyheatmap_accuracy(overall_filter()))

  output$functionality_recommendation <- formattable::renderFormattable(render_funkyheatmap_functionality(overall_filter()))

  output$scalability_recommendation <- formattable::renderFormattable(render_funkyheatmap_scalability(overall_filter()))

  output$usability_recommendation <- formattable::renderFormattable(render_funkyheatmap_usability(overall_filter()))



  ############################################################################
  ############################### Simulation #################################
  ############################################################################
  observeEvent(input$choose_method, {
    method <- input$choose_method
    ############################### Cell or gene number setting check
    check_data_size <- parameter_name_check(method, other_prior = list(cell_num = 1000, gene_num = 1000), original_name = TRUE)
    if(is.null(check_data_size)){
      updateNumericInput(session = session,
                         inputId = "cell_num_simulate",
                         label = "Cell Number can not be set for this method",
                         value = 0,
                         min = 0,
                         max = 0)
      updateNumericInput(session = session,
                         inputId = "gene_num_simulate",
                         label = "Gene Number can not be set for this method",
                         value = 0,
                         min = 0,
                         max = 0)
    }else if(!"cell_num" %in% names(check_data_size)){
      updateNumericInput(session = session,
                         inputId = "cell_num_simulate",
                         label = "Cell Number can not be set for this method",
                         value = 0,
                         min = 0,
                         max = 0)
      updateNumericInput(session = session,
                         inputId = "gene_num_simulate",
                         label = "Gene Number",
                         value = 1000,
                         min = 1,
                         max = 30000)
    }else if(!"gene_num" %in% names(check_data_size)){
      updateNumericInput(session = session,
                         inputId = "cell_num_simulate",
                         label = "Cell Number",
                         value = 1000,
                         min = 1,
                         max = 30000)
      updateNumericInput(session = session,
                         inputId = "gene_num_simulate",
                         label = "Gene number can not be set for this method",
                         value = 0,
                         min = 0,
                         max = 0)
    }else{
      updateNumericInput(session = session,
                         inputId = "cell_num_simulate",
                         label = "Cell Number",
                         value = 1000,
                         min = 1,
                         max = 30000)
      updateNumericInput(session = session,
                         inputId = "gene_num_simulate",
                         label = "Gene Number",
                         value = 1000,
                         min = 1,
                         max = 30000)
    }
  })
  toListen <- reactive({
    list(input$functionality, input$choose_method)
  })
  observeEvent(toListen(), {
    method <- input$choose_method
    ############################### Match method and functionality and return to users
    selected_functionality <- input$functionality
    functions_check <- functionality_check(method = method,
                                           selected_functions = selected_functionality)

    if(!"matrix" %in% functions_check$approved_functions){
      selected_functionality <- c(selected_functionality, "matrix")
    }

    if(!is.null(functions_check$failed_functions)){
      failed_functionality <- functions_check$failed_functions
      if("group" %in% failed_functionality){
        selected_functionality <- selected_functionality[-which("group" == selected_functionality)]
      }
      if("DEGs" %in% failed_functionality){
        selected_functionality <- selected_functionality[-which("DEGs" == selected_functionality)]
      }
      if("batch" %in% failed_functionality){
        selected_functionality <- selected_functionality[-which("batch" == selected_functionality)]
      }
      if("trajectory" %in% failed_functionality){
        selected_functionality <- selected_functionality[-which("trajectory" == selected_functionality)]
      }
      if("spatial" %in% failed_functionality){
        selected_functionality <- selected_functionality[-which("spatial" == selected_functionality)]
      }
    }
    updateCheckboxGroupInput(session,
                             inputId = "functionality",
                             choices = c("Count matrix" = "matrix",
                                         "Cell groups" = "group",
                                         "DEGs" = "DEGs",
                                         "Cell batches" = "batch",
                                         "Trajectory" = "trajectory",
                                         "Spatial transcriptome" = "spatial"),
                             selected = selected_functionality)
    ############################### Update prior settings when the functionality is not defined
    if(!"group" %in% selected_functionality){
      if(method == "muscat"){
        updateNumericInput(session,
                           inputId = "group_num",
                           value = 2,
                           min = 1,
                           max = 2)
      }else{
        updateNumericInput(session,
                           inputId = "group_num",
                           value = 1,
                           min = 1,
                           max = 100)
      }
    }
    if(!"DEGs" %in% selected_functionality){
      updateNumericInput(session,
                         inputId = "DEGs_prop",
                         value = 0.2,
                         min = 0,
                         max = 1)
    }
    if(!"batch" %in% selected_functionality){
      if(method == "Lun2"){
        updateNumericInput(session,
                           inputId = "batch_num",
                           value = 2,
                           min = 2,
                           max = 2)
      }else{
        updateNumericInput(session,
                           inputId = "batch_num",
                           value = 1,
                           min = 1,
                           max = 100)
      }
    }
  })

  ############################### Upload data and prior information
  ### upload reference data
  upload_data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = utils::read.table(input$upload$datapath, sep = ",", header = TRUE),
           tsv = utils::read.table(input$upload$datapath, sep = "\t", header = TRUE),
           rds = readRDS(input$upload$datapath),
           validate("Invalid file; Please upload a .csv or .tsv or .rds file of gene expression matrix"))
  })

  ### upload prior information
  upload_prior <- reactive({
    req(input$upload_prior)
    ext <- tools::file_ext(input$upload_prior$name)
    switch(ext,
           csv = utils::read.table(input$upload_prior$datapath, sep = ",", header = TRUE),
           tsv = utils::read.table(input$upload_prior$datapath, sep = "\t", header = TRUE),
           rds = readRDS(input$upload_prior$datapath),
           validate("Invalid file; Please upload a .csv or .tsv or .rds file of gene expression matrix"))
  })

  ### load built-in dataset or uploaded dataset
  output$data_text <- renderText("Expression matrix of reference data")
  determine_which_data <- reactive({
    load_data <- input$load_data
    if(load_data == "no"){
      ref_data <- simshiny::data
    }else{
      ref_data <- upload_data()
    }
    ref_data
  })
  output$data <- renderTable({
    ref_data <- determine_which_data() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("gene_name")
    ref_data[1:10,1:10]
  })

  ### determine whether the prior information is provided from users
  output$prior_text <- renderText("Prior information for parameter estimation")
  determine_prior <- reactive({
    load_prior <- input$load_prior
    if(load_prior == "no"){
      prior_info <- NULL
    }else{
      prior_info <- upload_prior()
    }
    prior_info
  })
  output$prior <- renderTable({
    prior_info <- upload_prior() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("id")
    column_num <- ncol(prior_info)
    if(column_num >= 10){
      column_num <- 10
    }
    prior_info[1:10,1:column_num]
  })

  ### check prior information and submitted own data
  check_prior_info <- reactive({
    prior_info <- determine_prior()
    if(is.null(prior_info)){
      NULL
    }else{
      if(!any(colnames(prior_info) %in% c("group.condition", "batch.condition"))){
        "ERROR: The prior information should contain the column of 'group.condition' for cell group labels or 'batch.condition' for cell batch labels."
      }else{
        NULL
      }
    }
  })
  output$check_own_data <- renderText(check_prior_info())
  ### check prior information and submitted data
  check_both_provided <- reactive({
    prior_info <- determine_prior()
    if(input$load_prior == "yes" & !is.null(prior_info) & input$load_data == "no"){
      "ERROR: You also need to upload your own reference data!"
    }else{
      NULL
    }
  })
  output$check_own_both <- renderText(check_both_provided())
  ### Check spatial prior information
  check_spatial_info <- reactive({
    if(input$load_prior == "yes" & input$choose_method == "SRTsim" |
       input$load_prior == "yes" & input$choose_method == "scDesign3" & "spatial" %in% input$functionality){
      "Note: the prior information should contain 'spatial.x' and 'spatial.y' columns which represent the spatial coordinates of each spot or cell."
    }else{
      NULL
    }
  })
  output$spatial_info <- renderText(check_spatial_info())

  ############################### simulation (starting simulation when users click the actionButton)
  ### make notes for the path of output files
  local <- reactiveValues(export_file = NULL)
  local2 <- reactiveValues(export_file2 = NULL)
  ### simulation when clicking the button
  observeEvent(input$simulation, {
    ### update messages
    output$error_return <- renderText("")
    output$please_download <- renderText("")
    ################## get simulation input
    ### data
    ref_data <- determine_which_data()
    prior_info <- determine_prior()
    ### method
    method <- input$choose_method
    if(method == "VeloSim"){
      require(VeloSim)
    }
    ### cell and gene number
    cell_num <- input$cell_num_simulate
    if(cell_num == 0){
      cell_num <- ncol(ref_data)
    }
    gene_num <- input$gene_num_simulate
    if(gene_num == 0){
      gene_num <- nrow(ref_data)
    }
    ### functionality parameters
    group_num <- input$group_num
    DEGs_prop <- input$DEGs_prop
    batch_num <- input$batch_num

    ############################################################################
    ############################ Prior information #############################
    ############################################################################
    if(is.null(prior_info)){
      prior_info <- NULL
    }
    ### method split and simulate prior
    simulate_prior <- list(cell_num = cell_num,
                           gene_num = gene_num,
                           group_num = group_num,
                           batch_num = batch_num,
                           DEGs = DEGs_prop)
    # method <- stringr::str_replace(method, pattern = "—", replacement = "-")
    split_name <- stringr::str_split(method, "-", simplify = TRUE)[1, ]
    if(length(split_name) == 2){
      if(split_name[1] == "SCRIP"){
        mode <- "BP"
      }else{
        simulate_prior[[split_name[2]]] <- TRUE
      }
    }else if(length(split_name) == 3){
      mode <- paste0(split_name[2], "-", split_name[3])
    }else{
      mode <- NULL
    }
    simulate_prior[["mode"]] <- mode
    sub_method <- split_name[1]
    ### start the spinner
    shinycssloaders::showPageSpinner(caption = paste0("Simulating data using ", method, "...Please wait...", sub_method))
    error <- try({
      ############################################################################
      ########################### Estimate Parameters ############################
      ############################################################################
      env <- asNamespace("simmethods")
      if(sub_method == "scDesign"){
        parameters <- NULL
        estimate_prior <- NULL
      }else{
        estimate_function_name <- paste0(sub_method, "_estimation")
        assign(estimate_function_name, get(estimate_function_name, envir = env))
        ### prior information for estimation when it is unavailable
        estimate_prior <- simulate_prior
        if(sub_method == "scDesign3"){
          if(is.null(prior_info$group.condition)){
            if(!is.null(prior_info$group_num)){
              estimate_prior[["group.condition"]] <- sample(1:prior_info$group_num, ncol(ref_data), TRUE)
            }
          }
        }
        if(sub_method == "Lun2" | sub_method == "scDesign3"){
          if(is.null(prior_info$batch.condition)){
            if(!is.null(prior_info$batch_num)){
              estimate_prior[["batch.condition"]] <- sample(1:prior_info$batch_num, ncol(ref_data), TRUE)
            }
          }
        }
        ##### spatial info
        if(method == "SRTsim" & is.null(prior_info$spatial.x) |
           method == "SRTsim" & is.null(prior_info$spatial.y) |
           method == "scDesign3" & is.null(prior_info$spatial.x) & "spatial" %in% input$functionality |
           method == "scDesign3" & is.null(prior_info$spatial.y) & "spatial" %in% input$functionality){
          estimate_prior[["spatial.x"]] <- 1:ncol(ref_data)
          estimate_prior[["spatial.y"]] <- 1:ncol(ref_data)
        }
        ### some methods can only specify the number of cell groups or batches in the estimation step
        estimate_prior <- predetermine_parameters(method, estimate_prior, dim(ref_data))
        estimation_prior <- list(ref_data = ref_data,
                                 other_prior = list(group.condition = estimate_prior$group.condition,
                                                    batch.condition = estimate_prior$batch.condition,
                                                    spatial.x = estimate_prior$spatial.x,
                                                    spatial.y = estimate_prior$spatial.y),
                                 verbose = FALSE,
                                 seed = as.integer(runif(1, min = 1, max = .Machine$integer.max)))
        arguments <- estimation_prior[intersect(names(estimation_prior), names(formals(estimate_function_name)))]
        estimated_parameters <- do.call(estimate_function_name, arguments)
        parameters <- estimated_parameters$estimate_result
      }
      ############################################################################
      ############################ Simulate Datasets #############################
      ############################################################################
      ### simulation function
      simulate_function_name <- paste0(sub_method, "_simulation")
      assign(simulate_function_name, get(simulate_function_name, envir = env))
      ### check parameter names
      new_prior <- parameter_name_check(method = method,
                                        other_prior = simulate_prior,
                                        original_name = TRUE,
                                        function_name = TRUE)
      new_prior[["cell_num"]] <- cell_num
      new_prior[["gene_num"]] <- gene_num
      new_prior <- change_parameter_value(new_prior)
      ### add trajectory parameters
      if(isTRUE(simulate_prior$paths)){
        new_prior[["paths"]] <- TRUE
      }
      if(isTRUE(simulate_prior$tree) | isTRUE(simulate_prior$traj)){
        new_prior[["traj"]] <- TRUE
      }
      if(!is.null(simulate_prior$mode)){
        new_prior[["mode"]] <- simulate_prior$mode
      }
      ### change parameters
      if(!is.null(new_prior)){
        if(sub_method == "ESCO"){
          if(length(parameters) == 3){
            tree <- parameters[["tree"]]
            group <- parameters[["group"]]
            parameters <- set_parameters(parameters = parameters[["estimate_result"]],
                                         other_prior = new_prior,
                                         method = sub_method)
            parameters <- list("tree" = tree,
                               "group" = group,
                               "estimate_result" = parameters)
          }else{
            parameters <- set_parameters(parameters = parameters,
                                         other_prior = new_prior,
                                         method = sub_method)
          }
        }else{
          parameters <- set_parameters(parameters = parameters,
                                       other_prior = new_prior,
                                       method = sub_method)
        }
      }
      simulation_prior <- list(parameters = parameters,
                               other_prior = new_prior,
                               return_format = "list",
                               verbose = TRUE,
                               seed = as.integer(stats::runif(1, min = 1, max = .Machine$integer.max)))
      arguments <- simulation_prior[intersect(names(simulation_prior), names(formals(simulate_function_name)))]
      if(sub_method == "SCRIP" | sub_method == "scDesign"){
        arguments[["ref_data"]] <- ref_data
      }
      simulated_datasets <- do.call(simulate_function_name, arguments)
      simulated_datasets <- simulated_datasets$simulate_result

      ### download rds files
      dir <- tempdir()
      save_name <- paste0(method, "_simulated_data_", time_string(), ".rds")
      file_path <- file.path(dir, save_name)
      saveRDS(simulated_datasets, file_path)
      local$export_file <- file_path

      ### download xlsx files
      dataset_names <- list("Simulated_Gene_Matrix" = simulated_datasets$count_data %>%
                              as.data.frame() %>%
                              tibble::rownames_to_column(var = "gene_name"),
                            "Cell_Metadata" = simulated_datasets$col_meta,
                            "Gene_Matadata" = simulated_datasets$row_meta)
      dir2 <- tempdir()
      save_name2 <- paste0(method, "_simulated_data_", time_string(), ".xlsx")
      file_path2 <- file.path(dir2, save_name2)
      openxlsx::write.xlsx(dataset_names, file = file_path2)
      local2$export_file2 <- file_path2
    }, silent = FALSE)
    if(methods::is(error, "try-error")){
      ### close the spinner
      shinycssloaders::hidePageSpinner()
      output$error_return <- renderText(paste0("The simulation procedure failed. Please change another method or other parameters or raise an issue on Github.\n",
                                               as.character(error), "."))
    }else{
      shinycssloaders::hidePageSpinner()
      output$please_download <- renderText("Successfully simulating! Please download xlsx or rds files!")
      ### download button (.xlsx)
      output$download_1 <- renderUI({
        downloadButton("download_xlsx", "Download .xlsx")
      })
      ### download button (.rds)
      output$download_2 <- renderUI({
        downloadButton("download_rds", "Download .rds")
      })
    }

    ############################## Display simulated data
    output$simulated_data_text <- renderText("Expression matrix of simulated data")
    output$simulated_data <- renderTable({
      simulated_data <- simulated_datasets$count_data %>%
        as.data.frame() %>%
        tibble::rownames_to_column("gene_name")
      simulated_data[1:10,1:10]
    })
    output$cell_info_text <- renderText("Metadata of simulated cells")
    output$meta_cells <- renderTable({
      cell_info <- simulated_datasets$col_meta %>%
        as.data.frame() %>%
        tibble::rownames_to_column("id")
      if(ncol(cell_info) >= 10){
        show_column <- 10
      }else{
        show_column <- ncol(cell_info)
      }
      cell_info[1:10,1:show_column]
    })
    output$gene_info_text <- renderText("Metadata of simulated genes")
    output$meta_genes <- renderTable({
      gene_info <- simulated_datasets$row_meta %>%
        as.data.frame() %>%
        tibble::rownames_to_column("id")
      if(ncol(gene_info) >= 10){
        show_column <- 10
      }else{
        show_column <- ncol(gene_info)
      }
      gene_info[1:10,1:show_column]
    })

    ### output assessment
    output$umap <- renderPlot({
      ref_prior <- data.frame("cell_name" = colnames(ref_data))
      if(!is.null(estimate_prior$group.condition)){
        ref_prior <- ref_prior %>%
          mutate(
            "group.condition" = paste0("Group_", estimate_prior$group.condition)
          )
      }
      if(!is.null(estimate_prior$batch.condition)){
        ref_prior <- ref_prior %>%
          mutate(
            "batch.condition" = paste0("Batch_", estimate_prior$batch.condition)
          )
      }
      umap_result <- umap_result(ref_data = ref_data,
                                 simulated_data = simulated_datasets$count_data,
                                 ref_prior = ref_prior,
                                 cell_info = simulated_datasets$col_meta)
      umap_result
    })
    output$property <- renderPlot({
      property_result <- property_result(ref_data = ref_data,
                                         simulated_data = simulated_datasets$count_data)
      property_result
    })
  })

  ############################## Download simulated data (.csv or .rds)
  output$download_xlsx <- downloadHandler(
    filename = function(){
      paste0(input$choose_method, "_simulated_data_", time_string(), ".xlsx")
    },
    content = function(file){
      file.copy(
        from = local2$export_file2,
        to = file
      )
    }
  )
  output$download_rds <- downloadHandler(
    filename = function(){
      paste0(input$choose_method, "_simulated_data_", time_string(), ".rds")
    },
    content = function(file){
      file.copy(
        from = local$export_file,
        to = file
      )
    }
  )

}
