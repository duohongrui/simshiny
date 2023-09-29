get_responses <- function(input){
  overall <- readRDS(system.file("overall_data.rds", package = "simshiny")) %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c("accuracy", "functionality", "scalability", "usability")), ~ round(., digits = 2))
    )
  prediction_model <- simshiny::models
  reactive({
    ############################################################################
    ############################# Get responses ################################
    ############################################################################
    ### functionality
    determine_function <- input$determine_function
    functions <- input$determine_which_function
    priors <- input$determine_which_prior
    function_trade_off <- input$trade_off

    ### no functionality
    trade_off <- input$trade_off2

    ### spatial data
    spatial <- input$determine_spatial

    ### cell number
    cell_num <- input$cell_num_slide

    ### gene number
    gene_num <- input$gene_num_slide

    ### time
    time_estimation <- input$time_estimation %>% time_process()
    time_simulation <- input$time_simulation %>% time_process()

    ### memory
    memory_estimation <- input$memory_estimation %>% memory_process()
    memory_simulation <- input$memory_simulation %>% memory_process()

    ### criteria weight
    criteria_accuracy <- input$criteria_accuracy
    criteria_functionality <- input$criteria_functionality
    criteria_scalability <- input$criteria_scalability
    criteria_usability <- input$criteria_usability


    ### error proportion
    error_proportion <- input$error_proportion

    ############################################################################
    ############################ Filter criteria ###############################
    ############################################################################

    ### functionality
    if(!is.null(determine_function)){
      if(determine_function == "yes"){
        ### when determine_function is yes
        methods1 <- overall$id[overall$Category != "Class 5"]
        if(!is.null(functions)){
          if(any(stringr::str_detect(functions, "group"))){
            methods1 <- dplyr::intersect(methods1, overall$id[overall$`Simulate Groups` == "\u2713"])
          }
          if(any(stringr::str_detect(functions, "DEGs"))){
            methods1 <- dplyr::intersect(methods1, overall$id[overall$`Simulate DEGs` == "\u2713"])
          }
          if(any(stringr::str_detect(functions, "batch"))){
            methods1 <- dplyr::intersect(methods1, overall$id[overall$`Simulate Batches` == "\u2713"])
          }
          if(any(stringr::str_detect(functions, "trajectory"))){
            methods1 <- dplyr::intersect(methods1, overall$id[overall$`Simulate Trajectory` == "\u2713"])
          }
        }

        ### which prior information can users provide
        if(!is.null(priors)){
          if(any(stringr::str_detect(priors, "none"))){
            methods2 <- overall$id[!stringr::str_detect(overall$`Prior Information`, "required")]
          }else{
            methods2 <- overall$id[overall$`Prior Information` == ""]
            if(any(stringr::str_detect(priors, "group"))){
              methods2 <- append(methods2, overall$id[stringr::str_detect(overall$`Prior Information`, "group")])
            }
            if(any(stringr::str_detect(priors, "batch"))){
              methods2 <- append(methods2, overall$id[stringr::str_detect(overall$`Prior Information`, "batch")])
            }
            methods2 <- unique(methods2)
          }
        }else{
          methods2 <- overall$id
        }

        ### which aspect do users care about
        rank_criteria <- function_trade_off

      }else if(determine_function == "no"){
        ### when determine_function is no
        ### which aspect do users care about
        rank_criteria <- trade_off
        methods1 <- methods2 <- overall$id
      }
      selected_method <- dplyr::intersect(methods1, methods2)
    }else{
      rank_criteria <- "accuracy"
      selected_method <- overall$id
    }


    ### determined spatial data
    if(!is.null(spatial)){
      if(spatial == "yes"){
        spatial_method <- c("SRTsim", "scDesign3")
      }else{
        spatial_method <- NULL
      }
    }else{
      spatial_method <- NULL
    }


    ### Predictions and expectations
    prediction_result <- prediction(model = prediction_model, cell_num = cell_num, gene_num = gene_num)
    expectations <- prediction_result %>%
      dplyr::mutate(
        "exp_estimation_time" = time_estimation,
        "exp_estimation_memory" = memory_estimation,
        "exp_simulation_time" = time_simulation,
        "exp_simulation_memory" = memory_simulation
      )
    determine_scalability_method <- expectations %>%
      dplyr::transmute(
        "estimation_time" = dplyr::case_when(
          exp_estimation_time < estimation_time ~ FALSE,
          exp_estimation_time >= estimation_time ~ TRUE,
        ),
        "estimation_memory" = dplyr::case_when(
          exp_estimation_memory < estimation_memory ~ FALSE,
          exp_estimation_memory >= estimation_memory ~ TRUE,
        ),
        "simulation_time" = dplyr::case_when(
          exp_simulation_time < simulation_time ~ FALSE,
          exp_simulation_time >= simulation_time ~ TRUE,
        ),
        "simulation_memory" = dplyr::case_when(
          exp_simulation_memory < simulation_memory ~ FALSE,
          exp_simulation_memory >= simulation_memory ~ TRUE,
        )
      ) %>%
      mutate(
        "method" = prediction_result$method
      ) %>%
      dplyr::rowwise() %>%
      mutate(
        "approval_num" = sum(dplyr::c_across(1:4), na.rm = TRUE)
      )

    ### filter methods that meet the requirements. If not, the methods with 20 highest scalability scores will be returned.
    approved_methods_index <- which(determine_scalability_method$approval_num == 4)
    if(S4Vectors::isEmpty(approved_methods_index)){
      normalized_scalability <- .normalization(data = prediction_result, columns = colnames(prediction_result)[-1], reverse = TRUE)
      normalized_scalability <- normalized_scalability %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          "scalability_score" = mean(dplyr::c_across(2:5), na.rm = TRUE)
        )
      approved_methods <- normalized_scalability$method[order(normalized_scalability$scalability_score, decreasing = TRUE)][1:20]
    }else{
      approved_methods <- determine_scalability_method$method[approved_methods_index]
    }


    ##### the weights to calculate the overall score
    # if(!is.null(determine_function)){
    #   if(determine_function == "yes"){
    #     critera_scores <- overall %>%
    #       dplyr::select(c("accuracy", "functionality", "scalability", "usability"))
    #     overall_score <- "no"
    #   }
    #   if(determine_function == "no"){
    #
    #   }
    # }

    selected_methods <- append(spatial_method, selected_method)
    selected_methods <- dplyr::intersect(selected_methods, approved_methods)
    ### rank order
    if(rank_criteria == "accuracy"){
      rank_index <- order(overall %>% dplyr::pull("accuracy"), decreasing = TRUE)
    }else if(rank_criteria == "functionality"){
      rank_index <- order(overall %>% dplyr::pull("functionality"), decreasing = TRUE)
    }else if(rank_criteria == "scalability"){
      rank_index <- order(overall %>% dplyr::pull("scalability"), decreasing = TRUE)
    }
    overall_filter <- overall[rank_index, ]
    method_index <- which(overall_filter$id %in% selected_methods)
    overall_filter <- overall_filter %>%
      dplyr::slice(method_index)

    ### spatial methods
    if(!is.null(spatial_method)){
      spatial_method_index <- which(overall_filter$id %in% c("SRTsim", "scDesign3"))
      if(!S4Vectors::isEmpty(spatial_method_index)){
        if(length(spatial_method_index) == nrow(overall_filter)){
          overall_output <- overall_filter
        }else{
          overall_output <- overall_filter %>%
            dplyr::slice(-spatial_method_index)
          if(length(spatial_method_index) == 2){
            if(overall_filter$id[spatial_method_index[1]] == "scDesign3"){
              spatial_method_index <- rev(spatial_method_index)
            }
          }
          spatial_method_info <- overall_filter[which(overall_filter$id %in% overall_filter$id[spatial_method_index]), ]
          overall_output <- rbind(spatial_method_info, overall_output)
        }
      }else{
        overall_output <- overall_filter
      }
    }else{
      overall_output <- overall_filter
    }

    ### determine the stability of methods by the error proportions
    overall_output <- overall_output %>%
      mutate(
        "Stability" = case_when(
          error_proportion > 0.3 ~ "Unstable",
          TRUE ~ "Stable"
        ),
        "Overall" = round(overall, digits = 2),
        "rows" = dplyr::n()
      )

    ### bold font for recommendation
    num <- ceiling(nrow(overall_output) * 0.3)
    bold <- c(rep(1, num), rep(0, nrow(overall_output) - num))
    overall_output <- overall_output %>%
      mutate(
        "bold" = bold,
        "recommend" = dplyr::case_when(
          bold == 1 & Stability == "Unstable" ~ 0,
          bold == 1 & Stability == "Stable" ~ 1,
          TRUE ~ 0
        ),
        " " = recommend,
        "short_name" = stringr::str_split(id, pattern = "-", simplify = TRUE)[1]
      ) %>%
      dplyr::rename("Method" = "id",
                    "Acc" = "Accuracy",
                    "Accuracy" = "accuracy",
                    "Functionality" = "functionality",
                    "Scalability" = "scalability",
                    "Usability" = "usability")
    # method_URL <- method_URL_table
    # method_URL$Vignette <- stringr::str_split(method_URL$Vignette, pattern = "/", simplify = TRUE)[, 8]
    # overall_output <- dplyr::left_join(overall_output, method_URL, by = "short_name")
    overall_output
  })

}


