# online_simulation <- function(ref_data,
#                               prior_info,
#                               simulation_input){
#
#   ############################################################################
#   ############################ Prior information #############################
#   ############################################################################
#   if(is.null(prior_info)){
#     prior_info <- NULL
#   }
#
#   ############################################################################
#   ########################## Method and data size ############################
#   ############################################################################
#   ### method
#   method <- simulation_input$method
#
#   ### cell and gene number
#   cell_num <- simulation_input$cell_num
#   gene_num <- simulation_input$gene_num
#
#
#   ############################################################################
#   ########################### Estimate Parameters ############################
#   ############################################################################
#   env <- asNamespace("simmethods")
#   estimate_function_name <- paste0(method, "_estimation")
#   assign(estimate_function_name, get(estimate_function_name, envir = env))
#   estimation_prior <- list(ref_data = ref_data,
#                            other_prior = list(group.condition = prior_info$group.condition,
#                                               batch.condition = prior_info$batch.condition),
#                            verbose = FALSE,
#                            seed = as.integer(runif(1, min = 1, max = .Machine$integer.max)))
#   arguments <- estimation_prior[intersect(names(estimation_prior), names(formals(estimate_function_name)))]
#   estimated_parameters <- do.call(estimate_function_name, arguments)
#
#   ############################################################################
#   ############################ Simulate Datasets #############################
#   ############################################################################
#   simulate_function_name <- paste0(method, "_simulation")
#   assign(simulate_function_name, get(simulate_function_name, envir = env))
#   simulation_prior <- list(parameters = estimated_parameters$estimate_result,
#                            other_prior = list(batchCells = cell_num,
#                                               nGenes = gene_num),
#                            return_format = "list",
#                            verbose = FALSE,
#                            seed = as.integer(runif(1, min = 1, max = .Machine$integer.max)))
#   arguments <- simulation_prior[intersect(names(simulation_prior), names(formals(simulate_function_name)))]
#   simulated_datasets <- do.call(simulate_function_name, arguments)
#   simulated_datasets <- simulated_datasets$simulate_result
#   return(simulated_datasets)
# }
