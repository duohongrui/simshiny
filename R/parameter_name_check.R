parameter_name_check <- function(method, other_prior, original_name = FALSE, function_name = FALSE){
  if(!is.null(other_prior)){
    parameter_names <- names(other_prior)
    method_parameters <- method_parameters()
    new_prior <- NULL
    for(i in parameter_names){
      if(is.null(other_prior[[i]])){
        next
      }
      sub_menu <- names(method_parameters[[i]])
      for(j in sub_menu){
        ### If not match, which means the method do not have this setting
        if(method %in% method_parameters[[i]][[j]]){
          if(original_name){
            new_prior[[i]] <- other_prior[[i]]
          }
          new_prior[[j]] <- other_prior[[i]]
        }
      }
    }
    return(new_prior)
  }else{
    return(NULL)
  }
}


method_parameters <- function(){
  list("cell_num" = list("batchCells" = c("Splat",
                                          "SCRIP-GP-trendedBCV",
                                          "SCRIP-GP-commonBCV",
                                          "SCRIP-BGP-commonBCV",
                                          "SCRIP-BP",
                                          "SCRIP-BGP-trendedBCV",
                                          "SplatPop",
                                          "SplatPop-paths",
                                          "Splat-paths",
                                          "SCRIP-paths"),
                         "nCells" = c("scDesign3-tree",
                                      "VeloSim",
                                      "dyntoy",
                                      "scDesign",
                                      "muscat",
                                      "scDesign2",
                                      "Kersplat",
                                      "dropsim"),
                         "groupCells" = c("Lun"),
                         "cell.plates" = c("Lun2")),
       "gene_num" = list("nGenes" = c("Splat",
                                      "SCRIP-GP-trendedBCV",
                                      "SCRIP-GP-commonBCV",
                                      "SCRIP-BGP-commonBCV",
                                      "SCRIP-BP",
                                      "SCRIP-BGP-trendedBCV",
                                      "SplatPop",
                                      "SplatPop-paths",
                                      "Splat-paths",
                                      "SCRIP-paths",
                                      "scDesign3-tree",
                                      "VeloSim",
                                      "dyntoy",
                                      "Lun",
                                      "muscat",
                                      "Lun2",
                                      "Kersplat",
                                      "dropsim")),
       "group_num" = list("prob.group" = c("Splat",
                                           "SCRIP-GP-trendedBCV",
                                           "SCRIP-GP-commonBCV",
                                           "SCRIP-BGP-commonBCV",
                                           "SCRIP-BP",
                                           "SCRIP-BGP-trendedBCV",
                                           "SplatPop",
                                           "SplatPop-paths",
                                           "Splat-paths",
                                           "SCRIP-paths",
                                           "scDesign2",
                                           "Lun"),
                          "nGroups" = c("scDesign",
                                        "muscat")),
       "batch_num" = list("batchCells" = c("Splat",
                                           "SCRIP-GP-trendedBCV",
                                           "SCRIP-GP-commonBCV",
                                           "SCRIP-BGP-commonBCV",
                                           "SCRIP-BP",
                                           "SCRIP-BGP-trendedBCV",
                                           "SplatPop",
                                           "SplatPop-paths",
                                           "Splat-paths",
                                           "SCRIP-paths"),
                          # "cell.plates" = c("Lun2"),
                          "batch.condition" = c("SPARSim")),
       "DEGs" = list("de.prob" = c("SPARSim",
                                   "Splat",
                                   "SCRIP-GP-trendedBCV",
                                   "SCRIP-GP-commonBCV",
                                   "SCRIP-BGP-commonBCV",
                                   "SCRIP-BP",
                                   "SCRIP-BGP-trendedBCV",
                                   "SplatPop",
                                   "SplatPop-paths",
                                   "Splat-paths",
                                   "SCRIP-paths",
                                   "scDesign",
                                   "Lun",
                                   "muscat")),
       "spatial" = list("spatial.x" = c("SRTsim",
                                        "scDesign3"),
                        "spatial.y" = c("SRTsim",
                                        "scDesign3")))
}
