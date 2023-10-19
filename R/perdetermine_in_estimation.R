predetermine_parameters <- function(method, estimate_prior, data_dim){
  if(method == "SPARSim" |
     method == "scDesign3" |
     method == "scDesign2" |
     method == "SRTsim" |
     method == "Lun2"){
    cell_num <- data_dim[2]
    ### group.condition
    if(estimate_prior[["group_num"]] > 1){
      message("Adding group labels for cells...")
      group_num <- estimate_prior[["group_num"]]
      if(cell_num == 160 & estimate_prior[["gene_num"]] == 4000 & group_num == 2){
        group.condition <- simshiny::group_condition
      }else{
        prop <- rep(round(1/group_num, digits = 2), group_num)
        prop[length(prop)] <- 1-sum(prop[1:(group_num - 1)])
        cell_number <- proportionate(number = cell_num,
                                     result_sum_strict = cell_num,
                                     prop = prop,
                                     prop_sum_strict = 1,
                                     digits = 0)
        group.condition <- c()
        for(i in 1:group_num){
          group.condition <- append(group.condition, rep(i, cell_number[i]))
        }
      }
      estimate_prior[["group.condition"]] <- group.condition
    }
    ### batch.condition
    if(estimate_prior[["batch_num"]] > 1 |
       method == "Lun2"){
      message("Adding batch labels for cells...")
      batch_num <- estimate_prior[["batch_num"]]
      ### allocate condition labels for cells
      if(cell_num == 160 & estimate_prior[["gene_num"]] == 4000 & batch_num == 2){
        batch.condition <- simshiny::group_condition
      }else{
        prop <- rep(round(1/batch_num, digits = 2), batch_num)
        prop[length(prop)] <- 1-sum(prop[1:(batch_num - 1)])
        cell_number <- proportionate(number = cell_num,
                                     result_sum_strict = cell_num,
                                     prop = prop,
                                     prop_sum_strict = 1,
                                     digits = 0)
        batch.condition <- c()
        for(i in 1:batch_num){
          batch.condition <- append(batch.condition, rep(i, cell_number[i]))
        }
      }
      estimate_prior[["batch.condition"]] <- batch.condition
    }
  }
  return(estimate_prior)
}
