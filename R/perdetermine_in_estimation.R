predetermine_parameters <- function(method, estimate_prior){
  if(method == "SPARSim"){
    if(estimate_prior[["group_num"]] > 1){
      group_num <- estimate_prior[["group_num"]]
      cell_num <- estimate_prior[["cell_num"]]
      if(cell_num == 160 & estimate_prior[["gene_num"]] == 4001){
        group.condition <- simshiny::group_condition
      }else{
        prop <- rep(round(1/group_num, digits = 2), group_num)
        prop[length(prop)] <- 1-sum(prop[1:(group_num-1)])
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
  }
  return(estimate_prior)
}
