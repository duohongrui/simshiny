change_parameter_value <- function(new_prior){
  parameter_name <- names(new_prior)
  #################################### Gene number
  if(!is.null(new_prior[["gene_num"]])){
    new_prior[["nGenes"]] <- new_prior[["gene_num"]]
    new_prior <- new_prior[-which("gene_num" == names(new_prior))]
  }
  #################################### Cell number
  if(!is.null(new_prior[["cell_num"]])){
    if(!is.null(new_prior[["nCells"]])){
      new_prior[["nCells"]] <- new_prior[["cell_num"]]
      new_prior <- new_prior[-which("cell_num" == names(new_prior))]
    }
  }
  #################################### group
  ##### prob.group
  if(!is.null(new_prior[["prob.group"]])){
    if(!is.null(new_prior[["group_num"]])){
      if(new_prior[["group_num"]] > 1){
        prop <- rep(round(1/new_prior[["group_num"]], digits = 2), new_prior[["group_num"]])
        prop[length(prop)] <- 1-sum(prop[1:(new_prior[["group_num"]]-1)])
        new_prior[["prob.group"]] <- prop
      }else{
        new_prior[["prob.group"]] <- 1
      }
    }
    new_prior <- new_prior[-which("group_num" == names(new_prior))]
  }
  ##### nGroups
  if(!is.null(new_prior[["nGroups"]])){
    new_prior[["nGroups"]] <- new_prior[["group_num"]]
    new_prior <- new_prior[-which("group_num" == names(new_prior))]
  }
  ##### groupCells
  if(!is.null(new_prior[["groupCells"]])){
    new_prior <- new_prior[-which("cell_num" == names(new_prior))]
  }
  ##### group.condition
  if(!is.null(new_prior[["group_num"]])){
    if(new_prior[["group_num"]] > 1){
      if(!is.null(new_prior[["group.condition"]])){
        prop <- rep(round(1/new_prior[["group_num"]], digits = 2), new_prior[["group_num"]])
        prop[length(prop)] <- 1-sum(prop[1:(new_prior[["group_num"]]-1)])
        cell_number <- proportionate(number = new_prior[["cell_num"]],
                                     result_sum_strict = new_prior[["cell_num"]],
                                     prop = prop,
                                     prop_sum_strict = 1,
                                     digits = 0)
        group.condition <- c()
        for(i in 1:new_prior[["group_num"]]){
          group.condition <- append(group.condition, rep(i, cell_number[i]))
        }
        new_prior[["group.condition"]] <- group.condition
        new_prior <- new_prior[-which("group_num" == names(new_prior))]
        new_prior <- new_prior[-which("cell_num" == names(new_prior))]
      }
    }
  }
  #################################### batch
  ##### batchCells
  if(!is.null(new_prior[["batchCells"]])){
    if(!is.null(new_prior[["batch_num"]])){
      if(new_prior[["batch_num"]] > 1){
        prop <- rep(round(1/new_prior[["batch_num"]], digits = 2), new_prior[["batch_num"]])
        prop[length(prop)] <- 1-sum(prop[1:(new_prior[["batch_num"]]-1)])
        new_prior[["batchCells"]] <- proportionate(number = new_prior[["cell_num"]],
                                                   result_sum_strict = new_prior[["cell_num"]],
                                                   prop = prop,
                                                   prop_sum_strict = 1,
                                                   digits = 0)
      }else{
        new_prior[["batchCells"]] <- new_prior[["cell_num"]]
      }
    }
    new_prior <- new_prior[-which("cell_num" == names(new_prior))]
    new_prior <- new_prior[-which("batch_num" == names(new_prior))]
  }
  ##### batch.condition
  if(!is.null(new_prior[["batch.condition"]])){
    prop <- rep(round(1/new_prior[["batch_num"]], digits = 2), new_prior[["batch_num"]])
    prop[length(prop)] <- 1-sum(prop[1:(new_prior[["batch_num"]]-1)])
    cell_number <- proportionate(number = new_prior[["cell_num"]],
                                 result_sum_strict = new_prior[["cell_num"]],
                                 prop = prop,
                                 prop_sum_strict = 1,
                                 digits = 0)
    batch.condition <- c()
    for(i in 1:new_prior[["batch_num"]]){
      batch.condition <- append(batch.condition, rep(i, cell_number[i]))
    }
    new_prior[["batch.condition"]] <- batch.condition
    new_prior <- new_prior[-which("batch_num" == names(new_prior))]
    new_prior <- new_prior[-which("cell_num" == names(new_prior))]
  }
  #################################### DEGs
  if(!is.null(new_prior[["DEGs"]])){
    new_prior <- new_prior[-which("DEGs" == names(new_prior))]
  }
  return(new_prior)
}


proportionate <- function(
    number,
    result_sum_strict = NULL,
    prop,
    prop_sum_strict = NULL,
    digits = 0
){
  # Check
  if(!is.null(prop_sum_strict)){
    if(sum(prop) != prop_sum_strict){
      prop[length(prop)] <- prop_sum_strict - sum(prop[1:(length(prop) - 1)])
    }
  }
  # Assign
  len_prop <- length(prop)
  assign_num <- number * prop
  assign_num <- round(assign_num, digits = digits)
  if(!is.null(result_sum_strict)){
    if(sum(assign_num) != result_sum_strict){
      assign_num <- c(assign_num[1:(len_prop-1)],
                      result_sum_strict - sum(assign_num[1:(len_prop-1)]))
      assertthat::assert_that(sum(assign_num) == result_sum_strict)
    }
  }
  return(assign_num)
}
