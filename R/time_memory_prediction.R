#' Predict Time and Memory
#'
#' @param model Established RF model
#' @param cell_num The number of cell that need to simulated
#' @param gene_num The number of gene that need to simulated
#'
#' @importFrom stats predict
#' @importFrom purrr map_dfc map_dfr
#' @importFrom S4Vectors isEmpty
#' @importFrom dplyr pull slice filter relocate case_when c_across rowwise arrange
#' @importFrom tidyr everything
#'
prediction <- function(model, cell_num, gene_num){
  result <- purrr::map_dfc(names(model), .f = function(x){
    each_model <- model[[x]]
    each_method <- purrr::map_dfr(names(each_model), .f = function(y){
      if(is.null(each_model[[y]])){
        tmp <- tidyr::tibble(x = NA)
      }else{
        data <- data.frame(cell_num = cell_num, gene_num = gene_num)
        tmp <- tidyr::tibble(x = stats::predict(each_model[[y]], data))
      }
      colnames(tmp) <- x
      tmp
    })
    each_method
  })
  colnames(result) <- names(model)
  result <- result %>%
    dplyr::mutate(
      method = names(model$estimation_time)
    ) %>%
    dplyr::relocate("method", .before = "estimation_time")
  return(result)
}
