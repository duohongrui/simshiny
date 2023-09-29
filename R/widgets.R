collapsePanel <- function(..., header = "", show_on_start = FALSE, id = "") {
  collapse_id <- paste0("collapse", sample(1:100000000, 1))
  div(
    class = "panel panel-default",
    div(
      class = paste0("panel-heading", ifelse(show_on_start, "", " collapsed")),
      `data-target` = paste0("#", collapse_id),
      `data-toggle` = "collapse",
      span(icon("caret-down"), header)
    ),
    div(
      id = collapse_id,
      class = paste0("panel-collapse collapse", ifelse(show_on_start, " in", "")),
      div(
        ...
      )
    ),
    id = id
  )
}


update_weights <- function(v1, v2, v3, v4 = NULL){
  if(is.null(v4)){
    ratio <- c(round(v2/c(v2 + v3), digits = 2), round(v3/c(v2 + v3), digits = 2))
    remaining <- 1 - v1
    new_values <- c(remaining * ratio[1], 1 - (v1 + remaining * ratio[1]))
  }else{
    ratio <- c(round(v2/c(v2 + v3 + v4), digits = 2), round(v3/c(v2 + v3 + v4), digits = 2), round(v4/c(v2 + v3 + v4), digits = 2))
    remaining <- 1 - v1
    new_values <- c(remaining * ratio[1], remaining * ratio[2], 1 - (v1 + remaining * ratio[1] + remaining * ratio[2]))
  }
  return(new_values)
}

