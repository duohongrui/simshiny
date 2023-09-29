.normalization <- function(data, columns, reverse = FALSE){
  data <- data %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(columns), ~ pnorm((.x - mean(.x, na.rm = TRUE))/sd(.x, na.rm = TRUE)))
    ) %>%
    dplyr::ungroup()
  if(reverse){
    data <- data %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(columns), ~ 1 - .x)
      )
  }
  data <- data %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(columns), ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)))
    )
  return(data)
}
