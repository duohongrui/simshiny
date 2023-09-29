### save prediction model for time and memory
models <- readRDS("../../Documents/prediction_model.rds")
usethis::use_data(models, overwrite = TRUE)


### method URL
methods <- simmethods::get_method()
method_names <- names(methods)
method_URL_table <- purrr::map_dfr(method_names, .f = function(x){
  method_definition <- methods[[x]][[paste0(x, "_method")]]
  if(is.null(method_definition$url)){
    if(is.null(method_definition$authors$github)){
      URL <- NA
    }else{
      URL <- method_definition$authors$github
    }
  }else{
    URL <- method_definition$url
  }
  vignette <- method_definition$vignette
  tibble::tibble("short_name" = x,
                 "URL" = URL,
                 "Vignette" = vignette)
})
usethis::use_data(method_URL_table, overwrite = TRUE)
