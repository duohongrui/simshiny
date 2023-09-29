time_convert <- function(time, min = 1, max = 60*60*8){
  purrr::map_chr(time, function(x) {
    if(is.na(x)){
      NA
    } else if (x < min) {
      paste0("<", time_convert(min))
    } else if (x > max) {
      paste0(">", time_convert(max))
    } else if(x <= 60) {
      paste0(round(x), "s")
    } else if (x <= (60*60)) {
      paste0(round(x/60, digits = 1), "m")
    } else if (x <= (60*60*24)) {
      paste0(round(x/60/60, digits = 1), "h")
    } else {
      paste0(round(x/60/60/24, digits = 1), "d")
    }
  })
}

time_process <- function(x) {
  if(is.na(x)) {
    NA
  }else if (x == "\U221E") {
    Inf
  }else if (str_detect(x, "([0-9]*)[smhd]")) {
    number <- as.numeric(gsub("([0-9]*)[smhd]", "\\1", x))
    if(endsWith(x, "s")) {
      number
    }else if (endsWith(x, "m")) {
      number * 60
    }else if (endsWith(x, "h")) {
      number * 60 * 60
    }
  }else {
    stop("Invalid time: ", x)
  }
}


memory_convert <- function(memory, min = 1, max = 1024 * 1024) {
  purrr::map_chr(memory, function(x) {
    if(is.na(x)) {
      NA
    } else if (x < min) {
      paste0("<", memory_convert(min))
    } else if (x > max) {
      paste0(">", memory_convert(max))
    } else if (x <= 2^10) {
      paste0(round(x), "MB")
    } else if (x <= 2^20) {
      paste0(round(x/1024), "GB")
    }
  })
}


memory_process <- function(x) {
  if(is.na(x)) {
    NA
  }else if (x == "\U221E") {
    Inf
  }else {
    number <- as.numeric(gsub("([0-9]*)[MG]?B", "\\1", x))
    unit <- gsub("[0-9]*([kMGT]?B)", "\\1", x)
    if(unit == "MB") {
      number
    }else if (unit == "GB") {
      number * 2^10
    }else {
      stop("Invalid memory: ", x)
    }
  }
}
