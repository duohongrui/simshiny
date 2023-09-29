#' Render Funkyheatmap
#'
#' @param data The overall data of this benchmarking study
#'
#' @importFrom tibble tribble
#' @importFrom RColorBrewer brewer.pal
#' @importFrom dplyr %>% mutate select n rename
#' @importFrom formattable formattable color_bar proportion formatter icontext style color_tile
#' @importFrom tidyr drop_na replace_na
#'
#'
render_funkyheatmap <- function(data){

  summary_plot <- data %>%
    dplyr::select(" ",
                  "Method",
                  # "URL",
                  # "Vignette",
                  "Language",
                  "Prior Information",
                  "Accuracy",
                  "Functionality",
                  "Scalability",
                  "Usability",
                  "Overall",
                  "Stability",
                  "recommend") %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c("Functionality")), ~ tidyr::replace_na(.x, 0))
    ) %>%
    formattable::formattable(align = c("c", "l", "c", "l", "l", "l", "l", "l", "l", "c"),
                             list(` ` = formattable::formatter("span",
                                                              x ~ formattable::icontext(ifelse(x > 0, "thumbs-up", ""),
                                                                                        ifelse(x > 0, "", "")),
                                                              style = x ~ formattable::style(color = ifelse(x > 0, "green", ""),
                                                                                             "font-weight" = ifelse(x > 0, "bold", NA),
                                                                                             "font-size" = ifelse(x >= 0, "13px", "10px"))),
                                  # URL = formattable::formatter("a",
                                  #                              href = x ~ sprintf("%s", x),
                                  #                              x ~ formattable::icontext("ok",
                                  #                                                        "link"),
                                  #                              style = x ~ formattable::style(color = "purple", "font-size" = "13px")),
                                  # Vignette = formattable::formatter("span",
                                  #                                   href = x ~ sprintf("http://47.254.148.113/software/Simsite/references/methods/%s", x),
                                  #                                   x ~ formattable::icontext(ifelse(!is.null(x), "link", ""),
                                  #                                                             "link"),
                                  #                                   style = x ~ formattable::style(color = "purple", "font-size" = "13px")),
                                  Method = formattable::formatter("span",
                                                                  style = ~ formattable::style("font-weight" = ifelse(recommend > 0, "bold", NA),
                                                                                               "font-size" = ifelse(recommend >= 0, "13px", "10px"))),
                                  Language = formattable::formatter("span",
                                                                    style = x ~ formattable::style("font-size" = "13px")),
                                  `Prior Information` = formattable::formatter("span",
                                                                               style = x ~ formattable::style("font-size" = "13px")),
                                  Accuracy = formattable::color_bar(RColorBrewer::brewer.pal(9, "Blues")[5]),
                                  Functionality = formattable::color_bar(RColorBrewer::brewer.pal(9, "Reds")[5]),
                                  Scalability = formattable::color_bar(RColorBrewer::brewer.pal(9, "Greens")[5]),
                                  Usability = formattable::color_bar(RColorBrewer::brewer.pal(9, "YlOrBr")[5]),
                                  Overall = formattable::color_bar(RColorBrewer::brewer.pal(9, "Greys")[5]),
                                  Stability = formattable::formatter("span",
                                                                     x ~ formattable::icontext(ifelse(x == "Unstable", "exclamation-sign", ""),
                                                                                               ifelse(x == "Unstable", "Unstable", "Stable")),
                                                                     style = x ~ formattable::style(color = ifelse(x == "Unstable", "red", "green"),
                                                                                                    "font-size" = "13px")),
                                  recommend = FALSE))
  summary_plot
}


render_funkyheatmap_accuracy <- function(data){

  accuracy_data <- data %>%
    dplyr::select(c(159, 1, 158, 11, 15:22, 61:62)) %>%
    dplyr::rename(
      "scRNA-seq data" = "acc_scRNA-seq data",
      "ST data" = "acc_spatial transcriptome data",
      "BH_distance" = "bhattacharyya"
    ) %>%
    dplyr::mutate(
      dplyr::across(5:14, ~ round(.x, digits = 2))
    ) %>%
    dplyr::mutate(
      dplyr::across(4:14, ~ tidyr::replace_na(.x, NA))
    )

  summary_plot <- accuracy_data %>%
    formattable::formattable(align = c("c", "l", "l", rep("l", 11)),
                             list(` ` = formattable::formatter("span",
                                                               x ~ formattable::icontext(ifelse(x > 0, "thumbs-up", ""),
                                                                                         ifelse(x > 0, "", "")),
                                                               style = x ~ formattable::style(color = ifelse(x > 0, "green", ""),
                                                                                              "font-weight" = ifelse(x > 0, "bold", NA),
                                                                                              "font-size" = ifelse(x >= 0, "13px", "10px"))),
                                  Method = formattable::formatter("span",
                                                                  style = ~ formattable::style("font-weight" = ifelse(recommend > 0, "bold", NA),
                                                                                               "font-size" = ifelse(recommend >= 0, "13px", "10px"))),
                                  ### RColorBrewer::brewer.pal(9, "Blues")[c(6,2)]
                                  Accuracy = formattable::color_tile("#4292C6", "#DEEBF7"),
                                  ### RColorBrewer::brewer.pal(9, "PuBuGn")[c(6,2)]
                                  KDE= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  KS= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  MAD= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  MAE= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  OV= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  RMSE= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  BH_distance= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  multiKS= formattable::color_tile("#3690C0", "#ECE2F0"),
                                  `scRNA-seq data` = formattable::color_tile("#1D91C0", "#EDF8B1"),
                                  `ST data` = formattable::color_tile("#1D91C0", "#EDF8B1"),
                                  recommend = FALSE))
  summary_plot
}


render_funkyheatmap_functionality <- function(data){

  functionality_data <- data %>%
    dplyr::select(c(159, 1, 158, 12, 69, 76, 84, 92, 97:98)) %>%
    dplyr::rename(
      "Group score" = "Group_score",
      "DEGs score" = "DEGs_score",
      "Batch score" = "Batch_score",
      "Trajectory score" = "Trajectory_score",
      "scRNA-seq data" = "func_scRNA-seq data",
      "ST data" = "func_spatial transcriptome data",
    ) %>%
    dplyr::mutate(
      dplyr::across(5:10, ~ round(.x, digits = 2))
    ) %>%
    dplyr::mutate(
      dplyr::across(4:10, ~ tidyr::replace_na(.x, NA))
    )

  summary_plot <- functionality_data %>%
    formattable::formattable(align = c("c", "l", "l", rep("l", 7)),
                             list(` ` = formattable::formatter("span",
                                                               x ~ formattable::icontext(ifelse(x > 0, "thumbs-up", ""),
                                                                                         ifelse(x > 0, "", "")),
                                                               style = x ~ formattable::style(color = ifelse(x > 0, "green", ""),
                                                                                              "font-weight" = ifelse(x > 0, "bold", NA),
                                                                                              "font-size" = ifelse(x >= 0, "13px", "10px"))),
                                  Method = formattable::formatter("span",
                                                                  style = ~ formattable::style("font-weight" = ifelse(recommend > 0, "bold", NA),
                                                                                               "font-size" = ifelse(recommend >= 0, "13px", "10px"))),
                                  ### RColorBrewer::brewer.pal(9, "Reds")[c(6,2)]
                                  Functionality = formattable::color_tile("#EF3B2C", "#FEE0D2"),
                                  ### RColorBrewer::brewer.pal(9, "RdPu")[c(6,2)]
                                  formattable::area(col = 5:8) ~ formattable::color_tile("#DD3497", "#FDE0DD"),
                                  `scRNA-seq data` = formattable::color_tile("#1D91C0", "#EDF8B1"),
                                  `ST data` = formattable::color_tile("#1D91C0", "#EDF8B1"),
                                  recommend = FALSE))
  summary_plot
}



render_funkyheatmap_scalability <- function(data){

  scalability_data <- data %>%
    dplyr::select(c(159, 1, 158, 13, 122:127)) %>%
    dplyr::rename(
      "Estimation time" = "estimation time",
      "Estimation memory" = "estimation memory",
      "Simulation time" = "simulation time",
      "Simulation memory" = "simulation memory",
      "Time" = "time",
      "Memory" = "memory",
    ) %>%
    dplyr::mutate(
      dplyr::across(5:10, ~ round(.x, digits = 2))
    ) %>%
    dplyr::mutate(
      dplyr::across(4:10, ~ tidyr::replace_na(.x, NA))
    )

  summary_plot <- scalability_data %>%
    formattable::formattable(align = c("c", "l", "l", rep("l", 7)),
                             list(` ` = formattable::formatter("span",
                                                               x ~ formattable::icontext(ifelse(x > 0, "thumbs-up", ""),
                                                                                         ifelse(x > 0, "", "")),
                                                               style = x ~ formattable::style(color = ifelse(x > 0, "green", ""),
                                                                                              "font-weight" = ifelse(x > 0, "bold", NA),
                                                                                              "font-size" = ifelse(x >= 0, "13px", "10px"))),
                                  Method = formattable::formatter("span",
                                                                  style = ~ formattable::style("font-weight" = ifelse(recommend > 0, "bold", NA),
                                                                                               "font-size" = ifelse(recommend >= 0, "13px", "10px"))),
                                  ### RColorBrewer::brewer.pal(9, "Greens")[c(6,2)]
                                  Scalability = formattable::color_tile("#41AB5D", "#E5F5E0"),
                                  ### RColorBrewer::brewer.pal(9, "GnBu")[c(6,2)]
                                  formattable::area(col = 5:10) ~ formattable::color_tile("#4EB3D3", "#E0F3DB"),
                                  recommend = FALSE))
  summary_plot
}


render_funkyheatmap_usability <- function(data){

  usability_data <- data %>%
    dplyr::rename(
      "Error" = "error",
    ) %>%
    dplyr::select(c(159, 1, 158, 14, 148:153, 66:67)) %>%
    dplyr::mutate(
      dplyr::across(5:10, ~ round(.x, digits = 2))
    ) %>%
    dplyr::mutate(
      dplyr::across(4:10, ~ tidyr::replace_na(.x, NA))
    )

  summary_plot <- usability_data %>%
    formattable::formattable(align = c("c", "l", "l", rep("l", 7)),
                             list(` ` = formattable::formatter("span",
                                                               x ~ formattable::icontext(ifelse(x > 0, "thumbs-up", ""),
                                                                                         ifelse(x > 0, "", "")),
                                                               style = x ~ formattable::style(color = ifelse(x > 0, "green", ""),
                                                                                              "font-weight" = ifelse(x > 0, "bold", NA),
                                                                                              "font-size" = ifelse(x >= 0, "13px", "10px"))),
                                  Method = formattable::formatter("span",
                                                                  style = ~ formattable::style("font-weight" = ifelse(recommend > 0, "bold", NA),
                                                                                               "font-size" = ifelse(recommend >= 0, "13px", "10px"))),
                                  ### RColorBrewer::brewer.pal(9, "YlOrBr")[c(6,2)]
                                  Usability = formattable::color_tile("#EC7014", "#FFF7BC"),
                                  ### RColorBrewer::brewer.pal(9, "YlOrRd")[c(6,2)]
                                  formattable::area(col = 5:10) ~ formattable::color_tile("#FC4E2A", "#FFEDA0"),
                                  Error = formattable::formatter("span",
                                                                 style = ~ formattable::style("font-size" = ifelse(error_proportion > 0.3, "13px", "10px"),
                                                                                              "color" = ifelse(error_proportion > 0.3, "red", "green"))),
                                  recommend = FALSE,
                                  error_proportion = FALSE))
  summary_plot
}

