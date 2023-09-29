#' Prediction Models of Each Method
#'
#' RF models for predicting time and memory with a given number of cell and gene in both parameter estimation and simulation.
#'
"models"


#' Single-cell RNA-seq Data from GSE54695
#'
#' @description A matrix contains the single-cell gene expression data from
#' GSE54695. Total 160 cells are cultured in two types of medium, two-inhibitor (2i)
#' and serum. In addtion, the count matrix contains ERCC spike-in information and
#' it can be very important when uses are intended to simulate datasets by BEARscc
#' and other methods.
#'
#' @source \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE54695}
"data"


#' Group information of 160 Cells in GSE54695
#'
#' @description Group information of 160 clls in GSE54695. Total 160 cells are
#' cultured in two types of medium, 0 for two-inhibitor (2i) and 1 for serum.
#'
"group_condition"


#' Relevent URLs of methods
#'
#' A tibble containing the URLs of methods.
#'
#' @format A tibble:
#' \describe{
#'   \item{\code{short_name}}{The short name of simulation methods}
#'   \item{\code{URL}}{The URLs of method}
#'   \item{\code{Vignette}}{The vignette URLs of methods}
#' }
"method_URL_table"
