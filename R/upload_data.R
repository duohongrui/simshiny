# upload_data <- function(input){
#   reactive({
#     if(is.null(input$upload$name)){
#       NULL
#     }else{
#       ext <- tools::file_ext(input$upload$name)
#       switch(ext,
#              csv = read.table(input$upload$datapath, sep = ",", header = TRUE),
#              tsv = read.table(input$upload$datapath, sep = "\t", header = TRUE),
#              rds = readRDS(input$upload$datapath),
#              validate("Invalid file; Please upload a .csv or .tsv or .rds file of gene expression matrix"))
#     }
#   })
# }
#
#
# upload_prior <- function(input){
#   reactive({
#     if(is.null(input$upload_prior$name)){
#       NULL
#     }else{
#       ext <- tools::file_ext(input$upload_prior$name)
#       switch(ext,
#              csv = read.table(input$upload_prior$datapath, sep = ",", header = TRUE),
#              tsv = read.table(input$upload_prior$datapath, sep = "\t", header = TRUE),
#              rds = readRDS(input$upload_prior$datapath),
#              validate("Invalid file; Please upload a .csv or .tsv or .rds file of gene expression matrix"))
#     }
#   })
# }
