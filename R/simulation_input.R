# get_simulation_input <- function(input){
#   ############################################################################
#   ############################# Get responses ################################
#   ############################################################################
#   reactive({
#     ### method
#     method <- input$choose_method
#
#     ### cell and gene number
#     cell_num <- input$cell_num
#     gene_num <- input$gene_num
#
#     ### click
#     click <- input$simulation
#
#     simulation_info <- list(
#       method = method,
#       cell_num = cell_num,
#       gene_num = gene_num,
#       click = click
#     )
#     simulation_info
#   })
#
# }
#
#
