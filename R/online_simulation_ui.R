online_simulation_ui <- function(){
  tabPanel(
    ###############
    ###  PAGE3 ####
    ###############
    title = "Online Simulation",
    tags$style("ul li {
                 padding: 0px 10px 0px 10px;
                 text-align: center
               }"),
    tags$style(HTML("
         button.btn.dropdown-toggle.btn-default {
          background-color: white
         }")),
    ### sidebar configuration
    sidebarLayout(
      sidebarPanel(

        ### method
        shinyWidgets::pickerInput(
          inputId = "choose_method",
          label = tags$h5("Choose your simulation method"),
          choices = c("SPARSim",
                      "Splat",
                      "SCRIP-GP-trendedBCV",
                      "SCRIP-GP-commonBCV",
                      "SCRIP-BGP-commonBCV",
                      "SCRIP-BP",
                      "SCRIP-BGP-trendedBCV",
                      "SplatPop",
                      "SplatPop-paths",
                      "Splat-paths",
                      "SCRIP-paths",
                      "scDesign3-tree",
                      "VeloSim",
                      "dyntoy",
                      "scDesign",
                      "Lun",
                      "muscat",
                      "scDesign3",
                      "SRTsim",
                      "scDesign2",
                      "Lun2",
                      "zinbwave",
                      "Kersplat",
                      "dropsim"),
          selected = "Splat",
          options = list(
            `actions-box` = TRUE,
            showContent = FALSE),
          choicesOpt = list(style = rep(("color:black;"), 39))
        ),
        ### cell number
        numericInput(inputId = "cell_num_simulate",
                     label = tags$h5("Cell Number"),
                     value = 1000,
                     min = 1,
                     max = 30000),
        ### gene number
        numericInput(inputId = "gene_num_simulate",
                     label = tags$h5("Gene Number"),
                     value = 1000,
                     min = 1,
                     max = 30000),
        ### functionality
        checkboxGroupInput(
          inputId = "functionality",
          label = tags$h5("Which type(s) of data do you need?"),
          choices = c("Count matrix" = "matrix",
                      "Cell groups" = "group",
                      "DEGs" = "DEGs",
                      "Cell batches" = "batch",
                      "Trajectory" = "trajectory",
                      "Spatial transcriptome" = "spatial"),
          selected = "matrix"
        ),
        ### group number
        conditionalPanel(condition = "input.functionality.indexOf('group') > -1",
                         numericInput(inputId = "group_num",
                                      label = tags$h5("Cell group number"),
                                      value = 2,
                                      min = 1,
                                      max = 100)
        ),
        ### DEGs proportion
        conditionalPanel(condition = "input.functionality.indexOf('DEGs') > -1",
                         numericInput(inputId = "DEGs_prop",
                                      label = tags$h5("Proportion of DEGs"),
                                      value = 0.2,
                                      min = 0,
                                      max = 1),
        ),
        ### batch number
        conditionalPanel(condition = "input.functionality.indexOf('batch') > -1",
                         numericInput(inputId = "batch_num",
                                      label = tags$h5("Cell batch number"),
                                      value = 2,
                                      min = 1,
                                      max = 100),
        ),

        ### Whether to load own reference data
        radioGroupButtons(
          inputId = "load_data",
          label = tags$h5("Do you load your own reference data?"),
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),

        ### upload new file
        conditionalPanel(condition = "input.load_data == 'yes'",
                         tags$h5("Upload Reference Data"),
                         tags$h6("A gene expression matrix with cells on the column and genes on the rows (.csv, .tsv or .rds). This file should be no more than 1GB.", style = "color: black;"),
                         fileInput("upload", NULL, accept = c(".csv", ".tsv", ".rds"), buttonLabel = "Upload")
                         ),

        ### Whether to load own reference data
        radioGroupButtons(
          inputId = "load_prior",
          label = tags$h5("Do you load your prior information?"),
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),

        ### upload new file
        conditionalPanel(condition = "input.load_prior == 'yes'",
                         tags$h5("Upload Prior Information"),
                         tags$h6("First column is the cell ids named as cell.name; the column of cell group labels should be named as group.condition; the column of cell batch labels should be named as batch.condition (.csv, .tsv or .rds).", style = "color: black;"),
                         fileInput("upload_prior", NULL, accept = c(".csv", ".tsv", ".rds"), buttonLabel = "Upload Prior"),
                         span(textOutput("check_own_data"), style = "color:red; font-weight:bold"),
                         span(textOutput("check_own_both"), style = "color:red; font-weight:bold"),
                         span(textOutput("spatial_info"), style = "color:orange; font-weight:bold")
        ),

        br(),
        ### Click and simulation
        actionBttn(
          inputId = "simulation",
          label = "Start Simulation!",
          style = "material-flat",
          color = "danger",
          onclick = "Shiny.setInputValue('has_click', this.innerText);"
        ),
        br(),
        br(),
        span(textOutput("error_return"), style = "color:red; font-weight:bold"),
        span(textOutput("please_download"), style = "color:purple; font-weight:bold"),
        # ### when simulating, the signal will be given to users
        # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
        #                  tags$div("Simulating data...Please wait...", id = "loadmessage")),
        br(),
        uiOutput("download_1", inline = TRUE),
        uiOutput("download_2", inline = TRUE)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Reference data",
                   br(),
                   span(textOutput("data_text"), style = "color:purple; font-weight:bold; font-size:17px"),
                   tableOutput("data"),
                   conditionalPanel("input.load_prior == 'yes'",
                                    span(textOutput("prior_text"), style = "color:purple; font-weight:bold; font-size:17px")),
                   tableOutput("prior")),
          tabPanel(title = "Simulated data",
                   br(),
                   span(textOutput("simulated_data_text"), style = "color:purple; font-weight:bold; font-size:17px"),
                   tableOutput("simulated_data"),
                   span(textOutput("cell_info_text"), style = "color:purple; font-weight:bold; font-size:17px"),
                   tableOutput("meta_cells"),
                   span(textOutput("gene_info_text"), style = "color:purple; font-weight:bold; font-size:17px"),
                   tableOutput("meta_genes")),
          tabPanel(title = "Output assessment",
                   br(),
                   plotOutput("umap", width = "100%", height = "400px") %>% shinycssloaders::withSpinner(),
                   plotOutput("property", width = "100%", height = "600px") %>% shinycssloaders::withSpinner())

        )
      )
    )
  )
}
