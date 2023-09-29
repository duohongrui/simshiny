method_choice_ui <- function(){
  tabPanel(
    title = "Method Choice",
    tags$style("ul li {
                 padding: 0px 10px 0px 10px;
                 text-align: center
               }"),
    ###############
    ###  PAGE2 ####
    ###############
    sidebarLayout(
      sidebarPanel(
        style = "height: 100vh; overflow-y: auto; ",
        ### whether need functionality
        radioGroupButtons(
          inputId = "determine_function",
          label = tags$h5("Do you want to simulate a dataset with cell groups, cell batches, DEGs, or trajectories?"),
          choices = c("Yes" = "yes", "No" = "no"),
          selected = character(0)
        ),
        conditionalPanel(
          condition = "input.determine_function == 'yes'",
          ### which functionality
          checkboxGroupInput(inputId = "determine_which_function",
                             label = tags$h5("Which type(s) of the data (or method functionality) do you need?"),
                             choices = c("Cell groups" = "group",
                                         "DEGs" = "DEGs",
                                         "Cell batches" = "batch",
                                         "Trajectory" = "trajectory")),
          ### which prior information
          checkboxGroupInput(inputId = "determine_which_prior",
                             label = tags$h5("Which type(s) of prior information can you provide?"),
                             choices = c("none")),
          ### trade-off between the performance of functionality and accuracy
          prettyRadioButtons(inputId = "trade_off",
                             label = tags$h5("Which aspect of method performance do you care about?"),
                             choices = c("Accuracy on the similarity between the simulated and real data" = "accuracy",
                                         "Functionality performance under different simulation scenarios" = "functionality"),
                             thick = TRUE,
                             selected = "accuracy"),
        ),
        conditionalPanel(
          condition = "input.determine_function == 'no'",
          ### trade-off between the performance of functionality and accuracy
          prettyRadioButtons(inputId = "trade_off2",
                             label = tags$h5("Which aspect of method performance do you care about more?"),
                             choices = c("Accuracy on the similarity between the simulated and real data" = "accuracy",
                                         "Scalability on the execution time and memory usage" = "scalability"),
                             thick = TRUE,
                             selected = "accuracy"),
        ),
        ### whether need spatial data
        radioGroupButtons(
          inputId = "determine_spatial",
          label = tags$h5("Do you want to simulate spatial transcriptome data?"),
          choices = c("Yes" = "yes", "No" = "no"),
          selected = character(0)
        ),
        ### cell number
        numericInput(inputId = "cell_num",
                     label = tags$h5("Cell Number"),
                     value = 1000,
                     min = 10,
                     max = 20000),
        sliderInput(inputId = "cell_num_slide",
                    label = NULL,
                    value = 1000,
                    step = 10,
                    min = 10,
                    max = 20000),

        ### gene number
        numericInput(inputId = "gene_num",
                     label = tags$h5("Gene Number"),
                     value = 1000,
                     min = 100,
                     max = 40000),
        sliderInput(inputId = "gene_num_slide",
                    label = NULL,
                    value = 1000,
                    step = 10,
                    min = 100,
                    max = 40000),
        br(),
        tags$h5("Expected Time Consuming"),

        ### time bottom
        radioGroupButtons(
          inputId = "time_step",
          label = tags$h5("Step"),
          choices = c("Estimation" = "estimation", "Simulation" = "simulation"),
          selected = "estimation"
        ),

        ### time
        conditionalPanel(condition = "input.time_step == 'estimation'",
                         sliderTextInput(inputId = "time_estimation",
                                         label = NULL,
                                         choices = time_convert(c(seq(10, 60, 5),
                                                                  seq(300, 3600, 300),
                                                                  seq(5400, 3600*4, 1800))),
                                         selected = "5m",
                                         grid = TRUE)
                         ),
        conditionalPanel(condition = "input.time_step == 'simulation'",
                         sliderTextInput(inputId = "time_simulation",
                                         label = NULL,
                                         choices = time_convert(c(seq(10, 60, 5),
                                                                  seq(300, 3600, 300),
                                                                  seq(60*60*1.5, 60*60*6, 1800))),
                                         selected = "60s",
                                         grid = TRUE)
        ),

        ### memory
        br(),
        tags$h5("Expected Memory Usage"),
        ### memory bottom
        radioGroupButtons(
          inputId = "memory_step",
          label = tags$h5("Step"),
          choices = c("Estimation" = "estimation", "Simulation" = "simulation"),
          selected = "estimation"
        ),
        conditionalPanel(condition = "input.memory_step == 'estimation'",
                         sliderTextInput(inputId = "memory_estimation",
                                         label = NULL,
                                         choices = memory_convert(c(seq(100, 2^10, 100),
                                                                    seq(2^10*1.5, 2^10*4, 500),
                                                                    seq(2^10*5, 2^10*50, 2^10*5))),
                                         selected = "5GB",
                                         grid = TRUE)),
        conditionalPanel(condition = "input.memory_step == 'simulation'",
                         sliderTextInput(inputId = "memory_simulation",
                                         label = NULL,
                                         choices = memory_convert(c(seq(100, 2^10, 100),
                                                                    seq(2^10*1.5, 2^10*4, 500),
                                                                    seq(2^10*5, 2^10*30, 2^10*5),
                                                                    seq(2^10*50, 2^10*512, 2^10*50))),
                                         selected = "5GB",
                                         grid = TRUE)
                         )
        # ### criteria_weights
        # conditionalPanel(
        #   condition = "input.determine_function == 'no'",
        #   br(),
        #   tags$h5("Weights of the evaluation criteria"),
        #   tags$h6("The sum of weights should be sum to 1!!!", style = "color: red;"),
        #   ### criteria_accuracy
        #   numericInput(inputId = "criteria_accuracy",
        #                label = tags$h5("Accuracy"),
        #                value = 0.33,
        #                min = 0,
        #                max = 1),
        #   ### criteria_scalability
        #   numericInput(inputId = "criteria_scalability",
        #                label = tags$h5("Scalability"),
        #                value = 0.33,
        #                min = 0,
        #                max = 1),
        #   ### criteria_usability
        #   numericInput(inputId = "criteria_usability",
        #                label = tags$h5("Usability"),
        #                value = 0.34,
        #                min = 0,
        #                max = 1),
        # ),
        # conditionalPanel(
        #   br(),
        #   tags$h5("Weights of the evaluation criteria"),
        #   tags$h6("The sum of weights should be sum to 1!!!", style = "color: red;"),
        #   condition = "input.determine_function == 'yes'",
        #   ### criteria_accuracy
        #   numericInput(inputId = "criteria_accuracy",
        #                label = tags$h5("Accuracy"),
        #                value = 0.25,
        #                min = 0,
        #                max = 1),
        #   ### criteria_functionality
        #   numericInput(inputId = "criteria_functionality",
        #                label = tags$h5("Functionality"),
        #                value = 0.25,
        #                min = 0,
        #                max = 1),
        #   ### criteria_scalability
        #   numericInput(inputId = "criteria_scalability",
        #                label = tags$h5("Scalability"),
        #                value = 0.25,
        #                min = 0,
        #                max = 1),
        #   ### criteria_usability
        #   numericInput(inputId = "criteria_usability",
        #                label = tags$h5("Usability"),
        #                value = 0.25,
        #                min = 0,
        #                max = 1)
        # ),
        #
        # ### error rate
        # br(),
        # sliderTextInput(
        #   inputId = "error_proportion",
        #   label = tags$h5("The error probability of methods in executions"),
        #   grid = TRUE,
        #   force_edges = TRUE,
        #   selected = "<10%",
        #   choices = c("0%", paste0("<", as.character(seq(5, 90, 5)), "%"))
        # )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Overall Performance", formattable::formattableOutput("overall_recommendation")%>% shinycssloaders::withSpinner()),
          tabPanel("Accuracy", formattable::formattableOutput("accuracy_recommendation")%>% shinycssloaders::withSpinner()),
          tabPanel("Functionality", formattable::formattableOutput("functionality_recommendation")%>% shinycssloaders::withSpinner()),
          tabPanel("Scalability", formattable::formattableOutput("scalability_recommendation")%>% shinycssloaders::withSpinner()),
          tabPanel("Usability", formattable::formattableOutput("usability_recommendation")%>% shinycssloaders::withSpinner())
        )
      )
    )
  )
}
