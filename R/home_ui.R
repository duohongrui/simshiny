# home_ui <- function(){
#   tabPanel(
#     ###############
#     ###  home ####
#     ###############
#     title = "Home",
#     tags$style("ul li {
#                  padding: 0px 10px 0px 10px;
#                  text-align: center
#                }"),
#     tags$h3(
#       "Simsite provides a convenient web server to choose suitable methods and perform simulation tasks"
#     ),
#     imageOutput("home_img"),
#     tags$h4(" dataset posted on"),
#     tags$a(
#       href="https://figshare.shef.ac.uk/articles/dataset/Hadfield_Green_Roof_5-year_Dataset/11876736",
#       "Click here for data!"
#     ),
#     tags$h5("Data collected as part of the EU funded 'Collaborative research and development of green roof system technology' project from the Sheffield, UK, green roof testbeds."),
#     h5("Data includes 5 years of:"),
#     tags$li("Rainfall data (1-minute resolution)"),
#     tags$li("Green roof runoff data for 9 roof configurations (1-minute resolution)"),
#     tags$li("Soil moisture content at 3 depths for 4 roof configurations (5-minute resolution)"),
#     tags$li("Climate data sufficient to calculate FAO-56 Penman-Monteith (1-hour resolution)"),
#     h5("Due to difficulties in monitoring testbed runoff, there are occasions where runoff data is considered invalid. A separate data-file indicates individual storm events where runoff data is considered to be valid."),
#     a(href="https://github.com/mitochondrion420/simshiny", "Click here for the GitHub repo!")
#   )
# }
