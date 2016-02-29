# User Interface

# Corsica Team App
# Last edited 2-28-2016
# Manny

require(shinydashboard)

shinyUI(navbarPage("Teams", id = "tab", inverse = F, windowTitle = "Corsica | Teams",
                   
                   tabPanel("Team Stats", value = "stats",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                # Formatting      
                                tags$head(tags$style(".container-fluid {font-size: 13px; color: #2B547E; background-color: #ECF0F5;}")),
                                tags$head(tags$style(".box {background-color: #F5F5F5; border-color: #4863A0; border: solid; border-width: 1px;}")),
                                tags$head(tags$style(".box-header {background-color: #4863A0; color: #FFFFFF;}")),
                                tags$head(tags$style(".rightAlign{float:right;} .bottom{height:50px}")),
                                tags$head(tags$style(".center {text-align: center; margin: 23px 0 0 0;}")),
                                tags$head(tags$style(".center-low {text-align: center; margin: 29px 0 0 0;}")),
                                tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}", ".shiny-output-error:before { visibility: hidden;}"),
                                tags$style(".navbar-default {background-color: #4863A0; border-color: #ffffff;}"),
                                tags$style(".navbar-default .navbar-nav li a {background-color: #4863A0; color: #ffffff;}"),
                                tags$style(".navbar-default .navbar-nav .active a {background-color: #ffffff; color: #4863A0;}"),
                                tags$style(".navbar-default .navbar-brand {background-color: #4863A0; color: #ffffff;}"),
                                tags$style(".dataTable thead tr {background-color: #4863A0; color: #ffffff;}"),
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("Team Stats")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(6, helpText("Loading the data may take a few seconds. Thanks for your patience.")),
                                  column(6, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(2, uiOutput("s1")),
                                  column(2, selectInput("strength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4"), selected = "5v5")),
                                  column(2, selectInput("venue", "Venue", choices = c("Any", "Home", "Away"), selected = "Any")),
                                  column(3, selectInput("adjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                                  column(2, selectInput("type", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular"))
                                ),
                                
                                fluidRow(
                                  column(2, uiOutput("s2")),
                                  column(2, selectInput("score", "Score State", choices = c("Any", "Leading", "Trailing", "Even", "+3", "+2", "+1", "-1", "-2", "-3"), selected = "Any")),
                                  column(2, selectInput("report", "Report", choices = c("On-Ice", "Context", "Counts"), selected = "On-Ice")),
                                  column(2, tags$div(class = "center", downloadButton("dl", "Download File"))),
                                  column(3, tags$div(class = "center-low", checkboxInput("aggregate", "Aggregate Seasons", value = TRUE)))
                                ),
                                
                                # Table output
                                DT::dataTableOutput("t1")
                                
                              )
                            )),
                   
                   tabPanel("Custom Query", value = "custom",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("Custom Query")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(6, helpText("Set your desired parameters, then press the Load button. Try to limit your query to only the desired information in order to hasten the load time.")),
                                  column(6, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(3, dateRangeInput("date", "Date Range", min = "2007-10-01", max = Sys.Date(), start = "2015-10-01", end = Sys.Date(), format = "yyyy-mm-dd")),
                                  column(2, selectInput("qstrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
                                  column(2, selectInput("qvenue", "Venue", choices = c("Any", "Home", "Away"), selected = "Any")),
                                  column(2, tags$div(class = "center", downloadButton("qdl", "Download File"))),
                                  column(3, tags$div(class = "center-low", checkboxInput("qaggregate", "Aggregate Games", value = TRUE)))
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(3, selectInput("qadjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                                  column(2, selectInput("qscore", "Score State", choices = c("Any", "Leading", "Trailing", "Even"), selected = "Any")),
                                  column(2, uiOutput("qteam")),
                                  column(2, selectizeInput("columns", "Report", choices = c("Corsi", "Fenwick", "Shots on goal", "Goals", "Expected Goals", "Extras"), selected = NULL, multiple = TRUE)),
                                  column(2, tags$div(class = "center", actionButton("go", "Load", width = "100%"), style = "border-radius: 4px; box-shadow: 3px 3px 3px;"))
                                ),
                                
                                # Output
                                DT::dataTableOutput("t2")
                                
                              )
                              
                            )),
                   
                   tabPanel("Rolling Average", value = "average",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("Rolling Average")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(6, helpText("Compute and plot rolling averages according to custom specifications.")),
                                  column(6, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(1),
                                  column(4, dateRangeInput("s1date", "Date Range", min = "2007-10-01", max = Sys.Date(), start = "2015-10-01", end = Sys.Date(), format = "yyyy-mm-dd")),
                                  column(2, uiOutput("s1team")),
                                  column(2, selectInput("s1strength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
                                  column(2, selectInput("s1measure", "Measure", choices = c("CF%", "FF%", "SF%", "GF%", "xGF%",
                                                                                            "CF60", "CA60", "FF60", "FA60",
                                                                                            "SF60", "SA60", "GF60", "GA60",
                                                                                            "xGF60", "xGA60",
                                                                                            "Sh%", "Sv%", "PDO"), selected = "CF%")),
                                  column(1)
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(3),
                                  column(6,
                                         column(6, numericInput("n", "Games in average", value = 25, min = 1, max = 50)),
                                         column(6, selectInput("s1adjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None"))
                                  ),
                                  column(3)
                                ),
                                
                                # Output
                                uiOutput("panel")
                                
                              )
                              
                            ))
                   
))