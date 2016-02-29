# User Interface

# Corsica Combo App
# Last edited 2-28-2016
# Manny

require(shinydashboard)

shinyUI(navbarPage("Combos", id = "tab", inverse = F, windowTitle = "Corsica | Combos",
                   
                   tabPanel("Lines", value = "line",
                            
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
                                  column(6, h2("Line Stats")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(6, helpText("Loading the data may take a few seconds. Thanks for your patience.")),
                                  column(6, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(2, uiOutput("l1")),
                                  column(2, selectInput("lstrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "3v3"), selected = "5v5")),
                                  column(2, selectInput("ladjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                                  column(2, selectInput("ltype", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular")),
                                  column(2, uiOutput("l3")),
                                  column(2, tags$div(class = "center-low", checkboxInput("laggregate", "Aggregate Seasons", value = TRUE)))
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(2, uiOutput("l2")),
                                  column(2, selectInput("lreport", "Report", choices = c("On-Ice", "Off-Ice", "Relative", "Individual", "Context", "Counts"), selected = "On-Ice")),
                                  column(3, sliderInput("ltoi", "TOI Minimum", min = 0, max = 5000, value = 50, step = 10)),
                                  column(3, uiOutput("lname")),
                                  column(2, tags$div(class = "center", downloadButton("ldl", "Download File")))
                                ),
                                
                                # Output
                                DT::dataTableOutput("t1")
                                
                              )
                              
                            )),
                   
                   tabPanel("Pairings", value = "pair",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("Pairing Stats")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(6, helpText("Loading the data may take a few seconds. Thanks for your patience.")),
                                  column(6, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(2, uiOutput("p1")),
                                  column(2, selectInput("pstrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "3v3"), selected = "5v5")),
                                  column(2, selectInput("padjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                                  column(2, selectInput("ptype", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular")),
                                  column(2, uiOutput("p3")),
                                  column(2, tags$div(class = "center-low", checkboxInput("paggregate", "Aggregate Seasons", value = TRUE)))
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(2, uiOutput("p2")),
                                  column(2, selectInput("preport", "Report", choices = c("On-Ice", "Off-Ice", "Relative", "Individual", "Context", "Counts"), selected = "On-Ice")),
                                  column(3, sliderInput("ptoi", "TOI Minimum", min = 0, max = 5000, value = 50, step = 10)),
                                  column(3, uiOutput("pname")),
                                  column(2, tags$div(class = "center", downloadButton("pdl", "Download File")))
                                ),
                                
                                # Output
                                DT::dataTableOutput("t2")
                                
                              )
                              
                            )),
                   
                   tabPanel("WOWY", value = "wowy",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("With Or Without You")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(6, helpText("Set your desired parameters, then press the Load button. Try to limit your query to only the desired information in order to hasten the load time.")),
                                  column(6, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(2),
                                  column(8,
                                         column(7, dateRangeInput("wdate", "Date Range", min = "2007-10-01", max = Sys.Date(), start = "2015-10-01", end = Sys.Date(), format = "yyyy-mm-dd")),
                                         column(5, uiOutput("wname"))
                                  ),
                                  column(2)
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(1),
                                  column(10,
                                         column(5, selectInput("wadjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                                         column(2, selectInput("wvenue", "Venue", choices = c("Any", "Home", "Away"), selected = "Any")),
                                         column(3, selectInput("wstrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
                                         column(2, tags$div(class = "center", actionButton("go", "Load", width = "100%"), style = "border-radius: 4px; box-shadow: 3px 3px 3px;"))
                                  ),
                                  column(1)
                                ),
                                
                                # Dashboard output
                                uiOutput("dash")
                                
                              )
                              
                            )),
                   
                   tabPanel("Assists", value = "assists",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                tags$div(class = "soon", style = "text-align: center; padding: 250px 0 275px 0;", tags$h2("Coming soon..."))
                                
                              )
                              
                            ))
                   
))


