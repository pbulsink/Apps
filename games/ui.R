########################################################################################################################################################################################################
######################## TESTING #######################################################################################################################################################################
########################################################################################################################################################################################################
# User Interface

# Corsica Games App
# Last edited 3-11-2016
# Manny

require(shinydashboard)

shinyUI(navbarPage("Games", id = "tab", inverse = F, windowTitle = "Corsica | Games",
                   
                   tabPanel("Game Stats", value = "games",
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
                                tags$style(".dataTable thead tr {background-color: #F5F5F5; color: #2B547E;}"),
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("Games")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(9, helpText("Select a game from the menu below. Loading the data may take a few seconds. Thanks for your patience.")),
                                  column(3, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Game menu
                                fluidRow(
                                  column(2),
                                  box(
                                    fluidRow(
                                      column(6, uiOutput("date")),
                                      column(6, uiOutput("id"))
                                    ),
                                    width = 8,
                                    title = "Select Game",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                  ),
                                  column(2)
                                ),
                                
                                tabsetPanel(
                                  
                                  tabPanel("Summary", id = "sum",
                                           
                                           fluidRow(
                                             box(
                                               column(2, radioButtons("team1", "Game State", choices = list("All Situations", "5v5"), selected = "5v5")),
                                               column(10, DT::dataTableOutput("t1")),
                                               width = 12,
                                               title = "Teams",
                                               solidHeader = TRUE,
                                               collapsible = TRUE
                                             )
                                           ),
                                           
                                           fluidRow(
                                             box(
                                               DT::dataTableOutput("g1"),
                                               width = 5,
                                               title = "Goalies",
                                               solidHeader = TRUE,
                                               collapsible = TRUE
                                             ),
                                             box(
                                               DT::dataTableOutput("p1"),
                                               width = 7,
                                               title = "Shots by Period",
                                               solidHeader = TRUE,
                                               collapsible = TRUE
                                             )
                                           ),
                                           
                                           fluidRow(
                                             box(
                                               fluidRow(
                                                 column(2),
                                                 column(2, radioButtons("plot1", "Game State", choices = list("All Situations", "5v5"), selected = "5v5")),
                                                 column(2, radioButtons("plot3", "Adjustment", choices = list("None", "Score and Venue"), selected = "None")),
                                                 column(3, selectInput("plot2", "Measure", choices = list("CF", "SF", "xG"), selected = "CF"))
                                               ),
                                               plotOutput("s1"),
                                               width = 12,
                                               title = "Cumulative Shots Chart",
                                               solidHeader = TRUE,
                                               collapsible = TRUE
                                             )
                                           ),
                                           
                                           fluidRow(
                                             uiOutput("box1")
                                           ),
                                           
                                           fluidRow(
                                             uiOutput("box2")
                                           )
                                           
                                  ),
                                  tabPanel("Skaters", id = "skater",
                                           
                                           fluidRow(
                                             uiOutput("skatertable1")
                                           ),
                                           
                                           fluidRow(
                                             uiOutput("skatertable2")
                                           )
                                           
                                  ),
                                  tabPanel("Combos", id = "combo",
                                           
                                           fluidRow(
                                             uiOutput("combotable1")
                                           ),
                                           
                                           fluidRow(
                                             uiOutput("combotable2")
                                           )
                                           
                                  ),
                                  tabPanel("Boxscore", id = "box"),
                                  tabPanel("Stories", id = "stories",
                                           
                                           fluidRow(
                                             box(
                                               uiOutput("stories"),
                                               width = 12,
                                               title = "Stories",
                                               solidHeader = TRUE,
                                               collapsible = TRUE
                                             )
                                           )
                                           
                                  )
                                  
                                )
                                
                              )))
))