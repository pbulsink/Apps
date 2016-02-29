# User Interface

# Corsica Player App
# Last edited 2-28-2016
# Manny

require(shinydashboard)

shinyUI(navbarPage("Skaters", id = "tab", inverse = F, windowTitle = "Corsica | Skaters",
                   
                   tabPanel("Skater Stats", value = "stats",
                            
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
                                  column(6, h2("Skater Stats")),
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
                                  column(2, selectInput("strength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
                                  column(2, selectInput("venue", "Venue", choices = c("Any", "Home", "Away"), selected = "Any")),
                                  column(3, selectInput("adjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                                  column(2, selectInput("type", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular"))
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(2, uiOutput("s2")),
                                  column(2, selectInput("score", "Score State", choices = c("Any", "Leading", "Trailing", "Even"), selected = "Any")),
                                  column(2, selectInput("report", "Report", choices = c("On-Ice", "Off-Ice", "Relative", "Individual", "Context", "Counts"), selected = "On-Ice")),
                                  column(3, sliderInput("toi", "TOI Minimum", min = 0, max = 5000, value = 50, step = 10)),
                                  column(2, tags$div(class = "center-low", checkboxInput("aggregate", "Aggregate Seasons", value = TRUE)))
                                ),
                                
                                # Input row 3
                                fluidRow(
                                  column(2, selectInput("pos", "Position", choices = c("Any", "Forward", "Defence"), selected = "Any")),
                                  column(2, uiOutput("s3")),
                                  column(4, uiOutput("name")),
                                  column(1),
                                  column(2, tags$div(class = "center", downloadButton("dl", "Download File")))
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
                                  column(4, dateRangeInput("date", "Date Range", min = "2007-10-01", max = Sys.Date(), start = "2015-10-01", end = Sys.Date(), format = "yyyy-mm-dd")),
                                  column(2, selectInput("qstrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
                                  column(2, selectInput("qscore", "Score State", choices = c("Any", "Leading", "Trailing", "Even"), selected = "Any")),
                                  column(2, selectInput("qvenue", "Venue", choices = c("Any", "Home", "Away"), selected = "Any"))
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(4, uiOutput("qname")),
                                  column(2, uiOutput("q3")),
                                  column(2, tags$div(class = "center", downloadButton("qdl", "Download File"))),
                                  column(3, tags$div(class = "center-low", checkboxInput("qaggregate", "Aggregate Games", value = TRUE)))
                                ),
                                
                                # Input row 3
                                fluidRow(
                                  column(3, selectInput("qadjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
                                  column(1),
                                  column(2, selectizeInput("columns", "Report", choices = c("Corsi", "Fenwick", "Shots on goal", "Goals", "Expected Goals", "Scoring", "Individual", "Extras"), selected = NULL, multiple = TRUE)),
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
                                  column(4, dateRangeInput("s1date", "Date Range", min = "2007-10-01", max = Sys.Date(), start = "2015-10-01", end = Sys.Date(), format = "yyyy-mm-dd")),
                                  column(3, uiOutput("s1name")),
                                  column(2, selectInput("s1strength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
                                  column(2, selectInput("s1measure", "Measure", choices = c("CF%", "FF%", "SF%", "GF%", "xGF%",
                                                                                            "Rel.CF%", "Rel.FF%", "Rel.SF%", "Rel.GF%", "Rel.xGF%",
                                                                                            "CF60", "CA60", "FF60", "FA60",
                                                                                            "SF60", "SA60", "GF60", "GA60",
                                                                                            "xGF60", "xGA60",
                                                                                            "iCF60", "iFF60", "iSF60", "ixG60",
                                                                                            "G60", "A60", "P60", "P160",
                                                                                            "Sh%", "Sv%", "PDO",
                                                                                            "iSh%", "ixFSh%"), selected = "CF%")),
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
                              
                            )),
                   
                   tabPanel("Usage Charts", value = "usage",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("Usage Charts")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(6, helpText("Generate usage charts for a selected team or custom list of players.")),
                                  column(6, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(1),
                                  column(2, uiOutput("u1")),
                                  column(2, uiOutput("uteam")),
                                  column(3, uiOutput("uname")),
                                  column(2, selectInput("utype", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular"))
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(1),
                                  column(2, uiOutput("u2")),
                                  column(2, selectInput("ustrength", "Strength State", choices = c("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
                                  column(3, sliderInput("utoi", "TOI Minimum", min = 0, max = 5000, value = 100, step = 50)),
                                  column(2, selectInput("uvenue", "Venue", choices = c("Any", "Home", "Away"), selected = "Any"))
                                  
                                ),
                                
                                # Sidebar layout
                                fluidRow(
                                  
                                  box(
                                    
                                    selectInput("xaxis", "X-Axis Variable", choices = c("OZS%", "DZS%", "NZS%", "ZSR",
                                                                                        "OZF%", "DZF%", "NZF%", "ZFR",
                                                                                        "Rel.ZSR",
                                                                                        "TOI.QoT", "CF.QoT", "xGF.QoT",
                                                                                        "TOI.QoC", "CF.QoC", "xGF.QoC",
                                                                                        "TOI%"), selected = "ZSR"),
                                    selectInput("yaxis", "Y-Axis Variable", choices = c("OZS%", "DZS%", "NZS%", "ZSR",
                                                                                        "OZF%", "DZF%", "NZF%", "ZFR",
                                                                                        "Rel.ZSR",
                                                                                        "TOI.QoT", "CF.QoT", "xGF.QoT",
                                                                                        "TOI.QoC", "CF.QoC", "xGF.QoC",
                                                                                        "TOI%"), selected = "TOI.QoT"),
                                    selectInput("colour", "Colour", choices = c("CF%", "Rel.CF%",
                                                                                "FF%", "Rel.FF%",
                                                                                "GF%", "Rel.GF%",
                                                                                "xGF%", "Rel.xGF%",
                                                                                "TOI%"), selected = "CF%"),
                                    selectInput("size", "Size", choices = c("CF%", "Rel.CF%",
                                                                            "FF%", "Rel.FF%",
                                                                            "GF%", "Rel.GF%",
                                                                            "xGF%", "Rel.xGF%",
                                                                            "TOI%"), selected = "TOI%"),
                                    width = 3,
                                    title = "Inputs",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                    
                                  ),
                                  
                                  box(
                                    
                                    plotOutput("uplot"),
                                    width = 9,
                                    title = "Usage Chart",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                    
                                  )
                                  
                                )
                                
                              )
                              
                            )),
                   
                   tabPanel("PvP", value = "pvp",
                            
                            dashboardPage(     
                              dashboardHeader(disable = TRUE),
                              dashboardSidebar(disable = TRUE),
                              dashboardBody(
                                
                                # Header text
                                fluidRow(
                                  column(6, h2("Player vs. Peers")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(9, helpText("Generate comparative dashboards according to specifications. All stats are 5v5 unless otherwise specified.")),
                                  column(3, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Panels
                                fluidRow(
                                  box(
                                    fluidRow(
                                      column(3, uiOutput("p1")),
                                      column(3, selectInput("ptype", "Season Type", choices = c("Regular", "Playoffs", "Both"), selected = "Regular")),
                                      column(6, uiOutput("pname"))
                                    ),
                                    fluidRow(
                                      column(3, uiOutput("p2")),
                                      column(3, tags$div(class = "center-low", checkboxInput("paggregate", "Aggregate", value = TRUE))),
                                      column(6, selectInput("padjust", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None"))
                                    ),
                                    width = 8,
                                    title = "Player",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                    
                                  ),
                                  
                                  box(
                                    fluidRow(
                                      column(7, uiOutput("ppos")),
                                      column(5, uiOutput("radio"))
                                    ),
                                    fluidRow(
                                      column(12, sliderInput("ptoi", "TOI Minimum", min = 0, max = 5000, value = 400, step = 50))
                                    ),
                                    width = 4,
                                    title = "Peers",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                    
                                  )),
                                
                                fluidRow(
                                  
                                  ## Distribution
                                  box(
                                    plotOutput("dist"),
                                    selectizeInput("dist", "Measure", choices = c("CF%", "FF%", "SF%", "GF%", "xGF%",
                                                                                  "Rel.CF%", "Rel.FF%", "Rel.SF%", "Rel.GF%", "Rel.xGF%",
                                                                                  "CF60", "CA60", "FF60", "FA60",
                                                                                  "SF60", "SA60", "GF60", "GA60",
                                                                                  "xGF60", "xGA60",
                                                                                  "Rel.CF60", "Rel.CA60", "Rel.FF60", "Rel.FA60",
                                                                                  "Rel.SF60", "Rel.SA60", "Rel.GF60", "Rel.GA60",
                                                                                  "Rel.xGF60", "Rel.xGA60", 
                                                                                  "Rel.xFSv%", "Rel.Sv%",
                                                                                  "Rel.xFSh%", "Rel.Sh%",
                                                                                  "iCF60", "iFF60", "iSF60", "ixG60",
                                                                                  "G60", "A60", "P60", "P160",
                                                                                  "Sh%", "Sv%", "PDO", "xPDO",
                                                                                  "iSh%", "ixFSh%", "iPENDIFF"), 
                                                   selected = "Rel.CF%", multiple = TRUE, options = list(maxItems = 1)),
                                    width = 6,
                                    title = "Distribution",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                  ),
                                  
                                  ## Radial
                                  box(
                                    plotOutput("rad"),
                                    selectizeInput("rad", "Measures", choices = c("CF%", "FF%", "SF%", "GF%", "xGF%",
                                                                                  "Rel.CF%", "Rel.FF%", "Rel.SF%", "Rel.GF%", "Rel.xGF%",
                                                                                  "PDO", "xPDO", "iPENDIFF",
                                                                                  "CF60", "FF60","SF60", "GF60", "xGF60",
                                                                                  "Rel.CF60", "Rel.FF60", "Rel.SF60", "Rel.GF60", "Rel.xGF60",
                                                                                  "iCF60", "iFF60", "iSF60", "ixG60",
                                                                                  "iSh%", "ixFSh%", "Sh%",
                                                                                  "Rel.xFSh%", "Rel.Sh%",
                                                                                  "G60", "A60", "P60", "P160",
                                                                                  "Rel.xFSv%", "Rel.Sv%", "Sv%",
                                                                                  "CA60", "FA60","SA60", "GA60", "xGA60",
                                                                                  "Rel.CA60", "Rel.FA60", "Rel.SA60", "Rel.GA60", "Rel.xGA60"), 
                                                   selected = c("Rel.xGF%", "Rel.xGF60", "P60", "ixG60", "A60", "Rel.xGA60"),
                                                   multiple = TRUE, options = list(maxItems = 10)),
                                    width = 6,
                                    title = "Radar",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                  )),
                                
                                fluidRow(
                                  
                                  ## Scatter
                                  box(
                                    plotOutput("scat"),
                                    fluidRow(
                                      column(2),
                                      column(3, selectizeInput("scatx", "X-Axis Variable", choices = c("CF%", "FF%", "SF%", "GF%", "xGF%",
                                                                                                       "Rel.CF%", "Rel.FF%", "Rel.SF%", "Rel.GF%", "Rel.xGF%",
                                                                                                       "PDO", "xPDO", "iPENDIFF",
                                                                                                       "CF60", "FF60","SF60", "GF60", "xGF60",
                                                                                                       "Rel.CF60", "Rel.FF60", "Rel.SF60", "Rel.GF60", "Rel.xGF60",
                                                                                                       "iCF60", "iFF60", "iSF60", "ixG60",
                                                                                                       "iSh%", "ixFSh%", "Sh%",
                                                                                                       "Rel.xFSh%", "Rel.Sh%",
                                                                                                       "G60", "A60", "P60", "P160",
                                                                                                       "Rel.xFSv%", "Rel.Sv%", "Sv%",
                                                                                                       "CA60", "FA60","SA60", "GA60", "xGA60",
                                                                                                       "Rel.CA60", "Rel.FA60", "Rel.SA60", "Rel.GA60", "Rel.xGA60"),
                                                               selected = "P60",
                                                               options = list(maxItems = 1))),
                                      column(2),
                                      column(3, selectizeInput("scaty", "Y-Axis Variable", choices = c("CF%", "FF%", "SF%", "GF%", "xGF%",
                                                                                                       "Rel.CF%", "Rel.FF%", "Rel.SF%", "Rel.GF%", "Rel.xGF%",
                                                                                                       "PDO", "xPDO", "iPENDIFF",
                                                                                                       "CF60", "FF60","SF60", "GF60", "xGF60",
                                                                                                       "Rel.CF60", "Rel.FF60", "Rel.SF60", "Rel.GF60", "Rel.xGF60",
                                                                                                       "iCF60", "iFF60", "iSF60", "ixG60",
                                                                                                       "iSh%", "ixFSh%", "Sh%",
                                                                                                       "Rel.xFSh%", "Rel.Sh%",
                                                                                                       "G60", "A60", "P60", "P160",
                                                                                                       "Rel.xFSv%", "Rel.Sv%", "Sv%",
                                                                                                       "CA60", "FA60","SA60", "GA60", "xGA60",
                                                                                                       "Rel.CA60", "Rel.FA60", "Rel.SA60", "Rel.GA60", "Rel.xGA60"),
                                                               selected = "Rel.xGF%",
                                                               options = list(maxItems = 1))),
                                      column(2)
                                    ),
                                    width = 12,
                                    title = "Scatter",
                                    solidHeader = TRUE,
                                    collapsible = TRUE
                                  ))
                                
                                
                              )
                              
                            ))
                   
                   
))

