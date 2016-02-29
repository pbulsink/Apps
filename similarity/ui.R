# User Interface

# Corsica Similarity Calculator App
# Last edited 2-28-2016
# Manny

require(shinydashboard)

shinyUI(navbarPage("Similarity Calculator", id = "tab", inverse = F, windowTitle = "Corsica | Similarity Calculator",
                   
                   tabPanel("Similarity", value = "sim",
                            
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
                                  column(6, h2("Similarity Calculator")),
                                  column(6, tags$div(class = "rightAlign", checked = NA, tags$a(href = "http://www.corsica.hockey/", target = "_parent", tags$h2("Corsica ↩", style = "color: #2B547E;"))))
                                ),
                                
                                # Help text
                                fluidRow(
                                  column(9, helpText("Loading the data may take a few seconds. Thanks for your patience. All stats are 5v5 unless otherwise specified.")),
                                  column(3, tags$div(class = "rightAlign", helpText("Confused? Consult the", tags$a(href = "http://www.corsica.hockey/blog/2016/02/03/glossary/", "Glossary"))))
                                ),
                                
                                # Input row 1
                                fluidRow(
                                  column(2, uiOutput("s1")),
                                  column(3, uiOutput("name")),
                                  column(2, tags$div(class = "center-low", checkboxInput("aggregate", "Aggregate Seasons", value = FALSE))),
                                  column(3, sliderInput("toi", "TOI Minimum", min = 0, max = 5000, value = 400, step = 50))
                                ),
                                
                                # Input row 2
                                fluidRow(
                                  column(2, uiOutput("s2")),
                                  column(7, selectizeInput("sliderselect", "Select Slider Measures", 
                                                           choices = list("G/60" = "G60", "A/60" = "A60", "P/60" = "P60", "CF/60" = "CF60", "CA/60" = "CA60", "CF%" = "CF%",
                                                                          "Rel CF/60" = "Rel.CF60", "Rel CA/60" = "Rel.CA60", "Rel CF%" = "Rel.CF%", "xGF/60" = "xGF60", "xGA/60" = "xGA60",
                                                                          "xGF%" = "xGF%", "Rel xGF/60" = "Rel.xGF60", "Rel xGA/60" = "Rel.xGA60", "Rel xGF%" = "Rel.xGF%",
                                                                          "GF/60" = "GF60", "GA/60" = "GA60", "GF%" = "GF%", "Rel GF/60" = "Rel.GF60", "Rel GA/60" = "Rel.GA60", "Rel GF%" = "Rel.GF%",
                                                                          "iCF/60" = "iCF60", "ixFSh%" = "ixFSh%", "ixG/60" = "ixG60", "TOI%" = "TOI%", "TOI% QoT" = "TOI.QoT", 
                                                                          "CF% QoT" = "CF.QoT", "xGF% QoT" = "xGF.QoT", "TOI% QoC" = "TOI.QoC", "CF% QoC" = "CF.QoC", "xGF% QoC" = "xGF.QoC",  
                                                                          "ZSR" = "ZSR", "iPEND/60" = "iPEND60", "iPENT/60" = "iPENT60", "iHF/60" = "iHF60", "iFO%" = "iFO%",
                                                                          "iGVA/60" = "iGVA60", "iTKA/60" = "iTKA60", "iBLK/60" = "iBLK60"),
                                                           selected = c("G60", "A60", "P60", "Rel.xGF60", "Rel.xGA60",
                                                                        "iCF60", "ixFSh%", "TOI%", "TOI.QoT", "ZSR"),
                                                           multiple = TRUE,
                                                           width = "100%"
                                  )),
                                  column(3, uiOutput("name2"))
                                ),
                                
                                # Main panel
                                fluidRow(
                                  
                                  uiOutput("sliders"),
                                  
                                  mainPanel(
                                    DT::dataTableOutput("t1"),
                                    width = 9)
                                  
                                )
                                
                              )
                            )
                            
                   )
                   
))

