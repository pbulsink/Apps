# User Interface

# Data Downloader App
# Last edited 2-13-2016
# Manny

require(shinydashboard)

shinyUI(dashboardPage(
  
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
    tags$head(tags$style(".container-fluid {font-size: 13px; color: #2B547E; background-color: #ECF0F5;}")),
    tags$style(type = "text/css", ".shiny-output-error {visibility: hidden;}", 
               ".shiny-output-error:before { visibility: hidden;}",
               ".shiny-input-container { width: 100%; text-align: center; margin: 0 auto; }"),
    
    fluidRow(
      column(4),
      column(4, selectizeInput("select", "Select File", choices = list("No file selected" = "http://www.corsica.hockey/data/",
                                                                       "Play-By-Play: 2007-2008 (RData)" = "http://159.203.24.113:3838/data/pbp20072008.Rda",
                                                                       "Play-By-Play: 2008-2009 (RData)" = "http://159.203.24.113:3838/data/pbp20082009.Rda",
                                                                       "Play-By-Play: 2009-2010 (RData)" = "http://159.203.24.113:3838/data/pbp20092010.Rda",
                                                                       "Play-By-Play: 2010-2011 (RData)" = "http://159.203.24.113:3838/data/pbp20102011.Rda",
                                                                       "Play-By-Play: 2011-2012 (RData)" = "http://159.203.24.113:3838/data/pbp20112012.Rda",
                                                                       "Play-By-Play: 2012-2013 (RData)" = "http://159.203.24.113:3838/data/pbp20122013.Rda",
                                                                       "Play-By-Play: 2013-2014 (RData)" = "http://159.203.24.113:3838/data/pbp20132014.Rda",
                                                                       "Play-By-Play: 2014-2015 (RData)" = "http://159.203.24.113:3838/data/pbp20142015.Rda",
                                                                       "Play-By-Play: 2015-2016 (RData)" = "http://159.203.24.113:3838/data/pbp20152016.Rda",
                                                                       "Roster (RData)" = "http://159.203.24.113:3838/data/roster.Rda"),
                               selected = "")),
      column(4)
    ),
    
    fluidRow(
      column(4),
      column(4, htmlOutput("text")),
      column(4)
    ),
    
    fluidRow(
      column(5),
      column(2, htmlOutput("button")),
      column(5)
    ),
    
    fluidRow(
      tags$div(class = "separator", style = "width: 100%; height: 400px;")
    )
    
  )
  
))