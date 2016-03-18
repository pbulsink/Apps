########################################################################################################################################################################################################
######################## TESTING #######################################################################################################################################################################
########################################################################################################################################################################################################
# Server

# Corsica Games App
# Last edited 3-14-2016
# Manny

# Load libraries
require(shiny)
require(shinydashboard)
require(dplyr)
require(Kmisc)
require(DT)
require(RSQLite)

shinyServer(function(input, output, session) {
  
  # Load game list
  link <- "/srv/shiny-server/games.sqlite"
  con <- dbConnect(SQLite(), link)
  
  gamedb <- dbReadTable(con, "games")
  
  dbDisconnect(con)
  
  # Date select input
  output$date <- renderUI({
    
    dateInput("date", "Select Date", min = min(as.Date(gamedb$Date)), max = max(as.Date(gamedb$Date)), value = max(as.Date(gamedb$Date)))
    
  })
  
  # Game select input
  output$id <- renderUI({
    
    sub.db <- filter(gamedb, Date == input$date) %>% data.frame()
    
    glist <- paste(sub.db$Game.ID,
                   ": ",
                   sub.db$Away.Team,
                   " at ",
                   sub.db$Home.Team,
                   sep = "")
    
    ids <- sub.db$Newcode
    
    names(ids) <- glist
    
    selectizeInput("id", "Select Game", choices = ids, selected = NULL, multiple = TRUE, options = list(maxItems = 1))
    
  })
  
  # Reactive values
  values <- reactiveValues()
  
  gamedata <- function() { 
    
    myData = new.env()
    vars <- load(paste("/srv/shiny-server/data/", input$id, ".RData", sep = ""), envir = myData) 
    
    for (var in vars)
      values[[var]] <- get(var, myData)
    
  }
  
  ########################################################################################################################################################################
  ################## SUMMARY #############################################################################################################################################
  ########################################################################################################################################################################
  
  ### TEAM ###
  sumteam <- reactive({
    
    gamedata()
    data <- values$team
    
    # Strength state input
    if(input$team1 == "All Situations") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- "5v5"
    }
    
    sum <- filter(data, Strength.State %in% strengthvector) %>% group_by(Team) %>%
      summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA), SF = sum(SF), SA = sum(SA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA)) %>%
      mutate(CPM = CF - CA, CF. = CF/(CF + CA)*100, xGPM = xGF - xGA, xGF. = xGF/(xGF + xGA)*100, Sv. = (1 - (GA/SA))*100, Sh. = (GF/SF)*100) %>%
      data.frame()
    
    select(sum, -c(CA, SA, GA, xGA)) %>% data.frame()
    
  })
  
  t1.contents <- reactive({
    
    data <- sumteam()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- mutate_each(data, funs(form), -c(Team))
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    colnames(t1) <- gsub("PM$", "+/-", colnames(t1))
    
    t1
    
  })
  
  output$t1 <- DT::renderDataTable({
    
    data <- t1.contents()
    
    datatable(data,
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = F, pageLength = 2, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F) %>%
      formatStyle('Team', fontWeight = "bold")
    
  })
  
  ### GOALIE ###
  sumgoalie <- reactive({
    
    gamedata()
    data <- values$goalie
    
    sum <- group_by(data, Player) %>%
      summarise(GA = sum(GA), SA = sum(SA)) %>%
      mutate(Sv. = (1 - (GA/SA))*100) %>%
      data.frame()
    
    sum
    
  })
  
  g1.contents <- reactive({
    
    data <- sumgoalie()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    g1 <- mutate_each(data, funs(form), -c(Player))
    
    colnames(g1) <- gsub("[.]$", "%", colnames(g1))
    
    g1
    
  })
  
  output$g1 <- DT::renderDataTable({
    
    data <- g1.contents()
    
    datatable(data,
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = F, pageLength = 2, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F) %>%
      formatStyle('Player', fontWeight = "bold")
    
  })
  
  ### SHOTS BY PERIOD ###
  sumperiod <- reactive({
    
    gamedata()
    data <- values$team
    
    sum <- group_by(data, Team) %>%
      summarise(First = sum(SF*(Period == 1)), Second = sum(SF*(Period == 2)), Third = sum(SF*(Period == 3)), OT = sum(SF*(Period == 4 | Period == "OT")), Total = sum(SF)) %>%
      data.frame()
    
    sum
    
  })
  
  output$p1 <- DT::renderDataTable({
    
    data <- sumperiod()
    
    datatable(data,
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = F, pageLength = 2, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F,
              colnames = c("Team", "1", "2", "3", "OT", "Total")) %>%
      formatStyle('Team', fontWeight = "bold") %>%
      formatStyle('Total', 'border-left' = 'solid', 'border-width' = '1px')
    
  })
  
  ### SHOT CHART ###
  s1.contents <- reactive({
    
    gamedata()
    data <- values$pbp %>% mutate(Seconds = as.numeric(as.character(Seconds))) %>% arrange(Seconds)
    
    data$xG[which(is.na(data$xG) == TRUE)] <- 0
    
    # Strength state input
    if(input$plot1 == "All Situations") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- "5v5"
    }
    
    series <- filter(data, ev.team != "UKN" & {Strength.State %in% strengthvector | Event == "OFF"}) %>% group_by(ev.team) %>% 
      mutate(Minute = Seconds/60,
             CF = as.numeric(as.character(cumsum(Event %in% c("SHOT", "GOAL", "MISS", "BLOCK")))), 
             SF = as.numeric(as.character(cumsum(Event %in% c("SHOT", "GOAL")))),
             xg = as.numeric(as.character(cumsum(xG))),
             MCF = as.numeric(as.character(cumsum(cweight2*(Event %in% c("SHOT", "GOAL", "MISS", "BLOCK"))))), 
             MSF = as.numeric(as.character(cumsum(sweight2*(Event %in% c("SHOT", "GOAL"))))),
             Mxg = as.numeric(as.character(cumsum(fweight2*(xG))))) %>%
      select(c(Event, ev.team, Minute, p1, CF, SF, xg, MCF, MSF, Mxg)) %>%
      rename(Team = ev.team, xG = xg, MxG = Mxg) %>%
      data.frame()
    
    # Adjust
    if (input$plot3 == "Score and Venue") {
      series <- select(series, -c(CF, SF, xG)) %>% rename(CF = MCF, SF = MSF, xG = MxG) %>% data.frame()
    } else {
      series <- select(series, -c(MCF, MSF, MxG)) %>% data.frame()
    }
    
    series
    
  })
  
  output$s1 <- renderPlot({
    
    require(ggplot2)
    
    data <- s1.contents()
    
    if (input$plot3 == "None") {
      title.measure <- input$plot2
    } else {
      title.measure <- paste("Score and Venue Adjusted", input$plot2)
    }
    
    goals <- filter(data, Event == "GOAL") %>% data.frame()
    
    if(input$plot2 == "CF") {
      
      p <- ggplot(data, aes(x = Minute, y = CF, group = Team)) + 
        geom_ribbon(aes(ymin = 0, ymax = CF, fill = Team, colour = Team), alpha = 0.2) +
        geom_point(data = goals, aes(x = Minute, y = CF, colour = Team), size = 3, alpha = 0.8, shape = 18) +
        geom_text(data = goals, aes(label = p1, colour = Team), size = 2.5, vjust = -1.5) +
        scale_fill_manual(values = c("dodgerblue", "red1")) +
        scale_colour_manual(values = c("dodgerblue", "red1")) +
        theme(
          panel.background = element_rect(fill = "#EFEFEF"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(color = "#4863A0", size = 2),
          plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "in"),
          legend.title = element_blank()
        ) +
        labs(
          title = paste(unique(data$Team)[1], " vs. ", unique(data$Team)[2], " Cumulative ", title.measure, " Chart (", input$plot1, ")", sep = ""),
          y = title.measure
        )
      
    } else if(input$plot2 == "SF") {
      
      p <- ggplot(data, aes(x = Minute, y = SF, group = Team)) + 
        geom_ribbon(aes(ymin = 0, ymax = SF, fill = Team, colour = Team), alpha = 0.2) +
        geom_point(data = goals, aes(x = Minute, y = SF, colour = Team), size = 3, alpha = 0.8, shape = 18) +
        geom_text(data = goals, aes(label = p1, colour = Team), size = 2.5, vjust = -1.5) +
        scale_fill_manual(values = c("dodgerblue", "red1")) +
        scale_colour_manual(values = c("dodgerblue", "red1")) +
        theme(
          panel.background = element_rect(fill = "#EFEFEF"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(color = "#4863A0", size = 2),
          plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "in"),
          legend.title = element_blank()
        ) +
        labs(
          title = paste(unique(data$Team)[1], " vs. ", unique(data$Team)[2], " Cumulative ", title.measure, " Chart (", input$plot1, ")", sep = ""),
          y = title.measure
        )
      
    } else if(input$plot2 == "xG") {
      
      p <- ggplot(data, aes(x = Minute, y = xG, group = Team)) + 
        geom_ribbon(aes(ymin = 0, ymax = xG, fill = Team, colour = Team), alpha = 0.2) +
        geom_point(data = goals, aes(x = Minute, y = xG, colour = Team), size = 3, alpha = 0.8, shape = 18) +
        geom_text(data = goals, aes(label = p1, colour = Team), size = 2.5, vjust = -1.5) +
        scale_fill_manual(values = c("dodgerblue", "red1")) +
        scale_colour_manual(values = c("dodgerblue", "red1")) +
        theme(
          panel.background = element_rect(fill = "#EFEFEF"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(color = "#4863A0", size = 2),
          plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "in"),
          legend.title = element_blank()
        ) +
        labs(
          title = paste(unique(data$Team)[1], " vs. ", unique(data$Team)[2], " Cumulative ", title.measure, " Chart (", input$plot1, ")", sep = ""),
          y = title.measure
        )
      
    }
    
    if(length(input$id) > 0) {print(p)}
    
  })
  
  ### PLAYER STATS ###
  # Team 1
  sumplayer1 <- reactive({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[1]
    
    sum <- filter(data, Team == team) %>% group_by(Player) %>%
      summarise(a.TOI = sum(TOI), a.CF = sum(CF), a.CA = sum(CA), P = sum(G) + sum(A1) + sum(A2),
                f.TOI = sum(TOI*(Strength.State == "5v5")), f.CF = sum(CF*(Strength.State == "5v5")), f.CA = sum(CA*(Strength.State == "5v5"))) %>%
      mutate(a.CPM = a.CF - a.CA, f.CPM = f.CF - f.CA, a.CF. = a.CF/(a.CF + a.CA)*100, f.CF. = f.CF/(f.CF + f.CA)*100) %>%
      data.frame()
    
    sum
    
  })
  
  sump.contents1 <- reactive({
    
    data <- sumplayer1()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    p1 <- mutate_each(data, funs(form), -c(Player)) %>% select(c(Player, a.TOI, P, a.CF, a.CA, a.CPM, a.CF., f.TOI, f.CF, f.CA, f.CPM, f.CF.)) %>% data.frame()
    
    colnames(p1) <- gsub("[.]$", "%", colnames(p1))
    colnames(p1) <- gsub("PM$", "+/-", colnames(p1))
    
    p1
    
  })
  
  output$sump1 <- DT::renderDataTable({
    
    data <- sump.contents1()
    
    datatable(data,
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = F, pageLength = 2, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F,
              colnames = c("Player", "TOI", "P", "CF", "CA", "C+/-", "CF%", "5v5 TOI", "5v5 CF", "5v5 CA", "5v5 C+/-", "5v5 CF%")) %>%
      formatStyle('Player', fontWeight = "bold") %>%
      formatStyle('f.TOI', 'border-left' = 'solid', 'border-width' = '1px')
    
  })
  
  output$box1 <- renderUI({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[1]
    
    box(
      DT::dataTableOutput("sump1"),
      width = 12,
      title = paste(team, "Skater Stats"),
      solidHeader = TRUE,
      collapsible = TRUE
    )
    
  })
  
  # Team 2
  sumplayer2 <- reactive({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[2]
    
    sum <- filter(data, Team == team) %>% group_by(Player) %>%
      summarise(a.TOI = sum(TOI), a.CF = sum(CF), a.CA = sum(CA), P = sum(G) + sum(A1) + sum(A2),
                f.TOI = sum(TOI*(Strength.State == "5v5")), f.CF = sum(CF*(Strength.State == "5v5")), f.CA = sum(CA*(Strength.State == "5v5"))) %>%
      mutate(a.CPM = a.CF - a.CA, f.CPM = f.CF - f.CA, a.CF. = a.CF/(a.CF + a.CA)*100, f.CF. = f.CF/(f.CF + f.CA)*100) %>%
      data.frame()
    
    sum
    
  })
  
  sump.contents2 <- reactive({
    
    data <- sumplayer2()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    p2 <- mutate_each(data, funs(form), -c(Player)) %>% select(c(Player, a.TOI, P, a.CF, a.CA, a.CPM, a.CF., f.TOI, f.CF, f.CA, f.CPM, f.CF.)) %>% data.frame()
    
    colnames(p2) <- gsub("[.]$", "%", colnames(p2))
    colnames(p2) <- gsub("PM$", "+/-", colnames(p2))
    
    p2
    
  })
  
  output$sump2 <- DT::renderDataTable({
    
    data <- sump.contents2()
    
    datatable(data,
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = F, pageLength = 2, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F,
              colnames = c("Player", "TOI", "P", "CF", "CA", "C+/-", "CF%", "5v5 TOI", "5v5 CF", "5v5 CA", "5v5 C+/-", "5v5 CF%")) %>%
      formatStyle('Player', fontWeight = "bold") %>%
      formatStyle('f.TOI', 'border-left' = 'solid', 'border-width' = '1px')
    
  })
  
  output$box2 <- renderUI({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[2]
    
    box(
      DT::dataTableOutput("sump2"),
      width = 12,
      title = paste(team, "Skater Stats"),
      solidHeader = TRUE,
      collapsible = TRUE
    )
    
  })
  
  ########################################################################################################################################################################
  ################## SKATERS #############################################################################################################################################
  ########################################################################################################################################################################
  
  ### TEAM 1 ###
  sumskater1 <- reactive({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[1]
    
    # Strength input
    if (input$skater1.1 == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$skater1.1
    }
    
    # Score input
    if (input$skater1.2 == "Any") {
      scorevector <- c(-1:1)
    } else if (input$skater1.2 == "Leading") {
      scorevector <- 1
    } else if (input$skater1.2 == "Trailing") {
      scorevector <- -1
    } else if (input$skater1.2 == "Even") {
      scorevector <- 0
    }
    
    sum <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
    {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Team == team) %>%
      group_by(Player) %>%
      summarise(Position = first(Position), TOI = sum(TOI), tTOI = sum(tTOI),
                G = sum(G), A1 = sum(A1), A2 = sum(A2),
                CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA),
                ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), AxGF = sum(AxGF), AxGA = sum(AxGA), AGF = sum(AGF), AGA = sum(AGA),
                MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA), MxGF = sum(MxGF), MxGA = sum(MxGA), MGF = sum(MGF), MGA = sum(MGA),
                OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                iCF = sum(iCF), iSF = sum(iSF), ixG = sum(ixG),
                iPEND = sum(iPEND), iPENT = sum(iPENT), iFOW = sum(iFOW), iFOL = sum(iFOL),
                iHF = sum(iHF), iHA = sum(iHA), iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA),
                tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
                tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
                tMFF = sum(tMFF), tMFA = sum(tMFA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA)) %>%
      data.frame() %>%
      mutate(OCF = tCF - CF, OCA = tCA - CA,
             OFF = tFF - FF, OFA = tFA - FA,
             OGF = tGF - GF, OGA = tGA - GA,
             OxGF = txGF - xGF, OxGA = txGA - xGA, 
             OACF = tACF - ACF, OACA = tACA - ACA,
             OAFF = tAFF - AFF, OAFA = tAFA - AFA,
             OAGF = tAGF - AGF, OAGA = tAGA - AGA,
             OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
             OMCF = tMCF - MCF, OMCA = tMCA - MCA,
             OMFF = tMFF - MFF, OMFA = tMFA - MFA,
             OMGF = tMGF - MGF, OMGA = tMGA - MGA,
             OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA) %>%
      select(-c(tCF:tMxGA)) %>% data.frame()
    
    # Adjust
    if (input$skater1.3 == "Score, Zone and Venue") {
      sum <- select(sum, -c(CF, CA, FF, FA, GF, GA, xGF, xGA, MCF, MCA, MFF, MFA, MGF, MGA, MxGF, MxGA,
                            OCF, OCA, OFF, OFA, OGF, OGA, OxGF, OxGA, OMCF, OMCA, OMFF, OMFA, OMGF, OMGA, OMxGF, OMxGA)) %>% 
        rename(CF = ACF, CA = ACA, FF = AFF, FA = AFA, GF = AGF, GA = AGA, xGF = AxGF, xGA = AxGA,
               OCF = OACF, OCA = OACA, OFF = OAFF, OFA = OAFA, OGF = OAGF, OGA = OAGA, OxGF = OAxGF, OxGA = OAxGA) %>% data.frame()
    } else if (input$skater1.3 == "Score and Venue") {
      sum <- select(sum, -c(CF, CA, FF, FA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, AGF, AGA, AxGF, AxGA,
                            OCF, OCA, OFF, OFA, OGF, OGA, OxGF, OxGA, OACF, OACA, OAFF, OAFA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        rename(CF = MCF, CA = MCA, FF = MFF, FA = MFA, GF = MGF, GA = MGA, xGF = MxGF, xGA = MxGA,
               OCF = OMCF, OCA = OMCA, OFF = OMFF, OFA = OMFA, OGF = OMGF, OGA = OMGA, OxGF = OMxGF, OxGA = OMxGA) %>% data.frame()
    } else {
      sum <- select(sum, -c(MCF, MCA, MFF, MFA, MGF, MGA, MxGF, MxGA, ACF, ACA, AFF, AFA, AGF, AGA, AxGF, AxGA,
                            OMCF, OMCA, OMFF, OMFA, OMGF, OMGA, OMxGF, OMxGA, OACF, OACA, OAFF, OAFA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        data.frame()
    }
    
    sum <- mutate(sum,
                  OTOI = tTOI - TOI,
                  CPM = CF - CA, CF. = CF/(CF + CA)*100, CF60 = CF/TOI*60, CA60 = CA/TOI*60,
                  OCF. = OCF/(OCF + OCA)*100,
                  Rel.CF. = CF. - OCF.,
                  FPM = FF - FA, FF. = FF/(FF + FA)*100, FF60 = FF/TOI*60, FA60 = FA/TOI*60,
                  OFF. = OFF/(OFF + OFA)*100,
                  Rel.FF. = FF. - OFF.,
                  xGPM = xGF - xGA, xGF. = xGF/(xGF + xGA)*100, xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60,
                  OxGF. = OxGF/(OxGF + OxGA)*100,
                  Rel.xGF. = xGF. - OxGF.,
                  GPM = GF - GA,
                  A = A1 + A2, P = G + A, P1 = G + A1,
                  OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
                  ZSR = OZS/(OZS + DZS),
                  iFO. = iFOW/(iFOW + iFOL)*100, iPENDIFF = iPEND - iPENT) %>%
      data.frame()
    
    sum
    
  })
  
  skater1.contents <- reactive({
    
    data <- sumskater1()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    p1 <- mutate_each(data, funs(form), -c(Player, Position)) %>% data.frame()
    
    # Report input
    if (input$skater1.4 == "On-Ice") {
      reportvector <- which(colnames(p1) %in% c("TOI",
                                                "CF", "CA", "CPM", "CF60", "CA60", "CF.",
                                                "FF", "FA", "FPM", "FF60", "FA60", "FF.",
                                                "GF", "GA", "GPM", 
                                                "xGF", "xGA", "xGPM", "xGF60", "xGA60", "xGF."))
    } else if (input$skater1.4 == "Relative") {
      reportvector <- which(colnames(p1) %in% c("TOI",
                                                "Rel.CF.",
                                                "Rel.FF.", 
                                                "Rel.xGF."))
    } else if (input$skater1.4 == "Individual") {
      reportvector <- which(colnames(p1) %in% c("TOI",
                                                "G", "A1", "A2", "A", "P", "P1",
                                                "iCF", "iSF", "ixG",
                                                "iHF", "iHA", "iGVA", "iTKA", "iBLK",
                                                "iFOW", "iFOL", "iFO.",
                                                "iPENT", "iPEND", "iPENDIFF"))
    } else if (input$skater1.4 == "Context") {
      reportvector <- which(colnames(p1) %in% c("TOI",
                                                "OZS", "DZS", "NZS", "OTF",
                                                "OZS.", "DZS.", "NZS.", "ZSR"))
    }
    
    p1 <- select(p1, c(Player, Position, reportvector)) %>% data.frame()
    
    colnames(p1) <- gsub("[.]$", "%", colnames(p1))
    colnames(p1) <- gsub("PM$", "+/-", colnames(p1))
    
    p1
    
  })
  
  output$skater1 <- DT::renderDataTable({
    
    data <- skater1.contents()
    
    datatable(data,
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = F, pageLength = 2, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F) %>%
      formatStyle('Player', fontWeight = "bold")
    
  })
  
  output$skatertable1 <- renderUI({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[1]
    
    box(
      column(3, selectInput("skater1.1", "Strength State", choices = list("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
      column(3, selectInput("skater1.2", "Score State", choices = c("Any", "Leading", "Trailing", "Even"), selected = "Any")),
      column(3, selectInput("skater1.3", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
      column(3, selectInput("skater1.4", "Report", choices = c("On-Ice", "Relative", "Individual", "Context"), selected = "On-Ice")),
      DT::dataTableOutput("skater1"),
      width = 12,
      title = paste(team, "Skater Stats"),
      solidHeader = TRUE,
      collapsible = TRUE
    )
    
  })
  
  ### TEAM 2 ###
  sumskater2 <- reactive({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[2]
    
    # Strength input
    if (input$skater2.1 == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$skater2.1
    }
    
    # Score input
    if (input$skater2.2 == "Any") {
      scorevector <- c(-1:1)
    } else if (input$skater2.2 == "Leading") {
      scorevector <- 1
    } else if (input$skater2.2 == "Trailing") {
      scorevector <- -1
    } else if (input$skater2.2 == "Even") {
      scorevector <- 0
    }
    
    sum <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
    {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Team == team) %>%
      group_by(Player) %>%
      summarise(Position = first(Position), TOI = sum(TOI), tTOI = sum(tTOI),
                G = sum(G), A1 = sum(A1), A2 = sum(A2),
                CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA),
                ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), AxGF = sum(AxGF), AxGA = sum(AxGA), AGF = sum(AGF), AGA = sum(AGA),
                MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA), MxGF = sum(MxGF), MxGA = sum(MxGA), MGF = sum(MGF), MGA = sum(MGA),
                OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                iCF = sum(iCF), iSF = sum(iSF), ixG = sum(ixG),
                iPEND = sum(iPEND), iPENT = sum(iPENT), iFOW = sum(iFOW), iFOL = sum(iFOL),
                iHF = sum(iHF), iHA = sum(iHA), iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA),
                tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
                tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
                tMFF = sum(tMFF), tMFA = sum(tMFA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA)) %>%
      data.frame() %>%
      mutate(OCF = tCF - CF, OCA = tCA - CA,
             OFF = tFF - FF, OFA = tFA - FA,
             OGF = tGF - GF, OGA = tGA - GA,
             OxGF = txGF - xGF, OxGA = txGA - xGA, 
             OACF = tACF - ACF, OACA = tACA - ACA,
             OAFF = tAFF - AFF, OAFA = tAFA - AFA,
             OAGF = tAGF - AGF, OAGA = tAGA - AGA,
             OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
             OMCF = tMCF - MCF, OMCA = tMCA - MCA,
             OMFF = tMFF - MFF, OMFA = tMFA - MFA,
             OMGF = tMGF - MGF, OMGA = tMGA - MGA,
             OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA) %>%
      select(-c(tCF:tMxGA)) %>% data.frame()
    
    # Adjust
    if (input$skater2.3 == "Score, Zone and Venue") {
      sum <- select(sum, -c(CF, CA, FF, FA, GF, GA, xGF, xGA, MCF, MCA, MFF, MFA, MGF, MGA, MxGF, MxGA,
                            OCF, OCA, OFF, OFA, OGF, OGA, OxGF, OxGA, OMCF, OMCA, OMFF, OMFA, OMGF, OMGA, OMxGF, OMxGA)) %>% 
        rename(CF = ACF, CA = ACA, FF = AFF, FA = AFA, GF = AGF, GA = AGA, xGF = AxGF, xGA = AxGA,
               OCF = OACF, OCA = OACA, OFF = OAFF, OFA = OAFA, OGF = OAGF, OGA = OAGA, OxGF = OAxGF, OxGA = OAxGA) %>% data.frame()
    } else if (input$skater2.3 == "Score and Venue") {
      sum <- select(sum, -c(CF, CA, FF, FA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, AGF, AGA, AxGF, AxGA,
                            OCF, OCA, OFF, OFA, OGF, OGA, OxGF, OxGA, OACF, OACA, OAFF, OAFA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        rename(CF = MCF, CA = MCA, FF = MFF, FA = MFA, GF = MGF, GA = MGA, xGF = MxGF, xGA = MxGA,
               OCF = OMCF, OCA = OMCA, OFF = OMFF, OFA = OMFA, OGF = OMGF, OGA = OMGA, OxGF = OMxGF, OxGA = OMxGA) %>% data.frame()
    } else {
      sum <- select(sum, -c(MCF, MCA, MFF, MFA, MGF, MGA, MxGF, MxGA, ACF, ACA, AFF, AFA, AGF, AGA, AxGF, AxGA,
                            OMCF, OMCA, OMFF, OMFA, OMGF, OMGA, OMxGF, OMxGA, OACF, OACA, OAFF, OAFA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        data.frame()
    }
    
    sum <- mutate(sum,
                  OTOI = tTOI - TOI,
                  CPM = CF - CA, CF. = CF/(CF + CA)*100, CF60 = CF/TOI*60, CA60 = CA/TOI*60,
                  OCF. = OCF/(OCF + OCA)*100,
                  Rel.CF. = CF. - OCF.,
                  FPM = FF - FA, FF. = FF/(FF + FA)*100, FF60 = FF/TOI*60, FA60 = FA/TOI*60,
                  OFF. = OFF/(OFF + OFA)*100,
                  Rel.FF. = FF. - OFF.,
                  xGPM = xGF - xGA, xGF. = xGF/(xGF + xGA)*100, xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60,
                  OxGF. = OxGF/(OxGF + OxGA)*100,
                  Rel.xGF. = xGF. - OxGF.,
                  GPM = GF - GA,
                  A = A1 + A2, P = G + A, P1 = G + A1,
                  OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
                  ZSR = OZS/(OZS + DZS),
                  iFO. = iFOW/(iFOW + iFOL)*100, iPENDIFF = iPEND - iPENT) %>%
      data.frame()
    
    sum
    
  })
  
  skater2.contents <- reactive({
    
    data <- sumskater2()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    p2 <- mutate_each(data, funs(form), -c(Player, Position)) %>% data.frame()
    
    # Report input
    if (input$skater2.4 == "On-Ice") {
      reportvector <- which(colnames(p2) %in% c("TOI",
                                                "CF", "CA", "CPM", "CF60", "CA60", "CF.",
                                                "FF", "FA", "FPM", "FF60", "FA60", "FF.",
                                                "GF", "GA", "GPM", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGPM", "xGF60", "xGA60", "xGF."))
    } else if (input$skater2.4 == "Relative") {
      reportvector <- which(colnames(p2) %in% c("TOI",
                                                "TOI",
                                                "Rel.CF.",
                                                "Rel.FF.", 
                                                "Rel.xGF."))
    } else if (input$skater2.4 == "Individual") {
      reportvector <- which(colnames(p2) %in% c("TOI",
                                                "G", "A1", "A2", "A", "P", "p2",
                                                "iCF", "iSF", "ixG",
                                                "iHF", "iHA", "iGVA", "iTKA", "iBLK",
                                                "iFOW", "iFOL", "iFO.",
                                                "iPENT", "iPEND", "iPENDIFF"))
    } else if (input$skater2.4 == "Context") {
      reportvector <- which(colnames(p2) %in% c("TOI",
                                                "OZS", "DZS", "NZS", "OTF",
                                                "OZS.", "DZS.", "NZS.", "ZSR"))
    }
    
    p2 <- select(p2, c(Player, Position, reportvector)) %>% data.frame()
    
    colnames(p2) <- gsub("[.]$", "%", colnames(p2))
    colnames(p2) <- gsub("PM$", "+/-", colnames(p2))
    
    p2
    
  })
  
  output$skater2 <- DT::renderDataTable({
    
    data <- skater2.contents()
    
    datatable(data,
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = F, pageLength = 2, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F) %>%
      formatStyle('Player', fontWeight = "bold")
    
  })
  
  output$skatertable2 <- renderUI({
    
    gamedata()
    data <- values$player
    
    team <- sort(unique(data$Team))[2]
    
    box(
      column(3, selectInput("skater2.1", "Strength State", choices = list("All", "5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3"), selected = "5v5")),
      column(3, selectInput("skater2.2", "Score State", choices = c("Any", "Leading", "Trailing", "Even"), selected = "Any")),
      column(3, selectInput("skater2.3", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None")),
      column(3, selectInput("skater2.4", "Report", choices = c("On-Ice", "Relative", "Individual", "Context"), selected = "On-Ice")),
      DT::dataTableOutput("skater2"),
      width = 12,
      title = paste(team, "Skater Stats"),
      solidHeader = TRUE,
      collapsible = TRUE
    )
    
  })
  
  ########################################################################################################################################################################
  ################## COMBOS ##############################################################################################################################################
  ########################################################################################################################################################################
  
  ### TEAM 1 ###
  sumcombo1 <- reactive({
    
    gamedata()
    data <- values$combo
    
    team <- sort(unique(data$Team))[1]
    
    # Strength state input
    if(input$combo1.1 == "All Situations") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- "5v5"
    }
    
    # Period input
    if(input$combo1.2 == "Total") {
      periodvector <- 1:3
    } else {
      periodvector <- input$combo1.2
    }
    
    sum <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                    Period %in% periodvector & Team == team) %>%
      group_by(Combo.Code) %>%
      summarise(P1 = first(P1), P2 = first(P2), P3 = first(P3),
                P1.POS = first(P1.POS), P2.POS = first(P2.POS), P3.POS = first(P3.POS),
                TOI = sum(TOI),
                CF = sum(CF), CA = sum(CA), GF = sum(GF), GA = sum(GA),
                ACF = sum(ACF), ACA = sum(ACA), AGF = sum(AGF), AGA = sum(AGA),
                MCF = sum(MCF), MCA = sum(MCA),  MGF = sum(MGF), MGA = sum(MGA),
                OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS)) %>%
      data.frame()
    
    # Adjust
    if (input$combo1.3 == "Score, Zone and Venue") {
      sum <- select(sum, -c(CF, CA, GF, GA, MCF, MCA, MGF, MGA)) %>% 
        rename(CF = ACF, CA = ACA, GF = AGF, GA = AGA) %>% data.frame()
    } else if (input$combo1.3 == "Score and Venue") {
      sum <- select(sum, -c(CF, CA, GF, GA, ACF, ACA, AGF, AGA)) %>% 
        rename(CF = MCF, CA = MCA, GF = MGF, GA = MGA) %>% data.frame()
    } else {
      sum <- select(sum, -c(MCF, MCA, MGF, MGA, ACF, ACA, AGF, AGA)) %>% 
        data.frame()
    }
    
    sum <- mutate(sum, CPM = CF - CA, GPM = GF - GA) %>%
      data.frame()
    
    sum
    
  })
  
  combo1.contents <- reactive({
    
    data <- sumcombo1()
    
    t1 <- rbind_list(
      filter(data, grepl("C|L|R", P1.POS) == T & grepl("C|L|R", P2.POS) == T & grepl("C|L|R", P3.POS) == T) %>%
        mutate(Type = "Line") %>% arrange(desc(TOI)) %>% slice(1:4) %>% data.frame(),
      cbind(Combo.Code = "", TOI = NULL, CPM = NULL) %>% data.frame(),
      filter(data, grepl("C|L|R", P1.POS) == F & grepl("C|L|R", P2.POS) == F & is.na(P3.POS) == T) %>%
        mutate(Type = "Pair") %>% arrange(desc(TOI)) %>% slice(1:3) %>%data.frame()
    ) %>% data.frame()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- mutate_each(t1, funs(form), -c(Combo.Code, P1, P2, P3, P1.POS, P2.POS, P3.POS, Type)) %>% data.frame()
    
    # colnames(t1) <- gsub("PM$", "+/-", colnames(t1))
    
    t1
    
  })
  
  output$combo1 <- renderPlot({
    
    require(ggplot2)
    
    data <- combo1.contents()
    
    data$Order <- 1:length(data$Combo.Code)
    
    data <- mutate(data, 
                   Label = gsub("-", " - ", gsub("-X", "", Combo.Code)), 
                   CPMLabel = paste(paste(rep("  ", times = max(nchar(Combo.Code)) + 8), collapse = ""), ifelse(is.na(CPM) == TRUE, "", CPM)),
                   TOILabel = paste(ifelse(is.na(TOI) == TRUE, "", TOI), paste(rep("  ", times = max(nchar(Combo.Code)) + 8), collapse = "")),
                   GFLabel = ifelse(GF == 0, "", as.numeric(as.character(GF)))
    ) %>% data.frame()
    
    p <- ggplot(data, aes(y = desc(Order), x = 1)) + 
      geom_text(aes(label = Label), family = "metrophobic", fontface = "bold") +
      geom_text(aes(label = CPMLabel, colour = CPM), family = "metrophobic", fontface = "bold", size = 4) +
      geom_text(aes(label = TOILabel, alpha = TOI), family = "metrophobic", fontface = "bold", size = 4, colour = "dodgerblue") +
      geom_text(aes(label = GFLabel), family = "metrophobic", fontface = "bold", size = 4, colour = "darkorchid", vjust = 2) +
      scale_color_gradient2(low = "red1", high = "dodgerblue", mid = "white", midpoint = 0) +
      labs(
        title = "",
        x = NULL,
        y = NULL
      ) +
      theme(
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )
    
    print(p)
    
  })
  
  output$combotable1 <- renderUI({
    
    gamedata()
    data <- values$combo
    
    team <- sort(unique(data$Team))[1]
    
    box(
      column(2,
             fluidRow(radioButtons("combo1.1", "Game State", choices = list("All Situations", "5v5"), selected = "5v5", inline = FALSE)),
             fluidRow(radioButtons("combo1.2", "Period", choices = c(1, 2, 3, "Total"), selected = "Total", inline = FALSE)),
             fluidRow(selectInput("combo1.3", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None"))
      ),
      column(10,
             plotOutput("combo1")
      ),
      width = 12,
      title = paste(team, "Line Stats"),
      solidHeader = TRUE,
      collapsible = TRUE
    )
    
  })
  
  ### TEAM 2 ###
  sumcombo2 <- reactive({
    
    gamedata()
    data <- values$combo
    
    team <- sort(unique(data$Team))[2]
    
    # Strength state input
    if(input$combo2.1 == "All Situations") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- "5v5"
    }
    
    # Period input
    if(input$combo2.2 == "Total") {
      periodvector <- 1:3
    } else {
      periodvector <- input$combo2.2
    }
    
    sum <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                    Period %in% periodvector & Team == team) %>%
      group_by(Combo.Code) %>%
      summarise(P1 = first(P1), P2 = first(P2), P3 = first(P3),
                P1.POS = first(P1.POS), P2.POS = first(P2.POS), P3.POS = first(P3.POS),
                TOI = sum(TOI),
                CF = sum(CF), CA = sum(CA), GF = sum(GF), GA = sum(GA),
                ACF = sum(ACF), ACA = sum(ACA), AGF = sum(AGF), AGA = sum(AGA),
                MCF = sum(MCF), MCA = sum(MCA),  MGF = sum(MGF), MGA = sum(MGA),
                OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS)) %>%
      data.frame()
    
    # Adjust
    if (input$combo2.3 == "Score, Zone and Venue") {
      sum <- select(sum, -c(CF, CA, GF, GA, MCF, MCA, MGF, MGA)) %>% 
        rename(CF = ACF, CA = ACA, GF = AGF, GA = AGA) %>% data.frame()
    } else if (input$combo2.3 == "Score and Venue") {
      sum <- select(sum, -c(CF, CA, GF, GA, ACF, ACA, AGF, AGA)) %>% 
        rename(CF = MCF, CA = MCA, GF = MGF, GA = MGA) %>% data.frame()
    } else {
      sum <- select(sum, -c(MCF, MCA, MGF, MGA, ACF, ACA, AGF, AGA)) %>% 
        data.frame()
    }
    
    sum <- mutate(sum, CPM = CF - CA, GPM = GF - GA) %>%
      data.frame()
    
    sum
    
  })
  
  combo2.contents <- reactive({
    
    data <- sumcombo2()
    
    t2 <- rbind_list(
      filter(data, grepl("C|L|R", P1.POS) == T & grepl("C|L|R", P2.POS) == T & grepl("C|L|R", P3.POS) == T) %>%
        mutate(Type = "Line") %>% arrange(desc(TOI)) %>% slice(1:4) %>% data.frame(),
      cbind(Combo.Code = "", TOI = NULL, CPM = NULL) %>% data.frame(),
      filter(data, grepl("C|L|R", P1.POS) == F & grepl("C|L|R", P2.POS) == F & is.na(P3.POS) == T) %>%
        mutate(Type = "Pair") %>% arrange(desc(TOI)) %>% slice(1:3) %>%data.frame()
    ) %>% data.frame()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t2 <- mutate_each(t2, funs(form), -c(Combo.Code, P1, P2, P3, P1.POS, P2.POS, P3.POS, Type)) %>% data.frame()
    
    # colnames(t1) <- gsub("PM$", "+/-", colnames(t1))
    
    t2
    
  })
  
  output$combo2 <- renderPlot({
    
    require(ggplot2)
    
    data <- combo2.contents()
    
    data$Order <- 1:length(data$Combo.Code)
    
    data <- mutate(data, 
                   Label = gsub("-", " - ", gsub("-X", "", Combo.Code)), 
                   CPMLabel = paste(paste(rep("  ", times = max(nchar(Combo.Code)) + 8), collapse = ""), ifelse(is.na(CPM) == TRUE, "", CPM)),
                   TOILabel = paste(ifelse(is.na(TOI) == TRUE, "", TOI), paste(rep("  ", times = max(nchar(Combo.Code)) + 8), collapse = "")),
                   GFLabel = ifelse(GF == 0, "", as.numeric(as.character(GF)))
    ) %>% data.frame()
    
    p <- ggplot(data, aes(y = desc(Order), x = 1)) + 
      geom_text(aes(label = Label), family = "metrophobic", fontface = "bold") +
      geom_text(aes(label = CPMLabel, colour = CPM), family = "metrophobic", fontface = "bold", size = 4) +
      geom_text(aes(label = TOILabel, alpha = TOI), family = "metrophobic", fontface = "bold", size = 4, colour = "dodgerblue") +
      geom_text(aes(label = GFLabel), family = "metrophobic", fontface = "bold", size = 4, colour = "darkorchid", vjust = 2) +
      scale_color_gradient2(low = "red1", high = "dodgerblue", mid = "white", midpoint = 0) +
      labs(
        title = "",
        x = NULL,
        y = NULL
      ) +
      theme(
        panel.background = element_rect(fill = "#F5F5F5"),
        plot.background = element_rect(fill = "#F5F5F5", colour = "#F5F5F5"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none"
      )
    
    print(p)
    
  })
  
  output$combotable2 <- renderUI({
    
    gamedata()
    data <- values$combo
    
    team <- sort(unique(data$Team))[2]
    
    box(
      column(2,
             fluidRow(radioButtons("combo2.1", "Game State", choices = list("All Situations", "5v5"), selected = "5v5", inline = FALSE)),
             fluidRow(radioButtons("combo2.2", "Period", choices = c(1, 2, 3, "Total"), selected = "Total", inline = FALSE)),
             fluidRow(selectInput("combo2.3", "Adjustment", choices = c("None", "Score and Venue", "Score, Zone and Venue"), selected = "None"))
      ),
      column(10,
             plotOutput("combo2")
      ),
      width = 12,
      title = paste(team, "Line Stats"),
      solidHeader = TRUE,
      collapsible = TRUE
    )
    
  })
  
  ########################################################################################################################################################################
  ################## BOXSCORE ############################################################################################################################################
  ########################################################################################################################################################################
  
  
  
  ########################################################################################################################################################################
  ################## STORIES #############################################################################################################################################
  ########################################################################################################################################################################
  
  ### Story List ###
  # Team goals in period or time span
  # Comebacks
  
  # Write stories
  storylist <- reactive({
    
    gamedata()
    pbp <- values$pbp
    team <- values$team
    goalie <- values$goalie
    player <- values$player
    combo <- values$combo
    
    # PBP
    pbp$Score.Diff <- as.numeric(as.character(pbp$Home.Score)) - as.numeric(as.character(pbp$Away.Score))
    
    ## Summarise team
    # All
    teamall <- group_by(team, Team) %>%
      summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA), SF = sum(SF), SA = sum(SA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA)) %>%
      mutate(CPM = CF - CA, CF. = CF/(CF + CA)*100, xGPM = xGF - xGA, xGF. = xGF/(xGF + xGA)*100, SPM = SF - SA) %>%
      data.frame()
    
    # 5v5
    team5v5 <- filter(team, Strength.State == "5v5") %>% group_by(Team) %>%
      summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA), SF = sum(SF), SA = sum(SA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA)) %>%
      mutate(CPM = CF - CA, CF. = CF/(CF + CA)*100, xGPM = xGF - xGA, xGF. = xGF/(xGF + xGA)*100, SPM = SF - SA) %>%
      data.frame()
    
    # By period
    teamper <- group_by(team, Team, Period) %>%
      summarise(CF = sum(CF), CA = sum(CA), SF = sum(SF), SA = sum(SA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA)) %>%
      data.frame()
    
    ## Summarise goalie
    # All
    goalieall <- group_by(goalie, Player) %>%
      summarise(TOI = sum(TOI), GA = sum(GA), SA = sum(SA)) %>%
      mutate(Sv. = (1 - (GA/SA))*100) %>%
      data.frame()
    
    ## Summarise player
    # All
    playerall <- group_by(player, Player) %>%
      summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA),
                G = sum(na.omit(G)), A = (sum(na.omit(A1)) + sum(na.omit(A2))), iFOW = sum(na.omit(iFOW)), iFOL = sum(na.omit(iFOL)), iSF = sum(iSF)) %>%
      mutate(CPM = CF - CA, CF. = CF/(CF + CA)*100, xGPM = xGF - xGA, xGF. = xGF/(xGF + xGA)*100, P = G + A,
             iFO. = iFOW/(iFOW + iFOL), iFO = iFOW + iFOL) %>%
      data.frame()
    
    # 5v5
    player5v5 <- filter(player, Strength.State == "5v5") %>% group_by(Player) %>%
      summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA), xGF = sum(xGF), xGA = sum(xGA), GF = sum(GF), GA = sum(GA)) %>%
      mutate(CPM = CF - CA, CF. = CF/(CF + CA)*100, xGPM = xGF - xGA, xGF. = xGF/(xGF + xGA)*100) %>%
      data.frame()
    
    ## Summarise combo
    # 5v5
    lines5v5 <- filter(combo, grepl("C|L|R", P1.POS) == T & grepl("C|L|R", P2.POS) == T & grepl("C|L|R", P3.POS) == T & Strength.State == "5v5") %>% 
      group_by(Combo.Code) %>% 
      summarise(P1 = first(P1), P2 = first(P2), P3 = first(P3), CF = sum(CF), CA = sum(CA), GF = sum(GF), GA = sum(GA)) %>%
      mutate(CPM = CF - CA, GPM = GF - GA) %>%
      data.frame()
    
    pairs5v5 <- filter(combo, grepl("C|L|R", P1.POS) == F & grepl("C|L|R", P2.POS) == F & is.na(P3.POS) == T & Strength.State == "5v5") %>% 
      group_by(Combo.Code) %>% 
      summarise(P1 = first(P1), P2 = first(P2), CF = sum(CF), CA = sum(CA), GF = sum(GF), GA = sum(GA)) %>%
      mutate(CPM = CF - CA, GPM = GF - GA) %>%
      data.frame()
    
    stories <- NULL
    
    if(length(which(teamall$CPM >= 25)) > 0) { # Attempts, all situations
      team1 <- first(teamall$Team[which(teamall$CPM >= 25)])
      team2 <- first(teamall$Team[which(teamall$Team != team1)])
      stories <- c(stories, paste(team1,
                                  "out-attempts", 
                                  team2, 
                                  teamall$CF[which(teamall$Team == team1)], 
                                  "to", 
                                  teamall$CA[which(teamall$Team == team1)]))
    }
    if(length(which(team5v5$CPM >= 20)) > 0) { # Attempts, 5v5
      team1 <- first(team5v5$Team[which(team5v5$CPM >= 20)])
      team2 <- first(team5v5$Team[which(team5v5$Team != team1)])
      stories <- c(stories, paste(team1, 
                                  "out-attempts", 
                                  team2, 
                                  team5v5$CF[which(team5v5$Team == team1)], 
                                  "to", 
                                  team5v5$CA[which(team5v5$Team == team1)],
                                  "at 5v5"))
    }
    if(length(which(teamall$SPM >= 15)) > 0) { # Shots, all situations
      team1 <- first(teamall$Team[which(teamall$SPM >= 15)])
      team2 <- first(teamall$Team[which(teamall$Team != team1)])
      stories <- c(stories, paste(team1,
                                  "out-shoots", 
                                  team2, 
                                  teamall$SF[which(teamall$Team == team1)], 
                                  "to", 
                                  teamall$SA[which(teamall$Team == team1)]))
    }
    if(length(which(teamper$GF >= 3)) > 0) { # Goals in a period
      list <- NULL
      for(i in 1:length(which(teamper$GF >= 3))) {
        row <- teamper[which(teamper$GF >= 3), ][i, ]
        list[i] <- paste(row$Team,
                         "scores",
                         row$GF,
                         "times in period",
                         row$Period)
      }
      stories <- c(stories, list)
    }
    if(length(which(goalieall$GA == 0 & goalieall$TOI >= 55)) > 0) { # Shutout
      goalie <- goalieall$Player[which(goalieall$GA == 0 & goalieall$TOI >= 55)]
      stories <- c(stories, paste(goalie,
                                  "earns",
                                  goalieall$SA[which(goalieall$Player == goalie)],
                                  "save shutout"))
    }
    if(length(which(goalieall$GA >= 5)) > 0) { # Goalie GA
      list <- NULL
      for(i in 1:length(which(goalieall$GA >= 5))) {
        row <- goalieall[which(goalieall$GA >= 5), ][i, ]
        list[i] <- paste(row$Player,
                         "allows",
                         row$GA,
                         "goals on",
                         row$SA,
                         "shots")
      }
      stories <- c(stories, list)
    }
    if(length(which(goalieall$GA > 0 & goalieall$Sv. >= 95)) > 0) { # Goalie Saves
      list <- NULL
      for(i in 1:length(which(goalieall$GA > 0 & goalieall$Sv. >= 95))) {
        row <- goalieall[which(goalieall$GA > 0 & goalieall$Sv. >= 95), ][i, ]
        list[i] <- paste(row$Player,
                         "stops",
                         row$SA - row$GA,
                         "of",
                         row$SA,
                         "shots")
      }
      stories <- c(stories, list)
    }
    if(length(which(as.numeric(as.character(playerall$P)) >= 3)) > 0) { # Player points
      list <- NULL
      for(i in 1:length(which(as.numeric(as.character(playerall$P)) >= 3))) {
        row <- playerall[which(as.numeric(as.character(playerall$P)) >= 3), ][i, ]
        list[i] <- paste(row$Player,
                         "records",
                         ifelse(as.numeric(as.character(row$G)) == 3, "hattrick,", paste(as.numeric(as.character(row$G)), "G,", sep = "")),
                         paste(as.numeric(as.character(row$A)), "A", sep = "")
        )
      }
      stories <- c(stories, list)
    }
    if(length(which(as.numeric(as.character(playerall$TOI)) >= 30)) > 0) { # Player TOI
      list <- NULL
      for(i in 1:length(which(as.numeric(as.character(playerall$TOI)) >= 30))) {
        row <- playerall[which(as.numeric(as.character(playerall$TOI)) >= 30), ][i, ]
        list[i] <- paste(row$Player,
                         "plays",
                         round(row$TOI, 1),
                         "minutes"
        )
      }
      stories <- c(stories, list)
    }
    if(length(which(as.numeric(as.character(playerall$iFO)) >= 10 & as.numeric(as.character(playerall$iFO.)) >= 0.75)) > 0) { # Player face-offs
      list <- NULL
      for(i in 1:length(which(as.numeric(as.character(playerall$iFO)) >= 10 & as.numeric(as.character(playerall$iFO.)) >= 0.75))) {
        row <- playerall[which(as.numeric(as.character(playerall$iFO)) >= 10 & as.numeric(as.character(playerall$iFO.)) >= 0.75), ][i, ]
        list[i] <- paste(row$Player,
                         "wins",
                         row$iFOW,
                         "of",
                         row$iFO,
                         "face-offs"
        )
      }
      stories <- c(stories, list)
    }
    if(length(which(as.numeric(as.character(playerall$iSF)) >= 6)) > 0) { # Player shots
      list <- NULL
      for(i in 1:length(which(as.numeric(as.character(playerall$iSF)) >= 6))) {
        row <- playerall[which(as.numeric(as.character(playerall$iSF)) >= 6), ][i, ]
        list[i] <- paste(row$Player,
                         "records",
                         row$iSF,
                         "shots on goal"
        )
      }
      stories <- c(stories, list)
    }
    if(length(which(abs(as.numeric(as.character(player5v5$CPM))) >= 15)) > 0) { # Player Corsi differential
      list <- NULL
      for(i in 1:length(which(abs(as.numeric(as.character(player5v5$CPM))) >= 15))) {
        row <- player5v5[which(abs(as.numeric(as.character(player5v5$CPM))) >= 15), ][i, ]
        list[i] <- paste(row$Player,
                         "is",
                         paste("+", row$CF, sep = ""),
                         paste("/-", row$CA, sep = ""),
                         "in 5v5 shots"
        )
      }
      stories <- c(stories, list)
    }
    if(length(which(as.numeric(as.character(lines5v5$GF)) >= 3)) > 0) { # Line goals for
      list <- NULL
      for(i in 1:length(which(as.numeric(as.character(lines5v5$GF)) >= 3))) {
        row <- lines5v5[which(as.numeric(as.character(lines5v5$GF)) >= 3), ][i, ]
        list[i] <- paste(paste(row$P1, ", ", row$P2, " and ", row$P3, sep = ""),
                         "line on ice for",
                         row$GF,
                         "goals for at 5v5"
        )
      }
      stories <- c(stories, list)
    }
    if(length(which(as.numeric(as.character(pairs5v5$GA)) >= 3)) > 0) { # Pair goals against
      list <- NULL
      for(i in 1:length(which(as.numeric(as.character(pairs5v5$GA)) >= 3))) {
        row <- pairs5v5[which(as.numeric(as.character(pairs5v5$GA)) >= 3), ][i, ]
        list[i] <- paste(paste(row$P1, row$P2, sep = " and "),
                         "pairing on ice for",
                         row$GA,
                         "goals against at 5v5"
        )
      }
      stories <- c(stories, list)
    }
    if(length(which(grepl("CHLG", pbp$Description) == TRUE) > 0)) { # Challenges
      list <- NULL
      for(i in 1:length(which(grepl("CHLG", pbp$Description) == TRUE))) {
        row <- pbp[which(grepl("CHLG", pbp$Description) == TRUE), ][i, ]
        success <- ifelse(length(which(pbp$Event == "GOAL" & round(as.numeric(as.character(pbp$Seconds)), 0) == round(as.numeric(as.character(row$Seconds)), 0))) > 0, FALSE, TRUE)
        if(grepl("INTERFERENCE", row$Description) == TRUE) {
          reason <- "goalie interference"
        } else if(grepl("OFF-SIDE", row$Description) == TRUE) {
          reason <- "off-side"
        }
        if(grepl("VIS", row$Description) == TRUE) {
          team1 <- row$Away.Team
          team2 <- row$Home.Team
        } else if(grepl("HM", row$Description) == TRUE) {
          team1 <- row$Home.Team
          team2 <- row$Away.Team
        }
        list[i] <- paste(team1,
                         ifelse(success == TRUE, "successfully", "unsuccessfully"),
                         "challenges",
                         team2,
                         "goal in period",
                         row$Period,
                         "on",
                         reason
        )
      }
      stories <- c(stories, list)
    }
    if(length(which({as.numeric(as.character(pbp$Score.Diff)) <= -2 & max(as.numeric(as.character(pbp$Home.Score))) > max(as.numeric(as.character(pbp$Away.Score)))} | 
    {as.numeric(as.character(pbp$Score.Diff)) >= 2 & max(as.numeric(as.character(pbp$Away.Score))) > max(as.numeric(as.character(pbp$Home.Score)))})) > 0) {
      if(max(as.numeric(as.character(pbp$Home.Score))) > max(as.numeric(as.character(pbp$Away.Score)))) {
        team <- first(unique(pbp$Home.Team[!is.na(pbp$Home.Team)]))
        min <- abs(min(as.numeric(as.character(pbp$Score.Diff))))
      } else if(max(as.numeric(as.character(pbp$Away.Score))) > max(as.numeric(as.character(pbp$Home.Score)))) {
        team <- first(unique(pbp$Away.Team[!is.na(pbp$Away.Team)]))
        min <- abs(max(as.numeric(as.character(pbp$Score.Diff))))
      }
      stories <- c(stories, paste(team,
                                  "overcomes",
                                  min,
                                  "goal deficit"))
    }
    
    stories
    
  })
  
  # Output stories
  output$stories <- renderUI({
    
    text <- storylist()
    
    list <- NULL
    
    for(i in 1:length(text)) {
      
      story <- text[i]
      
      list[[i]] <- tags$li(tags$h5(story))
      
    }
    
    list
    
  })
  
})
