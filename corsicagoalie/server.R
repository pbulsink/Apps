# Server

# Corsica Goalie App
# Last edited 2-28-2016
# Manny

# Load libraries
require(shiny)
require(shinydashboard)
require(dplyr)
require(Kmisc)
require(DT)
require(RSQLite)

# Load season list
link <- "/srv/shiny-server/fenwicka.sqlite"
con <- dbConnect(SQLite(), link)

seasons <- sqliteQuickColumn(con, "goalieseason", "Season")
names <- sqliteQuickColumn(con, "goalieseason", "Player")
teams <- sqliteQuickColumn(con, "goalieseason", "Team")

shinyServer(function(input, output) {
  
  ### TAB: GOALIE STATS 
  
  # Season inputs
  output$s1 <- renderUI(selectInput("s1", "From", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  output$s2 <- renderUI(selectInput("s2", "To", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  
  # Team input
  output$s3 <- renderUI(selectInput("s3", "Team", choices = c("Any", sort(unique(substr(as.character(teams), start = 1, stop = 3)))), selected = "Any"))
  
  # Player input
  output$name <- renderUI(selectizeInput("name", "Search Players", choices = unique(as.character(names)), selected = NULL, multiple = TRUE))
  
  # Construct query
  query <- reactive({
    
    # Season input
    seasonvector <- as.character(seq(from = as.numeric(input$s1), to = as.numeric(input$s2), by = 10001))
    
    # Type input
    if (input$type == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$type
    }
    
    paste("SELECT * FROM goalieseason WHERE Season IN (", paste(seasonvector, collapse = ","), ") AND [Season.Type] IN ('", paste(typevector, collapse = "','"), "')", sep = "")
    
  })
  
  # Load data
  data <- reactive({
    
    query <- query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  # Subset data
  subdata <- reactive({
    
    data <- data()
    
    # Strength input
    if (input$strength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "XvX")
    } else {
      strengthvector <- input$strength
    }
    
    # Venue input
    if (input$venue == "Any") {
      venuevector <- c("Home", "Away")
    } else {
      venuevector <- input$venue
    }
    
    # Score input
    if (input$score == "Any") {
      scorevector <- c(-3:3)
    } else if (input$score == "Leading") {
      scorevector <- c(1:3)
    } else if (input$score == "Trailing") {
      scorevector <- c(-3:-1)
    } else if (input$score == "Even") {
      scorevector <- 0
    } else {
      scorevector <- as.numeric(input$score)
    }
    
    # Team input
    if (input$s3 == "Any") {
      teamvector <- ""
    } else {
      teamvector <- input$s3
    }
    
    # Players input
    if (length(input$name) < 1) {
      playervector <- tolower(unique(as.character(data$Player)))
    } else {
      playervector <- tolower(input$name)
    }
    
    # Filter
    if (input$aggregate == TRUE) {
      goaliegp <- group_by(data, Player, Season, Season.Type) %>% summarise(GP = max(GP)) %>% data.frame() %>%
        group_by(Player) %>% summarise(GP = sum(GP)) %>% data.frame()
      
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Venue %in% venuevector &
        grepl(teamvector, Team) == TRUE & tolower(Player) %in% playervector) %>% 
        group_by(Player) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  Season.Type = paste(unique(Season.Type), collapse = "/"), Team = paste(unique(Team), collapse = "/"), 
                  GP = goaliegp$GP[match(first(Player), goaliegp$Player)], TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                  DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), DISTA = sum(DISTA),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA), 
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DISTA = DISTA/FA,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    } else {
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Venue %in% venuevector &
        grepl(teamvector, Team) == TRUE & tolower(Player) %in% playervector) %>% 
        group_by(Player, Season, Season.Type) %>% 
        summarise(Team = paste(unique(Team), collapse = "/"),  GP = max(GP), TOI = sum(TOI), 
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA),
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                  DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), DISTA = sum(DISTA),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA), 
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DISTA = DISTA/FA,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    }
    
    arrange(sub, Player) %>% select(c(Player, Season, Season.Type, Team, GP, TOI, # /Base
                                      G, A1, A2, A, P, P1,
                                      G60, A160, A260, A60, P60, P160,
                                      iPENT, iPEND, iPENDIFF, # /Individual
                                      CF, CA, CF60, CA60, CF., CSh., CSv.,
                                      FF, FA, FF60, FA60, FF., FSh., FSv.,
                                      SF, SA, SF60, SA60, SF., Sh., Sv.,
                                      xGF, xGA, xGF60, xGA60, xGF., xFSh., xFSv., Adj.FSv.,
                                      PDO, xPDO,
                                      GF, GA, GF60, GA60, GF.,
                                      FO., PENDIFF, # /On-Ice
                                      OZS, DZS, NZS, OZS., DZS., NZS., ZSR,
                                      OZF, DZF, NZF, OZF., DZF., NZF., ZFR,
                                      Avg.DISTA, # /Context
                                      FOW, FOL, HF, HA, GVA, TKA, PENT, PEND,
                                      RBF, RBA, RSF, RSA, # /Counts
                                      ACF, ACA, ACF60, ACA60, ACF.,
                                      AFF, AFA, AFF60, AFA60, AFF., 
                                      ASF, ASA, ASF60, ASA60, ASF.,
                                      AxGF, AxGA, AxGF60, AxGA60, AxGF.,
                                      AGF, AGA, AGF60, AGA60, AGF.,
                                      MCF, MCA, MCF60, MCA60, MCF.,
                                      MFF, MFA, MFF60, MFA60, MFF., 
                                      MSF, MSA, MSF60, MSA60, MSF.,
                                      MxGF, MxGA, MxGF60, MxGA60, MxGF.,
                                      MGF, MGA, MGF60, MGA60, MGF. # /Adjusted
    )) %>% data.frame()
    
  })
  
  # Filter by TOI
  toi.filter <- reactive({
    
    data <- subdata()
    newdata <- filter(data, TOI >= input$toi) %>% data.frame()
    
    newdata
    
  })
  
  # Table contents
  table.contents <- reactive({
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- toi.filter() %>%
      mutate_each(funs(form), -c(Player, Season, Season.Type, Team, GP, ACF:MGF.))
    
    # Report input
    if (input$report == "Shots Faced") {
      reportvector <- which(colnames(t1) %in% c("CA", "CSv.", "FA", "FSv.", "SA", "Sv.",
                                                "GA", "xGA", "xFSv.", "Adj.FSv.",
                                                "Avg.DISTA", "RBA", "RSA"))
    } else if (input$report == "On-Ice") {
      reportvector <- which(colnames(t1) %in% c("CF", "CA", "CF60", "CA60", "CF.", "CSh.", "CSv.",
                                                "FF", "FA", "FF60", "FA60", "FF.", "FSh.", "FSv.",
                                                "SF", "SA", "SF60", "SA60", "SF.", "Sh.", "Sv.",
                                                "GF", "GA", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGF60", "xGA60", "xGF.", "xFSh.", "xFSv.", "Adj.FSv.",
                                                "PDO", "xPDO", "PENDIFF", "FO."))
    } else if (input$report == "Individual") {
      reportvector <- which(colnames(t1) %in% c("G", "A1", "A2", "A", "P", "P1",
                                                "G60", "A160", "A260", "A60", "P60", "P160",
                                                "iPENT", "iPEND", "iPENDIFF"))
    } else if (input$report == "Context") {
      reportvector <- which(colnames(t1) %in% c("OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "OZS.", "DZS.", "NZS.", "OZF.", "DZF.", "NZF.",
                                                "ZSR", "ZFR", "PDO", "xPDO", "Avg.DISTA"))
    } else if (input$report == "Counts") {
      reportvector <- which(colnames(t1) %in% c("CF", "CA", "FF", "FA", "SF", "SA", "GF", "GA", "xGF", "xGA", 
                                                "OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "FOW", "FOL", "HF", "HA", "GVA", "TKA", "PENT", "PEND",
                                                "G", "A1", "A2", "A", "P", "P1", "iPENT", "iPEND",
                                                "RBF", "RBA", "RSF", "RSA"))
    }
    
    # Adjustment
    if (input$adjust == "Score, Zone and Venue") {
      t1 <- mutate(t1, CF = format(round(ACF, 2), nsmall = 2), CA = format(round(ACA, 2), nsmall = 2), CF60 = format(round(ACF60, 2), nsmall = 2), CA60 = format(round(ACA60, 2), nsmall = 2), CF. = format(round(100*ACF., 2), nsmall = 2),
                   FF = format(round(AFF, 2), nsmall = 2), FA = format(round(AFA, 2), nsmall = 2), FF60 = format(round(AFF60, 2), nsmall = 2), FA60 = format(round(AFA60, 2), nsmall = 2), FF. = format(round(100*AFF., 2), nsmall = 2),
                   SF = format(round(ASF, 2), nsmall = 2), SA = format(round(ASA, 2), nsmall = 2), SF60 = format(round(ASF60, 2), nsmall = 2), SA60 = format(round(ASA60, 2), nsmall = 2), SF. = format(round(100*ASF., 2), nsmall = 2),
                   GF = format(round(AGF, 2), nsmall = 2), GA = format(round(AGA, 2), nsmall = 2), GF60 = format(round(AGF60, 2), nsmall = 2), GA60 = format(round(AGA60, 2), nsmall = 2), GF. = format(round(100*AGF., 2), nsmall = 2),
                   xGF = format(round(AxGF, 2), nsmall = 2), xGA = format(round(AxGA, 2), nsmall = 2), xGF60 = format(round(AxGF60, 2), nsmall = 2), xGA60 = format(round(AxGA60, 2), nsmall = 2), xGF. = format(round(100*AxGF., 2), nsmall = 2))
    } else if (input$adjust == "Score and Venue") {
      t1 <- mutate(t1, CF = format(round(MCF, 2), nsmall = 2), CA = format(round(MCA, 2), nsmall = 2), CF60 = format(round(MCF60, 2), nsmall = 2), CA60 = format(round(MCA60, 2), nsmall = 2), CF. = format(round(100*MCF., 2), nsmall = 2),
                   FF = format(round(MFF, 2), nsmall = 2), FA = format(round(MFA, 2), nsmall = 2), FF60 = format(round(MFF60, 2), nsmall = 2), FA60 = format(round(MFA60, 2), nsmall = 2), FF. = format(round(100*MFF., 2), nsmall = 2),
                   SF = format(round(MSF, 2), nsmall = 2), SA = format(round(MSA, 2), nsmall = 2), SF60 = format(round(MSF60, 2), nsmall = 2), SA60 = format(round(MSA60, 2), nsmall = 2), SF. = format(round(100*MSF., 2), nsmall = 2),
                   GF = format(round(MGF, 2), nsmall = 2), GA = format(round(MGA, 2), nsmall = 2), GF60 = format(round(MGF60, 2), nsmall = 2), GA60 = format(round(MGA60, 2), nsmall = 2), GF. = format(round(100*MGF., 2), nsmall = 2),
                   xGF = format(round(MxGF, 2), nsmall = 2), xGA = format(round(MxGA, 2), nsmall = 2), xGF60 = format(round(MxGF60, 2), nsmall = 2), xGA60 = format(round(MxGA60, 2), nsmall = 2), xGF. = format(round(100*MxGF., 2), nsmall = 2))
    }
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    select(t1, c(Player, Season, Season.Type, Team, GP, TOI, reportvector))
    
  })
  
  # Table output
  output$t1 <- DT::renderDataTable({
    
    t1 <- table.contents()
    datatable(t1, 
              extensions = list(FixedColumns = list(leftColumns = 2)),
              options = list(searching = F, paging = T, pageLength = 50, scrollX = T, info = FALSE, autoWidth = T,
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
  
  # Output file
  output$dl <- downloadHandler(
    filename = paste("Corsica_Goalie.Stats_", 
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 0, stop = 2),
                     "h",
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 4, stop = 5),
                     ".csv",
                     sep = ""),
    content = function(file) {
      write.csv(table.contents(), file)
    }
  )
  
  ### TAB: CUSTOM QUERY
  
  # Mutate functions
  mutate.corsi <- function(x) {
    mutate(x,
           CA60 = CA/TOI*60, CSv. = 1 - GA/CA)
  }
  
  mutate.fenwick <- function(x) {
    mutate(x,
           FA60 = FA/TOI*60, FSv. = 1 - GA/FA)
  }
  
  mutate.shots <- function(x) {
    mutate(x,
           SA60 = SA/TOI*60, Sv. = 1 - GA/SA)
  }
  
  mutate.xG <- function(x) {
    mutate(x,
           FA60 = FA/TOI*60, FSv. = 1 - GA/FA,
           xGA60 = xGA/TOI*60, xFSv. = 1 - xGA/FA,
           Adj.FSv. = FSv. - xFSv.)
  }
  
  mutate.extras <- function(x) {
    mutate(x,
           OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
           OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
           ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
           PENDIFF = PEND - PENT)
  }
  
  # Team input
  output$q3 <- renderUI(selectInput("q3", "Team", choices = c("Any", sort(unique(substr(as.character(teams), start = 1, stop = 3)))), selected = "Any"))
  
  # Player input
  output$qname <- renderUI(selectizeInput("qname", "Search Players", choices = unique(as.character(names)), selected = NULL, multiple = TRUE))
  
  # Construct query
  custom.query <- reactive({
    
    # Strength input
    if (input$qstrength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$qstrength
    }
    
    # Venue input
    if (input$qvenue == "Any") {
      venuevector <- c("Home", "Away")
    } else {
      venuevector <- input$qvenue
    }
    
    # Score input
    if (input$qscore == "Any") {
      scorevector <- c(-1:1)
    } else if (input$qscore == "Leading") {
      scorevector <- 1
    } else if (input$qscore == "Trailing") {
      scorevector <- -1
    } else if (input$qscore == "Even") {
      scorevector <- 0
    } else {
      scorevector <- as.numeric(input$qscore)
    }
    
    # Team input
    if (input$q3 == "Any") {
      teamvector <- unique(as.character(teams))
    } else {
      teamvector <- input$q3
    }
    
    # Players input
    if (length(input$qname) < 1) {
      playervector <- gsub("'", "''", unique(as.character(names)))
    } else {
      playervector <- gsub("'", "''", input$qname)
    }
    
    # Report input
    columnvector <- c("Player", "Date", "Team", "TOI")
    
    if ("Corsi" %in% input$columns) {
      columnvector <- c(columnvector,
                        "CA", "GA")
    }
    
    if ("Fenwick" %in% input$columns) {
      columnvector <- c(columnvector,
                        "FA", "GA")
    }
    
    if ("Shots on goal" %in% input$columns) {
      columnvector <- c(columnvector,
                        "SA", "GA")
    }
    
    if ("Expected Goals" %in% input$columns) {
      columnvector <- c(columnvector,
                        "xGA", "FA", "GA")
    }
    
    if ("Extras" %in% input$columns) {
      columnvector <- c(columnvector,
                        "OZS", "DZS", "NZS",
                        "OZF", "DZF", "NZF", 
                        "PENT", "PEND", "RBA", "RSA")
    }
    
    paste("SELECT ",
          paste(unique(columnvector), collapse = ","),
          " FROM goaliegame WHERE Date >= '", 
          input$date[1],
          "' AND Date <= '", 
          input$date[2],
          "' AND ((Venue == 'Home' AND [Strength.State] IN ('",
          paste(strengthvector, collapse = "','"),
          "')) OR (Venue == 'Away' AND [Strength.State] IN ('",
          paste(str_rev(strengthvector), collapse = "','"),
          "'))) AND Venue IN ('",
          paste(venuevector, collapse = "','"),
          "') AND [Score.Cat] IN (",
          paste(scorevector, collapse = ","),
          ") AND Team IN ('",
          paste(teamvector, collapse = "','"),
          "') AND Player IN ('",
          paste(playervector, collapse = "','"),
          "')",
          sep = "")
    
  })
  
  # Load data
  custom.data <- reactive({
    
    query <- custom.query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  # Aggregate
  sum.data <- reactive({
    
    data <- custom.data()
    
    if (input$qaggregate == TRUE) {
      gpref <- group_by(data, Player, Team) %>% summarise(GP = length(unique(Date))) %>%
        mutate(code = paste(Player, Team, sep = ".")) %>% data.frame()
      
      sumdata <- group_by(data, Player, Team) %>% mutate(GP = 1) %>% summarise_each(funs(sum), -c(Player, Date, Team)) %>% 
        mutate(Date = paste(format(input$date[1], format = "%b'%y"), format(input$date[2], format = "%b'%y"), sep = "-"),
               GP = gpref$GP[match(paste(Player, Team, sep = "."), gpref$code)]) %>% data.frame()
    } else {
      sumdata <- group_by(data, Player, Team, Date) %>% summarise_each(funs(sum), -c(Player, Team, Date)) %>% arrange(desc(Date)) %>%  data.frame()
    }
    
    if ("Corsi" %in% input$columns) {
      sumdata <- sumdata %>% mutate.corsi() %>% data.frame()
    }
    
    if ("Fenwick" %in% input$columns) {
      sumdata <- sumdata %>% mutate.fenwick() %>% data.frame()
    }
    
    if ("Shots on goal" %in% input$columns) {
      sumdata <- sumdata %>% mutate.shots() %>% data.frame()
    }
    
    if ("Expected Goals" %in% input$columns) {
      sumdata <- sumdata %>% mutate.xG() %>% data.frame()
    }
    
    if ("Extras" %in% input$columns) {
      sumdata <- sumdata %>% mutate.extras() %>% data.frame()
    }
    
    # Requirement-based mutations
    if ("SF" %in% colnames(sumdata) & "GF" %in% colnames(sumdata)) {
      sumdata <- sumdata %>% mutate(Sh. = GF/SF*100, Sv. = (1 - GA/SA)*100, PDO = Sh. + Sv.) %>% data.frame()
    }
    
    if ("CF" %in% colnames(sumdata) & "GF" %in% colnames(sumdata)) {
      sumdata <- sumdata %>% mutate(CSh. = GF/CF*100, CSv. = (1 - GA/CA)*100) %>% data.frame()
    }
    
    if ("FF" %in% colnames(sumdata) & "GF" %in% colnames(sumdata)) {
      sumdata <- sumdata %>% mutate(FSh. = GF/FF*100, FSv. = (1 - GA/FA)*100) %>% data.frame()
    }
    
    if ("FF" %in% colnames(sumdata) & "xGF" %in% colnames(sumdata)) {
      sumdata <- sumdata %>% mutate(xFSh. = xGF/FF*100, xFSv. = (1 - xGA/FA)*100, xPDO = xFSh. + xFSv.) %>% data.frame()
    }
    
    sumdata
    
  })
  
  # Table contents
  query.contents <- eventReactive(input$go, {
    
    data <- sum.data()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    # Format
    data <- data %>% mutate_each(funs(form), -c(Player, Team, Date))
    
    if ("GP" %in% colnames(data)) {
      prime <- na.omit(match(c("Player", "Date", "Team", "GP"), colnames(data)))
    } else {
      prime <- na.omit(match(c("Player", "Date", "Team"), colnames(data)))
    }
    
    nonprime <- which(colnames(data) %in% c("Player", "Team", "Date", "GP") == FALSE)
    
    data <- select(data, prime, nonprime) %>% data.frame() # ORDER COLUMNS
    
    colnames(data) <- gsub("[.]$", "%", colnames(data))
    
    data
    
  })
  
  # Table output
  output$t2 <- DT::renderDataTable({
    
    t2 <- query.contents()
    
    datatable(t2, 
              extensions = list(FixedColumns = list(leftColumns = 2)),
              options = list(searching = F, paging = T, pageLength = 50, scrollX = T, info = FALSE, autoWidth = T,
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
  
  # Output file
  output$qdl <- downloadHandler(
    filename = paste("Corsica_Goalie.Stats_", 
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 0, stop = 2),
                     "h",
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 4, stop = 5),
                     ".csv",
                     sep = ""),
    content = function(file) {
      write.csv(query.contents(), file)
    }
  )
  
  ### TAB: ROLLING AVERAGE
  
  # Player input
  output$s1name <- renderUI(selectizeInput("s1name", "Player(s)", choices = unique(as.character(names)), selected = NULL, multiple = TRUE, options = list(maxItems = 5)))
  
  # Construct query
  s1.query <- reactive({
    
    # Players input
    if (length(input$s1name) < 1) {
      playervector <- NULL
    } else {
      playervector <- gsub("'", "''", input$s1name)
    }
    
    # Strength input
    if (input$s1strength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$s1strength
    }
    
    paste("SELECT * FROM goaliegame WHERE Date >= '", 
          input$s1date[1],
          "' AND Date <= '", 
          input$s1date[2],
          "' AND Player IN ('",
          paste(playervector, collapse = "','"),
          "') AND ((Venue == 'Home' AND [Strength.State] IN ('",
          paste(strengthvector, collapse = "','"),
          "')) OR (Venue == 'Away' AND [Strength.State] IN ('",
          paste(str_rev(strengthvector), collapse = "','"),
          "')))",
          sep = "")
    
  })
  
  # Load data
  s1.data <- reactive({
    
    query <- s1.query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  # Plot contents
  s1.contents <- reactive({
    
    data <- s1.data()
    
    pre <- group_by(data, Player, Date) %>% summarise(index = 1, Season = first(Season), TOI = sum(TOI),
                                                      CA = sum(CA), FA = sum(FA), SA = sum(SA), GA = sum(GA),
                                                      xGA = sum(xGA)) %>% 
      group_by(Player) %>% mutate(gamenum = cumsum(index)) %>% arrange(Date) %>% data.frame()
    
    avg <- group_by(pre, Player, Date) %>% mutate(Season = first(Season), gamenum = first(gamenum),
                                                  TOI.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$TOI),
                                                  CA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$CA),
                                                  FA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$FA),
                                                  SA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$SA),
                                                  GA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$GA),
                                                  xGA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$xGA),
                                                  Sv. = (gamenum >= input$n)*((1 - GA.sum/SA.sum)*100),
                                                  FSv. = (gamenum >= input$n)*((1 - GA.sum/FA.sum)*100),
                                                  xFSv. = (gamenum >= input$n)*((1 - xGA.sum/FA.sum)*100),
                                                  Adj.FSv. = (gamenum >= input$n)*(FSv. - xFSv.)
    ) %>% data.frame()
    
    colnames(avg) <- gsub("[.]$", "%", colnames(avg))
    
    avg
    
  })
  
  # Plot output
  output$s1plot <- renderPlot({
    
    require(ggplot2)
    require(ggthemes)
    
    data <- s1.contents()
    
    col.index <- which(colnames(data) == input$s1measure)
    
    series <- filter(data, gamenum >= input$n) %>% select(c(Player, Season, Date, gamenum, col.index))
    
    colnames(series)[5] <- "measure"
    
    if (input$s1measure == "Adj.FSv%") {
      mid <- 0
    } else {
      mid <- -100
    }
    
    p <- ggplot(series, aes(x = as.Date(Date), y = measure)) + 
      stat_smooth(aes(group = interaction(Season, Player), col = Player, fill = Player), se = TRUE, level = 0.95, span = 0.3, alpha = 0.2) +
      scale_color_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange")) +
      scale_fill_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange")) +
      geom_line(y = mid, col = "black", linetype = "dashed") +
      labs(
        title = paste(paste(input$s1name, collapse = ", "), " Rolling ", input$n, "-Game Average ", input$s1measure, sep = ""), 
        x = NULL, 
        y = input$s1measure
      ) +
      theme(
        panel.background = element_rect(fill = "#EFEFEF"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 50, hjust = 1),
        plot.background = element_rect(color = "#4863A0", size = 2),
        plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "in"),
        legend.title = element_blank()
      )
    
    if (length(input$s1name) > 0) {print(p)}
    
  })
  
  # Conditional panel
  output$panel <- renderUI({
    
    if (length(input$s1name) > 0) {
      
      box(
        plotOutput("s1plot"),
        
        # Help text
        helpText("Data are smoothed using LOESS method. Ribbons represent 95% confidence area."),
        
        tags$div(class = "bottom"),
        
        width = 12,
        title = "Rolling Average",
        solidHeader = TRUE,
        collapsible = TRUE
        
      )
      
    }
    
  })
  
})
