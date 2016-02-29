# Server

# Corsica Team App
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

seasons <- sqliteQuickColumn(con, "teamseason", "Season")
teams <- sqliteQuickColumn(con, "teamseason", "Team")

shinyServer(function(input, output) {
  
  ### TAB: TEAM STATS 
  
  # Season inputs
  output$s1 <- renderUI(selectInput("s1", "From", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  output$s2 <- renderUI(selectInput("s2", "To", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  
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
    
    paste("SELECT * FROM teamseason WHERE Season IN (", paste(seasonvector, collapse = ","), ") AND [Season.Type] IN ('", paste(typevector, collapse = "','"), "')", sep = "")
    
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
    
    # Filter
    if (input$aggregate == TRUE) {
      teamgp <- group_by(data, Team, Season, Season.Type) %>% summarise(GP = max(GP)) %>% data.frame() %>%
        group_by(Team) %>% summarise(GP = sum(GP)) %>% data.frame()
      
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Venue %in% venuevector) %>% 
        group_by(Team) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  Season.Type = paste(unique(Season.Type), collapse = "/"),
                  GP = teamgp$GP[match(first(Team), teamgp$Team)], TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                  DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), DISTF = sum(DISTF), DISTA = sum(DISTA),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, 
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, 
               xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100, Adj.FSv. = FSv. - xFSv.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DISTF = DISTF/FF, Avg.DISTA = DISTA/FA,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    } else {
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Venue %in% venuevector) %>% 
        group_by(Team, Season, Season.Type) %>% 
        summarise(GP = max(GP), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA),
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                  DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), DISTF = sum(DISTF), DISTA = sum(DISTA),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, 
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, 
               xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100, Adj.FSv. = FSv. - xFSv.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DISTF = DISTF/FF, Avg.DISTA = DISTA/FA,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    }
    
    arrange(sub, Team) %>% select(c(Team, Season, Season.Type, GP, TOI, # /Base
                                    CF, CA, CF60, CA60, CF., CSh., CSv.,
                                    FF, FA, FF60, FA60, FF., FSh., FSv.,
                                    SF, SA, SF60, SA60, SF., Sh., Sv.,
                                    xGF, xGA, xGF60, xGA60, xGF., 
                                    xFSh., xFSv., Adj.FSv.,
                                    PDO, xPDO,
                                    GF, GA, GF60, GA60, GF.,
                                    FO., PENDIFF, # /On-Ice
                                    OZS, DZS, NZS, OZS., DZS., NZS., ZSR,
                                    OZF, DZF, NZF, OZF., DZF., NZF., ZFR,
                                    Avg.DISTF, Avg.DISTA, # /Context
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
  
  # Table contents
  table.contents <- reactive({
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- subdata() %>%
      mutate_each(funs(form), -c(Team, Season, Season.Type, ACF:MGF.))
    
    # Report input
    if (input$report == "On-Ice") {
      reportvector <- which(colnames(t1) %in% c("CF", "CA", "CF60", "CA60", "CF.", "CSh.", "CSv.",
                                                "FF", "FA", "FF60", "FA60", "FF.", "FSh.", "FSv.",
                                                "SF", "SA", "SF60", "SA60", "SF.", "Sh.", "Sv.",
                                                "GF", "GA", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGF60", "xGA60", "xGF.", "xFSh.", "xFSv.", "Adj.FSv.",
                                                "PDO", "xPDO", "PENDIFF", "FO."))
    } else if (input$report == "Context") {
      reportvector <- which(colnames(t1) %in% c("OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "OZS.", "DZS.", "NZS.", "OZF.", "DZF.", "NZF.",
                                                "ZSR", "ZFR", "PDO", "xPDO", "Avg.DISTF", "Avg.DISTA"))
    } else if (input$report == "Counts") {
      reportvector <- which(colnames(t1) %in% c("CF", "CA", "FF", "FA", "SF", "SA", "GF", "GA", "xGF", "xGA", 
                                                "OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "FOW", "FOL", "HF", "HA", "GVA", "TKA", "PENT", "PEND",
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
    select(t1, c(Team, Season, Season.Type, GP, TOI, reportvector))
    
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
      formatStyle('Team', fontWeight = "bold")
    
  })
  
  # Output file
  output$dl <- downloadHandler(
    filename = paste("Corsica_Team.Stats_", 
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
           CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, 
           CPM = CF - CA)
  }
  
  mutate.fenwick <- function(x) {
    mutate(x,
           FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100,
           FPM = FF - FA)
  }
  
  mutate.shots <- function(x) {
    mutate(x,
           SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100,
           SPM = SF - SA)
  }
  
  mutate.goals <- function(x) {
    mutate(x,
           GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100,
           GPM = GF - GA)
  }
  
  mutate.xG <- function(x) {
    mutate(x,
           xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100,
           xGPM = xGF - xGA)
  }
  
  mutate.extras <- function(x) {
    mutate(x,
           OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
           OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
           ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
           PENDIFF = PEND - PENT, FO. = FOW/(FOW + FOL)*100)
  }
  
  # Team input
  output$qteam <- renderUI(selectizeInput("qteam", "Team", choices = sort(unique(substr(as.character(teams), start = 1, stop = 3))), selected = NULL, multiple = TRUE))
  
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
    
    # Teams input
    if (length(input$qteam) < 1) {
      teamvector <- unique(as.character(teams))
    } else {
      teamvector <- input$qteam
    }
    
    # Report input
    columnvector <- c("Team", "Date", "TOI")
    
    if ("Corsi" %in% input$columns) {
      columnvector <- c(columnvector,
                        "CF", "CA",
                        "MCF", "MCA",
                        "ACF", "ACA")
    }
    
    if ("Fenwick" %in% input$columns) {
      columnvector <- c(columnvector,
                        "FF", "FA",
                        "MFF", "MFA",
                        "AFF", "AFA")
    }
    
    if ("Shots on goal" %in% input$columns) {
      columnvector <- c(columnvector,
                        "SF", "SA",
                        "MSF", "MSA",
                        "ASF", "ASA")
    }
    
    if ("Goals" %in% input$columns) {
      columnvector <- c(columnvector,
                        "GF", "GA",
                        "MGF", "MGA",
                        "AGF", "AGA")
    }
    
    if ("Expected Goals" %in% input$columns) {
      columnvector <- c(columnvector,
                        "xGF", "xGA",
                        "MxGF", "MxGA",
                        "AxGF", "AxGA")
    }
    
    if ("Extras" %in% input$columns) {
      columnvector <- c(columnvector,
                        "OZS", "DZS", "NZS",
                        "OZF", "DZF", "NZF",
                        "FOW", "FOL", "PENT", "PEND",
                        "GVA", "TKA", 
                        "RBF", "RBA", "RSF", "RSA")
    }
    
    paste("SELECT ",
          paste(unique(columnvector), collapse = ","),
          " FROM teamgame WHERE Date >= '", 
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
      gpref <- group_by(data, Team) %>% summarise(GP = length(unique(Date))) %>% data.frame()
      
      sumdata <- group_by(data, Team) %>% summarise_each(funs(sum), -c(Date, Team)) %>% 
        mutate(Date = paste(format(input$date[1], format = "%b'%y"), format(input$date[2], format = "%b'%y"), sep = "-"),
               GP = gpref$GP[match(Team, gpref$Team)]) %>% data.frame()
    } else {
      sumdata <- group_by(data, Team, Date) %>% summarise_each(funs(sum), -c(Team, Date)) %>% arrange(desc(Date)) %>% data.frame()
    }
    
    if ("Corsi" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(CF, CA, MCF, MCA)) %>% rename(CF = ACF, CA = ACA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(CF, CA, ACF, ACA)) %>% rename(CF = MCF, CA = MCA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(ACF, ACA, MCF, MCA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.corsi() %>% data.frame()
    }
    
    if ("Fenwick" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(FF, FA, MFF, MFA)) %>% rename(FF = AFF, FA = AFA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(FF, FA, AFF, AFA)) %>% rename(FF = MFF, FA = MFA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(AFF, AFA, MFF, MFA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.fenwick() %>% data.frame()
    }
    
    if ("Shots on goal" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(SF, SA, MSF, MSA)) %>% rename(SF = ASF, SA = ASA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(SF, SA, ASF, ASA)) %>% rename(SF = MSF, SA = MSA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(ASF, ASA, MSF, MSA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.shots() %>% data.frame()
    }
    
    if ("Goals" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(GF, GA, MGF, MGA)) %>% rename(GF = AGF, GA = AGA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(GF, GA, AGF, AGA)) %>% rename(GF = MGF, GA = MGA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(AGF, AGA, MGF, MGA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.goals() %>% data.frame()
    }
    
    if ("Expected Goals" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(xGF, xGA, MxGF, MxGA)) %>% rename(xGF = AxGF, xGA = AxGA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(xGF, xGA, AxGF, AxGA)) %>% rename(xGF = MxGF, xGA = MxGA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(AxGF, AxGA, MxGF, MxGA)) %>% data.frame()
      }
      
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
    if (input$qaggregate == TRUE) {
      data <- data %>%
        mutate_each(funs(form), -c(Team, Date))
    } else {
      data <- data %>%
        mutate_each(funs(form), -c(Team, Date))
    }
    
    if ("GP" %in% colnames(data)) {
      prime <- na.omit(match(c("Team", "Date", "GP"), colnames(data)))
    } else {
      prime <- na.omit(match(c("Team", "Date"), colnames(data)))
    }
    
    nonprime <- which(colnames(data) %in% c("Team", "Date", "GP") == FALSE)
    
    data <- select(data, prime, nonprime) %>% data.frame() # ORDER COLUMNS
    
    colnames(data) <- gsub("[.]$", "%", colnames(data))
    colnames(data) <- gsub("PM$", "+/-", colnames(data))
    
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
      formatStyle('Team', fontWeight = "bold")
    
  })
  
  # Output file
  output$qdl <- downloadHandler(
    filename = paste("Corsica_Team.Stats_", 
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
  
  # Team input
  output$s1team <- renderUI(selectizeInput("s1team", "Team(s)", choices = sort(unique(substr(as.character(teams), start = 1, stop = 3))), selected = NULL, multiple = TRUE, options = list(maxItems = 5)))
  
  # Construct query
  s1.query <- reactive({
    
    # Teams input
    if (length(input$s1team) < 1) {
      teamvector <- NULL
    } else {
      teamvector <- input$s1team
    }
    
    # Strength input
    if (input$s1strength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$s1strength
    }
    
    paste("SELECT * FROM teamgame WHERE Date >= '", 
          input$s1date[1],
          "' AND Date <= '", 
          input$s1date[2],
          "' AND Team IN ('",
          paste(teamvector, collapse = "','"),
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
    
    pre <- group_by(data, Team, Date) %>% summarise(index = 1, Season = first(Season), TOI = sum(TOI),
                                                    CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                                                    xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                                                    AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                                                    MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                                                    MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                                                    OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                                                    DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL)) %>% 
      group_by(Team) %>% mutate(gamenum = cumsum(index)) %>% arrange(Date) %>% data.frame()
    
    # Adjust
    if (input$s1adjust == "Score, Zone and Venue") {
      pre <- select(pre, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA)) %>% 
        rename(CF = ACF, CA = ACA, FF = AFF, FA = AFA, SF = ASF, SA = ASA, GF = AGF, GA = AGA, xGF = AxGF, xGA = AxGA) %>% data.frame()
    } else if (input$s1adjust == "Score and Venue") {
      pre <- select(pre, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA)) %>% 
        rename(CF = MCF, CA = MCA, FF = MFF, FA = MFA, SF = MSF, SA = MSA, GF = MGF, GA = MGA, xGF = MxGF, xGA = MxGA) %>% data.frame()
    } else {
      pre <- select(pre, -c(MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA)) %>% 
        data.frame()
    }
    
    avg <- group_by(pre, Team, Date) %>% mutate(Season = first(Season), gamenum = first(gamenum),
                                                TOI.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$TOI),
                                                CF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$CF),
                                                CA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$CA),
                                                FF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$FF),
                                                FA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$FA),
                                                SF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$SF),
                                                SA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$SA),
                                                GF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$GF),
                                                GA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$GA),
                                                xGF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$xGF),
                                                xGA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Team == first(Team))*pre$xGA),
                                                CF. = (gamenum >= input$n)*(CF.sum/(CF.sum + CA.sum)*100),
                                                CF60 = (gamenum >= input$n)*(CF.sum/TOI.sum*60),
                                                CA60 = (gamenum >= input$n)*(CA.sum/TOI.sum*60),
                                                FF. = (gamenum >= input$n)*(FF.sum/(FF.sum + FA.sum)*100),
                                                FF60 = (gamenum >= input$n)*(FF.sum/TOI.sum*60),
                                                FA60 = (gamenum >= input$n)*(FA.sum/TOI.sum*60),
                                                SF. = (gamenum >= input$n)*(SF.sum/(SF.sum + SA.sum)*100),
                                                SF60 = (gamenum >= input$n)*(SF.sum/TOI.sum*60),
                                                SA60 = (gamenum >= input$n)*(SA.sum/TOI.sum*60),
                                                GF. = (gamenum >= input$n)*(GF.sum/(GF.sum + GA.sum)*100),
                                                GF60 = (gamenum >= input$n)*(GF.sum/TOI.sum*60),
                                                GA60 = (gamenum >= input$n)*(GA.sum/TOI.sum*60),
                                                xGF. = (gamenum >= input$n)*(xGF.sum/(xGF.sum + xGA.sum)*100),
                                                xGF60 = (gamenum >= input$n)*(xGF.sum/TOI.sum*60),
                                                xGA60 = (gamenum >= input$n)*(xGA.sum/TOI.sum*60),
                                                Sh. = (gamenum >= input$n)*(GF.sum/SF.sum*100),
                                                Sv. = (gamenum >= input$n)*((1 - GA.sum/SA.sum)*100),
                                                PDO = (gamenum >= input$n)*(Sv. + Sh.)) %>% data.frame()
    
    colnames(avg) <- gsub("[.]$", "%", colnames(avg))
    
    avg
    
  })
  
  # Plot output
  output$s1plot <- renderPlot({
    
    require(ggplot2)
    require(ggthemes)
    
    data <- s1.contents()
    
    col.index <- which(colnames(data) == input$s1measure)
    
    series <- filter(data, gamenum >= input$n) %>% select(c(Team, Season, Date, gamenum, col.index))
    
    colnames(series)[5] <- "measure"
    
    if (input$s1measure %in% c("CF%", "FF%", "SF%", "GF%", "xGF%")) {
      mid <- 50
    } else if (input$s1measure == "PDO") {
      mid <- 100
    } else {
      mid <- -100
    }
    
    if (input$s1adjust == "None") {
      title.measure <- input$s1measure
    } else {
      title.measure <- paste(input$s1adjust, "Adjusted", input$s1measure)
    }
    
    p <- ggplot(series, aes(x = as.Date(Date), y = measure)) + 
      stat_smooth(aes(group = interaction(Season, Team), col = Team, fill = Team), se = TRUE, level = 0.95, span = 0.3, alpha = 0.2) +
      scale_color_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange")) +
      scale_fill_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange")) +
      geom_line(y = mid, col = "black", linetype = "dashed") +
      labs(
        title = paste(paste(input$s1team, collapse = ", "), " Rolling ", input$n, "-Game Average ", title.measure, sep = ""), 
        x = NULL, 
        y = title.measure
      ) +
      theme(
        panel.background = element_rect(fill = "#EFEFEF"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 50, hjust = 1),
        plot.background = element_rect(color = "#4863A0", size = 2),
        plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "in"),
        legend.title = element_blank()
      )
    
    p
    
  })
  
  # Conditional panel
  output$panel <- renderUI({
    
    if (length(input$s1team) > 0) {
      
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
