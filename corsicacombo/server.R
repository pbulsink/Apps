# Server

# Corsica Combo App
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

seasons <- sqliteQuickColumn(con, "lineseason", "Season")
names <- sqliteQuickColumn(con, "playerseason", "Player")

shinyServer(function(input, output, session) {
  
  # Load required data
  pairquery <- reactive({
    
    # Season input
    seasonvector <- as.character(seq(from = as.numeric(input$p1), to = as.numeric(input$p2), by = 10001))
    
    # Type input
    if (input$ptype == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$ptype
    }
    
    paste("SELECT * FROM pairseason WHERE Season IN (", paste(seasonvector, collapse = ","), ") AND [Season.Type] IN ('", paste(typevector, collapse = "','"), "')", sep = "")
    
  })
  
  linequery <- reactive({
    
    # Season input
    seasonvector <- as.character(seq(from = as.numeric(input$l1), to = as.numeric(input$l2), by = 10001))
    
    # Type input
    if (input$ltype == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$ltype
    }
    
    paste("SELECT * FROM lineseason WHERE Season IN (", paste(seasonvector, collapse = ","), ") AND [Season.Type] IN ('", paste(typevector, collapse = "','"), "')", sep = "")
    
  })
  
  data <- reactive({
    
    if (input$tab == "line") {
      
      query <- linequery()
      
      # Link to database
      link <- "/srv/shiny-server/fenwicka.sqlite"
      con <- dbConnect(SQLite(), link)
      
      # Query database
      db.query <- dbSendQuery(con, query)
      data <- fetch(db.query, -1)
      
      dbDisconnect(con)
      
      data <- data
      
    } else if (input$tab == "pair") {
      
      query <- pairquery()
      
      # Link to database
      link <- "/srv/shiny-server/fenwicka.sqlite"
      con <- dbConnect(SQLite(), link)
      
      # Query database
      db.query <- dbSendQuery(con, query)
      data <- fetch(db.query, -1)
      
      dbDisconnect(con)
      
      data <- data
      
    }
    
    data
    
  })
  
  # Season inputs
  output$l1 <- renderUI(selectInput("l1", "From", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  output$l2 <- renderUI(selectInput("l2", "To", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  output$p1 <- renderUI(selectInput("p1", "From", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  output$p2 <- renderUI(selectInput("p2", "To", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  
  # Team inputs
  output$l3 <- renderUI(selectInput("l3", "Team", choices = c("Any", sort(unique(substr(as.character(data()$Team), start = 1, stop = 3))), selected = "Any")))
  output$p3 <- renderUI(selectInput("p3", "Team", choices = c("Any", sort(unique(substr(as.character(data()$Team), start = 1, stop = 3))), selected = "Any")))
  
  # Player inputs
  output$lname <- renderUI(selectizeInput("lname", "Search Players", choices = unique(c(unique(as.character(data()$P1)), unique(as.character(data()$P2)), unique(as.character(data()$P3)))), selected = NULL, multiple = TRUE))
  output$pname <- renderUI(selectizeInput("pname", "Search Players", choices = unique(c(unique(as.character(data()$P1)), unique(as.character(data()$P2)))), selected = NULL, multiple = TRUE))
  
  ## Subset data
  # Lines
  subline <- reactive({
    
    data <- data()
    
    # Strength input
    if (input$lstrength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "3v3", "XvX")
    } else {
      strengthvector <- input$lstrength
    }
    
    # Team input
    if (input$l3 == "Any") {
      teamvector <- ""
    } else {
      teamvector <- input$l3
    }
    
    # Players input
    if (length(input$lname) < 1) {
      playervector <- tolower(unique(c(unique(as.character(data$P1)), unique(as.character(data$P2)), unique(as.character(data$P3)))))
    } else {
      playervector <- tolower(input$lname)
    }
    
    # Filter
    if (input$laggregate == TRUE) {
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                      grepl(teamvector, Team) == TRUE & {tolower(P1) %in% playervector | tolower(P2) %in% playervector | tolower(P3) %in% playervector}) %>% 
        group_by(Combo.Code) %>% 
        summarise(P1 = first(P1), P2 = first(P2), P3 = first(P3), 
                  Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI),
                  Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  Season.Type = paste(unique(Season.Type), collapse = "/"),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                  SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                  ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                  AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
                  FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
                  GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
                  P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
                  P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
                  P3.G = sum(P3.G), P3.A1 = sum(na.omit(P3.A1)), P3.A2 = sum(na.omit(P3.A2)),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               OxFSh. = OxGF/OFF*100, OxFSv. = (1 - (OxGA/OFA))*100, Rel.xFSh. = xFSh. - OxFSh., Rel.xFSv. = xFSv. - OxFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               OAFF. = OAFF/(OAFF + OAFA), Rel.AFF. = AFF. - OAFF.,
               OAFF60 = OAFF/OTOI*60, OAFA60 = OAFA/OTOI*60, Rel.AFF60 = AFF60 - OAFF60, Rel.AFA60 = AFA60 - OAFA60,
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               OASF. = OASF/(OASF + OASA), Rel.ASF. = ASF. - OASF.,
               OASF60 = OASF/OTOI*60, OASA60 = OASA/OTOI*60, Rel.ASF60 = ASF60 - OASF60, Rel.ASA60 = ASA60 - OASA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               OMFF. = OMFF/(OMFF + OMFA), Rel.MFF. = MFF. - OMFF.,
               OMFF60 = OMFF/OTOI*60, OMFA60 = OMFA/OTOI*60, Rel.MFF60 = MFF60 - OMFF60, Rel.MFA60 = MFA60 - OMFA60,
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               OMSF. = OMSF/(OMSF + OMSA), Rel.MSF. = MSF. - OMSF.,
               OMSF60 = OMSF/OTOI*60, OMSA60 = OMSA/OTOI*60, Rel.MSF60 = MSF60 - OMSF60, Rel.MSA60 = MSA60 - OMSA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               OZSR = OOZS/(OOZS + ODZS)*100, Rel.ZSR = ZSR - OZSR,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT,
               P1.A = P1.A1 + P1.A2, P1.P = P1.G + P1.A, P1.P1 = P1.G + P1.A1, P1.G60 = P1.G/TOI*60, P1.A60 = P1.A/TOI*60, P1.P60 = P1.P/TOI*60,
               P2.A = P2.A1 + P2.A2, P2.P = P2.G + P2.A, P2.P1 = P2.G + P2.A1, P2.G60 = P2.G/TOI*60, P2.A60 = P2.A/TOI*60, P2.P60 = P2.P/TOI*60,
               P3.A = P3.A1 + P3.A2, P3.P = P3.G + P3.A, P3.P1 = P3.G + P3.A1, P3.G60 = P3.G/TOI*60, P3.A60 = P3.A/TOI*60, P3.P60 = P3.P/TOI*60,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    } else {
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                      grepl(teamvector, Team) == TRUE & {tolower(P1) %in% playervector | tolower(P2) %in% playervector | tolower(P3) %in% playervector}) %>% 
        group_by(Combo.Code, Season, Season.Type) %>% 
        summarise(P1 = first(P1), P2 = first(P2), P3 = first(P3), 
                  Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                  SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                  ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                  AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
                  FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
                  GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
                  P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
                  P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
                  P3.G = sum(P3.G), P3.A1 = sum(na.omit(P3.A1)), P3.A2 = sum(na.omit(P3.A2)),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               OxFSh. = OxGF/OFF*100, OxFSv. = (1 - (OxGA/OFA))*100, Rel.xFSh. = xFSh. - OxFSh., Rel.xFSv. = xFSv. - OxFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               OAFF. = OAFF/(OAFF + OAFA), Rel.AFF. = AFF. - OAFF.,
               OAFF60 = OAFF/OTOI*60, OAFA60 = OAFA/OTOI*60, Rel.AFF60 = AFF60 - OAFF60, Rel.AFA60 = AFA60 - OAFA60,
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               OASF. = OASF/(OASF + OASA), Rel.ASF. = ASF. - OASF.,
               OASF60 = OASF/OTOI*60, OASA60 = OASA/OTOI*60, Rel.ASF60 = ASF60 - OASF60, Rel.ASA60 = ASA60 - OASA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               OMFF. = OMFF/(OMFF + OMFA), Rel.MFF. = MFF. - OMFF.,
               OMFF60 = OMFF/OTOI*60, OMFA60 = OMFA/OTOI*60, Rel.MFF60 = MFF60 - OMFF60, Rel.MFA60 = MFA60 - OMFA60,
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               OMSF. = OMSF/(OMSF + OMSA), Rel.MSF. = MSF. - OMSF.,
               OMSF60 = OMSF/OTOI*60, OMSA60 = OMSA/OTOI*60, Rel.MSF60 = MSF60 - OMSF60, Rel.MSA60 = MSA60 - OMSA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               OZSR = OOZS/(OOZS + ODZS)*100, Rel.ZSR = ZSR - OZSR,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT,
               P1.A = P1.A1 + P1.A2, P1.P = P1.G + P1.A, P1.P1 = P1.G + P1.A1, P1.G60 = P1.G/TOI*60, P1.A60 = P1.A/TOI*60, P1.P60 = P1.P/TOI*60,
               P2.A = P2.A1 + P2.A2, P2.P = P2.G + P2.A, P2.P1 = P2.G + P2.A1, P2.G60 = P2.G/TOI*60, P2.A60 = P2.A/TOI*60, P2.P60 = P2.P/TOI*60,
               P3.A = P3.A1 + P3.A2, P3.P = P3.G + P3.A, P3.P1 = P3.G + P3.A1, P3.G60 = P3.G/TOI*60, P3.A60 = P3.A/TOI*60, P3.P60 = P3.P/TOI*60,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    }
    # FIX GP
    
    arrange(sub, desc(TOI)) %>% select(c(P1, P2, P3, Season, Season.Type, Team, TOI, OTOI, # /Base
                                         P1.G, P1.A1, P1.A2, P1.A, P1.P, P1.P1,
                                         P2.G, P2.A1, P2.A2, P2.A, P2.P, P2.P1,
                                         P3.G, P3.A1, P3.A2, P3.A, P3.P, P3.P1,
                                         P1.G60, P1.A60, P1.P60,
                                         P2.G60, P2.A60, P2.P60,
                                         P3.G60, P3.A60, P3.P60, # /Individual
                                         CF, CA, CF60, CA60, CF., CSh., CSv.,
                                         FF, FA, FF60, FA60, FF., FSh., FSv.,
                                         SF, SA, SF60, SA60, SF., Sh., Sv.,
                                         xGF, xGA, xGF60, xGA60, xGF., xFSh., xFSv., Adj.FSv.,
                                         PDO, xPDO,
                                         GF, GA, GF60, GA60, GF.,
                                         FO., PENDIFF, # /On-Ice
                                         OCF, OCA, OCF60, OCA60, OCF., OCSh., OCSv.,
                                         OFF, OFA, OFF60, OFA60, OFF., OFSh., OFSv.,
                                         OSF, OSA, OSF60, OSA60, OSF., OSh., OSv.,
                                         OxGF, OxGA, OxGF60, OxGA60, OxGF., OxFSh., OxFSv.,
                                         OGF, OGA, OGF60, OGA60, OGF., 
                                         OOZS, ODZS, ONZS, OZSR, # /Off-Ice
                                         Rel.CF60, Rel.CA60, Rel.CF., Rel.CSh., Rel.CSv.,
                                         Rel.FF60, Rel.FA60, Rel.FF., Rel.FSh., Rel.FSv.,
                                         Rel.SF60, Rel.SA60, Rel.SF., Rel.Sh., Rel.Sv.,
                                         Rel.GF60, Rel.GA60, Rel.GF., 
                                         Rel.xGF60, Rel.xGA60, Rel.xGF., Rel.xFSh., Rel.xFSv., 
                                         Rel.ZSR, # /Relative
                                         OZS, DZS, NZS, OZS., DZS., NZS., ZSR,
                                         OZF, DZF, NZF, OZF., DZF., NZF., ZFR, TOI., # /Context
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
                                         MGF, MGA, MGF60, MGA60, MGF.,
                                         OACF, OACA, OACF60, OACA60, OACF.,
                                         OAFF, OAFA, OAFF60, OAFA60, OAFF., 
                                         OASF, OASA, OASF60, OASA60, OASF.,
                                         OAxGF, OAxGA, OAxGF60, OAxGA60, OAxGF.,
                                         OAGF, OAGA, OAGF60, OAGA60, OAGF.,
                                         OMCF, OMCA, OMCF60, OMCA60, OMCF.,
                                         OMFF, OMFA, OMFF60, OMFA60, OMFF., 
                                         OMSF, OMSA, OMSF60, OMSA60, OMSF.,
                                         OMxGF, OMxGA, OMxGF60, OMxGA60, OMxGF.,
                                         OMGF, OMGA, OMGF60, OMGA60, OMGF.,
                                         Rel.ACF., Rel.AFF., Rel.ASF., Rel.AGF., Rel.AxGF., 
                                         Rel.MCF., Rel.MFF., Rel.MSF., Rel.MGF., Rel.MxGF.,
                                         Rel.ACF60, Rel.ACA60, Rel.AFF60, Rel.AFA60, 
                                         Rel.ASF60, Rel.ASA60, Rel.AGF60, Rel.AGA60, 
                                         Rel.AxGF60, Rel.AxGA60,
                                         Rel.MCF60, Rel.MCA60, Rel.MFF60, Rel.MFA60, 
                                         Rel.MSF60, Rel.MSA60, Rel.MGF60, Rel.MGA60, 
                                         Rel.MxGF60, Rel.MxGA60 # /Adjusted
    )) %>% data.frame()
    
  })
  
  # Filter by TOI
  line.filter <- reactive({
    
    data <- subline()
    newdata <- filter(data, TOI >= input$ltoi) %>% data.frame()
    
    newdata
    
  })
  
  # Pairs
  subpair <- reactive({
    
    data <- data()
    
    # Strength input
    if (input$pstrength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "3v3", "XvX")
    } else {
      strengthvector <- input$pstrength
    }
    
    # Team input
    if (input$p3 == "Any") {
      teamvector <- ""
    } else {
      teamvector <- input$p3
    }
    
    # Players input
    if (length(input$pname) < 1) {
      playervector <- tolower(unique(c(unique(as.character(data$P1)), unique(as.character(data$P2)))))
    } else {
      playervector <- tolower(input$pname)
    }
    
    # Filter
    if (input$paggregate == TRUE) {
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                      grepl(teamvector, Team) == TRUE & {tolower(P1) %in% playervector | tolower(P2) %in% playervector}) %>% 
        group_by(Combo.Code) %>% 
        summarise(P1 = first(P1), P2 = first(P2),
                  Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI),
                  Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  Season.Type = paste(unique(Season.Type), collapse = "/"),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                  SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                  ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                  AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
                  FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
                  GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
                  P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
                  P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               OxFSh. = OxGF/OFF*100, OxFSv. = (1 - (OxGA/OFA))*100, Rel.xFSh. = xFSh. - OxFSh., Rel.xFSv. = xFSv. - OxFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               OAFF. = OAFF/(OAFF + OAFA), Rel.AFF. = AFF. - OAFF.,
               OAFF60 = OAFF/OTOI*60, OAFA60 = OAFA/OTOI*60, Rel.AFF60 = AFF60 - OAFF60, Rel.AFA60 = AFA60 - OAFA60,
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               OASF. = OASF/(OASF + OASA), Rel.ASF. = ASF. - OASF.,
               OASF60 = OASF/OTOI*60, OASA60 = OASA/OTOI*60, Rel.ASF60 = ASF60 - OASF60, Rel.ASA60 = ASA60 - OASA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               OMFF. = OMFF/(OMFF + OMFA), Rel.MFF. = MFF. - OMFF.,
               OMFF60 = OMFF/OTOI*60, OMFA60 = OMFA/OTOI*60, Rel.MFF60 = MFF60 - OMFF60, Rel.MFA60 = MFA60 - OMFA60,
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               OMSF. = OMSF/(OMSF + OMSA), Rel.MSF. = MSF. - OMSF.,
               OMSF60 = OMSF/OTOI*60, OMSA60 = OMSA/OTOI*60, Rel.MSF60 = MSF60 - OMSF60, Rel.MSA60 = MSA60 - OMSA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               OZSR = OOZS/(OOZS + ODZS)*100, Rel.ZSR = ZSR - OZSR,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT,
               P1.A = P1.A1 + P1.A2, P1.P = P1.G + P1.A, P1.P1 = P1.G + P1.A1, P1.G60 = P1.G/TOI*60, P1.A60 = P1.A/TOI*60, P1.P60 = P1.P/TOI*60,
               P2.A = P2.A1 + P2.A2, P2.P = P2.G + P2.A, P2.P1 = P2.G + P2.A1, P2.G60 = P2.G/TOI*60, P2.A60 = P2.A/TOI*60, P2.P60 = P2.P/TOI*60,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    } else {
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
                      grepl(teamvector, Team) == TRUE & {tolower(P1) %in% playervector | tolower(P2) %in% playervector}) %>% 
        group_by(Combo.Code, Season, Season.Type) %>% 
        summarise(P1 = first(P1), P2 = first(P2),
                  Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                  SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                  ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                  AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
                  FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
                  GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
                  P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
                  P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               OxFSh. = OxGF/OFF*100, OxFSv. = (1 - (OxGA/OFA))*100, Rel.xFSh. = xFSh. - OxFSh., Rel.xFSv. = xFSv. - OxFSv.,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AFF60 = AFF/TOI*60, AFA60 = AFA/TOI*60, AFF. = AFF/(AFF + AFA), 
               OAFF. = OAFF/(OAFF + OAFA), Rel.AFF. = AFF. - OAFF.,
               OAFF60 = OAFF/OTOI*60, OAFA60 = OAFA/OTOI*60, Rel.AFF60 = AFF60 - OAFF60, Rel.AFA60 = AFA60 - OAFA60,
               ASF60 = ASF/TOI*60, ASA60 = ASA/TOI*60, ASF. = ASF/(ASF + ASA), 
               OASF. = OASF/(OASF + OASA), Rel.ASF. = ASF. - OASF.,
               OASF60 = OASF/OTOI*60, OASA60 = OASA/OTOI*60, Rel.ASF60 = ASF60 - OASF60, Rel.ASA60 = ASA60 - OASA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MFF60 = MFF/TOI*60, MFA60 = MFA/TOI*60, MFF. = MFF/(MFF + MFA), 
               OMFF. = OMFF/(OMFF + OMFA), Rel.MFF. = MFF. - OMFF.,
               OMFF60 = OMFF/OTOI*60, OMFA60 = OMFA/OTOI*60, Rel.MFF60 = MFF60 - OMFF60, Rel.MFA60 = MFA60 - OMFA60,
               MSF60 = MSF/TOI*60, MSA60 = MSA/TOI*60, MSF. = MSF/(MSF + MSA), 
               OMSF. = OMSF/(OMSF + OMSA), Rel.MSF. = MSF. - OMSF.,
               OMSF60 = OMSF/OTOI*60, OMSA60 = OMSA/OTOI*60, Rel.MSF60 = MSF60 - OMSF60, Rel.MSA60 = MSA60 - OMSA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
               OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
               ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
               OZSR = OOZS/(OOZS + ODZS)*100, Rel.ZSR = ZSR - OZSR,
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT,
               P1.A = P1.A1 + P1.A2, P1.P = P1.G + P1.A, P1.P1 = P1.G + P1.A1, P1.G60 = P1.G/TOI*60, P1.A60 = P1.A/TOI*60, P1.P60 = P1.P/TOI*60,
               P2.A = P2.A1 + P2.A2, P2.P = P2.G + P2.A, P2.P1 = P2.G + P2.A1, P2.G60 = P2.G/TOI*60, P2.A60 = P2.A/TOI*60, P2.P60 = P2.P/TOI*60,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.) %>%
        data.frame()
    }
    # FIX GP
    
    arrange(sub, desc(TOI)) %>% select(c(P1, P2, Season, Season.Type, Team, TOI, OTOI, # /Base
                                         P1.G, P1.A1, P1.A2, P1.A, P1.P, P1.P1,
                                         P2.G, P2.A1, P2.A2, P2.A, P2.P, P2.P1,
                                         P1.G60, P1.A60, P1.P60,
                                         P2.G60, P2.A60, P2.P60, # /Individual
                                         CF, CA, CF60, CA60, CF., CSh., CSv.,
                                         FF, FA, FF60, FA60, FF., FSh., FSv.,
                                         SF, SA, SF60, SA60, SF., Sh., Sv.,
                                         xGF, xGA, xGF60, xGA60, xGF., xFSh., xFSv., Adj.FSv.,
                                         PDO, xPDO,
                                         GF, GA, GF60, GA60, GF.,
                                         FO., PENDIFF, # /On-Ice
                                         OCF, OCA, OCF60, OCA60, OCF., OCSh., OCSv.,
                                         OFF, OFA, OFF60, OFA60, OFF., OFSh., OFSv.,
                                         OSF, OSA, OSF60, OSA60, OSF., OSh., OSv.,
                                         OxGF, OxGA, OxGF60, OxGA60, OxGF., OxFSh., OxFSv.,
                                         OGF, OGA, OGF60, OGA60, OGF., 
                                         OOZS, ODZS, ONZS, OZSR, # /Off-Ice
                                         Rel.CF60, Rel.CA60, Rel.CF., Rel.CSh., Rel.CSv.,
                                         Rel.FF60, Rel.FA60, Rel.FF., Rel.FSh., Rel.FSv.,
                                         Rel.SF60, Rel.SA60, Rel.SF., Rel.Sh., Rel.Sv.,
                                         Rel.GF60, Rel.GA60, Rel.GF., 
                                         Rel.xGF60, Rel.xGA60, Rel.xGF., Rel.xFSh., Rel.xFSv., 
                                         Rel.ZSR, # /Relative
                                         OZS, DZS, NZS, OZS., DZS., NZS., ZSR,
                                         OZF, DZF, NZF, OZF., DZF., NZF., ZFR, TOI., # /Context
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
                                         MGF, MGA, MGF60, MGA60, MGF.,
                                         OACF, OACA, OACF60, OACA60, OACF.,
                                         OAFF, OAFA, OAFF60, OAFA60, OAFF., 
                                         OASF, OASA, OASF60, OASA60, OASF.,
                                         OAxGF, OAxGA, OAxGF60, OAxGA60, OAxGF.,
                                         OAGF, OAGA, OAGF60, OAGA60, OAGF.,
                                         OMCF, OMCA, OMCF60, OMCA60, OMCF.,
                                         OMFF, OMFA, OMFF60, OMFA60, OMFF., 
                                         OMSF, OMSA, OMSF60, OMSA60, OMSF.,
                                         OMxGF, OMxGA, OMxGF60, OMxGA60, OMxGF.,
                                         OMGF, OMGA, OMGF60, OMGA60, OMGF.,
                                         Rel.ACF., Rel.AFF., Rel.ASF., Rel.AGF., Rel.AxGF., 
                                         Rel.MCF., Rel.MFF., Rel.MSF., Rel.MGF., Rel.MxGF.,
                                         Rel.ACF60, Rel.ACA60, Rel.AFF60, Rel.AFA60, 
                                         Rel.ASF60, Rel.ASA60, Rel.AGF60, Rel.AGA60, 
                                         Rel.AxGF60, Rel.AxGA60,
                                         Rel.MCF60, Rel.MCA60, Rel.MFF60, Rel.MFA60, 
                                         Rel.MSF60, Rel.MSA60, Rel.MGF60, Rel.MGA60, 
                                         Rel.MxGF60, Rel.MxGA60 # /Adjusted
    )) %>% data.frame()
    
  })
  
  # Filter by TOI
  pair.filter <- reactive({
    
    data <- subpair()
    newdata <- filter(data, TOI >= input$ptoi) %>% data.frame()
    
    newdata
    
  })
  
  ## Table contents
  # Lines
  line.contents <- reactive({
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- line.filter() %>%
      mutate_each(funs(form), -c(P1, P2, P3, Season, Season.Type, Team, ACF:Rel.MxGA60))
    
    # Report input
    if (input$lreport == "On-Ice") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "CF60", "CA60", "CF.", "CSh.", "CSv.",
                                                "FF", "FA", "FF60", "FA60", "FF.", "FSh.", "FSv.",
                                                "SF", "SA", "SF60", "SA60", "SF.", "Sh.", "Sv.",
                                                "GF", "GA", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGF60", "xGA60", "xGF.", "xFSh.", "xFSv.", "Adj.FSv.",
                                                "PDO", "xPDO", "PENDIFF", "FO."))
    } else if (input$lreport == "Off-Ice") {
      reportvector <- which(colnames(t1) %in% c("OTOI",
                                                "OCF", "OCA", "OCF60", "OCA60", "OCF.", "OCSh.", "OCSv.",
                                                "OFF", "OFA", "OFF60", "OFA60", "OFF.", "OFSh.", "OFSv.",
                                                "OSF", "OSA", "OSF60", "OSA60", "OSF.", "OSh.", "OSv.",
                                                "OGF", "OGA", "OGF60", "OGA60", "OGF.",
                                                "OxGF", "OxGA", "OxGF60", "OxGA60", "OxGF.", "OxFSh.", "OxFSv.",
                                                "OOZS", "ODZS", "ONZS", "OZSR"))
    } else if (input$lreport == "Relative") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "Rel.CF60", "Rel.CA60", "Rel.CF.", "Rel.CSh.", "Rel.CSv.",
                                                "Rel.FF60", "Rel.FA60", "Rel.FF.", "Rel.FSh.", "Rel.FSv.",
                                                "Rel.SF60", "Rel.SA60", "Rel.SF.", "Rel.Sh.", "Rel.Sv.",
                                                "Rel.GF60", "Rel.GA60", "Rel.GF.", 
                                                "Rel.xGF60", "Rel.xGA60", "Rel.xGF.", "Rel.xFSh.", "Rel.xFSv.",
                                                "Rel.ZSR"))
    } else if (input$lreport == "Individual") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "P1.G", "P1.A1", "P1.A2", "P1.A", "P1.P", "P1.P1",
                                                "P2.G", "P2.A1", "P2.A2", "P2.A", "P2.P", "P2.P1",
                                                "P3.G", "P3.A1", "P3.A2", "P3.A", "P3.P", "P3.P1",
                                                "P1.G60", "P1.A60", "P1.P60",
                                                "P2.G60", "P2.A60", "P2.P60",
                                                "P3.G60", "P3.A60", "P3.P60"))
    } else if (input$lreport == "Context") {
      reportvector <- which(colnames(t1) %in% c("TOI", "TOI.",
                                                "OZS", "DZS", "NZS",
                                                "OZF", "DZF", "NZF", 
                                                "OZS.", "DZS.", "NZS.", "OZF.", "DZF.", "NZF.",
                                                "ZSR", "ZFR", "PDO", "xPDO"))
    } else if (input$lreport == "Counts") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "FF", "FA", "SF", "SA", "GF", "GA", "xGF", "xGA", 
                                                "P1.G", "P1.A1", "P1.A2", "P1.A", "P1.P", "P1.P1",
                                                "P2.G", "P2.A1", "P2.A2", "P2.A", "P2.P", "P2.P1",
                                                "P3.G", "P3.A1", "P3.A2", "P3.A", "P3.P", "P3.P1",
                                                "OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "FOW", "FOL", "HF", "HA", "GVA", "TKA", "PENT", "PEND",
                                                "RBF", "RBA", "RSF", "RSA"))
    }
    
    # Adjustment
    if (input$ladjust == "Score, Zone and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(ACF, 2), nsmall = 2)), CA = as.numeric(format(round(ACA, 2), nsmall = 2)), CF60 = as.numeric(format(round(ACF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(ACA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*ACF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.ACF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(AFF, 2), nsmall = 2)), FA = as.numeric(format(round(AFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(AFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(AFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*AFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.AFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(ASF, 2), nsmall = 2)), SA = as.numeric(format(round(ASA, 2), nsmall = 2)), SF60 = as.numeric(format(round(ASF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(ASA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*ASF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.ASF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(AGF, 2), nsmall = 2)), GA = as.numeric(format(round(AGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(AGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(AGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*AGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.AGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(AxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(AxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(AxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(AxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*AxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.AxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OACF, 2), nsmall = 2)), OCA = as.numeric(format(round(OACA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OACF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OAFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OAFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OAFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OASF, 2), nsmall = 2)), OSA = as.numeric(format(round(OASA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OASF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OAGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OAGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OAGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OAxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OAxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OAxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OAxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.ACF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.ACA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.AFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.AFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.ASF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.ASA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.AGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.AGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.AxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.AxGA60, 2), nsmall = 2)))
    } else if (input$ladjust == "Score and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(MCF, 2), nsmall = 2)), CA = as.numeric(format(round(MCA, 2), nsmall = 2)), CF60 = as.numeric(format(round(MCF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(MCA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*MCF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.MCF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(MFF, 2), nsmall = 2)), FA = as.numeric(format(round(MFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(MFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(MFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*MFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.MFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(MSF, 2), nsmall = 2)), SA = as.numeric(format(round(MSA, 2), nsmall = 2)), SF60 = as.numeric(format(round(MSF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(MSA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*MSF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.MSF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(MGF, 2), nsmall = 2)), GA = as.numeric(format(round(MGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(MGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(MGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*MGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.MGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(MxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(MxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(MxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(MxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*MxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.MxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OMCF, 2), nsmall = 2)), OCA = as.numeric(format(round(OMCA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OMCF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OMFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OMFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OMFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OMSF, 2), nsmall = 2)), OSA = as.numeric(format(round(OMSA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OMSF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OMGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OMGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OMGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OMxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OMxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OMxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OMxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.MCF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.MCA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.MFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.MFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.MSF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.MSA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.MGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.MGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.MxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.MxGA60, 2), nsmall = 2)))
    }
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    select(t1, c(P1, P2, P3, Season, Season.Type, Team, reportvector))
    
  })
  
  # Pairs
  pair.contents <- reactive({
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- pair.filter() %>%
      mutate_each(funs(form), -c(P1, P2, Season, Season.Type, Team, ACF:Rel.MxGA60))
    
    # Report input
    if (input$preport == "On-Ice") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "CF60", "CA60", "CF.", "CSh.", "CSv.",
                                                "FF", "FA", "FF60", "FA60", "FF.", "FSh.", "FSv.",
                                                "SF", "SA", "SF60", "SA60", "SF.", "Sh.", "Sv.",
                                                "GF", "GA", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGF60", "xGA60", "xGF.", "xFSh.", "xFSv.", "Adj.FSv.",
                                                "PDO", "xPDO", "PENDIFF", "FO."))
    } else if (input$preport == "Off-Ice") {
      reportvector <- which(colnames(t1) %in% c("OTOI",
                                                "OCF", "OCA", "OCF60", "OCA60", "OCF.", "OCSh.", "OCSv.",
                                                "OFF", "OFA", "OFF60", "OFA60", "OFF.", "OFSh.", "OFSv.",
                                                "OSF", "OSA", "OSF60", "OSA60", "OSF.", "OSh.", "OSv.",
                                                "OGF", "OGA", "OGF60", "OGA60", "OGF.",
                                                "OxGF", "OxGA", "OxGF60", "OxGA60", "OxGF.", "OxFSh.", "OxFSv.",
                                                "OOZS", "ODZS", "ONZS", "OZSR"))
    } else if (input$preport == "Relative") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "Rel.CF60", "Rel.CA60", "Rel.CF.", "Rel.CSh.", "Rel.CSv.",
                                                "Rel.FF60", "Rel.FA60", "Rel.FF.", "Rel.FSh.", "Rel.FSv.",
                                                "Rel.SF60", "Rel.SA60", "Rel.SF.", "Rel.Sh.", "Rel.Sv.",
                                                "Rel.GF60", "Rel.GA60", "Rel.GF.", 
                                                "Rel.xGF60", "Rel.xGA60", "Rel.xGF.", "Rel.xFSh.", "Rel.xFSv.",
                                                "Rel.ZSR"))
    } else if (input$preport == "Individual") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "P1.G", "P1.A1", "P1.A2", "P1.A", "P1.P", "P1.P1",
                                                "P2.G", "P2.A1", "P2.A2", "P2.A", "P2.P", "P2.P1",
                                                "P1.G60", "P1.A60", "P1.P60",
                                                "P2.G60", "P2.A60", "P2.P60"))
    } else if (input$preport == "Context") {
      reportvector <- which(colnames(t1) %in% c("TOI", "TOI.",
                                                "OZS", "DZS", "NZS",
                                                "OZF", "DZF", "NZF", 
                                                "OZS.", "DZS.", "NZS.", "OZF.", "DZF.", "NZF.",
                                                "ZSR", "ZFR", "PDO", "xPDO"))
    } else if (input$preport == "Counts") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "FF", "FA", "SF", "SA", "GF", "GA", "xGF", "xGA", 
                                                "P1.G", "P1.A1", "P1.A2", "P1.A", "P1.P", "P1.P1",
                                                "P2.G", "P2.A1", "P2.A2", "P2.A", "P2.P", "P2.P1",
                                                "OZS", "DZS", "NZS", "OZF", "DZF", "NZF", 
                                                "FOW", "FOL", "HF", "HA", "GVA", "TKA", "PENT", "PEND",
                                                "RBF", "RBA", "RSF", "RSA"))
    }
    
    # Adjustment
    if (input$padjust == "Score, Zone and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(ACF, 2), nsmall = 2)), CA = as.numeric(format(round(ACA, 2), nsmall = 2)), CF60 = as.numeric(format(round(ACF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(ACA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*ACF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.ACF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(AFF, 2), nsmall = 2)), FA = as.numeric(format(round(AFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(AFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(AFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*AFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.AFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(ASF, 2), nsmall = 2)), SA = as.numeric(format(round(ASA, 2), nsmall = 2)), SF60 = as.numeric(format(round(ASF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(ASA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*ASF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.ASF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(AGF, 2), nsmall = 2)), GA = as.numeric(format(round(AGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(AGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(AGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*AGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.AGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(AxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(AxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(AxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(AxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*AxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.AxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OACF, 2), nsmall = 2)), OCA = as.numeric(format(round(OACA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OACF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OAFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OAFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OAFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OASF, 2), nsmall = 2)), OSA = as.numeric(format(round(OASA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OASF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OAGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OAGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OAGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OAxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OAxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OAxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OAxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.ACF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.ACA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.AFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.AFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.ASF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.ASA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.AGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.AGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.AxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.AxGA60, 2), nsmall = 2)))
    } else if (input$padjust == "Score and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(MCF, 2), nsmall = 2)), CA = as.numeric(format(round(MCA, 2), nsmall = 2)), CF60 = as.numeric(format(round(MCF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(MCA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*MCF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.MCF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(MFF, 2), nsmall = 2)), FA = as.numeric(format(round(MFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(MFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(MFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*MFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.MFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(MSF, 2), nsmall = 2)), SA = as.numeric(format(round(MSA, 2), nsmall = 2)), SF60 = as.numeric(format(round(MSF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(MSA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*MSF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.MSF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(MGF, 2), nsmall = 2)), GA = as.numeric(format(round(MGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(MGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(MGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*MGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.MGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(MxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(MxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(MxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(MxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*MxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.MxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OMCF, 2), nsmall = 2)), OCA = as.numeric(format(round(OMCA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OMCF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OMFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OMFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OMFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OMSF, 2), nsmall = 2)), OSA = as.numeric(format(round(OMSA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OMSF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OMGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OMGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OMGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OMxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OMxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OMxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OMxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.MCF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.MCA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.MFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.MFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.MSF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.MSA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.MGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.MGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.MxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.MxGA60, 2), nsmall = 2)))
    }
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    select(t1, c(P1, P2, Season, Season.Type, Team, reportvector))
    
  })
  
  # Table output
  # Lines
  output$t1 <- DT::renderDataTable({
    
    t1 <- line.contents()
    datatable(t1, 
              extensions = list(FixedColumns = list(leftColumns = 4)),
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
      formatStyle(c("P1", "P2", "P3"), fontWeight = "bold")
    
  })
  
  # Pairs
  output$t2 <- DT::renderDataTable({
    
    t2 <- pair.contents()
    datatable(t2, 
              extensions = list(FixedColumns = list(leftColumns = 3)),
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
      formatStyle(c("P1", "P2"), fontWeight = "bold")
    
  })
  
  ## Output file
  # Lines
  output$ldl <- downloadHandler(
    filename = paste("Corsica_Line.Stats_", 
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 0, stop = 2),
                     "h",
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 4, stop = 5),
                     ".csv",
                     sep = ""),
    content = function(file) {
      write.csv(line.contents(), file)
    }
  )
  
  # Pairs
  output$pdl <- downloadHandler(
    filename = paste("Corsica_Pair.Stats_", 
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 0, stop = 2),
                     "h",
                     substr(gsub("^[0-9]+-[0-9]+-[0-9]+ ", "",Sys.time()), start = 4, stop = 5),
                     ".csv",
                     sep = ""),
    content = function(file) {
      write.csv(pair.contents(), file)
    }
  )
  
  ### TAB: WOWY
  
  # Player input
  output$wname <- renderUI(selectizeInput("wname", "Player", choices = unique(as.character(names)), selected = NULL, multiple = TRUE, options = list(maxItems = 1)))
  
  # Teammate input
  output$tm <- renderUI(
    conditionalPanel(
      condition = length(tm.reg()) > 0,
      selectizeInput("tm", "Teammate(s)", choices = tm.all(), selected = tm.reg(), multiple = TRUE)
    ))
  
  
  # WOWY Query
  w.query <- reactive({
    
    # Players input
    if (length(input$wname) < 1) {
      playervector <- NULL
    } else {
      playervector <- gsub("'", "''", input$wname)
    }
    
    # Strength input
    if (input$wstrength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$wstrength
    }
    
    # Venue input
    if (input$wvenue == "Any") {
      venuevector <- c("Home", "Away")
    } else {
      venuevector <- input$wvenue
    }
    
    # Columns
    columnvector <- c("TOI", "Date", "P1", "P2", "[Combo.Code]",
                      "CF", "CA", "FF", "FA",
                      "SF", "SA", "GF", "GA",
                      "xGF", "xGA",
                      "ACF", "ACA", "AFF", "AFA",
                      "ASF", "ASA", "AGF", "AGA",
                      "AxGF", "AxGA",
                      "MCF", "MCA", "MFF", "MFA",
                      "MSF", "MSA", "MGF", "MGA",
                      "MxGF", "MxGA",
                      "OZS", "DZS", "NZS",
                      "OZF", "DZF", "NZF",
                      "[P1.G]", "[P1.A1]", "[P1.A2]",
                      "[P2.G]", "[P2.A1]", "[P2.A2]",
                      "tTOI",
                      "OCF", "OCA", "OFF", "OFA",
                      "OSF", "OSA", "OGF", "OGA",
                      "OxGF", "OxGA",
                      "OACF", "OACA", "OAFF", "OAFA",
                      "OASF", "OASA", "OAGF", "OAGA",
                      "OAxGF", "OAxGA",
                      "OMCF", "OMCA", "OMFF", "OMFA",
                      "OMSF", "OMSA", "OMGF", "OMGA",
                      "OMxGF", "OMxGA")
    
    paste("SELECT ",
          paste(unique(columnvector), collapse = ","),
          " FROM combogame WHERE Date >= '", 
          input$wdate[1],
          "' AND Date <= '", 
          input$wdate[2],
          "' AND (P1 IN ('",
          paste(playervector, collapse = "','"),
          "') OR P2 IN ('",
          paste(playervector, collapse = "','"),
          "')) AND ((Venue == 'Home' AND [Strength.State] IN ('",
          paste(strengthvector, collapse = "','"),
          "')) OR (Venue == 'Away' AND [Strength.State] IN ('",
          paste(str_rev(strengthvector), collapse = "','"),
          "'))) AND Venue IN ('",
          paste(venuevector, collapse = "','"),
          "')",
          sep = "")
    
  })
  
  # Player Query
  p.query <- reactive({
    
    # Players input
    if (length(input$wname) < 1) {
      playervector <- NULL
    } else {
      playervector <- gsub("'", "''", input$wname)
    }
    
    # Teammates input
    if (length(input$tm) < 1) {
      tmvector <- NULL
    } else {
      tmvector <- gsub("'", "''", input$tm)
    }
    
    # Strength input
    if (input$wstrength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$wstrength
    }
    
    # Venue input
    if (input$wvenue == "Any") {
      venuevector <- c("Home", "Away")
    } else {
      venuevector <- input$wvenue
    }
    
    # Columns
    columnvector <- c("Player", "TOI", "Date",
                      "CF", "CA", "FF", "FA",
                      "SF", "SA", "GF", "GA",
                      "xGF", "xGA",
                      "ACF", "ACA", "AFF", "AFA",
                      "ASF", "ASA", "AGF", "AGA",
                      "AxGF", "AxGA",
                      "MCF", "MCA", "MFF", "MFA",
                      "MSF", "MSA", "MGF", "MGA",
                      "MxGF", "MxGA",
                      "OZS", "DZS", "NZS",
                      "OZF", "DZF", "NZF",
                      "G", "A1", "A2",
                      "tTOI",
                      "OCF", "OCA", "OFF", "OFA",
                      "OSF", "OSA", "OGF", "OGA",
                      "OxGF", "OxGA",
                      "OACF", "OACA", "OAFF", "OAFA",
                      "OASF", "OASA", "OAGF", "OAGA",
                      "OAxGF", "OAxGA",
                      "OMCF", "OMCA", "OMFF", "OMFA",
                      "OMSF", "OMSA", "OMGF", "OMGA",
                      "OMxGF", "OMxGA")
    
    paste("SELECT ",
          paste(unique(columnvector), collapse = ","),
          " FROM playergame WHERE Date >= '", 
          input$wdate[1],
          "' AND Date <= '", 
          input$wdate[2],
          "' AND Player IN ('",
          paste(c(playervector, tmvector), collapse = "','"),
          "') AND ((Venue == 'Home' AND [Strength.State] IN ('",
          paste(strengthvector, collapse = "','"),
          "')) OR (Venue == 'Away' AND [Strength.State] IN ('",
          paste(str_rev(strengthvector), collapse = "','"),
          "'))) AND Venue IN ('",
          paste(venuevector, collapse = "','"),
          "')",
          sep = "")
    
  })
  
  # Load combo data
  w.data <- eventReactive(input$go, {
    
    query <- w.query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  # Load player data
  p.data <- reactive({
    
    query <- p.query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  # Teammates list
  tm.reg <- reactive({
    
    if (length(input$wname) < 1) {
      
      tm.list <- NULL
      
      return(tm.list)
      
    } else {
      
      data <- w.sum() %>% select(c(TM, TOI)) %>% arrange(desc(TOI))
      
      tm.list <- unique(as.character(data$TM))[1:10]
      
      return(tm.list)
      
    }
    
  })
  
  tm.all <- reactive({
    
    data <- w.sum()
    
    tm.list <- unique(as.character(data$TM))
    
    tm.list
    
  })
  
  # Aggregate
  w.sum <- reactive({
    
    w.data <- w.data()
    p.data <- p.data()
    
    w.sum <- rbind_list(
      filter(w.data, P1 == input$wname) %>% group_by(Combo.Code) %>%
        summarise(Player = first(P1), TM = first(P2),
                  TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                  SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                  ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                  AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
                  P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
                  P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA)) %>%
        data.frame(),
      filter(w.data, P2 == input$wname) %>% group_by(Combo.Code) %>%
        summarise(Player = first(P2), TM = first(P1),
                  TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                  SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                  ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                  AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
                  P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
                  P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA)) %>%
        data.frame()
    ) %>% data.frame() %>% group_by(Combo.Code) %>%
      summarise(Player = first(Player), TM = first(TM),
                TOI = sum(TOI),
                CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
                P1G = sum(P1.G), P1A1 = sum(na.omit(P1.A1)), P1A2 = sum(na.omit(P1.A2)),
                P2G = sum(P2.G), P2A1 = sum(na.omit(P2.A1)), P2A2 = sum(na.omit(P2.A2)),
                tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA)) %>%
      data.frame()
    
    # Adjust
    if (input$wadjust == "Score, Zone and Venue") {
      w.sum <- select(w.sum, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA,
                                OCF, OCA, OFF, OFA, OSF, OSA, OGF, OGA, OxGF, OxGA, OMCF, OMCA, OMFF, OMFA, OMSF, OMSA, OMGF, OMGA, OMxGF, OMxGA)) %>% 
        rename(CF = ACF, CA = ACA, FF = AFF, FA = AFA, SF = ASF, SA = ASA, GF = AGF, GA = AGA, xGF = AxGF, xGA = AxGA,
               OCF = OACF, OCA = OACA, OFF = OAFF, OFA = OAFA, OSF = OASF, OSA = OASA, OGF = OAGF, OGA = OAGA, OxGF = OAxGF, OxGA = OAxGA) %>% data.frame()
    } else if (input$wadjust == "Score and Venue") {
      w.sum <- select(w.sum, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA,
                                OCF, OCA, OFF, OFA, OSF, OSA, OGF, OGA, OxGF, OxGA, OACF, OACA, OAFF, OAFA, OASF, OASA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        rename(CF = MCF, CA = MCA, FF = MFF, FA = MFA, SF = MSF, SA = MSA, GF = MGF, GA = MGA, xGF = MxGF, xGA = MxGA,
               OCF = OMCF, OCA = OMCA, OFF = OMFF, OFA = OMFA, OSF = OMSF, OSA = OMSA, OGF = OMGF, OGA = OMGA, OxGF = OMxGF, OxGA = OMxGA) %>% data.frame()
    } else {
      w.sum <- select(w.sum, -c(MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA,
                                OMCF, OMCA, OMFF, OMFA, OMSF, OMSA, OMGF, OMGA, OMxGF, OMxGA, OACF, OACA, OAFF, OAFA, OASF, OASA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        data.frame()
    }
    
    p.sum <- group_by(p.data, Player) %>%
      summarise(TOI = sum(TOI),
                CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
                SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
                ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
                AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
                OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
                G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)),
                tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA)) %>%
      data.frame()
    
    # Adjust
    if (input$wadjust == "Score, Zone and Venue") {
      p.sum <- select(p.sum, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA,
                                OCF, OCA, OFF, OFA, OSF, OSA, OGF, OGA, OxGF, OxGA, OMCF, OMCA, OMFF, OMFA, OMSF, OMSA, OMGF, OMGA, OMxGF, OMxGA)) %>% 
        rename(CF = ACF, CA = ACA, FF = AFF, FA = AFA, SF = ASF, SA = ASA, GF = AGF, GA = AGA, xGF = AxGF, xGA = AxGA,
               OCF = OACF, OCA = OACA, OFF = OAFF, OFA = OAFA, OSF = OASF, OSA = OASA, OGF = OAGF, OGA = OAGA, OxGF = OAxGF, OxGA = OAxGA) %>% data.frame()
    } else if (input$wadjust == "Score and Venue") {
      p.sum <- select(p.sum, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA,
                                OCF, OCA, OFF, OFA, OSF, OSA, OGF, OGA, OxGF, OxGA, OACF, OACA, OAFF, OAFA, OASF, OASA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        rename(CF = MCF, CA = MCA, FF = MFF, FA = MFA, SF = MSF, SA = MSA, GF = MGF, GA = MGA, xGF = MxGF, xGA = MxGA,
               OCF = OMCF, OCA = OMCA, OFF = OMFF, OFA = OMFA, OSF = OMSF, OSA = OMSA, OGF = OMGF, OGA = OMGA, OxGF = OMxGF, OxGA = OMxGA) %>% data.frame()
    } else {
      p.sum <- select(p.sum, -c(MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA,
                                OMCF, OMCA, OMFF, OMFA, OMSF, OMSA, OMGF, OMGA, OMxGF, OMxGA, OACF, OACA, OAFF, OAFA, OASF, OASA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        data.frame()
    }
    
    merged <- merge(w.sum,
                    p.sum %>%
                      rename(P1.CF = CF, P1.CA = CA, P1.FF = FF, P1.FA = FA, P1.SF = SF, P1.SA = SA, P1.GF = GF, P1.GA = GA, P1.xGF = xGF, P1.xGA = xGA,
                             P1.OCF = OCF, P1.OCA = OCA, P1.OFF = OFF, P1.OFA = OFA, P1.OSF = OSF, P1.OSA = OSA, P1.OGF = OGF, P1.OGA = OGA, P1.OxGF = OxGF, P1.OxGA = OxGA,
                             P1.G = G, P1.A1 = A1, P1.A2 = A2,
                             P1.TOI = TOI, P1.tTOI = tTOI, P1.OZS = OZS, P1.DZS = DZS, P1.NZS = NZS, P1.OZF = OZF, P1.DZF = DZF, P1.NZF = NZF),
                    by.x = "Player", by.y = "Player", all.x = TRUE
    ) %>% data.frame() %>%
      merge(p.sum %>%
              rename(P2.CF = CF, P2.CA = CA, P2.FF = FF, P2.FA = FA, P2.SF = SF, P2.SA = SA, P2.GF = GF, P2.GA = GA, P2.xGF = xGF, P2.xGA = xGA,
                     P2.OCF = OCF, P2.OCA = OCA, P2.OFF = OFF, P2.OFA = OFA, P2.OSF = OSF, P2.OSA = OSA, P2.OGF = OGF, P2.OGA = OGA, P2.OxGF = OxGF, P2.OxGA = OxGA,
                     P2.G = G, P2.A1 = A1, P2.A2 = A2,
                     P2.TOI = TOI, P2.tTOI = tTOI, P2.OZS = OZS, P2.DZS = DZS, P2.NZS = NZS, P2.OZF = OZF, P2.DZF = DZF, P2.NZF = NZF),
            by.x = "TM", by.y = "Player", all.x = TRUE
      ) %>% data.frame()
    
    wowy <- mutate(merged,
                   CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, Rel.CF. = CF. - (OCF/(OCF + OCA)*100),
                   FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, Rel.FF. = FF. - (OFF/(OFF + OFA)*100),
                   SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Rel.SF. = SF. - (OSF/(OSF + OSA)*100),
                   GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, Rel.GF. = GF. - (OGF/(OGF + OGA)*100),
                   xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Rel.xGF. = xGF. - (OxGF/(OxGF + OxGA)*100),
                   ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
                   Sh. = GF/SF*100, Sv. = (1 - GA/SA)*100, PDO = Sh. + Sv.,
                   P1.TOI = P1.TOI - TOI,
                   P1.CF = P1.CF - CF, P1.CA = P1.CA - CA, 
                   P1.FF = P1.FF - FF, P1.FA = P1.FA - FA, 
                   P1.SF = P1.SF - SF, P1.SA = P1.SA - SA, 
                   P1.GF = P1.GF - GF, P1.GA = P1.GA - GA, 
                   P1.xGF = P1.xGF - xGF, P1.xGA = P1.xGA - xGA, 
                   P1.OZS = P1.OZS - OZS, P1.DZS = P1.DZS - DZS, P1.NZS = P1.NZS - NZS,
                   P1.OZF = P1.OZF - OZF, P1.DZF = P1.DZF - DZF, P1.NZF = P1.NZF - NZF,
                   P1.CF60 = (P1.CF)/(P1.TOI)*60, P1.CA60 = (P1.CA)/(P1.TOI)*60, 
                   P1.CF. = (P1.CF/(P1.CF + P1.CA))*100, P1.Rel.CF. = P1.CF. - ((P1.OCF + CF)/(P1.OCF + CF + P1.OCA + CA)*100),
                   P1.FF60 = (P1.FF)/(P1.TOI)*60, P1.FA60 = (P1.FA)/(P1.TOI)*60, 
                   P1.FF. = (P1.FF/(P1.FF + P1.FA))*100, P1.Rel.FF. = P1.FF. - ((P1.OFF + FF)/(P1.OFF + FF + P1.OFA + FA)*100),
                   P1.SF60 = (P1.SF)/(P1.TOI)*60, P1.SA60 = (P1.SA)/(P1.TOI)*60, 
                   P1.SF. = (P1.SF/(P1.SF + P1.SA))*100, P1.Rel.SF. = P1.SF. - ((P1.OSF + SF)/(P1.OSF + SF + P1.OSA + SA)*100),
                   P1.GF60 = (P1.GF)/(P1.TOI)*60, P1.GA60 = (P1.GA)/(P1.TOI)*60, 
                   P1.GF. = (P1.GF/(P1.GF + P1.GA))*100, P1.Rel.GF. = P1.GF. - ((P1.OGF + GF)/(P1.OGF + GF + P1.OGA + GA)*100),
                   P1.xGF60 = (P1.xGF)/(P1.TOI)*60, P1.xGA60 = (P1.xGA)/(P1.TOI)*60, 
                   P1.xGF. = (P1.xGF/(P1.xGF + P1.xGA))*100, P1.Rel.xGF. = P1.xGF. - ((P1.OxGF + xGF)/(P1.OxGF + xGF + P1.OxGA + xGA)*100),
                   P1.ZSR = P1.OZS/(P1.OZS + P1.DZS)*100, P1.ZFR = P1.OZF/(P1.OZF + P1.DZF)*100, 
                   P1.Sh. = (P1.GF - GF)/(P1.SF - SF)*100, P1.Sv. = (1 - (P1.GA - GA)/(P1.SA - SA))*100, P1.PDO = P1.Sh. + P1.Sv.,
                   P2.TOI = P2.TOI - TOI,
                   P2.CF = P2.CF - CF, P2.CA = P2.CA - CA, 
                   P2.FF = P2.FF - FF, P2.FA = P2.FA - FA, 
                   P2.SF = P2.SF - SF, P2.SA = P2.SA - SA, 
                   P2.GF = P2.GF - GF, P2.GA = P2.GA - GA, 
                   P2.xGF = P2.xGF - xGF, P2.xGA = P2.xGA - xGA, 
                   P2.OZS = P2.OZS - OZS, P2.DZS = P2.DZS - DZS, P2.NZS = P2.NZS - NZS,
                   P2.OZF = P2.OZF - OZF, P2.DZF = P2.DZF - DZF, P2.NZF = P2.NZF - NZF,
                   P2.CF60 = (P2.CF)/(P2.TOI)*60, P2.CA60 = (P2.CA)/(P2.TOI)*60, 
                   P2.CF. = (P2.CF/(P2.CF + P2.CA))*100, P2.Rel.CF. = P2.CF. - ((P2.OCF + CF)/(P2.OCF + CF + P2.OCA + CA)*100),
                   P2.FF60 = (P2.FF)/(P2.TOI)*60, P2.FA60 = (P2.FA)/(P2.TOI)*60, 
                   P2.FF. = (P2.FF/(P2.FF + P2.FA))*100, P2.Rel.FF. = P2.FF. - ((P2.OFF + FF)/(P2.OFF + FF + P2.OFA + FA)*100),
                   P2.SF60 = (P2.SF)/(P2.TOI)*60, P2.SA60 = (P2.SA)/(P2.TOI)*60, 
                   P2.SF. = (P2.SF/(P2.SF + P2.SA))*100, P2.Rel.SF. = P2.SF. - ((P2.OSF + SF)/(P2.OSF + SF + P2.OSA + SA)*100),
                   P2.GF60 = (P2.GF)/(P2.TOI)*60, P2.GA60 = (P2.GA)/(P2.TOI)*60, 
                   P2.GF. = (P2.GF/(P2.GF + P2.GA))*100, P2.Rel.GF. = P2.GF. - ((P2.OGF + GF)/(P2.OGF + GF + P2.OGA + GA)*100),
                   P2.xGF60 = (P2.xGF)/(P2.TOI)*60, P2.xGA60 = (P2.xGA)/(P2.TOI)*60, 
                   P2.xGF. = (P2.xGF/(P2.xGF + P2.xGA))*100, P2.Rel.xGF. = P2.xGF. - ((P2.OxGF + xGF)/(P2.OxGF + xGF + P2.OxGA + xGA)*100),
                   P2.ZSR = P2.OZS/(P2.OZS + P2.DZS)*100, P2.ZFR = P2.OZF/(P2.OZF + P2.DZF)*100,
                   P2.Sh. = (P2.GF - GF)/(P2.SF - SF)*100, P2.Sv. = (1 - (P2.GA - GA)/(P2.SA - SA))*100, P2.PDO = P2.Sh. + P2.Sv.
    ) %>%
      data.frame()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    # Format
    wowy <- mutate_each(wowy, funs(form), -c(Player, TM, Combo.Code))
    
    wowy
    
  })
  
  # Table contents
  w.contents <- reactive({
    
    data <- w.sum() %>% filter(!is.na(P2.TOI)) %>% arrange(desc(TOI))
    
    # Stack
    stacked <- rbind_list(
      mutate(data, Situation = paste(Player, "WITH", TM), order = cumsum(!is.na(TM)), sit2 = "Together") %>%
        select(c(Situation, order, sit2, Player, TM, TOI:xGA, OZS:NZF, CF60:PDO)) %>%
        data.frame(),
      mutate(data, Situation = paste(Player, "WITHOUT", TM), order = cumsum(!is.na(TM)), sit2 = "Player Without") %>%
        select(c(Situation, order, sit2, Player, TM, P1.TOI:P1.xGA, P1.OZS:P1.NZF, P1.CF60:P1.PDO)) %>% 
        rename(TOI = P1.TOI, CF = P1.CF, CA = P1.CA, FF = P1.FF, FA = P1.FA, SF = P1.SF, SA = P1.SA, GF = P1.GF, GA = P1.GA, xGF = P1.xGF, xGA = P1.xGA,
               OZS = P1.OZS, DZS = P1.DZS, NZS = P1.NZS, OZF = P1.OZF, DZF = P1.DZF, NZF = P1.NZF,
               CF60 = P1.CF60, CA60 = P1.CA60, CF. = P1.CF., Rel.CF. = P1.Rel.CF.,
               FF60 = P1.FF60, FA60 = P1.FA60, FF. = P1.FF., Rel.FF. = P1.Rel.FF.,
               SF60 = P1.SF60, SA60 = P1.SA60, SF. = P1.SF., Rel.SF. = P1.Rel.SF.,
               GF60 = P1.GF60, GA60 = P1.GA60, GF. = P1.GF., Rel.GF. = P1.Rel.GF.,
               xGF60 = P1.xGF60, xGA60 = P1.xGA60, xGF. = P1.xGF., Rel.xGF. = P1.Rel.xGF.,
               ZSR = P1.ZSR, ZFR = P1.ZFR,
               Sh. = P1.Sh., Sv. = P1.Sv., PDO = P1.PDO) %>%
        data.frame(),
      mutate(data, Situation = paste(TM, "WITHOUT", Player), order = cumsum(!is.na(TM)), sit2 = "Teammate Without") %>%
        select(c(Situation, order, sit2, Player, TM, P2.TOI:P2.xGA, P2.OZS:P2.NZF, P2.CF60:P2.PDO)) %>% 
        rename(TOI = P2.TOI, CF = P2.CF, CA = P2.CA, FF = P2.FF, FA = P2.FA, SF = P2.SF, SA = P2.SA, GF = P2.GF, GA = P2.GA, xGF = P2.xGF, xGA = P2.xGA,
               OZS = P2.OZS, DZS = P2.DZS, NZS = P2.NZS, OZF = P2.OZF, DZF = P2.DZF, NZF = P2.NZF,
               CF60 = P2.CF60, CA60 = P2.CA60, CF. = P2.CF., Rel.CF. = P2.Rel.CF.,
               FF60 = P2.FF60, FA60 = P2.FA60, FF. = P2.FF., Rel.FF. = P2.Rel.FF.,
               SF60 = P2.SF60, SA60 = P2.SA60, SF. = P2.SF., Rel.SF. = P2.Rel.SF.,
               GF60 = P2.GF60, GA60 = P2.GA60, GF. = P2.GF., Rel.GF. = P2.Rel.GF.,
               xGF60 = P2.xGF60, xGA60 = P2.xGA60, xGF. = P2.xGF., Rel.xGF. = P2.Rel.xGF.,
               ZSR = P2.ZSR, ZFR = P2.ZFR,
               Sh. = P2.Sh., Sv. = P2.Sv., PDO = P2.PDO) %>%
        data.frame()
    ) %>% data.frame() %>% arrange(order)
    
    # Report input
    columnvector <- c("Situation", "sit2", "Player", "TM", "TOI")
    
    if ("Corsi" %in% input$columns) {
      columnvector <- c(columnvector,
                        "CF", "CA",
                        "CF60", "CA60",
                        "CF.", "Rel.CF.")
    }
    
    if ("Fenwick" %in% input$columns) {
      columnvector <- c(columnvector,
                        "FF", "FA",
                        "FF60", "FA60",
                        "FF.", "Rel.FF.")
    }
    
    if ("Shots on goal" %in% input$columns) {
      columnvector <- c(columnvector,
                        "SF", "SA",
                        "SF60", "SA60",
                        "SF.", "Rel.SF.",
                        "Sh.", "Sv.")
    }
    
    if ("Goals" %in% input$columns) {
      columnvector <- c(columnvector,
                        "GF", "GA",
                        "GF60", "GA60",
                        "GF.", "Rel.GF.",
                        "Sh.", "Sv.")
    }
    
    if ("Expected Goals" %in% input$columns) {
      columnvector <- c(columnvector,
                        "xGF", "xGA",
                        "xGF60", "xGA60",
                        "xGF.", "Rel.xGF.")
    }
    
    if ("Context" %in% input$columns) {
      columnvector <- c(columnvector,
                        "OZS", "DZS", "NZS",
                        "OZF", "DZF", "NZF",
                        "ZSR", "ZFR",
                        "Sh.", "Sv.", "PDO")
    }
    
    indexvector <- which(colnames(stacked) %in% columnvector)
    
    # Subset columns
    t3 <- select(stacked, indexvector) %>% data.frame()
    colnames(t3) <- gsub("[.]$", "%", colnames(t3))
    
    t3
    
  })
  
  # Axis input
  output$axis <- renderUI(selectInput("axis", "Y-Axis Variable", 
                                      choices = unique(c("CF%", colnames(w.contents())[which(colnames(w.contents()) %in% 
                                                                                               c("CF%", "CF60", "CA60", "Rel.CF%",
                                                                                                 "FF%", "FF60", "FA60", "Rel.FF%",
                                                                                                 "SF%", "SF60", "SA60", "Rel.SF%",
                                                                                                 "GF%", "GF60", "GA60", "Rel.GF%",
                                                                                                 "xGF%", "xGF60", "xGA60", "Rel.xGF%",
                                                                                                 "Sh%", "Sv%", "PDO")
                                      )])), 
                                      selected = "CF%"))
  
  # Table output
  output$t3 <- DT::renderDataTable({
    
    t3 <- w.contents()
    
    excl <- which(colnames(t3) %in% c("Player", "TM", "sit2"))
    
    t3 <- select(t3, -c(excl))
    
    datatable(t3, 
              extensions = list(FixedColumns = list(leftColumns = 1)),
              options = list(searching = F, paging = T, pageLength = 30, scrollX = T, info = FALSE, autoWidth = T,
                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                             selection = list(mode = 'single', target = 'row'),
                             dom = 'ltp',
                             scrollCollapse = T,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#4863A0', 'color': '#fff'});",
                               "}")),
              rownames = F) %>%
      formatStyle(c("Situation"), fontWeight = "bold")
    
  })
  
  # Bubble plot output
  output$plot1 <- renderPlot({
    
    require(ggplot2)
    require(ggthemes)
    
    data <- w.contents()
    
    col.index <- which(colnames(data) == input$axis)
    
    series <- select(data, c(Player, TM, sit2, TOI, col.index))
    
    colnames(series)[5] <- "measure"
    
    if (input$wadjust == "None") {
      title.measure <- input$axis
    } else {
      title.measure <- paste(input$wadjust, "Adjusted", input$axis)
    }
    
    p <- ggplot(series, aes(x = TM, y = measure)) + 
      geom_point(aes(color = sit2, size = TOI), alpha = 0.6) +
      scale_size(range = c(6, 20), guide = "none") +
      scale_color_manual(values = c("limegreen", "red1", "dodgerblue")) +
      labs(
        title = paste(input$wname, "With and Without Teammates"), 
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
    
    print(p)
    
  })
  
  # Violin plot output
  output$plot2 <- renderPlot({
    
    require(ggplot2)
    require(ggthemes)
    
    data <- w.contents()
    
    col.index <- which(colnames(data) == input$axis)
    
    series <- select(data, c(Player, TM, sit2, TOI, col.index))
    
    colnames(series)[5] <- "measure"
    
    if (input$wadjust == "None") {
      title.measure <- input$axis
    } else {
      title.measure <- paste(input$wadjust, "Adjusted", input$axis)
    }
    
    p <- ggplot(series, aes(x = sit2, y = measure, weight = TOI)) + 
      geom_violin(aes(fill = sit2, color = sit2), alpha = 0.5) +
      scale_size(range = c(10, 20), guide = "none") +
      scale_fill_manual(values = c("limegreen", "red1", "dodgerblue")) +
      scale_color_manual(values = c("limegreen", "red1", "dodgerblue")) +
      labs(
        title = paste(input$wname, "With and Without Teammates"), 
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
    
    print(p)
    
  })
  
  # Dashboard output
  output$dash <- renderUI({
    
    if (length(tm.all()) > 1 & length(w.contents()) > 1) {
      
      fluidPage(
        
        fluidRow(
          box(
            uiOutput("tm"),
            if(length(input$columns) < 1) {
              selectizeInput("columns", "Report", choices = c("Corsi", "Fenwick", "Shots on goal", "Goals", "Expected Goals", "Scoring", "Context"), 
                             selected = c("Corsi", "Expected Goals", "Scoring"), multiple = TRUE)
            } else {
              selectizeInput("columns", "Report", choices = c("Corsi", "Fenwick", "Shots on goal", "Goals", "Expected Goals", "Scoring", "Context"), 
                             selected = input$columns, multiple = TRUE)
            },
            uiOutput("axis"),
            width = 3,
            title = "Inputs",
            solidHeader = TRUE,
            collapsible = TRUE
            
          ),
          box(
            
            tabsetPanel(
              
              tabPanel("Bubble",
                       tags$div(class = "bottom"),
                       plotOutput("plot1")
              ),
              
              tabPanel("Violin",
                       tags$div(class = "bottom"),
                       plotOutput("plot2")
              )
              
            ),
            
            width = 9,
            title = "Graphical",
            solidHeader = TRUE,
            collapsible = TRUE
            
          )
        ),
        
        fluidRow(
          
          DT::dataTableOutput("t3"),
          
          tags$div(class = "bottom")
          
          
        ))
      
    }
    
  })
  
})
