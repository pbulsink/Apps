# Server

# Corsica Player App
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

seasons <- sqliteQuickColumn(con, "playerseason", "Season")
names <- sqliteQuickColumn(con, "playerseason", "Player")
teams <- sqliteQuickColumn(con, "playerseason", "Team")

shinyServer(function(input, output) {
  
  ### TAB: PLAYER STATS 
  
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
    
    paste("SELECT * FROM playerseason WHERE Season IN (", paste(seasonvector, collapse = ","), ") AND [Season.Type] IN ('", paste(typevector, collapse = "','"), "')", sep = "")
    
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
      scorevector <- c(-1:1)
    } else if (input$score == "Leading") {
      scorevector <- 1
    } else if (input$score == "Trailing") {
      scorevector <- -1
    } else if (input$score == "Even") {
      scorevector <- 0
    }
    
    # Position input
    if (input$pos == "Any") {
      posvector <- unique(as.character(data$Position))
    } else if (input$pos == "Forward") {
      posvector <- c("R", "C", "L")
    } else if (input$pos == "Defence") {
      posvector <- "D"
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
      playergp <- group_by(data, Player, Season, Season.Type) %>% summarise(GP = max(GP)) %>% data.frame() %>%
        group_by(Player) %>% summarise(GP = sum(GP)) %>% data.frame()
      
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Venue %in% venuevector & 
        grepl(paste(posvector, collapse = "|"), Position) == TRUE & grepl(teamvector, Team) == TRUE & tolower(Player) %in% playervector) %>% 
        group_by(Player) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  Season.Type = paste(unique(Season.Type), collapse = "/"),
                  Position = first(Position), Team = paste(unique(Team), collapse = "/"), 
                  GP = playergp$GP[match(first(Player), playergp$Player)], TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
                  iRB = sum(iRB), iRS = sum(iRS),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>%
        data.frame()
    } else {
      sub <- filter(data, {{Venue == "Home" & Strength.State %in% strengthvector} | {Venue == "Away" & Strength.State %in% str_rev(strengthvector)}} & 
      {{Venue == "Home" & Score.Cat %in% scorevector} | {Venue == "Away" & Score.Cat %in% -scorevector}} & Venue %in% venuevector &
        grepl(paste(posvector, collapse = "|"), Position) == TRUE & grepl(teamvector, Team) == TRUE & tolower(Player) %in% playervector) %>% 
        group_by(Player, Season, Season.Type) %>% 
        summarise(Position = first(Position), Team = paste(unique(Team), collapse = "/"), 
                  GP = max(GP), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA),
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
                  iRB = sum(iRB), iRS = sum(iRS),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>%
        data.frame()
    }
    
    arrange(sub, Player) %>% select(c(Player, Season, Season.Type, Position, Team, GP, TOI, OTOI, # /Base
                                      G, A1, A2, A, P, P1,
                                      G60, A160, A260, A60, P60, P160,
                                      iCF, iCF60, iCSh., iFF, iFF60, iFSh.,
                                      iSF, iSF60, iSh., ixG, ixG60, ixFSh.,
                                      iHF, iHA, iGVA, iTKA, iBLK, 
                                      iFOW, iFOL, iFO.,
                                      iPENT, iPEND, iPENDIFF, 
                                      iRB, iRS, # /Individual
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
                                      OZS, DZS, NZS, OTF, OZS., DZS., NZS., ZSR,
                                      OZF, DZF, NZF, OZF., DZF., NZF., ZFR, TOI.,
                                      TOI.QoT, CF.QoT, xGF.QoT,
                                      TOI.QoC, CF.QoC, xGF.QoC, Avg.DIST, # /Context
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
      mutate_each(funs(form), -c(Player, Season, Season.Type, Position, Team, GP, ACF:Rel.MxGA60))
    
    # Report input
    if (input$report == "On-Ice") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "CF60", "CA60", "CF.", "CSh.", "CSv.",
                                                "FF", "FA", "FF60", "FA60", "FF.", "FSh.", "FSv.",
                                                "SF", "SA", "SF60", "SA60", "SF.", "Sh.", "Sv.",
                                                "PDO", "xPDO", "GF", "GA", "GF60", "GA60", "GF.",
                                                "xGF", "xGA", "xGF60", "xGA60", "xGF.", "xFSh.", "xFSv.", "Adj.FSv.",
                                                "PENDIFF", "FO."))
    } else if (input$report == "Off-Ice") {
      reportvector <- which(colnames(t1) %in% c("OTOI",
                                                "OCF", "OCA", "OCF60", "OCA60", "OCF.", "OCSh.", "OCSv.",
                                                "OFF", "OFA", "OFF60", "OFA60", "OFF.", "OFSh.", "OFSv.",
                                                "OSF", "OSA", "OSF60", "OSA60", "OSF.", "OSh.", "OSv.",
                                                "OGF", "OGA", "OGF60", "OGA60", "OGF.",
                                                "OxGF", "OxGA", "OxGF60", "OxGA60", "OxGF.", "OxFSh.", "OxFSv.",
                                                "OOZS", "ODZS", "ONZS", "OZSR"))
    } else if (input$report == "Relative") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "Rel.CF60", "Rel.CA60", "Rel.CF.", "Rel.CSh.", "Rel.CSv.",
                                                "Rel.FF60", "Rel.FA60", "Rel.FF.", "Rel.FSh.", "Rel.FSv.",
                                                "Rel.SF60", "Rel.SA60", "Rel.SF.", "Rel.Sh.", "Rel.Sv.",
                                                "Rel.GF60", "Rel.GA60", "Rel.GF.", 
                                                "Rel.xGF60", "Rel.xGA60", "Rel.xGF.", "Rel.xFSh.", "Rel.xFSv.",
                                                "Rel.ZSR"))
    } else if (input$report == "Individual") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "G", "A1", "A2", "A", "P", "P1",
                                                "G60", "A160", "A260", "A60", "P60", "P160",
                                                "iCF", "iCF60", "iCSh.",
                                                "iFF", "iFF60", "iFSh.",
                                                "iSF", "iSF60", "iSh.",
                                                "ixG", "ixG60", "ixFSh.",
                                                "iHF", "iHA", "iGVA", "iTKA", "iBLK",
                                                "iFOW", "iFOL", "iFO.",
                                                "iPENT", "iPEND", "iPENDIFF",
                                                "Avg.DIST", "iRB", "iRS"))
    } else if (input$report == "Context") {
      reportvector <- which(colnames(t1) %in% c("TOI", "TOI.",
                                                "OZS", "DZS", "NZS", "OTF",
                                                "OZF", "DZF", "NZF", 
                                                "OZS.", "DZS.", "NZS.", "OZF.", "DZF.", "NZF.",
                                                "ZSR", "ZFR", "PDO", "xPDO", 
                                                "TOI.QoT", "CF.QoT", "xGF.QoT",
                                                "TOI.QoC", "CF.QoC", "xGF.QoC",
                                                "Avg.DIST"))
    } else if (input$report == "Counts") {
      reportvector <- which(colnames(t1) %in% c("TOI",
                                                "CF", "CA", "FF", "FA", "SF", "SA", "GF", "GA", "xGF", "xGA", 
                                                "OZS", "DZS", "NZS", "OTF", "OZF", "DZF", "NZF", 
                                                "FOW", "FOL", "HF", "HA", "GVA", "TKA", "PENT", "PEND",
                                                "G", "A1", "A2", "A", "P", "P1", 
                                                "iCF", "iFF", "iSF", "ixG",
                                                "iHF", "iHA", "iGVA", "iTKA", "iBLK", 
                                                "iFOW", "iFOL", "iPENT", "iPEND",
                                                "RBF", "RBA", "RSF", "RSA"))
    }
    
    # Adjustment
    if (input$adjust == "Score, Zone and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(ACF, 2), nsmall = 2)), CA = as.numeric(format(round(ACA, 2), nsmall = 2)), CF60 = as.numeric(format(round(ACF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(ACA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*ACF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.ACF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(AFF, 2), nsmall = 2)), FA = as.numeric(format(round(AFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(AFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(AFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*AFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.AFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(ASF, 2), nsmall = 2)), SA = as.numeric(format(round(ASA, 2), nsmall = 2)), SF60 = as.numeric(format(round(ASF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(ASA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*ASF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.ASF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(AGF, 2), nsmall = 2)), GA = as.numeric(format(round(AGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(AGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(AGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*AGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.AGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(AxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(AxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(AxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(AxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*AxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.AxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OACF, 2), nsmall = 2)), OCA = as.numeric(format(round(OACA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OACF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OACA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OAFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OAFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OAFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OAFA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OASF, 2), nsmall = 2)), OSA = as.numeric(format(round(OASA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OASF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OASA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OAGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OAGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OAGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OAGA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OAxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OAxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OAxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OAxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OACF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.ACF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.ACA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.AFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.AFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.ASF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.ASA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.AGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.AGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.AxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.AxGA60, 2), nsmall = 2)))
    } else if (input$adjust == "Score and Venue") {
      t1 <- mutate(t1, CF = as.numeric(format(round(MCF, 2), nsmall = 2)), CA = as.numeric(format(round(MCA, 2), nsmall = 2)), CF60 = as.numeric(format(round(MCF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(MCA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*MCF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.MCF., 2), nsmall = 2)),
                   FF = as.numeric(format(round(MFF, 2), nsmall = 2)), FA = as.numeric(format(round(MFA, 2), nsmall = 2)), FF60 = as.numeric(format(round(MFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(MFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*MFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.MFF., 2), nsmall = 2)),
                   SF = as.numeric(format(round(MSF, 2), nsmall = 2)), SA = as.numeric(format(round(MSA, 2), nsmall = 2)), SF60 = as.numeric(format(round(MSF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(MSA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*MSF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.MSF., 2), nsmall = 2)),
                   GF = as.numeric(format(round(MGF, 2), nsmall = 2)), GA = as.numeric(format(round(MGA, 2), nsmall = 2)), GF60 = as.numeric(format(round(MGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(MGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*MGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.MGF., 2), nsmall = 2)),
                   xGF = as.numeric(format(round(MxGF, 2), nsmall = 2)), xGA = as.numeric(format(round(MxGA, 2), nsmall = 2)), xGF60 = as.numeric(format(round(MxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(MxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*MxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.MxGF., 2), nsmall = 2)),
                   OCF = as.numeric(format(round(OMCF, 2), nsmall = 2)), OCA = as.numeric(format(round(OMCA, 2), nsmall = 2)), OCF60 = as.numeric(format(round(OMCF60, 2), nsmall = 2)), OCA60 = as.numeric(format(round(OMCA60, 2), nsmall = 2)), OCF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OFF = as.numeric(format(round(OMFF, 2), nsmall = 2)), OFA = as.numeric(format(round(OMFA, 2), nsmall = 2)), OFF60 = as.numeric(format(round(OMFF60, 2), nsmall = 2)), OFA60 = as.numeric(format(round(OMFA60, 2), nsmall = 2)), OFF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OSF = as.numeric(format(round(OMSF, 2), nsmall = 2)), OSA = as.numeric(format(round(OMSA, 2), nsmall = 2)), OSF60 = as.numeric(format(round(OMSF60, 2), nsmall = 2)), OSA60 = as.numeric(format(round(OMSA60, 2), nsmall = 2)), OSF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OGF = as.numeric(format(round(OMGF, 2), nsmall = 2)), OGA = as.numeric(format(round(OMGA, 2), nsmall = 2)), OGF60 = as.numeric(format(round(OMGF60, 2), nsmall = 2)), OGA60 = as.numeric(format(round(OMGA60, 2), nsmall = 2)), OGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   OxGF = as.numeric(format(round(OMxGF, 2), nsmall = 2)), OxGA = as.numeric(format(round(OMxGA, 2), nsmall = 2)), OxGF60 = as.numeric(format(round(OMxGF60, 2), nsmall = 2)), OxGA60 = as.numeric(format(round(OMxGA60, 2), nsmall = 2)), OxGF. = as.numeric(format(round(100*OMCF., 2), nsmall = 2)),
                   Rel.CF60 = as.numeric(format(round(Rel.MCF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.MCA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.MFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.MFA60, 2), nsmall = 2)),
                   Rel.SF60 = as.numeric(format(round(Rel.MSF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.MSA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.MGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.MGA60, 2), nsmall = 2)), 
                   Rel.xGF60 = as.numeric(format(round(Rel.MxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.MxGA60, 2), nsmall = 2)))
    }
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    select(t1, c(Player, Season, Season.Type, Position, Team, GP, reportvector))
    
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
    filename = paste("Corsica_Skater.Stats_", 
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
           OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
           CPM = CF - CA)
  }
  
  mutate.fenwick <- function(x) {
    mutate(x,
           FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100,
           OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
           FPM = FF - FA)
  }
  
  mutate.shots <- function(x) {
    mutate(x,
           SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100,
           OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
           SPM = SF - SA)
  }
  
  mutate.goals <- function(x) {
    mutate(x,
           GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100,
           OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
           GPM = GF - GA)
  }
  
  mutate.xG <- function(x) {
    mutate(x,
           xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100,
           OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
           xGPM = xGF - xGA)
  }
  
  mutate.extras <- function(x) {
    mutate(x,
           OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
           OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
           ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100)
  }
  
  mutate.scoring <- function(x) {
    mutate(x,
           A = A1 + A2, P = G + A, P1 = G + A1,
           G60 = G/TOI*60, A60 = A/TOI*60, 
           P60 = P/TOI*60, P160 = G/TOI*60)
  }
  
  mutate.individual <- function(x) {
    mutate(x,
           iCF60 = iCF/TOI*60, iFF60 = iFF/TOI*60,
           iSF60 = iSF/TOI*60, ixG60 = ixG/TOI*60,
           ixFSh. = ixG/iFF*100,
           iFO. = iFOW/(iFOW + iFOL),
           iPENDIFF = iPEND - iPENT)
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
    columnvector <- c("Player", "Date", "Team", "Position", "TOI")
    
    if ("Corsi" %in% input$columns) {
      columnvector <- c(columnvector,
                        "CF", "CA",
                        "MCF", "MCA",
                        "ACF", "ACA",
                        "OCF", "OCA",
                        "OMCF", "OMCA",
                        "OACF", "OACA")
    }
    
    if ("Fenwick" %in% input$columns) {
      columnvector <- c(columnvector,
                        "FF", "FA",
                        "MFF", "MFA",
                        "AFF", "AFA",
                        "OFF", "OFA",
                        "OMFF", "OMFA",
                        "OAFF", "OAFA")
    }
    
    if ("Shots on goal" %in% input$columns) {
      columnvector <- c(columnvector,
                        "SF", "SA",
                        "MSF", "MSA",
                        "ASF", "ASA",
                        "OSF", "OSA",
                        "OMSF", "OMSA",
                        "OASF", "OASA")
    }
    
    if ("Goals" %in% input$columns) {
      columnvector <- c(columnvector,
                        "GF", "GA",
                        "MGF", "MGA",
                        "AGF", "AGA",
                        "OGF", "OGA",
                        "OMGF", "OMGA",
                        "OAGF", "OAGA")
    }
    
    if ("Expected Goals" %in% input$columns) {
      columnvector <- c(columnvector,
                        "xGF", "xGA",
                        "MxGF", "MxGA",
                        "AxGF", "AxGA",
                        "OxGF", "OxGA",
                        "OMxGF", "OMxGA",
                        "OAxGF", "OAxGA")
    }
    
    if ("Extras" %in% input$columns) {
      columnvector <- c(columnvector,
                        "OZS", "DZS", "NZS",
                        "OZF", "DZF", "NZF",
                        "iRB", "iRS")
    }
    
    if ("Scoring" %in% input$columns) {
      columnvector <- c(columnvector,
                        "G", "A1", "A2")
    }
    
    if ("Individual" %in% input$columns) {
      columnvector <- c(columnvector,
                        "iCF", "iFF", "iSF", "ixG",
                        "iFOW", "iFOL", "iPENT", "iPEND",
                        "iHF", "iHA",
                        "iGVA", "iTKA", "iBLK")
    }
    
    paste("SELECT ",
          paste(unique(columnvector), collapse = ","),
          " FROM playergame WHERE Date >= '", 
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
      
      sumdata <- group_by(data, Player, Team, Position) %>% mutate(GP = 1) %>% summarise_each(funs(sum), -c(Player, Date, Team, Position)) %>% 
        mutate(Date = paste(format(input$date[1], format = "%b'%y"), format(input$date[2], format = "%b'%y"), sep = "-"),
               GP = gpref$GP[match(paste(Player, Team, sep = "."), gpref$code)]) %>% data.frame()
    } else {
      sumdata <- group_by(data, Player, Team, Position, Date) %>% summarise_each(funs(sum), -c(Player, Team, Date, Position)) %>% arrange(desc(Date)) %>% data.frame()
    }
    
    if ("Corsi" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(CF, CA, MCF, MCA, OCF, OCA, OMCF, OMCA)) %>% rename(CF = ACF, CA = ACA, OCF = OACF, OCA = OACA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(CF, CA, ACF, ACA, OCF, OCA, OACF, OACA)) %>% rename(CF = MCF, CA = MCA, OCF = OMCF, OCA = OMCA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(ACF, ACA, MCF, MCA, OACF, OACA, OMCF, OMCA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.corsi() %>% data.frame()
    }
    
    if ("Fenwick" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(FF, FA, MFF, MFA, OFF, OFA, OMFF, OMFA)) %>% rename(FF = AFF, FA = AFA, OFF = OAFF, OFA = OAFA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(FF, FA, AFF, AFA, OFF, OFA, OAFF, OAFA)) %>% rename(FF = MFF, FA = MFA, OFF = OMFF, OFA = OMFA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(AFF, AFA, MFF, MFA, OAFF, OAFA, OMFF, OMFA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.fenwick() %>% data.frame()
    }
    
    if ("Shots on goal" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(SF, SA, MSF, MSA, OSF, OSA, OMSF, OMSA)) %>% rename(SF = ASF, SA = ASA, OSF = OASF, OSA = OASA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(SF, SA, ASF, ASA, OSF, OSA, OASF, OASA)) %>% rename(SF = MSF, SA = MSA, OSF = OMSF, OSA = OMSA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(ASF, ASA, MSF, MSA, OASF, OASA, OMSF, OMSA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.shots() %>% data.frame()
    }
    
    if ("Goals" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(GF, GA, MGF, MGA, OGF, OGA, OMGF, OMGA)) %>% rename(GF = AGF, GA = AGA, OGF = OAGF, OGA = OAGA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(GF, GA, AGF, AGA, OGF, OGA, OAGF, OAGA)) %>% rename(GF = MGF, GA = MGA, OGF = OMGF, OGA = OMGA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(AGF, AGA, MGF, MGA, OAGF, OAGA, OMGF, OMGA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.goals() %>% data.frame()
    }
    
    if ("Expected Goals" %in% input$columns) {
      
      # Adjust
      if (input$qadjust == "Score, Zone and Venue") {
        sumdata <- select(sumdata, -c(xGF, xGA, MxGF, MxGA, OxGF, OxGA, OMxGF, OMxGA)) %>% rename(xGF = AxGF, xGA = AxGA, OxGF = OAxGF, OxGA = OAxGA) %>% data.frame()
      } else if (input$qadjust == "Score and Venue") {
        sumdata <- select(sumdata, -c(xGF, xGA, AxGF, AxGA, OxGF, OxGA, OAxGF, OAxGA)) %>% rename(xGF = MxGF, xGA = MxGA, OxGF = OMxGF, OxGA = OMxGA) %>% data.frame()
      } else {
        sumdata <- select(sumdata, -c(AxGF, AxGA, MxGF, MxGA, OAxGF, OAxGA, OMxGF, OMxGA)) %>% data.frame()
      }
      
      sumdata <- sumdata %>% mutate.xG() %>% data.frame()
    }
    
    if ("Extras" %in% input$columns) {
      sumdata <- sumdata %>% mutate.extras() %>% data.frame()
    }
    
    if ("Scoring" %in% input$columns) {
      sumdata <- sumdata %>% mutate.scoring() %>% data.frame()
    }
    
    if ("Individual" %in% input$columns) {
      sumdata <- sumdata %>% mutate.individual() %>% data.frame()
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
    
    if ("iSF" %in% colnames(sumdata) & "G" %in% colnames(sumdata)) {
      sumdata <- sumdata %>% mutate(iSh. = G/iSF*100) %>% data.frame()
    }
    
    if ("iCF" %in% colnames(sumdata) & "G" %in% colnames(sumdata)) {
      sumdata <- sumdata %>% mutate(iCSh. = G/iCF*100) %>% data.frame()
    }
    
    if ("iFF" %in% colnames(sumdata) & "G" %in% colnames(sumdata)) {
      sumdata <- sumdata %>% mutate(iFSh. = G/iFF*100) %>% data.frame()
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
        mutate_each(funs(form), -c(Player, Team, Position, Date))
    } else {
      data <- data %>%
        mutate_each(funs(form), -c(Player, Team, Position, Date))
    }
    
    if ("GP" %in% colnames(data)) {
      prime <- na.omit(match(c("Player", "Date", "Team", "Position", "GP"), colnames(data)))
    } else {
      prime <- na.omit(match(c("Player", "Date", "Team", "Position"), colnames(data)))
    }
    
    nonprime <- which(colnames(data) %in% c("Player", "Team", "Position", "Date", "GP") == FALSE)
    
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
      formatStyle('Player', fontWeight = "bold")
    
  })
  
  # Output file
  output$qdl <- downloadHandler(
    filename = paste("Corsica_Skater.Stats_", 
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
    
    paste("SELECT * FROM playergame WHERE Date >= '", 
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
                                                      G = sum(G), A1 = sum(A1), A2 = sum(A2),
                                                      iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                                                      CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                                                      xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                                                      AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                                                      MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                                                      MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                                                      OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                                                      OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                                                      OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                                                      OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                                                      OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
                                                      DZF = sum(DZF), NZF = sum(NZF)) %>% 
      group_by(Player) %>% mutate(gamenum = cumsum(index)) %>% arrange(Date) %>% data.frame()
    
    # Adjust
    if (input$s1adjust == "Score, Zone and Venue") {
      pre <- select(pre, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA,
                            OCF, OCA, OFF, OFA, OSF, OSA, OGF, OGA, OxGF, OxGA, OMCF, OMCA, OMFF, OMFA, OMSF, OMSA, OMGF, OMGA, OMxGF, OMxGA)) %>% 
        rename(CF = ACF, CA = ACA, FF = AFF, FA = AFA, SF = ASF, SA = ASA, GF = AGF, GA = AGA, xGF = AxGF, xGA = AxGA,
               OCF = OACF, OCA = OACA, OFF = OAFF, OFA = OAFA, OSF = OASF, OSA = OASA, OGF = OAGF, OGA = OAGA, OxGF = OAxGF, OxGA = OAxGA) %>% data.frame()
    } else if (input$s1adjust == "Score and Venue") {
      pre <- select(pre, -c(CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA,
                            OCF, OCA, OFF, OFA, OSF, OSA, OGF, OGA, OxGF, OxGA, OACF, OACA, OAFF, OAFA, OASF, OASA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        rename(CF = MCF, CA = MCA, FF = MFF, FA = MFA, SF = MSF, SA = MSA, GF = MGF, GA = MGA, xGF = MxGF, xGA = MxGA,
               OCF = OMCF, OCA = OMCA, OFF = OMFF, OFA = OMFA, OSF = OMSF, OSA = OMSA, OGF = OMGF, OGA = OMGA, OxGF = OMxGF, OxGA = OMxGA) %>% data.frame()
    } else {
      pre <- select(pre, -c(MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA,
                            OMCF, OMCA, OMFF, OMFA, OMSF, OMSA, OMGF, OMGA, OMxGF, OMxGA, OACF, OACA, OAFF, OAFA, OASF, OASA, OAGF, OAGA, OAxGF, OAxGA)) %>% 
        data.frame()
    }
    
    avg <- group_by(pre, Player, Date) %>% mutate(Season = first(Season), gamenum = first(gamenum),
                                                  TOI.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$TOI),
                                                  CF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$CF),
                                                  CA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$CA),
                                                  FF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$FF),
                                                  FA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$FA),
                                                  SF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$SF),
                                                  SA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$SA),
                                                  GF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$GF),
                                                  GA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$GA),
                                                  xGF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$xGF),
                                                  xGA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$xGA),
                                                  OCF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OCF),
                                                  OCA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OCA),
                                                  OFF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OFF),
                                                  OFA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OFA),
                                                  OSF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OSF),
                                                  OSA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OSA),
                                                  OGF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OGF),
                                                  OGA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OGA),
                                                  OxGF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OxGF),
                                                  OxGA.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$OxGA),
                                                  G.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$G),
                                                  A1.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$A1),
                                                  A2.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$A2),
                                                  iCF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$iCF),
                                                  iFF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$iFF),
                                                  iSF.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$iSF),
                                                  ixG.sum = sum((pre$gamenum >= first(gamenum) - input$n & pre$gamenum <= first(gamenum) & pre$Player == first(Player))*pre$ixG),
                                                  CF. = (gamenum >= input$n)*(CF.sum/(CF.sum + CA.sum)*100),
                                                  CF60 = (gamenum >= input$n)*(CF.sum/TOI.sum*60),
                                                  CA60 = (gamenum >= input$n)*(CA.sum/TOI.sum*60),
                                                  OCF. = (gamenum >= input$n)*(OCF.sum/(OCF.sum + OCA.sum)*100),
                                                  Rel.CF. = (gamenum >= input$n)*(CF. - OCF.),
                                                  iCF60 = (gamenum >= input$n)*(iCF.sum/TOI.sum*60),
                                                  FF. = (gamenum >= input$n)*(FF.sum/(FF.sum + FA.sum)*100),
                                                  FF60 = (gamenum >= input$n)*(FF.sum/TOI.sum*60),
                                                  FA60 = (gamenum >= input$n)*(FA.sum/TOI.sum*60),
                                                  OFF. = (gamenum >= input$n)*(OFF.sum/(OFF.sum + OFA.sum)*100),
                                                  Rel.FF. = (gamenum >= input$n)*(FF. - OFF.),
                                                  iFF60 = (gamenum >= input$n)*(iFF.sum/TOI.sum*60),
                                                  SF. = (gamenum >= input$n)*(SF.sum/(SF.sum + SA.sum)*100),
                                                  SF60 = (gamenum >= input$n)*(SF.sum/TOI.sum*60),
                                                  SA60 = (gamenum >= input$n)*(SA.sum/TOI.sum*60),
                                                  OSF. = (gamenum >= input$n)*(OSF.sum/(OSF.sum + OSA.sum)*100),
                                                  Rel.SF. = (gamenum >= input$n)*(SF. - OSF.),
                                                  iSF60 = (gamenum >= input$n)*(iSF.sum/TOI.sum*60),
                                                  GF. = (gamenum >= input$n)*(GF.sum/(GF.sum + GA.sum)*100),
                                                  GF60 = (gamenum >= input$n)*(GF.sum/TOI.sum*60),
                                                  GA60 = (gamenum >= input$n)*(GA.sum/TOI.sum*60),
                                                  xGF. = (gamenum >= input$n)*(xGF.sum/(xGF.sum + xGA.sum)*100),
                                                  xGF60 = (gamenum >= input$n)*(xGF.sum/TOI.sum*60),
                                                  xGA60 = (gamenum >= input$n)*(xGA.sum/TOI.sum*60),
                                                  OxGF. = (gamenum >= input$n)*(OxGF.sum/(OxGF.sum + OxGA.sum)*100),
                                                  Rel.xGF. = (gamenum >= input$n)*(xGF. - OxGF.),
                                                  ixG60 = (gamenum >= input$n)*(ixG.sum/TOI.sum*60),
                                                  Sh. = (gamenum >= input$n)*(GF.sum/SF.sum*100),
                                                  Sv. = (gamenum >= input$n)*((1 - GA.sum/SA.sum)*100),
                                                  PDO = (gamenum >= input$n)*(Sv. + Sh.),
                                                  G60 = (gamenum >= input$n)*(G.sum/TOI.sum*60),
                                                  A60 = (gamenum >= input$n)*((A1.sum + A2.sum)/TOI.sum*60),
                                                  P60 = (gamenum >= input$n)*((G.sum + A1.sum + A2.sum)/TOI.sum*60),
                                                  P160 = (gamenum >= input$n)*((G.sum + A1.sum)/TOI.sum*60),
                                                  iSh. = (gamenum >= input$n)*(G.sum/iSF.sum*100),
                                                  ixFSh. = (gamenum >= input$n)*(ixG.sum/iFF.sum*100)) %>% data.frame()
    
    colnames(avg) <- gsub("[.]$", "%", colnames(avg))
    
    avg
    
  })
  
  # Plot output
  output$s1plot <- renderPlot({
    
    require(ggplot2)
    
    data <- s1.contents()
    
    col.index <- which(colnames(data) == input$s1measure)
    
    series <- filter(data, gamenum >= input$n) %>% select(c(Player, Season, Date, gamenum, col.index))
    
    colnames(series)[5] <- "measure"
    
    if (input$s1measure %in% c("CF%", "FF%", "SF%", "GF%", "xGF%")) {
      mid <- 50
    } else if (input$s1measure == "PDO") {
      mid <- 100
    } else if (input$s1measure %in% c("Rel.CF%", "Rel.FF%", "Rel.SF%", "Rel.GF%", "Rel.xGF%")) {
      mid <- 0
    } else {
      mid <- -100
    }
    
    if (input$s1adjust == "None") {
      title.measure <- input$s1measure
    } else {
      title.measure <- paste(input$s1adjust, "Adjusted", input$s1measure)
    }
    
    p <- ggplot(series, aes(x = as.Date(Date), y = measure)) + 
      stat_smooth(aes(group = interaction(Season, Player), col = Player, fill = Player), se = TRUE, level = 0.95, span = 0.3, alpha = 0.2) +
      scale_color_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange")) +
      scale_fill_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange")) +
      geom_line(y = mid, col = "black", linetype = "dashed") +
      labs(
        title = paste(paste(input$s1name, collapse = ", "), " Rolling ", input$n, "-Game Average ", title.measure, sep = ""), 
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
  
  ### TAB: USAGE CHARTS
  
  # Season inputs
  output$u1 <- renderUI(selectInput("u1", "From", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  output$u2 <- renderUI(selectInput("u2", "To", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  
  # Team input
  output$uteam <- renderUI({
    if(length(input$uname) < 1) {selectInput("uteam", "Team", choices = sort(unique(substr(as.character(teams), start = 1, stop = 3))), selected = "ANA")}
  })
  
  # Player input
  output$uname <- renderUI(selectizeInput("uname", "Custom List", choices = unique(as.character(names)), selected = NULL, multiple = TRUE))
  
  # Construct query
  u.query <- reactive({
    
    # Players input
    if (length(input$uname) < 1) {
      playervector <- NULL
    } else {
      playervector <- gsub("'", "''", input$uname)
    }
    
    # Season input
    seasonvector <- as.character(seq(from = as.numeric(input$u1), to = as.numeric(input$u2), by = 10001))
    
    # Type input
    if (input$utype == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$utype
    }
    
    # Strength input
    if (input$ustrength == "All") {
      strengthvector <- c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "XvX")
    } else {
      strengthvector <- input$ustrength
    }
    
    # Venue input
    if (input$uvenue == "Any") {
      venuevector <- c("Home", "Away")
    } else {
      venuevector <- input$uvenue
    }
    
    if(length(input$uname) < 1) {
      
      paste("SELECT * FROM playerseason WHERE Season IN (",
            paste(seasonvector, collapse = ","),
            ") AND [Season.Type] IN ('",
            paste(typevector, collapse = "','"),
            "') AND ((Venue == 'Home' AND [Strength.State] IN ('",
            paste(strengthvector, collapse = "','"),
            "')) OR (Venue == 'Away' AND [Strength.State] IN ('",
            paste(str_rev(strengthvector), collapse = "','"),
            "'))) AND Venue IN ('",
            paste(venuevector, collapse = "','"),
            "') AND Team == '",
            input$uteam,
            "'",
            sep = "")
      
    } else {
      
      paste("SELECT * FROM playerseason WHERE Season IN (",
            paste(seasonvector, collapse = ","),
            ") AND [Season.Type] IN ('",
            paste(typevector, collapse = "','"),
            "') AND ((Venue == 'Home' AND [Strength.State] IN ('",
            paste(strengthvector, collapse = "','"),
            "')) OR (Venue == 'Away' AND [Strength.State] IN ('",
            paste(str_rev(strengthvector), collapse = "','"),
            "'))) AND Venue IN ('",
            paste(venuevector, collapse = "','"),
            "') AND Player IN ('",
            paste(playervector, collapse = "','"),
            "')",
            sep = "")
      
    }
    
  })
  
  u.data <- reactive({
    
    query <- u.query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  u.sum <- reactive({
    
    data <- u.data()
    
    sum <- group_by(data, Player) %>% 
      summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                TOI = sum(TOI), tTOI = sum(tTOI),
                CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), GF = sum(GF), GA = sum(GA),
                xGF = sum(xGF), xGA = sum(xGA),
                OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA),
                OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA),
                OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS),
                OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
                S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>%
      mutate(OZS. = OZS/(OZS + DZS + NZS)*100, DZS. = DZS/(OZS + DZS + NZS)*100, NZS. = NZS/(OZS + DZS + NZS)*100,
             OZF. = OZF/(OZF + DZF + NZF)*100, DZF. = DZF/(OZF + DZF + NZF)*100, NZF. = NZF/(OZF + DZF + NZF)*100,
             ZSR = OZS/(OZS + DZS)*100, ZFR = OZF/(OZF + DZF)*100,
             OZSR = OOZS/(OOZS + ODZS)*100, Rel.ZSR = ZSR - OZSR, 
             TOI. = TOI/tTOI*100,
             CF. = CF/(CF + CA)*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
             FF. = FF/(FF + FA)*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
             GF. = GF/(GF + GA)*100, OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
             xGF. = xGF/(xGF + xGA)*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
             TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
             TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>%
      data.frame()
    
    sum
    
  })
  
  # Plot contents
  u.contents <- reactive({
    
    data <- u.sum()
    
    colnames(data) <- gsub("[.]$", "%", colnames(data))
    
    colvector <- which(colnames(data) %in% c(input$xaxis, input$yaxis, input$colour, input$size))
    
    filter(data, TOI > input$utoi) %>% select(c(Player, Season, colvector))
    
  })
  
  # Plot output
  output$uplot <- renderPlot({
    
    require(ggplot2)
    
    data <- u.contents()
    
    colnames(data)[which(colnames(data) == input$xaxis)] <- "xaxis"
    colnames(data)[which(colnames(data) == input$yaxis)] <- "yaxis"
    colnames(data)[which(colnames(data) == input$colour)] <- "colour"
    colnames(data)[which(colnames(data) == input$size)] <- "size"
    
    if(length(input$uname) < 1) {
      titletext <- input$uteam
    } else {
      titletext <- "(Custom Player List)"
    }
    
    p <- ggplot(data, aes(x = xaxis, y = yaxis, label = Player)) + 
      geom_point(aes(color = colour, size = size), alpha = 0.8) +
      scale_size(range = c(10, 20), guide = "none") +
      scale_color_gradient2(low = "red1", high = "dodgerblue", mid = "white", midpoint = mean(data$colour)) +
      geom_text(size = 3) +
      labs(
        title = paste(titletext, first(data$Season), "Usage Chart"), 
        x = input$xaxis, 
        y = input$yaxis,
        colour = input$colour
      ) +
      theme(
        panel.background = element_rect(fill = "#EFEFEF"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "#4863A0", size = 2),
        plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "in")
      )
    
    print(p)
    
  })
  
  ### TAB: PLAYER VS PEERS
  
  # Season inputs
  output$p1 <- renderUI(selectInput("p1", "From", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  output$p2 <- renderUI(selectInput("p2", "To", choices = sort(unique(seasons), decreasing = TRUE), selected = as.character(max(as.numeric(seasons)))))
  
  # Player input
  output$pname <- renderUI(selectizeInput("pname", "Search Player", choices = unique(as.character(names)), selected = NULL, multiple = TRUE, options = list(maxItems = 1)))
  
  # Player query
  p1.query <- reactive({
    
    # Players input
    if (length(input$pname) < 1) {
      playervector <- NULL
    } else {
      playervector <- gsub("'", "''", input$pname)
    }
    
    # Season input
    seasonvector <- as.character(seq(from = as.numeric(input$p1), to = as.numeric(input$p2), by = 10001))
    
    # Type input
    if (input$ptype == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$ptype
    }
    
    paste("SELECT * FROM playerseason WHERE Season IN ('", 
          paste(seasonvector, collapse = "','"),
          "') AND [Season.Type] IN ('",
          paste(typevector, collapse = "','"),
          "') AND Player IN ('",
          paste(playervector, collapse = "','"),
          "') AND [Strength.State] == '5v5'",
          sep = "")
    
  })
  
  # Player data
  p1.data <- reactive({
    
    query <- p1.query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  # Summarise player data
  p1.sum <- reactive({
    
    data <- p1.data()
    
    if(input$paggregate == TRUE) {
      sum <- group_by(data, Player) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  TOI = sum(TOI), Position = first(Position),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
                  iRB = sum(iRB), iRS = sum(iRS),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>%
        data.frame()
    } else {
      sum <- group_by(data, Player, Season) %>% 
        summarise(Position = first(Position), 
                  TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA),
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
                  iRB = sum(iRB), iRS = sum(iRS),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>%
        data.frame()
    }
    
    # Adjust
    if (input$padjust == "Score, Zone and Venue") {
      sum <- mutate(sum, CF60 = as.numeric(format(round(ACF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(ACA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*ACF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.ACF., 2), nsmall = 2)),
                    FF60 = as.numeric(format(round(AFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(AFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*AFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.AFF., 2), nsmall = 2)),
                    SF60 = as.numeric(format(round(ASF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(ASA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*ASF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.ASF., 2), nsmall = 2)),
                    GF60 = as.numeric(format(round(AGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(AGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*AGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.AGF., 2), nsmall = 2)),
                    xGF60 = as.numeric(format(round(AxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(AxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*AxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.AxGF., 2), nsmall = 2)),
                    Rel.CF60 = as.numeric(format(round(Rel.ACF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.ACA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.AFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.AFA60, 2), nsmall = 2)),
                    Rel.SF60 = as.numeric(format(round(Rel.ASF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.ASA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.AGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.AGA60, 2), nsmall = 2)), 
                    Rel.xGF60 = as.numeric(format(round(Rel.AxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.AxGA60, 2), nsmall = 2)))
    } else if (input$padjust == "Score and Venue") {
      sum <- mutate(sum, CF60 = as.numeric(format(round(MCF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(MCA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*MCF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.MCF., 2), nsmall = 2)),
                    FF60 = as.numeric(format(round(MFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(MFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*MFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.MFF., 2), nsmall = 2)),
                    SF60 = as.numeric(format(round(MSF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(MSA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*MSF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.MSF., 2), nsmall = 2)),
                    GF60 = as.numeric(format(round(MGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(MGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*MGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.MGF., 2), nsmall = 2)),
                    xGF60 = as.numeric(format(round(MxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(MxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*MxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.MxGF., 2), nsmall = 2)),
                    Rel.CF60 = as.numeric(format(round(Rel.MCF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.MCA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.MFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.MFA60, 2), nsmall = 2)),
                    Rel.SF60 = as.numeric(format(round(Rel.MSF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.MSA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.MGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.MGA60, 2), nsmall = 2)), 
                    Rel.xGF60 = as.numeric(format(round(Rel.MxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.MxGA60, 2), nsmall = 2)))
    }
    
    sum
    
  })
  
  # Player position
  p.pos <- reactive({
    
    data <- p1.sum()
    
    posvector <- paste(unique(data$Position), collapse = "/")
    
    if(grepl("C|L|R", posvector) == TRUE & grepl("D", posvector) == FALSE) {
      pos <- "F"
    } else if(grepl("C|L|R", posvector) == FALSE & grepl("D", posvector) == TRUE) {
      pos <- "D"
    } else {
      pos <- "Any"
    }
    
    pos
    
  })
  
  # Peers query
  p2.query <- reactive({
    
    # Season input
    seasonvector <- as.character(seq(from = as.numeric(input$p1), to = as.numeric(input$p2), by = 10001))
    
    # Type input
    if (input$ptype == "Both") {
      typevector <- c("Regular", "Playoffs")
    } else {
      typevector <- input$ptype
    }
    
    # Position input
    if (input$ppos == "Any") {
      posvector <- c("R", "C", "L", "D")
    } else if (input$ppos == "Forward") {
      posvector <- c("R", "C", "L")
    } else if (input$ppos == "Defence") {
      posvector <- "D"
    }
    
    paste("SELECT * FROM playerseason WHERE Season IN ('", 
          paste(seasonvector, collapse = "','"),
          "') AND [Season.Type] IN ('",
          paste(typevector, collapse = "','"),
          "') AND Position IN ('",
          paste(posvector, collapse = "','"),
          "') AND [Strength.State] == '5v5'",
          sep = "")
    
  })
  
  # Player data
  p2.data <- reactive({
    
    query <- p2.query()
    
    # Link to database
    link <- "/srv/shiny-server/fenwicka.sqlite"
    con <- dbConnect(SQLite(), link)
    
    # Query database
    db.query <- dbSendQuery(con, query)
    data <- fetch(db.query, -1)
    
    dbDisconnect(con)
    
    data
    
  })
  
  # Summarise player data
  p2.sum <- reactive({
    
    data <- p2.data()
    
    if(input$pradio == "Career") {
      sum <- group_by(data, Player) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  TOI = sum(TOI), Position = first(Position),
                  CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
                  iRB = sum(iRB), iRS = sum(iRS),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>%
        data.frame()
    } else {
      sum <- group_by(data, Player, Season) %>% 
        summarise(Position = first(Position), 
                  TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA),
                  MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                  MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
                  OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
                  PENT = sum(PENT), PEND = sum(PEND), iDIST = sum(iDIST),
                  iCF = sum(iCF), iFF = sum(iFF), iSF = sum(iSF), ixG = sum(ixG),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA), OFF = sum(OFF), OFA = sum(OFA), OSF = sum(OSF), OSA = sum(OSA), 
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA), OAFF = sum(OAFF), OAFA = sum(OAFA), 
                  OASF = sum(OASF), OASA = sum(OASA), OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMFF = sum(OMFF), OMFA = sum(OMFA), OMSF = sum(OMSF), OMSA = sum(OMSA), OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  OOZS = sum(OOZS), ODZS = sum(ODZS), ONZS = sum(ONZS),
                  RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
                  iRB = sum(iRB), iRS = sum(iRS),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>% 
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100, CSh. = GF/CF*100, CSv. = (1 - (GA/CA))*100,
               iCF60 = iCF/TOI*60, iCSh. = G/iCF*100, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               OCSh. = OGF/OCF*100, OCSv. = (1 - (OGA/OCA))*100, Rel.CSh. = CSh. - OCSh., Rel.CSv. = CSv. - OCSv., 
               FF60 = FF/TOI*60, FA60 = FA/TOI*60, FF. = FF/(FF + FA)*100, FSh. = GF/FF*100, FSv. = (1 - (GA/FA))*100,
               iFF60 = iFF/TOI*60, iFSh. = G/iFF*100, OFF. = OFF/(OFF + OFA)*100, Rel.FF. = FF. - OFF.,
               OFF60 = OFF/OTOI*60, OFA60 = OFA/OTOI*60, Rel.FF60 = FF60 - OFF60, Rel.FA60 = FA60 - OFA60,
               OFSh. = OGF/OFF*100, OFSv. = (1 - (OGA/OFA))*100, Rel.FSh. = FSh. - OFSh., Rel.FSv. = FSv. - OFSv.,
               SF60 = SF/TOI*60, SA60 = SA/TOI*60, SF. = SF/(SF + SA)*100, Sh. = GF/SF*100, Sv. = (1 - (GA/SA))*100,
               iSF60 = iSF/TOI*60, iSh. = G/iSF*100, OSF. = OSF/(OSF + OSA)*100, Rel.SF. = SF. - OSF.,
               OSF60 = OSF/OTOI*60, OSA60 = OSA/OTOI*60, Rel.SF60 = SF60 - OSF60, Rel.SA60 = SA60 - OSA60,
               OSh. = OGF/OSF*100, OSv. = (1 - (OGA/OSA))*100, Rel.Sh. = Sh. - OSh., Rel.Sv. = Sv. - OSv.,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100, xFSh. = xGF/FF*100, xFSv. = (1 - (xGA/FA))*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100, Adj.FSv. = FSv. - xFSv.,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
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
               FO. = FOW/(FOW + FOL)*100, PENDIFF = PEND - PENT, Avg.DIST = iDIST/iFF,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPENDIFF = iPEND - iPENT, iFO. = iFOW/(iFOW + iFOL)*100,
               PDO = Sv. + Sh., xPDO = xFSv. + xFSh.,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>%
        data.frame()
    }
    
    # Adjust
    if (input$padjust == "Score, Zone and Venue") {
      sum <- mutate(sum, CF60 = as.numeric(format(round(ACF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(ACA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*ACF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.ACF., 2), nsmall = 2)),
                    FF60 = as.numeric(format(round(AFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(AFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*AFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.AFF., 2), nsmall = 2)),
                    SF60 = as.numeric(format(round(ASF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(ASA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*ASF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.ASF., 2), nsmall = 2)),
                    GF60 = as.numeric(format(round(AGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(AGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*AGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.AGF., 2), nsmall = 2)),
                    xGF60 = as.numeric(format(round(AxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(AxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*AxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.AxGF., 2), nsmall = 2)),
                    Rel.CF60 = as.numeric(format(round(Rel.ACF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.ACA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.AFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.AFA60, 2), nsmall = 2)),
                    Rel.SF60 = as.numeric(format(round(Rel.ASF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.ASA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.AGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.AGA60, 2), nsmall = 2)), 
                    Rel.xGF60 = as.numeric(format(round(Rel.AxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.AxGA60, 2), nsmall = 2)))
    } else if (input$padjust == "Score and Venue") {
      sum <- mutate(sum, CF60 = as.numeric(format(round(MCF60, 2), nsmall = 2)), CA60 = as.numeric(format(round(MCA60, 2), nsmall = 2)), CF. = as.numeric(format(round(100*MCF., 2), nsmall = 2)), Rel.CF. = as.numeric(format(round(100*Rel.MCF., 2), nsmall = 2)),
                    FF60 = as.numeric(format(round(MFF60, 2), nsmall = 2)), FA60 = as.numeric(format(round(MFA60, 2), nsmall = 2)), FF. = as.numeric(format(round(100*MFF., 2), nsmall = 2)), Rel.FF. = as.numeric(format(round(100*Rel.MFF., 2), nsmall = 2)),
                    SF60 = as.numeric(format(round(MSF60, 2), nsmall = 2)), SA60 = as.numeric(format(round(MSA60, 2), nsmall = 2)), SF. = as.numeric(format(round(100*MSF., 2), nsmall = 2)), Rel.SF. = as.numeric(format(round(100*Rel.MSF., 2), nsmall = 2)),
                    GF60 = as.numeric(format(round(MGF60, 2), nsmall = 2)), GA60 = as.numeric(format(round(MGA60, 2), nsmall = 2)), GF. = as.numeric(format(round(100*MGF., 2), nsmall = 2)), Rel.GF. = as.numeric(format(round(100*Rel.MGF., 2), nsmall = 2)),
                    xGF60 = as.numeric(format(round(MxGF60, 2), nsmall = 2)), xGA60 = as.numeric(format(round(MxGA60, 2), nsmall = 2)), xGF. = as.numeric(format(round(100*MxGF., 2), nsmall = 2)), Rel.xGF. = as.numeric(format(round(100*Rel.MxGF., 2), nsmall = 2)),
                    Rel.CF60 = as.numeric(format(round(Rel.MCF60, 2), nsmall = 2)), Rel.CA60 = as.numeric(format(round(Rel.MCA60, 2), nsmall = 2)), Rel.FF60 = as.numeric(format(round(Rel.MFF60, 2), nsmall = 2)), Rel.FA60 = as.numeric(format(round(Rel.MFA60, 2), nsmall = 2)),
                    Rel.SF60 = as.numeric(format(round(Rel.MSF60, 2), nsmall = 2)), Rel.SA60 = as.numeric(format(round(Rel.MSA60, 2), nsmall = 2)), Rel.GF60 = as.numeric(format(round(Rel.MGF60, 2), nsmall = 2)), Rel.GA60 = as.numeric(format(round(Rel.MGA60, 2), nsmall = 2)), 
                    Rel.xGF60 = as.numeric(format(round(Rel.MxGF60, 2), nsmall = 2)), Rel.xGA60 = as.numeric(format(round(Rel.MxGA60, 2), nsmall = 2)))
    }
    
    sum
    
  })
  
  # Filter by TOI
  p2.TOI <- reactive({
    
    data <- p2.sum()
    
    filter(data, TOI >= input$ptoi & Player != input$pname) %>% data.frame()
    
  })
  
  # Position input
  output$ppos <- renderUI({
    
    if(p.pos() == "F") {
      selectInput("ppos", "Position", choices = c("Forward", "Defence", "Any"), selected = "Forward")
    } else if(p.pos() == "D") {
      selectInput("ppos", "Position", choices = c("Forward", "Defence", "Any"), selected = "Defence")
    } else {
      selectInput("ppos", "Position", choices = c("Forward", "Defence", "Any"), selected = "Any")
    }
    
  })
  
  # Radio input
  output$radio <- renderUI({
    
    if(input$paggregate == TRUE) {
      radioButtons("pradio", "", choices = c("Seasons", "Career"), selected = "Career")
    } else {
      radioButtons("pradio", "", choices = c("Seasons", "Career"), selected = "Seasons")
    }
    
  })
  
  # Plot contents
  pvp.contents <- reactive({
    
    p1.data <- p1.sum()
    p2.data <- p2.TOI()
    
    newdata <- rbind_list(p1.data, p2.data) %>% data.frame()
    
    colnames(newdata) <- gsub("[.]$", "%", colnames(newdata))
    
    newdata
    
  })
  
  ## Distribution
  output$dist <- renderPlot({
    
    require(ggplot2)
    
    data <- pvp.contents()
    
    col.index <- which(colnames(data) == input$dist)
    
    series <- select(data, c(Player, Season, col.index))
    
    colnames(series)[3] <- "measure"
    
    if (input$padjust == "None") {
      title.measure <- input$dist
    } else {
      title.measure <- paste(input$padjust, "Adjusted", input$dist)
    }
    
    if (input$dist %in% c("GF60", "GA60", "xGF60", "xGA60", "Rel.GF60", "Rel.GA60", "Rel.xGF60", "Rel.xGA60", "Rel.xFSv%", "iCF60", "iFF60", "iSF60", "ixG60", "G60", "A60", "P60", "P160")) {
      digs <- 2
    } else {
      digs <- 1
    }
    
    pseason <- paste(substr(as.character(min(as.numeric(input$p1))), start = 1, stop = 4), substr(as.character(max(as.numeric(input$p2))), start = 5, stop = 8), sep = "-")
    pmat <- filter(series, Player == input$pname) %>% data.frame()
    
    dens <- density(series$measure)
    df.dens <- data.frame(x = dens$x, y = dens$y)
    df.dens$season <- pmat$Season[match(round(df.dens$x, digs), round(pmat$measure, digs))]
    
    p <- ggplot(series, aes(x = measure)) + 
      geom_density(fill = "dodgerblue", colour = "dodgerblue", alpha = 0.13) +
      xlim(min(series$measure), max(series$measure)) +
      geom_area(data = filter(df.dens, round(x, digs) %in% round(pmat$measure, digs)), aes(x = x, y = y, colour = season, fill = season, group = season), alpha = 0.5) +
      scale_color_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange", "cyan2", "chartreuse1", "sienna1", "deeppink", "tan")) +
      scale_fill_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange", "cyan2", "chartreuse1", "sienna1", "deeppink", "tan")) +
      labs(
        title = paste(input$pname, pseason, "vs. Distribution of", input$dist),
        x = title.measure
      ) +
      theme(
        panel.background = element_rect(fill = "#EFEFEF"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "#4863A0", size = 2),
        plot.margin = unit(c(0.2, 0.1, 0.2, 0.2), "in"),
        legend.title = element_blank(),
        title = element_text(size = 9)
      ) +
      guides(colour = FALSE)
    
    if(length(input$pname) > 0) {print(p)}
    
  })
  
  ## Radial
  output$rad <- renderPlot({
    
    data <- pvp.contents()
    
    col.index <- which(colnames(data) %in% input$rad)
    
    subdata <- select(data, c(Player, Season, col.index)) %>% 
      mutate_each(funs(percent_rank), -c(Player, Season)) %>% 
      filter(Player == input$pname) %>%
      select(-c(Player)) %>%
      data.frame()
    
    series <- cbind(
      rep(subdata$Season, times = length(col.index)),
      rep(colnames(subdata)[-1], each = length(subdata$Season)),
      round(as.numeric(as.character(unlist(subdata[, -1]))), 2)
    ) %>% data.frame()
    
    colnames(series) <- c("Season", "Measure", "Value")
    
    inverse <- c("CA60", "FA60", "SA60", "GA60", "xGA60", "Rel.CA60", "Rel.FA60", "Rel.SA60", "Rel.GA60", "Rel.xGA60")
    
    series$Value <- abs(2*(series$Measure %in% inverse) - (as.numeric(as.character(series$Value)) + 1*(series$Measure %in% inverse)))
    series$Measure <- gsub("[.]$", "%", series$Measure)
    
    mid <- mutate(series, Value = 0.5)
    vlines <- cbind(xx = c(1, length(series$Measure)), yy = c(0.5, 0.5)) %>% data.frame()
    
    pseason <- paste(substr(as.character(min(as.numeric(input$p1))), start = 1, stop = 4), substr(as.character(max(as.numeric(input$p2))), start = 5, stop = 8), sep = "-")
    
    if (input$padjust == "None") {
      title.measure <- input$rad
    } else {
      title.measure <- paste(input$padjust, "Adjusted", input$rad)
    }
    
    p <- ggplot(series, aes(y = as.numeric(as.character(Value)), x = Measure, group = Season)) + 
      coord_polar(start = pi) +
      geom_point(alpha = 0) +
      geom_ribbon(aes(ymin = 0, ymax = as.numeric(as.character(Value)), fill = Season), alpha = 0.25) +
      scale_fill_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange", "cyan2", "chartreuse1", "sienna1", "deeppink", "tan")) +
      geom_ribbon(data = mid, aes(ymin = (Value - 0.02), ymax = (Value + 0.02)), alpha = 0.15) +
      geom_vline(data = vlines, aes(xintercept = xx), linetype = "dashed") +
      labs(
        title = paste(input$pname, pseason, "Percentiles"),
        y = NULL,
        x = NULL
      ) +
      theme(
        panel.background = element_rect(fill = "#EFEFEF"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "#4863A0", size = 2),
        plot.margin = unit(c(0.2, 0.1, 0.1, 0.1), "in"),
        legend.title = element_blank(),
        title = element_text(size = 9)
      )
    
    if(length(input$pname) > 0) {print(p)}
    
  })
  
  ## Scatter
  output$scat <- renderPlot({
    
    data <- pvp.contents()
    
    colnames(data)[which(colnames(data) == input$scatx)] <- "xaxis"
    colnames(data)[which(colnames(data) == input$scaty)] <- "yaxis"
    
    series <- select(data, c(Player, Season, xaxis, yaxis)) %>% data.frame()
    pdata <- filter(series, Player == input$pname) %>% data.frame()
    
    x1 <- mean(series$xaxis) - sd(series$xaxis) # left
    x2 <- mean(series$xaxis) + sd(series$xaxis) # right
    y1 <- mean(series$yaxis) - sd(series$yaxis) # bottom
    y2 <- mean(series$yaxis) + sd(series$yaxis) # top
    
    stdev1 <- cbind(xx = c(x1, x1, x2, x2),
                    yy = c(y1, y2, y2, y1)) %>% 
      data.frame()
    
    stdev2 <- cbind(xx = c(x1, x2),
                    yy = c(y1, y1)) %>% 
      data.frame()
    
    pseason <- paste(substr(as.character(min(as.numeric(input$p1))), start = 1, stop = 4), substr(as.character(max(as.numeric(input$p2))), start = 5, stop = 8), sep = "-")
    
    p <- ggplot(series, aes(x = xaxis, y = yaxis)) + 
      geom_point(alpha = 0.15, size = 4, colour = "black", fill = "black") +
      geom_point(data = pdata, aes(x = xaxis, y = yaxis, colour = Season), size = 3, alpha = 0.9) +
      geom_line(data = stdev1, aes(x = xx, y = yy), linetype = "dashed", alpha = 0.9, colour = "dodgerblue") +
      geom_line(data = stdev2, aes(x = xx, y = yy), linetype = "dashed", alpha = 0.9, colour = "dodgerblue") +
      geom_point(data = data.frame(cbind(xx = mean(series$xaxis), yy = mean(series$yaxis))), aes(x = xx, y = yy), colour = "dodgerblue", shape = 3) +
      scale_colour_manual(values = c("dodgerblue", "limegreen", "red1", "darkorchid", "darkorange", "cyan2", "chartreuse1", "sienna1", "deeppink", "tan")) +
      labs(
        title = paste(input$pname, pseason, "vs. Scatter of", input$scatx, "and", input$scaty), 
        x = input$scatx, 
        y = input$scaty
      ) +
      theme(
        panel.background = element_rect(fill = "#EFEFEF"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(color = "#4863A0", size = 2),
        plot.margin = unit(c(0.6, 0.2, 0.2, 0.2), "in")
      )
    
    if(length(input$pname) > 0) {print(p)}
    
  })
  
  
})
