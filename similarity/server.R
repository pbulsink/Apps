# Server

# Corsica Similarity Calculator App
# Last edited 2-28-2016
# Manny

# Load libraries
require(shiny)
require(shinydashboard)
require(dplyr)
require(Kmisc)
require(DT)
require(RSQLite)

# Required columns
colvector <- c("Player", "Season", "Position", "TOI",
               "CF", "CA", "GF", "GA", "xGF", "xGA",
               "ACF", "ACA", "AGF", "AGA", "AxGF", "AxGA",
               "MCF", "MCA", "MGF", "MGA", "MxGF", "MxGA",
               "OCF", "OCA", "OGF", "OGA", "OxGF", "OxGA",
               "OACF", "OACA", "OAGF", "OAGA", "OAxGF", "OAxGA",
               "OMCF", "OMCA", "OMGF", "OMGA", "OMxGF", "OMxGA",
               "OZS", "DZS", "NZS",
               "iCF", "iFF", "ixG",
               "iFOW", "iFOL", "iPEND", "iPENT",
               "iHF", "iGVA", "iTKA", "iBLK",
               "G", "A1", "A2", "tTOI",
               "[S.TOIT]", "[S.TOI.T]", "[S.CF.T]", "[S.xGF.T]",
               "[S.TOIC]", "[S.TOI.C]", "[S.CF.C]", "[S.xGF.C]"
)

# Load data
link <- "/srv/shiny-server/fenwicka.sqlite"
con <- dbConnect(SQLite(), link)

query <- dbSendQuery(con, paste("SELECT ", paste(colvector, collapse = ", "), " FROM playerseason WHERE [Strength.State] == '5v5'", sep = ""))
data <- fetch(query, -1)

data$Position[which(grepl("C|R|L", data$Position) == TRUE)] <- "F"
data$Position[which(grepl("C|R|L|F", data$Position) == FALSE)] <- "D"

dbDisconnect(con)

shinyServer(function(input, output, session) {
  
  ### TAB: SIMILARITY
  
  # Season inputs
  output$s1 <- renderUI({
    
    if(input$aggregate == TRUE) {
      selectInput("s1", "From", choices = sort(unique(data$Season), decreasing = TRUE), selected = as.character(max(as.numeric(data$Season))))
    } else {
      selectInput("s1", "Season", choices = sort(unique(data$Season), decreasing = TRUE), selected = as.character(max(as.numeric(data$Season))))
    }
    
  })
  
  output$s2 <- renderUI({
    
    if(input$aggregate == TRUE) {
      selectInput("s2", "To", choices = sort(unique(data$Season), decreasing = TRUE), selected = as.character(max(as.numeric(data$Season))))
    }
    
  })
  
  # Player inputs
  output$name <- renderUI(selectizeInput("name", "Search Player", choices = unique(as.character(data$Player)), selected = NULL, multiple = TRUE, options = list(maxItems = 1)))
  output$name2 <- renderUI(selectizeInput("name2", "Search Comparables", choices = unique(as.character(data$Player)), selected = NULL, multiple = TRUE))
  
  # Player position
  p.pos <- reactive({
    
    # Season input
    if(length(input$s2) > 0) {
      seasonvector <- as.character(seq(from = as.numeric(input$s1), to = as.numeric(input$s2), by = 10001))
    } else {
      seasonvector <- input$s1
    }
    
    pos <- paste(unique(data$Position[which(data$Player == input$name & data$Season %in% seasonvector)], collapse = "|"))
    
    pos
    
  })
  
  # Sliders
  output$sliders <- renderUI({
    
    initial <- box(
      
      if("G60" %in% input$sliderselect) {sliderInput("g60", "G/60", min = 0, max = 1, value = 0.3, step = 0.1, ticks = FALSE)},
      if("A60" %in% input$sliderselect) {sliderInput("a60", "A/60", min = 0, max = 1, value = 0.3, step = 0.1, ticks = FALSE)},
      if("P60" %in% input$sliderselect) {sliderInput("p60", "P/60", min = 0, max = 1, value = 0.8, step = 0.1, ticks = FALSE)},
      if("CF60" %in% input$sliderselect) {sliderInput("cf60", "CF/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("CA60" %in% input$sliderselect) {sliderInput("ca60", "CA/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("CF%" %in% input$sliderselect) {sliderInput("cf", "CF%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("Rel.CF60" %in% input$sliderselect) {sliderInput("relcf60", "Rel CF/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("Rel.CA60" %in% input$sliderselect) {sliderInput("relca60", "Rel CA/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("Rel.CF%" %in% input$sliderselect) {sliderInput("relcf", "Rel CF%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("xGF60" %in% input$sliderselect) {sliderInput("xgf60", "xGF/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("xGA60" %in% input$sliderselect) {sliderInput("xga60", "xGA/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("xGF%" %in% input$sliderselect) {sliderInput("xgf", "xGF%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("Rel.xGF60" %in% input$sliderselect) {sliderInput("relxgf60", "Rel xGF/60", min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE)},
      if("Rel.xGA60" %in% input$sliderselect) {sliderInput("relxga60", "Rel xGA/60", min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE)},
      if("Rel.xGF%" %in% input$sliderselect) {sliderInput("relxgf", "Rel xGF%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("GF60" %in% input$sliderselect) {sliderInput("gf60", "GF/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("GA60" %in% input$sliderselect) {sliderInput("ga60", "GA/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("GF%" %in% input$sliderselect) {sliderInput("gf", "GF%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("Rel.GF60" %in% input$sliderselect) {sliderInput("relgf60", "Rel GF/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("Rel.GA60" %in% input$sliderselect) {sliderInput("relga60", "Rel GA/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("Rel.GF%" %in% input$sliderselect) {sliderInput("relgf", "Rel GF%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("iCF60" %in% input$sliderselect) {sliderInput("icf60", "iCF/60", min = 0, max = 1, value = 0.2, step = 0.1, ticks = FALSE)},
      if("ixG60" %in% input$sliderselect) {sliderInput("ixg60", "ixG/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("ixFSh%" %in% input$sliderselect) {sliderInput("ixfsh", "ixFSh%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("TOI%" %in% input$sliderselect) {sliderInput("toi.", "TOI%", min = 0, max = 1, value = 0.5, step = 0.1, ticks = FALSE)},
      if("TOI.QoT" %in% input$sliderselect) {sliderInput("toiqot", "TOI% QoT", min = 0, max = 1, value = 0.3, step = 0.1, ticks = FALSE)},
      if("CF.QoT" %in% input$sliderselect) {sliderInput("cfqot", "CF% QoT", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("xGF.QoT" %in% input$sliderselect) {sliderInput("xgfqot", "xGF% QoT", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("TOI.QoC" %in% input$sliderselect) {sliderInput("toiqoc", "TOI% QoC", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("CF.QoC" %in% input$sliderselect) {sliderInput("cfqoc", "CF% QoC", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("xGF.QoC" %in% input$sliderselect) {sliderInput("xgfqoc", "xGF% QoC", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("ZSR" %in% input$sliderselect) {sliderInput("zsr", "ZSR", min = 0, max = 1, value = 0.4, step = 0.1, ticks = FALSE)},
      if("iPEND60" %in% input$sliderselect) {sliderInput("ipend60", "PEND/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("iPENT60" %in% input$sliderselect) {sliderInput("ipent60", "PENT/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("iHF60" %in% input$sliderselect) {sliderInput("ihf60", "iHF/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("iTKA60" %in% input$sliderselect) {sliderInput("itka60", "iTKA/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("iGVA60" %in% input$sliderselect) {sliderInput("igva60", "iGVA/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("iBLK60" %in% input$sliderselect) {sliderInput("iblk60", "iBLK/60", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      if("iFO%" %in% input$sliderselect) {sliderInput("ifo", "FO%", min = 0, max = 1, value = 0, step = 0.1, ticks = FALSE)},
      
      width = 3,
      title = "Inputs",
      solidHeader = TRUE,
      collapsible = TRUE
      
    )
    
    initial
    
  })
  
  # Aggregate
  sumdata <- reactive({
    
    posvector <- p.pos()
    
    if(input$aggregate == TRUE) {
      
      sub <- filter(data, grepl(posvector, Position) == T) %>%
        group_by(Player) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MGF = sum(MGF), MGA = sum(MGA), 
                  MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS),
                  iCF = sum(iCF), ixG = sum(ixG), iFF = sum(iFF),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA),
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA),
                  OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>%
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100,
               iCF60 = iCF/TOI*60, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               ZSR = OZS/(OZS + DZS)*100,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPEND60 = iPEND/TOI*60, iPENT60 = iPENT/TOI*60, iFO. = iFOW/(iFOW + iFOL)*100,
               iHF60 = iHF/TOI*60, iGVA60 = iGVA/TOI*60, iTKA60 = iTKA/TOI*60, iBLK60 = iBLK/TOI*60,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>% 
        data.frame()
      
    } else {
      
      sub <- filter(data, grepl(posvector, Position) == T) %>%
        group_by(Player, Season) %>% 
        summarise(TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MGF = sum(MGF), MGA = sum(MGA), 
                  MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS),
                  iCF = sum(iCF), ixG = sum(ixG), iFF = sum(iFF),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA),
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA),
                  OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>%
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100,
               iCF60 = iCF/TOI*60, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               ZSR = OZS/(OZS + DZS)*100,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPEND60 = iPEND/TOI*60, iPENT60 = iPENT/TOI*60, iFO. = iFOW/(iFOW + iFOL)*100,
               iHF60 = iHF/TOI*60, iGVA60 = iGVA/TOI*60, iTKA60 = iTKA/TOI*60, iBLK60 = iBLK/TOI*60,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>% 
        data.frame()
      
    }
    
    sub
    
  })
  
  # Filter by TOI
  toi.filter <- reactive({
    
    data <- sumdata()
    newdata <- filter(data, TOI >= input$toi) %>% data.frame()
    
    newdata
    
  })
  
  p.data <- reactive({
    
    sumdata <- sumdata()
    
    if(input$aggregate == FALSE) {
      pdata <- filter(sumdata, Player == input$name & Season == input$s1) %>% data.frame()
    } else {
      
      seasonvector <- as.character(seq(from = as.numeric(input$s1), to = as.numeric(input$s2), by = 10001))
      
      pdata <- filter(data, Player == input$name & Season %in% seasonvector) %>%
        group_by(Player) %>% 
        summarise(Season = paste(substr(as.character(min(as.numeric(Season))), start = 1, stop = 4), substr(as.character(max(as.numeric(Season))), start = 5, stop = 8), sep = "-"),
                  TOI = sum(TOI),
                  CF = sum(CF), CA = sum(CA), GF = sum(GF), GA = sum(GA),
                  xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA),
                  AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), 
                  MCF = sum(MCF), MCA = sum(MCA), MGF = sum(MGF), MGA = sum(MGA), 
                  MxGF = sum(MxGF), MxGA = sum(MxGA),
                  OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS),
                  iCF = sum(iCF), ixG = sum(ixG), iFF = sum(iFF),
                  iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF),
                  iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK),
                  G = sum(G), A1 = sum(A1), A2 = sum(A2), iPENT = sum(iPENT), iPEND = sum(iPEND),
                  tTOI = sum(tTOI), OCF = sum(OCF), OCA = sum(OCA),
                  OGF = sum(OGF), OGA = sum(OGA), OxGF = sum(OxGF), OxGA = sum(OxGA), OACF = sum(OACF), OACA = sum(OACA),
                  OAGF = sum(OAGF), OAGA = sum(OAGA), OAxGF = sum(OAxGF), OAxGA = sum(OAxGA), OMCF = sum(OMCF), OMCA = sum(OMCA), 
                  OMGF = sum(OMGF), OMGA = sum(OMGA), OMxGF = sum(OMxGF), OMxGA = sum(OMxGA),
                  S.TOIT = sum(na.omit(S.TOIT)), S.TOI.T = sum(na.omit(S.TOI.T)), S.CF.T = sum(na.omit(S.CF.T)), S.xGF.T = sum(na.omit(S.xGF.T)),
                  S.TOIC = sum(na.omit(S.TOIC)), S.TOI.C = sum(na.omit(S.TOI.C)), S.CF.C = sum(na.omit(S.CF.C)), S.xGF.C = sum(na.omit(S.xGF.C))) %>%
        mutate(OTOI = tTOI - TOI, TOI. = TOI/tTOI*100,
               CF60 = CF/TOI*60, CA60 = CA/TOI*60, CF. = CF/(CF + CA)*100,
               iCF60 = iCF/TOI*60, OCF. = OCF/(OCF + OCA)*100, Rel.CF. = CF. - OCF.,
               OCF60 = OCF/OTOI*60, OCA60 = OCA/OTOI*60, Rel.CF60 = CF60 - OCF60, Rel.CA60 = CA60 - OCA60,
               GF60 = GF/TOI*60, GA60 = GA/TOI*60, GF. = GF/(GF + GA)*100,
               OGF. = OGF/(OGF + OGA)*100, Rel.GF. = GF. - OGF.,
               OGF60 = OGF/OTOI*60, OGA60 = OGA/OTOI*60, Rel.GF60 = GF60 - OGF60, Rel.GA60 = GA60 - OGA60,
               xGF60 = xGF/TOI*60, xGA60 = xGA/TOI*60, xGF. = xGF/(xGF + xGA)*100,
               ixG60 = ixG/TOI*60, ixFSh. = ixG/iFF*100, OxGF. = OxGF/(OxGF + OxGA)*100, Rel.xGF. = xGF. - OxGF.,
               OxGF60 = OxGF/OTOI*60, OxGA60 = OxGA/OTOI*60, Rel.xGF60 = xGF60 - OxGF60, Rel.xGA60 = xGA60 - OxGA60,
               ACF60 = ACF/TOI*60, ACA60 = ACA/TOI*60, ACF. = ACF/(ACF + ACA), 
               OACF. = OACF/(OACF + OACA), Rel.ACF. = ACF. - OACF.,
               OACF60 = OACF/OTOI*60, OACA60 = OACA/OTOI*60, Rel.ACF60 = ACF60 - OACF60, Rel.ACA60 = ACA60 - OACA60,
               AGF60 = AGF/TOI*60, AGA60 = AGA/TOI*60, AGF. = AGF/(AGF + AGA), 
               OAGF. = OAGF/(OAGF + OAGA), Rel.AGF. = AGF. - OAGF.,
               OAGF60 = OAGF/OTOI*60, OAGA60 = OAGA/OTOI*60, Rel.AGF60 = AGF60 - OAGF60, Rel.AGA60 = AGA60 - OAGA60,
               AxGF60 = AxGF/TOI*60, AxGA60 = AxGA/TOI*60, AxGF. = AxGF/(AxGF + AxGA),
               OAxGF. = OAxGF/(OAxGF + OAxGA), Rel.AxGF. = AxGF. - OAxGF.,
               OAxGF60 = OAxGF/OTOI*60, OAxGA60 = OAxGA/OTOI*60, Rel.AxGF60 = AxGF60 - OAxGF60, Rel.AxGA60 = AxGA60 - OAxGA60,
               MCF60 = MCF/TOI*60, MCA60 = MCA/TOI*60, MCF. = MCF/(MCF + MCA),
               OMCF. = OMCF/(OMCF + OMCA), Rel.MCF. = MCF. - OMCF.,
               OMCF60 = OMCF/OTOI*60, OMCA60 = OMCA/OTOI*60, Rel.MCF60 = MCF60 - OMCF60, Rel.MCA60 = MCA60 - OMCA60,
               MGF60 = MGF/TOI*60, MGA60 = MGA/TOI*60, MGF. = MGF/(MGF + MGA), 
               OMGF. = OMGF/(OMGF + OMGA), Rel.MGF. = MGF. - OMGF.,
               OMGF60 = OMGF/OTOI*60, OMGA60 = OMGA/OTOI*60, Rel.MGF60 = MGF60 - OMGF60, Rel.MGA60 = MGA60 - OMGA60,
               MxGF60 = MxGF/TOI*60, MxGA60 = MxGA/TOI*60, MxGF. = MxGF/(MxGF + MxGA),
               OMxGF. = OMxGF/(OMxGF + OMxGA), Rel.MxGF. = MxGF. - OMxGF.,
               OMxGF60 = OMxGF/OTOI*60, OMxGA60 = OMxGA/OTOI*60, Rel.MxGF60 = MxGF60 - OMxGF60, Rel.MxGA60 = MxGA60 - OMxGA60,
               ZSR = OZS/(OZS + DZS)*100,
               A = A1 + A2, P = G + A, P1 = G + A1, 
               G60 = G/TOI*60, A160 = A1/TOI*60, A260 = A2/TOI*60, A60 = A/TOI*60,
               P60 = P/TOI*60, P160 = P1/TOI*60, iPEND60 = iPEND/TOI*60, iPENT60 = iPENT/TOI*60, iFO. = iFOW/(iFOW + iFOL)*100,
               iHF60 = iHF/TOI*60, iGVA60 = iGVA/TOI*60, iTKA60 = iTKA/TOI*60, iBLK60 = iBLK/TOI*60,
               TOI.QoT = S.TOI.T/S.TOIT*100, CF.QoT = S.CF.T/S.TOIT*100, xGF.QoT = S.xGF.T/S.TOIT*100,
               TOI.QoC = S.TOI.C/S.TOIC*100, CF.QoC = S.CF.C/S.TOIC*100, xGF.QoC = S.xGF.C/S.TOIC*100) %>% 
        data.frame()
    }
    
    pdata
    
  })
  
  # Similarity calculation
  simdata <- reactive({
    
    data <- toi.filter()
    
    pdata <- p.data()
    
    if(input$aggregate == TRUE) {
      data <- rbind_list(data, pdata) %>% data.frame()
    }
    
    # Inputs vector
    inputs <- NULL
    
    if("G60" %in% input$sliderselect) {inputs <- c(inputs, input$g60)} else {inputs <- c(inputs, 0)}
    if("A60" %in% input$sliderselect) {inputs <- c(inputs, input$a60)} else {inputs <- c(inputs, 0)}
    if("P60" %in% input$sliderselect) {inputs <- c(inputs, input$p60)} else {inputs <- c(inputs, 0)}
    if("CF60" %in% input$sliderselect) {inputs <- c(inputs, input$cf60)} else {inputs <- c(inputs, 0)}
    if("CA60" %in% input$sliderselect) {inputs <- c(inputs, input$ca60)} else {inputs <- c(inputs, 0)}
    if("CF%" %in% input$sliderselect) {inputs <- c(inputs, input$cf)} else {inputs <- c(inputs, 0)}
    if("Rel.CF60" %in% input$sliderselect) {inputs <- c(inputs, input$relcf60)} else {inputs <- c(inputs, 0)}
    if("Rel.CA60" %in% input$sliderselect) {inputs <- c(inputs, input$relca60)} else {inputs <- c(inputs, 0)}
    if("Rel.CF%" %in% input$sliderselect) {inputs <- c(inputs, input$relcf)} else {inputs <- c(inputs, 0)}
    if("xGF60" %in% input$sliderselect) {inputs <- c(inputs, input$xgf60)} else {inputs <- c(inputs, 0)}
    if("xGA60" %in% input$sliderselect) {inputs <- c(inputs, input$xga60)} else {inputs <- c(inputs, 0)}
    if("xGF%" %in% input$sliderselect) {inputs <- c(inputs, input$xgf)} else {inputs <- c(inputs, 0)}
    if("Rel.xGF60" %in% input$sliderselect) {inputs <- c(inputs, input$relxgf60)} else {inputs <- c(inputs, 0)}
    if("Rel.xGA60" %in% input$sliderselect) {inputs <- c(inputs, input$relxga60)} else {inputs <- c(inputs, 0)}
    if("Rel.xGF%" %in% input$sliderselect) {inputs <- c(inputs, input$relxgf)} else {inputs <- c(inputs, 0)}
    if("GF60" %in% input$sliderselect) {inputs <- c(inputs, input$gf60)} else {inputs <- c(inputs, 0)}
    if("GA60" %in% input$sliderselect) {inputs <- c(inputs, input$ga60)} else {inputs <- c(inputs, 0)}
    if("GF%" %in% input$sliderselect) {inputs <- c(inputs, input$gf)} else {inputs <- c(inputs, 0)}
    if("Rel.GF60" %in% input$sliderselect) {inputs <- c(inputs, input$relgf60)} else {inputs <- c(inputs, 0)}
    if("Rel.GA60" %in% input$sliderselect) {inputs <- c(inputs, input$relga60)} else {inputs <- c(inputs, 0)}
    if("Rel.GF%" %in% input$sliderselect) {inputs <- c(inputs, input$relgf)} else {inputs <- c(inputs, 0)}
    if("iCF60" %in% input$sliderselect) {inputs <- c(inputs, input$icf60)} else {inputs <- c(inputs, 0)}
    if("ixG60" %in% input$sliderselect) {inputs <- c(inputs, input$ixg60)} else {inputs <- c(inputs, 0)}
    if("ixFSh%" %in% input$sliderselect) {inputs <- c(inputs, input$ixfsh)} else {inputs <- c(inputs, 0)}
    if("TOI%" %in% input$sliderselect) {inputs <- c(inputs, input$toi.)} else {inputs <- c(inputs, 0)}
    if("TOI.QoT" %in% input$sliderselect) {inputs <- c(inputs, input$toiqot)} else {inputs <- c(inputs, 0)}
    if("CF.QoT" %in% input$sliderselect) {inputs <- c(inputs, input$cfqot)} else {inputs <- c(inputs, 0)}
    if("xGF.QoT" %in% input$sliderselect) {inputs <- c(inputs, input$xgfqot)} else {inputs <- c(inputs, 0)}
    if("TOI.QoC" %in% input$sliderselect) {inputs <- c(inputs, input$toiqoc)} else {inputs <- c(inputs, 0)}
    if("CF.QoC" %in% input$sliderselect) {inputs <- c(inputs, input$cfqoc)} else {inputs <- c(inputs, 0)}
    if("xGF.QoC" %in% input$sliderselect) {inputs <- c(inputs, input$xgfqoc)} else {inputs <- c(inputs, 0)}
    if("ZSR" %in% input$sliderselect) {inputs <- c(inputs, input$zsr)}else {inputs <- c(inputs, 0)}
    if("iPEND60" %in% input$sliderselect) {inputs <- c(inputs, input$ipend60)} else {inputs <- c(inputs, 0)}
    if("iPENT60" %in% input$sliderselect) {inputs <- c(inputs, input$ipent60)} else {inputs <- c(inputs, 0)}
    if("iHF60" %in% input$sliderselect) {inputs <- c(inputs, input$ihf60)} else {inputs <- c(inputs, 0)}
    if("iTKA60" %in% input$sliderselect) {inputs <- c(inputs, input$itka60)} else {inputs <- c(inputs, 0)}
    if("iGVA60" %in% input$sliderselect) {inputs <- c(inputs, input$igva60)} else {inputs <- c(inputs, 0)}
    if("iBLK60" %in% input$sliderselect) {inputs <- c(inputs, input$iblk60)} else {inputs <- c(inputs, 0)}
    if("iFO%" %in% input$sliderselect) {inputs <- c(inputs, input$ifo)} else {inputs <- c(inputs, 0)}
    
    distlist <- list(
      ((data$G60 - pdata$G60)/(max(na.omit(data$G60)) - min(na.omit(data$G60))))^2,
      ((data$A60 - pdata$A60)/(max(na.omit(data$A60)) - min(na.omit(data$A60))))^2,
      ((data$P60 - pdata$P60)/(max(na.omit(data$P60)) - min(na.omit(data$P60))))^2,
      ((data$CF60 - pdata$CF60)/(max(na.omit(data$CF60)) - min(na.omit(data$CF60))))^2,
      ((data$CA60 - pdata$CA60)/(max(na.omit(data$CA60)) - min(na.omit(data$CA60))))^2,
      ((data$CF. - pdata$CF.)/(max(na.omit(data$CF.)) - min(na.omit(data$CF.))))^2,
      ((data$Rel.CF60 - pdata$Rel.CF60)/(max(na.omit(data$Rel.CF60)) - min(na.omit(data$Rel.CF60))))^2,
      ((data$Rel.CA60 - pdata$Rel.CA60)/(max(na.omit(data$Rel.CA60)) - min(na.omit(data$Rel.CA60))))^2,
      ((data$Rel.CF. - pdata$Rel.CF.)/(max(na.omit(data$Rel.CF.)) - min(na.omit(data$Rel.CF.))))^2,
      ((data$xGF60 - pdata$xGF60)/(max(na.omit(data$xGF60)) - min(na.omit(data$xGF60))))^2,
      ((data$xGA60 - pdata$xGA60)/(max(na.omit(data$xGA60)) - min(na.omit(data$xGA60))))^2,
      ((data$xGF. - pdata$xGF.)/(max(na.omit(data$xGF.)) - min(na.omit(data$xGF.))))^2,
      ((data$Rel.xGF60 - pdata$Rel.xGF60)/(max(na.omit(data$Rel.xGF60)) - min(na.omit(data$Rel.xGF60))))^2,
      ((data$Rel.xGA60 - pdata$Rel.xGA60)/(max(na.omit(data$Rel.xGA60)) - min(na.omit(data$Rel.xGA60))))^2,
      ((data$Rel.xGF. - pdata$Rel.xGF.)/(max(na.omit(data$Rel.xGF.)) - min(na.omit(data$Rel.xGF.))))^2,
      ((data$GF60 - pdata$GF60)/(max(na.omit(data$GF60)) - min(na.omit(data$GF60))))^2,
      ((data$GA60 - pdata$GA60)/(max(na.omit(data$GA60)) - min(na.omit(data$GA60))))^2,
      ((data$GF. - pdata$GF.)/(max(na.omit(data$GF.)) - min(na.omit(data$GF.))))^2,
      ((data$Rel.GF60 - pdata$Rel.GF60)/(max(na.omit(data$Rel.GF60)) - min(na.omit(data$Rel.GF60))))^2,
      ((data$Rel.GA60 - pdata$Rel.GA60)/(max(na.omit(data$Rel.GA60)) - min(na.omit(data$Rel.GA60))))^2,
      ((data$Rel.GF. - pdata$Rel.GF.)/(max(na.omit(data$Rel.GF.)) - min(na.omit(data$Rel.GF.))))^2,
      ((data$iCF60 - pdata$iCF60)/(max(na.omit(data$iCF60)) - min(na.omit(data$iCF60))))^2,
      ((data$ixG60 - pdata$ixG60)/(max(na.omit(data$ixG60)) - min(na.omit(data$ixG60))))^2,
      ((data$ixFSh. - pdata$ixFSh.)/(max(na.omit(data$ixFSh.)) - min(na.omit(data$ixFSh.))))^2,
      ((data$TOI. - pdata$TOI.)/(max(na.omit(data$TOI.)) - min(na.omit(data$TOI.))))^2,
      ((data$TOI.QoT - pdata$TOI.QoT)/(max(na.omit(data$TOI.QoT)) - min(na.omit(data$TOI.QoT))))^2,
      ((data$CF.QoT - pdata$CF.QoT)/(max(na.omit(data$CF.QoT)) - min(na.omit(data$CF.QoT))))^2,
      ((data$xGF.QoT - pdata$xGF.QoT)/(max(na.omit(data$xGF.QoT)) - min(na.omit(data$xGF.QoT))))^2,
      ((data$TOI.QoC - pdata$TOI.QoC)/(max(na.omit(data$TOI.QoC)) - min(na.omit(data$TOI.QoC))))^2,
      ((data$CF.QoC - pdata$CF.QoC)/(max(na.omit(data$CF.QoC)) - min(na.omit(data$CF.QoC))))^2,
      ((data$xGF.QoC - pdata$xGF.QoC)/(max(na.omit(data$xGF.QoC)) - min(na.omit(data$xGF.QoC))))^2,
      ((data$ZSR - pdata$ZSR)/(max(na.omit(data$ZSR)) - min(na.omit(data$ZSR))))^2,
      ((data$iPEND60 - pdata$iPEND60)/(max(na.omit(data$iPEND60)) - min(na.omit(data$iPEND60))))^2,
      ((data$iPENT60 - pdata$iPENT60)/(max(na.omit(data$iPENT60)) - min(na.omit(data$iPENT60))))^2,
      ((data$iHF60 - pdata$iHF60)/(max(na.omit(data$iHF60)) - min(na.omit(data$iHF60))))^2,
      ((data$iTKA60 - pdata$iTKA60)/(max(na.omit(data$iTKA60)) - min(na.omit(data$iTKA60))))^2,
      ((data$iGVA60 - pdata$iGVA60)/(max(na.omit(data$iGVA60)) - min(na.omit(data$iGVA60))))^2,
      ((data$iBLK60 - pdata$iBLK60)/(max(na.omit(data$iBLK60)) - min(na.omit(data$iBLK60))))^2,
      ((data$iFO. - pdata$iFO.)/(max(na.omit(data$iFO.)) - min(na.omit(data$iFO.))))^2
    )
    
    distvec <- unlist(distlist)
    distvec[which(is.na(distvec) == TRUE)] <- 0
    
    distmat <- rep(inputs, each = length(data$Player))*distvec %>% matrix(ncol = length(inputs), byrow = FALSE) %>% data.frame()
    
    
    
    Similarity <- (1 - sqrt(distmat[, 1] + distmat[, 2] +
                              distmat[, 3] + distmat[, 4] +
                              distmat[, 5] + distmat[, 6] +
                              distmat[, 7] + distmat[, 8] +
                              distmat[, 9] + distmat[, 10] +
                              distmat[, 11] + distmat[, 12] +
                              distmat[, 13] + distmat[, 14] +
                              distmat[, 15] + distmat[, 16] +
                              distmat[, 17] + distmat[, 18] +
                              distmat[, 19] + distmat[, 20] +
                              distmat[, 21] + distmat[, 22] +
                              distmat[, 23] + distmat[, 24] +
                              distmat[, 25] + distmat[, 26] +
                              distmat[, 27] + distmat[, 28] +
                              distmat[, 29] + distmat[, 30] +
                              distmat[, 31] + distmat[, 32] +
                              distmat[, 33] + distmat[, 34] +
                              distmat[, 35] + distmat[, 36] +
                              distmat[, 37] + distmat[, 38] +
                              distmat[, 39]
    )/sqrt(sum(inputs)))*100
    
    data <- cbind(Similarity, data) %>% data.frame()
    
    data
    
  })
  
  # Table contents
  table.contents <- reactive({
    
    # Players input
    if (length(input$name2) < 1) {
      playervector <- unique(as.character(data$Player))
    } else {
      playervector <- input$name2
    }
    
    data <- simdata() %>% filter(Player %in% playervector) %>% data.frame()
    
    # Format function
    form <- function(x) {as.numeric(format(round(as.numeric(as.character(x)), 2), nsmall = 2))}
    
    t1 <- mutate_each(data, funs(form), -c(Player, Season))
    
    colnames(t1) <- gsub("[.]$", "%", colnames(t1))
    col.index <- which(colnames(t1) %in% input$sliderselect)
    
    select(t1, c(Similarity, Player, Season, col.index)) %>% arrange(desc(Similarity)) %>% mutate(Similarity = paste(Similarity, "%", sep = ""))
    
  })
  
  # Table output
  output$t1 <- DT::renderDataTable({
    
    t1 <- table.contents()
    datatable(t1, 
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
      formatStyle('Player', fontWeight = "bold")
    
  })
  
  
})
