library(shinydashboard, warn.conflicts = FALSE)
library(pheatmap)
library(ggplot2)
library(ECharts2Shiny)
library(leaflet)
library(ggplot2)
library(tidyverse, warn.conflicts = FALSE)
library(hrbrthemes)
library(scales)
library(plotly,warn.conflicts = FALSE)
library(rgdal)
library(reshape2)
library(sf)
library("rgdal")

WRP <- read.csv("data/iterations_labeled_seasonal.csv") #slider data


FMM <- read.csv("data/FFM_percentiles_wetdrybaseflowmag_02172021_baselineWRP.csv")

FlowRanges <- read.csv("data/FlowRanges_Species_RecUses_Allnodes_03142021.csv")

river <- readOGR("data/LAR_reporting_reaches.shp")
river <- spTransform(river, CRS("+proj=longlat +datum=WGS84 +no_defs"))

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "data/www",
      package = "imageissue"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}


goodV <- 0

ui <- dashboardPage(
  dashboardHeader(title = "WRP Discharge Heatmap"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Maps", tabName = "transport", icon =icon("map")),
      menuItem("Sensitivity Curves", tabName = "tree", icon =icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "transport",
    fluidPage(
      tags$style(HTML("


      .box.box-solid.box-primary>.box-header {
        color:#060606;
        background:#68B256
                          }
      
      .box.box-solid.box-primary{
      border-bottom-color:#68B256;
      border-left-color:#68B256;
      border-right-color:#68B256;
      border-top-color:#68B256;
      }
      .box.box-solid.box-success>.box-header {
        color:#fff;
        background:#C70039
                          }
      
      .box.box-solid.box-success{
      border-bottom-color:#C70039;
      border-left-color:#C70039;
      border-right-color:#C70039;
      border-top-color:#C70039;
      }
      .form-group {
      margin-bottom: 5px;
      margin-top: 5px
      }

                                    ")),
      box(title = strong("Heatmap"), status = "success", solidHeader = TRUE,
          selectInput("hab", "Beneficial Use:",
                      c("ALL","EST", "WILD","RARE", "MIGR", "SPWN", "COLD")),
          plotOutput(outputId = "heat", height = "70vh")),
      box(status = "success",
        selectInput("season", "Season Baseflow:",
                    c("Wet" = "wet", "Dry" = "dry")),
        sliderInput("wrp", "WRP Discharge (cfs):",
                    min = 1, max = 100, value = 1)),
      box(title = strong("LA River"), status = "primary", solidHeader = TRUE,
          selectInput("use", width = "92%",textOutput("other"),
                      c( 
                        "Willow Adult",
                        "Steelhead Migration (Prolonged)",
                        "Typha Adult",
                        "Cladophora Adult",
                        "SAS Adult",
                        "Steelhead Migration (Burst)",
                        "Kayak",
                        "Fishing")),
          leafletOutput("LosAngeles", width = "100%", height = 550))
    )),
    tabItem(tabName = "tree",
    fluidPage(
      tags$style(HTML("


      .box.box-solid.box-primary>.box-header {
        color:#060606;
        background:#69b3a2
                          }
      
      .box.box-solid.box-primary{
      border-bottom-color:#69b3a2;
      border-left-color:#69b3a2;
      border-right-color:#69b3a2;
      border-top-color:#69b3a2;
      }
                                 ")),
      box(title = strong("Percentiles"),
          selectInput("season2", "Season Baseflow:",
                      c("Wet" = "wet", "Dry" = "dry")),
          selectInput("node", "Node:",
                      c("LA11", "F34D", "F57C",
                        "GLEN","LA3","LA8","LA13","LA14","F319","F300")),
          plotOutput("p10"), status = "primary", solidHeader = TRUE)
    )
    )
      )
    )
  )




popCon <- function(name){
  link <- paste("?url=inTabpanel_", name, sep = "")
  name <- paste(sep = "<br/>",
                actionLink(link, name),
                "This one would also update to another tab")
}



#Did they fall in the range? 
fmDry <- FMM[!grepl("Wet", FMM$Season),] #dry only
fmWet <- FMM[!grepl("Dry", FMM$Season),] #wet only


flowRDry0 <- FlowRanges[!grepl("Wet_BFL_Mag_50", FlowRanges$metric),] #dry 
flowRDry1 <- flowRDry0[!grepl("Wet_BFL_Mag_10", flowRDry0$metric),] #dry 

flowRWet0 <- FlowRanges[!grepl("DS_Mag_50", FlowRanges$metric),] #wet 
flowRWet1 <- flowRWet0[!grepl("DS_Mag_10", flowRWet0$metric),] #wet


aCol = "blue"
bCol = "red"
cCol = "red"
dCol = "red"
eCol = "red"
fCol = "red"
gCol = "red"
hCol = "red"
iCol = "red"
jCol = "red"
kCol = "red"
lCol = "red"
mCol = "red"

color = 'blue'
aCol = "black"

server <- function(input, output, session) {
  observe({
    x <- input$hab
    if(x == 'ALL'){
      x <-  c( 
        "Willow Adult",
        "Steelhead Migration (Prolonged)",
        "Typha Adult",
        "Cladophora Adult",
        "SAS Adult",
        "Steelhead Migration (Burst)",
        "Kayak",
        "Fishing")
    }
    if(x == 'COLD'){
      x <- c("SAS Adult","Steelhead Migration (Prolonged)",
             "Steelhead Migration (Burst)")
    }
    if(x == 'SPWN'){
      x <- "SAS Adult"
    }
    if(x == 'MIGR'){
      x <- c("Steelhead Migration (Prolonged)",
             "Steelhead Migration (Burst)")
    }
    if(x == "Recreation"){
      x <- c("Kayak",
      "Fishing")
    }
    if(x == "RARE"){
      x<- c("SAS Adult",
            "Steelhead Migration (Burst)",
            "Willow Adult",
            "Steelhead Migration (Prolonged)")
    }
    if(x == "WILD"){
      x<-c("Willow Adult", "Typha Adult",
           "Cladophora Adult")
    }
    if(x == "EST"){
      x <- c("Steelhead Migration (Prolonged)",
             "Steelhead Migration (Burst)",
             "Cladophora Adult")
    }
    updateSelectInput(session, "use",
                      choices = x)
  })
  output$other <- renderText({
    val<- paste("Focal Species: ", input$hab, sep="")
    val
  })
  
  observe(
    output$scen<-{(
      renderText(which.min(abs(WRP$Med_Q_cfs - input$wrp)))
    )}
  )
  
  output$heat<-renderPlot({
    scen <- which.min(abs(WRP$Med_Q_cfs - input$wrp))
    print(scen)
    F300p10Dry <- 0.900017708 + 0.805599066*input$wrp
    F300p90Dry <- 9.236788795 + 0.876596075*input$wrp
    F300p10Wet <- 0.900017708 + 0.805599066*input$wrp
    F300p90Wet <- 9.236788795 + 0.876596075*input$wrp
    
    F319p10Dry <- 47.39457326 + 1.005771489*input$wrp
    F319p90Dry <- 76.45154635 + 1.043169622*input$wrp
    F319p10Wet <- 47.39457326 + 1.005771489*input$wrp
    F319p90Wet <- 76.45154635 + 1.043169622*input$wrp
    
    F57Cp10Dry <- 11.99113979 + 0.907820603*input$wrp
    F57Cp90Dry <- 25.59802376 + 1.046174046*input$wrp
    F57Cp10Wet <-11.99113979 + 0.907820603*input$wrp
    F57Cp90Wet <-25.59802376 + 1.046174046*input$wrp
    
    LA13p10Dry <-6.623349565 + 0.820509704*input$wrp
    LA13p90Dry <-20.28892415 + 0.870686647*input$wrp
    LA13p10Wet <-6.623349565 + 0.820509704*input$wrp
    LA13p90Wet <-20.28892415 + 0.870686647*input$wrp
    
    LA11p10Dry<-11.95112229 + 0.888542946*input$wrp
    LA11p90Dry<-23.76124488 + 1.049193898*input$wrp
    LA11p10Wet<-11.95112229 + 0.888542946*input$wrp
    LA11p90Wet<-23.76124488 + 1.049193898*input$wrp
    
    F34Dp10Dry<-45.94858526 + 0.998084005*input$wrp
    F34Dp90Dry<-71.72411613 + 0.95174529*input$wrp
    F34Dp10Wet<-45.94858526 + 0.998084005*input$wrp
    F34Dp90Wet<-71.72411613 + 0.95174529*input$wrp
    
    LA14p10Dry<-4.912058332 + 0.827809744*input$wrp
    LA14p90Dry<-18.18934302 + 0.88306858*input$wrp
    LA14p10Wet<-4.912058332 + 0.827809744*input$wrp
    LA14p90Wet<-18.18934302 + 0.88306858*input$wrp
    
    GLENp10Dry<-9.885996252 + 0.918687061*input$wrp
    GLENp90Dry<-22.38736668 + 1.048267846*input$wrp
    GLENp10Wet<-9.885996252 + 0.918687061*input$wrp
    GLENp90Wet<-22.38736668 + 1.048267846*input$wrp
    
    
    LA8p10Dry<-27.92521955 + 0.921523533*input$wrp
    LA8p90Dry<-39.67282263 + 1.012418222*input$wrp
    LA8p10Wet<-27.92521955 + 0.921523533*input$wrp
    LA8p90Wet<-39.67282263 + 1.012418222*input$wrp
    
    LA3p10Dry<-46.63140866 + 1.007484302*input$wrp
    LA3p90Dry<-75.29510986 + 1.002334493*input$wrp
    LA3p10Wet<-46.63140866 + 1.007484302*input$wrp
    LA3p90Wet<-75.29510986 + 1.002334493*input$wrp
    
    if(input$season == "wet"){
      fm <- fmWet
      flow <- flowRWet1
      F300p10 <- F300p10Wet
      F300p90 <- F300p90Wet
      
      F319p10 <- F319p10Wet
      F319p90 <- F319p90Wet
      
      F57Cp10 <-F57Cp10Wet
      F57Cp90 <- F57Cp90Wet
      
      LA13p10 <- LA13p10Wet
      LA13p90 <- LA13p90Wet
      
      LA11p10 <- LA11p10Wet
      LA11p90 <- LA11p90Wet
      
      F34Dp10 <- F34Dp10Wet
      F34Dp90 <- F34Dp90Wet
      
      LA14p10 <- LA14p10Wet
      LA14p90 <- LA14p90Wet
      
      GLENp10 <- GLENp10Wet
      GLENp90 <- GLENp90Wet
      
      LA8p10 <- LA8p10Wet
      LA8p90 <- LA8p90Wet
      
      LA3p10 <- LA3p10Wet
      LA3p90 <- LA3p90Wet
      
    }
    else{
      fm <- fmDry
      flow <-flowRDry1
      
      F300p10 <- F300p10Dry
      F300p90 <- F300p90Dry
      
      F319p10 <- F319p10Dry
      F319p90 <- F319p90Dry
      
      F57Cp10 <-F57Cp10Dry
      F57Cp90 <- F57Cp90Dry
      
      LA13p10 <- LA13p10Dry
      LA13p90 <- LA13p90Dry
      
      LA11p10 <- LA11p10Dry
      LA11p90 <- LA11p90Dry
      
      F34Dp10 <- F34Dp10Dry
      F34Dp90 <- F34Dp90Dry
      
      LA14p10 <- LA14p10Dry
      LA14p90 <- LA14p90Dry
      
      GLENp10 <- GLENp10Dry
      GLENp90 <- GLENp90Dry
      
      LA8p10 <- LA8p10Dry
      LA8p90 <- LA8p90Dry
      
      LA3p10 <- LA3p10Dry
      LA3p90 <- LA3p90Dry
    }
    
    row1 <- fm[which(fm$ReportingNode == "F300" & fm$Scenario == scen),]
    F300p <- 50
    
    row2 <- fm[which(fm$ReportingNode == "F319" & fm$Scenario == scen),]
    F319p <- row2$p50
    
    row3 <- fm[which(fm$ReportingNode == "F34D" & fm$Scenario == scen),]
    F34Dp <- row3$p50
    
    
    row4 <- fm[which(fm$ReportingNode == "F37B" & fm$Scenario == scen),]
    F37Bp <- row4$p50
    
    
    row5 <- fm[which(fm$ReportingNode == "F45B" & fm$Scenario == scen),]
    F45Bp <- row5$p50
    
    
    row6 <- fm[which(fm$ReportingNode == "F57C" & fm$Scenario == scen),]
    F57Cp <- row6$p50
    
    
    row7 <- fm[which(fm$ReportingNode == "GLEN" & fm$Scenario == scen),]
    GLENp <- row7$p50
    
    
    row8 <- fm[which(fm$ReportingNode == "LA1" & fm$Scenario == scen),]
    LA1p <- row8$p50
    
    
    row9 <- fm[which(fm$ReportingNode == "LA11" & fm$Scenario == scen),]
    LA11p <- row9$p50
    
    
    row10 <- fm[which(fm$ReportingNode == "LA13" & fm$Scenario == scen),]
    LA13p <- row10$p50
    
    
    row11 <- fm[which(fm$ReportingNode == "LA14" & fm$Scenario == scen),]
    LA14p <- row11$p50
    
    
    row12 <- fm[which(fm$ReportingNode == "LA2" & fm$Scenario == scen),]
    LA2p <- row12$p50
    
    
    row13 <- fm[which(fm$ReportingNode == "LA20_2" & fm$Scenario == scen),]
    LA20_2p<- row13$p50
    
    
    row14 <- fm[which(fm$ReportingNode == "LA20" & fm$Scenario == scen),]
    LA20p <- row14$p50
    
    
    row15 <- fm[which(fm$ReportingNode == "LA3" & fm$Scenario == scen),]
    LA3p <- row15$p50
    
    
    row16 <- fm[which(fm$ReportingNode == "LA8" & fm$Scenario == scen),]
    LA8p <- row16$p50
    
    
    first_c <- c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20")
                      
    
    range <- flowRWet1[which(flowRWet1$Species_Label == "Willow Adult" & flowRWet1$Node == "F57C"),]
    as.integer(range$Lower_Limit[1])
    
    range <- na.omit(flowRWet1[which(flowRWet1$Node == "LA1"),])
    range
    x <- 54
    if(x > range$Lower_Limit[1]){
      print("hi")
    }
    range
    new = 0 
    fowGet <- function(name, species, num, var){
      p10 <- paste(var, "10", sep= "")
      p90 <- paste(var, "90", sep= "")
      
  
      range1 <- flow[which(flow$Species_Label == species & flow$Node == name),]
      range2 <- range1[!is.na(range1$Lower_Limit),]
      range <- range2[!is.na(range1$Upper_Limit),]
      
      p10 <- as.numeric(noquote(p10))
      p90 <- as.numeric(noquote(p90))
      

      lower <- as.numeric(noquote(range$Lower_Limit[1]))
      upper <- as.numeric(noquote(range$Upper_Limit[1]))
      

      if(is.na(lower) | is.na(upper)){
        return("Unknown")
      }
      if(nrow(range) == 0){
        new <- "Unknown"
        return(new)
      }
      if(toString(range$Lower_Limit) == "TBD" | toString(range$Upper_Limit) == "TBD"){
        new = "Unknown"
        return(new)
      }
      if(as.numeric(p10) > upper){ 
        if(as.numeric(p90) > upper){
          new <- 300
          return("Too High")
        }
      }
      if(as.numeric(p90) < lower) {
        if(as.numeric(p10) < lower){
            new <- 225
            return("Too Low")
        } 
      }
      else{
        return("Within Range")
      }
    }
    
    
    F300WA <- fowGet("F300", "Willow Adult", 1, F300p)   
    F319WA <-fowGet("F319", "Willow Adult", 2, F319p)
    F34DWA <-fowGet("F34D", "Willow Adult", 3, F34Dp)
    #F37BWA <- fowGet("F37B", "Willow Adult", 4, F37Bp)
    F45BWA <- fowGet("F45B", "Willow Adult", 5, F45Bp)
    F57CWA <- fowGet("F57C", "Willow Adult", 6, F57Cp)
    GLENWA<-fowGet("GLEN", "Willow Adult", 7, GLENp)
    #LA1WA <-fowGet("LA1", "Willow Adult", 8, LA1p)
    LA11WA <-fowGet("LA11", "Willow Adult", 9, LA11p)
    LA13WA <-fowGet("LA13", "Willow Adult", 10, LA13p)
    LA14WA<-fowGet("LA14", "Willow Adult", 11, LA14p)
    #LA2WA<-fowGet("LA2", "Willow Adult", 12, LA2p)
    LA20_2WA<-fowGet("LA20_2", "Willow Adult", 13, LA20_2p)
    LA20WA<-fowGet("LA20", "Willow Adult", 14, LA20p)
    LA3WA<-fowGet("LA3", "Willow Adult", 15, LA3p)
    LA8WA<-fowGet("LA8", "Willow Adult", 16, LA8p)
    
    
    Willow_Adult <- c(F319WA,LA3WA,F34DWA, LA8WA,F57CWA,
                  LA11WA, GLENWA,LA13WA, LA14WA, F300WA, LA20_2WA, LA20WA)
    
    
    F300TA <- fowGet("F300", "Typha Adult", 1, F300p)   
    F319TA <-fowGet("F319", "Typha Adult", 2, F319p)
    F34DTA <-fowGet("F34D", "Typha Adult", 3, F34Dp)
    #F347BTA <- fowGet("F37B", "Typha Adult", 4, F37Bp)
    F45BTA <- fowGet("F45B", "Typha Adult", 5, F45Bp)
    F57CTA <- fowGet("F57C", "Typha Adult", 6, F57Cp)
    GLENTA<-fowGet("GLEN", "Typha Adult", 7, GLENp)
    #LA1TA <-fowGet("LA1", "Typha Adult", 8, LA1p)
    LA11TA <-fowGet("LA11", "Typha Adult", 9,LA11p)
    LA13TA <-fowGet("LA13", "Typha Adult", 10,LA13p)
    LA14TA<-fowGet("LA14", "Typha Adult", 11,LA14p)
    #LA2TA<-fowGet("LA2", "Typha Adult", 12,LA2p)
    LA20_2TA<-fowGet("LA20_2", "Typha Adult", 13,LA20_2p)
    LA20TA<-fowGet("LA20", "Typha Adult", 14,LA20p)
    LA3TA<-fowGet("LA3", "Typha Adult", 15,LA3p)
    LA8TA<-fowGet("LA8", "Typha Adult", 16,LA8p)
    
    
    Typha_Adult <- c(F319TA,LA3TA,F34DTA, LA8TA,F57CTA,
                     LA11TA, GLENTA,LA13TA, LA14TA, F300TA, LA20_2TA, LA20TA)
    
    F300MP <- fowGet("F300", "Steelhead Migration (Prolonged)", 1,F300p)   
    F319MP <-fowGet("F319", "Steelhead Migration (Prolonged)", 2,F319p)
    F34DMP <-fowGet("F34D", "Steelhead Migration (Prolonged)", 3,F34Dp)
    #F347BMP <- fowGet("F37B", "Steelhead Migration (Prolonged)", 4,F37Bp)
    F45BMP <- fowGet("F45B", "Steelhead Migration (Prolonged)", 5,F45Bp)
    F57CMP <- fowGet("F57C", "Steelhead Migration (Prolonged)", 6,F57Cp)
    GLENMP<-fowGet("GLEN", "Steelhead Migration (Prolonged)", 7,GLENp)
    #LA1MP <-fowGet("LA1", "Steelhead Migration (Prolonged)", 8,LA1p)
    LA11MP <-fowGet("LA11", "Steelhead Migration (Prolonged)", 9,LA11p)
    LA13MP <-fowGet("LA13", "Steelhead Migration (Prolonged)", 10,LA13p)
    LA14MP<-fowGet("LA14", "Steelhead Migration (Prolonged)", 11,LA14p)
    #LA2MP<-fowGet("LA2", "Steelhead Migration (Prolonged)", 12,LA2p)
    LA20_2MP<-fowGet("LA20_2", "Steelhead Migration (Prolonged)", 13,LA20_2p)
    LA20MP<-fowGet("LA20", "Steelhead Migration (Prolonged)", 14,LA20p)
    LA3MP<-fowGet("LA3", "Steelhead Migration (Prolonged)", 15,LA3p)
    LA8MP<-fowGet("LA8", "Steelhead Migration (Prolonged)", 16,LA8p)
    
    
    Steelhead_Migration_Prolonged <- c(F319MP,LA3MP,F34DMP, LA8MP,F57CMP,
                                       LA11MP, GLENMP,LA13MP, LA14MP, F300MP, LA20_2MP, LA20MP)
    
    F300SS <- fowGet("F300", "Steelhead Migration (Smolts)", 1,F300p)   
    F319SS <-fowGet("F319", "Steelhead Migration (Smolts)", 2,F319p)
    F34DSS <-fowGet("F34D", "Steelhead Migration (Smolts)", 3,F34Dp)
    F347BSS <- fowGet("F37B", "Steelhead Migration (Smolts)", 4,F37Bp)
    F45BSS <- fowGet("F45B", "Steelhead Migration (Smolts)", 5,F45Bp)
    F57CSS <- fowGet("F57C", "Steelhead Migration (Smolts)", 6,F57Cp)
    GLENSS<-fowGet("GLEN", "Steelhead Migration (Smolts)", 7,GLENp)
    LA1SS <-fowGet("LA1", "Steelhead Migration (Smolts)", 8,LA1p)
    LA11SS <-fowGet("LA11", "Steelhead Migration (Smolts)", 9,LA11p)
    LA13SS <-fowGet("LA13", "Steelhead Migration (Smolts)", 10,LA13p)
    LA14SS<-fowGet("LA14", "Steelhead Migration (Smolts)", 11,LA14p)
    LA2SS<-fowGet("LA2", "Steelhead Migration (Smolts)", 12,LA2p)
    LA20_2SS<-fowGet("LA20_2", "Steelhead Migration (Smolts)", 13,LA20_2p)
    LA20SS<-fowGet("LA20", "Steelhead Migration (Smolts)", 14,LA20p)
    LA3SS<-fowGet("LA3", "Steelhead Migration (Smolts)", 15,LA3p)
    LA8SS<-fowGet("LA8", "Steelhead Migration (Smolts)", 16,LA8p)
    
    Steelhead_Migration_Smolts <- c(F319SS,LA3SS,F34DSS, LA8SS,F57CSS,
                                    LA11SS, GLENSS,LA13SS, LA14SS, F300SS, LA20_2SS, LA20SS)
    
    F300SB <- fowGet("F300", "Steelhead Migration (Burst)", 1,F300p)   
    F319SB <-fowGet("F319", "Steelhead Migration (Burst)", 2,F319p)
    F34DSB <-fowGet("F34D", "Steelhead Migration (Burst)", 3,F34Dp)
    F347BSB <- fowGet("F37B", "Steelhead Migration (Burst)", 4,F37Bp)
    F45BSB <- fowGet("F45B", "Steelhead Migration (Burst)", 5,F45Bp)
    F57CSB <- fowGet("F57C", "Steelhead Migration (Burst)", 6,F57Cp)
    GLENSB<-fowGet("GLEN", "Steelhead Migration (Burst)", 7,GLENp)
    LA1SB <-fowGet("LA1", "Steelhead Migration (Burst)", 8,LA1p)
    LA11SB <-fowGet("LA11", "Steelhead Migration (Burst)", 9,LA11p)
    LA13SB <-fowGet("LA13", "Steelhead Migration (Burst)", 10,LA13p)
    LA14SB<-fowGet("LA14", "Steelhead Migration (Burst)", 11,LA14p)
    LA2SB<-fowGet("LA2", "Steelhead Migration (Burst)", 12,LA2p)
    LA20_2SB<-fowGet("LA20_2", "Steelhead Migration (Burst)", 13,LA20_2p)
    LA20SB<-fowGet("LA20", "Steelhead Migration (Burst)", 14,LA20p)
    LA3SB<-fowGet("LA3", "Steelhead Migration (Burst)", 15,LA3p)
    LA8SB<-fowGet("LA8", "Steelhead Migration (Burst)", 16,LA8p)
    
    Steelhead_Migration_Burst <- c(F319SB,LA3SB,F34DSB, LA8SB,F57CSB,
                                   LA11SB, GLENSB,LA13SB, LA14SB, F300SB, LA20_2SB, LA20SB)
    
    F300AD <- fowGet("F300", "SAS Adult", 1,F300p)   
    F319AD <-fowGet("F319", "SAS Adult", 2,F319p)
    F34DAD <-fowGet("F34D", "SAS Adult", 3,F34Dp)
    F347BAD <- fowGet("F37B", "SAS Adult", 4,F37Bp)
    F45BAD <- fowGet("F45B", "SAS Adult", 5,F45Bp)
    F57CAD <- fowGet("F57C", "SAS Adult", 6,F57Cp)
    GLENAD<-fowGet("GLEN", "SAS Adult", 7,GLENp)
    LA1AD <-fowGet("LA1", "SAS Adult", 8,LA1p)
    LA11AD <-fowGet("LA11", "SAS Adult", 9,LA11p)
    LA13AD <-fowGet("LA13", "SAS Adult", 10,LA13p)
    LA14AD<-fowGet("LA14", "SAS Adult", 11,LA14p)
    LA2AD<-fowGet("LA2", "SAS Adult", 12,LA2p)
    LA20_2AD<-fowGet("LA20_2", "SAS Adult", 13,LA20_2p)
    LA20AD<-fowGet("LA20", "SAS Adult", 14,LA20p)
    LA3AD<-fowGet("LA3", "SAS Adult", 15,LA3p)
    LA8AD<-fowGet("LA8", "SAS Adult", 16,LA8p)
    
    SAS_Adult <- c(F319AD,LA3AD,F34DAD, LA8AD,F57CAD,
                   LA11AD, GLENAD,LA13AD, LA14AD, F300AD, LA20_2AD, LA20AD)
    
    F300KA <- fowGet("F300", "Rec. Use Kayak", 1,F300p)   
    F319KA <-fowGet("F319", "Rec. Use Kayak", 2,F319p)
    F34DKA <-fowGet("F34D", "Rec. Use Kayak", 3,F34Dp)
    F347BKA <- fowGet("F37B", "Rec. Use Kayak", 4,F37Bp)
    F45BKA <- fowGet("F45B", "Rec. Use Kayak", 5,F45Bp)
    F57CKA <- fowGet("F57C", "Rec. Use Kayak", 6,F57Cp)
    GLENKA<-fowGet("GLEN", "Rec. Use Kayak", 7,GLENp)
    LA1KA <-fowGet("LA1", "Rec. Use Kayak", 8,LA1p)
    LA11KA <-fowGet("LA11", "Rec. Use Kayak", 9,LA11p)
    LA13KA <-fowGet("LA13", "Rec. Use Kayak", 10,LA13p)
    LA14KA<-fowGet("LA14", "Rec. Use Kayak", 11,LA14p)
    LA2KA<-fowGet("LA2", "Rec. Use Kayak", 12,LA2p)
    LA20_2KA<-fowGet("LA20_2", "Rec. Use Kayak", 13,LA20_2p)
    LA20KA<-fowGet("LA20", "Rec. Use Kayak", 14,LA20p)
    LA3KA<-fowGet("LA3", "Rec. Use Kayak", 15,LA3p)
    LA8KA<-fowGet("LA8", "Rec. Use Kayak", 16,LA8p)
    
    Kayak <- c(F319KA,LA3KA,F34DKA, LA8KA,F57CKA,
               LA11KA, GLENKA,LA13KA, LA14KA, F300KA, LA20_2KA, LA20KA)
    
    F300FI <- fowGet("F300", "Rec. Use Fishing", 1,F300p)   
    F319FI <-fowGet("F319", "Rec. Use Fishing", 2,F319p)
    F34DFI <-fowGet("F34D", "Rec. Use Fishing", 3,F34Dp)
    F347BFI <- fowGet("F37B", "Rec. Use Fishing", 4,F37Bp)
    F45BFI <- fowGet("F45B", "Rec. Use Fishing", 5,F45Bp)
    F57CFI <- fowGet("F57C", "Rec. Use Fishing", 6,F57Cp)
    GLENFI<-fowGet("GLEN", "Rec. Use Fishing", 7,GLENp)
    LA1FI <-fowGet("LA1", "Rec. Use Fishing", 8,LA1p)
    LA11FI <-fowGet("LA11", "Rec. Use Fishing", 9,LA11p)
    LA13FI <-fowGet("LA13", "Rec. Use Fishing", 10,LA13p)
    LA14FI<-fowGet("LA14", "Rec. Use Fishing", 11,LA14p)
    LA2FI<-fowGet("LA2", "Rec. Use Fishing", 12,LA2p)
    LA20_2FI<-fowGet("LA20_2", "Rec. Use Fishing", 13,LA20_2p)
    LA20FI<-fowGet("LA20", "Rec. Use Fishing", 14,LA20p)
    LA3FI<-fowGet("LA3", "Rec. Use Fishing", 15,LA3p)
    LA8FI<-fowGet("LA8", "Rec. Use Kayak", 16,LA8p)
    
    
    Fishing <- c(F319FI,LA3FI,F34DFI, LA8FI,F57CFI,
                 LA11FI, GLENFI,LA13FI, LA14FI, F300FI, LA20_2FI, LA20FI)
    
    F300CA <- fowGet("F300", "Cladophora Adult", 1,F300p)   
    F319CA <-fowGet("F319", "Cladophora Adult", 2,F319p)
    F34DCA <-fowGet("F34D", "Cladophora Adult", 3,F34Dp)
    F347BCA <- fowGet("F37B", "Cladophora Adult", 4,F37Bp)
    F45BCA <- fowGet("F45B", "Cladophora Adult", 5,F45Bp)
    F57CCA <- fowGet("F57C", "Cladophora Adult", 6,F57Cp)
    GLENCA<-fowGet("GLEN", "Cladophora Adult", 7,GLENp)
    LA1CA <-fowGet("LA1", "Cladophora Adult", 8,LA1p)
    LA11CA <-fowGet("LA11", "Cladophora Adult", 9,LA11p)
    LA13CA <-fowGet("LA13", "Cladophora Adult", 10,LA13p)
    LA14CA<-fowGet("LA14", "Cladophora Adult", 11,LA14p)
    LA2CA<-fowGet("LA2", "Cladophora Adult", 12,LA2p)
    LA20_2CA<-fowGet("LA20_2", "Cladophora Adult", 13,LA20_2p)
    LA20CA<-fowGet("LA20", "Cladophora Adult", 14,LA20p)
    LA3CA<-fowGet("LA3", "Cladophora Adult", 15,LA3p)
    LA8CA<-fowGet("LA8", "Cladophora Adult", 16,LA8p)
    
 
  
    
    Cladophora <- c(F319CA,LA3CA,F34DCA, LA8CA,F57CCA,
                    LA11CA, GLENCA,LA13CA, LA14CA, F300CA, LA20_2CA, LA20CA)
    
    if(input$hab == "EST"){
      goodV <- data.frame(
        node = c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
        Cladophora_Adult = c(F319CA,LA3CA,F34DCA,LA8CA,F57CCA,LA11CA, GLENCA, LA13CA,
                             LA14CA,F300CA, LA20_2CA,LA20CA),
        Steelhead_Migration_Burst = c(F319SB,LA3SB,F34DSB,LA8SB,F57CSB,LA11SB, GLENSB, LA13SB,
                                      LA14SB,F300SB, LA20_2SB,LA20SB),
        Steelhead_Migration_Prolonged = c(F319MP,LA3MP,F34DMP,LA8MP,F57CMP,LA11MP, GLENMP, LA13MP,
                                          LA14MP,F300MP, LA20_2MP,LA20MP))
      colnames(goodV)<- c('Node', 'Cladophora Adult', 'Steelhead Migration Burst',
                          'Steelhead Migration Prolonged')
      
    }
    if(input$hab == "WILD"){
      goodV <- data.frame(
        node = c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
        Cladophora_Adult = c(F319CA,LA3CA,F34DCA,LA8CA,F57CCA,LA11CA, GLENCA, LA13CA,
                             LA14CA,F300CA, LA20_2CA,LA20CA),
        Typha_Adult = c(F319TA,LA3TA,F34DTA,LA8TA,F57CTA,LA11TA, GLENTA, LA13TA,
                        LA14TA,F300TA, LA20_2TA,LA20TA),
        Willow_Adult = c(F319WA,LA3WA,F34DWA,LA8WA,F57CWA,LA11WA, GLENWA, LA13WA,
                         LA14WA,F300WA, LA20_2WA,LA20WA)
        )
      colnames(goodV)<- c('Node', 'Cladophora Adult', 'Typha Adult',
                          'Willow Adult')
    }
    if(input$hab == "RARE"){
      goodV <- data.frame(
        node = c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
        SAS_Adult = c(F319AD,LA3AD,F34DAD,LA8AD,F57CAD,LA11AD, GLENAD, LA13AD,
                      LA14AD,F300AD, LA20_2AD,LA20AD),
        Willow_Adult = c(F319WA,LA3WA,F34DWA,LA8WA,F57CWA,LA11WA, GLENWA, LA13WA,
                         LA14WA,F300WA, LA20_2WA,LA20WA),
        Steelhead_Migration_Burst = c(F319SB,LA3SB,F34DSB,LA8SB,F57CSB,LA11SB, GLENSB, LA13SB,
                                      LA14SB,F300SB, LA20_2SB,LA20SB),
        Steelhead_Migration_Prolonged = c(F319MP,LA3MP,F34DMP,LA8MP,F57CMP,LA11MP, GLENMP, LA13MP,
                                          LA14MP,F300MP, LA20_2MP,LA20MP))
      colnames(goodV)<- c('Node', 'SAS Adult', 'Willow Adult',
                          'Steelhead Migration Burst','Steelhead Migration Prolonged')
    }
    if(input$hab == "Recreation"){
      goodV <- data.frame(
        node = c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
        Fishing = c(F319FI,LA3FI,F34DFI,LA8FI,F57CFI,LA11FI, GLENFI, LA13FI,
                    LA14FI,F300FI, LA20_2FI,LA20FI),
        Kayak = c(F319KA,LA3KA,F34DKA,LA8KA,F57CKA,LA11KA, GLENKA, LA13KA,
                  LA14KA,F300KA, LA20_2KA,LA20KA))
      colnames(goodV)<- c('Node', 'Fishing', 'Kayak')
    }
      if(input$hab == "MIGR"){
        goodV <- data.frame(
          node = c("F319","LA3","LA34D", "LA8","F57C",
                   "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
      
          Steelhead_Migration_Burst = c(F319SB,LA3SB,F34DSB,LA8SB,F57CSB,LA11SB, GLENSB, LA13SB,
                                        LA14SB,F300SB, LA20_2SB,LA20SB),
          Steelhead_Migration_Prolonged = c(F319MP,LA3MP,F34DMP,LA8MP,F57CMP,LA11MP, GLENMP, LA13MP,
                                            LA14MP,F300MP, LA20_2MP,LA20MP))
        colnames(goodV)<- c('Node',
                            'Steelhead Migration Burst','Steelhead Migration Prolonged')
      }
    if(input$hab == "SPWN"){
      goodV <- data.frame(
        node = c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
        
        SAS_Adult = c(F319AD,LA3AD,F34DAD,LA8AD,F57CAD,LA11AD, GLENAD, LA13AD,
                      LA14AD,F300AD, LA20_2AD,LA20AD))
      colnames(goodV)<- c('Node', 'SAS Adult')
    }
    if(input$hab == "COLD"){
      goodV <- data.frame(
        node = c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
        
        SAS_Adult = c(F319AD,LA3AD,F34DAD,LA8AD,F57CAD,LA11AD, GLENAD, LA13AD,
                      LA14AD,F300AD, LA20_2AD,LA20AD),
        Steelhead_Migration_Burst = c(F319SB,LA3SB,F34DSB,LA8SB,F57CSB,LA11SB, GLENSB, LA13SB,
                                      LA14SB,F300SB, LA20_2SB,LA20SB),
        Steelhead_Migration_Prolonged = c(F319MP,LA3MP,F34DMP,LA8MP,F57CMP,LA11MP, GLENMP, LA13MP,
                                          LA14MP,F300MP, LA20_2MP,LA20MP))
      colnames(goodV)<- c('Node', 'SAS Adult',
                          'Steelhead Migration Burst','Steelhead Migration Prolonged')
    }
    
    
    
    if(input$hab == "ALL"){
    goodV <- data.frame(
      node = c("F319","LA3","LA34D", "LA8","F57C",
                      "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20"),
      Cladophora_Adult = c(F319CA,LA3CA,F34DCA,LA8CA,F57CCA,LA11CA, GLENCA, LA13CA,
                           LA14CA,F300CA, LA20_2CA,LA20CA),
      Fishing = c(F319FI,LA3FI,F34DFI,LA8FI,F57CFI,LA11FI, GLENFI, LA13FI,
                           LA14FI,F300FI, LA20_2FI,LA20FI),
      Kayak = c(F319KA,LA3KA,F34DKA,LA8KA,F57CKA,LA11KA, GLENKA, LA13KA,
                           LA14KA,F300KA, LA20_2KA,LA20KA),
      SAS_Adult = c(F319AD,LA3AD,F34DAD,LA8AD,F57CAD,LA11AD, GLENAD, LA13AD,
                LA14AD,F300AD, LA20_2AD,LA20AD),
      Steelhead_Migration_Burst = c(F319SB,LA3SB,F34DSB,LA8SB,F57CSB,LA11SB, GLENSB, LA13SB,
                LA14SB,F300SB, LA20_2SB,LA20SB),
      Steelhead_Migration_Prolonged = c(F319MP,LA3MP,F34DMP,LA8MP,F57CMP,LA11MP, GLENMP, LA13MP,
                LA14MP,F300MP, LA20_2MP,LA20MP),
      Typha_Adult = c(F319TA,LA3TA,F34DTA,LA8TA,F57CTA,LA11TA, GLENTA, LA13TA,
                LA14TA,F300TA, LA20_2TA,LA20TA),
      Willow_Adult = c(F319WA,LA3WA,F34DWA,LA8WA,F57CWA,LA11WA, GLENWA, LA13WA,
                LA14WA,F300WA, LA20_2WA,LA20WA)
    )
    colnames(goodV)<- c('Node','Cladophora Adult', 'Fishing', 'Kayak', 'SAS Adult',
                        'Steelhead Migration Burst','Steelhead Migration Prolonged',
                        'Typha Adult', 'Willow Adult')
    }
    
  
    
    goodV$Node <- factor(goodV$Node, levels = goodV$Node)
    
    
    dat3 <- melt(goodV, id.var = 'Node')
    ggplot(dat3, aes(variable, Node)) + geom_tile(aes(fill = value),
                                                    colour = "white") + 
      scale_fill_manual(values=c("firebrick3", "pink2","#d3d3d3", "springgreen4"))+
      theme(text = element_text(size = 15),
            axis.text.x = element_text(angle = 30, hjust = 1))+
      labs(x = "Focal Species", y ="Node") +
      guides(fill=guide_legend(title="Within Flow Range"))+
      theme(legend.title=element_text(size=13))
      
  })
  output$table.output <- renderTable({
   ex_matrix
  })
  
  PopcontentLA20 <- paste(sep = "<br/>",
                          paste0("<img src='LA20_2.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about LA20</b>"),
                          paste0("Los Angeles River within Sepulveda Basin"))
  PopcontentLA11 <- paste(sep = "<br/>",
                          paste0("<img src='LA11.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about LA11</b>"),
                          paste0("Los Angeles River at Glendale Narrows"))
  PopcontentLA20_2 <- paste(sep = "<br/>",
                            paste0("<img src='LA20_2.PNG", "'style=width:300px;height:200px;/>"),
                            paste0("<b>Learn about LA20_2</b>"),
                            paste0("Los Angeles River above Sepulveda Basin"))
  
  PopcontentF34D <- paste(sep = "<br/>",
                          paste0("<img src='F34D.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about F34D</b>"),
                          paste0("Los Angeles River above confluence with Rio Hondo"))
  
  PopcontentF57C <- paste(sep = "<br/>",
                          paste0("<img src='F57C.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about F57C</b>"),
                          paste0("Los Angeles River above confluence with Arroyo Seco"))
  
  PopcontentF300 <- paste(sep = "<br/>",
                          paste0("<img src='F300.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about F300</b>"),
                          paste0("Los Angeles River above confluence with Burbank Channel"))
  
  PopcontentF319 <- paste(sep = "<br/>",
                          paste0("<img src='F319.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about F319</b>"),
                          paste0("Los Angeles River at Wardlow Gage"))
  
  PopcontentGLEN <- paste(sep = "<br/>",
                          paste0("<img src='GLEN.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about GLEN</b>"),
                          paste0("Los Angeles River below Glendale WRP"))
  
  PopcontentLA3 <- paste(sep = "<br/>",
                         paste0("<img src='LA3.jpg", "'style=width:300px;height:200px;/>"),
                         paste0("<b>Learn about LA3</b>"),
                         paste0("Los Angeles River below confluence with Rio Hondo"))
  
  PopcontentLA8 <- paste(sep = "<br/>",
                         paste0("<img src='LA8.jpg", "'style=width:300px;height:200px;/>"),
                         paste0("<b>Learn about LA8</b>"),
                         paste0("Los Angeles River above confluence with Rio Hondo"))
  
  PopcontentLA13 <- paste(sep = "<br/>",
                          paste0("<img src='LA13.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about LA13</b>"),
                          paste0("Los Angeles River above Glendale WRP"))
  PopcontentLA14 <- paste(sep = "<br/>",
                          paste0("<img src='LA14.jpg", "'style=width:300px;height:200px;/>"),
                          paste0("<b>Learn about LA14</b>"),
                          paste0("Los Angeles River below confluence with Burbank Channel"))
 
 
  output$LosAngeles <-  renderLeaflet({
    scen <- which.min(abs(WRP$Med_Q_cfs - input$wrp))
    F300p10Dry <- 0.900017708 + 0.805599066*input$wrp
    F300p90Dry <- 9.236788795 + 0.876596075*input$wrp
    F300p10Wet <- 0.900017708 + 0.805599066*input$wrp
    F300p90Wet <- 9.236788795 + 0.876596075*input$wrp
    
    F319p10Dry <- 47.39457326 + 1.005771489*input$wrp
    F319p90Dry <- 76.45154635 + 1.043169622*input$wrp
    F319p10Wet <- 47.39457326 + 1.005771489*input$wrp
    F319p90Wet <- 76.45154635 + 1.043169622*input$wrp
    
    F57Cp10Dry <- 11.99113979 + 0.907820603*input$wrp
    F57Cp90Dry <- 25.59802376 + 1.046174046*input$wrp
    F57Cp10Wet <-11.99113979 + 0.907820603*input$wrp
    F57Cp90Wet <-25.59802376 + 1.046174046*input$wrp
    
    LA13p10Dry <-6.623349565 + 0.820509704*input$wrp
    LA13p90Dry <-20.28892415 + 0.870686647*input$wrp
    LA13p10Wet <-6.623349565 + 0.820509704*input$wrp
    LA13p90Wet <-20.28892415 + 0.870686647*input$wrp
    
    LA11p10Dry<-11.95112229 + 0.888542946*input$wrp
    LA11p90Dry<-23.76124488 + 1.049193898*input$wrp
    LA11p10Wet<-11.95112229 + 0.888542946*input$wrp
    LA11p90Wet<-23.76124488 + 1.049193898*input$wrp
    
    F34Dp10Dry<-45.94858526 + 0.998084005*input$wrp
    F34Dp90Dry<-71.72411613 + 0.95174529*input$wrp
    F34Dp10Wet<-45.94858526 + 0.998084005*input$wrp
    F34Dp90Wet<-71.72411613 + 0.95174529*input$wrp
    
    LA14p10Dry<-4.912058332 + 0.827809744*input$wrp
    LA14p90Dry<-18.18934302 + 0.88306858*input$wrp
    LA14p10Wet<-4.912058332 + 0.827809744*input$wrp
    LA14p90Wet<-18.18934302 + 0.88306858*input$wrp
    
    GLENp10Dry<-9.885996252 + 0.918687061*input$wrp
    GLENp90Dry<-22.38736668 + 1.048267846*input$wrp
    GLENp10Wet<-9.885996252 + 0.918687061*input$wrp
    GLENp90Wet<-22.38736668 + 1.048267846*input$wrp
    
    
    LA8p10Dry<-27.92521955 + 0.921523533*input$wrp
    LA8p90Dry<-39.67282263 + 1.012418222*input$wrp
    LA8p10Wet<-27.92521955 + 0.921523533*input$wrp
    LA8p90Wet<-39.67282263 + 1.012418222*input$wrp
    
    LA3p10Dry<-46.63140866 + 1.007484302*input$wrp
    LA3p90Dry<-75.29510986 + 1.002334493*input$wrp
    LA3p10Wet<-46.63140866 + 1.007484302*input$wrp
    LA3p90Wet<-75.29510986 + 1.002334493*input$wrp
    
    if(input$season == "wet"){
      fm <- fmWet
      flow <- flowRWet1
      F300p10 <- F300p10Wet
      F300p90 <- F300p90Wet
      
      F319p10 <- F319p10Wet
      F319p90 <- F319p90Wet
      
      F57Cp10 <-F57Cp10Wet
      F57Cp90 <- F57Cp90Wet
      
      LA13p10 <- LA13p10Wet
      LA13p90 <- LA13p90Wet
      
      LA11p10 <- LA11p10Wet
      LA11p90 <- LA11p90Wet
      
      F34Dp10 <- F34Dp10Wet
      F34Dp90 <- F34Dp90Wet
      
      LA14p10 <- LA14p10Wet
      LA14p90 <- LA14p90Wet
      
      GLENp10 <- GLENp10Wet
      GLENp90 <- GLENp90Wet
      
      LA8p10 <- LA8p10Wet
      LA8p90 <- LA8p90Wet
      
      LA3p10 <- LA3p10Wet
      LA3p90 <- LA3p90Wet
      
    }
    else{
      fm <- fmDry
      flow <-flowRDry1
      
      F300p10 <- F300p10Dry
      F300p90 <- F300p90Dry
      
      F319p10 <- F319p10Dry
      F319p90 <- F319p90Dry
      
      F57Cp10 <-F57Cp10Dry
      F57Cp90 <- F57Cp90Dry
      
      LA13p10 <- LA13p10Dry
      LA13p90 <- LA13p90Dry
      
      LA11p10 <- LA11p10Dry
      LA11p90 <- LA11p90Dry
      
      F34Dp10 <- F34Dp10Dry
      F34Dp90 <- F34Dp90Dry
      
      LA14p10 <- LA14p10Dry
      LA14p90 <- LA14p90Dry
      
      GLENp10 <- GLENp10Dry
      GLENp90 <- GLENp90Dry
      
      LA8p10 <- LA8p10Dry
      LA8p90 <- LA8p90Dry
      
      LA3p10 <- LA3p10Dry
      LA3p90 <- LA3p90Dry
    }
    
    row1 <- fm[which(fm$ReportingNode == "F300" & fm$Scenario == scen),]
    F300p <- 50
    
    row2 <- fm[which(fm$ReportingNode == "F319" & fm$Scenario == scen),]
    F319p <- row2$p50
    
    row3 <- fm[which(fm$ReportingNode == "F34D" & fm$Scenario == scen),]
    F34Dp <- row3$p50
    
    
    row4 <- fm[which(fm$ReportingNode == "F37B" & fm$Scenario == scen),]
    F37Bp <- row4$p50
    
    
    row5 <- fm[which(fm$ReportingNode == "F45B" & fm$Scenario == scen),]
    F45Bp <- row5$p50
    
    
    row6 <- fm[which(fm$ReportingNode == "F57C" & fm$Scenario == scen),]
    F57Cp <- row6$p50
    
    
    row7 <- fm[which(fm$ReportingNode == "GLEN" & fm$Scenario == scen),]
    GLENp <- row7$p50
    
    
    row8 <- fm[which(fm$ReportingNode == "LA1" & fm$Scenario == scen),]
    LA1p <- row8$p50
    
    
    row9 <- fm[which(fm$ReportingNode == "LA11" & fm$Scenario == scen),]
    LA11p <- row9$p50
    
    
    row10 <- fm[which(fm$ReportingNode == "LA13" & fm$Scenario == scen),]
    LA13p <- row10$p50
    
    
    row11 <- fm[which(fm$ReportingNode == "LA14" & fm$Scenario == scen),]
    LA14p <- row11$p50
    
    
    row12 <- fm[which(fm$ReportingNode == "LA2" & fm$Scenario == scen),]
    LA2p <- row12$p50
    
    
    row13 <- fm[which(fm$ReportingNode == "LA20_2" & fm$Scenario == scen),]
    LA20_2p<- row13$p50
    
    
    row14 <- fm[which(fm$ReportingNode == "LA20" & fm$Scenario == scen),]
    LA20p <- row14$p50
    
    
    row15 <- fm[which(fm$ReportingNode == "LA3" & fm$Scenario == scen),]
    LA3p <- row15$p50
    
    
    row16 <- fm[which(fm$ReportingNode == "LA8" & fm$Scenario == scen),]
    LA8p <- row16$p50
    
    
    first_c <- c("F319","LA3","LA34D", "LA8","F57C",
                 "LA11", "GLEN","LA13", "LA14", "F300", "LA20_2", "LA20")
    
    
    fowGet <- function(name, species, num, var){
      p10 <- paste(var, "10", sep= "")
      p90 <- paste(var, "90", sep= "")
      
      
      range1 <- flow[which(flow$Species_Label == species & flow$Node == name),]
      range2 <- range1[!is.na(range1$Lower_Limit),]
      range <- range2[!is.na(range1$Upper_Limit),]
      
      p10 <- as.numeric(noquote(p10))
      p90 <- as.numeric(noquote(p90))
      
      
      lower <- as.numeric(noquote(range$Lower_Limit[1]))
      upper <- as.numeric(noquote(range$Upper_Limit[1]))
      
      
      
      if(is.na(lower) | is.na(upper)){
        return(150)
      }
      if(nrow(range) == 0){
        new <- 150
        return(new)
      }
      if(toString(range$Lower_Limit) == "TBD" | toString(range$Upper_Limit) == "TBD"){
        new = 150
        return(new)
      }
      if(as.numeric(p10) > upper){ 
        if(as.numeric(p90) > upper){
          new <- 300
          return(new)
        }
      }
      if(as.numeric(p90) < lower) {
        if(as.numeric(p10) < lower){
          new <- 225
          return(new)
        } 
      }
      else{
        return(0)
      }
    }
    
    
    F300WA <- fowGet("F300", "Willow Adult", 1, F300p)   
    F319WA <-fowGet("F319", "Willow Adult", 2, F319p)
    F34DWA <-fowGet("F34D", "Willow Adult", 3, F34Dp)
    #F37BWA <- fowGet("F37B", "Willow Adult", 4, F37Bp)
    F45BWA <- fowGet("F45B", "Willow Adult", 5, F45Bp)
    F57CWA <- fowGet("F57C", "Willow Adult", 6, F57Cp)
    GLENWA<-fowGet("GLEN", "Willow Adult", 7, GLENp)
    #LA1WA <-fowGet("LA1", "Willow Adult", 8, LA1p)
    LA11WA <-fowGet("LA11", "Willow Adult", 9, LA11p)
    LA13WA <-fowGet("LA13", "Willow Adult", 10, LA13p)
    LA14WA<-fowGet("LA14", "Willow Adult", 11, LA14p)
    #LA2WA<-fowGet("LA2", "Willow Adult", 12, LA2p)
    LA20_2WA<-fowGet("LA20_2", "Willow Adult", 13, LA20_2p)
    LA20WA<-fowGet("LA20", "Willow Adult", 14, LA20p)
    LA3WA<-fowGet("LA3", "Willow Adult", 15, LA3p)
    LA8WA<-fowGet("LA8", "Willow Adult", 16, LA8p)
    
    
    
    F300TA <- fowGet("F300", "Typha Adult", 1, F300p)   
    F319TA <-fowGet("F319", "Typha Adult", 2, F319p)
    F34DTA <-fowGet("F34D", "Typha Adult", 3, F34Dp)
    #F347BTA <- fowGet("F37B", "Typha Adult", 4, F37Bp)
    F45BTA <- fowGet("F45B", "Typha Adult", 5, F45Bp)
    F57CTA <- fowGet("F57C", "Typha Adult", 6, F57Cp)
    GLENTA<-fowGet("GLEN", "Typha Adult", 7, GLENp)
    #LA1TA <-fowGet("LA1", "Typha Adult", 8, LA1p)
    LA11TA <-fowGet("LA11", "Typha Adult", 9,LA11p)
    LA13TA <-fowGet("LA13", "Typha Adult", 10,LA13p)
    LA14TA<-fowGet("LA14", "Typha Adult", 11,LA14p)
    #LA2TA<-fowGet("LA2", "Typha Adult", 12,LA2p)
    LA20_2TA<-fowGet("LA20_2", "Typha Adult", 13,LA20_2p)
    LA20TA<-fowGet("LA20", "Typha Adult", 14,LA20p)
    LA3TA<-fowGet("LA3", "Typha Adult", 15,LA3p)
    LA8TA<-fowGet("LA8", "Typha Adult", 16,LA8p)
    
    
    F300MP <- fowGet("F300", "Steelhead Migration (Prolonged)", 1,F300p)   
    F319MP <-fowGet("F319", "Steelhead Migration (Prolonged)", 2,F319p)
    F34DMP <-fowGet("F34D", "Steelhead Migration (Prolonged)", 3,F34Dp)
    #F347BMP <- fowGet("F37B", "Steelhead Migration (Prolonged)", 4,F37Bp)
    F45BMP <- fowGet("F45B", "Steelhead Migration (Prolonged)", 5,F45Bp)
    F57CMP <- fowGet("F57C", "Steelhead Migration (Prolonged)", 6,F57Cp)
    GLENMP<-fowGet("GLEN", "Steelhead Migration (Prolonged)", 7,GLENp)
    #LA1MP <-fowGet("LA1", "Steelhead Migration (Prolonged)", 8,LA1p)
    LA11MP <-fowGet("LA11", "Steelhead Migration (Prolonged)", 9,LA11p)
    LA13MP <-fowGet("LA13", "Steelhead Migration (Prolonged)", 10,LA13p)
    LA14MP<-fowGet("LA14", "Steelhead Migration (Prolonged)", 11,LA14p)
    #LA2MP<-fowGet("LA2", "Steelhead Migration (Prolonged)", 12,LA2p)
    LA20_2MP<-fowGet("LA20_2", "Steelhead Migration (Prolonged)", 13,LA20_2p)
    LA20MP<-fowGet("LA20", "Steelhead Migration (Prolonged)", 14,LA20p)
    LA3MP<-fowGet("LA3", "Steelhead Migration (Prolonged)", 15,LA3p)
    LA8MP<-fowGet("LA8", "Steelhead Migration (Prolonged)", 16,LA8p)
    

    F300SB <- fowGet("F300", "Steelhead Migration (Burst)", 1,F300p)   
    F319SB <-fowGet("F319", "Steelhead Migration (Burst)", 2,F319p)
    F34DSB <-fowGet("F34D", "Steelhead Migration (Burst)", 3,F34Dp)
    F347BSB <- fowGet("F37B", "Steelhead Migration (Burst)", 4,F37Bp)
    F45BSB <- fowGet("F45B", "Steelhead Migration (Burst)", 5,F45Bp)
    F57CSB <- fowGet("F57C", "Steelhead Migration (Burst)", 6,F57Cp)
    GLENSB<-fowGet("GLEN", "Steelhead Migration (Burst)", 7,GLENp)
    LA1SB <-fowGet("LA1", "Steelhead Migration (Burst)", 8,LA1p)
    LA11SB <-fowGet("LA11", "Steelhead Migration (Burst)", 9,LA11p)
    LA13SB <-fowGet("LA13", "Steelhead Migration (Burst)", 10,LA13p)
    LA14SB<-fowGet("LA14", "Steelhead Migration (Burst)", 11,LA14p)
    LA2SB<-fowGet("LA2", "Steelhead Migration (Burst)", 12,LA2p)
    LA20_2SB<-fowGet("LA20_2", "Steelhead Migration (Burst)", 13,LA20_2p)
    LA20SB<-fowGet("LA20", "Steelhead Migration (Burst)", 14,LA20p)
    LA3SB<-fowGet("LA3", "Steelhead Migration (Burst)", 15,LA3p)
    LA8SB<-fowGet("LA8", "Steelhead Migration (Burst)", 16,LA8p)
    
    
    F300AD <- fowGet("F300", "SAS Adult", 1,F300p)   
    F319AD <-fowGet("F319", "SAS Adult", 2,F319p)
    F34DAD <-fowGet("F34D", "SAS Adult", 3,F34Dp)
    F347BAD <- fowGet("F37B", "SAS Adult", 4,F37Bp)
    F45BAD <- fowGet("F45B", "SAS Adult", 5,F45Bp)
    F57CAD <- fowGet("F57C", "SAS Adult", 6,F57Cp)
    GLENAD<-fowGet("GLEN", "SAS Adult", 7,GLENp)
    LA1AD <-fowGet("LA1", "SAS Adult", 8,LA1p)
    LA11AD <-fowGet("LA11", "SAS Adult", 9,LA11p)
    LA13AD <-fowGet("LA13", "SAS Adult", 10,LA13p)
    LA14AD<-fowGet("LA14", "SAS Adult", 11,LA14p)
    LA2AD<-fowGet("LA2", "SAS Adult", 12,LA2p)
    LA20_2AD<-fowGet("LA20_2", "SAS Adult", 13,LA20_2p)
    LA20AD<-fowGet("LA20", "SAS Adult", 14,LA20p)
    LA3AD<-fowGet("LA3", "SAS Adult", 15,LA3p)
    LA8AD<-fowGet("LA8", "SAS Adult", 16,LA8p)
    
    
    F300KA <- fowGet("F300", "Rec. Use Kayak", 1,F300p)   
    F319KA <-fowGet("F319", "Rec. Use Kayak", 2,F319p)
    F34DKA <-fowGet("F34D", "Rec. Use Kayak", 3,F34Dp)
    F347BKA <- fowGet("F37B", "Rec. Use Kayak", 4,F37Bp)
    F45BKA <- fowGet("F45B", "Rec. Use Kayak", 5,F45Bp)
    F57CKA <- fowGet("F57C", "Rec. Use Kayak", 6,F57Cp)
    GLENKA<-fowGet("GLEN", "Rec. Use Kayak", 7,GLENp)
    LA1KA <-fowGet("LA1", "Rec. Use Kayak", 8,LA1p)
    LA11KA <-fowGet("LA11", "Rec. Use Kayak", 9,LA11p)
    LA13KA <-fowGet("LA13", "Rec. Use Kayak", 10,LA13p)
    LA14KA<-fowGet("LA14", "Rec. Use Kayak", 11,LA14p)
    LA2KA<-fowGet("LA2", "Rec. Use Kayak", 12,LA2p)
    LA20_2KA<-fowGet("LA20_2", "Rec. Use Kayak", 13,LA20_2p)
    LA20KA<-fowGet("LA20", "Rec. Use Kayak", 14,LA20p)
    LA3KA<-fowGet("LA3", "Rec. Use Kayak", 15,LA3p)
    LA8KA<-fowGet("LA8", "Rec. Use Kayak", 16,LA8p)
    
    F300FI <- fowGet("F300", "Rec. Use Fishing", 1,F300p)   
    F319FI <-fowGet("F319", "Rec. Use Fishing", 2,F319p)
    F34DFI <-fowGet("F34D", "Rec. Use Fishing", 3,F34Dp)
    F347BFI <- fowGet("F37B", "Rec. Use Fishing", 4,F37Bp)
    F45BFI <- fowGet("F45B", "Rec. Use Fishing", 5,F45Bp)
    F57CFI <- fowGet("F57C", "Rec. Use Fishing", 6,F57Cp)
    GLENFI<-fowGet("GLEN", "Rec. Use Fishing", 7,GLENp)
    LA1FI <-fowGet("LA1", "Rec. Use Fishing", 8,LA1p)
    LA11FI <-fowGet("LA11", "Rec. Use Fishing", 9,LA11p)
    LA13FI <-fowGet("LA13", "Rec. Use Fishing", 10,LA13p)
    LA14FI<-fowGet("LA14", "Rec. Use Fishing", 11,LA14p)
    LA2FI<-fowGet("LA2", "Rec. Use Fishing", 12,LA2p)
    LA20_2FI<-fowGet("LA20_2", "Rec. Use Fishing", 13,LA20_2p)
    LA20FI<-fowGet("LA20", "Rec. Use Fishing", 14,LA20p)
    LA3FI<-fowGet("LA3", "Rec. Use Fishing", 15,LA3p)
    LA8FI<-fowGet("LA8", "Rec. Use Kayak", 16,LA8p)
  
  
    F300CA <- fowGet("F300", "Cladophora Adult", 1,F300p)   
    F319CA <-fowGet("F319", "Cladophora Adult", 2,F319p)
    F34DCA <-fowGet("F34D", "Cladophora Adult", 3,F34Dp)
    F347BCA <- fowGet("F37B", "Cladophora Adult", 4,F37Bp)
    F45BCA <- fowGet("F45B", "Cladophora Adult", 5,F45Bp)
    F57CCA <- fowGet("F57C", "Cladophora Adult", 6,F57Cp)
    GLENCA<-fowGet("GLEN", "Cladophora Adult", 7,GLENp)
    LA1CA <-fowGet("LA1", "Cladophora Adult", 8,LA1p)
    LA11CA <-fowGet("LA11", "Cladophora Adult", 9,LA11p)
    LA13CA <-fowGet("LA13", "Cladophora Adult", 10,LA13p)
    LA14CA<-fowGet("LA14", "Cladophora Adult", 11,LA14p)
    LA2CA<-fowGet("LA2", "Cladophora Adult", 12,LA2p)
    LA20_2CA<-fowGet("LA20_2", "Cladophora Adult", 13,LA20_2p)
    LA20CA<-fowGet("LA20", "Cladophora Adult", 14,LA20p)
    LA3CA<-fowGet("LA3", "Cladophora Adult", 15,LA3p)
    LA8CA<-fowGet("LA8", "Cladophora Adult", 16,LA8p)
    if(input$use == "Cladophora Adult"){
      aCol <- ifelse(LA20CA == 0,"#006400", "red")
      aCol <- ifelse(LA20CA == 150,"grey", aCol)
      bCol <- ifelse(LA20_2CA == 0, "#006400", "red")
      bCol <- ifelse(LA20_2CA == 150, "grey", bCol)
      cCol <- ifelse(F300CA == 0,"#006400",  "red")
      cCol <- ifelse(F300CA == 150,"grey",  cCol)
      dCol <- ifelse(LA14CA == 0, "#006400", "red")
      dCol <- ifelse(LA14CA == 150, "grey", dCol)
      eCol <-  ifelse(LA13CA == 0, "#006400", "red")
      eCol <-  ifelse(LA13CA == 150, "grey", eCol)
      fCol <-  ifelse(GLENCA == 0, "#006400", "red")
      fCol <-  ifelse(GLENCA == 150, "grey", fCol)
      gCol <-  ifelse(LA11CA == 0, "#006400", "red")
      gCol <-  ifelse(LA11CA == 150, "grey", gCol)
      hCol <- ifelse(F57CCA == 0, "#006400", "red")
      hCol <- ifelse(F57CCA == 150, "grey", hCol)
      iCol <- ifelse(LA8CA == 0, "#006400", "red")
      iCol <- ifelse(LA8CA == 150, "grey", iCol)
      jCol <- ifelse(F34DCA == 0, "#006400", "red")
      jCol <- ifelse(F34DCA == 150, "grey", jCol)
      kCol <- ifelse(LA3CA == 0, "#006400", "red")
      kCol <- ifelse(LA3CA == 150, "grey", kCol)
      lCol <- ifelse(F319CA == 0, "#006400", "red")
      lCol <- ifelse(F319CA == 150, "grey", lCol)
      
    }
    if(input$use == "Fishing"){
      aCol <- ifelse(LA20FI == 0,"#006400", "red")
      aCol <- ifelse(LA20FI == 150,"grey", aCol)
      bCol <- ifelse(LA20_2FI == 0, "#006400", "red")
      bCol <- ifelse(LA20_2FI == 150, "grey", bCol)
      cCol <- ifelse(F300FI == 0,"#006400",  "red")
      cCol <- ifelse(F300FI == 150,"grey",  cCol)
      dCol <- ifelse(LA14FI == 0, "#006400", "red")
      dCol <- ifelse(LA14FI == 150, "grey", dCol)
      eCol <-  ifelse(LA13FI == 0, "#006400", "red")
      eCol <-  ifelse(LA13FI == 150, "grey", eCol)
      fCol <-  ifelse(GLENFI == 0, "#006400", "red")
      fCol <-  ifelse(GLENFI == 150, "grey", fCol)
      gCol <-  ifelse(LA11FI == 0, "#006400", "red")
      gCol <-  ifelse(LA11FI == 150, "grey", gCol)
      hCol <- ifelse(F57CFI == 0, "#006400", "red")
      hCol <- ifelse(F57CFI == 150, "grey", hCol)
      iCol <- ifelse(LA8FI == 0, "#006400", "red")
      iCol <- ifelse(LA8FI == 150, "grey", iCol)
      jCol <- ifelse(F34DFI == 0, "#006400", "red")
      jCol <- ifelse(F34DFI == 150, "grey", jCol)
      kCol <- ifelse(LA3FI == 0, "#006400", "red")
      kCol <- ifelse(LA3FI == 150, "grey", kCol)
      lCol <- ifelse(F319FI == 0, "#006400", "red")
      lCol <- ifelse(F319FI == 150, "grey", lCol)
    }
    if(input$use == "Willow Adult"){
      aCol <- ifelse(LA20WA == 0,"#006400", "red")
      aCol <- ifelse(LA20WA == 150,"grey", aCol)
      bCol <- ifelse(LA20_2WA == 0, "#006400", "red")
      bCol <- ifelse(LA20_2WA == 150, "grey", bCol)
      cCol <- ifelse(F300WA == 0,"#006400",  "red")
      cCol <- ifelse(F300WA == 150,"grey",  cCol)
      dCol <- ifelse(LA14WA == 0, "#006400", "red")
      dCol <- ifelse(LA14WA == 150, "grey", dCol)
      eCol <-  ifelse(LA13WA == 0, "#006400", "red")
      eCol <-  ifelse(LA13WA == 150, "grey", eCol)
      fCol <-  ifelse(GLENWA == 0, "#006400", "red")
      fCol <-  ifelse(GLENWA == 150, "grey", fCol)
      gCol <-  ifelse(LA11WA == 0, "#006400", "red")
      gCol <-  ifelse(LA11WA == 150, "grey", gCol)
      hCol <- ifelse(F57CWA == 0, "#006400", "red")
      hCol <- ifelse(F57CWA == 150, "grey", hCol)
      iCol <- ifelse(LA8WA == 0, "#006400", "red")
      iCol <- ifelse(LA8WA == 150, "grey", iCol)
      jCol <- ifelse(F34DWA == 0, "#006400", "red")
      jCol <- ifelse(F34DWA == 150, "grey", jCol)
      kCol <- ifelse(LA3WA == 0, "#006400", "red")
      kCol <- ifelse(LA3WA == 150, "grey", kCol)
      lCol <- ifelse(F319WA == 0, "#006400", "red")
      lCol <- ifelse(F319WA == 150, "grey", lCol)
    }
    if(input$use == "Steelhead Migration (Prolonged)"){
      aCol <- ifelse(LA20MP == 0,"#006400", "red")
      aCol <- ifelse(LA20MP == 150,"grey", aCol)
      bCol <- ifelse(LA20_2MP == 0, "#006400", "red")
      bCol <- ifelse(LA20_2MP == 150, "grey", bCol)
      cCol <- ifelse(F300MP == 0,"#006400",  "red")
      cCol <- ifelse(F300MP == 150,"grey",  cCol)
      dCol <- ifelse(LA14MP == 0, "#006400", "red")
      dCol <- ifelse(LA14MP == 150, "grey", dCol)
      eCol <-  ifelse(LA13MP == 0, "#006400", "red")
      eCol <-  ifelse(LA13MP == 150, "grey", eCol)
      fCol <-  ifelse(GLENMP == 0, "#006400", "red")
      fCol <-  ifelse(GLENMP == 150, "grey", fCol)
      gCol <-  ifelse(LA11MP == 0, "#006400", "red")
      gCol <-  ifelse(LA11MP == 150, "grey", gCol)
      hCol <- ifelse(F57CMP == 0, "#006400", "red")
      hCol <- ifelse(F57CMP == 150, "grey", hCol)
      iCol <- ifelse(LA8MP == 0, "#006400", "red")
      iCol <- ifelse(LA8MP == 150, "grey", iCol)
      jCol <- ifelse(F34DMP == 0, "#006400", "red")
      jCol <- ifelse(F34DMP == 150, "grey", jCol)
      kCol <- ifelse(LA3MP == 0, "#006400", "red")
      kCol <- ifelse(LA3MP == 150, "grey", kCol)
      lCol <- ifelse(F319MP == 0, "#006400", "red")
      lCol <- ifelse(F319MP == 150, "grey", lCol)
    }
    if(input$use == "Typha Adult"){
      aCol <- ifelse(LA20TA == 0,"#006400", "red")
      aCol <- ifelse(LA20TA == 150,"grey", aCol)
      bCol <- ifelse(LA20_2TA == 0, "#006400", "red")
      bCol <- ifelse(LA20_2TA == 150, "grey", bCol)
      cCol <- ifelse(F300TA == 0,"#006400",  "red")
      cCol <- ifelse(F300TA == 150,"grey",  cCol)
      dCol <- ifelse(LA14TA == 0, "#006400", "red")
      dCol <- ifelse(LA14TA == 150, "grey", dCol)
      eCol <-  ifelse(LA13TA == 0, "#006400", "red")
      eCol <-  ifelse(LA13TA == 150, "grey", eCol)
      fCol <-  ifelse(GLENTA == 0, "#006400", "red")
      fCol <-  ifelse(GLENTA == 150, "grey", fCol)
      gCol <-  ifelse(LA11TA == 0, "#006400", "red")
      gCol <-  ifelse(LA11TA == 150, "grey", gCol)
      hCol <- ifelse(F57CTA == 0, "#006400", "red")
      hCol <- ifelse(F57CTA == 150, "grey", hCol)
      iCol <- ifelse(LA8TA == 0, "#006400", "red")
      iCol <- ifelse(LA8TA == 150, "grey", iCol)
      jCol <- ifelse(F34DTA == 0, "#006400", "red")
      jCol <- ifelse(F34DTA == 150, "grey", jCol)
      kCol <- ifelse(LA3TA == 0, "#006400", "red")
      kCol <- ifelse(LA3TA == 150, "grey", kCol)
      lCol <- ifelse(F319TA == 0, "#006400", "red")
      lCol <- ifelse(F319TA == 150, "grey", lCol)
    }
    if(input$use == "SAS Adult"){
      aCol <- ifelse(LA20AD == 0,"#006400", "red")
      aCol <- ifelse(LA20AD == 150,"grey", aCol)
      bCol <- ifelse(LA20_2AD == 0, "#006400", "red")
      bCol <- ifelse(LA20_2AD == 150, "grey", bCol)
      cCol <- ifelse(F300AD == 0,"#006400",  "red")
      cCol <- ifelse(F300AD == 150,"grey",  cCol)
      dCol <- ifelse(LA14AD == 0, "#006400", "red")
      dCol <- ifelse(LA14AD == 150, "grey", dCol)
      eCol <-  ifelse(LA13AD == 0, "#006400", "red")
      eCol <-  ifelse(LA13AD == 150, "grey", eCol)
      fCol <-  ifelse(GLENAD == 0, "#006400", "red")
      fCol <-  ifelse(GLENAD == 150, "grey", fCol)
      gCol <-  ifelse(LA11AD == 0, "#006400", "red")
      gCol <-  ifelse(LA11AD == 150, "grey", gCol)
      hCol <- ifelse(F57CAD == 0, "#006400", "red")
      hCol <- ifelse(F57CAD == 150, "grey", hCol)
      iCol <- ifelse(LA8AD == 0, "#006400", "red")
      iCol <- ifelse(LA8AD == 150, "grey", iCol)
      jCol <- ifelse(F34DAD == 0, "#006400", "red")
      jCol <- ifelse(F34DAD == 150, "grey", jCol)
      kCol <- ifelse(LA3AD == 0, "#006400", "red")
      kCol <- ifelse(LA3AD == 150, "grey", kCol)
      lCol <- ifelse(F319AD == 0, "#006400", "red")
      lCol <- ifelse(F319AD == 150, "grey", lCol)
    }
    if(input$use == "Steelhead Migration (Burst)"){
      aCol <- ifelse(LA20SB == 0,"#006400", "red")
      aCol <- ifelse(LA20SB == 150,"grey", aCol)
      bCol <- ifelse(LA20_2SB == 0, "#006400", "red")
      bCol <- ifelse(LA20_2SB == 150, "grey", bCol)
      cCol <- ifelse(F300SB == 0,"#006400",  "red")
      cCol <- ifelse(F300SB == 150,"grey",  cCol)
      dCol <- ifelse(LA14SB == 0, "#006400", "red")
      dCol <- ifelse(LA14SB == 150, "grey", dCol)
      eCol <-  ifelse(LA13SB == 0, "#006400", "red")
      eCol <-  ifelse(LA13SB == 150, "grey", eCol)
      fCol <-  ifelse(GLENSB == 0, "#006400", "red")
      fCol <-  ifelse(GLENSB == 150, "grey", fCol)
      gCol <-  ifelse(LA11SB == 0, "#006400", "red")
      gCol <-  ifelse(LA11SB == 150, "grey", gCol)
      hCol <- ifelse(F57CSB == 0, "#006400", "red")
      hCol <- ifelse(F57CSB == 150, "grey", hCol)
      iCol <- ifelse(LA8SB == 0, "#006400", "red")
      iCol <- ifelse(LA8SB == 150, "grey", iCol)
      jCol <- ifelse(F34DSB == 0, "#006400", "red")
      jCol <- ifelse(F34DSB == 150, "grey", jCol)
      kCol <- ifelse(LA3SB == 0, "#006400", "red")
      kCol <- ifelse(LA3SB == 150, "grey", kCol)
      lCol <- ifelse(F319SB == 0, "#006400", "red")
      lCol <- ifelse(F319SB == 150, "grey", lCol)
    }
    if(input$use == "Kayak"){
      aCol <- ifelse(LA20KA == 0,"#006400", "red")
      aCol <- ifelse(LA20KA == 150,"grey", aCol)
      bCol <- ifelse(LA20_2KA == 0, "#006400", "red")
      bCol <- ifelse(LA20_2KA == 150, "grey", bCol)
      cCol <- ifelse(F300KA == 0,"#006400",  "red")
      cCol <- ifelse(F300KA == 150,"grey",  cCol)
      dCol <- ifelse(LA14KA == 0, "#006400", "red")
      dCol <- ifelse(LA14KA == 150, "grey", dCol)
      eCol <-  ifelse(LA13KA == 0, "#006400", "red")
      eCol <-  ifelse(LA13KA == 150, "grey", eCol)
      fCol <-  ifelse(GLENKA == 0, "#006400", "red")
      fCol <-  ifelse(GLENKA == 150, "grey", fCol)
      gCol <-  ifelse(LA11KA == 0, "#006400", "red")
      gCol <-  ifelse(LA11KA == 150, "grey", gCol)
      hCol <- ifelse(F57CKA == 0, "#006400", "red")
      hCol <- ifelse(F57CKA == 150, "grey", hCol)
      iCol <- ifelse(LA8KA == 0, "#006400", "red")
      iCol <- ifelse(LA8KA == 150, "grey", iCol)
      jCol <- ifelse(F34DKA == 0, "#006400", "red")
      jCol <- ifelse(F34DKA == 150, "grey", jCol)
      kCol <- ifelse(LA3KA == 0, "#006400", "red")
      kCol <- ifelse(LA3KA == 150, "grey", kCol)
      lCol <- ifelse(F319KA == 0, "#006400", "red")
      lCol <- ifelse(F319KA == 150, "grey", lCol)
    }
    LosAngeles <- leaflet(options = leafletOptions( 
      minZoom = 8,     maxZoom = 16, zoomControl = FALSE )) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)}") %>%
      setView(lng = -118.2437, lat =34.0522, zoom = 10)%>%
      addTiles(options = providerTileOptions(opacity = 1)) %>%
      addCircleMarkers(lng=-118.247706, lat=34.108175, radius = 11,
                       opacity = 1, color = gCol, 
                       popup=PopcontentLA11)%>% #MarkerHome
      addCircleMarkers(lng=-118.514642, lat=34.184708, radius = 11, 
                       color =  aCol, opacity = 1,popup=PopcontentLA20) %>%
      addCircleMarkers(lng=-118.484394, lat=34.173175, radius = 11, 
                       color =  bCol,opacity = 1,popup=PopcontentLA20_2)%>%
      addCircleMarkers(lng=-118.173592, lat=33.948297, radius = 11, 
                       color =  jCol,opacity = 1,popup=PopcontentF34D) %>%
      addCircleMarkers(lng=-118.228233, lat=34.085803, radius = 11, 
                       color =  hCol,opacity = 1,popup=PopcontentF57C) %>%
      addCircleMarkers(lng=-118.275339, lat=34.136858, radius = 11, 
                       color =  fCol,opacity = 1,popup=PopcontentGLEN) %>%
      addCircleMarkers(lng=-118.196333, lat=33.8639, radius = 11, 
                       color =  kCol,opacity = 1,popup=PopcontentLA3) %>%
      addCircleMarkers(lng=-118.228603, lat=34.040528, radius = 11, 
                       color =  iCol,opacity = 1,popup=PopcontentLA8) %>%
      addCircleMarkers(lng=-118.299758, lat=34.156839, radius = 11, 
                       color =  dCol,opacity = 1,popup=PopcontentLA14) %>%
      addCircleMarkers(lng=-118.205728, lat=33.817733, radius = 11, 
                       color =  lCol,opacity = 1,popup=PopcontentF319) %>%
      addCircleMarkers(lng=-118.378453, lat=34.1411, radius = 11, 
                       color =  cCol,opacity = 1,popup=PopcontentF300) %>%
      addCircleMarkers(lng=-118.278844, lat=34.150964, radius = 11, 
                       color =  eCol,opacity = 1,popup=PopcontentLA13) %>%
      addPolylines(data=river,weight=4,col = 'blue')
    
  
    
  })
  
  output$p10 <- renderPlot({
    
    x = 0:100
    F300p10Dry <- 0.900017708 + 0.805599066*x
    print(x)
    F300p10Wet <- 0.900017708 + 0.805599066*x
    
    F319p10Dry <- 47.39457326 + 1.005771489*x
    F319p10Wet <- 47.39457326 + 1.005771489*x
    
    F57Cp10Dry <- 11.99113979 + 0.907820603*x
    F57Cp10Wet <-11.99113979 + 0.907820603*x
    
    LA13p10Dry <-6.623349565 + 0.820509704*x
    LA13p10Wet <-6.623349565 + 0.820509704*x
    
    LA11p10Dry<-11.95112229 + 0.888542946*x
    LA11p10Wet<-11.95112229 + 0.888542946*x
    
    F34Dp10Dry<-45.94858526 + 0.998084005*x
    F34Dp10Wet<-45.94858526 + 0.998084005*x
    
    LA14p10Dry<-4.912058332 + 0.827809744*x
    LA14p10Wet<-4.912058332 + 0.827809744*x
    
    GLENp10Dry<-9.885996252 + 0.918687061*x
    GLENp10Wet<-9.885996252 + 0.918687061*x
    
    
    LA8p10Dry<-27.92521955 + 0.921523533*x
    LA8p10Wet<-27.92521955 + 0.921523533*x
    
    LA3p10Dry<-46.63140866 + 1.007484302*x
    LA3p10Wet<-46.63140866 + 1.007484302*x
    F300p50Dry <- 7.515974555 + 0.818915631*x
    F300p50Wet <- 7.515974555 + 0.818915631*x
    
    F319p50Dry <- 59.7864079 + 0.968084353*x
    F319p50Wet <- 59.7864079 + 0.968084353*x
    
    F57Cp50Dry <- 19.36427883 + 1.019596427*x
    F57Cp50Wet <-19.36427883 + 1.019596427*x
    
    LA13p50Dry <-12.49375321 + 0.895458007*x
    LA13p50Wet <-12.49375321 + 0.895458007*x
    
    LA11p50Dry<-17.56386445 + 1.017568674*x
    LA11p50Wet<-17.56386445 + 1.017568674*x
    
    F34Dp50Dry<-55.60826969 + 0.946218349*x
    F34Dp50Wet<-55.60826969 + 0.946218349*x
    
    LA14p50Dry<-10.53879805 + 0.902100964*x
    LA14p50Wet<-10.53879805 + 0.902100964*x
    
    GLENp50Dry<-16.06509232 + 1.016729283*x
    GLENp50Wet<-16.06509232 + 1.016729283*x
    
    
    LA8p50Dry<-33.05435573 + 0.98455202*x
    LA8p50Wet<-33.05435573 + 0.98455202*x
    
    LA3p50Dry<-59.08826412 + 0.970926122*x
    LA3p50Wet<-59.08826412 + 0.970926122*x
    F300p90Dry <- 9.236788795 + 0.876596075*x
    F300p90Wet <- 9.236788795 + 0.876596075*x
    
    F319p90Dry <- 76.45154635 + 1.043169622*x
    F319p90Wet <- 76.45154635 + 1.043169622*x
    
    F57Cp90Dry <- 25.59802376 + 1.046174046*x
    F57Cp90Wet <-25.59802376 + 1.046174046*x
    
    LA13p90Dry <-20.28892415 + 0.870686647*x
    LA13p90Wet <-20.28892415 + 0.870686647*x
    
    LA11p90Dry<-23.76124488 + 1.049193898*x
    LA11p90Wet<-23.76124488 + 1.049193898*x
    
    F34Dp90Dry<-71.72411613 + 0.95174529*x
    F34Dp90Wet<-71.72411613 + 0.95174529*x
    
    LA14p90Dry<-18.18934302 + 0.88306858*x
    LA14p90Wet<-18.18934302 + 0.88306858*x
    
    GLENp90Dry<-22.38736668 + 1.048267846*x
    GLENp90Wet<-22.38736668 + 1.048267846*x
    LA8p90Dry<-39.67282263 + 1.012418222*x
    LA8p90Wet<-39.67282263 + 1.012418222*x
    LA3p90Dry<-75.29510986 + 1.002334493*x
    LA3p90Wet<-75.29510986 + 1.002334493*x
    
    
    
    if(input$season2 == "wet"){
      fm <- fmWet
      flow <- flowRWet1
      F300p10 <- F300p10Wet
      F319p10 <- F319p10Wet
      F57Cp10 <-F57Cp10Wet
      LA13p10 <- LA13p10Wet
      LA11p10 <- LA11p10Wet
      F34Dp10 <- F34Dp10Wet
      LA14p10 <- LA14p10Wet
      GLENp10 <- GLENp10Wet
      LA8p10 <- LA8p10Wet
      LA3p10 <- LA3p10Wet
      F300p50 <- F300p50Wet
      F319p50 <- F319p50Wet
      F57Cp50 <-F57Cp50Wet
      LA13p50 <- LA13p50Wet
      LA11p50 <- LA11p50Wet
      F34Dp50 <- F34Dp50Wet
      LA14p50 <- LA14p50Wet
      GLENp50 <- GLENp50Wet
      LA8p50 <- LA8p50Wet
      LA3p50 <- LA3p50Wet
      F300p90 <- F300p90Wet
      
      F319p90 <- F319p90Wet
      
      F57Cp90 <- F57Cp90Wet
      
      
      LA13p90 <- LA13p90Wet
      
      
      LA11p90 <- LA11p90Wet
      
      
      F34Dp90 <- F34Dp90Wet
      
      
      LA14p90 <- LA14p90Wet
      
      GLENp90 <- GLENp90Wet
      
      
      LA8p90 <- LA8p90Wet
      
      
      LA3p90 <- LA3p90Wet
      
    }
    else{
      fm <- fmDry
      flow <-flowRDry1
      F300p10 <- F300p10Dry
      F319p10 <- F319p10Dry
      F57Cp10 <-F57Cp10Dry
      LA13p10 <- LA13p10Dry
      LA11p10 <- LA11p10Dry
      F34Dp10 <- F34Dp10Dry
      LA14p10 <- LA14p10Dry
      GLENp10 <- GLENp10Dry
      LA8p10 <- LA8p10Dry
      LA3p10 <- LA3p10Dry
      F300p50 <- F300p50Dry
      F319p50 <- F319p50Dry
      F57Cp50 <-F57Cp50Dry
      LA13p50 <- LA13p50Dry
      LA11p50 <- LA11p50Dry
      F34Dp50 <- F34Dp50Dry
      LA14p50 <- LA14p50Dry
      GLENp50 <- GLENp50Dry
      LA8p50 <- LA8p50Dry
      LA3p50 <- LA3p50Dry
      F300p90 <- F300p90Dry
      
      F319p90 <- F319p90Dry
      
      F57Cp90 <- F57Cp90Dry
      
      LA13p90 <- LA13p90Dry
      
      LA11p90 <- LA11p90Dry
      
      F34Dp90 <- F34Dp90Dry
      
      LA14p90 <- LA14p90Dry
      
      GLENp90 <- GLENp90Dry
      
      LA8p90 <- LA8p90Dry
      
      LA3p90 <- LA3p90Dry
    }
    name = "red"
    fun.1 <- function(x) x^2 + x
      if(input$node == "LA11"){
        fun.1 <- LA11p10
        name = "LA11"
      }
    if(input$node == "F300"){
      fun.1 <- F300p10
      name = "F300"
    }
    if(input$node == "F319"){
      fun.1 <- F319p10
      name = "F319"
    }
    if(input$node == "LA13"){
      fun.1 <- LA13p10
      name = "LA13"
    }
    if(input$node == "F34D"){
      fun.1 <- F34Dp10
      name = "F34D"
    }
    if(input$node == "LA14"){
      fun.1 <- LA14p10
      name = "LA14"
    }
    if(input$node == "GLEN"){
      fun.1 <- GLENp10
      name = "GLEN"
    }
    if(input$node == "LA8"){
      fun.1 <- LA8p10
      name = "LA8"
    }
    if(input$node == "LA3"){
      fun.1 <- LA3p10
      name = "LA3"
    }
    if(input$node == "F57C"){
      fun.1 <- F57Cp10
      name = "F57C"
    }
    if(input$node == "LA20"){
      fun.1 <- F57Cp10
      name = "F57C"
    }
    if(input$node == "LA20_2"){
      fun.1 <- F57Cp10
      name = "F57C"
    }
    fun.2 <- function(x) x^2 + x
    if(input$node == "LA11"){
      fun.2 <- LA11p50
      name = "LA11"
    }
    if(input$node == "F300"){
      fun.2 <- F300p50
      name = "F300"
    }
    if(input$node == "F319"){
      fun.2 <- F319p50
      name = "F319"
    }
    if(input$node == "LA13"){
      fun.2 <- LA13p50
      name = "LA13"
    }
    if(input$node == "F34D"){
      fun.2 <- F34Dp50
      name = "F34D"
    }
    if(input$node == "LA14"){
      fun.2 <- LA14p50
      name = "LA14"
    }
    if(input$node == "GLEN"){
      fun.2 <- GLENp50
      name = "GLEN"
    }
    if(input$node == "LA8"){
      fun.2 <- LA8p50
      name = "LA8"
    }
    if(input$node == "LA3"){
      fun.2 <- LA3p50
      name = "LA3"
    }
    if(input$node == "F57C"){
      fun.2 <- F57Cp50
      name = "F57C"
    }
    fun.3 <- function(x) x^2 + x
    if(input$node == "LA11"){
      fun.3 <- LA11p90
      name = "LA11"
    }
    if(input$node == "F300"){
      fun.3 <- F300p90
      name = "F300"
    }
    if(input$node == "F319"){
      fun.3 <- F319p90
      name = "F319"
    }
    if(input$node == "LA13"){
      fun.3 <- LA13p90
      name = "LA13"
    }
    if(input$node == "F34D"){
      fun.3 <- F34Dp90
      name = "F34D"
    }
    if(input$node == "LA14"){
      fun.3 <- LA14p90
      name = "LA14"
    }
    if(input$node == "GLEN"){
      fun.3 <- GLENp90
      name = "GLEN"
    }
    if(input$node == "LA8"){
      fun.3 <- LA8p90
      name = "LA8"
    }
    if(input$node == "LA3"){
      fun.3 <- LA3p90
      name = "LA3"
    }
    if(input$node == "F57C"){
      fun.3 <- F57Cp90
      name = "F57C"
    }
    aData = data.frame(x =x, y = fun.1)
    bData = data.frame(x=x, y = fun.2)
    cData = data.frame(x=x, y = fun.3)
    plotter <- data.frame(
      x = 0:100,
      p10 = fun.1,
      p50 = fun.2,
      p90 = fun.3
    )
    
    ggplot(plotter, aes(x=x)) + 
      geom_line(aes(y = p50, colour="50th Percentile"),size = 1, linetype = 1, color = "blue") +
      labs(x = "Average Annual Season WRP Discharge (cfs)", y = "Season Baseflow Magnitude (cfs)")+
      ylim(c(0,175)) +
      theme(axis.text = element_text(size = 12),
            axis.title=element_text(size=14,face="bold"),
            legend.text = element_text(size = 15))+
      scale_colour_manual(values = c("blue"))+
      geom_ribbon(data =plotter,
                  aes(ymin=p10,ymax=p90), fill = 'grey', alpha = .7)+
      ggtitle("10th - 90th Percentile")
      
    
  })
}




shinyApp(ui, server)

