library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(rsconnect)

library(dplyr)
library(tidyr)
library(readr)
library(geosphere)
library(zoo)
library(DT)
library(janitor)

#library(ggplot2)
library(leaflet)
library(plotly)

#load local life history sheet
life.history<- read_csv("Fish Life History.csv", col_names = TRUE) %>%
  mutate(a=as.numeric(a)) %>%
  mutate(b=as.numeric(b)) %>%
  mutate(aLL = ifelse(is.na(aLL),"0",aLL)) %>%
  mutate(bLL = ifelse(is.na(bLL),"1",bLL)) %>%
  mutate(bLL=as.numeric(bLL)) %>%
  mutate(aLL=as.numeric(aLL)) %>%
  mutate(FB.Max=FB.Length_MAX) %>%
  mutate(FB.Max=as.numeric(FB.Max)) %>%
  select(c(Genus_species, aLL, bLL, a, b, FB.Max)) 

#load local species region list
species.region <- read_csv("Species_list_All_regions.csv", col_names = TRUE)

options(shiny.maxRequestSize = 30 * 1024^2)

ui <- dashboardPage(
  
  # title ----
  dashboardHeader(title = "EM Tools ROV"),
  
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Tidying Raw USBL Logs", tabName = "USBL"),
                menuItem("Raw Data Check", tabName = "checks"),
                menuItem("Formatting Data", tabName = "format"),
                menuItem("Adding Biomass", tabName = "biomass")
                
    )
  ),
  
  # body ----
  dashboardBody(
    tabItems(
      
      ########################
      # Tab 1 - USBL Tidy ----
      tabItem(
        tabName = "USBL",
        mainPanel(
          fluidRow(
            column(8,
                   box(width = NULL, title = "Tidying Raw USBL Position Data",
                       "Upload raw csv outputs from SeaTrac Pinpoint below, data output will be in a tidy format 
          that has headers and calculated distance between GPS positions ready for calculating transects.", br(),
                       br(),
                       "1. Enter site name", br(),
                       "2. Upload raw log file (Drag & drop or select from file)", br(),
                       "3. Once data has loaded, select rolling mean factor (default is 4), total trasect length will be displayed 
          and adjusted accordingly", br(),
                       "4. Press the download button and the tidy log data will be written to the downloads folder")
            ),
            
            column(3,
                   textInput("sitename", "1. Enter Site Name"),
                   fileInput("upload.USBL", "2. Drag & Drop USBL Log (.csv)",
                             accept=c('csv', 'comma-separated-values','.csv'))),
            column(3,
                   numericInput("rm", "Rolling Mean n:", 4, min = 2, max = 10))
          ),
          
          fluidRow(
            column(3,
                   infoBoxOutput(width = NULL, "info.USBL")),
            column(3,
                   downloadButton("download.USBL", "Download Log (.csv)"))
          ),
          
          fluidRow(offset = 0,
                   column(12, offset = 0, height = NULL,
                          box(width = NULL,
                              leafletOutput("USBL.track")))
          ),
          
          fluidRow(offset = 0,
                   column(12,
                          box(width = NULL,
                              plotlyOutput("Transect.plot")))),
          
          fluidRow(offset = 0,
                   column(12,
                          #withSpinner(
                          box(width = NULL,
                              #box(title = "USBL Tidy Log", height = NULL, width = NULL, status = "primary",
                              #withSpinner(
                              DTOutput("table.USBL")))),
          width = 12
        )
      ),
      
      
      ##############################
      # Tab 2 - Raw Data Checks ----
      
      tabItem(
        tabName = "checks",
        
        fluidRow(
          column(width = 6,
                 box(
                   title = "Region Map",
                   img(src = "Bioregion-State.png", height="90%", width="90%"), br(),
                   selectInput("Region", "1. Select Region", choices = c("North Coast", "Gascoyne Coast", "West Coast", "South Coast"))),
                 column(width = 6, offset = 0,
                        textInput("study", "2. Enter Project Name"),
                        fileInput("upload.raw.length", "3. Drag & Drop CSV File Here",
                                  accept=c('csv', 'comma-separated-values','.csv')),
                        downloadButton("download.checks.lengths","4a. Download Checks List (.csv)"),
                        downloadButton("download.format","4b. Download Formatted Length Data (.csv)"))),
          fluidRow(
            column(width = 12,
                   withSpinner(
                     DTOutput("table.checks")))))
        
      ),
      
      #############################
      # Tab 3 - Formatting ----
      
      tabItem(
        tabName = "format", "Formatting Raw Data and Exporting Tidy Data",
        textInput("study2", "1. Enter Project Name"),
        fileInput("upload.format", "2a. Drag & Drop Raw Length File (.csv) Here",
                  accept=c('csv', 'comma-separated-vales', '.csv')),
        fileInput("upload.period", "2b. Drag & Drop All Period File (.csv) Here",
                  accept=c('csv', 'comma-separated-vales', '.csv')),
        downloadButton("download.tidy", "Download Formatted Data (.csv)"),
        downloadButton("download.transect", "Download Transect List (.csv)"),
        withSpinner(
          DTOutput("table.format")),
        withSpinner(
          DTOutput("table.transect"))
        
      ),
      
      
      
      #############################
      # Tab 4 - Adding Biomass ----
      
      tabItem(
        tabName = "biomass",
        mainPanel(
          fluidRow(
            column(8,
                   box(width = NULL, title = "Adding Biomass to Tidy Data",
                       "1. Enter study name", br(),
                       "2. Upload tidy data file (Drag & drop or select from file)", br(),
                       "3. Tab 1 will display data with biomass where 3d points will have a zero mass value, 
              Tab 2 will display biomass data and a mean length of the species will be used to calculate biomass for 3d points", br(),
                       "4. Press the download button and the tidy biomass data will be written to the downloads folder")),
            
            column(4, offset = 0,
                   textInput("studyname", "1. Enter Project Name"),
                   fileInput("upload.length", "2. Drag & Drop CSV File Here",
                             accept=c('csv', 'comma-separated-values','.csv')))
          ),
          
          fluidRow(
            downloadButton("download.biomass","Download Biomass (.csv)"),
            downloadButton("download.biomass.mean", "Download Biomass with Mean (.csv)"),
            downloadButton("download.biomass.table", "Download Species List (.csv)")
          ),
          
          fluidRow(
            tabBox(
              title = "Biomass Outputs",
              id = "tabset1", height = "250px", width = "600px",
              tabPanel("Biomass Data", withSpinner(DTOutput("table.biomass"))),
              tabPanel("Biomass with Mean", withSpinner(DTOutput("table.biomass.mean"))),
              tabPanel("Species List w Biomass", withSpinner(DTOutput("table.biomass.table"))))),
          width = 12
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  ### USBL -------------------------------------------------------    
  USBL<-reactive({
    if (is.null(input$upload.USBL))
      return(NULL)                
    tidydata <- read_csv(input$upload.USBL$datapath, col_names=FALSE) %>%
      rename(timedate = 1, latitude = 3, longitude = 4, depth = 7) %>%
      filter(!(latitude == "nan")) %>%
      mutate(site = input$sitename) %>%
      select(c(site,3,4,7,timedate)) %>%
      distinct(timedate, .keep_all = T) %>%
      separate(timedate, sep = " ", remove = TRUE, c("wday", "month", "day", "time", "year")) %>%
      mutate(date = paste(day, month, year, sep = " ")) %>%
      mutate(latRm = rollmean(latitude, input$rm, na.pad=TRUE, align="right"))%>%
      mutate(longRm = rollmean(longitude, input$rm, na.pad=TRUE, align="right"))%>%
      tail(-(input$rm+4))
    
    tidydata <- mutate(tidydata, 
                       dist = distHaversine(cbind(longRm, latRm),
                                            cbind(lag(longRm), lag(latRm)))) %>%
      mutate(cumulative_dist = c(0,cumsum(dist[-1]))) %>%
      mutate(dist = round(dist, 2)) %>%
      mutate(cumulative_dist = round(cumulative_dist, 2)) %>%
      mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
      select(c(site, time, date, depth, latitude, longitude, latRm, longRm, dist, cumulative_dist))
  })
  
  Transect.profile <- reactive({
    if (is.null(input$upload.USBL))
      return(NULL)
    
    plot.data<- USBL() %>%
      mutate(depth = depth * -1) %>%
      mutate(across(cumulative_dist, round, 2))
  })
  
  output$info.USBL <- renderInfoBox({
    infoBox("Total Transect Length (m)", sum(USBL()$dist))
  })
  
  output$Transect.plot <- renderPlotly({
    if (is.null(input$upload.USBL))
      return(NULL)
    Plot.profile <- plot_ly(Transect.profile(), x = ~cumulative_dist, y = ~depth, 
                            type = 'scatter', mode = 'lines',
                            line = list(color = 'rgb(22, 96, 167)', width = 3),
                            hoverinfo = 'text',
                            text = ~paste('</br> Time:', time,
                                          '</br> Depth:', depth,
                                          '</br> Distance:', cumulative_dist)) %>%
      layout(title = paste(input$sitename, "- Transect Depth Profile"),
             xaxis = list(title = "Transect Distance (m)"),
             yaxis = list(title = "Depth (m)"))
    
  })
  
  output$table.USBL <- renderDataTable({
    if (is.null(input$upload.USBL))
      return(NULL)
    datatable(data = USBL(),
              options = list(
                scrollX = TRUE, scrollY = "600px", pageLength = 50
              ))
  })
  
  output$download.USBL <- downloadHandler(
    filename = function () { 
      paste(input$sitename, "_tidy.csv", sep="")
    },
    content = function(file) {
      write.csv(USBL(), file)
    })
  
  output$USBL.track <- renderLeaflet({
    if (is.null(input$upload.USBL))
      return(NULL)
    leaflet() %>%
      #addProviderTiles(providers$Esri.WorldImagery) %>%
      addTiles() %>%
      addPolylines(data = USBL(), lng = ~longRm, lat = ~latRm, weight = 1.5)
    #addMarkers(data = USBL(), lng = ~longRm, lat = ~latRm)
  })
  
  # Checks -----------------------------------------------------------------------------
  
  checks<-reactive({
    if (is.null(input$upload.raw.length))
      return(NULL)
    # Read in raw data in upload .csv
    raw.length.data<-read_csv(input$upload.raw.length$datapath, skip = 4, col_names = T, trim_ws = T) %>%
      rename(Length = `Length (mm)`)%>%
      mutate(Length = as.numeric(Length))%>%
      rename(Range.mm = `Range (mm)`)%>%
      rename(RMS = `RMS (mm)`)%>%
      rename(PeriodTime = 5)%>% ### NOTE: you might need to change this to (HSM) or (mins) if you get an error
      rename(Comment1 = Comment...19)%>%
      rename(Comment2 = Comment...35)%>%
      mutate(Genus = ifelse(is.na(Genus), Family,Genus))%>% #fill in any blank Genus names with family
      mutate(Genus_species = paste(Genus, Species, sep = ' '))%>% #paste Genus species together
      select(-25,-26,-27)
    
    # Blank points
    L.Blank.sync.points <- filter(raw.length.data,is.na(Family) & Number > 0)%>%
      arrange(OpCode)%>%
      mutate(Error = "Blank point") %>%
      mutate(CAAB.web = if_else(Code > 1, paste("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", Code, sep =""), "NA")) %>%
      select(c(OpCode, Error, Filename, Family, Genus,Species, Genus_species, 19, PeriodTime, Code, RMS, Range.mm, Length, Number, CAAB.web))
    
    # Check for fish measured outside of predetermined range > 8m
    Out.of.range<-filter(raw.length.data,Range.mm>8000)%>%
      arrange(OpCode)%>%
      mutate(Error = "Fish outside of range") %>%
      mutate(CAAB.web = if_else(Code > 1, paste("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", Code, sep =""), "NA")) %>%
      select(c(OpCode, Error, Filename, Family, Genus,Species, Genus_species, 19, PeriodTime, Code, RMS, Range.mm, Length, Number, CAAB.web))
    
    #Check for RMS Errors >20
    RMS.high<-filter(raw.length.data, RMS>20 & Length>0)%>%
      arrange(OpCode)%>%
      mutate(Error = "RMS too high") %>%
      mutate(CAAB.web = if_else(Code > 1, paste("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", Code, sep =""), "NA")) %>%
      select(c(OpCode, Error, Filename, Family, Genus,Species, Genus_species, 19, PeriodTime, Code, RMS, Range.mm, Length, Number, CAAB.web))
    
    # Check on the BIG fish length data > 800
    Fish.greater.than.1.meter<-filter(raw.length.data,Length>800)%>%
      arrange(OpCode)%>%
      mutate(Error = "Large fish") %>%
      mutate(CAAB.web = if_else(Code > 1, paste("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", Code, sep =""), "NA")) %>%
      select(c(OpCode, Error, Filename, Family, Genus,Species, Genus_species, 19, PeriodTime, Code, RMS, Range.mm, Length, Number, CAAB.web))
    
    #Fish not in region
    Region.list <- read_csv("Species_list_All_regions.csv") %>%
      filter(Region == input$Region)
    
    #Fish missing from life history
    L.Not.in.life.history<-anti_join(raw.length.data, Region.list, by=c("Genus_species"))%>%
      distinct(OpCode,Genus_species, .keep_all = TRUE)%>%
      arrange(OpCode)%>%
      drop_na(Family) %>%
      mutate(Error = "Not in Region") %>%
      mutate(CAAB.web = if_else(Code > 1, paste("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", Code, sep =""), "NA")) %>%
      select(c(OpCode, Error, Filename, Family, Genus,Species, Genus_species, 19, PeriodTime, Code, RMS, Range.mm, Length, Number, CAAB.web))
    
    #Fish larger than life history MAX length
    Wrong.length<-merge(raw.length.data, life.history,"Genus_species","Genus_species",all.x=TRUE)%>%
      #mutate(FB.Length_MAX = as.numeric(FB.Length_MAX))%>%
      mutate(Length = as.numeric(Length))%>%
      mutate(Length.diff = (FB.Max-Length))%>%
      mutate(PercentDiff = (((Length-FB.Max)/(FB.Max))*100))%>%
      mutate(PercentDiff = round(PercentDiff,2))%>%
      filter(Length.diff < 0)%>%
      arrange(OpCode)%>%
      mutate(Error = "Wrong Length") %>%
      mutate(CAAB.web = if_else(Code > 1, paste("https://www.cmar.csiro.au/caab/taxon_report.cfm?caab_code=", Code, sep =""), "NA")) %>%
      select(c(OpCode, Error, Filename, Family, Genus,Species, Genus_species, PeriodTime, Code, RMS, Range.mm, Length, FB.Max, PercentDiff, Number, CAAB.web))
    
    L.errors <- 
      bind_rows(L.Blank.sync.points, Out.of.range, RMS.high, Fish.greater.than.1.meter, L.Not.in.life.history)
    
  })
  
  output$table.checks <- renderDataTable({
    if (is.null(input$upload.raw.length))
      return(NULL)
    datatable(data = checks(),
              options = list(scrollX = TRUE, scrollY = "600px", pageLength = 100, 
                             lengthMenu = list(c(20, 50, 100, -1), c('20', '50', '100', 'All'))
              ))
  })
  
  output$download.checks.lengths <- downloadHandler(
    filename = function () { 
      paste(input$study, "_Length_3d_Error_Checks.csv", sep="")
    },
    content = function(file) {
      write.csv(checks(), file, row.names = FALSE)
    })
  
  
  ## Formatting -------------------------
  
  format<- reactive({
    if (is.null(input$upload.format))
      return(NULL)
    
    Length.data<-read_csv(input$upload.format$datapath, skip = 4, col_names=T, trim_ws = T)%>%
      rename(Length.mm = `Length (mm)`)%>%
      rename(Range.mm =`Range (mm)`)%>%
      rename(RMS.mm = `RMS (mm)`)%>%
      rename(Precision.mm = `Precision (mm)`)%>%
      rename(PeriodTime = 5) %>% ### NOTE: you might need to change this to (HSM) or (mins) if you get an error
      drop_na(Family, Period) %>%
      mutate(Length.mm = ifelse((is.na(Length.mm)),"0", Length.mm))%>%
      mutate(Length.mm = as.numeric(Length.mm)) %>%
      mutate(Number = as.numeric(Number)) %>%
      mutate(Period = as.character(Period)) %>%
      mutate(Genus = ifelse(is.na(Genus), Family,Genus))%>% #fill in any blank Genus names with family
      mutate(Genus_species = paste(Genus, Species, sep = ' '))%>% #paste Genus species together
      mutate(Transect = paste(OpCode, Period, sep="-")) %>%
      mutate(Precision.ratio = ifelse(Length.mm == 0, "0", (Precision.mm/Length.mm)*100)) %>%
      select(Transect, OpCode, Period, Filename, PeriodTime, RMS.mm, Precision.mm, Precision.ratio, Range.mm, TapeReader, 20:26, Family, Genus, Species, Genus_species, Code, Length.mm, Number) %>%
      remove_empty("cols")
    
  })
  
  output$table.format <- renderDataTable({
    if (is.null(input$upload.format))
      return(NULL)
    datatable(data = format(),
              options = list(scrollX = TRUE, scrollY = "600px", pageLength = 100, 
                             lengthMenu = list(c(20, 50, 100, -1), c('20', '50', '100', 'All'))
              ))
  })
  
  output$download.tidy <- downloadHandler(
    filename = function () { 
      paste(input$study2, "_Length_3d_tidy.csv", sep="")
    },
    content = function(file) {
      write.csv(format(), file, row.names = FALSE)
    })
  
  
  transects<- reactive({
    if (is.null(input$upload.period))
      return(NULL)
    
    Period.data<-read_csv(input$upload.period$datapath, skip = 4, col_names = T) %>%
      mutate(Transect = paste(OpCode, Period, sep ="-")) %>%
      rename(Start.time = 4) %>%
      rename(End.time = 7) %>%
      rename(Period.length.mins = 9) %>%
      select(Transect, OpCode, Period, Time, Start.time, End.time, Period.length.mins, TapeReader, Depth, 13:21) %>%
      remove_empty("cols")
    
  })
  
  output$table.transect <- renderDataTable({
    if (is.null(input$upload.period))
      return(NULL)
    datatable(data = transects(),
              options = list(scrollX = TRUE, scrollY = "600px", pageLength = 100, 
                             lengthMenu = list(c(20, 50, 100, -1), c('20', '50', '100', 'All'))
              ))
  })
  
  output$download.transect <- downloadHandler(
    filename = function () { 
      paste(input$study2, "_Transect_List.csv", sep="")
    },
    content = function(file) {
      write.csv(transects(), file, row.names = FALSE)
    })
  
  
  ## Biomass ----------------------------
  
  biomass <- reactive({
    if (is.null(input$upload.length))
      return(NULL)
    
    biomass.data<-read_csv(input$upload.length$datapath, col_names = T) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      mutate(Number = as.numeric(Number))%>%
      mutate(Length.cm = Length.mm/10) %>%
      left_join(life.history, by = "Genus_species") %>%
      #merge(lh, by.x="Gen"us_species",by.y="Genus_species", all.x=TRUE)%>%
      mutate(AdjLength=(Length.cm*bLL)+aLL)%>%
      mutate(AdjLength=as.numeric(AdjLength))%>%
      mutate(Biomass.g=(AdjLength^b)*a*Number)%>%
      mutate(Biomass.kg = Biomass.g/1000) %>%
      mutate(Biomass.kg = round(Biomass.kg, 2)) %>%
      mutate(Biomass.g = round(Biomass.g, 2)) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      select(c(-FB.Max,-AdjLength, -aLL, -bLL, -a, -b, -Length.cm)) %>%
      relocate(Genus_species, .before = Family)
  })
  
  output$table.biomass <- renderDataTable({
    if (is.null(input$upload.length))
      return(NULL)
    datatable(data = biomass(),
              options = list(scrollX = TRUE, scrollY = "600px", pageLength = 100, 
                             lengthMenu = list(c(20, 50, 100, -1), c('20', '50', '100', 'All'))
              ))
  })
  
  
  biomass.mean <- reactive({
    if (is.null(input$upload.length))
      return(NULL)
    
    Lengths<-read_csv(input$upload.length$datapath, col_names = T) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      mutate(Number = as.numeric(Number))%>%
      mutate(Length.cm = Length.mm/10) %>%
      left_join(life.history, by = "Genus_species") %>%
      #merge(lh, by.x="Gen"us_species",by.y="Genus_species", all.x=TRUE)%>%
      mutate(AdjLength=(Length.cm*bLL)+aLL)%>%
      mutate(AdjLength=as.numeric(AdjLength))%>%
      mutate(Biomass.g=(AdjLength^b)*a*Number)%>%
      mutate(Biomass.kg = Biomass.g/1000) %>%
      mutate(Biomass.kg = round(Biomass.kg, 2)) %>%
      mutate(Biomass.g = round(Biomass.g, 2)) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      select(c(-FB.Max,-AdjLength, -aLL, -bLL, -a, -b)) %>%
      relocate(Genus_species, .before = Family) %>%
      filter(Length.mm > 0)
    
    Mean.lengths<-Lengths %>%
      group_by(Genus_species) %>%
      summarise(Length.mean = mean(Length.mm))
    
    Three.d.points<-read_csv(input$upload.length$datapath, col_names = T, trim_ws = T) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      mutate(Number = as.numeric(Number))%>%
      mutate(Length.cm = Length.mm/10) %>%
      left_join(life.history, by = "Genus_species") %>%
      #merge(lh, by.x="Gen"us_species",by.y="Genus_species", all.x=TRUE)%>%
      mutate(AdjLength=(Length.cm*bLL)+aLL)%>%
      mutate(AdjLength=as.numeric(AdjLength))%>%
      mutate(Biomass.g=(AdjLength^b)*a*Number)%>%
      mutate(Biomass.kg = Biomass.g/1000) %>%
      mutate(Biomass.kg = round(Biomass.kg, 2)) %>%
      mutate(Biomass.g = round(Biomass.g, 2)) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      select(c(-FB.Max,-AdjLength, -aLL, -bLL, -a, -b)) %>%
      relocate(Genus_species, .before = Family) %>%
      filter(Length.mm == 0)
    
    Length.data.w.mean<-rbind(Lengths,Three.d.points)
    
    biomass.mean <- Length.data.w.mean %>%
      left_join(life.history, by = "Genus_species") %>%
      mutate(AdjLength=(Length.cm*bLL)+aLL)%>%
      mutate(AdjLength=as.numeric(AdjLength))%>%
      mutate(Biomass.g=(AdjLength^b)*a*Number)%>%
      mutate(Biomass.kg=Biomass.g/1000)%>%
      mutate(Biomass.kg = round(Biomass.kg, 2)) %>%
      mutate(Biomass.g = round(Biomass.g, 2)) %>%
      select(c(-FB.Max,-AdjLength, -aLL, -bLL, -a, -b, -Length.cm))
    
  })
  
  output$table.biomass.mean <- renderDataTable({
    if (is.null(input$upload.length))
      return(NULL)
    datatable(data = biomass.mean(),
              options = list(scrollX = TRUE, scrollY = "600px", pageLength = 100, 
                             lengthMenu = list(c(20, 50, 100, -1), c('20', '50', '100', 'All'))
              ))
  })
  
  
  biomass.table <- reactive({
    if (is.null(input$upload.length))
      return(NULL)
    
    Length.w.Biomass<-read_csv(input$upload.length$datapath, col_names = T) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      mutate(Number = as.numeric(Number))%>%
      mutate(Length.cm = Length.mm/10) %>%
      left_join(life.history, by = "Genus_species") %>%
      #merge(lh, by.x="Gen"us_species",by.y="Genus_species", all.x=TRUE)%>%
      mutate(AdjLength=(Length.cm*bLL)+aLL)%>%
      mutate(AdjLength=as.numeric(AdjLength))%>%
      mutate(Biomass.g=(AdjLength^b)*a*Number)%>%
      mutate(Biomass.kg = Biomass.g/1000) %>%
      mutate(Biomass.kg = round(Biomass.kg, 2)) %>%
      mutate(Biomass.g = round(Biomass.g, 2)) %>%
      mutate(Length.mm = as.numeric(Length.mm))%>%
      select(c(-FB.Max,-AdjLength, -aLL, -bLL, -a, -b, -Length.cm)) %>%
      relocate(Genus_species, .before = Family)
    
    
    L.Species.list.1<-Length.w.Biomass %>%
      drop_na(Family)%>%
      group_by(Genus_species) %>%
      summarise(CountTotal = sum(Number)) %>%
      mutate_if(is.numeric, round, 2)
    
    L.Species.list.2<-Length.w.Biomass %>%
      drop_na(Family)%>%
      group_by(Genus_species) %>%
      summarise(MeanLength = mean(Length.mm)) %>%
      mutate_if(is.numeric, round, 2)
    
    L.Species.list.3<-Length.w.Biomass%>%
      drop_na(Family)%>%
      filter(Length.mm > 0)%>%
      group_by(Genus_species)%>%
      summarise(MinLength = min(Length.mm, na.rm = TRUE),
                MaxLength = max(Length.mm, na.rm = TRUE),
                BiomassTotal.kg = sum(Biomass.kg, na.rm = TRUE))%>%
      mutate_if(is.numeric, round, 2)
    
    L.Species.list <-
      left_join(L.Species.list.1,L.Species.list.2, by = "Genus_species") %>%
      left_join(., L.Species.list.3, by="Genus_species")
    
    
    ## Remove NaNs and Infs #
    
    L.Species.list<-do.call(data.frame, lapply(L.Species.list, function(x) {replace(x, is.infinite(x) | is.na(x), 0)})) %>%
      select(c(Genus_species, MinLength, MaxLength, MeanLength, CountTotal, BiomassTotal.kg))  
    
  })
  
  output$table.biomass.table <- renderDataTable({
    if (is.null(input$upload.length))
      return(NULL)
    datatable(data = biomass.table(),
              options = list(scrollX = TRUE, scrollY = "600px", pageLength = 100, 
                             lengthMenu = list(c(20, 50, 100, -1), c('20', '50', '100', 'All'))
              ))
  })
  
  
  output$download.biomass <- downloadHandler(
    filename = function () { 
      paste(input$studyname, "_tidy_w_biomass.csv", sep="")
    },
    content = function(file) {
      write.csv(biomass(), file, row.names= FALSE)
    })
  
  output$download.biomass.mean <- downloadHandler(
    filename = function () { 
      paste(input$studyname, "_tidy_w_biomass_w_mean.csv", sep="")
    },
    content = function(file) {
      write.csv(biomass.mean(), file, row.names = FALSE)
    })
  output$download.biomass.table <- downloadHandler(
    filename = function () { 
      paste(input$studyname, "_species_list_w_biomass_summary.csv", sep="")
    },
    content = function(file) {
      write.csv(biomass.table(), file, row.names = FALSE)
    })
  
}

## Shinyapp ---------------------------

shinyApp(ui = ui, server = server)

#deployApp('G:/My Drive/Work/R/EMTools/EMToolsROV')
