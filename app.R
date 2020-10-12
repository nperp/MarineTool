rm(list=ls())
library(shiny)
library(data.table)
library(shiny.semantic)
library(shinycssloaders)
library(geosphere)
library(leaflet)
library(zoo)
library(lubridate)
source('./www/auxiliar_functions.R')


# Load Data ####
shipData <- fread('./www/ships.csv',
                  colClasses = c(rep('numeric',6),rep('character',2),
                                 rep('numeric',2),rep('character',3),
                                 rep('numeric',6),rep('character',7)),
                  encoding = 'UTF-8',
                  select=c('ship_type','SHIPNAME','DATETIME','LAT','LON',
                           'SPEED','DWT','FLAG','port',
                           'is_parked','LENGTH','WIDTH'))
shipData[,SHIPNAME:= gsub('[[:punct:] ]+',' ',SHIPNAME)]
shipData[,SHIPNAME:=trimws(SHIPNAME)]
shipData[,DATETIME:=lubridate::ymd_hms(DATETIME)]
shipData[,SPEED:=SPEED/10]
setkeyv(shipData,c('ship_type','SHIPNAME','DATETIME'))



# Create Dropdowns ####
# userInputUI
userInputsUI <- function(id){
    ns <- NS(id)
    div(class='ui two column stackable grid',
        div(class='column',
            selectInput(ns('veselTypeSelected'),
                        label="Select Vessel type",
                        choices=sort(unique(shipData$ship_type)),
                        selected = 'NULL',
                        default_text = "Select Vessel type",
                        type = "selection fluid"
            )),
        div(class='column',uiOutput(ns('selectedNameUI'))
        )
    )
    
}

# userInput Server ##
userInputsServer <- function(id){
    moduleServer(
        id,
        function(input, output, session) {
            
            
            # Find Ship Names based on Selected Vessel type ####
            shipNames <- reactive({
                unique(shipData[,.(ship_type,SHIPNAME)])[
                    ship_type==input[['veselTypeSelected']],
                ]$SHIPNAME
            })
            
            # Create the UI with choices being the ship names ####
            output$selectedNameUI <- renderUI({
                req(shipNames())
                selectInput('shipNameSelected',
                            choices=sort(shipNames()),
                            label = "Select Ship Name",
                            selected = "NULL",
                            default_text = "Select Ship Name",
                            type = "selection fluid")
            })
            
            
            return(reactive(input[['veselTypeSelected']]))
        }
    )
}

# Create Info ####
# Create Info UI ##
InfoUI <- function(id){
    ns <- NS(id)
    div(class='ui raised placeholder segment',
        uiOutput(ns('information'))
        
    )   
    
}

# Create Info Server ##
InfoServer <- function(input, output, session,data){
    distanceMoved <- reactive({data()$distanceMoved})
    maxSPEED <- reactive({data()$maxSPEED})
    minSPEED <-  reactive({data()$minSPEED})
    averageSPEED <-  reactive({data()$averageSPEED})
    maxDistTraveled <- reactive({data()$maxDistTraveled})
    infoOfMaxVel <- reactive({data()$infoOfMaxVel})
    shipType <- reactive({data()$shipType})
    shipName <- reactive({data()$shipName})
    shipLength <- reactive({data()$shipLength})
    shipWidth <- reactive({data()$shipWidth})
    shipDWT <- reactive({data()$shipDWT})
    shipDestination <- reactive({data()$shipDestination})
    isParked <- reactive({data()$isParked})
    flag <- reactive({data()$flag})
    velocity <- reactive({maxDistTraveled()$PosDiff/1000/time_length(maxDistTraveled()$timeDiff,'h')/1.85})
    startEndPoints <- reactive({data()$startEndPoints})
    fullData <- reactive({data()$fullData})
    output$information <- renderUI({
        div(
            div(class='ui raised segment',
                header('General Info',description='',icon = 'info'),
                div(class="ui five mini stackable statistics",
                    div(class="statistic", div(class='value',shipLength()),
                        div(class='label','Length')),
                    div(class="statistic", div(class='value',shipWidth()),
                        div(class='label','Width')),
                    div(class="statistic", div(class='value',shipDWT()),
                        div(class='label','DWT')),
                    div(class="statistic", div(class='value',icon("anchor"),
                                               shipDestination())),
                    div(class="statistic", div(class='value',
                                               icon("flag"),toupper(flag()))),
                    
                )),
            
            div(class='ui raised segment',
                header('Speed',description='',icon = 'tachometer alternate'),
                div(class="ui three statistics",
                    div(class="statistic", div(class='value',minSPEED()),
                        div(class='label','MIN (kn)')),
                    div(class="statistic", div(class='value',
                                               round(averageSPEED(),2)),
                        div(class='label','AVG (kn)')),
                    div(class="statistic", div(class='value',maxSPEED()),
                        div(class='label','MAX (kn)')),
                    
                )),
            div(class="ui raised segment",
                header("Distance",description = '',icon = "location arrow"),
                div(class="ui items",
                    div(class='item',
                        div(class='content',
                            div(class='description',
                                paste0("Total Distance traveled: ",round(distanceMoved(),2),' m'))
                        )),
                    div(class="item", 
                        div(class='content',
                            div(class='description',
                                if(isParked()==T){
                                    paste0('Not moved since ',startEndPoints()[1,DATETIME],'. Parked in ',shipDestination())
                                           }else{
                            paste0(paste0("Max distance traveled between observations is ",
                                       round(maxDistTraveled()$PosDiff,2),
                                       " meters that was traveled in ",round(time_length(maxDistTraveled()$timeDiff,'min'),2),
                                       " minutes between ",maxDistTraveled()$DateTimePrev,
                                       ' and ',maxDistTraveled()$DATETIME),
                                      if(velocity() > maxSPEED()+5){
                                          paste0('...Most probably issue in Data to be handled, as: ', round(velocity(),2),
                                          ' kn is higher than the max speed')
                                      })
                                   }
                            )
                        )
                    )
                )
            )
        )
        
    })
}

# Leaflet Map ####
# Create Map UI ##
mapUI <- function(id){
    ns <- NS(id)
    withSpinner(leafletOutput(ns('distplot')),type=6)
}
# Create Map Server ##
mapServer <- function(input, output, session,listOfInfo){
    
    maxDistTraveled <- reactive({listOfInfo()$maxDistTraveled})
    startEndPoint <- reactive({listOfInfo()$startEndPoint})
    maxDiststartEndPoint <- reactive({ 
        rbind(maxDistTraveled()[,.(LAT=LatPrev,LON=LongPrev,
                                   DATETIME=DateTimePrev,group='MaxDistStart')],
              maxDistTraveled()[,.(LAT,LON,DATETIME,group='MaxDistEnd'),])
    })
    startEndIcon <- iconList(MaxDistStart = makeIcon("./www/start-16.png", 
                                              iconWidth = 24, iconHeight =24),
                             MaxDistEnd = makeIcon("./www/stop-16.png",
                                            iconWidth = 16, iconHeight =16))
    
    output$distplot <- renderLeaflet({
        
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron,
                             options = providerTileOptions(noWrap = TRUE))%>% 
            #addMarkers(data = startEndPoint() ,lng = ~LON,lat = ~LAT,
            #           icon = startEndIcon[startEndPoint()$group],
            #           popup=startEndPoint()$DATETIME)#%>% 
             addMarkers(data=maxDiststartEndPoint(),lng=~LON,lat=~LAT,
                        icon = startEndIcon[maxDiststartEndPoint()$group],
                        popup = maxDiststartEndPoint()$DATETIME)%>%
             addPolylines(data = maxDiststartEndPoint(),lng = ~LON,lat = ~LAT)
    })
}


# Main App ####
marineApp <- function(){
    # ui ####
    ui <- semanticPage(
        # header ##
        header("Ship Info",description = 'by Nikos Perpinias' , icon = "ship"),
        # inputs ##
        segment(
            userInputsUI('myApp')
        ),
        # report ##
        div(class='ui two column stackable grid',
            div(class='column',
                div(class='ui raised segment', 
                    tags$style(type = "text/css",
                               "#myMap-distplot {height: calc(85vh - 80px) !important;}"),
                    mapUI('myMap')
                )),
            div(class='column',
                InfoUI('myInfo')
            )
        )
    )
    # server ####
    server <-  function(input, output, session) {
        #user input module
        veselType <-  userInputsServer("myApp")
        
        #data manimupalation
        selectedShipInfo <- reactive({
            req(input$shipNameSelected)
            req(veselType())
            finalDS <- shipData[ship_type==veselType() & SHIPNAME==input[['shipNameSelected']]]
            if(nrow(finalDS)>0){
                return(finalDS)
            }else{
                return(NULL)
            }})
        listOfInfo <- eventReactive(selectedShipInfo(), {
            req(selectedShipInfo())
            ManipulateData(copy(selectedShipInfo()))
        })
        
        #reporting modules
        callModule(mapServer,'myMap',listOfInfo=listOfInfo)
        callModule(InfoServer,'myInfo',data=listOfInfo)
        
    }
    shinyApp(ui, server)
}



marineApp()