#AirMiles3D

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
Sys.setenv(RGL_USE_NULL = TRUE)  #suppress popup 3D display
library("rgl")      #for 3D plotting
source("jaxmat.R")

#final version

stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Great Circle Routes", titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(stylesheet,
             column(width=4,
                    useShinyjs(),
                    
                    fluidRow(
                      column(width = 6,
                             textInput("new_city", "Enter new city", "")
                      ),
                      column(width = 3,
                             numericInput("new_city_lat", "Enter latitude", 0)
                      ),
                      column(width = 3,
                             numericInput("new_city_lng", "Enter longitude", 0)
                      )
                    ),
                    actionButton("add_city", "Submit"),
                    
                    checkboxInput("threeD","Use 3D Display", value = FALSE),
                    selectInput("selstart","Starting City",choices = "Boston"),
                    uiOutput("geogA"),
                    uiOutput("vecA"),
                    selectInput("seldest","Destination City",choices = "London"),
                    uiOutput("geogB"),
                    uiOutput("vecB"),
                    actionBttn("btnadd","Add Route"),
                    uiOutput("msgdist"),
                    uiOutput("dirvec"),
                    uiOutput("heading"),
                    uiOutput("pole")
             ),
             column(width=8,
                    rglwidgetOutput("globe", width = "100%", height = "700px"),
                    plotOutput("mercator", width = "100%", height = "700px")
                    # uiOutput("plot")  #will be replaced by 3D or Mercator display
             )
    )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics

source("spherical.R")


#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  
  #when variable changes, a reactive variable is saved
  #eventReactive(input$add_city, )
  
  #when variable changes, code is run, but no variable is returned
  
  vA <- NULL
  vB <- NULL
  use3d <- FALSE
  cityDF <- sph.makeCityDF("cities.csv")
  
  row_to_add_r = eventReactive(input$add_city,{
    row_to_add = data.frame( Name = input$new_city, Lat = input$new_city_lat, Long = input$new_city_lng )
  })
  
  # https://shiny.rstudio.com/reference/shiny/0.14/reactiveValues.html
  rv_list = reactiveValues( city_df = cityDF )
  
  observeEvent(input$add_city, {
    x = nrow(rv_list$city_df) + 1
    row_to_add = data.frame( X = x, Name = input$new_city, Lat = input$new_city_lat, Long = input$new_city_lng )
    rv_list$city_df = rbind( rv_list$city_df, row_to_add )
  })
  
  observe({
    updateSelectInput(session, "selstart", choices = rv_list$city_df$Name, selected = "Boston")
    updateSelectInput(session, "seldest", choices = rv_list$city_df$Name, selected = "London")
  })
  
  #New data frame to keep track of all the routes
  rteDF <- data.frame(Start = character(100),Dest = character(100),
                        stringsAsFactors=FALSE)
  nroute <- 0;

    redrawMap <- function(){
    if (use3d){
      show("globe")
      hide("mercator")
      output$globe <- renderRglwidget({
        try(rgl.close(), silent = TRUE)
        sph.blankPlot3D()
        rgl.viewpoint(theta = 0, phi = -90, zoom = 0.5)
        #sph.showCities3D(cityDF)
        sph.showCities3D(rv_list$city_df)
        if (nroute == 0)
          return(rglwidget(width = "700px", height = "700px"))
        for (i in (1:nroute)){
          #sph.plotRoute3D(cityDF,rteDF[i,1], rteDF[i,2],nstop = 5)
          #sph.plotRoute3D(rv_list$city_df,rteDF[i,1], rteDF[i,2],nstop = 5)
          sph.plotRoute3D_Straight(rv_list$city_df,rteDF[i,1], rteDF[i,2])
        }
        return(rglwidget())
      })
    }
    else{
      hide("globe")
      show("mercator")
      output$mercator <- renderPlot({
        try(rgl.close(), silent = TRUE)
        sph.blankPlot()
        #sph.showCities(cityDF)
        sph.showCities(rv_list$city_df)
        if (nroute == 0)
          return()
        for (i in (1:nroute)){
          #sph.plotRoute(cityDF,rteDF[i,1], rteDF[i,2],nstop = 5)
          #sph.plotRoute(rv_list$city_df,rteDF[i,1], rteDF[i,2],nstop = 10)
          sph.plotRouteMerc(rv_list$city_df,rteDF[i,1], rteDF[i,2])
          
        }
      })
    }
  }



    #Functions that respond to events in the input
  observeEvent(input$threeD,{
    use3d <<- input$threeD
    redrawMap()
  })

  observeEvent(input$selstart,{
    #ll <- sph.latlong(cityDF,input$selstart)
    ll <- sph.latlong(rv_list$city_df,input$selstart)
    
    output$geogA <- renderUI(paste("Latitude",round(ll[1],2),
                                  "Longitude",round(ll[2],2)))
    vA <<- sph.makeXYZ(ll)
    output$vecA <- renderUI(jax.vector(sph.makeXYZ(ll),name = "v_A"))
  })

  observeEvent(input$seldest,{
    #ll <- sph.latlong(cityDF,input$seldest)
    ll <- sph.latlong(rv_list$city_df, input$seldest)
    output$geogB <- renderUI(paste("Latitude",round(ll[1],2),
                                   "Longitude",round(ll[2],2)))
    vB <<- sph.makeXYZ(ll)
    output$vecB <- renderUI(jax.vector(sph.makeXYZ(ll),name = "v_B"))
  })
  observeEvent(input$btnadd,{
    if (input$selstart == input$seldest)
      return()
    nroute <<- nroute+1
    rteDF[nroute,1] <<- input$selstart
    rteDF[nroute,2] <<- input$seldest
    output$msgdist <- renderUI(paste("Distance",
              round(sph.distance(vA,vB,unit = "kilometers"),1),"kilometers"))
    vAB <- sph.directionVector(vA,vB)
    output$dirvec <- renderUI(jax.vector(vAB, name = "v_{AB}"))
    angle <- round(sph.compass(vA,vAB), digits = 2)
    output$heading <- renderUI(paste("Takeoff heading", angle,"north of east"))
    pole <- vA%x%vB
    if (pole[1] <0 )
      pole <- -pole
    output$pole <- renderUI(p("Pole of great circle",
                              jax.vector(round(pole,3), name= "N_{AB}")))
    redrawMap()
  })


}

#Run the app
shinyApp(ui = ui, server = server)
