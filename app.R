library(shiny)

aem<-read.csv("aemkhdisastereventextractreportforopendatasharing1.csv",stringsAsFactors = F)
aem$url=gsub("resource","category",aem$url)
aem$author<-NULL
aem$documentType<-NULL
aem$resourceType<-NULL
aem$resourceUrl<-NULL
aem$subject<-NULL
aem$subjects<-NULL
aem$startDate=gsub(" .*","",aem$startDate)
aem$startDate<-as.Date(aem$startDate,"%m/%d/%Y")
aem$endDate=gsub(" .*","",aem$endDate)
aem$title<-gsub("Envionmental","Environmental",aem[,"title"])
aem$category<-as.factor(gsub(" .*","",aem[,"title"]))
aem$Date<-gsub(".*/","",aem$startDate)
aem$Title<-aem$title
aem$title<-NULL
aem$Description<-aem$description
aem$description<-NULL
aem$URL<-aem$url
aem$url<-NULL
aem$Regions<-aem$regions
aem$regions<-NULL

library(shinydashboard)
library(maps)
library(sp)
library(maptools)
library(geosphere)
nodes<-read.csv(file="CapitalCities.csv",sep=",",header=TRUE)
edges<-read.csv(file="EdgeListCapCities.csv",sep=",",header=TRUE)
ozDATA<-readShapeSpatial("aus_adm1")
cats<-read.csv("cats.csv",header=T)

header <- dashboardHeader(title = "Disaster Events Dashboard")

body <- dashboardBody(tabItems(
  tabItem("introduction",
    fluidRow(
      h1("Disaster Events"),
      p(style="font-weight:bold","Shiny app demo by Mark Paget Nov 2016"),
      p(style="border-top:1px #3c8dbc solid;margin-top:20px;padding-top:20px","This is my first attempt at Shiny Dashboards, absolutely love R and data having studied data science for last 2.5 years.",icon("smile-o")),
      p("After completing Rstudio's Shiny Server Tutorials - took some fairly narly data and produced some nice looking tables and plots. For further details as well as source code please see my ",a(href="https://coding-school.com/shiny-dashboard/",icon("file-code-o"),title="Blog Post","Blog Post"),". Please feel free to contact me via my Blog, etc"),
      p(style="font-size:small","* Public data supplied by the Australian Government",
         a(href="http://data.gov.au/dataset/26e2ebff-6cd5-4631-9653-18b56526e354",icon("file-code-o"),title="Data Source"),
         a(href="http://creativecommons.org/licenses/by/3.0/au/",icon("info-circle"),title="Creative Commons License")
      )
    )
  ),
  tabItem("deaths",
    fluidRow(plotOutput("deaths"))
  ),
  tabItem("injuries",
          fluidRow(plotOutput("injuries"))
  ),        
  tabItem("property",
          fluidRow(plotOutput("property")),
          fluidRow(plotOutput("building"))
  ),
  tabItem("vehicles",
          fluidRow(plotOutput("aircraft")),
          fluidRow(plotOutput("cars")),
          fluidRow(plotOutput("boats"))
  ),
  tabItem("agriculture",
          fluidRow(plotOutput("farms")),
          fluidRow(plotOutput("livestock"))
  ),  
  tabItem("map",
    fluidRow(
      checkboxGroupInput("mapCats", "Fields:", unique(aem$category),inline=T),
      plotOutput("showMap")
    )
  ),
  tabItem(tabName = "tables",
    fluidRow(
      selectInput("cat", "Categories:", unique(aem$category)),
      checkboxGroupInput("variable", "Fields:", c("Description","Date","Regions","Deaths","URL"),inline=T)
    ),
    fluidRow(tableOutput("data"))
  )
),tags$head(tags$style(HTML(".content { margin:10px 15px}; img { padding:5px;margin:5px; } "))))


sidebar <- dashboardSidebar(
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Introduction",tabName = "introduction",icon = icon("dashboard")),
    menuItem("Tables",icon = icon("th"),tabName = "tables"),
    menuItem("Charts",icon = icon("bar-chart-o"), 
      menuItem("Deaths",tabName = "deaths"),
      menuItem("Injuries",tabName = "injuries"),
      menuItem("Property", tabName = "property"),
      menuItem("Vehicles", tabName = "vehicles"),
      menuItem("Agriculture", tabName = "agriculture")
    ),
    menuItem("Map",icon=icon("map"),tabName="map")
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
    server <- function(input, output) {
    output$data <- renderTable({
      subset(aem,category==input$cat,select=c("Title",input$variable),drop=F)[order(subset(aem,category==input$cat)$startDate,decreasing=T),]
    }, striped=T,hover=T,bordered=T,rownames = TRUE)
    output$deaths<-renderPlot({
      plot(aem$category,aem$Deaths,main="Deaths",col="red",las=2,cex.axis=0.75)
    })
    output$injuries<-renderPlot({
      plot(aem$category,aem$Injuries,main="Injuries",col="red",las=2,cex.axis=0.75)
    })
    output$property<-renderPlot({
      plot(aem$category,aem$Home.s..destroyed,main="Homes Destroyed",col="red",las=2,cex.axis=0.75)
    })
    output$building<-renderPlot({
      plot(aem$category,aem$Building.s..destroyed,main="Buildings Destroyed",col="red",las=2,cex.axis=0.75)
    })
    output$aircraft<-renderPlot({
      plot(aem$category,aem$Aircraft.destroyed,main="Aircraft Destroyed",col="red",las=2,cex.axis=0.75)
    })
    output$cars<-renderPlot({
      plot(aem$category,aem$Motor.Vehicle.s..destroyed,main="Cars Destroyed",col="red",las=2,cex.axis=0.75)
    })
    output$boats<-renderPlot({
      plot(aem$category,aem$Water.vessel.s..destroyed,main="Boats Destroyed",col="red",las=2,cex.axis=0.75)
    })
    output$farms<-renderPlot({
      plot(aem$category,aem$Farm.s..destroyed,main="Farms Destroyed",col="red",las=2,cex.axis=0.75)
    })
    output$livestock<-renderPlot({
      plot(aem$category,aem$Livestock.destroyed,main="Livestock Destroyed",col="red",las=2,cex.axis=0.75)
    })
    output$showMap<-renderPlot({
      
      plot(ozDATA,lwd=0.01,bg="#242424",col="#64881f",ylim=c(-46,-10),xlim=c(125,145))
      points(nodes$Longitude,nodes$Latitude,pch=16,cex=1.5,col="red")
      
      mcats <- vector(mode="character", length=0)
      mCol <- vector(mode="character", length=0)
      mPch <- vector(mode="numeric", length=0)
      
      for(y in input$mapCats) {
        yCol<-as.character(unlist(subset(cats,cat==y,col)))
        yPch<-unlist(subset(cats,cat==y,pch))
        #cat("yCol:",yCol," yPch:",yPch)
        points(aem[aem$category==y,c("lon","lat")],pch=yPch,cex=1.5,col=yCol)
        mcats<-c(mcats,y)
        mCol<-c(mCol,yCol)
        mPch<-c(mPch,yPch)
      }
      if(length(mcats > 0)) {
        legend("topright",legend=mcats,col=mCol,pch=mPch)
      }
    },height = 600, width = 800)
  }
)