library(shiny)
library(leaflet)
library(datasets)
library(ggplot2)
library(RCurl)
library(RJSONIO)
library(dplyr)
library(ggmap)
library(geosphere)
library(DT)
library(shinydashboard)

#setting dashboard theme
theme_set(theme_classic())

df1<-read.csv("eventsPostcode.csv")
d2<-read.csv("postcodes.csv")

names(df1)[6]<-paste("category")
postcodeList = unique(d2$postcode)
eventCatList = unique(df1$category)

ui = dashboardPage( skin ="blue",
                    dashboardHeader(
                      title ="Outdoor Places and Activities Near You",
                      titleWidth =400
                    ), 
                    dashboardSidebar(disable = TRUE
                    ),
                    dashboardBody(
                      
                      #css
                      
                      tags$head(tags$style(HTML('
                                                .content{
                                                padding-top :3px;}

                                                .dashboardHeaderr .logo {
                                                font-family: "Georgia";
                                                font-weight: 800;
                                                color: black;
                                                }
                                               
                                                }
                                                h3{
                                                
                                                font-weight: 800;
                                                font-family: "Georgia";
                                                line-height: 1.1;
                                                color: black;
                                                
                                                }
                                                
                                                .img-local{
                                                align :right;
                                                }
                                                
                                                '))),
                      #creating different tabs 
                      fluidRow(
                        
                        column(width =3,
                               box(title ="Filter here!", solidHeader = T, status ="primary",width = 100, 
                                   
                                   selectInput("postcodeSelected","Select your Post code",choices =postcodeList,selected = "3185"),
                                   selectInput("eventsSelected","Choose a category",choices =eventCatList,selected = "Park/Garden/Reserve")
                               )),
                        column(width =9 , 
                               box( title ="Outdoor Places and Activities Map", solidHeader = T, status ="primary", width =100, leafletOutput("mymap")
                               ),
                               box( title ="Activites Details", solidHeader = T, status ="primary", width =100,DT::dataTableOutput("mytable")
                               )
                        )
                      )
                      
                      
                      )
                      )



server<-shinyServer(function(input, output) {
  output$mymap <- renderLeaflet({
    
    
    df3 <-df1[df1$category == input$eventsSelected,]
    df2 <-d2[d2$postcode == input$postcodeSelected,]
    
    df3$distance<-distVincentyEllipsoid(df3[,c('longitude','latitude')],df2[,c('longitude','latitude')])
    df3$distance<-df3$distance/1609.344
    df3$distance<-round(df3$distance,2)
    df3<-df3[order(df3$distance),]
    df3<-df3[1:10,]
    
    df3
    leaflet(df3) %>% addTiles() %>%
      addAwesomeMarkers(~longitude, ~latitude,  label=~as.character(Name),#popup= ~paste(""),
                        labelOptions = labelOptions(noHide = T, direction = 'top',textsize = "7px", textOnly = F))
    
    
  })
  
  output$mytable = renderDataTable({
    names(df1)[6]<-paste("category")
    
    
    df3 <-df1[df1$category == input$eventsSelected,]
    df2 <-d2[d2$postcode == input$postcodeSelected,]
    
    df3$distance<-distVincentyEllipsoid(df3[,c('longitude','latitude')],df2[,c('longitude','latitude')])
    df3$distance<-df3$distance/1609.344
    df3$distance<-round(df3$distance,1)
    
    df3<-df3[order(df3$distance),]
    
    names(df3)[names(df3) == 'distance'] <- 'Distance(KM)'
    
    
    datatable(head(df3[,-c(5,8,9)],10),rownames =FALSE
              ,options = list(
                pageLength = 5, 
                lengthMenu = c(2,5), paging = T,searching = FALSE)
    )
    
  })
  
})

# call shinyApp and launch it
shinyApp(ui=ui, server=server)