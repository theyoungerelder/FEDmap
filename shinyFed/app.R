

library(shiny)
library(leafem)
library(png)
library(rgdal)
library(tidyverse)
library(leaflet)
library(sf)
library(shinyWidgets)
library(RColorBrewer)
library(stringi)
library(rsconnect)


# read geojson data
farms <- readOGR('data/farms/map.geojson')
microlots <- readOGR('data/microlots/map.geojson')

# cleaning 
microlots@data$type <- as.factor(microlots@data$type)
farms@data$type <- as.factor(farms@data$lotID)
microlots@data$acres <- as.numeric(microlots@data$acres)


# icon
fedIcon <- makeIcon( iconUrl = "data/pics/3dgifmaker46984.gif", 
                     iconHeight = 40, 
                     iconWidth = 40)
# logo link 
logoLink <- 'https://cvws.icloud-content.com/B/AUII4rzk6Y91S1pkQjbKfnmlE09wAcNVugWj2JE-93z8QxmVeFTax8ov/3dgifmaker46984.gif?o=Ah3_Sy00ZSKv3tjhdWCDnWhO4kc0hWoJxJ6m5vCSFrh4&v=1&x=3&a=CAog09OLVAbki3Xd4ZIBSyyvWg6-JaqpIXEiQNlIZcUWMeQSbRCG9Z7OmTEYhtL6z5kxIgEAUgSlE09wWgTax8ovaibul-Pz_clo8O3meANJTQmpT3lznqLzaQpaeO2k76E_JA9ZC73oTHImqlGYJsOyzANRf4zNbcdFxobcfSePQzulT2qNqisY2nA0mHmB9Gk&e=1690505750&fl=&r=18292d57-ddad-4c0a-a9bf-68c84a826484-1&k=ttRsGsSbmI6Zo9umH7sZcw&ckc=com.apple.clouddocs&ckz=com.apple.CloudDocs&p=70&s=40OqoFPCr6HcbquUBJAZcBb3KVI&cd=i'

# color pallete

factpal <- colorFactor(hcl.colors(6), microlots@data$type) # HERE, this should be reactive
farmpal <- colorFactor(palette = c('blue', 'lightblue'), farms@data$type) # HERE, this should be reactive







 

ui <- fluidPage(
 

    titlePanel(img(src = "logo.png", height = 240, width = 500)),

    sidebarLayout(
        sidebarPanel(NULL,
                      pickerInput(inputId = 'Analytics', 
                                  label = 'Choose a Farm',
                                  choices = unique(microlots@data$farmName),
                                  multiple = T
                                  ) #pickerinput
                     ), #sidebarpanel
        mainPanel(h1('Finca Mapping'),
                  h4('this web app is designed for the visualization
                     of agricultural information from 
                     Finca Eldorado and Finca Eliza', align = 'center'
                     ),
                  textOutput('selectedFarm'),
                  leafletOutput('maps')
                  ) #mainpanel
                 )#sidebarlayout
               )#fluidpage

server <- function(input, output) {
  output$selectedFarm <- renderText({paste('you have chosen to view the',
                                           input$maps,
                                           'map'
  )
                                    }
                                   )#rendertext
  
  selectedFarm <- reactive({ 
    microlotsSubset <- microlots[microlots@data$farmName == input$maps]
    return(microlotsSubset)
    
  }) # reactive 
  
  output$maps <- renderLeaflet({

        leaflet() %>% 
      addProviderTiles('Esri.WorldImagery', group = 'satelite') %>% 
      addProviderTiles('Esri.WorldTopoMap') %>% 
      addPolygons(data = selectedFarm(), # HERE, how to subset reactive expression
                  weight = 2, 
                  opacity = 1000, 
                  color = factpal(microlots@data$type),
                  fillColor =  factpal(microlots@data$type),
                  label = microlots@data$type,
                  popup = stri_paste('plants: ',
                                     microlots@data$plantQuant,
                                     'acres: ',
                                     round(microlots@data$acres, 2), 
                                     sep = ' '),
                  group = 'lots') %>% 
      addPolygons(data = farms,
                  weight = 1, 
                  opacity = 1,
                  color = farmpal(farms@data$lotID),
                  group = 'farms',
                  label = ~type,
                  popup = stri_paste('acres: ',
                                     farms@data$acres)) %>%
        
      addLogo(img = '3dgifmaker46984.gif',
             height = 100,
             width = 100,
             offset.x = 600,
             offset.y = 270,
              url = 'https://www.fincaeldorado.com/',
              src = c("remote")) %>% 
    
      addLayersControl(baseGroups =  c('lots', 'farms'), overlayGroups = c('satelite')) %>% 
        
      hideGroup('satelite') # makes the default overlaygroup unchecked. 
    
  })
                                

}

# list of variables made in the UI
# 'Eldorado'
# 'Eliza'
# 'LaNoria'
# 'ALL'

# what to do next
# add analytics to this info
# what kind of graphing can we do?


# Run the application 
shinyApp(ui = ui, server = server)
