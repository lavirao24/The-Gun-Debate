# Loading the required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(rgdal)
library(plotly)

# Reading data sets
usa_vio <- read.csv("usa_vio.csv",header = TRUE)
incidents_state <- read.csv("incidentsByState.csv",header = TRUE)
inc_cat <- read.csv("inc_cat.csv",header = TRUE)
gun_law <- read.csv("gun_law.csv",header = TRUE)
usa_pop <- read.csv("usa_pop.csv",header = TRUE)
states <- readOGR(dsn = "cb_2017_us_state_500k.shp", layer = "cb_2017_us_state_500k", encoding = "UTF-8")

#Joining the guns data set into usa_vio data set
usa_vio<-rename(usa_vio, c("Date"="date"))
guns <- readRDS("guns.rds")
usa_vio <- left_join(usa_vio,guns,by="incident_id")

#Adding strictness and per100k attrubute to the spatial data set states
addPer100k <- data.frame(id=states$GEOID, name=states$NAME)
addPer100k <- left_join(addPer100k, incidents_state %>% select(State, Per100000,Strictness) %>% rename(name=State), by="name")
addPer100k$Per100000[is.na(addPer100k$Per100000)] <- 0
states$per100k <- addPer100k$Per100000
states$Strictness <- addPer100k$Strictness

# Adding individual law attributes to the spatial data set states
add_FRR <- data.frame(id=states$GEOID, name=states$NAME)
add_FRR <- left_join(add_FRR, gun_law %>% select(State, Firearm.Registration.Required) %>% rename(name=State), by="name")
states$Firearm.Registration.Required <- add_FRR$Firearm.Registration.Required

add_OCL <- data.frame(id=states$GEOID, name=states$NAME)
add_OCL <- left_join(add_OCL, gun_law %>% select(State, Open.Carry.Legal.) %>% rename(name=State), by="name")
states$Open.Carry.Legal. <- add_OCL$Open.Carry.Legal.

add_PRC <- data.frame(id=states$GEOID, name=states$NAME)
add_PRC <- left_join(add_PRC, gun_law %>% select(State, Permit.Required.to.Carry.) %>% rename(name=State), by="name")
states$Permit.Required.to.Carry. <- add_PRC$Permit.Required.to.Carry.

add_PRP <- data.frame(id=states$GEOID, name=states$NAME)
add_PRP <- left_join(add_PRP, gun_law %>% select(State, Permit.Required.to.Purchase.) %>% rename(name=State), by="name")
states$Permit.Required.to.Purchase. <- add_PRP$Permit.Required.to.Purchase.

#Creating a separate dataset for only accidental and self harm categories
CatTable <- inc_cat %>% count(incident_characteristics)
Dead <- c("Murder/Suicide","Suicide^", "Attempted Murder/Suicide (one variable unsuccessful)","Suicide - Attempt","Accidental Shooting - Injury","Accidental Shooting","Defensive Use","Pistol-whipping")
CatTable <- CatTable %>% filter(incident_characteristics %in% Dead)



# UI code for the application
ui <- fluidPage(
  

#CETINKAYA-RUNDEL, M. (2019). Shiny - Dashboards. Retrieved 19 June 2020, from https://shiny.rstudio.com/articles/dashboards.html  
    dashboardPage(
        dashboardHeader(title = "The Gun Debate"),
        dashboardSidebar(
          #Code for different menu items
            sidebarMenu(
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Violence VS Gun Laws", tabName = "relation", icon = icon("globe-americas")),
                menuItem("Terrorism/Gang/Drug Factor", tabName = "fac", icon = icon("tablets")),
                menuItem("Gun Laws", tabName = "glaw", icon = icon("gavel")),
                menuItem("Gun Violence Category", tabName = "cat", icon = icon("chart-bar"))
            )
        ),
        dashboardBody(
            tabItems(
              # home tab content
                tabItem(tabName = "home",
                        h3(strong("About the App:", style = "color:black")),
                        p("The goal of this app is to help users understand the relation between Gunlaws in different states of USA and the number of Gun violence incidents in those states.
                          The user also has the options of seeing what specific laws are present in which state and also and overview of level of strictness in gun laws in different states
                          There are also options to view what factor does Terrorisam,Drugs and Gang activities contribute to Gun Violence 
                          and finally there is the option to see what specific category of gun violence has how many incidents and how many of these categories were accidents and self harm cases.", 
                          style = "color:black"), 
                        
                        p(h3(strong("Description of Tabs:", style = "color:black"))),
                        p(h5(strong("Home:", style = "color:black"))), 
                        p("This tab contains breif description of all tabs", style = "color:black"),
                        
                        p(h5(strong("Violence VS Gun Laws:", style = "color:black"))),
                        p("This tab contains choropleth for incidents per 100000 inhabitants. Options that are provided to the user are  selecting the period, check box to view strictness choropleth", style = "color:black"),
                        
                        p(h5(strong("Terrorism/Gang/Drug Factor:", style = "color:black"))),
                        p("This tab contains the bubbel map which shows the terrorism, drugs and gang activity factor. Users have the option to select which factor they want to view by clicking on the radio button.", style = "color:black"),
                     
                        p(h5(strong("Gun Laws:", style = "color:black"))), 
                        p("This window contains choropleth for different types of gun laws in each state of USA. Options that are provided to the user is selecting type of gun law by clicking on the radio buttons for different laws", style = "color:black"),
                        
                        p(h5(strong("Gun Violence Category:", style = "color:black"))), 
                        p("This tab contains a bar graph to show the different types of gun violence categories and number of incidents in each category. Users are provided with the option of selecting top N categories by moving the sliders bar and also a checkbox to see self-harm/accidents/self-defence categories.", style = "color:black")),
                
                # First tab content
                tabItem(tabName = "relation",
                        h3("Choropleth showing incidents per 100,000 inhabitants"),
                        p("By selecting the period you can view the number of gun violence incidents in that period"),
                        p("By clicking on the check box you can view the level of strictness on gun laws"),
                        fluidRow(sidebarPanel(width=5,
                                              dateRangeInput(inputId = "start",
                                                             label = "Select period",
                                                             start = min(usa_vio$date),
                                                             end = max(usa_vio$date),
                                                             min = min(usa_vio$date),
                                                             max = max(usa_vio$date)),
                                              checkboxInput("stri","Strictness of law choropleth", FALSE)
                        ),
                        mainPanel(width=10,height=10,
                                  fluidRow(
                                      leafletOutput("mymap")
                                  ))
                        )
                ),
                
                # Second tab content
                
                tabItem(tabName = "fac",
                        
                        h3("Bubble Map to show Terrorism/Gang/Drug factor since 2013 to 2018"),
                        p("By clicking on the radio buttons bubbel maps for different factors can be viewed"),
                        p("On hovering over the bubble Location and number of victims can be viewed."),
                        
                        sidebarLayout(position="left",
                                      sidebarPanel(radioButtons("fac", "Law type:",
                                                                c("Terrorism" = "Terrorism Involvement",
                                                                  "Gang Activity" = "Gang involvement",
                                                                  "Drugs" = "Drug involvement"))) ,
                                      mainPanel(leafletOutput("factor"))
                        )
                ),
                
                # third tab content
                tabItem(tabName = "glaw",
                        h3("Choropleth to show if the selected law is present in that state or not"),
                        p("By clicking on the radio buttons Choropleth for different factors can be viewed"),
                        p("On hovering over the map name of the state can be viewed"),
                        sidebarLayout(position = "left", 
                                      sidebarPanel(radioButtons("law", "Law type:",
                                                                c("Firearm Registration Required" = "states$Firearm.Registration.Required",
                                                                  "Permit Required To Carry" = "states$Permit.Required.to.Carry.",
                                                                  "Permit Required To Purchase" = "states$Permit.Required.to.Purchase.",
                                                                  "Open Carry Legal" = "states$Open.Carry.Legal."))),
                                      mainPanel(leafletOutput("gun_law"))
                        )
                ),
                
                # Fourth tab content
                tabItem(tabName = "cat",
                        h3("Bar graph to show top N gun violence categories"),
                        p("Top N categories can be selected by moving the slider"),
                        p("On hovering over bar graph exat number of incidents and category name can be viewed"),
                        sidebarLayout(position="left",
                                      sidebarPanel(width=3,sliderInput(inputId = "num",
                                                               label = "Choose Top N states with most incidents",
                                                               value = 10,
                                                               min = 1,
                                                               max = 30),
                                                   checkboxInput("Dead","Self-Harm/Accidental/Self-Defense", FALSE)),
                                      mainPanel(plotlyOutput("vcat")))
                )
                
            )
        )
    )
)

#server logic required to draw visualisations
server <- function(input, output) {

    #Window 1 Server code
    year_cho <-  observeEvent(input$start,{ 
        year_cho <- usa_vio %>% filter(usa_vio$date >= input$start[1] & usa_vio$date <= input$start[2])
        incidents_state <- year_cho  %>% group_by(State) %>% summarize(stateIncidents=n())
        incidents_state <-left_join(incidents_state, usa_pop, by="State")
        incidents_state <-left_join(incidents_state, gun_law, by="State")
        incidents_state$Per100000 <- round((incidents_state$stateIncidents/incidents_state$POPESTIMATE2017)*100000)
        
        addPer100k <- data.frame(id=states$GEOID, name=states$NAME)
        addPer100k <- left_join(addPer100k, incidents_state %>% select(State, Per100000,Strictness) %>% rename(name=State), by="name")
        addPer100k$Per100000[is.na(addPer100k$Per100000)] <- 0
        states$per100k <- addPer100k$Per100000
        states$Strictness <- addPer100k$Strictness
        
       
        check <-  observeEvent(input$stri,{
            
          # condition for check box
            if(input$stri==TRUE){
              
              # Strictness choropleth
              #Leaflet for R - Introduction. (2020). Retrieved 19 June 2020, from https://rstudio.github.io/leaflet/
                bins <- c(0, 0.25, 0.5, 0.75, 1)
                pal1 <- colorBin("RdYlGn", domain = states$Strictness, bins = bins)
                state_popup1 <- paste0("<strong>State: </strong>",
                                       states$NAME,
                                       "<br><strong>Strictness </strong>",
                                       states$Strictness) %>% lapply(htmltools::HTML)
                output$mymap<-renderLeaflet({
                    leaflet(data = states) %>%
                        setView(lng=-96, lat=37.8, zoom=4) %>%
                        addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",
                                                                                 accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                        addPolygons(
                            fillColor = ~pal1(states$Strictness),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.7,
                            highlight = highlightOptions(
                                weight = 5,
                                color = "#666",
                                dashArray = "",
                                fillOpacity = 0.7,
                                bringToFront = TRUE),
                            label = state_popup1,
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>%
                        addLegend(pal = pal1, values = ~Strictness, opacity = 0.7, title = "Strictness", position = "bottomright")
                    
                })
            }
          
            #incidents per100k map
            #Leaflet for R - Introduction. (2020). Retrieved 19 June 2020, from https://rstudio.github.io/leaflet/
            else{ pal1 <- colorNumeric("YlOrRd", domain=states$per100k)
            state_popup1 <- paste0("<strong>State: </strong>",
                                   states$NAME,
                                   "<br><strong>Incident Per100,000 inhabitants </strong>",
                                   states$per100k,
                                   "<br><strong>Strictness </strong>",
                                   states$Strictness) %>% lapply(htmltools::HTML)
            output$mymap<-renderLeaflet({
                leaflet(data = states) %>%
                    setView(lng=-96, lat=37.8, zoom=4) %>%
                    addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",
                                                                             accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                    addPolygons(
                        fillColor = ~pal1(states$per100k),
                        weight = 2,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            dashArray = "",
                            fillOpacity = 0.7,
                            bringToFront = TRUE),
                        label = state_popup1,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
                    addLegend(pal = pal1, values = ~states$per100k, opacity = 0.7, title = "Incidents", position = "bottomright")
                
            })
            }
        })
    })    
    
    #Window 2 Server code 
    
    # customised legend for bubble map
    #Swan, H. (2019). Custom legend with R leaflet- circles and squares in same plot legends. Retrieved 19 June 2020, from https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
    circle_legend <- function(map, colors, labels, sizes, opacity = 0.5){
        colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
        labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
        
        return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = "Victims", position = "bottomright"))
    }
    
    # bubble map code
    output$factor<-renderLeaflet({
        fact_data <- inc_cat %>% filter(incident_characteristics==input$fac)
        fact_data <- left_join(fact_data, usa_vio %>% select(incident_id, longitude, latitude, location_description, victims), by="incident_id")
        
        labels <- paste0("<strong>City: </strong>", fact_data$city_or_county, 
                         "<br><strong>Location: </strong>", fact_data$location_description,
                         "<br><strong>Victims </strong>", fact_data$victims) %>% lapply(htmltools::HTML)
        leaflet(fact_data) %>%
            setView(lng=-96, lat=37.8, zoom=4) %>%
            addTiles() %>%
            addCircleMarkers(~longitude, ~latitude, color = "blue", radius=~sqrt(victims), label = labels)%>%
            circle_legend(colors = "blue", labels = c("0-10", "10-150", "150>"), sizes = c(10, 20, 40))
        
    })
    
    
    
    #Window 3 Server code   
    
    # conditions for radio buttons
    gLaw <-  observeEvent(input$law,{ 
        
        if(input$law=="states$Firearm.Registration.Required"){
            gLaw <- states$Firearm.Registration.Required
            law_name = "Firearm Registration Required"
        }
        
        if(input$law=="states$Permit.Required.to.Carry."){
            gLaw <- states$Permit.Required.to.Carry.
            law_name = "Permit Required to Carry"
        }
        
        if(input$law=="states$Permit.Required.to.Purchase."){
            gLaw <- states$Permit.Required.to.Purchase.
            law_name = "Permit Required to Purchase"
        }
        
        if(input$law=="states$Open.Carry.Legal."){
            gLaw <- states$Open.Carry.Legal.
            law_name = "Open Carry Legal"
        }
        
      # code for individual gun law choropleths
      #Leaflet for R - Introduction. (2020). Retrieved 19 June 2020, from https://rstudio.github.io/leaflet/
        output$gun_law<-renderLeaflet({
            pal <- colorFactor(palette=c("Red","Blue"), domain = gLaw)
            
            state_popup <- paste0("<strong>State: </strong>", 
                                  states$NAME, 
                                  "<br><strong>Law Present</strong>", 
                                  gLaw) %>% lapply(htmltools::HTML)
            
            leaflet(data = states) %>%
                setView(lng=-96, lat=37.8, zoom=4) %>%
                addProviderTiles("MapBox", options = providerTileOptions(id = "mapbox.light",
                                                                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
                addPolygons(
                    fillColor = ~pal(gLaw),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = state_popup,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")) %>%
                addLegend(pal=pal, values=gLaw, opacity = 0.7, title = law_name, position = "bottomright")
        })
        
    })
    
    # Window 4 server code
    #Fitton, D. (2020). Plotly in R: How to make ggplot2 charts interactive with ggplotly - Blog - Musgrave Analytics. Retrieved 19 June 2020, from https://www.musgraveanalytics.com/blog/2018/8/24/how-to-make-ggplot2-charts-interactive-with-plotly
    
    observeEvent(input$Dead,{
        # condition for check box
        if(input$Dead){
          #code for self-harm/accidental categories bar graph
            output$vcat<-renderPlotly({
                ggplotly(CatTable %>% 
                             ggplot(aes(x=reorder(incident_characteristics, n), y=n, text=paste0("Category: ",incident_characteristics,
                                                                                                 "<br> Number of Incidents: ",n))) +
                             geom_bar(stat='identity', fill='red') + coord_flip() +
                             labs(x='Incident Category', y='Number of incidents', title="Number of incidents by category"),
                         tooltip=c("text"))
            })
        }
        
        else{  
          #code for  categories bar graph
            output$vcat<-renderPlotly({
                ggplotly(inc_cat %>% count(incident_characteristics) %>% top_n(input$num, wt=n) %>%
                             ggplot(aes(x=reorder(incident_characteristics, n), y=n,text=paste0("Category: ",incident_characteristics,
                                                                                                "<br> Number of Incidents: ",n))) +
                             geom_bar(stat='identity',fill="dodgerblue") +
                             coord_flip() + labs(x='Incident Category', y='number of incidents',title="Number of incidents by category"),
                         tooltip=c("text"))
            })
        }
    })   
}

# Run the application 
shinyApp(ui = ui, server = server)
