#selected packages that my users potentially need to download



#library that i used to build my shiny app
library(shiny)
library(leaflet)
library(RColorBrewer)
#install.packages("shinydashboard")
library(shinydashboard)
#install.packages('htmltools')
library(htmltools)
#install.packages('plotly')
library(plotly)

#install.packages('devtools'd)
#library(devtools)
#install_github("nik01010/dashboardthemes")
#if needing to instal this library ran the code above, this is not installed from the Cran packages
library(dashboardthemes)


#install.packages('dplyr')
library(dplyr)
#install.packages('data.table')
library(data.table)
#note for the project to work both rgdal and sp must be the R released versions. 
#install.packages('rgdal')
library(rgdal)
#install.packages('sp')
library(sp)
#install.packages('shinyalert')
library(shinyalert)





#loading crash data
crash<-read.csv('crash_dataframe.csv')
#changing the data structure
crash$crash_date=as.Date(crash$crash_date, "%Y-%m-%d")
crash$year<-format(crash$crash_date,"%Y")

#loading offence data
offence1<-read.csv('merged_offence_location1.csv')
#changing the data structure
offence1$Offence_Month=as.Date(offence1$Offence_Month, "%d/%m/%y")

#shapefile
act_map <- readOGR("ACT_LOCALITY_POLYGON_shp.shp")


#ui, with the three main parts, header, sidebar, body
ui <- dashboardPage(
  dashboardHeader(title='Menu Bar'),
  dashboardSidebar(
    #four main pages
    sidebarMenu( menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
                 menuItem("Car Crash", tabName = "Car_Crash", icon = icon("chart-bar")),
                 menuItem("Traffic Offence", tabName = "Traffic_Offence", icon = icon("th")),
                 menuItem("Crashes VS Offences", tabName = "Crash_VS_Offences", icon = icon("calendar")))),
  
  dashboardBody(
    #welcome message because the dataset using is big:
    useShinyalert(),
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),
    tabItems(
      
      # First tab content
      tabItem(tabName = "Introduction",
              #the title of the project
              h1(span("The Car Crash trends and Traffic Offence trends in ACT", 
                      style = "font-weight: 400"), 
                 style = "font-family: 'Arial'; color: #fff; font-size:45px; text-align: center;
                 background-image: url('Act1.png');padding: 350px")
              ),
      
      # Second tab content
      tabItem(tabName = "Car_Crash",
              fluidRow(
                column(width = 4,
                       #information     
                       box(title='Information',collapsible = TRUE,width=NULL,"The page focues on the interactive exploration of car crash trends."
                           ,br(),'   ',br(),"you are able to see the trends on different types of crash over years.",
                           "and explore the effects of weather and lighting conditions",br(),
                           '   ',br(),'BEFORE YOU START, please click on the menu bar icon to hide the menu for a better experience.'
                           ,br()," " ,br(),'Enjoy your journey !'),br(),' ',br(),
                       
                       #main input for the tab, year and crash severity
                       box(title = "Inputs", width =NULL,height = 600,status = "info", solidHeader = TRUE,
                           sliderInput("Year", "Year:",
                                       min = 2012, max = 2019,
                                       value = 2019),br()," ",br(),
                           selectInput('Crash_Severity', 'Crash Severity', c('All','Property Damage Only','Injury','Fatal'), 
                                       selected = 'All', multiple = FALSE,
                                       selectize = TRUE, width = NULL, size = NULL),br(),' ',br(),'*mouse over the graph(below) for more details',
                           #little line graph for the trend
                           plotlyOutput('linecrash'))),
                #progress box and donut and sunburst 
                column(width = 4,valueBoxOutput("progressBox",width=12),
                       box(width=12,height=380,title='Crash Severity Proportions','*mouse over the graph(below) for more details',plotlyOutput('barcrash')),
                       box(width=12,height=380,title='Weather and lighing conditions','*click on the weather(below) for more details',plotlyOutput('sunburstcrash'))),
                #leaflet dot density map with selections for grouping the dots by different colours
                column(width=4,valueBoxOutput("progressBox2",width=12),box(width=12,title='car crash hotspots','*mouse over  the dots on the map for more details',leafletOutput("crashdotmap"),selectInput('dots', 'Group map dots by:', c('Weather','Lighting Conditions','Crash Severity','None'), selected = 'None', multiple = FALSE,
                                                                                                                                                                                                            selectize = TRUE, width = NULL, size = NULL)),' ',br(),' ',br(),' ',br(),
                       
                       #data source window
                       box(width=12,title='Data Source',status='warning',solidHeader = TRUE,
                           'Thank you for visiting this site!',br(),' ',br(),
                           'Hopefully, you gained a better understanding of the car crash trends.',
                           br(),' ',br(),'The data source come from the ACT open data website:',br(),
                           tags$a(href="https://www.data.act.gov.au/Transport/ACT-Road-Crash-Data/6jn4-m8rx", "Click Here")
                       )
                       
                       
                ))),
      
      
      # Third tab content
      tabItem(tabName = "Traffic_Offence",fluidRow(
        
        
        column(width = 4,
               # Introduction of the pages
               box(title='Information',collapsible = TRUE,width=NULL,"Now you have gained some understandings of the car crashes trends"
                   ,br(),'   ',br(),"Lets Look at the traffic offence trends in ACT!!",
                   "You are able to explore the different offence types and penalty amounts trends, as well as the locations of offences in this page",br(),
                   '   ',br(),'BEFORE YOU START, please click on the menu bar icon to hide the menu for a better experience.'
                   ,br()," " ,br(),'Enjoy your journey !'),
               #main inputs, year and offence types
               box(title = "Inputs", width =NULL,height = 630,status = "info", solidHeader = TRUE,'*School zone data were recorded since 2015',br(),
                   sliderInput("offenceyear", "Year:",
                               min = 2012, max = 2019,
                               value = 2019),br()," ",br(),'*SZ=School Zone, NSZ=Non-School Zone',
                   selectInput('Oftype', 'Offence Type:', c('All', "Speed- <=15Km/H(SZ)","Speed- 15-30Km/H(SZ)","Speed- >30Km/H(SZ)",
                                                            "Speed- <=15Km/H(NSZ)","Speed- 15-30Km/H(NSZ)","Speed- >30Km/H(NSZ)",
                                                            "Traffic Light Red" ), 
                               selected = 'All', multiple = FALSE,
                               selectize = TRUE, width = NULL, size = NULL),br()," " ,
                   #line plot for the line offence
                   br(),'*mouse over the graph(below) for more details',plotlyOutput('lineoffence')),
               
               #data source for tab 3
               box(width=12,title='Data Source',status='warning',solidHeader = TRUE,
                   'Thank you for visiting this site!',br(),' ',br(),
                   'Hopefully, you gained a better understanding of the Offence trends.',
                   br(),' ',br(),'The data source come from the ACT open data website:',
                   tags$a(href="https://www.data.act.gov.au/Transport/Traffic-camera-offences-and-fines/2sx9-4wg7", "Click Here(source1)"),br(),
                   tags$a(href="https://www.data.act.gov.au/Justice-Safety-and-Emergency/Traffic-speed-camera-locations/426s-vdu4", "Click Here(source2)"))
        ),
        #donut chart for the proportion of the offences
        column(width = 4,"*mouse over the graph(below) for more details",box(width=12,height=280,plotlyOutput('donutoff'))),
        #two value boxes, for the total offence number  and the particular offence number of the year
        column(width = 4,valueBoxOutput("progressBox3",width=12),valueBoxOutput("progressBox4",width=12),box(width=12,height=75,'*If the labels in the donut plot is not loaded properly,                                                                                             please select the year and the type of offence again.')),
        #bubble map, interactive, it changes as the type and the year changes
        column(width = 8,'*mouse over the graph(below) for more details',box(width=12,plotlyOutput("bubble")),box(width=12,title='Offence Hotspots','*mouse over the map(below) for more details',leafletOutput('offencebubblemap')))
        
      )
      
      ),
      
      
      #fourth tab
      tabItem(tabName = "Crash_VS_Offences",
              # the values boxes represent the number and % changes of the offence/crash type of selected in the particular year
              fluidRow(valueBoxOutput('progressBox5',width=3),valueBoxOutput('progressBox6',width=3),valueBoxOutput('progressBox7',width=3),valueBoxOutput('progressBox8',width=3)),
              
              fluidRow(column(width = 4,
                              #main information tab for the introduction of the page
                              
                              box(title='Information And Data Source',collapsible = TRUE,width=NULL,"Now you have gained some understandings of the car crashes and offence trends independently."
                                  ,br(),'   ',br(),"Lets Look at these trends toghether !!",
                                  br(),
                                  '   ',br(),'BEFORE YOU START, please click on the menu bar icon to hide the menu for a better experience.'
                                  ,br()," " ,br(),'Enjoy your journey !',br(),' ',br(),'Data Source:',br(),
                                  tags$a(href="https://data.gov.au/dataset/ds-dga-83468f0c-313d-4354-9592-289554eb2dc9/details", "Click Here(ShapeFile)")),
                              #main select inputs for the page year,crash severity,offence type                     
                              box(title = "Inputs", width =NULL,height = 700,status = "info", solidHeader = TRUE,'*School zone data were recorded since 2015',br(),
                                  sliderInput("yeartotal", "Year:",
                                              min = 2012, max = 2019,
                                              value = 2019),br()," ",br(),
                                  
                                  selectInput('Crash_Severity2', 'Crash Severity', c('All','Property Damage Only','Injury','Fatal'), 
                                              selected = 'All', multiple = FALSE,
                                              selectize = TRUE, width = NULL, size = NULL),
                                  
                                  
                                  '*SZ=School Zone, NSZ=Non-School Zone',
                                  selectInput('Oftype2', 'Offence Type:', c('All', "Speed- <=15Km/H(SZ)","Speed- 15-30Km/H(SZ)","Speed- >30Km/H(SZ)",
                                                                            "Speed- <=15Km/H(NSZ)","Speed- 15-30Km/H(NSZ)","Speed- >30Km/H(NSZ)",
                                                                            "Traffic Light Red" ), 
                                              selected = 'All', multiple = FALSE,
                                              selectize = TRUE, width = NULL, size = NULL),br(),'   ',br(),'*mouse over the graph(below) for more details',
                                  #lineplot for the trends of crash and offences               
                                  plotlyOutput('lineboth')
                              )),
                       
                       # the map for offences, it can be either chloropleth or proportional symbol map
                       column(width=8,box(width=12,selectInput('offencemap', 'Map type for traffic offences:', c('Chloropleth', "Proportional symbol map"), 
                                                               selected = 'Chloropleth', multiple = FALSE,selectize = TRUE, width = NULL, size = NULL),'*mouse over the map(below) for more details',leafletOutput('offencechloro')),
                              
                              # the map for crahses, it can be either chloropleth or dot density map
                              
                              box(width=12,selectInput('crashmap', 'Map type for car crashes:', c('Chloropleth', "Dot density map"), 
                                                       selected = 'Chloropleth', multiple = FALSE,selectize = TRUE, width = NULL, size = NULL),'*mouse over the map(below) for more details',leafletOutput('crashchloro')))
                       
              )))))












#input, output, session function
server<- function(input,output,session){
  #welcome message
  shinyalert("Welcome", paste("Thank you for visiting my project!!!",'The project will give you a better understanding of the car crashes and traffic offences.
                              Please be patient while the data is loading, especially with the fourth page, as my datasets are big with over 150k rows in total.' ), type = "info")
  
  
  #car crash trend line plot, adjust the frequency table
  output$linecrash <- renderPlotly({
    #two type of crash severity leads to different ways of filtering the dataframe
    if (input$Crash_Severity=='All'){
      #we only showing data from 2012-2019, as 2020 data isnt complete
      crash<- crash %>% filter(year!=2020)
    } else{
      #we only showing data from 2012-2019, as 2020 data isnt complete
      crash<-crash[crash$year!=2020&crash$crash_severity==input$Crash_Severity,]
    } 
    
    #setting up the frequency table
    crashno<-data.frame(table(crash$year))
    names(crashno)[1]<-'year'
    names(crashno)[2]<-'crash_no'
    levels(crashno$year)<-c('2012','2013','2014','2015','2016','2017','2018','2019')
    
    #margin settings for the graph 
    m <- list(
      l = 20,
      r = 10,
      b = 5,
      t = 20,
      pad = 4
    )
    #interactive plot_ly graph
    fig <- plot_ly(crashno, x = ~year)
    fig <-fig %>% add_lines(y = ~crash_no, name = 'crash no',line = list(color = 'black', width = 2))
    fig <- fig %>% layout(title = paste(input$Crash_Severity,'Crash trend'),font=list(color="white",size=9),
                          yaxis = list(title = "crash number",showgrid = FALSE,zeroline = FALSE,linewidth=0,color='black',tickangle = 90),
                          xaxis = list(title="year",showgrid = FALSE,zeroline = FALSE,linewidth=0,color='black'),
                          showlegend = FALSE) %>%layout(paper_bgcolor='transparent',plot_bgcolor='transparent')%>% 
      layout(autosize = T,margin = m,height=230)
    # having the rectangle shapes for highlighing the area of the year selected by the select input
    fig <- layout(fig, shapes = 
                    list(type = "rect",
                         fillcolor = "red", line = list(color = "red"),opacity = 0.2,
                         x0 = input$Year-2011.5, x1 = input$Year-2012.5, xref = "x",y0 = 0, y1 = 1, yref = "paper"))
    
    fig
    
    
    
    
  })
  
  
  #dot density map for the crash hotspots 
  output$crashdotmap<-renderLeaflet({
    
    #different types of crash severity lead to different ways of filtering the dataframe
    if (input$Crash_Severity=='All'){
      data<-crash[crash$year==input$Year,]
    } else{
      data<-crash[crash$year==input$Year& crash$crash_severity==input$Crash_Severity,]
    }
    
    #depending on the input select dot option, we group the colours of the map by different types
    if(input$dots=='None'){
      col='#339999'
      #setting the labels for display by using html tool for the format
      labels <- sprintf(
        "<strong>Date: </strong>%s<br/><strong>Severity: </strong>%s<br/><strong>Weather: </strong>%s<br/><strong>Lighting: </strong>%s<br/>",
        data$crash_date,data$crash_severity,data$weather_condition,data$lighting_condition
      ) %>% lapply(htmltools::HTML)
      
      #actual leaflet map
      leaflet(data = data) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter)%>% # the dark layout
        addCircleMarkers(~x, ~y,color = col,fillColor = col,
                         fillOpacity = 0.6, 
                         radius=1, stroke=FALSE, weight = 0.5,label=labels,
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "9px",
                           direction = "auto")) %>%
        setView(lat = -35.2809, lng = 149.1300, zoom = 11)
      
    } else if(input$dots=='Weather'){  
      #weather as the grouping factor, 9 different weather types, so gives 9 different colours 
      pal <-colorFactor(palette = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6'), 
                        levels = levels(crash$weather_condition))
      col<-pal(data$weather_condition)
      labels <- sprintf(
        "<strong>Date: </strong>%s<br/><strong>Severity: </strong>%s<br/><strong>Weather: </strong>%s<br/><strong>Lighting: </strong>%s<br/>",
        data$crash_date,data$crash_severity,data$weather_condition,data$lighting_condition
      ) %>% lapply(htmltools::HTML)
      
      #actual leaflet map, colour of dots group by weather
      leaflet(data = data) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter)%>% 
        addCircleMarkers(~x, ~y,color = col,fillColor = col,
                         fillOpacity = 0.6, 
                         radius=1, stroke=FALSE, weight = 0.5,label=labels,
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "9px",
                           direction = "auto")) %>%
        setView(lat = -35.2809, lng = 149.1300, zoom = 11)%>%  
        addLegend(pal = pal, values =~weather_condition, opacity = 1,position='bottomright')
      
      
    } else if(input$dots=='Lighting Conditions'){
      a<-'lighting_condition'
      #lighting conditions as the grouping factor, number of levels=6
      pal<-colorFactor(palette=c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c'),
                       levels = levels(crash$lighting_condition))
      
      col<-pal(crash$lighting_condition)
      labels <- sprintf(
        "<strong>Date: </strong>%s<br/><strong>Severity: </strong>%s<br/><strong>Weather: </strong>%s<br/><strong>Lighting: </strong>%s<br/>",
        data$crash_date,data$crash_severity,data$weather_condition,data$lighting_condition
      ) %>% lapply(htmltools::HTML)
      
      #actual leaflet map, colour of dots group by lighting
      
      leaflet(data = data) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter)%>% 
        addCircleMarkers(~x, ~y,color = col,fillColor = col,
                         fillOpacity = 0.6, 
                         radius=1, stroke=FALSE, weight = 0.5,label=labels,
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "9px",
                           direction = "auto")) %>%
        setView(lat = -35.2809, lng = 149.1300, zoom = 11) %>%  addLegend(pal = pal, values =~lighting_condition,
                                                                          opacity = 1,position='bottomright')
      
      
    } else if(input$dots=='Crash Severity'){
      #three types of crash severity 
      pal<-colorFactor(palette=c('#a6cee3','#1f78b4','#b2df8a'),
                       levels = levels(crash$crash_severity))
      
      col<-pal(crash$crash_severity)
      
      labels <- sprintf(
        "<strong>Date: </strong>%s<br/><strong>Severity: </strong>%s<br/><strong>Weather: </strong>%s<br/><strong>Lighting: </strong>%s<br/>",
        data$crash_date,data$crash_severity,data$weather_condition,data$lighting_condition
      ) %>% lapply(htmltools::HTML)
      
      leaflet(data = data) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter)%>% 
        addCircleMarkers(~x, ~y,color = col,fillColor = col,
                         fillOpacity = 0.6, 
                         radius=1, stroke=FALSE, weight = 0.5,label=labels,
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "9px",
                           direction = "auto")) %>%
        setView(lat = -35.2809, lng = 149.1300, zoom = 11) %>%  addLegend(pal = pal, values =~crash_severity,
                                                                          opacity = 0.7,position='bottomright')
    }
  })
  
  #donut graph for the proportion of the different crash severity
  output$barcrash<-renderPlotly({
    
    data<- crash[crash$year==input$Year,]
    #depending on the input of the severity, we highlight different region of the bar, and setting the rest as grey
    if (input$Crash_Severity=='All'){
      col_bar<-c('#ff26c9','#7affb8',"#bf28ff")
    } else if(input$Crash_Severity=='Property Damage Only'){
      col_bar<-c('#f0f0f0','#f0f0f0',"#bf28ff")
    } else if(input$Crash_Severity=='Injury'){
      col_bar<-c('#f0f0f0','#7affb8','#f0f0f0')
    } else if(input$Crash_Severity=='Fatal'){
      col_bar<-c('#ff26c9','#f0f0f0','#f0f0f0')
    }
    #data preparing for the donut graph
    crash_type<-data.frame(table(data$crash_severity))
    crash_type$perc<-(crash_type$Freq)/sum(crash_type$Freq)
    names(crash_type)[1]='Crash_type'
    
    #setting the margin of the graph
    m <- list(
      l = 5,
      r = 5,
      b = 5,
      t = 8,
      pad = 4
    )
    
    #drashing the donut graph
    fig <- crash_type %>% plot_ly(labels = ~Crash_type, values = ~Freq,marker = list(colors = col_bar,
                                                                                     line = list(color = '#FFFFFF', width = 0.3)),
                                  textinfo = 'label+percent',textfont = list(color = 'white',size=9))
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%layout(paper_bgcolor='transparent')%>% 
      layout(autosize = T,margin = m,height=300)
    
    fig})
  
  
  #value box for the total number of the car crash at the particular year
  output$progressBox <- renderValueBox({
    sum_crash<-length(crash[crash$year==input$Year,]$crash_severity)
    valueBox(
      paste0(sum_crash), paste("Total Car Crashes in ",input$Year),
      color = "fuchsia")
  })
  
  #value box for the number of the selected car crash at the particular year
  output$progressBox2 <- renderValueBox({
    #depending on the selected crash severity, we calculate the sum crash at different ways
    if (input$Crash_Severity=='All'){
      sum_crash<-length(crash[crash$year==input$Year,]$crash_severity)
    } else{
      sum_crash<-length(crash[crash$year==input$Year&crash$crash_severity==input$Crash_Severity,]$crash_severity)
    } 
    valueBox(
      paste0(sum_crash), paste(input$Crash_Severity,"Crashes in ",input$Year),color = "olive")
  })
  
  
  #sunburst graph to look at the different weather, lighting, proportions
  output$sunburstcrash<-renderPlotly({
    
    if (input$Crash_Severity=='All'){
      crash<-crash[crash$year==input$Year,]
    } else{
      crash<-crash[crash$year==input$Year&crash$crash_severity==input$Crash_Severity,]
    }
    
    #preparing the data for the sunburst plot
    #for weather conditions
    table1<-data.frame(table(crash$weather_condition))
    names(table1)[1]<-'label'
    # the table has to have parents, label and frequency
    # lighting conditions's parent is weather, weather's parents is car crash
    table1$parent<-'Car Crash'
    
    levels(table1$label) = c(levels(table1$label),'Car Crash')
    row<-c('Car Crash',length(crash$crash_id),"")
    table1<-rbind(table1,row)
    #for lighting conditions 
    table2<-crash %>% group_by(weather_condition) %>% count(lighting_condition)
    # the table has to have parents, label and frequency
    # the table has to have parents, label and frequency
    # lighting conditions's parent is weather, weather's parents is car crash
    names(table2)[1:3]<-c('parent','label','Freq')
    table2 <- table2[c(2,3,1)]
    total<-rbind(table1,table2)
    
    m <- list(
      l = 5,
      r = 5,
      b = 5,
      t = 5,
      pad = 4
    )
    
    #sunburst plotting with plotly library
    fig<-plot_ly(
      total,
      type='sunburst',
      labels =~label,parents = ~parent, marker = list(line = list(color = '#FFFFFF', width = 0.3)),
      values = ~Freq,branchvalues = 'total',textfont = list(color = 'white'))%>%layout(colorway = c(
        "#636efa","#e763fa","#ab63fa","#3bd1ef","#b6e880","#ef553b",'#00cc96','#19d3f3',"#FFA15A"))%>% layout(showlegend = F,
                                                                                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                                                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>%layout(paper_bgcolor='transparent')%>% 
      layout(autosize = T,margin = m,height=300)
    fig
    
  })
  
  #tab three graphs
  #offence line plot
  output$lineoffence<-renderPlotly({
    #data selection based on the select input
    if (input$Oftype=='All'){
      
      offenceno<-data.frame(table(offence1$Year))
      offenceno$Var1<-as.numeric(levels(offenceno$Var1))
      
    } else{
      offenceno<-data.frame(table(offence1[offence1$Offence_Desc==input$Oftype,]$Year))
      offenceno$Var1<-as.numeric(levels(offenceno$Var1))
      
    }
    
    #alter dataframe, because no data is recorded for the sz speeding before 2015, for <=15,15-30,>30
    if(input$Oftype=='Speed- <=15Km/H(SZ)'|input$Oftype=='Speed- 15-30Km/H(SZ)'|input$Oftype=='Speed- >30Km/H(SZ)'){
      offenceno=offenceno[-length(offenceno$Var1),]
      offenceno<-rbind(offenceno,c(2012,0),c(2013,0),c(2014,0))
      
      #alter dataframe, because no data is recorded for the sz speeding for 2015 >30
      
      if (input$Oftype=='Speed- >30Km/H(SZ)'){
        offenceno<-rbind(offenceno,c(2015,0))
      }
      
    } else{
      #otherwise we delete the data for 2010,2011,2020
      offenceno=offenceno[c(-1,-2,-11),]
    }
    
    
    names(offenceno)[1]<-'year'
    names(offenceno)[2]<-'offence_no'
    
    
    m <- list(
      l = 20,
      r = 10,
      b = 5,
      t = 20,
      pad = 4
    )
    
    
    #plotly plotting the line plots for offence trends
    fig <- plot_ly(offenceno, x = ~year)
    fig <- fig %>% add_lines(y = ~offence_no, name = "offence no",line = list(color = 'black', width = 2))
    
    fig <- fig %>% layout(title = paste(input$Oftype,'Offences'),font=list(color="white",size=9),
                          yaxis = list(title = "offence number",showgrid = FALSE,zeroline = FALSE,linewidth=0,color='black',tickangle = 90),
                          xaxis = list(title="year",showgrid = FALSE,zeroline = FALSE,linewidth=0,color='black'),
                          showlegend = FALSE) %>%layout(paper_bgcolor='transparent',plot_bgcolor='transparent')%>% 
      layout(autosize = T,margin = m,height=220)
    #adding the rectangle shape to highlight the year of selected
    fig <- layout(fig, shapes = 
                    list(type = "rect",
                         fillcolor = "red", line = list(color = "red"),opacity = 0.2,
                         x0 = input$offenceyear-0.5, x1 = input$offenceyear+0.5, xref = "x",
                         y0 = 0, y1 = 1, yref = "paper"))
    
    fig
    
    
    
    
    
    
  })
  
  
  #value box for the total number of offences in the year of interests
  output$progressBox3 <- renderValueBox( {
    #select the year from the select input
    total_offence<-length(offence1[offence1$Year==input$offenceyear,]$Offence_Desc)
    
    #creare the total offence box
    valueBox(total_offence,paste("Total offences in ",input$Year),
             color = "fuchsia")})
  
  #value box for the number of offences of interests in the year of interests
  output$progressBox4 <- renderValueBox( {
    #select the year from the select input
    if(input$Oftype=='All'){
      type_offence<-length(offence1[offence1$Year==input$offenceyear,]$Offence_Desc)
    } else{
      type_offence<-length(offence1[offence1$Offence_Desc==input$Oftype&offence1$Year==input$offenceyear,]$Offence_Desc)
    }
    
    # for the time when there is no record we tell the user there is no record
    if(type_offence==0){
      valueBox('No record',paste(input$Oftype," offences in ",input$offenceyear),
               color = "teal")}
    
    # for the time when there are records  we print down the offence number
    else{
      valueBox(type_offence,paste(input$Oftype," offences in ",input$offenceyear),
               color = "teal")
    }})
  
  #bubble plot for the offence no,offence type, year and penalty amount
  output$bubble<-renderPlotly({
    
    #prepare the data for bubble plot
    #we only want data between 2012-2019
    offence1<- offence1 %>% filter(Year<2020 & Year>2011)
    data2<-aggregate(Sum_Pen_Amt~Year+Offence_Desc,data=offence1,FUN=sum)
    data1<-offence1 %>% group_by(Year) %>% count(Offence_Desc) %>% right_join(data2,by=c('Year','Offence_Desc'))
    data1$Year=as.factor(data1$Year)
    #scale the data to million
    data1$Sum_Pen_Amt=data1$Sum_Pen_Amt/100000
    
    
    
    
    #plotting the bubble plot
    fig <- plot_ly(data1, x=~Year, y=~Sum_Pen_Amt, hoverinfo ="text",text=~paste("Year: ", Year, "\n Offence type(M): ", Offence_Desc, "\n Offence no: ", n , "\n Penalty amount(in mil): ",Sum_Pen_Amt), type = 'scatter', mode = 'markers',
                   marker = list(size = sqrt(as.numeric(data1$n))/4), 
                   color = ~Offence_Desc,colors =c('#ff26c9','#7affb8',"#bf28ff",'#FFC75F','#00D2FC',"#FF6F91",'#F9F871'))
    
    fig <- fig %>% layout(title = 'Offence Penalty amount over years',font=list(color="white"),
                          yaxis = list(showgrid = FALSE,zeroline = FALSE,linewidth=0,color='white'),
                          xaxis = list(showgrid = FALSE,zeroline = FALSE,linewidth=0,color='white'),
                          showlegend = FALSE) %>%layout(paper_bgcolor='transparent',plot_bgcolor='transparent')
    
    
    fig <- layout(fig, shapes = 
                    list(type = "rect",
                         fillcolor = "red", line = list(color = "red"),opacity = 0.2,
                         x0 = input$offenceyear-2011.5, x1 = input$offenceyear-2012.5, xref = "x",
                         y0 = 0, y1 = 700, yref = "y"))
    
    #when we choose a particular offence type, i add a arrow next to the point of interest as a highlight
    
    if((input$Oftype=="Speed- <=15Km/H(SZ)"|input$Oftype=="Speed- 15-30Km/H(SZ)") & input$offenceyear<2015){
      points=NULL
    }else if(input$Oftype=="Speed- >30Km/H(SZ)" & input$offenceyear<2016){
      points=NULL
    }else if(input$Oftype!='All'){
      points=data1[data1$Year==input$offenceyear&data1$Offence_Desc==input$Oftype,]
      
      fig <- fig %>% add_annotations(x = points$Year,
                                     y = points$Sum_Pen_Amt,
                                     text = ' ',
                                     xref = "x",
                                     yref = "y",
                                     showarrow = TRUE,
                                     arrowhead = 3,
                                     arrowsize = .5,
                                     ax = 20,
                                     ay = -30,arrowcolor='red') }
    
    fig})
  
  
  
  
  # donut plot for the proportion of different offence type
  output$donutoff<-renderPlotly({
    
    # prepare the data set for the offence type
    offence2<-data.frame(table(offence1[offence1$Year==input$offenceyear,]$Offence_Desc))
    offence2$perc<-offence2$Freq/length(offence1[offence1$Year==input$offenceyear,]$Offence_Desc)
    names(offence2)[1]='offence_type'
    
    m <- list(
      l = 35,
      r = 35,
      b = 20,
      t = 40,
      pad = 4)
    #based on the selectinput for offence type, we use different colours and high light different area of the plots
    if (input$Oftype=='All'){
      col=c('#ff26c9','#7affb8',"#bf28ff",'#FFC75F','#00D2FC',"#FF6F91",'#F3C5FF')}
    else if((input$Oftype=="Speed- <=15Km/H(SZ)"|input$Oftype=="Speed- 15-30Km/H(SZ)") & input$offenceyear<2015){
      col=rep('grey',7)
    }else if(input$Oftype=="Speed- >30Km/H(SZ)" & input$offenceyear<2016){
      col=rep('grey',7)} else{
        offencetype=c("Speed- <=15Km/H(NSZ)","Speed- <=15Km/H(SZ)","Speed- >30Km/H(NSZ)","Speed- >30Km/H(SZ)",
                      "Speed- 15-30Km/H(NSZ)","Speed- 15-30Km/H(SZ)",
                      "Traffic Light Red" )
        col1=c('#ff26c9','#7affb8',"#bf28ff",'#FFC75F','#00D2FC',"#FF6F91",'#F3C5FF')
        col=rep('grey',7)
        col[which(offencetype==input$Oftype)]=col1[which(offencetype==input$Oftype)]
      }
    
    #plotting the donut plot
    fig <- offence2 %>% plot_ly(labels = ~offence_type, values = ~Freq, textfont = list(color = 'white', size = 9 ),marker = list(colors = col,                                                                                                              line = list(color = '#FFFFFF', width = 0.3)),textposition = 'auto')
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title='Offence type Proportion',font=list(color = 'white'),showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>% layout(autosize = T,margin = m) %>%layout(paper_bgcolor='transparent',height=250)
  })
  
  
  #proportional symbol map for the traffic offences
  output$offencebubblemap<-renderLeaflet({
    
    #select the data for the particular offence type and the year
    if (input$Oftype=='All'){
      data<-offence1[offence1$Year==input$offenceyear,]
    } else{
      data<-offence1[offence1$Year==input$offenceyear & offence1$Offence_Desc==input$Oftype,]
    }
    #building the right structure for plotting
    p<-data %>%group_by(LATITUDE, LONGITUDE,Camera_Type_x)%>%summarise(n=n())
    p<-p[complete.cases(p), ]
    
    # designing the label when hovoring over the symbol 
    labels <- sprintf("<strong>Year: </strong>%s<br/><strong>Camera Type: </strong>%s<br/><strong>Number of Offences: </strong>%s",
                      input$offenceyear,tolower(p$Camera_Type_x),p$n) %>% lapply(htmltools::HTML)
    #scale the colour greadient
    qpal <- colorNumeric(palette = "PuRd",domain = p$n)  
    #during these two conditions there are no data 
    if((input$Oftype=="Speed- <=15Km/H(SZ)"|input$Oftype=="Speed- 15-30Km/H(SZ)") & input$offenceyear<2015){
      offencemap<-leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%setView(lat = -35.2809, lng = 149.1300, zoom = 11) 
    }else if(input$Oftype=="Speed- >30Km/H(SZ)" & input$offenceyear<2016){
      offencemap<-leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%setView(lat = -35.2809, lng = 149.1300, zoom = 11) 
      
    } else{
      #plotting the proportional symbol map
      offencemap<-leaflet(data =p) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addCircleMarkers(~LONGITUDE, ~LATITUDE,color =~qpal(n),fillColor = ~qpal(n),
                         fillOpacity = 0.6,  radius=~sqrt(n)/2.5, stroke=FALSE, weight = 2,
                         label=labels,
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "10px",
                           direction = "auto"))%>%setView(lat = -35.2809, lng = 149.1300, zoom = 11) }
    
    offencemap
    
  })
  
  
  
  #plots for tab four  
  #maps for car crashes
  output$crashchloro<-renderLeaflet({
    # if the chloropleth option is chosen then we do a chloropleth, otherwise, a dot density plot
    if (input$crashmap=='Chloropleth'){
      
      if (input$Crash_Severity2=='All'){
        data<-crash[crash$year==input$yeartotal,]
      } else{
        data<-crash[crash$year==input$yeartotal & crash$crash_severity==input$Crash_Severity2,]
      }
      
      # we convert the data and filter out all the nulls
      data = as.data.table(data)
      data = data[x!=""]
      data = data[y!=""]
      
      #convert the datatable to a spatial object
      coordinates(data) = c("x","y")
      crs.geo1 = CRS("+proj=longlat")  
      proj4string(data) = crs.geo1
      proj4string(act_map) = crs.geo1
      #group the data based on the act map suburb boundary
      cr_agg = aggregate(x=data['crash_severity'],by=act_map,FUN=length)
      
      # with the areas of no crashes, we replace nan with 0
      cr_agg$crash_severity[is.na(cr_agg$crash_severity)] <- 0
      #colour palettes 
      qpal = colorBin("Purples", cr_agg$crash_severity, bins=4)
      #plotting the chloropleth
      leaflet(cr_agg) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
        addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.7, smoothFactor = 0.5,
                    color="white",dashArray = "3",fillColor = ~qpal(crash_severity),weight = 1,
                    highlight = highlightOptions(
                      weight = 2,
                      color = "red",
                      dashArray = "4",
                      fillOpacity = 0.7,
                      bringToFront = F),
                    label = paste('Crash no:',cr_agg$crash_severity),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "10px",
                      direction = "auto")) %>%setView(lat = -35.2809, lng = 149.1200, zoom = 10)}
    else{
      
      # for dot sensity map
      #we select the data we want first for the particular year and crash severity
      if (input$Crash_Severity2=='All'){
        data<-crash[crash$year==input$yeartotal,]
      } else{
        data<-crash[crash$year==input$yeartotal & crash$crash_severity==input$Crash_Severity2,]
      }
      
      
      #design the labels we want for the dots
      labels <- sprintf(
        "<strong>Date:</strong>%s<br/><strong>Crash Severity:</strong>%s<br/><strong>Weather Condition: </strong>%s<br/><strong>Lighting Condition: </strong>%s<br/>",
        data$crash_date,data$crash_severity,data$weather_condition,data$lighting_condition
      ) %>% lapply(htmltools::HTML)
      
      col='tomato'
      #leaflet map for the dot density for car crashes
      leaflet(data = data) %>% 
        addProviderTiles(providers$CartoDB.DarkMatter)%>% 
        addCircleMarkers(~x, ~y,color = col,fillColor = col,
                         fillOpacity = 0.6, 
                         radius=1, stroke=FALSE, weight = 0.5,label=labels,
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "13px",
                           direction = "auto")) %>%
        setView(lat = -35.2809, lng = 149.1300, zoom = 11)}
    
    
  })
  
  
  # the traffice offences maps
  output$offencechloro<-renderLeaflet({
    #for cloropleth
    if (input$offencemap=='Chloropleth'){ 
      
      #select the data from the particular year and the offence type
      if (input$Oftype2=='All'){
        data<-offence1[offence1$Year==input$yeartotal,]
      } else{
        data<-offence1[offence1$Year==input$yeartotal & offence1$Offence_Desc==input$Oftype2,]}
      
      
      if((input$Oftype2=="Speed- <=15Km/H(SZ)"|input$Oftype2=="Speed- 15-30Km/H(SZ)") & input$yeartotal<2016){
        offencemap<-leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
          setView(lat = -35.2809, lng = 149.1300, zoom = 11) 
      }else if(input$Oftype2=="Speed- >30Km/H(SZ)" & input$yeartotal<2017){
        offencemap<-leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
          setView(lat = -35.2809, lng = 149.1300, zoom = 11) 
      } else{
        #select the data of non nulls
        data = as.data.table(data)
        data = data[LATITUDE!=""]
        data = data[LONGITUDE!=""]
        
        #convert the data dataframe to an spatial object
        coordinates(data) = c("LONGITUDE","LATITUDE")
        crs.geo1 = CRS("+proj=longlat")  
        proj4string(data) = crs.geo1
        proj4string(act_map) = crs.geo1
        # group the data by act sububr boundary
        off_agg = aggregate(x=data['Offence_Desc'],by=act_map,FUN=length)
        
        # with the null values for the suburb we replace it with zeros 
        off_agg$Offence_Desc[is.na(off_agg$Offence_Desc)] <- 0
        #colour bins for the cloropleths
        qpal = colorBin(c('#fef2f9','#fac6e5','#ffb0e1','#ff80ce'), off_agg$Offence_Desc, bins=4)
        
        #traffic offences for the chloropleths
        
        offencemap<-leaflet(off_agg) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels)%>%
          addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.7, smoothFactor = 0.5,
                      color="white",dashArray = "3",fillColor = ~qpal(Offence_Desc),weight = 1,
                      highlight = highlightOptions(
                        weight = 1,
                        color = "red",
                        dashArray = "4",
                        fillOpacity = 0.7,
                        bringToFront = F),
                      label = paste('Offence no:',off_agg$Offence_Desc),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "10px",
                        direction = "auto")) %>%setView(lat = -35.2809, lng = 149.1200, zoom = 10)}
      offencemap
    }
    else{
      
      
      #for the proportional symbol maps
      #select data of interests based on the selected year and the offence type
      if (input$Oftype2=='All'){
        data<-offence1[offence1$Year==input$yeartotal,]
      } else{
        data<-offence1[offence1$Year==input$yeartotal & offence1$Offence_Desc==input$Oftype2,]
      }
      
      
      
      #convert the dataframe to the one we need for the proportional dataframe
      p<-data %>%group_by(LATITUDE, LONGITUDE,Camera_Type_x)%>%summarise(n=n())
      p<-p[complete.cases(p), ]
      # design a colour gradient firsy  
      qpal <- colorNumeric(palette = "PuRd",domain = p$n)  
      
      
      #set up labels for the symbols
      labels <- sprintf("<strong>Year: </strong>%s<br/><strong>Camera Type: </strong>%s<br/><strong>Number Of Offences: </strong>%s",
                        input$yeartotal,tolower(p$Camera_Type_x),p$n) %>% lapply(htmltools::HTML)
      
      #proportional symbol map by leaflet with styling， for the first two conditions, there are no data
      if((input$Oftype2=="Speed- <=15Km/H(SZ)"|input$Oftype2=="Speed- 15-30Km/H(SZ)") & input$yeartotal<2016){
        offencemap<-leaflet(data =p) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
          setView(lat = -35.2809, lng = 149.1300, zoom = 11) 
      }else if(input$Oftype2=="Speed- >30Km/H(SZ)" & input$yeartotal<2017){
        offencemap<-leaflet(data =p) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
          setView(lat = -35.2809, lng = 149.1300, zoom = 11) 
      }else{
        offencemap<-leaflet(data =p) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addCircleMarkers(~LONGITUDE, ~LATITUDE,color =~qpal(n),fillColor = ~qpal(n),
                           fillOpacity = 0.6,  radius=~sqrt(n)/2.5, stroke=FALSE, weight = 2,
                           label=labels,
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "13px",
                             direction = "auto"))%>%setView(lat = -35.2809, lng = 149.1300, zoom = 11) 
        
        offencemap}}
  })
  
  #for the line plot for both car crash and offence with two axis
  output$lineboth<-renderPlotly({
    #select the data we want for plotting for particular car crash severity
    if (input$Crash_Severity2=='All'){
      crash<- crash %>% filter(year!=2020)
    } else{
      crash<-crash[crash$year!=2020&crash$crash_severity==input$Crash_Severity2,]
    } 
    
    #select the data we want for plotting for particular traffice offence type
    
    if (input$Oftype2=='All'){
      crash<- crash %>% filter(year!=2020)
    } else{
      offence1<-offence1[offence1$Year!=2020 & offence1$Offence_Desc==input$Oftype2,]
    }
    
    #counting the offence number by years
    offenceno<-data.frame(table(offence1$Year))
    offenceno$Var1<-as.numeric(levels(offenceno$Var1))
    
    #with school zone speeding data,  the years data are missing,we need to fill in the blanks
    if(input$Oftype=='Speed- <=15Km/H(SZ)'|input$Oftype=='Speed- 15-30Km/H(SZ)'|input$Oftype=='Speed- >30Km/H(SZ)'){
      offenceno=offenceno[-length(offenceno$Var1),]
      offenceno<-rbind(offenceno,c(2012,0),c(2013,0),c(2014,0))}
    if (input$Oftype=='Speed- >30Km/H(SZ)'){
      offenceno<-rbind(offenceno,c(2015,0))
    }
    
    
    names(offenceno)[1]<-'year'
    names(offenceno)[2]<-'offence_no'
    
    
    #for car crash number count
    crashno<-data.frame(table(crash$year))
    crashno$Var1<-as.numeric(levels(crashno$Var1))
    
    names(crashno)[1]<-'year'
    names(crashno)[2]<-'crash_no'
    
    
    #merge the offenceno, and crash no data frame by year
    data<-right_join(offenceno,crashno,by='year')
    
    #the setting for the second y axis
    ay <- list(
      tickfont = list(color = "grey"),
      overlaying = "y",
      side = "right",
      title = "offence number",
      color='grey',
      tickangle = 90
    )
    
    #setting for margin
    m <- list(
      l = 20,
      r = 30,
      b = 5,
      t = 20,
      pad = 4
    )
    
    #plotting the line graph by plotly with double axis
    fig <- plot_ly(data, x = ~year)
    #first line and first y axis
    fig <-fig %>% add_lines(y = ~crash_no, name = 'crash no',line = list(color = 'black', width = 2))
    # second line with second axis
    fig <- fig %>% add_lines(y = ~offence_no, name = "offence no", yaxis = "y2",line = list(color = 'grey', width = 2))
    fig <- fig %>% layout(
      yaxis2 = ay,
      xaxis = list(title="year"))
    
    #styling
    fig <- fig %>% layout(title = paste('Crashes VS Offences'),font=list(color="white",size=9),
                          yaxis = list(title = "crash number",showgrid = FALSE,zeroline = FALSE,linewidth=0,color='black',tickangle = 90),
                          xaxis = list(showgrid = FALSE,zeroline = FALSE,linewidth=0,color='black'),
                          showlegend = FALSE) %>%layout(paper_bgcolor='transparent',plot_bgcolor='transparent')%>% 
      layout(autosize = T,margin = m,height=230)
    
    #adding the rectangle shape for highlighting the year of interest
    fig <- layout(fig, shapes = 
                    list(type = "rect",
                         fillcolor = "red", line = list(color = "red"),opacity = 0.2,
                         x0 = input$yeartotal-.5, x1 = input$yeartotal+0.5, xref = "x",
                         y0 = 0, y1 = 1, yref = "paper"))
    
    fig
  })
  
  
  #progress box 1 for the offence no of the selected offence type and year
  output$progressBox5 <- renderValueBox( {
    
    # the second and the third condition has no value
    if(input$Oftype2=='All'){
      type_offence<-length(offence1[offence1$Year==input$yeartotal,]$Offence_Desc)
    }  else if((input$Oftype2=="Speed- <=15Km/H(SZ)"|input$Oftype2=="Speed- 15-30Km/H(SZ)") & input$yeartotal<2015){
      type_offence<-0
    }else if(input$Oftype2=="Speed- >30Km/H(SZ)" & input$yeartotal<2016){
      type_offence<-0
      
    } else{
      type_offence<-length(offence1[offence1$Offence_Desc==input$Oftype2&offence1$Year==input$yeartotal,]$Offence_Desc)
    }
    #with some certain years and type of the offences, the data point are missing so we write it as no record
    if(type_offence==0){
      valueBox('No record',tags$p(paste(input$Oftype2," offences in ",input$yeartotal),style = "font-size: 75%;"),
               color = "fuchsia")}
    else{
      valueBox(type_offence,tags$p(paste(input$Oftype2," offences in ",input$yeartotal),style = "font-size: 75%;"),
               color = "fuchsia")
    }})
  
  
  #progress box 2 for the % offence no  change of the selected offence type and year
  output$progressBox6 <- renderValueBox( {
    
    #with the total offence number the beginning year is 2012
    if(input$Oftype2=='All'){
      type_offence<-length(offence1[offence1$Year==input$yeartotal,]$Offence_Desc)
      ori_offence<-length(offence1[offence1$Year==2012,]$Offence_Desc)
      oriyear=2012
      
      # with the offence type of speeding <=12,15-30, the beginning year is 2015
    } else if (input$Oftype2=='Speed- <=15Km/H(SZ)'|input$Oftype2=='Speed- 15-30Km/H(SZ)'){
      type_offence<-length(offence1[offence1$Offence_Desc==input$Oftype2&offence1$Year==input$yeartotal,]$Offence_Desc)
      ori_offence<-length(offence1[offence1$Year==2015&offence1$Offence_Desc==input$Oftype2,]$Offence_Desc)
      oriyear=2015
      
      # with the offence type of speeding >30, the beginning year is 2016
    } else if(input$Oftype2=='Speed- >30Km/H(SZ)'){
      type_offence<-length(offence1[offence1$Offence_Desc==input$Oftype2&offence1$Year==input$yeartotal,]$Offence_Desc)
      ori_offence<-length(offence1[offence1$Year==2016&offence1$Offence_Desc==input$Oftype2,]$Offence_Desc)
      oriyear=2015
      
      
      #with the the rest of the offence type, the beginning year is 2012
    } else{
      type_offence<-length(offence1[offence1$Offence_Desc==input$Oftype&offence1$Year==input$yeartotal,]$Offence_Desc)
      ori_offence<-length(offence1[offence1$Year==2012&offence1$Offence_Desc==input$Oftype2,]$Offence_Desc)
      oriyear=2012
    }
    #percentage change of the offence
    per_change=((type_offence-ori_offence)/ori_offence)*100
    
    # before 2015，2016 some certain offence were not recorded, so we do not write anything in the box   
    if(type_offence==0){
      valueBox('%',paste("change of offences since ", oriyear),
               color = "teal")}
    else{
      
      # when there is a value, we write the the percentage_change
      valueBox(paste(round(per_change,2),'%'),subtitle = tags$p(paste("change of the offence type since ", oriyear),style = "font-size: 75%;"),
               color = "teal")
    }})
  
  #progress box 3 for the no of crashes for the severity of interests and the year of interests
  output$progressBox7 <- renderValueBox( {
    if(input$Crash_Severity2=='All'){
      type_crash<-length(crash[crash$year==input$yeartotal,]$crash_id)
    } else{
      type_crash<-length(crash[crash$crash_severity==input$Crash_Severity2&crash$year==input$yeartotal,]$crash_id)
    }
    
    #the value we want to print down, with the substitle font size being smaller, as wanting to keep in one line 
    valueBox(type_crash,tags$p(paste(input$Crash_Severity2," crashes in ",input$yeartotal),style = "font-size: 75%;"),
             color = "orange")
  })
  
  
  #progress box 4 for the no of crashes for the severity of interests and the year of interests
  output$progressBox8 <- renderValueBox( {
    #filter data based on the year and crash severity of selected, where the beginning year is 2012
    if(input$Crash_Severity2=='All'){
      ori_crash<-length(crash[crash$year==2012,]$crash_id)
      type_crash<-length(crash[crash$year==input$yeartotal,]$crash_id)
      oriyear=2012
    } else{
      ori_crash<-length(crash[crash$crash_severity==input$Crash_Severity2 & crash$year==2012,]$crash_id)
      type_crash<-length(crash[crash$crash_severity==input$Crash_Severity2&crash$year==input$yeartotal,]$crash_id)
      oriyear=2012
    }
    #percentage change is calculated
    per_change=((type_crash-ori_crash)/ori_crash)*100
    #the value we want to print down, with the substitle font size being smaller, as wanting to keep in one line 
    valueBox(paste(round(per_change
                         ,2),'%'),subtitle = tags$p(paste("change of the crash type since ", oriyear),style = "font-size: 75%;"),
             color = "teal")
  })
  
}

shinyApp(ui=ui,server=server)

