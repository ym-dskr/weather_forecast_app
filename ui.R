library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(plotly)
library(DT)
library(dashboardthemes)
library(leaflet)
library(leaflet.extras)
library(highcharter)
library(xts)
library(zoo)

ui <- dashboardPage(
    dashboardHeader(
        title = "Weather Forecast App"
    ),
    # 
    
    
    dashboardSidebar(
        sidebarMenu(
           
            menuItem("マニュアル", tabName = "Manual", icon = icon("th")),
            
            menuItem("地図・グラフ", tabName = "Map", icon = icon("map-marker")),
            
            numericInput("lat", label = "緯度", value = 35.680),
            numericInput("lon", label = "経度", value = 139.764),
            
            selectInput("parameter", 
                        multiple = T,
                        choices = c("cloudcover", "direct_radiation", "precipitation",     
                                    "pressure_msl", "relativehumidity_2m", "snow_depth",       
                                    "temperature_2m", "windspeed_10m", "weathercode"),
                        label = "パラメータ",
                        selected = "temperature_2m"),
            
            # actionButton("trigger2", label = "現在位置情報入力", style="color: #fff; background-color: #337ab7; border-color: red"),
            actionButton("trigger", label = "更新", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            
            # menuItem("3.グラフ", tabName = "Graph"),
            
            menuItem("データダウンロード", tabName = "Table", icon = icon("download")),
            
            h5(paste0("<div align='center'><font color = 'white'> Update Time : <br/> ",
                      textOutput("nows") , "</font></div>") %>% lapply(htmltools::HTML))
            
            # menuItem("ライセンス", tabName = "License")
        )
    ),
    
    dashboardBody(
        shinyDashboardThemes("blue_gradient"),
        
        # tags$head(tags$style(HTML('
        #                         /* body */
        #                         .content-wrapper, .right-side {
        #                         background-color: #7da2d1;
        #                         }
        #                         
        #                         '))),
    
        
        tabItems(
            
            tabItem(
                    tabName = "Manual",
                    titlePanel("使い方"),
                    box(
                        width = 12,
                        height = "auto",
                        title = includeHTML("text/manual.html")
                    ),
                    box(
                        width = 6,
                        height = "auto",
                        title = "取得パラメータ",
                        status = "info",
                        dataTableOutput("parameters", height = "auto")
                    ),
                    box(
                        width = 6,
                        height = "auto",
                        title = "気象解釈コード",
                        status = "info",
                        dataTableOutput("weathercode", height = "auto")
                    )
                    
                    
                    
                    
            ),
            
            tabItem(tabName = "Map",
                    # titlePanel("予報対象地点：選択マップ"),
                    # textOutput("Click_text"  %>% lapply(htmltools::HTML)),
                    box(
                        title = HTML("地点をクリックし、更新ボタンを押すとその地点の気象予報データが更新・表示されます。"),
                        status = "info",
                        solidHeader = T,
                        width = 12,
                        height = "auto",
                        withSpinner(leafletOutput("map"))),
                    box(
                        status = "info",
                        solidHeader = T,
                        width = 12,
                        height = "auto",
                        tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
                        # plotlyOutput("graph",  height = 'auto', width = 'auto')
                        withSpinner(highchartOutput("graph"))
                        )
                    ),
            
            tabItem(tabName = "Table",
                    titlePanel(HTML("Download <br> <h5><a href='https://open-meteo.com/'>Weather data by Open-Meteo.com</a></h5>")),
                    textOutput("Click_text" %>% lapply(htmltools::HTML)),
                    box(
                        status = "info",
                        solidHeader = T,
                        width = 12,
                        height = "auto",
                        withSpinner(dataTableOutput("table")))
            )
            
            # tabItem(tabName = "Graph",
            #         titlePanel("Graph"),
                    # textOutput("Click_text" %>% lapply(htmltools::HTML)),
                    # box(
                    #     status = "info",
                    #     solidHeader = T,
                    #     width = 12,
                    #     height = "auto",
                    #     tags$head(tags$script('
                    #     var dimension = [0, 0];
                    #     $(document).on("shiny:connected", function(e) {
                    #     dimension[0] = window.innerWidth;
                    #     dimension[1] = window.innerHeight;
                    #     Shiny.onInputChange("dimension", dimension);
                    #     });
                    #     $(window).resize(function(e) {
                    #     dimension[0] = window.innerWidth;
                    #     dimension[1] = window.innerHeight;
                    #     Shiny.onInputChange("dimension", dimension);
                    #     });
                    #     ')),
                    #     # plotlyOutput("graph",  height = 'auto', width = 'auto')
                    #     highchartOutput("graph")
                    # )
            # ),
            
            # tabItem(tabName = "License",
            #         titlePanel("ライセンス"),
            #         h5("<a href='https://open-meteo.com/'>Weather data by Open-Meteo.com</a>" %>% lapply(htmltools::HTML)))
        
            
            )
        
        # tabItem("table", dataTableOutput("table"))
    )
)



