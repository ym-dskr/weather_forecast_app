library(shiny)
library(leaflet)
library(leaflet.extras)


shinyServer(function(input, output, session) {
    
    # table
    output$table <- renderDataTable({
        # open meteo
        input$trigger
        
        lat <- isolate(input$lat)
        lon <- isolate(input$lon)
        
        source("open_meteo.R")
        
        url <- paste0(url1, round(lat, 2), url2, round(lon, 3), url3)
        res <- fromJSON(url)
        
        if (length(res$hourly$temperature_2m) != length(res$hourly$time)){
            res$hourly$time <- res$hourly$time[1:166]
        }
        
        temp_df <- data.table::rbindlist(res["hourly"], use.names = T) 
        selection <- colnames(temp_df) %>% sort()
        temp_df <- temp_df %>% 
            dplyr::select(selection) %>% 
            dplyr::select(time, isolate(input$parameter)) %>% 
            relocate(time)
        
        # temp_df$time <- ymd_hm(temp_df$time, tz = "Asia/Tokyo")
        
        DT::datatable(temp_df,
                      # filter='top',
                      rownames = F,
                      extensions = c('Buttons'), 
                      selection = "single",
                      # escape = FALSE, style = 'bootstrap', class = 'table-bordered table-condensed',
                      options=list(dom = 'Bfrtip', 
                                   buttons = c('pdf','csv', 'print'), 
                                   scrollX = TRUE,
                                   scrollY = 600,
                                   scrollCollapse = TRUE,
                                   pageLength = 168,
                                   # fixedHeader = TRUE,
                                   autoWidth=T))
        
    })
    
    # map
    output$map <- renderLeaflet({
        input$trigger
        
        map <- leaflet() %>% addTiles(group="OSM") %>%
            # fitBounds(lng1, lat1, lng2, lat2) %>% 
            addProviderTiles("Esri.WorldImagery", group = "Image") %>% 
            addLayersControl(baseGroups=c("OSM", "Image"),
                             options=layersControlOptions(collapsed = FALSE)) %>%
            addScaleBar(position="bottomleft")%>% 
            addMeasure(position = "topright", primaryLengthUnit = "meters", 
                       primaryAreaUnit = "sqmeters", activeColor = "#3D535D",
                       completedColor = "#7D4479") %>% 
            addMiniMap(position="bottomright") %>% 
            addResetMapButton() %>%
            addSearchOSM()
        
        map %>% 
            addMarkers(lng = isolate(input$lon),
                       lat = isolate(input$lat),
                       label = paste0('<strong>', "lng : ", isolate(input$lat), '</strong>', "<br>",
                                      '<strong>', "lon :", isolate(input$lon), '</strong>', "<br>"
                       ) %>%
                           lapply(htmltools::HTML))
        
    })
    
    
    observe({
        click = input$map_click
        if(is.null(click))
            return()
        
        input$trigger
        
        lat <- isolate(input$lat)
        lon <- isolate(input$lon)
        
        source("open_meteo.R")
        
        url <- paste0(url1, round(lat, 2), url2, round(lon, 3), url3)
        res <- fromJSON(url)
        
        if (length(res$hourly$temperature_2m) != length(res$hourly$time)){
            res$hourly$time <- res$hourly$time[1:166]
        }
        
        text <- paste0("Latitude: ", round(click$lat, 4), "<br>", "Longtitude: ", round(click$lng, 4))
        text2 <- paste0("Nearest Lattice point of your selected point ->  Lat: ", res$latitude, "  Lon: ", res$longitude)
        map_proxy = leafletProxy("map") %>%
            clearPopups() %>%
            addPopups(click$lng, click$lat, text)
        
        updateNumericInput(session, inputId = "lon", value = click$lng)
        updateNumericInput(session, inputId = "lat", value = click$lat)
        
        output$Click_text <- renderText({
            text2 
        })
    })
    
    # graph
    # observeEvent(input$dimension,{
    #     output$graph <- renderPlotly({
    #         input$trigger
    #         
    #         lat <- isolate(input$lat)
    #         lon <- isolate(input$lon)
    #         
    #         source("open_meteo.R")
    #         
    #         url <- paste0(url1, round(lat, 2), url2, round(lon, 3), url3)
    #         res <- fromJSON(url)
    #         
    #         if (length(res$hourly$temperature_2m) != length(res$hourly$time)){
    #             res$hourly$time <- res$hourly$time[1:166]
    #         }
    #         
    #         temp_df <- data.table::rbindlist(res["hourly"], use.names = T) 
    #         selection <- colnames(temp_df) %>% sort()
    #         temp_df <- temp_df %>% 
    #             dplyr::select(selection) %>% 
    #             relocate(time)
    #         
    #         temp_df$time <- ymd_hm(temp_df$time, tz = "Asia/Tokyo")
    #         
    #         temp_df_long <- temp_df %>% 
    #             pivot_longer(cols = -time, names_to = "Params", values_to = "Values")
    #         
    #         g <- ggplot(temp_df_long) +
    #             geom_line(aes(x = time, y = Values)) +
    #             facet_wrap(.~Params, scales = "free") +
    #             ggtitle(paste0())
    #         
    #         ggplotly(g, width = (0.8*as.numeric(input$dimension[1])), height = 0.8*as.numeric(input$dimension[2]))
    #     })
    # })
    
    
    
    # observeEvent(input$dimension,{
    output$graph <- renderHighchart({
        input$trigger
        
        lat <- isolate(input$lat)
        lon <- isolate(input$lon)
        
        source("open_meteo.R")
        
        url <- paste0(url1, round(lat, 2), url2, round(lon, 3), url3)
        res <- fromJSON(url)
        
        if (length(res$hourly$temperature_2m) != length(res$hourly$time)){
            res$hourly$time <- res$hourly$time[1:166]
        }
        
        temp_df <- data.table::rbindlist(res["hourly"], use.names = T)
        selection <- colnames(temp_df) %>% sort()
        temp_df <- temp_df %>%
            dplyr::select(selection) %>%
            dplyr::select(time, isolate(input$parameter)) %>% 
            relocate(time)
        
        temp_df$time <- ymd_hm(temp_df$time, tz = "Asia/Tokyo")
        
        temp_df_long <- temp_df %>% 
            pivot_longer(cols = -c(time), names_to = "Params", values_to = "Values") %>% 
            transform(Values = as.numeric(Values)) %>% 
            arrange(Params,time)
        
        hc <- highcharter::hchart(temp_df_long, "line", 
                                  hcaes(x = datetime_to_timestamp(time + 32400), 
                                        y = Values,
                                        group = Params)) %>%
            hc_xAxis(type = "datetime",
                     title = list(text = "Datetime")) %>% 
            hc_title(
                text = paste0("Forecast Weather at Location : ", res$latitude, ", ",res$longitude, 
                              " : <h5><a href='https://open-meteo.com/'>Weather data by Open-Meteo.com</a></h5>"),
                margin = 5) 
        
        hc
    })
        
    
    output$nows <- renderText({
        input$trigger
        
        nowtime <- (Sys.time() + 60 * 60 * 9) %>% as.character()
    })
    
    output$def_param <- renderImage({
        return(list(src = "img/def_param.png",width = "100%", height = "300px", contentType = "image/png",alt = "Alignment"))
    }, deleteFile = FALSE) 
    
    output$weather_code <- renderImage({
        return(list(src = "img/weather_code.png",width = "100%", height = "300px", contentType = "image/png",alt = "Alignment"))
    }, deleteFile = FALSE) 
    
    
})

