library(data.table)
library(bit64)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)  #日期处理包
library(shiny)
library(leaflet)
library(lattice)
library(plotly)
library(chorddiag)  #绘制chord


#
shmetro_in <- fread("data/shmetro_in.csv",encoding="UTF-8")
shmetro_out <- fread("data/shmetro_out.csv",encoding="UTF-8")

## 进出地铁站数据
shmetro_line_in_out <- fread("data/shmetro_line_in_out.csv",encoding="UTF-8")

## 进出地铁站关联
in_out <- shmetro_line_in_out %>%
  spread(line_out,count)
in_out[is.na(in_out)]<-0

## 地铁站经纬度
stations <- fread("data/stations.csv",encoding="UTF-8")
stations <- stations %>% 
  select(c(1:5)) %>%
  arrange(line,line_id)
stations_no <- nrow(stations)
for (i in 1:stations_no) {
  s <- stations$station[i]
  stations$lines[i] <- paste(stations[stations$station==s,]$line,sep="",collapse="/")
}

#地铁颜色
lines_color <- data.frame("line"=c(1:13,16),"color"=c("#ED3229","#36B854","#FFD823","#320176","#823094","#CF047A","#F3560F","#008CC1","#91C5DB","#C7AFD3","#8C2222","#007a61","#ec91cc","#32D2CA"))

pal <- colorFactor(as.character(lines_color$color), domain = stations$line)

#辅助函数绘制线路
draw_line_add <- function(l_no,line_s_id=NULL){
  line_color <- lines_color[lines_color$line==l_no,]$color
  line_data <- stations[stations$line==l_no,]
  if(is.null(line_s_id)){
    draw_lines <- Shanghai %>%
      addPolylines(lat=line_data$gps_lat,lng=line_data$gps_lon,color=line_color,weight=2)
  }else{
    draw_lines <- Shanghai %>%
      addPolylines(lat=line_data$gps_lat[line_s_id],lng=line_data$gps_lon[line_s_id],color=line_color,weight=2)
  }
  return(draw_lines)
}

## 上海线路地图
Shanghai <- leaflet() %>% 
  setView(lng = 121.60, lat = 31.20, zoom = 10) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(position = "bottomleft",pal=pal,values = stations$line)

for(l in unique(stations$line)){
  line_length <- nrow(stations[stations$line==l,])
  if(l==4){
    #由于4号线为环线，需将首尾相连
    Shanghai <- draw_line_add(l_no=l)
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(1,line_length))
  }else if(l==10){
    #由于10号线在龙溪路站以后分为两条线路，需分两端绘制
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(1:(line_length-3)))
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(24,(line_length-2):line_length))
  }else if(l==11){
    #由于11号线在嘉定新城站以后分为两条线路，需分两端绘制
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(1:(line_length-7)))
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(28,(line_length-6):line_length))
  }else{
    Shanghai <- draw_line_add(l_no=l)
  }
}

##绘制chord图
metro_chord <- data.matrix(as.data.frame(in_out)[,c(2:15)])
haircolors <- in_out$line_in
dimnames(metro_chord) <- list(have = haircolors,
                              prefer = colnames(metro_chord))

groupColors <- c("#ED3229","#36B854","#FFD823","#320176","#823094","#CF047A","#F3560F","#008CC1","#91C5DB","#C7AFD3","#8C2222","#007a61","#ec91cc","#32D2CA")
#chorddiag(metro_chord, groupColors = groupColors, margin=50, showTicks=F, groupnamePadding = 5)

b <- list(x = 0, y = 1,bgcolor = "#00FFFFFF")
yax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

xax <- list(
  title = "",
  titlefont = list(size = 8),
  tickangle = -20,
  color = "black"
)

shinyServer(function(input, output, session) {
  ## 进站流量统计
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    if(input$select_line=="All"){
      shmetro_in %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time,"%H:%M:%S")))/300))
    }else{
      shmetro_in %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time,"%H:%M:%S")))/300),line==as.numeric(input$select_line))
    }
  })
  
  stations_in_top5 <- reactive({
    filteredData() %>%
      group_by(station) %>%
      summarise(count=sum(count),line=min(line)) %>%
      arrange(desc(count)) %>%
      head(5) %>%
      as.data.frame()
  })
  
  ## time
  output$output_slider_time  <- renderText({
    paste0("Time: ", format(input$slider_time,"%H:%M:%S"))
  })
  
  output$map <- renderLeaflet({
    Shanghai %>%
      addCircles(stations$gps_lon, stations$gps_lat,color = pal(stations$line), radius=1,popup = paste(stations$station,stations$lines),fillOpacity = 1,stroke = FALSE) %>%
      clearMarkerClusters() %>%
      clearMarkers()
  })
  
  observe({
    data_in_circle <- data.table(filteredData())[, count := sum(count), by=list(station, M5)] %>%
      arrange(count)
    
    leafletProxy("map", data = data_in_circle) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addCircleMarkers(data_in_circle$gps_lon,data_in_circle$gps_lat, color = pal(data_in_circle$line), fillOpacity = 0.5,stroke = FALSE, popup=paste(data_in_circle$station,data_in_circle$line,data_in_circle$count,sep=","), radius=(data_in_circle$count)^(1/2.5))
  })
  
  # top5
  output$in_top5 <- renderPlotly({
    # If no stations_in_top5 are in view, don't plot
    if (nrow(stations_in_top5()) == 0)
      return(NULL)
    
    plot_ly(stations_in_top5(),
            x = stations_in_top5()$station,
            y = stations_in_top5()$count,
            type = "bar",
            marker = list(color = pal(stations_in_top5()$line)),
            bgcolor = "#00FFFFFF") %>%
      layout(showlegend=FALSE,
             yaxis=yax,xaxis=xax,plot_bgcolor='#00FFFFFF',
             paper_bgcolor='#00FFFFFF')
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = "bottomleft",pal=pal,values = stations$line)
    }
  })
  
  ## 出站流量统计
  # Reactive expression for the data subsetted to what the user selected
  filteredData_out <- reactive({
    if(input$select_line_out=="All"){
      shmetro_out %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time_out,"%H:%M:%S")))/300))
    }else{
      shmetro_in %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time_out,"%H:%M:%S")))/300),line==as.numeric(input$select_line_out))
    }
  })
  
  stations_out_top5 <- reactive({
    filteredData_out() %>%
      group_by(station) %>%
      summarise(count=sum(count),line=min(line)) %>%
      arrange(desc(count)) %>%
      head(5) %>%
      as.data.frame()
  })
  
  ## time
  output$output_slider_time_out  <- renderText({
    paste0("Time: ", format(input$slider_time_out,"%H:%M:%S"))
  })
  
  output$map_out <- renderLeaflet({
    Shanghai %>%
      addCircles(stations$gps_lon, stations$gps_lat,color = pal(stations$line), radius=1,popup = paste(stations$station,stations$lines),fillOpacity = 1,stroke = FALSE) %>%
      clearMarkerClusters() %>%
      clearMarkers()
  })
  
  observe({
    data_out_circle <- data.table(filteredData_out())[, count := sum(count), by=list(station, M5)] %>%
      arrange(count)
    
    leafletProxy("map_out", data = filteredData_out()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addCircleMarkers(data_out_circle$gps_lon, data_out_circle$gps_lat, color = pal(data_out_circle$line),fillOpacity = 0.5,stroke = FALSE,  popup=paste(data_out_circle$station,data_out_circle$line,data_out_circle$count,sep=","), radius=(data_out_circle$count)^(1/2.5))
  })
  
  # top5
  output$out_top5 <- renderPlotly({
    # If no stations_in_top5 are in view, don't plot
    if (nrow(stations_out_top5()) == 0)
      return(NULL)
    
    plot_ly(stations_out_top5(),
            x = stations_out_top5()$station,
            y = stations_out_top5()$count,
            type = "bar",
            marker = list(color = pal(stations_out_top5()$line)),
            bgcolor = "#00FFFFFF") %>%
      layout(showlegend=FALSE,
             yaxis=yax,xaxis=xax,plot_bgcolor='#00FFFFFF',
             paper_bgcolor='#00FFFFFF')
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map_out")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend_out) {
      proxy %>% addLegend(position = "bottomleft",pal=pal,values = stations$line)
    }
  })
  
  ## 线路关联
  output$line_chord <- renderChorddiag({
    chorddiag(metro_chord, groupColors = groupColors, showTicks=F, groupnamePadding = 5)
  })
})




