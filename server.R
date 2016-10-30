library(data.table)
library(dplyr)
library(tidyr)
library(scales)
library(lubridate)  #日期处理包
library(shiny)
library(leaflet)
library(lattice)
library(plotly)
library(chorddiag)  #绘制chord
library(RColorBrewer)
library(ggplot2)


## 地铁站经纬度
stations <- fread("data/stations.csv",encoding="UTF-8")
stations <- stations %>% 
  select(c(1:5)) %>%
  arrange(line,line_id)
## 2015/4/1尚未开通线路
## 11号线：秀沿路（含）-迪斯尼
## 12号线：汉中路（含）-七莘路
## 13号线：江宁路（含）—世博大道
arrange(stations[stations$line==13,],line_id)
stations <- stations %>%
  filter(line != 11 | (line == 11 & line_id>=4)) %>%
  filter(line != 12 | (line == 12 & line_id<=16)) %>%
  filter(line != 13 | (line == 13 & line_id<=10)) 

## 地铁站进站数据和出站数据
shmetro_in <- fread("data/shmetro_in.csv",encoding="UTF-8")
shmetro_out <- fread("data/shmetro_out.csv",encoding="UTF-8")
shmetro_in <- data.table(left_join(shmetro_in,stations,by=c("station","line"),all.x=T,all.y=F))
shmetro_out <- data.table(left_join(shmetro_out,stations,by=c("station","line"),all.x=T,all.y=F))

## 进出地铁站数据
shmetro_line_in_out <- fread("data/shmetro_line_in_out.csv",encoding="UTF-8")

## 进出地铁站关联
in_out <- shmetro_line_in_out %>%
  spread(line_out,count)
in_out[is.na(in_out)]<-0

## 虚拟换乘数据
shmetro_transfer <- fread("data/shmetro_transfer.csv", encoding="UTF-8")

## 热力图数据
shmetro_in_heatmap <- shmetro_in %>%
  mutate(M30=floor(M5/6), station=paste(ifelse(line_id<10, paste("0", line_id, sep=""), line_id), station, "进站", sep=".")) %>%
  group_by(line, station, line_id, M30) %>%
  summarise(count=sum(count))  %>%
  ungroup() %>%
  mutate(HM30 = substr(as.POSIXct(M30*30*60, origin = "2015-04-01", tz = "GMT"),12,16)) %>%
  select(line, station, M30, HM30, count)
shmetro_out_heatmap <- shmetro_out %>%
  mutate(M30=floor(M5/6), station=paste(ifelse(line_id<10, paste("0", line_id, sep=""), line_id), station, "出站", sep=".")) %>%
  group_by(line, station, line_id, M30) %>%
  summarise(count=sum(count)) %>%
  ungroup() %>%
  mutate(HM30 = substr(as.POSIXct(M30*30*60, origin = "2015-04-01", tz = "GMT"),12,16)) %>%
  select(line, station, M30, HM30, count)
shmetro_heatmap <- rbind(shmetro_in_heatmap,shmetro_out_heatmap)
shmetro_heatmap[shmetro_heatmap$HM30=="00:00",]$HM30 <- "24:00"

## 乘站数量和人次
paths_persion <- fread("data/paths_persion.csv", header = T, sep=",", encoding = "UTF-8")

invisible(gc())

## 地铁颜色
lines_color <- data.frame("line"=c(1:13,16),"color"=c("#ED3229","#36B854","#FFD823","#320176","#823094","#CF047A","#F3560F","#008CC1","#91C5DB","#C7AFD3","#8C2222","#007a61","#ec91cc","#32D2CA"))
pal <- colorFactor(as.character(lines_color$color), domain = stations$line)

## 辅助函数绘制线路
draw_line_add <- function(l_no,line_s_id=NULL){
  line_color <- lines_color[lines_color$line==l_no,]$color
  line_data <- stations[stations$line==l_no,]
  if(is.null(line_s_id)){
    draw_lines <- Shanghai %>%
      addPolylines(lat=line_data$gps_lat,lng=line_data$gps_lon,color=line_color,weight=2)
  }else{
    draw_lines <- Shanghai %>%
      addPolylines(lat=filter(line_data, line_id %in% line_s_id)$gps_lat,
                   lng=filter(line_data, line_id %in% line_s_id)$gps_lon,
                   color=line_color,weight=2)
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
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(1:28))
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(24,29:31))
  }else if(l==11){
    #由于11号线在嘉定新城站以后分为两条线路，需分两端绘制
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(4:31))
    Shanghai <- draw_line_add(l_no=l,line_s_id=c(28,32:38))
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
yax <- list(title = "",zeroline = FALSE,showline = FALSE,showticklabels = FALSE,showgrid = FALSE)
xax <- list(title = "",titlefont = list(size = 8),tickangle = -20,color = "black")
m <- list(l = 100, r = 0, b = 30, t = 30, pad = 0) 
l_x10_y10 <- list(x = 0.75, y = 1,bgcolor = "#00FFFFFF") #设置白色透明色

shinyServer(function(input, output, session) {
  ####进站流量统计####
  filteredData <- reactive({
    if(input$select_line=="All"){
      shmetro_in %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time,"%H:%M:%S")))/300)) %>%
        group_by(station,gps_lat,gps_lon) %>%
        summarise(count=sum(count),line=min(line)) %>%
        ungroup()
    }else{
      shmetro_in %>%
        subset(station %in% stations[stations$line==as.numeric(input$select_line),]$station) %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time,"%H:%M:%S")))/300)) %>%
        group_by(station,gps_lat,gps_lon) %>%
        summarise(count=sum(count),line=as.numeric(input$select_line))  %>%
        ungroup()
    }
  })
  
  stations_in_top5 <- reactive({
    filteredData() %>%
      group_by(station) %>%
      summarise(count=sum(count),line=min(line)) %>%
      arrange(desc(count)) %>%
      head(5) %>%
      mutate(station=factor(station, levels = unique(station)[order(count, decreasing = TRUE)])) %>%
      as.data.frame()
  })##plot bar sort by count
  
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
    leafletProxy("map") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addCircleMarkers(filteredData()$gps_lon,filteredData()$gps_lat, color = pal(filteredData()$line), fillOpacity = 0.5,stroke = FALSE, popup=paste(filteredData()$station,filteredData()$line,filteredData()$count,sep=","), radius=(filteredData()$count)^(1/2.5))
  })
  
  ## in_top5
  output$in_top5 <- renderPlotly({
    if (nrow(stations_in_top5()) == 0)# If no stations_in_top5 are in view, don't plot
      return(NULL)
    
    ## plotly通过bgcolor = "#00FFFFFF" 设置背景色，最新版已不适用使用plot_bgcolor和paper_bgcolor
    plot_ly(stations_in_top5(),
            x = stations_in_top5()$station,
            y = stations_in_top5()$count,
            type = "bar",
            marker = list(color = pal(stations_in_top5()$line))) %>%
      layout(showlegend=FALSE,
             yaxis=yax,xaxis=xax,
             plot_bgcolor=toRGB(rgb(1,1,1), alpha = 0),
             paper_bgcolor=toRGB(rgb(1,1,1), alpha = 0))
  })
  
  ##控制是否显示legend
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearControls() ##线删除，再根据判断是否显示
    if (input$legend) {
      proxy %>% addLegend(position = "bottomleft",pal=pal,values = stations$line)
    }
  })
  
  ####出站流量统计####
  filteredData_out <- reactive({
    if(input$select_line_out=="All"){
      shmetro_out %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time_out,"%H:%M:%S")))/300)) %>%
        group_by(station,gps_lat,gps_lon) %>%
        summarise(count=sum(count),line=min(line)) %>%
        ungroup()
    }else{
      shmetro_out %>%
        subset(station %in% stations[stations$line==as.numeric(input$select_line_out),]$station) %>%
        filter(M5==ceiling(period_to_seconds(hms(format(input$slider_time_out,"%H:%M:%S")))/300)) %>%
        group_by(station,gps_lat,gps_lon) %>%
        summarise(count=sum(count),line=as.numeric(input$select_line_out))  %>%
        ungroup()
    }
  })
  
  stations_out_top5 <- reactive({
    filteredData_out() %>%
      group_by(station) %>%
      summarise(count=sum(count),line=min(line)) %>%
      arrange(desc(count)) %>%
      head(5) %>%
      mutate(station=factor(station, levels = unique(station)[order(count, decreasing = TRUE)])) %>%
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
    leafletProxy("map_out") %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      addCircleMarkers(filteredData_out()$gps_lon, filteredData_out()$gps_lat,color=pal(filteredData_out()$line),fillOpacity = 0.5,stroke=FALSE,popup=paste(filteredData_out()$station,filteredData_out()$line,filteredData_out()$count,sep=","),radius=(filteredData_out()$count)^(1/2.5))
  })
  
  # out_top5
  output$out_top5 <- renderPlotly({
    if (nrow(stations_out_top5()) == 0)
      return(NULL)
    
    plot_ly(stations_out_top5(),
            x = stations_out_top5()$station,
            y = stations_out_top5()$count,
            type = "bar",
            marker = list(color = pal(stations_out_top5()$line))) %>%
      layout(showlegend=FALSE,
             yaxis=yax,xaxis=xax,
             plot_bgcolor=toRGB(rgb(1,1,1), alpha = 0),
             paper_bgcolor=toRGB(rgb(1,1,1), alpha = 0))
  })
  
  observe({
    proxy <- leafletProxy("map_out")
    proxy %>% clearControls()
    if (input$legend_out) {
      proxy %>% addLegend(position = "bottomleft",pal=pal,values = stations$line)
    }
  })
  
  ####热力图####
  heatmap_data <- reactive({
    shmetro_heatmap %>%
      filter(line==input$heatmap_line)
  })
  
  output$heatmap_plot <- renderPlotly({
    gg <- ggplot(heatmap_data(), aes(HM30, station)) + 
      geom_tile(aes(fill = count), colour = "white") + 
      scale_fill_gradientn(colours = brewer.pal(11, "RdYlGn")[10:1]) +
      theme_grey(base_size = 10) + 
      labs(list(title = "", x = "", y = "")) + 
      theme(axis.text.x = element_text(size = 9, angle = 315, hjust = 0, colour = "steelblue"),
            axis.ticks.length = unit(.05, "cm"),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            text = element_text(family = 'STKaiti'))
    ##axis.ticks = element_blank(),
    gg_p <- ggplotly(gg)
    gg_p %>% 
      layout(autosize = F, width = 800, height = 550, margin = m)
  })
  
  ####线路关联统计####
  output$line_chord <- renderChorddiag({
    chorddiag(metro_chord, groupColors = groupColors, showTicks=F, groupnamePadding = 5)
  })
  
  ####乘坐数量####
  output$paths_persion_plot <- renderPlotly({
    gg_paths_persion <- ggplot(paths_persion,aes(paths,value)) +
      geom_bar(stat = "identity",width=0.8) +
      theme_grey(base_size = 10) + 
      labs(list(title = "", x = "乘坐站数", y = "乘坐人次")) + 
      theme(axis.ticks.length = unit(.05, "cm"),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            text = element_text(family = 'STKaiti'))
    ggplotly(gg_paths_persion) %>% 
      layout(autosize = F, width = 800, height = 550, margin = m)
  })
  
  ####虚拟换乘####
  output$gg_transfer_plot1 <- renderPlotly({
    gg_transfer <- ggplot(filter(shmetro_transfer,station=="陕西南路"),aes(duration,value)) +
      geom_bar(aes(fill=transfer),stat = "identity",position="dodge"
               ,width=0.8) +
      theme_grey(base_size = 10) + 
      labs(list(title = "陕西南路", x = "换乘时间(分钟)", y = "换乘人次")) + 
      theme(axis.ticks.length = unit(.05, "cm"),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            text = element_text(family = 'STKaiti'))
    ggplotly(gg_transfer) %>% 
      layout(legend = l_x10_y10, autosize = F, width = 600, height = 400, margin = m)
  })
  
  output$gg_transfer_plot2 <- renderPlotly({
    gg_transfer <- ggplot(filter(shmetro_transfer,station=="上海火车站"),aes(duration,value)) +
      geom_bar(aes(fill=transfer),stat = "identity",position="dodge"
               ,width=0.8) +
      theme_grey(base_size = 10) + 
      labs(list(title = "上海火车站", x = "换乘时间(分钟)", y = "换乘人次")) + 
      theme(axis.ticks.length = unit(.05, "cm"),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            text = element_text(family = 'STKaiti'))
    ggplotly(gg_transfer) %>% 
      layout(legend = l_x10_y10, autosize = F, width = 600, height = 400, margin = m)
  })
})


