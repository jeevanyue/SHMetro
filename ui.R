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
#shmetro_in <- fread("data/shmetro_in.csv",encoding="UTF-8")
#shmetro_out <- fread("data/shmetro_out.csv",encoding="UTF-8")

#地铁颜色
lines_color <- data.frame("line"=c(1:13,16),"color"=c("#ED3229","#36B854","#FFD823","#320176","#823094","#CF047A","#F3560F","#008CC1","#91C5DB","#C7AFD3","#8C2222","#007a61","#ec91cc","#32D2CA"))

shinyUI(navbarPage("SHMetro",
                         
                         tabPanel("进站流量",
                                  div(class="outer",
                                      
                                      #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                      
                                      leafletOutput("map", width = "100%", height = "100%"),
                                      absolutePanel(top = 10, right = 10,
                                                    h4(textOutput("output_slider_time")),
                                                    sliderInput("slider_time", "Time:",
                                                                #min=as.POSIXct(min(filter(shmetro_in, M5>30)$M5)*5*60, origin = "2015-04-01", tz = "GMT"),
                                                                #max=as.POSIXct(max(shmetro_in$M5)*5*60, origin = "2015-04-01", tz = "GMT"),
                                                                #value=as.POSIXct(min(shmetro_in$M5)*5*60, origin = "2015-04-01", tz = "GMT"),
                                                                min = as.POSIXct(5*60*60, origin = "2015-04-01", tz = "GMT"),
                                                                max = as.POSIXct(24*60*60, origin = "2015-04-01", tz = "GMT"),
                                                                value = as.POSIXct(5*60*60, origin = "2015-04-01", tz = "GMT"),
                                                                step = 60*5,
                                                                timeFormat = "%T",
                                                                timezone = "GMT"),
                                                    selectInput("select_line", "Line",
                                                                c("All",lines_color$line)),
                                                    h4("TOP 5"),
                                                    plotlyOutput("in_top5",height = 200),
                                                    checkboxInput("legend", "Show legend", TRUE)
                                      )
                                  )
                         ),
                         tabPanel("出站流量",
                                  div(class="outer",
                                      
                                      #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                      
                                      leafletOutput("map_out", width = "100%", height = "100%"),
                                      absolutePanel(top = 10, right = 10,
                                                    h4(textOutput("output_slider_time_out")),
                                                    sliderInput("slider_time_out", "Time:",
                                                                #min=as.POSIXct(min(filter(shmetro_in, M5>30)$M5)*5*60, origin = "1960-01-01", tz = "GMT"),
                                                                #max=as.POSIXct(max(shmetro_in$M5)*5*60, origin = "1960-01-01", tz = "GMT"),
                                                                #value=as.POSIXct(min(shmetro_in$M5)*5*60, origin = "1960-01-01", tz = "GMT"),
                                                                min = as.POSIXct(5*60*60, origin = "2015-04-01", tz = "GMT"),
                                                                max = as.POSIXct(24*60*60, origin = "2015-04-01", tz = "GMT"),
                                                                value = as.POSIXct(5*60*60, origin = "2015-04-01", tz = "GMT"),
                                                                step = 60*5,
                                                                timeFormat = "%T",
                                                                timezone = "GMT"),
                                                    selectInput("select_line_out", "Line",
                                                                c("All",lines_color$line)),
                                                    h4("TOP 5"),
                                                    plotlyOutput("out_top5",height = 200),
                                                    checkboxInput("legend_out", "Show legend", TRUE)
                                      )
                                  )
                         ),
                         tabPanel("线路关联",
                                  div(class="outer",
                                      
                                      #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                      tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                      chorddiagOutput("line_chord", width = "100%",height="100%")
                                  )
                         )
)
)
