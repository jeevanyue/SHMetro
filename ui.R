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

#地铁颜色
lines_color <- data.frame("line"=c(1:13,16),"color"=c("#ED3229","#36B854","#FFD823","#320176","#823094","#CF047A","#F3560F","#008CC1","#91C5DB","#C7AFD3","#8C2222","#007a61","#ec91cc","#32D2CA"))

shinyUI(navbarPage("SHMetro",
                   ####进站流量####
                   tabPanel("进站流量",
                            div(class="outer",
                                #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                leafletOutput("map", width = "100%", height = "100%"),
                                absolutePanel(top = 10, right = 10,
                                              h4(textOutput("output_slider_time")),
                                              sliderInput("slider_time", "Time:",
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
                   
                   ####出战流量####
                   tabPanel("出站流量",
                            div(class="outer",
                                tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                leafletOutput("map_out", width = "100%", height = "100%"),
                                absolutePanel(top = 10, right = 10,
                                              h4(textOutput("output_slider_time_out")),
                                              sliderInput("slider_time_out", "Time:",
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
                   ####热力图####
                   tabPanel("热力图",
                            div(class="outer",
                                tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                absolutePanel(top = 10, right = 10, width = 60,
                                              selectInput("heatmap_line", "Line",
                                                          c(lines_color$line))
                                ),
                                absolutePanel(top = 20, left = 100,
                                              plotlyOutput("heatmap_plot", width = "800px", height="600px")
                                )
                            )
                   ),
                   
                   ####线路关联####
                   tabPanel("线路关联",
                            div(class="outer",
                                tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                chorddiagOutput("line_chord", width = "100%",height="100%")
                            )
                   ),
                   
                   ####乘站数量####
                   tabPanel("乘站数量",
                            div(class="outer",
                                tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                plotlyOutput("paths_person_plot", width = "100%",height="100%")
                            )
                   ),
                   
                   ####虚拟换乘####
                   tabPanel("虚拟换乘",
                            div(class="outer",
                                tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                                absolutePanel(top = 20, left = 10,
                                              plotlyOutput("gg_transfer_plot1", width = "600px", height="400px")
                                ),
                                absolutePanel(top = 20, right = 10,
                                              plotlyOutput("gg_transfer_plot2", width = "600px", height="400px")
                                )
                            )
                   )
)
)
