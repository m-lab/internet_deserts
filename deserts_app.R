library(tidyverse)
library(shiny)
library(leaflet)
library(sp)
library(viridis)

circles_plt<-read.csv("circles.csv")
uniq.client<-read.csv("client_list.csv")
uniq.serv<-read.csv("serv_list.csv")
M.rtt <- read.csv("M_rtt.csv")
M.speed <- read.csv("M_speed.csv")
M.tot <- read.csv("M_num.csv")
color_list<-viridis(40, option="cividis")%>%str_sub(start=1,end=-3)
leg_labs=c("0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 40", "40 to max")
leg_cols<-c("#002A63" ,"#2A406C", "#50576C" ,"#6E6F73" ,"#BBAE6F", "#FFEA46")
server <- function(input, output, session) {
  library(geosphere)
  line_labs=c()
  circ_labs=c()
  top = 49.3457868 # north lat
  left = -124.7844079 # west long
  right = -66.9513812 # east long
  bottom = 24.7433195
  
  # circles<-D0%>%filter(med_dist>0)%>%filter(between(client_lat, bottom, top))%>%filter(between(serv_lat, bottom, top))%>%
  #   filter(between(serv_lon, left, right))%>%filter(between(client_lon, left, right))%>%filter(tot_ip>50000)%>%
  #   select(client_lat, client_lon, serv_lat, serv_lon)%>%distinct
  # circles_plt<-bind_rows(circles[,c(1,2)],circles[,c(3,4)])%>%distinct()%>%select(lat=client_lat, lon=client_lon)
  # circles_plt<-data.frame(circles_plt, ids = 1:nrow(circles_plt))
  
  gen_rand <- function(n) {
    a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }
  
  client_circ<- function(map, x, y) addCircleMarkers(map, x, y, radius=8, color="#ffa500", fillColor="#ffa500", fillOpacity=.75, 
                                                     opacity=.75, weight=2, stroke=TRUE, layerId="Selected")
  serv_circ<- function(map, x, y, id_labs, group_name) addCircleMarkers(map, x, y, radius=6, color="#ffd994", fillColor="#ffd994", 
                                                                        fillOpacity=.75, opacity=.75, weight=2, stroke=TRUE, layerId=id_labs, group = group_name)
  
  get_client_server_shapes<-function(uniq.client1, uniq.serv1,M.speed1,M.tot1, p1){
    # lat.keep<-which(D1$client_lat==p1$lat)
    # lon.keep<-which(D1$client_lon==p1$lng)
    # keeps<-lon.keep[which(lon.keep%in%lat.keep)]
    # Dn<-D1[keeps,]
    # 
    client_row_num<-uniq.client1%>%filter(client_lat==p1$lat)%>%filter(client_lon==p1$lng)
    uniq.serv1<-uniq.serv1%>%select(serv_lat, serv_lon)
    client_M_ind<-which(M.speed1[client_row_num$X1.nrow.uniq.client.,]>0)
    speeds<-M.speed1[client_row_num$X1.nrow.uniq.client.,client_M_ind]
    tots<-M.tot1[client_row_num$X1.nrow.uniq.client.,client_M_ind]
    dest_servs<-uniq.serv1[client_M_ind,c(1,2)]
    plotData1<-data.frame("client_lat"=rep(p1$lat, nrow(dest_servs)),
                          "client_lon"=rep(p1$lng, nrow(dest_servs)),  dest_servs)%>%distinct%>%na.omit
    
    
    #Dn<-D%>%filter(client_lat==lat.match)%>%filter(client_lon==lon.match)
    
    # plotData1<-Dn%>%mutate(client_lat = as.numeric(as.character(client_lat)),
    #                        serv_lat = as.numeric(as.character(serv_lat)),
    #                        client_lon = as.numeric(as.character(client_lon)),
    #                        serv_lon = as.numeric(as.character(serv_lon))) %>%
    #   group_by(client_lat,client_lon,serv_lat,serv_lon) %>%
    #   summarize(count=sum(tot_ip))
    
    L<-vector(mode="list", length=nrow(plotData1))
    
    for(i in 1:length(L)){
      L[[i]]<-gcIntermediate(c(plotData1$client_lon[i],plotData1$client_lat[i]),
                             c(plotData1$serv_lon[i],plotData1$serv_lat[i]),addStartEnd=TRUE,sp=TRUE)
    }
    spatial.L<-do.call(rbind, L)
    client_loc<-plotData1[,c(1,2)]%>%distinct
    serv_locs<-plotData1[,c(3,4)]%>%distinct
    return(list("client"=client_loc, "servers"=serv_locs,"connecting_lines"=spatial.L, "line_speed"=speeds, "line_usage"=tots))
  }
  
  output$Map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4))%>%addTiles()%>%
      addCircleMarkers(data=circles_plt, lng =circles_plt$lon,lat=circles_plt$lat,
                       radius=2, color="#2c2c2c", stroke=FALSE, fillOpacity=0.5, group="locations", layerId = ~ids)%>%
      setView(lat=39.8285889,lng=-98.5806116, zoom =4.3)%>%
      setMaxBounds(lng1 = left, lat1 = bottom, lng2 = right, lat2 = top )%>%
      addLegend(position="topright",colors = leg_cols, labels =leg_labs )
    
  })
  
  observeEvent(input$Map_marker_click, { # update the map markers and view on map clicks
    p <- input$Map_marker_click
    proxy <- leafletProxy("Map")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      shapes<-get_client_server_shapes(uniq.client, uniq.serv, M.speed, M.tot, p)
      line_labs <- gen_rand(length(shapes$connecting_lines))
      circ_labs <- gen_rand(nrow(shapes$servers))
      weight_vals<-(shapes$line_usage[-1]+1)^((1/6))%>%as.numeric%>%unlist#log(shapes$line_usage[-1]+1)%>%as.numeric%>%unlist
      
      speeds<- as.numeric(shapes$line_speed[-1])
      #for a legend
      if(max(speeds)>40){
        speed_leg<-cut(speeds, breaks=c(0,5,10,15, 20, 40, max(speeds)))
      }else{
        speed_leg<-cut(speeds, breaks=c(0,5,10,15, 20, 40))
      }

      speed_ind<-speed_leg%>%str_split(",")%>%
        lapply(function(x)return(round(mean(as.numeric(c(str_sub(x[1],2),str_sub(x[2],end=-2)))))))%>%unlist
      speed_ind<-ifelse(speed_ind<41, speed_ind+1, 40)
      cols<-color_list[speed_ind]

      proxy%>%clearGroup("end_points") %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom)%>%
        addPolylines(data=shapes$connecting_lines, layerId = line_labs,color=cols, group="end_points", opacity =.6,
                     weight =weight_vals*2)%>%
        serv_circ(shapes$servers$serv_lon,shapes$servers$serv_lat, id_labs=circ_labs, "end_points")%>%
        client_circ(shapes$client$client_lon,shapes$client$client_lat)
      
    }
  })
  # 
  # observeEvent(input$Map_marker_click, { # update the location selectInput on map clicks
  #   p <- input$Map_marker_click
  #   if(!is.null(p$id)){
  #     if(is.null(input$location) || input$location!=p$id) updateSelectInput(session, "location", selected=p$id)
  #   }
  # })
  # 
  # observeEvent(input$location, { # update the map markers and view on location selectInput changes
  #   p <- input$Map_marker_click
  #   p2 <- subset(circles_plt, ids==input$location)
  #   proxy <- leafletProxy("Map")
  #   if(nrow(p2)==0){
  #     proxy %>% removeMarker(layerId="Selected")
  #   } else if(length(p$id) && input$location!=p$id){
  #     proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
  #   } else if(!length(p$id)){
  #     proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
  #   }
  # })
  
}

ui <- bootstrapPage(
  titlePanel("M-lab client-server connections"),
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("Map", width="100%", height="100%")
)

shinyApp(ui, server)





