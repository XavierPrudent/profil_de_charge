#####################################################
## User input data
set.user.data <- function(){
  gtfs  <<- "/Users/lavieestuntoucan/civ-sto/data/GTFS/sto/jan2017-july2017/"
  chipCards <<- "/Users/lavieestuntoucan/civ-sto/data/cartes_a_puces/sto/destination_estimee/Deplacement2017JanFev.CSV"
  routes.sto <<- c( 11, 17, 22, 23, 24, 25, 26, 27, 29, 31, 
                    32, 33, 34, 35, 36, 37, 38, 40, 41, 44, 
                    45, 46, 47, 55, 59, 67, 87, 85, 82, 94, 
                    98, 93, 95, 300, 400, 200, 20 )
  trip.tag <<- "HIV"
  my.week  <<- c("Tuesday","Wednesday","Thursday")
  my.week.tag <<- "Semaine"
  output.dir <<- "/Users/lavieestuntoucan/civ-sto/tech/profil_de_charge/out/"
  first.day <<- as.Date("2017-01-09")
  last.day <<- as.Date("2017-02-26")
}
#####################################################
read.gtfs <- function(){
  trips <<- fread(paste0(gtfs,"trips.txt"))
  routes <<- fread(paste0(gtfs,"routes.txt"))
  stops <<- fread(paste0(gtfs,"stops.txt"))
  stop_times <<- fread(paste0(gtfs,"stop_times.txt"))
}
#####################################################
shape.cut.gtfs <- function(){
  ## Analysis cuts
  trips <<- trips %>% rename(route_var_id = route_id)
  routes <<- routes %>% rename(route_var_id = route_id) %>% rename(route_id = route_short_name)
  ## Data shaping
  routes <<- routes %>% filter(route_id %in% routes.sto)
  trips <<- trips %>% filter(route_var_id %in% routes$route_var_id & 
                               grepl(my.week.tag,trip_id) &
                               grepl(trip.tag, trip_id)) %>% 
    left_join(routes,by="route_var_id")
  stop_times <<- stop_times %>% filter(grepl(trip.tag, trip_id) & 
                                         trip_id %in% trips$trip_id)
}
#####################################################
read.cards <- function(){
  cards <<- fread(chipCards,sep="~",colClasses=c(NumCarteSerie="character"),header=TRUE)
  cards <<- cards %>% 
    mutate(DateDeplacement = as.Date(DateDeplacement), 
           weekday = weekdays(DateDeplacement) )
}
#####################################################
plot.freq <- function(){
  p <- plot_ly(data = cards , x = ~DateDeplacement, type = "histogram") %>%
    layout(xaxis=list(type="date", tickformat="%d/%m", title="Date du déplacement"),
           yaxis=list(title="Décompte"))
  p
  htmlwidgets::saveWidget(p, paste0(output.dir,"frequentation_jours.html"))
}
#####################################################
cut.data.period <- function(){
  cards <<- cards %>% 
    filter(DateDeplacement >= first.day & DateDeplacement <= last.day) %>%
    filter(weekday %in% my.week)
}
#####################################################
## define a helper function
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}
#####################################################
## Merge all trips info
merge.trips.info <- function(){
  DateDeplacement <- c(cards$DateDeplacement,cards$DateDeplacement,cards$DateDeplacement,
                       cards$DateDeplacement,cards$DateDeplacement,cards$DateDeplacement,
                       cards$DateDeplacement,cards$DateDeplacement,cards$DateDeplacement,cards$DateDeplacement) 
  ## transform all columns to NA if empty
  cards <<- cards %>% mutate_all(funs(empty_as_na)) 
  ## append all trips
  HeureTransaction <- c(cards$HeureTransaction,cards$HeureTransaction2,cards$HeureTransaction3,
                        cards$HeureTransaction4,cards$HeureTransaction5,cards$HeureTransaction6,
                        cards$HeureTransaction7,cards$HeureTransaction8,cards$HeureTransaction9,cards$HeureTransaction10)
  NoLigne <- c(cards$NoLigne1,cards$NoLigne2,cards$NoLigne3,
               cards$NoLigne4,cards$NoLigne5,cards$NoLigne6,
               cards$NoLigne7,cards$NoLigne8,cards$NoLigne9,cards$NoLigne10)
  NumArret <- c(cards$NumArret1,cards$NumArret2,cards$NumArret3,
                cards$NumArret4,cards$NumArret5,cards$NumArret6,
                cards$NumArret7,cards$NumArret8,cards$NumArret9,cards$NumArret10)
  NumArretDebarquement <- c(cards$NumArretDebarquement1,cards$NumArretDebarquement2,cards$NumArretDebarquement3,
                            cards$NumArretDebarquement4,cards$NumArretDebarquement5,cards$NumArretDebarquement6,
                            cards$NumArretDebarquement7,cards$NumArretDebarquement8,cards$NumArretDebarquement9,cards$NumArretDebarquement10)
  ## Make the dataframe
  cards <<- data.frame(DateDeplacement,HeureTransaction,NoLigne,NumArret,NumArretDebarquement)
  cards <<- cards %>% filter(!is.na(NoLigne) & !is.na(NumArret) )
  ## Hour
  cards <<- cards %>% 
    separate(col=HeureTransaction, sep=":", into=c("Hres","Mins","Secs")) %>% 
    mutate(weekday=weekdays(DateDeplacement)) %>%
    mutate(HeureTransaction = round(as.numeric(Hres) + as.numeric(Mins)/60,digits=1)) %>% select(-c(Hres,Mins,Secs))
  cards.clean <<- cards %>% filter(!is.na(NumArretDebarquement))
}
#####################################################
frac.destOK <- function(){
  n.all <- nrow(cards)
  n.destOK <- cards %>% filter(!is.na(NumArretDebarquement)) %>% nrow()
  eff.destOK  <<-  n.destOK / n.all
  frac.cartes <<- 0.80
  n.days <<- length(unique(cards$DateDeplacement))
}
#####################################################
## Count charge and plot profile per routes
plot.charge.profiles <- function(){
  ## Loop over the routes
  for( i.route_id in routes.sto){
    ##
    ## Extract variantes
    list.var <- extract_variantes(i.route_id)
    for( i.var in list.var$trip_headsign){
      ##
      ## Count
      df <- count.charge(i.route_id,i.var)
      print(df)
      if( is.na(df)[1] ) next
      ##
      ## Plot the data and save
      p <- create.plotly(df)
      htmlwidgets::saveWidget(p, paste0(output.dir,"profilCharge_ligne",i.route_id,"_",i.var,".html"))
          }
  }
}
#####################################################
create.plotly <- function(df){
  p <- plot_ly(df, x = ~stop_id, y = ~mont, type = 'bar', name = 'Montées',hoverinfo = 'text',
               text = ~paste('Arrêt : ', stop_id,
                             '</br>Nom :', stop_name,
                             '</br>Montées : ', mont)) %>%
    add_trace(y = ~-des, name = 'Descentes', hoverinfo = 'text',
              text = ~paste('Arrêt : ', stop_id,
                            '</br>Nom :', stop_name,
                            '</br>Descentes : ', des)) %>%
    add_trace(y = ~charge, name = 'Charge', type='scatter', mode = 'lines+markers',
              hoverinfo = 'text',
              text = ~paste('Arrêt : ', stop_id,
                            '</br>Nom :', stop_name,
                            '</br>Charge : ', charge))  %>%
    layout(margin = list(l=50, r=50, b=100, t=50, pad=0),
           yaxis = list(title = 'Décompte'), xaxis = list(title = "Numéro de l'arrêt"))
  return(p)
}
#####################################################
## For a given route, extract the variantes
extract_variantes <- function(i.route_id){
  list.var <- trips %>% filter(route_id == i.route_id) %>% dplyr::select(trip_headsign) %>% unique()
  return(list.var)
}
#####################################################
count.charge <- function(i.route_id,i.var){
  
  cat(paste(i.route_id,i.var,"\n"))
  ##
  ## Extract the list of stops
  list.stops <- extract_stops(i.route_id,i.var)
  if( is.na(list.stops)[1]) return(NA)
  if( nrow(list.stops) == 0) return(NA)
  if( nrow(list.stops) != length(unique(list.stops$stop_id)) ) return(NA) # no loops
  ##
  ## Extract trips on this line and variant
  
  ## Clean data (only valid final stops)
  i.cards.clean <- cards.clean %>% filter(NoLigne == i.route_id & 
                                            !is.na(NumArretDebarquement) &
                                            NumArret %in% list.stops$stop_id &
                                            NumArretDebarquement %in% list.stops$stop_id) %>% 
    rowwise() %>% 
    mutate(NumArretSeq = list.stops[which(list.stops$stop_id == NumArret),"stop_seq"],
           NumArretDebarquementSeq = list.stops[which(list.stops$stop_id == NumArretDebarquement),"stop_seq"]) %>%
    filter(NumArretDebarquementSeq > NumArretSeq)
  ## All data (final stop valid or NA)
  i.cards <- cards %>% filter(NoLigne == i.route_id & 
                                NumArret %in% list.stops$stop_id)
  ##
  ## Number of in and out, mont.glo is the number of montees from data w/wo valid final stops
  df.stops.j <- list.stops %>% 
    mutate(mont.glo=NA,mont=NA,mont.tot=NA,des=NA,des.tot=NA,charge=NA) %>%
    rowwise() %>% 
    mutate(mont.glo = length(which(i.cards$NumArret == stop_id)), 
           mont = length(which(i.cards.clean$NumArret == stop_id)), 
           des = length(which(i.cards.clean$NumArretDebarquement == stop_id)))
  ##
  ## Scale up according to the algorithm efficiency
  df.stops.j <- mutate(df.stops.j,  mont = mont / eff.destOK, des = des / eff.destOK)
  ##
  ## Scale up according to the fraction of trips with cards
  df.stops.j <- mutate(df.stops.j,  mont.glo = mont.glo / frac.cartes, mont = mont / frac.cartes, des = des / frac.cartes)
  ##
  ## Average down to the number of days included
  df.stops.j <- mutate(df.stops.j,  mont.glo = mont.glo / n.days, mont = mont / n.days, des = des / n.days )
  ##
  ## Total loading
  df.stops.j$mont <- round(df.stops.j$mont)
  df.stops.j$des <- round(df.stops.j$des)
  df.stops.j$mont.tot <- round(cumsum(df.stops.j$mont))
  df.stops.j$des.tot <- round(cumsum(df.stops.j$des))
  df.stops.j$charge <- round(df.stops.j$mont.tot - df.stops.j$des.tot)
  df.stops.j$stop_id <- factor(df.stops.j$stop_id, levels = df.stops.j$stop_id)
  
  return(df.stops.j)
}
#####################################################
## For a given route and variante, extract the list of trips and their stop
extract_stops <- function(i.route_id,i.var){
  list.stops <- list()
  list.trips <- trips %>% 
    filter(route_id == i.route_id & trip_headsign == i.var) %>% 
    dplyr::select(trip_id) %>% unique()
  if( nrow(list.trips) == 0) return(NA)
  
  ## Loop over trips
  for( i in 1:nrow(list.trips) ){
    list.stops[[i]] <- stop_times %>% 
      filter(trip_id == list.trips$trip_id[i]) %>% 
      dplyr::select(stop_id)
  }
  
  ## Return only 1 list, the longest
  list.stops <- unique(list.stops)
  if( length(list.stops) > 1){
    cnt <- sapply(list.stops, nrow)
    list.stops <- list.stops[cnt == max(cnt)]
  }
  list.stops <- as.data.frame(list.stops)
  if( nrow(list.stops) == 0) return(NA)
  
  ## Append the stop name
  list.stops <- list.stops %>% 
    mutate(stop_id = as.character(stop_id) ) %>% 
    left_join(stops,by="stop_id") %>% 
    dplyr::select(stop_id,stop_name) %>%
    mutate(stop_seq=row_number())
  
  return(list.stops)
}
#####################################################
count.md <- function(i.route_id,list.stops,md){
  ##
  ## Extract trips on this line for these stops
  i.cards <- cards %>% filter(NoLigne == i.route_id & !is.na(NumArretDebarquement) )
  if( md == "m" ) i.cards <- i.cards %>% filter(NumArret %in% list.stops)
  if( md == "d" ) i.cards <- i.cards %>% filter(NumArretDebarquement %in% list.stops)
  list.stops <- data.frame(stop_id=list.stops)
  ##
  ## Number of in and out
  df.stops.j <- list.stops %>% 
    mutate(mont=NA,des=NA) %>%
    rowwise() %>% 
    mutate(mont = length(which(i.cards$NumArret == stop_id)), 
           des = length(which(i.cards$NumArretDebarquement == stop_id)))
  ##
  ## Scale up according to the algorithm efficiency
  df.stops.j <- mutate(df.stops.j,  mont = mont / eff.destOK, des = des / eff.destOK)
  ##
  ## Scale up according to the fraction of trips with cards
  df.stops.j <- mutate(df.stops.j,  mont = mont / frac.cartes, des = des / frac.cartes)
  ##
  ## Average down to the number of days included
  df.stops.j <- mutate(df.stops.j,  mont = mont / n.days, des = des / n.days )
  ##
  ## Total loading
  df.stops.j$mont <- round(df.stops.j$mont)
  df.stops.j$des <- round(df.stops.j$des)
  if( md == "m" ) df.stops.j <- select(df.stops.j,c(stop_id,mont))
  if( md == "d" ) df.stops.j <- select(df.stops.j,c(stop_id,des))
  colnames(df.stops.j)[2] <- "count"
  return(df.stops.j)
}



