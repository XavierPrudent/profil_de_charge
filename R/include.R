

#####################################################
cut.data.period <- function(){
  cards <<- cards %>% filter(DateDeplacement >= first.day & DateDeplacement <= last.day) %>%
    filter(weekday %in% my.week)
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
read.gtfs <- function(){
  trips <<- fread(paste0(gtfs,"trips.txt"))
  routes <<- fread(paste0(gtfs,"routes.txt"))
  stops <<- fread(paste0(gtfs,"stops.txt"))
  stop_times <<- fread(paste0(gtfs,"stop_times.txt"))
}

#####################################################
shape.cut.gtfs <- function(){
  ## Analysis cuts
  trips <<- trips %>% separate(route_id,sep="-",into=c("route_id","var"))
  routes <<- routes %>% separate(route_id,sep="-",into=c("route_id","var"))
  ## Data shaping
  trips <<- trips %>% filter(route_id %in% routes.sto & 
                                grepl(my.week.tag,trip_id) &
                                grepl(trip.tag, trip_id))
  routes <<- routes %>% filter(route_id %in% routes.sto)
  stop_times <<- stop_times %>% filter(grepl(trip.tag, trip_id) & 
                                          trip_id %in% trips$trip_id)
}

#####################################################
## For a given route, extract the variantes
extract_variantes <- function(i.route_id){
  list.var <- trips %>% filter(route_id == i.route_id) %>% dplyr::select(trip_headsign) %>% unique()
  return(list.var)
}

#####################################################
## For a given route and variante, extract the list of trips and their stop
extract_stops <- function(i.route_id,i.var){
  list.stops <- list()
  list.trips <- trips %>% 
    filter(route_id == i.route_id & trip_headsign == i.var) %>% 
    dplyr::select(trip_id) %>% unique()
  
  ## Loop over trips
  i <- 1
  for( i.trip in list.trips$trip_id){
    list.stops[[i]] <- stop_times %>% filter(trip_id == i.trip) %>% dplyr::select(stop_id)
    i <- i +1
  }
  
  ## Return only 1 list, the longest
  list.stops <- unique(list.stops)
  if( length(list.stops) > 1){
    cnt <- sapply(list.stops, length)
    list.stops <- list.stops[cnt == max(cnt)]
  }
  list.stops <- as.data.frame(list.stops)
  
  ## Append the stop name
  list.stops <- list.stops %>% 
    mutate(stop_id = as.character(stop_id) ) %>% 
    left_join(stops,by="stop_id") %>% 
    dplyr::select(stop_id,stop_name) %>%
    mutate(stop_seq=row_number())
  
  return(list.stops)
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
                       cards$DateDeplacement,cards$DateDeplacement,cards$DateDeplacement,cards$DateDeplacement) ## transform all columns to NA if empty
  ## Empty as NA
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
  cards <<- data.frame(DateDeplacement,HeureTransaction,NoLigne,NumArret,NumArretDebarquement)
  cards <<- cards %>% filter(!is.na(NumArretDebarquement))
}



