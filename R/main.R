
source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")
source("/Users/lavieestuntoucan/civ-sto/tech/profil_de_charge/R/include.R")

## Difference entre 31 et 131, sachant qu'il existe des variantes pour la 31 seule
## Pourquoi certains arrêts de ligne, contenus dans la BD, ne sont pas dans le gtfs? (67%)
## pas de profile de charge pour les lignes avec 2fois le meme arrêt

#################
## User input data
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
#################

## Read gtfs data
read.gtfs()

## Analysis cuts and shaping
shape.cut.gtfs()

## Chip card data
read.cards()

## Plot counting per day
plot.freq()

## Select data period
cut.data.period()

## Merge of trips of a user
merge.trips.info()

## Loop over the routes
for( i.route_id in routes.sto){
  ##
  ## Extract variantes
  list.var <- extract_variantes(i.route_id)
  for( i.var in list.var$trip_headsign){
    cat(paste(i.route_id,i.var,"\n"))
    ##
    ## Extract the list of stops
    list.stops <- extract_stops(i.route_id,i.var)
    if( nrow(list.stops) == 0) next
    if( nrow(list.stops) != length(unique(list.stops$stop_id)) ) next # no loops
    ##
    ## Extract trips on this line and variant
    i.cards <- cards %>% filter(NoLigne == i.route_id & 
                                  !is.na(NumArretDebarquement) &
                                  NumArret %in% list.stops$stop_id &
                                  NumArretDebarquement %in% list.stops$stop_id) %>% 
      rowwise() %>% 
      mutate(NumArretSeq = list.stops[which(list.stops$stop_id == NumArret),"stop_seq"],
             NumArretDebarquementSeq = list.stops[which(list.stops$stop_id == NumArretDebarquement),"stop_seq"]) %>%
      filter(NumArretDebarquementSeq > NumArretSeq)
    ##
    ## Number of in and out
    df.stops.j <- list.stops %>% 
      mutate(mont=NA,mont.tot=NA,des=NA,des.tot=NA,charge=NA) %>%
      rowwise() %>% 
      mutate(mont = length(which(i.cards$NumArret == stop_id)), 
             des = length(which(i.cards$NumArretDebarquement == stop_id)))
    ##
    ## Total loading
    df.stops.j$mont.tot <- cumsum(df.stops.j$mont)
    df.stops.j$des.tot <- cumsum(df.stops.j$des)
    df.stops.j$charge <- df.stops.j$mont.tot - df.stops.j$des.tot 
    df.stops.j$stop_id <- factor(df.stops.j$stop_id, levels = df.stops.j$stop_id)
    ##
    ## Plot the whole
    p <- plot_ly(df.stops.j, x = ~stop_id, y = ~mont, type = 'bar', name = 'Montées',hoverinfo = 'text',
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
    
    out.html <- paste0(output.dir,"profilCharge_ligne",i.route_id,"_var",j,".html")
    htmlwidgets::saveWidget(p, out.html)
  }
}




########################################################################

# ggplot(data=df.stops.j) + 
#    geom_bar(aes(stop_seq,mont),stat="identity",fill="red") +
#    geom_bar(aes(stop_seq,-des),stat="identity",fill="blue") +
#    geom_point(aes(x=stop_seq,y=charge),color="forestgreen",size=1) +
#    geom_line(aes(x=stop_seq,y=charge),color="forestgreen",size=1) +
#    scale_x_continuous(breaks=df.stops.j$stop_seq,labels = df.stops.j$stop_id) +
#    theme(axis.text.x = element_text(angle=60, hjust=1)) +
#    xlab("Numéro de l'arrêt") +
#    ylab("Nombre de personnes")
