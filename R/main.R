
## Compute charge profile for STO lines heading to Ottawa downtown
## Consider only a portion of Jan/Feb 2017 and average it to 1 day

source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")
source("/Users/lavieestuntoucan/civ-sto/tech/profil_de_charge/R/include.R")

## donnes brutes cartes puces : montees, efficacité par ligne sur les arrêts d'ottawa?

## User input data
set.user.data()

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

## Fraction of trips with destination estimated
frac.destOK()

## Plot charge profile - 24h
plot.charge.profiles()

#########################################
## Time semi-histogram (moving mean)
cards.ott <- cards %>% filter(NoLigne %in% routes.sto & NumArret >= 5000 & NumArret < 6000 )
bing <- seq(0,24,0.25)
hist.list <- vector("list",length=length(bing))
for( i in 1:length(bing)){
  fr <- cards.ott %>% 
    filter( HeureTransaction <= bing[i] & HeureTransaction > bing[i] - 1) %>% 
    nrow() / cards.ott %>% nrow()
  hist.list[[i]] <- round(100*fr,digits = 1)
}
hist.df <- rbindlist(lapply(hist.list, as.list))
hist.df$t <- bing

p <- plot_ly(hist.df, x = ~t, y = ~V1, type = 'scatter', mode='lines') %>%
  layout(yaxis = list(title = 'Pourcentage des déplacements (%)'), 
         xaxis = list(title = "Heure de déplacements"))
htmlwidgets::saveWidget(p, paste0(output.dir,"plots/achaldg_heure_lignesSTO_arretsOttawa.html"))

####################
## Remplissage des matrices
my.lines.1 <- c(22,25,26,29,40,41,44,45,46,47)
my.lines.2 <- c(48)
my.lines.3 <- c(11,17)
my.lines.4 <- c(27)
my.lines.5 <- c(85,88,93,94,95,98)
my.lines.6 <- c(20,31,32,33)
my.lines.7 <- c(23,24,34)
my.lines.8 <- c(35,36,37,38)
my.lines.9 <- c(55,59,67,200,400)

my.stops.1 <- c(5048,5040,5032,5024,5025)
my.stops.2 <- c(5026,5030,5034,5042,5050)
my.stops.3 <- c(5014,5022,5030,5034,5042,5050)

my.lines <- my.lines.9
my.stops <- my.stops.3
md <- "m"
create.matrix.md(my.lines,my.stops,md)

my.stops.1 <- c(5048,5040,5032,5024,5025)
my.stops.2 <- c(5026,5030,5034,5042,5050)
my.stops.3 <- c(5014,5022,5030,5034,5042,5050)
stops.sto <- c(my.stops.1,my.stops.2,my.stops.3)

round(cards %>% filter(NoLigne == 400 & NumArret == 5030 ) %>% nrow() /21 / eff.destOK)

round(cards %>% filter(NoLigne == 11 & NumArret %in% my.stops.2) %>% nrow() /21 / eff.destOK)
round(cards %>% filter(NoLigne == 11 & NumArret %in% stops.sto) %>% nrow() /21 / eff.destOK)

round(cards %>% filter(NoLigne == 11 & NumArret %in% stops.sto & !(NumArret %in% my.stops.2)) %>% nrow() /21 / eff.destOK)
round(cards %>% filter(NoLigne == 11 & NumArretDebarquement %in% stops.sto & !(NumArret %in% my.stops.2)) %>% nrow() /21 / eff.destOK)
cards %>% filter(NoLigne == 11 & NumArretDebarquement == 5022) %>% nrow() 

round(cards %>% filter(NoLigne == 87 & NumArret %in% stops.sto) %>% nrow() /21 / eff.destOK)
round(cards %>% filter(NoLigne == 87 & NumArretDebarquement %in% stops.sto) %>% nrow() /21 / eff.destOK)

round(cards %>% filter(NumArret %in% stops.sto) %>% nrow() / 21 / eff.destOK)
x <- cards %>% filter(NumArret %in% stops.sto)

ggplot() +
  geom_histogram(data=x,aes(x=HeureTransaction,y=7950*..count../sum(..count..)),binwidth = 1) +
  scale_x_continuous(breaks = 0:25) +
  scale_y_continuous(breaks=seq(0,2500,100))

ggplot() +
  geom_histogram(data=x,aes(x=HeureTransaction,y=100*..count../sum(..count..)),binwidth = 1) +
  scale_x_continuous(breaks = 0:25) +
  scale_y_continuous(breaks=seq(0,100,2))

######
create.matrix.md <- function(my.lines,my.stops,md){
  m <- matrix(NA, ncol = length(my.lines), nrow = length(my.stops))
  for( i in 1:length(my.lines)){
    df <- count.md(my.lines[i],my.stops,md)
    for( j in 1:length(my.stops) ){
      if( !is.na(df)[1]){
       n <- df %>% filter(stop_id == my.stops[j]) %>% select(count) %>% as.numeric()
      }else{
        n <- NA
      }
      m[j,i] <- n
    }
  } 
  write.table(m,file = "/Users/lavieestuntoucan/Desktop/temp.csv",sep=",",row.names = F,quote = F,col.names = F)
  print(m)
}
##



my.lines <- c(22,25,26,29,40,41,44,45,46,47,48,11,17,27,85,88,93,
              94,95,98,20,31,32,33,23,24,34,35,36,37,38,55,59,67,87,200,300,400)
my.stops <- c(5048,5040,5032,5024,5025,
              5026,5030,5034,5042,5050,
              5014,5022,5030,5034,5042,5050,5016)
md <- "m"
create.matrix.md(my.lines,my.stops,md)

create.matrix.md <- function(my.lines,my.stops,md){
  m <- matrix(NA, ncol = length(my.lines), nrow = length(my.stops))
  for( i in 1:length(my.lines)){
    df <- count.md(my.lines[i],my.stops,md)
    for( j in 1:length(my.stops) ){
      if( !is.na(df)[1]){
        n <- df %>% filter(stop_id == my.stops[j]) %>% select(count) %>% unique() %>% as.numeric()
      }else{
        n <- NA
      }
      m[j,i] <- n
    }
  } 
  m <- as.data.frame(m)
  colnames(m) <- my.lines
  m$stops <- my.stops
  write.table(m,file = "/Users/lavieestuntoucan/Desktop/temp.csv",sep=",",quote = F,row.names = F)
  print(m)
}



# 
# create.matrix <- function(my.lines,my.stops,md){
#   m <- matrix(NA, ncol = length(my.lines), nrow = length(my.stops))
#   for( i in 1:length(my.lines)){
# 
#     if( nrow(i.var) == 1 ) df <- count.charge(my.lines[i],i.var$trip_headsign[1]) 
#     if( nrow(i.var) > 1 ){
#       datalist = list()
#       for(j in 1:nrow(i.var)){
#         datalist[[j]] <- count.charge(my.lines[i],i.var$trip_headsign[j]) 
#       }
#       df <- do.call(rbind, datalist)
#     }
#     for( j in 1:length(my.stops) ){
#       if( !is.na(df)[1]){
#         if( md == "d" ) n <- df %>% filter(stop_id == my.stops[j]) %>% select(des)
#         if( md == "m" ) n <- df %>% filter(stop_id == my.stops[j]) %>% select(mont)
#         colnames(n) <- "count"
#         cat(paste(i,j,"\n"))
#         print(n)
#         if( nrow(n) == 1 ) n <- n$count
#         if( nrow(n) > 1 ) n <- sum(n$count)
#       }else{
#         n <- NA
#       }
#       cat(paste(i,j,n,"\n"))
#       m[j,i] <- n
#     }
#   } 
#   write.table(m,file = "/Users/lavieestuntoucan/Desktop/temp.csv",sep=",",row.names = F,quote = F,col.names = F)
#   print(m)
# }

########################################################################
