library(jsonlite)
library(data.table)
telep <- fromJSON('hu_telepules.geojson')
adat <- data.frame(telep$features$properties)
my_v <- c('kisterseg', 'megye', 'name', 'regio')
adat <- data.table(adat[,my_v])
names(adat)[3]<- "telepules"

write.csv(adat, 'telepules_geo_adat.csv', row.names = F)


egyhazi <- fread('egyhazi_iskolak.csv')
