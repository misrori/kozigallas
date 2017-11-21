library(jsonlite)
library(data.table)
# telep <- fromJSON('hu_telepules.geojson')
# adat <- data.frame(telep$features$properties)
# my_v <- c('kisterseg', 'megye', 'name', 'regio')
# adat <- data.table(adat[,my_v])
# names(adat)[3]<- "telepules"
#write.csv(adat, 'telepules_geo_adat.csv', row.names = F)


egyhazi <- data.table(fread('egyhazi_iskolak.csv'))
telep_adatok <- data.table(fread('telepules_geo_adat.csv'))
telep_tipusok <- data.table(fread('telepules_tipus.csv'))

setkey(telep_tipusok, telepules)
setkey(telep_adatok, telepules)
telep_besorolassal <- telep_tipusok[telep_adatok]

egyhazitelep <- egyhazi$Varos

telep_besorolassal$egyhazi_iskola <- ifelse(telep_besorolassal$telepules %in%egyhazitelep,1,0)

write.csv(telep_besorolassal, 'telepules_besorolasssal_egyhazzal.csv', row.names = F)
