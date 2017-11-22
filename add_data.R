library(jsonlite)
library(data.table)
# telep <- fromJSON('hu_telepules.geojson')
# adat <- data.frame(telep$features$properties)
# my_v <- c('kisterseg', 'megye', 'name', 'regio')
# adat <- data.table(adat[,my_v])
# names(adat)[3]<- "telepules"
#write.csv(adat, 'telepules_geo_adat.csv', row.names = F)


egyhazi <- data.table(fread('egyhazi_iskolak.csv'))
egyhazi[startsWith(egyhazi$Varos, 'Budapest'),]$Varos <- "Budapest"


telep_adatok <- data.table(fread('telepules_geo_adat.csv'))
telep_tipusok <- data.table(fread('telepules_tipus.csv'))

setkey(telep_tipusok, telepules)
setkey(telep_adatok, telepules)
telep_besorolassal <- telep_tipusok[telep_adatok]
telep_besorolassal[is.na(telep_besorolassal$tipus),]$tipus <- "nem_hatranyos _besorolasu"


setkey(egyhazi, Varos)
egyhazi_iskolak_telepules_tipussal <- telep_besorolassal[egyhazi]
write.csv(egyhazi_iskolak_telepules_tipussal, 'egyhazi_iskolak_telepules_tipussal.csv', row.names = F)


egyhazitelep <- egyhazi$Varos

telep_besorolassal$egyhazi_iskola <- ifelse(telep_besorolassal$telepules %in%egyhazitelep,1,0)

write.csv(telep_besorolassal, 'telepules_besorolasssal_egyhazzal.csv', row.names = F)


###tisztitaás 
kozig_allasok <- data.table(fread("eredmeny.csv"))

kozig_allasok[munkavegzes_helye=='']$munkavegzes_helye<- 'Nem_megadott , Nem_megadott'
kozig_allasok[munkavegzes_helye=='Buidapest']$munkavegzes_helye<- 'Budapest'
kozig_allasok[startsWith(kozig_allasok$munkavegzes_helye,'Budapest'),]$munkavegzes_helye<- 'Budapest, Budapest'



kozig_allasok$van_e_vesszo<- ifelse(grepl(',',kozig_allasok$munkavegzes_helye),1,0 )
#manuális javítás 

write.csv(kozig_allasok, 'kozig_allasok_man.csv', row.names = F)
# read back

kozig_allasok <- data.table(fread("kozig_allasok_man.csv"))
kozig_allasok$munkavegzes_helye <- trimws(sapply(strsplit(kozig_allasok$munkavegzes_helye,',', fixed = T), '[[', 2))

sum(unique(kozig_allasok$munkavegzes_helye) %in% telep_adatok$telepules)

unique(kozig_allasok$munkavegzes_helye[kozig_allasok$munkavegzes_helye %in%telep_adatok$telepules ==F])

#javitas 
#####
kozig_allasok[munkavegzes_helye=='Gánt- Bányatelep']$munkavegzes_helye <- 'Gánt'
kozig_allasok[munkavegzes_helye=='Polgárdi-Tekerespuszta']$munkavegzes_helye <- 'Polgárdi'
kozig_allasok[munkavegzes_helye=='Parádfürdő']$munkavegzes_helye <- 'Parád'
kozig_allasok[munkavegzes_helye=='Szécsény járás és települései']$munkavegzes_helye <- 'Szécsény'
kozig_allasok[munkavegzes_helye=='Sopron-Brennbergbánya']$munkavegzes_helye <- 'Sopron'
kozig_allasok[munkavegzes_helye=='Mátraháza']$munkavegzes_helye <- 'Pálosvörösmart'
kozig_allasok[munkavegzes_helye=='Bernecebaráti- Kemence-Penc']$munkavegzes_helye <- 'Bernecebaráti'
kozig_allasok[munkavegzes_helye=='Kecskemét-Méntelek']$munkavegzes_helye <- 'Kecskemét'
kozig_allasok[munkavegzes_helye=='2120 Dunakeszi']$munkavegzes_helye <- 'Dunakeszi'
kozig_allasok[munkavegzes_helye=='Gyermekjóléti Szolgálat ellátási területe']$munkavegzes_helye <- 'Budapest'
kozig_allasok[munkavegzes_helye=='Érd-Dabas-Göd']$munkavegzes_helye <- 'Érd'
kozig_allasok[munkavegzes_helye=='Lakásotthonok']$munkavegzes_helye <- 'Budapest'
kozig_allasok[munkavegzes_helye=='Abasár']$munkavegzes_helye <- 'Pálosvörösmart'
kozig_allasok[munkavegzes_helye=='Miskolc-Egyetemváros']$munkavegzes_helye <- 'Miskolc'
kozig_allasok[munkavegzes_helye=='Kikunhalas']$munkavegzes_helye <- 'Kiskunhalas'
kozig_allasok[munkavegzes_helye=='Tiszaföldvár-Homok']$munkavegzes_helye <- 'Tiszaföldvár'
kozig_allasok[munkavegzes_helye=='Állampusztai Orsz. Bv. Int.']$munkavegzes_helye <- 'Budapest'
kozig_allasok[munkavegzes_helye=='Egerági Táltos Csikó Óvoda']$munkavegzes_helye <- 'Eger'
kozig_allasok[munkavegzes_helye=='Kőkút-Gyöngyöspuszta']$munkavegzes_helye <- 'Kőkút'
kozig_allasok[munkavegzes_helye=='Szekszárd és környéke']$munkavegzes_helye <- 'Szekszárd'
kozig_allasok[munkavegzes_helye=='az ellátott tanulók otthonában']$munkavegzes_helye <- 'Budapest'
kozig_allasok[munkavegzes_helye=='Karcagi Tankerületi Központ intézményei']$munkavegzes_helye <- 'Karcag'
kozig_allasok[munkavegzes_helye=='9023']$munkavegzes_helye <- 'Győr'
kozig_allasok[munkavegzes_helye=='Nyirád és Szőc település']$munkavegzes_helye <- 'Nyirád'
kozig_allasok[munkavegzes_helye=='8360']$munkavegzes_helye <- 'Keszthely'
kozig_allasok[munkavegzes_helye=='Ifjúság Utcai Óvoda Hétszinvirág Tagintézménye']$munkavegzes_helye <- 'Budapest'
kozig_allasok[munkavegzes_helye=='Ifjúság Utcai Óvoda Szoboszlói Úti Tagintézménye']$munkavegzes_helye <- 'Budapest'
kozig_allasok[munkavegzes_helye=='7624']$munkavegzes_helye <- 'Pécs'
kozig_allasok[munkavegzes_helye=='Jászkarajenő közigazgatási területe']$munkavegzes_helye <- 'Jászkarajenő'
kozig_allasok[munkavegzes_helye=='Veszprém-Gyulafirátót']$munkavegzes_helye <- 'Veszprém'
kozig_allasok[munkavegzes_helye=='VECSÉS']$munkavegzes_helye <- 'Vecsés'
kozig_allasok[munkavegzes_helye=='Kisléta\"\"\"\"Harmónia\"\"\"\"Egyes.Szoc.Intéz.Ápoló-Gond.Otth.']$munkavegzes_helye <- 'Nyírbogdány'
kozig_allasok[munkavegzes_helye=='Szeged-Tápé']$munkavegzes_helye <- 'Szeged'
kozig_allasok[munkavegzes_helye=='Sajóhídvég']$munkavegzes_helye <- 'Ónod'
kozig_allasok[munkavegzes_helye=='Kamut és Bélmegyer települések']$munkavegzes_helye <- 'Kamut'
kozig_allasok[munkavegzes_helye=='Polgárdi- Ipartelep']$munkavegzes_helye <- 'Polgárdi'
kozig_allasok[munkavegzes_helye=='Szin  Bódvaszilasi Közös.Önk.Hiv. Szini Kirendelts']$munkavegzes_helye <- 'Bódvaszilas'
#####
sum(kozig_allasok$munkavegzes_helye=='Nem_megadott')


setkey(kozig_allasok,munkavegzes_helye)
final_df <- telep_besorolassal[kozig_allasok]

write.csv(f, 'allasok_telep_adatokkal.csv', row.names = F)
# read back

write.xlsx(f, 'vege.xlsx', sheetName="Sheet1",  col.names=TRUE)

f$tanar<-
ifelse(
  grepl('tanár',tolower(f$allas_megnevezese)) | grepl('tanító',tolower(f$allas_megnevezese)) |
  grepl('pedagógus',tolower(f$allas_megnevezese)) | grepl('óvoda',tolower(f$allas_megnevezese)) |
  grepl('iskola',tolower(f$allas_megnevezese)) | grepl('óvodapedagógus',tolower(f$allas_megnevezese)) |
  grepl('gyógypedagógus',tolower(f$allas_megnevezese)) | grepl('pszichológus',tolower(f$allas_megnevezese)) |
  grepl('logopédus',tolower(f$allas_megnevezese)) ,
  1,0)



f$tankerulet_felelos<-
  ifelse(
    grepl('tankerület',tolower(f$kozzetevo)) | grepl('óvoda',tolower(f$kozzetevo)) |
      grepl('iskola',tolower(f$kozzetevo)) | grepl('szakképzés',tolower(f$kozzetevo)) |
      grepl('szakképzési',tolower(f$kozzetevo)) | grepl('szakképző',tolower(f$kozzetevo)),
    1,0)


f2<- f[,c(1:2,11,3, 12, 4:10)]

write.xlsx(f2, 'vege_2.xlsx', sheetName="Sheet1",  col.names=TRUE)
