library(httr)
library(stringi)
library(rvest)
library(XML)
library(jsonlite)
library(data.table)

my_clear <- function(szoveg){
  tolower(gsub('.','_',gsub(':','',gsub(' ','_',iconv(szoveg,from="UTF-8",to="ASCII//TRANSLIT"))), fixed=T))
}

eredmy_szoveg_clean <- function(szoveg) {
  return(trimws(gsub(' +',' ',gsub('\r\n', '', szoveg, fixed = T))))
}

get_page<- function(i){
  
  
  r <- POST(paste0("https://kozigallas.gov.hu/publicsearch.aspx?p=",i), body = list('p'=i))
  adat <- read_html(content(r, 'text' ))
  write_html(adat, 'tmp.html')
  output<-html_nodes(adat, ".joblist table")
  k <- output %>%
    html_table()
  my_df <- data.table(data.frame(k[1]))
  names(my_df)<- my_clear(as.character(unname(unlist(my_df[1,]))))
  my_df<- my_df[-1,]
  # my_df[munkavegzes_helye=='']$munkavegzes_helye<- 'NA , NA'
  # my_df[munkavegzes_helye=='Buidapest']$munkavegzes_helye<- 'Budapest'
  # my_df[munkavegzes_helye=='Budapest XX. kerület']$munkavegzes_helye<- 'Budapest'
  # my_df$munkavegzes_helye<- ifelse(grepl(',',my_df$munkavegzes_helye), my_df$munkavegzes_helye, paste0(my_df$munkavegzes_helye, ','))
  # my_df[munkavegzes_helye=='Pilisjászfalu,']$munkavegzes_helye<- 'NA , Pilisjászfalu'
  my_df$allas_megnevezese <- sapply(strsplit(my_df$allas_megnevezese, '\r', fixed = T), '[[', 1)
  #my_df$megye <- trimws(sapply(strsplit(gsub('\r\n','', my_df$munkavegzes_helye), ',', fixed = T), '[[', 1))
  #my_df$munkavegzes_helye <- trimws(sapply(strsplit(gsub('\r\n','', ifelse(my_df$munkavegzes_helye=='Budapest,', 'Budapest, Budapest', my_df$munkavegzes_helye)), ',', fixed = T), '[[', 2))
  my_df <- my_df[,-'X7']
  
  linkek <- output%>%
    html_nodes('a')%>%
    html_attr('href')
  id <- sapply(strsplit(linkek, "'", fixed = T),"[[", 2)
  my_df$id <- id 
  

  return(my_df)
  
}





r <- GET("https://kozigallas.gov.hu/publicsearch.aspx?")
my_list <- list()
i<- 1
while(TRUE){
  print(i)
  t<- get_page(i)
  my_list[[i]]<- t
  i<- i+1
  if(nrow(t)!=10){
    break
  }
}



final_adat <- rbindlist(my_list)

write.csv(final_adat, 'eredmeny.csv', row.names =F)

print('készen vagyok')


a <-fread('eredmeny.csv')



