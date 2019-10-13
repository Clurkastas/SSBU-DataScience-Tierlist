rm(list=ls())

library(tidyverse)
library(data.table) #fread()
library(rvest) #scraping web pages
library(stringr)
library(naniar)

#To stay legally clean: First check the robot.txt

url_robots <- "https://smashboards.com/robots.txt"
robots.txt <- read_table(url_robots, col_names = F)
robots.txt <- rbind(robots.txt, paste0("Source: ",url_robots), paste(Sys.Date()))
write.csv(robots.txt, paste0("smashboardsrobottxt", Sys.Date()))

# Wanted Data Links:

#get number of pages with data on it!
url_sb_basic <- read_html("https://smashboards.com/threads/ssbu-tournament-results.464865")
pageinfo <- html_nodes(url_sb_basic, css = '.pageNav-page a')
lastpage_url <- xml_attrs(pageinfo[[5]])[["href"]]
lp_url_split <- unlist(strsplit(lastpage_url, "-"))
lp_num <- as.numeric(lp_url_split[length(lp_url_split)])
pages_num <- NA
for (i in 1:lp_num){
  pages_num[i] <- i
}

#get data from all X pages
url_smashboards <- "https://smashboards.com/threads/ssbu-tournament-results.464865/page-"
urls_smashboards <- lapply(pages_num, function(x) paste0(url_smashboards,x))
web_list <- lapply(urls_smashboards, function(x) read_html(x))
## get data from htmls and then unlist the list of multiple vectors into one vector and turn it into a list again
rawdata_list <- lapply(web_list, function(x) html_nodes(x, css = ".bbWrapper"))
rawfighter_list <- lapply(rawdata_list,
                          function(x) lapply(x,
                                             function(y) as.character(y))) #Verschachtelung, um selbe anzahl an Listenelementen am Ende zu haben in beiden Listen
rawdata_list <- lapply(rawdata_list, function(x) html_text(x))
rawdata_list <- as.list(unlist(rawdata_list))
rawfighter_list <- as.list(unlist(rawfighter_list))


#remove everything except unnecessary data from workspace
rm(list = ls()[!ls() %in% c("rawdata_list","rawfighter_list")])

#HOW TO CLEAN THE STRING DATA?
#[1] extract "http://..." strings
tour_urls <- lapply(rawdata_list, FUN = function(x) str_extract_all(x, "http\\S+\\s*"))
sample(tour_urls)[1]
# Teil 1 fertig

#[2] extract results strings
tour_results <- lapply(rawfighter_list, FUN = function(x) str_extract_all(x, "[:digit:](\\.|st|nd|rd|th|\\:|[:space:])(.*?)\n"))
sample(tour_results)[1]
# es sind noch die Zeilen mit Turnierdaten drin, also nur mit Sprites behalten ("<img")
tour_results <- lapply(tour_results, 
                     function(x) lapply(x,
                                        function(y) grep('<img', y, value=TRUE)))
tour_results[[6]][[1]]
#dann einzelne Infos herausziehen?
#-- erste Nummer: Platzierung
tour_place <- lapply(tour_results, 
                     function(x) lapply(x,
                                        function(y) str_extract(y, "[:digit:]+(?=[^[:digit:]])")))
sample(tour_place)[1]

#-- Nach Platzierung bzw. erstes Leerzeichen bis nächstes leerzeichen: Spielername
tour_person <- lapply(tour_results, 
                     function(x) lapply(x,
                                        function(y) str_extract(y, "[:space:].{1,30}(?=([img]))")))
sample(tour_person)[1]
#----dann: alles vor "<" löschen
tour_person <- lapply(tour_person, 
                      function(x) lapply(x,
                                         function(y) str_extract(y, ".+?(?=<)")))
tour_person[[6]][[1]]

#-- zwischen 'title\"' und LEERZEICHEN: Fighter Name
tour_fighter <- lapply(tour_results, 
                             function(x) lapply(x,
                                                function(y) str_extract_all(y, "title(.*?)[:space:]")))
sample(tour_fighter)[1]
tour_fighter <- lapply(tour_fighter, 
                       function(x) lapply(x,
                                          function(y) str_replace_all(y, 'title',"")))
tour_fighter <- lapply(tour_fighter, 
                       function(x) lapply(x,
                                          function(y) str_replace_all(y, '([:punct:]|=)',"")))
tour_fighter[[6]][[1]]

#[3] extract "(...)" strings
tour_info <- lapply(rawdata_list, FUN = function(x) str_extract_all(x, "\\({1}(.*?)\\){1}"))
sample(tour_info)[1]
#--     :Teilnehmeranzahl
tour_participants <- lapply(tour_info, 
                                  function(x) lapply(x,
                                                       function(y)
                                                         str_extract_all(y, "\\d(.*?)[:space:]")))
sample(tour_participants)[1]
#--     :Ort des Turniers


#[4] extract tourn. name strings
tour_names <- lapply(rawdata_list, FUN = function(x) str_extract_all(x, "(.*?)\\([:digit:]"))
sample(tour_names)[1]
#dann vor "(" abschneiden
tour_names <- lapply(tour_names, 
                      function(x) lapply(x,
                         function(y)
                            str_extract(y, ".+?(?=\\()"))) #everything before first offene Klammer
sample(tour_names)[1]


##### Wie füge ich all diese Daten zusammen?
#1218 Einträge: mit 1-x Turniernamen,Spielerzahlen, Ort
# mit noch mehr Daten zu spielern und platzierung,
# und noch mehr Daten zu gespileten CHarakteren
figh <- unlist(tour_fighter)
name <- unlist(tour_names)
part <- unlist(tour_participants)
plac <- unlist(tour_place)
urls <- unlist(tour_urls)
pers <- unlist(tour_person)
xxx <- data.frame(A=pers,B=figh,C=plac)
megalist <- list(tour_names, tour_participants, tour_urls, 
                 tour_person, tour_fighter, tour_place)
ultradat <- data.frame()
for (i in 1:length(tour_names)) {
  ultradat[i,1] <- as.character(megalist[[1]][[i]])
  ultradat[i,2] <- as.character(megalist[[2]][[i]])
  ultradat[i,3] <- as.character(megalist[[3]][[i]])
  ultradat[i,4] <- as.character(megalist[[4]][[i]])
  ultradat[i,5] <- as.character(megalist[[5]][[i]])
  ultradat[i,6] <- as.character(megalist[[6]][[i]])
}
names(ultradat) <- c("tour","num_par","url","player","fighter","place")
# make NAs
ultradat <- ultradat %>%
  naniar::replace_with_na(replace = list(tour = c("character(0)", "list()","list(character(0))"),
                                         num_par = c("character(0)", "list()","list(character(0))"),
                                         url = c("character(0)", "list()","list(character(0))"),
                                         player = c("character(0)", "list()","list(character(0))"),
                                         fighter = c("character(0)", "list()","list(character(0))"),
                                         place = c("character(0)", "list()","list(character(0))")))
# Delete all complete NA lines
ultradat <- ultradat[rowSums( is.na(ultradat) ) <=1, ]
#make two datasets:
#1) single tournaments

#2) mutliple tournaments

#read in fighter names
fighter <- read.table("fighter_names.csv")
