rm(list=ls())

library(tidyverse)
library(data.table) #fread()
library(rvest) #scraping web pages
library(stringr)

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
tour_results <- lapply(rawfighter_list, FUN = function(x) str_extract_all(x, "[:digit:](\\.|st|nd|rd|th|\\:)(.*?)\n"))
sample(tour_results)[1]
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



#read in fighter names
fighter <- read.table("fighter_names.csv")
