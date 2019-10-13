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

#remove entrys that are citations
citations <- str_which(rawdata_list, "[:space:]said.\n")
for (i in citations){
  rawdata_list[[i]] <- NA
  rawfighter_list[[i]] <- NA
}


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
                                                function(y) str_extract_all(y, "title(.*?)[:space:]{3}")))
sample(tour_fighter)[1]
tour_fighter <- lapply(tour_fighter, 
                       function(x) lapply(x,
                                          function(y) str_replace_all(y, 'title',"")))
tour_fighter <- lapply(tour_fighter, 
                       function(x) lapply(x,
                                          function(y) str_replace_all(y, '([:punct:]|=)',"")))
tour_fighter <- lapply(tour_fighter, 
                       function(x) lapply(x,
                                          function(y) str_replace_all(y, '[:space:]{3}',"+")))
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
basics <- data.frame(A=pers,B=figh,C=plac)
megalist <- list(tour_names, tour_participants, tour_urls, 
                 tour_person, tour_fighter, tour_place)

# make "basics" useable:
split_into_multiple <- function(column, pattern = ", ", into_prefix){
  cols <- str_split_fixed(column, pattern, n = Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as.tibble(cols)
  # name the 'cols' tibble as 'into_prefix_1', 'into_prefix_2', ..., 'into_prefix_m' 
  # where m = # columns of 'cols'
  m <- dim(cols)[2]
  
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}
basics <- basics[-c(1:8),] # erste 8 Lines löschen weil: Muster
basics2 <- basics %>% 
  bind_cols(split_into_multiple(.$B, "\\+ ", "fighter")) %>% 
  # selecting those that start with 'type_' will remove the original 'type' column
  select(c("A","C"), starts_with("fighter_"))
basics3 <- basics2 %>% 
  gather(key, fighter, -c(A,C), na.rm = T)
names(basics3) <- c("player","placement","key","fighter")
basics3$fighter <- str_replace(basics3$fighter, 
            pattern = "^c", #pattern = a leading small "c"
            replacement = "")
basics3$fighter <- str_replace(basics3$fighter, 
                               pattern = "\\+$", #pattern = a "+" at the end
                               replacement = "")

#Corrections:
basics3$fighter[basics3$fighter=="Rosalina amp Luma"] <- "Rosalina & Luma"
basics3$fighter[basics3$fighter=="Alph"] <- "Olimar"
basics3$fighter[basics3$fighter=="Banjo amp Kazooie"] <- "Banjo & Kazooie"
basics3$fighter[basics3$fighter=="Mr Game amp Watch"] <- "Mr Game & Watch"
basics3$fighter[basics3$fighter=="Happy Sheep"] <- NA
basics3$fighter[basics3$fighter=="Ludwig"] <- "Bowser Jr"
basics3$fighter[basics3$fighter=="Morton"] <- "Bowser Jr"
basics3$fighter[basics3$fighter=="Larry"] <- "Bowser Jr"
basics3$fighter[basics3$fighter=="Lemmy"] <- "Bowser Jr"
basics3$fighter[basics3$fighter=="Iggy"] <- "Bowser Jr"
basics3$fighter[basics3$fighter=="Wendy"] <- "Bowser Jr"
basics3$fighter[basics3$fighter=="Bayonetta Alt"] <- "Bayonetta"
basics3$fighter[basics3$fighter=="Pokemon Trainer Female"] <- "Pokemon Trainer"
basics3$fighter[basics3$fighter=="Villager Female"] <- "Villager"
basics3$fighter[basics3$fighter=="Corrin Female"] <- "Corrin"
basics3$fighter[basics3$fighter=="Inkling Boy"] <- "Inkling"
basics3$fighter[basics3$fighter=="haracter0"] <- NA
basics3$fighter[basics3$fighter=="Wii Fit Trainer Male"] <- "Wii Fit Trainer"
basics3$fighter[basics3$fighter=="Substitute"] <- NA
basics3$fighter[basics3$fighter=="Mii Fighters"] <- NA

#sort out NA
basics3 <- basics3[is.na(basics3$fighter)==F,]


# how often does eat character get mentioned?
n_fighter <- basics3 %>%
  group_by(fighter) %>%
  summarise(count = n()) %>%
  arrange(count)
n_fighter$fighter <- factor(n_fighter$fighter, levels = n_fighter$fighter[order(n_fighter$count)])
n_fighter %>%
  ggplot(aes(y=fighter, x=count)) +
  geom_point()

n_fighter2 <- basics3 %>%
  group_by(fighter, placement) %>%
  summarise(count = n()) %>%
  arrange(count)
n_fighter2$placement <- as.numeric(n_fighter2$placement)
n_fighter2 <- n_fighter2[n_fighter2$placement<4,]
n_fighter2 %>%
  ggplot(aes(y=fighter, x=count)) +
  facet_wrap(~placement) +
  geom_point()



