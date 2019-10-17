rm(list=ls())

library(data.table) #fread()
library(rvest) #scraping web pages
library(stringr)
library(naniar)
library(tidyverse)

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
datedata_list <- lapply(web_list, function(x) html_nodes(x, css = ".message-attribution-main .u-dt"))
rawdata_list <- lapply(web_list, function(x) html_nodes(x, css = ".bbWrapper"))
rawfighter_list <- lapply(rawdata_list,
                          function(x) lapply(x,
                                             function(y) as.character(y))) #Verschachtelung, um selbe anzahl an Listenelementen am Ende zu haben in beiden Listen
rawdata_list <- lapply(rawdata_list, function(x) html_text(x))
rawdata_list <- as.list(unlist(rawdata_list))
rawfighter_list <- as.list(unlist(rawfighter_list))
datedata_list <- lapply(datedata_list, function(x) html_text(x))
datedata_list <- as.list(unlist(datedata_list))

#remove everything except unnecessary data from workspace
rm(list = ls()[!ls() %in% c("rawdata_list","rawfighter_list","datedata_list")])

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
names(megalist) <- c("names", "participants", "urls", 
                     "persons", "fighters", "placements")

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
basics3$fighter[basics3$fighter=="Mr Game amp Watch"] <- "Mr. Game & Watch"
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
basics3$fighter[basics3$fighter=="Robin Female"] <- "Robin"
basics3$fighter[basics3$fighter=="Corrin Female"] <- "Corrin"
basics3$fighter[basics3$fighter=="Inkling Boy"] <- "Inkling"
basics3$fighter[basics3$fighter=="haracter0"] <- NA
basics3$fighter[basics3$fighter=="Wii Fit Trainer Male"] <- "Wii Fit Trainer"
basics3$fighter[basics3$fighter=="Substitute"] <- NA
basics3$fighter[basics3$fighter=="Mii Fighters"] <- NA
basics3$fighter[basics3$fighter=="Dragon Quest Hero"] <- "Hero"
basics3$fighter[basics3$fighter=="ROB"] <- "R.O.B."
basics3$fighter[basics3$fighter=="Richter Belmont"] <- "Richter"
basics3$fighter[basics3$fighter=="Simon Belmont"] <- "Simon"
basics3$fighter[basics3$fighter=="PacMan"] <- "Pac-Man"
basics3$fighter[basics3$fighter=="King K Rool"] <- "King K. Rool"
basics3$fighter[basics3$fighter=="Dr Mario"] <- "Dr. Mario"
basics3$fighter[basics3$fighter=="Bowser Jr"] <- "Bowser Jr."

#sort out NA
basics3 <- basics3[is.na(basics3$fighter)==F,]

write.csv(basics3, "smashboards_tournaments1.csv")


# make functions to map on each "tour_..."-list
minusEbene <- as_mapper(~.x[[1]])
megalist2 <- map(megalist, ~map(.x, minusEbene))
megalist2$participants <- map(megalist2$participant, ~as.character(.x))
megalist2$fighters <- map(megalist2$fighters, ~str_replace_all(.x,"^c",""))
megalist2$fighters <- map(megalist2$fighters, ~str_replace_all(.x,"\\+",""))
#das ist gut!

#welche listen sind gleich lang (je elemente)
testx <- map_df(megalist2, ~map_dbl(.x, length))
summary(ifelse(testx$fighters==testx$persons,T,F)) #exakt gleich
summary(ifelse(testx$fighters==testx$placements,T,F)) #exakt gleich
summary(ifelse(testx$urls==testx$names,T,F)) #größtenteils gleich..

#wie bekomme ich daraus jetzt Datensätze die alle gleich lang sind?
matchit_Df <- map(megalist2, ~map_df(.x, as.character))

matchit_Df <- pmap(megalist2[4:6], ~rbind(..1,..2,..3))
matchit_Df <- map(matchit_Df,~t(data.frame(matrix(unlist(.x), nrow=3))))
matchit_Df2 <- map2(matchit_Df,1:length(matchit_Df), ~cbind(.x,.y)) #for: entry_nr
matchit_Df2 <- map(matchit_Df2, ~(data.frame(matrix(unlist(.x), ncol=4))))
matchit_Df3 <- map_df(matchit_Df2, ~.x)
names(matchit_Df3) <- c("player","characters","placement","entry_nr")

### Andere Daten aufbereiten :)
#tournament names 
matchit_names2 <- map2(megalist2[[1]],1:length(megalist2[[1]]), ~cbind(.x,.y)) #for: entry_nr
matchit_names2[[40]]
matchit_names3 <- map(matchit_names2, ~(data.frame(matrix(unlist(.x), ncol=2))))
matchit_names4 <- map_df(matchit_names3, ~.x)
names(matchit_names4) <- c("tournament","entry_nr")
#tournament names 
matchit_participant2 <- map2(megalist2[[2]],1:length(megalist2[[2]]), ~cbind(.x,.y)) #for: entry_nr
matchit_participant2[[40]]
matchit_participant3 <- map(matchit_participant2, ~(data.frame(matrix(unlist(.x), ncol=2))))
matchit_participant4 <- map_df(matchit_participant3, ~.x)
names(matchit_participant4) <- c("participants","entry_nr")
#tournament names 
matchit_url2 <- map2(megalist2[[3]],1:length(megalist2[[3]]), ~cbind(.x,.y)) #for: entry_nr
matchit_url2[[40]]
matchit_url3 <- map(matchit_url2, ~(data.frame(matrix(unlist(.x), ncol=2))))
matchit_url4 <- map_df(matchit_url3, ~.x)
names(matchit_url4) <- c("url","entry_nr")

megalist3 <- list(matchit_Df3, matchit_names4, matchit_participant4, matchit_url4)

#remove everything except unnecessary data from workspace
rm(list = ls()[!ls() %in% c("megalist3","datedata_list")])

# Fighter-Data
d1 <- as.data.frame(megalist3[1])
#already clean enough

# tournament names
d2 <- as.data.frame(megalist3[2])
vec_temp <- (d2[,1]==d2[,2])#
d2 <- d2[!vec_temp,] 
d2 <- d2[!is.na(d2[,1]),] #not keep NA

# participants nr
d3 <- as.data.frame(megalist3[3])
vec_temp <- str_detect(d3[,1],"character")
vec_temp[is.na(vec_temp)] <- TRUE
d3 <- d3[!vec_temp,] #only keep entries with information

# url
d4 <- as.data.frame(megalist3[4])
vec_temp <- str_detect(d4[,1],"^http")
vec_temp[is.na(vec_temp)] <- TRUE
d4 <- d4[vec_temp,] #only keep entries with information (with "http..." at the beginning)
d4 <- d4[!is.na(d4[,1]),] #not keep NA

#date
d5 <- unlist(datedata_list)
d5 <- cbind(d5, as.character(1:length(d5)))
d5 <- as.data.frame(d5)
names(d5) <- c("entry_date","entry_nr")


# now: run sth on them

entry_id <- function(dat, entry_nr=entry_nr){
  dat %>% group_by(entry_nr) %>% mutate(entry_id = paste0(entry_nr,"-",row_number())) %>% ungroup()
}
d2 <- d2 %>% entry_id()
d3 <- d3 %>% entry_id()
d4 <- d4 %>% entry_id()
d5 <- d5 %>% entry_id()

# für d1 muss ich nun auch diese "entry_id" erstellen
# das wird aber etwas schwieriger, weil die subgruppierung anders funktioniert:
# jedesmal wenn die Platzierung neu bei 1 anfängt
d1$first <- d1$placement==1
d1$seperatelistings <- cumsum(d1$first)

d1_help <- d1 %>% 
  group_by(entry_nr, seperatelistings) %>% 
  summarise(entry_id = "1")
d1_help <- d1_help %>% 
  group_by(entry_nr) %>%
  mutate(entry_id = paste0(entry_nr,"-",row_number())) %>%
  ungroup()

d1 <- left_join(d1,d1_help, by=c("entry_nr","seperatelistings"))

d5$entry_nr <- as.character(1:length(d5$entry_nr))
# now all the dataframes are ready to be joined together.

dat <- d1 %>% 
  full_join(d5) %>% #this one by nr because date is per total entry
  full_join(d2, by="entry_id") %>%
  full_join(d3, by="entry_id") %>%
  full_join(d4, by="entry_id") 

# everything merged as good as it can be.
#clean the rest
dat <- dat[,c("entry_nr.x",
              "entry_id",
              "entry_date",
              "tournament",
              "url",
              "player",
              "characters",
              "placement")] #keep useful columns

dat <- dat[dat$entry_id!="1-1",] #delete first entry, it's a pattern from the admin to use
dat$entry_date <- lubridate::mdy(dat$entry_date)

# unnütz entfernen 2:
dat <- dat[is.na(dat$placement)==F,]

#some dates are not transfered (only for -1 and not -2* classes)

dat_help <- dat %>% group_by(entry_nr.x, entry_date) %>% summarise()
dat_help <- dat_help[is.na(dat_help$entry_date)==F,]
dat <- full_join(dat[,c(1:2,4:8)], dat_help)
names(dat) <- c("entry_nr", "entry_id", "tournament", 
  "url", "player", "characters", "placement", "entry_date")
dat <- dat[,c("entry_nr", "entry_date", "entry_id", "tournament", 
              "url", "player", "characters", "placement")]

dat$characters <- str_replace_all(dat$characters, "Rosalina amp Luma","Rosalina & Luma")
dat$characters <- str_replace_all(dat$characters, "Alph", "Olimar")
dat$characters <- str_replace_all(dat$characters, "Banjo amp Kazooie", "Banjo & Kazooie")
dat$characters <- str_replace_all(dat$characters, "Mr Game amp Watch", "Mr. Game & Watch")
dat$characters <- str_replace_all(dat$characters, "Happy Sheep", "")
dat$characters <- str_replace_all(dat$characters, "Ludwig", "Bowser Jr")
dat$characters <- str_replace_all(dat$characters, "Morton", "Bowser Jr")
dat$characters <- str_replace_all(dat$characters, "Larry", "Bowser Jr")
dat$characters <- str_replace_all(dat$characters, "Lemmy", "Bowser Jr")
dat$characters <- str_replace_all(dat$characters, "Iggy", "Bowser Jr")
dat$characters <- str_replace_all(dat$characters, "Wendy", "Bowser Jr")
dat$characters <- str_replace_all(dat$characters, "Bayonetta Alt", "Bayonetta")
dat$characters <- str_replace_all(dat$characters, "Pokemon Trainer Female", "Pokemon Trainer")
dat$characters <- str_replace_all(dat$characters, "Villager Female", "Villager")
dat$characters <- str_replace_all(dat$characters, "Robin Female", "Robin")
dat$characters <- str_replace_all(dat$characters, "Corrin Female", "Corrin")
dat$characters <- str_replace_all(dat$characters, "Inkling Boy", "Inkling")
dat$characters <- str_replace_all(dat$characters, "haracter0", "")
dat$characters <- str_replace_all(dat$characters, "Wii Fit Trainer Male", "Wii Fit Trainer")
dat$characters <- str_replace_all(dat$characters, "Substitute", "")
dat$characters <- str_replace_all(dat$characters, "Mii Fighters", "")
dat$characters <- str_replace_all(dat$characters, "Dragon Quest Hero", "Hero")
dat$characters <- str_replace_all(dat$characters, "ROB", "R.O.B.")
dat$characters <- str_replace_all(dat$characters, "Richter Belmont", "Richter")
dat$characters <- str_replace_all(dat$characters, "Simon Belmont", "Simon")
dat$characters <- str_replace_all(dat$characters, "PacMan", "Pac-Man")
dat$characters <- str_replace_all(dat$characters, "King K Rool", "King K. Rool")
dat$characters <- str_replace_all(dat$characters, "Dr Mario", "Dr. Mario")
dat$characters <- str_replace_all(dat$characters, "Bowser Jr", "Bowser Jr.")

#more details on fighters!
char_names <- data.table::fread("fighter_names.csv")[,2] %>% unlist()
char_names[char_names=="Pokémon Trainer"] <- "Pokemon Trainer"

x<-length(dat)
dat <- cbind(dat,data.frame(matrix(0, ncol=length(char_names))))
for(i in char_names){
  print(i)
  x <- x+1
  dat[,x][str_detect(dat$characters,i)] <- i
  dat$characters <- str_replace_all(dat$characters, i, "")
}
names(dat) <- c("entry_nr", "entry_date", "entry_id", "tournament", 
                                          "url", "player", "characters", "placement",char_names)

#almost there
dat$nr <- as.factor(1:length(dat$entry_nr))
dat$entry_nr <- as.numeric(dat$entry_nr)
dat$placement <- as.numeric(dat$placement)
data_long <- gather(dat, nr, character, `Dr. Mario`:`Banjo & Kazooie`, factor_key=TRUE)
data_long <- data_long[data_long$character!=0,c(1:6,8,10)]
data_long <- data_long[data_long$placement!=0,]

summary(data_long)

#Include Patch Data!

patch_url <- "https://www.ssbwiki.com/List_of_updates_%28SSBU%29"
patch_nr <- html_nodes(read_html(patch_url), "h3") 
patch_nr <- patch_nr[2:(length(patch_nr)-6)]
patches <- map(patch_nr, ~xml_attrs(xml_child(.x,1)))
patches <- map_chr(patches, ~.x["id"])

patch_date <- html_nodes(read_html(patch_url), "p") 
patch_date <- html_text(patch_date)
patch_date <- patch_date[3:length(patch_date)]

patch_dat <- as.data.frame(cbind(patches, patch_date))
names(patch_dat) <- c("patch","date")
patch_dat$date <- str_replace_all(patch_dat$date,"This update was released ","")
patch_dat$date <- str_replace_all(patch_dat$date,"\\(.*","") #delete everything after a bracket
patch_dat$date <- str_replace_all(patch_dat$date,"at.*","") #delete everything after "at"
patch_dat$date <- lubridate::mdy(patch_dat$date)
today <- lubridate::ymd(Sys.Date())

v_inter <- lubridate::interval("2017-01-01","2017-01-05")
for (i in 1:length(patch_dat$date)){
  inter <- lubridate::interval(patch_dat$date[i], patch_dat$date[i+1])
  v_inter <- c(v_inter, inter)
}

patch_dat <- rbind(patch_dat, c("today",as.character(today)))
v_inter <- c(v_inter[2:length(v_inter)],interval(patch_dat$date[length(patch_dat$date)],today))

fff <- list(patch_dat, v_inter)

library(lubridate)
data_long$patch <- as.character("newest")

str(fff)

fff[[1]][["patch"]]
fff[[1]][["date"]]
fff[[2]]
patch <- as.character(patch_dat$patch)
str(patch)
for (i in 1:length(patch)){
  data_long$patch[data_long$entry_date %within% v_inter[i]]  <- patch[i]
}
newest_patch <- patch[(length(patch)-1)]
data_long$patch[data_long$patch=="newest"] <- newest_patch

write.csv(data_long, paste0("smashboards_long",today,".csv"))
