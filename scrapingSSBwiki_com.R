rm(list=ls())

library(tidyverse)
library(data.table) #fread()
library(rvest) #scraping web pages

#To stay legally clean: First check the robot.txt

robots_url <- "https://www.ssbwiki.com/robots.txt"
robots.txt <- read_table("https://www.ssbwiki.com/robots.txt", col_names = F)
robots.txt <- rbind(robots.txt, paste0("Source: ",robots_url), paste(Sys.Date()))
write.csv(robots.txt, paste0("ssbwikitrobottxt", Sys.Date()))

# Writing Down Original Fighters Names:

fighter <- c("Mario","Luigi","Peach","Bowser","Dr. Mario",
             #"Rosalina & Luma", 
              "Bowser Jr.","Roy","Bowser", "Yoshi",
              "Donkey Kong","Diddy Kong","Zelda","Ganondorf",
              "Sheik","Young Link", "Toon Link", "Link",
              "Dark Samus", "Zero Suit Samus", "Samus", "Kirby","Meta Knight",
              "King Dedede", "Fox", 
             #"Captain Falcon", 
              "Falco", "Wolf", 
              "Pikachu", "Jigglypuff","Pichu","Mewtwo",
             #"Pokémon Trainer",
              "Lucario","Greninja","Ness",
              "Lucas","Ice Climbers","Marth","Roy",
              "Ike","Lucina","Robin","Corrin",
              "Mr. Game & Watch",
              "Palutena","Dark Pit", "Pit",
              "Wario","Olimar","R.O.B.","Villager",
              "Wii Fit Trainer", "Little Mac",
              "Shulk", "Duck Hunt",
              "Snake","Sonic","Mega Man",
              "Pac-Man",
              "Ryu", "Cloud", "Bayonetta",
              "Mii Brawler",
              "Mii Swordfighter", 
              "Mii Gunner",
              "Daisy", "Piranha Plant", 
              "King K. Rool", "Ridley",
              "Incineroar", 
             #"Chrom", 
              "Isabelle",
              "Inkling", "Ken", "Simon", "Richter",
              "Joker", "Hero", "Banjo & Kazooie")
length(fighter)
is.character(fighter)
is.factor(fighter)
write.table(fighter, "fighter_names.csv")

# Wanted Data Links:

fighter_prepare <- str_replace_all(string = fighter, pattern=" ", replacement="_")
fighter_prepare <- str_replace_all(string = fighter_prepare, pattern="&", replacement="%26")
dat <- cbind(fighter, paste0("https://www.ssbwiki.com/" ,fighter_prepare, "_(SSBU)"))
dat <- as.data.frame(dat)
#Umweg um Char aus Factor zu machen: (warum auch immer)
dat$fighter2 <- as.character(dat[,1])
dat$ssb_url2 <- as.character(dat[,2])
dat <- dat[,3:4]
colnames(dat) <- c("fighter","ssb_url")
str(dat)

#Inkorrekte und unregelmäßige Links korrigieren:
dat[fighter=="Captain Falcon","ssb_url"]<- "https://www.ssbwiki.com/Captain_Falcon_%28SSBU%29"



scrape_ssb <- function(url){
  webpage <- read_html(url)
  nodes <- html_table(html_nodes(webpage, ".wikitable"), fill=T)
  c1 <- nodes[[1]][[1]]
  c2 <- nodes[[1]][[2]]
  c3 <- nodes[[1]][[3]]
  c4 <- nodes[[1]][[4]]
  c5 <- nodes[[1]][[5]]
  dat_default <- cbind(c1,c2,c3,c4,c5)
}

#komischerweise funktioniert der folgende Befehl nicht mit: 
#"Rosalina & Luma",
#"Captain Falcon", 
#"Pokémon Trainer", 
#"Chrom",
dat1 <- NULL
for (i in 1:length(fighter)){
  dat1[[dat[i,1]]] <- scrape_ssb(dat[i,2])
}

data <- c(NA,NA,NA,NA,NA,NA)
for (i in 1:length(fighter)){
  data <- rbind(data, cbind(dat[i,1],as.data.frame(dat1[[dat[i,1]]])))
}

dat_long <- cbind(as.character(data[,1]),
                  as.character(data[,2]),
                  as.character(data[,4]),
                  as.character(data[,5]),
                  as.character(data[,6]))
colnames(dat_long) <- c("fighter","attack_type",
                        "attack_name","damage_raw","description")
write.csv(dat_long, file="ssb_attackdata.csv")
dat_long2 <- fread(file="ssb_attackdata.csv", header = T)


