rm(list=ls())

# Required for Accessing API's (HTTP or HTTPS URL's from Web)
library(httr)
# Convert json/text in lists to data frame
library(rlist)
# Additional functions to convert json/text to data frame
library(jsonlite)
# Manipulate data
library(dplyr)
#harvesting unstructured data
library(rvest)
#CHROME ADDON TO SCRAPE: 
#SelectorGadget
#tidyverse
library(tidyverse)
#ggplot2
library(ggplot2)
#bilder/ Icons als Plotpunkte im Graph verwenden:
library(ggimage)

url_endings <- c("AirAcceleration","AirSpeed","FallSpeed", 
                 "DashSpeed", "RunSpeed",
                 "WalkSpeed", "Weight")
urls <- paste0("http://kuroganehammer.com/Ultimate/",url_endings)
kuro_scratch <- function(url){
  webpage <- read_html(url)
  table <- html_table(html_nodes(webpage, "#AutoNumber1"))
  dat <- table[[length(table)]]
  dat
}



datlist <- lapply(urls, FUN=kuro_scratch)
dat_ssbu <- as.data.frame(datlist[[1]])
dat_ssbu <- full_join(dat_ssbu,as.data.frame(datlist[[2]]),by="CHARACTER")
dat_ssbu <- full_join(dat_ssbu,as.data.frame(datlist[[3]]),by="CHARACTER")
dat_ssbu <- full_join(dat_ssbu,as.data.frame(datlist[[4]]),by="CHARACTER")
dat_ssbu <- full_join(dat_ssbu,as.data.frame(datlist[[5]]),by="CHARACTER")
dat_ssbu <- full_join(dat_ssbu,as.data.frame(datlist[[6]]),by="CHARACTER")
dat_ssbu <- full_join(dat_ssbu,as.data.frame(datlist[[7]]),by="CHARACTER")

dat_ssbu <- dat_ssbu[, -grep('RANK', names(dat_ssbu))]

### Data-Cleaning ############

#nach name Sortieren
dat_ssbu <- dat_ssbu[order(dat_ssbu$CHARACTER),]
#Ganz leere Zeilen raus
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="",]
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="Pit, but edgy",]
#Schreibfehler M. Game & Watch korrigieren
dat_ssbu[dat_ssbu$CHARACTER=="Mr. Game & Watch", 12] <- dat_ssbu[dat_ssbu$CHARACTER=="M. Game & Watch", 12]
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="M. Game & Watch",]
#Schreibfehler Dank Samus korrigieren
dat_ssbu[dat_ssbu$CHARACTER=="Dark Samus", 10] <- dat_ssbu[dat_ssbu$CHARACTER=="Dank Samus", 10]
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="Dank Samus",]
#Schreibfehler Dank Samus korrigieren
dat_ssbu[dat_ssbu$CHARACTER=="Mii Swordfighter", 10] <- dat_ssbu[dat_ssbu$CHARACTER=="Mii Swordspider", 10]
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="Mii Swordspider",]
#Schreibfehler Dedede korrigieren
dat_ssbu[dat_ssbu$CHARACTER=="King Dedede", 2:4] <- dat_ssbu[dat_ssbu$CHARACTER=="Dedede", 2:4]
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="Dedede",]
#Schreibfehler Dr Mario/ Educated Mario korrigieren
dat_ssbu[dat_ssbu$CHARACTER=="Dr. Mario", 2:4] <- dat_ssbu[dat_ssbu$CHARACTER=="Dr Mario", 2:4]
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="Dr Mario",]
dat_ssbu[dat_ssbu$CHARACTER=="Dr. Mario", 12] <- dat_ssbu[dat_ssbu$CHARACTER=="Educated Mario", 12]
dat_ssbu <- dat_ssbu[dat_ssbu$CHARACTER!="Educated Mario",]
#Inkorrekte Namen anpassen:
dat_ssbu[dat_ssbu$CHARACTER=="Rosalina","CHARACTER"] <- "Rosalina and Luma"
dat_ssbu[dat_ssbu$CHARACTER=="Bowser Jr","CHARACTER"] <- "Bowser Jr."
dat_ssbu[dat_ssbu$CHARACTER=="Mr. Game & Watch","CHARACTER"] <- "Mr. Game and Watch"
dat_ssbu[dat_ssbu$CHARACTER=="PAC-MAN","CHARACTER"] <- "Pac-Man"


#renaming
names <- c("character","acceleration_max","acceleration_base",
           "acceleration_total","airspeed_max","fallspeed_max",
           "fallspeed_fast","speed_increase","dash_initial",
           "runspeed_max","walkspeed_max","weight")
names(dat_ssbu) <- names

### GGIMAGE einsetzen:
# SSBU Icons als Plotpunkte:

icon_url <- "https://www.serebii.net/smashbrosultimate/characters.shtml"
webpage <- read_html(icon_url)
table <- html_nodes(webpage, ".cen img")
length(table)
x <- 0
for (i in 1:length(table)){
  if(x==0){url_ending <- NULL
          x <- 1}
  url_ending <- c(url_ending, xml_attrs(table[[i]])[["src"]])
  }
url_start <- "https://www.serebii.net/smashbrosultimate/"
ssbu_link <- (paste0(url_start,url_ending))
ssbu_name <- html_text(html_nodes(read_html("https://www.serebii.net/smashbrosultimate/characters.shtml"),
               "u"))
ssbu_number <- html_text(html_nodes(read_html("https://gamewith.net/smashbros-ultimate/article/show/2147"),
                                     "#tabledata td:nth-child(1)"))
ssbu_info <- cbind(ssbu_number,ssbu_name,ssbu_link)
ssbu_info <- as.data.frame(ssbu_info)
colnames(ssbu_info) <- c("number","character","picture")

#Match Picture Dataset to Dataset of Analysis


dat_ssbu2 <- full_join(ssbu_info, dat_ssbu, by = "character")
write.csv(dat_ssbu2, "ssbu_basestats_icons.csv")

### Exploration #######
cordat <- cor(dat_ssbu[c(2:7,9:12)], method = c("pearson"), use="pairwise.complete.obs")
?cor()

dat_ssbu2 %>%
  ggplot(aes(x=weight,y=walkspeed_max)) +
  geom_jitter() +
  geom_smooth(method="lm")+
  geom_image(aes(image=picture))

dat_ssbu2 %>%
  ggplot(aes(x=weight,y=fallspeed_max)) +
  geom_jitter() +
  geom_smooth(method="lm")+
  geom_image(aes(image=picture))

dat_ssbu2 %>%
  ggplot(aes(x=fallspeed_max,y=dash_initial)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  geom_image(aes(image=picture))

# jetzt hätte ich noch gerne Tierlists als AV!
# taken from: https://www.proguides.com/super-smash-bros-ultimate/characters/
# von 1 (Tier E) bis 6 (Tier S)

tier_s <- as.data.frame(cbind(c("Fox","Pikachu","Daisy","Pichu","Peach","Lucina",
                  "Wolf","Palutena","Inkling","Joker"),6))
colnames(tier_s) <- c("character","tier")
tier_s$tier <- as.numeric(6)
tier_a <- as.data.frame(cbind(c("Mario","Yoshi","Marth","Roy","Chrom","Zero Suit Samus",
                  "Wario","Ike","Pokemon Trainer","Olimar",
                  "R.O.B.","Mega Man","Greninja","Pac-Man",
                  "Shulk","Ken"),5))
colnames(tier_a) <- c("character","tier")
tier_a$tier <- as.numeric(5)
tier_b <- as.data.frame(cbind(c("Link","Luigi","Ness","Captain Falcon",
                  "Bowser","Falco","Young Link","Mr. Game and Watch",
                  "Meta Knight","Diddy Kong","Lucario","Wii Fit Trainer",
                  "Ryu","Hero","Banjo and Kazooie"),4))
colnames(tier_b) <- c("character","tier")
tier_b$tier <- as.numeric(4)
tier_c <- as.data.frame(cbind(c("Ice Climbers","Sheik","Zelda","Ganondorf",
                  "Mewtwo","Lucas","Sonic","King Dedede",
                  "Toon Link","Rosalina and Luma","Duck Hunt","Cloud",
                  "Bayonetta","Simon","Richter","Mii Swordfighter"),3))
colnames(tier_c) <- c("character","tier")
tier_c$tier <- as.numeric(3)
tier_d <- as.data.frame(cbind(c("Donkey Kong","Samus","Dark Samus","Dr. Mario",
                  "Pit","Dark Pit","Villager","Robin",
                  "Corrin","Ridley","King K. Rool","Incineroar",
                  "Mii Brawler","Mii Gunner"),2))
colnames(tier_d) <- c("character","tier")
tier_d$tier <- as.numeric(2)
tier_e <- as.data.frame(cbind(c("Kirby","Jigglypuff","Little Mac",
                  "Bowser Jr.","Isabelle","Piranha Plant"),1))
colnames(tier_e) <- c("character","tier")
tier_e$tier <- as.numeric(1)

tier <- tier_s %>%
  full_join(tier_a, by="character")%>%
  full_join(tier_b, by="character")%>%
  full_join(tier_c, by="character")%>%
  full_join(tier_d, by="character")%>%
  full_join(tier_e, by="character")

for (i in 2:length(colnames(tier))){
  tier[is.na(tier[,i])==T,i] <- 0
}

tier$tier <- tier[,2] +tier[,3] +tier[,4] +tier[,5] +tier[,6] +tier[,7]
tier <- tier[,c(1,8)]
dat_ssbu3 <- dat_ssbu2 %>%
  full_join(tier,by="character")

#lineare Regression auf Tierlist ########

lm1 <- lm(data=dat_ssbu3, tier ~  fallspeed_max +
                          dash_initial)
sum_lm1 <- summary(lm1)
coeff_lm1 <- round(sum_lm1[["coefficients"]],2)

#dash initial am häufigsten signifikant #wieauchimmer

dat_ssbu3 %>%
  ggplot(aes(x=tier,y=dash_initial)) +
  geom_jitter() +
  geom_smooth() +
  geom_image(aes(image=picture))

dat_ssbu3$tier_dich <- 0
dat_ssbu3[is.na(dat_ssbu3$tier),"tier"] <- 0
dat_ssbu3[dat_ssbu3$tier>3,"tier_dich"] <- 1
dat_ssbu3[dat_ssbu3$tier==0,"tier_dich"] <- NA
dat_ssbu3[dat_ssbu3$tier==0,"tier"] <- NA

dat_ssbu3 %>%
  ggplot(aes(x=airspeed_max,y=dash_initial)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  geom_image(aes(image=picture))

lm1 <- lm(data=dat_ssbu3, tier_dich ~  fallspeed_max +
            dash_initial + airspeed_max + runspeed_max)
sum_lm1 <- summary(lm1)
coeff_lm1 <- round(sum_lm1[["coefficients"]],2)
