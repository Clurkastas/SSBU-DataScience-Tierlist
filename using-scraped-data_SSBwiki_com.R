rm(list=ls())

#load packages
library(tidyverse)
library(data.table) #fread()
library(rvest) #scraping web pages
library(stringr)

#functions
#erste nummer entziehen aus String
numextract <- function(string){ 
  as.numeric(str_extract(string, "\\-*\\d+\\.*\\d*"))
} 

#get data from other script
dat_long <- fread(file="ssb_attackdata.csv", header = T)
#important information:
#"Rosalina & Luma",
#"Captain Falcon", 
#"Pokémon Trainer" &
#"Chrom" could not be scraped!


#clean damage_raw:
dat_long$damage_raw <- str_replace_all(dat_long$damage_raw,
                                       pattern = "%",
                                       replacement = "")
dat_long$damage_some <- as.numeric(dat_long$damage_raw)

new_dam <- c("damage_raw1","damage_raw2",
"damage_raw3","damage_raw4",
"damage_raw5","damage_raw6",
"damage_raw7","damage_raw8",
"damage_raw9")
dat_long2 <- dat_long %>% 
  separate( damage_raw, into=new_dam, sep = "\\),")

dat_long2$damage_raw1 <- numextract(dat_long2$damage_raw1)
dat_long2$damage_raw2 <- numextract(dat_long2$damage_raw2)
dat_long2$damage_raw3 <- numextract(dat_long2$damage_raw3)
dat_long2$damage_raw4 <- numextract(dat_long2$damage_raw4)
dat_long2$damage_raw5 <- numextract(dat_long2$damage_raw5)
dat_long2$damage_raw6 <- numextract(dat_long2$damage_raw6)
dat_long2$damage_raw7 <- numextract(dat_long2$damage_raw7)
dat_long2$damage_raw8 <- numextract(dat_long2$damage_raw8)
dat_long2$damage_raw9 <- numextract(dat_long2$damage_raw9)

#Durchschnitt und SUmme von Multihits berechnen zum weiterverwenden
dat_long3 <- dat_long2 %>% 
  rowwise() %>% 
  mutate(attack_sum=sum(c(damage_raw1,damage_raw2,damage_raw3,
                       damage_raw4,damage_raw5,damage_raw6,
                       damage_raw7,damage_raw8,damage_raw9), na.rm = T),
         attack_mean=mean(c(damage_raw1,damage_raw2,damage_raw3,
                      damage_raw4,damage_raw5,damage_raw6,
                      damage_raw7,damage_raw8,damage_raw9), na.rm = T))
dat_long4 <- dat_long3[is.na(dat_long3$fighter)==F,c("V1","fighter","attack_type","attack_name",
                         "description","attack_sum","attack_mean")]

data_wide <- reshape2::dcast(dat_long4,
                             fighter  ~ attack_type,
                             value.var="attack_mean",
                             fun.aggregate = mean)


# wir haben also von grund auf schon ein paar gute Daten :)
useful_dat_long <- dat_long4[dat_long4$attack_type!="Edge attack"&
                              dat_long4$attack_type!="Floor attack (back)"&
                              dat_long4$attack_type!="Floor attack (front)"&
                              dat_long4$attack_type!="Floor attack (trip)"&
                              dat_long4$attack_type!="Pummel"&
                              dat_long4$attack_type!="Final Smash",]
useful_dat_long2 <- dat_long4[dat_long4$attack_type!="Final Smash",]

useful_dat_long2 %>%
  ggplot(aes(x=attack_mean, col=attack_type), alpha=0.5) +
  facet_wrap(attack_type~.) +
  #geom_jitter(width = 0.5, height = 0.1) +
  geom_density() +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.position="none")

data_wide %>%
  ggplot(aes(y=`Back aerial`, x=`Dash attack`, col=fighter), alpha=0.5) +
  #facet_grid(~attack_type) +
  geom_jitter(width = 0.5, height = 0.1) +
  theme(legend.position="none")

data_wide <- data_wide[,-2]
names(data_wide) <- c("fighter","BAIR","BThrow","Dash","DAIR","DSmash",
                      "DownB","DThrow","DTILT","Edge","FinalSmash","Floorback",
                      "Floorfront","Floortrip","FAIR","FSmash","FThrow",
                      "FTILT","Grab","ZAIR","NAIR","Neutral","NeutralB",
                      "Pummel","SideB","UpAIR","UpSmash","UpB","UpThrow",
                      "UpTILT")

cor_table <- round(cor(data_wide[,3:21], use="pairwise.complete.obs"),2)


#regression auf tier_list?
data_wide$tierZeRo <- NA
data_wide$tierZeRo[data_wide$fighter=="Little Mac"] <- 1
C_tier <- c("Ridley","Bowser Jr.","Piranha Plant","Mr. Game & Watch")
data_wide$tierZeRo[grep(x=data_wide$fighter,pattern = paste(C_tier, collapse = '|'))] <- 2
A_tier <- c("Ike","Palutena","Young Link", "Snake", "Roy",
            "Chrom", "R.O.B", "Shulk", "Pokemon Trainer",
            "Marth", "Link", "Mega Man", "Yoshi", "Lucario",
            "Ness", "Cloud", "ZSS", "Sonic", "Pac-Man",
            "Lucas", "Mii Swordfighter")
data_wide$tierZeRo[grep(x=data_wide$fighter,pattern = paste(A_tier, collapse = '|'))] <- 4
#B-tier after A because of "Link"
B_tier <- c("Ganondorf","Corrin","Falco","Mario","Dr. Mario",
            "Bayonetta","Captain Falco","Meta Knight","Richter","Simon",
            "Toon Link","Samus","Dark Samus","Wii Fit","Mii Gunner",
            "Luigi", "Donkey Kong", "Bowser", "Villager",
            "Isabelle", "Ryu", "Ken", "Ice Climbers", "Mewtwo",
            "Diddy Kong", "Kirby", "Incineroar", "Robin", "Duck Hunt",
            "Rosalina", "Sheik", "Zelda", "Jigglypuff",
            "King Dedede", "King K. Rool",
            "Pit", "Dark Pit", "Mii Brawler")
data_wide$tierZeRo[grep(x=data_wide$fighter,pattern = paste(B_tier, collapse = '|'))] <- 3
S_tier <- c("Pichu", "Peach", "Daisy", "Olimar", "Lucina",
            "Pikachu", "Wolf", "Fox", "Wario", "Inkling", "Greninja", "Joker")
data_wide$tierZeRo[grep(x=data_wide$fighter,pattern = paste(S_tier, collapse = '|'))] <- 5
data_wide$tierZeRo <- as.factor(data_wide$tierZeRo)
levels(data_wide$tierZeRo) <- c("D","C","B","A","S")

data_wide$tierZeRo <- as.numeric(data_wide$tierZeRo)
lm01 <-   lm(data_wide,
     formula= tierZeRo ~ 
       #BAIR + BThrow + Dash + DAIR + 
       DSmash + DTILT + FThrow + DThrow +
     #DownB +   Edge + FinalSmash + Floorback +
     #Floorfront + Floortrip + FAIR + FSmash + 
     FTILT + NAIR + Neutral + NeutralB +
     Pummel + SideB + UpAIR + UpSmash + UpB + UpThrow +
     UpTILT)
summary(lm01)

#DownTilt super?

data_wide %>%
  ggplot(aes(x=DTILT)) +
  geom_density()


