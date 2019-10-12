rm(list=ls()) #delete workspace

# Packages
library(tidyverse)
library(rvest) #harvesting unstructured data
library(httr) #benötigt für RSelenium?
library(RSelenium) #load more of website (Scroll down)
library(roperators) #counting words
library(xml2)
library(stringr) #searching for figthers names

# Navigate to page with RSelenium
url <- "https://www.youtube.com/results?search_query=ssbu+grand+finals&sp=CAI%253D"
driver <- rsDriver(browser=c("firefox"))
remote_driver <- driver[["client"]]
#remote_driver$open() #öffnet nur leeres fenster
remote_driver$navigate(url)
#Sys.sleep(160) #2:40 Minute um sich als nicht-Roboter zu identifizieren, das geht nach circa 1-2h wieder weg

#Runterscrollen um mehr Daten zu bekommen
webElem <- remote_driver$findElement("css", "body") #body auswhählen
for (i in 1:30){
  webElem$sendKeysToElement(list(key = "end")) #scrollen
  Sys.sleep(3) #laden lassen damit erneut heruntergescrollt werden kann
}

# Relevante Daten (Überschriften) aus der Seite ziehen
alldata <- xml2::read_html(remote_driver$getPageSource()[[1]])%>% #transform to html
  rvest::html_nodes("#video-title") #extract title nodes
links <- alldata %>%
  html_attr("href") #extract link
names <- alldata %>%
  html_attr("title") #extract title

#Close connection
remote_driver$close() #bzw window
rm(remote_driver)
rm(webElem)
rm(driver)

#Jetzt hab ich Daten!
dat1 <- as.data.frame(cbind(links,names))
write.csv(dat1, file="raw_data.csv")

#zuerst: Daten cleanen
dat1 <- dat1[dat1$names!="",] #ohne Titel ("") raus
dat1 <- dat1[is.na(dat1$names)==F,] # ohne Titel (NA) raus
dat1$names <- as.character(dat1$names)
datalength <- length(dat1$names) 
write.csv(datalength,file="datalength.csv")


#als nächstes: Charakternamen rausziehen
library(stringr)

#namen von: https://www.ssbwiki.com/Super_Smash_Bros._Ultimate
#wichtig: 
## Dr. Mario vor Mario
## Robin vor R.O.B/ROB
namechecklist <- list(c("Dr. Mario","Dr Mario"),"Mario","Luigi","Peach",
                      "Rosalina", 
                      c("Bowser Jr.","Roy","Morton","Iggy","Wendy",
                        "Lemmy","Larry","Ludwig"), 
                      "Bowser", "Yoshi",
                      c("Donkey Kong","DK"),c("Diddy Kong","Diddy"),"Zelda", c("Ganondorf","Ganon"),
                      "Sheik","Young Link", "Toon Link", "Link",
                      "Dark Samus",c("ZSS","Zero Suit Samus"), "Samus", "Kirby","Meta Knight",
                      "King Dedede", "Fox", "Captain Falcon", "Falco", "Wolf", 
                      "Pikachu", "Jigglypuff","Pichu","Mewtwo",
                      c("Pokemon Trainer","Squirtle","Ivysaur","Charizard"),
                      "Lucario","Greninja","Ness",
                      "Lucas","Ice Climbers","Marth","Roy",
                      "Ike","Lucina","Robin","Corrin",
                      c("Game and Watch","Game & Watch","G&W","G and W"),
                      "Palutena","Dark Pit", "Pit",
                      "Wario",c("Olimar","Alph"),c("R.O.B","ROB"),"Villager",
                      c("Wii Fit","WFT"), "Little Mac",
                      "Shulk", "Duck Hunt",
                      "Snake","Sonic",c("Mega Man","Megaman"),
                      c("Pac-Man","Pacman","Pac Man"),
                      "Ryu", "Cloud", "Bayonetta",
                      c("Mii Brawler","Brawler"), 
                      c("Mii Swordfighter","Mii Sword","Swordfighter"), 
                      c("Mii Gunner","Gunner"),
                      "Daisy", "Piranha Plant", 
                      c("King K. Rool","King K Rool"), "Ridley",
                      "Incineroar", "Chrom", "Isabelle",
                      "Inkling", "Ken", "Simon", "Richter",
                      "Joker", "Hero", c("Banjo","Kazooie"), "Terry")
str(namechecklist)

# für jeden Listeneintrag schauen, ob das Muster erkennbar ist :)
y <- list(NA,NA)
colnames <- c(NA,NA)
for (i in 1:length(namechecklist)){
  y[[i]] <- str_detect(dat1$names, regex(paste(namechecklist[[i]], collapse = '|'), ignore_case = TRUE))
  num = 0
  for (j in 1:length(y[[i]])){
    if(y[[i]][j]==TRUE){
      print(paste0("wird ersetzt",num))
      num <- num+1
      dat1$names[j] <- str_replace_all(string = dat1$names[j], 
                                       pattern = namechecklist[[i]], 
                                       replacement = " ")[1] #replace patterns so that "mario" and "dr. mario" (etc) don't interfere
    }
  }
  dat1[,i+2] <- y[[i]]
  colnames[i] <- as.character(namechecklist[[i]][1])
}
names(dat1) <- c("link","title",colnames)

dat1 <- dat1[,-c(1,2)]
for (i in 1:length(dat1[1,])){
  dat1[,i] <- as.numeric(dat1[,i])
}
wtf <- t(dat1) #transponieren

rowSums <- as.integer(rowSums(wtf))
dat2 <- data.frame(cbind(colnames,rowSums))
dat2$rowSums <- as.character(dat2$rowSums)
dat2$rowSums <- as.numeric(dat2$rowSums)


dat3 <- dat2 %>% 
  arrange(-rowSums)
dat3$tier<-NA
dat3$tier[dat3$rowSums>18] <- "S"
dat3$tier[dat3$rowSums<=18 & dat3$rowSums>8] <- "A"
dat3$tier[dat3$rowSums<=8 & dat3$rowSums>2] <- "B"
dat3$tier[dat3$rowSums<=2 & dat3$rowSums>0] <- "C"
dat3$tier[dat3$rowSums<=0 & dat3$rowSums>-1] <- "D"
dat3$tier <- as.factor(dat3$tier)
dat3$tier = factor(dat3$tier,levels(dat3$tier)[c(4,3,2,1,5)])

#Compare tier list to ZeRo's tier list (https://smash-tier-list.com/ultimate)

dat3$colnames <- as.character(dat3$colnames)
dat3$tierZeRo <- NA
dat3$tierZeRo[dat3$colnames=="Little Mac"] <- 1
C_tier <- c("Ridley","Bowser Jr.","Piranha Plant","Game and Watch")
dat3$tierZeRo[grep(x=dat3$colnames,pattern = paste(C_tier, collapse = '|'))] <- 2
A_tier <- c("Ike","Palutena","Young Link", "Snake", "Roy",
            "Chrom", "R.O.B", "Shulk", "Pokemon Trainer",
            "Marth", "Link", "Mega Man", "Yoshi", "Lucario",
            "Ness", "Cloud", "ZSS", "Sonic", "Pac-Man",
            "Lucas", "Mii Swordfighter")
dat3$tierZeRo[grep(x=dat3$colnames,pattern = paste(A_tier, collapse = '|'))] <- 4
#B-tier after A because of "Link"
B_tier <- c("Ganondorf","Corrin","Falco","Mario","Dr. Mario",
            "Bayonetta","Captain Falco","Meta Knight","Richter","Simon",
            "Toon Link","Samus","Dark Samus","Wii Fit","Mii Gunner",
            "Luigi", "Donkey Kong", "Bowser", "Villager",
            "Isabelle", "Ryu", "Ken", "Ice Climbers", "Mewtwo",
            "Diddy Kong", "Kirby", "Incineroar", "Robin", "Duck Hunt",
            "Rosalina", "Sheik", "Zelda", "Jigglypuff",
            c("King Dedede","Dedede"), "King K. Rool",
            "Pit", "Dark Pit", "Mii Brawler")
dat3$tierZeRo[grep(x=dat3$colnames,pattern = paste(B_tier, collapse = '|'))] <- 3
S_tier <- c("Pichu", "Peach", "Daisy", "Olimar", "Lucina",
            "Pikachu", "Wolf", "Fox", "Wario", "Inkling", "Greninja")
dat3$tierZeRo[grep(x=dat3$colnames,pattern = paste(S_tier, collapse = '|'))] <- 5
dat3$tierZeRo <- as.factor(dat3$tierZeRo)
levels(dat3$tierZeRo) <- c("D","C","B","A","S")

dat3$tierZeRo <- as.numeric(dat3$tierZeRo)
dat3$tier <- as.numeric(dat3$tier)
cor(dat3$tierZeRo, dat3$rowSums, use="pairwise.complete.obs")
cor(dat3$tierZeRo, dat3$tier, use="pairwise.complete.obs")
cor(dat3$tier, dat3$rowSums, use="pairwise.complete.obs")
table(dat3$tierZeRo, dat3$tier)

#log10 rowSums (Ausreißer Ness)
dat3$log10rowSums <- log10(dat3$rowSums+1) #+1 damit 0 nicht -Infinty wird
#dat3$log10rowSums[dat3$rowSums==0] <- NA
cor(dat3$tierZeRo, dat3$log10rowSums, use="pairwise.complete.obs")

names(dat3) <- c("fighter","amount","tier_data","tier_ZeRo","amount_log10")

#save it locally (for later)
write.csv(dat3, file = "SSBU_youtube_tierlist.csv")