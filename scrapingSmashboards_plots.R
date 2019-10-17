rm(list=ls())


library(data.table) #fread()
library(stringr)
library(tidyverse)
library(ggimage)

### load data #####################################################
today <- Sys.Date()
ssbu_smashboards <- read.csv(paste0("smashboards_long",today,".csv"))
ssbu_base <- read.csv("ssbu_basestats_icons.csv")


### MODIFY DATA #################################################  
# how often does eat character get mentioned?
n_fighter <- ssbu_smashboards %>%
  group_by(character) %>%
  summarise(count = n()) %>%
  arrange(count)
#per placement
n_fighter2 <- ssbu_smashboards %>%
  group_by(character, placement) %>%
  summarise(count = n()) %>%
  arrange(count)
n_fighter2$placement <- as.numeric(n_fighter2$placement)

n_fighter %>%
  ggplot(aes(y=reorder(character, count), x=count)) +
  geom_point()


#per patch
#per placement
n_patch <- ssbu_smashboards %>%
  group_by(character, patch) %>%
  summarise(count = n()) %>%
  arrange(count)


#download images for plot
for (i in 1:length(ssbu_base$picture)){
  png <- paste0(as.character(ssbu_base$character[i]), ".png")
  ssbu_base$pic_local[i] <- png
  #download.file(as.character(ssbu_base$picture[i]),png, mode = 'wb') #deactivated
}
ssbu_base$pic_local[is.na(ssbu_base$picture)] <- NA

  

### merge data ######################

ssbu_base$character <- str_replace_all(ssbu_base$character, "and", "&")
colnames(n_fighter) <- c("character","count")
colnames(n_fighter2) <- c("character","placement","count")
dat <- full_join(ssbu_base, n_fighter, by="character")
dat2 <- full_join(ssbu_base, n_fighter2, by="character")
dat_all <- full_join(ssbu_smashboards,dat, by="character")

dat_patch <- full_join(ssbu_base, n_patch, by="character")
names(n_fighter) <- c("character","total_count")
dat_patch <- full_join(dat_patch, n_fighter, by="character")
dat_patch <- dat_patch %>%
  arrange(total_count)

### plots ##################################################

n_fighter2 %>%
  ggplot(aes(x=count, y=count, label = character)) +
  facet_wrap(~placement, scales = "free") +
  geom_text(position=position_jitter(width=0,height=0))
#anderer plot 2
n_fighter2 %>%
  ggplot(aes(x=count, y=reorder(character, count), color = character)) +
  facet_wrap(~placement, scales = "free") +
  geom_point(position=position_jitter(width=0,height=0)) +
  theme(legend.position = "none")


## Image-Plot
dat2 <- dat2[dat2$placement==1|dat2$placement==2|dat2$placement==3|dat2$placement==4,]
dat2 <- dat2[is.na(dat2$count)==F,]
dat2$placement <- as.factor(dat2$placement)
levels(dat2$placement) <- c("1st place","2nd place","3rd place","4th place")
dat2$character <- reorder(dat2$character, dat2$count)
dat$character <- reorder(dat$character, dat$count)

someplot <- dat2 %>%
  ggplot(aes(y=count, x= character)) +
  facet_wrap(~placement, scales="free") +
  #geom_smooth(method="lm") +
  geom_image(aes(image=pic_local), position = position_jitter(width=0.2)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=12)) +
  ggtitle("Number of mentioned Character per Placement")+
  scale_y_continuous(breaks = c((1:20)*10))


#make a picture out of it
plot(someplot)
ggsave("someplot.png", width = 20, height = 20, units = "cm") 
  
### with log10
someplot_log10 <- dat2 %>%
  ggplot(aes(y=log10(count), x= character)) +
  facet_wrap(~placement, scales="free") +
  #geom_smooth(method="lm") +
  geom_image(aes(image=pic_local), position = position_jitter(width=0.2)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=12)) +
  ggtitle("Number of mentioned Character per Placement") 



#make a picture out of it
plot(someplot_log10)
ggsave("someplot_log10.png", width = 20, height = 20, units = "cm") 

##without groups
someplot3 <- dat %>%
  ggplot(aes(y=log10(count)*10, x= character)) +
  geom_image(aes(image=pic_local), position = position_jitter(width=0.2)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=12)) +
  ggtitle("Number of mentioned Character")+
  scale_y_continuous(breaks = c((1:50)*1))


#make a picture out of it
plot(someplot3)
ggsave("someplot3.png", width = 20, height = 20, units = "cm")




## patch groups
dat_patch$character <- reorder(dat_patch$character, dat_patch$total_count)

n_patch_plot <- dat_patch %>%
  ggplot(aes(y=log10(count)*10, x= character)) +
  facet_wrap(~patch, scales="free") +
  geom_image(aes(image=pic_local), position = position_jitter(width=0.2)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        text = element_text(size=12)) +
  ggtitle("Number of mentioned Character")+
  scale_y_continuous(breaks = c((1:50)*1)) +
  geom_smooth(method="lm")


#make a picture out of it
plot(n_patch_plot)
ggsave("n_patch_plot.png", width = 20, height = 20, units = "cm")



### Calculations ############
library(lme4)

#Frage 1: UV: count
lme1 <- lmer(data = dat_all,
           formula = placement ~ 1 + character + 1|patch)
summary(lme1)





