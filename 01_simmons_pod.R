##############################################
#NAME: 01_simmons_pod.R
#PURPOSE: Analyze Bill Simmons podcast data
#DATE: October 2017
##############################################

#References
#http://danielphadley.com/ggplot-Logo/
#http://kateto.net/networks-r-igraph

rm(list=ls())
options(scipen=999)
options(tibble.width = Inf)

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(jsonlite)
library(httr)
library(RSelenium)
library(V8)
library(rvest)
library(knitr)
library(scales)
library(reshape)
library(ggrepel)
library(stringr)
library(igraph)
library(ggraph)
library(widyr)
library(broom)
library(gridExtra)
library(gtable)
library(extrafont)
library(tidytext)
library(ggplot2)
library(magick)
library(here) # For making the script run without a wd
library(magrittr) # For piping the logo
library(stringr)

path<-'/Users/walkerag/Documents/simmons_pod/'

#############################################
#DATA READ-IN
#############################################

pod.dat<-read_csv(file = paste0(path,"/input/bill_simmons_podcast.csv"))

#############################################
#DATA CLEANUP
#############################################

#Format the episode descriptions
pod.dat$desc_format <- sapply(pod.dat$Description,function(row) iconv(row, "latin1", "ASCII", sub=""))
pod.dat$desc_format<-tolower(pod.dat$desc_format)

#No 's
pod.dat$desc_format<- gsub("'s",'',pod.dat$desc_format)
head(pod.dat)

#No punctuation
pod.dat$desc_format<- gsub('[[:punct:]]+',' ',pod.dat$desc_format)

#No numbers
pod.dat$desc_format<- gsub('[[:digit:]]+','',pod.dat$desc_format)

#############################################
#GUEST COUNTS
#############################################

pod.dat.guest<-subset(pod.dat,select=-c(Title,Description,desc_format))

#Wide to long
guest.count.gather <- gather(pod.dat.guest,GuestNum,Guest,Guest1:Guest9)
head(guest.count.gather,n=50)

#Check some episodes
guest.count.gather[guest.count.gather$EpisodeNumber==268,]
guest.count.gather[guest.count.gather$EpisodeNumber==275,]
guest.count.gather[guest.count.gather$EpisodeNumber==40,]

#Remove the NA's
guest.count.gather<-guest.count.gather %>% filter(!is.na(Guest))

#Guest counts by episode
episode.counts<-guest.count.gather %>% group_by(EpisodeNumber) %>% summarise(total=n())
head(episode.counts)

#Counts by guest
guest.counts<-guest.count.gather %>% group_by(Guest) %>% summarise(total=n())
head(guest.counts)

#Take out none
guest.counts<-guest.counts[guest.counts$Guest!="None",]

sum(guest.counts$total)

#Format Bill's Dad entry
guest.counts[guest.counts$Guest=="Bill Dad","Guest"]<-"Bill's Dad"

#Convert to factor, make ordering based on appearances
guest.counts$Guest <- factor(guest.counts$Guest, levels = guest.counts$Guest[order(guest.counts$total)])

#Guest plot
ggplot(guest.counts %>% filter(total>=3),aes(Guest,total)) +
  geom_col(width = 0.5,fill="springgreen3") +
  coord_flip() +
  xlab(NULL) +
  ylab("Appearances") +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 10.5
                         ,family="Arial"
    )
    ,title = element_text(face="bold")
    ,axis.title.y=element_text(margin=margin(0,10,0,0),face="plain") 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
    ,panel.background = element_rect(fill = '#f1f1f1')
  ) +
  ggtitle("Guests On The Bill Simmons Podcast \n(Minimum 3 Appearances)") + guides(fill=FALSE) +
  ggsave(filename = paste0(here("/"), 'simmons_guests', ".png"),
         width = 6, height = 7.75, dpi = 300)

#Call back the plot
background <- image_read(paste0(here("/"), "simmons_guests.png"))
plot(background)

#Bring in a logo
logo_raw <- image_read(paste0(path,"/input/podlogo.png")) 

#Position logo on plot
newimage<-image_composite(background, logo_raw
                          , offset = "+980+600"
                          ,operator = "Over")
plot(newimage)

#Write plot
image_write(newimage, paste0(path,"/output/simmons_graph_guests.png"))

#############################################
#GENDER COUNTS
#############################################

#Subset to female guests
guest.counts.female<-guest.counts[guest.counts$Guest %in% c('Mallory Rubin','Juliet Litman','Sarah Tiana','Abby Wambach'
                                                            ,'Charlize Theron','Diana Taurasi','Katie Baker'
                                                            ,'Katie Nolan','Sally Jenkins'),]

#Create gender variable
guest.counts$gender<-ifelse(!(guest.counts$Guest %in% c('Mallory Rubin','Juliet Litman','Sarah Tiana','Abby Wambach'
                                                            ,'Charlize Theron','Diana Taurasi','Katie Baker'
                                                            ,'Katie Nolan','Sally Jenkins')),'Men','Women')
guest.counts$gender<-factor(guest.counts$gender,levels=c('Women','Men'))
table(guest.counts$gender)

#Check counts
guest.counts %>% group_by(gender) %>% summarise(total_app=sum(total))
guest.counts %>% filter(gender=="Women")
415+18
18/433

#Plot gender distribution
ggplot(guest.counts,aes(gender,total,fill=gender)) +
  geom_col(width = 0.5) +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 10.5
                         ,family="Arial"
)
    ,title = element_text(face="bold")
    ,axis.text.x=element_text(face="bold")
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
    ,panel.background = element_rect(fill = '#f1f1f1')
  ) +
  xlab(NULL) +
  ylab("Appearances") +
  ggtitle("The Bill Simmons Podcast \nIs A Very Male Affair") + guides(fill=FALSE) +
  ggsave(filename = paste0(here("/"), 'simmons', ".png"),
         width = 3, height = 3.75, dpi = 300)

#Call back the plot
background <- image_read(paste0(here("/"), "simmons.png"))
plot(background)

#Bring in Simmons pic
logo_raw <- image_read(paste0(path,"/input/simmons_frown.png")) 

#Position on plot
newimage<-image_composite(background, logo_raw
                , offset = "+230+175"
                ,operator = "Over")
plot(newimage)
image_write(newimage, paste0(path,"/output/simmons_gender_graph.png"))


################
#TOPIC GRAPH
################

pod.dat.topic<-pod.dat
pod.dat.topic[is.na(pod.dat.topic)]<-"NotAGuest"

#Remove guest names
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest1), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest2), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest3), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest4), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest5), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest6), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest7), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest8), "")
pod.dat.topic$desc_format<-str_replace(pod.dat.topic$desc_format, tolower(pod.dat.topic$Guest9), "")

#Put in tidy format
text_df<-pod.dat.topic %>% dplyr::select(EpisodeNumber,desc_format) %>%
  unnest_tokens(input=desc_format, word,to_lower=FALSE,drop=FALSE)

#Put guest names in tidy format
head(guest.counts)
guest.counts$Guest2<-as.character(guest.counts$Guest)
text_df.guest<-guest.counts %>%
  unnest_tokens(input=Guest2, word,to_lower=TRUE,drop=FALSE)

#Remove guest names
text_df <- text_df %>%
  anti_join(text_df.guest)

#Remove stop words
text_df <- text_df %>%
  anti_join(stop_words)

#Take out some unnecessary words
text_df<-text_df[!(text_df$word %in% c(
  'ringer','ringers','brings','joined','joins','discuss','talk','talks','hbo','hbos'
  ,'simmons','include','topics','oconnor','welcomes','discusses'
  ,'discuss'))
  ,]

head(text_df)

#Get word occurrences
text_df<-text_df %>% group_by(word) %>% mutate(total=n())

#Pairwise counts
#At least 10 occurrences
title_word_pairs <- text_df %>% filter(total>=10) %>%
  ungroup() %>%
  pairwise_count(item=word, EpisodeNumber, sort = TRUE, upper = FALSE)
head(title_word_pairs)

title_word_pairs$Matches<-title_word_pairs$n

#ABSOLUTE COUNT VERSION
#Removed some outlier words that were being displayed way out on the graph edges.
title_word_pairs %>%
  filter(Matches >= 5) %>%
  filter(!item1 %in% c('lonzo','los','angeles','ball','times','york','hall','fame')) %>%
  filter(!item2 %in% c('lonzo','los','angeles','ball','times','york','hall','fame')) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "sphere"
         #,niter=3500
         ) +
  geom_edge_link(aes(edge_alpha = Matches, edge_width = Matches), edge_colour = "springgreen3") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size=9.5, 
                 point.padding = unit(0.2, "lines")) +
  theme(text = element_text(size = 7.5,family="Arial Bold")
        ,legend.key.size = unit(2.5,"line")
        ,legend.text = element_text(size=20)
        ,legend.title = element_text(size=20)
        ,panel.border = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,legend.box.background = element_blank()
        ,legend.key=element_blank()
        ,title = element_text(size=35)
  ) +
  ggtitle("NFL, NBA Talk Dominates The Bill Simmons Podcast")
