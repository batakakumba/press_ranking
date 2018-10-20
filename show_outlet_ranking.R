
library("feather")
library(plotly)
library(plyr)
library(stringr)
library(dplyr)
library(stringi)
library(stringdist)
library(formattable)

#show of interest:
#show <- "FEAR THE WALKING DEAD"

scaling <- function(x) {
  (x - min(x[!is.na(x)])) / (max(x[!is.na(x)]) - min(x[!is.na(x)]))
}





##reading social data and calculating mean of circulation for all outlets
social <- read_feather("data/press_social.fth")
if(nrow(social[stringr::str_detect(social$PROGRAM,pattern = "HISTORY OF HORROR"),])!=0)
  social[stringr::str_detect(social$PROGRAM,pattern = "HISTORY OF HORROR"),]$PROGRAM <- "VISIONARIES: HISTORY OF HORROR"
social[social$SEASON=="49" & social$PROGRAM=="LODGE 49",]$SEASON <- "1"
social[social$SEASON=="9" & social$PROGRAM=="WALKING DEAD",]$SEASON <- "9A"

spends <- read_feather("data/spends.fth")
social <-merge(social,spends[, c("PROGRAM", "SEASON", "PREMIERE_DATE")],by = c("PROGRAM", "SEASON"),all.x = T)
social$WEEK_NUMBER <- ifelse(difftime(social$PREMIERE_DATE, social$RUN_DATE, units = "weeks") < 0, 
                             floor(difftime(social$PREMIERE_DATE,social$RUN_DATE,units = "weeks")), 
                             ceiling(difftime(social$PREMIERE_DATE,social$RUN_DATE,units = "weeks")))

#Selecting assets published before premiere (25-week bound is necessery because 
#in the social data there are assets published up to several years before the premiere)
social <- social %>% filter(WEEK_NUMBER > -1 & WEEK_NUMBER <= 25)
social_mean <- social %>% group_by(OUTLET) %>% dplyr::summarise(MEAN_CIRC_SOCIAL = round(mean(CIRCULATION,na.rm=T),2))
if(nrow(social[stringr::str_detect(social$PROGRAM,pattern = "HISTORY OF HORROR"),])!=0)
social[stringr::str_detect(social$PROGRAM,pattern = "HISTORY OF HORROR"),]$PROGRAM <- "VISIONARIES: HISTORY OF HORROR"

## reading press data
#calculating week number as difference between date when asset was published and premiere date
press <-  read_feather("data/press_listing.fth")
press[stringr::str_detect(press$PROGRAM,pattern = "HISTORY OF HORROR"),]$PROGRAM <- "VISIONARIES: HISTORY OF HORROR"
press[press$SEASON=="9" & press$PROGRAM=="WALKING DEAD",]$SEASON <- "9A"
press[press$SEASON=="49" & press$PROGRAM=="LODGE 49",]$SEASON <- "1"


press <-merge(press,spends[, c("PROGRAM", "SEASON", "PREMIERE_DATE")],by = c("PROGRAM", "SEASON"),all.x = T)
press$WEEK_NUMBER <- ifelse(difftime(press$PREMIERE_DATE, press$RUN_DATE, units = "weeks") < 0, 
                            floor(difftime(press$PREMIERE_DATE,press$RUN_DATE,units = "weeks")), 
                            ceiling(difftime(press$PREMIERE_DATE,press$RUN_DATE,units = "weeks")))

#Selecting assets published before premiere (25-week bound is necessery because 
#in the press data there are assets published up to several years before the premiere)
press <- press %>% filter(WEEK_NUMBER > -1 & WEEK_NUMBER <= 25)
press <- press[!is.na(press$OUTLET),]




default_df_future <- readRDS("data/default_df_future.RDS")

press <- merge(press,default_df_future[,c("PROGRAM","SEASON","UNRECOGNISED_CONTR","SUM_CIRCULATION")],by = c("PROGRAM","SEASON"),all.x = T)


type_freq <- (merge(press %>% 
                      dplyr::group_by(PROGRAM,SEASON,OUTLET) %>% 
                      dplyr::summarise(SUM_CIRCULATION=sum(CIRCULATION),
                                       MEAN_CIRCULATION = round(mean(CIRCULATION),2)),
                    
                    press[!duplicated(paste(press$PROGRAM,press$SEASON,press$OUTLET)),] %>% 
                      dplyr::group_by(OUTLET,PROGRAM,SEASON) %>% 
                      dplyr::summarise(sum_unrec=sum(UNRECOGNISED_CONTR),
                                       mean_unrec = round(mean(UNRECOGNISED_CONTR),2)),
                    by=c("OUTLET","SEASON","PROGRAM")))



TMP <- press[!is.na(press$TONALITY), ] %>% 
  group_by(OUTLET,SEASON,PROGRAM) %>% 
  filter(TONALITY == "positive") %>% 
  dplyr::summarise(TONALITY_POSITIVE = sum(CIRCULATION))

TMP1 <- press[!is.na(press$TONALITY), ] %>% 
  group_by(OUTLET,SEASON,PROGRAM) %>% 
  dplyr::summarise(TONALITY_COUNT = sum(CIRCULATION))

TMP1$PER_POS_TONALITY <- NA

if (NROW(TMP1[paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM), ]) !=0)
  TMP1[paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM), ]$PER_POS_TONALITY <- TMP$TONALITY_POSITIVE / TMP1[paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM), ]$TONALITY_COUNT
if (NROW(TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM)), ]) !=0)
  TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM)), ]$PER_POS_TONALITY <- 0
type_freq<- merge(type_freq,TMP1[, c(1,2,3,5)],by = c("OUTLET","SEASON","PROGRAM"),all.x = T)


TMP <- press[!is.na(press$TONALITY), ] %>% 
  group_by(OUTLET,SEASON,PROGRAM) %>% 
  filter(TONALITY == "negative") %>% 
  dplyr::summarise(TONALITY_NEGATIVE = sum(CIRCULATION))

TMP1 <- press[!is.na(press$TONALITY), ] %>% 
  group_by(OUTLET,SEASON,PROGRAM) %>% 
  dplyr::summarise(TONALITY_COUNT = sum(CIRCULATION))
TMP1$PER_NEG_TONALITY <- NA
if (NROW(TMP1[paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM), ]) !=0)
  TMP1[paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM), ]$PER_NEG_TONALITY <- round(TMP$TONALITY_NEGATIVE / TMP1[paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM), ]$TONALITY_COUNT,2)
if (NROW(TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM)), ]) !=0)
  TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON,TMP1$PROGRAM) %in% paste(TMP$OUTLET,TMP$SEASON,TMP$PROGRAM)), ]$PER_NEG_TONALITY <- 0

type_freq<- merge(type_freq,TMP1[, c(1,2,3,5)],by = c("OUTLET","SEASON","PROGRAM"),all.x = T)
type_freq$WEIGHTED_CICRULATION <-type_freq$SUM_CIRCULATION
type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$WEIGHTED_CICRULATION <- round(type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$SUM_CIRCULATION*type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$PER_POS_TONALITY -type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$SUM_CIRCULATION*type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$PER_NEG_TONALITY,2)





corr_data <-
  (type_freq[!is.na(type_freq$WEIGHTED_CICRULATION) &
               !is.na(type_freq$sum_unrec), ] %>% group_by(OUTLET) %>% dplyr::summarise(
                 corr_weighted = round(cor(WEIGHTED_CICRULATION, sum_unrec), 2),
                 quantity_of_seasons = length(sum_unrec)
               ))

corr_data <- corr_data[!is.na(corr_data$quantity_of_seasons),][corr_data[!is.na(corr_data$quantity_of_seasons),]$quantity_of_seasons>3,]

tmp <- press %>% dplyr::group_by(OUTLET) %>% dplyr::summarise(MEAN_CIRCULATION = round(mean(CIRCULATION),2))
corr_data <- merge(corr_data,tmp,by="OUTLET",all.x=T)
corr_data <- merge(corr_data,social_mean,by="OUTLET",all.x = T)

corr_data$CORRELATION<-NA
centers <- kmeans(corr_data[!is.na(corr_data$corr_weighted),]$corr_weighted, centers = 10)$centers
centers <- sort(centers)
corr_data[!is.na(corr_data$corr_weighted),]$CORRELATION<- kmeans(corr_data[!is.na(corr_data$corr_weighted),]$corr_weighted, centers = centers)$cluster


corr_data$OUTLET_PRESS_SIZE<-NA
centers <- kmeans(corr_data[!is.na(corr_data$MEAN_CIRCULATION),]$MEAN_CIRCULATION, centers = 10)$centers
centers <- sort(centers)
corr_data[!is.na(corr_data$MEAN_CIRCULATION),]$OUTLET_PRESS_SIZE<- kmeans(corr_data[!is.na(corr_data$MEAN_CIRCULATION),]$MEAN_CIRCULATION,centers = centers)$cluster


corr_data$QUANTITY_OF_SEASONS<-NA
centers <- kmeans(corr_data[!is.na(corr_data$quantity_of_seasons),]$quantity_of_seasons, centers = 10)$centers
centers <- sort(centers)
corr_data[!is.na(corr_data$quantity_of_seasons),]$QUANTITY_OF_SEASONS<- kmeans(corr_data[!is.na(corr_data$quantity_of_seasons),]$quantity_of_seasons, centers = centers)$cluster


corr_data[is.na(corr_data$MEAN_CIRC_SOCIAL),]$MEAN_CIRC_SOCIAL <- 0
corr_data$OUTLET_SOCIAL_SIZE<-NA
centers <- kmeans(corr_data[!is.na(corr_data$MEAN_CIRC_SOCIAL),]$MEAN_CIRC_SOCIAL, centers = 10)$centers
centers <- sort(centers)
corr_data[!is.na(corr_data$MEAN_CIRC_SOCIAL),]$OUTLET_SOCIAL_SIZE<- kmeans(corr_data[!is.na(corr_data$MEAN_CIRC_SOCIAL),]$MEAN_CIRC_SOCIAL,centers = centers)$cluster



corr_data$FINAL_INDEX <-round((corr_data$CORRELATION+corr_data$QUANTITY_OF_SEASONS+corr_data$OUTLET_PRESS_SIZE+corr_data$OUTLET_SOCIAL_SIZE)/4,1)


corr_data_all1 <- corr_data[,c("OUTLET","corr_weighted")]

corr_data_all <- corr_data[,!(colnames(corr_data) %in% c("corr_weighted","quantity_of_seasons","MEAN_CIRCULATION","MEAN_CIRC_SOCIAL"))]
corr_data_all <- corr_data_all[order(-corr_data_all$FINAL_INDEX),]
corr_data_all$RANK <- mapvalues(x = corr_data_all$FINAL_INDEX,from = unique(corr_data_all$FINAL_INDEX),to = 1:length(unique(corr_data_all$FINAL_INDEX)))




##########################################################################

press <- press[press$OUTLET %in% corr_data_all1$OUTLET,]
write.csv(corr_data_all,"corr_data_all.csv",row.names = F)

show_outlet_ranking <- function(show){
  # show <- "BETTER CALL SAUL"
  serial <- press[press$PROGRAM == show,]
  number_of_seasons <- length(unique(serial$SEASON))
  
  type_freq <- merge(serial %>% 
                       dplyr::group_by(OUTLET,SEASON) %>% 
                       dplyr::summarise(SUM_CIRCULATION=sum(CIRCULATION),
                                        MEAN_CIRCULATION = round(mean(CIRCULATION),2),
                                        LENGTH = length(CIRCULATION)),
                     serial[!duplicated(paste(serial$PROGRAM,serial$SEASON,serial$OUTLET)),] %>% 
                       dplyr::group_by(OUTLET,SEASON) %>% 
                       dplyr::summarise(sum_unrec=sum(UNRECOGNISED_CONTR),
                                        mean_unrec = round(mean(UNRECOGNISED_CONTR)),2),by=c("OUTLET","SEASON"))
  
  
  
  # calculating PER_POSITIVE_TONALITY 
  TMP <- serial[!is.na(serial$TONALITY), ] %>% 
    group_by(OUTLET,SEASON) %>% 
    filter(TONALITY == "positive") %>% 
    dplyr::summarise(TONALITY_POSITIVE = sum(CIRCULATION))
  
  TMP1 <- serial[!is.na(serial$TONALITY), ] %>% 
    group_by(OUTLET,SEASON) %>% 
    dplyr::summarise(TONALITY_COUNT = sum(CIRCULATION))
  TMP1$PER_POS_TONALITY <- NA
  if (NROW(TMP1[paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON), ]) !=0)
    TMP1[paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON), ]$PER_POS_TONALITY <- TMP$TONALITY_POSITIVE / TMP1[paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON), ]$TONALITY_COUNT
  if (NROW(TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON)), ]) !=0)
    TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON)), ]$PER_POS_TONALITY <- 0
  type_freq<- merge(type_freq,TMP1[, c(1,2,4)],by = c("OUTLET","SEASON"),all.x = T)
  
  # calculating PER_NEGATIVE_TONALITY
  TMP <- serial[!is.na(serial$TONALITY), ] %>% 
    group_by(OUTLET,SEASON) %>% filter(TONALITY == "negative") %>% 
    dplyr::summarise(TONALITY_NEGATIVE = sum(CIRCULATION))
  TMP1 <- serial[!is.na(serial$TONALITY), ] %>% 
    group_by(OUTLET,SEASON) %>% 
    dplyr::summarise(TONALITY_COUNT = sum(CIRCULATION))
  TMP1$PER_NEG_TONALITY <- NA
  if (NROW(TMP1[paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON), ]) !=0)
    TMP1[paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON), ]$PER_NEG_TONALITY <- round(TMP$TONALITY_NEGATIVE / TMP1[paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON), ]$TONALITY_COUNT,2)
  if (NROW(TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON)), ]) !=0)
    TMP1[!(paste(TMP1$OUTLET,TMP1$SEASON) %in% paste(TMP$OUTLET,TMP$SEASON)), ]$PER_NEG_TONALITY <- 0
  type_freq<- merge(type_freq,TMP1[, c(1,2,4)],by = c("OUTLET","SEASON"),all.x = T)
  
  #COMPUTING CIRCULATION WEIGHTED BY TONALITY
  #IF TONALITY IS NA  WEIGHTED BY TONALITY IS EQUAL TO ORDINARY CIRCULATION
  type_freq$WEIGHTED_CICRULATION <-type_freq$SUM_CIRCULATION
  type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$WEIGHTED_CICRULATION <- round(type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$SUM_CIRCULATION*type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$PER_POS_TONALITY -type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$SUM_CIRCULATION*type_freq[!(is.na(type_freq$PER_NEG_TONALITY) | is.na(type_freq$PER_POS_TONALITY)),]$PER_NEG_TONALITY,2)
  
  
  
  #calculating metrics for outlets:
  #  - correlation between unrecognised delivery and weighted circulation
  #  - quantity of seasons with outlet
  
  corr_data <- (
    type_freq[!is.na(type_freq$WEIGHTED_CICRULATION) &
                !is.na(type_freq$sum_unrec), ] %>% group_by(OUTLET) %>% dplyr::summarise(
                  corr_weighted = round(cor(WEIGHTED_CICRULATION, sum_unrec), 2),
                  quantity_of_seasons = length(SEASON)
                ))
  
  
  #if number of seasons with outlets is  lower then 3, outlet is out of interest, because correlation information is'nt stable
  
  corr_data[corr_data$quantity_of_seasons<=3,]$corr_weighted  <-  
    corr_data_all1[corr_data_all1$OUTLET %in% corr_data[corr_data$quantity_of_seasons<=3,]$OUTLET,]$corr_weighted
  
  
  show_outlets <- corr_data$OUTLET
  #corr_data$quanity_scaled <- round(scaling(corr_data$quantity_of_seasons),2)                                                                                                              
  
  #calculating mean circulation for each outlet
  tmp <- press[press$PROGRAM==show,] %>% 
    dplyr::group_by(OUTLET,SEASON) %>% 
    dplyr::summarise(MEAN_CIRCULATION = round(mean(CIRCULATION),2))
  
  
  
  #tmp$MEAN_CIRCULATION_SCALED <- round(scaling(tmp$MEAN_CIRCULATION),2)
  corr_data <- merge(tmp,corr_data,by=c("OUTLET"),all.y=T)
  
  tmp <- social[social$PROGRAM==show,] %>% 
    dplyr::group_by(OUTLET,SEASON) %>% 
    dplyr::summarise(MEAN_CIRC_SOCIAL = round(mean(CIRCULATION),2))
  #calculating mean circulation for each outlet
  # tmp <- social[social$PROGRAM==show,] %>% 
  #   dplyr::group_by(OUTLET,SEASON) %>% 
  #   dplyr::summarise(MEAN_SOCIAL_CIRCULATION = round(mean(CIRCULATION),2))
  
  #tmp$MEAN_CIRCULATION_SCALED <- round(scaling(tmp$MEAN_CIRCULATION),2)
  corr_data <- merge(corr_data,tmp,by=c("OUTLET","SEASON"),all.x=T)
  
  
  
  
  ##using kmeans to divide all metrics in groups from 1 to 10(min is the worst, max is the better)
  
  corr_data$CORRELATION<-NA
  centers <- kmeans(corr_data[!is.na(corr_data$corr_weighted),]$corr_weighted, centers = 10)$centers
  centers <- sort(centers)
  corr_data[!is.na(corr_data$corr_weighted),]$CORRELATION <- 
    kmeans(corr_data[!is.na(corr_data$corr_weighted),]$corr_weighted, centers = centers)$cluster
  
  
  corr_data$OUTLET_PRESS_SIZE<-NA
  centers <- kmeans(corr_data[!is.na(corr_data$MEAN_CIRCULATION),]$MEAN_CIRCULATION, centers = 10)$centers
  centers <- sort(centers)
  corr_data[!is.na(corr_data$MEAN_CIRCULATION),]$OUTLET_PRESS_SIZE<- 
    kmeans(corr_data[!is.na(corr_data$MEAN_CIRCULATION),]$MEAN_CIRCULATION,centers = centers)$cluster
  
  
  corr_data[is.na(corr_data$MEAN_CIRC_SOCIAL),]$MEAN_CIRC_SOCIAL <- 0
  corr_data$OUTLET_SOCIAL_SIZE<-NA
  if(NROW(corr_data[corr_data$MEAN_CIRC_SOCIAL!=0,])<10){
    corr_data[corr_data$MEAN_CIRC_SOCIAL!=0,]$OUTLET_SOCIAL_SIZE <- round(scaling(corr_data[corr_data$MEAN_CIRC_SOCIAL!=0,]$MEAN_CIRC_SOCIAL)*9+1,0)
    
    
    corr_data[(corr_data$MEAN_CIRC_SOCIAL)==0,]$OUTLET_SOCIAL_SIZE<- 1
  }else{
    centers <- kmeans(corr_data$MEAN_CIRC_SOCIAL, centers = 10)$centers
    centers <- sort(centers)
    corr_data[!is.na(corr_data$MEAN_CIRC_SOCIAL),]$OUTLET_SOCIAL_SIZE<- 
      kmeans(corr_data$MEAN_CIRC_SOCIAL,centers = centers)$cluster
  }
  
  corr_data$QUANTITY_OF_SEASONS <- corr_data$quantity_of_seasons+10-number_of_seasons
  
  corr_data$FINAL_INDEX <-round((corr_data$CORRELATION+
                                   corr_data$QUANTITY_OF_SEASONS+
                                   corr_data$OUTLET_PRESS_SIZE+
                                   corr_data$OUTLET_SOCIAL_SIZE)/4,1)
  
  show_rank <- corr_data[,!(colnames(corr_data) %in% c("corr_weighted","quantity_of_seasons","MEAN_CIRCULATION","MEAN_CIRC_SOCIAL"))]
  
  show_rank <- show_rank[order(-show_rank$FINAL_INDEX),]
  for(i in 1:length(unique(show_rank$SEASON)))
    if (i==1){
      tmp <- corr_data_all[!(corr_data_all$OUTLET %in% show_rank[show_rank$SEASON==unique(show_rank$SEASON)[i],]$OUTLET),]
      tmp$SEASON <- unique(show_rank$SEASON)[i]
      corr_data_promising <- tmp[1:25,]
    }else{
      tmp <- corr_data_all[!(corr_data_all$OUTLET %in% show_rank[show_rank$SEASON==unique(show_rank$SEASON)[i],]$OUTLET),]
      tmp$SEASON <- unique(show_rank$SEASON)[i]
      corr_data_promising <- rbind(corr_data_promising,
                                   tmp[1:25,])
      
    }
  
  return(list(show_rank,corr_data_promising))
}

shows <- unique(press$PROGRAM)
for(i in 1:length(shows)){
  models <- show_outlet_ranking(shows[i])
  models[[1]] <- models[[1]][order(models[[1]]$SEASON),]
  models[[1]]<-models[[1]] %>% group_by(SEASON) %>% mutate(RANK=1:length(OUTLET))
  models[[1]]<-models[[1]] %>% group_by(SEASON) %>% dplyr::top_n(wt=-RANK,n = 25)
  models[[1]]<-models[[1]][,!(colnames(models[[1]])=="RANK")]
  models[[1]]$PROGRAM <- shows[i]
  
  #models[[2]] <- models[[2]][order(models[[2]]$SEASON),]
  models[[2]]<-models[[2]] %>% group_by(SEASON) %>% mutate(RANK=1:length(OUTLET))
  models[[2]]<-models[[2]] %>% group_by(SEASON) %>% dplyr::top_n(wt=-RANK,n = 25)
  models[[2]]<-models[[2]][,!(colnames(models[[2]])=="RANK")]
  models[[2]]$PROGRAM <- shows[i]
  
  
  if(i==1){
    show_outlet_rank <- models[[1]]
    show_promising_rank <- models[[2]]
  }else{  
    show_outlet_rank <- rbind(show_outlet_rank,models[[1]])
    show_promising_rank  <- rbind(show_promising_rank ,models[[2]])
  }
  
  
}


show_outlet_rank <- show_outlet_rank[order(show_outlet_rank$PROGRAM,show_outlet_rank$SEASON,-show_outlet_rank$FINAL_INDEX),]
show_promising_rank <- show_promising_rank[order(show_promising_rank$PROGRAM,show_promising_rank$SEASON,-show_promising_rank$FINAL_INDEX),]

write.csv(show_outlet_rank,"show_outlet_rank.csv",row.names = F)
write.csv(show_promising_rank,"show_promising_rank.csv",row.names = F)
