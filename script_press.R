library("feather")
library(dplyr)

#scaling function which scales data from 1 to 10
#there are cases when all values are equal, and it's impossible to scale them, so we  give them all 10 points
scaling <- function(x) {

  k<-round(((x - min(x[!is.na(x)])) / (max(x[!is.na(x)]) - min(x[!is.na(x)])))*9+1,1)
  if(length(x[!duplicated(x)])==1)
    k<-rep(10,length(x))
  return(k)
  
}

#press and social  data
social <- read_feather("data/press_social.fth")
if(nrow(social[stringr::str_detect(social$PROGRAM,pattern = "HISTORY OF HORROR"),])!=0)
  social[stringr::str_detect(social$PROGRAM,pattern = "HISTORY OF HORROR"),]$PROGRAM <- "VISIONARIES: HISTORY OF HORROR"
social[social$SEASON=="49" & social$PROGRAM=="LODGE 49",]$SEASON <- "1"
social[social$SEASON=="9" & social$PROGRAM=="WALKING DEAD",]$SEASON <- "9A"
colnames(social) <-
  gsub(colnames(social),
       pattern = "\\.",
       replacement = "\\_")
spends <- read_feather("data/spends.fth")

#social$RUN_DATE <- as.Date(social$RUN_DATE, format = "%m.%d.%y")
press <- read_feather("data/press_listing.fth")
press[stringr::str_detect(press$PROGRAM,pattern = "HISTORY OF HORROR"),]$PROGRAM <- "VISIONARIES: HISTORY OF HORROR"

press<- rbind(social,press[,!(colnames(press) %in% "TALENT")])
press[press$SEASON=="9" & press$PROGRAM=="WALKING DEAD",]$SEASON <- "9A"
press[press$SEASON=="49" & press$PROGRAM=="LODGE 49",]$SEASON <- "1"
press <-merge(press,spends[, c("PROGRAM", "SEASON", "PREMIERE_DATE")],by = c("PROGRAM", "SEASON"),all.x = T)

#LSD contains group assignment based on contribution curves
LSD <- readxl::read_xlsx("data/LSD_group_assignment.xlsx")

#spends contains premiere date for all programs
#spends <- read_feather("data/spends.fth")

#adding premiere date to press data from spends
#press <-merge(press,spends[, c("PROGRAM", "SEASON", "PREMIERE_DATE")],by = c("PROGRAM", "SEASON"),all.x = T)
rm(spends)

#press data without Run date and Circulation are not of interest
press <- press[!is.na(press$RUN_DATE), ]
press <- press[!is.na(press$CIRCULATION),]
press$CIRCULATION <- as.numeric(press$CIRCULATION)
#next step - computing percent of positive tonality
#observations without information about tonality are useless, remember to get rid of them
#TMP - Sum pf positive circulation
#TMP1 - Sum of circulation
TMP <- press[!is.na(press$TONALITY), ] %>% 
  group_by(PROGRAM, SEASON) %>% filter(TONALITY == "positive") %>% 
  dplyr::summarise(TONALITY_POSITIVE = sum(CIRCULATION))

TMP1 <- press[!is.na(press$CIRCULATION), ] %>% 
  group_by(PROGRAM, SEASON) %>% 
  dplyr::summarise(TONALITY_COUNT = sum(CIRCULATION))

TMP1$PER_POSITIVE_TONALITY <- NA

#computing percent of positive tonality combining information from TMP1 nad TMP
if (NROW(TMP1[paste(TMP1$PROGRAM, TMP1$SEASON) %in% 
              paste(TMP$PROGRAM, TMP$SEASON), ]) !=0)
  TMP1[paste(TMP1$PROGRAM, TMP1$SEASON) %in% 
         paste(TMP$PROGRAM, TMP$SEASON), ]$PER_POSITIVE_TONALITY <- round(TMP$TONALITY_POSITIVE / TMP1[paste(TMP1$PROGRAM, TMP1$SEASON) %in% paste(TMP$PROGRAM, TMP$SEASON), ]$TONALITY_COUNT,2)

#shows beyond TMP(data frame with sum of positive circulation)  don't contain assets with positive tonality at all
if (NROW(TMP1[!(paste(TMP1$PROGRAM, TMP1$SEASON) %in% paste(TMP$PROGRAM, TMP$SEASON)), ]) !=0)
  TMP1[!(paste(TMP1$PROGRAM, TMP1$SEASON) %in% paste(TMP$PROGRAM, TMP$SEASON)), ]$PER_POSITIVE_TONALITY <- 0

#adding information obtained to press data
press <- merge(press,TMP1[, c(1, 2, 4)],by = c("PROGRAM", "SEASON"),all.x = T)
rm(TMP,TMP1)
#converting run_date to date format
press$RUN_DATE <- as.Date(press$RUN_DATE, format = "%m.%d.%y")

#calculating week number as difference between date when asset was published and premiere date
press$WEEK_NUMBER <- ifelse(difftime(press$PREMIERE_DATE, press$RUN_DATE, units = "weeks") < 0, 
                            floor(difftime(press$PREMIERE_DATE,press$RUN_DATE,units = "weeks")), 
                            ceiling(difftime(press$PREMIERE_DATE,press$RUN_DATE,units = "weeks")))

#Selecting assets published before premiere (25-week bound is necessery because 
#in the press data there are assets published up to several years before the premiere)
press<- press %>% filter(WEEK_NUMBER >= 0 & WEEK_NUMBER <= 25)

#selecting columns we need
press <- press[, c("PROGRAM","SEASON","CIRCULATION","OUTLET","RUN_DATE","TONALITY","STORY_TYPE","PREMIERE_DATE","WEEK_NUMBER","PER_POSITIVE_TONALITY")]

#Creating data frame with programs(not assets as it was in press) information
first_episodes <- press[!duplicated(press[, c("PROGRAM", "SEASON")]), ]

#Calculating sum circulation for every programs
#for convenience circulation is diveded by 10^6
avg <- press %>% group_by(PROGRAM, SEASON) %>% dplyr::summarise(SUM_CIRCULATION = round(sum(CIRCULATION,na.rm = T)/10^6))
first_episodes <- merge(first_episodes, avg, by = c("PROGRAM", "SEASON"))
rm(avg)
#Calculating unique outlets for every programs
first_episodes <- merge(first_episodes, press %>% group_by(PROGRAM, SEASON) %>% distinct(OUTLET) %>% dplyr::summarise(UNIQUE_OUTLETS = length(OUTLET)), by = c("PROGRAM", "SEASON"), all.x = T)

#Calculating recency week 3 - ratio of common circulation of 3 weeks before premiere to all circulation
#for convenience circulation isstill divided by 10^6
RECENCY_DATA_W3 <- press %>%
  filter(WEEK_NUMBER <= 3) %>%
  group_by(PROGRAM, SEASON) %>%
  dplyr::summarise(CIRC_BEFORE_W3 = sum(CIRCULATION/10^6))

#merging obtained data
first_episodes <- inner_join(first_episodes, RECENCY_DATA_W3, by = c("PROGRAM", "SEASON")) %>% mutate(RECENCY_W3 = round(CIRC_BEFORE_W3 / SUM_CIRCULATION,2))
rm(RECENCY_DATA_W3)

LSD$GROUP<-as.character(LSD$GROUP)

#Creating new group index to number groups from highest to lowest

ch <- c("PREMIERES TIER 3 & RETURNING TIER 1","PREMIERES TIER 4 & RETURNING TIER 2","FEAR+BREAKING BAD",                  
 "PREACHER+PREMIERES TIER 2","BCS+BADLANDS+PREMIERES TIER 1","RETURNING TIER 3","WALKING DEAD TIER 2","WALKING DEAD TIER 1" )
LSD$GROUP1 <- as.numeric(as.character(plyr::mapvalues(as.character(LSD$GROUP), ch, c(6, 7, 2, 5, 4, 8, 1, 3))))
first_episodes <- merge(first_episodes[,c( "PROGRAM", "SEASON", "PREMIERE_DATE", "SUM_CIRCULATION" ,"PER_POSITIVE_TONALITY", "UNIQUE_OUTLETS","RECENCY_W3")],
                        LSD[,c("PROGRAM","PREMIERE_DATE","GROUP","GROUP1")],by=c("PROGRAM","PREMIERE_DATE"))


first_episodes$GROUP_CLUSTER <- first_episodes$GROUP1

#Dividing programs on old(not first seasons) and new(first seasons)
first_episodes_old_shows <-
  first_episodes %>% filter(SEASON != 1)

first_episodes_new_shows <-
  first_episodes %>% filter(SEASON == 1)

#renaming groups for new shows and joining some of them(2-3,5-6,7-8) as we have only 17 shows and 8 groups
first_episodes_new_shows[first_episodes_new_shows$GROUP1<5,]$GROUP <- "5+ contribution groups"
first_episodes_new_shows[first_episodes_new_shows$GROUP1<5,]$GROUP_CLUSTER <- 9

first_episodes_new_shows[first_episodes_new_shows$GROUP1>4 & first_episodes_new_shows$GROUP1<7,]$GROUP <- "3-4 lowest groups"
first_episodes_new_shows[first_episodes_new_shows$GROUP1>4 & first_episodes_new_shows$GROUP1<7,]$GROUP_CLUSTER <- 10

first_episodes_new_shows[first_episodes_new_shows$GROUP1>6,]$GROUP <- "1-2 lowest groups"
first_episodes_new_shows[first_episodes_new_shows$GROUP1>6,]$GROUP_CLUSTER <- 11

#scaling metrics obtained and computing press index
first_episodes_old_shows <-
  first_episodes_old_shows %>% group_by(GROUP_CLUSTER) %>% dplyr::mutate(
    SUM_CIRCULATION_SCALED = scaling(SUM_CIRCULATION),
    UNIQUE_OUTLETS_SCALED = scaling(UNIQUE_OUTLETS),
    PER_POSITIVE_TONALITY_SCALED = scaling(PER_POSITIVE_TONALITY),
    RECENCY_W3_SCALED = scaling(RECENCY_W3),
    PRESS_INDEX_GROUP_CLUSTER = (SUM_CIRCULATION_SCALED * 0.4 + UNIQUE_OUTLETS_SCALED * 0.2 + PER_POSITIVE_TONALITY_SCALED *0.1 + RECENCY_W3_SCALED * 0.3))

first_episodes_new_shows <-
  first_episodes_new_shows %>% group_by(GROUP_CLUSTER) %>% dplyr::mutate(
    SUM_CIRCULATION_SCALED = scaling(SUM_CIRCULATION),
    UNIQUE_OUTLETS_SCALED = scaling(UNIQUE_OUTLETS),
    PER_POSITIVE_TONALITY_SCALED = scaling(PER_POSITIVE_TONALITY),
    RECENCY_W3_SCALED = scaling(RECENCY_W3),
    PRESS_INDEX_GROUP_CLUSTER = (SUM_CIRCULATION_SCALED * 0.4 +  UNIQUE_OUTLETS_SCALED * 0.2 + PER_POSITIVE_TONALITY_SCALED *0.1 + RECENCY_W3_SCALED * 0.3))

DATA <- rbind(as.data.frame(first_episodes_new_shows),
              as.data.frame(first_episodes_old_shows))

DATA <- DATA[,c("PROGRAM",
                "SEASON",
                "PREMIERE_DATE",
                "GROUP",
                "SUM_CIRCULATION_SCALED",
                "UNIQUE_OUTLETS_SCALED",       
                "PER_POSITIVE_TONALITY_SCALED", 
                "RECENCY_W3_SCALED",
                "PRESS_INDEX_GROUP_CLUSTER"   )]
write.csv(DATA,"press_index_data.csv",row.names = F)
