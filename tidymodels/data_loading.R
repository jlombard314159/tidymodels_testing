#Goals: 5-10 min talk about data and general strategies to investigate
##Feature engineering -> We make some assumptions about. how do we test these assumptions
#(10mins) -> walk through the various ways I am making columns to split groups (steroid users vs not steroid users)
##Modeling steps
#Setting up an initial model, running, then updating
##Tracking results somehow -> or maybe just using targets to set up these sorts of workflows
#See https://mdneuzerling.com/post/machine-learning-pipelines-with-tidymodels-and-targets/
rm(list=ls())

library(tidymodels)
library(dplyr)
library(ggplot2)
options(tibble.width = Inf)

batting <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/Batting.csv')
parent_data <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/Master.csv')
# pitching <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/Pitching.csv')

batting$SF[is.na(batting$SF)] <- 0
batting$IBB[is.na(batting$IBB)] <- 0

#Helpful to have things sorted by year f or each playerID
batting <- batting %>% group_by(playerID) %>% 
  arrange(yearID)

#Injuries -> steroid use. Use low year to normal year as a surrogate
batting <- batting %>% group_by(playerID) %>% 
  mutate(PreviousYearInjury = AB - lag(AB))

batting <- batting %>% filter(yearID > 1945) %>% 
  filter(AB > 130) %>% 
  mutate(OB_perc = (H+BB + HBP)/(AB + HBP +BB+SF),
         X1B = (H - HR - X2B - X3B),
         SLUG = (X1B + 2 * X2B + 3*X3B + 4*HR)/AB,
         OPS = OB_perc+ SLUG)

#Remove if only two years of playing
short_stint <- batting %>% group_by(playerID) %>% 
  summarize(years_played = length(yearID)) %>% 
  filter(years_played >= 3)

batting <- batting %>%  filter(playerID %in% short_stint$playerID)

#Steroid specific information
#Add in known steroid users
known_steroids <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/mlb_steroid_users.csv')

parent_data$FullName <- paste0(parent_data$nameFirst,' ',parent_data$nameLast)

batting$FullName <- parent_data$FullName[match(paste0(batting$playerID),
                                                       paste0(parent_data$playerID))]

batting$KnownSteroid <- 0
batting$KnownSteroid[batting$FullName %in% known_steroids$Player.Name] <- 1

batting$SteroidEra <- 0
batting$SteroidEra[batting$yearID > 1993 & batting$yearID < 2006] <- 1
batting$SteroidEra <- as.factor(batting$SteroidEra)

#As expected
batting[batting$OPS > 1.3,]
# 
# plot_metric_by_year <- function(data,metric){
#   
#   
# }
# 
# ggplot(batting, aes(x = yearID, y =OPS, colour = SteroidEra)) + 
#   geom_point(alpha = 0.25)
#   geom_smooth()


#-------------------------------------------------------------------------------
#Add in list of known steroid users (very small list)
  
#-------------------------------------------------------------------------------#
#Where to start. Maybe the average rate of change of OPS is too high
batting <- batting %>% group_by(playerID) %>% 
  mutate(OPS_Change = OPS - lag(OPS))

#Lets define an outlier: OPS is high (?) and the avg ROC of OPS is abnormally high
#abnormal: top 90% quantile

batting_summary <- batting %>% group_by(FullName) %>% 
  summarize(OPS = mean(OPS), HR = mean(HR), OPS_Change_Avg = mean(OPS_Change, na.rm=TRUE),
            SLUG = mean(SLUG), OPS_Incr_Perc = sum(OPS_Change > 0, na.rm=TRUE)/(length(yearID) - 1),
            Years = length(yearID),
            PrevInjuryCount = sum(PreviousYearInjury > 150, na.rm=TRUE)/(length(yearID)-1),
            KnownSteroid = unique(KnownSteroid))

batting_summary$HighPrevInjuryCount <- 0
batting_summary$HighPrevInjuryCount[batting_summary$PrevInjuryCount > 0.5] <- 1

batting_summary[batting_summary$OPS_Incr_Perc > 0.8 & 
                  batting_summary$Years > 5,]

batting_summary[batting_summary$HR > 35,]


#-------------------------------------------------------------------------------
#Some modeling
##See https://machinelearningmastery.com/one-class-classification-algorithms/
##for later actual outlier detection type models

#Split data up into test/train
set.seed(314159)
library(rsample)

data_split <- initial_split(batting_summary, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

steroid_use <- recipe(KnownSteroid ~ .,
                      data = train_data) 

steroid_use <- steroid_use %>% 
  update_role(FullName, new_role = 'Player Name')

summary(steroid_use)

steroid_use <- steroid_use %>% 
  step_dummy(all_nominal_predictors())

#-------------------------------------------------------------------------------------------------
#To really iterate:
#Fiddle with the assumptions for feature engineering
#TODO: Steps below can be altered or possibly put int oa function (?)  to show the
#various ways to alter things
batting <- batting %>% group_by(playerID) %>% 
  mutate(OPS_Change = OPS - lag(OPS))

#Lets define an outlier: OPS is high (?) and the avg ROC of OPS is abnormally high
#abnormal: top 90% quantile

batting_summary <- batting %>% group_by(FullName) %>% 
  summarize(OPS = mean(OPS), HR = mean(HR), OPS_Change_Avg = mean(OPS_Change, na.rm=TRUE),
            SLUG = mean(SLUG), OPS_Incr_Perc = sum(OPS_Change > 0, na.rm=TRUE)/(length(yearID) - 1),
            Years = length(yearID),
            PrevInjuryCount = sum(PreviousYearInjury > 150, na.rm=TRUE)/(length(yearID)-1),
            KnownSteroid = unique(KnownSteroid))

#Fiddle with the modeling process


