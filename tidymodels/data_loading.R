rm(list=ls())

library(tidymodels)
library(dplyr)
library(ggplot2)


batting <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/Batting.csv')
parent_data <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/Master.csv')
pitching <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/Pitching.csv')

batting$SF[is.na(batting$SF)] <- 0
batting$IBB[is.na(batting$IBB)] <- 0


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

#As expected
batting[batting$OPS > 1.3,]

batting$SteroidEra <- 0
batting$SteroidEra[batting$yearID > 1993 & batting$yearID < 2006] <- 1
batting$SteroidEra <- as.factor(batting$SteroidEra)

plot_metric_by_year <- function(data,metric){
  
  
}

ggplot(batting, aes(x = yearID, y =OPS, colour = SteroidEra)) + 
  geom_point(alpha = 0.25)
  geom_smooth()


#-------------------------------------------------------------------------------
#Add in list of known steroid users (very small list)
  
#-------------------------------------------------------------------------------#
#Where to start. Maybe the average rate of change of OPS is too high

batting <- batting %>% group_by(playerID) %>% 
  mutate(OPS_Change = OPS - lag(OPS))

#Lets define an outlier: OPS is high (?) and the avg ROC of OPS is abnormally high
#abnormal: top 90% quantile

batting_summary <- batting %>% group_by(playerID) %>% 
  summarize(OPS = mean(OPS), HR = mean(HR), OPS_Change_Avg = mean(OPS_Change, na.rm=TRUE),
            SLUG = mean(SLUG), OPS_Incr_Perc = sum(OPS_Change > 0, na.rm=TRUE)/(length(yearID) - 1),
            Years = length(yearID))

batting_summary[batting_summary$OPS_Incr_Perc > 0.8 & 
                  batting_summary$Years > 5,]

batting_summary[batting_summary$HR > 35,]

#Add in known steroid users
known_steroids <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/mlb_steroid_users.csv')

parent_data$FullName <- paste0(parent_data$nameFirst,' ',parent_data$nameLast)

batting_summary$FullName <- parent_data$FullName[match(paste0(batting_summary$playerID),
                                                       paste0(parent_data$playerID))]

batting_summary$KnownSteroid <- 0
batting_summary$KnownSteroid[batting_summary$FullName %in% known_steroids$Player.Name] <- 1

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



#---------------

svm_default <- svm_poly(mode = "classification")

svm_fit <- 
  svm_default %>% 
  fit(formula = )


