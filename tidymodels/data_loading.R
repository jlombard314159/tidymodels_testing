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


#-------------------------------------------------------------------------------
#Some modeling


svm_default <- svm_poly(mode = "classification")

svm_fit <- 
  svm_default %>% 
  fit(formula = )


