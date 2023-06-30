#Goals: 5-10 min talk about data and general strategies to investigate
##Feature engineering -> We make some assumptions about. how do we test these assumptions
#(10mins) -> walk through the various ways I am making columns to split groups (steroid users vs not steroid users)
##Modeling steps
#Setting up an initial model, running, then updating
##Tracking results somehow -> or maybe just using targets to set up these sorts of workflows
#See https://mdneuzerling.com/post/machine-learning-pipelines-with-tidymodels-and-targets/
rm(list=ls())

##TODO: Functionize a lot of the code (mainly for targets)
##TODO: Finish the single walkthrough
##TODO: Finish the demonstration of the more complex step
##TODO: Do a plot of the various models or something (on training data)
##TODO: Show the pipeline being ran in targets

library(tidymodels)
library(dplyr)
library(ggplot2)
options(tibble.width = Inf)

batting <- read.csv('C:/Users/jlomb/Documents/PersonalProjects/tidymodels_testing/Batting.csv')
parent_data <- read.csv('C:/Users/jlomb/Documents/PersonalProjects/tidymodels_testing/Master.csv')
# pitching <- read.csv('C:/Users/jlomb/Documents/Personal Coding/tidymodels/Pitching.csv')

#Remove some NA values
batting$SF[is.na(batting$SF)] <- 0
batting$IBB[is.na(batting$IBB)] <- 0

#Helpful to have things sorted by year f or each playerID
batting <- batting %>% group_by(playerID) %>% 
  arrange(yearID)

#Injuries -> steroid use. Use low year to normal year as a surrogate
batting <- batting %>% group_by(playerID) %>% 
  mutate(PreviousYearInjury = AB - lag(AB))

batting <- batting %>% filter(yearID > 1945) %>% 
  filter(AB > 60) %>% 
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
known_steroids <- read.csv('C:/Users/jlomb/Documents/PersonalProjects/tidymodels_testing/mlb_steroid_users.csv')

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
#   geom_smooth(

#-------------------------------------------------------------------------------#
#Where to start. Maybe the average rate of change of OPS is too high
batting <- batting %>% group_by(playerID) %>% 
  mutate(OPS_Change = OPS - lag(OPS),
         HR_Change = HR - lag(HR))

#Lets define an outlier: OPS is high (?) and the avg ROC of OPS is abnormally high
#abnormal: top 90% quantile

batting_summary <- batting %>% group_by(FullName) %>% 
  summarize(OPS = mean(OPS), HR = mean(HR), OPS_Change_Avg = mean(OPS_Change, na.rm=TRUE),
            SLUG = mean(SLUG), OPS_Incr_Perc = sum(OPS_Change > 0, na.rm=TRUE)/(length(yearID) - 1),
            Years = length(yearID),
            PrevInjuryCount = sum(PreviousYearInjury > 150, na.rm=TRUE)/(length(yearID)-1),
            KnownSteroid = unique(KnownSteroid),
            HR_Incr_Perc = sum(HR_Change > 0, na.rm=TRUE)/(length(yearID)-1))

batting_summary$HighPrevInjuryCount <- 0
batting_summary$HighPrevInjuryCount[batting_summary$PrevInjuryCount > 0.5] <- 1

batting_summary[batting_summary$OPS_Incr_Perc > 0.8 & 
                  batting_summary$Years > 5,]

batting_summary[batting_summary$HR > 35,]

batting_summary %>% group_by(KnownSteroid) %>% 
  summarize(mean(HR),
            mean(SLUG),
            mean(OPS),
            mean(OPS_Incr_Perc),
            mean(HR_Incr_Perc))

#-------------------------------------------------------------------------------
#Some modeling
##See https://machinelearningmastery.com/one-class-classification-algorithms/
##for later actual outlier detection type models

#Split data up into test/train
set.seed(314159)
library(rsample)

#Split up by strata (steroid usage)
data_split <- initial_split(batting_summary, prop = 3/4,
                            strata = KnownSteroid)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

steroid_use <- recipe(KnownSteroid ~ .,
                      data = train_data) 

steroid_use <- steroid_use %>% 
  update_role(FullName, new_role = 'Player Name')

summary(steroid_use)

steroid_use <- steroid_use %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate(KnownSteroid = as.factor(KnownSteroid)) %>% 
  step_normalize(all_numeric_predictors())

#------------------------------------------------------------------------------------------------
#Actual modeling

log_reg <- logistic_reg() %>% 
  set_engine("glm")

steroid_use_mflow <- 
  workflow() %>% 
  add_model(log_reg) %>% 
  add_recipe(steroid_use)

steroid_fit <- 
  steroid_use_mflow %>% 
  fit(data = train_data)

steroid_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

#----------------------------------------------------------------------------------------------
# a much better holistic resource (https://www.tmwr.org/workflow-sets.html)

full_steroid_model <- recipe(KnownSteroid ~ .,
                             data = train_data) %>% 
  update_role(FullName, new_role = 'Player Name') %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate(KnownSteroid = as.factor(KnownSteroid)) %>% 
  step_normalize(all_numeric_predictors())

minimum_model <- recipe(KnownSteroid ~ HR + Years + 
                          PrevInjuryCount,data = train_data) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate(KnownSteroid = as.factor(KnownSteroid)) %>% 
  step_normalize(all_numeric_predictors())

rf_spec <- 
  rand_forest(mtry = 5, min_n = tune(), trees = 25) %>% 
  set_engine("randomForest") %>% 
  set_mode("classification")

elastic_net <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine('glmnet')

all_steroid_models <- 
  workflow_set(
    preproc = list(full_model= full_steroid_model,
                   minimum_model = minimum_model),
    models = list(lr = log_reg, rf = rf_spec,
                  glmnet = elastic_net),
    cross = TRUE
)

train_folds  <- vfold_cv(train_data, strata = KnownSteroid, repeats = 5)

all_models <- all_steroid_models %>%
  workflow_map(resamples = train_folds)

all_models %>% 
  rank_results() %>% 
  filter(.metric == 'roc_auc') %>% 
  select(model, .config, rank, wflow_id)

#-----------------------------------------------------------------
#Select a top model (maybe do in another script)
best_results <- all_models %>% 
  extract_workflow_set_result("minimum_model_lr") 
  
best_model <- all_models %>% 
  extract_workflow('minimum_model_lr')


test_fit <-
  best_model %>% 
  last_fit(data_split)

test_fit %>% 
  collect_metrics()

test_fit %>% 
  collect_predictions() %>% 
  roc_curve(KnownSteroid, .pred_1) %>% 
  autoplot()

#---------------------------------------------------------------
#Handle class imbalance
# https://juliasilge.com/blog/himalayan-climbing/

#---------------------------------------------------------------
#Do something with the test data now (?)


#----------------------------------------------------------------
#Reusing steps
#No clue how to 
test_recipe <- recipe() %>% 
step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate(KnownSteroid = as.factor(KnownSteroid)) %>% 
  step_normalize(all_numeric_predictors())

