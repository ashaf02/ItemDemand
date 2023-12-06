install.packages("forecast")
library(vroom)
library(dplyr)
library(recipes)
library(parsnip)
library(forecast)
library(workflows)
library(tidymodels)
library(tidyverse)
library(modeltime)
library(timetk)

## Read in the data
storeTrain <- vroom("train.csv")
storeTest <- vroom("test.csv")

## Filter down to just 1 store item for exploration and model building
storeItem <- storeTrain %>%
  filter(store==2, item==2)
testItem <- storeTest %>%
  filter(store==2, item==2)

## Recipe
storeRecipe <- recipe(sales~., data=storeItem) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year")

## Apply the recipe to your data
prep <- prep(storeRecipe)
baked <- bake(prep, new_data = storeItem)

## Random Forest
my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Create a workflow with model & recipe
forest_wf <- workflow() %>%
  add_recipe(storeRecipe) %>%
  add_model(my_mod)

# Set up grid of tuning values 
tuning_grid <- grid_regular(mtry(range =c(1,7)), min_n(), levels = 3) 

# Set up K-fold CV
folds <- vfold_cv(storeItem, v = 5, repeats = 1)

# Find best tuning parameters 
CV_results <- forest_wf %>%
  tune_grid(
    resamples = folds,
    grid = tuning_grid,
    metrics = metric_set(smape),
    control = control_grid(verbose = TRUE)  # Enable verbose output to monitor progress
  )

## Find Best Tuning Parameters
bestTune <- CV_results %>%
  select_best("smape")

bestTune

collect_metrics(CV_results) %>%
  filter(mtry==7, min_n==21) %>%
  pull(mean)

## Cross-Validation in Time Series
cv_split <- time_series_split(storeItem, assess="3 months", cumulative = TRUE)

## Exponential Smoothing
es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
p1 <- cv_results %>%
modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = storeItem
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(
                         .interactive = FALSE
)

## Refit to all data then forecast
es_fullfit <- cv_results %>%
  modeltime_refit(data = storeItem)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest, by="date") %>%
  select(id, sales)

p2 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=FALSE)

## RUNNING AGAIN WITH DIFFERENT STORE ITEM COMBINATION
storeItem2 <- storeTrain %>%
  filter(store==3, item==17)

## Cross-Validation in Time Series
cv_split <- time_series_split(storeItem2, assess="3 months", cumulative = TRUE)

## Exponential Smoothing
es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItem2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast
es_fullfit <- cv_results %>%
  modeltime_refit(data = storeItem2)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=storeTest, by="date") %>%
  select(id, sales)

p4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(p1,p3,p2,p4, nrows=2)

## SARIMA
## Read in the data
storeTrain <- vroom("train.csv")
storeTest <- vroom("test.csv")

## Filter down to just 1 store item for exploration and model building
storeItem <- storeTrain %>%
  filter(store==2, item==2)

## Recipe
storeRecipe <- recipe(sales~., data=storeItem) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year")

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         # non_seasona_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
  set_engine("auto_arima")

# Set up grid of tuning values 
tuning_grid <- grid_regular(mtry(range =c(1,7)), min_n(), levels = 3) 

# Set up K-fold CV
folds <- vfold_cv(storeItem, v = 5, repeats = 1)

# Find best tuning parameters 
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data= testing(cv_split))

## Cross-Validation in Time Series
cv_split <- time_series_split(storeItem, assess="3 months", cumulative = TRUE)

arima_wf <- workflow() %>%
  add_recipe(storeRecipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))


## Visualize CV results
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItem
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Refit to all data then forecast
fullfit <- cv_results %>%
  modeltime_refit(data = storeItem)

p2 <- fullfit %>%
  modeltime_forecast(
    new_data = testItem, 
    actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=TRUE)

storeItem2 <- storeTrain %>%
  filter(store==3, item==17)
testItem2 <- storeTest %>%
  filter(store==3, item==17)

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItem2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Refit to all data then forecast
fullfit <- cv_results %>%
  modeltime_refit(data = storeItem2)

p4 <- fullfit %>%
  modeltime_forecast(
    new_data = testItem2, 
    actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

plotly::subplot(p1,p3,p2,p4, nrows=2)

## Prophet
## Read in the data
storeTrain <- vroom("train.csv")
storeTest <- vroom("test.csv")

## Filter down to just 1 store item for exploration and model building
storeItem <- storeTrain %>%
  filter(store==2, item==2)
testItem <- storeTest %>%
  filter(store==2, item==2)

## Recipe
storeRecipe <- recipe(sales~., data=storeItem) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_date(date, features="dow") %>%
  step_date(date, features="month") %>%
  step_date(date, features="year")

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

# Find best tuning parameters 
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data= testing(cv_split))

## Cross-Validation in Time Series
cv_split <- time_series_split(storeItem, assess="3 months", cumulative = TRUE)


## Visualize CV results
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItem
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Refit to all data then forecast
fullfit <- cv_results %>%
  modeltime_refit(data = storeItem)

p2 <- fullfit %>%
  modeltime_forecast(
    new_data = testItem, 
    actual_data = storeItem) %>%
  plot_modeltime_forecast(.interactive=TRUE)

storeItem2 <- storeTrain %>%
  filter(store==3, item==17)
testItem2 <- storeTest %>%
  filter(store==3, item==17)

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = storeItem2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Refit to all data then forecast
fullfit <- cv_results %>%
  modeltime_refit(data = storeItem2)

p4 <- fullfit %>%
  modeltime_forecast(
    new_data = testItem2, 
    actual_data = storeItem2) %>%
  plot_modeltime_forecast(.interactive=TRUE)

plotly::subplot(p1,p3,p2,p4, nrows=2)
