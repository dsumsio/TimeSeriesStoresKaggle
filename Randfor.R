library(tidyverse)
library(tidymodels)
library(parsnip)
library(vroom)

train <- vroom('train.csv')
test <- vroom('test.csv')

storeItem_1 <- train %>%
  filter(store==8, item==32)

recipe <- recipe(sales ~ ., data=storeItem_1) %>%
  step_date(date, features="decimal") %>%
  step_date(date, features="dow") %>%
  step_mutate(date_dow = as.factor(date_dow)) %>%
  step_rm(store, item) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

prep(recipe)
  

rand_for <- rand_forest(mtry = tune(),
                        min_n = tune(),
                        trees = 5000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rand_for) 

grid_of_tuning_params_randfor <- grid_regular(mtry(range = c(1,100)),
                                              min_n(),
                                              levels = 3)

folds <- vfold_cv(train, v = 5, repeats = 1)

## Run the CV
CV_results <- wf %>%
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params_randfor,
    metrics = metric_set(smape)
  )

CV_results %>%
  show_best(metric = 'smape')










