library(tidyverse)
library(tidymodels)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(vroom)

train_data<- vroom('train.csv')
test_data <- vroom('test.csv')

train <- train_data %>% filter(store==8, item==33)
test <- test_data %>% filter(store==8, item ==33)

cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_recipe <- recipe(sales ~ ., data=train) %>%
  step_date(date, features="decimal") %>%
  step_date(date, features="dow") %>%
  step_mutate(date_dow = as.factor(date_dow)) %>%
  step_rm(store, item) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=7, # default max p to tune
                         non_seasonal_ma=3, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
                         ) %>%
    set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

plot_3 <- cv_results %>%
  modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = training(cv_split)
                   ) %>%
    plot_modeltime_forecast(.interactive=FALSE)

fullfit <- cv_results %>%
  modeltime_refit(data=train)


plot_4 <- fullfit %>%
  modeltime_forecast(
                   new_data = test,
                   actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=FALSE)


plotly::subplot(plot_1,plot_3,plot_2,plot_4, nrows=2)

