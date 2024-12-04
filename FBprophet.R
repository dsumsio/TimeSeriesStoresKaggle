library(tidyverse)
library(tidymodels)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(vroom)

train_data<- vroom('train.csv')
test_data <- vroom('test.csv')

train <- train_data %>% filter(store==4, item==34)
test <- test_data %>% filter(store==4, item ==34)

cv_split <- time_series_split(train, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = 'prophet') %>%
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_model,
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




