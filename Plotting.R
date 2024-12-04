library(vroom)
library(tidyverse)
library(ggplot2)
library(patchwork)

train <- vroom('train.csv')
test <- vroom('test.csv')

storeItem_1 <- train %>%
  filter(store==8, item==32)

storeItem_2 <- train %>%
  filter(store==3, item==12)


plot1.1 <- storeItem_1 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

plot1.2 <- storeItem_1 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot1.3 <- storeItem_1 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)

plot2.1 <- storeItem_2 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

plot2.2 <- storeItem_2 %>%
  pull(sales) %>%
  forecast::ggAcf(.)

plot2.3 <- storeItem_2 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365)


final <- (plot1.1 | plot1.2 | plot1.3) /
  (plot2.1 | plot2.2 | plot2.3)


png("my_plot.png", width = 800, height = 600)




