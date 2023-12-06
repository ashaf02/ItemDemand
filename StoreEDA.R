library(tidyverse)
library(tidymodels)
library(vroom)
library(forecast)
library(gridExtra)

## Read in the data
StoreTrain <- vroom("train.csv")
StoreTest <- vroom("test.csv")

acf1 <- StoreTrain %>% 
  filter(store==1, item==1) %>%
  pull(sales) %>%
  forecast::ggAcf(.)
acf1

acf2 <- StoreTrain %>% 
  filter(store==2, item==2) %>%
  pull(sales) %>%
  forecast::ggAcf(.)
acf2

acf3 <- StoreTrain %>% 
  filter(store==3, item==3) %>%
  pull(sales) %>%
  forecast::ggAcf(.)
acf3

acf4 <- StoreTrain %>% 
  filter(store==4, item==4) %>%
  pull(sales) %>%
  forecast::ggAcf(.)
acf4

grid.arrange(acf1, acf2, acf3, acf4, ncol = 2)
