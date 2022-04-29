#generate simulated data of top popularity amoung third and fourth generation kpop groups 

library(knitr)
library(tidyverse)
library(tidyr)
library(dplyr)


kpop_mid <- read.csv(file="inputs/data/raw_data_popular.csv", header=T)
set.seed(7217)

simulated_dataset <-
  tibble(
    kpop_group = sample(
      x = kpop_mid |> 
        select(name) |> unique() |> unlist(),
      size = 20,
      replace = FALSE
    ),
    popularity = sample(
      x = c(50:100),
      size = 20,
      replace = TRUE
    ),
    followers = sample(
      x = c(400000:40000000),
      size = 20,
      replace = TRUE
    ),
    overall_score = popularity + followers
  ) |>
  select(kpop_group, popularity, followers, overall_score) |>
  arrange(-overall_score)

simulated_dataset
