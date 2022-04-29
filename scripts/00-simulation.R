#generate simulated data of top popularity amoung third and fourth generation kpop groups 

#### Preamble ####
# Purpose: Clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander [CHANGE THIS TO YOUR NAME!!!!]
# Data: 3 January 2021
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?

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
