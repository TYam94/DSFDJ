
# DSFDJ code 07 ===============================================================
# Generation of simulated data

library(tidyverse)
set.seed(42)

n_obs <- 100
n_rabbits <- 10

DaysInTheWoods <- tibble(
    Cat   = rbinom(n_obs, 1, 0.5), 
    Manju = rbinom(n_obs, 1, 0.5)  ) |> 
  dplyr::mutate(
    Tiger = rbinom(n_obs, 1, prob = 0.1 + 0.4 * Cat + 0.4 * Manju),
    Fox   = rbinom(n_obs, 1, prob = 0.2 + 0.7 * Tiger)              ) |> 
  dplyr::mutate(
    RabbitsEscaped = 
      rbinom(n_obs, n_rabbits, 
             prob = 0.0 * Fox + 0.1 * Cat + 0.7 * Tiger + 0.1 * Manju) ) |> 
  dplyr::select(RabbitsEscaped, Fox, Tiger, Cat, Manju) |> 
  print()

table(DaysInTheWoods$Cat)
table(DaysInTheWoods$Manju)
table(DaysInTheWoods$Tiger)
table(DaysInTheWoods$Fox)

# write_excel_csv(DaysInTheWoods, "data/DaysInTheWoods.csv")
