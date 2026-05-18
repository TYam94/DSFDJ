
# DSFDJ code 06 ===============================================================
# Spurious regression and GLS

## Preparation ================================================================
### Libraries -----------------------------------------------------------------
library(tidyverse)
library(nlme)
library(ggsci)
library(ggpubr)
library(broom)
library(broom.helpers)
library(patchwork)

### seed ----------------------------------------------------------------------
# This will fix the table of "random" numbers to be appear
set.seed(6)

### Random walk ---------------------------------------------------------------
sim_ts <- tibble( 
    y0 = rnorm(1000 * 100)     # Normally distributed values (1000 * 100 times)
    ) |> 
  dplyr::mutate(
    s = as.integer((row_number() - 1) %/% 1000), # series ID
    t = as.integer((row_number() - 1) %%  1000)  # time (1 - 1000) 
    ) |> 
  dplyr::group_by(s) |> 
  dplyr::mutate( y = cumsum(y0) ) |>  # Make cum-sum for each series
  dplyr::ungroup() |>
  dplyr::select(s, t, y0, y) |> 
  print()

# Take two series for instance
sim_ts1 <- sim_ts |> dplyr::filter(s == 0) |> print()
sim_ts2 <- sim_ts |> dplyr::filter(s == 1) |> print()


## Spurious regression ========================================================
### Do take a glance ----------------------------------------------------------
p1a <- sim_ts1 |>
  ggplot(aes(x = t, y = y)) + 
  geom_point() + 
  theme_classic()

p1b <- sim_ts2 |>
  ggplot(aes(x = t, y = y)) + 
  geom_point() + 
  theme_classic()

(p1a / p1b)

### linear model with OLS -----------------------------------------------------
# Fit `lm` (with OLS, unspecified)
sim_ts1_ols <- lm(data = sim_ts1, y ~ t)
summary(sim_ts1_ols)

sim_ts2_ols <- lm(data = sim_ts2, y ~ t)
summary(sim_ts2_ols)

# Plot with the line
p2a <- sim_ts1 |>
  ggplot() + 
  geom_abline(
    slope = broom::tidy(sim_ts1_ols)$estimate[2], 
    intercept = broom::tidy(sim_ts1_ols)$estimate[1], 
    colour = "blue") + 
  geom_point(aes(x = t, y = y)) + 
  theme_classic()

p2b <- sim_ts2 |>
  ggplot(aes(x = t, y = y)) + 
  geom_abline(
    slope = broom::tidy(sim_ts2_ols)$estimate[2], 
    intercept = broom::tidy(sim_ts2_ols)$estimate[1], 
    colour = "blue") + 
  geom_point() + 
  theme_classic()

(p2a / p2b)

### Two series ----------------------------------------------------------------
p3 <- sim_ts1 |> 
  dplyr::rename("y_s1" = "y") |> 
  dplyr::left_join(by = c("t"), sim_ts2 |> dplyr::rename("y_s2" = "y") ) |> 
  ggplot(aes(x = y_s1, y = y_s2)) +
  geom_point() +
  ggpubr::stat_cor() + 
  theme_classic()
(p3)

cor.test(sim_ts1$y, sim_ts2$y)

### Frequency of spurious regression ------------------------------------------
# Do linear regression with OLS for each series
sim_ts_ols <- sim_ts |> 
  dplyr::group_by(s) |> 
  dplyr::reframe( lm(y ~ t) |> summary() |> broom::tidy() ) |> 
  dplyr::filter(term == "t") |> 
  dplyr::mutate(q.value = p.adjust(p.value)) |> 
  print()

# False discoveries were obviously frequent
table(sim_ts_ols$q.value < 0.05)

# Plot
sim_ts |>
  dplyr::left_join(by = c("s"), sim_ts_ols, relationship = "many-to-one") |> 
  dplyr::mutate(significant = (q.value < 0.05)) |> 
  ggplot() + 
  geom_line(aes(x = t, y = y, group = s, colour = significant), alpha = 0.2) + 
  theme_classic() + 
  ggsci::scale_color_lancet()


### Auto-correlation ----------------------------------------------------------
# Residuals
plot(sim_ts1_ols, which = 1)
plot(sim_ts2_ols, which = 1)

# Auto-correlation
cor(sim_ts1$y, lag(sim_ts1$y), use = "pairwise.complete.obs")
cor(sim_ts2$y, lag(sim_ts2$y), use = "pairwise.complete.obs")


## GLS ========================================================================
### Fit randam walks with gls -------------------------------------------------
# Fit `lm` with GLS, specifying auto-regression of residuals
sim_ts1_gls <- nlme::gls(data = sim_ts1, y ~ t, correlation = corAR1())
summary(sim_ts1_ols)
summary(sim_ts1_gls)

sim_ts2_gls <- nlme::gls(data = sim_ts2, y ~ t, correlation = corAR1())
summary(sim_ts2_ols)
summary(sim_ts2_gls)

### Series with true trend ----------------------------------------------------
sim_ts3 <- tibble(t = seq(0, 999), r = rnorm(1000)) |> 
  dplyr::mutate(y = cumsum(r) + 0.02 * t) |> 
  print()

ggplot(sim_ts3) + geom_point(aes(x = t, y = y)) + theme_classic()

sim_ts3_ols <- lm(data = sim_ts3, y ~ t)
sim_ts3_gls <- nlme::gls(data = sim_ts3, y ~ t, correlation = corAR1())

summary(sim_ts3_ols)
summary(sim_ts3_gls)


## Session info ===============================================================
sessionInfo()
# > sessionInfo()
# R version 4.5.2 (2025-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
#   LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=Japanese_Japan.utf8  LC_CTYPE=Japanese_Japan.utf8   
# [3] LC_MONETARY=Japanese_Japan.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=Japanese_Japan.utf8    
# 
# time zone: Asia/Tokyo
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices datasets  utils     methods   base     
# 
# other attached packages:
#  [1] patchwork_1.3.2      broom.helpers_1.22.0 broom_1.0.12        
#  [4] ggpubr_0.6.3         ggsci_4.2.0          nlme_3.1-169        
#  [7] lubridate_1.9.5      forcats_1.0.1        stringr_1.6.0       
# [10] dplyr_1.2.0          purrr_1.2.1          readr_2.2.0         
# [13] tidyr_1.3.2          tibble_3.3.1         ggplot2_4.0.2       
# [16] tidyverse_2.0.0     
# 
# loaded via a namespace (and not attached):
#  [1] gtable_0.3.6       ggsignif_0.6.4     compiler_4.5.2    
#  [4] renv_1.1.7         tidyselect_1.2.1   scales_1.4.0      
#  [7] lattice_0.22-9     R6_2.6.1           labeling_0.4.3    
# [10] generics_0.1.4     Formula_1.2-5      backports_1.5.0   
# [13] car_3.1-5          pillar_1.11.1      RColorBrewer_1.1-3
# [16] tzdb_0.5.0         rlang_1.1.7        utf8_1.2.6        
# [19] stringi_1.8.7      S7_0.2.1           timechange_0.4.0  
# [22] cli_3.6.5          withr_3.0.2        magrittr_2.0.4    
# [25] grid_4.5.2         rstudioapi_0.18.0  hms_1.1.4         
# [28] lifecycle_1.0.5    vctrs_0.7.2        rstatix_0.7.3     
# [31] glue_1.8.0         farver_2.1.2       abind_1.4-8       
# [34] carData_3.0-6      tools_4.5.2        pkgconfig_2.0.3

