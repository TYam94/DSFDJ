
# DSFDJ code 02 ===============================================================
# Linear models

## Preparation ================================================================
### Libraries -----------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(ggsci)
library(broom)
library(broom.helpers)
library(gt)
library(gtsummary)

### Data ----------------------------------------------------------------------
DDR_SCORE01 <- read_csv("data/DDR_DP_SCORE_01.csv") |> print()
dput(names(DDR_SCORE01))
dput(sort(unique(DDR_SCORE01$tune)))

DDR_SCORE02 <- read_csv("data/DDR_DP_SCORE_02.csv") |> print()
dput(names(DDR_SCORE02))
dput(sort(unique(DDR_SCORE02$tune)))

DDR_SCORE03 <- read_csv("data/DDR_DP_SCORE_03.csv") |> print()
dput(names(DDR_SCORE03))
dput(sort(unique(DDR_SCORE03$tune)))

DDR_DATA_raw <- read_csv("data/DDR_DP_tunedata.csv") |> print()

### Preprocess ----------------------------------------------------------------
dput(names(DDR_DATA_raw))

DDR_DATA <- DDR_DATA_raw |> 
  dplyr::mutate(
    NOTES_total  = NOTES + FA,
    BPM_decrease = 1 - (BPM_min / BPM_max) ) |> 
  dplyr::select(
    "略称", "曲名", "難易度", "レベル", 
    "NOTES_total", "FA", "BPM_max", "BPM_decrease",
    "STREAM", "VOLTAGE", "AIR", "FREEZE", "CHAOS")


## Linear regression ==========================================================
### Extract the subset of interest --------------------------------------------
df4 <- dplyr::bind_rows(DDR_SCORE01, DDR_SCORE02, DDR_SCORE03) |> 
  dplyr::select("tune", "SCORE", "Mv", "Pf", "Gr", "Gd", "OK", "Miss") |> 
  print()

### Simple application --------------------------------------------------------
df4a <- df4 |> 
  dplyr::mutate(judges_sum = (Mv + Pf + Gr + Gd + OK + Miss)) |> 
  dplyr::mutate(score_bulk = (SCORE / 1000000) * judges_sum ) |> 
  print()

score_lm1 <- lm(data = df4a, formula = score_bulk ~ Mv + Pf + Gr + Gd + OK + Miss)

summary(score_lm1)
broom::tidy(score_lm1, conf.int = TRUE)

### Checking consistency ------------------------------------------------------
df4_check <- df4a |> 
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |> 
  dplyr::distinct(tune, judges_sum, NOTES_total) |> 
  dplyr::arrange(tune, judges_sum) |> 
  print()

df4_check |> dplyr::group_by(tune) |> dplyr::filter(n() > 1)
  
df4_check |> dplyr::filter(judges_sum != NOTES_total)

### Modified application ------------------------------------------------------
df4b <- df4 |> 
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |> 
  dplyr::mutate(score_bulk = (SCORE / 1000000) * NOTES_total ) |> 
  print()

score_lm2 <- lm(data = df4b, formula = score_bulk ~ Mv + Pf + Gr + Gd + OK + Miss)

summary(score_lm2)
broom::tidy(score_lm2, conf.int = TRUE)

### Another pipeline ----------------------------------------------------------
score_lm2s <- df4 |> 
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |> 
  dplyr::mutate(score_bulk = (SCORE / 1000000) * NOTES_total ) |> 
  dplyr::select("score_bulk", "Mv", "Pf", "Gr", "Gd", "OK", "Miss") |> 
  print() |> 
  lm(formula = score_bulk ~ .) |> 
  print()

summary(score_lm2s)
broom::tidy(score_lm2s, conf.int = TRUE)

### Table and figure ----------------------------------------------------------
score_lm2 |> 
  gtsummary::tbl_regression(estimate_fun = ~style_number(.x, digits = 3))

broom::tidy(score_lm2, conf.int = TRUE) |> 
  dplyr::filter(term != "(Intercept)") |> 
  dplyr::mutate(term = fct_rev(factor(term, levels = term))) |> 
  ggplot(aes(x = estimate, y = term)) + 
  geom_vline(xintercept = 0, colour = "blue", linetype = "dashed") +
  geom_vline(xintercept = 1, colour = "red",  linetype = "dashed") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) + 
  geom_point() +
  labs(x = "proportion", y = "") +
  theme_bw()

### Residual plot -------------------------------------------------------------
# handy plotting
plot(score_lm2, which = 1)

# step by step
tibble(
    score_bulk = df4b$score_bulk, 
    score_bulk_predicted = predict(score_lm2, df4b) ) |> 
  dplyr::mutate(residual = (score_bulk - score_bulk_predicted)) |> 
  ggplot() +
  geom_point(aes(x = score_bulk, y = residual)) + 
  theme_bw()


## Logistic regression ========================================================
### Extract the subset of interest --------------------------------------------
df5 <- DDR_SCORE03 |>
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |> 
  dplyr::mutate(is_cleared = (percent > 0)) |> 
  print()

### Attempt 1 -----------------------------------------------------------------
clear_glm1 <- df5 |> 
  dplyr::select(is_cleared, SCORE, BPM_max, BPM_decrease, NOTES_total, FA) |> 
  glm(formula = is_cleared ~ ., family = binomial(link = "logit"))
summary(clear_glm1)

df5 |>  
  ggplot(aes(x = is_cleared, y = SCORE, colour = is_cleared)) +
  geom_boxplot(outliers = FALSE) +
  ggbeeswarm::geom_quasirandom() +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(
    aspect.ratio = 2, 
    legend.position = "none", 
    axis.text.x  = element_text(angle = 90, size = 12) )

### Attempt 2 -----------------------------------------------------------------
clear_glm2 <- df5 |> 
  dplyr::select(is_cleared, Miss, BPM_max, BPM_decrease, NOTES_total, FA) |> 
  glm(formula = is_cleared ~ ., family = binomial(link = "logit"))
summary(clear_glm2)

df5 |>  
  ggplot(aes(x = is_cleared, y = Miss, colour = is_cleared)) +
  geom_boxplot(outliers = FALSE) +
  ggbeeswarm::geom_quasirandom() +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(
    aspect.ratio = 2, 
    legend.position = "none", 
    axis.text.x  = element_text(angle = 90, size = 12) )

df5 |>  
  ggplot(aes(x = (NOTES_total - 3 * FA), y = Miss, colour = is_cleared)) +
  geom_point() +
  geom_abline(slope = -0.1, intercept = 92, linetype = "dashed") +
  ggsci::scale_color_npg() +
  theme_bw() +
  theme(aspect.ratio = 1)

### Attempt 3 -----------------------------------------------------------------
clear_glm3 <- df5 |> 
  dplyr::select(is_cleared, BPM_max, BPM_decrease, NOTES_total, FA) |> 
  glm(formula = is_cleared ~ ., family = binomial(link = "logit"))
summary(clear_glm3)
broom::tidy(clear_glm3, conf.int = TRUE)

clear_glm3 |> 
  gtsummary::tbl_regression(estimate_fun = ~style_number(.x, digits = 3))


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
#  [1] gtsummary_2.5.0      gt_1.3.0             broom.helpers_1.22.0
#  [4] broom_1.0.12         ggsci_4.2.0          ggbeeswarm_0.7.3    
#  [7] lubridate_1.9.5      forcats_1.0.1        stringr_1.6.0       
# [10] dplyr_1.2.0          purrr_1.2.1          readr_2.2.0         
# [13] tidyr_1.3.2          tibble_3.3.1         ggplot2_4.0.2       
# [16] tidyverse_2.0.0     
# 
# loaded via a namespace (and not attached):
#  [1] gtable_0.3.6       beeswarm_0.4.0     xfun_0.57         
#  [4] tzdb_0.5.0         vctrs_0.7.2        tools_4.5.2       
#  [7] generics_0.1.4     parallel_4.5.2     pkgconfig_2.0.3   
# [10] RColorBrewer_1.1-3 S7_0.2.1           lifecycle_1.0.5   
# [13] compiler_4.5.2     farver_2.1.2       vipor_0.4.7       
# [16] litedown_0.9       htmltools_0.5.9    sass_0.4.10       
# [19] pillar_1.11.1      crayon_1.5.3       commonmark_2.0.0  
# [22] tidyselect_1.2.1   digest_0.6.39      stringi_1.8.7     
# [25] labeling_0.4.3     labelled_2.16.0    fastmap_1.2.0     
# [28] grid_4.5.2         cli_3.6.5          magrittr_2.0.4    
# [31] cards_0.7.1        utf8_1.2.6         withr_3.0.2       
# [34] scales_1.4.0       backports_1.5.0    bit64_4.6.0-1     
# [37] timechange_0.4.0   bit_4.6.0          hms_1.1.4         
# [40] evaluate_1.0.5     knitr_1.51         haven_2.5.5       
# [43] markdown_2.0       rlang_1.1.7        glue_1.8.0        
# [46] xml2_1.5.2         renv_1.1.7         rstudioapi_0.18.0 
# [49] vroom_1.7.0        R6_2.6.1           fs_2.0.1


