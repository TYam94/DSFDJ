
# DSFDJ code 03 ===============================================================
# Model evaluation, negative binomial regression, multi-group comparison

## Preparation ================================================================
### Libraries -----------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(ggsci)
library(broom)
library(broom.helpers)
library(gt)
library(gtsummary)
library(MASS)
library(patchwork)
library(PRROC)

### seed ----------------------------------------------------------------------
# This will fix the table of "random" numbers to be appear
set.seed(400)

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


## Evaluation of `glm` ========================================================
### Extract the subset of interest --------------------------------------------
df5 <- DDR_SCORE03 |>
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |> 
  dplyr::mutate(is_cleared = (percent > 0)) |> 
  print()

### Fit `glm` -----------------------------------------------------------------
# This is identical to attempt #3 in the previous script
clear_glm <- df5 |> 
  dplyr::select(is_cleared, BPM_max, BPM_decrease, NOTES_total, FA) |> 
  glm(formula = is_cleared ~ ., family = binomial(link = "logit"))

### Focusing on beta estimate -------------------------------------------------
summary(clear_glm)
broom::tidy(clear_glm, conf.int = TRUE)

### Focusing on predictive performances ---------------------------------------
# Make tibble of results
# "response" from logistic regression is predicted probabilities
# "link" from glm is linear predictors
df5_preds <- df5 |> 
  dplyr::mutate(
    pred_prob = predict(clear_glm, newdata = df5, type = "response"),
    pred_link = predict(clear_glm, newdata = df5, type = "link")       ) |>
  dplyr::mutate(pred_bool = (pred_prob > 0.5)) |> 
  dplyr::select(tune, is_cleared, pred_prob, pred_link, pred_bool) |> 
  print()

# Plot
p1a <- df5_preds |> 
  ggplot() + 
  ggbeeswarm::geom_quasirandom(aes(x = is_cleared, y = pred_prob, colour = is_cleared)) + 
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw()

p1b <- df5_preds |> 
  ggplot() +
  ggbeeswarm::geom_quasirandom(aes(x = is_cleared, y = pred_link, colour = is_cleared)) + 
  theme_bw()

((p1a + theme(legend.position = "none")) + p1b)

# Confusion matrix
df5_preds |> dplyr::select(is_cleared, pred_bool) |> table()

# ROC
PRROC::roc.curve(scores.class0 = df5_preds$pred_prob, weights.class0 = df5_preds$is_cleared)

p2 <- PRROC::roc.curve(
    scores.class0 = df5_preds$pred_prob, 
    weights.class0 = df5_preds$is_cleared,
    curve = TRUE
    )$curve |> 
  as_tibble(.name_repair = ~c("FPR", "TPR", "threshold")) |> 
  ggplot() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_line(aes(x = FPR, y = TPR)) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
(p2)


## Manual calculation of ROC curve ============================================
### Make candidate list of threshold ------------------------------------------
threshold_candidates <- c(
    min(df5_preds$pred_link) - 0.01, 
    sort(unique(df5_preds$pred_link)),
    max(df5_preds$pred_link) + 0.01
    ) |> 
  print()

### Count TP, FP, TN, FN for each thresholds ----------------------------------
# Make empty tibble as container
threshold_cms <- tibble(NULL)

# Make confusion matrix (abbreviated as `cm` here) for each
for( thr in threshold_candidates){
  cm_each <- df5_preds |> 
    dplyr::summarise(
      TP = sum( pred_link >  thr &  is_cleared),
      FP = sum( pred_link >  thr & !is_cleared),
      TN = sum( pred_link <= thr & !is_cleared),
      FN = sum( pred_link <= thr &  is_cleared) ) |> 
    dplyr::mutate(threshold = thr)
  
  threshold_cms <- dplyr::bind_rows(threshold_cms, cm_each)
}

### Calculate TPR and FPR -----------------------------------------------------
threshold_cms <- threshold_cms |> 
  dplyr::mutate(
    TPR = TP / (TP + FN), # TPR = Sensitivity = Recall
    FPR = FP / (FP + TN), # FPR = (1 - Specificity)
    ) |> 
  dplyr::select("threshold", everything()) |>  # This line only reorders columns
  print()
  
### Make ROC curve ------------------------------------------------------------
threshold_cms |> 
  dplyr::arrange(FPR, TPR) |> 
  ggplot(aes(x = FPR, y = TPR)) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_line() + 
  geom_point(aes(colour = threshold)) + 
  scale_colour_gradient(low = "blue", high = "red") + 
  labs(title = "ROC curve") + 
  theme_bw() + theme(aspect.ratio = 1)


## Overfitting ================================================================
### Take 50% as training set --------------------------------------------------
df5_train <- df5 |> 
  dplyr::group_by(is_cleared) |> 
  dplyr::sample_frac(size = 0.5) |> 
  dplyr::ungroup() |> 
  print()

### Fit `glm` with test set ---------------------------------------------------
clear_glm_split <- df5_train |> 
  dplyr::select(is_cleared, BPM_max, BPM_decrease, NOTES_total, FA) |> 
  glm(formula = is_cleared ~ ., family = binomial(link = "logit"))
summary(clear_glm_split)
broom::tidy(clear_glm_split, conf.int = TRUE)

### Focusing on predictive performances ---------------------------------------
# Make tibble of results 
df5_preds_split <- df5 |> 
  dplyr::mutate(
    in_train  = tune %in% df5_train$tune,
    pred_prob = predict(clear_glm_split, newdata = df5, type = "response"),
    pred_link = predict(clear_glm_split, newdata = df5, type = "link")     ) |>
  dplyr::mutate(pred_bool = (pred_prob > 0.5)) |> 
  dplyr::select(tune, in_train, is_cleared, pred_prob, pred_link, pred_bool) |> 
  print()

# Confusion matrix
df5_preds_split |> dplyr::select(is_cleared, pred_bool, in_train) |> table()

# ROC
df5_preds_split |> 
  dplyr::reframe(
    .by = c("in_train"),
    PRROC::roc.curve(scores.class0  = pred_prob, weights.class0 = is_cleared)$auc)

p3 <- df5_preds_split |> 
  dplyr::reframe(
    .by = c("in_train"),
    PRROC::roc.curve(
        scores.class0  = pred_prob, weights.class0 = is_cleared, curve = TRUE
        )$curve |> 
      as_tibble(.name_repair = ~c("FPR", "TPR", "threshold"))
    ) |> 
  ggplot() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_line(aes(x = FPR, y = TPR, group = in_train, colour = in_train)) + 
  theme_bw() + 
  theme(aspect.ratio = 1)
(p3)

### Comparison of models ------------------------------------------------------
# Model fitted with whole data
broom::tidy(clear_glm, conf.int = TRUE)

# Model fitted with a half of the data
broom::tidy(clear_glm_split, conf.int = TRUE)


## Negative binomial regression ===============================================
### Extract the subset of interest --------------------------------------------
df2 <- DDR_SCORE01 |> 
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |>
  dplyr::filter(tune %in% c("シルドリDDP", "トリジャニDDP")) |>
  dplyr::mutate(
    # Make `tune` to ordered vector
    tune = factor(tune, levels = c("トリジャニDDP", "シルドリDDP")),
    # Force `day` to be treated as factor (category), not as quantity
    day = factor(day)
    ) |> 
  print()

### Negative binomial regression ----------------------------------------------
gfc_negbinom1 <- df2 |> 
  glm.nb(data = _, formula = Miss + Gd ~ tune + day + offset(log(NOTES_total)))
summary(gfc_negbinom1)
gtsummary::tbl_regression(gfc_negbinom1)

### If you want Likelihood Ratio Test, ----------------------------------------
gfc_negbinom0 <- df2 |> 
  glm.nb(data = _, formula = Miss + Gd ~ day + offset(log(NOTES_total)))
summary(gfc_negbinom0)

anova(gfc_negbinom1, gfc_negbinom0)


## ANOVA ======================================================================
### First, do take a glance ---------------------------------------------------
DDR_SCORE02 |> 
  ggplot(aes(x = tune, y = SCORE, colour = tune)) +
  geom_boxplot(outliers = FALSE) +
  ggbeeswarm::geom_quasirandom() +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(
    aspect.ratio = 2, 
    legend.position = "none", 
    axis.text.x  = element_text(angle = 90, size = 12) )

### Statistical test ----------------------------------------------------------
# ANOVA of three groups
anova(aov(data = DDR_SCORE02, SCORE ~ tune))

# Tukey
TukeyHSD(aov(data = DDR_SCORE02, SCORE ~ tune))

### Manually comparing B and C ------------------------------------------------
# Estimating equal variances (Student's t)
DDR_SCORE02 |>
  dplyr::filter(tune %in% c("LondonBEDP", "LondonCEDP")) |> 
  t.test(data = _, SCORE ~ tune, var.equal = TRUE)

# Not estimating equal variances (Welch's t)
DDR_SCORE02 |>
  dplyr::filter(tune %in% c("LondonBEDP", "LondonCEDP")) |> 
  t.test(data = _, SCORE ~ tune)


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
#  [1] PRROC_1.4            rlang_1.1.7          patchwork_1.3.2     
#  [4] MASS_7.3-65          gtsummary_2.5.0      gt_1.3.0            
#  [7] broom.helpers_1.22.0 broom_1.0.12         ggsci_4.2.0         
# [10] ggbeeswarm_0.7.3     lubridate_1.9.5      forcats_1.0.1       
# [13] stringr_1.6.0        dplyr_1.2.0          purrr_1.2.1         
# [16] readr_2.2.0          tidyr_1.3.2          tibble_3.3.1        
# [19] ggplot2_4.0.2        tidyverse_2.0.0     
# 
# loaded via a namespace (and not attached):
#  [1] gtable_0.3.6       beeswarm_0.4.0     xfun_0.57         
#  [4] tzdb_0.5.0         vctrs_0.7.2        tools_4.5.2       
#  [7] generics_0.1.4     parallel_4.5.2     pkgconfig_2.0.3   
# [10] RColorBrewer_1.1-3 S7_0.2.1           lifecycle_1.0.5   
# [13] compiler_4.5.2     farver_2.1.2       litedown_0.9      
# [16] vipor_0.4.7        htmltools_0.5.9    sass_0.4.10       
# [19] pillar_1.11.1      crayon_1.5.3       commonmark_2.0.0  
# [22] tidyselect_1.2.1   digest_0.6.39      stringi_1.8.7     
# [25] labeling_0.4.3     labelled_2.16.0    fastmap_1.2.0     
# [28] grid_4.5.2         cli_3.6.5          magrittr_2.0.4    
# [31] cards_0.7.1        utf8_1.2.6         withr_3.0.2       
# [34] scales_1.4.0       backports_1.5.0    bit64_4.6.0-1     
# [37] timechange_0.4.0   bit_4.6.0          hms_1.1.4         
# [40] evaluate_1.0.5     knitr_1.51         haven_2.5.5       
# [43] markdown_2.0       glue_1.8.0         xml2_1.5.2        
# [46] renv_1.1.7         rstudioapi_0.18.0  vroom_1.7.0       
# [49] R6_2.6.1           fs_2.0.1 

