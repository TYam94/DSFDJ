
# DSFDJ code 02 ===============================================================
# Linear models

## Preparation ================================================================
### Libraries -----------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(ggsci)
library(broom)
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

DDR_DATA <- read_csv("data/DDR_DP_tunedata.csv") |> print()


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
  dplyr::distinct(tune, judges_sum, NOTES, FA) |> 
  dplyr::arrange(tune, judges_sum) |> 
  print()

df4_check |> dplyr::group_by(tune) |> dplyr::filter(n() > 1)
  
df4_check |> dplyr::filter(judges_sum != (NOTES + FA))

### Modified application ------------------------------------------------------
df4b <- df4 |> 
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |> 
  dplyr::mutate(score_bulk = (SCORE / 1000000) * (NOTES + FA) ) |> 
  print()

score_lm2 <- lm(data = df4b, formula = score_bulk ~ Mv + Pf + Gr + Gd + OK + Miss)

summary(score_lm2)
broom::tidy(score_lm2, conf.int = TRUE)

### Another pipeline ----------------------------------------------------------
score_lm2s <- df4 |> 
  dplyr::left_join(by = c("tune" = "略称"), DDR_DATA) |> 
  dplyr::mutate(score_bulk = (SCORE / 1000000) * (NOTES + FA) ) |> 
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
  dplyr::select(is_cleared, SCORE, BPM_max, NOTES, FA) |> 
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
  dplyr::select(is_cleared, Miss, BPM_min, BPM_max, NOTES, FA) |> 
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
  ggplot(aes(x = (NOTES - 2 * FA), y = Miss, colour = is_cleared)) +
  geom_point() +
  geom_abline(slope = -0.1, intercept = 92, linetype = "dashed") +
  ggsci::scale_color_npg() +
  theme_bw() +
  theme(aspect.ratio = 1)

### Attempt 3 -----------------------------------------------------------------
clear_glm3 <- df5 |> 
  dplyr::select(is_cleared, BPM_min, BPM_max, NOTES, FA) |> 
  glm(formula = is_cleared ~ ., family = binomial(link = "logit"))
summary(clear_glm3)
broom::tidy(clear_glm3, conf.int = TRUE)

### Attempt 4 -----------------------------------------------------------------
clear_glm4 <- df5 |> 
  dplyr::mutate(
    BPM_ratio = 1 - (BPM_min / BPM_max),
    NOTES_sum = (NOTES + FA)
    ) |> 
  dplyr::select(
    "is_cleared",
    "NOTES_sum", "FA", "BPM_max", "BPM_ratio") |> 
  glm(data = _, formula = is_cleared ~ ., family = binomial(link = "logit"))

summary(clear_glm4)
broom::tidy(clear_glm4, conf.int = TRUE)

tibble(predict(clear_glm1), df5$is_cleared)


## Session info ===============================================================
sessionInfo()

