
# DSFDJ code 01 ===============================================================
# Two group comparison

## Preparation ================================================================
### Libraries -----------------------------------------------------------------
library(tidyverse)
library(ggbeeswarm)
library(ggsci)
library(patchwork)

### Data ----------------------------------------------------------------------
DDR_SCORE01 <- read_csv("data/DDR_DP_SCORE_01.csv") |> print()
dput(names(DDR_SCORE01))
dput(sort(unique(DDR_SCORE01$tune)))


## Behavior of NA =============================================================
### In calculation ------------------------------------------------------------
print(5 + NA)
max(1, 10, -5, NA)

### In logical operations -----------------------------------------------------
any(TRUE,  TRUE,  FALSE)
any(FALSE, FALSE, FALSE)
all(TRUE,  TRUE,  FALSE)
all(TRUE,  TRUE,  TRUE)

any(TRUE,  TRUE,  FALSE, NA)
any(FALSE, FALSE, FALSE, NA)
all(TRUE,  TRUE,  FALSE, NA)
all(TRUE,  TRUE,  TRUE,  NA)

### In tibble -----------------------------------------------------------------
animals <- tibble(
  species = c("dog", "monkey", "cat", "pheasant", "snake", "starfish"),
  is_ally = c(TRUE,  TRUE,     FALSE, TRUE,       FALSE,   NA)
    ) |> 
  print()

animals |> dplyr::filter(is_ally)
animals |> dplyr::filter(!is_ally)


## Types ======================================================================
chrs_auto <- seq(1, 22)
class(chrs_auto)
<<<<<<< HEAD
purrr::keep(chrs_auto, ~(1 <= . & . <= 22))

chrs_all <- c(seq(1, 22), "X")
class(chrs_all)
purrr::keep(chrs_all, ~(1 <= . & . <= 22))
chrs_all |> purrr::keep(~. != "X") |> purrr::keep(~(1 <= . & . <= 22))

chrs_num <- as.character(seq(1, 24))
class(chrs_num)
purrr::keep(chrs_num, ~(1 <= . & . <= 22))
=======
purrr::keep(chrs_auto, ~(0 < . & . < 2))

chrs_all <- c(seq(1, 22), "X")
class(chrs_all)
purrr::keep(chrs_all, ~(0 < . & . < 2))
chrs_all |> purrr::keep(~. != "X") |> purrr::keep(~(0 < . & . < 2))
>>>>>>> 75c6b010191824ed6df295427008bb2ca5ef3497


## Example 1 ==================================================================
### Extract the subset of interest --------------------------------------------
df1 <- DDR_SCORE01 |> 
  dplyr::filter(tune %in% c("3y3sBDP",  "キモプリBDP")) |> 
  print()

### First, do take a glance ---------------------------------------------------
p1 <- df1 |> 
  ggplot(aes(x = tune, y = SCORE, colour = tune)) +
  geom_boxplot(outliers = FALSE) +
  ggbeeswarm::geom_quasirandom() +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(
    aspect.ratio = 2, 
    legend.position = "none", 
    axis.text.x  = element_text(angle = 90, size = 12) )
(p1)

### Statistical test ----------------------------------------------------------
t.test(data = df1, SCORE ~ tune)
wilcox.test(data = df1, SCORE ~ tune)


## Example 2 ==================================================================
### Extract the subset of interest --------------------------------------------
df2 <- DDR_SCORE01 |> 
  dplyr::filter(tune %in% c("シルドリDDP", "トリジャニDDP")) |> 
  dplyr::mutate(tune = factor(tune, levels = c("トリジャニDDP", "シルドリDDP"))) |> 
  print()

### First, do take a glance ---------------------------------------------------
p2 <- df2 |> 
  ggplot(aes(x = tune, y = SCORE, colour = tune)) +
  geom_boxplot(outliers = FALSE) +
  ggbeeswarm::geom_quasirandom() +
  ggsci::scale_color_npg() +
  theme_classic() +
  theme(
    aspect.ratio = 2, 
    legend.position = "none", 
    axis.text.x  = element_text(angle = 90, size = 12) )
(p2)

### Statistical test ----------------------------------------------------------
t.test(data = df2, SCORE ~ tune)
wilcox.test(data = df2, SCORE ~ tune)


## Appendix ===================================================================
p3a <- df1 |> 
  ggplot(aes(x = tune, y = SCORE)) +
  geom_boxplot(outliers = FALSE) +
  ggbeeswarm::geom_quasirandom(aes(colour = factor(day))) +
  ggsci::scale_color_lancet() +
  theme_classic() +
  theme(
    aspect.ratio = 2, 
    legend.position = "none", 
    axis.text.x  = element_text(angle = 90, size = 12) )

p3b <- df2 |> 
  ggplot(aes(x = tune, y = SCORE)) +
  geom_boxplot(outliers = FALSE) +
  ggbeeswarm::geom_quasirandom(aes(colour = factor(day))) +
  ggsci::scale_color_lancet() +
  theme_classic() +
  theme(
    aspect.ratio = 2, 
    legend.position = "none", 
    axis.text.x  = element_text(angle = 90, size = 12) )

(p3a + p3b) # This operation errors out without `patchwork` package


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
#  [1] patchwork_1.3.2  ggsci_4.2.0      ggbeeswarm_0.7.3 lubridate_1.9.5 
#  [5] forcats_1.0.1    stringr_1.6.0    dplyr_1.2.0      purrr_1.2.1     
#  [9] readr_2.2.0      tidyr_1.3.2      tibble_3.3.1     ggplot2_4.0.2   
# [13] tidyverse_2.0.0 
# 
# loaded via a namespace (and not attached):
#  [1] bit_4.6.0          gtable_0.3.6       crayon_1.5.3      
#  [4] compiler_4.5.2     renv_1.1.7         tidyselect_1.2.1  
#  [7] parallel_4.5.2     scales_1.4.0       R6_2.6.1          
# [10] labeling_0.4.3     generics_0.1.4     pillar_1.11.1     
# [13] RColorBrewer_1.1-3 tzdb_0.5.0         rlang_1.1.7       
# [16] utf8_1.2.6         stringi_1.8.7      S7_0.2.1          
# [19] bit64_4.6.0-1      timechange_0.4.0   cli_3.6.5         
# [22] withr_3.0.2        magrittr_2.0.4     grid_4.5.2        
# [25] vroom_1.7.0        rstudioapi_0.18.0  hms_1.1.4         
# [28] beeswarm_0.4.0     lifecycle_1.0.5    vipor_0.4.7       
# [31] vctrs_0.7.2        glue_1.8.0         farver_2.1.2      
# [34] tools_4.5.2        pkgconfig_2.0.3


