# This script compares performance of our current Her2 stain to a new stain

# load libraries
library(tidyverse)
library(gt)

# import raw data
stain_raw <- read_csv("data/her2_cases.csv")

# stain_raw <- 
#   readxl::read_excel(
#     "data/her2_cases.xlsx", 
#     sheet = "Form1",
#     col_types = 
#       c(
#         "skip", "skip", "skip", "skip", "skip",
#         "text", "text", "text", "text", "text",
#         "numeric", "text", "text", "text", "numeric",
#         "numeric", "numeric", "numeric", "numeric", "numeric", 
#         "numeric", "numeric", "numeric", "numeric", "text"
#         
#       ))

# Dual Probe ISH Group Definitions:
# Group 1 = HER2/CEP17 ratio ≥2.0; ≥4.0 HER2 signals/cell
# Group 2 = HER2/CEP17 ratio ≥2.0; <4.0 HER2 signals/cell
# Group 3 = HER2/CEP17 ratio <2.0; ≥6.0 HER2 signals/cell
# Group 4 = HER2/CEP17 ratio <2.0; ≥4.0 and <6.0 HER2 signals/cell
# Group 5 = HER2/CEP17 ratio <2.0; <4.0 HER2 signals/cell

# Assign DISH group
stain_dish <- 
  stain_raw %>% 
  mutate(
    dish_grp = case_when(
      `Her2 Chr17 ratio2` >= 2.0 & `Her2 signal` >= 4.0 ~ 1,
      `Her2 Chr17 ratio2` >= 2.0 & `Her2 signal` < 4.0 ~ 2,
      `Her2 Chr17 ratio2` < 2.0 & `Her2 signal` >= 6.0 ~ 3,
      `Her2 Chr17 ratio2` < 2.0 & `Her2 signal` >= 4.0 & 
        `Her2 signal` < 6.0 ~ 4,
      `Her2 Chr17 ratio2` < 2.0 & `Her2 signal` < 4.0 ~ 5,
      TRUE ~ as.numeric(NA)
    ),
    dish_grp_z = case_when(
      `z_Her2 Chr17 ratio` >= 2.0 & `z_Her2 signal` >= 4.0 ~ 1,
      `z_Her2 Chr17 ratio` >= 2.0 & `z_Her2 signal` < 4.0 ~ 2,
      `z_Her2 Chr17 ratio` < 2.0 & `z_Her2 signal` >= 6.0 ~ 3,
      `z_Her2 Chr17 ratio` < 2.0 & `z_Her2 signal` >= 4.0 & 
        `Her2 signal` < 6.0 ~ 4,
      `z_Her2 Chr17 ratio` < 2.0 & `Her2 signal` < 4.0 ~ 5,
      TRUE ~ as.numeric(NA)
    )
  )

stain_dish %>% 
  select(
    Pathologist,
    Cytotech,
    `Case ID`,
    `IHC Concurrent`,
    `HER2/CEP17 current` = `Her2 Chr17 ratio2`,
    `HER2/CEP17 new` = `z_Her2 Chr17 ratio`,
    `CEP17 signal current` = `Chromosome 17 signal`,
    `CEP17 signal new` = `z_Chromosome 17 signal`,
    `HER2 signal current` = `Her2 signal`,
    `HER2 signal new` = `z_Her2 signal`,
    `DISH Group current` = dish_grp,
    `DISH Group new` = dish_grp_z,
    `comments (current)` = Her2_comments,
    `comments (new)` = z_Her2_comments
  ) %>% 
  gt() %>% 
  fmt_missing(
    columns = everything(),
    missing_text = "--"
  )
  
