# This script compares performance of our current Her2 stain to a new stain

# load libraries
library(tidyverse)

# import raw data
stain_raw <- readxl::read_excel("data/her2_cases.xlsx")
