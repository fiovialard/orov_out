# Code to generate report from interim database 

# Date: 09/09/2026

## load  package 
require("dataMaid") # make codebook
require(here) # select folders
require(tidyverse) #tidy data
require(readxl) # read excel files
require(labelled) # label data 

## load data and codebook
orov_clean <- readRDS(here::here("1_data","05_clean_data","orov_out_clean_final_2026-09-29.RData"))

orov_var_description <- read_xlsx(here::here("1_data/01_dict", "codebook_orov_out.xlsx"))


## add description to variable
# Keep only variables that exist in both, to avoid errors
dic <- orov_var_description[orov_var_description$var_name %in% names(orov_clean), ]

# Named list: names = columns, values = labels
labs <- setNames(as.list(dic$var_description), dic$var_name)

# Apply all labels at once
var_label(orov_clean) <- labs
var_label(orov_clean)


makeCodebook(orov_clean,tile = "OROV-OUT master database", replace = TRUE,
             file = here::here("1_data", "01_dict", "orov_out_codebook.Rmd"))
