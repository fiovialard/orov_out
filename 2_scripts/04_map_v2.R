## maps of arboviral cases 2024-2025 in the Americas with overlap of our own cases and map of reported arboviral cases

##by Fio Vialard

# Last updated: 22/09/2026



# 1. Set up  --------------------------------------------------------------

require(tidyverse) # tidy functions
require(sf) # for shapefile data
require(spData) # to transform shapefile data 
require(here) # to easily call sub-folders
require(cowplot) # combine multiple plots
# #install.packages("waffle")
# require(waffle)
require(RColorBrewer) # color palette
#install.packages("ggnewscale")
require(ggnewscale) # create a shared fill legend
require(patchwork) # combine plots with other elements (e.g. images)

# load our data 
l_orov <- readRDS(here::here("1_data","05_clean_data" ,"orov_out_descriptive_analysis_objects.RData"))

#load paho national data 
load(here::here("1_data", "04_paho","paho_data.RData"))

# load functions 
source(here::here("2_scripts", "00_functions.R"))

orov_clean <- l_orov$orov_clean

#summarize n_complications by site
df_summary <- orov_clean |>
  group_by(site2, complication2) |>
  summarise(n_complications = n())|>
  filter(complication2 !="Arthritis") # remove arthritis

df_summary_severe <- df_summary|>
  filter(complication2 %in% c("Haemorrhagic", "Severe disease"))

df_summary_pregnancy <- df_summary|>
  filter(complication2=="Pregnancy complication")


# load data from geoBoundaries
v_bad_bunny_iso <- c("ABW", "AIA", "ARG", "ATG", "BES", "BHS", "BLM", "BLZ", "BMU", "BOL", "BRA", "BRB", 
                 "CAN", "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", 
                 "GLP", "GRD", "GRL", "GTM", "GUF", "GUY", "HND", "HTI", "JAM", "KNA", "LCA",
                 "MEX", "MSR", "MTQ", "NIC", "PAN", "PER", "PRY", "SLV", "SUR", "TCA", "TTO", 
                 "URY", "USA", "VCT", "VEN", "VGB", "VIR")

l_bad_bunny_shapefiles <- lapply(
  v_bad_bunny_iso,
  function(x) {
    read_sf(
      file.path(
        here::here("1_data", "02_shapefiles"),
        paste0("geoBoundaries-", x, "-ADM0-all"),
        paste0("geoBoundaries-", x, "-ADM0_simplified.geojson")
      )
    )
  }
)




# add puerto rico and files with subnational boundaries 
v_adm1_iso <- c("BRA", "PER", "PAN")



l_shape_adm2_shapefiles <- lapply(
  v_adm1_iso,
  function(x) {
    read_sf(
      file.path(
        here::here("1_data", "02_shapefiles"),
        paste0("geoBoundaries-", x, "-ADM1-all"),
        paste0("geoBoundaries-", x, "-ADM1_simplified.geojson")
      )
    )
  }
)


pr_files <- read_sf(here::here("1_data", "02_shapefiles","geoBoundaries-PRI-ADM2-all/geoBoundaries-PRI-ADM2_simplified.geojson"))

pr_files 


# combine into one dataset 

df_shapefiles <- bind_rows(l_bad_bunny_shapefiles, .id = "iso") |>
  add_row(bind_rows(l_shape_adm2_shapefiles, .id = "iso")) |>
  add_row(pr_files)


bra_files <- read_sf(here::here("1_data", "02_shapefiles","geoBoundaries-BRA-ADM0-all/geoBoundaries-BRA-ADM0_simplified.geojson"))

bra_files_adm1 <- read_sf(here::here("1_data", "02_shapefiles","geoBoundaries-BRA-ADM1-all/geoBoundaries-BRA-ADM1_simplified.geojson")) 


# palette 
v_brewer_blues<- brewer.pal(n = 5, name = "Blues")

# keep only national files 
df_shapefiles_national <- df_shapefiles |>
  filter(shapeType=="ADM0")





# 2. Add data cases from PAHO and cases from case series-------------------------------------------
#PAHO cases soruces:  https://www.paho.org/sites/default/files/2025-08/2025-aug-13-epi-update-oropouche.pdf (2025)
# https://www.paho.org/sites/default/files/2025-02/2025-feb-11-actualizacion-epi-oropouche-esfinal.pdf (2024)



df_shapefiles <- df_shapefiles |>
  mutate(n_2025 =
           case_when(
             shapeISO == "VEN" ~ 5,
             shapeISO =="COL" ~ 26,
             shapeISO == "CUB" ~ 28,
             shapeISO =="GUY" ~ 1,
             shapeISO == "PER" ~ 330,
             shapeISO == "BRA" ~ 11888, 
             shapeISO == "PAN" ~ 501,
             shapeISO == "BR-AL" ~5,
             shapeISO == "BR-AP" ~ 99,
             shapeISO == "BR-BA" ~ 11,
             shapeISO == "BR-CE" ~ 693,
             shapeISO%in% c("BR-DF", "BR-MS", "BR-PI", "BR-RR") ~ 1,
             shapeISO == "BR-ES" ~ 6322,
             shapeISO == "BR-MG" ~ 1366,
             shapeISO == "BR-PA" ~ 10, 
             shapeISO == "BR-PR" ~ 47, 
             shapeISO == "BR-PB" ~645,
             shapeISO == "BR-PE" ~2,
             shapeISO == "BR-RJ" ~ 2497,
             shapeISO == "BR-RO" ~ 7,
             shapeISO == "BR-SC" ~15,
             shapeISO == "BR-SP" ~143,
             shapeISO == "BR-TO" ~22,
             shapeISO == "PA-5" ~265, 
             shapeISO == "PA-8" ~236,
             shapeISO == "PE-LOR" ~ 164,
             shapeISO == "PE-JUN" ~ 74, 
             shapeISO == "PE-AYA" ~ 38,
             shapeISO == "PE-CUS" ~ 23,
             shapeISO == "PE-SAM" ~ 20, 
             shapeISO == "PE-UCA" ~ 7,
             shapeISO == "PE-CAJ" ~3,
             shapeISO == "PE-AMA" ~1,
             T ~NA 
           ),
         n_2024 = case_when(
           shapeISO == "BRB" ~2,
         shapeISO =="BOL" ~ 356,
         shapeISO == "BRA" ~ 13785,
         shapeISO == "COL" ~ 74,
         shapeISO == "CUB" ~ 626,
         shapeISO == "ECU" ~ 3,
         shapeISO == "GUY" ~ 3,
         shapeISO =="PAN" ~ 16,
         shapeISO == "PER" ~  1263,
         shapeISO == "BR-AL" ~ 120, 
         shapeISO == "BR-AM" ~ 3231,
         shapeISO == "BR-AP" ~ 128,
         shapeISO == "BR-AC" ~273, 
         shapeISO == "BR-BA" ~  891,
         shapeISO == "BR-RR" ~278,
         shapeISO == "BR-CE" ~ 257,
         shapeISO == "BR-MS" ~ 1,
         shapeISO == "BR-ES" ~  5864,
         shapeISO == "BR-MA" ~33, 
         shapeISO == "BR-MG" ~  246,
         shapeISO == "BR-MT" ~18,
         shapeISO == "BR-PA" ~ 172, 
         shapeISO == "BR-PB" ~ 6,
         shapeISO == "BR-PE" ~ 146,
         shapeISO == "BR-PI" ~ 30,
         shapeISO == "BR-RJ" ~ 151,
         shapeISO == "BR-RO" ~ 1711,
         shapeISO == "BR-SE" ~ 34,
         shapeISO == "BR-SC" ~ 178,
         shapeISO == "BR-SP" ~ 9,
         shapeISO == "BR-TO" ~ 8,
         shapeISO == "PA-5" ~ 16,
         shapeISO == "PE-LOR" ~ 729,
         shapeISO == "PE-MDD" ~ 313,
         shapeISO == "PE-HUC" ~ 40,
         shapeISO == "PE-JUN" ~ 21, 
         shapeISO == "PE-SAM" ~ 18, 
         shapeISO %in% c("PE-TUM", "PE-CUS", "PE-PUN") ~ 1,
         shapeISO == "PE-UCA" ~ 139,
         T ~NA ),
         n_total =  rowSums(across(c(n_2024, n_2025)), na.rm = TRUE),
         n_total = ifelse(n_total ==0, NA, n_total),
         n_cases = case_when(
           shapeISO == "BR-PB" ~ sum(orov_clean$site2=="Paraíba"),
           shapeISO == "BR-CE" ~ sum(orov_clean$site2=="Ceará"),
           shapeISO == "PA-5" ~ sum(orov_clean$site2=="Darién"),
           shapeISO == "BR-ES" ~ sum(orov_clean$site2=="Espirito Santo"),
           shapeISO == "BR-PE" ~ sum(orov_clean$site2=="Pernambuco"),
           shapeISO == "BR-RJ" ~ sum(orov_clean$site2=="Rio de Janeiro"),
           shapeISO == "BR-AM" ~ sum(orov_clean$site2=="Amazonas"),
           shapeISO == "PE-LOR" ~sum(orov_clean$site2=="Loreto"),
           T ~NA
         ))

# remove GRL, US and CAN for now 

df_shapefiles<- df_shapefiles |>
  filter(!shapeISO %in% c( "CAN", "USA", "GRL"))
  

# checks that total cases == subunit cases
df_shapefiles |>   filter(shapeISO=="BRA") |> pull(n_2025) == sum(df_shapefiles |>   filter(shapeGroup=="BRA" & shapeType == "ADM1") |> pull(n_2025), na.rm = T)
df_shapefiles |>   filter(shapeISO=="BRA") |> pull(n_2024) == sum(df_shapefiles |>   filter(shapeGroup=="BRA" & shapeType == "ADM1") |> pull(n_2024), na.rm = T)

df_shapefiles |>   filter(shapeISO=="PER") |> pull(n_2025) == sum(df_shapefiles |>   filter(shapeGroup=="PER" & shapeType == "ADM1") |> pull(n_2025), na.rm = T)
df_shapefiles |>   filter(shapeISO=="PER") |> pull(n_2024) == sum(df_shapefiles |>   filter(shapeGroup=="PER" & shapeType == "ADM1") |> pull(n_2024), na.rm = T)

df_shapefiles |>   filter(shapeISO=="PAN") |> pull(n_2025) == sum(df_shapefiles |>   filter(shapeGroup=="PAN" & shapeType == "ADM1") |> pull(n_2025), na.rm = T)
df_shapefiles |>   filter(shapeISO=="PAN") |> pull(n_2024) == sum(df_shapefiles |>   filter(shapeGroup=="PAN" & shapeType == "ADM1") |> pull(n_2024), na.rm = T)

## Additional administrative sub-region to add 
#get extra adm1 shapefiles 
v_adm1_iso_extra <- c("VEN", "COL", "GUY", "BOL", "CUB", "ECU")


l_shape_adm2_shapefiles_extra <- lapply(
  v_adm1_iso_extra,
  function(x) {
    read_sf(
      file.path(
        here::here("1_data", "02_shapefiles"),
        paste0("geoBoundaries-", x, "-ADM1-all"),
        paste0("geoBoundaries-", x, "-ADM1_simplified.geojson")
      )
    )
  }
)


df_shapefiles_new <- df_shapefiles |>
  add_row(bind_rows(l_shape_adm2_shapefiles_extra, .id = "iso"))

# add subnational cases 
# note: VEN did not specify the region so kept national, CUB only gave breakdown of 55% of cases in 2024

df_shapefiles_new <- df_shapefiles_new |>
  mutate(n_2025 = case_when(
    shapeISO == "CO-VAU" ~ 19,
    shapeISO == "CO-TOL" ~4,
    shapeISO == "CO-COR" ~ 2,
    shapeISO == "CO-HUI" ~1,
    shapeISO == "CU-01" ~ 6,
    shapeISO == "CU-05" ~5,
    shapeISO == "CU-14" ~4, 
    shapeISO %in% c("CU-03", "CU-04") ~ 3,
    shapeISO == "CU-15" ~ 2,
    shapeISO %in%c("CU-06", "CU-08", "CU-09", "CU-10", "CU-13") ~ 1,
    shapeISO == "GY-MA" ~1,
    T ~ n_2025
  ),
  n_2024 = case_when(
    shapeISO == "BO-L" ~ 268,
    shapeISO == "BO-B" ~ 76,
    shapeISO == "BO-N" ~12,
    shapeISO == "CO-AMA" ~ 70,
    shapeISO %in% c("CO-CAQ", "CO-MET") ~1,
    shapeISO == "CU-03" ~ 176,
    shapeISO == "CU-13" ~ 75,
    shapeISO == "CU-01" ~ 49,
    shapeISO == "CU-06" ~ 40,
    shapeISO %in% c("CU-04", "CU-05") ~38,
    shapeISO %in% c("EC-S", "EC-B", "EC-R") ~ 1,
    shapeISO == "GY-MA" ~3,
    T ~ n_2024
  ),
  n_total =  rowSums(across(c(n_2024, n_2025)), na.rm = TRUE),
  n_total = ifelse(n_total ==0, NA, n_total),
  n_cases_severe =  case_when(
    shapeISO == "BR-CE" ~ sum(df_summary_severe[df_summary_severe$site2=="Ceará","n_complications"]),
    shapeISO == "PA-5" ~ sum(df_summary_severe[df_summary_severe$site2=="Darién","n_complications"]),
    shapeISO == "BR-ES" ~ sum(df_summary_severe[df_summary_severe$site2=="Espirito Santo","n_complications"]),
    shapeISO == "BR-RJ" ~ sum(df_summary_severe[df_summary_severe$site2=="Rio de Janeiro","n_complications"]),
    shapeISO == "BR-AM" ~ sum(df_summary_severe[df_summary_severe$site2=="Amazonas","n_complications"]),
    shapeISO == "PE-LOR" ~sum(df_summary_severe[df_summary_severe$site2=="Loreto","n_complications"]),
    T ~NA
  ),
  n_cases_preg =  case_when(
    shapeISO == "BR-PB" ~ pull(df_summary_pregnancy[df_summary_pregnancy$site2=="Paraíba","n_complications"]),
    shapeISO == "BR-CE" ~ pull(df_summary_pregnancy[df_summary_pregnancy$site2=="Ceará","n_complications"]),
    shapeISO == "BR-ES" ~ pull(df_summary_pregnancy[df_summary_pregnancy$site2=="Espirito Santo","n_complications"]),
    shapeISO == "BR-PE" ~ pull(df_summary_pregnancy[df_summary_pregnancy$site2=="Pernambuco","n_complications"]),
    shapeISO == "PE-LOR" ~sum(df_summary_pregnancy[df_summary_pregnancy$site2=="Loreto","n_complications"]),
    T ~NA))

# check that all cases haev been correctly imputed 
df_shapefiles_new |>   filter(shapeISO=="COL") |> pull(n_2025) == sum(df_shapefiles_new |>   filter(shapeGroup=="COL" & shapeType == "ADM1") |> pull(n_2025), na.rm = T)
df_shapefiles_new |>   filter(shapeISO=="COL") |> pull(n_2024) == sum(df_shapefiles_new |>   filter(shapeGroup=="COL" & shapeType == "ADM1") |> pull(n_2024), na.rm = T) + 2 # imported cases 


df_shapefiles_new |>   filter(shapeISO=="ECU") |> pull(n_2024) == sum(df_shapefiles_new |>   filter(shapeGroup=="ECU" & shapeType == "ADM1") |> pull(n_2024), na.rm = T) 


df_shapefiles_new |>   filter(shapeISO=="GUY") |> pull(n_2025) == sum(df_shapefiles_new |>   filter(shapeGroup=="GUY" & shapeType == "ADM1") |> pull(n_2025), na.rm = T)
df_shapefiles_new |>   filter(shapeISO=="GUY") |> pull(n_2024) == sum(df_shapefiles_new |>   filter(shapeGroup=="GUY" & shapeType == "ADM1") |> pull(n_2024), na.rm = T)

df_shapefiles_new |>   filter(shapeISO=="CUB") |> pull(n_2025) == sum(df_shapefiles_new |>   filter(shapeGroup=="CUB" & shapeType == "ADM1") |> pull(n_2025), na.rm = T)

df_shapefiles_new |>   filter(shapeISO=="BOL") |> pull(n_2024) == sum(df_shapefiles_new |>   filter(shapeGroup=="BOL" & shapeType == "ADM1") |> pull(n_2024), na.rm = T)







# 4.Fig1 map ----------------------------------------------------
df_map <- df_shapefiles_new |>
filter(!shapeISO %in% c("BRA", "PAN", "PER", "USA", "ECU", "BOL", "GUY", "COL"), # remove countries adm0 for which we have subunits
       !(shapeGroup %in% c("VEN", "CUB") & shapeType == "ADM1"))|>  # remove subunits for venezuela and cuba because we dont have/want that level of granularity
  mutate(n_log = log(n_total))|> # create log scale
  mutate(n_total_cat = factor(case_when( # create color categories 
    n_total < 100 ~ "<100",
    n_total >= 100 & n_total <= 500 ~ "100-500",
    n_total > 500 & n_total <= 1000 ~ "501-1000",
    n_total > 1000 & n_total <= 5000 ~ "1001-5000",
    n_total > 5000 ~ ">5000",
    T ~ NA), levels = c("<100", "100-500", "501-1000", "1001-5000", ">5000")
  ))

# create new map 


map <- map_continent(data = df_map , fill_var = "n_total_cat",
                     fill_scale = scale_fill_brewer(palette = "Blues", na.value = "lightgrey", 
                                                    breaks = c("<100", "100-500", "501-1000", "1001-5000", ">5000"),
                                                    guide = guide_legend(order = 1)),
                     
)
map



## Create bubbles for n of cases by site

# Per-site bubble offsets from centroid (positive x = right, negative x = left)
# label_x_offset: nudges label relative to bubble (+ = right, - = left)
# hjust: 0 = label to the right of anchor, 1 = label to the left of anchor
df_bubble_offsets <- tribble(
  ~shapeName,              ~x_pos, ~y_offset, ~label,              ~label_y_offset, ~hjust, ~ x_offset,
  "Amazonas",               -83,       -10,       "Amazonas",           -2,              1,  -2, 
  "Ceara",                  -33,          4,        "Ceará",               -2.5,              0, 3, 
  "Paraiba",                -33,          0,        "Paraíba",             -2.5,              0, 0.5, 
  "Pernambuco",             -33,          -3.5,        "Pernambuco",          -2.5,              0, 0.5,
  "Espirito Santo",         -33,          0,        "Espírito Santo",      -3.5,              0, 3.5,
  "Rio de Jeneiro",         -33,          -3,        "Rio de Janeiro",      -2,              0, 2,
  "Loreto",                 -83,        0,        "Loreto",             -2,              1, -2,
  "Provincia de Darién",    -83,        -4,        "Darién",             -2,              1, -2
)

df_bubble_offsets <- df_bubble_offsets |>
  mutate(label_x_offset=0)

# apply out theme to the mao and add annotations with complications 
map_rev <- apply_theme(map, offset_tab = df_bubble_offsets,
                       label_data = df_map |> filter(!is.na(n_cases) & n_cases > 0)) +
  geom_point(data = df_map |>
                filter(!is.na(n_cases) & n_cases > 0) |>
                left_join(df_bubble_offsets, by = "shapeName") |>
                mutate(x = x_pos,
                       y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
                aes(x=x, y = y, size = n_cases),
              shape = 21, stroke =1, fill = "white") + # shape and thickness of the bubble
   guides(size = guide_legend(order = 2)) +
  scale_size_continuous(name = "Cases included",range = c(3,12), breaks = c(50,100,250,500,1250), # create scale for included cases
                        guide = guide_legend(ncol = 1)) +
   ggnewscale::new_scale(new_aes = "size" ) + # reset the scale
   geom_point(data = df_map |>
                filter(!is.na(n_cases_severe) & n_cases_severe > 0) |> # add severe cases
                left_join(df_bubble_offsets, by = "shapeName") |>
                mutate(x = x_pos + x_offset,
                       y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
              aes(x=x, y = y, size = n_cases_severe),
              color = "#801515")+
   scale_size_continuous(name = "Haemorrhagic manifestations and/or severe disease", # renamed in second round of revisions
                         range=c(1,3), breaks = c(1,5,25), # scale for severe cases
                         guide = guide_legend(ncol = 1, order = 3))+
   ggnewscale::new_scale(new_aes = "size" ) + # reset the scale
   geom_point(data = df_map |>
                filter(!is.na(n_cases_preg) & n_cases_preg > 0) |> # add pregnacy cases
                left_join(df_bubble_offsets, by = "shapeName") |>
                mutate(x = x_pos + x_offset + 2 * sign(x_offset), #add more space in the direction of the offset
                       y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
              aes(x=x, y = y, size = n_cases_preg),
              color = "#D46A6A")+
   scale_size_continuous(name = "Adverse pregnancy outcome cases", range=c(1,2), breaks = c(1,5), # scale for severe cases
                         guide = guide_legend(ncol = 1, order = 4))+
   theme(
     legend.text = element_text(size = 8),
     legend.title = element_text(size = 8),
     legend.key.size = unit(0.4, "cm"),
     legend.position = "left")


 map_rev
 
 

# 5. Fig S1 arbo maps  -------------------------------------------------------

## arboviral disease map
# change to wide format
# incidence rates, separate map for 4 diseases
 
 # select the data of interest
 df_national_clean <- df_national |>
   select(Country.or.Subregion, Year, cumulative_incidence, disease)|>
   pivot_wider(names_from = c(disease, Year), values_from = cumulative_incidence)|>
   mutate(total= rowSums(
     pick(ends_with("_2025")),
     na.rm = TRUE
   ))
          
# clean country names           
 pattern <- regex("Bolivia|Panama|Venezuela|Dominica(?!n)|Chile", ignore_case = TRUE)
 
 df_shapefiles_national <- df_shapefiles_national |>
   mutate(
     shapeName = case_when(
       str_detect(shapeName, regex(pattern, ignore_case = TRUE)) ~ str_extract(shapeName, regex(pattern, ignore_case = TRUE)),
       shapeName == "the Argentine Republic" ~ "Argentina",
       shapeName == "Curacao" ~ "Curaçao",
       T ~shapeName
     )
   )

 # join both datasets together
df_shapefiles_national_combined<-  df_shapefiles_national |>
   left_join(df_national_clean, by = c(shapeName="Country.or.Subregion"))|>
  filter(!shapeISO %in% c("USA", "CAN", "GRL"))|>
  # transform 0 so that it does not introduce errors
  mutate(across(matches("_2025$|^total$"), ~ ifelse(. == 0, NA, .)))
  
# transform df_bubble_offset so that the label isn't moved down
# df_bubble_offsets2 <- df_bubble_offsets |>
#   mutate(label_y_offset = 0)

# create a shared fill scale object 

shared_fill_scale <- scale_fill_distiller(
  palette = "OrRd",
  trans = "log",
  limits = range(df_shapefiles_national_combined$total, na.rm = TRUE),
  direction = 1,
  labels = scales::label_number(),
  na.value = "lightgrey",
  breaks = c(0.14, 2.5, 55, 3500)
)


# OROV map ----------------------------------------------------------------


# OROV MAP
orov_map <- map_continent(df_shapefiles_national_combined, 
                          fill_var = "orov_2025",
                          legend = "Reported cumulative OROV incidence rate \n2024-2025 (per 100 000)",
                          fill_scale =  shared_fill_scale,
                          show_adm1 = F)+
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "left"
  )

  

l_arbomaps <-  list(orov_map)


# CHIKV map ---------------------------------------------------------------



## chikv 
chikv_map <- map_continent(df_shapefiles_national_combined, 
                          fill_var = "chikv_2025",
                          legend = "Reported cumulative CHIKV incidence rate \n2024-2025 (per 100 000)",
                          fill_scale = shared_fill_scale,
                          show_adm1 = F)+
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "left"
  )



l_arbomaps <-  c(l_arbomaps, list(chikv_map))



# ZIKV map ----------------------------------------------------------------


zikv_map <- map_continent(df_shapefiles_national_combined, 
                           fill_var = "zikv_2025",
                           legend = "Reported cumulative ZIKV incidence rate \n2024-2025 (per 100 000)",
                           fill_scale = shared_fill_scale,
                          show_adm1 = F)+
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "left"
  )



l_arbomaps <- c(l_arbomaps, list(zikv_map))


# DENV map ----------------------------------------------------------------



## denv map 

denv_map <- map_continent(df_shapefiles_national_combined, 
                           fill_var = "denv_2025",
                           legend = "Reported cumulative DENV incidence rate \n2024-2025 (per 100 000)",
                           fill_scale = shared_fill_scale,
                          show_adm1 = F)+
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "left"
  )



l_arbomaps<- c(l_arbomaps,list(denv_map))


names(l_arbomaps) <- c("orov", "chikv", "zikv", "denv")

## save all on same grid 
grid_arbomaps <- plot_grid(
  orov_map,
  zikv_map,
  chikv_map,
  denv_map,
  labels = "auto" # lowercase
)




# 9. Save  ----------------------------------------------------------------

# save the amount of cases reported for admin units of site in a table

l_map <-  with(
  df_shapefiles_new |>
    filter(!is.na(n_cases) | shapeISO == "BR-MG"| shapeISO=="BR-SP"),
  setNames(
    split(
      data.frame(n_total, n_cases, n_cases_severe, n_cases_preg),
      seq_len(nrow(data.frame(n_total, n_cases, n_cases_severe, n_cases_preg)))
    ),
    shapeISO
  )
)


saveRDS(l_map, file = here::here("1_data",  "05_clean_data" ,"orov_out_case_reported_paho.RData"))

# post-revision figures after two rounds of revision

## save rev2 
ggsave(filename = here::here("4_outputs", "fig1_rev2.pdf"),
       plot = map_rev,
       device = "pdf",
       width = 300,
       height = 200,
       units = c("mm"),
       dpi = 300
)

ggsave(filename = here::here("4_outputs", "fig1_rev2.svg"),
       plot = map_rev,
       device = "svg",
       width = 300,
       height = 200,
       units = c("mm"),
       dpi = 300
)

#save arbomaps separately and together
for (i in names(l_arbomaps)) {
  ggsave(
    filename = here::here("4_outputs", paste0("fig_s1_rev2_", i, ".svg")),
    plot = l_arbomaps[[i]],
    device = "svg",
    height = 200,
    width = 300,
    units = c("mm")
  )
}


# together 
ggsave(filename = here::here("4_outputs", "fig_s1_rev2.pdf"),
       plot = grid_arbomaps,
       device = "pdf",
       width = 300,
       height = 200,
       units = c("mm"),
       dpi = 300
)

ggsave(filename = here::here("4_outputs", "fig_s1_rev2.svg"),
       plot = grid_arbomaps,
       device = "svg",
       width = 300,
       height = 200,
       units = c("mm"),
       dpi = 300
)


# EXTRA- Outputs for PhD symposium  ----------------------------------------------

## extract geographic limits of the Amazon 
amazon_basin <- st_read(here::here("1_data", "02_shapefiles", "Limites_RAISG_2025", "Lim_Biogeografico.shp"))

# make sure the crs match
amazon_basin <- st_transform(amazon_basin, st_crs(df_map))

amazon_outline <- st_cast(amazon_basin, "MULTILINESTRING")

# add to map 
map_cases <- map +
  geom_sf(data = amazon_outline, color = "darkgreen", linewidth = 0.8)+
  annotate("text", x = -45, y = 7, label = "Amazon limits (RAISG 2025)", size = 5, color = "darkgreen")+
  theme(legend.position = "none")

map_cases

ggsave(filename = here::here("3_intermediate", "presentations","map_cases.pdf"),
       plot = map_cases,
       device = "pdf",
       width = 400,
       height = 300,
       units = c("mm"),
       dpi = 300
)

names(l_map) <- c("br_am", "br_ce", "br_pb", "br_pe", "br_mg", "br_es", "br_rj", "br_sp", "pe_lor", "pa_dar")

df_bubble_offsets_2 <- tribble(
  ~shapeName,              ~x_pos, ~y_offset, ~label,              ~label_y_offset, ~hjust, ~ label_x_offset, ~x_offset,
  "Amazonas",               -83,       -10,       paste0("Amazonas", " (n=",pull(l_map$br_am[2]), ")"),           -2,              1,  0, -2.5,
  "Ceara",                  -33,          6,        paste0("Ceará", " (n=",pull(l_map$br_ce[2]), ")"),               -2.5,              0, 2, 3.5, 
  "Paraiba",                -33,          0,        paste0("Paraíba", "\n(n=",pull(l_map$br_pb[2]), ")"),             -2.5,              0, 2, 1,
  "Pernambuco",             -33,          -6,        paste0("Pernambuco", "\n(n=",pull(l_map$br_pe[2]), ")"),          -2.5,              0, 2, 1,
  "Espirito Santo",         -33,          -2.5,        paste0("Espírito Santo","\n(n=",pull(l_map$br_es[2]), ")"),       -3.5,              0, 2, 4,
  "Rio de Jeneiro",         -33,          -7,        paste0("Rio de Janeiro", "\n(n=",pull(l_map$br_rj[2]), ")"),      -2.5,              0, 2,2.5,
  "Loreto",                 -83,        0,        paste0("Loreto"," (n=",pull(l_map$pe_lor[2]), ")") ,             -2,              1, 0, -2.5,
  "Provincia de Darién",    -83,        -4,        paste0("Darién", " (n=",pull(l_map$pa_dar[2]), ")"),             -2,              1, 0, -2.5
)


##add sites and n 
map_sites <- apply_theme(map_cases, offset_tab = df_bubble_offsets_2,
                       label_data = df_map |> filter(!is.na(n_cases) & n_cases > 0),
                       set_size_lab = 5,
                       set_size_legend = 12) +
  geom_point(data = df_map |>
               filter(!is.na(n_cases) & n_cases > 0) |>
               left_join(df_bubble_offsets_2, by = "shapeName") |>
               mutate(x = x_pos,
                      y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
             aes(x=x, y = y, size = n_cases),
             shape = 21, stroke =1, fill = "white") + # shape and thickness of the bubble
  guides(size = guide_legend(order = 2)) +
  scale_size_continuous(name = "Cases included",range = c(5,14), breaks = c(50,100,250,500,1250), # create scale for included cases, range = size of smallest and biggest bubble
                        guide = guide_legend(ncol = 1)) +
  ggnewscale::new_scale(new_aes = "size" )+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-120, -10), expand = TRUE)

map_sites

ggsave(filename = here::here("3_intermediate", "presentations","map_sites.pdf"),
       plot = map_sites,
       device = "pdf",
       width = 400,
       height = 300,
       units = c("mm"),
       dpi = 300
)

# add complication + legend 
map_compl <- map_sites+
geom_point(data = df_map |>
             filter(!is.na(n_cases_severe) & n_cases_severe > 0) |> # add severe cases
             left_join(df_bubble_offsets_2, by = "shapeName") |>
             mutate(x = x_pos + x_offset,
                    y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
           aes(x=x, y = y, size = n_cases_severe),
           color = "#801515")+
  scale_size_continuous(name = "Haemorrhagic manifestations and/or severe disease", # renamed in second round of revisions
                        range=c(2,4), breaks = c(1,5,25), # scale for severe cases
                        guide = guide_legend(ncol = 1, order = 3))+
  ggnewscale::new_scale(new_aes = "size" ) + # reset the scale
  geom_point(data = df_map |>
               filter(!is.na(n_cases_preg) & n_cases_preg > 0) |> # add pregnacy cases
               left_join(df_bubble_offsets_2, by = "shapeName") |>
               mutate(x = x_pos + x_offset + 2 * sign(x_offset), #add more space in the direction of the offset
                      y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
             aes(x=x, y = y, size = n_cases_preg),
             color = "#D46A6A")+
  scale_size_continuous(name = "Adverse pregnancy outcome cases", range=c(2,3), breaks = c(1,5), # scale for severe cases
                        guide = guide_legend(ncol = 1, order = 4))

map_compl



ggsave(filename = here::here("3_intermediate", "presentations","map_compl.pdf"),
       plot = map_compl,
       device = "pdf",
       width = 400,
       height = 300,
       units = c("mm"),
       dpi = 300
)



# EXTRA - outputs for Linkedin post --------------------------------------

# start with empty map + amazon limits 

map_li <- map_continent(data = df_map , fill_var = "n_total_cat",
                     fill_scale = scale_fill_brewer(palette = "Blues", na.value = "lightgrey", 
                                                    breaks = c("<100", "100-500", "501-1000", "1001-5000", ">5000"),
                                                    guide = guide_legend(order = 1)),
                     legend = "PAHO-reported cases from 2024-2025"
)+
  geom_sf(data = amazon_outline, color = "darkgreen", linewidth = 0.8)+
  annotate("text", x = -45, y = 7, label = "Endemic area", size = 5, color = "darkgreen")+
  theme(legend.position = "left")

map_li

# extract legend 
# get legends of plots 
legend_paho <- get_legend(map_li)




# add bubbles
map_li_annotated <- apply_theme(map_li, offset_tab = df_bubble_offsets,
                       label_data = df_map |> filter(!is.na(n_cases) & n_cases > 0),
                       set_size_lab = 5) +
  geom_point(data = df_map |>
               filter(!is.na(n_cases) & n_cases > 0) |>
               left_join(df_bubble_offsets, by = "shapeName") |>
               mutate(x = x_pos,
                      y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
             aes(x=x, y = y, size = n_cases),
             shape = 21, stroke =1, fill = "white") + # shape and thickness of the bubble
  guides(size = guide_legend(order = 2)) +
  scale_size_continuous(name = "Number of included cases",
                        range = c(2,14), breaks = c(1,5,25,50,100,250,500,1250), # create scale for included cases
                        limits = c(1, 1300),  
                        guide = guide_legend(ncol = 1))+
  coord_sf(xlim = c(-120, -10), expand = TRUE)

map_li_annotated

#get legend 
# get legends of plots 
legend_bubbles <- get_legend(map_li_annotated)

# add adverse cases and remove legends
map_li_final <- map_li_annotated+
  geom_point(data = df_map |>
               filter(!is.na(n_cases_severe) & n_cases_severe > 0) |> # add severe cases
               left_join(df_bubble_offsets, by = "shapeName") |>
               mutate(x = x_pos + x_offset,
                      y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
             aes(x=x, y = y, size = n_cases_severe),
             color = "#801515")+
  geom_point(data = df_map |>
               filter(!is.na(n_cases_preg) & n_cases_preg > 0) |> # add pregnacy cases
               left_join(df_bubble_offsets, by = "shapeName") |>
               mutate(x = x_pos + x_offset + 2 * sign(x_offset), #add more space in the direction of the offset
                      y = st_centroid(geometry) %>% st_coordinates(.) %>% .[,2] + y_offset),
             aes(x=x, y = y, size = n_cases_preg),
             color = "#D46A6A")+
  theme(legend.position = "none")

map_li_final  


ggsave(filename = here::here("3_intermediate", "presentations","legend_linked_in1.pdf"),
       plot = legend_paho,
       device = "pdf",
       width = 400,
       height = 300,
       units = c("mm"),
       dpi = 300
)

ggsave(filename = here::here("3_intermediate", "presentations","legend_linked_in2.pdf"),
       plot = legend_bubbles,
       device = "pdf",
       width = 400,
       height = 300,
       units = c("mm"),
       dpi = 300
)

ggsave(filename = here::here("3_intermediate", "presentations","map_linked_in.pdf"),
       plot = map_li_final,
       device = "pdf",
       width = 400,
       height = 300,
       units = c("mm"),
       dpi = 300
)


