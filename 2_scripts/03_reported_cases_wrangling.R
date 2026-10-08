## loading and wranging of all PAHO ARBO reported cases 

##by Fio Vialard

# Last updated: 03/06/2026



# 1. Set-up ---------------------------------------------------------------

# load packages 

# load packages
if (!requireNamespace("pacman", quietly = TRUE)){install.packages("pacman")}

require(pacman)
pacman::p_load(
  tidyverse, # for data wrangling
  cowplot, # to arrange figures 
  flextable, # for customizable  table
  readxl, # to read excel files
  here,# to find the absolute path of the project
  stringi) # clean names

# read .csv files 

# create filenames
prefix <- c(rep("chikv_",2), rep("denv_",2), rep("zikv_",2),  rep("orov_",2))

suffix<- rep(paste0(2024:2025,"_national"),length(prefix)/2)


v_filenames <- paste0(prefix, suffix)

# read all files 
l_national_files <- lapply(v_filenames, function(f) {
  read.csv(here::here("1_data/04_paho", paste0(f, ".csv")), colClasses = "character")
})

names(l_national_files) <- v_filenames # keep the old names


# 2. Combine all files --------------------------------------------------------

# standardise column names before binding
standardize_cols <- function(df) {
  rename_map <- c(
    "País.o.Subregión"   = "País.o.Subregion",            # denv missing accent
    "Year"               = "Año",                          # denv uses Spanish
    "Población.x.1000"   = "Población.X.1000",             # denv capitalised X
    "Total.de.Casos..b." = "Total.de.Casos.de.Dengue..b.", # denv disease-specific name
    "CFR..f."            = "Letalidad..f.",                 # chikv vs denv/zikv
    "Legend.Ranges"      = "Lengend.Ranges",                # typo in chikv
    "confirmed_cases"    = "Casos.Confirmados",             # chikv
    "confirmed_cases"    = "Confirmed",                     # denv
    "confirmed_cases"    = "Confirmados",                   # zikv
    "suspected_cases"    = "Suspected.Cases..b.",           # chikv
    "suspected_cases"    = "Casos.Sospechosos..b.",        # zikv 
  "Year.Filter" = "Año..Filtro.",
  "incidence" = "Incidencia.Acumulada...c.",
  "incidence" = "Incidencia.Acumulada..c.",
  "incidence" = "Tasa.de.Incidencia..c.",
  "imported" = "Casos.Importados",
  "imported" = "Importados"
    )
  # only rename columns that exist in this df
  to_rename <- rename_map[rename_map %in% names(df)]
  if (length(to_rename) > 0) rename(df, all_of(to_rename)) else df
}

l_national_files <- lapply(l_national_files, standardize_cols)

l_national_files[1:6] <- lapply(
  l_national_files[1:6],
  \(x) mutate(
    x,
    Country.or.Subregion = str_remove(Country.or.Subregion, "\\*$")
  )
)

# combine all files by country 
df_national <- bind_rows(l_national_files[1:6], .id = "source") |>
  mutate(disease = str_extract(source, "^[^_]+")) |>
  select(-source) |>
  type_convert() |>
  select(-c(País.o.Subregión, Fecha.del.Último.Reporte, Fecha.Último.Reporte,
            Último.Caso.Reportado,In...Out.of.Subregions, Year.Filter,
            Cat.Cumulative.Incidence, End.Date, Last_Case, Last_Report, Notes, Reference,
            StartDate, Type, Última.Semana.Epidemiológica.Reportada..a., X0, Countries.not.reporting,
            EpiMonth, EpiYear, LCREW, Legend.Ranges,Measures.Filter,Epidemiological.Week..a.,
            Total.Countries, X.C.TC..x100..e.,Number.of.Records, Cat.Incidencia.Acumulada,Última.Semana.Epi.Reportada..a.,
           Sospechosos, Suspected)) |> # remove columns that are not useful 
rename(cfr = CFR..f.,
       deaths = Muertes,
       population_x_1000 = Población.x.1000,
       total_cases = Total.de.Casos..b.,
       denv_serotype = Serotipo,
       prop_severe_denv = X.DG.D..x100..e.,
       severe_denv = Dengue.Grave..d.,
       ) |>
  mutate(suspected_cases = case_when(
    disease == "denv" ~total_cases,
    T ~ suspected_cases
  )) |>
  filter(!is.na(Iso3166)) # remove totals by subregions

sum(rowSums(table(df_national$Country.or.Subregion, df_national$disease))) <= length(unique(df_national$Country.or.Subregion))*6
#the total rowsums for each country should not exceed 6 (3 diseases, each with values for 2024 & 2025) times the number of countries included


# combine df_orov separately 
df_orov <- bind_rows(l_national_files$orov_2024_national, l_national_files$orov_2025_national)

# change df_orov to wide format 

df_orov_wide <- df_orov |>
  pivot_wider(names_from = Measure.Names, values_from = Measure.Values)|>
  mutate(Pais = stri_trans_general(Pais, "Latin-ASCII"),
         Pais = str_trim(case_when(
           Pais == "Brasil" ~"Brazil",
           Pais == "Islas Caiman" ~"Cayman Islands",
           Pais == "Estados Unidos" ~"United States of America",
           T ~ Pais
         )))|>
  complete(Pais, Year) |>
  arrange(Pais, Year) |>
  group_by(Pais) |>
  fill(`Población X 1000`, .direction = "downup") |>
  ungroup()




# standardise orov columns to match df_national, then bind
df_national <- df_orov_wide |>
  rename(
    Country.or.Subregion = Pais,
    EW                   = SE,
    population_x_1000    = `Población X 1000`,
    cfr                  = `Letalidad (e)`,
    incidence            = `Incidencia (d)`,
    deaths               = `Muertes `,
    imported             = `Importados `,
    confirmed_cases      = `Confirmados laboratorio (b)`,
    suspected_cases      = `Sospechosos (a)`,
    neuro_cases = "Manifestaciones Neurológicas",
    hospitalizations = "Hospitalizados " 
  ) |>
  type_convert() |>
  mutate(disease = "orov") |>
  bind_rows(df_national)

sum(rowSums(table(df_national$Country.or.Subregion, df_national$disease))) <= 8*length(unique(df_national$Country.or.Subregion))


# 3. Calculate cumulative incidence rates ---------------------------------
# confirmed rates only, per 100 000

df_national<- df_national |>
  mutate(p_incidence_confirmed = confirmed_cases/(population_x_1000*1000)*100000)%>%
  group_by(disease, Country.or.Subregion) %>%
  mutate(
    cumulative_cases = cumsum(replace_na(confirmed_cases, 0)),
    cumulative_incidence = cumulative_cases / (first(population_x_1000)*1000)*100000
  ) %>%
  ungroup()



# check that I calculated correctly based on existing data
check <- df_national |>
  filter(!is.na(Lab.Confirm.Incidence.rate))
sum(check$Lab.Confirm.Incidence.rate - check$p_incidence_confirmed) < 0.01



# 4. Save results  --------------------------------------------------------

# as Rdata 
save.image(here::here("1_data", "04_paho","paho_data.RData"))
# as .csv 
write.csv2(df_national, here::here("1_data/04_paho", "combined_national_data.csv"), row.names = FALSE)
