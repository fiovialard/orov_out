##script to generate figures

# By Fio Vialard

# Last update: 03/09/2026


# 1. Set up ---------------------------------------------------------------
require(tidyverse)
#install.packages("paletteer")
require(paletteer) # color palettes 
require(ggrepel) # repel labels
require(grid)
require(gridExtra) # combine plots
require(cowplot) # combine plots
#install.packages("ggpubr")
#install.packages("carData") # required for ggpubr
require(ggpubr) # for plot themes 
# #install.packages("egg")
require(egg) # for plot layout
require(rlang) # for enquo
source(here::here("2_scripts", "00_functions.R")) # source the functions from their specific location 

l_orov <- readRDS(here::here("1_data","05_clean_data" ,"orov_out_descriptive_analysis_objects.RData"))

# palettes 
#v_ordem_e_progresso <- c("#002776", "#009b3a", "#fedf00")

# extract the palette colours 
v_green <- paletteer_d("fishualize::Acanthostracion_polygonius")

v_green2 <- c("black", "#509E22", "#6CB83E", "#8FD665", "#B4E995" )

v_frost <- paletteer_d("nord::frost")

v_heatdark <- c("grey" ,"#E03C31FF", "#BA0C2FFF")

v_seagreen <- setdiff(paletteer_d("unikn::pal_seegruen"),"#0AA398FF")

v_prince <- c("grey","#6650B4FF", "#59449CFF", "#4B3984FF", "#3E2D6CFF")


v_green_blue <-  rev(c("#2CB11BFF", "#95C65CFF", "#AFDFEFFF","#54BCD1FF", "#1BB6AFFF", "#0099D5FF","#007BC3FF", "#172869FF"))
names(v_green_blue) <- levels(l_orov$orov_clean$site2)

v_red <- c("grey", "#D46A6A", "#801515")
v_red2 <- c("grey", "#D46A6A", "#AA3939", "#801515", "#550000")
v_orange <- c("grey", "#D38647", "#8F480D")
v_orange2 <- c("grey", "#F5AE73", "#D38647","#B56726", "#8F480D" )

v_tubes <- c("#6650B4FF", "grey",  "darkred", "gold")

v_ad <- paletteer_d("lisa::AlbrechtDurer")
v_seapunk <- paletteer_d("vapoRwave::seaPunk")
v_summerknight <- paletteer_d("DresdenColor::summerknight")

# Remove everything after \r\n OR the text "yes/no/not known"
replacement_vector <- str_remove(l_orov$attr_manaus$dictionary$variable_description, "\\r?\\n.*|\\s*yes/no/not known") |> 
  str_trim(side = "right")   # trim only the right side


replacement_vector <- setNames(replacement_vector, l_orov$attr_manaus$dictionary$variable_name)

replacement_vector["abdominal_pain"] <- "Abdominal pain"
replacement_vector["complication2"] <- "Complication type"
replacement_vector["n_comorb"] <- "Any comorbidities"
replacement_vector["age_cat"] <- "Age category"
replacement_vector["final_outcome"] <-  "Final outcome"
replacement_vector["n_ws"] <-  "Number of warning signs"
replacement_vector["n_sx_cat"] <- "Number of symptoms"
replacement_vector["n_comorb2"] <- "Number of comorbidities"
replacement_vector["n_sx_length"] <- "Length of symptoms"
replacement_vector["abnormal_lab"] <- "Laboratory results"
replacement_vector["diarrhea"] <- "Diarrhoea"

orov_clean <- l_orov$orov_clean
clin <- l_orov$clin

#selected sx for small multiple graphs
v_var_symptoms<- clin %>%
  select(fever, 
         nausea,
         headache,
         myalgia,
         ro_pain,
         nausea,
         vomiting,
         skin_rash,
         abdominal_pain,
         diarrhea,
         dizziness) |>
  colnames()

# remove the unfrequent symptoms for now
v_var_symptoms_all <- clin |>
  select(fever:dizziness) |>
  colnames()
  
  
  v_var_symptoms_all<- v_var_symptoms_all[!v_var_symptoms_all %in% c(
  "odynophagia", "conjunct_injection", "mental",
  "limb_weakness", "chest_pain", "stiff_neck", "seizures"
)]

##laboratory resutls only
  
df_lab  <- l_orov$df_lab

## CTs by sample type 
df_ct_sample <- orov_clean |>
  select(p_id,site2,
         matches("_ct$|_ct_fu$|_pos$|_pos_fu$"),
         matches("^time_sx_to_pcr")) |>
  mutate(across(matches("_ct"), as.numeric),
         across(starts_with("time_sx_to_pcr"), as.numeric)) |>
  pivot_longer(
           cols = matches("(_ct$|_ct_fu$|_pos$|_pos_fu$)"),
           names_to = c("sample", ".value"), #create 3 columns: 1 for sample, 1 for pos and one for ct (coded as .value)
           names_pattern = "(.*)_(ct|ct_fu|pos|pos_fu)"
         )|>
  mutate(
    sample_type = str_extract(sample, "serum|blood|urine|semen|csf|edta"),
    fu = str_detect(sample, "_fu$"),
    time_to_test = case_when(
      sample_type == "serum" ~ time_sx_to_pcr,
      sample_type == "blood" ~ time_sx_to_pcr,
      sample_type == "csf" ~ time_sx_to_pcr,
      sample_type == "urine" & !fu ~ time_sx_to_pcr_urine,
      sample_type == "urine" & fu ~ time_sx_to_pcr_urine_fu,
      sample_type == "semen" & !fu ~ time_sx_to_pcr_semen,
      sample_type == "semen" & fu ~ time_sx_to_pcr_semen_fu,
      sample_type == "edta" & !fu ~ time_sx_to_pcr_edta,
      sample_type == "edta" & fu ~ time_sx_to_pcr_edta_fu,
      fu ~ time_sx_to_pcr_fu,
      TRUE ~ NA_real_
    )
  ) |>
  select(p_id, ct, ct, time_to_test, sample_type, pos, site2)
  

# Main Fig ------------------------------------------------------------





# Figure 2A ---------------------------------------------------------------
p_sx_mult_age <- barplot_facet(clin, v_var_symptoms, 
                               "age_cat",
                               replacement_vector = replacement_vector,
                               names_to_col = "Symptoms",
                               set_facet_rows = 2)+ 
  scale_fill_manual(values = rev(as.character(v_seapunk)))+
  scale_color_manual(values = after_scale(alpha(rev(as.character(v_seapunk)), 0.9)))+
  labs(fill = "", colour = "")+
  theme(legend.position = "right")+
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = F
    ))

p_sx_mult_age







# Figure 2B ---------------------------------------------------------------


p_sx_risk_f<- barplot_facet(clin, v_var_symptoms, 
                            "risk_factors", 
                            replacement_vector = replacement_vector, 
                            names_to_col = "Symptoms",
                            set_facet_rows =2 )+ 
  scale_fill_manual(values = v_orange) +
  scale_color_manual(values = after_scale(alpha(v_orange, 0.9)))+
  labs(fill = "", colour = "")+
  theme(legend.position = "right")+
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = F
    )
  )

p_sx_risk_f


# Figure 2C  --------------------------------------------------------------


p_sx_complication_f <- barplot_facet(
  clin, v_var_symptoms, 
  "complication2", 
  replacement_vector = replacement_vector, 
  names_to_col = "Symptoms",
  set_facet_rows = 2
)+
  scale_fill_manual(values = v_red2) +
  scale_color_manual(values = after_scale(alpha(v_red2, 0.9)))+
  labs(fill = "", color= "")+
  theme(legend.direction = "vertical",
        legend.position = "right")+
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = F
    )
  )

p_sx_complication_f

# Fig 2 all ----------------------------------------------------------



# get legends of plots 
legend_a <- get_legend(p_sx_mult_age)


legend_b <- get_legend(p_sx_risk_f)
legend_c <- get_legend(p_sx_complication_f)


# remove legends from plots 
p_sx_mult_age <- p_sx_mult_age +
  theme(legend.position = "none")

p_sx_risk_f <- p_sx_risk_f+
  theme(legend.position = "none")

p_sx_complication_f <- p_sx_complication_f+
  theme(legend.position = "none")

# add back top right
p_sx_mult_age<- ggdraw(p_sx_mult_age) +
  draw_plot(legend_a, x = 0.78, y = 0.25, width = 0.2, height = 0.2)

p_sx_risk_f<- ggdraw(p_sx_risk_f) +
  draw_plot(legend_b, x = 0.78, y = 0.25, width = 0.2, height = 0.2)

p_sx_complication_f<- ggdraw(p_sx_complication_f) +
  draw_plot(legend_c, x = 0.60, y = 0.35, width = 0.2, height = 0.1)


# add to list 
plot_list <- c(list(p_sx_mult_age))

plot_list <- c(plot_list, list(p_sx_risk_f))
plot_list <- c(plot_list, list(p_sx_complication_f))

# 3 plots 
fig2_grid<- plot_grid(p_sx_mult_age, p_sx_risk_f,  p_sx_complication_f, nrow = 3, labels = "auto",
                      align = "v")

fig2_grid

# Figure 4 ----------------------------------------------------------------

# transform the data

orov_clean <- orov_clean |>
  mutate(
    complication2 = fct_rev(complication2),
    n_comorb = factor(n_comorb, levels = c("Yes", "No")),
    n_yes = rowSums(across(c(hypertension, diabetes, immuno, obesity ,renal, cvd, other_comorbidity_bin), ~ .x == "Yes"), na.rm = TRUE),
    n_comorb2 = fct_infreq(case_when(
      n_yes == 0 ~ "0",
      n_yes == 1 ~ "1",
      T ~ "≥2",  # catches any other comorbidity not listed above
    )),
    n_ws = factor(n_ws), 
    n_sx_cat = factor(case_when(
      n_sx <=2 ~ "0-2",
      n_sx %in% c(3,4) ~ "3-4",
      n_sx %in% c(5,6) ~ "5-6",
      n_sx > 6 ~ ">6",
      is.na(n_sx) ~NA
    )),
    n_sx_cat = fct_relevel(n_sx_cat,">6", after = Inf ),
    n_sx_length = factor(
      case_when(
        sx_length <= 2 ~ "1-2 days",
        sx_length > 2 & sx_length < 5 ~ "3-4 days",
        sx_length == 5 ~"5 days",
        sx_length >5 ~ ">5 days",
        T ~NA
      ), levels = c("1-2 days", "3-4 days","5 days",">5 days")
    ))


# lab results separatel

df_lab_long <- orov_clean %>%
  select(complication2, haematocrit_lab_elevated, platelets_lab_low, alt_lab_elevated) %>%
  group_by(complication2) %>%
  summarise(
    # Numerators (Yes count)
    `Elevated haematocrit_n` = sum(haematocrit_lab_elevated == "Yes", na.rm = TRUE),
    `Thrombocytopenia_n`     = sum(platelets_lab_low == "Yes", na.rm = TRUE),
    `Elevated ALT_n`          = sum(alt_lab_elevated == "Yes", na.rm = TRUE),
    `None*_n`                 = sum(if_all(c(haematocrit_lab_elevated, platelets_lab_low, alt_lab_elevated), ~ .x=="No"), na.rm = TRUE),
    
    # Denominators (Non-NA count for each test)
    `Elevated haematocrit_den` = sum(!is.na(haematocrit_lab_elevated)),
    `Thrombocytopenia_den`     = sum(!is.na(platelets_lab_low)),
    `Elevated ALT_den`          = sum(!is.na(alt_lab_elevated)),
    `None*_den`                 = sum(!if_any(c(haematocrit_lab_elevated, platelets_lab_low, alt_lab_elevated), ~ is.na(.x))),
    
    .groups = "drop"
  ) %>%
  # Pivot both numerators (_n) and denominators (_den) long
  pivot_longer(
    cols = -complication2,
    names_to = c("test", ".value"),
    names_sep = "_"
  ) %>%
  mutate(
    pct_group = (n / den) * 100,
    test = fct_relevel(test, c("None*", "Elevated haematocrit", "Thrombocytopenia", "Elevated ALT"))
  ) %>%
  rename(n_total_tested = den)





v_lab_levels <- levels(df_lab_long$test)

# comorbidity
p_comorb <- grouped_barplot(orov_clean, 
                            strat_var = "n_comorb2", 
                            xvar = "complication2",
                            v_replacement = replacement_vector,
                            v_palette = v_orange)+
  theme(
    axis.text.x = element_blank()
  )+
  labs(y="", fill = "", color = "")+
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = F
    )
  )+
  ggtitle("Comorbidities")



p_comorb

# age 
p_age <- grouped_barplot(orov_clean, 
                         strat_var = "age_cat", 
                         xvar = "complication2",
                         v_replacement = replacement_vector,
                         v_palette = as.character(rev(v_seapunk)))+
  theme(
    axis.text.x = element_blank()
  )+
  theme(
    axis.text.y = element_blank()
  )+
  labs(y="", fill = "", color = "")+
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = F
    )
  )+
  ggtitle("Age")


p_age

# warning signs 

p_ws <- grouped_barplot(orov_clean, 
                        strat_var = "n_ws", 
                        xvar = "complication2",
                        v_replacement = replacement_vector,
                        v_palette = v_heatdark)+
  theme(
    axis.text.x = element_blank()
  )+
  labs(y="", fill = "", color = "")+
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = F
    )
  )+
  ggtitle("Warning signs")


p_ws

# n symptoms 
p_n_sx <- grouped_barplot(orov_clean, 
                          strat_var = "n_sx_cat", 
                          xvar = "complication2",
                          v_replacement = replacement_vector,
                          v_palette = as.character(v_frost))+
  theme(
    axis.text.x = element_blank()
  )+
  theme(
    axis.text.y = element_blank()
  )+
  labs(y="", fill = "", color = "")+
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = F
    )
  )+
  ggtitle("Symptoms")

p_n_sx

p_n_sx_days <- grouped_barplot(orov_clean, 
                               strat_var = "n_sx_length", 
                               xvar = "complication2",
                               v_replacement = replacement_vector,
                               v_palette = v_seagreen)+
  labs(fill = "", color = "")+
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = F
    )
  )+
  ggtitle("Symptoms length")

p_n_sx_days


p_n_lab <- df_lab_long |>
  mutate(
    test = fct_rev(test))  |>
  ggplot(
    aes(
      x = complication2,
      y = pct_group,
      fill = test,
      color = test
    )
  ) +
  geom_bar(stat = "identity", position = "dodge", 
           width = 0.7, alpha = 0.7
  ) +
  scale_x_discrete(drop = FALSE) + 
  labs(y= "Percentage  of patients",fill= "", color = "", x="")+
  scale_fill_manual(
    values = v_prince,
    breaks = v_lab_levels,
    drop = FALSE
  ) +
  scale_color_manual(
    values = alpha(v_prince, 0.7 + 0.2),
    breaks = v_lab_levels,
    drop = FALSE
  )+
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(2.5, "mm"),
    legend.spacing.y = unit(1, "mm"),
    axis.text.y = element_blank()
  )+
  scale_y_continuous(limits = c(0, 102), expand = c(0, 0))+
  coord_flip(clip = 'off')+
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = F
    )
  )+
  ggtitle("Laboratory results")

p_n_lab

# get legends of plots 
legend_a <- get_legend(p_comorb)
legend_b <- get_legend(p_age)
legend_c <- get_legend(p_ws)
legend_d <- get_legend(p_n_sx)
legend_e <- get_legend(p_n_sx_days)
legend_f <- get_legend(p_n_lab)


# remove legends 
p_comorb <- p_comorb+
  theme(legend.position = "none")

p_age <- p_age+
  theme(legend.position = "none")

p_ws <- p_ws+
  theme(legend.position = "none")

p_n_sx <- p_n_sx+
  theme(legend.position = "none")

p_n_sx_days <- p_n_sx_days+
  theme(legend.position = "none")

p_n_lab <- p_n_lab +
  theme(legend.position = "none")

# add back legend 
p_comorb <- p_comorb+
  draw_plot(legend_a, x = 5, y = 95, width = 0.2, height = 0.2)

p_age <- p_age+
  draw_plot(legend_b, x = 5, y = 78, width = 0.2, height = 0.2)

p_ws <- p_ws+
  draw_plot(legend_c, x = 5, y = 95, width = 0.2, height = 0.2)

p_n_sx <- p_n_sx+
  draw_plot(legend_d, x = 5, y = 95, width = 0.2, height = 0.2)


p_n_sx_days <- p_n_sx_days+
  draw_plot(legend_e, x = 5, y = 85, width = 0.2, height = 0.2)

p_n_lab <- p_n_lab+
  draw_plot(legend_f, x = 4, y = 77, width = 0.2, height = 0.2)


# add to list 
plot_list <- c(plot_list, list(p_comorb))
plot_list <- c(plot_list, list(p_age))
plot_list <- c(plot_list, list(p_ws))
plot_list <- c(plot_list, list(p_n_sx))
plot_list <- c(plot_list, list(p_n_sx_days))
plot_list <- c(plot_list, list(p_n_lab))

# combine into grid
aligned_plots <- cowplot::align_plots(p_age, p_n_sx, p_n_lab ,align = 'v', axis = 'l')

fig_4_top <- plot_grid(p_comorb, aligned_plots[[1]], labels = "auto", axis = "b",
                       rel_widths = c(1.2,1))

fig_4_verse <- plot_grid(p_ws,  aligned_plots[[2]], labels = c("c", "d"), axis = "b",
                         rel_widths = c(1.2,1),align = "h")

fig_4_bottom  <- plot_grid(p_n_sx_days, aligned_plots[[3]], labels = c("e", "f"), axis = "tblr",
                           rel_widths = c(1.2,1))

fig4_grid <-  plot_grid(fig_4_top, fig_4_verse, fig_4_bottom, ncol = 1)



fig4_grid

# reverse complication factor again for later graphs

orov_clean <- orov_clean |>
  mutate(
    complication2 = fct_rev(complication2))

# S Figs ------------------------------------------------------------------





# Fig S2  ---------------------------------------------------------------

p_sx_overall <- make_barplot(clin,
                             v_var_symptoms_all,
                             replacement_vector = replacement_vector, names_to_col = "Symptoms")+
  theme(axis.title.y = element_blank())



p_sx_overall
plot_list <- c(plot_list, list(p_sx_overall))



# Figure S3 rev1 ---------------------------------------------------------------


p_sx_mult_site <- 
  barplot_facet(clin, v_var_symptoms_all, "site2", replacement_vector = replacement_vector, names_to_col = "Symptoms")+
  scale_fill_manual(values = unname(v_green_blue))+
  scale_color_manual(values = after_scale(alpha(unname(v_green_blue), 0.9)))+
  labs(fill="", colour = "")+
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = F
    ))

p_sx_mult_site

plot_list <- c(plot_list , list(p_sx_mult_site))





# Fig S4 -------------------------------------------------------------


# new Fig  post-revision 

clin_s4 <- clin |>
  filter(!is.na(sx_length)) |>
  filter(!site%in% c("pernambuco")) |>
  add_count(site2, name = "site_n") |>
  mutate(
    site2 := forcats::fct_relabel(
      site2,
      ~ paste0(.x, " (n=", site_n[match(.x, site2)], ")")
    ))


fig_s4_bottom<- make_hist3(data = clin_s4,
           set_x = "sx_length",
           set_color = "site2",
           set_v_colors = setdiff(v_green_blue, c(v_green_blue["Paraíba"])),
           set_xlab = "Length of symptoms",
           set_ylim_max = 10,
           set_breaks = c(2,4,6,8,10))

fig_s4_top <- make_hist3(data = clin_s4,
                         set_x = "sx_length",
                         set_color = "site2",
                         set_v_colors = setdiff(v_green_blue, c(v_green_blue["Paraíba"])),
                         set_xlab = "",
                         set_ylab = "",
                         set_ylim_min = 15)+
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x  = element_blank()
  )
  

fig_s4_new <- plot_grid(fig_s4_top, fig_s4_bottom, ncol = 1, align = "v",
                        rel_heights = c(1,2))
plot_list <- c(plot_list, list(fig_s4_new))






# Fig S5 -------------------------------------------------------------


##Figure S5 post revisions 

  clin_s5 <- clin |>
      filter(!is.na(time_sx_to_pcr)) |>
      add_count(site2, name = "site_n") |>
      mutate(
        site2 := forcats::fct_relabel(
          site2,
          ~ paste0(.x, " (n=", site_n[match(.x, site2)], ")")
        ))
  



fig_s5_bottom <- make_hist3(clin_s5, set_x = "time_sx_to_pcr",
           set_color = "site2", set_ylim_max = 50)



fig_s5_top <- make_hist3(clin_s5, set_x = "time_sx_to_pcr",
                         set_color = "site2", set_ylim_min = 70,
                         set_xlab = "",
                         set_ylab = "")+
  theme(
  legend.position = "none",
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.x  = element_blank()
)


## combine both graphs together 

fig_s5_new <- plot_grid(fig_s5_top, fig_s5_bottom, ncol = 1, align = "v",
          rel_heights = c(1,2))

plot_list <- c(plot_list, list(fig_s5_new))


# Figure S6 ---------------------------------------------------------------


p_time_sx_pcr_urine <- df_ct_sample |>
  filter(sample_type=="urine", pos=="Positive") |>
  make_hist2(fill=site2, x=time_to_test, xlab="Time since symptom onset (days)", set_ylim = 10, set_xlim = 55)+
  scale_y_continuous(limits = c(0, 10.5), expand = c(0, 0), breaks = seq(2,10,by = 2))+
  scale_fill_manual(values = c("#AFDFEFFF", "#95C65CFF"))+
  scale_color_manual(values = c("#AFDFEFFF", "#95C65CFF"))+
  theme(legend.position = "bottom")+
  ylab("Number of samples")

df_ct_sample |>
  filter(sample_type=="urine", pos=="Positive") |> distinct(p_id) |> nrow() == 53

p_time_sx_pcr_urine

plot_list <- c(plot_list, list(p_time_sx_pcr_urine))



# Figure S7 rev1 ---------------------------------------------------------------



v_age_cat <- levels(clin$age_cat)

df_age_ct <- clin|>
  mutate(age_cat=fct_relevel(age_cat, c("18 to <40",   "40 to <65",  "65 and above",  "5 to <18", "0 to <5" )))|>
  arrange(age_cat) |>
  filter(!is.na(orov_serum_ct))

table(df_age_ct$age_cat, useNA = "ifany")
summary(df_age_ct$orov_serum_ct)

 p_ct_histo_age <-  df_age_ct |> 
   ggplot( aes(x = orov_serum_ct, fill = age_cat, color =age_cat)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 0.75, linewidth =0.25) +
  scale_fill_manual(values = c("#532E57FF","#A997ABFF","#7EC488FF", "#569874FF","#296656FF"), breaks = v_age_cat, drop = FALSE)+
  scale_color_manual(values= after_scale(alpha(c("#532E57FF","#A997ABFF","#7EC488FF", "#569874FF","#296656FF"), 0.9)), # does not work if i do not call manually
                     breaks = v_age_cat, drop = F)+
   labs(fill = "", color = "", y="Number of patients", x="Ct value") +
   theme_classic() +
   theme(
     legend.position = "bottom",
     axis.text = element_text(size = 10),
     axis.title = element_text(size = 10),
     legend.text = element_text(size = 6),
     legend.key.size = unit(1.25, "mm"),
     legend.spacing.y = unit(0.5, "mm")
   )+
   scale_y_continuous(limits = c(0, 102), expand = c(0, 0))+ # get rid of the white space under the bar
   scale_x_continuous(limits = c(0, 42), expand = c(0, 0))+ # get rid of the white space under the bar
   guides(
     fill = guide_legend(
       ncol=1,
       byrow = F
     )
   )

p_ct_histo_age


# Fig s8 new --------------------------------------------------------------


# post revision version 
s8_rev1 <- 
  orov_clean |>
  filter(!is.na(orov_serum_ct),!is.na(sx_length))|>
  mutate(complication = fct_drop(complication))|>
  ggplot(aes(x=sx_length, y=orov_serum_ct, color =site2, shape = complication)) +
  scale_color_manual(values = v_green_blue) +  
  geom_point(size = 2)+
  xlab("Length of symptoms")+
  ylab("Ct value")+
  labs(color = "", shape = "")+
  theme_classic()+
  scale_x_continuous(
    labels = function(x) ifelse(x == 0, "<24h", paste0(x, "d"))
  )+
  theme(
    axis.text   = element_text(size = 10),
    axis.title  = element_text(size = 10),
    legend.title= element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.position = "bottom"
  )+
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = F
    ),
    shape = guide_legend(
      nrow = 2,
      byrow = F
    )
  )

plot_list <- c(plot_list, list(s8_rev1))


# Figure S9 rev1---------------------------------------------------------------



# add the CT values for sb
v_compl_levels <- levels(orov_clean$complication2)

stillbirth_rows <- orov_clean |>
  filter(!is.na(still_birth_ct)) |>
  mutate(
    p_id = paste0(p_id, "_sb"),
    orov_serum_ct = still_birth_ct,
    complication2 = factor("Stillbirth",
                           levels = c(v_compl_levels, "Stillbirth"))
  )

# Bind them to original dataset
orov_clean2 <- bind_rows(orov_clean, stillbirth_rows)

# check
nrow(orov_clean2)> nrow(orov_clean)


# labels 
labels_preg <- orov_clean |>
  filter(!is.na(orov_serum_ct), time_sx_to_pcr<= 15) |>
  mutate(
    pregnancy_outcomes = case_when(!pregnancy_outcomes %in% c("Preterm", "Abortion") ~NA, 
                                   T ~pregnancy_outcomes),
    nudge_y = ifelse(pregnancy_outcomes == "Preterm",5, 0),
    nudge_x = ifelse(pregnancy_outcomes == "Abortion", 1, 0)
  )

df_compl_ct <- orov_clean2 |>
  mutate(complication2=fct_relevel(complication2,
                                   "Pregnancy complication", 
                                   "Stillbirth", 
                                   after = Inf),
         pregnancy_outcomes = case_when(pregnancy_outcomes== "Stillbirth" ~NA, # de-label
                                        T ~pregnancy_outcomes))|>
  filter(!is.na(orov_serum_ct))

table(df_compl_ct$complication2)

p_ct_histo_compl <- 
  df_compl_ct|> 
  ggplot( aes(x = orov_serum_ct, fill = complication2, color =complication2, #label = pregnancy_outcomes
              )) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 0.75, linewidth= 0.25) +
  scale_fill_manual(values = c(v_red2, "red"), breaks = levels(orov_clean2$complication2), drop = FALSE)+
  scale_color_manual(values= after_scale(alpha(c(v_red2, "red"), 0.9)), # does not work if i do not call manually
                     breaks = levels(orov_clean2$complication2), drop = F)+
  labs(fill = "", color = "", y="Number of patients", x="Ct value") +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.key.size = unit(1.25, "mm"),
    legend.spacing.y = unit(0.5, "mm")
  )+
  scale_y_continuous(limits = c(0, 102), expand = c(0, 0))+ # get rid of the white space under the bar
  scale_x_continuous(limits = c(0, 42), expand = c(0, 0))+ # get rid of the white space under the bar
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = F
    )
  )+
  geom_text_repel(
    data = labels_preg,                  # dataframe containing your labels
    aes(x = orov_serum_ct, 
        y = 1,
        label = pregnancy_outcomes),  
    nudge_y = 8,# moves labels above the bar
    nudge_x = c(5,-6),
    size = 3,
    segment.color = "#D46A6A",            # color of the connecting segment
    segment.size = 0.25,                  # thickness of the segment
    segment.curvature = 0.1,             # optional: small curve for nicer look
    max.overlaps = Inf,
    min.segment.length = 0,
    show.legend = FALSE
  )

p_ct_histo_compl

# extract and remove plot legend 

legend_a <- get_legend(p_ct_histo_age)
legend_b <- get_legend(p_ct_histo_compl)


# remove legends from plots 
p_ct_histo_age <- p_ct_histo_age +
  theme(legend.position = "none")

p_ct_histo_compl <- p_ct_histo_compl+
  theme(legend.position = "none")



# add back top 
p_ct_histo_age<- ggdraw(p_ct_histo_age) +
  draw_plot(legend_a, x = 0.7, y = 0.7, width = 0.3, height = 0.3)

p_ct_histo_compl<- ggdraw(p_ct_histo_compl) +
  draw_plot(legend_b, x = 0.50, y = 0.78, width = 0.3, height = 0.3)



# add plots to list
plot_list <- c(plot_list, list(p_ct_histo_age))
plot_list <- c(plot_list, list(p_ct_histo_compl))






# Fig s10 ---------------------------------------------------------
pos <- position_jitter(width = 0.25, seed = 0109)

df_ct_time <- orov_clean |>
  filter(!is.na(orov_serum_ct)&time_sx_to_pcr<= 15)


df_ct_time_summary <- df_ct_time |>
  group_by(site2) |>
  summarise(ct_median = median(orov_serum_ct),
            ct_q25 = quantile(orov_serum_ct, 0.25),
            ct_q75 = quantile(orov_serum_ct,0.75),
            n_ct = n())


df_ct_time_site <- df_ct_time |>
  group_by(site2, time_sx_to_pcr) |>
  summarise(ct_median = median(orov_serum_ct),
            ct_q25 = quantile(orov_serum_ct, 0.25),
            ct_q75 = quantile(orov_serum_ct,0.75)) |>
  ungroup() |>
  mutate(
    site2 = factor(
      site2,
      levels = c(
        "Espirito Santo",
        "Ceará",
        "Amazonas",
        "Darién",
        "Rio de Janeiro"
      )
    )
  )

# check 
sum(df_ct_time_summary$n_ct) == 1029

table(df_ct_time$complication)


# new levels 
new_levels <- c(
  "Espirito Santo",
  "Ceará",
  "Amazonas",
  "Darién",
  "Rio de Janeiro"
)

## violin plot with scatter
p_fig_s10_rev1<- df_ct_time|>
  filter(time_sx_to_pcr<=5)|>
  mutate(
    site2 = factor(
      site2,
      levels = new_levels
    )
  ) |>
  ggplot(aes(x=time_sx_to_pcr, y=orov_serum_ct, 
             color = site2
  )) +
  scale_color_manual(values = after_scale(alpha(v_green_blue,0.9)))+
  geom_violin(aes(group=cut_width(time_sx_to_pcr, 1)), scale = "width", # the violin width was acting weird so I had to rescale with these arguments
    trim = FALSE,
    show.legend = F) +
    geom_point(position = pos, alpha = 0.5)+
  stat_summary(
    aes(group = cut_width(time_sx_to_pcr, 1)),
    fun = median,
    geom = "crossbar",
    width = 0.75,
    show.legend = F
  ) +
  stat_summary(
    aes(group = cut_width(time_sx_to_pcr, 1)),
    fun = median,
    geom = "point",
    size = 2.5,
    shape = 21,
    fill= "white",
    show.legend = F
  )+
  labs(y="Ct value", x = "Time since symptom onset", color ="Site")+
  scale_x_continuous(
    breaks = 0:15,
    labels = function(x) ifelse(x == 0, "<24h", paste0(x, "d"))
  )+
  theme_classic()+
  theme(
    axis.text.y   = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.title  = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.key.size = unit(1.25, "mm"),
    legend.spacing.y = unit(0.5, "mm"),
    legend.position = "bottom")+
  facet_wrap(~site2, nrow = 1,
             strip.position = "bottom")

p_fig_s10_rev1

plot_list <- c(plot_list,list(p_fig_s10_rev1))




# Fig S11 rev1 -------------------------------------------------------------
 
p_ct_scatter2 <- 
  df_ct_time|>
  mutate(
    site2 = factor(
      site2,
      levels = new_levels
    )
  ) |>
  ggplot(aes(x=time_sx_to_pcr, y=orov_serum_ct, color = site2
  )) +
  scale_color_manual(values = v_green_blue) +
  geom_point(position = pos)+
  xlab("Time since symptom onset")+
  ylab("Ct value")+
  labs(color = "", alpha = "", size = "")+
  theme_classic()+
  scale_x_continuous(
    breaks = 0:15,
    labels = function(x) ifelse(x == 0, "<24h", paste0(x, "d"))
  )+
  theme(
    axis.text   = element_text(size = 10),
    axis.title  = element_text(size = 10),
    legend.text = element_text(size = 6),
    legend.key.size = unit(1.25, "mm"),
    legend.spacing.y = unit(0.5, "mm"),
    legend.position = "bottom"
  )+
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = F
    )
  )
p_ct_scatter2

plot_list <- c(plot_list, list(p_ct_scatter2))



# 8. Save -----------------------------------------------------------------
## save median ct value per site df 
l_ct_data <- list(df_ct_time_site, df_ct_time_summary)

names(l_ct_data) <- c("df_ct_time_site", "df_ct_time_summary")

saveRDS(l_ct_data, file = here::here("1_data","05_clean_data" ,"l_ct_data.RData"))


names(plot_list) <- c("2A", "2B", "2C", "4A", "4B", "4C", "4D", "4E","4F", "s2", "s3", "s4", "s5", "s6", "s8", "s7","s9", "s10", "s11")


## .svg
plot_list_sq<- plot_list[setdiff(names(plot_list), "s3")]

## post-revisions 
# .svg

v_names <- names(plot_list_sq)

for (i in v_names) {
  ggsave(
    filename = here::here("4_outputs", paste0(i,"_rev2" ,".svg")),
    plot = plot_list_sq[[i]],
    device = "svg",
    height = 154,
    width = 154,
    units = c("mm")
  )
}



#.pdf 

for (i in v_names) {
  ggsave(
    filename = here::here("4_outputs", paste0(i,"_rev2" ,".pdf")),
    plot = plot_list_sq[[i]],
    device = "pdf",
    height = 154,
    width = 154,
    units = c("mm")
  )
}

## save grids
ggsave(filename = here::here("4_outputs", "fig2_grid_rev2.svg"),
       plot = fig2_grid,
       device = "svg",
       height = 210,
       width = 154,
       unit = c("mm")
)

ggsave(filename = here::here("4_outputs", "fig4_grid_rev2.svg"),
       plot = fig2_grid,
       device = "svg",
       height = 210,
       width = 200,
       unit = c("mm")
)

##pdf


ggsave(filename = here::here("4_outputs", "fig2_grid_rev2.pdf"),
       plot = fig2_grid,
       device = "pdf",
       height = 210,
       width = 154,
       unit = c("mm")
)


ggsave(filename = here::here("4_outputs", "fig4_grid_rev2.pdf"),
       plot = fig4_grid,
       device = "pdf",
       height = 210,
       width = 200,
       unit = c("mm")
)



## save s3 in a wide format 

ggsave(
  filename = here::here("4_outputs", "s3_rev2.svg"),
  plot = plot_list$s3,
  device = "svg",
  height = 154,
  width = 154,
  units = c("mm")
)




