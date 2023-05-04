library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(ggforce)
#library(cansim)
#you need to change the cut and max year--------------------------
cut <- 3
min_year <- 2010 # first year for everything else
max_year <- 2022 #last year for everything
tbbl_name <- paste0("stokes_",cut)
#functions--------------------------------------

get_cor <- function(tbbl){
  tbbl%>%
    select(stokes_industry, correlation, cut)%>%
    distinct()%>%
    mutate(correlation=paste0("cor: ",correlation),
           year=2010)
}

write_most_recent <- function(tbbl_name, cut){
  wide_stokes <- get(tbbl_name)%>%
    mutate(from_stokes=round(from_stokes))%>%
    pivot_wider(names_from = year, values_from = from_stokes)%>%
    mutate(stokes_industry=name, .before="name")%>%
    select(-name)%>%
    mutate(stokes_industry=paste(stokes_industry, "from stokes", sep=": "))

  both_wide <- bind_rows(wide_stokes, wide_stokes_agg)%>%
    arrange(stokes_industry)

  write_csv(both_wide, here("out",paste0("to_vs_from_stokes_cut_", cut, ".csv")))
}

read_stokes <- function(file){
  tbbl <- read_excel(here("data",file), sheet = "Labour Market", n_max = 52, skip=2)
  colnames(tbbl)[1] <- "name"
  tbbl <- tbbl%>%
    filter(name !="% Change",
           name != "Employment (000s)",
           name != "Total")%>%
    mutate(across(-name, ~ 1000*.x),
           name=make_clean_names(name))%>%
    pivot_longer(cols=-name, names_to = "year", values_to = "from_stokes")
}


join_with_internal <- function(tbbl2, tbbl1, cut){
  #browser()
  inner_join(tbbl1, tbbl2, by=c("stokes_industry"="name","year"="year"))%>%
    group_by(stokes_industry)%>%
    mutate(correlation=round(cor(to_stokes, from_stokes),3))%>%
    arrange(correlation)%>%
    mutate(cut=cut)
}

make_plot <- function(data, cor, stokes_industry){
  ggplot(data, aes(to_stokes, from_stokes, colour=as.numeric(year)))+
    geom_abline(slope=1, intercept = 0, colour="white", lwd=1.2)+
    geom_point()+
    geom_text(
      data    = cor,
      mapping = aes(x = -Inf, y = -Inf, label = correlation),
      hjust   = -1,
      vjust   = -1
    )+
    scale_colour_viridis_c()+
    scale_x_continuous(labels = scales::comma)+
    scale_y_continuous(labels = scales::comma)+
    labs(colour="year",
         x="Employment supplied to Stokes",
         y="Employment from Stokes",
         title=str_to_title(stokes_industry),
         subtitle="Diagonal white line in background indicates equivilence")+
    theme(axis.text.x = element_text(angle = 15))+
    facet_wrap(~cut, labeller = "label_both")
}

#THE PROGRAM---------------------------------

#discard old numbering of LMO industries (they have changed)---------------
old_mapping <- read_csv(here("data","naics_2_lmo_2_aggregate_industry_mapping.csv"))%>%
  clean_names()%>%
  select(contains("industry"))%>%
  distinct()%>%
  mutate(lmo_detailed_industry=janitor::make_clean_names(lmo_detailed_industry),
         stokes_industry=NA)

#this file has the correct names and numbering for 2023----------------
our_forecasts <- read_excel(here("data","manualy_fixed_gas.xlsx"), skip=3, n_max = 64)%>%
  select(-Note, -`10-yr`)%>%
  separate(industry,  c("lmo_ind_code", "lmo_detailed_industry"), sep=": ")%>%
  mutate(lmo_detailed_industry=janitor::make_clean_names(lmo_detailed_industry))%>%
  pivot_longer(cols=-contains("lmo"), names_to = "year", values_to = "value")%>%
  group_by(lmo_ind_code, lmo_detailed_industry)%>%
  nest()%>%
  left_join(old_mapping)%>%
  arrange(lmo_ind_code)

#manually add the aggregate industries for the new LMO industries--------------
our_forecasts$aggregate_industry[our_forecasts$lmo_detailed_industry=="social_assistance_excluding_child_care"] <- "health_care_and_social_assistance"
our_forecasts$aggregate_industry[our_forecasts$lmo_detailed_industry=="child_day_care_services"] <- "health_care_and_social_assistance"
our_forecasts$aggregate_industry[our_forecasts$lmo_detailed_industry=="entertainment_and_recreation"] <- "information,_culture_and_recreation"

#manually add stokes industries----------------

our_forecasts$stokes_industry[our_forecasts$lmo_detailed_industry=="farms_and_support_activities"] <- "agriculture"
our_forecasts$stokes_industry[our_forecasts$lmo_detailed_industry %in% c("fishing_hunting_and_trapping",
                                                                         "forestry_logging_and_support_activities",
                                                                         "mining",
                                                                         "support_activities_for_mining_and_oil_and_gas_extraction")] <- "other_other_primary"
our_forecasts$stokes_industry[our_forecasts$lmo_detailed_industry=="oil_and_gas_extraction"] <- "oil_gas"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry=="manufacturing"] <- "manufacturing"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry=="construction"] <- "construction"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry=="utilities"] <- "utilities"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry=="transportation_and_warehousing"] <- "transportation_warehousing"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry%in%c("retail_trade", "wholesale_trade")] <- "trade"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry=="finance,_insurance_and_real_estate"] <- "finance_insurance_real_estate"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry %in% c("professional,_scientific_and_technical_services",
                                                                      "business,_building_and_other_support_services")] <- "professional_scientific_managerial"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry =="accommodation_and_food_services"] <- "accommodation_food_services"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry =="educational_services"] <- "education_services"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry =="health_care_and_social_assistance"] <- "health_social_services"
our_forecasts$stokes_industry[our_forecasts$lmo_detailed_industry %in% c("publishing_industries",
                                                                         "motion_picture_and_sound_recording_industries",
                                                                         "telecommunications",
                                                                         "broadcasting_data_processing_and_information",
                                                                         "performing_arts_spectator_sports_and_related_industries",
                                                                         "entertainment_and_recreation",
                                                                         "automotive_repair_and_maintenance",
                                                                         "personal_non_automotive_repair_and_non_profit_services")] <- "other_services"
our_forecasts$stokes_industry[our_forecasts$aggregate_industry =="public_administration"] <- "government_services"

#save the mapping to disk------------------
our_forecasts%>%
  select(-data)%>%
  write_csv(here("out","lmo64_agg_stokes_mapping.csv"))

#aggregate from 64 to 18 industries----------------------------
agg_employment <- our_forecasts%>%
  unnest(data)%>%
  group_by(aggregate_industry, year)%>%
  summarize(employment=sum(value))

#write wide data with totals to disk-------------------------------------
agg_employment%>%
  mutate(employment=round(employment))%>%
  pivot_wider(names_from = year, values_from = "employment")%>%
  mutate(aggregate_industry=str_to_title(str_replace_all(aggregate_industry, "_"," ")))%>%
  adorn_totals()%>%
  write_csv(here("out","LMO64_to_18_industries.csv"))

#aggregate by stokes industries--------------------
stokes_agg <- our_forecasts%>%
  unnest(data)%>%
  group_by(stokes_industry, year)%>%
  summarize(to_stokes=sum(value))%>%
  pivot_wider(names_from = stokes_industry, values_from = to_stokes)%>%
  mutate(other_primary = other_other_primary + oil_gas)%>%
  pivot_longer(cols=-year, names_to = "stokes_industry", values_to = "to_stokes")

#read in the stokes data----------------------

stokes_1 <- read_stokes("BritishColumbiaTables_1.xlsx")
both_1 <- join_with_internal(stokes_1, stokes_agg, cut="first")
cor_tbbl_1 <- get_cor(both_1)

if(cut==1){
  both <- both_1
  cor_tbbl <- cor_tbbl_1
}else if(cut==2){
  stokes_2 <- read_stokes("BritishColumbiaTables_2.xlsx")
  both_2 <- join_with_internal(stokes_2, stokes_agg, cut="second")
  cor_tbbl_2 <- get_cor(both_2)
  cor_tbbl <- bind_rows(cor_tbbl_1, cor_tbbl_2)
  both <- bind_rows(both_1, both_2)
}else if(cut==3){
  stokes_2 <- read_stokes("BritishColumbiaTables_2.xlsx")
  both_2 <- join_with_internal(stokes_2, stokes_agg, cut="second")
  cor_tbbl_2 <- get_cor(both_2)
  stokes_3 <- read_stokes("BritishColumbiaTables_3.xlsx")
  both_3 <- join_with_internal(stokes_3, stokes_agg, cut="third")
  cor_tbbl_3 <- get_cor(both_3)
  cor_tbbl <- bind_rows(cor_tbbl_1, cor_tbbl_2, cor_tbbl_3)
  both <- bind_rows(both_1, both_2, both_3)
}else if(cut==4){
  stokes_2 <- read_stokes("BritishColumbiaTables_2.xlsx")
  both_2 <- join_with_internal(stokes_2, stokes_agg, cut="second")
  cor_tbbl_2 <- get_cor(both_2)
  stokes_3 <- read_stokes("BritishColumbiaTables_3.xlsx")
  both_3 <- join_with_internal(stokes_3, stokes_agg, cut="third")
  cor_tbbl_3 <- get_cor(both_3)
  both_4 <- join_with_internal(stokes_4, stokes_agg, cut="fourth")
  cor_tbbl_4 <- get_cor(both_4)
  cor_tbbl <- bind_rows(cor_tbbl_1, cor_tbbl_2, cor_tbbl_3, cor_tbbl_4)
  both <- bind_rows(both_1, both_2, both_3, both_4)
}

cor_tbbl <- cor_tbbl%>%
  group_by(stokes_industry)%>%
  nest()%>%
  rename(cor=data)

both <- both%>%
  group_by(stokes_industry)%>%
  nest()


# make the comparision plots---------------------------

plot_data <- full_join(both, cor_tbbl)%>%
  mutate(plot=pmap(list(data, cor, stokes_industry), make_plot))

pdf(here("out", paste0("plots_cut_",cut,".pdf")), onefile = TRUE, height=8.5, width=11)
plot_data%>%
  select(plot)%>%
  walk(print)
dev.off()

wide_stokes_agg <- stokes_agg%>%
  mutate(to_stokes=round(to_stokes))%>%
  pivot_wider(names_from = year, values_from = to_stokes)%>%
  mutate(stokes_industry=paste(stokes_industry, "to stokes", sep=": "))

write_most_recent(tbbl_name, cut)

# cansim_dat <- cansim::get_cansim("14-10-0023-01")
#
# cansim <- cansim_dat%>%
#   clean_names()%>%
#   filter(geo=="British Columbia",
#          ref_date %in% min_year:max_year,
#          sex=="Both sexes",
#          age_group=="15 years and over",
#          labour_force_characteristics=="Employment",
#          north_american_industry_classification_system_naics=="Total, all industries")%>%
#   select(syear=ref_date, cansim=val_norm)
#
# write_csv(cansim, here("out","cansim_table14-10-0023-01.csv"))

