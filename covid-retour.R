library(tidyverse)

#### INCIDENCE

read_delim("https://www.data.gouv.fr/fr/datasets/r/57d44bd6-c9fd-424f-9a72-7834454f9e3c",";") %>%
  arrange(jour) %>%
  filter(cl_age90 == "0") %>%
  mutate(taux_incid_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/pop*100000) %>%
  filter(jour >= "2020-09-01") %>%
  select(jour,taux_incid_glissant) %>%
  write_csv("incidence_france.csv")

read_delim("https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c",";") %>%
  arrange(jour) %>%
  filter(cl_age90 == "0") %>%
  mutate(taux_posiv_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/slider::slide_dbl(T,sum,.before=6,.after=0,.complete=TRUE)*100) %>%
  filter(jour >= "2020-09-01") %>%
  select(jour,taux_posiv_glissant) %>%
  write_csv("positivite_france.csv")

read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
  mutate(epci2020 = ifelse(epci2020 == "245900410",as.character("200093201"),as.character(epci2020))) %>%
  left_join(read_delim("https://www.banatic.interieur.gouv.fr/V5/fichiers-en-telechargement/telecharger.php?zone=N&date=01/10/2020&format=A","\t",locale = locale(encoding = "ISO-8859-1")) %>%
              mutate(`N° SIREN` = as.character(`N° SIREN`)),
            by = c("epci2020" = "N° SIREN")) %>%
  mutate(dernier_jour = as.Date(substr(semaine_glissante,12,21))) %>%
  select(epci2020,`Nom du groupement`,dernier_jour,clage_65,ti) %>%
  pivot_wider(names_from = c("clage_65","dernier_jour"),values_from = ti) %>%
  left_join(read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
              mutate(epci2020 = ifelse(epci2020 == "245900410",as.character("200093201"),as.character(epci2020))) %>%
              left_join(read_delim("https://www.banatic.interieur.gouv.fr/V5/fichiers-en-telechargement/telecharger.php?zone=N&date=01/10/2020&format=A","\t",locale = locale(encoding = "ISO-8859-1")) %>%
                          mutate(`N° SIREN` = as.character(`N° SIREN`)),
                        by = c("epci2020" = "N° SIREN")) %>%
              mutate(dernier_jour = as.Date(substr(semaine_glissante,12,21))) %>%
              filter(dernier_jour == max(dernier_jour)) %>%
              select(epci2020,`Nom du groupement`,dernier_jour,clage_65,ti) %>%
              pivot_wider(names_from = clage_65,values_from = ti,names_prefix = "age_")) %>%
  select(nom=`Nom du groupement`,age_0,starts_with("0_"),age_65,starts_with("65_")) %>%
  write_csv("incidence_metro.csv")

read_delim("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",";") %>%
  filter(sexe == 0) %>%
  group_by(jour) %>%
  summarise(hosp_1 = sum(hosp),
            rea_1 = sum(rea)) %>%
  mutate(jour_confi = as.integer(difftime(jour,as.Date("2020-03-17"),units = "days"))) %>%
  filter(jour_confi >= 0 & jour_confi < 100) %>%
  full_join(read_delim("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",";") %>%
              filter(sexe == 0) %>%
              group_by(jour) %>%
              summarise(hosp_2 = sum(hosp),
                        rea_2 = sum(rea)) %>%
              mutate(jour_confi = as.integer(difftime(jour,as.Date("2020-10-30"),units = "days"))) %>%
              filter(jour_confi > -25 & jour_confi < 100),
            by=c("jour_confi"="jour_confi")) %>%
  select(jour_confi,hosp_1,rea_1,hosp_2,rea_2) %>%
  write_csv("compa_confinements.csv")

read_delim("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",";") %>%
  left_join(readxl::read_excel("~/INSEE/ensemble-2020.xlsx",sheet="Départements",skip=7) %>%
              select(dep=`Code département`,nom_dpt=`Nom du département`,pop=`Population totale`)) %>%
  filter(sexe == 0 & jour == max(as.Date(jour))) %>%
  group_by(jour,nom_dpt,dep) %>%
  summarise(tx_hosp = sum(hosp)/pop*100000) %>%
  left_join(read_delim("https://www.data.gouv.fr/fr/datasets/r/4acad602-d8b1-4516-bc71-7d5574d5f33e",",") %>% select(extract_date,departement,tx_rea=taux_occupation_sae),
            by=c("jour"="extract_date","dep"="departement")) %>%
  left_join(read_delim("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",";") %>%
              filter(sexe == 0) %>%
              group_by(jour,dep) %>%
              summarise(hosp = sum(hosp),
                        rea = sum(rea)) %>%
              filter(jour >= as.Date("2020-08-01")) %>%
              pivot_wider(names_from=jour,values_from=c("hosp","rea"))) %>%
  ungroup() %>%
  select(nom_dpt,`Taux d'hosp.`=tx_hosp,starts_with("hosp_"),`Taux de réa.`=tx_rea,starts_with("rea_")) %>%
  write_csv("hosp_departements.csv")


read_delim("2020_FR_Region_Mobility_Report.csv",",") %>%
  filter(is.na(sub_region_1) & is.na(sub_region_2)) %>% 
  select(date,retail=retail_and_recreation_percent_change_from_baseline,grocery=grocery_and_pharmacy_percent_change_from_baseline,parks=parks_percent_change_from_baseline,transit=transit_stations_percent_change_from_baseline,workplaces=workplaces_percent_change_from_baseline,residential=residential_percent_change_from_baseline) %>%
  mutate(jour_confi = as.integer(difftime(date,as.Date("2020-10-30"),units = "days"))) %>%
  filter(jour_confi > -25 & jour_confi < 100) %>%
  left_join(read_delim("2020_FR_Region_Mobility_Report.csv",",") %>%
              filter(is.na(sub_region_1) & is.na(sub_region_2)) %>% 
              select(date,retail=retail_and_recreation_percent_change_from_baseline,grocery=grocery_and_pharmacy_percent_change_from_baseline,parks=parks_percent_change_from_baseline,transit=transit_stations_percent_change_from_baseline,workplaces=workplaces_percent_change_from_baseline,residential=residential_percent_change_from_baseline) %>%
              mutate(jour_confi = as.integer(difftime(date,as.Date("2020-03-17"),units = "days"))) %>%
              filter(jour_confi > -25 & jour_confi < 100),
            by=c("jour_confi")) %>%
  select(jour_confi,starts_with("retail"),starts_with("transit"),starts_with("transit"),starts_with("workplaces"),starts_with("residential")) %>%
  write_csv("mobilite.csv")


