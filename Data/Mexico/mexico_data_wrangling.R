

setwd("D:\\DAP 2\\final-project-diego_khristel")

library(tidyverse)
library(readxl)
library(stringi)
library(stringr)

s# To begin with some csv files from ENDIREH 2021 will be imported, relevant columns are going to be
# selected and variable names are going to be adjusted to have them similar if not equal to the peruvian
# dataframe. Some data is going to be wrangled so both dataframes have the same information (eg. same values
# for single in marital status)


#Both the Demographic files are uploaded through Google Drive

sit_couple_mx_2021 <- read.csv("Data\\Mexico\\conjunto_de_datos_TB_SEC_VI.csv")

sit_couple_mx_2021 <- sit_couple_mx_2021 %>%
  rename("w_willing_sex" = P6_2_4, 
         "w_house_chores" = P6_1_3) %>% 
  mutate(w_willing_sex = ifelse(w_willing_sex > 90, NA, w_willing_sex - 1), 
         NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", "")) %>%
  select(c("ID_VIV",
           "ID_PER",
           "NOM_ENT",
           "w_willing_sex",
           "w_house_chores",
           "FAC_VIV",
           "FAC_MUJ"))
sit_couple_mx2 <-  read.csv("Data\\Mexico\\conjunto_de_datos_TB_SEC_IV.csv")
sit_couple_mx2 <- sit_couple_mx2 %>%
  rename("employed"= P4_1) %>%
  mutate(employed = employed - 1,
         NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", ""))  %>%
  select(c("ID_VIV",
           "ID_PER",
           "NOM_ENT",
           "employed",
           "FAC_VIV",
           "FAC_MUJ"))

sit_couple_mx5 <- read.csv("Data\\Mexico\\conjunto_de_datos_TSDem.csv")
sit_couple_mx5 <- sit_couple_mx5 %>%
  rename("years_old" = EDAD, 
         "education_level" = NIV, 
         "marital_status" = P2_16, 
         "sex" = SEXO) %>%
  mutate(NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", ""),
          education_level = ifelse(education_level<=5, education_level + 1,
                                  ifelse(education_level<=7, 6, 
                                         ifelse(education_level==11, 8, 7))),
  marital_status = ifelse(marital_status==2, 5, 
                          ifelse(marital_status==3, 4, 
                                 ifelse(marital_status==4, 3, 
                                        ifelse(marital_status==5, 2, marital_status))))) %>%
    select(c("ID_VIV",
           "ID_PER",
           "NOM_ENT",
           "education_level",
           "marital_status",
           "FAC_VIV",
           "FAC_MUJ"))


sit_couple_mx6 <- read.csv("Data\\Mexico\\conjunto_de_datos_TB_SEC_XV.csv")
sit_couple_mx6 <- sit_couple_mx6 %>%
  mutate( NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", "")) %>%
  rename("w_chooseto_work_study" = ifelse(P15_1AB_1>=2, NA, P15_1AB_1)) %>% 
  select(c("ID_VIV",
           "ID_PER",
           "NOM_ENT",
           "w_chooseto_work_study",
           "FAC_VIV",
           "FAC_MUJ"))

sit_couple_mx3 <- read.csv("Data\\Mexico\\conjunto_de_datos_TB_SEC_XIII.I.csv")
sit_couple_mx3 <- sit_couple_mx3 %>% 
  rename("w_conflict_jelousy" = P13_1_2_3) %>%
  mutate(w_conflict_jelousy = ifelse(w_conflict_jelousy==9, NA, w_conflict_jelousy),
         NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", "")) %>%
  select(c("ID_VIV", 
           "ID_PER", 
           "NOM_ENT",
           "w_conflict_jelousy", 
           "FAC_VIV", 
           "FAC_MUJ"))


# Merge all dataframes
df_list <- list(sit_couple_mx_2021, sit_couple_mx2, sit_couple_mx3, sit_couple_mx5, sit_couple_mx6)

mexico_2021 <- df_list %>% reduce(full_join, by=c("ID_VIV", 'ID_PER', "NOM_ENT", "FAC_VIV", "FAC_MUJ"))

mexico_2021 <- mutate(mexico_2021, year_poll=2021)

# Same as before, but with ENDIREH 2016

endireh_2016_1 <- read.csv("Data\\Mexico\\conjunto_de_datos_tsdem_endireh_2016.csv")

endireh_2016_1 <- endireh_2016_1 %>%
  rename("ID_VIV" = ï..ID_VIV, 
         "years_old" = EDAD, 
         "sex" = SEXO, 
         "education_level" = NIV, 
         "marital_status" = P2_16,) %>% 
  mutate( NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", ""),
          marital_status = ifelse(marital_status==1 | marital_status==6, marital_status, 
                                 ifelse(marital_status==2, 5, 
                                        ifelse(marital_status==3, 4, 
                                                ifelse(marital_status==4, 3, 
                                                        ifelse(marital_status==5, 2, NA))))),
         education_level = ifelse(education_level <=5, education_level + 1, 
                                  ifelse(education_level<=8, 6, 
                                         ifelse(education_level <= 10, 7,
                                                ifelse(education_level==11, 8, NA))))) %>% 
  select(c("ID_VIV",
           "ID_MUJ",
           "NOM_ENT",
           "education_level",
           "marital_status",
           "FAC_VIV",
           "FAC_MUJ"))

endireh_2016_2 <- read.csv("Data\\Mexico\\conjunto_de_datos_tb_sec_iv_endireh_2016.csv")

endireh_2016_2 <- endireh_2016_2 %>% 
  rename("employed" = P4_1, 
         "ID_VIV" = ï..ID_VIV) %>%
  mutate(employed = employed - 1,
         NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", "")) %>%
  select(c("ID_VIV",
           "ID_MUJ",
           "NOM_ENT",
           "employed",
           "FAC_VIV",
           "FAC_MUJ"))


endireh_2016_3 <- read.csv("Data\\Mexico\\conjunto_de_datos_tb_sec_xv_endireh_2016.csv")

endireh_2016_3 <- endireh_2016_3 %>%
  rename("w_willing_sex" = P15_1_9, 
         "w_house_chores" = P15_1_3, 
         "ID_VIV" = ï..ID_VIV,
         "w_chooseto_work_study" = P15_1_7) %>% 
  mutate(w_willing_sex = ifelse(w_willing_sex==9, NA, w_willing_sex - 1), 
         w_house_chores = ifelse(w_house_chores== 9, NA, w_house_chores - 1), 
         w_chooseto_work_study = ifelse(w_chooseto_work_study==9, NA, w_chooseto_work_study - 1),
         NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", "")) %>% 
  select(c("ID_VIV",
           "ID_MUJ",
           "NOM_ENT",
           "w_willing_sex",
           "w_house_chores",
           "w_chooseto_work_study",
           "FAC_VIV",
           "FAC_MUJ"))

endireh_2016_4 <- read.csv("Data\\Mexico\\conjunto_de_datos_tb_sec_xii.i_endireh_2016.csv")


endireh_2016_4 <- endireh_2016_4 %>% 
  rename("ID_VIV" = ï..ID_VIV, 
         "w_conflict_jelousy" = P12_1_1_5) %>% 
  mutate(w_conflict_jelousy = ifelse(w_conflict_jelousy==9, NA, w_conflict_jelousy - 1),
         NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", "")) %>% 
  select(c("ID_VIV",
           "ID_MUJ",
           "NOM_ENT",
           "w_conflict_jelousy",
           "FAC_VIV",
           "FAC_MUJ"))
df_list_2016 <- list(endireh_2016_1, endireh_2016_2, endireh_2016_3, endireh_2016_4)

mexico_2016 <- df_list_2016 %>% reduce(full_join, by=c("ID_VIV", 'ID_MUJ', "NOM_ENT", "FAC_VIV", "FAC_MUJ"))

mexico_2016 <- mexico_2016 %>%
  mutate(year_poll=2016) %>%
  rename("ID_PER" = ID_MUJ)

mexico_complete <- mexico_2016 %>% 
  rbind(mexico_2021)


psychological_violence <- read_xlsx("Data\\Mexico\\Cuadro SIESVIM.xlsx",skip=1)

physical_violence <- read_excel("Data\\Mexico\\Cuadro SIESVIM (1).xlsx", skip=1)

sexual_violence <- read_excel("Data\\Mexico\\Cuadro SIESVIM (2).xlsx", skip=1)


psychological_violence <- psychological_violence %>%
  rename("NOM_ENT" = "Entidad federativa") %>% 
  select(c("NOM_ENT", "2016", "2021")) %>%
  head(33) %>%
  filter(NOM_ENT != "Estados Unidos Mexicanos") %>%
  pivot_longer(cols=c("2016", "2021"), names_to = "year_poll", values_to = "mexico_violencia_psicologica")
physical_violence <- physical_violence %>% 
  rename(NOM_ENT = "Entidad federativa") %>% 
  select(c(NOM_ENT, "2016", "2021")) %>%
  head(33) %>%
  filter(NOM_ENT != "Estados Unidos Mexicanos") %>%
  pivot_longer(cols=c("2016", "2021"), names_to = "year_poll", values_to = "mexico_violencia_fisica")
sexual_violence <- sexual_violence %>% 
  rename(NOM_ENT = "Entidad federativa")%>% 
  select(c(NOM_ENT, "2016", "2021")) %>%
  head(33) %>%
  filter(NOM_ENT != "Estados Unidos Mexicanos") %>%
  pivot_longer(cols=c("2016", "2021"), names_to = "year_poll", values_to = "mexico_violencia_sexual")

final_list <- list(psychological_violence, physical_violence, sexual_violence)
violence <- final_list %>% reduce(full_join, by=c("NOM_ENT", "year_poll"))

violence <- violence %>% 
  mutate(year_poll = as.integer(year_poll), 
         NOM_ENT = str_replace_all(stri_trans_general(str_to_sentence(NOM_ENT), id="Latin-ASCII"), "[\n]", "")) 

mexico_data <- full_join(mexico_complete, violence, by=c("NOM_ENT", "year_poll"))

mexico_data <- mexico_data %>%
  rename("state" = NOM_ENT) %>% 
  mutate(Index=(w_willing_sex +w_house_chores +  w_chooseto_work_study + w_conflict_jelousy)/4)

write.csv(mexico_data, "Data\\Mexico\\mexico_data.csv")

  
