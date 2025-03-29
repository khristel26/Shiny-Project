library(tidyverse)
library(data.table)
#install.packages("importinegi")
#install.packages("inegiR")
library(inegiR)
library(importinegi)
library(haven)
library(dplyr)
#install.packages("foreign")
library(foreign)
library(readxl)
library(sf)
library(rnaturalearth)
library(shiny)
library(shinyWidgets)
#install.packages("shinythemes")
library(shinythemes)
library(rsconnect)
#install.packages("png")
library(png)
#install.packages("questionr")
library(questionr)
#install.packages("DT")
library(DT)
#install.packages("gganimate")
library(gganimate)
library(scales)
#install.packages("viridis")
library(viridis)
#install.packages("hrbrthemes")
library(hrbrthemes)
#install.packages("gifski")
library(gifski)
library(gridExtra)
library(fixest)
#install.packages("pdftools")
library(pdftools)
#install.packages("magick")
library(magick)
#install.packages("broom")
require(broom) # for tidy()
#install.packages("knitr")
require(knitr)
library(stargazer)
library(WDI)
library(ggplot2)
#install.packages("extrafont")
library("extrafont")
library(rvest)
library(tidytext)
library(udpipe)
library(nametagger)
library(tm)
library(textdata)
#install.packages("stopwords")
library(stopwords)
#install.packages("wordcloud2")
library(wordcloud2)
#install.packages("webshot")
library(webshot)
library(htmlwidgets)

# Data wrangling

# 1. Setting path containing Peruvian dataframes:
  
path <- "/Users/khristelzavaleta/Desktop/Uchicago/Q4/Data and programming II/Homework/final-project-diego_khristel"
setwd("/Users/khristelzavaleta/Desktop/Uchicago/Q4/Data and programming II/Homework/final-project-diego_khristel")

## World Bank femicides data

# 2. Automatically retrieved dataset

femicides <- WDI(
  indicator = "VC.IHR.PSRC.FE.P5", country = c("MX", "PE"),
  start = 2011, end = 2020
)

write.csv(femicides, paste0(path, "/Data/final_dataframes/femicides_mexicoperu.csv"))


## Peru data wrangling

# 3. Loading national poll:

enares_2019 <- read_dta(paste0(path, "/Data/Peru/14_v_c2cap400.dta"))
enares_2019_2 <- read_dta(paste0(path, "/Data/Peru/12_v_c2cap200.dta"), encoding = "latin1")

enares_2015 <- read.dbf(paste0(path, "/Data/Peru/08_CRS02_CAP400.dbf"))

enares_2015_2 <- read.dbf(paste0(path, "/Data/Peru/06_CRS02_CAP200.dbf"))

enares_2013 <- read.dbf(paste0(path, "/Data/Peru/11_CRS02_CAP400.dbf")) %>%
  rename("CCDD" = "C2CCDD")
enares_2013_2 <- read.dbf(paste0(path, "/Data/Peru/09_CRS02_CAP200.dbf"))



# 4. Function to clean data and select variables of interest

dfunction <- function(df, postpone_goals, obey, sexrelations, cheating, year) {
  names(df)[length(df)] <- "factor"

  regions <- read_xlsx(paste0(path, "/Data/Peru/geodir-ubigeo-inei.xlsx"))
  regions$region <- substr(regions$Ubigeo, 1, 2)

  regions_2 <- regions %>%
    group_by(region) %>%
    count(Departamento)

  df_1 <- df %>%
    mutate(
      w_postpone_goals = ifelse(postpone_goals == 1 | postpone_goals == 2, 0, 1),
      w_obey = ifelse(obey == 1 | obey == 2, 0, 1),
      w_willing_sex = ifelse(sexrelations == 1 | sexrelations == 2, 0, 1),
      punish_cheating = ifelse(cheating == 1 | cheating == 2, 0, 1),
      year_poll = year
    )

  df_2 <- merge(df_1, regions_2[, c("region", "Departamento")],
    by.x = c("CCDD"),
    by.y = c("region")
  )

  return(df_2)
}

# Function to add fix effects

dfunction_2 <- function(df) {
  setnames(df, replace(names(df), c(
    length(df), length(df) - 1, length(df) - 2,
    length(df) - 3, length(df) - 4
  ), c("sex", "marital_status", "employed", "education_level", "years_old")))
}



# 5. Applying the function with the specific columns codes for each year poll

# 5.1. Year 2019

peru_2019 <- dfunction(
  enares_2019, enares_2019$C2P401_10, enares_2019$C2P401_9,
  enares_2019$C2P401_7, enares_2019$C2P401_5, 2019
) %>% select(
  ID, HOGAR_ID, PERSONA_ID, w_postpone_goals, w_obey, w_willing_sex, punish_cheating,
  year_poll, factor, Departamento
)

# Merging data related to marital status, studies, years old, employed

peru_2019 <- merge(peru_2019, enares_2019_2[, c(
  "ID", "HOGAR_ID", "PERSONA_ID",
  "C1P208_A", "C1P210", "C1P211", "C1P212", "C1P207"
)],
by = c("ID", "HOGAR_ID", "PERSONA_ID"), all.x = TRUE
)

peru_2019 <- dfunction_2(peru_2019) %>%
  add_column(., CONGLOMERA = NA, .before = "ID") # others df have 4 key columns



# 5.2. Year 2015


peru_2015 <- dfunction(
  enares_2015, enares_2015$C2P403_1, enares_2015$C2P406_1,
  enares_2015$C2P407_3, enares_2015$C2P411_2, 2015
) %>%
  select(
    CONGLOMERA, NSELV, HOGARN, PERSONA_ID, w_postpone_goals, w_obey,
    w_willing_sex, punish_cheating, year_poll,
    factor, Departamento
  )

peru_2015 <- merge(peru_2015, enares_2015_2[, c(
  "CONGLOMERA", "NSELV", "HOGARN",
  "PERSONA_ID", "C2P208_A", "C2P210", "C2P211", "C2P212", "C2P207"
)],
by = c("CONGLOMERA", "NSELV", "HOGARN", "PERSONA_ID")
)

peru_2015 <- dfunction_2(peru_2015)
peru_2015$CONGLOMERA <- as.character(peru_2015$CONGLOMERA)



# 5.3. Year 2013

peru_2013 <- dfunction(
  enares_2013, enares_2013$C2P4031, enares_2013$C2P4061,
  enares_2013$C2P4073, enares_2013$C2P4112, 2013
) %>%
  select(
    C2CONGLOME, C2NSELV, C2HOGARN, C2P201, w_postpone_goals, w_obey,
    w_willing_sex, punish_cheating, year_poll,
    factor, Departamento
  )


peru_2013 <- merge(peru_2013, enares_2013_2[, c(
  "C2CONGLOME", "C2NSELV", "C2HOGARN", "C2P201", "C2P208ANIO",
  "C2P210", "C2P211", "C2P212", "C2P207"
)],
by = c("C2CONGLOME", "C2NSELV", "C2HOGARN", "C2P201")
)

peru_2013 <- dfunction_2(peru_2013)
peru_2013$C2CONGLOME <- as.character(peru_2013$C2CONGLOME)


# 6. Binding data from 2013, 2015 and 2019

peru_data <- as.data.frame(mapply(c, peru_2013, peru_2015, peru_2019))

peru_data$employed <- replace(peru_data$employed, peru_data$employed == 2, 0)
peru_data$education_level <- as.numeric(peru_data$education_level) - 1
peru_data[, 5:9] <- lapply(peru_data[, 5:9], as.numeric)
peru_data <- peru_data[, c(9, 11, 1:8, 12:16, 10)] # Reorder column by position



# 7. Creating Peruvian Index of social tolerance to violence (per year)

peru_data_2 <- peru_data %>%
  group_by(Departamento, year_poll, factor) %>%
  summarise(
    punish_cheating = wtd.mean(punish_cheating, as.numeric(factor)),
    w_postpone_goals = wtd.mean(w_postpone_goals, as.numeric(factor)),
    w_obey = wtd.mean(w_obey, as.numeric(factor)),
    w_willing_sex = wtd.mean(w_willing_sex, as.numeric(factor))
  ) %>%
  group_by(Departamento, year_poll) %>%
  summarise(
    punish_cheating = mean(punish_cheating), w_postpone_goals = mean(w_postpone_goals),
    w_obey = mean(w_obey), w_willing_sex = mean(w_willing_sex)
  ) %>%
  group_by(Departamento, year_poll) %>%
  mutate(Index = sum(punish_cheating, w_postpone_goals, w_obey, w_willing_sex) / 4) %>%
  mutate(across(where(is.numeric), round, 3))



# 8. Merging data with the dependent variables


dependent_variables <- c(
  "peru_violencia_sexual", "peru_violencia_psicologica",
  "peru_violencia_fisica"
)
rep_str <- c(
  "Áncash" = "Ancash", "Apurímac" = "Apurimac", "Huánuco" = "Huanuco",
  "Junín" = "Junin", "San Martín" = "San Martin"
)


for (i in dependent_variables) {
  assign(i, read_xlsx(paste0(path, "/Data/Peru/", i, ".xlsx")) %>%
    mutate_at(c(2:14), as.numeric) %>%
    mutate(across(where(is.numeric), round, 2)) %>%
    select("Ámbito geográfico", "2013", "2015", "2019") %>%
    pivot_longer("2013":"2019", names_to = "year", values_to = i) %>%
    mutate(`Ámbito geográfico` = str_replace_all(`Ámbito geográfico`, rep_str)))
}

data <- c("peru_data_2", "peru_data")

peru_data_2 <- peru_data_2 %>%
  merge(peru_violencia_sexual,
    by.x = c("Departamento", "year_poll"),
    by.y = c("Ámbito geográfico", "year"), all.x = TRUE
  ) %>%
  merge(peru_violencia_psicologica,
    by.x = c("Departamento", "year_poll"),
    by.y = c("Ámbito geográfico", "year"), all.x = TRUE
  ) %>%
  merge(peru_violencia_fisica,
    by.x = c("Departamento", "year_poll"),
    by.y = c("Ámbito geográfico", "year"), all.x = TRUE
  )

peru_data_long <- peru_data %>%
  merge(peru_violencia_sexual,
    by.x = c("Departamento", "year_poll"),
    by.y = c("Ámbito geográfico", "year"), all.x = TRUE
  ) %>%
  merge(peru_violencia_psicologica,
    by.x = c("Departamento", "year_poll"),
    by.y = c("Ámbito geográfico", "year"), all.x = TRUE
  ) %>%
  merge(peru_violencia_fisica,
    by.x = c("Departamento", "year_poll"),
    by.y = c("Ámbito geográfico", "year"), all.x = TRUE
  )

peru_data_long <- peru_data_long %>%
  mutate(Index = (w_obey * as.numeric(factor) + w_postpone_goals * as.numeric(factor) +
    w_willing_sex * as.numeric(factor) +
    punish_cheating * as.numeric(factor)) / (as.numeric(factor) * 4))


# 9. Save the peruvian dataframe as a csv file

write.csv(peru_data_2, paste0(path, "/Data/final_dataframes/peru_data.csv"), row.names = FALSE)
write.csv(peru_data_long, paste0(path, "/Data/final_dataframes/peru_data_long.csv"), row.names = FALSE)


### Mexico data wrangling

# 1. Reading Data

mexico_data <- read_csv(paste0(path, "/Data/final_dataframes/mexico_data.csv"))

rep_str_mexico <- c(
  "Estado de mexico" = "México", "Mexico" = "México", "Baja california" = "Baja California",
  "Baja california sur" = "Baja California Sur", "Ciudad de mexico" = "Ciudad de México",
  "Coahuila de zaragoza" = "Coahuila de Zaragoza", "Michoacan de ocampo" = "Michoacán de Ocampo",
  "Nuevo leon" = "Nuevo León", "Queretaro" = "Querétaro", "Quintana roo" = "Quintana Roo",
  "San luis potosi" = "San Luis Potosí", 
  "Veracruz de ignacio de la llave" = "Veracruz de Ignacio de la Llave", "Yucatan" = "Yucatán"
)

mexico_data <- mexico_data %>%
  drop_na(w_willing_sex, w_house_chores, w_chooseto_work_study, w_conflict_jelousy) %>%
  mutate(state = str_replace_all(state, rep_str_mexico))

mexico_data$w_conflict_jelousy[mexico_data$year_poll == 2021] <-
  ifelse(mexico_data$w_conflict_jelousy[mexico_data$year_poll == 2021] == 1, 0, 1)

mexico_data$w_house_chores[mexico_data$year_poll == 2021] <-
  ifelse(mexico_data$w_house_chores[mexico_data$year_poll == 2021] == 1, 0, 1)

mexico_data$w_chooseto_work_study[mexico_data$year_poll == 2021] <-
  ifelse(mexico_data$w_chooseto_work_study[mexico_data$year_poll == 2021] == 1, 0, 1)

mexico_data$w_chooseto_work_study <- ifelse(mexico_data$w_chooseto_work_study == 0, 1, 0)


mexico_data <- mexico_data %>%
  mutate(state = str_replace_all(state, c("Baja California sur" = "Baja California Sur")))


# 2. Creating Mexican Index


mexico_data_short <- mexico_data %>%
  group_by(state, year_poll, FAC_MUJ) %>%
  summarise(
    w_willing_sex = wtd.mean(w_willing_sex, FAC_MUJ),
    w_house_chores = wtd.mean(w_house_chores, FAC_MUJ),
    w_chooseto_work_study = wtd.mean(w_chooseto_work_study, FAC_MUJ),
    w_conflict_jelousy = wtd.mean(w_conflict_jelousy, FAC_MUJ)
  ) %>%
  group_by(state, year_poll) %>%
  summarise(
    w_willing_sex = mean(w_willing_sex), w_house_chores = mean(w_house_chores),
    w_chooseto_work_study = mean(w_chooseto_work_study),
    w_conflict_jelousy = mean(w_conflict_jelousy)
  ) %>%
  group_by(state, year_poll) %>%
  mutate(Index = sum(
    w_willing_sex, w_house_chores, w_chooseto_work_study,
    w_conflict_jelousy
  ) / 4) %>%
  mutate(across(where(is.numeric), round, 3))


mex_summ <- mexico_data %>%
  group_by(state, year_poll) %>%
  summarise(
    mexico_violencia_psicologica = mean(mexico_violencia_psicologica),
    mexico_violencia_fisica = mean(mexico_violencia_fisica),
    mexico_violencia_sexual = mean(mexico_violencia_sexual)
  )

mexico_data_short <- mexico_data_short %>%
  merge(mex_summ, by = c("state", "year_poll"))


mexico_data <- mexico_data %>%
  mutate(Index = (w_willing_sex + w_house_chores +
                    w_chooseto_work_study + w_conflict_jelousy) / 4)

# 3. Save the mexican dataframe as a csv file

write.csv(mexico_data, paste0(path, "/Data/final_dataframes/mexico_data_long.csv"),
  row.names = FALSE
)
write.csv(mexico_data_short, paste0(path, "/Data/final_dataframes/mexico_data_short.csv"),
  row.names = FALSE
)

# Plotting

# 1. Shape files to be merge with the data

# - Peru

peru_shapefile <- st_read(paste0(path, "/Data/Peru/Peru_shapefile/per_admbnda_adm1_ign_20200714.shp"))
peru_shapefile <- st_transform(peru_shapefile, 4326)

peru_data_sf <- peru_data_2 %>%
  merge(peru_shapefile[, c("ADM1_ES", "geometry")],
    by.x = c("Departamento"),
    by.y = c("ADM1_ES"), all.y = TRUE
  )

peru_data_sf <- st_sf(peru_data_sf)

# - Mexico

mexico_shapefile <- st_read(paste0(path, "/Data/Mexico/mexico_shapefile/01_32_ent.shp"))
mexico_shapefile <- st_transform(mexico_shapefile, 4326)

mexico_data_short_sf <- mexico_data_short %>%
  merge(mexico_shapefile[, c("NOMGEO", "geometry")],
    by.x = c("state"),
    by.y = c("NOMGEO"), all.y = TRUE
  )

mexico_data_short_sf <- st_sf(mexico_data_short_sf)

## R plots

### Maps

#### Heat Map - Peru

plot_map_peru <- peru_data_sf %>%
  group_by(Departamento) %>%
  summarise(Index = mean(Index)) %>%
  ggplot() +
  geom_sf(aes(fill = Index)) +
  ggtitle("Index of social tolerance to violence - Heat map ") +
  labs(y = "Latitude", x = "Longitude") +
  scale_fill_distiller(palette = "RdPu", direction = 1) +
  theme_minimal()

plot_map_peru

ggsave(filename = paste0(path, "/images/plot_map_peru.png"), plot = plot_map_peru)

#### Heat Map - Mexico

plot_map_mexico <- mexico_data_short_sf %>%
  group_by(state) %>%
  summarise(Index = mean(Index)) %>%
  ggplot() +
  geom_sf(aes(fill = Index)) +
  ggtitle("Index of social tolerance to violence - Heat map ") +
  labs(y = "Latitude", x = "Longitude") +
  scale_fill_distiller(palette = "RdPu", direction = 1) +
  theme_minimal()

plot_map_mexico

ggsave(filename = paste0(path, "/images/plot_map_mexico.png"), plot = plot_map_mexico)

### Scatter plot

# - Exploratory analysis : Index vs dependent variables - Peru

violencia_psicologica <- ggplot(data = peru_data_2, aes(x = Index, y = peru_violencia_psicologica)) +
  geom_point(fill = "skyblue", shape = 21)

violencia_fisica <- ggplot(data = peru_data_2, aes(x = Index, y = peru_violencia_fisica)) +
  geom_point(fill = "skyblue", shape = 21)

violencia_sexual <- ggplot(data = peru_data_2, aes(x = Index, y = peru_violencia_sexual)) +
  geom_point(fill = "skyblue", shape = 21)

plot_scatter_peru <- grid.arrange(violencia_psicologica, violencia_fisica, violencia_sexual, ncol = 2)

ggsave(filename = paste0(path, "/images/plot_scatter_peru.png"), plot = plot_scatter_peru)


# - Exploratory analysis : Index vs dependent variables - Mexico

violencia_psicologica_mx <- ggplot(data = mexico_data_short, aes(
  x = Index,
  y = mexico_violencia_psicologica
)) +
  geom_point(fill = "skyblue", shape = 21)

violencia_fisica_mx <- ggplot(data = mexico_data_short, aes(
  x = Index,
  y = mexico_violencia_fisica
)) +
  geom_point(fill = "skyblue", shape = 21)

violencia_sexual_mx <- ggplot(data = mexico_data_short, aes(
  x = Index,
  y = mexico_violencia_sexual
)) +
  geom_point(fill = "skyblue", shape = 21)

plot_scatter_mexico <- grid.arrange(violencia_psicologica_mx, violencia_fisica_mx,
  violencia_sexual_mx,
  ncol = 2
)

ggsave(filename = paste0(path, "/images/plot_scatter_mexico.png"), plot = plot_scatter_mexico)

### Lines plot

# - Femicides Mexico and Peru

plot_femicides <- ggplot(femicides, aes(
  x = year, y = VC.IHR.PSRC.FE.P5,
  group = country, color = country
)) +
  geom_line() +
  scale_color_manual(values = c("cyan3", "#FF83FA")) +
  scale_y_continuous(breaks = seq(1, 7, by = 1), limits = c(1, 7)) +
  scale_x_continuous(breaks = seq(2011, 2020, by = 1), limits = c(2011, 2020)) +
  ggtitle("Femicides per 100,000 female") +
  theme_ipsum() +
  labs(y = "femicides per 100,000 female", x = "Year") +
  geom_point(size = 2, shape = 21)

ggsave(filename = paste0(path, "/images/plot_femicides.png"), plot = plot_femicides)

plot_femicides

### Animated plot

# - Animation Peru - Progression analysis : Index vs sexual violence (colored by state)

per_dep_var <- c("peru_violencia_sexual", "peru_violencia_fisica", "peru_violencia_psicologica")


for (i in per_dep_var) {
  assign(i, ggplot(data = peru_data_2) +
    geom_point(aes(x = Index, y = peru_data_2[, c(i)], fill = Departamento, size = Index),
      shape = 21,
      alpha = 0.5
    ) +
    scale_size(range = c(1, 10)) +
    scale_fill_viridis(discrete = TRUE, guide = "none", option = "A") +
    theme_ipsum())
}


gif_peru_1 <- peru_violencia_sexual +
  labs(x = "Index", y = "Sexual violence", title = "Index vs Sexual violence") +
  transition_time(as.integer(year_poll)) +
  labs(title = "Peru - Index vs Sexual violence progression by year: {frame_time}")

gif_peru_2 <- peru_violencia_fisica +
  labs(x = "Index", y = "Physical violence", title = "Index vs Physical violence") +
  transition_time(as.integer(year_poll)) +
  labs(title = "Peru - Index vs Physical violence progression by year: {frame_time}")

gif_peru_3 <- peru_violencia_psicologica +
  labs(x = "Index", y = "Psychological violence", title = "Index vs Psychological violence") +
  transition_time(as.integer(year_poll)) +
  labs(title = "Peru - Index vs Psychological violence progression by year: {frame_time}")

anim_save(paste0(path, "/images/gif_peru_1.gif"), animation = gif_peru_1, height = 400, width = 500)
anim_save(paste0(path, "/images/gif_peru_2.gif"), animation = gif_peru_2, height = 400, width = 500)
anim_save(paste0(path, "/images/gif_peru_3.gif"), animation = gif_peru_3, height = 400, width = 500)



# - Animation Mexico - Progression analysis : Index vs sexual violence (colored by state)

mex_dep_var <- c("mexico_violencia_sexual", "mexico_violencia_fisica", "mexico_violencia_psicologica")


for (i in mex_dep_var) {
  assign(i, ggplot(data = mexico_data_short) +
    geom_point(aes(x = Index, y = mexico_data_short[, c(i)], fill = state, size = Index),
      shape = 21,
      alpha = 0.5
    ) +
    scale_size(range = c(1, 10)) +
    scale_fill_viridis(discrete = TRUE, guide = "none", option = "A")
    +
    theme_ipsum())
}


gif_mexico_1 <- mexico_violencia_sexual +
  labs(x = "Index", y = "Sexual violence", title = "Index vs Sexual violence") +
  transition_time(as.integer(year_poll)) +
  labs(title = "Mexico - Index vs Sexual violence progression by year: {frame_time}")

gif_mexico_2 <- mexico_violencia_fisica +
  labs(x = "Index", y = "Physical violence", title = "Index vs Physical violence") +
  transition_time(as.integer(year_poll)) +
  labs(title = "Mexico - Index vs Physical violence progression by year: {frame_time}")

gif_mexico_3 <- mexico_violencia_psicologica +
  labs(x = "Index", y = "Psychological violence", title = "Index vs Psychological violence") +
  transition_time(as.integer(year_poll)) +
  labs(title = "Mexico - Index vs Psychological violence progression by year: {frame_time}")

anim_save(paste0(path, "/images/gif_mexico_1.gif"),
  animation = gif_mexico_1,
  height = 400, width = 500
)
anim_save(paste0(path, "/images/gif_mexico_2.gif"),
  animation = gif_mexico_2,
  height = 400, width = 500
)
anim_save(paste0(path, "/images/gif_mexico_3.gif"),
  animation = gif_mexico_3,
  height = 400, width = 500
)

# - Animation femicides

gif_2 <- plot_femicides +
  transition_reveal(year) +
  labs(title = "Femicides per 100,000 female: {frame_along}")

anim_save(paste0(path, "/images/gif_2.gif"), animation = gif_2)

gif_2



# - Image created for the home page of shiny

america <- ne_countries(
  continent = c("south america", "north america"), scale = "small",
  returnclass = "sf"
)

peru <- ne_countries(country = "peru", scale = "small", returnclass = "sf")
mexico <- ne_countries(country = "mexico", scale = "small", returnclass = "sf")

plot_peru_mexico <- ggplot() +
  geom_sf(data = america[(america$sovereignt != "Canada" &
    america$sovereignt != "United States of America" &
    america$sovereignt != "Denmark"), ]) +
  geom_sf(data = peru, fill = "red") +
  geom_sf(data = mexico, fill = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

ggsave(plot_peru_mexico, path = path, file = "plot_peru_mexico.png")


## Shiny code

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel(
    strong("Tolerance for violence and its relation to violence against women. A comparison between Mexico and Peru")
  ),
  navbarPage(
    "Project components",
    tabPanel(
      icon("home"),
      fluidRow(
        column(imageOutput(outputId = "image"), width = 3),
        column(
          
          br(),
          p("The objective of this work is to establish how tolerance to violence affects violence against women (femicides and complaints of physical, psychological, and sexual violence).

Mexico and Peru have surveys on social relations, and in them, among other topics, they ask questions related to violence.",
            style = "text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
          ),
          br(),
          p("In this sense, we have created an",
            strong("Index of social tolerance to violence"), "composed of the following dummy variables:",
            br(), br(),
            strong("- Input variables to create the Peruvian Index:"),
            br(), strong("w_postpone_goals:"), "1=Agree, 0=Disagree with the following statement: The woman must first fulfill her role as mother, wife or housewife, and then fulfill her own dreams.",
            br(), strong("w_obey:"), "1=Agree, 0=Disagree with the following statement: If a woman disrespects her husband or partner, she deserves some form of punishment.",
            br(), strong("w_willing_sex:"),
            "1=Agree, 0=Disagree with the following statement: The woman should always be willing to have sexual relations when her husband or partner wants it.",
            br(), strong("punish_cheating:"),
            "1=Agree, 0=Disagree with the following statement: The woman who is unfaithful to her husband or partner must have some form of punishment for him", br(),
            br(),
            strong("- Input variables to create the Mexican Index:"), br(),
            strong("w_willing_sex:"),
            "1=Agree, 0=Disagree with the following statement: Woman should always be willing to have sexual relations when her husband or partner wants it.", br(), strong("w_house_chores:"),
            "1=Agree, 0=Disagree with the following statement: Woman should always be the one in charge of doing the household chores even if they work and/or their partner does not.", br(), strong("w_chooseto_work_study:"),
            "1=Disagree, 0=Agree with the following statement: Women should always choose if they want to work or study. (Disagreeing means their partner should choose if a woman works or studies)", br(),
            strong("w_conflict_jelousy:"),
            "1=Agree, 0=Disagree with the following statement: Men are entitled to create conflict with women if they develop friendship with other men.",
            style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
          ), br(),
          p(strong("Control variables: fix effects"),
            br(), strong("year_old:"), "age of the person surveyed",
            br(), strong("education_level:"),
            "contains eight levels, from 0 meaning no education to 7 meaning Master/PHD studies",
            br(),
            strong("employed:"), "variable dummy, 1= employed and 0 = unemployed",
            br(),
            strong("marital_status:"), "6 levels", br(),
            strong("Departamento"), "Peruvian states",
            style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
          ), br(),
          p(strong("Dependent variables"),
            br(),
            "We will make our dependent variables to interact with three dependent variables (thus, we will have three models per country):", br(),
            strong("physical_violence:"), "percentage of women who experienced physical violence",
            br(), strong("psychological_violence:"),
            "percentage of women who experienced psychological violence",
            br(), strong("sexual_violence:"),
            "percentage of women who experienced sexual violence",
            style = "text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"
          ),
          width = 8
        )
      )
    ), tabPanel(
      "1. Data wrangling",
      fluidRow(
        column(
          align = "center", width = 12,
          radioButtons(
            inputId = "countries",
            label = "Choose between the data sets",
            choices = c("Peru", "Mexico")
          )
        )
      ),
      br(),
      tags$style(".fa-database {color:#E87722}"),
      h3(p(em("Countries DataSet outputs"), icon("paperclip", lib = "font-awesome"),
           style =
             "color:black;text-align:center"
      )),
      fluidRow(column(dataTableOutput("RawData"),
                      width = 12
      ))
    ), tabPanel(
      "2. Plotting",
      br(),
      sidebarLayout(
        sidebarPanel(
          fluidRow(column(align = "center", width = 10, radioButtons(
            inputId = "countries_P_M",
            label = "Choose between countries",
            choices = c("Peru", "Mexico")
          ))),
          br(), fluidRow(
            column(
              align = "center", width = 10, sliderInput(
                inputId = "index_limits",
                label = "Filter Index",
                min = 0, max = 0.793,
                value = c(0, 0.8)
              )
            )
          )
        ),
        mainPanel(
          h3(strong("Plot of incidence of Index of social tolerance to violence per state")),
          fluidRow(column(
            width = 10,
            align = "center", plotOutput("plot")
          ))
        )
      ),
      hr(),
      h3(p(strong("Animated plots"), icon("paperclip", lib = "font-awesome"),
           style = "color:black;text-align:center"
      )),
      br(),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(align = "center", width = 5, radioButtons(
              inputId = "country_P_M",
              label = p("Choose between countries", style = "color:#EE7AE9;text-align:justify"),
              choices = c("Peru", "Mexico")
            )), column(align = "center", width = 7, radioButtons(
              inputId = "type_violence",
              label = p("Plot type of violence", style = "color:#EE7AE9;text-align:justify"),
              choices = c("Sexual violence", "Psychological violence", "Physical violence")
            )), br(),
            br(),
            br(),
            column(br(), p(strong("In the case of the country of Mexico, a growth in tolerance to violence can be observed over time. On the other hand, Peru does not show a marked trend over time."),
                           style = "color:#CD96CD;text-align:justify"
            ), width = 12)
          ), br()
        ),
        mainPanel(
          h3(strong("Progression of Index through the years", style = "color:#EEA9B8")),
          fluidRow(column(
            width = 10,
            align = "center", imageOutput("plot_animated")
          )),
          br(),
          h3(strong("Feminicides per country", style = "color:#EEA9B8")),
          fluidRow(column(
            width = 10,
            align = "center", imageOutput("plot_animated_2")
          ))
        )
      )
    ), tabPanel(
      "3. Text processing", fluidRow(
        column(width = 2),
        column(h4(p(strong("Sentiment Analysis"), style = "color:black;text-align:center")),
               br(),
               p("We will analyze the policies of each country to fight violence against women. Therefore, in this section we analyze the sentiment of the following two policy documents:", br(), 
                 br(),
                 "- ", strong("'Violence based on gender: Conceptual Framework for Public Policy and State action'"), "from Peru, and", br(), br(),
                 "- the ", strong("'Manual for Gendered Violence  in Diverse Contexts'"), "from Mexico.", style = "color:black;text-align:center"),
               width = 8, style = "background-color:lavender;border-radius: 10px"
        )
      ), br(), 
      br(), 
      br(), 
      br(),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(align = "center", width = 5, radioButtons(
              inputId = "count_Per_Mex",
              label = p("Choose between countries", style = "color:#EE7AE9;text-align:justify"),
              choices = c("Peru", "Mexico"))),
            column(align = "center", width = 7, radioButtons(
              inputId = "sentiment",
              label = p("Type of sentiment Analysis", style = "color:#EE7AE9;text-align:justify"),
              choices = c("Bing", "Affin")
            ))
          )),
        mainPanel(
          h3(strong("Text analysis", style = "color:#EEA9B8")),
          fluidRow(column(
            width = 10,
            align = "center", imageOutput("text_analysis")
          )))
      ), br(), br(), 
      br(), br(), 
      br(),
      hr(),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(align = "center", width = 12, radioButtons(
              inputId = "Per_Mex",
              label = p("Choose between countries", style = "color:#EE7AE9;text-align:justify"),
              choices = c("Peru", "Mexico"))
            ))
        ),
        mainPanel(
          h3(strong("Word Cloud analysis", style = "color:#EEA9B8")),
          fluidRow(column(
            width = 10,
            align = "center", imageOutput("cloud_analysis")
          )))
      )),
    tabPanel(
      "4. Analysis",
      fluidRow(
        column(width = 2),
        column(h4(p(strong("Exploratory analysis"), style = "color:black;text-align:center")),
               br(),
               p("We will study the relationships between the dependent variable and all the independent variables (the Index and the variables that compounded it)", style = "color:black;text-align:center"),
               width = 8, style = "background-color:lavender;border-radius: 10px"
        )
      ), hr(),
      tabsetPanel(
        tabPanel(
          strong("Peru"), br(),
          tabsetPanel(
            tabPanel(
              "w_postpone_goals: she must postpone her rol as woman", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely disagree"), "and 1", strong("completely agree"),
                           "with the following statement:", br(),
                           strong("The woman must first fulfill her role as mother, wife or housewife, and then fulfill her own dreams"),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion1"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "w_obey: women punished if disrespect", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely disagree"), "and 1", strong("completely agree"),
                           "with the following statement:", br(),
                           strong("If a woman disrespects her husband or partner, she deserves some form of punishment"),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion2"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "w_willing_sex: women must be willing to have sex", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely disagree"), "and 1", strong("completely agree"),
                           "with the following statement:", br(),
                           strong("The woman should always be willing to have sexual relations when her husband or partner wants it"),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion3"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "punish_cheating: punish women for cheating", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely disagree"), "and 1", strong("completely agree"),
                           "with the following statement:", br(),
                           strong("The woman who is unfaithful to her husband or partner must have some form of punishment for him"),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion4"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "Index: index of the four previous variables", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("The index is composed of the four variables showed before:", br(),
                           strong(
                             "w_postpone_goals", br(), "w_obey", br(), "w_willing_sex",
                             br(), "punish_cheating"
                           ),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion5"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            )
          )
        ), tabPanel(
          strong("Mexico"),
          br(),
          tabsetPanel(
            tabPanel(
              "w_willing_sex: women must be willing to have sex", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely disagree"), "and 1", strong("completely agree"),
                           "with the following statement:", br(),
                           strong("Woman should always be willing to have sexual relations when her husband or partner wants it."),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion6"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "w_house_chores: woman in charge of household chores", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely disagree"), "and 1", strong("completely agree"),
                           "with the following statement:", br(),
                           strong("Woman should always be the one in charge of doing the household chores even if they work and/or their partner does not."),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion7"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "w_chooseto_work_study: women choose if they work or study", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely agree"), "and 1", strong("completely disagree"),
                           "with the following statement:", br(),
                           strong("Women should always choose if they want to work or study"),
                           "(Disagreeing means their partner should choose if a woman works or studies)",
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion8"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "w_conflict_jelousy: men and women in conflict for jelousy", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("This variable is measured from 0 to 1, with 0 being",
                           strong("completely disagree"), "and 1", strong("completely agree"),
                           "with the following statement:", br(),
                           strong("Men are entitled to create conflict with women if they develop friendship with other men"),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion9"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            ),
            tabPanel(
              "Index: index of the four previous variables", br(),
              sidebarLayout(
                sidebarPanel(
                  br(),
                  column(br(),
                         p("The index is composed of the four variables showed before:", br(),
                           strong(
                             "w_willing_sex", br(), "w_house_chores", br(), "w_chooseto_work_study",
                             br(), "w_conflict_jelousy"
                           ),
                           style = "color:black;text-align:center"
                         ),
                         width = 12, style = "background-color:#C6E2FF;border-radius: 10px"
                  ),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                mainPanel(
                  column(br(),
                         plotOutput("Dispersion10"),
                         width = 12, style = "border:1px solid black"
                  ), br()
                )
              )
            )
          )
        )
      ),
      br(),
      br(),
      hr(), fluidRow(
        column(width = 2),
        column(
          h4(p(strong("Building the model"), icon("code"),
               style = "color:black;text-align:center"
          )), br(),
          p(" We will select the independent variables and the fixed effects we would like to include in the model",
            style = "color:black;text-align:center"
          ),
          width = 8, style = "background-color:lavender;border-radius: 10px"
        )
      ),
      br(),
      br(),
      tabsetPanel(tabPanel(strong("Peru"), sidebarLayout(
        sidebarPanel(
          fluidRow(column(
            checkboxGroupInput(
              inputId = "included_variables",
              p("Please, select the independent variables to include:",
                style = "color:#2E8B57;text-align:justify"
              ),
              choices = c("w_postpone_goals", "w_obey", "w_willing_sex", "punish_cheating", "Index"),
              selected = c("w_postpone_goals", "w_obey", "w_willing_sex", "punish_cheating")
            ),
            p("We recommended that the index variable does not interact with other variables because it is composed of the other four variables (thus, select it alone)"),
            width = 10
          ))
        ), mainPanel(
          h3(p(strong("Model example", style = "color:pink")), align = "center"),
          fluidRow(column(verbatimTextOutput("model_example"), width = 12))
        )
      )), tabPanel(strong("Mexico"), sidebarLayout(
        sidebarPanel(
          fluidRow(column(
            checkboxGroupInput(
              inputId = "included_variables_mx",
              p("Please, select the independent variables to include:",
                style = "color:#2E8B57;text-align:justify"
              ),
              choices = c(
                "w_willing_sex", "w_house_chores", "w_chooseto_work_study",
                "w_conflict_jelousy", "Index"
              ),
              selected = c(
                "w_willing_sex", "w_house_chores", "w_chooseto_work_study",
                "w_conflict_jelousy"
              )
            ),
            p("We recommended that the index variable does not interact with other variables because it is composed of the other four variables (thus, select it alone)"),
            width = 10
          ))
        ), mainPanel(
          h3(p(strong("Model example", style = "color:pink")), align = "center"),
          fluidRow(column(verbatimTextOutput("model_example_mx"), width = 12))
        )
      ))),
      br(),
      br(),
      hr(), fluidRow(
        column(width = 2),
        column(
          h4(p(strong("Final model"), icon("code"),
               style = "color:black;text-align:center"
          )),
          width = 8, style = "background-color:lavender;border-radius: 10px"
        )
      ),
      br(),
      br(),
      tabsetPanel(tabPanel(
        strong("Peru"),
        sidebarLayout(
          sidebarPanel(
            fluidRow(column(
              p("Our final model just include the Index, that is compoused of the four variables shown before)"),
              width = 10
            ))
          ), mainPanel(
            h3(p(strong("Model", style = "color:pink")), align = "center"),
            fluidRow(column(verbatimTextOutput("final_model"), width = 12))
          )
        )
      ), tabPanel(strong("Mexico"), sidebarLayout(
        sidebarPanel(
          fluidRow(column(
            p("Our final model just include the Index, that is compoused of the four variables shown before)"),
            width = 10
          ))
        ), mainPanel(
          h3(p(strong("Model", style = "color:pink")), align = "center"),
          fluidRow(column(verbatimTextOutput("final_model_mx"), width = 12))
        )
      )))
    )
  )
)

server <- function(input, output) {
  peru_data <- read_csv("Data/final_dataframes/peru_data.csv")
  peru_data_long <- read_csv("Data/final_dataframes/peru_data_long.csv")
  peru_shapefile <- st_read("Data/Peru/Peru_shapefile/per_admbnda_adm1_ign_20200714.shp")
  peru_shapefile <- st_transform(peru_shapefile, 4326)
  mexico_data_short <- read_csv("Data/final_dataframes/mexico_data_short.csv")
  mexico_data_long <- read_csv("Data/final_dataframes/mexico_data_long.csv")
  mexico_shapefile <- st_read("Data/Mexico/mexico_shapefile/01_32_ent.shp")
  mexico_shapefile <- st_transform(mexico_shapefile, 4326)
  
  
  p <- peru_data %>%
    group_by(Departamento) %>%
    summarise(Index = mean(Index)) %>%
    merge(peru_shapefile[, c("ADM1_ES", "geometry")],
          by.x = c("Departamento"),
          by.y = c("ADM1_ES"), all.y = TRUE
    )
  
  p <- st_sf(p)
  
  m <- mexico_data_short %>%
    merge(mexico_shapefile[, c("NOMGEO", "geometry")],
          by.x = c("state"),
          by.y = c("NOMGEO"), all.y = TRUE
    )
  
  m <- st_sf(m)
  
  output$image <- renderImage(
    {
      ofile <- "www/plot_peru_mexico.jpg"
      list(
        src = ofile,
        contentType = "image/jpg", width = "300px", height = "280px"
      )
    },
    deleteFile = FALSE
  )
  
  output$RawData <- renderDataTable(
    if (input$countries == "Peru") {
      datatable({
        peru_data
      })
    } else if (input$countries == "Mexico") {
      datatable({
        mexico_data_short
      })
    }
  )
  
  data <- reactive({
    if (input$countries_P_M == "Peru") {
      p %>%
        filter(Index > input$index_limits[1] & Index < input$index_limits[2])
    } else if (input$countries_P_M == "Mexico") {
      m %>%
        filter(Index > input$index_limits[1] & Index < input$index_limits[2])
    }
  })
  
  output$plot <- renderPlot({
    if (input$countries_P_M == "Peru") {
      ggplot(data = data()) +
        geom_sf(aes(fill = Index)) +
        ggtitle("Peru Heat map by state") +
        labs(y = "Latitude", x = "Longitude") +
        scale_fill_distiller(palette = "RdPu", direction = 1) +
        theme_minimal()
    } else if (input$countries_P_M == "Mexico") {
      ggplot(data = data()) +
        geom_sf(aes(fill = Index)) +
        ggtitle("Mexico Heat map by state") +
        labs(y = "Latitude", x = "Longitude") +
        scale_fill_distiller(palette = "RdPu", direction = 1) +
        theme_minimal()
    }
  })
  
  output$plot_animated <- renderImage(
    {
      if (input$type_violence == "Sexual violence") {
        if (input$country_P_M == "Mexico") {
          return(list(
            src = "www/gif_mexico_1.gif",
            contentType = "image/gif", width = "500px", height = "400px"
          ))
        } else if (input$country_P_M == "Peru") {
          return(list(
            src = "www/gif_peru_1.gif",
            contentType = "image/gif", width = "500px", height = "400px"
          ))
        }
      } else if (input$type_violence == "Physical violence") {
        if (input$country_P_M == "Peru") {
          return(list(
            src = "www/gif_peru_2.gif",
            contentType = "image/gif", width = "500px", height = "400px"
          ))
        } else if (input$country_P_M == "Mexico") {
          return(list(
            src = "www/gif_mexico_2.gif",
            contentType = "image/gif", width = "500px", height = "400px"
          ))
        }
      } else {
        if (input$country_P_M == "Mexico") {
          return(list(
            src = "www/gif_mexico_3.gif",
            contentType = "image/gif", width = "500px", height = "400px"
          ))
        } else if (input$country_P_M == "Peru") {
          return(list(
            src = "www/gif_peru_3.gif",
            contentType = "image/gif", width = "500px", height = "400px"
          ))
        }
      }
    },
    deleteFile = FALSE
  )
  
  
  output$plot_animated_2 <- renderImage(
    {
      if (input$country_P_M == "Peru") {
        ofile <- "www/gif_2.gif"
        list(
          src = ofile,
          contentType = "image/gif", width = "500px", height = "400px"
        )
      } else if (input$country_P_M == "Mexico") {
        ofile <- "www/gif_2.gif"
        list(
          src = ofile,
          contentType = "image/gif", width = "500px", height = "400px"
        )
      }
    },
    deleteFile = FALSE
  )
  
  
  output$text_analysis <- renderImage({
    if (input$count_Per_Mex == "Peru") {
      if (input$sentiment == "Bing") {
        list(
          src = "www/peru_sentiment_bing.png",
          contentType = "image/png", width = "500px", height = "500px")
      }
      
      else if (input$sentiment == "Affin") {
        list(
          src = "www/peru_sentiment_affin.png",
          contentType = "image/png", width = "500px", height = "500px")
      }
    } else if (input$count_Per_Mex == "Mexico") {
      if (input$sentiment == "Bing") {
        list(
          src = "www/mexico_sentiment_bing.png",
          contentType = "image/png", width = "500px", height = "500px")       
      }
      else if (input$sentiment == "Affin") {
        list(
          src = "www/mexico_sentiment_affin.png",
          contentType = "image/png", width = "500px", height = "500px")
      }
    }
  },
  deleteFile = FALSE
  )
  
  
  output$cloud_analysis <- renderImage({
    if (input$Per_Mex == "Peru") {
      list(
        src = "www/peru_wordcloud.png",
        contentType = "image/png", width = "800px", height = "800px"
      )
    } else if (input$Per_Mex == "Mexico") {
      list(
        src = "www/mexico_wordcloud.png",
        contentType = "image/png", width = "800px", height = "800px"
      )
    }
  },
  deleteFile = FALSE
  )
  
  
  output$Dispersion1 <- renderPlot({
    w_postpone_goals_1 <- ggplot(data = peru_data, aes(
      x = w_postpone_goals,
      y = peru_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman postpone goals vs Psychological violence", "\n"),
        x = paste("Agree that woman must postpone goals", "\n"),
        y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_postpone_goals_2 <- ggplot(data = peru_data, aes(
      x = w_postpone_goals,
      y = peru_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman postpone goals vs Physical violence", "\n"),
        x = "Agree that woman must postpone goals", y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_postpone_goals_3 <- ggplot(data = peru_data, aes(
      x = w_postpone_goals,
      y = peru_violencia_sexual
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman postpone goals vs Sexual violence", "\n"),
        x = "Agree that woman must postpone goals", y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(w_postpone_goals_2, w_postpone_goals_3, w_postpone_goals_1, ncol = 2)
  })
  
  output$Dispersion2 <- renderPlot({
    w_obey_1 <- ggplot(data = peru_data, aes(x = w_obey, y = peru_violencia_psicologica)) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman disrespect vs Psychological violence", "\n"),
        x = paste("Agree that punished woman if disrespect", "\n"),
        y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_obey_2 <- ggplot(data = peru_data, aes(x = w_obey, y = peru_violencia_fisica)) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman disrespect vs Physical violence", "\n"),
        x = "Agree that punished woman if disrespect", y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_obey_3 <- ggplot(data = peru_data, aes(x = w_obey, y = peru_violencia_sexual)) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman disrespect vs Sexual violence", "\n"),
        x = "Agree that punished woman if disrespect", y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(w_obey_2, w_obey_3, w_obey_1, ncol = 2)
  })
  
  output$Dispersion3 <- renderPlot({
    w_willing_sex_1 <- ggplot(data = peru_data, aes(
      x = w_willing_sex,
      y = peru_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman willing to sex vs Psychological violence", "\n"),
        x = paste("Agree that women must be willing to have sex with her partner", "\n"),
        y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_willing_sex_2 <- ggplot(data = peru_data, aes(
      x = w_willing_sex,
      y = peru_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman willing to sex vs Physical violence", "\n"),
        x = "Agree that women must be willing to have sex with her partner",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_willing_sex_3 <- ggplot(data = peru_data, aes(
      x = w_willing_sex,
      y = peru_violencia_sexual
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman willing to sex vs Sexual violence", "\n"),
        x = "Agree that women must be willing to have sex with her partner",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(w_willing_sex_2, w_willing_sex_3, w_willing_sex_1, ncol = 2)
  })
  
  
  output$Dispersion4 <- renderPlot({
    punish_cheating_1 <- ggplot(data = peru_data, aes(
      x = punish_cheating,
      y = peru_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman cheats on partner vs Psychological violence", "\n"),
        x = paste("Agree that women must be punished if cheats on her partner", "\n"),
        y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    punish_cheating_2 <- ggplot(data = peru_data, aes(
      x = punish_cheating,
      y = peru_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman cheats on partner vs Physical violence", "\n"),
        x = "Agree that women must be punished if cheats on her partner",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    punish_cheating_3 <- ggplot(data = peru_data, aes(
      x = punish_cheating,
      y = peru_violencia_sexual
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Woman cheats on partner vs Sexual violence", "\n"),
        x = "Agree that women must be punished if cheats on her partner",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(punish_cheating_2, punish_cheating_3, punish_cheating_1, ncol = 2)
  })
  
  output$Dispersion5 <- renderPlot({
    Index_1 <- ggplot(data = peru_data, aes(x = Index, y = peru_violencia_psicologica)) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Index vs Psychological violence", "\n"),
        x = paste("Indexkhr", "\n"), y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    Index_2 <- ggplot(data = peru_data, aes(x = Index, y = peru_violencia_fisica)) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Index vs Physical violence", "\n"), x = "Index",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    Index_3 <- ggplot(data = peru_data, aes(x = Index, y = peru_violencia_sexual)) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Index vs Sexual violence", "\n"), x = "Index",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(Index_2, Index_3, Index_1, ncol = 2)
  })
  
  
  output$Dispersion6 <- renderPlot({
    w_willing_sex_1 <- ggplot(data = mexico_data_short, aes(
      x = w_willing_sex,
      y = mexico_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_willing_sex vs Psychological violence", "\n"),
        x = paste("w_willing_sex", "\n"), y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_willing_sex_2 <- ggplot(data = mexico_data_short, aes(
      x = w_willing_sex,
      y = mexico_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_willing_sex vs Physical violence", "\n"), x = "w_willing_sex",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_willing_sex_3 <- ggplot(data = mexico_data_short, aes(
      x = w_willing_sex,
      y = mexico_violencia_sexual
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_willing_sex vs Sexual violence", "\n"), x = "w_willing_sex",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(w_willing_sex_1, w_willing_sex_2, w_willing_sex_3, ncol = 2)
  })
  
  
  output$Dispersion7 <- renderPlot({
    w_house_chores_1 <- ggplot(data = mexico_data_short, aes(
      x = w_house_chores,
      y = mexico_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_house_chores vs Psychological violence", "\n"),
        x = paste("w_house_chores", "\n"), y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_house_chores_2 <- ggplot(data = mexico_data_short, aes(
      x = w_house_chores,
      y = mexico_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_house_chores vs Physical violence", "\n"), x = "w_house_chores",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_house_chores_3 <- ggplot(data = mexico_data_short, aes(
      x = w_house_chores,
      y = mexico_violencia_sexual
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_house_chores vs Sexual violence", "\n"), x = "w_house_chores",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(w_house_chores_1, w_house_chores_2, w_house_chores_3, ncol = 2)
  })
  
  
  output$Dispersion8 <- renderPlot({
    w_chooseto_work_study_1 <- ggplot(data = mexico_data_short, aes(
      x = w_chooseto_work_study,
      y = mexico_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_chooseto_work_study vs Psychological violence", "\n"),
        x = paste("w_chooseto_work_study", "\n"), y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_chooseto_work_study_2 <- ggplot(data = mexico_data_short, aes(
      x = w_chooseto_work_study,
      y = mexico_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_chooseto_work_study vs Physical violence", "\n"),
        x = "w_chooseto_work_study",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_chooseto_work_study_3 <- ggplot(
      data = mexico_data_short,
      aes(
        x = w_chooseto_work_study,
        y = mexico_violencia_sexual
      )
    ) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_chooseto_work_study vs Sexual violence", "\n"),
        x = "w_chooseto_work_study",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(w_chooseto_work_study_1, w_chooseto_work_study_2,
                 w_chooseto_work_study_3,
                 ncol = 2
    )
  })
  
  
  output$Dispersion9 <- renderPlot({
    w_conflict_jelousy_1 <- ggplot(data = mexico_data_short, aes(
      x = w_conflict_jelousy,
      y = mexico_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_conflict_jelousy vs Psychological violence", "\n"),
        x = paste("w_conflict_jelousy", "\n"), y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_conflict_jelousy_2 <- ggplot(data = mexico_data_short, aes(
      x = w_conflict_jelousy,
      y = mexico_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_conflict_jelousy vs Physical violence", "\n"),
        x = "w_conflict_jelousy",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    w_conflict_jelousy_3 <- ggplot(data = mexico_data_short, aes(
      x = w_conflict_jelousy,
      y = mexico_violencia_sexual
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "w_conflict_jelousy vs Sexual violence", "\n"),
        x = "w_conflict_jelousy",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(w_conflict_jelousy_3, w_conflict_jelousy_2, w_conflict_jelousy_1, ncol = 2)
  })
  
  
  output$Dispersion10 <- renderPlot({
    Index_1 <- ggplot(data = mexico_data_short, aes(
      x = Index,
      y = mexico_violencia_psicologica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Index vs Psychological violence", "\n"),
        x = paste("Index", "\n"), y = "% W experience psychological violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    Index_2 <- ggplot(data = mexico_data_short, aes(
      x = Index,
      y = mexico_violencia_fisica
    )) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Index vs Physical violence", "\n"), x = "Index",
        y = "% W experience physical violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    Index_3 <- ggplot(data = mexico_data_short, aes(x = Index, y = mexico_violencia_sexual)) +
      geom_point(fill = "skyblue", shape = 21) +
      labs(
        title = paste("\n", "Index vs Sexual violence", "\n"), x = "Index",
        y = "% W experience sexual violence"
      ) +
      theme(
        plot.title = element_text(
          color = "black", size = 14, face = "bold.italic",
          hjust = 0.5
        ),
        axis.title.x = element_text(color = "deeppink3", size = 10),
        axis.title.y = element_text(color = "deeppink3", size = 10)
      )
    
    grid.arrange(Index_1, Index_2, Index_3, ncol = 2)
  })
  
  Model_example <- reactive({
    variables <- input$included_variables
    variables_col <- paste(variables, collapse = " + ")
    depen_var <- c("peru_violencia_fisica", "peru_violencia_sexual", "peru_violencia_psicologica")
    fix <- c("| years_old + education_level + employed + marital_status + Departamento")
    
    fit1 <- feols(formula(paste0(depen_var[1], "~", variables_col, fix)), peru_data_long)
    fit2 <- feols(formula(paste0(depen_var[2], "~", variables_col, fix)), peru_data_long)
    fit3 <- feols(formula(paste0(depen_var[3], "~", variables_col, fix)), peru_data_long)
    etable(list(fit1, fit2, fit3),
           tex = FALSE,
           fitstat = c("n", "r2")
    )
  })
  
  
  output$model_example <- renderPrint({
    Model_example()
  })
  
  Model_exampleMX <- reactive({
    variables <- input$included_variables_mx
    variables_col <- paste(variables, collapse = " + ")
    depen_var <- c(
      "mexico_violencia_fisica", "mexico_violencia_sexual",
      "mexico_violencia_psicologica"
    )
    fix <- c("| education_level + employed + marital_status + state")
    
    fit1 <- feols(formula(paste0(depen_var[1], "~", variables_col, fix)), mexico_data_long)
    fit2 <- feols(formula(paste0(depen_var[2], "~", variables_col, fix)), mexico_data_long)
    fit3 <- feols(formula(paste0(depen_var[3], "~", variables_col, fix)), mexico_data_long)
    etable(list(fit1, fit2, fit3),
           tex = FALSE,
           fitstat = c("n", "r2")
    )
  })
  
  output$model_example_mx <- renderPrint({
    Model_exampleMX()
  })
  
  output$final_model <- renderPrint({
    fit1 <- feols(formula(peru_violencia_fisica ~ Index | years_old + education_level
                          + employed + marital_status + Departamento), peru_data_long)
    fit2 <- feols(formula(peru_violencia_sexual ~ Index | years_old + education_level
                          + employed + marital_status + Departamento), peru_data_long)
    fit3 <- feols(formula(peru_violencia_psicologica ~ Index | years_old + education_level
                          + employed + marital_status + Departamento), peru_data_long)
    
    etable(list(fit1, fit2, fit3),
           tex = FALSE,
           fitstat = c("n", "r2"), signif.code = NA
    )
  })
  
  output$final_model_mx <- renderPrint({
    fit1 <- feols(formula(mexico_violencia_fisica ~ Index | education_level
                          + employed + marital_status + state), mexico_data_long)
    fit2 <- feols(formula(mexico_violencia_sexual ~ Index | education_level
                          + employed + marital_status + state), mexico_data_long)
    fit3 <- feols(formula(mexico_violencia_psicologica ~ Index | education_level
                          + employed + marital_status + state), mexico_data_long)
    
    etable(list(fit1, fit2, fit3),
           tex = FALSE,
           fitstat = c("n", "r2"), signif.code = NA
    )
  })
}

shinyApp(ui = ui, server = server)

# 3. Text processing

## Loading the policy documents from both Mexico and Peru

mimp_peru <- pdf_text(paste0(path, "/Data/Peru/MIMP-violencia-basada_en_genero.pdf"))

mpg_mexico <- pdf_text(paste0(path, "/Data/Mexico/Manual_Violencia_de_G_nero_en_Diversos_Contextos2.pdf"))


## Separating words, sentences and ngrams

mexico <- tibble(text = mpg_mexico)
word_tokens_mexico <- unnest_tokens(mexico, word_tokens, text, token = "words")
sentence_tokens_mexico <- unnest_tokens(mexico, sent_tokens, text, token = "sentences")
ngram_tokens_mexico <- unnest_tokens(mexico, ngram_tokens, text, token = "ngrams", n = 2)

peru <- tibble(text = mimp_peru)
word_tokens_peru <- unnest_tokens(peru, word_tokens, text, token = "words")
sentence_tokens_peru <- unnest_tokens(peru, sent_tokens, text, token = "sentences")
ngram_tokens_peru <- unnest_tokens(peru, ngram_tokens, text, token = "ngrams", n = 2)



## Adding Spanish words to stop words using the tm library
stop_words_spanish <- bind_rows(
  stop_words,
  data_frame(
    word = stopwords("spanish"),
    lexicon = "custom"
  )
)

peru_no_sw <- anti_join(word_tokens_peru, stop_words_spanish,
                        by = c("word_tokens" = "word")
)

mexico_no_sw <- anti_join(word_tokens_mexico, stop_words_spanish,
                          by = c("word_tokens" = "word")
)

## Analyzing sentiments
sentiment_nrc <-
  get_sentiments("nrc") %>%
  rename(nrc = sentiment)
sentiment_afinn <-
  get_sentiments("afinn") %>%
  rename(affin = value)
sentiment_bing <-
  get_sentiments("bing") %>%
  rename(bing = sentiment)


## Plotting the Peruvian policy document

### Plotting the Peruvian policy document

peru_no_sw <- peru_no_sw %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_bing, by = c("word_tokens" = "word"))


### Sentiment bing

ggplot(data = filter(peru_no_sw, !is.na(bing))) +
  geom_histogram(aes(bing, fill = bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title =
      "Peruvian Gender Based Policy Conceptual Framework Sentiment (Bing)"
  ) +
  theme(legend.position = "none")

ggsave("images/peru_sentiment_bing.png", width = 8, height = 7)


### Sentiment affin

ggplot(data = filter(peru_no_sw, !is.na(affin))) +
  geom_histogram(aes(affin, fill = affin < 0), stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = 
         "Peruvian Gender Based Policy Conceptual Framework Statement (AFFIN)") +
  theme(legend.position = "none")

ggsave("images/peru_sentiment_affin.png", width = 8, height = 7)


## Plotting the Mexican policy document

### Plotting the Mexican policy document
mexico_no_sw <- mexico_no_sw %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_bing, by = c("word_tokens" = "word"))

ggplot(data = filter(mexico_no_sw, !is.na(bing))) +
  geom_histogram(aes(bing, fill = bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Mexican Gender Based Policy Conceptual Framework Sentiment (Bing)") +
  theme(legend.position = "none")

ggsave("images/mexico_sentiment_bing.png", width = 8, height = 7)

### Sentiment affin
ggplot(data = filter(mexico_no_sw, !is.na(affin))) +
  geom_histogram(aes(affin, fill = affin < 0), stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "Mexican Gender Based Policy Conceptual Framework Statement (AFFIN)") +
  theme(legend.position = "none")

ggsave("images/mexico_sentiment_affin.png", width = 8, height = 7)


## Create wordclouds

#webshot::install_phantomjs()

###  Mexico word cloud

mexico_wordnumber <- table(mexico_no_sw$word_tokens)
mexico_wordcloud <- wordcloud2(data = tail(mexico_wordnumber, 500), size = 5)

saveWidget(mexico_wordcloud, "images/mexico_wordcloud.html", selfcontained = FALSE)

webshot("images/mexico_wordcloud.html", "images/mexico_wordcloud.png",
        vwidth = 1992,
        vheight = 1744, delay = 15, selector = "#canvas"
)

### Peru word cloud

peru_wordnumber <- table(peru_no_sw$word_tokens)
peru_wordcloud <- wordcloud2(data = tail(peru_wordnumber, 500), size = 5)

saveWidget(peru_wordcloud, "images/peru_wordcloud.html", selfcontained = FALSE)

webshot("images/peru_wordcloud.html", "images/peru_wordcloud.png",
        vwidth = 800,
        vheight = 800, delay = 10, selector = "#canvas"
)

# Regressions

## Regression Peru

fit1 <- feols(formula(peru_violencia_fisica ~ Index | years_old + education_level
  + employed + marital_status + Departamento), peru_data_long)
fit2 <- feols(formula(peru_violencia_sexual ~ Index | years_old + education_level
  + employed + marital_status + Departamento), peru_data_long)
fit3 <- feols(formula(peru_violencia_psicologica ~ Index | years_old + education_level
  + employed + marital_status + Departamento), peru_data_long)

etable <- etable(list(fit1, fit2, fit3),
  tex = FALSE,
  fitstat = c("n", "r2"), signif.code = NA, export = paste0(path, "/images/reg_peru.txt")
)


etable


## Regression Mexico

fit1 <- feols(formula(mexico_violencia_fisica ~ Index | education_level
+ employed + marital_status + state), mexico_data_long)
fit2 <- feols(formula(mexico_violencia_sexual ~ Index | education_level
+ employed + marital_status + state), mexico_data_long)
fit3 <- feols(formula(mexico_violencia_psicologica ~ Index | education_level
+ employed + marital_status + state), mexico_data_long)

etable <- etable(list(fit1, fit2, fit3),
  tex = FALSE,
  fitstat = c("n", "r2"), signif.code = NA, export = paste0(path, "/images/reg_mex.txt")
)


etable