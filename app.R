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
#install.packages("knitr")
require(knitr)
library(WDI)
library(ggplot2)
#install.packages("extrafont")
library("extrafont")

#setwd("/Users/khristelzavaleta/Desktop/Uchicago/Q4/Data and programming II/Homework/final-project-diego_khristel")

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
            "- the ", strong("'Manual for Gendered Violence  in Diverse Contexts'"), "from Mexico.",
            style = "color:black;text-align:center"
          ),
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
              choices = c("Peru", "Mexico")
            )),
            column(align = "center", width = 7, radioButtons(
              inputId = "sentiment",
              label = p("Type of sentiment Analysis", style = "color:#EE7AE9;text-align:justify"),
              choices = c("Bing", "Affin")
            ))
          )
        ),
        mainPanel(
          h3(strong("Text analysis", style = "color:#EEA9B8")),
          fluidRow(column(
            width = 10,
            align = "center", imageOutput("text_analysis")
          ))
        )
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
              choices = c("Peru", "Mexico")
            ))
          )
        ),
        mainPanel(
          h3(strong("Word Cloud analysis", style = "color:#EEA9B8")),
          fluidRow(column(
            width = 10,
            align = "center", imageOutput("cloud_analysis")
          ))
        )
      )
    ),
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


  output$text_analysis <- renderImage(
    {
      if (input$count_Per_Mex == "Peru") {
        if (input$sentiment == "Bing") {
          list(
            src = "www/peru_sentiment_bing.png",
            contentType = "image/png", width = "500px", height = "500px"
          )
        } else if (input$sentiment == "Affin") {
          list(
            src = "www/peru_sentiment_affin.png",
            contentType = "image/png", width = "500px", height = "500px"
          )
        }
      } else if (input$count_Per_Mex == "Mexico") {
        if (input$sentiment == "Bing") {
          list(
            src = "www/mexico_sentiment_bing.png",
            contentType = "image/png", width = "500px", height = "500px"
          )
        } else if (input$sentiment == "Affin") {
          list(
            src = "www/mexico_sentiment_affin.png",
            contentType = "image/png", width = "500px", height = "500px"
          )
        }
      }
    },
    deleteFile = FALSE
  )


  output$cloud_analysis <- renderImage(
    {
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