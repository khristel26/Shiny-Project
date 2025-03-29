# Data and Programming for Public Policy II - R Programming
# PPHA 30536


## Final Project: Reproducible Research
## Autumn 2022


## Due: Wednesday, December 7th by midnight on GitHub

## Team: Khristel Zavaleta Gamarra and Diego Madrazo Paheco

# Project: Tolerance for violence and its relation to violence against women. A comparison between Mexico and Peru.

## Relevant urls:

### shiny:
https://khristel26.shinyapps.io/final-project-diego_khristel/

  # The shiny capacity was reached and, therefore, is out of memory. This causes the shiny online app to crash and not     load completely.


### Google Drive: 

https://drive.google.com/file/d/14OPBvNYbD5vE2vJCKeRkRavZWG4uD5b6/view?usp=sharing

https://drive.google.com/file/d/1piqn7PXBoWyMOy-fkTjMqQLeYO8T6Ivv/view?usp=sharing


Both violence and violence against women are prevalent in many Latin American countries, including Mexico and Peru. We decided to study how the tolerance to the former affects the latter. Hence, the research question of this project is: How is tolerance for violence related to violence against women? 

The approach we took was to gather data from the surveys Mexico and Peru does about social relations and dynamics. For the Andean country, it is the National Poll About Social Relations (ENARES) and for the country that borders USA is the National Poll About the Dynamics of Relations in Households (ENDIREH). We chose the last three editions of these polls but we decided to exclude the 2011 edition of ENDIREH for reasons that will be discussed in a following paragraph. 

Once we had the surveys, we analyzed the questions and chose some that could be as similar as both surveys allowed and showed how tolerant are both societies towards violence, gender roles and violence against women. These variables where used as inputs for an Index of Social Tolerance to Violence. 


From the same surveys we gathered fixed effect variables that were age, educational level, if they were employed, marital status and their local administrative region. 

From official sources we also gathered the prevalence of sexual, physical, and psychological violence in each administrative region of both Peru and Mexico as dependent variables. With all the gathered information we we created the aforementioned Index. 

With the data gathered we created a heat cloropeth map of each administrative region of each country. This visually illustrates that in Peru the Departament with the highest Index of Social Tolerance to Violence is Ancash while Huancavelica tolerates  violence the least. In the Mexican case, the southern state of Chiapas has the lowest score in the Index while Baja California Sur the highest. 

An interesting finding is that while Ancash has the highest score across the board, Mexico has in general higher values. 

Continuing with the plotting section, we created animated plots that illustrate the relationship between each of the dependent variables and our Index throughout time. Finally, our last plot of this section illustrate how femicides have been increasing for the last 10 years in both countries, though the rate is higher in Mexico, being consistent with the higher scores of the Index. The femicides data was obtained through World Bank Data.

For the text analysis we chose two policy documents, one from each country, to analyze the sentiment of these documents and, therefore, how the issue is considered. To add to this information, we did a cloud map to trace the highest repeated words to see how addressing violence is focused.

The text analysis shows that Mexico in general has a more negative language in this document, which is shown with both the affin and bing analysis. Nonetheless, some ofthe most common words in the Mexican case are of "prevention", "security"; while the Peruvian shows words as "traffic"and "rape". 

To end, we created a model to illustrate which of our questions has the most significance. In the Peruvian case, the variable psychological has the greatest impact while in the Mexican case it is sexual violence. Again, Mexico has higher scores. 

## Challenges

The first challenge we faced was working with survey data. The way it is organized is challenging as sometimes several csv files have to be read to understand one dataframe. In addition, questions are not the same and standarazing them can me difficult. This can be true even with demographics: values for marital status and educational level where different in both countries so we had to stablish criteria that would have the similar interpretations. 

Another challenges with survey data is that the 2011 ENDIREH was incompatible. It didn't have information like state that was needed to continue. That was the reason it was excluded. If we decided to continue, it was in obsolete format (.dbf).

## Strengths and weaknesses

The main strength is that is shows how tolerance violence has increased in Mexico and Peru and how it is related to violence against women. The main strenght is that it raises questions of, how much does this data helps to make generalizations and how tweaking the data to make similarities have it limits. 