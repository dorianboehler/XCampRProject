# XCampRProject: Rent Calculator

## About the Project
The goal of this project was to produce a Shiny application that estimates the total rent of a flat whose characteristics the user of the application can specify. Shiny is an R package for creating interactive web applications. In order to achieve this goal, we
* downloaded data on apartment rental offers in Germany from [kaggle.com](https://www.kaggle.com/corrieaar/apartment-rental-offers-in-germany),
* cleaned the data,
* analysed the data by computing descriptive statistics and estimating a multiple linear regression model,
* visualised the statistical results, and
* built the application.

The application is composed of the three tabs
* *Rent Calculator*, which is the main part of the application,
* *Variable Description*, which briefly describes the variables that the user of the application can specify, and
* *Description and Analysis of the Data*, which describes (at the national level and the state level) and analyses the underlying data.

The application is particularly targeted at people who are planning to move to Germany or to move house in Germany.

## Folder Structure
This GitHub repository comprises code, graphics and data.

The code is split into [prepareApp.R](code/prepareApp.R) and [app.R](code/app.R). The file [prepareApp.R](code/prepareApp.R) reads, cleans, analyses, and visualises the data. The file [app.R](code/app.R) generates the user interface and builds the server, which uses reactive programming. The folder [code](code) also contains the folder [www](code/www), in which an image that is used in the application is saved.

The folder [data](data) contains [the data on apartment rental offers in Germany](data/immoData.zip), which is described in more detail in the application, and [necessary data to produce maps](data/DEU_adm).

## Installation

1. **Download the GitHub repository**: We strongly advise people who would like to run the code to save the GitHub repository locally in the given folder structure.

2. **Install packages**: The following packages are required:
  * `tidyverse`
  * `stringr`
  * `formattable`
  * `rgdal`
  * `ggmap`
  * `ggthemes`
  * `tidymodels`
  * `shiny`
  * `shinythemes`
  * `shinyWidgets`


3. **Set the working directory**: When you run [prepareApp.R](code/prepareApp.R) or [app.R](code/app.R), the working directory is automatically set to the right path, provided that the given folder structure has not been changed.

4. **Needed only once (!): Run** [**prepareApp.R**](code/prepareApp.R): The output of this file is
  * saved in the folder [code](code),
  * called *prepareApp.RData*, and
  * used by the file [app.R](code/app.R).


5. **Run** [**app.R**](code/app.R): Run the application either by clicking on the "Run App" button in RStudio or by using the keyboard shortcut `Cmd/Ctrl` + `Shift` + `Enter`. Again, this works only if [prepareApp.R](code/prepareApp.R) has been run once so that *prepareApp.RData* has been produced.

## Sources
* Data on apartment rental offers in Germany: [https://www.kaggle.com/corrieaar/apartment-rental-offers-in-germany](https://www.kaggle.com/corrieaar/apartment-rental-offers-in-germany)
* Data to create maps: [https://www.diva-gis.org/gdata](https://www.diva-gis.org/gdata)

## Team
* Dorian Böhler ([XCamp](https://codingxcamp.com): dorianboehler)
* Gabriel Buchli ([XCamp](https://codingxcamp.com): electrostar)
* Raphael Buss ([XCamp](https://codingxcamp.com): RaphaelB)
* Martin Rutishauser ([XCamp](https://codingxcamp.com): maruto)
