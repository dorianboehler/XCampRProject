# In this R script, the application is created. There are two main parts (after the preparation). 
# First, there is the user interface. Second, there is the server, where the computations are done.


# Preparation -------------------------------------------------------------

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Run prepareApp.R
source("prepareApp.R")


# User interface ----------------------------------------------------------

ui <- fluidPage(theme = shinytheme("superhero"),
                
                # Use navigation bare at the top of the page
                navbarPage("Rent Calculator",
                           
                           # First tab
                           tabPanel("Rent Calculator",
                                    
                                    # Data input on the left side of the page
                                    sidebarPanel(
                                      titlePanel("Input"),
                                      
                                      # Define drop-downs to select input (the left string refers to the variable name in the server, the next string refers to the name in the application and the last string refers to the content of the drop-downs)
                                      # When "Any" is chosen, the variable is excluded from the prediction
                                      selectInput("regio1", "State", choices = c("Choose State", regio1_levels)),
                                      selectInput("firingTypes", "Firing Type", choices = c("Any", firingTypes_levels)),
                                      selectInput("condition", "Condition", choices = c("Any", condition_levels)),
                                      selectInput("floor", "Floor", choices = c("Any", floor_levels)),
                                      selectInput("yearConstructedRounded", "Construction Year", choices = c("Any", yearConstructedRounded_levels)),
                                      # Radio buttons for binary inputs
                                      radioButtons("balcony", "Balcony", c("Yes", "No", "Any"), selected = "Any", inline = TRUE),
                                      radioButtons("cellar", "Cellar", c("Yes", "No", "Any"), selected = "Any", inline = TRUE),
                                      # Splitlayout between choosing a value for the prediction and excluding a variable from the prediction
                                      splitLayout(cellWidths = c("75%", "25%"),
                                                  numericInput("livingSpace", "Living space (sq m)", value = 50, min = 10, max = 10000),
                                                  prettyCheckbox("notuse_livingSpace", "Any", value = TRUE, status = "danger", shape = "round", outline = TRUE, bigger = TRUE)),
                                      splitLayout(cellWidths = c("75%", "25%"),
                                                  sliderInput("noRooms", "Number of Rooms", value = 2.5, min = 1, max = 10, step = 0.5),
                                                  prettyCheckbox("notuse_noRooms", "Any", value = TRUE, status = "danger", shape = "round", outline = TRUE, bigger = TRUE))
                                    ),
                                    
                                    # Show results of data input
                                    mainPanel(
                                      fluidRow(
                                        # Include an image on the top
                                        img(src="HSG_Alumni_Haus_Mrz20_128.jpg", align = "center", height = 500, width = "100%"),
                                        br(),
                                        # Main title
                                        h1("Rent Calculator", align = "center"),
                                        # Subtitle
                                        h2("Information"),
                                        # Information about the application
                                        p("Hier Text reinschreiben: Um herauszufinden wo, im Text nach 1234 suchen. Hier kann jemand noch etwas reinschreiben. Dient auch zum auffüllen der Seite. Andererseits auflisten wie man Daten eingibt und was dies bedeutet. Hier bitte noch schreiben, dass Anzahl Zimmer und m2 nicht gleichzeitig benutzt werden sollen (komische Resultate)"),
                                        h2("Result"),
                                        # Prediction
                                        htmlOutput("prediction")
                                      )
                                    )
                           ),
                           
                           # Second tab: Tab with a description and an analysis of the data
                           tabPanel("Description and Analysis of the Data",
                                    mainPanel(
                                      fluidRow(
                                        h1(strong("Data"), align = "center"),
                                        br(),
                                        p("The data set contains information on apartment rental offers in Germany.
                                        It was scraped from Immoscout24, which is the biggest real estate platform in Germany. 
                                        It contains all offers from the 22 September 2018, the 10 May 2019 and the 8 October 2019."),
                                        br(),
                                        p("We observe the following means of total rent, year constructed, living space and number of rooms in the data set:"),
                                        br(),
                                        tableOutput("table"),
                                        br(),
                                        p("Total rent is", strong("on a monthly basis"), "."),
                                        br(),
                                        p("When we examine the data set in more detail by computing the mean of total rent at the state level, we see the following:"),
                                        br(),
                                        h2("Mean total rent (€) per state", align = "center"),
                                        br(),
                                        plotOutput("plot1"),
                                        br(),
                                        p("Unsurprisingly, the states with the most expensive rental offers on average are the two city states Hamburg (€ 1110) and Berlin (€ 1029), 
                                        and the south states Hessen (€ 1023), Bayern (€ 1001) and Baden-Württemberg (€ 1000). 
                                        The states with the cheapest rental offers on average are all in the East (Thüringen with € 490, Sachsen-Anhalt with € 493 and Sachsen with € 526)."),
                                        br(),
                                        p("Computing the mean year constructed at the state level, we see the following:"),
                                        br(),
                                        h2("Mean year constructed per state", align = "center"),
                                        br(),
                                        plotOutput("plot2"),
                                        br(),
                                        p("The states with the youngest real estate on average are all in the south (Bayern with 1988, Baden-Württemberg with 1984 and Rheinland-Pfalz with 1982). 
                                        Interestingly, while Berlin was one of the states with the most expensive rental offers on average, 
                                        it is one of the state with the oldest real estate on average (1964). 
                                        Only Sachsen (1947), Sachsen-Anhalt (1958), and Thüringen (1961) (all in the East) rank worse with regard to year constructed."),
                                        br(),
                                        p("Computing the mean living space (sq m) at the state level, we see the following:"),
                                        br(),
                                        h2("Mean living space (sq m) per state", align = "center"),
                                        br(),
                                        plotOutput("plot3"),
                                        br(),
                                        p("And again, the states with the largest real estate on average are all in the south (Rheinland-Pfalz with 77 sq m, Baden-Württemberg with 77 sq m and Hessen with 76 sq m). 
                                        Thüringen (61 sq m), Sachsen-Anhalt (61 sq m) and Sachsen (62 sq m) rank among the states with the smallest real estate on average. 
                                        Only Mecklenburg-Vorpommern (61 sq m) (in the north-east) ranks worse with regard to living space (sq m)."),
                                        br(),
                                        p("Computing the mean number of rooms at the state level, we see the following:"),
                                        br(),
                                        h2("Mean number of rooms per state", align = "center"),
                                        br(),
                                        plotOutput("plot4"),
                                        br(),
                                        p("With regard to number of rooms, the states are very similar. The difference between the state that ranks best (Baden-Württemberg with 2.7) 
                                        and the state that ranks worst (Berlin with 2.3) is only about 0.5. As expected, the city states Berlin (2.3), Hamburg (2.4) and Bremen (2.5) are all in the lower half."),
                                        br(),
                                        p("Computing the mean condition at the state level, we see the following:"),
                                        br(),
                                        h2("Mean condition per state (the lighter, the better)", align = "center"),
                                        br(),
                                        plotOutput("plot5"),
                                        br(),
                                        p("The states with the qualitatively best real estate on average are all in the south. 
                                        While the grades go from 1 to 9, Bayern has a grade of 5.9, Baden-Württemberg has a grade of 5.7 and Hessen has a grade of 5.6. 
                                        The city states Hamburg (5.6), Berlin (5.5), and Bremen (5.5) follow. Interestingly, Saarland (4.7) (in the south-west) ranks worst with regard to condition."),
                                        br(),
                                        h1(strong("Multiple regression model"), align = "center"),
                                        br(),
                                        p("Being interested in the ceteris paribus effect of different variables on total rent, 
                                        we estimate a multiple regression model. We include the following variables in the analysis:"),
                                        tags$li("state,"), tags$li("balcony,"), tags$li("firing type,"), tags$li("cellar,"), tags$li("living space (sq m),"), tags$li("condition,"), tags$li("floor, and"), tags$li("year constructed."),
                                        br(),
                                        p("We exclude the number of rooms because of multicollinearity with living space (sq m)."),
                                        br(),
                                        p("These variables are all statistically significant (mostly at the 1% level) and explain about 75% of the variation in total rent. 
                                        You can find a detailed presentation of the results at the bottom of this section."),
                                        br(),
                                        p("Clearly, total rent is most strongly affected by the state in which a property is located. These coefficients are as follows:"),
                                        br(),
                                        h2("Rent increases (€) as a result of different locations (ceteris paribus)", align = "center"),
                                        br(),
                                        plotOutput("plot6"),
                                        br(),
                                        p("Therefore, a property with the same characteristics costs up to € 478 more just because of the state in which it is located. 
                                        For example, if you want to rent a fully renovated, ground-floor property that was constructed in the 2000s with a balcony, renewable energy, a cellar, and a living space of 50 sq m, 
                                        you have to pay € 404 in Thüringen and € 882 in Hamburg. The states with the highest coefficients are the two city states Hamburg (€ 478) and Berlin (€ 421), 
                                        which is similar to our results above."),
                                        br(),
                                        p("The year in which a property was constructed affects total rent in a very interesting way:"),
                                        br(),
                                        h2("Rent increases (€) as a result of different years constructed (ceteris paribus)", align = "center"),
                                        br(),
                                        plotOutput("plot7"),
                                        br(),
                                        p("From 1900 (and before) to 1989, the relationship between age of a property and total rent of a property is positive. 
                                        After 1989, however, this relationship turns negative. Unsurprisingly, the largest rent increase must be expected when a property was constructed in or after 2020 (€ 185 compared to the 1980s)."),
                                        br(),
                                        p("The following plot shows that the firing type does not have a very strong effect on total rent:"),
                                        br(),
                                        h2("Rent increases (€) as a result of different firing types (ceteris paribus)", align = "center"),
                                        br(),
                                        plotOutput("plot8"),
                                        br(),
                                        p("Some money (€ 50 to € 100) can be saved by using electricity or pellet heating. 
                                        Perhaps surprisingly, renewable energy is not among the most expensive firing types (€ 52 cheaper than the most expensive firing type). 
                                        This may be explained by the high subsidy of green energy in Germany (see", 
                                          a("here", href = "https://www.bloomberg.com/news/articles/2021-01-12/germany-paid-record-38-billion-for-green-power-growth-in-2020"), "for example)."),
                                        br(),
                                        p("The condition of a property affects total rent as follows:"),
                                        br(),
                                        h2("Rent increases (€) as a result of different conditions (ceteris paribus)", align = "center"),
                                        br(),
                                        plotOutput("plot9"),
                                        br(),
                                        p("Interestingly, first-time use appears to be an important driver of total rent."),
                                        br(),
                                        p("The floor of a property does not have a practically very significant effect on total rent:"),
                                        br(),
                                        h2("Rent increases (€) as a result of different floors (ceteris paribus)", align = "center"),
                                        br(),
                                        plotOutput("plot10"),
                                        br(),
                                        p("Obviously, living underground is cheaper than living overground. The rent difference is about € 43."),
                                        br(),
                                        p("Lastly, the prices of a balcony and a cellar are as follows:"),
                                        br(),
                                        h2("Rent increase (€) as a result of a balcony or a cellar (ceteris paribus)", align = "center"),
                                        br(),
                                        plotOutput("plot11"),
                                        br(),
                                        p("A balcony and a cellar cost about € 36 and € 7, respectively. These prices may be lower than expected."),
                                        br(),
                                        p("It is important to emphasise that these numbers all refer to expected values, from which the numbers in a specific case can (or rather will) deviate.", style = "color:red"),
                                        br(),
                                        h2("Detailed presentation of the results", align = "center"),
                                        br(),
                                        verbatimTextOutput(outputId = "regOut"),
                                        br(),
                                        h1(strong("R packages used"), align = "center"),
                                        br(),
                                        tags$li(code("tidyverse"), ": Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686,", 
                                                a("https://doi.org/10.21105/joss.01686", href = "https://doi.org/10.21105/joss.01686")),
                                        tags$li(code("stringr"), ": Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0.",
                                                a("https://CRAN.R-project.org/package=stringr", href = "https://CRAN.R-project.org/package=stringr")),
                                        tags$li(code("formattable"), ": Kun Ren and Kenton Russell (2021). formattable: Create 'Formattable' Data Structures. R package version 0.2.1.",
                                                a("https://CRAN.R-project.org/package=formattable", href = "https://CRAN.R-project.org/package=formattable")),
                                        tags$li(code("rgdal"), ": Roger Bivand, Tim Keitt and Barry Rowlingson (2021). rgdal: Bindings for the 'Geospatial' Data Abstraction Library. R package version 1.5-27.",
                                                a("https://CRAN.R-project.org/package=rgdal", href = "https://CRAN.R-project.org/package=rgdal")),
                                        tags$li(code("ggmap"), ": D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL",
                                                a("http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf", href = "http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf")),
                                        tags$li(code("ggthemes"), ": Jeffrey B. Arnold (2021). ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'. R package version 4.2.4.", 
                                                a("https://CRAN.R-project.org/package=ggthemes", href = "https://CRAN.R-project.org/package=ggthemes")),
                                        tags$li(code("tidymodels"), ": Kuhn et al., (2020). Tidymodels: a collection of packages for modeling and machine learning using tidyverse principles.",
                                                a("https://www.tidymodels.org", href = "https://www.tidymodels.org")),
                                        tags$li(code("shiny"), ": Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application Framework for R. R package version 1.7.1.",
                                                a("https://CRAN.R-project.org/package=shiny", href = "https://CRAN.R-project.org/package=shiny")),
                                        tags$li(code("shinythemes"), ": Winston Chang (2021). shinythemes: Themes for Shiny. R package version 1.2.0.",
                                                a("https://CRAN.R-project.org/package=shinythemes", href = "https://CRAN.R-project.org/package=shinythemes")),
                                        tags$li(code("shinyWidgets"), ": Victor Perrier, Fanny Meyer and David Granjon (2021). shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.6.2.",
                                                a("https://CRAN.R-project.org/package=shinyWidgets", href = "https://CRAN.R-project.org/package=shinyWidgets")),
                                        br(),
                                        br(),
                                        br()
                                      )
                                    )
                           )
                )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # First, we need to define which independent variables should be used for the prediction.
  # This works the following way: A reactive that contains a vector of booleans is created. These booleans
  # are aligned to the predefined vector that contains the names of all independent variables (indep_var). 
  # If the user chooses an independent variable to be "Any", the respective boolean will be FALSE and
  # the independent variable will be excluded. If the user chooses a certain value for an independent variable,
  # the respective boolean will be TRUE and the independent variable will be included.
  
  used_var_bool <- reactive(c(input$regio1 != "Choose State", input$balcony != "Any",
                              input$firingTypes != "Any", input$cellar != "Any",
                              input$notuse_livingSpace != TRUE, input$condition != "Any",
                              input$notuse_noRooms != TRUE, input$floor != "Any", 
                              input$yearConstructedRounded != "Any"))
  
  # Define the roles of the variables
  recipe_formula <- reactive(
    immo_data_inp %>%
      recipe() %>%
      update_role(totalRent, new_role = "outcome") %>%
      update_role(!!!all_of(indep_var[used_var_bool()]), new_role = "predictor") %>%
      prep() %>%
      formula()
  )
  
  # Create the model
  reg_model_inp <- reactive(
    lm(recipe_formula(), data = immo_data_inp)
  )
  
  # Total rent prediction
  output$prediction <- renderUI({
    
    # Replace "Yes" and "No" with TRUE and FALSE, respectively
    if(input$balcony == "Yes") {
      balcony <- TRUE
    } else {
      balcony <- FALSE
    }
    
    if(input$cellar == "Yes"){
      cellar <- TRUE
    } else {
      cellar <- FALSE
    }
    
    # Create a data frame with the values that the user chooses and are therefore used to predict total rent
    reg_model_inp_data <- data.frame(matrix(nrow = 1, ncol = 0))
    
    # Additionally transform the inputs in such a way that they match the underlying data
    if(input$regio1 != "Choose State") {
      reg_model_inp_data$regio1 <- str_replace_all(input$regio1, "-", "_")
    }
    if(input$balcony != "Any") {
      reg_model_inp_data$balcony <- as.logical(balcony)
    }
    if(input$yearConstructedRounded != "Any") {
      reg_model_inp_data$yearConstructedRounded <- str_to_lower(str_replace_all(input$yearConstructedRounded, " ", "_"))
    }
    if(input$firingTypes != "Any") {
      reg_model_inp_data$firingTypes <- str_to_lower(str_replace_all(input$firingTypes, " ", "_"))
    }
    if(input$cellar != "Any") {
      reg_model_inp_data$cellar <- as.logical(cellar)
    }
    if(input$notuse_livingSpace != TRUE) {
      reg_model_inp_data$livingSpace <- input$livingSpace
    }
    if(input$condition != "Any") {
      reg_model_inp_data$condition <- str_to_lower(str_replace_all(input$condition, " ", "_"))
    }
    if(input$notuse_noRooms != TRUE) {
      reg_model_inp_data$noRooms <- input$noRooms
    }
    if(input$floor != "Any") {
      reg_model_inp_data$floor <- str_to_lower(str_replace_all(input$floor, " ", "_"))
    }
    
    # Only return a result if state is specified
    if(input$regio1 != "Choose State") {
      textOutput <- paste0("The expected monthly total rent of a property that matches your wishes is ", 
                           "<font color=\"#00B341\"><b>", round(predict(reg_model_inp(), reg_model_inp_data), 0), 
                           " €", "</font color=\"#00B341\"></b>", ".")
    } else {
      # Text that is displayed if the user has not chosen a state
      textOutput <- "Please select a state."
    }
    
    # Define the variable prediction
    HTML(paste(textOutput))
  })
  
  # Multiple regression model for the purpose of data analysis (second tab)
  output$regOut <- renderPrint({summary(reg_model)})
  
  # Table and plots that will be displayed in the second tab
  output$table <- renderTable({
    table1
  })
  
  output$plot1 <- renderPlot({
    plot1
  })    
  output$plot2 <- renderPlot({
    plot2
  })    
  output$plot3 <- renderPlot({
    plot3
  })    
  output$plot4 <- renderPlot({
    plot4
  })    
  output$plot5 <- renderPlot({
    plot5
  })    
  output$plot6 <- renderPlot({
    plot6
  })    
  output$plot7 <- renderPlot({
    plot7
  })    
  output$plot8 <- renderPlot({
    plot8
  })    
  output$plot9 <- renderPlot({
    plot9
  })    
  output$plot10 <- renderPlot({
    plot10
  })    
  output$plot11 <- renderPlot({
    plot11
  })
}


# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)


