# Set up ------------------------------------------------------------------

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required packages
library(tidyverse, quietly = TRUE)
library(stringr, quietly = TRUE)
library(formattable, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(ggmap, quietly = TRUE)
library(ggthemes, quietly = TRUE)
library(tidymodels, quietly = TRUE)
library(shiny, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)


# Data import -------------------------------------------------------------

# Load data on apartment rental offers in Germany
# Data from: https://www.kaggle.com/corrieaar/apartment-rental-offers-in-germany (login required)
immo_data <- read_csv("../data/immoData.zip")


# Data preparation --------------------------------------------------------

# Select all variables that we deem to be relevant in our model (and do not have more than 50 percent NAs)
immo_data_inp <- immo_data %>%
  select(totalRent, regio1, balcony, yearConstructed, firingTypes, cellar, 
         livingSpace, condition, noRooms, floor)

# Remove all values that contain NAs
immo_data_inp <- na.omit(immo_data_inp)

# Remove outliers in continuous variables
continuous_variables <- c("totalRent", "yearConstructed", "livingSpace", "noRooms")

for(i in 1:length(continuous_variables)) {
  col_num <- which(colnames(immo_data_inp) == continuous_variables[i])
  
  outliers <- boxplot.stats(immo_data_inp[[col_num]])$out
  outliers_i <- which(immo_data_inp[[col_num]] %in% outliers)
  
  immo_data_inp <- immo_data_inp[-outliers_i, ]
}

remove(continuous_variables, col_num, outliers, outliers_i)

# Look at the data more closely and remove observations that do not make sense
summary(immo_data_inp)

immo_data_inp <- immo_data_inp %>%
  filter(totalRent >= 100) %>% # There are some observations with total rents equal to zero or nearly equal to zero
  filter(condition != "ripe_for_demolition") # Only one observation

# Round yearConstructed and produce a categorical variable based on this
immo_data_inp$yearConstructedRounded <- plyr::round_any(immo_data_inp$yearConstructed, 10)

for(i in 1:nrow(immo_data_inp)) {
  if(immo_data_inp$yearConstructed[i] < 1900) {
    immo_data_inp$yearConstructedRounded[i] <- "before_1900"
  }
}

immo_data_inp$yearConstructedRounded <- as.factor(immo_data_inp$yearConstructedRounded)

# Investigate the variable firingTypes more closely
unique(immo_data_inp$firingTypes) # There are 91 different categories, which is too many for our application

for(i in 1:nrow(immo_data_inp)) {
  if(grepl(":", immo_data_inp$firingTypes[i])) {
    immo_data_inp$firingTypes[i] <- "mixed"
  }
} # Change all mixed firing types to "mixed"

for(i in 1:nrow(immo_data_inp)) {
  if(grepl("environmental|geothermal|regenerative|bio|wind|renewable|solar|hydro", immo_data_inp$firingTypes[i])) {
    immo_data_inp$firingTypes[i] <- "renewable_energy"
  }
} # Put all renewable energy sources into the same category

for(i in 1:nrow(immo_data_inp)) {
  if(grepl("district", immo_data_inp$firingTypes[i])) {
    immo_data_inp$firingTypes[i] <- "district_heating"
  }
} # Put all district heating systems into the same category

firingTypes_count <- immo_data_inp %>%
  group_by(firingTypes) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) # Count how often each category of firing type appears and put firing types that are rare into "others" 

firingTypes_remove <- firingTypes_count$firingTypes[11:nrow(firingTypes_count)]
immo_data_inp$firingTypes[immo_data_inp$firingTypes %in% firingTypes_remove] <- "other" 

remove(firingTypes_count, firingTypes_remove)

# Produce a categorical variable based on floor
immo_data_inp <- immo_data_inp %>%
  mutate(noFloor = floor) %>%
  mutate(floor = "underground")

for(i in 1:nrow(immo_data_inp)) {
  if(immo_data_inp$noFloor[i] == 0) {
    immo_data_inp$floor[i] <- "ground_floor"
  } else if(immo_data_inp$noFloor[i] == 1) {
    immo_data_inp$floor[i] <- "first_floor"
  } else if(immo_data_inp$noFloor[i] > 1) {
    immo_data_inp$floor[i] <- "second_floor_or_higher"
  }
}

immo_data_inp$floor <- as.factor(immo_data_inp$floor)

immo_data_inp <- select(immo_data_inp, -noFloor)

# Change datatypes of variables
categorical_variables <- c("regio1", "firingTypes", "condition")

for(i in 1:length(categorical_variables)) {
  immo_data_inp <- mutate_at(immo_data_inp, categorical_variables[i], as.factor)
}

remove(categorical_variables)

# Re-level regio1, firingTypes, condition, floor and yearConstructedRounded
immo_data_inp$regio1 <- relevel(immo_data_inp$regio1, ref = "Thüringen")
immo_data_inp$firingTypes <- relevel(immo_data_inp$firingTypes, ref = "electricity")
immo_data_inp$condition <- relevel(immo_data_inp$condition, "need_of_renovation")
immo_data_inp$floor <- relevel(immo_data_inp$floor, ref = "underground")
immo_data_inp$yearConstructedRounded <- relevel(immo_data_inp$yearConstructedRounded, ref = "1980")


# Data analysis -----------------------------------------------------------

# Compute a few descriptive statistics for Germany
table1 <- immo_data_inp %>%
  summarise("Mean Rent (€)" = round(mean(totalRent)),
            "Mean Year Constructed" = round(mean(yearConstructed)),
            "Mean Living Space (sq m)" = round(mean(livingSpace)),
            "Mean Number of Rooms" = round(mean(noRooms), digits = 1))

row.names(table1) <- "Rental offers in Germany"
table1 <- formattable(table1)

# Create linear regression model (OLS)
cor(immo_data_inp$livingSpace, immo_data_inp$noRooms) # The correlation between these two variables is very high (0.7557787), so we do not include both variables in the model (because of multicollinearity)

reg_model <- lm(totalRent ~ . - noRooms - yearConstructed, data = immo_data_inp) # We include yearConstructedRounded instead of yearConstructed
summary(reg_model)

# Create vector with all independent variables for the shiny app
indep_var <- names(immo_data_inp)[c(2, 3, 5:ncol(immo_data_inp))]


# Data visualisation ------------------------------------------------------

# Compute descriptive statistics per state and show them in maps
# Source of the code that creates the maps: https://rstudio-pubs-static.s3.amazonaws.com/297613_b47a07743e9d48ee928d1efe947f6b3f.html
germany <- readOGR(dsn = "../data/DEU_adm", layer = "DEU_adm1", encoding = "UTF-8", use_iconv = TRUE) # Read the data that we need to create the maps
bundes <- fortify(germany)

bundes$regio1 <- factor(as.numeric(bundes$id)) # Add the state names
levels(bundes$regio1) <- germany$NAME_1

immo_data_inp$conditionOrdered <- ordered(immo_data_inp$condition, levels = c("need_of_renovation",
                                                                              "negotiable",
                                                                              "well_kept",
                                                                              "modernized",
                                                                              "refurbished",
                                                                              "fully_renovated",
                                                                              "first_time_use_after_refurbishment",
                                                                              "mint_condition",
                                                                              "first_time_use")) # Order the conditions so that we can compute the mean (even though this can actually not be done reliably with ordinals, it may be interesting to see the result)

condition_levels <- str_to_title(str_replace_all(levels(immo_data_inp$conditionOrdered), "_", " ")) # This vector will be used in the shiny app

descr_stats <- immo_data_inp %>%
  group_by(regio1) %>%
  summarise(totalRent = mean(totalRent),
            yearConstructed = mean(yearConstructed),
            livingSpace = mean(livingSpace),
            noRooms = mean(noRooms),
            condition = mean(as.integer(conditionOrdered))) # Compute descriptive statistics per state

immo_data_inp <- select(immo_data_inp, -conditionOrdered)

descr_stats$regio1 <- as.character(descr_stats$regio1) # Align the state names in the immo data with the state names in the maps data
for(i in 1:nrow(descr_stats)) {
  descr_stats$regio1[i] <- str_replace_all(descr_stats$regio1[i], "_", "-")
}

bundes <- merge(bundes, descr_stats) # Merge the immo data and the maps data

berlin <- filter(bundes, regio1 == "Berlin") # In the ggplot, we need to create an additional layer that only contains Berlin because Berlin gets covered by Brandenburg otherwise (this is wrongly not done in the source code, so that you cannot see Berlin there)
bremen <- filter(bundes, regio1 == "Bremen") # In the ggplot, we need to create an additional layer that only contains Bremen because Bremen gets covered by Niedersachsen otherwise (this is wrongly not done in the source code, so that you cannot see Bremen there)

plot1 <- ggplot(mapping = aes(x = long, y = lat, group = group, fill = totalRent)) +
  geom_polygon(data = bundes) +
  geom_polygon(data = berlin) +
  geom_polygon(data = bremen) +
  labs(fill = "Mean total rent (€)") +
  coord_map() +
  theme_map() +
  theme(legend.position = "right") # Create a map that shows the mean total rent per state

plot2 <- ggplot(mapping = aes(x = long, y = lat, group = group, fill = yearConstructed)) +
  geom_polygon(data = bundes) +
  geom_polygon(data = berlin) +
  geom_polygon(data = bremen) +
  labs(fill = "Mean year constructed") +
  coord_map() +
  theme_map() +
  theme(legend.position = "right") # Create a map that shows the mean year constructed per state

plot3 <- ggplot(mapping = aes(x = long, y = lat, group = group, fill = livingSpace)) +
  geom_polygon(data = bundes) +
  geom_polygon(data = berlin) +
  geom_polygon(data = bremen) +
  labs(fill = "Mean living space (sq m)") +
  coord_map() +
  theme_map() +
  theme(legend.position = "right") # Create a map that shows the mean living space per state

plot4 <- ggplot(mapping = aes(x = long, y = lat, group = group, fill = noRooms)) +
  geom_polygon(data = bundes) +
  geom_polygon(data = berlin) +
  geom_polygon(data = bremen) +
  labs(fill = "Mean number of rooms") +
  coord_map() +
  theme_map() +
  theme(legend.position = "right") # Create a map that shows the mean number of rooms per state

plot5 <- ggplot(mapping = aes(x = long, y = lat, group = group, fill = condition)) +
  geom_polygon(data = bundes) +
  geom_polygon(data = berlin) +
  geom_polygon(data = bremen) +
  coord_map() +
  theme_map() +
  theme(legend.position =  "none") # Create a map that approximatively shows the mean condition per state

remove(berlin, bremen, bundes, descr_stats, germany)

# Analyse the results of the linear regression model: regio1
coefficients <- reg_model$coefficients # Extract all coefficients from the model

coefficients_regio1 <- data.frame("regio1" = character(),
                                  "coefficient" = numeric())
n <- 1
for(i in 1:length(coefficients)) {
  if(grepl("regio1", names(coefficients)[i])) {
    coefficients_regio1[n, 1] <- str_replace(str_replace_all(names(coefficients)[i], "_", "-"), "regio1", "")
    coefficients_regio1[n, 2] <- coefficients[[i]]
    
    n <- n + 1
  }
} # Extract all regio1 coefficients from the model and re-name them

states_germany <- data.frame("regio1" = c("Baden-Württemberg", "Bayern", "Berlin",
                                          "Brandenburg", "Bremen", "Hamburg", "Hessen",
                                          "Mecklenburg-Vorpommern", "Niedersachsen",
                                          "Nordrhein-Westfalen", "Rheinland-Pfalz",
                                          "Saarland", "Sachsen", "Sachsen-Anhalt", 
                                          "Schleswig-Holstein", "Thüringen"),
                             "abbreviation" = c("BW", "BY", "BE", "BB", "HB", "HH",
                                                "HE", "MV", "NI", "NW", "RP", "SL", 
                                                "SN", "ST", "SH", "TH")) # Create a data frame with all state names and abbreviations

coefficients_regio1 <- full_join(coefficients_regio1, states_germany) # Merge the two data frames

coefficients_regio1[which(coefficients_regio1$abbreviation == "TH"), 2] <- 0 # Assign 0 to Thüringen, which is the state with the lowest total rent and we have therefore chosen as the base group

regio1_levels <- sort(unique(coefficients_regio1$regio1)) # This vector will be used in the shiny app

plot6 <- ggplot(coefficients_regio1, aes(x = reorder(abbreviation, coefficient), y = coefficient, fill = coefficient)) +
  geom_bar(stat = "identity") +
  ylim(0, 600) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "German state", y = "Rent increase (€)") +
  theme_minimal() +
  theme(legend.position = "none") # Create a bar plot that shows the rent increases as a result of different locations (ceteris paribus)

remove(coefficients_regio1, states_germany)

# Analyse the results of the model: year constructed rounded
coefficients_yearConstructedRounded <- data.frame("yearConstructedRounded" = character(),
                                                  "coefficient" = numeric())
n <- 1
for(i in 1:length(coefficients)) {
  if(grepl("yearConstructedRounded", names(coefficients)[i])) {
    coefficients_yearConstructedRounded[n, 1] <- str_replace(str_replace_all(names(coefficients)[i], "_", " "), "yearConstructedRounded", "")
    coefficients_yearConstructedRounded[n, 2] <- coefficients[[i]]
    
    n <- n + 1
  }
} # Extract all yearConstructedRounded coefficients from the model and re-name them

coefficients_yearConstructedRounded <- add_row(coefficients_yearConstructedRounded, 
                                               yearConstructedRounded = "1980",
                                               coefficient = 0) # Add the base group "1980", which is the year constructed rounded with the lowest total rent 

coefficients_yearConstructedRounded$yearConstructedRounded <- ordered(coefficients_yearConstructedRounded$yearConstructedRounded,
                                                                      levels = c("before 1900",
                                                                                 seq(1900, 2020, by = 10))) # Order the factor levels according to time

yearConstructedRounded_levels <- levels(coefficients_yearConstructedRounded$yearConstructedRounded) # This vector will be used in the shiny app

plot7 <- ggplot(coefficients_yearConstructedRounded, aes(x = yearConstructedRounded, y = coefficient, fill = coefficient)) +
  geom_bar(stat = "identity") +
  ylim(0, 300) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Year constructed (rounded)", y = "Rent increase (€)") +
  theme_minimal() +
  theme(legend.position = "none") # Create a bar plot that shows the rent increases as a result of different years constructed (ceteris paribus)

remove(coefficients_yearConstructedRounded)

# Analyse the results of the model: firing type
coefficients_firingTypes <- data.frame("firingTypes" = character(),
                                       "coefficient" = numeric())
n <- 1
for(i in 1:length(coefficients)) {
  if(grepl("firingTypes", names(coefficients)[i])) {
    coefficients_firingTypes[n, 1] <- str_to_title(str_replace(str_replace_all(names(coefficients)[i], "_", " "),
                                                               "firingTypes", ""))
    coefficients_firingTypes[n, 2] <- coefficients[[i]]
    
    n <- n + 1
  }
} # Extract all firingTypes coefficients from the model and re-name them

coefficients_firingTypes <- coefficients_firingTypes %>%
  filter(firingTypes != "Mixed" & firingTypes != "Other") %>%
  add_row(firingTypes = "Electricity", coefficient = 0) # Do not show "Mixed" and "Other" in the following bar plot and add the base group "Electricity", which is the firing type with the lowest total rent

firingTypes_levels <- sort(coefficients_firingTypes$firingTypes)
firingTypes_levels <- c(firingTypes_levels, "Mixed", "Other") # This vector will be used in the shiny app

plot8 <- ggplot(coefficients_firingTypes, aes(x = reorder(firingTypes, coefficient), y = coefficient, fill = coefficient)) +
  geom_bar(stat = "identity") +
  ylim(0, 300) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Firing type", y = "Rent increase (€)") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal() +
  theme(legend.position = "none") # Create a bar plot that shows the rent increases as a result of different firing types (ceteris paribus)

remove(coefficients_firingTypes)

# Analyse the results of the model: condition
coefficients_condition <- data.frame("condition" = character(),
                                     "coefficient" = numeric())
n <- 1
for(i in 1:length(coefficients)) {
  if(grepl("condition", names(coefficients)[i])) {
    coefficients_condition[n, 1] <- str_to_title(str_replace(str_replace_all(names(coefficients)[i], "_", " "),
                                                             "condition", ""))
    coefficients_condition[n, 2] <- coefficients [[i]]
    
    n <- n + 1
  }
} # Extract all condition coefficients from the model and re-name them

coefficients_condition <- add_row(coefficients_condition, condition = "Need Of Renovation", coefficient = 0) # Add the base group "Need Of Renovation", which is the condition with the lowest total rent

plot9 <- ggplot(coefficients_condition, aes(x = reorder(condition, coefficient), y = coefficient, fill = coefficient)) +
  geom_bar(stat = "identity") +
  ylim(0, 300) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Condition", y = "Rent increase (€)") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_minimal() +
  theme(legend.position = "none") # Create a bar plot that shows the rent increases as a result of different conditions (ceteris paribus)

remove(coefficients_condition)

# Analyse the results of the model: floor
coefficients_floor <- data.frame("floor" = character(),
                                 "coefficient" = numeric())
n <- 1
for(i in 1:length(coefficients)) {
  if(grepl("floor", names(coefficients)[i])) {
    coefficients_floor[n, 1] <- str_to_title(str_replace(str_replace_all(names(coefficients)[i], "_", " "),
                                                         "floor", ""))
    coefficients_floor[n, 2] <- coefficients [[i]]
    
    n <- n + 1
  }
} # Extract all floor coefficients from the model and re-name them

coefficients_floor <- add_row(coefficients_floor, floor = "Underground", coefficient = 0) # Add the base group "Underground", which is the floor with the lowest total rent
coefficients_floor$floor <- ordered(coefficients_floor$floor, levels = c("Underground", 
                                                                         "Ground Floor",
                                                                         "First Floor",
                                                                         "Second Floor Or Higher")) # Order the factor levels according to the height of the floor 

floor_levels <- levels(coefficients_floor$floor) # This vector will be used in the shiny app

plot10 <- ggplot(coefficients_floor, aes(x = floor, y = coefficient, fill = coefficient)) +
  geom_bar(stat = "identity") +
  ylim(0, 300) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Floor", y = "Rent increase (€)") +
  theme_minimal() +
  theme(legend.position = "none") # Create a bar plot that shows the rent increases as a result of floors (ceteris paribus)

remove(coefficients_floor)

# Analyse the results of the model: balcony and cellar
coefficients_balcony_cellar <- data.frame("variable" = c("Balcony", "Cellar"),
                                          "coefficient" = c(coefficients[["balconyTRUE"]], 
                                                            coefficients[["cellarTRUE"]])) # Extract the balcony coefficient and the cellar coefficient from the model and re-name them

plot11 <- ggplot(coefficients_balcony_cellar, aes(x = variable, y = coefficient)) +
  geom_bar(stat = "identity", fill = "red") +
  ylim(0, 100) +
  labs(x = element_blank(), y = "Rent increase (€)") +
  theme_minimal() +
  theme(legend.position = "none") # Create a bar plot that shows the rent increase as a result of a balcony or a cellar (ceteris paribus)

remove(coefficients_balcony_cellar, coefficients, i, n)


# Save workspace ----------------------------------------------------------

# Save workspace, which is then loaded in app.R
save.image(file = "prepareApp.RData")


