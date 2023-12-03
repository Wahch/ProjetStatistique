#Partie Data Import and Exploration

#Importation des données
data <- read.csv(file = file.choose(), header = TRUE, sep = ';', na.strings = c("", "NA"))
print(data)

#Compréhension des données
View(data)
dim(data)
summary(data)

#Etudier les variables disponibles
library(dplyr)
glimpse(data)

# Vérifier le type des variables
str(data)

#Exemple des valeurs uniques de chaque colonne
unique(data$sex)
unique(data$age)
unique(data$parent_status)
unique(data$travel_time)
unique(data$mother_education)

#installer le package tidyverse
library(tidyverse)

View(data)

#Remplacer les colonnes par leurs vraies valeurs
data$school <- data$sex
data$sex <- data$age
data$age <- data$family_size
data$family_size <- data$parent_status
data$parent_status <- data$mother_education
data$mother_education <- data$travel_time
data$travel_time <- data$study_time
data$study_time <- data$class_failures
data$class_failures <- data$school_support
data$school_support <- data$family_support
data$family_support <- data$extra_paid_classes
data$extra_paid_classes <- data$higher_ed
data$higher_ed<- data$free_time
data$free_time <- data$health
data$health <- data$absences
data$absences <- data$final_grade
data$final_grade <- data$x


#Eliminer la variable X qui est supplémentaire
data <- data[, -which(names(data) == "X")]
View(data)

#Remplacer les valeurs manquantes par NA
library(dplyr)
library(stringr)

data <- data %>%
  mutate_all(~ ifelse(str_replace_all(as.character(.), "\\s+", "") == "", NA, .))

#Visualiser les données après cette opération
View(data)

#Exportation de ce dataset organisé
#write.csv(data, file = "C:\\Users\\admin\\Desktop\\Studies\\4ème\\Stat\\ProjetStatistique\\your_filename.csv", row.names = FALSE)

#Transformation du dataset en dataframe
data <- as.data.frame(data)

# Gestion des missing values: 

#Nombre de valeurs NA par variable avec la librairie VisDat
library(visdat)
colSums(is.na(data))
vis_miss(data)


#Imputation des valeurs NA numériques:
data <- data %>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
vis_miss(data)
# Suite à cette opération, le pourcentage des données NA a changé de 1.7% à 1.3%

# Imputation des valeurs NA de type char en utilisant le mode (most frequent value):
data <- data %>%
  mutate_if(is.character, function(x) ifelse(is.na(x), names(sort(table(x), decreasing = TRUE)[1]), x))
vis_miss(data)

#Le pourcentage de missing values est maintenant à 0%
View(data)

# Exemple outliers avec la variable 'family_size'
boxplot(data$family_size, main = "Boîte à Moustaches pour l'Âge")

