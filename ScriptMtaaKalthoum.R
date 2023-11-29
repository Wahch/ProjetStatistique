#Partie Data Import and Exploration

#Importation des données
data <- read.csv(file = file.choose() , header = TRUE , sep = ';')
print(data)

#Compréhension des données
view(data)
dim(data)
str(data)
summary(data)

#Etudier les variables disponibles
glimpse(data)

#Exemple des valeurs uniques
unique(data$sex)
unique(data$age)
unique(data$parent_status)
unique(data$travel_time)
unique(data$mother_education)

view(data)
#installer le package tidyverse
library(tidyverse)

Data_Students <- data

#Partie Data Cleaning:

#Remplacer les colonnes par leurs vraies valeurs
Data_Students$school <- Data_Students$sex
Data_Students$sex <- Data_Students$age
Data_Students$age <- Data_Students$family_size
Data_Students$family_size <- Data_Students$parent_status
Data_Students$parent_status <- Data_Students$mother_education
Data_Students$mother_education <- Data_Students$travel_time
Data_Students$travel_time <- Data_Students$study_time
Data_Students$study_time <- Data_Students$class_failures
Data_Students$class_failures <- Data_Students$school_support
Data_Students$school_support <- Data_Students$family_support
Data_Students$family_support <- Data_Students$extra_paid_classes
Data_Students$extra_paid_classes <- Data_Students$higher_ed
Data_Students$higher_ed<- Data_Students$free_time
Data_Students$free_time <- Data_Students$health
Data_Students$health <- Data_Students$absences
Data_Students$absences <- Data_Students$final_grade
Data_Students$final_grade <- Data_Students$x


#Eliminer la variable X qui est supplémentaire
Data_Students <- Data_Students[, -which(names(Data_Students) == "X")]

view(Data_Students)

#Exportation de ce dataset organisé
#write.csv(Data_Students, file = "C:\\Users\\admin\\Desktop\\Studies\\4ème\\Stat\\ProjetStatistique\\your_filename.csv", row.names = FALSE)

# Gestion des missing values: 
#Nombre de valeurs manquantes par variable avec la librairie VisDat
library(visdat)
colSums(is.na(Data_Students))
vis_miss(Data_Students)

# Exemple outliers avec la variable 'family_size'
boxplot(data$family_size, main = "Boîte à Moustaches pour l'Âge")

