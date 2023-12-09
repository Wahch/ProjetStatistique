#Installation des packages
install.packages("ggplot2")
install.packages("shiny")
install.packages("cluster")
install.packages("randomForest")

# Importation des bibliothèques
library(dplyr)
library(stringr)
library(visdat)
library(cluster)
library(randomForest)
library(shiny)
library(ggplot2)

#Partie Data Import et Exploration

#Importation des données
data <- read.csv(file = file.choose(), header = TRUE, sep = ';', na.strings = c("", "NA"))
print(data)

#Compréhension des données
View(data)
dim(data)
summary(data)

#Etudier les variables disponibles
glimpse(data)

# Vérifier le type des variables
str(data)

#Exemple des valeurs uniques de chaque colonne
unique(data$study_time)
unique(data$age)
unique(data$parent_status)
unique(data$travel_time)
unique(data$mother_education)


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
data$final_grade <- data$X


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

# valeurs abberantes
Outliers_replacement <- function(variable_to_check, Q1, Q3) {
  Vmax = Q3+1.5*(Q3-Q1)
  Vmin = Q1-1.5*(Q3-Q1)
  outliers_inf<-which(data$variable_to_check<Vmin)
  outliers_sup<-which(data$variable_to_check>Vmax)
  for (variable in outliers_inf) { 
    data$variable_to_check[variable] <- NA 
  }
  for (variable in outliers_sup) { 
    data$variable_to_check[variable] <- NA 
  }
}

#Age 
names(data)
boxplot(data$age,main = "Age")
summary(data$age)
Q1 = 16
Q3 = 18
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(data$age<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  data$age[variable] = NA 
}
valeurs_aberrantes_sup<-which(data$age>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  data$age[variable] = NA 
}
boxplot(data$age,main = "Age")

#free_time
names(data)
boxplot(data$free_time,main = "Free time")
summary(data$free_time)
Q1 = 3
Q3 = 4
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(data$free_time<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  data$free_time[variable] = NA 
}
valeurs_aberrantes_sup<-which(data$free_time>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  data$free_time[variable] = NA 
}
boxplot(data$free_time,main = "Free time")

#Health
names(data)
boxplot(data$health,main = "health")
summary(data$health)
Q1 = 3
Q3 = 5
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(data$free_time<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  data$free_time[variable] = NA 
}
valeurs_aberrantes_sup<-which(data$free_time>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  data$free_time[variable] = NA 
}
boxplot(data$free_time,main = "Free time")

#absences
names(data)
boxplot(data$absences,main = "Absences")
summary(data$absences)
Q1 = 0
Q3 = 8
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(data$absences<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  data$absences[variable] = NA 
}
valeurs_aberrantes_sup<-which(data$absences>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  data$absences[variable] = NA 
}
boxplot(data$absences,main = "Absences")

#final grade
names(data)
boxplot(data$final_grade,main = "Final Grade")
summary(data$final_grade)
Q1 = 8
Q3 = 14
Vmax = Q3+1.5*(Q3-Q1)
Vmin = Q1-1.5*(Q3-Q1)
valeurs_aberrantes_inf<-which(data$final_grade<Vmin)
valeurs_aberrantes_inf
for (variable in valeurs_aberrantes_inf) { 
  data$final_grade[variable] = NA 
}
valeurs_aberrantes_sup<-which(data$final_grade>Vmax)
valeurs_aberrantes_sup
for (variable in valeurs_aberrantes_sup) { 
  data$final_grade[variable] = NA 
}
boxplot(data$final_grade,main = "Final Grade")

#class_failure
names(data)
boxplot(data$class_failures,main = "Class Failures")
summary(data$class_failures)
# on change rien car la colonne continent que 0
boxplot(data$class_failures,main = "Class Failures")
sum(is.na(data)) 
T = (sum(is.na(data))/prod(dim(data)))*100
T
View(data)#2% de la dataset est manquante


#Exportation de ce dataset organisé
#write.csv(data, file = "C:\\Users\\admin\\Desktop\\Studies\\4ème\\Stat\\ProjetStatistique\\your_filename.csv", row.names = FALSE)

#Transformation du dataset en dataframe
data <- as.data.frame(data)
View(data)
str(data)

# Arrondir les valeurs entières
numeric_cols <- sapply(data, is.numeric)
data[, numeric_cols] <- lapply(data[, numeric_cols], round)
View(data)

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
colSums(is.na(data))


###Partie de l'analyse uni-variée:

#Les variables quantitatives
#Représentations graphiques:
hist(data$age, main = "Histogramme de l'âge", xlab = "Âge", col = "lightblue")
hist(data$class_failures, main = "Histogramme de redoublement", xlab = "redoublement", col = "lightblue")
hist(data$free_time, main = "Histogramme de temps libre", xlab = "", col = "lightblue")
hist(data$health, main = "Histogramme de sante", xlab = "", col = "lightblue")
hist(data$absences, main = "Histogramme des absences", xlab = "", col = "lightblue")
hist(data$final_grade,main="note final")

#Visualisation des relations entre les variables
pairs(data[, c("age", "free_time", "health", "absences", "final_grade")])

#Test de normalité:
shapiro.test(data$age)
shapiro.test(data$absences)
shapiro.test(data$health)
shapiro.test(data$free_time)
shapiro.test(data$class_failures)
#shapiro.test(data$X)
# OU la méthode:
qqnorm(data$age)
qqline(data$age)

######Les variables qualitatives
table_sexe <- table(data$sex)
barplot(table_sexe, main = "Distribution des sexes", xlab = "Sexe", ylab = "Fréquence")
table_famille <- table(data$family_size)
barplot(table_famille, main = "Taille de la famille", xlab = "Taille", ylab = "Fréquence")
table_school <- table(data$school)
barplot(table_school, main = "Distribution des lycées", xlab = "ecole", ylab = "Fréquence")
table_mothereducation <- table(data$mother_education)
barplot(table_mothereducation, main = "Niveau d'éducation de la mère", xlab = "Education Mére", ylab = "Fréquence")
table_statut <- table(data$parent_status)
barplot(table_statut, main = "", xlab = "status", ylab = "Fréquence")
table_travel <- table(data$travel_time)
barplot(table_travel, main = "", xlab = "Heures de transport", ylab = "Fréquence")
study_time <- table(data$study_time)
barplot(study_time, main = "", xlab = "Heures dédiés aux études", ylab = "Fréquence")
table_support <- table(data$school_support)
barplot(table_support, main = "", xlab = "Soutien académique", ylab = "Fréquence")
table_supprt_family <- table(data$family_support)
barplot(table_supprt_family, main = "", xlab = "Soutien familiale", ylab = "Fréquence")

########analyse bivarié
### quanti __quanti
plot(data$final_grade,data$free_time, main = "Nuage de points - Corrélation", xlab = "", ylab = "", col = "blue", pch = 16)
cor.test(data$final_grade,data$free_time)# faible corrélation
plot(data$final_grade,data$absences, main = "Nuage de points - Corrélation", xlab = "", ylab = "", col = "blue", pch = 16)
cor.test(data$final_grade,data$absences)#faible corrélation
plot(data$final_grade,data$class_failures, main = "Nuage de points - Corrélation", xlab = "Final Grade", ylab = "Nombre de redoublements", col = "blue", pch = 16)
cor.test(data$final_grade,data$class_failures) #ily'a une significativité et une corrélation négative 
cor.test(data$final_grade,data$health)# trés faible corrélation
plot(data$final_grade,data$health, main = "Nuage de points - Corrélation", xlab = "", ylab = "", col = "blue", pch = 16)

install.packages("ggplot2")
library(ggplot2)

### quali__quanti
resultats_chi2 <- chisq.test(data$study_time, data$final_grade)
print(resultats_chi2)
ggplot(data = data, aes(x = data$final_grade, y = data$mother_education)) +
  geom_boxplot() +
  labs(title = "Graphique de corrélation entre variables quantitatives et qualitatives",
       x = "Variable qualitative",
       y = "Variable quantitative")
resultats_chi3<- chisq.test(data$mother_education, data$final_grade)
print(resultats_chi3)
resultats_chi4<-chisq.test(data$school,data$final_grade)
print(resultats_chi4)
resultats<-chisq.test(data$school_support,data$final_grade)
print(resultats)# ily'a une relation significative 

# Installer le Shiny library
install.packages("shiny")
library(shiny)

# Definition du UI
ui <- fluidPage(
  titlePanel("Analyse Statistique Interactive"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable1", "Selectionner Variable 1:", choices = colnames(data), selected = "age"),
      selectInput("variable2", "Selectionner Variable 2:", choices = colnames(data), selected = "final_grade")
    ),
    
    mainPanel(
      plotOutput("scatter_plot"),
      textOutput("correlation_info")  # Updated textOutput for correlation information
    )
  )
)

# Definition du serveur
server <- function(input, output) {
  # Create a reactive plot
  output$scatter_plot <- renderPlot({
    # Check if selected variables have non-missing and finite values
    if (!any(is.na(data[[input$variable1]])) && all(is.finite(data[[input$variable1]])) &&
        !any(is.na(data[[input$variable2]])) && all(is.finite(data[[input$variable2]]))) {
      plot(data[[input$variable1]], data[[input$variable2]],
           xlab = input$variable1, ylab = input$variable2,
           main = paste("Scatter Plot of", input$variable1, "vs.", input$variable2))
    } else {
      # Print a message if there are issues with the data
      cat("Selected variables have missing or infinite values.")
    }
  })
  
  # Display correlation information
  output$correlation_info <- renderText({
    # Check if selected variables have non-missing and finite values
    if (!any(is.na(data[[input$variable1]])) && all(is.finite(data[[input$variable1]])) &&
        !any(is.na(data[[input$variable2]])) && all(is.finite(data[[input$variable2]]))) {
      correlation <- cor(data[[input$variable1]], data[[input$variable2]])
      paste("Correlation:", round(correlation, 3))
    } else {
      # Return an empty string if there are issues with the data
      ""
    }
  })
}

# Executer la Shiny app
shinyApp(ui, server)

#####Modelisations statistiques
 #Classification Ascendante Hiearchique(CAH)

# Transformation en variables indicatrices
variables_qualitatives <- model.matrix(~ . - 1, data = data)
# Selection des variables quantitatives
variables_quantitatives <-data[, c("age", "student_id", "class_failures", "free_time", "health", "absences", "final_grade")]
# Normalisation
variables_quantitatives_normalisees <- scale(variables_quantitatives)
# Fusion des donnees
donnees_finales<-cbind(variables_qualitatives, variables_quantitatives_normalisees)
View(donnees_finales)
# Function to calculate total within-cluster sum of squares (WCSS)
calculate_wcss <- function(dist_matrix, num_clusters) {
  hca <- hclust(dist_matrix, method = "ward.D2")
  clusters <- cutree(hca, k = num_clusters)
  kmeans_obj <- kmeans(dist_matrix, centers = num_clusters, nstart = 25)  # Using kmeans to calculate WCSS
  wcss <- sum(kmeans_obj$withinss)
  return(wcss)
}
# Function to plot the elbow method
plot_elbow_method <- function(dist_matrix, max_clusters = 10) {
  wcss_values <- sapply(1:max_clusters, function(k) calculate_wcss(dist_matrix, k))
  plot(1:max_clusters, wcss_values, type = "b", pch = 19, frame = FALSE, col = "blue",
       xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares (WCSS)",
       main = "Elbow Method for Optimal Number of Clusters")
  
  # Add a vertical line at the elbow point
  elbow_point <- kneeLocator(1:max_clusters, wcss_values, curve="convex", direction="decreasing")$knee
  abline(v = elbow_point, col = "red", lty = 2)
  text(elbow_point, max(wcss_values), paste("Optimal Clusters =", elbow_point), pos = 4, col = "red")
}
#Calcul de la Matrice de Dissimilarite
library(cluster)
matrice_dissimilarite <- daisy(donnees_finales)
print(matrice_dissimilarite)

# Plot the elbow method
plot_elbow_method(matrice_dissimilarite, max_clusters = 10)

#executer la cah
resultat_cah <- hclust(matrice_dissimilarite, method = "ward.D2")
#representation graphe 
plot(resultat_cah, hang = -1, cex = 0.8, main = "Dendrogramme de la CAH")



# Identification des facteurs 
#modele de Regression Lin?aire 
modele <- lm(final_grade ~ age + family_size + mother_education + travel_time + study_time + class_failures + school_support + family_support + extra_paid_classes + higher_ed + free_time + health + absences, data = data)
summary(modele)
## On va eliminer les variables le moins significative qui ont le p_value le plus elev? 
## Alors on elimine la variable mother_education puisque elle a la valeur p*=0.93 qui est le plus important

modele1 <- lm(final_grade ~ school + sex + age + family_size + parent_status + 
                travel_time + study_time + class_failures + 
            school_support + family_support + extra_paid_classes + higher_ed + 
            free_time + health + absences,
          data = data)
summary(modele1)

## Alors on elimine la variable free_time puisque elle a la valeur p*=0.74 qui est le plus important
modele2 <- lm(final_grade ~ school + sex + age + family_size + parent_status + 
                travel_time + study_time + class_failures + 
            school_support + family_support + extra_paid_classes + higher_ed + health + absences,
          data = data)

# Summary of the linear regression model
summary(modele2)

AIC(modele) 
AIC(modele1) 
AIC(modele2) 
# On va utiliser AIC pour choisir entre les trois, Le critere AIC mesure la qualite du modele en tenant compte du nombre de parametre a estimer et les residus du modele. Le meilleur modele est le modele avec le critere AIC le plus faibel.
#modele est le meilleur modele car AIC(modele) < AIC(modele1) < AIC(modele2)
R= residuals(modele)
plot(x = fitted(modele), y = R, main = "Graphique des résidus", xlab = "Valeurs ajustées", ylab = "Résidus")
abline(h = 0, col = "red", lty = 2)  # Ajoute une ligne horizontale à y = 0 pour référence

# Exemple de modele de foret aleatoire
library(randomForest)

# Diviser l'ensemble de données en ensembles d'entraînement et de test (par exemple, 80% d'entraînement et 20% de test)
set.seed(123)  # Pour la reproductibilité
indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[indices, ]
test_data <- data[-indices, ]

# Création du modèle Random Forest sur l'ensemble d'entraînement
modele_foret <- randomForest(final_grade ~ age + family_size + mother_education + travel_time + study_time + class_failures + school_support + family_support + extra_paid_classes + higher_ed + free_time + health + absences, data = train_data)

# Prédiction sur l'ensemble de test
predictions <- predict(modele_foret, newdata = test_data)

# Afficher l'importance des variables
var_imp <- importance(modele_foret)
barplot(var_imp[, "IncNodePurity"], main = "Variable Importance", col = "skyblue", las = 2)

