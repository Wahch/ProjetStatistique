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

# Partie Data Import et Exploration

# Importation des données
data <- read.csv(file = file.choose(), header = TRUE, sep = ';', na.strings = c("", "NA"))
print(data)

# Compréhension des données
View(data)
dim(data)
summary(data)

# Étudier les variables disponibles
glimpse(data)

# Vérifier le type des variables
str(data)

# Exemple des valeurs uniques de chaque colonne
unique(data$study_time)
unique(data$age)
unique(data$parent_status)
unique(data$travel_time)
unique(data$mother_education)

# Remplacer les colonnes par leurs vraies valeurs
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

# Éliminer la variable X qui est supplémentaire
data <- data[, -which(names(data) == "X")]

# Remplacer les valeurs manquantes par NA
data <- data %>%
  mutate_all(~ ifelse(str_replace_all(as.character(.), "\\s+", "") == "", NA, .))

# Visualiser les données après cette opération
View(data)

# Gestion des valeurs aberrantes pour certaines variables

# Age
Q1 = 16
Q3 = 18
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
valeurs_aberrantes_inf <- which(data$age < Vmin)
for (variable in valeurs_aberrantes_inf) {
  data$age[variable] = NA
}
valeurs_aberrantes_sup <- which(data$age > Vmax)
for (variable in valeurs_aberrantes_sup) {
  data$age[variable] = NA
}

# Free time
Q1 = 3
Q3 = 4
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
valeurs_aberrantes_inf <- which(data$free_time < Vmin)
for (variable in valeurs_aberrantes_inf) {
  data$free_time[variable] = NA
}
valeurs_aberrantes_sup <- which(data$free_time > Vmax)
for (variable in valeurs_aberrantes_sup) {
  data$free_time[variable] = NA
}

# Health
Q1 = 3
Q3 = 5
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
valeurs_aberrantes_inf <- which(data$health < Vmin)
for (variable in valeurs_aberrantes_inf) {
  data$health[variable] = NA
}
valeurs_aberrantes_sup <- which(data$health > Vmax)
for (variable in valeurs_aberrantes_sup) {
  data$health[variable] = NA
}

# Absences
Q1 = 0
Q3 = 8
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
valeurs_aberrantes_inf <- which(data$absences < Vmin)
for (variable in valeurs_aberrantes_inf) {
  data$absences[variable] = NA
}
valeurs_aberrantes_sup <- which(data$absences > Vmax)
for (variable in valeurs_aberrantes_sup) {
  data$absences[variable] = NA
}

# Final Grade
Q1 = 8
Q3 = 14
Vmax = Q3 + 1.5 * (Q3 - Q1)
Vmin = Q1 - 1.5 * (Q3 - Q1)
valeurs_aberrantes_inf <- which(data$final_grade < Vmin)
for (variable in valeurs_aberrantes_inf) {
  data$final_grade[variable] = NA
}
valeurs_aberrantes_sup <- which(data$final_grade > Vmax)
for (variable in valeurs_aberrantes_sup) {
  data$final_grade[variable] = NA
}

# Exportation de ce dataset organisé
#write.csv(data, file = "C:\\Users\\admin\\Desktop\\Studies\\4ème\\Stat\\ProjetStatistique\\your_filename.csv", row.names = FALSE)

# Transformation du dataset en dataframe
data <- as.data.frame(data)
View(data)
str(data)

# Arrondir les valeurs entières
numeric_cols <- sapply(data, is.numeric)
data[, numeric_cols] <- lapply(data[, numeric_cols], round)
View(data)

# Gestion des missing values:

# Nombre de valeurs NA par variable avec la librairie VisDat
colSums(is.na(data))
vis_miss(data)

# Imputation des valeurs NA numériques:
data <- data %>%
  mutate_if(is.numeric, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
vis_miss(data)
# Suite à cette opération, le pourcentage des données NA a changé de 1.7% à 1.3%

# Imputation des valeurs NA de type char en utilisant le mode (most frequent value):
data <- data %>%
  mutate_if(is.character, function(x) ifelse(is.na(x), names(sort(table(x), decreasing = TRUE)[1]), x))
vis_miss(data)
# Le pourcentage de missing values est maintenant à 0%

View(data)
colSums(is.na(data))

# Partie de l'analyse uni-variée:

# Les variables quantitatives
# Représentations graphiques:
hist(data$age, main = "Histogramme de l'âge", xlab = "Âge", col = "lightblue")
hist(data$class_failures, main = "Histogramme de redoublement", xlab = "Nombre d'échecs", col = "lightblue")
hist(data$free_time, main = "Histogramme de temps libre", xlab = "Heures Temps Libre", col = "lightblue")
hist(data$health, main = "Histogramme de sante", xlab = "Etat de santé", col = "lightblue")
hist(data$absences, main = "Histogramme des absences", xlab = "Nombre d'absences", col = "lightblue")
hist(data$final_grade,main="Histogramme de la note final")

# Visualisation des relations entre les variables
pairs(data[, c("age", "free_time", "health", "absences", "final_grade")])

# Test de normalité
shapiro.test(data$age)
shapiro.test(data$absences)
shapiro.test(data$health)
shapiro.test(data$free_time)
shapiro.test(data$class_failures)
#shapiro.test(data$X)
# OU la méthode:
qqnorm(data$age)
qqline(data$age)

# Variables qualitatives
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

# Analyse bivariée
# Quanti-Quanti
plot(data$final_grade, data$free_time, main = "Nuage de points - Corrélation", xlab = "", ylab = "", col = "blue", pch = 16)
cor.test(data$final_grade, data$free_time)  # faible corrélation
plot(data$final_grade, data$absences, main = "Nuage de points - Corrélation", xlab = "", ylab = "", col = "blue", pch = 16)
cor.test(data$final_grade, data$absences)  # faible corrélation
plot(data$final_grade, data$class_failures, main = "Nuage de points - Corrélation", xlab = "Final Grade", ylab = "Nombre de redoublements", col = "blue", pch = 16)
cor.test(data$final_grade, data$class_failures)  # Il y a une significativité et une corrélation négative
cor.test(data$final_grade, data$health)  # Très faible corrélation
plot(data$final_grade, data$health, main = "Nuage de points - Corrélation", xlab = "", ylab = "", col = "blue", pch = 16)

# Quanti-quali
resultats_chi2 <- chisq.test(data$study_time, data$final_grade)
print(resultats_chi2)
ggplot(data = data, aes(x = data$final_grade, y = data$mother_education)) +
  geom_boxplot() +
  labs(title = "Graphique de corrélation entre variables quantitatives et qualitatives",
       x = "Variable qualitative",
       y = "Variable quantitative")
resultats_chi3 <- chisq.test(data$mother_education, data$final_grade)
print(resultats_chi3)
resultats_chi4 <- chisq.test(data$school, data$final_grade)
print(resultats_chi4)
resultats <- chisq.test(data$school_support, data$final_grade)
print(resultats)  # Il y a une relation significative

# Analyse statistique interactive avec Shiny
# Définition du UI
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

# Définition du serveur
server <- function(input, output) {
  # Créer un graphique réactif
  output$scatter_plot <- renderPlot({
    # Vérifier si les variables sélectionnées ont des valeurs non manquantes et finies
    if (!any(is.na(data[[input$variable1]])) && all(is.finite(data[[input$variable1]])) &&
        !any(is.na(data[[input$variable2]])) && all(is.finite(data[[input$variable2]]))) {
      plot(data[[input$variable1]], data[[input$variable2]],
           xlab = input$variable1, ylab = input$variable2,
           main = paste("Scatter Plot of", input$variable1, "vs.", input$variable2))
    } else {
      # Imprimer un message s'il y a des problèmes avec les données
      cat("Selected variables have missing or infinite values.")
    }
  })
  
  # Afficher les informations de corrélation
  output$correlation_info <- renderText({
    # Vérifier si les variables sélectionnées ont des valeurs non manquantes et finies
    if (!any(is.na(data[[input$variable1]])) && all(is.finite(data[[input$variable1]])) &&
        !any(is.na(data[[input$variable2]])) && all(is.finite(data[[input$variable2]]))) {
      correlation <- cor(data[[input$variable1]], data[[input$variable2]])
      paste("Correlation:", round(correlation, 3))
    } else {
      # Retourner une chaîne vide s'il y a des problèmes avec les données
      ""
    }
  })
}

# Exécuter l'application Shiny
shinyApp(ui, server)

# Modélisations statistiques
# Classification Ascendante Hiearchique(CAH)

# Transformation en variables indicatrices
variables_qualitatives <- model.matrix(~ . - 1, data = data)
# Sélection des variables quantitatives
variables_quantitatives <- data[, c("age", "student_id", "class_failures", "free_time", "health", "absences", "final_grade")]
# Normalisation
variables_quantitatives_normalisees <- scale(variables_quantitatives)
# Fusion des données
donnees_finales <- cbind(variables_qualitatives, variables_quantitatives_normalisees)
View(donnees_finales)
# Fonction pour calculer la somme totale des carrés intra-cluster (WCSS)
calculate_wcss <- function(dist_matrix, num_clusters) {
  hca <- hclust(dist_matrix, method = "ward.D2")
  clusters <- cutree(hca, k = num_clusters)
  kmeans_obj <- kmeans(dist_matrix, centers = num_clusters, nstart = 25)  # Utilisation de kmeans pour calculer WCSS
  wcss <- sum(kmeans_obj$withinss)
  return(wcss)
}
# Fonction pour tracer la méthode du coude
plot_elbow_method <- function(dist_matrix, max_clusters = 10) {
  wcss_values <- sapply(1:max_clusters, function(k) calculate_wcss(dist_matrix, k))
  plot(1:max_clusters, wcss_values, type = "b", pch = 19, frame = FALSE, col = "blue",
       xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares (WCSS)",
       main = "Elbow Method for Optimal Number of Clusters")
  
  # Ajouter une ligne verticale au point du coude
  elbow_point <- kneeLocator(1:max_clusters, wcss_values, curve = "convex", direction = "decreasing")$knee
  abline(v = elbow_point, col = "red", lty = 2)
  text(elbow_point, max(wcss_values), paste("Optimal Clusters =", elbow_point), pos = 4, col = "red")
}
# Calcul de la Matrice de Dissimilarité
matrice_dissimilarite <- daisy(donnees_finales)
print(matrice_dissimilarite)

# Tracer la méthode du coude
plot_elbow_method(matrice_dissimilarite, max_clusters = 10)

# Exécuter la CAH
resultat_cah <- hclust(matrice_dissimilarite, method = "ward.D2")
# Représentation graphique du dendrogramme
plot(resultat_cah, hang = -1, cex = 0.8, main = "Dendrogramme de la CAH")

# Identification des facteurs
# Modèle de Régression Linéaire
modele <- lm(final_grade ~ age + family_size + mother_education + travel_time + study_time + class_failures + school_support + family_support + extra_paid_classes + higher_ed + free_time + health + absences, data = data)
summary(modele)
## On va éliminer les variables le moins significatives qui ont le p-value le plus élevé 
## On élimine la variable mother_education car elle a la valeur p*=0.93 qui est la plus importante
modele1 <- lm(final_grade ~ school + sex + age + family_size + parent_status + 
                travel_time + study_time + class_failures + 
                school_support + family_support + extra_paid_classes + higher_ed + 
                free_time + health + absences,
              data = data)
summary(modele1)

## On élimine la variable free_time car elle a la valeur p*=0.74 qui est la plus importante
modele2 <- lm(final_grade ~ school + sex + age + family_size + parent_status + 
                travel_time + study_time + class_failures + 
                school_support + family_support + extra_paid_classes + higher_ed + health + absences,
              data = data)

# Résumé du modèle de régression linéaire
summary(modele2)

AIC(modele) 
AIC(modele1) 
AIC(modele2) 
# On va utiliser AIC pour choisir entre les trois. Le critère AIC mesure la qualité du modèle en tenant compte du nombre de paramètres à estimer et les résidus du modèle. Le meilleur modèle est le modèle avec le critère AIC le plus faible.
# Le modèle est le meilleur car AIC(modele) < AIC(modele1) < AIC(modele2)
R <- residuals(modele)
plot(x = fitted(modele), y = R, main = "Graphique des résidus", xlab = "Valeurs ajustées", ylab = "Résidus")
abline(h = 0, col = "red", lty = 2)  # Ajoute une ligne horizontale à y = 0 pour référence

# Exemple de modèle de forêt aléatoire
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

# Conclusion
cat("\n\nCONCLUSION :\n")
cat("L'analyse exploratoire des données et les modélisations statistiques ont fourni des insights intéressants sur la relation entre différentes variables et la note finale des étudiants.\n")
cat("La corrélation entre certaines variables quantitatives et la note finale a été examinée, et des modèles de régression linéaire ont été ajustés pour prédire la note finale en fonction des caractéristiques des étudiants.\n")
cat("De plus, une Classification Ascendante Hiérarchique (CAH) a été réalisée pour explorer la structure des données.\n")
cat("Enfin, un modèle de forêt aléatoire a été utilisé pour prédire la note finale sur un ensemble de test.\n")       