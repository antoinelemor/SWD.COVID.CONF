# Base path
import_data_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"
export_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Results"

# Chargement des données (ajustez le nom du fichier si nécessaire)
swd_data <- read.csv(file.path(import_data_path, "SWD.frame.analysis.schools.sentences_1.csv"))
qc_data <- read.csv(file.path(import_data_path, "QC.frame.analysis.schools.sentences_1.csv"))

# Chargement des packages nécessaires
library(dplyr)
library(ggplot2)
library(lubridate)





## GRAPHIQUES DANS LE TEMPS ##

# Conversion de la colonne 'date' en type Date
swd_data$date <- as.Date(swd_data$date)
qc_data$date <- as.Date(qc_data$date)

# Analyse descriptive pour la Suède
# Fréquence des cadres
swd_frame_count <- swd_data %>% filter(check == 1) %>% 
  count(frame)

# Tendances temporelles
ggplot(swd_data, aes(x = date, fill = frame)) +
  geom_histogram(binwidth = 30, position = "dodge") +
  labs(title = "Fréquence des cadres au fil du temps en Suède", x = "Date", y = "Nombre d'occurrences")

# Répétez les analyses similaires pour les données du Québec
qc_frame_count <- qc_data %>% filter(check == 1) %>% 
  count(frame)

ggplot(qc_data, aes(x = date, fill = frame)) +
  geom_histogram(binwidth = 30, position = "dodge") +
  labs(title = "Fréquence des cadres au fil du temps au Québec", x = "Date", y = "Nombre d'occurrences")






## GRAPHIQUE AVEC CADRES NEUTRES ##

# Filtrage des données pour inclure uniquement les lignes où check = 1
swd_filtered <- swd_data %>% filter(check == 1)
qc_filtered <- qc_data %>% filter(check == 1)

# Remplacement des NA par 'neutral' dans les cadres
swd_filtered$frame <- replace(swd_filtered$frame, is.na(swd_filtered$frame), 'neutral')
qc_filtered$frame <- replace(qc_filtered$frame, is.na(qc_filtered$frame), 'neutral')

# Effectuer un test de significativité pour chaque cadre
results <- list()
unique_frames <- unique(na.omit(c(swd_filtered$frame, qc_filtered$frame)))

for (frame in unique_frames) {
  swd_count <- sum(swd_filtered$frame == frame)
  qc_count <- sum(qc_filtered$frame == frame)
  swd_total <- nrow(swd_filtered)
  qc_total <- nrow(qc_filtered)
  
  matrix <- matrix(c(swd_count, swd_total - swd_count, qc_count, qc_total - qc_count), nrow = 2)
  test <- fisher.test(matrix)
  
  results[[frame]] <- tidy(test)
}

# Création d'un dataframe pour les résultats
test_results <- do.call(rbind, results)
test_results$frame <- rownames(test_results)

# Préparation des données pour le graphique
swd_frame_proportions <- swd_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

qc_frame_proportions <- qc_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

swd_frame_proportions$region <- 'Sweden'
qc_frame_proportions$region <- 'Quebec'

combined_data <- rbind(swd_frame_proportions, qc_frame_proportions)
combined_data <- left_join(combined_data, test_results, by = "frame")

# Mise à jour du nom du cadre 'neutral'
combined_data$frame <- gsub("neutral", "Neutral", combined_data$frame)

# Création du graphique avec les ajustements
ggplot(combined_data, aes(x = frame, y = proportion, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("p = %.3f", p.value)), 
            position = position_dodge(width = 0.9), 
            vjust = -2, # ajustement vertical pour les valeurs p
            size = 3.7, # taille du texte pour les valeurs p
            fontface = "bold") + # mise en gras du texte
  geom_text(aes(label = sprintf("%.3f", proportion)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, # ajustement vertical pour les proportions
            size = 3.7, # taille du texte pour les proportions
            fontface = "bold") + # mise en gras du texte
  labs(title = "Comparaison des cadres interprétatifs des preuves scientifiques : le cas des écoles",
       x = "Cadre",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set1") +
  ylim(0, max(combined_data$proportion) * 1.04) + # Ajustement des limites de l'axe des y
  guides(fill = guide_legend(title = NULL)) # Enlever 'region' de la légende

ggsave(filename = "schools_restults_with_NA.png", path = export_path, width = 10, height = 8, units = "in")





## GRAPHIQUE SANS LES CADRES NEUTRES ##

# Filtrage des données pour inclure uniquement les lignes où check = 1 et les cadres 'Mitigation' ou 'Suppression'
swd_filtered <- swd_data %>% filter(check == 1, frame %in% c('Mitigation', 'Suppression'))
qc_filtered <- qc_data %>% filter(check == 1, frame %in% c('Mitigation', 'Suppression'))

# Effectuer un test de significativité pour chaque cadre
results <- list()
unique_frames <- unique(na.omit(c(swd_filtered$frame, qc_filtered$frame)))

for (frame in unique_frames) {
  swd_count <- sum(swd_filtered$frame == frame)
  qc_count <- sum(qc_filtered$frame == frame)
  swd_total <- nrow(swd_filtered)
  qc_total <- nrow(qc_filtered)
  
  cat("Frame:", frame, "\n")
  cat("SWD Count:", swd_count, "QC Count:", qc_count, "\n")
  cat("SWD Total:", swd_total, "QC Total:", qc_total, "\n")
  
  matrix <- matrix(c(swd_count, swd_total - swd_count, qc_count, qc_total - qc_count), nrow = 2)
  print(matrix)
  test <- fisher.test(matrix)
  
  results[[frame]] <- tidy(test)
}


# Création d'un dataframe pour les résultats
test_results <- do.call(rbind, results)
test_results$frame <- rownames(test_results)

# Préparation des données pour le graphique
swd_frame_proportions <- swd_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

qc_frame_proportions <- qc_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

swd_frame_proportions$region <- 'Sweden'
qc_frame_proportions$region <- 'Quebec'

combined_data <- rbind(swd_frame_proportions, qc_frame_proportions)
combined_data <- left_join(combined_data, test_results, by = "frame")

# Création du graphique avec les ajustements
ggplot(combined_data, aes(x = frame, y = proportion, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("p = %.3f", p.value)), 
            position = position_dodge(width = 0.9), 
            vjust = -2, # ajustement vertical pour les valeurs p
            size = 3.7, # taille du texte pour les valeurs p
            fontface = "bold") + # mise en gras du texte
  geom_text(aes(label = sprintf("%.3f", proportion)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, # ajustement vertical pour les proportions
            size = 3.7, # taille du texte pour les proportions
            fontface = "bold") + # mise en gras du texte
  labs(title = "Comparaison des cadres interprétatifs des preuves scientifiques : le cas des écoles",
       x = "Cadre",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set1") +
  ylim(0, max(combined_data$proportion) * 1.04) + # Ajustement des limites de l'axe des y
  guides(fill = guide_legend(title = NULL)) # Enlever 'region' de la légende

ggsave(filename = "schools_restults.png", path = export_path, width = 10, height = 8, units = "in")












## GRAPHIQUES EXPLORATOIRES AVEC SÉVÉRITÉ ##

# Chargement des packages nécessaires
library(dplyr)
library(ggplot2)
library(lubridate)

# Chargement des données COVID supplémentaires
covid_data <- read.csv("/Users/antoine/Documents/GitHub.nosync/QC.COVID_Data/QC.COVID_data_daily.csv")

# Assurez-vous que la colonne 'date' est au bon format dans toutes les bases de données
covid_data$date <- as.Date(covid_data$date)
qc_data$date <- as.Date(qc_data$date)

# Filtrage des données pour la période souhaitée (du 25 mars 2020 à mars 2022)
start_date <- as.Date("2020-03-25")
end_date <- as.Date("2022-03-31")

qc_data_filtered <- qc_data %>% 
  filter(date >= start_date & date <= end_date & check == 1)

covid_data_filtered <- covid_data %>% 
  filter(date >= start_date & date <= end_date)

# Création du graphique
ggplot() +
  geom_line(data = covid_data_filtered, aes(x = date, y = SPHM, group = 1), color = "blue") +
  geom_histogram(data = qc_data_filtered, aes(x = date, fill = frame), binwidth = 30, position = "dodge") +
  labs(title = "Fréquence des cadres et SPHM au fil du temps au Québec",
       x = "Date",
       y = "Nombre d'occurrences / Valeur SPHM") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(limits = c(start_date, end_date))








# Filtrage des données pour inclure uniquement les lignes où check = 1
swd_filtered <- swd_data %>% filter(check == 1)
qc_filtered <- qc_data %>% filter(check == 1)

# Remplacement des NA par 'neutral' dans les cadres
swd_filtered$frame <- replace(swd_filtered$frame, is.na(swd_filtered$frame), 'neutral')
qc_filtered$frame <- replace(qc_filtered$frame, is.na(qc_filtered$frame), 'neutral')

# Effectuer un test de significativité pour chaque cadre
results <- list()
unique_frames <- unique(na.omit(c(swd_filtered$frame, qc_filtered$frame)))

for (frame in unique_frames) {
  swd_count <- sum(swd_filtered$frame == frame)
  qc_count <- sum(qc_filtered$frame == frame)
  swd_total <- nrow(swd_filtered)
  qc_total <- nrow(qc_filtered)
  
  matrix <- matrix(c(swd_count, swd_total - swd_count, qc_count, qc_total - qc_count), nrow = 2)
  test <- fisher.test(matrix)
  
  results[[frame]] <- tidy(test)
}

# Création d'un dataframe pour les résultats
test_results <- do.call(rbind, results)
test_results$frame <- rownames(test_results)

# Préparation des données pour le graphique
swd_frame_proportions <- swd_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

qc_frame_proportions <- qc_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

swd_frame_proportions$region <- 'Sweden'
qc_frame_proportions$region <- 'Quebec'

combined_data <- rbind(swd_frame_proportions, qc_frame_proportions)
combined_data <- left_join(combined_data, test_results, by = "frame")

# Création du graphique avec des ajustements
ggplot(combined_data, aes(x = frame, y = proportion, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("p = %.3f", p.value)), 
            position = position_dodge(width = 0.9), 
            vjust = -2, # ajustement vertical pour les valeurs p
            size = 3.7) + # taille du texte pour les valeurs p
  geom_text(aes(label = sprintf("%.3f", proportion)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, # ajustement vertical pour les proportions
            size = 3.7) + # taille du texte pour les proportions
  labs(title = "Comparaison des cadres interpréatifs des preuves scientifiques : le cas des écoles",
       x = "Cadre",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set1") +
  ylim(0, max(combined_data$proportion) * 1.04) # Ajustement des limites de l'axe des y
















# Calcul des proportions pour la Suède avec le filtre
swd_frame_proportions <- swd_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Calcul des proportions pour le Québec avec le filtre
qc_frame_proportions <- qc_filtered %>%
  group_by(frame) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Fusion des données pour la comparaison
swd_frame_proportions$region <- 'Sweden'
qc_frame_proportions$region <- 'Quebec'

combined_data <- rbind(swd_frame_proportions, qc_frame_proportions)

# Création d'un graphique pour la comparaison
ggplot(combined_data, aes(x = frame, y = proportion, fill = region)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparaison des proportions de chaque cadre (check = 1) entre la Suède et le Québec",
       x = "Cadre",
       y = "Proportion") +
  scale_fill_brewer(palette = "Set1")









library(ggplot2)
library(dplyr)

# Assurez-vous que les dates sont au format Date
qc_data$date <- as.Date(qc_data$date)
covid_data$date <- as.Date(covid_data$date)

# Filtrage des données pour la période souhaitée
start_date <- as.Date("2020-03-25")
end_date <- as.Date("2022-03-31")

qc_data_filtered <- qc_data %>% 
  filter(date >= start_date & date <= end_date & check == 1 & !is.na(frame))

covid_data_filtered <- covid_data %>% 
  filter(date >= start_date & date <= end_date)

# Déterminer un facteur de multiplication
# Ce facteur peut être ajusté manuellement pour trouver un équilibre visuel
multiplication_factor <- 15  # Ajustez cette valeur selon les besoins

# Appliquer le facteur de multiplication
qc_data_filtered <- qc_data_filtered %>%
  group_by(date, frame) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(adjusted_count = count * multiplication_factor)

# Création du graphique
ggplot() +
  geom_bar(data = qc_data_filtered, aes(x = date, y = adjusted_count, fill = frame), 
           stat = "identity", position = "dodge") +
  geom_line(data = covid_data_filtered, aes(x = date, y = SPHM), color = "blue") +
  scale_y_continuous("Valeur SPHM / Occurrences de cadres ajustées",
                     sec.axis = sec_axis(~ . / multiplication_factor, name = "Nombre d'occurrences de cadres (réelles)")) +
  labs(title = "Fréquence des cadres et SPHM au fil du temps au Québec",
       x = "Date", y = "Valeur SPHM / Occurrences de cadres ajustées") +
  scale_fill_brewer(palette = "Set1") +
  scale_x_date(limits = c(start_date, end_date))
