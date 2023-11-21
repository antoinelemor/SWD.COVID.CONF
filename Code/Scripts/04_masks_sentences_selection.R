# Base path
import_data_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"
export_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"

# Chargement des données (ajustez le nom du fichier si nécessaire)
SWD.conf_tokenised <- read.csv(file.path(import_data_path, "SWD.conf_dict.csv"))

library(dplyr)
library(tidyr)


# Créer un dataframe avec des phrases uniques et inclure les colonnes nécessaires
phrases_uniques <- SWD.conf_tokenised %>%
  group_by(doc_id, sentence_id, unique_phrase_id) %>%
  summarise(sentence = first(sentence), 
            masks = first(masks),
            date = first(date), 
            .groups = 'drop') %>%
  arrange(doc_id, sentence_id)


# Étape 2: Filtrer les phrases positives et préparer la fonction d'intervalle

# Filtrer les phrases positives
phrases_positives <- phrases_uniques %>% 
  filter(masks == 1)

# Fonction pour trouver les intervalles
trouver_intervalles <- function(df, id, sentence_id, range = 8) {
  # Trouver les limites de l'intervalle
  start <- max(sentence_id - range, 1)
  end <- sentence_id + range
  
  # Filtrer les phrases dans l'intervalle tout en restant dans le même document
  interval_df <- df %>% 
    filter(doc_id == id, sentence_id >= start, sentence_id <= end)
  
  return(interval_df)
}

# Étape 3: Créer les intervalles et gérer les limites

# Initialiser un dataframe vide pour les résultats
intervalles_df <- data.frame()

# Boucle sur chaque phrase positive
for(i in 1:nrow(phrases_positives)) {
  # Obtenir les informations de la phrase actuelle
  current_id <- phrases_positives$doc_id[i]
  current_sentence_id <- phrases_positives$sentence_id[i]
  
  # Utiliser la fonction pour obtenir l'intervalle
  interval_df <- trouver_intervalles(phrases_uniques, current_id, current_sentence_id)
  
  # Ajouter l'intervalle au dataframe des résultats
  intervalles_df <- rbind(intervalles_df, interval_df)
}

# Supprimer les doublons éventuels
intervalles_df <- intervalles_df %>% distinct()

# Étape 4: Concaténer les phrases et créer le dataframe final (Modifié)

# Fonction pour traiter chaque groupe de doc_id
traiter_groupe <- function(df) {
  # Trier par sentence_id
  df <- df %>% arrange(sentence_id)
  
  # Trouver les groupes d'intervalles
  df$group <- cumsum(c(1, diff(df$sentence_id) > 1))
  
  # Concaténer les phrases dans chaque groupe d'intervalle
  df <- df %>%
    group_by(group) %>%
    summarise(doc_id = first(doc_id),
              unique_phrase_id = first(unique_phrase_id),
              sentences = paste(sentence, collapse = " "),
              date = first(date)) %>%
    ungroup()
  
  return(df)
}

# Appliquer la fonction sur chaque groupe de doc_id
SWD.masks.sentences <- intervalles_df %>%
  group_by(doc_id) %>%
  do(traiter_groupe(.)) %>%
  ungroup()

# Exportation en CSV
write.csv(SWD.masks.sentences, file.path(export_path, "SWD.masks.sentences.csv"), row.names = FALSE)

# Sélectionner les 5 premières lignes
SWD.masks.sentences.sample <- head(SWD.masks.sentences, 25)

# Exporter les 5 premières lignes en CSV
write.csv(SWD.masks.sentences.sample, file.path(export_path, "SWD.masks.sentences.sample.csv"), row.names = FALSE)
