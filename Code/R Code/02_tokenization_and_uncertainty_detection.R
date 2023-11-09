# Base path
import_data_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"
export_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"
model_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Code/R Code/Udpipe"

# Import textual database
input_file <- file.path(import_data_path, "SWD.conf_texts.csv")
SWD.conf_texts <- read.csv(input_file, header = TRUE, sep=",")

# Packages
library(udpipe)
library(dplyr)
library(dplyr)
library(cld3)

# Downloading French udpipe model
udpipe_download_model(language = "french", model_dir = model_path)

# Loading model
model_file <- file.path(model_path, "french-gsd-ud-2.5-191206.udpipe")
model <- udpipe_load_model(model_file)

# Tokenization
annotated_data <- udpipe_annotate(model, x = SWD.conf_texts$conf_txt, doc_id = SWD.conf_texts$doc_ID)
SWD.conf_tokenised <- as.data.frame(annotated_data)

# Adding dates to the database
SWD.conf_tokenised$date <- SWD.conf_texts$date[match(SWD.conf_tokenised$doc_id, SWD.conf_texts$doc_ID)]
SWD.conf_tokenised$date <- as.Date(SWD.conf_tokenised$date, format = "%Y-%m-%d")
                                   
# Detection of foreign languages
SWD.conf_tokenised <- SWD.conf_tokenised %>%
  mutate(lang = detect_language(sentence))

# Suppresing english sentences
english_sentences <- SWD.conf_tokenised %>%
  filter(grepl("en", lang)) %>%
  group_by(doc_id, sentence_id) %>%
  slice_head(n = 1)

SWD.conf_tokenised_english <- english_sentences

SWD.conf_tokenised <- SWD.conf_tokenised %>%
  anti_join(english_sentences, by = c("doc_id", "sentence_id"))

# Uncertainty detection
SWD.conf_tokenised_2 <- SWD.conf_tokenised %>%
  group_by(date) %>%
  mutate(uncertainty = ifelse(grepl("Mood=Cnd", feats) | 
                                (lemma == "pouvoir" & upos == "VERB" & !grepl("Form=Inf", feats)) |
                                lemma %in% c("possible", "possiblement", "probable", "probablement", "improbable") |
                                lemma %in% c("incertain", "incertaine", "incertaines", "incertains", "incertitude", "incertitudes", "hypothèse") | # champs de l'incertitude
                                (lemma == "suspect" & upos == "ADJ") | lemma == "suspicion" | # quelque chose de suspect
                                (upos == "VERB" & (lemma == "penser" | lemma == "croire" | lemma == "espérer" | lemma == "questionner" | lemma == "soupçonner")), # verbes d'incertitude
                              1, 0))

# Exporting database
output_file <- file.path(export_path, "SWD.conf_tokenised.csv")
write.csv(SWD.conf_tokenised, file = output_file, row.names = FALSE)