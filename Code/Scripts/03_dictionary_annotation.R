# Base path
import_data_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"
export_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"
dictionary_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Dictionnaries"

# Importing the dictionary
dictionary_file <- file.path(dictionary_path, "COVID.Frame.dict.csv")
dictionnary <- read.csv2(dictionary_file, sep=";")

# Importing the database
input_file <- file.path(import_data_path, "SWD.conf_tokenised.csv")
SWD.conf_tokenised <- read.csv(input_file, header = TRUE, sep=",")

# Creating "phrase_id" qcombining "sentence_id" and "doc_id"
SWD.conf_tokenised$unique_phrase_id <- paste(SWD.conf_tokenised$doc_id, SWD.conf_tokenised$sentence_id, sep="_")

# Search function for racine (search for the racine of the word) mode and full mode (search for the full word)
findWords <- function(words, token, mode) {
  if (mode == "racine") {
    return(any(sapply(words, function(x) if(nchar(x)>0) any(grepl(paste0("^", x), token, ignore.case = TRUE)) else FALSE)))
  } else if (mode == "complet") {
    return(any(sapply(words, function(x) if(nchar(x)>0) any(grepl(paste0("\\b", x, "\\b"), token, ignore.case = TRUE)) else FALSE)))
  } else {
    return(FALSE)
  }
}




# Search function for the "lemmes" mode
findLemmes <- function(lemmes, SWD.conf_tokenised, i) {
  search_words <- unlist(strsplit(lemmes, " "))

  if (any(grepl(paste0("^", search_words[1], "$"), SWD.conf_tokenised[i, "lemma"], ignore.case = TRUE))) {
    if (length(search_words) == 1) {
      return(TRUE)
    } else {
      next_i <- i + 1
      search_index <- 2
      while (next_i <= nrow(SWD.conf_tokenised) && SWD.conf_tokenised[next_i, "unique_phrase_id"] == SWD.conf_tokenised[i, "unique_phrase_id"]) {
        if (any(grepl(paste0("^", search_words[search_index], "$"), SWD.conf_tokenised[next_i, "lemma"], ignore.case = TRUE))) {
          if (search_index == length(search_words)) {
            return(TRUE)
          } else {
            search_index <- search_index + 1
          }
        }
        next_i <- next_i + 1
      }
    }
  }
  
  
  return(FALSE)
}



# Search function for token mode
findTokens <- function(tokens, SWD.conf_tokenised, i) {
  search_words <- unlist(strsplit(tokens, " "))
  
  next_i <- i + 1
  search_index <- 2
  
  if (any(grepl(paste0("^", search_words[1], "$"), SWD.conf_tokenised[i, "token"], ignore.case = TRUE))) {
    if (length(search_words) == 1) {
      return(TRUE)
    } else {
      while (next_i <= nrow(SWD.conf_tokenised) && SWD.conf_tokenised[next_i, "unique_phrase_id"] == SWD.conf_tokenised[i, "unique_phrase_id"]) {
        if (any(grepl(paste0("^", search_words[search_index], "$"), SWD.conf_tokenised[next_i, "token"], ignore.case = TRUE))) {
          search_index <- search_index + 1
          if (search_index > length(search_words)) {
            return(TRUE)
          }
        } else {
          break
        }
        next_i <- next_i + 1
      }
    }
  }
  
  return(FALSE)
}



# Apply the search functions

cols_to_check <- colnames(dictionnary)[!grepl("mode", colnames(dictionnary))]

for (i in seq_len(nrow(SWD.conf_tokenised))) {
  token <- SWD.conf_tokenised[i, "token"]
  lemma <- SWD.conf_tokenised[i, "lemma"]
  sentence_id <- SWD.conf_tokenised[i, "unique_phrase_id"]
  
  # verify each word for each category
  for (col in cols_to_check) {
    words_to_check <- dictionnary[, col]
    mode_to_check <- dictionnary[, "mode"]
    
    if (length(words_to_check) > 0) {
      word_found <- FALSE
      
      # veify if a token coressponds
      for (j in seq_along(words_to_check)) {
        word_to_check <- words_to_check[j]
        mode <- mode_to_check[j]
        if (!is.na(word_to_check) && nchar(word_to_check) > 0) {
          if (mode == "lemmes") {
            # search for lemmes
            if (findLemmes(word_to_check, SWD.conf_tokenised, i)) {
              word_found <- TRUE
              break
            }
          } else if (mode == "token") {
            # search for tokens
            if (findTokens(word_to_check, SWD.conf_tokenised, i)) {
              word_found <- TRUE
              break
            }
          } else {
            # search for whole words
            if (findWords(word_to_check, token, mode)) {
              word_found <- TRUE
              break
            }
          }
        }
      }
      
      # Write 1 if postivie
      if (word_found) {
        SWD.conf_tokenised[i, col] <- 1
        
    
        same_phrase <- SWD.conf_tokenised$unique_phrase_id == SWD.conf_tokenised[i, "unique_phrase_id"]
        SWD.conf_tokenised[same_phrase, col] <- 1
      }
    }
  }
}



# Exporting database
output_file <- file.path(export_path, "SWD.conf_dict.csv")
write.csv(SWD.conf_tokenised, file = output_file, row.names = FALSE)