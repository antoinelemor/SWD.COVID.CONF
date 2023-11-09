library(readtext)
library(dplyr)
library(stringr)

# Base path
import_texts_path <- "//Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Texts_Sweden/Translated_texts"
export_path <- "/Users/antoine/Documents/GitHub.nosync/SWD.COVID.CONF/Data/Database"


# Press conferences from the National Assembly website
dir1 <- file.path(import_texts_path)
files1 <- list.files(dir1, pattern = "*.txt", full.names = TRUE)
docs1 <- lapply(files1, readLines)
docs1 <- lapply(docs1, function(lines) {
  text <- paste(lines, collapse = "\n")
  text <- gsub("\r", "", text)
  text <- gsub("\n", " ", text)
  text
})

# Combining lists
docs <- c(docs1)
files <- c(files1)

# Creating a dataframe
SWD.conf_texts <- data.frame(doc_ID = basename(files), conf_txt = unlist(docs))

# Cleaning files names
SWD.conf_texts$doc_ID <- sub("\\_translated.txt", "", SWD.conf_texts$doc_ID)

# Converting dates
SWD.conf_texts$date <- as.Date(ifelse(grepl("-", SWD.conf_texts$doc_ID), 
                                     gsub("-", " ", SWD.conf_texts$doc_ID), 
                                     NA), 
                              format = "%Y %m %d")

# Creating 'date_str' variable for 'aaaa_mm_jj' format
SWD.conf_texts$date_str <- ifelse(grepl("-", SWD.conf_texts$doc_ID), 
                                 gsub("-", " ", SWD.conf_texts$doc_ID), 
                                 NA)

# Suppressing '-'
SWD.conf_texts$doc_ID <- gsub("-", "", SWD.conf_texts$doc_ID)

# Creating 'counter'
SWD.conf_texts$counter <- ave(SWD.conf_texts$doc_ID, SWD.conf_texts$doc_ID, FUN = seq_along)

# Adding counter to each date to make a unique ID
SWD.conf_texts$doc_ID <- paste0(SWD.conf_texts$doc_ID, SWD.conf_texts$counter)

# Deleting 'counter' and other useless variables
SWD.conf_texts <- SWD.conf_texts %>% select(-counter)
SWD.conf_texts$date_str<-NULL
SWD.conf_texts$word_count<-NULL

# Formating dates into %d/%m/%Y format
SWD.conf_texts$date <- as.Date(SWD.conf_texts$date, format = "%d/%m/%Y", origin="1970-01-01")

# Exporting textual database
output_file <- file.path(export_path, "SWD.conf_texts.csv")
write.csv(SWD.conf_texts, file = output_file, row.names = FALSE)
