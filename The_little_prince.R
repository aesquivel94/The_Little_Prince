# If is not install

# R options
options(warn = -1, scipen = 999)

# Load libraries
suppressMessages(if(!require("pacman")) install.packages("pacman"))
suppressMessages(pacman::p_load(purrr, pdftools, wordcloud, dplyr, reshape2, stringr, textdata, scales, tidytext, tidyr, ggplot2))


# Reading
Little_prince <- pdftools::pdf_text(pdf = "https://andonovicmilica.files.wordpress.com/2018/07/the_little_prince.pdf") %>% 
  readr::read_lines()

# Preprocess the text data
Little_prince_clean <- Little_prince %>%
  str_replace_all("\\s+", " ") %>%  # Remove extra whitespaces
  str_trim()  # Trim leading and trailing whitespaces



# Print the first few rows

Little_prince_df <- data_frame(Little_prince_clean)
chapter_titles <- Little_prince_df %>%
  str_extract_all("\\bChapter\\s\\d+\\b") %>%
  unlist()

leng <- chapter_titles %>% tibble() %>% distinct() %>% dim() %>% .[1] 
  paste('Chapter', 1:leng)

  length(Little_prince_clean)

  

Little_prince_clean %>% tibble(text = .) %>% unnest() %>% 
  mutate(row_lenght = Little_prince_clean %>% str_length(.)) %>% 
  dplyr::filter(row_lenght > 0) %>% 
  dplyr::select(text) %>% 
  mutate(row = 1:dim(.[1])) %>% 
  mutate(title = if_else(grepl('Chapter', text) == TRUE, 1, 0)) %>% 
  filter(title == 1) 




# usethis::create_from_github(
#   "https://github.com/aesquivel94/The_Little_Prince.git",
#   destdir = "C:/Users/dria-/OneDrive/Escritorio/Portafolio/book_text/",
#   fork = FALSE
# )

