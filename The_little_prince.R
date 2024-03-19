
# Loading Packages 

# R options
options(warn = -1, scipen = 999)

# Load libraries
suppressMessages(if(!require("pacman")) install.packages("pacman"))
suppressMessages(pacman::p_load(purrr, pdftools, wordcloud, dplyr, reshape2, stringr, textdata, scales, tidytext, tidyr, ggplot2))


# 1. Reading and preprocesing data: 

# Reading little prince.
Little_prince <- pdftools::pdf_text(pdf = "https://andonovicmilica.files.wordpress.com/2018/07/the_little_prince.pdf") %>% 
  readr::read_lines()

# Preprocess the text data
Little_prince_clean <- Little_prince %>%
  str_replace_all("\\s+", " ") %>%  # Remove extra whitespaces
  str_trim()  # Trim leading and trailing whitespaces


# Little prince as data_frame
Little_prince_df <- data_frame(Little_prince_clean)

# Identify the little prince chapter titles
chapter_titles <- Little_prince_df %>%
  str_extract_all("\\bChapter\\s\\d+\\b") %>%
  unlist()

leng <- chapter_titles %>% tibble() %>% distinct() %>% dim() %>% .[1] 
  paste('Chapter', 1:leng)

# length(Little_prince_clean) # How many rows has the little prince

  

# Chapters extraction.
  
  Little_prince_clean <- Little_prince_clean %>% 
    tibble(text = .) %>% 
    unnest() %>% 
    filter(row_number() >= which(Little_prince_clean == chapter_titles[1]) ) 

  
  
  
  # Define the title condition function
  title_condition <- function(text) {
    grepl("^Chapter", text)  # Example condition: Lines starting with "Chapter" are considered titles
  } 
  
  LP_tiles <-  Little_prince_clean %>%
    mutate(row_lenght = Little_prince_clean$text %>% str_length(.)) %>% 
    dplyr::filter(row_lenght > 0) %>% 
    dplyr::select(text) %>% 
    mutate(row = 1:dim(.[1])) 
  
  
  # Little prince filter and create the groups
  tidy_little_prince <- Little_prince_clean %>%
    tibble(text = .) %>% 
    unnest() %>% 
    mutate(row_length = str_length(text)) %>% 
    filter(row_length > 0) %>% 
    mutate(Start_chapter = if_else(grepl('Chapter', text), 1, 0)) %>%
    mutate(Chapter = paste0('Chapter ', cumsum(Start_chapter)) ) %>% 
    filter(!stringr::str_detect(text, 'Chapter') )


#### 
# =-------------------------------
# Stop words 

data(stop_words)

tidy_little_prince <- tidy_little_prince %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)  


# =----  Most frequent Words. 

tidy_little_prince %>% count(word, sort = TRUE) %>% 
  filter(n > 15) %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) + geom_col(fill = '#4CA0DB') + 
  labs(y = NULL) + theme_bw()



# Test scatterplot

frequency_LP <- tidy_little_prince %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(word) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) 



# Sentiment analysis with tidy data

# Dictionaries
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


###############################################################
#  Sentiment analysis with inner join
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# ---Joy
tidy_little_prince %>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


# --- Positive or negative words by chapter

Little_prince_sentiment <- tidy_little_prince %>%
  inner_join(get_sentiments("bing")) %>%
  # group_by(Chapter) %>% 
  count(sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)



#  Frequency 
bing_word_counts <- tidy_little_prince %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Graph
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) +
  theme_bw()

# --= 
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

# wordcloud

tidy_little_prince %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# Mix-sentiments

tidy_little_prince %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#D81B60", "#1E88E5"),
                   max.words = 100)


# 2.6 Looking at units beyond just words
# Start from here. 
p_and_p_sentences <- Little_prince_clean %>%
  filter(!stringr::str_detect(text, 'Chapter')) %>% 
  unnest_tokens(sentence, text, token = "sentences") 
  

p_and_p_sentences$sentence[2]





# =-------------------------------------------
LP_bigram <- Little_prince_clean %>% 
  tibble(text = .) %>% 
  unnest() %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))


LP_bigram %>%
  count(bigram, sort = TRUE)



bigrams_separated <- LP_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts



bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united




# - Trigram --- No useful by now. 

  Little_prince_clean %>% 
  tibble(text = .) %>% 
  unnest() %>% 
  # filter(row_number() > 190) %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
  
  

# =----
  
  bigram_tf_idf <- bigrams_united %>%
    count(bigram) %>% 
    # bind_tf_idf(bigram, n) %>%
    arrange(desc(n))

  
  
  bigrams_separated <- bigrams_united %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
  # new bigram counts:
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
  bigram_counts
  
  
  
  library(ggraph)
  set.seed(2017)
  
  bigram_graph <- bigram_counts %>%
    filter(n > 2) %>%
    influential::graph_from_data_frame()
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  bigram_graph
  
  
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
  
  
  # =- 
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
  
  count_bigrams <- function(dataset) {
    dataset %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word) %>%
      count(word1, word2, sort = TRUE)
  }
  
  visualize_bigrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  }
  
  
  
  
  # =--------------------------------
  library(widyr)
  
  
  LP_section_words <- Little_prince_clean %>% 
    tibble(text = .) %>% 
    unnest() %>% 
    mutate(section = row_number() %/% 10) %>%
    filter(section > 0) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)
  
  # count words co-occuring within sections
  word_pairs <- LP_section_words %>%
    pairwise_count(word, section, sort = TRUE)
  
  word_pairs
  
  
  word_pairs %>%
    filter(item1 == "prince")
  
  # Correlation
  
  word_cors <- LP_section_words %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, section, sort = TRUE)
  
  word_cors
  
  
  
  word_cors %>%
    filter(item1 == "prince")
  
  
  word_cors %>%
    filter(item1 %in% c("prince", "fox", "flower")) %>%
    group_by(item1) %>%
    slice_max(correlation, n = 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()
  
  
  
  set.seed(2016)
  
  word_cors %>%
    filter(correlation > .1) %>%
    influential::graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
  
  
  
  
  
  # 5.1 Tidying a document-term matrix
  
  ap_td <- tidy_little_prince 
  ap_td
  
  # ap_sentiments <-
  
  ap_sentiments <- ap_td %>% 
    inner_join(get_sentiments("bing"), by = 'word')
    
  ap_sentiments %>% mutate(term = word) %>% 
    count(sentiment, term) %>%
    # ungroup() %>%
    filter(n >= 5) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(n, term, fill = sentiment)) +
    geom_col() +
    labs(x = "Contribution to sentiment", y = NULL)

  
# Ideas:   
####  ---------------------------=
# Places
# Little_prince_clean 
# https://en.wikipedia.org/wiki/The_Little_Prince
# tokens <- tibble(line = 1, word = unlist(str_split(Little_prince_clean, "\\s+")))

# Tokenize the text into words
tokens <- tibble(word = unlist(str_split(Little_prince_clean, "\\s+")))

# Perform word frequency analysis to identify frequently occurring words
word_freq <- tokens %>%
  count(word, sort = TRUE)

# Filter out common stopwords
word_freq_filtered <- word_freq %>%
  anti_join(stop_words)

# Maybe this should be done by characer?

# Manually review the identified words to filter out irrelevant terms and identify actual place names
word_freq_filtered %>%
  filter(word %in% c("planet", "Earth", "Desert", "Asteroid")) %>% 
  ggplot (aes(x= word, y = n)) +
  geom_col() +
  theme_bw()



# =---------- Ideas part 2

# ap_sentiments


# word_pairs <- LP_section_words



# Idea <- word_cors %>%
#   filter(item1 %in% c("prince", "fox", "flower")) %>%
#   group_by(item1) %>%
#   slice_max(correlation, n = 6) %>%
#   ungroup() %>%
#   mutate(item2 = reorder(item2, correlation))





# Example from https://github.com/ricardo-bion/ggradar
# library(ggplot2)
# devtools::install_github("ricardo-bion/ggradar", 
#                          dependencies = TRUE)
# library(ggradar)
# suppressPackageStartupMessages(library(dplyr))
# library(scales)
# 
# mtcars %>%
#   add_rownames( var = "group" ) %>%
#   mutate_each(funs(rescale), -group) %>%
#   tail(4) %>% select(1:10) -> mtcars_radar
# 
# ggradar::ggradar(mtcars_radar,
#                  group.colours = RColorBrewer::brewer.pal(4, 'Set2')) +
# 
# knitr::kable(mtcars_radar,format="markdown") 
