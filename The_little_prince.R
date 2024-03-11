# If is not install

# usethis::create_from_github(
#   "https://github.com/aesquivel94/The_Little_Prince.git",
#   destdir = "C:/Users/dria-/OneDrive/Escritorio/Portafolio/book_text/",
#   fork = FALSE
# )

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

  

# Chapters extraction. 
titles <- Little_prince_clean %>% tibble(text = .) %>% unnest() %>% 
  mutate(row_lenght = Little_prince_clean %>% str_length(.)) %>% 
  dplyr::filter(row_lenght > 0) %>% 
  dplyr::select(text) %>% 
  mutate(row = 1:dim(.[1])) %>% 
  mutate(title = if_else(grepl('Chapter', text) == TRUE, 1, 0)) %>% 
  filter(title == 1) %>% .[nrow(.),1]





# Test ext

data(stop_words)

tidy_little_prince <- Little_prince_clean %>% 
  tibble(text = .) %>% 
  unnest() %>% 
  filter(row_number() > 190) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)  





tidy_little_prince %>%
  count(word, sort = TRUE) %>%
  filter(n > 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) + 
  theme_bw()



# Test scatterplot

frequency <- tidy_little_prince %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(word) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) 



# Sentiment analysis with tidy data

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")



#  Sentiment analysis with inner join


nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

# ---Joy
tidy_little_prince %>% 
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


# --- Positive or negative words 

Little_prince_sentiment <- tidy_little_prince %>%
  inner_join(get_sentiments("bing")) %>%
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
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


# 2.6 Looking at units beyond just words

p_and_p_sentences <- tibble(text = Little_prince_clean) %>% 
  unnest_tokens(sentence, text, token = "sentences")

p_and_p_sentences$sentence[2]





# =-------------------------------------------
LP_bigram <- Little_prince_clean %>% 
  tibble(text = .) %>% 
  unnest() %>% 
  filter(row_number() > 190) %>%
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




# - Trigram

  Little_prince_clean %>% 
  tibble(text = .) %>% 
  unnest() %>% 
  filter(row_number() > 190) %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

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

