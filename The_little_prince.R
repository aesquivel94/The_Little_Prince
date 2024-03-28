
# Loading Packages 

# R options
options(warn = -1, scipen = 999)

# Load libraries
suppressMessages(if(!require("pacman")) install.packages("pacman"))
suppressMessages(pacman::p_load(purrr, pdftools, wordcloud, dplyr, reshape2, stringr, textdata, scales, tidytext, tidyr, ggplot2, ggraph, widyr))


# 1. Reading and pre-procesing data: 

# Reading little prince.
Little_prince <- pdftools::pdf_text(pdf = "https://andonovicmilica.files.wordpress.com/2018/07/the_little_prince.pdf") %>% 
  readr::read_lines()

# Preprocess the text data
Little_prince_clean <- Little_prince %>%
  str_replace_all("\\s+", " ") %>%  # Remove extra white spaces
  str_trim()  # Trim leading and trailing white spaces


# Erase chapter titles: 
# Identify the little prince chapter titles
chapter_titles <- data_frame(Little_prince_clean) %>%
  str_extract_all("\\bChapter\\s\\d+\\b") %>%
  unlist()

# Count total chapters
leng <- chapter_titles %>% tibble() %>% distinct() %>% dim() %>% .[1] 



# Remove all of the rows that before the story start.
Little_prince_clean <- Little_prince_clean %>% 
  tibble(text = .) %>% 
  unnest() %>% 
  filter(row_number() >= which(Little_prince_clean == chapter_titles[1]) ) 

  
# Define the title condition function
  title_condition <- function(text) {
    grepl("^Chapter", text)  # Example condition: Lines starting with "Chapter" are considered titles
  } 
  

# 2. 
# Little prince filter and create the groups  
  
# Little_prince_clean %>% mutate(row_lenght = Little_prince_clean$text %>% str_length(.)) %>% 
#   dplyr::filter(row_lenght > 0) %>% dplyr::select(text) %>% mutate(row = 1:dim(.[1])) 
  
# Maybe i need to replace the boa constrictor word too. 
# Maybe join Rose and Flowers?
tidy_little_prince <- Little_prince_clean %>%
    tibble(text = .) %>% 
    unnest() %>% 
    # mutate(title = if_else(row_number() %in% (1 + which(str_detect(Little_prince_clean$text, 'Chapter')) ), 1, 0)) %>% 
    mutate(row_length = str_length(text)) %>% 
    filter(row_length > 0) %>% 
    mutate(Start_chapter = if_else(grepl('Chapter', text), 1, 0)) %>%
    mutate(Chapter = paste0('Chapter ', cumsum(Start_chapter)) ) %>%
    filter(!stringr::str_detect(text, 'Chapter') ) %>% 
    dplyr::select(-Start_chapter) %>%
    mutate(text = str_replace_all(text, c("grown ups" = "grownups" , "grown-ups" = "grownups", "little prince" = "littleprince")))


tidy_little_prince <- tidy_little_prince[-which(str_detect(tidy_little_prince$text, "[^0-9]") == FALSE),]



#### 
# =-------------------------------
# Stop words 
data(stop_words)

#############################################


# Filter words, omitting stop words. 
tidy_lp <- tidy_little_prince %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)  


# =----  Most frequent Words. 
Freq_grap <- tidy_lp %>% count(word, sort = TRUE) %>% 
  filter(n > 15) %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) + geom_col(fill = '#4CA0DB') + 
  labs(y = NULL, x = "Frequency") + 
  theme_bw()

# frequency_LP <- tidy_lp %>%
  # count(word) %>% mutate(proportion = n / sum(n)) %>%  select(-n) 



# Sentiment analysis with tidy data

# Dictionaries
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


###############################################################
#  Sentiment analysis with inner join
# -================================ Work from here

# Sentiment distribution.
tidy_lp %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = TRUE) %>% 
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(n, sentiment)) + geom_col(fill = '#4CA0DB') + 
  labs(y = NULL, x = "Frequency") + 
  theme_bw()


# --- Positive or negative words in the story.
Little_prince_sentiment <- tidy_lp %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)



#  Frequency 
bing_word_counts <- tidy_lp %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Graph
Pos_neg_cont <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Blues") + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL) +
  theme_bw()

# --= 
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)

# wordcloud Simple
tidy_lp %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))


# Positive and negative Sentiments
Neg_Pos_WC <- tidy_lp %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#D81B60", "#1E88E5"),
                   title.colors=c("#D81B60", "#1E88E5"),
                   max.words = 100, title.size=2.5)


# 2.6 Looking at units beyond just words

LP_bigram <- tidy_little_prince %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))


LP_bigram %>%  count(bigram, sort = TRUE)

bigrams_separated <- LP_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united # How can i use this?




# Maybe erase this part - Trigram --- No useful by now. 
  # tidy_little_prince %>% 
  # unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  # filter(!is.na(trigram)) %>%
  # separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  # filter(!word1 %in% stop_words$word,
  #        !word2 %in% stop_words$word,
  #        !word3 %in% stop_words$word) %>%
  # count(word1, word2, word3, sort = TRUE)
  
  

# =----
  
  bigram_tf_idf <- bigrams_united %>%
    count(bigram) %>% 
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
  
  
  
# =---------------
  # set.seed(2017)
  # 
  # bigram_graph <- bigram_counts %>%
  #   filter(n > 2) %>%
  #   influential::graph_from_data_frame()
  # 
  # a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  # 
  # ggraph(bigram_graph, layout = "fr") +
  #   geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
  #                  arrow = a, end_cap = circle(.07, 'inches')) +
  #   geom_node_point(color = "lightblue", size = 5) +
  #   geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  #   theme_void()
  
  

  
  # =--------------------------------
  # Working from here
  # =--------------------------------
  
  LP_section_words <- tidy_little_prince %>%
    mutate(section = row_number() %/% 10) %>%
    filter(section > 0) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)
  
  # count words co-occuring within sections
  word_pairs <- LP_section_words %>%
    pairwise_count(word, section, sort = TRUE)

  
  # Correlation
  word_cors <- LP_section_words %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, section, sort = TRUE)
  
  word_cors %>%
    filter(item1 %in% c("littleprince", "fox", "flower")) %>%
    group_by(item1) %>%
    slice_max(correlation, n = 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity", fill = 'lightblue') +
    facet_wrap(~ item1, scales = "free") +
    coord_flip() + theme_bw()
  
  
  # Correlation graph
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
  
  ap_sentiments <- tidy_lp %>% 
    inner_join(get_sentiments("bing"), by = 'word')
    
  ap_sentiments %>% mutate(term = word) %>% 
    count(sentiment, term) %>%
    filter(n >= 5) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(n, term, fill = sentiment)) +
    geom_col() +
    scale_fill_brewer(palette = "Accent") + 
    labs(x = "Contribution to sentiment", y = NULL) + theme_bw()
  
  

  
  
  ajam <- ap_sentiments %>% mutate(term = word) %>% 
    count(sentiment, term) %>% #  Chapter,
    # mutate(Chapter = str_remove(Chapter, 'Chapter') %>% as.numeric() ) %>%
    # distinct(sentiment , term, n, .keep_all=TRUE) %>%
    arrange(desc(n)) %>% filter(n > 5)
    
  # UpSetR::upset(fromList(ajam),nsets = 10) 
  
  # Assuming 'n' is a character vector
  ajam$term <- as.list(ajam$term)
  
  # Plotting with ggplot and scale_x_upset
  pl <- ggplot(data = ajam, aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    scale_x_upset(n_intersections = 15) +
    scale_fill_brewer(palette = "Accent") + 
    labs(y = "Contribution to sentiment", x = NULL) + 
    theme_bw() +
    theme(legend.position = "top")
  ggsave("C:/Users/dria-/OneDrive/Escritorio/movie_genre_barchart.png", plot = pl)
  
  # tidy_movies %>%
  #   distinct(title, year, length, .keep_all=TRUE) %>%
  #   ggplot(aes(x=Genres)) +
  #   geom_bar() + scale_x_upset(n_intersections = 20)
  
  

  
  
  # Ideas:   Main focus...
####  ---------------------------=
# Places
# Little_prince_clean 
# https://en.wikipedia.org/wiki/The_Little_Prince
# tokens <- tibble(line = 1, word = unlist(str_split(Little_prince_clean, "\\s+")))

# Tokenize the text into words
# tokens <- tibble(word = unlist(str_split(Little_prince_clean, "\\s+")))

# Perform word frequency analysis to identify frequently occurring words
# word_freq <- tokens %>%
#   count(word, sort = TRUE)

# Filter out common stopwords
# word_freq_filtered <- word_freq %>%
#   anti_join(stop_words)

# Maybe this should be done by character?

# Manually review the identified words to filter out irrelevant terms and identify actual place names
# word_freq_filtered %>%
#   filter(word %in% c("planet", "Earth", "Desert", "Asteroid")) %>% 
#   ggplot (aes(x= word, y = n)) + geom_col(fill = 'lightblue') +
#   theme_bw()

# =-----------

