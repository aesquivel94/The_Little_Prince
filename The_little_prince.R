
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
  
# Maybe i need to replace the boa constrictor word too. 
# Maybe join Rose and Flowers?
tidy_little_prince <- Little_prince_clean %>%
    tibble(text = .) %>% 
    unnest() %>% 
    mutate(row_length = str_length(text)) %>% 
    filter(row_length > 0) %>% 
    mutate(Start_chapter = if_else(grepl('Chapter', text), 1, 0)) %>%
    mutate(Chapter = paste0('Chapter ', cumsum(Start_chapter)) ) %>%
    filter(!stringr::str_detect(text, 'Chapter') ) %>% 
    dplyr::select(-Start_chapter) %>%
    mutate(text = str_to_lower(text)) %>% 
    mutate(text = str_replace_all(text, c("grown ups" = "grownUps" , "grown-ups" = "grownUps", "little prince" = "littlePrince",
                                          "boa" = "boaConstrictor", 'constrictor' = '')))


tidy_little_prince <- tidy_little_prince[-which(str_detect(tidy_little_prince$text, "[^0-9]") == FALSE),]



#### 
# =-------------------------------
# Stop words 
data(stop_words)

#############################################


# Filter words, omitting stop words. 
tidy_lp <- tidy_little_prince %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = ifelse(str_detect(word,'boa'), 'boaconstrictor', word)) 


# =----  Most frequent Words. 
pl <- tidy_lp %>% 
  count(word, sort = TRUE) %>%
  filter(n > 15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) + geom_col(fill = '#4CA0DB') + 
  labs(y = NULL, x = "Frequency") + 
  theme_bw() +
  geom_text(aes(x = n, label = n, angle = 0, hjust = 0, check_overlap = T))

ggsave("C:/Users/dria-/OneDrive/Escritorio/Freq_grap.png", plot = pl)



# Sentiment analysis with tidy data

# Dictionaries
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


###############################################################
#  Sentiment analysis with inner join
# -================================ Work from here

# Sentiment distribution.
pl <- tidy_lp %>% 
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, sort = TRUE) %>% 
  mutate(sentiment = reorder(sentiment, n)) %>%
  ggplot(aes(n, sentiment)) + geom_col(fill = '#4CA0DB') + 
  labs(y = NULL, x = "Frequency") + 
  theme_bw() +
  geom_text(aes(x = n, label = n, angle = 0, hjust = 0, check_overlap = T))

ggsave("C:/Users/dria-/OneDrive/Escritorio/Freq_type.png", plot = pl)


# --- Positive or negative words in the story - table.
Little_prince_sentiment <- tidy_lp %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)



#  Frequency of positive an negative sentiments in relation with words.
bing_word_counts <- tidy_lp %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Graph of negative and positive words. Spliting by sentiment types. 
Pos_neg_cont <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Blues") + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment", y = NULL) +
  theme_bw()  +
  geom_text(aes(x = n, label = n, angle = 0, hjust = 0, check_overlap = T))

ggsave("C:/Users/dria-/OneDrive/Escritorio/PN_count.png", plot = Pos_neg_cont)


# wordcloud Simple with not sentiments involve. 
tidy_lp %>% anti_join(stop_words) %>%
  count(word) %>% with(wordcloud(word, n, max.words = 50))


# worldcloud with positive and negative Sentiments.
pl <- tidy_lp %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#D81B60", "#1E88E5"),
                   title.colors=c("#D81B60", "#1E88E5"),
                   max.words = 100, title.size=2.5)

ggsave("C:/Users/dria-/OneDrive/Escritorio/WorldCloud_s.png", plot = pl)


# 2.6 Looking at units beyond just words
# LP_bigram <- tidy_little_prince %>%
#   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
#   filter(!is.na(bigram))
# LP_bigram %>%  count(bigram, sort = TRUE)
# bigrams_separated <- LP_bigram %>% separate(bigram, c("word1", "word2"), sep = " ")
# bigrams_filtered <- bigrams_separated %>%
#   filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
# # new bigram counts:
# bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
# bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
# # =----
# bigram_tf_idf <- bigrams_united %>% count(bigram) %>% arrange(desc(n))
# bigrams_separated <- bigrams_united %>% separate(bigram, c("word1", "word2"), sep = " ")
# bigrams_filtered <- bigrams_separated %>%
#     filter(!word1 %in% stop_words$word) %>% filter(!word2 %in% stop_words$word)
# # new bigram counts:
# bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
  

  
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
  
# word_cors %>%
#   filter(item1 %in% c("littleprince", "fox", "flower")) %>%
#   group_by(item1) %>%
#   slice_max(correlation, n = 6) %>%
#   ungroup() %>%
#   mutate(item2 = reorder(item2, correlation)) %>%
#   ggplot(aes(item2, correlation)) +
#   geom_bar(stat = "identity", fill = 'lightblue') +
#   facet_wrap(~ item1, scales = "free") +
#   coord_flip() + theme_bw()  
  
# Correlation graph
# set.seed(2016)
# 
# word_cors %>%
#   filter(correlation > 0.1) %>%
#   influential::graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
#   geom_node_point(color = "lightblue", size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE) +
#   theme_void()  
  

# 5.1 Tidying a document-term matrix

ap_sentiments <- tidy_lp %>% 
  inner_join(get_sentiments("bing"), by = 'word')

pl <- ap_sentiments %>% mutate(term = word) %>% 
  count(sentiment, term) %>%
  filter(n >= 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  scale_fill_brewer(palette = "Accent") + 
  labs(x = "Frequency", y = NULL) + theme_bw() +
  geom_text(aes(x = n, label = n, angle = 0, hjust = 0, check_overlap = T))

ggsave("C:/Users/dria-/OneDrive/Escritorio/WorldCloud_s.png", plot = pl)




# =------------ Working from here:

plot_df <- tidy_lp %>% 
  inner_join(get_sentiments("nrc")) %>%
  # count(sentiment, sort = TRUE) %>% 
  # mutate(sentiment = reorder(sentiment, n)) 
  group_by(sentiment) %>%
  count(sentiment, sort = TRUE)

# Basic radar chart
plt <- ggplot(plot_df) +
  # Make custom panel grid
  geom_col(aes(x = reorder(str_wrap(sentiment, 10), n), y = n, fill =sentiment
  ), position = "dodge2", show.legend = TRUE, alpha = .9) +
  # Add dots to represent the mean gain
  geom_point( aes(x = reorder(str_wrap(sentiment, 5),n),y = n),
    size = 3,color = "gray12") +
  ylim(-150,max(plot_df$n)) +
  # Lollipop shaft for mean gain per region
  geom_segment(aes(x = reorder(str_wrap(sentiment, 10), n),
                   y = 0, xend = reorder(str_wrap(sentiment, 10), n), yend = 600),
               linetype = "dashed", color = "gray12") + 
  # Make it circular!
  coord_polar(start = 0)  +
  theme_minimal()
 
  
  
plt <-  plt +
  # New fill and legend title for number of tracks per region
  scale_fill_brewer(palette = "Purples") #+
  #   "Amount of Tracks",
  theme(
    # Remove axis ticks and text
    axis.title = element_blank(), axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12)  )


# Final chart
plt <- plt +
  # Add labels
  labs(
    title = "\nEmotional Journey Through 'The Little Prince': \nA Sentiment Analysis Exploration",
    subtitle = paste(
      "This visualization depicts the predominant emotional landscape of 'The Little Prince',",
      "highlighting the frequency of various sentiments throughout the narrative.\n",
      "If you're exploring the emotional depth of literature, 'The Little Prince' offers a rich terrain,",
      "with diverse sentiments ranging from joy and trust to sadness and anger.",
      sep = "\n"
    ),
    caption = "\n\nData Visualisation by Alejandra Esquivel\nLink to Data: github.com/aesquivel94/The_Little_Prince") +
  # Customize general theme
  theme(
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white")
  ) 

# Use `ggsave("plot.png", plt,width=9, height=12.6)` to save it as in the output

plt +
  scale_x_discrete(labels = paste0(toupper(substr(plot_df$sentiment, 1, 1)), substr(plot_df$sentiment, 2, nchar(plot_df$sentiment))) )

                   