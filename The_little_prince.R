
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
  

# 2. Little prince filter and create the groups  

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
  ggplot(aes(n, word)) + geom_col(fill = 'lightsteelblue3') + 
  labs(y = NULL, x = "Frequency") + 
  theme_bw() +
  labs() +
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
# pl <-
# tidy_lp %>% 
#   inner_join(get_sentiments("nrc")) %>%
#   count(sentiment, sort = TRUE) %>% 
#   mutate(sentiment = reorder(sentiment, n)) %>%
#   ggplot(aes(n, sentiment)) + geom_col(fill = '#4CA0DB') + 
#   labs(y = NULL, x = "Frequency") + 
#   theme_bw() +
#   geom_text(aes(x = n, label = n, angle = 0, hjust = 0, check_overlap = T))
# 
# ggsave("C:/Users/dria-/OneDrive/Escritorio/Freq_type.png", plot = pl)


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
# Pos_neg_cont <- bing_word_counts %>%
#   group_by(sentiment) %>%
#   slice_max(n, n = 10) %>% 
#   ungroup() %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(n, word, fill = sentiment)) +
#   geom_col(show.legend = FALSE) +
#   scale_fill_brewer(palette = "Blues") + 
#   facet_wrap(~sentiment, scales = "free_y") +
#   labs(x = "Contribution to sentiment", y = NULL) +
#   theme_bw()  +
#   geom_text(aes(x = n, label = n, angle = 0, hjust = 0, check_overlap = T))
# 
# ggsave("C:/Users/dria-/OneDrive/Escritorio/PN_count.png", plot = Pos_neg_cont)


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

  


install.packages("ggwordcloud")
library(ggwordcloud)
set.seed(42)

love_words <- tidy_lp  %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

wcl <- ggplot(love_words, aes(label = word, size = n, color = sentiment)) +
  geom_text_wordcloud_area(
    mask = png::readPNG(system.file("extdata/hearth.png",
                                    package = "ggwordcloud", mustWork = TRUE
    )),
    rm_outside = TRUE
  )  +
  scale_size_area(max_size = 40) +
  theme_minimal() +
  scale_color_manual(values=c("darkred", "red"))

ggsave("C:/Users/dria-/OneDrive/Escritorio/Int.jpg", plot = wcl)


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
  

# 5.1 Tidying a document-term matrix

ap_sentiments <- tidy_lp %>% 
  inner_join(get_sentiments("bing"), by = 'word')

Freq_np <- ap_sentiments %>% mutate(term = word) %>%
  count(sentiment, term) %>%
  filter(n >= 5) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  scale_fill_manual(values=c("lightcoral","lightsteelblue3"),
                    labels = paste0(toupper(substr(ap_sentiments$sentiment, 1, 1)), 
                                    substr(ap_sentiments$sentiment, 2, 
                                           nchar(ap_sentiments$sentiment))))+
  scale_y_discrete(labels = paste0(toupper(substr(ap_sentiments$word, 1, 1)), 
                                   substr(ap_sentiments$word, 2, 
                                          nchar(ap_sentiments$word)))) +
  labs(x = "Frequency", y = NULL) + theme_bw() +
  geom_text(aes(x = n, label = n, angle = 0, hjust = 0, check_overlap = T))

ggsave("C:/Users/dria-/OneDrive/Escritorio/Freq_np.png", plot = Freq_np)




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
  theme_minimal() +
  scale_x_discrete(labels = paste0(toupper(substr(plot_df$sentiment, 1, 1)), 
                                   substr(plot_df$sentiment, 2, 
                                          nchar(plot_df$sentiment))) )
  

 plt <-  plt +
  # New fill and legend title for number of tracks per region
  scale_fill_brewer(palette = "Spectral", name = "Sentiment", 
                    labels = paste0(toupper(substr(plot_df$sentiment, 1, 1)), 
                                    substr(plot_df$sentiment, 2, 
                                           nchar(plot_df$sentiment)))) +
  #   "Amount of Tracks",
  theme( # Remove axis ticks and text
    axis.title = element_blank(), axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    # Use gray text for the region names
    axis.text.x = element_text(color = "gray12", size = 12)  ) +
   labs( plot.title=element_text(hjust=0.5, vjust=0.1, face='bold', colour="blue"),
    title = "\nEmotional Journey Through 'The Little Prince': \nA Sentiment Analysis Exploration\n",
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
    panel.background = element_rect(fill = "white", color = "white") )  

 plt
 
 ggsave("C:/Users/dria-/OneDrive/Escritorio/circular.jpg", plt, width=8, height=8.5)

                   