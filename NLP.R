#######################################################################################
## NLP(Natural Language Processing)
#LDA Analysis on different famous literatures 

### R.Script 

#### Set Environment 
if (!require(tidyr)){install.packages('tidyr');library(tidyr)}
if (!require(tidytext)){install.packages('tidytext');library(tidytext)}
if (!require(tidyverse)){install.packages('tidyverse');library(tidyverse)}
if (!require(gutenbergr)){install.packages("gutenbergr");library(gutenbergr)}
if (!require(stringr)){install.packages("stringr");library(stringr)}
if (!require(topicmodels)){install.packages("topicmodels");library(topicmodels)}
if (!require(wordcloud2)){install.packages("wordcloud2");library(wordcloud2)}
####Load Data 
titles <- c('War and Peace',
            'Twenty Thousand Leagues under the Sea',
            'Pride and Prejudice',
            'The War of the Worlds')

books <- gutenberg_works(title %in% titles)%>%
  gutenberg_download(meta_fields = 'title')
glimpse(books)

# Divide into documents, each representating a chapter 
by_chapter <- books %>%
  group_by(title)%>%
  mutate(chapter = cumsum(str_detect(text, regex('^chapter',ignore_case = TRUE))))%>%
  filter(chapter > 0)%>%
  ungroup%>%
  unite(document, title, chapter)

# Split into words 
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# Find document -word count 
word_counts <- by_chapter_word %>%
  anti_join(stop_words)%>%
  count(document, word, sort = TRUE)%>%
  ungroup()

c_dtm <- word_counts %>%
  cast_dtm(document, word, n)

### LDA
set.seed(123)
Chapter_lda <- LDA(c_dtm,k=4)
Chapter_lda
### Chapter topics 
topics <- tidy(Chapter_lda,matrix = 'beta')

top_terms <- topics %>%
  group_by(topic)%>%
  top_n(5,beta)%>%
  ungroup()%>%
  arrange(topic, -beta)
top_terms                         

top_terms %>%
  mutate(term = reorder_within(term,beta,topic))%>%
  ggplot(aes(x=beta, y=term, fill = factor(topic)))+
  geom_col(show.legend = F)+
  facet_wrap(~topic, scales = 'free')+
  scale_y_reordered()

# Basic plot
word_counts <- word_counts %>%
  select(-c('document'))
wordcloud2(data=word_counts, size=1.6)

