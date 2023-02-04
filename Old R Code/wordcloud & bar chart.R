library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(xlsx)
library(readxl)
library(dplyr)
library(tidyquant)

news_data <- read_xlsx("news_data.xlsx")
headline <- news_data$headline
headline_no_gold <- gsub("gold", "", headline)

headline_docs <- Corpus(VectorSource(headline_no_gold))


###### Remove Number, Punctuation, Whitespace
docs_cleaned <- headline_docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

#########Transform into lowercase
docs_cleaned <- tm_map(docs_cleaned, content_transformer(tolower))

######Remove Stopwords
docs_cleaned <- tm_map(docs_cleaned, removeWords, stopwords("english"))


dtm <- TermDocumentMatrix(docs_cleaned) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


#########Create the WordCloud
##### png("headline_wordcloud.png", width = 500, height = 500, res = 120)
set.seed(1313)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, 
          rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))
#### dev.off()


##########Make a Bar Chart of the most frequent word

df %>% top_n(15) %>% ggplot(aes(x = reorder(word, freq), y = freq, label = freq)) + 
  geom_bar(stat = "identity", position = "dodge", fill = "#2C3E50") +  
  geom_text(hjust = 1.2, color = "#F8F7F3", fontface = "bold") + 
  labs(y="Frequency", 
       x="",
       title = "Most Frequent Word in the News Headline") +
  coord_flip() + 
  theme_tq()

ggsave("most frequent word.png")

