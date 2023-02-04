
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)


news_data <- read_xlsx("news_data.xlsx")
headline <- news_data$headline
headline_no_gold <- gsub("gold", "", headline)

headline_docs <- Corpus(VectorSource(headline_no_gold))

docs_cleaned <- headline_docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

docs_cleaned <- tm_map(docs_cleaned, content_transformer(tolower))
docs_cleaned <- tm_map(docs_cleaned, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs_cleaned) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)




set.seed(1313) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

set.seed(1313)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, 
          rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))


png("headline_wordcloud.png", width = 500, height = 500, res = 120)
set.seed(1313)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, 
          max.words=200, random.order=FALSE, 
          rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

#############################33333
headline_cleaned <- gsub("u.s.", "us", headline)

#
headline_cleaned <- gsub("@\\w+", "", headline_cleaned)
headline_cleaned <- gsub("https?://.+", "", headline_cleaned)
headline_cleaned <- gsub("\\d+\\w*\\d*", "", headline_cleaned)
headline_cleaned <- gsub("#\\w+", "", headline_cleaned)
headline_cleaned <- gsub("[^\x01-\x7F]", "", headline_cleaned)
headline_cleaned <- gsub("[[:punct:]]", " ", headline_cleaned)

# Remove spaces and newlines
headline_cleaned <- gsub("\n", " ", headline_cleaned)
headline_cleaned <- gsub("^\\s+", "", headline_cleaned)
headline_cleaned <- gsub("\\s+$", "", headline_cleaned)
headline_cleaned <- gsub("[ |\t]+", " ", headline_cleaned)
