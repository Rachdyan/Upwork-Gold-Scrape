library(rvest)
library(dplyr)
library(purrr)
library(glue)
library(lubridate)
library(readxl)
library(xlsx)
library(tidyr)

url <- "https://www.reuters.com/news/archive/goldMktRpt?view=page&page=1&pageSize=10"

### COBA !

page <- read_html(url)

all_story <- page %>% html_nodes("article[class='story ']")

headline_raw <- all_story[1] %>% html_node("h3[class='story-title']") %>% html_text2()
headline <- sub("PRECIOUS-", "", headline_raw) %>% tolower()

link_raw <- all_story[1] %>% html_node("a") %>% html_attr("href")
link <- paste("https://www.reuters.com", link_raw, sep = "")

time <- all_story[1] %>% html_node("span[class='timestamp']") %>% html_text2()

############################################################################333
### Function

get_news_info <- function(url){
  page <- read_html(url)
  
  all_story <- page %>% html_nodes("article[class='story ']")
  
  headline <- map_chr(all_story, get_headline)
  link <- map_chr(all_story, get_link)
  time <- map_chr(all_story, get_time)
  
  result_df <- tibble(headline = headline, link = link, time = time)
}


get_headline <- function(story){
  headline_raw <- story %>% html_node("h3[class='story-title']") %>% html_text2()
  headline <- sub("PRECIOUS-", "", headline_raw) %>% tolower()
}


get_link <- function(story){
  link_raw <- story %>% html_node("a") %>% html_attr("href")
  link <- paste("https://www.reuters.com", link_raw, sep = "")
}

get_time <- function(story){
  time <- story %>% html_node("span[class='timestamp']") %>% html_text2()
}

################################################################################


all_url <- c()

for (i in 1:415) {
  url <- glue("https://www.reuters.com/news/archive/goldMktRpt?view=page&page={i}&pageSize=10")
  all_url <- c(all_url, url)
}


news_data_1_100 <- map_dfr(all_url[1:100], get_news_info)
news_data_101_200 <- map_dfr(all_url[101:200], get_news_info)
news_data_201_300 <- map_dfr(all_url[201:300], get_news_info)
news_data_301_415 <- map_dfr(all_url[301:415], get_news_info)

news_data <- rbind(news_data_1_100, news_data_101_200)
news_data <- rbind(news_data, news_data_201_300)
news_data <- rbind(news_data, news_data_301_415)

str(news_data)

news_data$time[1] <- "Dec 06 2022"

news_data_raw <- news_data



news_data$time <- mdy(news_data$time)

##REMOVE duplicate
news_data <- news_data %>% distinct(headline, .keep_all = T)
write.xlsx(news_data, "news_data.xlsx")

#######Load Gold Price
gold_price <- read_xlsx("Gold Prices.xltx")

gold_price$Date <- ymd(gold_price$Date)




#############################3

combined <- left_join(gold_price, news_data, by = c("Date" = 'time'))

combined <- combined %>% group_by(`S/No.`) %>% mutate(count_headline = sum(!is.na(headline)),
                                                   headline_index = row_number()) %>% ungroup()


combined[is.na(combined$headline), ]$headline_index <- 0

combined <- combined %>% mutate(headline_code = glue("Headline {headline_index}"))

combined <- combined %>% select(-(4:6))

wide_data <- combined %>% pivot_wider(names_from = headline_code, values_from = headline)


filled_data <- wide_data %>% group_by(Date) %>% fill(`Headline 1`, `Headline 2`, `Headline 3`, `Headline 4`, `Headline 5`, `Headline 6`)

results <- filled_data %>% group_by(Date) %>% filter(count_headline == headline_index)

results <- results %>% select(-count_headline, -headline_index, -`Headline 0`)

results <- results %>% ungroup()

write.xlsx(results, "Gold Prices With Headline.xlsx")



library(compareDF)
comparee <- compare_df(test_result, gold_price, c("`S/No.`"))
library(arsenal)

comparedf(test_result, gold_price)

summary(comparedf(test_result, gold_price))
