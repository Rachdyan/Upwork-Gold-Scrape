Scrape News Data
================
Rachdyan
2023-02-02

## Introduction

We want to merge the gold closing price data that we already have in
“Gold Data.xltx” with the news headline from Reuters Gold Market Report
(<https://www.reuters.com/news/archive/goldMktRpt>). The time period is
from 1/1/2019 to 11/9/2022

Load the needed libraries for the project

``` r
library(rvest)
library(dplyr)
library(purrr)
library(glue)
library(lubridate)
library(xlsx)
library(tidyr)
library(knitr)
library(kableExtra)
```

## Scrape the Data

The web link template is
“<https://www.reuters.com/news/archive/goldMktRpt?view=page&page=%7Bi%7D&pageSize=10>”
which {i} shows the current page.

Because the last price data we have from the “Gold Data.xltx” is until
1/1/2019, we only going to scrape the headline until that day which is
in page 432.

First, create functions to scrape the page

``` r
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
```

Next, create the url for all the pages using a for loop

``` r
all_url <- c()

for (i in 1:432) {
  url <- glue("https://www.reuters.com/news/archive/goldMktRpt?view=page&page={i}&pageSize=10")
  all_url <- c(all_url, url)
}
```

Scrape all the news headline data

``` r
news_data <- map_dfr(all_url, get_news_info)

## Remove Duplicate Data
news_data <- news_data %>% distinct(headline, .keep_all = T)
```

Display the data

``` r
news_data %>% head() %>% kable(format = "html") %>% kable_styling("striped") %>%
  scroll_box(height = "300px")
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:300px; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
headline
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
link
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
time
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
gold slips from nine-month peak as dollar regains lost ground
</td>
<td style="text-align:left;">
<https://www.reuters.com/article/global-precious/precious-gold-slips-from-nine-month-peak-as-dollar-regains-lost-ground-idUSL4N34I3DB>
</td>
<td style="text-align:left;">
10:31am EST
</td>
</tr>
<tr>
<td style="text-align:left;">
dovish fed cues propel gold to 9-month peak
</td>
<td style="text-align:left;">
<https://www.reuters.com/article/global-precious/precious-dovish-fed-cues-propel-gold-to-9-month-peak-idUSL4N34I1EG>
</td>
<td style="text-align:left;">
7:24am EST
</td>
</tr>
<tr>
<td style="text-align:left;">
gold rises to highest since april after powell strikes dovish tone
</td>
<td style="text-align:left;">
<https://www.reuters.com/article/global-precious/precious-gold-rises-to-highest-since-april-after-powell-strikes-dovish-tone-idUSL4N34I1II>
</td>
<td style="text-align:left;">
2:18am EST
</td>
</tr>
<tr>
<td style="text-align:left;">
gold hits more than 9-month high after powell strikes dovish tone
</td>
<td style="text-align:left;">
<https://www.reuters.com/article/global-precious/precious-gold-hits-more-than-9-month-high-after-powell-strikes-dovish-tone-idUSL1N34I03S>
</td>
<td style="text-align:left;">
Feb 01 2023
</td>
</tr>
<tr>
<td style="text-align:left;">
gold scales late-april highs after dovish fed
</td>
<td style="text-align:left;">
<https://www.reuters.com/article/global-precious/precious-gold-scales-late-april-highs-after-dovish-fed-idUSL4N34I062>
</td>
<td style="text-align:left;">
Feb 01 2023
</td>
</tr>
<tr>
<td style="text-align:left;">
gold hits over 9-month highs as u.s. fed chief strikes dovish tone
</td>
<td style="text-align:left;">
<https://www.reuters.com/article/global-precious/precious-gold-hits-over-9-month-highs-as-u-s-fed-chief-strikes-dovish-tone-idUSL4N34H33M>
</td>
<td style="text-align:left;">
Feb 01 2023
</td>
</tr>
</tbody>
</table>

</div>

## Export the Data

Save the data into excel files

``` r
write.xlsx(news_data, "./data/news_data.xlsx", col.names = F)
```
