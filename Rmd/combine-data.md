combine-data
================
Rachdyan
2023-02-03

## Introduction

We want to combine the gold price data we have with news headline data
that we scraped before.

Load the needed libraries

``` r
library(dplyr)
library(readxl)
library(xlsx)
library(lubridate)
library(glue)
library(tidyr)
library(knitr)
library(kableExtra)
```

## Load the data

Load the needed data

``` r
# Load the gold prices data
gold_prices <- read_xlsx("./data/Gold Prices.xltx")
```

    ## Warning: Expecting numeric in C1007 / R1007C3: got 'NA'

``` r
# Convert the date data to date format
gold_prices$Date <- ymd(gold_prices$Date)

# Load the news headline data
news_data <- read_xlsx("./data/news_data.xlsx")
```

    ## New names:
    ## • `` -> `...1`

``` r
news_data <- news_data[,-c(1,3)]
news_data$time <- mdy(news_data$time)
```

    ## Warning: 2 failed to parse.

## Combine the Data

``` r
## Left join the gold prices data with the news headline data
combined_data <- left_join(gold_prices, news_data, by = c("Date" = 'time'))

## Create a count_headline column to count the number of headlines on that day, and count_index column to give index to the headlines
combined_data <- combined_data %>% group_by(`S/No.`) %>% mutate(count_headline = sum(!is.na(headline)),
                                                   headline_index = row_number()) %>% ungroup()

## Make the headline_index value 0 for the date that doesn't have a headline
combined_data[is.na(combined_data$headline), ]$headline_index <- 0

## Create a headline_code column to give name to the column index
combined_data <- combined_data %>% mutate(headline_code = glue("Headline {headline_index}"))

## Remove unnecessary columns
combined_data <- combined_data %>% select(-(4:6))

## Convert the data to wide format
combined_data <- combined_data %>% pivot_wider(names_from = headline_code, values_from = headline)

## Fill the missing headline column by the previous value
combined_data <- combined_data %>% group_by(Date) %>% fill(`Headline 1`, `Headline 2`, `Headline 3`, `Headline 4`, `Headline 5`, `Headline 6`)

## Remove the uncomplete data on the same date
combined_data <- combined_data %>% group_by(Date) %>% filter(count_headline == headline_index)

## Tidy the data by removing unnecessary columns
combined_data <- combined_data %>% select(-count_headline, -headline_index, -`Headline 0`) %>% ungroup()
```

Display the data

``` r
combined_data %>% head() %>% kable() %>% kable_styling("striped") %>%
  scroll_box(height = "500px")
```

<div
style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:500px; ">

<table class="table table-striped" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
S/No. 
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Date
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
Closing Price
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Headline 1
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Headline 2
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Headline 3
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Headline 4
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Headline 5
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Headline 6
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
261
</td>
<td style="text-align:left;">
2019-01-01
</td>
<td style="text-align:right;">
1281.30
</td>
<td style="text-align:left;">
gold dips as renewed risk appetite lifts asian stocks
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
260
</td>
<td style="text-align:left;">
2019-01-02
</td>
<td style="text-align:right;">
1285.91
</td>
<td style="text-align:left;">
gold gains on global growth fears, falling asian stocks
</td>
<td style="text-align:left;">
gold off 6-1/2 month peak on equity rebound, stronger dollar
</td>
<td style="text-align:left;">
gold hits multi-month high as falling equities cement growth fears
</td>
<td style="text-align:left;">
gold hits over 6-month high on falling dollar, equities
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
259
</td>
<td style="text-align:left;">
2019-01-03
</td>
<td style="text-align:right;">
1295.26
</td>
<td style="text-align:left;">
gold climbs to over 6-month peak on global slowdown fears
</td>
<td style="text-align:left;">
gold rises to highest since mid-2018 on global growth woes
</td>
<td style="text-align:left;">
gold lifted by signs of ailing global economy
</td>
<td style="text-align:left;">
gold hits over 6-month high on growth fears, stock volatility
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
258
</td>
<td style="text-align:left;">
2019-01-04
</td>
<td style="text-align:right;">
1284.89
</td>
<td style="text-align:left;">
gold slides after solid jobs data; palladium crosses key \$1,300 mark
</td>
<td style="text-align:left;">
gold slips from six-month peak, still on course for weekly gain
</td>
<td style="text-align:left;">
gold trims gains as equities recoup on fresh trade talks
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
257
</td>
<td style="text-align:left;">
2019-01-07
</td>
<td style="text-align:right;">
1288.93
</td>
<td style="text-align:left;">
gold holds steady as fed rate pause bets weigh on dollar
</td>
<td style="text-align:left;">
gold gains on dollar weakness, dovish fed; palladium sets new record
</td>
<td style="text-align:left;">
gold rises, palladium hits record high as fed shift hopes hurt dollar
</td>
<td style="text-align:left;">
gold up as fed stance pricks dollar, stocks rally cap gains
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
256
</td>
<td style="text-align:left;">
2019-01-08
</td>
<td style="text-align:right;">
1285.20
</td>
<td style="text-align:left;">
gold steady as market awaits news on trade deal
</td>
<td style="text-align:left;">
gold dips as trade hopes lift stocks, palladium hits record
</td>
<td style="text-align:left;">
gold dips as dollar gains, trade deal hopes lift stocks
</td>
<td style="text-align:left;">
gold falls on improved risk sentiment, dollar recovery
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
</tr>
</tbody>
</table>

</div>

## Export the Data

Save the data into excel files

``` r
write.xlsx(combined_data, "./data/gold_prices_with_news.xlsx")
```
