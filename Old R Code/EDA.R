library(dplyr)
library(ggplot2)
library(xlsx)
library(lubridate)
library(yarrr)
library(readxl)
library(RColorBrewer)

data <- read.xlsx("Gold Prices With Headline.xlsx", 1)
data <- data[1:1005,]

########### Create Time Series Plot
data %>%
  ggplot(aes(x=Date,y=Closing.Price))+
  geom_line(color = "#2C3E50")+
  labs(y="Closing Price", 
       x="",
       title = "Gold Closing Price") + 
  theme_tq()



############## Create Time Series Plot by 4 Month Break
data %>%
  ggplot(aes(x=Date,y=Closing.Price))+
  geom_line(color = palette_light()[[1]])+
  labs(y="Closing Price\n", 
       x="",
       title = "Gold Closing Price") + 
  scale_x_date(date_breaks = "4 month", date_labels = "%b %Y") +
  theme_tq()

ggsave("timeseries.png")



###################Create Pirate Plot for Gold Price Distribution ###################################33

data <- data %>% mutate(month_year = format_ISO8601(Date, precision = "ym")) ####### Create a new column to store the motnh

op <- par(mar = c(5,7,4,2) + 0.1) ##### For Visual Purpose

########## png("gold_price_distribution.png", width = 1232, height = 600, res = 300)

pirateplot(formula = Closing.Price ~ month_year, 
           data = data,
           xaxt = "n",
           ylab = "Closing Price\n",
           xlab = "",
           main = "Gold Price Distribution")   #### Create the Gold Price Distribution Plot without the x label

######### Create custom x label for visual purpose
xlabels <- unique(data$month_year)
xlabels <- ym(xlabels) %>% format('%b %Y')
xlabels_fix <- xlabels[seq(2, length(xlabels), 4)]
axis(1, at = seq(2, length(xlabels), 4), xlabels_fix)

########### dev.off()  
