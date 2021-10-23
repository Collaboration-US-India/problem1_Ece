setwd('C:/Users/bakinero/Desktop/Data Mining')

install.packages("usmap")


library(tidyverse)
library(dplyr)
library(janitor)
library(data.table)
library(usmap)
library(ggplot2)


p1 <- read.csv("patents_10.csv", header = TRUE, na.strings = '..')

## finding out the variable type
str(p1)

#renaming variables and using the ones relative to my EDA

df <- p1[1:29597, -c(1:3,5,6,8,10:11 )] %>%  
  rename(type = ptype,                        
         coname = ee_name,
         cotype = ee_role_desc,
         country = ee_country,
         state = ee_state,
         city = ee_city) %>%
  drop_na()

##finding out the variables
head(df)

View(df)

#finding out how many different type of patents are in the dataset
table(df$type, df$country)

# Top 10 countries that has the most patents
df %>%
  group_by(country)%>%
  summarize(n=n()) %>%
  top_n(n = 10) %>%
  arrange(n)

#creating a new column will tell us the difference between applied patent year and granted patent year
df$diffyear <- df$grantyear - df$applyear

#The average wait year for patents
round(mean(df$diffyear), digits = 1)

#only Unites States Data
us_data <- df[df$country == "US",]

#showing the table with country and difference between applied year and grant year and finding out the average of US
table(us_data$country, us_data$diffyear)
mean(us_data$diffyear, digits = 1)

#showing type of patent state by state
table(us_data$state, us_data$type)

#Top 10 states in US applied for patents
us_table <- us_data %>%
            group_by(state)%>%
            summarize(n=n()) %>%
            top_n(n = 5)

#selecting the top 5 states that applied for grants
us_table <- us_data[us_data$state =="CA" |
                      us_data$state == "MA" |
                      us_data$state == "MI" |
                      us_data$state == "NY" |
                      us_data$state == "TX" | 
                      us_data$state == "WA" ]


#Bar chart
ggplot(data=us_table, aes_string(y= 'state'))+
  geom_bar( color="blue", fill="LIGHTBLUE") +
  ggtitle("Number of Patents by State", ) + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("Number of Patents") +ylab("States") + 
  coord_flip() +
  theme(legend.position = "top")