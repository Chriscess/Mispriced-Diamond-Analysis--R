###Load packages
library(tidyverse)
library(readr)
library(ggplot2)

###Load dataset
Mispriced_Diamonds<- read.csv("Mispriced-Diamonds.csv")
str(Mispriced_Diamonds)
head(Mispriced_Diamonds,10)
#### Summary statistics
Mispriced_Diamonds  %>%
  summarise(mean = mean(price),
            std = sd(price),
            median = median(price),
            minimum = min(price),
            maximum = max(price))

##### Summary statistics of carat
Mispriced_Diamonds  %>%
  summarise(mean = mean(carat),
            std = sd(carat),
            median = median(carat),
            minimum = min(carat),
            maximum = max(carat))

#### Summary of each clarity group.
Mispriced_Diamonds  %>%
  group_by (clarity) %>% 
  summarise(mean = mean(price),
            std = sd(price),
            median = median(price),
            minimum = min(price),
            maximum = max(price),
            count= n())

#### Box plot showing Diamond and clarity
ggplot(Mispriced_Diamonds, aes(x=reorder(clarity, price), y= price, fill = clarity))+
  geom_boxplot(show.legend = FALSE)+
  labs(x = "clarity",
       y = "price",
       title = "Diamonds Prices in relation to their Clarity",
       subtitile = "The outliers are clearly stated to show the variance in prices")+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 17,hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold"))

##### Scatter plot explaning carat in relation to price
ggplot(Mispriced_Diamonds, aes( x= carat, y = price))+
  geom_point(aes(colour = clarity), size = 1)+
  labs(x = "Carat",
       y = "Price",
       title = "Relationship Between Carat and Price")+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 17,hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold"))

ggplot(Mispriced_Diamonds, aes( x= carat, y = price))+
  geom_point(aes(colour = clarity), size = 0.5)+
  labs(x = "Carat",
       y = "Price",
       title = "Relationship Between Carat and Price")+
  facet_wrap(~clarity)

##### Histogram showing Carat Frequency
hist(Mispriced_Diamonds$carat,
     main= " Histogram Showing Carat",
     xlab= "Carat",
     ylab = "Frequency",
     col = "pink")

##### Barchat showing Clarity 
ggplot(Mispriced_Diamonds, aes(x=clarity, fill = clarity))+
  geom_bar(show.legend =FALSE )+
  labs(x = "Clarity", desc("clarity"),
       y = "Count",
       title = "Barchart Showing Clarity Count")+
  theme_minimal()+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 17,hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "bold"))
