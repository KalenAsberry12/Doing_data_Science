library(naniar)
library(tidyverse)
library(ggplot2)
library(GGally)
Library(dplyr)

## Calling Data
beers = read.csv(file.choose(),header = TRUE)

breweries = read.csv(file.choose(),header = TRUE)

##Summary of beers
summary(beers)

##Summary of breweries
summary(breweries)

## Analysis 1 
Breweachstate <-table(breweries$State)
Breweachstate


head(beers)
head(breweries)
## Merge both CSV togther
Brew_beers <- merge(x=beers, y=breweries, by.x = "Brewery_id", by.y = "Brew_ID")

## Rename
Brew_beers <- rename(Brew_beers, Beer = Name.x, Brewery = Name.y,
                  OZ = Ounces)
head(Brew_beers)



MissingValues <- sapply(Brew_beers, function(x)sum(is.na(x)))
MissingValues


# 4. Compute the median alcohol content unit for each state.
okay_beer <- Brew_beers %>%
  na.omit() %>%
  group_by(State) %>%
  summarise(Median = median(ABV)) %>%
  arrange(Median)

okay_beer


# 4. a Compute the median international bitterness unit for each state.
Bitterness <- Brew_beers %>%
  na.omit() %>%
  group_by(State) %>%
  summarise(Median = median(IBU)) %>%
  arrange(Median)


Bitterness


# 4. b Plot a bar chart to compare ABV by state
library(ggthemes)
ggplot(data=okay_beer, aes(x=State, y=Median)) +
  geom_bar(stat="identity", fill="blue")+
  theme_economist() + 
  scale_color_economist()+
  theme(axis.text.x=element_text(size=rel(0.8), angle=90)) +
  ggtitle("Median ABV by State") +
  labs(x="State",y="ABV")



# 4. c Plot a bar chart to compare IBU by state
ggplot(data=Bitterness, aes(x=State, y=Median)) +
  geom_bar(stat="identity", fill="blue")+
  theme_economist() + 
  scale_color_economist()+
  theme(axis.text.x=element_text(size=rel(0.8), angle=90))+
  ggtitle("Median IBU by State") +
  labs(x="State",y="IBU")




##MAX ABV AND IBU
Brew_beers[which.max(Brew_beers$ABV),]
Brew_beers[which.max(Brew_beers$IBU),]


##SUMMARY
BeerSummary <- (summary(Brew_beers$ABV))
print(BeerSummary)


# 7. Draw a scatter plot to compare relationship between beer 
# bitterness and alcohol content
ggplot(Brew_beers, aes(x=IBU, y= ABV)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) + # add linear regression line
  theme_economist() + 
  scale_color_economist()+
  theme(axis.text.x=element_text(size=rel(1.0)))+
  ggtitle("Correlation between IBU and ABV ") +
  labs(x="IBU",y="ABV")




