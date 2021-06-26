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
  theme_wsj() + 
  scale_colour_wsj("colors6")+
  theme(axis.text.x=element_text(size=rel(1.0)))+
  ggtitle("Correlation between IBU and ABV ") +
  labs(x="IBU",y="ABV")

head(Brew_beers)
#8
drinkKNN <- dplyr::filter(Brew_beers, Style == "IPA" | Style == "Ale")

iterations = 100
numks = 25
splitPerc = .70
set.seed(33)

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  accs = data.frame(accuracy = numeric(30), k = numeric(30))
  trainIndices = sample(1:dim(drinkKNN)[1],round(splitPerc * dim(drinkKNN)[1]))
  train = drinkKNN[trainIndices,]
  test = drinkKNN[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = class::knn(train[,c(1,3)],test[,c(1,3)],train$Style, prob = TRUE, k = i)
    table(classifications,test$Style)
    CM = confusionMatrix(table(classifications,test$Style))
    masterAcc[j,i] = CM$overall[1]
  }
}

MeanAcc = colMeans(masterAcc)
# Visually find the best value of k by using it's location in the dataframe based on the highest Mean value
plot(seq(1,numks,1),MeanAcc, type = "l", 
     col = "#c8201e",
     main = "Value for K Neighbors vs Accuracy", 
     sub = "Budweiser Consultation",
     xlab = "Value of K Neighbors",
     ylab = "Accuracy Rate (Percentage)")


#9

drinkKNN %>% 
  ggplot(aes(x = IBU, y = ABV, color = IPAAle)) +
  geom_point(show.legend = TRUE, na.rm = TRUE) +
  geom_abline(intercept =  comparisonCoef[1] , slope = comparisonCoef[2], color = "red", size = 1) +
  theme_classic() + 
  labs(title = "IBU vs ABV", 
       subtitle = "Budweiser Consultation",
       y = "Alcohol By Volume", 
       x = "Int'l Bitterness Unit",
       caption="ABV and IBU values imputed where necessary.") +
  scale_color_manual(values = c("red","blue","#green"),
                     name = "Type of Beer",
                     breaks = c("Ale", "IPA", "Neither"),
                     labels = c("Ale", "IPA", "Neither"))