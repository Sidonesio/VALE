
# load packages
library(quantmod)
library(Quandl)
library(dplyr)
library(ggpubr)
library(car)
library(olsrr)
library(nortest)
library(quantreg)
library(jtools)
library(xlsx)
library(ggplot2)
library(lubridate)
library(Hmisc)

# where data came from
# coal + nickel: markets.businessinsider
# vale: yahoo br
# copper: yahoo
# dolar + iron: brinvesting

# read / import data sets
vale1 <- read.csv("yahoobr.csv")
dolar1 <- read.csv("dolar.csv")
iron1 <- read.csv("iron.csv")
copper1 <- read.csv("copper.csv")
nickel1 <- read.csv("nickel.csv")
coal1 <- read.csv("coal.csv")
ibov1 <- getSymbols("^BVSP", src = "yahoo", from = "2010-01-01", 
                    to = "2020-10-31", auto.assign = FALSE)

# prepare data set "vale"
vale2 <- select(vale1, c("Date", "Adj.Close")) # select columns
colnames(vale2)[1] <- "date" # change column name
colnames(vale2)[2] <- "vale" # change column name
vale2$date <- as.Date(vale2$date) # set first column as date

# prepare data set "dolar"
dolar2 <- select(dolar1, 1:2) # select columns
colnames(dolar2)[1] <- "date" # change column name
colnames(dolar2)[2] <- "dolar" # change column name
dolar2$date <- dmy(dolar2$date) # set first column as date

# prepare data set "iron"
iron2 <- select(iron1, 1:2) # select columns
colnames(iron2)[1] <- "date" # change column name
colnames(iron2)[2] <- "iron" # change column name
iron2$date <- dmy(iron2$date) # set first column as date

# prepare data set "copper"
copper2 <- select(copper1, c("Date", "Adj.Close")) # select columns
colnames(copper2)[1] <- "date" # change column name
colnames(copper2)[2] <- "copper" # change column name
copper2$date <- as.Date(copper2$date) # set first column as date

# prepare data set "nickel"
nickel2 <- select(nickel1, c("Date", "Close")) # select columns
colnames(nickel2)[1] <- "date" # change column name
colnames(nickel2)[2] <- "nickel" # change column name
nickel2$date <- mdy(nickel2$date) # set first column as date

# prepare data set "coal"
coal2 <- select(coal1, c("Date", "Close")) # select columns
colnames(coal2)[1] <- "date" # change column name
colnames(coal2)[2] <- "coal" # change column name
coal2$date <- mdy(coal2$date) # set first column as date

# prepare data set "ibovespa"
ibov1 <- data.frame(date=index(ibov1), coredata(ibov1)) # change xts to dataframe
ibov2 <- select(ibov1, c("date", "BVSP.Adjusted")) # select columns
colnames(ibov2)[2] <- "ibov" # change column name

# merge data sets
first <- merge(vale2, dolar2, by = "date")
second <- merge(first, iron2, by = "date")
third <- merge(second, copper2, by = "date")
fourth <- merge(third, nickel2, by = "date")
fifth <- merge(fourth, coal2, by = "date")
last1 <- merge(fifth, ibov2, by = "date")

# prepare data set "last"
last1$vale <- as.numeric(last1$vale)
last1$dolar <- as.numeric(gsub(",", ".", gsub("\\.", "", last1$dolar)))
last1$iron <- as.numeric(gsub(",", ".", gsub("\\.", "", last1$iron)))
last1$copper <- as.numeric(last1$copper)
last1$nickel <- as.numeric(last1$nickel)
last1$coal <- as.numeric(last1$coal)

# exclude rows with NAs
last2 <- last1[complete.cases(last1),] 

# simple linear model regression
simple <- lm(vale ~ dolar, data = last2)
simple <- lm(vale ~ iron, data = last2)
simple <- lm(vale ~ copper, data = last2)
simple <- lm(vale ~ nickel, data = last2)
simple <- lm(vale ~ coal, data = last2)
summary(simple)

# multiple linear model regression
linear <- lm(vale ~ dolar + iron + copper + nickel + coal, data = last2)
summary(linear)
summ(linear)
CIlinear <- confint(linear)
summlinear <- tidy(linear)
write.xlsx(summlinear, file = "summlinear.xlsx")
write.xlsx(CIlinear, file = "CIlinear.xlsx")

# inspect graphically linear model assumptions
windows()
par(mfrow=c(2,2))
plot(linear, pch = 20)

# view linear model outliers through Cook’s barplot
ols_plot_cooksd_bar(linear)

# linear model residuals visual inspection
par(mfrow=c(1,1))
hist(linear$residuals) # histogram
plot(density(linear$residuals)) # kernel density plot
ggdensity(linear$residuals) # density plot
ggqqplot(linear$residuals) # Q-Q plot
qqPlot(linear$residuals, pch = 20) # Q-Q plot

# linear model residuals normality test
ad.test(linear$residuals) # Anderson-Darling normality test
shapiro.test(linear$residuals) # Shapiro-Wilk normality test
ks.test(linear$residuals,"pnorm",
        mean(linear$residuals),
        sd(linear$residuals)) #  Kolmogorov-Smirnov normality test

# simple quantile model regression
quant <- rq(vale ~ dolar, data = last2, tau = 0.50)
quant <- rq(vale ~ iron, data = last2, tau = 0.50)
quant <- rq(vale ~ copper, data = last2, tau = 0.50)
quant <- rq(vale ~ nickel, data = last2, tau = 0.50)
quant <- rq(vale ~ coal, data = last2, tau = 0.50)
summ(quant)

# multiple quantile model regression
quant <- rq(vale ~ dolar + iron + copper + nickel + coal, data = last2, 
            tau = 0.50)
summary(quant, se = "nid")
summary(quant)
summ(quant, confint = TRUE)
summquant <- tidy(quant)
write.xlsx(summquant, file = "summquant.xlsx")

# add quantile regression residuals to dataframe
last2$residuals <- quant$residuals

# check if vale beated ibovespa or not
test <- filter(last2, date <= "2019-10-30") # subset dataframe
test$valereturn <- (60.55/test$vale) - 1 # calculate vale return
test$ibovreturn <- (93952/test$ibov) - 1 # calculate ibovespa return
test$beat <- ifelse(test$valereturn > test$ibovreturn, 
                    "yes", "no") # compare both returns
test$beat <- as.factor(test$beat) # set beat variable as factor

# plot histogram by beat
qplot(residuals, data = test, geom = "density", color = beat)

# cut residuals into 7 quantiles and create new variable
test <- mutate(test, residualsgroup1 = cut2(residuals, g = 7))

# probability of beating "ibovespa" by quantile range
prob <- table(test$beat, test$residualsgroup1, exclude = NULL)
round(prop.table(prob, margin = 2), digits = 2)



