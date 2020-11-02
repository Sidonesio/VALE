
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

# import data sets
vale1 <- getSymbols("VALE", src = "yahoo", from = "2010-01-01", 
                    to = "2020-10-31", auto.assign = FALSE)
ibov1 <- getSymbols("^BVSP", src = "yahoo", from = "2010-01-01", 
                    to = "2020-10-31", auto.assign = FALSE)
iron1 <- Quandl("CHRIS/CME_TIO1")
nickel1 <- Quandl("CHRIS/MCX_NI1")
copper1 <- Quandl("CHRIS/CME_HG11")
coal1 <- Quandl("HKEX/01898")

# prepare data set "vale"
vale2 <- data.frame(date=index(vale1), coredata(vale1)) # change xts to dataframe
vale3 <- select(vale2, c("date", "VALE.Adjusted")) # select columns
colnames(vale3)[2] <- "vale" # change column name

# prepare data set "ibovespa"
ibov2 <- data.frame(date=index(ibov1), coredata(ibov1)) # change xts to dataframe
ibov3 <- select(ibov2, c("date", "BVSP.Adjusted")) # select columns
colnames(ibov3)[2] <- "ibov" # change column name

# prepare data set "iron"
iron2 <- select(iron1, c("Date", "Settle")) # select columns
colnames(iron2)[1] <- "date" # change column name
colnames(iron2)[2] <- "iron" # change column name

# prepare data set "nickel"
nickel2 <- select(nickel1, c("Date", "Close")) # select columns
colnames(nickel2)[1] <- "date" # change column name
colnames(nickel2)[2] <- "nickel" # change column name

# prepare data set "coal"
coal2 <- select(coal1, c("Date", "Nominal Price"))
colnames(coal2)[1] <- "date" # change column name
colnames(coal2)[2] <- "coal" # change column name

# merge data sets
first <- merge(vale3, iron2, by = "date")
second <- merge(first, nickel2, by = "date")
third <- merge(second, coal2, by = "date")
last1 <- merge(third, ibov3, by = "date")

# exclude rows with NAs
last2 <- last1[complete.cases(last1),] 

# linear model regression
linear <- lm(vale ~ iron + nickel + coal, data = last2)
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

# quantile model (gasto full)
quant <- rq(vale ~ iron + nickel + coal, data = last2, tau = 0.50)
summary(quant, se = "nid")
summary(quant)
summ(quant, confint = TRUE)
summquant <- tidy(quant)
write.xlsx(summquant, file = "summquant.xlsx")

# add quantile regression residuals to dataframe
last2$residuals <- quant$residuals

# check if vale beated ibovespa or not
test <- filter(last2, date <= "2019-07-29") # subset dataframe
test$valereturn <- (11.74/test$vale) - 1 # calculate vale return
test$ibovreturn <- (104109/test$ibov) - 1 # calculate ibovespa return
test$beat <- ifelse(test$valereturn > test$ibovreturn, "yes", "no") # compare both returns
test$beat <- as.factor(test$beat) # set beat variable as factor

# plot histogram by beat
qplot(residuals, data = test, geom = "density", color = beat)
