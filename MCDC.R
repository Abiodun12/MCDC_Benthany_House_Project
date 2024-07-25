setwd("/Users/someoddsense/Desktop/MCDC")
mydata<- read.csv("~/Desktop/MCDC/BHR(R).csv", na.strings = "n/a", head=TRUE)

names(mydata)

mydata[, 'Age.at.move.in']<- as.numeric(mydata[,'Age.at.move.in'])
mydata[, 'Age']<- as.numeric(mydata[,'Age'])
mydata[, 'Length.of.Stay....of.days.']<- as.numeric(mydata[,'Length.of.Stay....of.days.'])
mydata[, 'Children']<- as.numeric(mydata[,'Children'])

library(MASS)

library(logspline)

library(fitdistrplus)



descdist(mydata$Age, discrete = FALSE)

fit.norm<- fitdist(mydata$Age, "norm")

fit.gamma<-fitdist(mydata$willing_pay, "gamma", method="mme")

fit.ln<-fitdist(mydata$willing_pay, "lnorm")

plot(fit.norm)

plot(fit.gamma)

plot(fit.ln)



fit.norm$aic

fit.gamma$aic

fit.ln$aic

# testing for appropriate fit for general linear model for care_eco
descdist(mydata$care_eco, discrete = FALSE)

fit.norm<- fitdist(mydata$care_eco, "norm")

fit.gamma<-fitdist(mydata$care_eco, "gamma", method="mme")

fit.ln<-fitdist(mydata$care_eco, "lnorm")

plot(fit.norm)

plot(fit.gamma)

plot(fit.ln)



fit.norm$aic

fit.gamma$aic

fit.ln$aic

# heck yeah its normal

# generalized linear model for care_eco
library(glmmTMB)
glm.null <- glmmTMB(Age ~ 1, data = mydata, family = gaussian())
summary(glm.null)
#AIC 159.2

# glm full
library(glmmTMB)
glm.full <- glmmTMB(care_eco ~ willing_pay + buy_need + likely_donate, data = mydata, family = gaussian())
summary(glm.full)
#AIC 166.7

# glm working
library(glmmTMB)
glm.working <- glmmTMB(care_eco ~ price_imp + eco_imp + likely_trash, data = mydata, family = gaussian())
summary(glm.working)
#AIC 112.6

# glm more_need, eco_imp
library(glmmTMB)
glm.2 <- glmmTMB(care_eco ~ more_need + eco_imp, data = mydata, family = gaussian())
summary(glm.2)
#AIC 109.9

# glm perc_second, eco_imp
library(glmmTMB)
glm.3 <- glmmTMB(care_eco ~ perc_second + eco_imp, data = mydata, family = gaussian())
summary(glm.3)
#AIC 101.0

# glm shein, eco-imp
library(glmmTMB)
glm.4 <- glmmTMB(care_eco ~ shein + eco_imp, data = mydata, family = gaussian())
summary(glm.4)
#AIC 103.2

# glm brand & percent second
library(glmmTMB)
glm.5 <- glmmTMB(care_eco ~ brand_imp + perc_second, data = mydata, family = gaussian())
summary(glm.5)
#AIC 103.3

# glm num owned, eco-imp
library(glmmTMB)
glm.6 <- glmmTMB(care_eco ~ num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.6)
#AIC 98.1

# glm age, num owned, eco imp
library(glmmTMB)
glm.7 <- glmmTMB(care_eco ~ age + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.7)
#AIC 94.9

# glm more_need, num owned, eco imp
library(glmmTMB)
glm.8 <- glmmTMB(care_eco ~ more_need + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.8)
#AIC 94.6

# glm num wash, num owned, eco imp
library(glmmTMB)
glm.9 <- glmmTMB(care_eco ~ num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.9)
#AIC 81.3

# glm fit imp, num wash, num owned, eco imp
library(glmmTMB)
glm.10 <- glmmTMB(care_eco ~ fit_imp + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.10)
#AIC 79.6

# glm more cloth, num wash, num owned, eco imp
library(glmmTMB)
glm.11 <- glmmTMB(care_eco ~ more_cloth + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.11)
#AIC 76.8

# glm more need, num wash, num owned, eco imp
library(glmmTMB)
glm.11 <- glmmTMB(care_eco ~ more_need + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.11)
#AIC 74.4

# glm self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.12 <- glmmTMB(care_eco ~ self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.12)
#AIC 66.7

# glm gender, self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.13 <- glmmTMB(care_eco ~ gender + self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.13)
#AIC 65.9

# glm gender, self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.14 <- glmmTMB(care_eco ~ dispose_style + self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.14)
#AIC 64.9

# glm perc_second, self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.15 <- glmmTMB(care_eco ~ perc_second + self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.15)
#AIC 57.8
#might be new model, 72.9

# glm gender, perc_second, self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.16 <- glmmTMB(care_eco ~ gender + perc_second + self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.16)
#AIC 56.3

# glm buy_new, perc_second, self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.17 <- glmmTMB(care_eco ~ buy_new + perc_second + self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.17)
#AIC 46.8
#WINNER !!! adding more variables makes the model worse



#box plot attempts buy_new:
plot(care_eco ~ buy_new, data = mydata,
     xlab = "How Often do People Buy New Clothes",
     ylab = "How Much do People Care About Eco-Fashion")
#it looks like everyone relatively cares abut eco fashion, with 4 being most

#box plot attempts perc_second
plot(care_eco ~ self_mend, data = mydata,
     xlab = "How Willing are People to Mend Their Clothes",
     ylab = "How Much do People Care About Eco-Fashion")
#people who are more willing care more about eco-fashion

# correlation matrix
cor(mydata, method = "pearson")
#Error in cor(mydata, method = "pearson") : 'x' must be numeric

#google solution 1
library(ggcorrplot)
model.matrix(~0+., data=mydata) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
# contrasts can be applied only to factors with 2 or more levels


# NUMERIC only correlation
age<- mydata$age
price_imp <- mydata$price_imp
qual_imp <- mydata$qual_imp
brand_imp <- mydata$brand_imp
eco_imp<- mydata$eco_imp
fit_imp <- mydata$fit_imp
num_owned <- mydata$num_owned
num_wash <- mydata$num_wash
perc_second <- mydata$perc_second


numdat<- data.frame(age,price_imp,qual_imp,brand_imp,eco_imp,fit_imp,num_owned,num_wash,perc_second)
cor(numdat, method = "pearson")
# hmm didnt really work

myvars<- c("age","price_imp","qual_imp","brand_imp","eco_imp","fit_imp","num_owned","num_wash","perc_second")
newdata <- mydata[myvars]
cor(newdata, method = "pearson")
#keep getting na for all correlation

#correlation w klimas:
numdat2<- data.frame(mydata$age,mydata$num_owned,mydata$num_wash,mydata$perc_second)
cor(numdat2, method = "spearman")

# instead, graph numeric variables with scatter plots
plot(mydata$age,mydata$num_owned)
# no relationship
plot(mydata$age,mydata$num_wash)
# no relationship
plot(mydata$age,mydata$perc_second)
#some correlation
# need to add line to
plot(mydata$num_owned,mydata$num_wash)
# no correlation
plot(mydata$num_owned,mydata$perc_second)
# no correlation
plot(mydata$num_wash,mydata$perc_second)


#willing to pay histogram:

hist(mydata$willing_pay)
# Add titles
hist(mydata$willing_pay,
     main="Willingness to Pay More for Eco-Friendly Clothes",
     breaks = 6,
     xlab="% more willing to pay",
     ylab="Respondants")

ggplot(mydata, aes(x = willing_pay)) +
  geom_histogram(
    color = "white", fill = "black",
    binwidth = 1)+
  labs(x ='% More Willing to Pay', y='Number of Respondants', title = 'Willingness to Pay More for Eco-Friendly Clothes')

#willing to pay histogram with %s

barplot(prop.table(mydata$willing_pay2)) # this doesnt work because "Error in Summary.factor(c(1L, 3L, 1L, 1L, 3L, 3L, 4L, 2L, 1L, 5L, 1L,  : ‘sum’ not meaningful for factors" 

library(ggplot2)
# Most basic bar chart
ggplot(mydata, aes(x = factor(willing_pay2))) +
  geom_bar(color = "white", fill = "black")+  #YAY this worked !!
  labs(x ='% more willing to pay', y='Number of Respondants', title = 'Willingness to Pay more for Eco - Friendly Clothes')

# Add titles

hist(mydata$willing_pay2,
     main="Willingness to Pay more for Eco - Friendly Clothes",
     xlab="% more willing to pay",
     ylab="Respondants")
#this also doesnt work bc x values are not numeric

# care eco histogram

hist(mydata$care_eco)
# Add titles
hist(mydata$care_eco,
     main="How Much Respondants Care About Eco-Fashion",
     breaks = 6,
     xlab="Likert Scale of Caring",
     ylab="Respondants")

#This one works good !!!
ggplot(mydata, aes(x = care_eco)) +
  geom_histogram(
    color = "white", fill = "black",
    binwidth = 1)+
  labs(x ='Likert Scale of Caring', y='Number of Respondants', title = 'How Much Respondants Care About Eco-Fashion')

# perc_second histogram

ggplot(mydata, aes(x = perc_second)) +
  geom_histogram(
    bins = 100,
    color = "white", fill = "black")+
  labs(x ='% of Clothes', y='Number of Respondants', title = '% of Owned Clothes Bought 2nd Hand')
# doesn't work, maybe bc its up to 100?

hist(mydata$perc_second)
# Add titles
hist(mydata$perc_second,
     main="% of Clothes Bought 2nd Hand",
     xlab="% of Clothes",
     ylab="Respondants")
#idk why this only goes to 25 %

#scatter plot of perc_second and willing_pay

ggplot(mydata, aes(x=willing_pay, y=perc_second)) + geom_point()
#still don't know why this only goes up to 25

#linear regression attempts

lmCare = lm(mydata$care_eco~mydata$willing_pay, data = mydata)
summary(lmCare)

plot(mydata$care_eco + mydata$willing_pay, pch = 16, col = "blue") #Plot the results
abline(lmCare) #Add a regression line

lmCare = lm(mydata$care_eco~mydata$dispose_style, data = mydata)
summary(lmCare)

# Adjustments to code based on Klimas recomendations on 4/28

mydata[, 'buy_new']<- as.numeric(mydata[,'buy_new'])
mydata[, 'self_mend']<- as.numeric(mydata[,'self_mend'])

# glm buy_new, perc_second, self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.17 <- glmmTMB(care_eco ~ buy_new + perc_second + self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.17)
#AIC 73.3

library(glmmTMB)
glm.null <- glmmTMB(care_eco ~ 1, data = mydata, family = gaussian())
summary(glm.null)
#AIC 159.2

# glm perc_second, self mend, num wash, num owned, eco imp
library(glmmTMB)
glm.15 <- glmmTMB(care_eco ~ perc_second + self_mend + num_wash + num_owned + eco_imp, data = mydata, family = gaussian())
summary(glm.15)
#new model, 72.9

#box plot attempts buy_new:
boxplot(care_eco ~ self_mend, data = mydata,
        xlab = "How Willing are People to Mend Clothes",
        ylab = "How Much do People Care About Eco-Fashion")
#it looks like everyone relatively cares abut eco fashion, with 4 being most


