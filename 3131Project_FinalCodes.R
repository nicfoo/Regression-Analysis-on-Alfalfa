#Reading the dataset, converting yes/no categorical data to 1s and 0s

table <- read.table("http://www.statsci.org/data/general/landrent.txt", header = T)



table$Liming <- factor(table$Liming, levels=c("Yes","No"), labels=c(1,0))
table$Liming <- as.character(table$Liming)
table$Liming <- as.numeric(table$Liming)

Model1 <- lm(table$Rent ~ table$AllRent + table$Cows + table$Pasture + table$Liming)
summary(Model1)
vif(Model1)
colldiag(Model1)

plot(Model1)

#Looking at residuals vs fitted values
res <- Model1$residuals
fv <- Model1$fitted.values
plot(fv,res,xlab = "Fitted Values", ylab = "Residuals", main = "Plot of Residuals against the Fitted values")
abline(h=0,lty=2)

nullmodel <- lm(table$Rent ~ 1, data = table)
fullmodel <- lm(table$Rent ~ ., data = table)

#Stepwise based on AIC
step(nullmodel,data=table, scope=list(upper= fullmodel,lower=nullmodel),direction = "both", k = 2, test = "F")

#From AIC: Only keep Allrent & Cows only AllRent: 0.9214 Cows:0.3925

#Stepwise based on mallows Cp
step(fullmodel,data=table,scope = list(upper=fullmodel,lower=nullmodel),direction = "both",scale = (summary(fullmodel)$sigma)^2,k=2,test = "F")
#From Mallows CP keep AllRent & Cows only AllRent: 0.9214 Cows: 0.3925

#Stepwise regression step by step Partial F-test
cor(table2) 
#R= 0.878 for Allrent is highest so pick that first
#decide that 10% inclusion and 5% stay
modelAllrent <- lm(table2$Rent ~ table2$AllRent)
summary(modelAllrent)
modelAllrentCows = lm(table2$Rent ~ table2$AllRent + table2$Cows)
modelAllrentPasture = lm(table2$Rent ~ table2$AllRent + table2$Pasture)
modelAllrentLiming = lm(table2$Rent ~ table2$AllRent + table2$Liming)
anova(modelAllrent,modelAllrentCows)
anova(modelAllrent,modelAllrentPasture)
anova(modelAllrent,modelAllrentLiming)
#find the maximum F stat out of all of them -> choose that predictor
#now check if AllRent can be removed from the model with ALlRent and Cows
modelCows = lm(table$Rent ~ table$Cows)
anova(modelCows,modelAllrentCows)
#now check if pasture or liming can be included in the model w Allrent and cows
modelAllrentCowsPasture = lm(table2$Rent ~ table2$AllRent + table2$Cows + table2$Pasture)
modelAllrentCowsLiming = lm(table2$Rent ~ table2$AllRent + table2$Cows + table2$Liming)
anova(modelAllrentCows,modelAllrentCowsLiming)
anova(modelAllrentCows,modelAllrentCowsPasture)
#both cant be included by the 10% inclusion -> final fitted model is table$Rent ~ table$Cows + table$AllRent

#updated model is follows
Model2 <- lm(table$Rent ~ table$Cows + table$AllRent)
summary(Model2)
residuals <- Model2$residuals
plot(residuals~table$AllRent + table$Cows, main = "Plot of Residuals against Predictors")

#Rsquare : 0.8379 still relatively high

#rentAllrent <- lm(table$Rent ~ table$Pasture)
#res <- rentAllrent$residuals
#fv <- rentAllrent$fitted.values
#plot(fv,res,xlab = "Fitted Values", ylab = "Residuals", main = "Plot of Residuals against the Fitted values")
#abline(h=0,lty=2)

sapply(table,class)
table$Rent
#Looking at residuals vs fitted values
residuals <- Model2$residuals
fv <- Model2$fitted.values
plot(fv,res,xlab = "Fitted Values", ylab = "Residuals", main = "Plot of Residuals against the Fitted values")
abline(h=0,lty=2)
#Evidently, it is pattern 1(funnel shape) and that variance isnt constant. A transformation on the response(Rent) is needed
plot(Model2)

#boxcox transformation
library(MASS)
modelbc = boxcox(Model2,lambda = seq(-1,1,0.01))
modelbc$x[modelbc$y==max(modelbc$y)] #0.46, lambda approx 0.5


#since lambda approximately 0.5, a square root to the response variable would be needed
Model3<- lm(table$Rent ^ 0.5 ~ table$Cows + table$AllRent )
summary(Model3)

#R square is now 0.8685
res <- newfittedmodel$residuals
fv <- newfittedmodel$fitted.values
plot(fv,res,xlab = "Fitted Values", ylab = "Residuals", main = "Plot of Residuals against the Fitted values")
#evidently, residuals against fitted values, values are more random and evenly spaced with no apparent shape or pattern

#trying out quadratic terms (square to predictors)
cows2 <- table$Cows^2
AllRent2 <- table$AllRent^2
nullmodel <- lm(table$Rent^0.5 ~ 1, data = table)
fullmodel <- lm(table$Rent^0.5 ~ table$AllRent + table$Cows + cows2 + AllRent2 )


#stepwise based on AIC
step(nullmodel,data=table2, scope=list(upper= fullmodel,lower=nullmodel),direction = "both", k = 2, test = "F")
#final result says that we should keep both cows2 and Allrent2

#Stepwise based on Mallows C_p
step(fullmodel,data=table,scope = list(upper=fullmodel,lower=nullmodel),direction = "both",scale = (summary(fullmodel)$sigma)^2,k=2,test = "F")
#final results also say that we should keep both cows2 and allrent2

library(leaps)
x<- model.matrix(fullmodel)[,-1]
allcp <- leaps(x,table$Rent,method = "Cp")
for(i in 2:5){
  mincp <- min(allcp$Cp[allcp$size==i])
  whichmodel <- allcp$which[allcp$Cp==mincp,]
  namemodel <-names(whichmodel)[whichmodel==T]
  cat(namemodel,"\n",mincp,"\n")
}
#based on the output stopping at cp=3.50 for 1 2 3 would be the closest to number of parameters = 4

#Stepwise regression Partial F-test
#10% entry 5% stay
fullmodel <- lm(table$Rent^0.5 ~ table$AllRent + table$Cows + cows2 + AllRent2 )
#AR - 1 , C -2 , AR2 - 3 , C2 - 4
model123 <- lm(table$Rent^0. ~ table$AllRent + table$Cows + AllRent2)
model134 <- lm(table$Rent^0.5 ~ table$AllRent + cows2 + AllRent2)
model124 <- lm(table$Rent^0.5 ~ table$AllRent + table2$Cows + cows2)
model234 <- lm(table$Rent^0.5 ~ table$Cows + cows2 + AllRent2)
#10% entry 5% stay
anova(model123,fullmodel)
anova(model134,fullmodel)
anova(model124,fullmodel)
anova(model234,fullmodel)
#look at minimum F stat -> take out AllRent2 (3)
#model124 chosen , now see if can take out anymore?
model24 <- lm(table2$Rent^0.5 ~  table2$Cows + cows2)
model14 <- lm(table2$Rent^0.5 ~ table2$AllRent +  cows2)
model12 <- lm(table2$Rent^0.5 ~ table2$AllRent + table2$Cows)
anova(model24,model124)
anova(model14,model124)
anova(model12,model124)
#conclusion: final model still only requires Allrent , cows and cows2

#newest model is as follows:
Model4 <- lm(table$Rent^0.5 ~ table$AllRent + table$Cows + cows2)
summary(Model4)
plot(Model4)
residuals <- Model4$residuals
plot(residuals~table$AllRent + table$Cows + cows2)


residuals <- newestmodel$residuals
plot(residuals ~ table$AllRent + table$Cows + cows2)
#Rsquare = 0.8784
#Looking at residuals vs fitted values
res <- newestmodel$residuals
fv <- newestmodel$fitted.values
plot(fv,res,xlab = "Fitted Values", ylab = "Residuals", main = "Plot of Residuals against the Fitted values")
abline(h=0,lty=2)

#test interaction effects
interactionmodel <- lm(table$Rent^0.5 ~ table$AllRent + table$Cows + cows2 +table$AllRent*table$Cows)
summary(interactionmodel)
anova(interactionmodel,newestmodel)
#p value shows not relevant

#checking for outliers
library(car)
influence.measures(newestmodel)
rstudent(Model4)

#(p+1)/n = 2+1/67 = 0.044776
#dFFITS = 2([p+1]/[n-p-1])^0.5 = 0.433
#DFBETAS = 2/n^0.5 = 0.24433888 
#So look at hii, RSTUDENT , DFBETAS for B0 B1 B2 B3 B4 & dffits






