#' ---
#' title: "Categorical Data Analysis Pset 5"
#' author: "Aditya Khera"
#' date: "April 11, 2022"
#' ---

library(nnet)

#setting up table
data <- read.csv(file = "/Users/adityakhera/Desktop/Statistics/Categorical Data/slay.csv")
data$Group <- factor(data$Group, levels = c("Exposure Prone", "Fluid Contact", "Lab Staff", "Patient Contact", "No Patient Contact"))
data$Status <- factor(data$Status, levels = c("Yes", "No"))
data1 <- xtabs(Count ~ Group + Status, data = data)
data1

#estimate of group one postivity rate: we estimate the chance of having hepatitis in 
#occupational group one is 0.002267574
5/2205

rowtot <- apply(data1, 1, sum)
coltot <- apply(data1, 2, sum)
total <- sum(data1)
observed <- data1
expected <- rowtot %o% coltot / total

expected
expected[1,1]/sum(expected[1,])
#assuming the null hypothesis were true, we estimate the probability of hepatitis C 
#amoung occupational group one to be 0.002815844

rm(rowtot, coltot, total)

set1 <- data.frame(data1)
set2 <- set1[rep(1:nrow(set1), times = set1$Freq), -3]
row.names(set2) <- 1:nrow(set2)
set2.star <- data.frame(group = set2[,1], status = sample(set2[,2]))
permute <- xtabs(~ group + status, data = set2.star)
permute

rm(set1, set2)

ind.test <- chisq.test(data1, correct = F)
ind.test
#Chi-square test statistic is 4.5043, df = 4, and p-value is .342

LRT <- observed* log(observed/expected)
LRT <- 2*sum(LRT)
LRT
pchisq(3.735, 4, lower.tail=FALSE)
#LRT statistics is 3.7350, df = 4, and p-value is .44305
#We fail to reject the null hypothesis according to both tests, there is sufficient 
#evidence backing up the probability of hepatitis c is independent of occuptational group.

ind.test$stdres
#There don't seem to be any abmoral values in the standardized residuals table. 
#This makes sense since we fail to reject the null, so values in the standardized 
#residual table should be between -2 and 2. 

set.seed(414)
chisq.test(data1, correct=F, simulate.p.value = T, B=2000)
#The chi square test over our permuted dataset is not too different from the actual data.
#Both have p-vales that hover around .32 and indicate our null hypothesis is true.
#According to our findings, the chance of hepatitis C is equal amoung occupational groups. 



Gators <- "http://users.stat.ufl.edu/~aa/cat/data/Alligators.dat"
Gators <- read.table(file=Gators, header=TRUE)
boxplot(x~ y, data = Gators, varwidth = T)
#Food group F has a small distribution compared to Food Group O and generally corresponds
#to mid ranged gators vs Food Group O who encompasses larger gators as well. Food Group I 
#consists of the smallest Gators and represents a much narrower range of possible measurements. 

mod.fit <- multinom(y ~ x, data = Gators)
coefficients(mod.fit)

#log(pi(x)/pf(x)) = 4.08 -2.36x
#log(po(x)/pf(x)) = -1.62 -.11x

mod.fitnull <-update(mod.fit, ~ . - x)
anova(mod.fitnull, mod.fit, test = "Chisq")
#Test statistic is 16.80, and the p-value is .00022 giving very strong evidence that x
#or the length of the Gator is statistically significant to our model

#odds ratio of j=2 vs j=3 for a one increase is basically e^b3/e^b2
b2 <- coef(mod.fit)[1,2]
b3 <- coef(mod.fit)[2,2]
exp(b3)/exp(b2)
#Estimate a 11.7685 times increase in the odds of consuming "other" compared to 
#"invertibrate" given a one meter increase in length.

conf.beta <- confint(mod.fit)
CI.OR2 <- exp(conf.beta[2, 1:2, 1])
round(1/CI.OR2,2)
#We are 95% confident that a one meter increase in length corresponds to a 50.89 
#to 2.18 times increase in the odds ratio between fish vs. invertibrate.

range(Gators$x)
xvals <- seq(1, 4, .05)
pi.hat <- predict(mod.fit, data.frame(x = xvals), type = "probs")
matplot(matrix(rep(xvals, 3), ncol = 3), pi.hat, type = "l", ylim = c(0,1), lty = 1:3, lwd = 2, col = c("red", "blue", "black"), xlab = "length", ylab = "pi.hat", main= "p-value of group membership given length")
legend("right", inset = .05, col = c("red", "blue", "black"), lty = 1:3, lwd = 2, legend = c("Fish", "Invertibrate", "Other"))


