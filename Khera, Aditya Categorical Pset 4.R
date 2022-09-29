#' ---
#' title: "Categorical Data Analysis Pset 4"
#' author: "Aditya Khera"
#' date: "March 28, 2022"
#' ---

data <- read.csv(file = "/Users/adityakhera/Desktop/Statistics/Categorical Data/placekick.BW.csv")
head(data)

data$Good <- ifelse(data$Good=="Y", 1, 0)

#Exercise 1a
mod.fit <- glm(Good ~ Distance, data = data, family = binomial(link = logit))
summary(mod.fit)

#The coefficients of our model show that beta0 = 5.409295 and beta1 = -0.106270
#We can say logit(pi) = 5.409295 -0.106270 * distance
#pi.hat = exp(5.409295 -0.106270 * distance)/(1 + exp(5.409295 -0.106270 * distance))

#Exercise 1b constructed a scatter plot and a bubble plot for distance vs estimated probability
w <- aggregate(Good ~ Distance, data = data, FUN = sum)
n <- aggregate(Good ~ Distance, data = data, FUN = length)
w.n <- data.frame(distance = w$Distance, success = w$Good, trial = n$Good, prop = round(w$Good/n$Good, 4))

plot(prop ~ distance, data = w.n, xlab = "Distance", ylab = "Estimated Probability", xlim = c(18,68), main = "Scatter Plot")
plot(prop ~ distance, data = w.n, cex = sqrt(trial)/2, xlab = "Distance", ylab = "Estimated Probability", main = "Bubble Plot", xlim = c(18,68), ylim = c(0, 1.1))

#Exercise 1c overlaying our model onto the graph
x.vals <- 18:68
pi.hat <- predict(mod.fit, newdata = data.frame(Distance = x.vals), type = "response")
plot(prop ~ distance, data = w.n, cex = sqrt(trial)/2, xlab = "Distance", ylab = "Estimated Probability", main = "Bubble Plot", xlim = c(18,68), ylim = c(0, 1.1))
lines(x.vals, pi.hat)
#The model seems to fit the bubble plot extremely well. The line goes through most of the
#bubbles where there are large portions of data.

#Exercise 1d predicting distance = 68 meters
predict(mod.fit, newdata = data.frame(Distance = 68), type = "response")
#Using predict on our model, we obtain a p-value of .1397. This makes sense since it is a
#low probability, which is in line with the model we created. As distance increases the 
#likelihood of success decreases, so it makes sense that such a large distance would have
#a low probability associated with it. 

#Exercise 1e overlaying the confidence intervals onto the graph
plot(prop ~ distance, data = w.n, cex = sqrt(trial)/2, xlab = "Distance", ylab = "Estimated Probability", main = "Bubble Plot", xlim = c(18,68), ylim = c(0, 1.1))
lines(x.vals, pi.hat)
lin.pred <- predict(mod.fit, newdata = data.frame(Distance = x.vals), type = "link", se = T)
CI.lo <- lin.pred$fit - qnorm(.975) * lin.pred$se.fit
CI.low <- exp(CI.lo)/(1+exp(CI.lo))
lines(x.vals, CI.low, lty = 2)

CI.hi <- lin.pred$fit + qnorm(.975) * lin.pred$se.fit
CI.high <- exp(CI.hi)/(1+exp(CI.hi))
lines(x.vals, CI.high, lty =2 )

#Exercise 1f predict and interpret confidence interval of distance = 68
ci68 <- predict(mod.fit, newdata = data.frame(Distance = 68), type = "link", se = T)
ci68.2 <- ci68$fit + c(-1, 1)*qnorm(.975)*ci68$se.fit
exp(ci68.2)/(1+exp(ci68.2))
#We are 95% confident that the true probability of success from 68 yards away is within 
#the range <0.0994, 0.1931>. I would be cautious to accept this since we are extrapolating 
#the data, but our actual prediction falls within this range. It appears to be a 
#solid predictor.

#Exercise 2, setting the data into correct classes
data$Weather <- factor(data$Weather)
data$Temperature <- factor(data$Temperature)
data$Pressure <- factor(data$Pressure)

#2a new multi-model with more variables present
mod.multifit <- glm(Good ~ Distance + Weather + Wind15 + Temperature + Grass + Pressure, data = data, family = binomial(link = logit))
summary(mod.multifit)
#Given it's large associated beta value, the most significant factor is if it is raining
#snow rain outdoors during the kick. Since this is a binary variable, however, and distance 
#can range to 68 in our set, distance is greatest determiner of success with a placekick. 
#Addiitonally, distance has the lowest p-value, indicating it is the most statitscally 
#significant value in the model. 

#2b
exp(-10*mod.fit$coefficients[2])
exp(-10*mod.multifit$coefficients[2])
#The odds ratio is slightly larger in our new mutli-fit model compared to our previous 
#exercises model. The odds of success with every 10 meter decrease in distance corresponds 
#to a 2.8941 and 2.9767 times increase in the odds of success for each model respectively. 
#They are not equal since each model has different beta values, owing to the fact that 
#they're made of different variables, one with only distance the other including a number
#of others.

#2c calculating 6 odds ratios for the levels of weather 
exp(mod.multifit$coefficients[3])
#The odds of success while it is Inside is .8962 times greater or 1.115 times less than
#the baseline.
exp(mod.multifit$coefficients[4])
#The odds of success while there is Snow/Rain is .6446 times greater or 1.551 times less 
#than the baseline.
exp(mod.multifit$coefficients[5])
#The odds of success while there is Sun is .7800 times greater or 1.282 times less than 
#the baseline.

exp(mod.multifit$coefficients[3] - mod.multifit$coefficients[4])
#The odds of success while it is Inside are 1.3904 times greater than when there is 
#snow/rain
exp(mod.multifit$coefficients[3] - mod.multifit$coefficients[5])
#The odds of success while it is Inside are 1.1491 times greater than when there is sun
exp(mod.multifit$coefficients[5] - mod.multifit$coefficients[4])
#The odds of success while there Sun are 1.2101 times greater than when there is snow/rain.

#2D Calculating confidence intervals of the odds ratios
#i. Since "Clouds" is our baseline or first beta value the confidence interval for clouds 
#ss sun would just be the confidence interval for the odds ratio of beta_sun
beta_sun <- mod.multifit$coefficients[5]
beta_sun.SE <- 0.139487
sunCI <- beta_sun + c(-1,1) * qnorm(.975) * beta_sun.SE
exp(sunCI)
#We are 95% confident that the odds of success in the sun are <.5933, 1.0252> greater 
#than in clouds.


#ii Difference between "Inside" and "SnowRain" would involve 
vcov(mod.multifit)
betaInside <- mod.multifit$coefficients[3]
betaSnowRain <- mod.multifit$coefficients[4]

var_betaInside <- 4.589251e-02
var_betaSnowRain <- 4.731823e-02
cov_InsideSnowRain <- 7.383552e-03
var_insidesnowrain <- var_betaInside + var_betaSnowRain - 2* cov_InsideSnowRain

CIInsideSnowRain <- betaInside - betaSnowRain + c(-1, 1) * qnorm(.975) * sqrt(var_insidesnowrain)
exp(CIInsideSnowRain)
#We are 95% confident that the odds of success inside are <.8031, 2.4075> greater than 
#in snowrain.


frame2e <- data.frame(Distance = 50, Grass = 1, Pressure = "Y", Wind15 = 0, Weather = "Sun", Temperature = "Nice")
ciframe2e <- predict(mod.multifit, newdata = frame2e, type = "link", se = T)
ciframe2e2 <- ciframe2e$fit + c(-1, 1)*qnorm(.975)*ciframe2e$se.fit
exp(ciframe2e2)/(1+ exp(ciframe2e2))
#We are 95% sure that the true probability of success for the specified field goal kick 
#attempt is within the range <0.3743, 0.5964>.

#Exercise 3a building interaction and non interaction models
fit.noint <- glm(Good ~ Distance + Wind15, data = data, family = binomial(link = logit))
fit.wint <- glm(Good ~ Distance + Wind15 + Distance:Wind15, data = data, family = binomial(link = logit))

xvals <- 18:62
#Graphing non-interaction model with and without wind.
beta0hat <- coefficients(fit.noint)
linpred0noint <- beta0hat[1] + beta0hat[2]*xvals
linpred1noint <- beta0hat[1] + beta0hat[2]*xvals + beta0hat[3]
pihat0noint <- exp(linpred0noint)/(1+exp(linpred0noint))
pihat1noint <- exp(linpred1noint)/(1+exp(linpred1noint))
par(mfrow = c(1,2))
plot(xvals, pihat0noint, type = "l", lwd = 2, ylim = c(0,1), xlab = "Distance", ylab = "Estimated probability", main = "No interaction parameter")
lines(xvals, pihat1noint, lty = 2, lwd = 2)
legend("bottomleft", inset=.05, lwd=2, lty=1:2, legend=c("Wind=0", "Wind=1"))

#Graph interaction model with and without wind.
beta1hat <- coefficients((fit.wint))
linpred0int <- beta1hat[1] + beta1hat[2]*xvals
linpred1int <- beta1hat[1] + beta1hat[2]*xvals + beta1hat[3] + beta1hat[4]*xvals
pihat0int <- exp(linpred0int)/(1+exp(linpred0int))
pihat1int <- exp(linpred1int)/(1+exp(linpred1int))
plot(xvals, pihat0int, type = "l", lwd = 2, ylim = c(0,1), xlab = "Distance", ylab = "Estimated probability", main = "Interaction parameter")
lines(xvals, pihat1int, lty = 2, lwd = 2)
legend("bottomleft", inset=.05, lwd=2, lty=1:2, legend=c("Wind=0", "Wind=1"))

#As we can see from both graphs, as the distance of the kick increases, the estimated 
#probability of scoring decreases with it. The presence of wind only further decreases 
#the estimated probability, as seen by the lower wind = 1 lines in both models. The 
#interaction model further shows a greater decrease, though only nominal. We can see 
#from the interaction lines that the presence of wind:distance factor does not 
#significantly decrease estimated probability. 

betaint <- coefficients(fit.wint)
exp(betaint[3] + 30 * betaint[4])
#For a 30-yard field goal attempt, the odds of success in windy conditions are 77.31% of
#the odds of success in non-windy conditions.
exp(betaint[3] + 50 * betaint[4])
#For a 50-yard field goal attempt, the odds of success in windy conditions are 66.57% of 
#the odds of success in non-windy conditions.

summary(fit.wint)
anova(fit.noint, fit.wint, test = "Chisq")
#LRT gives .7135, Wald test gives .715. Both extremely high p-values indicating that the 
#interaction term is not statistically significant.


