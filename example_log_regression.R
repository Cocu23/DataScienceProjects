# Logistic Regression: One Numeric Predictor

library(MASS)
data("menarche")
str(menarche)
summary(menarche)

plot(Menarche/Total ~ Age, data=menarche)
glm.out = glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(logit), data=menarche)

plot(Menarche/Total ~ Age, data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")
summary(glm.out)


glm.out$coef
glm.out$fitted
glm.out$resid
glm.out$effects
anova(glm.out)


# Recall that the response variable is log odds, so the coefficient of "Age" can be 
# interpreted as "for every one year increase in age the odds of having reached menarche increase by exp(1.632) = 5.11 times."
# 
# To evaluate the overall performance of the model, look at the null 
# deviance and residual deviance near the bottom of the print out. 
# Null deviance shows how well the response is predicted by a model with nothing but an intercept (grand mean). 
# This is essentially a chi square value on 24 degrees of freedom, and indicates very 
# little fit (a highly significant difference between fitted values and observed values). 
# Adding in our predictors--just "Age" in this case--decreased the deviance by 3667 points on 1 degree of freedom. 
# Again, this is interpreted as a chi square value and indicates a highly significant decrease in deviance. 
# The residual deviance is 26.7 on 23 degrees of freedom. We use this to test the overall
# fit of the model by once again treating this as a chi square value. 
# A chi square of 26.7 on 23 degrees of freedom yields a p-value of 0.269. 
# The null hypothesis (i.e., the model) is not rejected. 
# The fitted values are not significantly different from the observed values.



# Logistic Regression: Multiple Numerical Predictors


