## 1 ##
# Consider the space shuttle data ?shuttle in the MASS library. Consider modeling the use of the autolander 
# as the outcome (variable name use). Fit a logistic regression model with autolander (variable auto) use 
# (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). Give the estimated odds ratio 
# for autolander use comparing head winds, labeled as "head" in the variable headwind (numerator) 
# to tail winds (denominator).

data("shuttle")
shuttle$use <- as.numeric(shuttle$use == "auto")
shuttle_model1 <- glm(use ~ wind - 1, data = shuttle, family = "binomial"(link = 'logit'))
summary(shuttle_model1)

exp(shuttle_model$coef[1])/exp(shuttle_model$coef[2])

## 2 ##
# Consider the previous problem. Give the estimated odds ratio for autolander use comparing head winds (numerator)
# to tail winds (denominator) adjusting for wind strength from the variable magn.

shuttle_model2 <- glm(use ~ wind + magn - 1, data = shuttle, family = "binomial"(link = 'logit'))
summary(shuttle_model2)

exp(shuttle_model2$coef[1])/exp(shuttle_model2$coef[2])

## 3 ##
#The coefficients reverse their signs.

## 4 ##
# Consider the insect spray data InsectSprays.  Fit a Poisson model using spray as a factor level.
# Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).

data("InsectSprays")
str(InsectSprays)
Insect_model <- glm(count ~ spray - 1, InsectSprays, family = "poisson")
summary(Insect_model)
exp(Insect_model$coef[1]) / exp(Insect_model$coef[2])

## 5 ##
# Consider a Poisson glm with an offset, t. So, for example, a model of the form 
# glm(count ~ x + offset(t), family = poisson) where 
#   x is a factor variable comparing a treatment (1) to a control (0) and 
#   t is the natural log of a monitoring time. 
# What is impact of the coefficient for x if we fit the model glm(count ~ x + offset(t2), family = poisson) 
# where 2 <- log(10) + t? 
# In other words, what happens to the coefficients if we change the units of the offset variable.
# (Note, adding log(10) on the log scale is multiplying by 10 on the original scale.)










