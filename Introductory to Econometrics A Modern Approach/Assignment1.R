library(wooldridge)

data("wage1")

wageModel <- lm(lwage ~ educ + exper + tenure, data = wage1)

summary(wageModel)

aaassdff