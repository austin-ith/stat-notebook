library(tidverse)
library(mosaic)

u.lm <- lm(gasbill ~ month + I(month^2), data = Utilities)
b <- u.lm$coefficients

plot(gasbill ~ month , data = Utilities)
curve(b[1] + b[2]*x + b[3]*x^2,add = TRUE)


uglm <- glm(gasbill>80 ~ month + I(month^2), data = Utilities, family = binomial)
c <- uglm$coefficients

plot(gasbill>80 ~ month, data = Utilities, pch = 21, bg=rgb(.53,.8,.91,.1), col = "skyblue")
curve(exp(c[1] + c[2]*x + c[3]*x^2)/(1+exp(c[1] + c[2]*x + c[3]*x^2)),add = TRUE)


uglm <- glm(gasbill>80 ~ I(month^2) + I(month^3), data = Utilities, family = binomial)
c <- uglm$coefficients

plot(gasbill>80 ~ month, data = Utilities)
curve(exp(c[1]+ c[2]*x^2 + c[3]*x^3)/(1+exp(c[1] + c[2]*x^2 + c[3]*x^3)),add = TRUE)


uglm <- glm(gasbill>80 ~ month + I(month^2) + I(month^3) + I(month^4), data = Utilities, family = binomial)
c <- uglm$coefficients

plot(gasbill>80 ~ month, data = Utilities)
curve(exp(c[1] + c[2]*x + c[3]*x^2 + c[4]*x^3 + c[5]*x^4)/(1+exp(c[1] + c[2]*x + c[3]*x^2 + c[4]*x^3 + c[5]*x^4)),add = TRUE)





