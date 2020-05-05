library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(hexbin)

dat5 <- dat %>%
  filter(aa1_2 != 0, year >= 2000)

dat6 <- dat %>%
  filter(year>=2000)

#-------------Temp vs RH-------------

ggplot(dat1, aes(x=avg.temp, y=avg.rh)) +  geom_point() +
  geom_smooth(method = "lm", se=F)

# Check the residual dependance plot (flat ->  parametric order of 1)
M4 <- lm(avg.temp~avg.rh, dat1)
dat1$res4 <- M4$residuals
ggplot(dat1) + aes(x=avg.rh, y=res4) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))

# Check the residual for abnormality (generally alligned with normal distribution)
ggplot(dat1, aes(sample=res4)) + geom_qq(distribution = qnorm) + geom_qq_line(distribution = qnorm)

# Check the spread-location plot (flat -> no change in function as a product of location)
s4 <- data.frame(std.res = sqrt(abs(residuals(M4))),
                 fit     = fitted.values(M4))
ggplot(s4) + aes(x=fit, y=std.res) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))





#----------Temp vs Rainfall---------

ggplot(dat2, aes(x=avg.temp, y=total.rf)) +  geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))

# Check the residual dependance plot (flat ->  parametric order of 1)
M5 <- lm(avg.temp~total.rf, dat2)
dat2$res5 <- M5$residuals

ggplot(dat2) + aes(x=total.rf, y=res5) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))

# Check the residual for abnormality (generally alligned with normal distribution)
ggplot(dat2, aes(sample=res5)) + geom_qq(distribution = qnorm) + geom_qq_line(distribution = qnorm)

# Check the spread-location plot (flat -> no change in function as a product of location)
s5 <- data.frame(std.res = sqrt(abs(residuals(M5))),
                 fit     = fitted.values(M5))
ggplot(s5) + aes(x=fit, y=std.res) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))




#------------RH vs Rainfall--------------

ggplot(dat2, aes(x=avg.rh, y=total.rf)) +  geom_point() +
  geom_smooth(method = MASS::rlm, se=F, formula = y~x + I(x^2)) +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))


# Check the residual dependance plot (flat ->  parametric order of 1)
M6 <- MASS::rlm(avg.rh~total.rf + + I(total.rf^2), dat2)
dat2$res6 <- M6$residuals

ggplot(dat2) + aes(x=total.rf, y=res6) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))

# Check the residual for abnormality (generally alligned with normal distribution)
ggplot(dat2, aes(sample=res6)) + geom_qq(distribution = qnorm) + geom_qq_line(distribution = qnorm)

# Check the spread-location plot (flat -> no change in function as a product of location)
s6 <- data.frame(std.res = sqrt(abs(residuals(M6))),
                 fit     = fitted.values(M6))
ggplot(s6) + aes(x=fit, y=std.res) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1))







