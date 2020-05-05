library(ggplot2)
library(dplyr)
library(lubridate)
library(grid)
library(gridExtra)

#Master Dataset-----
dat <- na.omit(LA_airport) %>% 
  filter(aa1_1 != 99, aa1_2 !=999.9, aa1_4 != 3) %>% 
  mutate(year = year(time),
         month = month(time),
         monthyear = decimal_date(ymd(paste(year,month,1, sep = "-"))),
         fval = (row_number() - 0.5)/n(),
         aa1_2 = aa1_2/aa1_1) %>% 
  filter(year != 1972, year != 2020)


#-------------Average Temperature by Year-----
dat1 <- dat %>%
  group_by(monthyear, month, year) %>% 
  summarise(avg.temp = mean(temp),
            avg.rh = mean(rh),
            total.rf = sum(aa1_2))
 
lm(avg.temp~monthyear, dat1)

# Explore structure using a loess fit
ggplot(dat1, aes(x=monthyear, y=avg.temp)) + geom_point() +
  geom_smooth(method = MASS::rlm, formula = y~x, se=FALSE) +
  ggtitle("Average Monthly Temperature") +
  xlab("Month") + ylab("Temperature(°C)")

# Check the residual dependance plot (flat ->  parametric order of 1)
M1 <- MASS::rlm(avg.temp~monthyear, dat1)
dat1$res1 <- M1$residuals

p1a <- ggplot(dat1) + aes(x=sqrt(monthyear), y=res1) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1)) +
  ggtitle("Residual Dependency of Temperature") +
  xlab("Month") + ylab("Residuals")

# Check the residual for abnormality (abnormal distribution)
p1b <- ggplot(dat1, aes(sample=res1)) + geom_qq(distribution = qnorm) +
  geom_qq_line(distribution = qnorm) +
  ggtitle("Quantile - Quantile plot of Temp") +
  xlab("Theoretical Distribution") + ylab("Sample Distribution")

# Check the spread-location plot (flat -> no change in function as a product of location)
s1 <- data.frame(std.res = sqrt(abs(residuals(M1))),
                 fit     = fitted.values(M1))
p1c <- ggplot(s1) + aes(x=fit, y=std.res) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1)) +
  ggtitle("Spread-Location plot of Temp") +
  xlab("Temperature(°C)") + ylab("Standard Residuals")

grid.arrange(p1a, p1b, p1c, nrow=1)

#----------Average Relative Humidity by year-----

# Explore structure using a loess fit
ggplot(dat %>% filter(year >=1980, year <= 1986), aes(x=monthyear, y=rh)) + geom_point() +
  geom_smooth(method = MASS::rlm, formula = y~x, se=F) +
  ggtitle("Average Monthly Humidity") +
  xlab("Month") + ylab("% Humidity")

# Check the residual dependance plot (no consistent trend)
M2 <- MASS::rlm(avg.rh~monthyear, dat1)
dat1$res2 <- M2$residuals

p2a <- ggplot(dat1) + aes(x=monthyear, y=res2) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1)) +
  ggtitle("Residual Dependency of Humidity") +
  xlab("Month") + ylab("Residuals")

# Check the residual for abnormality (generally alligned with normal distribution)
p2b <- ggplot(dat1, aes(sample=res2)) + geom_qq(distribution = qnorm) +
  geom_qq_line(distribution = qnorm)+
  ggtitle("Quantile - Quantile plot of Humidity") +
  xlab("Theoretical Distribution") + ylab("Sample Distribution")

# Check the spread-location plot (no major trend)
s2 <- data.frame(std.res = sqrt(abs(residuals(M2))),
                 fit     = fitted.values(M2))
p2c <- ggplot(s2) + aes(x=fit, y=std.res) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1)) +
  ggtitle("Spread-Location plot of Humidity") +
  xlab("% Humidity") + ylab("Standard Residuals")

grid.arrange(p2a, p2b, p2c, nrow=1)


#-----------Total Rainfall by Year-----
dat2 <- dat %>%
  group_by(year) %>% 
  summarise(avg.temp = mean(temp),
            avg.rh = mean(rh),
            total.rf = sum(aa1_2))

# Explore structure using a loess fit

ggplot(dat2, aes(x=year, y=total.rf)) + geom_point() +
  geom_smooth(method = MASS::rlm, formula = y~x + I(x^2), se=F)  +
  ggtitle("Total Yearly Rainfall") +
  xlab("Year") + ylab("Precipitation fall (mm)")

# Check the residual dependance plot (no major trend)
M3 <- MASS::rlm(total.rf~year + I(year^2), dat2)
dat2$res3 <- M3$residuals

p3a <- ggplot(dat2) + aes(x=year, y=res3) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1)) +
  ggtitle("Residual Dependency of Precipitation") +
  xlab("Year") + ylab("Residuals")

# Check the residual for abnormality (abnormal: shifts upward on both end)
p3b <- ggplot(dat2, aes(sample=res3)) + geom_qq(distribution = qnorm) +
  geom_qq_line(distribution = qnorm) +
  ggtitle("Quantile - Quantile plot of Precipitation") +
  xlab("Theoretical Distribution") + ylab("Sample Distribution")

# Check the spread-location plot (slopes upward)
s3 <- data.frame(std.res = sqrt(abs(residuals(M3))),
                 fit     = fitted.values(M3))
p3c <- ggplot(s3) + aes(x=fit, y=std.res) + geom_point() +
  geom_smooth(method = "loess", se=F, method.args = list(degree = 1, family="symmetric")) +
  ggtitle("Spread-Location plot of Precipitation") +
  xlab("Precipitation fall (mm)") + ylab("Standard Residuals")
  
grid.arrange(p3a, p3b, p3c, nrow=1)
