dat7 <- dat %>%
  group_by(month, day, year) %>% 
  summarise(monthly.temp = mean(temp),
            monthly.rh = mean(rh),
            monthly.rf = mean(aa1_2))

ggplot(dat7, aes(x=month, y=avg.temp, group=month)) + geom_boxplot()  
ggplot(dat7, aes(x=day, y=monthly.temp)) + geom_hex(bins=c(7,42)) + facet_wrap(~month, nrow=1)

ggplot(dat7, aes(x=month, y=monthly.rh, group=month)) + geom_boxplot() 
ggplot(dat7, aes(x=day, y=monthly.rh)) + geom_hex(bins=c(5,15)) + facet_wrap(~month, nrow=1)

dat7b <- dat %>%
  group_by(month, time) %>% 
  summarise(monthly.rf = mean(aa1_2))

ggplot(dat7b, aes(x=month, y=monthly.rf, group=month)) + geom_boxplot()
ggplot(dat7b, aes(x=time, y=monthly.rf)) + geom_point()


