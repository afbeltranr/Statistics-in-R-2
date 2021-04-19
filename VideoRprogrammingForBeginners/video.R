library(gapminder)
data('gapminder')

summary(gapminder)

x <- mean(gapminder$gdpPercap)

x

attach(gapminder)
median(pop)


hist(lifeExp)
hist(pop)
hist(log(pop))
boxplot(lifeExp ~ continent)
plot(lifeExp ~ gdpPercap)

plot(lifeExp ~ log(gdpPercap))

# dplyr for Data set manipulation

library(dplyr)


 gapminder %>% select(country, lifeExp) %>% filter(country == 'South Africa' | country == 'Ireland') %>% group_by(country) %>% summarise(Average_life = mean(lifeExp))

# Isthis difference  due to chance?

df1 <- gapminder %>% select(country, lifeExp) %>% filter(country == 'South Africa' | country == 'Ireland') 

t.test(data = df1, lifeExp ~ country)

## data visualization 

library(ggplot2)

gapminder %>% filter(gdpPercap < 50000) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, col = continent, size = pop)) +
  geom_point(alpha = 0.4) + geom_smooth(method = lm) + 
  facet_wrap(~continent)


gapminder %>% filter(gdpPercap < 50000) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, col = year, size = pop)) +
  geom_point(alpha = 0.4) + geom_smooth(method = lm) + 
  facet_wrap(~continent)

summary(lm(lifeExp ~ gdpPercap))

summary(lm(lifeExp ~ gdpPercap+pop))

x <- rnorm(10000, mean = 12, sd =0.01)
library(car)
qqPlot(x)
hist(x)
