install.packages("gapminder")
library(gapminder)
data("gapminder")

summary(gapminder)
attach(gapminder)

hist(lifeExp)
install.packages("dplyr")
library(dplyr)

gapminder %>%
  select(country,lifeExp) %>%
  filter(country == "South Africa" |
         country == "Ireland") %>%
  group_by(country) %>%
  summarise(average_life = mean(lifeExp))

df1 <- gapminder %>%
  select(country,lifeExp) %>%
  filter(country == "South Africa" |
           country == "Ireland")
t.test(data = df1  ,lifeExp ~ country)

#reject null hypothesis

install.packages("ggplot2")
library(ggplot2)

gapminder %>%
  filter(gdpPercap < 5000) %>%
  ggplot(aes(x = gdpPercap , y = lifeExp,col = continent, size = pop))+
  geom_point(alpha = 0.5)+ 
  geom_smooth(method =  lm)+
    facet_wrap(~continent)

#linear regression

summary(lm(lifeExp ~ gdpPercap+pop))
