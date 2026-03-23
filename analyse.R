library(tidyverse)
library(dslabs)

# Eksempeldata 

data("heights")
head(heights)
mean(heights$height)
heights %>% 
  group_by(sex) %>%
  summarise(mean_height = mean(height))
heights %>%
  ggplot(aes(x = sex, y = height)) +
  geom_boxplot()

# Gapminder øving

data("gapminder")
head(gapminder)
gapminder %>%
  group_by(continent) %>%
  summarise(mean_life = mean(life_expectancy))
gapminder %>%
  ggplot(aes(x = continent, y = life_expectancy)) +
geom_boxplot()

# Prosjektdata

data("gapminder")
head(gapminder)
names(gapminder)
glimpse(gapminder)
gapminder <- gapminder %>%
  mutate(gdp_per_capita = gdp / population)
gapminder %>%
  ggplot(aes(x = gdp_per_capita, y = life_expectancy)) +
  geom_point()
gapminder %>%
  ggplot(aes(x = gdp_per_capita, y = life_expectancy)) +
  geom_point() +
  scale_x_log10()
gapminder %>%
  ggplot(aes(x = gdp_per_capita, y = life_expectancy, color = continent)) +
  geom_point() +
  scale_x_log10()
gapminder %>%
  group_by(continent) %>%
  summarise(
    mean_life = mean(life_expectancy),
    mean_gdp = mean(gdp_per_capita)
  )
