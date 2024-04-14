# Assignment 2 - A story of life!

# Install and load necessary packages
install.packages("tidyverse")
install.packages("maps") 
install.packages("gapminder")
install.packages("plotly")

library(tidyverse)
library(readxl)
library(gapminder)
library(plotly)

# import datasets
unicef_metadata_raw <- read_excel("unicef_metadata_xl.xlsx")
unicef_ind_1_raw <- read_excel("unicef_indicator_1_xl.xlsx")
unicef_ind_2_raw <- read_excel("unicef_indicator_2_xl.xlsx")

# Data manipulations

unicef_metadata <- unicef_metadata_raw %>%
  select(country, alpha_3_code,
         time_period = year,
         population = `Population, total`,
         gdp_per_capita = `GDP per capita (constant 2015 US$)`,
         inflation = `Inflation, consumer prices (annual %)`, 
         life_expectancy = `Life expectancy at birth, total (years)`)

uni_ind1 <- unicef_ind_1_raw %>% 
  distinct(alpha_3_code, time_period, .keep_all = TRUE)

#filtering 2018, as the file has only 4 other years randomly
uni_ind2 <- unicef_ind_2_raw %>% 
  distinct(alpha_3_code, time_period, .keep_all = TRUE) %>%
  filter(time_period == 2018)



#Visualizations

# Map of life expectancy at different countries in year 1960
lifeExp_1960 <- unicef_metadata %>%
  filter(time_period == 1960)

map_world <- map_data("world")

head(map_world)

map_world %>% filter(str_detect(region, "USA"))

map_lifeExp_1960 <- full_join(map_world, lifeExp_1960, by = c("region" = "country"))

head(map_lifeExp_1960)

ggplot(map_lifeExp_1960) + 
  aes(long, lat, group = group, fill = life_expectancy) + 
  geom_polygon() +
  scale_fill_gradient(low = "red", high = "yellow", na.value = "grey") +
  theme_bw() + 
  labs(
    title = "Life Expectancy at different countries in the world in 1960",
    caption = "Note: Countries in grey have no data due to a mismatch with their names",
    x = "Latitude",
    y = "Longitude",
    fill = "Life Expectancy"
  )



# Government expenditure in % of GDP, on healthcare in 2018 and its impact on life expectancy in 2019

cntry_continents <- gapminder %>%
  select(country, continent) %>%
  distinct


govExp_2018 <- uni_ind2 %>%
  filter(time_period == 2018) %>%
  mutate(NextYear = time_period + 1) %>%
  inner_join(unicef_metadata, by = c("alpha_3_code" = "alpha_3_code",
                                     "country" = "country", 
                                     "NextYear" = "time_period")) %>%
  inner_join(cntry_continents, by = c("country" = "country"))


ggplot(data = govExp_2018) +
  aes(life_expectancy, obs_value,  color = continent) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ continent, nrow = 1) +
  labs(
    x = "Life Expectancy at Birth in Year 2019",
    y = "Govt expenditure on health (% of GDP)",
    title = "Correlation between Government Health Spending and Life Expectancy ",
    color = "Continents"
  )


#bar chart

foodConsumption <- uni_ind1 %>%
  filter(sex == "Total") %>%
  inner_join(unicef_metadata, by = c("alpha_3_code" = "alpha_3_code",
                                     "country" = "country", 
                                     "time_period" = "time_period")) %>%
  inner_join(cntry_continents, by = c("country" = "country")) %>%
  group_by(country) %>%
  summarise(mean_food = mean(obs_value))

cols = rainbow(26, s=.6, v=.9)[sample(1:26,26)]

ggplot(foodConsumption) +
  aes(country, mean_food, fill = country) +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_manual(values=cols) +
  labs(
    x = "Country",
    y = "Egg and/or flesh foods consumption in infants of 6-23 months",
    title = "Egg and/ or Flesh Food Consumption among infants/toddlers",
    color = "Countries"
  )


#time series chart - life expectancy by country over the years

lifeExp_TimeSeries <- unicef_metadata %>%
  inner_join(cntry_continents, by = c("country" = "country")) %>%
  ggplot() +
  aes(time_period, life_expectancy, group = country, color = continent) +
  geom_line() +
  theme_bw() +
  labs(
    x = "Year",
    y = "Life Expectancy at Birth",
    title = "Evolution of Life Expectancy over the years (1960 to 2022)",
    color = "Continents"
  )

ggplotly(lifeExp_TimeSeries) %>%
  layout(width = 800, height = 450)


# Evolution of life expectancy from 1992 to 2007 by continents

lifeExp_evolution <- unicef_metadata %>%
  inner_join(cntry_continents, by = c("country" = "country"))

ggplot(lifeExp_evolution, aes(time_period, life_expectancy, color = continent)) +
  geom_point(alpha=0.3, size = 1) +
  geom_smooth() +
  scale_x_continuous(breaks = c(1960, 1980, 2000, 2020)) +
  theme_bw() +
  theme_classic() +
  facet_wrap( ~ continent, nrow = 1) +
  labs(
    x = "Year",
    y = "Life Expectancy",
    title = "Evolution of Life Expectancy",
  ) + 
  guides(color = "none")

  

