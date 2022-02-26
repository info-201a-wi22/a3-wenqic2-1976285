library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(maps)
library(mapproj)
jail <- read.csv("incarceration_trends.csv")
#1. summary

## Which state has the highest jail population in the most recent year?
 max_state_1 <- jail %>% 
   filter(year == max(year)) %>% 
   group_by(state) %>% 
   summarize(total_in_state = sum(total_jail_pop, na.rm = T)) %>% 
   filter(total_in_state == max(total_in_state)) %>%
   pull(state)
max_state <- state.name[match(max_state_1, state.abb)]
  
 ## Which state has the highest black jail population in the most recent year?
 max_b_state_1 <- jail %>% 
   filter(year == max(year)) %>% 
   group_by(state) %>% 
   summarize(total_b_in_state = sum(black_jail_pop, na.rm = T)) %>% 
   filter(total_b_in_state == max(total_b_in_state)) %>% 
   pull(state)
max_b_state <- state.name[match(max_b_state_1, state.abb)]

 ## What is the proportion of black population in total jail population in the most recent year?
ave_b <- jail %>% 
  filter(year == max(year)) %>% 
  summarize(ave_black_jail_pop = mean(black_jail_pop, na.rm = TRUE)) %>% 
  pull(ave_black_jail_pop)
total_jail <- jail %>% 
  filter(year == max(year)) %>% 
  summarize(ave_total_jail_pop = mean(total_jail_pop, na.rm = TRUE)) %>% 
  pull(ave_total_jail_pop)
prop_b <- ave_b/total_jail*100

## What is the ratio of white and black jail population in the most recent year?
ave_w <- jail %>% 
  filter(year == max(year)) %>% 
  summarize(ave_white_jail_pop = mean(white_jail_pop, na.rm = TRUE)) %>% 
  pull(ave_white_jail_pop)
prop_w <- ave_w/total_jail*100
ratio_b_to_w <- prop_w/prop_b

## How much did the total jail population change from 2017 to 2018
total_17_18 <- jail %>% 
  filter(between(year, "2017", "2018")) %>% 
  group_by(year) %>% 
  summarize(total_pop = sum(total_jail_pop, na.rm = T)) %>% 
  spread(key = year,
         value = total_pop)
 diff_total <- total_17_18$`2018` - total_17_18$`2017` 
 
 
  
#2. Trend chart
jail_2 <- select(jail, c(`year`,`latinx_jail_pop`,`white_jail_pop`,`native_jail_pop`,`black_jail_pop`,`aapi_jail_pop`)) %>% 
  filter(between(year, "2010", "2018")) %>% 
  group_by(year) %>% 
  summarize(
    latinx = sum(latinx_jail_pop, na.rm = TRUE),
    white = sum(white_jail_pop, na.rm = TRUE),
    native = sum(native_jail_pop, na.rm = TRUE),
    black = sum(black_jail_pop, na.rm = TRUE),
    aapi = sum(aapi_jail_pop, na.rm = TRUE)
  ) %>% 
  gather(
    key = race,
    value = total_count,
    -year
  )

trend_chart <- ggplot(jail_2, aes(x = year, y = total_count, colour = race)) + 
  geom_line() + 
  geom_point() + 
  xlim(2010, 2018) +
  labs(x = "Year", y = "Jail population count", title = "Trends of jail population for different races")

 
#3. Comparison chart
jail_3 <- select(jail, c(`year`, `male_juvenile_jail_pop`, `female_juvenile_jail_pop`)) %>% 
  filter(year %in% c("2000", "2005", "2010", "2015")) %>% 
  group_by(year) %>% 
  summarize(
    male = mean(male_juvenile_jail_pop, na.rm = TRUE),
    female = mean(female_juvenile_jail_pop, na.rm = TRUE)
  ) %>% 
  gather(
    key = gender,
    value = average_count,
    -year
  )

comparison_chart <- ggplot(jail_3, aes(x = year, y = average_count, fill = gender)) + 
  geom_col() +
  labs(x = "Year", y = "Jail population count", title = " Comparison of juvenile jail population by gender")


#4. Map
jail_4 <- select(jail, c(`state`,`total_jail_pop`)) %>% 
  group_by(state) %>% 
  summarize(count_in_state = mean(total_jail_pop, na.rm = TRUE))
jail_4$state <- tolower(state.name[match(jail_4$state, state.abb)])

state_shape <- map_data("state") %>% 
  rename(state = region) %>% 
  left_join(jail_4, by="state")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = count_in_state),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "white", high = "red") +
  labs(fill = "Jail population") +
  blank_theme



 
 