# Incarceration Analysis and Graphs

# Load the tidyverse package
library(tidyverse)

# Load the dplyr package
library(dplyr)

# Load the ggplot2 and scales packages
library(ggplot2)
library(scales)

# Load incarceration data into variable named 'incar_data'

incar_data <- read.csv(file = 'https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv')
View(incar_data)

# Variable Set 1: Between all states, how many, when, and where do the **MOST**
# incarcerated black populations reside?

# Location of **MOST** incarcerated black population = CA, Los Angeles County
loc_most_black_pop <- incar_data %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name, state)
print(loc_most_black_pop)

# Year of **MOST** incarcerated black population = 1993
year_most_black_pop <- incar_data %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
print(year_most_black_pop)

# Number of **MOST** incarcerated black population = 13143.92 (hundreds)
num_most_black_pop <- incar_data %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(black_jail_pop)
print(num_most_black_pop)

# Variable Set 2: Between all states, how many, when, and where do the **MOST**
# incarcerated white populations reside?

# Location of **MOST** incarcerated white population = CA, Los Angeles County
loc_most_white_pop <- incar_data %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name, state)
print(loc_most_white_pop)

# Year of **MOST** incarcerated white population = 1993
year_most_white_pop <- incar_data %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
print(year_most_white_pop)

# Number of **MOST** incarcerated black population = 7036.59 (hundreds)
num_most_white_pop <- incar_data %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(white_jail_pop)
print(num_most_white_pop)

# Variable Set 3: What are the ratios for the highest single incarcerated
# black and white populations to each respective year's total population
# (both ended up being 1993 in CA, Los Angeles County, which allows for
# an equivalent ratio comparison).

# Ratio of highest incarcerated black population to highest incarcerated
# white population: 1.867939

ratio_hi_jail_pop_black_white <- num_most_black_pop / num_most_white_pop
print(ratio_hi_jail_pop_black_white)

# Ratio of highest incarcerated black population to respective year and
# locations's total population: 0.5600977
# Additional Note: Respective year (1993) jailed population: 23467.19 (hundreds)
num_hi_black_total_pop_1993 <- incar_data %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(total_jail_pop)

ratio_hi_jail_pop_black_total <- num_most_black_pop / num_hi_black_total_pop_1993
print(ratio_hi_jail_pop_black_total)

# Ratio of highest incarcerated white population to respective year and
# locations's total population: 0.299848
# Additional Note: Respective year (1993) jailed population: 23467.19 (hundreds)
num_hi_white_total_pop_1993 <- incar_data %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(total_jail_pop)

ratio_hi_jail_pop_white_total <- num_most_white_pop / num_hi_white_total_pop_1993
print(ratio_hi_jail_pop_white_total)

# Variable Set 4: Between all states, how many, when, and where do the **MOST**
# incarcerated AAPI, latinx, native, and 'other race' populations reside?

# Location of **MOST** incarcerated AAPI population = CA, Los Angeles County
loc_most_aapi_pop <- incar_data %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name, state)
print(loc_most_aapi_pop)

# Year of **MOST** incarcerated AAPI population = 1999
year_most_aapi_pop <- incar_data %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
print(year_most_aapi_pop)

# Number of **MOST** incarcerated AAPI population = 893 (hundreds)
num_most_aapi_pop <- incar_data %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  pull(aapi_jail_pop)
print(num_most_aapi_pop)

###############################################################################

# Location of **MOST** incarcerated latinx population = CA, Los Angeles County
loc_most_latinx_pop <- incar_data %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name, state)
print(loc_most_latinx_pop)

# Year of **MOST** incarcerated latinx population = 1993
year_most_latinx_pop <- incar_data %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
print(year_most_latinx_pop)

# Number of **MOST** incarcerated latinx population = 16594.81 (hundreds)
num_most_latinx_pop <- incar_data %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>% 
  pull(latinx_jail_pop)
print(num_most_latinx_pop)

###############################################################################

# Location of **MOST** incarcerated native population = AZ, Maricopa County
loc_most_native_pop <- incar_data %>%
  filter(native_jail_pop == max(native_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name, state)
print(loc_most_native_pop)

# Year of **MOST** incarcerated native population = 2007
year_most_native_pop <- incar_data %>%
  filter(native_jail_pop == max(native_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
print(year_most_native_pop)

# Number of **MOST** incarcerated native population = 425 (hundreds)
num_most_native_pop <- incar_data %>%
  filter(native_jail_pop == max(native_jail_pop, na.rm = TRUE)) %>% 
  pull(native_jail_pop)
print(num_most_native_pop)

###############################################################################

# Location of **MOST** incarcerated 'other race' population = IN, Marion County
loc_most_other_pop <- incar_data %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE)) %>% 
  pull(county_name, state)
print(loc_most_other_pop)

# Year of **MOST** incarcerated 'other race' population = 2002
year_most_other_pop <- incar_data %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
print(year_most_other_pop)

# Number of **MOST** incarcerated 'other race' population = 1642 (hundreds)
num_most_other_pop <- incar_data %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE)) %>% 
  pull(other_race_jail_pop)
print(num_most_other_pop)

# Variable Set 5: In the most recent year, what is the highest single population
# for incarcerated black, white, AAPI, latinx, and 'other race' individuals
# across all states?

# Number of **MOST** incarcerated black population in most recent
# year = 5024 (hundreds)
recent_num_most_black_pop <- incar_data %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  pull(black_jail_pop)
print(recent_num_most_black_pop)

# Number of **MOST** incarcerated white population in most recent
# year = 4577 (hundreds)
recent_num_most_white_pop <- incar_data %>%
  filter(year == max(year)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  pull(white_jail_pop)
print(recent_num_most_white_pop)

# Number of **MOST** incarcerated AAPI population in most recent
# year = 286.4 (hundreds)
recent_num_most_aapi_pop <- incar_data %>%
  filter(year == max(year)) %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE)) %>% 
  pull(aapi_jail_pop)
print(recent_num_most_aapi_pop)

# Number of **MOST** incarcerated latinx population in most recent
# year = 8728 (hundreds)
recent_num_most_latinx_pop <- incar_data %>%
  filter(year == max(year)) %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE)) %>% 
  pull(latinx_jail_pop)
print(recent_num_most_latinx_pop)

# Number of **MOST** incarcerated native population in most recent
# year = 379 (hundreds)
recent_num_most_native_pop <- incar_data %>%
  filter(year == max(year)) %>%
  filter(native_jail_pop == max(native_jail_pop, na.rm = TRUE)) %>% 
  pull(native_jail_pop)
print(recent_num_most_native_pop)

# Number of **MOST** incarcerated 'other race' population in most recent
# year = 608 (hundreds)
recent_num_most_other_pop <- incar_data %>%
  filter(year == max(year)) %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE)) %>% 
  pull(other_race_jail_pop)
print(recent_num_most_other_pop)

###############################################################################

# GRAPH 1

# Aggregate 10 highest incarcerated black populations (by year)
top_10_black_pop <- top_n(incar_data, 10, black_jail_pop)

top_10_black_pop_organized <- top_10_black_pop %>%
  group_by(year) %>%
  summarize(sum_pop = sum(black_jail_pop))

# Aggregate 10 highest incarcerated white populations (by year)
top_10_white_pop <- top_n(incar_data, 10, white_jail_pop)

top_10_white_pop_organized <- top_10_white_pop %>%
  group_by(year) %>%
  summarize(sum_pop = sum(white_jail_pop))

# Aggregate 10 highest incarcerated AAPI populations (by year)
top_10_aapi_pop <- top_n(incar_data, 10, aapi_jail_pop)

top_10_aapi_pop_organized <- top_10_aapi_pop %>%
  group_by(year) %>%
  summarize(sum_pop = sum(aapi_jail_pop))

# Aggregate 10 highest incarcerated latinx populations (by year)
top_10_latinx_pop <- top_n(incar_data, 10, latinx_jail_pop)

top_10_latinx_pop_organized <- top_10_latinx_pop %>%
  group_by(year) %>%
  summarize(sum_pop = sum(latinx_jail_pop))

# Aggregate 10 highest incarcerated native populations (by year)
top_10_native_pop <- top_n(incar_data, 10, native_jail_pop)

top_10_native_pop_organized <- top_10_native_pop %>%
  group_by(year) %>%
  summarize(sum_pop = sum(native_jail_pop))

# Aggregate 10 highest incarcerated 'other race' populations (by year)
top_10_other_race_pop <- top_n(incar_data, 10, other_race_jail_pop)

top_10_other_race_pop_organized <- top_10_other_race_pop %>%
  group_by(year) %>%
  summarize(sum_pop = sum(other_race_jail_pop))

# Show a bar chart combining the 10 highest incarcerated
# populations over time for each race (by year)
ggplot(NULL, aes(x = year, y = sum_pop)) + 
  labs(x = "Year", y = "Incarcerated Population (Hundreds)",
       title = "10 Single-Year Highest Incarcerated Populations Over Time for Each Race") +
  geom_col(data = top_10_black_pop_organized, aes(color = "Black")) +
  geom_col(data = top_10_white_pop_organized, aes(color = "White")) +
  geom_col(data = top_10_aapi_pop_organized, aes(color = "AAPI")) +
  geom_col(data = top_10_latinx_pop_organized, aes(color = "Latinx")) +
  geom_col(data = top_10_native_pop_organized, aes(color = "Native")) +
  geom_col(data = top_10_other_race_pop_organized, aes(color = "Other Race")) +
  scale_x_continuous(breaks= pretty_breaks())

###############################################################################

# GRAPH 2

# Aggregate 10 highest incarcerated black populations (by year)
top_10_black_pop <- top_n(incar_data, 10, black_jail_pop)

top_10_black_pop_organized_2 <- top_10_black_pop %>%
  group_by(year) %>%
  summarize(sum_pop2 = sum(black_jail_pop))

# Aggregate total populations for each year in the variable 'top_10_black_pop_organized_2'

top_10_total_pop_organized_2 <- top_10_black_pop %>%
  group_by(year) %>%
  summarize(sum_pop2 = sum(total_jail_pop))

# Plot boxplots of the 10 highest incarcerated black populations and the
# total incarcerated populations for each respective year

ggplot(NULL, aes(x = year, y = sum_pop2)) + 
  labs(x = "Year", y = "Incarcerated Population (Hundreds)",
       title = "10 Single-Year Highest Incarcerated Black Populations vs. Total Population") +
  geom_boxplot(data = top_10_black_pop_organized_2, aes(color = "Black")) +
  geom_boxplot(data = top_10_total_pop_organized_2, aes(color = "Total Population")) +
  scale_x_continuous(breaks= pretty_breaks())

###############################################################################

# Graph 3

# Load "maps" package and blank theme

library(maps)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# Load incarceration data and remove NA values from intended column
incarcerations <- incar_data %>%
  na.omit(black_jail_pop)# replace with lowercase for joining
View(incarcerations)

# Combine region, subregion and attach fips to the data frame
state_shape <- map_data("county") %>% # load state shapefile
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")

# Join incarceration data to the U.S. county shapefile
map_data <- state_shape %>% 
  left_join(incarcerations, by="fips")

# Draw the map setting the `fill` of each location using its total
# black jail population
ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "white",
    size = .0001) +
  coord_map() +
  scale_fill_continuous(na.value = "gray", low = "blue", high = "red") +
  blank_theme

