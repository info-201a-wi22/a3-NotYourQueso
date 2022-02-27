#Installing necessary packages
  library("dplyr")
  library("tidyverse")
  library("ggplot2") 


# Loading the dataset
incarceration <- read.csv("../source/incarceration_trends.csv",
                          stringsAsFactors = FALSE)


#..............................................................................#
#                       Summary Information Section:                           #
#         Answering preliminary questions to narrow project focus              #
#..............................................................................#
  

  # Q1: Which state had the highest population of prisoners in 2016?
    max_state_2016 <- incarceration %>%
      group_by(state) %>%
      filter(year == 2016) %>%
      summarize(sum_jail_pop = sum(total_jail_pop + total_prison_pop, na.rm = TRUE)) %>%
      filter(sum_jail_pop == max(sum_jail_pop)) %>%
      pull(state)

    # Q1A: What was the population?
      max_pop_2016 <- incarceration %>%
        group_by(state) %>%
        filter(year == 2016) %>%
        summarize(sum_pop = round(sum(total_jail_pop + total_prison_pop, na.rm = TRUE))) %>%
        filter(sum_pop == max(sum_pop)) %>%
        pull(sum_pop)

  # Q2: Which county had the highest pop?
      max_county_2016 <- incarceration %>%
        group_by(state, county_name) %>%
        filter(year == 2016,
               state == "TX") %>%
        summarize(sum_pop = round(sum(total_jail_pop + total_prison_pop, na.rm = TRUE))) %>%
        filter(sum_pop == max(sum_pop)) %>%
        pull(county_name)
      
    # Q2a: What was the population?
      max_county_count_2016 <- incarceration %>%
        group_by(state, county_name) %>%
        filter(year == 2016,
               state == "TX") %>%
        summarize(sum_pop = round(sum(total_jail_pop + total_prison_pop, na.rm = TRUE))) %>%
        filter(sum_pop == max(sum_pop)) %>%
        pull(sum_pop)

  # Q3: How has prisoner population changed since 2000?
      # Ex. "prison rate has decreased by x amount since 2000"
      
      # Calculates total population of prisoners in TX in 2000
      max_pop_2006 <- incarceration %>%
        group_by(state) %>%
        filter(year == 2006,
               state == "TX") %>%
        summarize(sum_pop = round(sum(total_jail_pop + total_prison_pop, na.rm = TRUE))) %>%
        filter(sum_pop == max(sum_pop)) %>%
        pull(sum_pop)
      
      # Calculates % change between 2000 and 2016
      diff_2016_2006 <- round((max_pop_2016 - max_pop_2006)  / max_pop_2006 * 100)
  
  # Q4: Which region had the highest prisoner population in 2016?
      max_region_2016 <- incarceration %>%
        group_by(region) %>%
        filter(year == 2016) %>%
        summarize(sum_jail_pop = sum(total_jail_pop + total_prison_pop, na.rm = TRUE)) %>%
        filter(sum_jail_pop == max(sum_jail_pop)) %>%
        pull(region)
  

      
#..............................................................................#
#                      Chart 1: Trends Over Time                               #
#          Observing Patterns in a Decade of Texas Prison Data                 #
#..............................................................................#
      
tx_race_pop <- incarceration %>%
  group_by(year) %>%
  
  # Selecting this range because it has the most data on prison populations. 
  filter(year >= 2006 & year <= 2016, state == "TX") %>%
  
  # Excluded AAPI & Native American data from this set due to missing/incomplete data.
  summarize(black = sum(black_prison_pop, na.rm = TRUE),
            latinx = sum(latinx_prison_pop, na.rm = TRUE),
            white = sum(white_prison_pop, na.rm = TRUE)) %>%
  gather(key = Race, value = population, - year)


trends_over_time <- ggplot(data = tx_race_pop) +
  geom_line(mapping = aes(x = year, y = population, color = Race)) +
  geom_point(mapping = aes(x = year, y = population)) +
  labs(
    title = "Prison Populations in Texas State between 2006 and 2016",
    x = "Year",
    y = "Population Count")

trends_over_time



#..............................................................................#
#                        Chart 2: Comparing Variables                          #
#               Comparing Texas Prison Admission Data in 2016                  #
#..............................................................................#

# Filters data by year, state, and race
chart2_data <- incarceration %>%
  filter(year == 2016) %>%
  filter(state == "TX") %>%
  
  # Excluded AAPI & Native American data due to missing/incomplete data
  select(state, 
         county_name, 
         white_prison_adm_rate, 
         latinx_prison_adm_rate, 
         black_prison_adm_rate, 
         total_prison_adm) 

# Selecting the top 10 counties with highest admission rate
  # There are too many TX counties to display them all. 
chart2_top5 <- chart2_data %>%
  
  # Using top 5 because 10 didn't format correctly in Rmd file--
  # it was too squishy and hard to read the county names. 
  top_n(5) %>%
  arrange(total_prison_adm) %>%
  gather(key = race, value = population, -county_name, -state, -total_prison_adm)


chart2_plot <- ggplot(chart2_top5) +
  geom_col(
    mapping = aes(x = county_name, y = population, fill = race), position = "dodge") +
  labs(
    title = "2016 Texas Prison Admissions by Race",
    subtitle = "In Top 5 Counties with Highest Prison Admission Rate",
    x = "County",
    y = "Prisoners Admitted in 2016")

chart2_plot



#..............................................................................#
#                             Chart 3: Mapping                                 #
#          Observing Geographic Patterns in 2016 Texas Jail Data               #
#..............................................................................# 
  
# Filters dataset by year, county, and population jailed Latinx prisoners
chart3_data <- incarceration %>%
  filter(year == "2016") %>%
  filter(state == "TX") %>%
  mutate(county = tolower(str_remove_all(county_name, " County")),
         # I chose to look at jail populations here due to limited/incomplete prison data
         latinx_jail = latinx_jail_pop) %>%
  select(county,latinx_jail)

TX_latinx_map <- map_data("county","texas") %>%
  rename(county = subregion) %>%
  left_join(chart3_data, by = "county")

# Minimalist theme required for the map. 
minimalist_theme <- theme_bw() +
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

# Generates state choropleth map displaying counties with highest jailed Latinx populations.
chart3_map <- ggplot(TX_latinx_map) +
  geom_polygon(mapping=aes(x = long,
                           y = lat,
                           group = group,
                           fill = latinx_jail),
               color = "white", 
               size = 0.25) +
  coord_map() +
  scale_fill_gradient("Jailed Latinx People",low="gray", high="red")+
  labs(x="Longitude", y="Latitude", title="Latinx Jail Population by County in 2016") +
  minimalist_theme

chart3_map
