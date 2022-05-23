# Packages
library(tidyverse)

#------------------------------------------------------------------------------
# Import World Bank Data
#------------------------------------------------------------------------------
# Data obtained from World Bank Open Data:
# https://data.worldbank.org/

# World Population per Country
world_pop <- read_csv('data/API_SP.POP.TOTL_DS2_en_csv_v2_4019998.csv',
                      skip = 3,
                      col_select = c(
                        -`Country Code`,-`Indicator Name`,-`Indicator Code`))

# World Women Population per Country
world_w_pop <- read_csv('data/API_SP.POP.TOTL.FE.IN_DS2_en_csv_v2_4030181.csv',
                      skip = 3,
                      col_select = c(
                        -`Country Code`,-`Indicator Name`,-`Indicator Code`))

# Birth Rates per Country
world_births <- read_csv('data/API_SP.DYN.CBRT.IN_DS2_en_csv_v2_4030067.csv',
                         skip = 3,
                         col_select = c(
                           -`Country Code`,-`Indicator Name`,-`Indicator Code`))

# Fertility Rate (births per woman) per Country
world_fertility <- read_csv('data/API_SP.DYN.TFRT.IN_DS2_EN_csv_v2_4023233.csv',
                         skip = 3,
                         col_select = c(
                           -`Country Code`,-`Indicator Name`,-`Indicator Code`))

# Death Rates per Country
world_deaths <- read_csv('data/API_SP.DYN.CDRT.IN_DS2_en_csv_v2_4030068.csv',
                         skip = 3,
                         col_select = c(
                           -`Country Code`,-`Indicator Name`,-`Indicator Code`))

#------------------------------------------------------------------------------
# Tidy Data
#------------------------------------------------------------------------------

# Function to Modify Tibble
modify_pop_df <- function(data.frame, threshold, opti){
  
  a <- data.frame %>%
    select(-`2021`,-`...67`) %>% 
    pivot_longer(cols = -c(`Country Name`),
                 names_to = 'year',
                 values_to = 'population') %>% 
    mutate(year = as.double(year)) %>% 
    
    left_join(world_births %>%
                select(-`2021`,-`...67`) %>% 
                pivot_longer(cols = -c(`Country Name`),
                             names_to = 'year',
                             values_to = 'births_1k') %>% 
                mutate(year = as.double(year)),
              by = c('Country Name','year')) %>% 
    mutate(births = round(population * (births_1k/1000)),
           birth_rate = births_1k/1000) %>% 
    
    left_join(world_deaths %>%
                select(-`2021`,-`...67`) %>% 
                pivot_longer(cols = -c(`Country Name`),
                             names_to = 'year',
                             values_to = 'deaths_1k') %>% 
                mutate(year = as.double(year)),
              by = c('Country Name','year')) %>% 
    mutate(deaths = round(population * (deaths_1k/1000)),
           death_rate = deaths_1k/1000)
  
  b <-
    if(missing(threshold)) {
      a %>% 
        left_join(
           y=data.frame %>% 
             select(-`2021`,-`...67`) %>% 
             pivot_longer(cols = -c(`Country Name`),
                          names_to = 'year',
                          values_to = 'population') %>% 
             mutate(year = as.double(year)) %>% 
             group_by(`Country Name`) %>% 
             summarise(pop_min = min(population)),
           by='Country Name'
         ) %>% 
         mutate(pop_change = population/pop_min,
                pop_growth = population-pop_min) %>% return()
    } else {
      a %>% 
        filter(year >= threshold) %>%
                 left_join(
                   y=data.frame %>% 
                     select(-`2021`,-`...67`) %>% 
                     pivot_longer(cols = -c(`Country Name`),
                                  names_to = 'year',
                                  values_to = 'population') %>% 
                     mutate(year = as.double(year)) %>% 
                     filter(year >= threshold) %>% 
                     group_by(`Country Name`) %>% 
                     summarise(pop_min = min(population)),
                   by='Country Name'
                 ) %>% 
                 mutate(pop_change = population/pop_min,
                        pop_growth = population-pop_min) %>% return()}
  
  if(missing(opti)) {
    return(b)
  } else {
    if(deparse(substitute(opti)) == "sa") {
      b %>% 
        filter(`Country Name` == 'Mozambique' |
                 `Country Name` == 'South Africa' |
                 `Country Name` == 'Botswana' |
                 `Country Name` == 'Namibia' |
                 `Country Name` == 'Angola' |
                 `Country Name` == 'Zimbabwe' |
                 `Country Name` == 'Eswatini' |
                 `Country Name` == 'Zambia' |
                 `Country Name` == 'Malawi' |
                 `Country Name` == 'Lesotho') %>% 
        return()
    } else {"Error: incorrect argument"}
  }
}

# Tibbles of World and Southern African Countries
world_pop_mod <- modify_pop_df(world_pop)
sa_pop_mod <- modify_pop_df(world_pop, 1, sa)
sa_pop_mod_95 <- modify_pop_df(world_pop, 1995, sa)

write.csv(world_pop_mod, "data/world_pop_mod.csv", row.names=FALSE)
write.csv(sa_pop_mod, "data/sa_pop_mod.csv", row.names=FALSE)
write.csv(sa_pop_mod_95, "data/sa_pop_mod_95.csv", row.names=FALSE)

write.csv(sa_pop_mod %>% 
            filter(`Country Name` == 'Mozambique') %>% 
            select(-`Country Name`),
          "data/moz_pop_mod.csv", row.names=FALSE)

# Births per Woman
world_fertility_mod <- world_fertility %>% 
  select(-`2021`,-`...67`) %>% 
  pivot_longer(cols = -c(`Country Name`),
               names_to = 'year',
               values_to = 'fertility_rate') %>% 
  mutate(year = as.double(year))

sa_fertility_mod <-
  world_fertility_mod[world_fertility_mod$`Country Name` == 'Mozambique' |
                        world_fertility_mod$`Country Name` == 'South Africa' |
                        world_fertility_mod$`Country Name` == 'Botswana' |
                        world_fertility_mod$`Country Name` == 'Namibia' |
                        world_fertility_mod$`Country Name` == 'Angola' |
                        world_fertility_mod$`Country Name` == 'Zimbabwe' |
                        world_fertility_mod$`Country Name` == 'Eswatini' |
                        world_fertility_mod$`Country Name` == 'Zambia' |
                        world_fertility_mod$`Country Name` == 'Malawi' |
                        world_fertility_mod$`Country Name` == 'Lesotho',]

sa_fertility_mod_95 <- sa_fertility_mod %>% 
  filter(year >= 1995)

moz_fertility_mod_95 <- world_fertility_mod %>% 
  filter(`Country Name` == 'Mozambique',
         year >= 1995) %>% 
  select(year, fertility_rate)

write.csv(moz_fertility_mod_95,
          "data/moz_fertility_mod_95.csv", row.names=FALSE)

#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------

# Population evolution of 10 SSA countries
plot_sa_pop <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=population, color = `Country Name`)) +
  geom_line()

ggsave("figures/plot_sa_pop.png",
       plot_sa_pop, width = 8, height = 4)

plot_sa_pop_change <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=pop_change, color = `Country Name`)) +
  geom_line() +
  ylab("Population/Initial population")

ggsave("figures/plot_sa_pop_change.png",
       plot_sa_pop_change, width = 8, height = 4)

plot_sa_pop_growth <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=pop_growth, color = `Country Name`)) +
  geom_line()

plot_sa_fertility_rate <- sa_fertility_mod %>% 
  ggplot(aes(x=year, y=fertility_rate, color = `Country Name`)) +
  geom_line() +
  ylab("Fertility Rate (births per woman)")

ggsave("figures/plot_sa_fertility_rate.png",
       plot_sa_fertility_rate, width = 8, height = 4)

plot_sa_births_1k <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=births_1k, color = `Country Name`)) +
  geom_line() +
  ylab("Birth rate, crude (per 1,000 people)")

ggsave("figures/plot_sa_births_1k.png",
       plot_sa_births_1k, width = 8, height = 4)

plot_sa_deaths_1k <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=deaths_1k, color = `Country Name`)) +
  geom_line() +
  ylab("Death rate, crude (per 1,000 people)")

ggsave("figures/plot_sa_deaths_1k.png",
       plot_sa_deaths_1k, width = 8, height = 4)

## SINCE 1995
plot_sa_pop_change_95 <- sa_pop_mod_95 %>% 
  ggplot(aes(x=year, y=pop_change, color = `Country Name`)) +
  geom_line() +
  ylab("Population/Initial population")

ggsave("figures/plot_sa_pop_change_95.png",
       plot_sa_pop_change_95, width = 8, height = 4)

plot_sa_fertility_rate_95 <- sa_fertility_mod_95 %>% 
  ggplot(aes(x=year, y=fertility_rate, color = `Country Name`)) +
  geom_line() +
  ylab("Fertility Rate (births per woman)")

ggsave("figures/plot_sa_fertility_rate_95.png",
       plot_sa_fertility_rate_95, width = 8, height = 4)

plot_sa_births_1k_95 <- sa_pop_mod_95 %>% 
  ggplot(aes(x=year, y=births_1k, color = `Country Name`)) +
  geom_line() +
  ylab("Birth rate, crude (per 1,000 people)")

ggsave("figures/plot_sa_births_1k_95.png",
       plot_sa_births_1k_95, width = 8, height = 4)

plot_sa_deaths_1k_95 <- sa_pop_mod_95 %>% 
  ggplot(aes(x=year, y=deaths_1k, color = `Country Name`)) +
  geom_line() +
  ylab("Death rate, crude (per 1,000 people)")

ggsave("figures/plot_sa_deaths_1k_95.png",
       plot_sa_deaths_1k_95, width = 8, height = 4)

# Mozambique
plot_moz_pop_change_95 <- sa_pop_mod_95 %>% 
  filter(`Country Name` == 'Mozambique') %>% 
  ggplot(aes(x=year, y=pop_change, color = `Country Name`)) +
  geom_line() +
  ylab("Population/Initial population")

ggsave("figures/plot_moz_pop_change_95.png",
       plot_moz_pop_change_95, width = 8, height = 4)

plot_moz_fertility_rate_95 <- sa_fertility_mod_95 %>% 
  filter(`Country Name` == 'Mozambique') %>% 
  ggplot(aes(x=year, y=fertility_rate, color = `Country Name`)) +
  geom_line() +
  ylab("Fertility Rate (births per woman)")

ggsave("figures/plot_moz_fertility_rate_95.png",
       plot_moz_fertility_rate_95, width = 8, height = 4)

plot_moz_births_1k_95 <- sa_pop_mod_95 %>% 
  filter(`Country Name` == 'Mozambique') %>% 
  ggplot(aes(x=year, y=births_1k, color = `Country Name`)) +
  geom_line() +
  ylab("Birth rate, crude (per 1,000 people)")

ggsave("figures/plot_moz_births_1k_95.png",
       plot_moz_births_1k_95, width = 8, height = 4)

plot_moz_deaths_1k_95 <- sa_pop_mod_95 %>% 
  filter(`Country Name` == 'Mozambique') %>% 
  ggplot(aes(x=year, y=deaths_1k, color = `Country Name`)) +
  geom_line() +
  ylab("Death rate, crude (per 1,000 people)")

ggsave("figures/plot_moz_deaths_1k_95.png",
       plot_moz_deaths_1k_95, width = 8, height = 4)
