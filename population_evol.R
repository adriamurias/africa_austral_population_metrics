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

# Birth Rates per Country
world_births <- read_csv('data/API_SP.DYN.TFRT.IN_DS2_EN_csv_v2_4023233.csv',
                         skip = 3,
                         col_select = c(
                           -`Country Code`,-`Indicator Name`,-`Indicator Code`))

#------------------------------------------------------------------------------
# Tidy Data
#------------------------------------------------------------------------------

# Population
sa_pop <- world_pop[world_pop$`Country Name` == 'Mozambique' |
                      world_pop$`Country Name` == 'South Africa' |
                      world_pop$`Country Name` == 'Botswana' |
                      world_pop$`Country Name` == 'Namibia' |
                      world_pop$`Country Name` == 'Angola' |
                      world_pop$`Country Name` == 'Zimbabwe' |
                      world_pop$`Country Name` == 'Eswatini' |
                      world_pop$`Country Name` == 'Zambia' |
                      world_pop$`Country Name` == 'Malawi' |
                      world_pop$`Country Name` == 'Lesotho',] %>% 
  select(-`2021`,-`...67`) %>% 
  pivot_longer(cols = -c(`Country Name`),
               names_to = 'year',
               values_to = 'population') %>% 
  mutate(year = as.double(year))

sa_pop_mod <- sa_pop %>% 
  left_join(
    y=sa_pop %>% 
      group_by(`Country Name`) %>% 
      summarise(pop_min = min(population)),
    by='Country Name'
  ) %>% 
  mutate(pop_change = population/pop_min,
         pop_growth = population-pop_min)

# row.names(sa_pop) <- sa_pop %>% pull(`Country Name`)
# 
# sa_pop_transp <- t(sa_pop)[-1,] %>% data.frame()

# Birth Rate
sa_births <- world_births[world_births$`Country Name` == 'Mozambique' |
                            world_births$`Country Name` == 'South Africa' |
                            world_births$`Country Name` == 'Botswana' |
                            world_births$`Country Name` == 'Namibia' |
                            world_births$`Country Name` == 'Angola' |
                            world_births$`Country Name` == 'Zimbabwe' |
                            world_births$`Country Name` == 'Eswatini' |
                            world_births$`Country Name` == 'Zambia' |
                            world_births$`Country Name` == 'Malawi' |
                            world_births$`Country Name` == 'Lesotho',] %>% 
  select(-`2021`,-`...67`)

sa_births_mod <- sa_births %>% 
  pivot_longer(cols = -c(`Country Name`),
               names_to = 'year',
               values_to = 'births per woman') %>% 
  mutate(year = as.double(year))

#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------

# Population evolution of 10 SSA countries
plot_sa_pop <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=population, color = `Country Name`)) +
  geom_line()

plot_sa_pop_change <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=pop_change, color = `Country Name`)) +
  geom_line()

plot_sa_pop_growth <- sa_pop_mod %>% 
  ggplot(aes(x=year, y=pop_growth, color = `Country Name`)) +
  geom_line()

# Fertility rate (births per woman) evolution of 10 SSA countries
plot_sa_births <- sa_births_mod %>% 
  ggplot(aes(x=year, y=`births per woman`, color = `Country Name`)) +
  geom_line()

## SINCE 1995
# Population evolution of 10 SSA countries
plot_sa_pop_change_95 <- sa_pop_mod %>% 
  filter(year >= 1995) %>% 
  ggplot(aes(x=year, y=pop_change, color = `Country Name`)) +
  geom_line()

# Fertility rate (births per woman) evolution of 10 SSA countries
plot_sa_births_95 <- sa_births_mod %>% 
  filter(year >= 1995) %>% 
  ggplot(aes(x=year, y=`births per woman`, color = `Country Name`)) +
  geom_line()

#------------------------------------------------------------------------------
# Adjust for Moz
#------------------------------------------------------------------------------

# Population evolution of 10 SSA countries
plot_moz_pop_95 <- sa_pop_mod %>% 
  filter(year >= 1995 & `Country Name` == 'Mozambique') %>% 
  ggplot(aes(x=year, y=population, color = `Country Name`)) +
  geom_line() + 
  geom_smooth(method='lm', formula= y~x)
