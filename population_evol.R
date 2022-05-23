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

# World Population per Country
world_w_pop <- read_csv('data/API_SP.POP.TOTL.FE.IN_DS2_en_csv_v2_4030181.csv',
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

# Function to Modify Tibble
modify_pop_df <- function(data.frame, threshold, opti){
  
  a <- data.frame %>%
    select(-`2021`,-`...67`) %>% 
    pivot_longer(cols = -c(`Country Name`),
                 names_to = 'year',
                 values_to = 'population') %>% 
    mutate(year = as.double(year))
  
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
world_pop_mod <- modify_pop_df(world_pop) %>% 
  left_join(modify_pop_df(world_w_pop) %>% 
              mutate(w_pop = population) %>% 
              select(`Country Name`, year, w_pop),
            by = c("Country Name", "year")) %>% 
  mutate(w_prop = w_pop/population)
sa_pop_mod <- modify_pop_df(world_pop, 1, sa)
sa_w_pop_mod <- modify_pop_df(world_w_pop, 1, sa)
sa_pop_mod_95 <- modify_pop_df(world_pop, 1995, sa)
sa_w_pop_mod_95 <- modify_pop_df(world_w_pop, 1995, sa)

moz_pop_mod_95 <- sa_pop_mod_95 %>% 
  filter(`Country Name` == 'Mozambique') %>% 
  select(year, population)
write.csv(moz_pop_mod_95, "data/moz_pop_mod_95.csv", row.names=FALSE)

moz_w_pop_mod_95 <- world_pop_mod %>% 
  filter(`Country Name` == 'Mozambique',
         year >= 1995) %>% 
  select(year, population, w_pop)
write.csv(moz_w_pop_mod_95, "data/moz_w_pop_mod_95.csv", row.names=FALSE)

# row.names(sa_pop) <- sa_pop %>% pull(`Country Name`)
# 
# sa_pop_transp <- t(sa_pop)[-1,] %>% data.frame()

# Birth Rate
world_births_mod <- world_births %>% 
  select(-`2021`,-`...67`) %>% 
  pivot_longer(cols = -c(`Country Name`),
               names_to = 'year',
               values_to = 'births per woman') %>% 
  mutate(year = as.double(year))

sa_births_mod <-
  world_births_mod[world_births_mod$`Country Name` == 'Mozambique' |
                     world_births_mod$`Country Name` == 'South Africa' |
                     world_births_mod$`Country Name` == 'Botswana' |
                     world_births_mod$`Country Name` == 'Namibia' |
                     world_births_mod$`Country Name` == 'Angola' |
                     world_births_mod$`Country Name` == 'Zimbabwe' |
                     world_births_mod$`Country Name` == 'Eswatini' |
                     world_births_mod$`Country Name` == 'Zambia' |
                     world_births_mod$`Country Name` == 'Malawi' |
                     world_births_mod$`Country Name` == 'Lesotho',]

moz_births_mod_95 <- world_births_mod %>% 
  filter(`Country Name` == 'Mozambique',
         year >= 1995) %>% 
  select(year, `births per woman`)
write.csv(moz_births_mod_95, "data/moz_births_mod_95.csv", row.names=FALSE)

# Moz
moz_pop_births_w <- world_pop_mod %>% 
  filter(`Country Name` == 'Mozambique') %>% 
  select(year, population, w_pop) %>% 
  left_join(world_births_mod %>% 
              filter(`Country Name` == 'Mozambique') %>% 
              select(year, `births per woman`),
            by = 'year') %>% 
  mutate(births = round(w_pop * `births per woman`))

write.csv(moz_pop_births_w, "data/moz_pop_births_w.csv", row.names=FALSE)

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

ggsave("figures/plot_sa_pop_growth.png",
       plot_sa_pop_growth, width = 8, height = 4)

# Fertility rate (births per woman) evolution of 10 SSA countries
plot_sa_births <- sa_births_mod %>% 
  ggplot(aes(x=year, y=`births per woman`, color = `Country Name`)) +
  geom_line()

ggsave("figures/plot_sa_births.png",
       plot_sa_births, width = 8, height = 4)

## SINCE 1995
# Population evolution of 10 SSA countries
plot_sa_pop_change_95 <- sa_pop_mod_95 %>% 
  ggplot(aes(x=year, y=pop_change, color = `Country Name`)) +
  geom_line()

ggsave("figures/plot_sa_pop_change_95.png",
       plot_sa_pop_change_95, width = 8, height = 4)

# Fertility rate (births per woman) evolution of 10 SSA countries
plot_sa_births_95 <- sa_births_mod %>% 
  filter(year >= 1995) %>% 
  ggplot(aes(x=year, y=`births per woman`, color = `Country Name`)) +
  geom_line()

ggsave("figures/plot_sa_births_95.png",
       plot_sa_births_95, width = 8, height = 4)

# Mozambique
plot_moz_pop <- moz_pop_births_w %>% 
  ggplot(aes(x=year, y=population)) +
  geom_line()

ggsave("figures/plot_moz_pop.png",
       plot_moz_pop, width = 8, height = 4)

plot_moz_births_w <- moz_pop_births_w %>% 
  ggplot(aes(x=year, y=`births per woman`)) +
  geom_line()

ggsave("figures/plot_moz_births_w.png",
       plot_moz_births_w, width = 8, height = 4)

plot_moz_births <- moz_pop_births_w %>% 
  ggplot(aes(x=year, y=births)) +
  geom_line()

ggsave("figures/moz_pop_births.png",
       plot_moz_births, width = 8, height = 4)
