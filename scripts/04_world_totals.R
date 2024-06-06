# Investigation of world totals:

# Stage 1: Load packages and data --------------------------------------------------------
library(tidyverse)
library(countrycode)
library(dplyr)

stunting <- read_csv('output-data/stunted_world_totals.csv')
anaemia <- read_csv('output-data/anaemia_world_totals.csv')

world_totals <- merge(stunting, anaemia)

# Stage 2: Investigate data visually --------------------------------------------------------

ggplot(world_totals, aes(x = year, y = Total_Births)) +
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=Total_anaemic_current_progress_rates, col = 'Rates fall at current pace'))+
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=Total_anaemic_2x_current_progress_rates, col = 'Rates fall 2x as fast'))+
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=Total_anaemic_constant_rates, col = 'Current rates'))+
  geom_line(aes(y=Total_anaemic, col = '2000-2019'))+
  labs(x = "", title = "Pregnant women anaemic, world, estimated", y="") +
  theme_minimal()+theme(legend.position = 'right', legend.title = element_blank())+expand_limits(y=0)

# Stats for text:
world_totals$Total_anaemic_constant_rates[world_totals$year == 2050]/world_totals$Total_Births[world_totals$year == 2050]
world_totals$Total_anaemic_constant_rates[world_totals$year == 2024]/world_totals$Total_Births[world_totals$year == 2024]
world_totals$Total_Stunted_constant_rates[world_totals$year == 2050]/world_totals$Total_Births[world_totals$year == 2050]
world_totals$Total_anaemic_current_progress_rates[world_totals$year == 2050]/world_totals$Total_Births[world_totals$year == 2050]
world_totals$Total_Stunted_current_progress_rates[world_totals$year == 2050]/world_totals$Total_Births[world_totals$year == 2050]
world_totals$Total_anaemic_2x_current_progress_rates[world_totals$year == 2050]/world_totals$Total_Births[world_totals$year == 2050]

# Total and % of those born times percentage under 5 stunted in those countries in 2000 and 2024:
world_totals$Total_Stunted_current_progress_rates[world_totals$year == 2024]*1000
world_totals$Total_Stunted_current_progress_rates[world_totals$year == 2024]/world_totals$Total_Births[world_totals$year == 2024]

world_totals$Total_Stunted_current_progress_rates[world_totals$year == 2000]*1000
world_totals$Total_Stunted_current_progress_rates[world_totals$year == 2000]/world_totals$Total_Births[world_totals$year == 2000]

# Anaemic mothers world rate in 2019:
world_totals$Total_anaemic_constant_rates[world_totals$year == 2019]/world_totals$Total_Births[world_totals$year == 2019]

