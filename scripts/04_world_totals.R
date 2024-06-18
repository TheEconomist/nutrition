# Investigation of world totals:
options(scipen = 999)

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


# Stage 4: Lives affected over time  --------------------------------------------------------

# Calculate cumulative sums
world_totals <- world_totals[order(world_totals$year), ]

world_totals$Cumulative_Stunted_constant_rates <- cumsum(world_totals$Total_Stunted_constant_rates)
world_totals$Cumulative_Stunted_current_progress <- cumsum(world_totals$Total_Stunted_current_progress_rates)
world_totals$Cumulative_Stunted_2x_current_progress <- cumsum(world_totals$Total_Stunted_2x_current_progress_rates)
world_totals$Cumulative_Anaemic_constant_rates <- cumsum(world_totals$Total_anaemic_constant_rates)
world_totals$Cumulative_Anaemic_current_progress <- cumsum(world_totals$Total_anaemic_current_progress_rates)
world_totals$Cumulative_Anaemic_2x_current_progress <- cumsum(world_totals$Total_anaemic_2x_current_progress_rates)

# Write data:
write_csv(world_totals, 'output-data/world_totals.csv')

# Plot the data
library(ggplot2)
world_totals <- read_csv('output-data/world_totals.csv')
ggplot(world_totals[world_totals$year >= 2019, ], aes(x = year)) +
  geom_line(aes(y = 1000*(Cumulative_Stunted_current_progress-Cumulative_Stunted_constant_rates)/1e6,
                color = "Stunting - Current Progress Rates")) +
  geom_line(aes(y = 1000*(Cumulative_Stunted_2x_current_progress-Cumulative_Stunted_constant_rates)/1e6,
                color = "Stunting - 2x Current Progress Rates")) +
  labs(title = "Cumulative total kids affected over time for different scenarios,\ncompared to rates staying what they are now, in millions",
       x = "",
       y = "") +
  theme_minimal()+theme(legend.title = element_blank())
ggsave('plots/cumulative_stunting.png')


ggplot(world_totals[world_totals$year >= 2019, ], aes(x = year)) +
  geom_line(aes(y = 1000*(Cumulative_Anaemic_current_progress-Cumulative_Anaemic_constant_rates)/1e6,
                color = "Anaemic - Current Progress Rates")) +
  geom_line(aes(y = 1000*(Cumulative_Anaemic_2x_current_progress-Cumulative_Anaemic_constant_rates)/1e6,
                color = "Anaemic - 2x Current Progress Rates")) +
  labs(title = "Cumulative total kids affected over time for different scenarios,\ncompared to rates staying what they are now, in millions",
       x = "",
       y = "") +
  theme_minimal()+theme(legend.title = element_blank())
ggsave('plots/cumulative_anaemia.png')



