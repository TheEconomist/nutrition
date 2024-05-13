# Data generation script

# Stage 1: Load packages and data --------------------------------------------------------
library(tidyverse)
library(countrycode)
library(dplyr)

# Load UN World Population Prospects
wp <- read_csv('source-data/WPP2022_Demographic_Indicators_Medium.csv')
wp$iso3c <- wp$ISO3_code
wp$year <- wp$Time

# Load malnutrition data from the WHO Global Health Observatory
mal <- data.frame(read_csv('source-data/GHO_nutrition.csv'))

# Exclude surveys with different definitions from the WHO:
mal <- mal[is.na(mal$Data.Source), ]

# Clean column names
names(mal) <- gsub("Stunting.prevalence.among.children.under.5.years.of.age....height.for.age...2.SD...", "", names(mal))
names(mal) <- gsub(".survey.based.estimates", "Survey_Based", names(mal))
names(mal) <- gsub(".model.based.estimates", "Model_Based", names(mal))
names(mal) <- gsub("Countries..territories.and.areas", "Country", names(mal))
names(mal) <- make.names(names(mal))
names(mal)[names(mal) == "Country"] <- "country"
names(mal)[names(mal) == "Year"] <- "year"

# Define the function to extract estimates and bounds
extract_estimate_and_range <- function(x) {
  if (is.na(x)) return(NA)  # Return NA if the input is NA
  main_estimate <- as.numeric(str_extract(x, "^[\\d\\.]+"))
  range <- str_extract(x, "\\[.*\\]")
  lower_bound <- as.numeric(str_extract(range, "[\\d\\.]+(?=-)"))
  upper_bound <- as.numeric(str_extract(range, "(?<=-)[\\d\\.]+"))
  c(main_estimate, lower_bound, upper_bound)
}

mal$estimate <- mal$estimate_low <- mal$estimate_high <- NA
mal$type <- 'survey'
for(i in 1:nrow(mal)){
  mal[i, c('estimate', 'estimate_low', 'estimate_high')] <- extract_estimate_and_range(mal$survey.based.estimates[i])
}
for(i in which(is.na(mal$estimate))){
  mal$type[i] <- 'modelled'
  mal[i, c('estimate', 'estimate_low', 'estimate_high')] <- extract_estimate_and_range(mal$model.based.estimates[i])
}

mal$iso3c <- countrycode(mal$country, 'country.name', 'iso3c')

# Merge the two:
dat <- merge(wp, mal, by=c('iso3c', 'year'), all = T)

# Stage 2: Rough estimate --------------------------------------------------------
# Plot data:
ggplot(dat[dat$year %in% 2000:2022, ], aes(x=year, y=estimate, group = iso3c))+geom_line(alpha = 0.2)+geom_line(data=dat[dat$year %in% 2000:2022 & dat$iso3c %in% dat$iso3c[dat$TPopulation1Jan > 200000], ], aes(col=country), size = 2)+theme(legend.title = element_blank())+xlab('Estimated stunting rates, by country')+ylab('')

# Project using existing rates
dat <- dat[!is.na(dat$iso3c), ]
for(i in unique(dat$iso3c)){
  dat$estimate[dat$year > 2022 & dat$iso3c == i] <- dat$estimate[dat$year == 2022 & dat$iso3c == i]
  dat$estimate_low[dat$year > 2022 & dat$iso3c == i] <- dat$estimate_low[dat$year == 2022 & dat$iso3c == i]
  dat$estimate_high[dat$year > 2022 & dat$iso3c == i] <- dat$estimate_high[dat$year == 2022 & dat$iso3c == i]
  if(is.na(dat$estimate[dat$iso3c == i & dat$year == 2022])){
    dat$estimate[dat$iso3c == i] <- 2
    dat$estimate_low[dat$iso3c == i] <- 2
    dat$estimate_high[dat$iso3c == i] <- 2
  }
}

dat$stunted_kids_rough_estimate <- dat$Births*dat$estimate/100
dat$stunted_kids_rough_estimate_low <- dat$Births*dat$estimate_low/100
dat$stunted_kids_rough_estimate_high <- dat$Births*dat$estimate_high/100


world_totals <- dat[!is.na(dat$iso3c), ] %>%
  group_by(year) %>%
  summarise(
    Total_Births = sum(Births, na.rm = TRUE),  # Summing up all births, removing NA values
    Total_Deaths = sum(Deaths, na.rm = TRUE),   # Summing up all deaths, removing NA values
    Total_Stunted = sum(stunted_kids_rough_estimate, na.rm = T),
    Total_Stunted_low = sum(stunted_kids_rough_estimate_low, na.rm = T),
    Total_Stunted_high = sum(stunted_kids_rough_estimate_high, na.rm = T)

  )

world_totals <- world_totals[world_totals$year %in% 2000:2100, ]
ggplot(world_totals, aes(x=year, y=Total_Stunted))+geom_line()+
  geom_line(aes(y=Total_Stunted_low), col='gray')+
  geom_line(aes(y=Total_Stunted_high), col='gray')+theme_minimal()

ggplot(world_totals, aes(x=year, y=Total_Stunted/Total_Births))+geom_ribbon(aes(ymin=Total_Stunted_low/Total_Births, ymax=Total_Stunted_high/Total_Births), fill ='gray')+geom_line()+xlab('Proportion of children stunted (rough estimate), World')

ggplot(world_totals, aes(x=year, y=Total_Stunted/Total_Births))+geom_line()


head(world_total)

# Stage 1: Load packages and data --------------------------------------------------------
# Stage 1: Load packages and data --------------------------------------------------------
