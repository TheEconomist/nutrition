# Investigations and projections of anaemia

# Stage 1: Load packages and data --------------------------------------------------------
library(tidyverse)
library(countrycode)
library(dplyr)

# Load UN World Population Prospects
wp <- read_csv('source-data/WPP2022_Demographic_Indicators_Medium.csv')
wp$iso3c <- wp$ISO3_code
wp$year <- wp$Time
wp <- wp %>% select(iso3c, year, Births, PopChange, TPopulation1July, TPopulationFemale1July, TPopulationMale1July, NetMigrations, Deaths, InfantDeaths, Under5Deaths)

# Load malnutrition data from the WHO Global Health Observatory
ana <- data.frame(read_csv('source-data/GHO_anaemia.csv'))

# Exclude surveys with different definitions from the WHO:
ana <- ana[is.na(ana$DataSource), ]

# Clean column names
ana <- ana %>% mutate(year = Period,
                      estimate_low = FactValueNumericLow,
                      estimate = FactValueNumeric,
                      estimate_high = FactValueNumericHigh,
                      country = Location,
                      iso3c = countrycode(Location, 'country.name', 'iso3c')) %>%
  select(year, country, iso3c, estimate, estimate_low, estimate_high)

# Updated function to handle inputs with and without spaces around the dash in bounds
extract_estimate_and_range <- function(x) {
  if (is.na(x)) return(NA)  # Return NA if the input is NA

  # Extract the main estimate and bounds using regex to handle optional spaces
  main_estimate <- as.numeric(str_extract(x, "^\\s*\\d+\\.?\\d*"))
  bounds <- as.numeric(unlist(str_match(x, "\\[\\s*(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*)\\s*\\]")[, -1]))

  # Assign lower and upper bounds from captured groups
  lower_bound <- bounds[1]
  upper_bound <- bounds[2]

  # Return a vector with the main estimate, lower, and upper bounds
  c(main_estimate, lower_bound, upper_bound)
}

# Merge the two:
dat <- merge(wp, ana, by=c('iso3c', 'year'), all = T)
dat <- dat[!is.na(dat$iso3c), ]

# Get regional estimates:
ana_regions <- data.frame(read_csv('source-data/GHO_anaemia_regions.csv'))
ana_regions <- ana_regions %>% mutate(region = UN.Region, anaemia_rate = Prevalence.of.anaemia.in.pregnant.women..aged.15.49....., year = Year) %>% select(region, year, anaemia_rate)
ana_regions$region[ana_regions$region == "Northern America (21)"] <- "Northern America"
for(i in 1:nrow(ana_regions)){
  ana_regions[i, c('region_estimate', 'region_estimate_low', 'region_estimate_high')] <- extract_estimate_and_range(ana_regions$anaemia_rate[i])
}

# Use regional estimate if national not available (this predominantly affects high-income countries where rates are very low, or tiny island states).
dat$region <- countrycode(dat$iso3c, 'iso3c', 'un.regionsub.name')
dat$region[dat$iso3c == 'TWN'] <- 'Eastern Asia'
dat$region[dat$iso3c == 'XKX'] <- 'Southern Europe'
dat <- merge(dat, ana_regions, by= c('year', 'region'), all.x= T)

dat$estimate[is.na(dat$estimate) & dat$year < 2020] <- dat$region_estimate[is.na(dat$estimate)]
dat$estimate_low[is.na(dat$estimate_low) & dat$year < 2020] <- dat$region_estimate_low[is.na(dat$estimate_low)]
dat$estimate_high[is.na(dat$estimate_high) & dat$year < 2020] <- dat$region_estimate_high[is.na(dat$estimate_high)]
unique(dat$iso3c[is.na(dat$estimate) & dat$year == 2019])

# Stage 2: Plot this data --------------------------------------------------------

# Plot data:
ggplot(dat[dat$year %in% 2000:2019, ], aes(x=year, y=estimate, group = iso3c))+geom_line(alpha = 0.2)+geom_line(data=dat[dat$year %in% 2000:2019 & dat$iso3c %in% dat$iso3c[dat$TPopulation1July > 200000], ], aes(col=country, linetype=country), size = 2)+theme_minimal()+theme(legend.title = element_blank())+xlab('')+ggtitle('Estimated anaemia rates in pregnant women, by country, %')+ylab('')
ggsave('plots/anaemia_rates_by_country.png', width = 6, height = 6)

# Stage 3: Projection using current rates --------------------------------------------------------

# Ensure data is filtered properly for non-NA ISO codes
dat <- dat[!is.na(dat$iso3c), ]

# Project future estimates based on 2019 values
dat <- dat %>%
  group_by(iso3c) %>%
  mutate(
    projected_estimate_constant_rates = if_else(year > 2019, first(estimate[year == 2019]), estimate),
    projected_estimate_constant_rates_low = if_else(year > 2019, first(estimate_low[year == 2019]), estimate_low),
    projected_estimate_constant_rates_high = if_else(year > 2019, first(estimate_high[year == 2019]), estimate_high)
  ) %>%
  ungroup()

# Calculate anaemic Pregnant women estimates
dat <- dat %>%
  mutate(
    anaemic_kids = Births * estimate/100,
    anaemic_kids_low = Births * estimate_low/100,
    anaemic_kids_high = Births * estimate_high/100,
    anaemic_kids_constant_rates = Births * projected_estimate_constant_rates / 100,
    anaemic_kids_constant_rates_low = Births * projected_estimate_constant_rates_low / 100,
    anaemic_kids_constant_rates_high = Births * projected_estimate_constant_rates_high / 100
  )

# Summarize world totals by year
world_totals <- dat %>%
  filter(year %in% 2000:2050) %>%
  group_by(year) %>%
  summarise(
    Total_Births = sum(Births, na.rm = TRUE),
    Total_anaemic_constant_rates = sum(anaemic_kids_constant_rates, na.rm = TRUE),
    Total_anaemic_low_constant_rates = sum(anaemic_kids_constant_rates_low, na.rm = TRUE),
    Total_anaemic_high_constant_rates = sum(anaemic_kids_constant_rates_high, na.rm = TRUE),
    .groups = 'drop'
  )

# Additional line plot for proportions at constant rates
ggplot(world_totals, aes(x = year, y = Total_anaemic_constant_rates / Total_Births)) +
  geom_line() +
  labs(x = "Year", y = "Proportion of anaemic Pregnant women (Constant Rates)")

# Stage 3: Projection assuming current progress continues --------------------------------------------------------
library(betareg)

dat$estimate_01 <- dat$estimate / 100
dat$year_squared <- dat$year*dat$year

# Quasi-binomial distribution fit:
train <- dat[dat$year %in% 2005:2010 & !is.na(dat$estimate), ]
glm_model <- glm(estimate_01 ~ as.factor(iso3c)*year, data = train, family = quasi(link = "logit", variance = "mu(1-mu)"), weights = Births)

# Check calibration:
ggplot()+geom_point(aes(x=train$estimate_01, y=glm_model$fitted.values), alpha = 0.2)+geom_abline(aes(intercept = 0, slope =1))

# Check out-of-sample predictions:
pred <- data.frame(
  dat[dat$year %in% 2011:2019 & !is.na(dat$estimate), c('estimate_01', 'Births')],
  pred =
    predict(glm_model, newdata = dat[dat$year %in% 2011:2019 & !is.na(dat$estimate), ], type = 'response'))

ggplot(pred, aes(x=estimate_01, y=pred, size = Births))+geom_point(alpha = 0.1)+
  geom_abline(aes(intercept = 0, slope =1))
summary(pred)
cor(pred)

# Generating predictions for future years using GLM model
train <- dat[dat$year %in% 2009:2019 & !is.na(dat$estimate), ]
glm_model <- glm(estimate_01 ~ as.factor(iso3c)*year, data = train, family = quasi(link = "logit", variance = "mu(1-mu)"), weights = Births)


dat <- dat %>%
  mutate(
    projected_estimate_current_progress_rates = predict(glm_model, newdata = dat[, ], type = "response")*100
  ) %>%
  mutate(
    anaemic_kids_projected_current_progress_rates = Births * (projected_estimate_current_progress_rates/100)
  )

# Summarize world totals by year based on the new projections
world_totals <- dat %>%
  filter(year %in% 2000:2050) %>%
  group_by(year) %>%
  summarise(
    Total_Births = sum(Births, na.rm = TRUE),
    Total_anaemic = sum(anaemic_kids),
    Total_anaemic_low = sum(anaemic_kids_low),
    Total_anaemic_high = sum(anaemic_kids_high),
    Total_anaemic_constant_rates = sum(anaemic_kids_constant_rates, na.rm = TRUE),
    Total_anaemic_low_constant_rates = sum(anaemic_kids_constant_rates_low, na.rm = TRUE),
    Total_anaemic_high_constant_rates = sum(anaemic_kids_constant_rates_high, na.rm = TRUE),
    Total_anaemic_current_progress_rates = sum(anaemic_kids_projected_current_progress_rates, na.rm = TRUE),
    .groups = 'drop'
  )
# Plotting the model projections
ggplot(world_totals, aes(x = year, y = Total_Births)) +
  geom_line(aes(y=Total_anaemic_current_progress_rates/Total_Births))+
  labs(x = "Year", y = "Total anaemic Pregnant women (Model-Based Projection)") +
  theme_minimal()

# Finally, what if progress was twice as fast?

# First check that every country is (projected to be) on the right track:
change <- dat[dat$year %in% c(2019, 2030), c('iso3c', 'projected_estimate_current_progress_rates', 'year')]
change <- change[order(change$year), ]

# Some countries are not, it appears.

# Cycle through countries, mindful that some countries appear to be getting worse:
dat$anaemia_in_2019 <- NA
for(i in unique(dat$iso3c)){
  dat$anaemia_in_2019[dat$iso3c == i] <- dat$estimate[dat$year == 2019 & dat$iso3c == i]
}
dat$projected_estimate_2x_current_progress_rates <- ifelse(dat$year > 2019 & dat$projected_estimate_current_progress_rates - dat$anaemia_in_2019 < 0,
                                                           dat$anaemia_in_2019 - 2*(dat$anaemia_in_2019-dat$projected_estimate_current_progress_rates),
                                                           dat$projected_estimate_constant_rates)
dat$anaemic_kids_projected_2x_current_progress_rates <- dat$Births*dat$projected_estimate_2x_current_progress_rates/100

# Summarize world totals by year based on the new projections
world_totals <- dat %>%
  filter(year %in% 2000:2050) %>%
  group_by(year) %>%
  summarise(
    Total_Births = sum(Births, na.rm = TRUE),
    Total_anaemic = sum(anaemic_kids),
    Total_anaemic_low = sum(anaemic_kids_low),
    Total_anaemic_high = sum(anaemic_kids_high),
    Total_anaemic_constant_rates = sum(anaemic_kids_constant_rates, na.rm = TRUE),
    Total_anaemic_low_constant_rates = sum(anaemic_kids_constant_rates_low, na.rm = TRUE),
    Total_anaemic_high_constant_rates = sum(anaemic_kids_constant_rates_high, na.rm = TRUE),
    Total_anaemic_current_progress_rates = sum(anaemic_kids_projected_current_progress_rates, na.rm = TRUE),
    Total_anaemic_2x_current_progress_rates = sum(anaemic_kids_projected_2x_current_progress_rates , na.rm = TRUE),
    .groups = 'drop'
  )

# Stage 4: Chart potential worlds --------------------------------------------------------
ggplot(world_totals, aes(x = year, y = Total_Births)) +
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=100*Total_anaemic_current_progress_rates/Total_Births, col = 'Rates fall at current pace'))+
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=100*Total_anaemic_2x_current_progress_rates/Total_Births, col = 'Rates fall 2x as fast'))+
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=100*Total_anaemic_constant_rates/Total_Births, col = 'Current rates'))+
  geom_line(aes(y=100*Total_anaemic/Total_Births, col = '2000-2019'))+
  labs(x = "", title = "Anaemia rate, % of mothers, world, estimated", y="") +
  theme_minimal()+ylim(c(0,40))+theme(legend.position = 'right', legend.title = element_blank())
ggsave('plots/amaemia_world_rates.png', height = 5, width = 5)

ggplot(world_totals, aes(x = year, y = Total_Births)) +
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=Total_anaemic_current_progress_rates, col = 'Rates fall at current pace'))+
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=Total_anaemic_2x_current_progress_rates, col = 'Rates fall 2x as fast'))+
  geom_line(data = world_totals[world_totals$year > 2018, ], aes(y=Total_anaemic_constant_rates, col = 'Current rates'))+
  geom_line(aes(y=Total_anaemic, col = '2000-2019'))+
  labs(x = "", title = "Pregnant women anaemic, world, estimated", y="") +
  theme_minimal()+theme(legend.position = 'right', legend.title = element_blank())+expand_limits(y=0)
ggsave('plots/amaemia_world_total.png', height = 5, width = 5)

# Stage 5: Export --------------------------------------------------------
write_csv(world_totals, 'output-data/anaemia_world_totals.csv')

dat <- dat %>% rename(anaemia_estimate_who = estimate,
                      anaemia_estimate_who_low = estimate_low,
                      anaemia_estimate_who_high = estimate_high,
                      anaemia_region_estimate_who = region_estimate,
                      anaemia_region_estimate_who_low = region_estimate_low,
                      anaemia_region_estimate_who_high = region_estimate_high) %>%
  select(-estimate_01, -year_squared, -anaemia_in_2019)
write_csv(dat, 'output-data/anaemia_by_country_with_estimates.csv')



