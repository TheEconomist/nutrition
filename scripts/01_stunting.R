# Investigations and projections of stunting

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

mal$estimate <- mal$estimate_low <- mal$estimate_high <- NA
mal$type <- 'survey'
for(i in 1:nrow(mal)){
  mal[i, c('estimate', 'estimate_low', 'estimate_high')] <- extract_estimate_and_range(mal$survey.based.estimates[i])
}
for(i in which(is.na(mal$estimate))){
  mal$type[i] <- 'modelled'
  mal[i, c('estimate', 'estimate_low', 'estimate_high')] <- extract_estimate_and_range(mal$model.based.estimates[i])
}

# Add iso3c
mal$iso3c <- countrycode(mal$country, 'country.name', 'iso3c')

# Merge the two:
dat <- merge(wp, mal, by=c('iso3c', 'year'), all = T)
dat <- dat[!is.na(dat$iso3c), ]

# Get regional estimates:
mal_regions <- data.frame(read_csv('source-data/GHO_nutrition_regions.csv'))
mal_regions <- mal_regions %>% filter(Indicator == 'Stunting prevalence among children under 5 years of age (% height-for-age <-2 SD), model-based estimates', Location.type == 'UN Region') %>% mutate(region = Location, stunting_rate = Value, year = Period) %>% select(region, year, stunting_rate)
for(i in 1:nrow(mal_regions)){
  mal_regions[i, c('region_estimate', 'region_estimate_low', 'region_estimate_high')] <- extract_estimate_and_range(mal_regions$stunting_rate[i])
}

# Use regional estimate if national not available (this predominantly affects high-income countries where rates are very low, or tiny island states).
dat$region <- countrycode(dat$iso3c, 'iso3c', 'un.regionsub.name')
dat$region[dat$iso3c == 'TWN'] <- 'Eastern Asia'
dat$region[dat$iso3c == 'XKX'] <- 'Southern Europe'
dat <- merge(dat, mal_regions, by= c('year', 'region'), all.x= T)

dat$estimate[is.na(dat$estimate)] <- dat$region_estimate[is.na(dat$estimate)]
dat$estimate_low[is.na(dat$estimate_low)] <- dat$region_estimate_low[is.na(dat$estimate_low)]
dat$estimate_high[is.na(dat$estimate_high)] <- dat$region_estimate_high[is.na(dat$estimate_high)]

# Stats for text:
res <- data.frame()
for(i in c(0:10)*5){
  res <- rbind(res, c(i,
                      sum(dat$Births[dat$year == 2000 & dat$iso3c %in% dat$iso3c[dat$year == 2022 & dat$estimate >= i]], na.rm = T)/sum(dat$Births[dat$year == 2000], na.rm = T),
                      sum(dat$Births[dat$year == 2022 & dat$iso3c %in% dat$iso3c[dat$year == 2022 & dat$estimate >= i]], na.rm = T)/sum(dat$Births[dat$year == 2022], na.rm = T),
                      sum(dat$Births[dat$year == 2050 & dat$iso3c %in% dat$iso3c[dat$year == 2022 & dat$estimate >= i]], na.rm = T)/sum(dat$Births[dat$year == 2050], na.rm = T)))
}

sum(dat$Births[dat$year == 2050 & dat$iso3c %in% dat$iso3c[dat$year == 2022 & dat$estimate > 5]])

colnames(res) <- c('percent_stunted',
                   'this_rate_or_worse_in_2000',
                   'this_rate_or_worse_in_2022',
                   'this_rate_or_worse_in_2050')
res$diff <- res[, 2]-res[, 3]
res$note <- NA
res$note[1] <- 'Stunting rate data ends in 2022'

sum(dat$Births[dat$year == 2050 & dat$iso3c %in% dat$iso3c[dat$year == 2022 & dat$estimate > 20]])-sum(dat$Births[dat$year == 2022 & dat$iso3c %in% dat$iso3c[dat$year == 2022 & dat$estimate > 20]])
sum(dat$Births[dat$year == 2050])-sum(dat$Births[dat$year == 2024])

# Stage 2: Plot this data --------------------------------------------------------

# Plot data:
ggplot(dat[dat$year %in% 2000:2022, ], aes(x=year, y=estimate, group = iso3c))+geom_line(alpha = 0.2)+geom_line(data=dat[dat$year %in% 2000:2022 & dat$iso3c %in% dat$iso3c[dat$TPopulation1July > 200000], ], aes(col=country, linetype=country), size = 2)+theme_minimal()+theme(legend.title = element_blank())+xlab('')+ggtitle('Estimated stunting rates, %, by country')+ylab('')
ggsave('plots/stunting_rates_by_country.png', width = 6, height = 6)

# Stage 3: Projection using current rates --------------------------------------------------------

# Ensure data is filtered properly for non-NA ISO codes
dat <- dat[!is.na(dat$iso3c), ]

# Project future estimates based on 2022 values
dat <- dat %>%
  group_by(iso3c) %>%
  mutate(
    projected_estimate_constant_rates = if_else(year > 2022, first(estimate[year == 2022]), estimate),
    projected_estimate_constant_rates_low = if_else(year > 2022, first(estimate_low[year == 2022]), estimate_low),
    projected_estimate_constant_rates_high = if_else(year > 2022, first(estimate_high[year == 2022]), estimate_high)
  ) %>%
  ungroup()

# Calculate stunted children estimates
dat <- dat %>%
  mutate(
    stunted_kids = Births * estimate/100,
    stunted_kids_low = Births * estimate_low/100,
    stunted_kids_high = Births * estimate_high/100,
    stunted_kids_constant_rates = Births * projected_estimate_constant_rates / 100,
    stunted_kids_constant_rates_low = Births * projected_estimate_constant_rates_low / 100,
    stunted_kids_constant_rates_high = Births * projected_estimate_constant_rates_high / 100
  )

# Summarize world totals by year
world_totals <- dat %>%
  filter(year %in% 1990:2050) %>%
  group_by(year) %>%
  summarise(
    Total_Births = sum(Births, na.rm = TRUE),
    Total_Stunted_constant_rates = sum(stunted_kids_constant_rates, na.rm = TRUE),
    Total_Stunted_low_constant_rates = sum(stunted_kids_constant_rates_low, na.rm = TRUE),
    Total_Stunted_high_constant_rates = sum(stunted_kids_constant_rates_high, na.rm = TRUE),
    .groups = 'drop'
  )

# Create a line plot for total stunted children at constant rates
ggplot(world_totals, aes(x = year, y = Total_Stunted_constant_rates)) +
  geom_line() +
  geom_line(aes(y = Total_Stunted_low_constant_rates), color = 'gray') +
  geom_line(aes(y = Total_Stunted_high_constant_rates), color = 'gray') +
  theme_minimal() +
  labs(x = "Year", y = "Total Stunted Children (Constant Rates)")

# Create a ribbon plot for the proportion of stunted children at constant rates
ggplot(world_totals, aes(x = year, y = Total_Stunted_constant_rates / Total_Births)) +
  geom_ribbon(aes(ymin = Total_Stunted_low_constant_rates / Total_Births, ymax = Total_Stunted_high_constant_rates / Total_Births), fill = 'gray') +
  geom_line() +
  labs(x = "Year", y = "Proportion of Children Stunted (Constant Rates Estimate)")

# Additional line plot for proportions at constant rates
ggplot(world_totals, aes(x = year, y = Total_Stunted_constant_rates / Total_Births)) +
  geom_line() +
  labs(x = "Year", y = "Proportion of Stunted Children (Constant Rates)")

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
                     dat[dat$year %in% 2011:2022 & !is.na(dat$estimate), c('estimate_01', 'Births')],
                   pred =
                     predict(glm_model, newdata = dat[dat$year %in% 2011:2022 & !is.na(dat$estimate), ], type = 'response'))

ggplot(pred, aes(x=estimate_01, y=pred, size = Births))+geom_point(alpha = 0.1)+
  geom_abline(aes(intercept = 0, slope =1))
summary(pred)
cor(pred)

# Generating predictions for future years using GLM model
train <- dat[dat$year %in% 2012:2022 & !is.na(dat$estimate), ]
glm_model <- glm(estimate_01 ~ as.factor(iso3c)*year, data = train, family = quasi(link = "logit", variance = "mu(1-mu)"), weights = Births)


dat <- dat %>%
  mutate(
    projected_estimate_current_progress_rates = ifelse(year >= 2022, predict(glm_model, newdata = dat[, ], type = "response")*100, estimate)
  ) %>%
  mutate(
    stunted_kids_projected_current_progress_rates = Births * (projected_estimate_current_progress_rates/100)
  )

# Summarize world totals by year based on the new projections
world_totals <- dat %>%
  filter(year %in% 1990:2050) %>%
  group_by(year) %>%
  summarise(
    Total_Births = sum(Births, na.rm = TRUE),
    Total_Stunted = sum(stunted_kids),
    Total_Stunted_low = sum(stunted_kids_low),
    Total_Stunted_high = sum(stunted_kids_high),
    Total_Stunted_constant_rates = sum(stunted_kids_constant_rates, na.rm = TRUE),
    Total_Stunted_low_constant_rates = sum(stunted_kids_constant_rates_low, na.rm = TRUE),
    Total_Stunted_high_constant_rates = sum(stunted_kids_constant_rates_high, na.rm = TRUE),
    Total_Stunted_current_progress_rates = sum(stunted_kids_projected_current_progress_rates, na.rm = TRUE),
    .groups = 'drop'
  )
# Plotting the model projections
ggplot(world_totals, aes(x = year, y = Total_Births)) +
  geom_line(aes(y=Total_Stunted_current_progress_rates/Total_Births))+
  labs(x = "Year", y = "Total Stunted Children (Model-Based Projection)") +
  theme_minimal()

# Finally, what if progress was twice as fast?

# First check that every country is (projected to be) on the right track:
change <- dat[dat$year %in% c(2022, 2030), c('iso3c', 'projected_estimate_current_progress_rates', 'year')]
change <- change[order(change$year), ]
# Some countries are not, it appears.

# Cycle through countries, mindful that some countries appear to be getting worse:
dat$stunting_in_2022 <- NA
for(i in unique(dat$iso3c)){
  dat$stunting_in_2022[dat$iso3c == i] <- dat$estimate[dat$year == 2022 & dat$iso3c == i]
}
dat$projected_estimate_2x_current_progress_rates <- ifelse(dat$year > 2022 & dat$projected_estimate_current_progress_rates - dat$stunting_in_2022 < 0,
                                                           dat$stunting_in_2022 - 2*(dat$stunting_in_2022-dat$projected_estimate_current_progress_rates),
                                                           dat$projected_estimate_constant_rates)
dat$stunted_kids_projected_2x_current_progress_rates <- dat$Births*dat$projected_estimate_2x_current_progress_rates/100

# Summarize world totals by year based on the new projections
world_totals <- dat %>%
  filter(year %in% 2000:2050) %>%
  group_by(year) %>%
  summarise(
    Total_Births = sum(Births, na.rm = TRUE),
    Total_Stunted = sum(stunted_kids),
    Total_Stunted_low = sum(stunted_kids_low),
    Total_Stunted_high = sum(stunted_kids_high),
    Total_Stunted_constant_rates = sum(stunted_kids_constant_rates, na.rm = TRUE),
    Total_Stunted_low_constant_rates = sum(stunted_kids_constant_rates_low, na.rm = TRUE),
    Total_Stunted_high_constant_rates = sum(stunted_kids_constant_rates_high, na.rm = TRUE),
    Total_Stunted_current_progress_rates = sum(stunted_kids_projected_current_progress_rates, na.rm = TRUE),
    Total_Stunted_2x_current_progress_rates = sum(stunted_kids_projected_2x_current_progress_rates , na.rm = TRUE),
    .groups = 'drop'
  )

# Stage 4: Chart potential worlds --------------------------------------------------------
ggplot(world_totals, aes(x = year, y = Total_Births)) +
  geom_line(data = world_totals[world_totals$year > 2021, ], aes(y=100*Total_Stunted_current_progress_rates/Total_Births, col = 'Rates fall at current pace'))+
  geom_line(data = world_totals[world_totals$year > 2021, ], aes(y=100*Total_Stunted_2x_current_progress_rates/Total_Births, col = 'Rates fall 2x as fast'))+
  geom_line(data = world_totals[world_totals$year > 2021, ], aes(y=100*Total_Stunted_constant_rates/Total_Births, col = 'Current rates'))+
  geom_line(aes(y=100*Total_Stunted/Total_Births, col = '2000-2022'))+
  labs(x = "", title = "Stunting rate, world, estimated, %", y="") +
  theme_minimal()+ylim(c(0,40))+theme(legend.position = 'right', legend.title = element_blank())
ggsave('plots/stunting_world_rates.png', height = 5, width = 5)

ggplot(world_totals, aes(x = year, y = Total_Births)) +
  geom_line(data = world_totals[world_totals$year > 2021, ], aes(y=Total_Stunted_current_progress_rates, col = 'Rates fall at current pace'))+
  geom_line(data = world_totals[world_totals$year > 2021, ], aes(y=Total_Stunted_2x_current_progress_rates, col = 'Rates fall 2x as fast'))+
  geom_line(data = world_totals[world_totals$year > 2021, ], aes(y=Total_Stunted_constant_rates, col = 'Current rates'))+
  geom_line(aes(y=Total_Stunted, col = '2000-2022'))+
  labs(x = "", title = "Children stunted, world, estimated, '000s", y="") +
  theme_minimal()+theme(legend.position = 'right', legend.title = element_blank())+expand_limits(y=0)
ggsave('plots/stunting_world_total.png', height = 5, width = 5)

# Stage 5: Export --------------------------------------------------------
write_csv(world_totals, 'output-data/stunted_world_totals.csv')

dat <- dat %>% rename(stunting_estimate_who = estimate,
                      stunting_estimate_who_low = estimate_low,
                      stunting_estimate_who_high = estimate_high,
                      stunting_region_estimate_who = region_estimate,
                      stunting_region_estimate_who_low = region_estimate_low,
                      stunting_region_estimate_who_high = region_estimate_high) %>%
  select(-estimate_01, -year_squared, -stunting_in_2022, -type, -Data.Source, -survey.based.estimates,
  -model.based.estimates)
write_csv(dat, 'output-data/stunting_by_country_with_estimates.csv')


