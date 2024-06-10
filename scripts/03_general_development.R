# Investigation of childhood environment
# Metadata: https://unstats.un.org/sdgs/metadata/files/Metadata-04-02-01.pdf

# Stage 1: Load packages and data --------------------------------------------------------
library(tidyverse)
library(countrycode)
library(dplyr)

# Load UN World Population Prospects
wp <- read_csv('source-data/WPP2022_Demographic_Indicators_Medium.csv')
wp$iso3c <- wp$ISO3_code
wp$year <- wp$Time
wp <- wp %>% select(iso3c, year, Births, PopChange,
                    TPopulation1July, TPopulationFemale1July,
                    TPopulationMale1July, NetMigrations, Deaths,
                    InfantDeaths, Under5Deaths)

# Load mental development dataset:
df <- read_csv('source-data/GHO_mental_development.csv')
head(df)
unique(df$Indicator)
unique(df$`Period type`)

df <- df %>%
  mutate(year = Period,
         country = Location,
         estimate = FactValueNumeric,
         iso3c = countrycode(Location, 'country.name', 'iso3c')) %>%
  filter(Dim1 == 'Both sexes') %>%
  select(year, estimate, iso3c, country)

df <- merge(df, wp, by=c('year', 'iso3c'), all.x=T)

# Stage 2: Chart countries --------------------------------------------------------

ggplot(df, aes(x=year, y=estimate, col=iso3c, size = TPopulation1July))+geom_point(col='gray')+ggtitle('Children aged 36-59 months who are developmentally on track in at least three of the following domains:\n literacy-numeracy, physical development, social-emotional development and learning (%)\nBy year of survey')+geom_point(data = df[df$TPopulation1July > 50000, ], aes(col=country))+theme_minimal()+ylab('')+xlab('')+
  scale_x_continuous(breaks = 2010:2023)
ggsave('plots/developmental_outcomes_by_country_and_year.png', width = 6, height = 6)


ggplot(df[df$year %in% 2014:2024 & df$TPopulation1July > 5000, ], aes(y=reorder(country, estimate), x=estimate, col=iso3c, size = TPopulation1July))+geom_point()+ggtitle('Children aged 36-59 months who are developmentally on track in at least three of the following domains:\n literacy-numeracy, physical development, social-emotional development and learning (%)\nSelected countries')+ylab('')+xlab('')+theme_minimal()+theme(legend.position = 'none')
ggsave('plots/developmental_outcomes_by_country.png', width = 6, height = 6)

# Stage 3: Merge with stunting/anaemia data and chart correlation --------------------------------------------------------
stunting <- read_csv('output-data/stunting_by_country_with_estimates.csv')[, c('stunting_estimate_who', 'stunting_estimate_who_low', 'stunting_estimate_who_high', 'iso3c', 'year')]
anaemia <- read_csv('output-data/anaemia_by_country_with_estimates.csv')[, c('anaemia_estimate_who', 'anaemia_estimate_who_low', 'anaemia_estimate_who_high', 'iso3c', 'year')]

df <- merge(df, stunting, by = c('iso3c', 'year'), all.x= T)
df <- merge(df, anaemia, by = c('iso3c', 'year'), all.x= T)

# Export to file:
write_csv(df, 'output-data/nutrition_and_developmental_outcomes.csv')

ggplot(df, aes(x=stunting_estimate_who, y=estimate, col=iso3c, size = TPopulation1July))+
  geom_point(col='gray')+ggtitle('Children aged 36-59 months who are developmentally on track in at least three of the following domains:\n literacy-numeracy, physical development, social-emotional development and learning (%)')+geom_point(data = df[df$TPopulation1July > 50000, ], aes(col=country))+
  theme_minimal()+theme(legend.title = element_blank())+ylab('')+xlab('Stunting rate, %')+
  guides(size = 'none')
ggsave('plots/developmental_outcomes_v_stunting.png', width = 9, height = 7)


ggplot(df, aes(x=anaemia_estimate_who, y=estimate, col=iso3c, size = TPopulation1July))+
  geom_point(col='gray')+ggtitle('Children aged 36-59 months who are developmentally on track in at least three of the following domains:\n literacy-numeracy, physical development, social-emotional development and learning (%)')+geom_point(data = df[df$TPopulation1July > 50000, ], aes(col=country))+
  theme_minimal()+theme(legend.title = element_blank())+ylab('')+xlab('Anaemia in pregnant women, %')+
  guides(size = 'none')
ggsave('plots/developmental_outcomes_v_amaemia.png', width = 9, height = 7)


# Stage 3: Check explanatory power --------------------------------------------------------

# Get GDP PC PPP data from World Bank:
library(WDI)
gdp_data <- WDI(indicator = "NY.GDP.PCAP.PP.CD", start = 2010, end = 2022) %>%
  select(iso3c, year, gdp_per_capita_ppp = NY.GDP.PCAP.PP.CD)
df <- merge(df, gdp_data, all.x = T)

# Subset to where all three indicators available:
sdf <- na.omit(df[, c('iso3c', 'year', 'estimate', 'gdp_per_capita_ppp', 'stunting_estimate_who', 'anaemia_estimate_who')])

# Check adjusted r-squared
summary(lm(estimate ~ gdp_per_capita_ppp, data = sdf))
summary(lm(estimate ~ stunting_estimate_who, data = sdf))
summary(lm(estimate ~ anaemia_estimate_who, data = sdf))


