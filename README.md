# Demography, nutrition and cognition

This repository houses _The Economist_'s data and scenarios of future nutritional outcomes based on past and present incidence of nutritional deficiencies and demographic projections. 

## Contents:
1. Estimates and projections to 2050 of stunting in newborn children and anaemia in pregnant women
2. Estimates of proportion of kids who are developmentally delayed, by country
3. Charts and stats based on these metrics

The analysis is organized into four scripts. 

* 01_stunting.R: This script takes as its inputs UN population projections and WHO country-year estimates of stunting prevalence among children under five years of age. Stunting is defined as abnormally low height for age. Where stunting rates are missing, which is the case mainly in a few high-income countries or tiny countries, stunting rates are assumed to be equal to the UN sub-region stunting rate (e.g. "Western Europe"). It assumes that a child's probability of being stunted is equal to the rate of stunting among those under five in the country in the child's year of birth. This enables us to calculate the world-wide stunting rate over time. It then calculates three scenarios. The first is if stunting continues at current rates, in this case 2022 values (which are the latest data point). The second is what would happen if trends of the past decade continue, or to be precise, the most recent decade of data. To estimate this, it uses a quasi-binomial generalised linear model with logit link with country fixed effects interacted with a year trend. The script offers various calibration plots to assess if such projections are reasonable. The third scenario estimates what would happen if rates declined twice as fast as projected based on trends of the past decade (and no country did worse).
* 02_anaemia.R: This script takes as its inputs UN population projections and WHO country-year estimates of anaemia prevalence in pregnant women. The approach is largely identical to that of the script to investigate stunting rates. Where rates are missing, which is the case mainly in a few high-income countries or tiny countries, rates are assumed to be equal to the UN sub-region rate (e.g. "Western Europe"). It then calculates three scenarios. The first is if anaemia in pregnant women continues to occur at current rates, in this case 2019 values (which are the latest data point). The second is what would happen if trends of the most recent decade continue, or to be precise, the most recent decade of data. To estimate this, it uses a quasi-binomial generalised linear model with logit link with country fixed effects interacted with a year trend. The script offers various calibration plots to assess if such projections are reasonable. The third scenario estimates what would happen if rates declined twice as fast as projected based on trends of the past decade (and no country did worse).
* 03_general_development.R: This script combines the stunting, anaemia, and demographic data with data on the proportion reaching developmental milestones from surveys, specifically, "children aged 36-59 months who are developmentally on track in at least three of the following domains: literacy-numeracy, physical development, social-emotional development and learning", as well as GDP per capita at purchasing power parity from the World Bank. Each survey is combined with the stunting, anaemia, birth and gdp per capita estimates from that year in the country in question.
* 04_world-totals.R: This script loads data on worldwide estimates and scenarios of stunting and anaemia rates and absolute numbers so that one can quickly inspect and chart it. 

Many of the charts used based on this data have versions available in the "plots" folder of this repository. If you have any questions about this analysis or spot an error or opportunity for improvement, please open an issue or email "sondresolstad@economist.com".

## Sources:
UN, WHO, UNICEF, World Bank
