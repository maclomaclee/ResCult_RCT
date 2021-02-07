#Sample size calculation and analysis code relating to manuscript 
#"Results of a randomised controlled trial comparing two different incentives to improve survey response rates"
# Manuscript authors: Malcolm Macleod, Kaitlyn Hair, Ezgi Tanriver-Ayder, Dayle Craske, Sara Shinton, Alan Campbell, Bridget Mellifont, and Lorna Thomson
# code author: Malcolm Macleod
# Date: 06 March 2021
# email malcolm.macleod@ed.ac.uk

#install.packages("epitools")
#install.packages("epiR")
#install.packages("DescTools")
#install.packages("readr")

library(epitools)
library(epiR)
library(DescTools)
library(readr)

# SECTION 1
# sample size calculation
# modeled inclusion of 5000 persons invited
epi.sscc(OR = 1.29, p0 = 0.10, n = 5000, power = NA, r = 1, sided.test = 2, conf.level = 0.95, method = "unmatched")

# actual inclusion of 10110 persons invited
epi.sscc(OR = 1.29, p0 = 0.10, n = 10110, power = NA, r = 1, sided.test = 2, conf.level = 0.95, method = "unmatched")

#SECTION 2
# primary outcome: univariate Odds Ratio, DescTools, oddsratio, method = wald
# consenting responses to survey invitation

ORtable_exclude_no_consent <- matrix(c(4410,4450,642,608), nrow = 2, ncol = 2)
oddsratio(ORtable_exclude_no_consent, conf.level = 0.95, method = "wald")

# survey respondents entering random draw
ORtable_random_draw <- matrix(c(202,143,440,465), nrow = 2, ncol = 2)
oddsratio(ORtable_random_draw, conf.level = 0.95, method = "wald")

# primary outcome: univariate Odds Ratio, DescTools, oddsratio, method = wald
# sensitivity analysis
ORtable_sensitivity_analysis <- matrix(c(4498,4450,554,608), nrow = 2, ncol = 2)
oddsratio(ORtable_sensitivity_analysis, conf.level = 0.95, method = "wald")

#SECTION 2
# secondary outcome: adjusted Odds Ratio, glm, family = binomial(link = logit)
# consenting responses to survey invitation

#intervention: 0 = £50, 1 = £1000
#staff: 0 = student, 1 = staff
#response: 0 = no response, 1 = response
#interac: 0 = none, 1 = £1000 * staff
#frequency: number of responses in each category

# effect on survey response rates

intervention <- read_csv("intervention.csv")
OR_table <- intervention
OR_data <- as.data.frame(OR_table)
umod <- glm(response ~ intervention, OR_data, family = binomial(link = "logit"), weights = frequency)
summary(umod)
amod <- glm(response ~ intervention + staff + interac, OR_data, family = binomial(link = "logit"), weights = frequency)
summary(amod)

# calculation of odds of entering draw
enter_draw <- read_csv("enter_draw.csv")
OR_table <- enter_draw
OR_data <- as.data.frame(OR_table)
umod <- glm(response ~ intervention, OR_data, family = binomial(link = "logit"), weights = frequency)
summary(umod)
amod <- glm(response ~ intervention + staff + interac, OR_data, family = binomial(link = "logit"), weights = frequency)
summary(amod)
