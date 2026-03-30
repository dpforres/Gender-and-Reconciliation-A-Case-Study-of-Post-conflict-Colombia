####################### Manuscript Code #########################
#### Gender and Reconciliation: A Case Study of Post-conflict Colombia ######

#### Install packages
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("ggeffects")
install.packages("stargazer")
install.packages("haven")
install.packages("margins")
install.packages("coefplot")
install.packages("sjPlot")
install.packages("insight")
install.packages("GGally")

library(tidyverse)
library(ggpubr)
library(rstatix)
library(ggeffects)
library(stargazer)
library(ggplot2)
library(haven)
library(margins)
library(dplyr)
library(coefplot)
library(insight)
library(sjPlot)
library(GGally)
library(broom)

# Import laop_13_14_16_18
lapop_13_14_16_18 <- read.csv("/Users/Desktop/lapop_13_14_16_18.csv")

# Create assets index. Remove responses that are not 0 and 1 and get sum of possessions values to get a measurement for income
cols_to_clean <- c("r4a", "r5", "r6", "r12", "r15")

lapop_13_14_16_18[cols_to_clean] <- lapply(lapop_13_14_16_18[cols_to_clean], function(x) ifelse(x %in% c(0, 1), x, NA))

# Remove NAs in these columns using complete.cases function
lapop_13_14_16_18 <- lapop_13_14_16_18[complete.cases(lapop_13_14_16_18[, c("r4a", "r5", "r6", "r12", "r15")]), ]

# Sum columns into new sum of assets column and rename "q10g"
lapop_13_14_16_18$q10g <- rowSums(lapop_13_14_16_18[, c("r4a", "r5", "r6", "r12", "r15")], na.rm = TRUE)

# Change names of columns
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  rename(Forgiveness = colpaz6a, Gender = q1, Centro = centro, Education = ed, Income = q10g, Age = q2y, Victim1 = wc1, Victim2 = wc2, Victim3 = wc3, Religiosity = q5b, Urban = ur)

# Remove 98s and 99s from Victim columns
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim1 != 98)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim1 != 99)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim2 != 98)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim3 != 98)

# Create binomial outcome variable. Convert all 2s to 0s.
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Victim1 = ifelse(Victim1 == 2, 0, Victim1))

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Victim2 = ifelse(Victim2 == 2, 0, Victim2))

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Victim3 = ifelse(Victim3 == 2, 0, Victim3))

# Create a new column called Victim and assign values based on responses
lapop_13_14_16_18$Victim <- ifelse(rowSums(lapop_13_14_16_18[, c("Victim1", "Victim2", "Victim3")]) > 0, 1, 0)

# Remove NAs, 98s, 99s from Education
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(Education) & Education != 98)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Education != 99)

# Remove NAs, 98s from Religiosity
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(Religiosity) & Religiosity != 98)

# Reverse Code so that greater religiosity has a higher value 
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Religiosity = dplyr::recode(Religiosity,
                                     `1` = 4, `2` = 3, `3` = 2, `4` = 1,
                                     .default = NA_real_))

# Ensure that for centro variable, all NAs are converted to 0s
lapop_13_14_16_18$Centro[is.na(lapop_13_14_16_18$Centro)] <- 0

# Check Urban for NAs, 98
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(Urban) &(Urban != 98))

# Modify the Urban column in the dataset so that "rural" is denoted by "0"
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Urban = ifelse(Urban == 2, 0, Urban))

# Copy dataset for other tables and plots
lapop_complete <- lapop_13_14_16_18

########################### Table 4 ####################################

# Clean Forgiveness column (forgiveness and reconciliation)
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(Forgiveness) & Forgiveness != 98)

# Convert all 2s to 0s to create binomial variable
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Forgiveness = ifelse(Forgiveness == 2, 0, Forgiveness))

########### Run logit with year fixed effects ##########
colpaz6a_total <- glm(
  Forgiveness ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
  data   = lapop_13_14_16_18,
  family = binomial)

summary(colpaz6a_total)

########### Run bivariate ###########
colpaz6a_bivariate <- glm(Forgiveness ~ Gender, data = lapop_13_14_16_18, family = binomial)

summary(colpaz6a_bivariate)

################ Generate the stargazer table for Table 4 ################
stargazer(
  colpaz6a_bivariate, 
  colpaz6a_total, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

# "GenderWomen" in the summary table refers to the likelihood that women would 
# support reconciliation using "Men" as the reference point. Manually re-label 
# in stargazer to "Women". Also re-label "Income" -> "Asset Index", 
# and "Centro" -> "Centro Democrático"

############## Run for Rebel-controlled Territory, Table 22 ###############

colpaz6a_total_rebel_control <- glm(
  Forgiveness ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
  data   = lapop_13_14_16_18,
  family = binomial)

summary(colpaz6a_total_rebel_control)

################ Generate the stargazer table for Table 22 ################
stargazer(
  colpaz6a_total_rebel_control, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

##########  Forgiveness ~ Gender * Rebel controlled Territory ############
colpaz6a_total_rebel_control_gender <- glm(
  Forgiveness ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
  data   = lapop_13_14_16_18,
  family = binomial)

summary(colpaz6a_total_rebel_control_gender)

################ Generate the stargazer table for Table 27 ################
stargazer(
  colpaz6a_total_rebel_control_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


#################### AME, Forgiveness ~ Gender ####################

AME <- glm(Forgiveness ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
           data   = lapop_13_14_16_18,
           family = binomial(link = "logit")) 

summary(AME)

# Get AME
Base <- data.frame(summary(margins(AME, type="response", change="dydx")))

Base


########################### Plot Marginal Effects ##############################

theme_set(theme_sjplot())

fit <- glm(Forgiveness ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
           data   = lapop_13_14_16_18,
           family = binomial(link = "logit")) 

plot <- plot_model(fit, type = "pred", terms = "Gender")

# Plot settings
plot <- plot +
  labs(
    x = "Gender",
    y = "Forgiveness and Reconciliation",
    title = "Predicted values of Forgiveness and Reconciliation"
  ) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Men", "Women")
  ) +
  theme(plot.title = element_text(size = 10))

######## Plot AME
print(plot)


######### TABLE 8, disaggregated years, Forgiveness ~ Gender ###########

# 2013
# Create 'subset_2013_forgiveness' by subsetting rows with year == 2013
subset_2013_forgiveness <- lapop_13_14_16_18 %>% filter(year == 2013)

# 2013 model with controls
forgiveness_2013 <- glm(Forgiveness ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim, 
                        data = subset_2013_forgiveness, 
                        family = binomial)

summary(forgiveness_2013)

#### 2014
# Create 'subset_2014_forgiveness' by subsetting rows with year == 2014
subset_2014_forgiveness <- lapop_13_14_16_18 %>% filter(year == 2014)

# Create 2014 model with controls
forgiveness_2014 <- glm(Forgiveness ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim, 
                        data = subset_2014_forgiveness, 
                        family = binomial)

summary(forgiveness_2014)

#### 2016
# Create 'subset_2016_forgiveness' by subsetting rows with year == 2016
subset_2016_forgiveness <- lapop_13_14_16_18 %>% filter(year == 2016)

# Create 2016 model with controls
forgiveness_2016 <- glm(Forgiveness ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim, 
                        data = subset_2016_forgiveness, 
                        family = binomial)

summary(forgiveness_2016)

#### 2018
# Create 'subset_2018_forgiveness' by subsetting rows with year == 2018
subset_2018_forgiveness <- lapop_13_14_16_18 %>% filter(year == 2018)

# Create 2018 model with controls
forgiveness_2018 <- glm(Forgiveness ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim, 
                        data = subset_2018_forgiveness, 
                        family = binomial)

summary(forgiveness_2018)


################## Create stargazer table for disaggregated years ###############

# Combine models into a list
model_list <- list(forgiveness_2013, forgiveness_2014, forgiveness_2016, forgiveness_2018)

# Generate the stargazer table with custom significance levels and notes
stargazer(
  model_list, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### Manually change the names of 'GenderWomen' to 'Women', 'Centro' to 'Centro Democrático', and 'Income' to 'Asset Index'


######### FIGURE 1, disaggregated coefficient plots, Forgiveness ~ Gender########

# Extract coefficients and confidence intervals from the first year (2013)
coef1 <- tidy(forgiveness_2013, conf.int = TRUE) %>%
  mutate(model = "2013") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (2013)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term))  

# Extract coefficients and confidence intervals from the aggregated years. 
# First get colpaz6a_total (total) from Table 1
coef2 <- tidy(colpaz6a_total, conf.int = TRUE) %>%
  filter(term == "GenderWomen") %>%
  mutate(model = "total") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (total)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term)) 

# Combine the two coefficient data frames
combined_coef <- bind_rows(coef1, coef2)

# Generate the coefficient plot
plot <- ggcoef(combined_coef)

# Define the order of the covariates on the y-axis
covariate_order <- c("Women (total)", "Women (2013)", "Urban", "Centro Democrático", "Education", "Asset Index", "Age", "Religiosity", "Victim")

# Define the limits for the x-axis
x_limits <- c(-1, 1)

# Rearrange the covariates on the y-axis and rename terms
plot <- plot + scale_y_discrete(limits = rev(covariate_order))

# Zoom in on the x-axis
plot <- plot + coord_cartesian(xlim = x_limits) +
  scale_x_continuous(limits = x_limits)

# Add a title to the plot
plot1 <- plot + labs(title = "2013") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(plot1)


##### Generate coefficient plot for 2014

# Extract coefficients and confidence intervals from the second year (2014)
coef1 <- tidy(forgiveness_2014, conf.int = TRUE) %>%
  mutate(model = "2014") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (2014)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term))  

# Extract coefficients and confidence intervals from the aggregated years (total)
coef2 <- tidy(colpaz6a_total, conf.int = TRUE) %>%
  filter(term == "GenderWomen") %>%
  mutate(model = "total") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (total)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term))  

# Combine the two coefficient data frames
combined_coef <- bind_rows(coef1, coef2)

# Generate the coefficient plot
plot <- ggcoef(combined_coef)

# Define the order of the covariates on the y-axis
covariate_order <- c("Women (total)", "Women (2014)", "Urban", "Centro Democrático", "Education", "Asset Index", "Age", "Religiosity", "Victim")

# Define the limits for the x-axis
x_limits <- c(-1, 1)

# Rearrange the covariates on the y-axis and rename terms
plot <- plot + scale_y_discrete(limits = rev(covariate_order))

# Zoom in on the x-axis
plot <- plot + coord_cartesian(xlim = x_limits) +
  scale_x_continuous(limits = x_limits)

# Add a title to the plot
plot2 <- plot + labs(title = "2014") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(plot2)


##### Generate coefficient plot for 2016

# Extract coefficients and confidence intervals from the third year (2016)
coef1 <- tidy(forgiveness_2016, conf.int = TRUE) %>%
  mutate(model = "2016") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (2016)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term)) 

# Extract coefficients and confidence intervals from the aggregated years (total)
coef2 <- tidy(colpaz6a_total, conf.int = TRUE) %>%
  filter(term == "GenderWomen") %>%
  mutate(model = "total") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (total)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term))  # Change Income to Asset Index

# Combine the two coefficient data frames
combined_coef <- bind_rows(coef1, coef2)

# Generate the coefficient plot
plot <- ggcoef(combined_coef)

# Define the order of the covariates on the y-axis
covariate_order <- c("Women (total)", "Women (2016)", "Urban", "Centro Democrático", "Education", "Asset Index", "Age", "Religiosity", "Victim")

# Define the limits for the x-axis
x_limits <- c(-1, 1)

# Rearrange the covariates on the y-axis and rename terms
plot <- plot + scale_y_discrete(limits = rev(covariate_order))

# Zoom in on the x-axis
plot <- plot + coord_cartesian(xlim = x_limits) +
  scale_x_continuous(limits = x_limits)

# Add a title to the plot
plot3 <- plot + labs(title = "2016") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(plot3)


#### Generate coefficient plot for 2018

# Extract coefficients and confidence intervals from the fourth year (2018)
coef1 <- tidy(forgiveness_2018, conf.int = TRUE) %>%
  mutate(model = "2018") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (2018)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term))  

# Extract coefficients and confidence intervals from the aggregated years (total)
coef2 <- tidy(colpaz6a_total, conf.int = TRUE) %>%
  filter(term == "GenderWomen") %>%
  mutate(model = "total") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (total)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term)) 

# Combine the two coefficient data frames
combined_coef <- bind_rows(coef1, coef2)

# Generate the coefficient plot
plot <- ggcoef(combined_coef)

# Define the order of the covariates on the y-axis
covariate_order <- c("Women (total)", "Women (2018)", "Urban", "Centro Democrático", "Education", "Asset Index", "Age", "Religiosity", "Victim")

# Define the limits for the x-axis
x_limits <- c(-1, 1)

# Rearrange the covariates on the y-axis and rename terms
plot <- plot + scale_y_discrete(limits = rev(covariate_order))

# Zoom in on the x-axis
plot <- plot + coord_cartesian(xlim = x_limits) +
  scale_x_continuous(limits = x_limits)

# Add a title to the plot
plot4 <- plot + labs(title = "2018") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(plot4)


#################### TABLE 3, Reintegration Attitudes ###########################

# Subset lapop_complete for 2013, 2014, and 2016 as colpaz4a (reintegration attitudes)
# only appears in the 2013, 2014, and 2016 surveys

# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Change colpaz4a to 0,1 binomial and remove NAs and 98s
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colpaz4a) & colpaz4a != 98)

# Convert all 2s to 0s to create binomial variable
lapop_13_14_16 <- lapop_13_14_16 %>%
  mutate(colpaz4a = ifelse(colpaz4a == 2, 0, colpaz4a))

# Change names of columns
lapop_13_14_16 <- lapop_13_14_16 %>%
  rename(Reintegration = colpaz4a)

########################### Fixed effects ####################################

# Format year as factor
reintegration_fixed_effects <- glm(Reintegration ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                                   data   = lapop_13_14_16,
                                   family = binomial)

summary(reintegration_fixed_effects)


####################### Run Bivariate Regression ##############################

reintegration_bivariate <- glm(Reintegration ~ Gender, data = lapop_13_14_16, family = binomial)

summary(reintegration_bivariate)

############ Create stargazer table for Reintegration, Table 3 ################
stargazer(
  reintegration_bivariate, 
  reintegration_fixed_effects, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

################ Table 21, Reintegration ~ Rebel-controlled ##################  
reintegration_rebel_control <- glm(Reintegration ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                                   data   = lapop_13_14_16,
                                   family = binomial)

summary(reintegration_rebel_control)

################ Create stargazer table for Reintegration ~ Rebel Territory
stargazer(
  reintegration_rebel_control, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

################ Table 26, Reintegration ~ Gender * Rebel-controlled Territory ##################  
reintegration_rebel_control_gender <- glm(Reintegration ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                                   data   = lapop_13_14_16,
                                   family = binomial)

summary(reintegration_rebel_control_gender)

################ Create stargazer table for Reintegration ~ Rebel Territory
stargazer(
  reintegration_rebel_control_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


#################### TABLE 20, Reintegration Attitudes disaggregated by year ###########################

# 2013
# Create 'subset_2013_reintegration' by subsetting rows with year == 2013
subset_2013_reintegration <- lapop_13_14_16 %>% filter(year == 2013)

# 2013 model
reintegration_2013 <- glm(Reintegration ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim, 
                          data = subset_2013_reintegration, 
                          family = binomial)

summary(reintegration_2013)

# 2014
# Create 'subset_2014_reintegration' by subsetting rows with year == 2014
subset_2014_reintegration <- lapop_13_14_16 %>% filter(year == 2014)

# 2014 model
reintegration_2014 <- glm(Reintegration ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim, 
                          data = subset_2014_reintegration, 
                          family = binomial)

summary(reintegration_2014)

# 2016
# Create 'subset_2016_reintegration' by subsetting rows with year == 2016
subset_2016_reintegration <- lapop_13_14_16 %>% filter(year == 2016)

# 2016 model
reintegration_2016 <- glm(Reintegration ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim, 
                          data = subset_2016_reintegration, 
                          family = binomial)

summary(reintegration_2016)


################################## TABLE 7 ######################################
################# Create stargazer table for Reintegration Attitudes ######################
stargazer(
  reintegration_2013, 
  reintegration_2014, 
  reintegration_2016,
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)



################################## TABLE 1 #####################################

### Child's Friend
# Reset lapop_13_14_16_18 dataset
lapop_13_14_16_18 <- lapop_complete

# Filter out NAs and 98s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(colrecon6), colrecon6 != 98)

############### Run Model ###############
colrecon6_total <- lm(colrecon6 ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                      data = lapop_13_14_16_18
)

summary(colrecon6_total)


############## Work (Man) ################

# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon7
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon7), colrecon7 != 98)

################ Run Model ################
colrecon7_total <- lm(colrecon7 ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                      data = lapop_13_14_16)

summary(colrecon7_total)


################ Work (Woman) #################

# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon8
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon8), colrecon8 != 98)

############### Run Model ################
colrecon8_total <- lm(colrecon8 ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                      data = lapop_13_14_16)

summary(colrecon8_total)


################ Same School ################

# Create 'lapop_complete' by subsetting rows with year == 2016, 2018
lapop_16_18 <- lapop_complete %>% filter(year %in% c(2016, 2018))

# Remove NAs and 98s from colrecon18
lapop_16_18 <- lapop_16_18 %>%
  filter(!is.na(colrecon18) & colrecon18 != 98)

############# Run Model ###############

colrecon18_total <- lm(colrecon18 ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                       data = lapop_16_18)

summary(colrecon18_total)

######### Demobilizing Ex-combatant as Neighbor ############
# COLDIS35F. Desmovilizados de los grupos armados. ¿No los quisiera tener de vecinos?
# Create 'lapop_14_16_18' by subsetting rows with year == 2014, 2016, 2018
lapop_14_16_18 <- lapop_complete %>% filter(year %in% c(2014, 2016, 2018))

# Clean IV column
lapop_14_16_18 <- lapop_14_16_18 %>%
  filter(coldis35f != 98)

# Reverse code, so that respondents ok with having desmovilizados as neighbors are '1'
lapop_14_16_18$coldis35f <- dplyr::if_else(lapop_14_16_18$coldis35f == 1, 0, 1)

# Run model 
vecino_desmovilizado <- glm(coldis35f ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year), 
                            data = lapop_14_16_18, 
                            family = binomial)

summary(vecino_desmovilizado)

############### Stargazer table, Table 1, Security Concerns ########################
stargazer(
  colrecon6_total, 
  colrecon7_total, 
  colrecon8_total, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### GenderWomen in the summary table refers to the likelihood that women would support interacting
#### with former combatants using Men as the reference point. Manually re-label "Women" in stargazer. 
#### Also re-label Income -> Asset Index and Centro -> Centro Democrático

############### Stargazer table, Table 2, Security Concerns ########################
stargazer(
  vecino_desmovilizado, 
  colrecon18_total, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### GenderWomen in the summary table refers to the likelihood that women would support interacting
#### with former combatants using Men as the reference point. Manually re-label "Women" in stargazer. 
#### Also re-label Income -> Asset Index and Centro -> Centro Democrático

############## Security Concerns in Rebel-controlled territory 
### Child's Friend
# Reset lapop_13_14_16_18 dataset
lapop_13_14_16_18 <- lapop_complete

# Filter out NAs and 98s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(colrecon6), colrecon6 != 98)

############### Run Model ###############
colrecon6_rebel_control <- lm(colrecon6 ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                      data = lapop_13_14_16_18
)

summary(colrecon6_rebel_control)

############## Child's friend ~ Gender * rebel_controlled ###########
colrecon6_rebel_control_gender <- lm(colrecon6 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                              data = lapop_13_14_16_18
)

summary(colrecon6_rebel_control_gender)

############## Work (Man) ################
# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon7
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon7), colrecon7 != 98)

################ Run Model ################
colrecon7_rebel_control <- lm(colrecon7 ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                      data = lapop_13_14_16)

summary(colrecon7_rebel_control)

############## Work (Man) ~ Gender * rebel_controlled ###########
colrecon7_rebel_control_gender <- lm(colrecon7 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                                     data = lapop_13_14_16
)

summary(colrecon7_rebel_control_gender)

################ Work (Woman) #################
# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon8
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon8), colrecon8 != 98)

############### Run Model ################
colrecon8_rebel_control <- lm(colrecon8 ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                      data = lapop_13_14_16)

summary(colrecon8_rebel_control)

############## Work (Woman) ~  Gender * rebel_controlled ###########
colrecon8_rebel_control_gender <- lm(colrecon8 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                                     data = lapop_13_14_16
)

summary(colrecon8_rebel_control_gender)

################ Same School ################
# Create 'lapop_complete' by subsetting rows with year == 2016, 2018
lapop_16_18 <- lapop_complete %>% filter(year %in% c(2016, 2018))

# Remove NAs and 98s from colrecon18
lapop_16_18 <- lapop_16_18 %>%
  filter(!is.na(colrecon18) & colrecon18 != 98)

############# Run Model ###############
colrecon18_rebel_control <- lm(colrecon18 ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                       data = lapop_16_18)

summary(colrecon18_rebel_control)

############## same school ~ Gender * rebel_controlled  ###########
colrecon18_rebel_control_gender <- lm(colrecon18 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                               data = lapop_16_18)

summary(colrecon18_rebel_control_gender)

######### Demobilizing Ex-combatant as Neighbor ############
# COLDIS35F. Desmovilizados de los grupos armados. ¿No los quisiera tener de vecinos?
# Create 'lapop_14_16_18' by subsetting rows with year == 2014, 2016, 2018
lapop_14_16_18 <- lapop_complete %>% filter(year %in% c(2014, 2016, 2018))

# Clean IV column
lapop_14_16_18 <- lapop_14_16_18 %>%
  filter(coldis35f != 98)

# Reverse code, so that respondents ok with having desmovilizados as neighbors are '1'
lapop_14_16_18$coldis35f <- dplyr::if_else(lapop_14_16_18$coldis35f == 1, 0, 1)

# Run model 
vecino_desmovilizado_rebel_control <- glm(coldis35f ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year), 
                            data = lapop_14_16_18, 
                            family = binomial)

summary(vecino_desmovilizado_rebel_control)

########### neighbor ~ Gender * rebel_controlled  ###########
vecino_desmovilizado_rebel_control_gender <- glm(coldis35f ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year), 
                                          data = lapop_14_16_18, 
                                          family = binomial)

summary(vecino_desmovilizado_rebel_control_gender)


############### Stargazer table, Table 19, Security Concerns ########################
stargazer(
  colrecon6_rebel_control, 
  colrecon7_rebel_control, 
  colrecon8_rebel_control, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### GenderWomen in the summary table refers to the likelihood that women would support interacting
#### with former combatants using Men as the reference point. Manually re-label "Women" in stargazer. 
#### Also re-label Income -> Asset Index and Centro -> Centro Democrático

############### Stargazer table, Table 20, Security Concerns ########################
stargazer(
  vecino_desmovilizado_rebel_control, 
  colrecon18_rebel_control, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

############### Stargazer table, Table 23, Security Concerns ########################
stargazer(
  colrecon6_rebel_control_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


#### GenderWomen in the summary table refers to the likelihood that women would support interacting
#### with former combatants using Men as the reference point. Manually re-label "Women" in stargazer. 
#### Also re-label Income -> Asset Index and Centro -> Centro Democrático


############### Stargazer table, Table 24, Security Concerns ########################
stargazer(
  colrecon7_rebel_control_gender, 
  colrecon8_rebel_control_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

############### Stargazer table, Table 25, Security Concerns ########################
stargazer(
  vecino_desmovilizado_rebel_control_gender, 
  colrecon18_rebel_control_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


############################# TABLE 9, Forgiveness, inclusion of Ideology as control variable ################################

# Import laop_13_14_16_18. Rename lapop_ideology
# Clean Forgiveness column (forgiveness and reconciliation)
lapop_ideology <- lapop_complete %>%
  filter(!is.na(Forgiveness) & Forgiveness != 98)

# Convert all 2s to 0s to create binomial variable
lapop_ideology <- lapop_ideology %>%
  mutate(Forgiveness = ifelse(Forgiveness == 2, 0, Forgiveness))

# Change names of columns
lapop_ideology <- lapop_ideology %>%
  rename(Ideology = l1)

#Check ideology for NAs, 98s
lapop_ideology <- lapop_ideology %>%
  filter(!is.na(Ideology) & Ideology != 98)


###### Run logit w/ fixed effects
colpaz6a_ideology <- glm(Forgiveness ~ Gender + Urban + Ideology + Education + Income + Age + Religiosity + Victim + factor(year),
                         data = lapop_ideology, 
                         family = binomial)

summary(colpaz6a_ideology)


######################## Run Bivariate Model ###############################
bivariate_ideology <- glm(Forgiveness ~ Gender, 
                          data = lapop_ideology, family = binomial)

summary(bivariate_ideology)

####### Table 4 stargazer table #######
stargazer(
  bivariate_ideology, 
  colpaz6a_ideology, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### GenderWomen in the summary table refers to the likelihood that women would support reconciliation
#### using Men as the reference point. Manually re-label in stargazer. Also re-label Income -> Asset Index


####################### TABLE 10 (continued from Table 9) #######################

#### Disaggregate lapop_ideology df by year
# 2013
# Create 'subset_2013_forgiveness' by subsetting rows with year == 2013
subset_2013_forgiveness <- lapop_ideology %>% filter(year == 2013)

# 2013 model with controls
forgiveness_ideology_2013 <- glm(Forgiveness ~ Gender + Urban + Ideology + Education + Income + Age + Religiosity + Victim, 
                                 data = subset_2013_forgiveness, 
                                 family = binomial)

summary(forgiveness_ideology_2013)

# 2014
# Create 'subset_2014_forgiveness' by subsetting rows with year == 2014
subset_2014_forgiveness <- lapop_ideology %>% filter(year == 2014)

# Create 2014 model with controls
forgiveness_ideology_2014 <- glm(Forgiveness ~ Gender + Urban + Ideology + Education + Income + Age + Religiosity + Victim, 
                                 data = subset_2014_forgiveness, 
                                 family = binomial)

summary(forgiveness_ideology_2014)

# 2016
# Create 'subset_2016_forgiveness' by subsetting rows with year == 2016
subset_2016_forgiveness <- lapop_ideology %>% filter(year == 2016)

# Create 2016 model with controls
forgiveness_ideology_2016 <- glm(Forgiveness ~ Gender + Urban + Ideology + Education + Income + Age + Religiosity + Victim, 
                                 data = subset_2016_forgiveness, 
                                 family = binomial)

summary(forgiveness_ideology_2016)

# 2018
# Create 'subset_2018_forgiveness' by subsetting rows with year == 2018
subset_2018_forgiveness <- lapop_ideology %>% filter(year == 2018)

# Create 2018 model with controls
forgiveness_ideology_2018 <- glm(Forgiveness ~ Gender + Urban + Ideology + Education + Income + Age + Religiosity + Victim, 
                                 data = subset_2018_forgiveness, 
                                 family = binomial)

summary(forgiveness_ideology_2018)

#### Replacing party affiliation with ideology, women are still less likely than 
#### men to forgive demobilizing members of the FARC, significant for all four years 

################## Create stargazer table for disaggregate years ###############
# Combine models into a list
model_list <- list(forgiveness_ideology_2013, forgiveness_ideology_2014, forgiveness_ideology_2016, forgiveness_ideology_2018)

# Generate the stargazer table with custom significance levels and notes
stargazer(
  model_list, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### Manually change the names of 'GenderWomen' to 'Women' and 'Income' to 'Asset Index'.


############## Table 11, Reintegration w/ Ideology control variable ################

# Reset lapop_13_14_16_18 dataset
lapop_13_14_16_18 <- read.csv("/Users/Desktop/lapop_13_14_16_18.csv")

# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16_ideology <- lapop_13_14_16_18 %>% filter(year %in% c(2013, 2014, 2016))

#Change colpaz4a to 0,1 binomial and remove NAs and 98s
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(!is.na(colpaz4a) & colpaz4a != 98)

# Convert all 2s to 0s to create binomial variable
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  mutate(colpaz4a = ifelse(colpaz4a == 2, 0, colpaz4a))

### Create assets index. Remove responses that are not 0 and 1 and get sum of possessions values to get a measurement for income
cols_to_clean <- c("r4a", "r5", "r6", "r12", "r15")

lapop_13_14_16_ideology[cols_to_clean] <- lapply(lapop_13_14_16_ideology[cols_to_clean], function(x) ifelse(x %in% c(0, 1), x, NA))

# Remove NAs in these columns using complete.cases function
lapop_13_14_16_ideology <- lapop_13_14_16_ideology[complete.cases(lapop_13_14_16_ideology[, c("r4a", "r5", "r6", "r12", "r15")]), ]

# Rename sum assets index column to q10g
lapop_13_14_16_ideology$q10g <- rowSums(lapop_13_14_16_ideology[, c("r4a", "r5", "r6", "r12", "r15")], na.rm = TRUE)

# Change names of columns
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  rename(Reintegration = colpaz4a, Gender = q1, Ideology = l1, Education = ed, Income = q10g, Age = q2y, Victim1 = wc1, Victim2 = wc2, Victim3 = wc3, Religiosity = q5b, Urban = ur)

# Remove 98s and 99s from Victim columns
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(Victim1 != 98)

lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(Victim1 != 99)

lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(Victim2 != 98)

lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(Victim3 != 98)

# Create binomial outcome variable. Convert all 2s to 0s.
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  mutate(Victim1 = ifelse(Victim1 == 2, 0, Victim1))

lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  mutate(Victim2 = ifelse(Victim2 == 2, 0, Victim2))

lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  mutate(Victim3 = ifelse(Victim3 == 2, 0, Victim3))

# Create a new column called Victim and assign values based on individual columns
lapop_13_14_16_ideology$Victim <- ifelse(rowSums(lapop_13_14_16_ideology[, c("Victim1", "Victim2", "Victim3")]) > 0, 1, 0)

# Remove NAs, 98s, 99s from Education
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(!is.na(Education) & Education != 98)

lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(Education != 99)

# Remove NAs, 98s from Religiosity
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(!is.na(Religiosity) & Religiosity != 98)

# Reverse Code so that greater religiosity has a higher value 
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  mutate(Religiosity = dplyr::recode(Religiosity,
                                     `1` = 4, `2` = 3, `3` = 2, `4` = 1,
                                     .default = NA_real_))

# Check Urban for NAs, 98
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(!is.na(Urban) &(Urban != 98))

# Modify the Urban column in the dataset so that "rural" is denoted by "0"
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  mutate(Urban = ifelse(Urban == 2, 0, Urban))

#Remove NAs, 98s in Ideology
lapop_13_14_16_ideology <- lapop_13_14_16_ideology %>%
  filter(!is.na(Ideology) & Ideology != 98)

###### Run logit w/ controls for all years
colpaz4a_total_ideology <- glm(Reintegration ~ Gender + Urban + Ideology + Education + Income + Age + Religiosity + Victim + factor(year),
                               data = lapop_13_14_16_ideology, 
                               family = binomial)

summary(colpaz4a_total_ideology)

######## Bivariate, Reintegration ~ Gender ##########
reintegration_bivariate_ideology <- glm(Reintegration ~ Gender, 
                                        data = lapop_13_14_16_ideology, 
                                        family = binomial)

summary(reintegration_bivariate_ideology)

#### Considering only the bivariate relationship between gender and support for 
#### the reintegration of demobilizing FARC members, women are less supportive than
#### men, significant at the 0.001 level

################# Create stargazer table for Reintegration, Table 11 ######################
stargazer(
  reintegration_bivariate_ideology, 
  colpaz4a_total_ideology, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### GenderWomen in the summary table refers to the likelihood that women would support reintegration
#### using Men as the reference point. Manually re-label in stargazer. Also re-label Income -> Asset Index


############### Table 12, 2021 Reintegration Attitudes ##################

# Import lapop_2021 dataset
lapop_2021 <- read.csv("/Users/Desktop/lapop_2021.csv")

######### Create income variable #########
#### Remove responses that are not 0 and 1 and get sum of possessions to form an assets index

cols_to_clean <- c("r6", "r15", "r16", "r18")

lapop_2021[cols_to_clean] <- lapply(lapop_2021[cols_to_clean], function(x) ifelse(x %in% c(0, 1), x, NA))

lapop_2021$q10g <- rowSums(lapop_2021[, c("r6", "r15", "r16", "r18")], na.rm = TRUE)

#### Change names of columns
lapop_2021 <- lapop_2021 %>%
  rename(Forgiveness = colpaz6a, Gender = q1tb, Ideology = coll1, Education = edr, Income = q10g, Age = q2, Victim = colwc10, Urban = ur1new)

# Remove NAs and 98s from Victim
lapop_2021 <- lapop_2021 %>%
  filter(!is.na(Victim) & Victim != 98)

# Remove NAs and 98s from Ideology
lapop_2021 <- lapop_2021 %>%
  filter(!is.na(Ideology) & Ideology != 98)

# Remove NAs, 98s, and 99s from Education
lapop_2021 <- lapop_2021 %>%
  filter(!is.na(Education) & Education != 98)

lapop_2021 <- lapop_2021 %>%
  filter(Education != 99)

# Remove NAs from age
lapop_2021 <- lapop_2021 %>%
  filter(!is.na(Age))

# Recode values in urban column to 1, 0
lapop_2021 <- lapop_2021 %>%
  mutate(
    Urban = case_when(
      Urban %in% c(1, 2, 3) ~ 1L,
      Urban == 4            ~ 0L,
      TRUE                  ~ NA_integer_   
    )
  ) %>%
  filter(!is.na(Urban))

# Save clean df
lapop_2021_complete <- lapop_2021

# Clean forgiveness and reconciliation variable
lapop_2021 <- lapop_2021 %>%
  filter(!is.na(Forgiveness) & Forgiveness != 98) %>%
  mutate(Forgiveness = ifelse(Forgiveness == 2, 0, Forgiveness))

# Run logit w/ controls
colpaz6a_total_2021 <- glm(Forgiveness ~ Gender + Urban + Ideology + Education + Income + Age + Victim, 
                           data = lapop_2021, 
                           family = binomial)

# Display the summary
summary(colpaz6a_total_2021)


# Run Bivariate
bivariate_2021 <- glm(Forgiveness ~ Gender, 
                      data = lapop_2021, 
                      family = binomial)

# Display the summary
summary(bivariate_2021)

# Generate the stargazer table
stargazer(
  bivariate_2021, 
  colpaz6a_total_2021, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### GenderWomen in the summary table refers to the likelihood that women would support reconciliation
#### using Men as the reference point. Manually re-label in stargazer. Also re-label Income -> Asset Index


############# Victimization Rates, Table 19 #################

# Reset lapop_13_14_16_18 dataset
lapop_13_14_16_18 <- lapop_complete

# Change Forgiveness to 0,1 binomial and remove NAs and 98s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(Forgiveness) & Forgiveness != 98)

# Convert all 2s to 0s to create binomial variable
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Forgiveness = ifelse(Forgiveness == 2, 0, Forgiveness))

# Victimization Rates for all four years
Gender_Victim <- glm(Victim ~ Gender, data = lapop_13_14_16_18, family = binomial)

summary(Gender_Victim)

# Get percentage of victims for men and women
table(lapop_13_14_16_18$Victim, lapop_13_14_16_18$Gender)

# Men
1619+916
916/2535

# Women
1600+905
905/2505

# Victimization rates by year
# 2013
# Create 'subset_victim_2013' by subsetting rows with year == 2013
subset_victim_2013 <- lapop_13_14_16_18 %>% filter(year == 2013)

# Create 2013 model 
victim_2013 <- glm(Victim ~ Gender, data = subset_victim_2013, family = binomial)

summary(victim_2013)

# Get percentage of victims for men and women
table(subset_victim_2013$Victim, subset_victim_2013$Gender)

# Men
456+236
236/692

# Women
448+256
256/704

# 2014
# Create 'subset_victim_2014' by subsetting rows with year == 2014
subset_victim_2014 <- lapop_13_14_16_18 %>% filter(year == 2014)

# Create 2014 model 
victim_2014 <- glm(Victim ~ Gender, data = subset_victim_2014, family = binomial)

summary(victim_2014)

# Get percentage of victims for men and women
table(subset_victim_2014$Victim, subset_victim_2014$Gender)

# Men
453+271
271/724

# Women
422+261
261/683

# 2016
# Create 'subset_victim_2016' by subsetting rows with year == 2016
subset_victim_2016 <- lapop_13_14_16_18 %>% filter(year == 2016)

# Create 2016 model 
victim_2016 <- glm(Victim ~ Gender, data = subset_victim_2016, family = binomial)

summary(victim_2016)

# Get percentage of victims for men and women
table(subset_victim_2016$Victim, subset_victim_2016$Gender)

# Men
476+270
270/746

# Women
471+240
240/711

# 2018
# Create 'subset_victim_2018' by subsetting rows with year == 2018
subset_victim_2018 <- lapop_13_14_16_18 %>% filter(year == 2018)

# Create 2018 model 
victim_2018 <- glm(Victim ~ Gender, data = subset_victim_2018, family = binomial)

summary(victim_2018)

# Get percentage of victims for men and women
table(subset_victim_2018$Victim, subset_victim_2018$Gender)

# Men 
234+139
139/373

# Women
259+148
148/407

# Manually input values into stargazer table


################################# TABLE 12 #####################################
### Types of Victimization

# Create binary outcome variable. Convert all 2s to 0s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(
    Victim1 = ifelse(Victim1 == 2, 0, Victim1),
    Victim2 = ifelse(Victim2 == 2, 0, Victim2),
    Victim3 = ifelse(Victim3 == 2, 0, Victim3),
    colwc8 = ifelse(colwc8 == 2, 0, colwc8),
    colwc9 = ifelse(colwc9 == 2, 0, colwc9)
  )

# Remove NAs
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(Victim1) & Victim1 != 98,
         !is.na(Victim2) & Victim2 != 98,
         !is.na(Victim3) & Victim3 != 98,
         !is.na(colwc8) & colwc8 != 98,
         !is.na(colwc9) & colwc9 != 98
  )


# Subset lapop_13_14_16_18 for 2013 and 2014 as colwc5, colwc6, and colwc7 only appear in the 2013 and 2014 surveys
lapop_13_14 <- lapop_13_14_16_18 %>% filter(year %in% c(2013, 2014))

lapop_13_14 <- lapop_13_14 %>%
  mutate(
    colwc5 = ifelse(colwc5 == 2, 0, colwc5),
    colwc6 = ifelse(colwc6 == 2, 0, colwc6),
    colwc7 = ifelse(colwc7 == 2, 0, colwc7)
  )

lapop_13_14 <- lapop_13_14 %>%
  filter(!is.na(colwc5) & colwc5 != 98,
         !is.na(colwc6) & colwc6 != 98,
         !is.na(colwc7) & colwc7 != 98
  )

######## OUTCOMES #########
# Victim1 (Lost Family Member) 
Victim1_gender <- glm(Victim1 ~ Gender, data = lapop_13_14_16_18, family = binomial)

summary(Victim1_gender)

# Women are not significantly more likely than men to lose a family member to the armed conflict

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manuallly.

table(lapop_13_14_16_18$Victim1, lapop_13_14_16_18$Gender)

# Men total and percentage of victims
1873+654
654/2527

# Women total and percentage of victims
1842+656
656/2498

# Victim2 (Displaced)
Victim2_gender <- glm(Victim2 ~ Gender, data = lapop_13_14_16_18, family = binomial)

summary(Victim2_gender)

# Women are not significantly more likely than men to be displaced due to the armed conflict

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manuallly.

table(lapop_13_14_16_18$Victim2, lapop_13_14_16_18$Gender)

# Men total and percentage of victims
1935+592
592/2527

# Women total and percentage of victims
1910+588
588/2498

# Victim3 (Refugee)
Victim3_gender <- glm(Victim3 ~ Gender, data = lapop_13_14_16_18, family = binomial)

summary(Victim3_gender)

# Women are not significantly more likely than men to be refugees due to the armed conflict

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manuallly.

table(lapop_13_14_16_18$Victim3, lapop_13_14_16_18$Gender)

# Men total and percentage of victims
2351+176
176/2527

# Women total and percentage of victims
2316+182
182/2498

# colwc8 (Kidnapping)

# Remove NAs and 98s from colwc8
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(colwc8) & colwc8 != 98)

# Kidnapping
colwc8_gender <- glm(colwc8 ~ Gender, data = lapop_13_14_16_18, family = binomial)

summary(colwc8_gender)

# Women are not significantly more likely than men to have been kidnapped during the armed conflict

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manually.

table(lapop_13_14_16_18$colwc8, lapop_13_14_16_18$Gender)

# Men total and percentage of victims
2390+137
137/2527

# Women total and percentage of victims
2381+117
117/2498

### colwc9 (Lost Property)
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(colwc9) & colwc9 != 98)

colwc9_gender <- glm(colwc9 ~ Gender, data = lapop_13_14_16_18, family = binomial)

summary(colwc9_gender)

# Women are not significantly more likely than men to lose property as a result of the armed conflict

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manuallly.

table(lapop_13_14_16_18$colwc9, lapop_13_14_16_18$Gender)

# Men total and percentage of victims
2118+409
409/2527

# Women total and percentage of victims
2057+441
441/2498

### colwc5 (Forced recruitment)
colwc5_gender <- glm(colwc5 ~ Gender, data = lapop_13_14, family = binomial)

summary(colwc5_gender)

# Women are not significantly more likely than men to be forcibly recruited

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manuallly.

table(lapop_13_14$colwc5, lapop_13_14$Gender)

# Men total and percentage of victims
1343+57
57/1400

# Women total and percentage of victims
1319+58
58/1377

# colwc6 (Sexual Violence)
lapop_13_14 <- lapop_13_14 %>%
  filter(!is.na(colwc6) & colwc6 != 98)

colwc6_gender <- glm(colwc6 ~ Gender, data = lapop_13_14, family = binomial)

summary(colwc6_gender)

# Women are significantly more likely than men to be victims of sexual violence
# due to the armed conflict

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manuallly.

table(lapop_13_14$colwc6, lapop_13_14$Gender)

# Men total and percentage of victims
1383+17
17/1400

# Women total and percentage of victims
1341+36
36/1377


# colwc7 (Torture)
lapop_13_14 <- lapop_13_14 %>%
  filter(!is.na(colwc7) & colwc7 != 98)

colwc7_gender <- glm(colwc7 ~ Gender, data = lapop_13_14, family = binomial)

summary(colwc7_gender)

# Women are not significantly more likely than men to be victims of torture due
# to the armed conflict

# Input p-value into stargazer table manually
# Get number of men and women in sample and percentage who are victims. Input manuallly.

table(lapop_13_14$colwc7, lapop_13_14$Gender)

# Men total and percentage of victims
1337+63
63/1400

# Women total and percentage of victims
1305+72
72/1377


############################ Interaction Gender * Victim, Table 16 ##################################
# Interacting gender and victim variables
lapop_13_14_16_18 <- lapop_complete

# Change Forgiveness to 0,1 binomial and remove NAs and 98s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(Forgiveness) & Forgiveness!= 98)

# Convert all 2s to 0s to create binomial variable
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(Forgiveness = ifelse(Forgiveness == 2, 0, Forgiveness))

### Run interaction effect
gender_victim_interaction <- glm(
  Forgiveness ~ Gender * Victim + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
  data   = lapop_13_14_16_18,
  family = binomial
)

summary(gender_victim_interaction)

### victimization does not appear to affect forgiveness “more” or “less” for women than for men


######################## Local Security (2013), Table 6 ##########################
# ¿Cree usted que la desmovilización de las FARC mejoraría o empeoraría la
# situación económica de su municipio o comunidad? [no leer alternativas]
# (1) La situación económica mejoraría(2) La situación económica seguiría igual(3) La situación económica empeoraría

# Create 'lapop_13' by subsetting rows with year == 2013
lapop_2013 <- lapop_complete %>% filter(year %in% c(2013))

# Remove 98s and NAs from outcome variable
lapop_2013 <- lapop_2013 %>%
  filter(!is.na(colpropaz5) & colpropaz5 != 98)

# Outcome variable. Rescale the responses so higher numbers believe security would improve with the demobilization of FARC combatants
lapop_2013$colpropaz5 <- ifelse(lapop_2013$colpropaz5 == 1, 3,
                                ifelse(lapop_2013$colpropaz5 == 3, 1, lapop_2013$colpropaz5))

# Run  model
security_2013 <- lm(colpropaz5 ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim,
                    data = lapop_2013)

summary(security_2013)


######################### colpropaz5 bivariate #####################

security_2013_bivariate <- lm(colpropaz5 ~ Gender, 
                              data = lapop_2013)

summary(security_2013_bivariate)

# Generate the stargazer table
stargazer(
  security_2013_bivariate, 
  security_2013, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

#### GenderWomen in the summary table refers to the likelihood that women would feel that security would 
#### improve with the demobilizztaion of FARC combatants. It uses Men as the reference point. 
#### Manually re-label in stargazer. Also re-label Income -> Asset Index





