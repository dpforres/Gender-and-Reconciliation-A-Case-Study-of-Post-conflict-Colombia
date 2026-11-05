####################### Manuscript Code #########################
#### When Peace Feels Threatening ######

############ Install packages ############

install.packages("tidyverse")
install.packages("haven")
install.packages("stargazer")
install.packages("ggeffects")
install.packages("sjPlot")
install.packages("marginaleffects")


############ Load packages ############

library(tidyverse)
library(haven)
library(stargazer)
library(ggeffects)
library(sjPlot)
library(marginaleffects)


# Import laop_13_14_16_18
lapop_13_14_16_18 <- read.csv("/Users/~/lapop_13_14_16_18.csv")

# Create assets index. Remove responses that are not 0 and 1 and get sum of possessions values to get a measurement for income
cols_to_clean <- c("r4a", "r5", "r6", "r12", "r15")

lapop_13_14_16_18[cols_to_clean] <- lapply(lapop_13_14_16_18[cols_to_clean], function(x) ifelse(x %in% c(0, 1), x, NA))

# Remove NAs in these columns using complete.cases function
lapop_13_14_16_18 <- lapop_13_14_16_18[complete.cases(lapop_13_14_16_18[, c("r4a", "r5", "r6", "r12", "r15")]), ]

# Sum columns into new sum of assets column and rename "q10g"
lapop_13_14_16_18$q10g <- rowSums(lapop_13_14_16_18[, c("r4a", "r5", "r6", "r12", "r15")], na.rm = TRUE)

# Change names of columns
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  rename(Gender = q1, Centro = centro, Education = ed, Income = q10g, Age = q2y, Victim1 = wc1, Victim2 = wc2, Victim3 = wc3, Religiosity = q5b, Urban = ur)

# Remove 98s and 99s from Victim columns
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim1 != 98)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim1 != 99)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim2 != 98)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(Victim3 != 98)

# Create binary outcome variable. Convert all 2s to 0s.
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

# Reverse code so that greater religiosity has a higher value 
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


################################## Tables 1 & 2 #####################################

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


# Clean IV column and reverse code
lapop_14_16_18 <- lapop_14_16_18 %>%
  mutate(
    coldis35f = case_when(
      coldis35f == 1 ~ 0,
      coldis35f == 0 ~ 1,
      coldis35f %in% c(98, 99) ~ NA_real_,
      is.na(coldis35f) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(coldis35f))

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



####################################### Adjusted predictions by gender (Figures 1 & 2) ######################################

### Figure 1 ###

pred_child <- avg_predictions(
  colrecon6_total,
  variables = "Gender"
) %>%
  mutate(outcome = "Child's Friend")

pred_child

pred_work_man <- avg_predictions(
  colrecon7_total,
  variables = "Gender"
) %>%
  mutate(outcome = "Work (Man)")

pred_work_man

pred_work_woman <- avg_predictions(
  colrecon8_total,
  variables = "Gender"
) %>%
  mutate(outcome = "Work (Woman)")

pred_work_woman

pred_school <- avg_predictions(
  colrecon18_total,
  variables = "Gender"
) %>%
  mutate(outcome = "Same School")

pred_school


# Combine them
pred_ols <- bind_rows(
  pred_child,
  pred_work_man,
  pred_work_woman,
  pred_school
)

pred_ols

# Plot
plot_ols <- ggplot(
  pred_ols,
  aes(
    x = Gender,
    y = estimate,
    group = Gender
  )
) +
  geom_point(size = 1) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.08
  ) +
  facet_wrap(
    ~ outcome,
    nrow = 1
  ) +
  scale_y_continuous(
    limits = c(3.5, 7),
    breaks = seq(3.5, 7, by = 0.5)
  ) +
  labs(
    x = "Gender",
    y = "Adjusted Predicted Support",
    title = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )


plot_ols


### Figure 2 ###

pred_neighbor <- avg_predictions(
  vecino_desmovilizado,
  by = "Gender",
  type = "response"
) %>%
  mutate(outcome = "Ex-combatant as Neighbor")

pred_neighbor

plot_neighbor <- ggplot(
  pred_neighbor,
  aes(
    x = Gender,
    y = estimate
  )
) +
  geom_point(size = 1) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.08
  ) +
  scale_y_continuous(
    limits = c(0.5, 0.7),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Gender",
    y = "Predicted Probability",
    title = "Former Combatant as Neighbor"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 11
    )
  )

plot_neighbor


############### Interaction Effects, Gender * Victim (Tables 8 & 9) ################

### Child's Friend
# Reset lapop_13_14_16_18 dataset
lapop_13_14_16_18 <- lapop_complete

# Filter out NAs and 98s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(colrecon6), colrecon6 != 98)


############### Run Model ###############
colrecon6_gender_victim <- lm(colrecon6 ~ Gender * Victim + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                              data = lapop_13_14_16_18
)

summary(colrecon6_gender_victim)


############## Work (Man) ################

# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon7
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon7), colrecon7 != 98)

################ Run Model ################
colrecon7_gender_victim <- lm(colrecon7 ~ Gender * Victim + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                              data = lapop_13_14_16)

summary(colrecon7_gender_victim)

################ Work (Woman) #################

# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))


# Remove NAs and 98s from colrecon8
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon8), colrecon8 != 98)


############### Run Model ################
colrecon8_gender_victim <- lm(colrecon8 ~ Gender * Victim + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                              data = lapop_13_14_16)

summary(colrecon8_gender_victim)


################ Same School ################

# Create 'lapop_complete' by subsetting rows with year == 2016, 2018
lapop_16_18 <- lapop_complete %>% filter(year %in% c(2016, 2018))

# Remove NAs and 98s from colrecon18
lapop_16_18 <- lapop_16_18 %>%
  filter(!is.na(colrecon18) & colrecon18 != 98)


############# Run Model ###############

colrecon18_gender_victim <- lm(colrecon18 ~ Gender * Victim + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                               data = lapop_16_18)

summary(colrecon18_gender_victim)


######### Demobilizing Ex-combatant as Neighbor ############
# COLDIS35F. Desmovilizados de los grupos armados. ¿No los quisiera tener de vecinos?
# Create 'lapop_14_16_18' by subsetting rows with year == 2014, 2016, 2018
lapop_14_16_18 <- lapop_complete %>% filter(year %in% c(2014, 2016, 2018))

# Clean column and run
lapop_14_16_18 <- lapop_14_16_18 %>%
  mutate(
    coldis35f = case_when(
      coldis35f == 1 ~ 0,
      coldis35f == 0 ~ 1,
      coldis35f %in% c(98, 99) ~ NA_real_,
      is.na(coldis35f) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(coldis35f))

# Run model 
coldis35f_gender_victim <- glm(coldis35f ~ Gender * Victim + Urban + Centro + Education + Income + Age + Religiosity + factor(year), 
                               data = lapop_14_16_18, 
                               family = binomial)

summary(coldis35f_gender_victim)


############### Table 8, Gender * Victim ########################
stargazer(
  colrecon6_gender_victim,
  colrecon7_gender_victim, 
  colrecon8_gender_victim, 
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



############### Table 9, Gender * Victim ########################
stargazer(
  coldis35f_gender_violence, 
  colrecon18_gender_violence, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)




############# Tables 11-13, Gender * direct_violence ##############
# Robustness check using 2014 data only

# Restrict to only 2014 where all victimization variables are included
lapop_robustness_2014 <- lapop_complete %>%
  filter(year == 2014)

############# Construct victimization index ##############

# Clean victimization variables and construct index
lapop_robustness_2014 <- lapop_robustness_2014 %>%
  filter(
    colwc6 != 98,
    colwc7 != 98,
    colwc8 != 98
  )

lapop_robustness_2014 <- lapop_robustness_2014 %>%
  mutate(colwc6 = ifelse(colwc6 == 2, 0, colwc6))

lapop_robustness_2014 <- lapop_robustness_2014 %>%
  mutate(colwc7 = ifelse(colwc7 == 2, 0, colwc7))

lapop_robustness_2014 <- lapop_robustness_2014 %>%
  mutate(colwc8 = ifelse(colwc8 == 2, 0, colwc8))

# Create victimization index
lapop_robustness_2014 <- lapop_robustness_2014 %>%
  mutate(
    direct_violence = case_when(
      
      # All four responses missing
      if_all(
        c(Victim1, colwc6, colwc7, colwc8),
        is.na
      ) ~ NA_real_,
      
      # At least one positive response
      if_any(
        c(Victim1, colwc6, colwc7, colwc8),
        ~ .x == 1
      ) ~ 1,
      
      # At least one valid response, none positive
      TRUE ~ 0
    )
  )


############# Clean outcome variables ##############

lapop_robustness_2014 <- lapop_robustness_2014 %>%
  mutate(
    # Child's Friend
    colrecon6 = ifelse(
      colrecon6 %in% c(98, 99),
      NA_real_, colrecon6
    ),
    
    # Work (Man)
    colrecon7 = ifelse(
      colrecon7 %in% c(98, 99),
      NA_real_, colrecon7
    ),
    
    # Work (Woman)
    colrecon8 = ifelse(
      colrecon8 %in% c(98, 99),
      NA_real_, colrecon8
    ),
    
    # Demobilized ex-combatant as neighbor
    coldis35f = case_when(
      coldis35f == 1 ~ 0,
      coldis35f == 0 ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Reintegration
    Reintegration = case_when(
      colpaz4a == 1 ~ 1,
      colpaz4a == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )



############# Run models ##############


### Child's Friend
colrecon6_gender_violence <- lm(
  colrecon6 ~ Gender * direct_violence +
    Urban + Centro + Education + Income +
    Age + Religiosity,
  data = lapop_robustness_2014
)


summary(colrecon6_gender_violence)


### Work (Man)
colrecon7_gender_violence <- lm(
  colrecon7 ~ Gender * direct_violence +
    Urban + Centro + Education + Income +
    Age + Religiosity,
  data = lapop_robustness_2014
)

summary(colrecon7_gender_violence)


### Work (Woman)
colrecon8_gender_violence <- lm(
  colrecon8 ~ Gender * direct_violence +
    Urban + Centro + Education + Income +
    Age + Religiosity,
  data = lapop_robustness_2014
)

summary(colrecon8_gender_violence)

### Demobilizing Ex-combatant as Neighbor
coldis35f_gender_violence <- glm(
  coldis35f ~ Gender * direct_violence +
    Urban + Centro + Education + Income +
    Age + Religiosity,
  data = lapop_robustness_2014,
  family = binomial
)

summary(coldis35f_gender_violence)

### Reintegration
reintegration_interaction_2014 <- glm(
  Reintegration ~ Gender * direct_violence +
    Urban + Centro + Education + Income +
    Age + Religiosity,
  data = lapop_robustness_2014,
  family = binomial
)

summary(reintegration_interaction_2014)


### Create stargazer tables


######## Table 11 ########
stargazer(
  colrecon6_gender_violence,
  colrecon7_gender_violence,
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

######## Table 12 ########
stargazer(
  colrecon8_gender_violence,
  coldis35f_gender_violence,
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)

######## Table 13 ########
stargazer(
  reintegration_interaction_2014,
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


############## Tables 14-17, Security Concerns (Gender * Rebel-controlled territory) #############
### Child's Friend
# Reset lapop_13_14_16_18 dataset
lapop_13_14_16_18 <- lapop_complete


# Filter out NAs and 98s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(colrecon6), colrecon6 != 98)


############## Child's friend ~ Gender * rebel_controlled ###########
colrecon6_rebel_control_gender <- lm(colrecon6 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                                     data = lapop_13_14_16_18
)


summary(colrecon6_rebel_control_gender)

############## Work (Man) ################
# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon7
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon7), colrecon7 != 98)

############## Work (Man) ~ Gender * rebel_controlled ###########
colrecon7_rebel_control_gender <- lm(colrecon7 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                                     data = lapop_13_14_16
)

summary(colrecon7_rebel_control_gender)

################ Work (Woman) #################
# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon8
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon8), colrecon8 != 98)

############## Work (Woman) ~  Gender * rebel_controlled ###########
colrecon8_rebel_control_gender <- lm(colrecon8 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                                     data = lapop_13_14_16
)

summary(colrecon8_rebel_control_gender)

################ Same School ################
# Create 'lapop_complete' by subsetting rows with year == 2016, 2018
lapop_16_18 <- lapop_complete %>% filter(year %in% c(2016, 2018))

# Remove NAs and 98s from colrecon18
lapop_16_18 <- lapop_16_18 %>%
  filter(!is.na(colrecon18) & colrecon18 != 98)


############## same school ~ Gender * rebel_controlled  ###########
colrecon18_rebel_control_gender <- lm(colrecon18 ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                                      data = lapop_16_18)

summary(colrecon18_rebel_control_gender)

######### Demobilizing Ex-combatant as Neighbor ############
# COLDIS35F. Desmovilizados de los grupos armados. ¿No los quisiera tener de vecinos?
# Create 'lapop_14_16_18' by subsetting rows with year == 2014, 2016, 2018
lapop_14_16_18 <- lapop_complete %>% filter(year %in% c(2014, 2016, 2018))

# Clean column and reverse code
lapop_14_16_18 <- lapop_14_16_18 %>%
  mutate(
    coldis35f = case_when(
      coldis35f == 1 ~ 0,
      coldis35f == 0 ~ 1,
      coldis35f %in% c(98, 99) ~ NA_real_,
      is.na(coldis35f) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(coldis35f))


########### neighbor ~ Gender * rebel_controlled  ###########
vecino_desmovilizado_rebel_control_gender <- glm(coldis35f ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + factor(year), 
                                                 data = lapop_14_16_18, 
                                                 family = binomial)


summary(vecino_desmovilizado_rebel_control_gender)


############### Table 14, Child's Friend ~ gender * rebel_controlled_territory ########################
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




############### Table 15, Work (man, woman) ~ gender * rebel_controlled_territory ########################
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


############### Table 16, neighbor, gender * rebel_controlled_territory ########################
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


############## Tables 18 & 19, Security Concerns, gender * urban #############
### Child's Friend
# Reset lapop_13_14_16_18 dataset
lapop_13_14_16_18 <- lapop_complete

# Filter out NAs and 98s
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  filter(!is.na(colrecon6), colrecon6 != 98)


############## Child's friend ~ Gender * Urban ###########
colrecon6_urban_gender <- lm(colrecon6 ~ Gender * Urban + Victim + Centro + Education + Income + Age + Religiosity + factor(year),
                             data = lapop_13_14_16_18
)

summary(colrecon6_urban_gender)


############## Work (Man) ################
# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Remove NAs and 98s from colrecon7
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon7), colrecon7 != 98)


############## Work (Man) ~ Gender * urban ###########
colrecon7_urban_gender <- lm(colrecon7 ~ Gender * Urban + Victim + Centro + Education + Income + Age + Religiosity + factor(year),
                             data = lapop_13_14_16
)

summary(colrecon7_urban_gender)


################ Work (Woman) #################
# Reset 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))


# Remove NAs and 98s from colrecon8
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colrecon8), colrecon8 != 98)


############## Work (Woman) ~  Gender * urban ###########
colrecon8_urban_gender <- lm(colrecon8 ~ Gender * Urban + Victim + Centro + Education + Income + Age + Religiosity + factor(year),
                             data = lapop_13_14_16
)


summary(colrecon8_urban_gender)


################ Same School ################
# Create 'lapop_complete' by subsetting rows with year == 2016, 2018
lapop_16_18 <- lapop_complete %>% filter(year %in% c(2016, 2018))


# Remove NAs and 98s from colrecon18
lapop_16_18 <- lapop_16_18 %>%
  filter(!is.na(colrecon18) & colrecon18 != 98)


############## same school ~ Gender * urban  ###########
colrecon18_urban_gender <- lm(colrecon18 ~ Gender * Urban + Victim + Centro + Education + Income + Age + Religiosity + factor(year),
                              data = lapop_16_18)


summary(colrecon18_urban_gender)




######### Demobilizing Ex-combatant as Neighbor ############
# COLDIS35F. Desmovilizados de los grupos armados. ¿No los quisiera tener de vecinos?
# Create 'lapop_14_16_18' by subsetting rows with year == 2014, 2016, 2018
lapop_14_16_18 <- lapop_complete %>% filter(year %in% c(2014, 2016, 2018))

# Clean column and reverse code
lapop_14_16_18 <- lapop_14_16_18 %>%
  mutate(
    coldis35f = case_when(
      coldis35f == 1 ~ 0,
      coldis35f == 0 ~ 1,
      coldis35f %in% c(98, 99) ~ NA_real_,
      is.na(coldis35f) ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(coldis35f))


########### neighbor ~ Gender * urban  ###########
vecino_desmovilizado_urban_gender <- glm(coldis35f ~ Gender * Urban + Victim + Centro + Education + Income + Age + Religiosity + factor(year), 
                                         data = lapop_14_16_18, 
                                         family = binomial)


summary(vecino_desmovilizado_urban_gender)


############### Table 18, Child's Friend, Work (Man), Work (Woman) ~ gender * urban ########################
stargazer(
  colrecon6_urban_gender,
  colrecon7_urban_gender,
  colrecon8_urban_gender,
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




############### Table 19, neighbor, same school gender * urban ########################
stargazer(
  vecino_desmovilizado_urban_gender, 
  colrecon18_urban_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


#### GenderWomen in the summary table refers to the likelihood that women would support reconciliation
#### using Men as the reference point. Manually re-label in stargazer. Also re-label Income -> Asset Index


######################## Local Security (2013), Table 5 ##########################
# Do you believe that the demobilization of the FARC would improve or worsen the security in your municipality or community? (1) security would improve; (2) security would remain the same; (3) security would worsen

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

# Generate the stargazer table (Table 5)
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


#################### TABLE 3, Reintegration Attitudes ###########################

# Subset lapop_complete for 2013, 2014, and 2016 as colpaz4a (reintegration attitudes)
# only appears in the 2013, 2014, and 2016 surveys

# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Change colpaz4a to 0,1 binary and remove NAs and 98s
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colpaz4a) & colpaz4a != 98)

# Convert all 2s to 0s to create binary variable
lapop_13_14_16 <- lapop_13_14_16 %>%
  mutate(colpaz4a = ifelse(colpaz4a == 2, 0, colpaz4a))

# Change name of column
lapop_13_14_16 <- lapop_13_14_16 %>%
  rename(Reintegration = colpaz4a)


########################### Run Model ####################################

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


########################### Plot Predicted Probabilities for Reintegration (Figure 4) ##############################

AME <- glm(
  Reintegration ~ Gender + Urban + Centro + Education + Income +
    Age + Religiosity + Victim + factor(year),
  data = lapop_13_14_16,
  family = binomial(link = "logit")
)

summary(AME)


#################### Predicted probabilities ####################

pred_reintegration <- avg_predictions(
  AME,
  variables = "Gender",
  type = "response"
)

pred_reintegration

avg_comparisons(
  AME,
  variables = "Gender",
  type = "response"
)


#################### Plot ####################

plot_reintegration <- ggplot(
  pred_reintegration,
  aes(
    x = Gender,
    y = estimate
  )
) +
  geom_point(size = 1) +
  geom_errorbar(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.08
  ) +
  scale_y_continuous(
    limits = c(0.5, 0.75),
    breaks = seq(0.5, 0.75, by = 0.05),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Gender",
    y = "Predicted Probability",
    title = "Support for Reintegration"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(
      hjust = 0.5,
      size = 11
    )
  )


plot_reintegration


#################### TABLE 6, Reintegration Attitudes disaggregated by year ###########################
# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Change colpaz4a to 0,1 binary and remove NAs and 98s
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colpaz4a) & colpaz4a != 98)

# Convert all 2s to 0s to create binary variable
lapop_13_14_16 <- lapop_13_14_16 %>%
  mutate(colpaz4a = ifelse(colpaz4a == 2, 0, colpaz4a))

# Change name of column
lapop_13_14_16 <- lapop_13_14_16 %>%
  rename(Reintegration = colpaz4a)

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


################################## TABLE 6 ######################################
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


######### FIGURE 3, disaggregated coefficient plots, reintegration ~ Gender########

# Extract coefficients and confidence intervals from the first year (2012)
coef1 <- tidy(reintegration_2013, conf.int = TRUE) %>%
  mutate(model = "2012") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (2012)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term))  

# Extract coefficients and confidence intervals from the aggregated years. 
# First get reintegration_fixed_effects from Table 3
coef2 <- tidy(reintegration_fixed_effects, conf.int = TRUE) %>%
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
covariate_order <- c("Women (total)", "Women (2012)", "Urban", "Centro Democrático", "Education", "Asset Index", "Age", "Religiosity", "Victim")

# Define the limits for the x-axis
x_limits <- c(-1, 1)

# Rearrange the covariates on the y-axis and rename terms
plot <- plot + scale_y_discrete(limits = rev(covariate_order))

# Zoom in on the x-axis
plot <- plot + coord_cartesian(xlim = x_limits) +
  scale_x_continuous(limits = x_limits)

# Add a title to the plot
plot1 <- plot + labs(title = "2012") +
  theme(plot.title = element_text(hjust = 0.5))

# Display the plot
print(plot1)


##### Generate coefficient plot for 2014

# Extract coefficients and confidence intervals from the second year (2014)
coef1 <- tidy(reintegration_2014, conf.int = TRUE) %>%
  mutate(model = "2014") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (2014)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term))  


# Extract coefficients and confidence intervals from the aggregated years (total)
coef2 <- tidy(reintegration_fixed_effects, conf.int = TRUE) %>%
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
coef1 <- tidy(reintegration_2016, conf.int = TRUE) %>%
  mutate(model = "2016") %>%
  mutate(term = ifelse(term == "GenderWomen", "Women (2016)", term)) %>%
  mutate(term = ifelse(term == "Centro", "Centro Democrático", term)) %>%
  mutate(term = ifelse(term == "Income", "Asset Index", term)) 

# Extract coefficients and confidence intervals from the aggregated years (total)
coef2 <- tidy(reintegration_fixed_effects, conf.int = TRUE) %>%
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



#################### TABLE 7, Reintegration Attitudes toward BACRIM ###########################

# Subset lapop_complete for 2013, 2014, and 2016 as colpaz4b (reintegration attitudes BACRIM)
# only appears in the 2013, 2014, and 2016 surveys

# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Change colpaz4b to 0,1 binary and remove NAs and 98s
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colpaz4b) & colpaz4b != 98)

# Convert all 2s to 0s to create binary variable
lapop_13_14_16 <- lapop_13_14_16 %>%
  mutate(colpaz4b = ifelse(colpaz4b == 2, 0, colpaz4b))

# Change name of column
lapop_13_14_16 <- lapop_13_14_16 %>%
  rename(Reintegration_BACRIM = colpaz4b)

########################### Fixed effects ####################################

# Format year as factor
reintegration_bacrim_fixed_effects <- glm(Reintegration_BACRIM ~ Gender + Urban + Centro + Education + Income + Age + Religiosity + Victim + factor(year),
                                          data   = lapop_13_14_16,
                                          family = binomial)

summary(reintegration_bacrim_fixed_effects)


####################### Run Bivariate Regression ##############################

reintegration_bacrim_bivariate <- glm(Reintegration_BACRIM ~ Gender, data = lapop_13_14_16, family = binomial)

summary(reintegration_bacrim_bivariate)


############ Create stargazer table for Reintegration, Table 3 ################
stargazer(
  reintegration_bacrim_bivariate, 
  reintegration_bacrim_fixed_effects, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


############################ Interaction Gender * Victim, Table 10 ##################################

# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))


# Change colpaz4a to 0,1 binary and remove NAs and 98s
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colpaz4a) & colpaz4a != 98)

# Convert all 2s to 0s to create binary variable
lapop_13_14_16 <- lapop_13_14_16 %>%
  mutate(colpaz4a = ifelse(colpaz4a == 2, 0, colpaz4a))

# Change name of column
lapop_13_14_16 <- lapop_13_14_16 %>%
  rename(Reintegration = colpaz4a)

### Run model
gender_victim_interaction <- glm(
  Reintegration ~ Gender * Victim + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
  data   = lapop_13_14_16,
  family = binomial
)

summary(gender_victim_interaction)


### victimization does not appear to affect Reintegration “more” or “less” for women than for men

#### Create Stargazer Table ####

stargazer(
  gender_victim_interaction, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)






################ Table 21, Reintegration ~ Rebel-controlled ##################  
# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Change colpaz4a to 0,1 binary and remove NAs and 98s
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colpaz4a) & colpaz4a != 98)

# Convert all 2s to 0s to create binary variable
lapop_13_14_16 <- lapop_13_14_16 %>%
  mutate(colpaz4a = ifelse(colpaz4a == 2, 0, colpaz4a))

# Change name of column
lapop_13_14_16 <- lapop_13_14_16 %>%
  rename(Reintegration = colpaz4a)

######## Run model ########
reintegration_rebel_control <- glm(Reintegration ~ rebel_controlled + Gender + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
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


################ Reintegration ~ Gender * Rebel-controlled Territory ##################  
reintegration_rebel_control_gender <- glm(Reintegration ~ Gender * rebel_controlled + Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                                          data   = lapop_13_14_16,
                                          family = binomial)


summary(reintegration_rebel_control_gender)


################ Table 17, Reintegration ~ Gender * Rebel Territory  
stargazer(
  reintegration_rebel_control_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)


################ Reintegration ~ Gender * Urban ##################  
# Create 'lapop_13_14_16' by subsetting rows with year == 2013, 2014, 2016
lapop_13_14_16 <- lapop_complete %>% filter(year %in% c(2013, 2014, 2016))

# Change colpaz4a to 0,1 binary and remove NAs and 98s
lapop_13_14_16 <- lapop_13_14_16 %>%
  filter(!is.na(colpaz4a) & colpaz4a != 98)

# Convert all 2s to 0s to create binary variable
lapop_13_14_16 <- lapop_13_14_16 %>%
  mutate(colpaz4a = ifelse(colpaz4a == 2, 0, colpaz4a))

# Change name of column
lapop_13_14_16 <- lapop_13_14_16 %>%
  rename(Reintegration = colpaz4a)

#### Run model
reintegration_urban_gender <- glm(Reintegration ~ Gender * Urban + Centro + Education + Income + Age + Religiosity + factor(year),
                                  data   = lapop_13_14_16,
                                  family = binomial)

summary(reintegration_urban_gender)

################ Table 20, Reintegration ~ Gender * Urban
stargazer(
  reintegration_urban_gender, 
  type = "latex",
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  star.char = c("+", "*", "**", "***"),
  notes = "$^{+}$p$<$0.1; $^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001",
  notes.append = FALSE,
  notes.align = "r"
)
