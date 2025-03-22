
#read.me############
#note: The code here is for the futher cleaning data to better build the model. 
#v0.2 update the religion code, update education and update income




#######lode the pacakage########
#For the future people here, this code here is for the data cleanin process of the 2020 American National election selection data.
#clean the environment
rm(list = ls())

library (lubridate)     #the time series processing package
library (tsbox)         #Make time series convverstion, aggregation and plotting simple
library (RColorBrewer)  #color palettes for R graphics
library(wesanderson)    #Provides color palettes inspired by Wes Anderson films.
library(writexl)        #Writes data frames to Excel files.
library(tidyr)          #Tidies messy data   
library(xts)            #Extensible time series class for R   
library(dplyr)          #Data manipulation and transformation.
library(openxlsx)       #Read, write, and edit Excel files.
library(readr)
library(ggplot2)
library(dplyr)
library(survey)
library(ggthemes)

####lode the data######
raw <- read_csv("cleaned_data.csv")

####check the data #####
#display the dataset
#head(raw)
##############rename the variable#########
#rename_the_variable
US_election <- raw %>% rename(
  religion =  V201457x,
  transgender_policy = V201411x, 
  weight = V200010a,
  tax_on_millionaries = V202325,
  government_waste_tax_money = V201235,
  edu_summary = V201511x,
  party_hand_tax = V201242,
  illegel_immiggration  = V201417,
  Lgbgt_job_discrimination  = V201412, 
  race = V201549x,
  gay_marriaige = V201416, 
  gay_adopt = V201415,
  income = V202468x, 
)

print(US_election )
#remove soem variable
US_election <- US_election %>% dplyr::select(-`...1`, -V200004, -V200006, -V200008, -V201007a, -V201566, -V201410, -V200007, -V201014b)

US_election <- na.omit(US_election)
#############################################
#########religion_processing##################


# =============================================================
# Step 1. Define religion codes for broad categories
# -------------------------------------------------------------
christian_codes <- c(
  10, 99, 100, 101, 109, 110,
  120:136, 149, 150, 155, 160:171,
  180:186, 199, 200, 201, 219,
  220:225, 229, 230:235, 240,
  242:246, 249:258, 260:264, 267,
  270:276, 279, 280, 281, 289,
  290:293, 300, 301, 303:306,
  400, 600,
  700:708, 719
)
jewish_codes <- c(502, 503, 524, 650)
islam_codes <- c(720)
eastern_religions_codes <- c(721, 722, 723, 730, 732)
indigenous_new_religions_codes <- c(724, 725, 726, 727, 735, 736, 740, 750, 790, 870, 879)
other_religions_codes <- c(695)
nonreligious_codes <- c(880, 888, 889)

# =============================================================
# Step 2. Single-Code Recoding Function
# -------------------------------------------------------------
recode_religion <- function(code) {
  if (code %in% christian_codes) {
    return("Christianity")
  } else if (code %in% jewish_codes) {
    return("Judaism")
  } else if (code %in% islam_codes) {
    return("Islam")
  } else if (code %in% eastern_religions_codes) {
    return("Eastern Religions")
  } else if (code %in% indigenous_new_religions_codes) {
    return("Indigenous & New Religious Movements")
  } else if (code %in% other_religions_codes) {
    return("Other")
  } else if (code %in% nonreligious_codes) {
    return("Nonreligious")
  } else {
    return(NA)
  }
}

# Vectorize the function to allow column-wise application
recode_religion_vec <- Vectorize(recode_religion)

# =============================================================
# Step 3. Apply Cleaning Function to Data
# -------------------------------------------------------------
clean_religion_data <- function(df, religion_column) {
  df <- df %>%
    rename(religion_code = all_of(religion_column)) %>%  # Temporarily rename input column
    mutate(
      religion = recode_religion_vec(religion_code),  # Use vectorized version
      religion = factor(religion, levels = c(
        "Christianity", "Judaism", "Islam", "Eastern Religions",
        "Indigenous & New Religious Movements", "Other", "Nonreligious"
      )),
      religious_binary = ifelse(religion == "Nonreligious", 0, 1)
    ) %>%
    dplyr::select(-religion_code)
  
  return(df)}
  
US_election <- clean_religion_data(US_election, "religion")

# Remove missing values
US_election <- na.omit(US_election)

# Check results
summary(US_election$religion)
summary(US_election$religious_binary)


# Step 1: Create new collapsed religion group
US_election <- US_election %>%
  mutate(
    religion_group = case_when(
      religion == "Christianity" ~ "Christian",
      religion == "Nonreligious" ~ "Non-religion",
      religion %in% c(
        "Judaism", "Islam", "Eastern Religions",
        "Indigenous & New Religious Movements", "Other"
      ) ~ "Other religions",
      TRUE ~ NA_character_
    ),
    religion_group = factor(religion_group, levels = c("Christian", "Other religions", "Non-religion"))
  )


# Convert the variable into an ordered factor
US_election$transgender_policy<- factor(
  US_election$transgender_policy, 
  levels = c(1, 2, 3, 4, 5, 6), 
  labels = c(
    "Very Strongly Against", 
    "Moderately Against", 
    "Slightly Against", 
    "Slightly In Favor", 
    "Moderately In Favor", 
    "Very Strongly In Favor"
  ),
  ordered = TRUE  # Ensures the variable is treated as ordered
)

US_election <- na.omit(US_election)

#############################################
#2. tax_on_millionaries######################


# Remove invalid responses
US_election <- US_election %>%
  filter(!tax_on_millionaries %in% c(-9, -8, -7, -6, -5))

# Convert TAX_MILLTAX to an ordered factor
US_election <- US_election %>%
  mutate( tax_on_millionaries = factor(
    tax_on_millionaries, 
    levels = c(1, 2, 3),  # Ensure correct order
    labels = c("Favor", "Oppose", "Neither"),
    ordered = TRUE  # Make it an ordered factor
  ))

# Check factor levels
levels(US_election$tax_on_millionaries)

# View summary to confirm
summary(US_election$tax_on_millionaries)

US_election <- na.omit(US_election)
################################################
#3.government_waste_money#######################
# Remove invalid responses
US_election <- US_election %>%
  filter(!government_waste_tax_money%in% c(-9, -8))

# Convert TRUSTGOV_WASTE to an ordered factor
US_election <- US_election %>%
  mutate(government_waste_tax_money = factor(
    government_waste_tax_money, 
    levels = c(1, 2, 3), 
    labels = c("Waste a lot", "Waste some", "Don't waste very much"),
    ordered = TRUE  # Ordered factor for regression analysis
  ))

# Check factor levels
levels(US_election$government_waste_tax_money)

# View summary
summary(US_election$government_waste_tax_money)

US_election <- na.omit(US_election)


################################################
#4.The_level_of_education#######################
# Clean edu_summary variable
US_election <-  US_election %>%
  mutate(edu_summary = as.numeric(edu_summary)) %>%  # Convert to numeric
  filter(edu_summary >= 1 & edu_summary <= 5)  # Keep only valid values

# Convert to an ordered factor (optional for ordinal logistic regression)
US_election$edu_summary <- factor( US_election$edu_summary, levels = 1:5, ordered = TRUE)

# Check summary
summary(US_election$edu_summary)

US_election <- na.omit(US_election)
################################################
#5.which_party_handle_tax_better################

# Step 1: Ensure numeric and keep valid values
US_election <- US_election %>%
  mutate(party_hand_tax = as.numeric(party_hand_tax)) %>%
  filter(party_hand_tax >= 1 & party_hand_tax <= 5)

# Step 2: Collapse into 3 groups
US_election <- US_election %>%
  mutate(
    party_hand_tax_group = case_when(
      party_hand_tax %in% c(1, 2) ~ "Democratic",
      party_hand_tax == 3        ~ "No difference",
      party_hand_tax %in% c(4, 5) ~ "Republican",
      TRUE ~ NA_character_
    ),
    party_hand_tax_group = factor(party_hand_tax_group, levels = c("Democratic", "No difference", "Republican"))
  )

# Step 3: Remove any remaining NAs (if needed)
US_election <- na.omit(US_election)

# Step 4: Check summary
summary(US_election$party_hand_tax_group)

################################################
#6.illegel_immigration #########################

# Clean IMMIG_IMMPOL variable
US_election <-  US_election %>%
  mutate(illegel_immiggration = as.numeric(illegel_immiggration)) %>%  # Convert to numeric
  filter(illegel_immiggration >= 1 & illegel_immiggration <= 4)  # Keep only valid values

# Convert to an ordered factor (useful for ordinal logistic regression)
US_election$illegel_immiggration <- factor( US_election$illegel_immiggration, levels = 1:4, ordered = TRUE)

# Check summary
summary( US_election$illegel_immiggration)

# View first few rows to confirm cleaning
head( US_election)

US_election <- na.omit(US_election)
################################################
#7. Ultra_gay_shit##############################

#7.1 The transgender policy
#7.1.1 drop all the -9
US_election <- US_election  %>% 
  dplyr::filter(! transgender_policy == -2 )


# No need to filter by numeric codes, just drop NA if needed
US_election <- US_election %>%
  filter(transgender_policy %in% c(
    "Very Strongly Against", "Moderately Against", "Slightly Against",
    "Slightly In Favor", "Moderately In Favor", "Very Strongly In Favor"
  ))

# Recode using recode(as.character(...)) style
US_election$trans_support_score <- dplyr::recode(
  as.character(US_election$transgender_policy),
  "Very Strongly Against" = 1,
  "Moderately Against" = 2,
  "Slightly Against" = 3,
  "Slightly In Favor" = 4,
  "Moderately In Favor" = 5,
  "Very Strongly In Favor" = 6
) %>% as.numeric()


#7.2 The gay marraige policy
#7.2.1 drop all the -9
US_election <- US_election  %>% 
  dplyr::filter(! gay_marriaige == -9 )

US_election$gay_marriage_score <- dplyr::recode(as.character(US_election$gay_marriaige),
                                                "1" = 2,
                                                "2" = 1,
                                                "3" = 0)

US_election$gay_marriage_score <- as.numeric(US_election$gay_marriaige)


#7.3 Lgbgt_job_discrimination
#7.3.1 remove the invalide answer

US_election <- US_election  %>% 
  dplyr::filter(! Lgbgt_job_discrimination == -9 )

US_election <- US_election  %>% 
  dplyr::filter(! Lgbgt_job_discrimination == -8 )


US_election$job_discrim_score <- dplyr::recode(as.character(US_election$Lgbgt_job_discrimination),
                                               "1" = 1,
                                               "2" = 0)

US_election$job_discrim_score <- as.numeric(US_election$job_discrim_score)


US_election$job_discrim_score<- as.numeric(US_election$job_discrim_score)

#remoce the na
US_election <- US_election %>%
  filter(!is.na(job_discrim_score))

#7.4 Lgbgt_job_discrimination
#7.4.1 remove the invalide answer
US_election <- US_election  %>% 
  dplyr::filter(! gay_adopt == -9 )

US_election <- US_election  %>% 
  dplyr::filter(! gay_adopt == -8 )

#rename the the variable
US_election$gay_adopt_score <- dplyr::recode(as.character(US_election$gay_adopt),
                                               "1" = 1,
                                               "2" = 0)

#check the variable
# #table(US_election$trans_support_score)
# table(US_election$job_discrim_score)
# table(US_election$gay_marriage_score)
# table(US_election$gay_adopt_score)


#now, its the time to construct the variable 
US_election$LGBT_friendly <- with(US_election, 
                                  trans_support_score + 
                                    job_discrim_score + 
                                    gay_marriage_score + 
                                    gay_adopt_score)



#and then put all the variable into the three large set 
US_election$LGBT_friendly_group <- cut(
  US_election$LGBT_friendly,
  breaks = c(-1, 3, 6, 10),
  labels = c("Low Support", "Moderate Support", "High Support")
)

#remove the NA's
 na.omit(US_election$LGBT_friendly_group)

############################################################################
#8. Income##############################
# Ensure income is numeric and keep only valid values (1-22)
US_election <- US_election %>%
  mutate(income = as.numeric(income)) %>%
  filter(income >= 1 & income <= 22)

# Create income groups for analysis
US_election <- US_election %>%
  mutate(
    income_grouped = cut(income, 
                         breaks = c(1, 5, 10, 15, 20, 22),  # Define group boundaries
                         labels = c("Low", "Lower-Mid", "Mid", "Upper-Mid", "High"), 
                         include.lowest = TRUE)
  )

US_election <- na.omit(US_election)
summary(US_election$income_grouped)

##############################
#9.plot_the_graphy###############################
data <- US_election  

# List of categorical variables to plot
categorical_vars <- c("transgender_policy", "tax_on_millionaries", "government_waste_tax_money", 
                      "edu_summary", "party_hand_tax", "illegel_immiggration", 
                      "Lgbgt_job_discrimination", "race", "gay_marriaige", "gay_adopt", 
                      "income", "transgender_policy", "religious_binary", "religions_group")

# Loop to create bar plots for each categorical variable
for (var in categorical_vars) {
  if (var %in% colnames(data)) {  # Ensure the variable exists in the dataset
    p <- ggplot(data, aes_string(x = var)) +
      geom_bar(fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(title = paste("Distribution of", var), x = var, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
    
    print(p)  # Print each plot
  } else {
    message(paste("Variable", var, "not found in dataset."))
  }
}

################################################
#10.race!###########################################


# Clean race variable in US_election dataset
US_election <- US_election %>%
  mutate(race = as.numeric(race)) %>%  # Convert to numeric
  filter(race >= 1 & race <= 6)  # Keep only valid values

# Convert to a factor for categorical analysis
US_election$race <- factor(US_election$race, 
                           levels = 1:6, 
                           labels = c("White", "Black", "Hispanic", 
                                      "Asian/Pacific Islander", 
                                      "Native American/Alaska Native", 
                                      "Multiple Races"))

# Check summary
summary(US_election$race)

# View first few rows to confirm cleaning
head(US_election)

US_election <- na.omit(US_election)


################################################



#10.The model construction######################

#remove the NA value
US_election <- na.omit(US_election)

table(US_election$race, US_election$religion_group)

#note:here is using the package called Svyry which is a shit package designed 
#for the survery data? god bless me

design <- svydesign(
  id = ~V200010c,        # Primary Sampling Unit (PSU)
  strata = ~V200010d,    # Stratification variable
  weights = ~weight,   # Full sample post-election weight
  data = US_election,    # Your dataset
  nest = TRUE            # Ensures PSU is correctly nested within strata
)
##################################################
##model_one_race###########
#list all the variable
  #tax_on_millionaries
  #government_waste_tax_money
  #edu_summary
  #party_hand_tax
  #illegel_immiggration
  #race
  #income_grouped
  #LGBT_friendly_group
  #religion_group

#########
#The_stage_one
model1_stage1_tax <- svyolr(government_waste_tax_money ~ race, design = design)

#

#the_stage_two
model1_stage2_tax <- svyolr(government_waste_tax_money ~ 
                              race + 
                              religion_group +
                              tax_on_millionaries + 
                              edu_summary +
                              party_hand_tax_group + 
                              income_grouped +
                              illegel_immiggration +
                              LGBT_friendly_group, 
                            design = design)


summary(model1_stage1_tax)
summary(model1_stage2_tax)
#note: the race has the impact but not a lot


################################################################################

#regression_two race vs LGBTQ
model2_stage1_LGBTQ_trans <- svyolr(LGBT_friendly_group ~ race, design = design)


model2_stage2_LGBTQ_trans  <- svyolr(LGBT_friendly_group ~ 
                                       race + 
                                       religion_group +
                                       tax_on_millionaries + 
                                       edu_summary +
                                       party_hand_tax_group + 
                                       income_grouped +
                                       illegel_immiggration +
                                       government_waste_tax_money,
                                     design = design)

#####The income group############################################################

#The_stage_one
model3_stage1_income <- svyolr(government_waste_tax_money ~ income_grouped, design = design)

#

#the_stage_two
model3_stage2_income <- svyolr(government_waste_tax_money ~ 
                                 race + 
                                 religion_group +
                                 tax_on_millionaries + 
                                 edu_summary +
                                 party_hand_tax_group + 
                                 income_grouped +
                                 illegel_immiggration +
                                 LGBT_friendly_group, 
                               design = design)


summary(model1_stage1_tax)
summary(model1_stage2_tax)
############



# Get coefficients
race_stage1 <- coef(model1_stage1_tax)[grep("^race", names(coef(model1_stage1_tax)))]
race_stage2 <- coef(model1_stage2_tax)[grep("^race", names(coef(model1_stage2_tax)))]

# Create data frame
race_effects <- data.frame(
  Race_Group = gsub("race", "", names(race_stage1)),
  Stage1 = as.numeric(race_stage1),
  Stage2 = as.numeric(race_stage2)
)

# Reshape for plotting
race_long <- pivot_longer(race_effects, cols = c(Stage1, Stage2), names_to = "Model", values_to = "Coefficient")

ggplot(race_long, aes(x = Race_Group, y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Race Effects on Perceptions of Government Waste",
       subtitle = "Ordinal Logistic Regression (Stage 1 vs Stage 2)",
       x = "Race Group", y = "Coefficient Estimate") +
  theme_economist() +
  scale_fill_economist()



# Extract coefficients
income_stage1 <- coef(model3_stage1_income)[grep("^income_grouped", names(coef(model3_stage1_income)))]
income_stage2 <- coef(model3_stage2_income)[grep("^income_grouped", names(coef(model3_stage2_income)))]

# Combine into a data frame
income_effects_tax <- data.frame(
  Income_Group = gsub("income_grouped", "", names(income_stage1)),
  Stage1 = as.numeric(income_stage1),
  Stage2 = as.numeric(income_stage2)
)

# Reshape for plotting
income_long_tax <- pivot_longer(income_effects_tax, cols = c(Stage1, Stage2), names_to = "Model", values_to = "Coefficient")



ggplot(income_long_tax, aes(x = Income_Group, y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Income Effects on Perceptions of Government Waste",
       subtitle = "Ordinal Logistic Regression (Stage 1 vs Stage 2)",
       x = "Income Group", y = "Coefficient Estimate") +
  theme_economist() +
  scale_fill_economist()



################################################################################

#regression_two race vs LGBTQ
model2_stage1_LGBTQ_trans <- svyolr(LGBT_friendly_group ~ race, design = design)
  
  
model2_stage2_LGBTQ_trans  <- svyolr(LGBT_friendly_group ~ 
                              race + 
                              religion_group +
                              tax_on_millionaries + 
                              edu_summary +
                              party_hand_tax_group + 
                              income_grouped +
                              illegel_immiggration +
                              government_waste_tax_money,
                            design = design)

#####The income group############################################################

#The_stage_one
model3_stage1_income <- svyolr(government_waste_tax_money ~ income_grouped, design = design)

#

#the_stage_two
model3_stage2_income <- svyolr(government_waste_tax_money ~ 
                              race + 
                              religion_group +
                              tax_on_millionaries + 
                              edu_summary +
                              party_hand_tax_group + 
                              income_grouped +
                              illegel_immiggration +
                              LGBT_friendly_group, 
                            design = design)


summary(model1_stage1_tax)
summary(model1_stage2_tax)
#note: the race has the impact but not a lot


################################################################################

#regression_two income vs LGBTQ
model3_stage1_LGBTQ_income <- svyolr(LGBT_friendly_group ~ income_grouped, design = design)

model3_stage2_LGBTQ_income  <- svyolr(LGBT_friendly_group ~ 
                                        race + 
                                        religion_group +
                                        tax_on_millionaries + 
                                        edu_summary +
                                        party_hand_tax_group + 
                                        income_grouped +
                                        illegel_immiggration +
                                        government_waste_tax_money,
                                      design = design)


# Extract coefficients
coef_stage1 <- coef(model3_stage1_LGBTQ_income)
coef_stage2 <- coef(model3_stage2_LGBTQ_income)

# Filter only income-related terms
income_stage1 <- coef_stage1[grep("^income_grouped", names(coef_stage1))]
income_stage2 <- coef_stage2[grep("^income_grouped", names(coef_stage2))]


# You should already have this:
income_effects <- data.frame(
  Income_Group = gsub("income_grouped", "", names(income_stage1)),
  Stage1 = as.numeric(income_stage1),
  Stage2 = as.numeric(income_stage2)
)

print(income_effects)


# Optional: Reshape data for ggplot2
library(tidyr)
library(dplyr)

income_long <- income_effects %>%
  pivot_longer(cols = c(Stage1, Stage2), names_to = "Model", values_to = "Coefficient")

library(ggplot2)

ggplot(income_long, aes(x = Income_Group, y = Coefficient, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Effect of Income Group on LGBT Support",
       subtitle = "Ordinal Logistic Regression Coefficients (Stage 1 vs Stage 2)",
       x = "Income Group",
       y = "Coefficient Estimate",
       fill = "Model") +
  theme_minimal(base_size = 14)




