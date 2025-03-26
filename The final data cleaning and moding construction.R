
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
  income = V20tab2468x, 
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

#########################################
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

#save the file
save(US_election, file = "moeding_data.RData")




