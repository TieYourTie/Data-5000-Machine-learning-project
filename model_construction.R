
################
#note: The code here is for the futher cleaning data to better build the model. 
#v0.1 update the model consturction and using the Survery package to reconstruct the 
################

###
#For the future people here, this code here is for the data cleanin process of the 2020 American National election selection data.

#####clean the environment and lode the package#################################
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
library(haven)          #the package can able lode the stata file
library(readr)
library(ggplot2)
library(dplyr)
library(survey)
###################################################################################

raw <- read_csv("cleaned_data.csv")



#display the dataset
#head(raw)

#rename the variable
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


########prcess_the_religion####################################

# 1. Christianity
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

# 2. Judaism
jewish_codes <- c(
  502, 503, 524, 650  # Orthodox, Conservative, Reform, Messianic Judaism
)

# 3. Islam
islam_codes <- c(
  720  # Islam
)

# 4. Eastern Religions
eastern_religions_codes <- c(
  721, 722, 723, 730, 732  # Buddhism, Hinduism, Baha'i, Sikhism, Konko Church
)

# 5. Indigenous & New Religious Movements
indigenous_new_religions_codes <- c(
  724, 725, 726, 727, 735, 736, 740, 750, 790, 870, 879  # Native American religions, New Age, Wicca, Paganism, etc.
)

# 6. Other
other_religions_codes <- c(
  695  # Multiple religious affiliations
)

# 7. Nonreligious
nonreligious_codes <- c(
  880, 888, 889  # No religion, unsure, refused to answer
)

########Function to recode a single religion code########################

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

# =============================================================
# Step 3. Vectorized function for multiple codes
# -------------------------------------------------------------
recode_religion_vec <- Vectorize(recode_religion)

# =============================================================
# Step 4. Apply to dataset for cleaning
# -------------------------------------------------------------
clean_religion_data <- function(df, religion_column) {
  df[[religion_column]] <- recode_religion_vec(df[[religion_column]])  # Apply recoding
  df[[religion_column]] <- factor(df[[religion_column]], levels = c(
    "Christianity", "Judaism", "Islam", "Eastern Religions",
    "Indigenous & New Religious Movements", "Other", "Nonreligious"
  ))  # Convert to factor
  
  return(df)
}

# =============================================================
# Step 6. Optional: Convert to binary variable for logistic regression
# -------------------------------------------------------------

#create new variable for religest and non-religoon
US_election <- clean_religion_data(US_election, "religion")
US_election$religious_binary <- ifelse(US_election$religion== "Nonreligious", 0, 1)

#remove the na
US_election <- US_election[!is.na(US_election$religious_binary), ]



US_election_clean <- US_election %>% dplyr::select(-`...1`, -V200004, -V200006, -V200008, -V201007a, -V201566, -V201410, -V200007, -V201014b)

#remove the NA value
US_election_clean <- na.omit(US_election_clean)

# 选择不包括 V200010c, V200010d, weight 这三列的数据框
US_election_clean <- US_election_clean %>%
  filter(if_all(-c(V200010c, V200010d, weight), ~ . >= 0))


# 过滤数据，确保 religion 保留，V200010c, V200010d, weight 允许负数，其他列不能有负数
filtered_data <- US_election_clean %>%
  filter(if_all(-c(V200010c, V200010d, weight, religion), ~ . >= 0))

# 查看处理后的数据
head(filtered_data)


design <- svydesign(
  id = ~V200010c,        # Primary Sampling Unit (PSU)
  strata = ~V200010d,    # Stratification variable
  weights = ~weight,   # Full sample post-election weight
  data = filtered_data,    # Your dataset
  nest = TRUE            # Ensures PSU is correctly nested within strata
)



model1_stage1_tax <- (government_waste_tax_money ~ race, design = design)


model_race_1 <- svyolr(factor(government_waste_tax_money) ~ factor(race), design = design)
summary(model_race_1)

model_income_2 <- svyolr(factor(government_waste_tax_money) ~ income + race + education + political_affiliation, design = design)
summary(model_income_2)





