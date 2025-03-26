
##############################################
######lode the pacakage########
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
raw <- load("~/Documents/GitHub/Data-5000-Machine-learning-project/moeding_data.RData")

####check the data #####
#display the dataset
#head(raw)



#9.plot_the_grap###############################
#9.1 first try some sampling ploting

data <- US_election  

# List of categorical variables to plot
categorical_vars <- c("transgender_policy", "tax_on_millionaries", "government_waste_tax_money", 
                      "edu_summary", "party_hand_tax", "illegel_immiggration", 
                      "Lgbgt_job_discrimination", "race", "gay_marriaige", "gay_adopt", 
                      "income", "transgender_policy", "religious_binary", "religions_group", 
                      "LGBT_friendly_group", "income_grouped")

# Loop to create bar plots for each categorical variable
for (var in categorical_vars) {
  if (var %in% colnames(data)) {  # Ensure the variable exists in the dataset
    p <- ggplot(data, aes_string(x = var)) +
      geom_bar(fill = "steelblue", color = "black") +
      theme_economist() +  # Apply Economist theme
      labs(title = paste("Distribution of", var), x = var, y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
    
    print(p)  # Print each plot
  } else {
    message(paste("Variable", var, "not found in dataset."))
  }
}



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


##Race
############################################################################
#regression_once race vs government waste tax money
############################################################################
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



summary(model1_stage1_tax)$df
summary(model1_stage2_tax)
############################################################################
#regression_two race vs LGBTQ
############################################################################
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

summary(model2_stage2_LGBTQ_trans)
##income
############################################################################
#income - government waste money
############################################################################
model3_stage1_income <- svyolr(government_waste_tax_money ~ income_grouped, design = design)

summary(model3_stage1_income )
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

summary(model3_stage2_income)

############################################################################
#income vs LGBTQ
############################################################################
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

summary(model3_stage1_LGBTQ_income)
summary(model3_stage2_LGBTQ_income )



############################################################################








