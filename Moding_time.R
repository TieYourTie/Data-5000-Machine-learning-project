
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
#update: this is the final cleaning processing
########################


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


################################################
#10.The model construction
################################################

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



summary(model1_stage1_tax)
summary(model1_stage2_tax)
summary(model1_stage1_tax)$df
summary(model1_stage2_tax)$df
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

summary(model2_stage1_LGBTQ_trans)
summary(model2_stage2_LGBTQ_trans)
summary(model2_stage1_LGBTQ_trans)$df
summary(model2_stage2_LGBTQ_trans)$df
##income
############################################################################
#income - government waste money
############################################################################
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

summary(model3_stage1_income)
summary(model3_stage2_income )
summary(model3_stage1_income) $df
summary(model3_stage2_income )$df

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
summary(model3_stage2_LGBTQ_income)
summary(model3_stage1_LGBTQ_income) $df
summary(model3_stage2_LGBTQ_income) $df

############################################################################
install.packages(ggeffects)
library(ggeffects)
pred1 <- ggpredict(model3_stage1_LGBTQ_income, terms = "income_grouped")
plot(pred1) + ggtitle("收入 vs 支持 LGBTQ 群体的态度")

pred2 <- ggpredict(model3_stage2_LGBTQ_income, terms = "income_grouped")
plot(pred2) + ggtitle("控制变量后：收入 vs LGBTQ 态度")


############################################################################
model4_stage1_tax_on_millionaries <- svyolr(tax_on_millionaries ~ income_grouped, design = design)

model4_stage2_tax_on_millionaries <- svyolr(tax_on_millionaries ~  
                                        LGBT_friendly_group + 
                                        race + 
                                        religion_group +
                                        edu_summary +
                                        party_hand_tax_group + 
                                        income_grouped +
                                        illegel_immiggration +
                                        government_waste_tax_money,
                                      design = design)

summary(model4_stage1_tax_on_millionaries )
summary(model4_stage2_tax_on_millionaries )
summary(model4_stage1_tax_on_millionaries )$df
summary(model4_stage2_tax_on_millionaries )$df







