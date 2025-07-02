# Load packages

library(tidyverse) # Tidy (clean and readable) coding
library(haven)     # Read SPSS, SAS, and Stata files
library(labelled)  # creating labels for variables (info under variable names)
library(table1)    # creating descriptive tables
library(naniar)    # Missing data
library(lubridate) # Date manipulation
library(VIM)       # Missing data
library(stringr)   # String manipulation
library(tableone)    # creating descriptive tables
library(knitr)       # for tables 
library(kableExtra)  # optional, for extra styling
library(htmlTable)   # for tables
library(statip)      # mfv function (mode)

################################
#####Outcome data extraction####
################################

#################### longitools harmonized dataset (long data format)
if(!exists("c66_harm")){
  c66_harm <- read.table("//kaappi.oulu.fi/nfbc$/projects/P0511/HARMONISED DATA/Data/LongITools_NFBC1966_long_format.txt",
                         header = T)
}

c66_46_longit <- c66_harm %>% 
  select(id, variable, measure, age) %>%
  filter(variable %in% c("hypertension_med", "edu", "smk_ever", "alc_unit", "pa_mod_vig","occup"), age > 45) %>%
  pivot_wider(names_from = variable, values_from = measure) %>%
  group_by(id) %>%
  mutate(
    #hypertension medication categorization
    c66_46_ht_med = factor(
      case_when(hypertension_med == 0 ~ "No",
                hypertension_med == 1 ~ "Yes"),
      levels = c("No", "Yes")
    ),
    #education categorization
    c66_46_edu = factor(
      case_when(edu == 1 ~ "High",
                edu == 2 ~ "Intermediate",
                edu == 3 ~ "Low"),
      levels = c("High", "Intermediate", "Low")
    ),
    #smoking categorization
    c66_46_smok = factor(
      case_when(smk_ever == 0 ~ "Never",
                smk_ever == 1 ~ "Former smoker",
                smk_ever == 2 ~ "Current smoker"),
      levels = c("Never", "Former smoker", "Current smoker")
    ),
    #alcohol intake categorization
    c66_46_alc = factor(
      case_when(alc_unit == 0 ~ "Never",
                alc_unit == 1 ~ "Light",
                alc_unit == 2 ~ "Moderate",
                alc_unit == 3 ~ "Moderate to Heavy",
                alc_unit == 4 ~ "Heavy"),
      levels = c("Never", "Light", "Moderate", "Moderate to Heavy", "Heavy")
    ),
    #occupational status categorization
    c66_46_occup = factor(
      case_when(occup == 1 ~ "Employed",
                occup == 2 ~ "Self-employed",
                occup == 3 ~ "Unemployed",
                occup == 4 ~ "Student, apprentice",
                occup == 5 ~ "Domestic tasks",
                occup == 6 ~ "Inactive/other"),
      levels = c("Employed", "Self-employed", "Unemployed", "Student, apprentice", "Domestic tasks", "Inactive/other")
    )
  ) %>%
  ungroup() %>%
  rename(project_ID = id, c66_46_phys= pa_mod_vig) %>%
  dplyr::select(project_ID, c66_46_ht_med, c66_46_edu, c66_46_smok, c66_46_alc, c66_46_phys, c66_46_occup)

###check for duplicates (long format)
# Count occurrences of IDs
id_counts <- c66_46_longit %>%
  group_by(project_ID) %>%
  summarise(count = n())

# Filter IDs that appear more than once
dup_ids <- id_counts %>% filter(count > 1) #no duplicates

#Add labels to variables:
#c66_46_ht_med take only codes starting with C02, C03, C07, C08 and C09 corresponding to:
# C02 ANTIHYPERTENSIVES
# C03 DIURETICS
# C07 BETA BLOCKING AGENTS
# C08 CALCIUM CHANNEL BLOCKERS
# C09 AGENTS ACTING ON THE RENIN-ANGIOTENSIN SYSTEM
var_label(c66_46_longit$c66_46_ht_med) <- "Hypertension medication at 46y"

# 0) 0 (Never drinking alcohol)
# 1) Light (< 3 units per week)
# 2) Moderate (>=3 - <7 units per week)
# 3) Moderate to heavy (>=7 - <14 units per week)
# 4) Heavy (>=14 units per week)
var_label(c66_46_longit$c66_46_alc) <- "Alcohol intake at 46y"

#"Classification according to International Standard Classification of Education 97/2011 (ISCED-97/2011). 
#1) High: matriculation examination and polytechnic education, university degree, some other, education unfinished or university degree
#2) Medium: vocational school, post-secondary education, polytechnic education
#3) Low: less than 9 years comprehensive school or no occupational education, vocational training course
var_label(c66_46_longit$c66_46_edu) <- "Education at 46y"

#Regular smoker is defined as usually smoking at least one cigarette per week.
var_label(c66_46_longit$c66_46_smok) <- "Smoking at 46y"

#Total duration of moderate to vigorous physical activity per week
var_label(c66_46_longit$c66_46_phys) <- "Physical activity at 46y h/week" 

# Occupational status of the study participant. Provide repeated measures in
# long format with corresponding age in years, Categorical
# 1) Employed
# 2) Self-employed
# 3) Unemployed
# 4) Student, apprentice, student
# 5) Domestic tasks (housewife etc.)
# 6) Inactive/other (Receiving benefits or pension etc.)
var_label(c66_46_longit$c66_46_occup) <- "Occupational status at 46y"

summary(c66_46_longit)

#################### Sex, Age at clinical exam. 46-years follow-up dataset
if (!exists("c66_ages")) {
  c66_ages <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/p0511_c66_ages.sas7bdat")
}

c66_ap_46 <- c66_ages %>% 
  dplyr::select(project_ID, C6646Q1_age, C6646C_age, gender) %>%
  mutate(
    #Age at clinical examination (questionnaire if no clinical exam info)
    c66_46_ap_age = case_when(
      is.na(C6646C_age) ~ C6646Q1_age,
      !is.na(C6646C_age) ~ C6646C_age   
    ),
    #sex categorization
    sex = factor(
      gender,         
      levels = c(1, 2),   
      labels = c("Male", "Female") 
    )
  ) %>%
  dplyr::select(project_ID,c66_46_ap_age, sex)

#Add labels to variables
var_label(c66_ap_46$c66_46_ap_age) <- "Age at 46y clinical examination"

summary(c66_ap_46)

#################### clinical measures, CIMT, date of clinical examination, exposures(monthy and daily), location, and metabolomics datasets
if(!exists("c66_46_d")){
  c66_46_d <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/p0511_c6646_data_p0005_154.sas7bdat")
  c66_46_c_cimt <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/p0511_c6646_poiminta1_p0684.sas7bdat")
  c66_46_c_date <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/p0511_c6646_poiminta2_p0684.sas7bdat")
  c66_46_met <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/p0511_c6646b_nmr_metabolomics.sas7bdat")
  c66_46_med <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/p0511_c6646t_medication.sas7bdat")
  #c66_31_duty <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/p0511_c66_0_31_data.sas7bdat")
  }

###################### Extracting clinical data (outcome=BP,physical activity, hypertension quest, diabetes quest, cholesterol) 
vars_c66_46 <- c("project_ID",
                 "C6646C_weigth_height_007",            # clinically measured BMI
                 "C6646Q1_63_1_1",                      # Self reported dd HT
                 "C6646Q1_63_2_1",                      # Self reported dd diabetes type 1
                 "C6646Q1_63_2_2",                      # Self reported dd diabetes type 2
                 "C6646C_brachial_bp_003",              # Clinical Exam sys bp1
                 "C6646C_brachial_bp_004",              # CE di bp1
                 "C6646C_brachial_bp_006",              # CE sys bp2
                 "C6646C_brachial_bp_007",              # CE di bp2
                 "C6646C_brachial_bp_009",              # CE sys bp3
                 "C6646C_brachial_bp_010",              # CE di bp3
                 "C6646C_CU_IMTavgLeftCCA",             # Carotid IMT left average mm
                 "C6646C_CU_IMTavgRightCCA",            # Carotid IMT right average mm
                 "t2m_c",                               # air temperature at 2 m in Celsius
                 "rh",                                  # relative humidity in %
                 "ws",                                  # wind speed in m/s
                 "y",                                   # year of the temperature measurement
                 "m",                                   # month of the temperature measurement
                 "C6646B_Serum_C",                      # total Serum cholesterol mmol/l
                 "C6646B_Glc",                          #fasting glucose mmol/l
                 "C6646T_medic_atc",                    #medicines taken ATC codes
                 "C6646B_plasma_fcmix_ogtt_120m_1"      #glucose tolerance test -2h glucose measurement
                 )

c66_46_d1 <- c66_46_d %>% 
  dplyr::select(any_of(vars_c66_46)) %>%
  rename(c66_46_q1_ht=C6646Q1_63_1_1,
         c66_46_q1_dbt1=C6646Q1_63_2_1,
         c66_46_q1_dbt2=C6646Q1_63_2_2,
         c66_46_c_bmi=C6646C_weigth_height_007,
         c66_46_sybp1=C6646C_brachial_bp_003,
         c66_46_dibp1=C6646C_brachial_bp_004,
         c66_46_sybp2=C6646C_brachial_bp_006,
         c66_46_dibp2=C6646C_brachial_bp_007,
         c66_46_sybp3=C6646C_brachial_bp_009,
         c66_46_dibp3=C6646C_brachial_bp_010,
         c66_46_gluc2h=C6646B_plasma_fcmix_ogtt_120m_1
         )

c66_46_d2 <- c66_46_d1 %>%
  group_by(project_ID) %>%
  mutate(
    #hypertension categorization based on the questionnaire
    c66_46_q1_ht = factor(c66_46_q1_ht, c(1, 2), c("No", "Yes")),
    #take the average of the 3 measurements
    c66_46_avrsybp = rowMeans(
      cbind(c66_46_sybp1, c66_46_sybp2, c66_46_sybp3),
      na.rm = TRUE
    ),
    c66_46_avrsybp = ifelse(is.nan(c66_46_avrsybp), NA, c66_46_avrsybp),
    #take the average of the 3 measurements
    c66_46_avrdibp = rowMeans(
      cbind(c66_46_dibp1, c66_46_dibp2, c66_46_dibp3),
      na.rm = TRUE
    ),
    c66_46_avrdibp = ifelse(is.nan(c66_46_avrdibp), NA, c66_46_avrdibp),
    #hypertension categorization based on the clinical examination
    c66_46_c_ht = case_when(
      c66_46_avrsybp >= 140 ~ 1,
      c66_46_avrdibp >= 90 ~ 1, 
      c66_46_avrsybp < 140 & c66_46_avrdibp < 90 ~ 0
    ),
    #hypertension based on the questionnaire
    c66_46_c_ht = factor(c66_46_c_ht, c(0, 1), c("No", "Yes")),
    #diabetes questionaire categorization into yes/no
    c66_46_q1_dbt1 = factor(c66_46_q1_dbt1, c(1, 2), c("No", "Yes")),
    c66_46_q1_dbt2 = factor(c66_46_q1_dbt2, c(1, 2), c("No", "Yes")),
    #bmi categorization
    c66_46_c_bmi_cat = factor(
      case_when(
      c66_46_c_bmi < 30 ~ "Nonobese",
      c66_46_c_bmi >= 30 ~  "Obese"),
      levels = c("Nonobese", "Obese"))
    )%>%
  ungroup() %>%
  dplyr::select(project_ID,c66_46_q1_dbt1,c66_46_q1_dbt2,
                c66_46_q1_ht,c66_46_c_ht,c66_46_avrsybp,c66_46_avrdibp,c66_46_c_bmi,c66_46_gluc2h,
                c66_46_c_bmi_cat)

var_label(c66_46_d2$c66_46_q1_dbt1) <- "Self-reported diabetes type 1 at 46y"
var_label(c66_46_d2$c66_46_q1_dbt2) <- "Self-reported diabetes type 2 at 46y"
var_label(c66_46_d2$c66_46_q1_ht) <- "Self-reported hypertension at 46y"
var_label(c66_46_d2$c66_46_c_ht) <- "Hypertension at46 clinical exam (sysbp>=140 or dibp>=90)"
var_label(c66_46_d2$c66_46_avrsybp) <- "Mean Systolic BP 46y"
var_label(c66_46_d2$c66_46_avrdibp) <- "Mean Diastolic BP 46y"
var_label(c66_46_d2$c66_46_c_bmi) <- "BMI at 46y"
var_label(c66_46_d2$c66_46_c_bmi_cat) <- "Normal or Obese"
var_label(c66_46_d2$c66_46_gluc2h) <- "2h glucose at 46y"

summary(c66_46_d2)

#################### Extracting metabolomics data (all variables are characters - transform to numeric)
c66_46_d3 <- c66_46_met %>% 
  dplyr::select(any_of(vars_c66_46)) %>%
  rename(c66_46_chol = C6646B_Serum_C,
         c66_46_glc = C6646B_Glc) %>%
  # Replace characters "NA" with NA
  mutate(
    across(
      everything(), 
      ~ replace(., . %in% c("NA", "TAG", "NaN"), NA)  # Replace multiple values with NA
    ),
    # Convert all columns to numeric
    c66_46_chol = as.numeric(gsub(",", ".", c66_46_chol)),
    c66_46_glc = as.numeric(gsub(",", ".", c66_46_glc))
  )

var_label(c66_46_d3$c66_46_chol) <- "Total Serum cholesterol at 46y"
var_label(c66_46_d3$c66_46_glc) <- "Fasting glucose at 46y"

summary(c66_46_d3)

#################### Extracting medicines data (long format), merging with d3 and d2 and create columns of diabetes and hypercholesterolemia yes/no
# Process data to categorize diseases based on medications and other variables(gluc,chol,diabetes questions)
c66_46_d4 <- c66_46_med %>% 
  #full join to include all unique project_IDs (because c66_46_med is only 5384 observations and we base our categorization on several variables)
  full_join(c66_46_d3, by = "project_ID") %>%
  full_join(c66_46_d2, by = "project_ID") %>%
  full_join(c66_46_longit, by = "project_ID") %>%
  rename(c66_46_med_atc = C6646T_medic_atc) %>%
  mutate(
    # Introducing NAs in empty cells of medicines taken
    c66_46_med_atc = ifelse(c66_46_med_atc %in% c(""," "), NA, c66_46_med_atc)
  ) %>%
  # Summarize information at the participant level
  group_by(project_ID) %>%
  summarise(
    # Diabetes categorization: check if any row has A10 medication (case nonsensitive and starts with) or other diabetes indicators
    #any - checks all rows (grouped by a subject) for the condition and returns TRUE if any of them is TRUE
    #case_when - checks the conditions in order and returns the first TRUE value
    c66_46_diab = factor(
      case_when(
        any(str_detect(c66_46_med_atc, "(?i)^A10")) ~ "Yes",
        any(c66_46_gluc2h >= 11.1) ~ "Yes",
        any(c66_46_q1_dbt1 == "Yes" | c66_46_q1_dbt2 == "Yes") ~ "Yes",
        any(c66_46_glc >= 7.0) ~ "Yes",
        any(c66_46_gluc2h < 11.1) ~ "No",
        any(c66_46_glc < 7.0) ~ "No",
        any(c66_46_q1_dbt1 == "No" & c66_46_q1_dbt2 == "No") ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    # Hypercholesterolemia categorization: check if any row has C10 medication (case nonsensitive and starts with) or cholesterol levels
    c66_46_hpcl = factor(
      case_when(
        any(str_detect(c66_46_med_atc, "(?i)^C10")) ~ "Yes",           
        any(c66_46_chol >= 6.216) ~ "Yes",
        any(c66_46_chol < 6.216) ~ "No",
      ),
      levels = c("No", "Yes")
    ),
    # Hypertension categorization: check if any row has medication or other hypertension indicators
    c66_46_ht_comb = factor(
      case_when(
        any(c66_46_ht_med == "Yes") ~ "Yes",           
        any(c66_46_q1_ht == "Yes") ~ "Yes",
        any(c66_46_c_ht == "Yes") ~ "Yes",
        any(c66_46_ht_med == "No") ~ "No",
        any(c66_46_q1_ht == "No") ~ "No",
        any(c66_46_c_ht == "No") ~ "No"
      ),
      levels = c("No", "Yes")) 
  ) %>%
  ungroup() 

var_label(c66_46_d4$c66_46_diab) <- "Diabetes at 46y(medication, glucose, gluc.test, questionnaire)"
var_label(c66_46_d4$c66_46_hpcl) <- "Hypercholesterolemia at 46y (medication, cholesterol)"
var_label(c66_46_d4$c66_46_ht_comb) <- "Hypertension combined at 46y (-medication(NO), questionnaire, clinical exam)"

###check for duplicates (long format)
# Count occurrences of IDs
id_counts <- c66_46_d4 %>%
  group_by(project_ID) %>%
  summarise(count = n())

# Filter IDs that appear more than once
dup_ids <- id_counts %>% filter(count > 1)  #no duplicates

summary(c66_46_d4)

#################### Carotid IMT left and right average extraction
c66_46_d5 <- c66_46_c_cimt %>% 
  dplyr::select(any_of(vars_c66_46)) %>%
  rename(c66_46_cimt_l_avg=C6646C_CU_IMTavgLeftCCA,
         c66_46_cimt_r_avg=C6646C_CU_IMTavgRightCCA) %>%
  select(project_ID, c66_46_cimt_l_avg, c66_46_cimt_r_avg) 

var_label(c66_46_d5$c66_46_cimt_l_avg) <- "Carotid IMT left average at 46y"
var_label(c66_46_d5$c66_46_cimt_r_avg) <- "Carotid IMT right average at 46y"

summary(c66_46_d5)

#################### Exact date of clinical examination for each participant + season of assessment
c66_46_d6 <- c66_46_c_date %>% 
  select(project_ID,clinical_date) %>%
  rename(c66_46_c_date=clinical_date) %>%
  # Convert to date format
  mutate(c66_46_c_date=ymd(c66_46_c_date),
         # Extract month from the date
         c66_46_season = factor(
           case_when(
             month(c66_46_c_date) %in% c(12, 1, 2) ~ "Winter",
             month(c66_46_c_date) %in% 3:5 ~ "Spring",
             month(c66_46_c_date) %in% 6:8 ~ "Summer",
             month(c66_46_c_date) %in% 9:11 ~ "Autumn"
           ),
           levels = c("Winter", "Spring", "Summer", "Autumn")),
         # Extract month from the date
         c66_46_season_c_w = factor(
           case_when(
             month(c66_46_c_date) %in% c(11, 12, 1, 2, 3, 4) ~ "Cold",
             month(c66_46_c_date) %in% 5:10 ~ "Warm",
           ),
           levels = c("Cold", "Warm"))
         ) 

var_label(c66_46_d6$c66_46_c_date) <- "Date of clinical examination at 46y"
var_label(c66_46_d6$c66_46_season) <- "Season of clinical examination at 46y"
var_label(c66_46_d6$c66_46_season_c_w) <- "Cold/Warm ar clinical examination at 46y"

summary(c66_46_d6)

#################### Outdoor workers at 31 years (DEACTIVATED)
#c66_31_w <- c66_31_duty %>% 
  #select(project_ID, ZP2601) %>%
  #rename(
    #c66_31_out_on = ZP2601       # Outdoors on duty
  #) %>%
  #mutate(
    # Outdoors on duty categorization
    #c66_31_worker = factor(
      #case_when(
        #c66_31_out_on > 0  ~ "Yes", 
        #c66_31_out_on==0 ~ "No"
      #),
      #levels = c("No", "Yes")
    #)
  #) %>%
  #select(project_ID, c66_31_worker)

    
#var_label(c66_31_w$c66_31_worker) <- "Outdoor workers at 31y"

#summary(c66_31_w)
#################### First merging of the datasets (variables except exposure)  
# Perform full joins to include all unique project_IDs  
c66_merged_outcome <- c66_46_d2 %>%
  full_join(c66_46_longit, by = "project_ID") %>%
  full_join(c66_ap_46, by = "project_ID") %>% 
  full_join(c66_46_d3, by = "project_ID") %>%
  full_join(c66_46_d4, by = "project_ID") %>%
  full_join(c66_46_d5, by = "project_ID") %>%
  full_join(c66_46_d6, by = "project_ID") #%>%
  #full_join(c66_31_w, by = "project_ID") 

#maybe include concentrations for better NA imputation, but the multicollinearity might be a problem

c66_merged_outcome %>%  #missing values
  is.na() %>%
  colSums()

summary(c66_merged_outcome)

################################ 
#####Exposure data extraction###
################################
if(!exists("c66_46_d7")){
c66_46_temp <- read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/nfbc1966_era5_nagrid3x3_t_wide.sas7bdat")
c66_46_daily_t <- read.csv("//kaappi.oulu.fi/nfbc$/projects/P0511/c66_ERA5_daily_temp_2011_2014.csv")
}
c66_46_urb_rur = read_sas("//kaappi.oulu.fi/nfbc$/projects/P0511/P1019/p0511_c66_urban_rural.sas7bdat")
c66_46_daily_hum_ws = read.csv("//kaappi.oulu.fi/nfbc$/projects/P0511/NFBC66_Daily_ERA5_2011_2014.csv")

####################  Prepare urban-rural dataframe (creating a date column with each month of the year and corresponding locations)

#==========================================================
# 1) Identify the DISTINCT (project_ID, year) combinations
#==========================================================
df_distinct <- c66_46_urb_rur %>%
  distinct(project_ID, year)

#==========================================================
# 2) For each (project_ID, year), build a list of 12 monthly dates
#    from January 1 to December 1, then unnest so each month is a row
#==========================================================
df_months <- df_distinct %>%
  rowwise() %>%
  mutate(
    # Build a monthly sequence for that row's "year"
    date_expos = list(
      seq.Date(
        from = as.Date(sprintf("%d-01-01", year)),  # e.g., 2005-01-01
        to   = as.Date(sprintf("%d-12-01", year)),  # e.g., 2005-12-01
        by   = "month"
      )
    )
  ) %>%
  unnest(date_expos) %>%
  ungroup()

#==========================================================
# 3) Prepare your intervals table (converting to Date)
#    so that we only have the columns we need
#==========================================================
df_intervals <- c66_46_urb_rur %>%
  mutate(
    residence_start = as.Date(residence_start),
    residence_end   = as.Date(residence_end)
  ) %>%
  select(project_ID, year, residence_start, residence_end, urban_rural_class)

#==========================================================
# 4) LEFT JOIN the month table with the intervals table
#    on (project_ID, year).  That gives one row for each:
#       (project_ID, year, month) x each interval row
#    Then, if the month is NOT in the [start, end] range, set it to NA
#==========================================================
df_joined <- df_months %>%
  left_join(df_intervals, by = c("project_ID", "year")) %>%
  mutate(
    urban_rural_class = if_else(
      date_expos >= residence_start & date_expos <= residence_end,
      urban_rural_class,    # keep if in range
      NA_character_         # else NA
    )
  )

#==========================================================
# 5) If multiple intervals match the same month, pick the first
#    non-NA "urban_rural_class". Then keep only one row per 
#    (project_ID, year, date_expos).
#==========================================================
df_monthly <- df_joined %>%
  group_by(project_ID, year, date_expos) %>%
  # For the group representing one month, gather all "urban_rural_class" values,
  # pick the first non-NA.  If none, remains NA.
  mutate(urban_rural_class = first(urban_rural_class[!is.na(urban_rural_class)])) %>%
  ungroup() %>%
  # Distinct to reduce duplicates to a single row per month
  distinct(project_ID, year, date_expos, .keep_all = TRUE) %>%
  select(project_ID, year, date_expos, urban_rural_class) %>%
  rename(c66_46_urban_rural = urban_rural_class) %>%
  mutate(c66_46_urban_rural = factor(
    case_when(
      c66_46_urban_rural %in% c("Inner urban area","Outer urban area","Peri-urban area")~"Urban",
      c66_46_urban_rural %in% c("Local centres in rural areas","Rural areas close to urban areas","Rural heartland areas","Sparsely populated rural areas")~"Rural"),
    levels = c("Urban", "Rural"))
  )

#==========================================================
# 6) "Fill-forward" any NA so that once we know
#    the location in a previous month, it stays the same for
#    the subsequent months within the same project_ID
#==========================================================
df_monthly_filled <- df_monthly %>%
  group_by(project_ID) %>%
  arrange(project_ID, year, date_expos) %>%
  fill(c66_46_urban_rural, .direction = "down") %>%
  ungroup()

# "df_monthly_filled" now has exactly one row per 
# (project_ID, year, month).  Any originally NA months 
# get the last known location carried forward.
#==========================================================

####################  Check incorrectly specified NA values
miss_scan_count(c66_46_temp, search = common_na_strings[1:22]) #4259 incorrectly specified NA values
#These values are not missing values, but actual values in the dataset. We will replace them with NA

#################### Create a correct dataset with temperature data
c66_46_d7 <- c66_46_temp %>%
  dplyr::select(any_of(vars_c66_46)) %>%
  # Replace characters "NA" and "TAG" with NA
  mutate(
    across(
      everything(), 
      ~ replace(., . %in% c("NA", "TAG", "NaN"), NA))  
  ) %>%
  # Rename columns
  rename(
    c66_46_temp_m = t2m_c,
    c66_46_rh_m = rh,
    c66_46_ws_m = ws
  ) %>%
  # Convert all columns to numeric (original dataset is character)
  mutate(
    across(-1, ~ as.numeric(gsub(",", ".", .))),
    # Create a date column from year and month and set the day to 1
    date_expos = make_date(y, m, 1) 
  ) %>%
  #join location data by project id and date
  left_join(df_monthly_filled, by = c("project_ID", "date_expos")) %>%
  select(project_ID, date_expos, c66_46_temp_m, c66_46_rh_m, c66_46_ws_m, c66_46_urban_rural) %>%
  # Join with the clinical examination (outcome) dataset
  full_join(c66_merged_outcome, by = "project_ID") %>% 
  # Calculate number of days since clinical exam to the date of exposure measurement (monthly data)
  mutate(days_since_clin_ex = as.numeric(date_expos - c66_46_c_date)) %>%  
  # Keep rows with temperature measurements within 1 year of clinical exam day
  filter(days_since_clin_ex >= 0 & days_since_clin_ex <= 365) %>%
  # group by project_ID 
  group_by(project_ID) %>%
  # summarise the 12 month data for each participant
  summarise(
    # Calculate number of valid months
    valid_months = sum(!is.na(c66_46_temp_m)),
    # Calculate annual mean temperature when => 6 month
    y1_avg_temp = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6, 
      mean(c66_46_temp_m, na.rm = TRUE), 
      NA
    ), 
    # Calculate annual sd temperature when => 6 month
    y1_sd_temp = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6, 
      sd(c66_46_temp_m, na.rm = TRUE), 
      NA
    ),
    # Calculate average apparent temperature of 12 preceding months
    y1_app_temp_avg_46 = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6 & sum(!is.na(c66_46_rh_m)) >= 6 & sum(!is.na(c66_46_ws_m)) >= 6, 
      mean(c66_46_temp_m+0.33*(c66_46_rh_m/100 * 6.105 * exp((17.27*c66_46_temp_m)/(237.7+c66_46_temp_m)))-
             0.7*c66_46_ws_m-4, na.rm = TRUE), 
      NA
    ),
    # Calculate sd apparent temperature of 12 preceding months
    y1_app_temp_sd_46 = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6 & sum(!is.na(c66_46_rh_m)) >= 6 & sum(!is.na(c66_46_ws_m)) >= 6, 
      sd(c66_46_temp_m+0.33*(c66_46_rh_m/100 * 6.105 * exp((17.27*c66_46_temp_m)/(237.7+c66_46_temp_m)))-
           0.7*c66_46_ws_m-4,na.rm = TRUE), 
      NA
    ),
    # Calculate annual avg relative humidity when => 6 month
    y1_avg_rh = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6, 
      mean(c66_46_rh_m, na.rm = TRUE), 
      NA
    ),# Calculate annual avg wind speed when => 6 month
    y1_avg_ws = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6, 
      mean(c66_46_ws_m, na.rm = TRUE), 
      NA
    ),
    # Calculate annual relative humidity sd when => 6 month
    y1_sd_rh = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6, 
      sd(c66_46_rh_m, na.rm = TRUE), 
      NA
    ),
    # Calculate annual wind speed sd when => 6 month
    y1_sd_ws = ifelse(
      sum(!is.na(c66_46_temp_m)) >= 6, 
      sd(c66_46_ws_m, na.rm = TRUE), 
      NA
    ),
    # mode for urban_rural variable
    c66_46_urban_rural = {
      # Convert factor to character so mfv1 returns a character
      tmp <- mfv1(as.character(c66_46_urban_rural), na_rm = TRUE)
      # Convert back to a factor with the desired levels
      factor(tmp, levels = c("Urban", "Rural"))
    }
    )%>%
  ungroup() 

var_label(c66_46_d7$y1_avg_temp) <- "Annual average temperature"
var_label(c66_46_d7$y1_sd_temp) <- "Annual temperature standard deviation"
var_label(c66_46_d7$y1_app_temp_avg_46) <- "Annual average apparent temperature"
var_label(c66_46_d7$y1_app_temp_sd_46) <- "Annual apparent temperature standard deviation"
var_label(c66_46_d7$y1_avg_rh) <- "Annual average relative humidity"
var_label(c66_46_d7$y1_sd_rh) <- "Annual relative humidity standard deviation"
var_label(c66_46_d7$y1_avg_ws) <- "Annual average wind speed"
var_label(c66_46_d7$y1_sd_ws) <- "Annual wind speed standard deviation"


summary(c66_46_d7)

####################  Combine daily temperature data with clinical exam. dates to get the temperature on the day of the outcome assessment

daily_temp <- c66_46_daily_t %>%
  mutate(
    # change "date" format from character to date
    date_daily_mean = ymd(date),
    # rename variable
    day_mean = Tmean_c
         ) %>%
  select(project_ID, date_daily_mean, day_mean)%>%
  full_join(c66_merged_outcome, by = "project_ID") %>%  
  mutate(
    # Calculate number of days since clinical exam to the date of exposure measurement (daily data)
    days_since_clinical = as.numeric(date_daily_mean - c66_46_c_date)
    )%>%
  # Keep rows with temperature measurements on the day of the clinical exam
  filter(days_since_clinical == 0)%>% 
  select(project_ID, day_mean, date_daily_mean)

var_label(daily_temp$day_mean) <- "Temperature on the day of the outcome assessment"
var_label(daily_temp$date_daily_mean) <- "Date of the temperature measurement"

summary(daily_temp)

################################################ 
##### Final dataset: one row per participant ###
################################################

c66_project_df <- c66_merged_outcome %>%
  full_join(c66_46_d7, by = "project_ID")%>%
  full_join(daily_temp, by = "project_ID") %>%
  # Remove rows with missing clinical date and 2h_glucose
  #gluc_2h, cimt.bp data says that people actually visited the clinic for this addittional assessment, but the date is missing only
  filter(!is.na(c66_46_c_date) | !is.na(c66_46_gluc2h) | !is.na(c66_46_avrsybp) |!is.na(c66_46_cimt_l_avg)) %>%
  ungroup() %>%
  select(-c66_46_c_date, -date_daily_mean, -valid_months) #remove variables that are not needed anymore

c66_merged_excl <- c66_merged_outcome %>%
  full_join(c66_46_d7, by = "project_ID")%>%
  full_join(daily_temp, by = "project_ID") %>%
  # Remove rows with missing outcome and exposure variables
  filter(!is.na(c66_46_avrsybp) & !is.na(c66_46_avrdibp) & !is.na(y1_app_temp_avg_46) & !is.na(y1_app_temp_sd_46)) %>%
  ungroup() %>%
  select(-c66_46_c_date, -date_daily_mean, -valid_months) #remove variables that are not needed anymore

# Find rows in data1 that are not in c66_project_df
rows_in_data1_not_in_data2 <- anti_join(c66_merged_excl, c66_project_df)
rows_in_data1_not_in_data2

# Find rows in data2 that are not in c66_merged_excl
rows_in_data2_not_in_data1 <- anti_join(c66_project_df, c66_merged_excl)
rows_in_data2_not_in_data1

#checking missing values
c66_project_df %>%
  is.na() %>%
  colSums() 

summary(c66_project_df)

###check for duplicates (long format)
# Count occurrences of IDs
id_counts <- c66_project_df %>%
  group_by(project_ID) %>%
  summarise(count = n())

# Filter IDs that appear more than once
dup_ids <- id_counts %>% filter(count > 1) #no duplicates

###### Save the final dataset with labels ######
# Define the specific file path 
file_path <- "//kaappi.oulu.fi/nfbc$/projects/P0511/Alex/1_c66_project_df.rds"

# Save
saveRDS(c66_project_df, file_path)

# Load
my_data <- readRDS(file_path)

################################################ 
##Comparing excluded and included participants##
################################################
#dataset without filtering NA values
final_with_na_exp_out = c66_merged_outcome %>%
  full_join(c66_46_d7, by = "project_ID")%>%
  full_join(daily_temp, by = "project_ID") %>%   
  select(-date_daily_mean) 

###### Table1 ######
# 1. Add a new column 'included' based on the presence of missing values
df <- final_with_na_exp_out %>%
  mutate(
    included = if_else(
      !is.na(c66_46_c_date), 
      TRUE, 
      FALSE
    )
  )%>%
  ungroup() %>% 
  select(-project_ID, -c66_46_c_date)

# 2. Convert 'included' to a factor
df$included <- factor(
  df$included,
  levels = c(FALSE, TRUE),
  labels = c("Excluded", "Included")
)

# 3. Generate Table 1
t1 = table1(
  ~ . | included,
  data = df,
  render.continuous  = "Mean (SD)",
  render.categorical = "FREQ (PCTnoNA)",
  overall            = "Total",
  topclass           = "Rtable1-zebra"
)

# 4. Save the HTML table
html_table <- htmlTable(as.matrix(t1))
writeLines(html_table, "table1_output.html")

############ Tableone with p-values####
# 1. Identify all variables except 'included'
vars <- setdiff(names(df), "included")

# 2. Which of these variables are factors? (for tableone's factorVars argument)
factorVars <- vars[sapply(df[vars], is.factor)]

# 3. Create tableone object
tab1 <- CreateTableOne(
  data       = df,
  vars       = vars,
  strata     = "included",
  factorVars = factorVars,
  addOverall = TRUE,
  test       = TRUE
)

# 4. Capture the table content as a matrix (no console printing)
tab1_matrix <- print(
  tab1,
  showAllLevels = TRUE,
  missing       = TRUE,
  printToggle   = FALSE  # do not print in console, just return as a matrix
)

# 5. Pass that matrix to kable for HTML output
kable(tab1_matrix, format = "html", escape = FALSE) %>%
  kable_styling("striped") %>%
  save_kable("tableone_output.html")  # Save the table as an HTML file

########Excluded (N=2692) vs included (N=5751)
#Difference between people who came to the clinical examination and those who did not:
#1) More people in the excluded group who takes hyp.med. 243 (18.2%)	vs 804 (14.7%). Missing values 	1356 (50.4%) vs	295 (5.1%).
#2) More less educated people in the excluded group: low 161 (12.4%) vs 374 ( 7.1)%, high 272 (21.0%) vs 1276 (24.2) in the included group. Missing values 1397 (51.9%)	vs 469 (8.2%).
#3) More current smokers and less never smokers in the excluded group: current  426 (32.7)	vs 1027 (19.1), never  545 (41.9)	2901 (53.8). Missing values 	1390 (51.6%) vs	362 (6.3%).
#4) More heavy drinkers in the excluded group: heavy  231 (17.4) vs	669 (12.3), but there are more light drinkers in incl:  482 (36.2)	2311 (42.4). Missing values  1362 (50.6%)	304 (5.3%).
#5) More unemployed in the excluded group: unemployed  110 (9.0)	vs 292 ( 5.5), inactive/other  60 ( 4.9)	vs 108 ( 2.0). Missing values  1471 (54.6%)	423 (7.4%)
#6) More males in the excluded group:  1507 (56.0) vs	2538 (44.1). Missing values  3 (0.1%)	0 (0%).
#7) More people with hypercholesterolemia in the excluded group:  86 (73.5)	1856 (32.3). Missing values  2575 (95.7%)	vs 6 (0.1%).
#8) More people with hypertension in the included group:  363 (26.8) vs	2428 (42.2). Missing values  1339 (49.7%)	1 (0.0%).






