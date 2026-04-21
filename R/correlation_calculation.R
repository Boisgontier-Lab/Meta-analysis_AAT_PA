library(readxl)
library(dplyr)
library(writexl)
library(stringr)
library(tidyr)
library(here)
library(janitor)
library(lmtest)
library(readr)
library(lubridate)
library(psych)
library(haven)

options(scipen = 999)

setwd() #SET_YOUR_WORKING_DIRECTORY

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Cheval 2014 ####
# ─────────────────────────────────────────────────────────────────────────────

# Importation
df <- read_excel("2014_Cheval_25juin_EtudeCaracteristiquesComportement.xls")

# Create a dataframe containing only MVPA and IAPA
df_C2014 <- df %>%
  transmute(
    ID = Nombreparticipant,
    MVPA = APLoisirMod + APLoisirInten,
    IAPA = MedEvitAP - MedAppAP
  ) %>%
  drop_na(MVPA, IAPA)

# Initialization
df_C2014$Mean_MVPA    <- NA_real_
df_C2014$Mean_IAPA    <- NA_real_
df_C2014$Pearson_corr <- NA_real_
df_C2014$p_value      <- NA_real_

# Correlation MVPA <> IAPA
corr_result <- cor.test(
  df_C2014$MVPA,
  df_C2014$IAPA,
  method = "pearson",
  use = "complete.obs"
)

# Store the results in the first row
df_C2014$Mean_MVPA[1]    <- mean(df_C2014$MVPA, na.rm = TRUE)
df_C2014$Mean_IAPA[1]    <- mean(df_C2014$IAPA, na.rm = TRUE)
df_C2014$Pearson_corr[1] <- unname(corr_result$estimate)
df_C2014$p_value[1]      <- corr_result$p.value

# Exportation
output_dir <- #SET_YOUR_OUTPUT_DIRECTORY

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write_xlsx(df_C2014, file.path(output_dir, "2014_Cheval_correlation.xlsx"))

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Cheval 2016b, JPAH ####
# ─────────────────────────────────────────────────────────────────────────────

# Import data
df <- read_sav("JPAH_simplifie.sav")

# Create dataframe with MVPA and IAPA
df_JPAH <- df %>%
  transmute(
    ID = Nombreparticipant,
    MVPA = MVPA_Hab_Lois,
    IAPA = IAPA_T1,
    Male = Homme,
    Age = Age
  ) %>%
  drop_na(MVPA, IAPA)

# Initialize result columns
df_JPAH$Mean_MVPA     <- NA_real_
df_JPAH$Mean_IAPA     <- NA_real_
df_JPAH$Pearson_corr  <- NA_real_
df_JPAH$p_value       <- NA_real_
df_JPAH$Prop_Women    <- NA_real_
df_JPAH$Mean_Age      <- NA_real_

# Compute Pearson correlation between MVPA and IAPA
corr_result <- cor.test(
  df_JPAH$MVPA,
  df_JPAH$IAPA,
  method = "pearson",
  use = "complete.obs"
)

# Compute proportion of women (Male = 0 → women)
prop_women <- mean(df_JPAH$Male == 0, na.rm = TRUE)

# Compute mean age
mean_age <- mean(df_JPAH$Age, na.rm = TRUE)

# Store results in first row
df_JPAH$Mean_MVPA[1]     <- mean(df_JPAH$MVPA, na.rm = TRUE)
df_JPAH$Mean_IAPA[1]     <- mean(df_JPAH$IAPA, na.rm = TRUE)
df_JPAH$Pearson_corr[1]  <- unname(corr_result$estimate)
df_JPAH$p_value[1]       <- corr_result$p.value
df_JPAH$Prop_Women[1]    <- prop_women
df_JPAH$Mean_Age[1]      <- mean_age

# Export results
write_xlsx(df_JPAH, file.path(output_dir, "2016b_Cheval_correlation.xlsx"))

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Woodman, 2025 ####
# ─────────────────────────────────────────────────────────────────────────────

# Import data
df <- read_excel("2025_Woodman.xlsx")

# Create dataframe with MVPA and IAPA
df_Woodman <- df %>%
  transmute(
    ID   = participant,
    MVPA = vigortotal + modtotal,
    IAPA = ExerciseAvd - ExerciseApp,
    Sex = gender,
    Age  = age
  ) %>%
  drop_na(MVPA, IAPA)

# Initialize result columns
df_Woodman$Mean_MVPA    <- NA_real_
df_Woodman$Mean_IAPA    <- NA_real_
df_Woodman$Pearson_corr <- NA_real_
df_Woodman$p_value      <- NA_real_
df_Woodman$Prop_Women   <- NA_real_
df_Woodman$Mean_Age     <- NA_real_

# Compute Pearson correlation between MVPA and IAPA
corr_result <- cor.test(
  df_Woodman$MVPA,
  df_Woodman$IAPA,
  method = "pearson",
  use = "complete.obs"
)

# Compute proportion of women (gender = 2 -> women)
prop_women <- mean(df_Woodman$Sex == 2, na.rm = TRUE)

# Compute mean age
mean_age <- mean(df_Woodman$Age, na.rm = TRUE)

# Store results in first row
df_Woodman$Mean_MVPA[1]    <- mean(df_Woodman$MVPA, na.rm = TRUE)
df_Woodman$Mean_IAPA[1]    <- mean(df_Woodman$IAPA, na.rm = TRUE)
df_Woodman$Pearson_corr[1] <- unname(corr_result$estimate)
df_Woodman$p_value[1]      <- corr_result$p.value
df_Woodman$Prop_Women[1]   <- prop_women
df_Woodman$Mean_Age[1]     <- mean_age

# Export results
write_xlsx(df_Woodman, file.path(output_dir, "2025_Woodman_correlation.xlsx"))

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Daou 2017 ####
# ─────────────────────────────────────────────────────────────────────────────
# Importation
df<- read_excel("2017_Daou.xlsx")

# MVPA = minutes_Vigorous(days_vigorous × time_vigorous) + minutes_Moderate(days_moderate × time_moderate)
# Rename columns
df1 <- df %>%
  rename(
    vig_days  = `IPAQ: Vigorous Execise Days Per Week`,
    vig_min   = `IPAQ: Vigorous Execise Time Per Session`,
    mod_days  = `IPAQ: Moderate Execise Days Per Week`,
    mod_min   = `IPAQ: Moderate Execise Time Per Session`,
    Impulses = `Impulses Toward Physical Activity (Lower Scores = Higher Impulse)`
  )

#Remove rows with NA
df1 <- df1 %>%
  drop_na(vig_days, vig_min, mod_days, mod_min, Impulses)


# MVPA calculation (in min/week)
df1 <- df1 %>%
  mutate(
    MVPA = (vig_days * vig_min) + (mod_days * mod_min)
  )

# Initialize new columns with NA
df1$Pearson_corr <- NA_real_
df1$p_value <- NA_real_
df1$Mean_MVPA<- NA_real_
df1$Mean_Age   <- NA_real_
df1$Prop_women <- NA_real_

# Correlation MVPA <> Impulses toward physical activity
corr_result <- cor.test(
  df1$MVPA,
  df1$Impulses,
  method = "pearson",
  use = "complete.obs",
)

# Insert the value only in first row 
df1$Pearson_corr[1] <- corr_result$estimate
df1$p_value[1] <- corr_result$p.value
df1$Mean_Age[1] <- mean(df1$Age, na.rm = TRUE)
df1$Prop_women[1] <- mean(df1$`Sex (Male = 0, Female = 1)`, na.rm = TRUE)

# MVPA mean calculation and display in first row
df1$Mean_MVPA[1] <- mean(df1$MVPA, na.rm = TRUE)

# Exportation
write_xlsx(df1, file.path(output_dir, "2017_Daou_correlation.xlsx"))


# ─────────────────────────────────────────────────────────────────────────────
#                            #### Cheval 2018 ####
# ─────────────────────────────────────────────────────────────────────────────
# Importation
df_cheval<- read.csv("2018_Cheval.csv")

# Extract median times for each approach_AP and avoid_AP combination
df_summary <- df_cheval %>%
  filter(
    Action %in% c("Approach", "Avoid"),
    str_starts(images, "AP")     
  ) %>%
  group_by(subject, Action) %>%
  summarise(
    median_RT = median(RT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Action,
    values_from = median_RT,
    names_prefix = "RT_"
  ) %>%
  rename(
    RT_approach_AP = RT_Approach,
    RT_avoid_AP    = RT_Avoid
  ) %>%
  mutate(
    IAPA = RT_avoid_AP - RT_approach_AP
  )

# Join other subject's data (age, sex, etc.)
df_cheval2 <- df_summary %>%
  left_join(
    df_cheval %>% 
      select(subject, age, sex, height, weight, stage_PA, MVPA) %>% 
      distinct(),
    by = "subject"
  ) %>% 
  select(subject, age, sex, height, weight, stage_PA, MVPA,
         RT_approach_AP, RT_avoid_AP, IAPA)


# Initialize new columns with NA
df_cheval2$Pearson_corr <- NA_real_
df_cheval2$p_value <- NA_real_
df_cheval2$Mean_MVPA <- NA_real_

# Correlation MVPA <> Impulses toward physical activity
corr_result2 <- cor.test(
  df_cheval2$MVPA,
  df_cheval2$IAPA,
  method = "pearson",
  use = "complete.obs",
)

# Insert the value only in first row 
df_cheval2$Pearson_corr[1] <- corr_result2$estimate
df_cheval2$p_value[1] <- corr_result2$p.value

# MVPA mean calculation and display in first row
df_cheval2$Mean_MVPA[1] <- mean(df_cheval2$MVPA, na.rm = TRUE)

# Exportation
write_xlsx(df_cheval2, file.path(output_dir, "2018_Cheval_correlation.xlsx"))

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Cheval 2018 ####
#                   calculation with neutral RT deducted from RT
# ─────────────────────────────────────────────────────────────────────────────
# Importation
df_cheval_neutral<- read.csv("2018_Cheval.csv")

df_summary <- df_cheval_neutral %>%
  mutate(
    RT_rel_neutral = parse_double(RT_rel_neutral, locale = locale(decimal_mark = ","))
  ) %>%
  filter(
    Action %in% c("Approach", "Avoid"),
    str_starts(images, "AP")
  ) %>%
  group_by(subject, Action) %>%
  summarise(
    median_RT = median(RT_rel_neutral, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Action,
    values_from = median_RT,
    names_prefix = "RT_"
  ) %>%
  rename(
    RT_approach_AP = RT_Approach,
    RT_avoid_AP    = RT_Avoid
  ) %>%
  mutate(
    IAPA = RT_avoid_AP - RT_approach_AP
  )

# Join other subject's data (age, sex, etc.)
df_cheval_neutral <- df_summary %>%
  left_join(
    df_cheval_neutral %>% 
      select(subject, age, sex, height, weight, stage_PA, MVPA) %>% 
      distinct(),
    by = "subject"
  ) %>% 
  select(subject, age, sex, height, weight, stage_PA, MVPA,
         RT_approach_AP, RT_avoid_AP, IAPA)

# Remove rows with NA in columns necessary for correlation and summaries
df_cheval_neutral <- df_cheval_neutral %>%
  drop_na(MVPA, IAPA, age, sex)

# Initialize new columns with NA
df_cheval_neutral$Pearson_corr <- NA_real_
df_cheval_neutral$p_value <- NA_real_
df_cheval_neutral$Mean_MVPA <- NA_real_
df_cheval_neutral$Mean_Age <- NA_real_
df_cheval_neutral$Prop_women <- NA_real_

# Correlation MVPA <> Impulses toward physical activity
corr_result2 <- cor.test(
  df_cheval_neutral$MVPA,
  df_cheval_neutral$IAPA,
  method = "pearson",
  use = "complete.obs",
)

# Insert the value only in first row 
df_cheval_neutral$Pearson_corr[1] <- corr_result2$estimate
df_cheval_neutral$p_value[1] <- corr_result2$p.value
df_cheval_neutral$Mean_Age[1] <- mean(df_cheval_neutral$age, na.rm = TRUE)
df_cheval_neutral$Prop_women[1] <- mean(df_cheval_neutral$sex == "F", na.rm = TRUE)

# MVPA mean calculation and display in first row
df_cheval_neutral$Mean_MVPA[1] <- mean(df_cheval_neutral$MVPA, na.rm = TRUE)

# Exportation
write_xlsx(df_cheval_neutral, file.path(output_dir, "2018_Cheval_correlation_neutral.xlsx"))


# ─────────────────────────────────────────────────────────────────────────────
#                            #### Locke 2021 ####
# ─────────────────────────────────────────────────────────────────────────────
# Importation
df_locke <- read_excel("2021_Locke.xlsx")

# Remove lines with NA values
df_locke <- df_locke[!is.na(df_locke$MVPA), ]

# Initialize columns with NA
df_locke$Pearson_corr <- NA_real_
df_locke$p_value <- NA_real_
df_locke$Mean_MVPA <- NA_real_

# MVPA mean calculation and display in first row
df_locke$Mean_MVPA[1] <- mean(df_locke$MVPA, na.rm = TRUE)

# Correlation MVPA <> Approach tendencie
corr_mvpa_approach <- cor.test(df_locke$MVPA, df_locke$Approach, 
                               method = "pearson", use = "complete.obs")

# Insert the value only in first row 
df_locke$Pearson_corr[1] <- corr_mvpa_approach$estimate
df_locke$p_value[1] <- corr_mvpa_approach$p.value

# Exportation
write_xlsx(df_locke, file.path(output_dir, "2021_Locke_correlation.xlsx"))

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Wang 2023 ####
# ─────────────────────────────────────────────────────────────────────────────
# Importation
df_wang<- read_excel("2023_Wang.xlsx")

# Initialize columns with NA
df_wang$Pearson_corr <- NA_real_
df_wang$p_value <- NA_real_
df_wang$Mean_Exe.Volume <- NA_real_

# MVPA (Exe.volume) mean calculation and display in first row
df_wang$Mean_Exe.Volume[1] <- mean(df_wang$Exe.Volume, na.rm = TRUE)

# Correlation MVPA(Exe.volume) <> Impulses toward physical activity
corr_exe_iapa <- cor.test(df_wang$`Exe.Volume`, df_wang$IAPA, method = "pearson", use = "complete.obs")

# Insert the value only in first row 
df_wang$Pearson_corr[1] <- corr_exe_iapa$estimate
df_wang$p_value[1] <- corr_exe_iapa$p.value

# Exportation
write_xlsx(df_wang, file.path(output_dir, "2023_Wang_correlation.xlsx"))


# ─────────────────────────────────────────────────────────────────────────────
#                            #### Quossi 2025 ####
# ─────────────────────────────────────────────────────────────────────────────
# Import data
df_quossi <- read.csv("2025_Quossi.csv")

# Remove extreme RT values (<150 ms or >3000 ms)
df_quossi <- df_quossi %>%
  filter(RT >= 150, RT <= 3000)

# Extract median RT for each Approach/Avoid combination and compute IAPA
df_quossi2 <- df_quossi %>%
  filter(
    Action %in% c("Approach", "Avoid"),
    str_starts(condition, "PA")
  ) %>%
  group_by(subject, Action) %>%
  summarise(median_RT = median(RT, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Action,
    values_from = median_RT,
    names_prefix = "RT_"
  ) %>%
  rename(
    RT_approach_AP = RT_Approach,
    RT_avoid_AP    = RT_Avoid
  ) %>%
  mutate(IAPA = RT_avoid_AP - RT_approach_AP)

# Compute mean MVPA per subject
df_mvpa_by_subject <- df_quossi %>%
  group_by(subject) %>%
  summarise(MVPA = mean(total_time_MVPA_clean, na.rm = TRUE), .groups = "drop")

# Extract demographics (age + gender)
df_demo <- df_quossi %>%
  group_by(subject) %>%
  summarise(
    age_clean    = dplyr::first(na.omit(age_clean)),
    gender_clean = dplyr::first(na.omit(gender_clean)),
    .groups = "drop"
  )

# Merge demographic and MVPA data into the main dataset
df_quossi2 <- df_quossi2 %>%
  left_join(df_mvpa_by_subject, by = "subject") %>%
  left_join(df_demo,            by = "subject")

# Remove rows with missing data in required columns
df_quossi2 <- df_quossi2 %>%
  drop_na(MVPA, IAPA, age_clean, gender_clean)

# Initialize result columns
df_quossi2$Pearson_corr <- NA_real_
df_quossi2$p_value      <- NA_real_
df_quossi2$Mean_MVPA    <- NA_real_
df_quossi2$Mean_Age     <- NA_real_
df_quossi2$Prop_women   <- NA_real_

# Compute Pearson correlation between MVPA and IAPA
corr_result2 <- cor.test(
  df_quossi2$MVPA,
  df_quossi2$IAPA,
  method = "pearson",
  use = "complete.obs"
)

# Store summary results in the first row
df_quossi2$Pearson_corr[1] <- corr_result2$estimate
df_quossi2$p_value[1]      <- corr_result2$p.value
df_quossi2$Mean_MVPA[1]    <- mean(df_quossi2$MVPA, na.rm = TRUE)
df_quossi2$Mean_Age[1]     <- mean(df_quossi2$age_clean, na.rm = TRUE)
df_quossi2$Prop_women[1]   <- mean(df_quossi2$gender_clean == "Women", na.rm = TRUE)

# Exportation
write_xlsx(df_quossi2, file.path(output_dir, "2025_Quossi_correlation.xlsx"))

# ─────────────────────────────────────────────────────────────────────────────
#                 #### Farajzadeh 2023; 2024; Goubran 2025 ####
#             Controlling for the bias to approach neutral images
# ─────────────────────────────────────────────────────────────────────────────

# Read in data
data1 <- read.csv("2024_Farajzadeh.csv")

# tidying data - Renaming factors  
cleandata1 <- data1 %>%
  clean_names() %>%
  rename(exptime = total_elapsed_time) %>%
  rename(down = responsekey_down) %>%
  rename(language = subj) %>%
  rename(up = responsekey_up) %>%
  rename(daytime = time)

# adding column with trial number of the whole study
cleandata1 <- cleandata1 %>%
  group_by(id) %>%
  mutate(trial_number_study = 1:n()) %>%
  ungroup()

# removing useless conditions from the "trialcode" column
cleanlightdata1 <- cleandata1[cleandata1$trialcode != "fixation" & 
                                cleandata1$trialcode != "reminder" & 
                                cleandata1$trialcode != "too_slow" &
                                cleandata1$trialcode != "instructionimages"&
                                cleandata1$trialcode != "error" & # screen information participants they made an error
                                cleandata1$lars11 == "5" & # attention check question
                                cleandata1$latency < 3000 &
                                cleandata1$latency > 150 &
                                cleandata1$height < 250 &
                                cleandata1$height > 50 &
                                cleandata1$weight < 250 &
                                cleandata1$weight > 30 , ]

# remove 3 first trial of each condition & 15 first trials of the study
cleanlightdata1 <- cleanlightdata1 %>%
  filter(!trial_num      %in% 1:6,
         !trial_number_study %in% 7:30)



# Selection of useful trials + labeling of the 4 boxes
data3 <- cleanlightdata1 %>%
  mutate(
    trialcode = as.character(trialcode),
    latency   = as.numeric(latency)
  ) %>%
  filter(
    correct == 1,
    (block_code == "avoid_ap"        & str_starts(trialcode, "ApAvoid"))         |
      (block_code == "approach_ap"     & str_starts(trialcode, "ApApproach"))      |
      (block_code == "avoid_circle"    & str_starts(trialcode, "circleAvoid"))     |
      (block_code == "approach_circle" & str_starts(trialcode, "circleApproach"))  |
      (block_code == "approach_circle" & str_starts(trialcode, "squareAvoid"))     |
      (block_code == "avoid_circle"    & str_starts(trialcode, "squareApproach"))
  ) %>%
  mutate(
    measure = case_when(
      block_code == "avoid_ap"        & str_starts(trialcode, "ApAvoid")          ~ "avoid_AP",
      block_code == "approach_ap"     & str_starts(trialcode, "ApApproach")       ~ "approach_AP",
      (block_code == "avoid_circle"   & str_starts(trialcode, "circleAvoid"))     |
        (block_code == "approach_circle"& str_starts(trialcode, "squareAvoid"))     ~ "avoid_neutral",
      (block_code == "approach_circle"& str_starts(trialcode, "circleApproach"))  |
        (block_code == "avoid_circle"   & str_starts(trialcode, "squareApproach"))  ~ "approach_neutral",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(measure))

# Separate aggregations (all in MEDIANs)
ap_summary <- data3 %>%
  filter(measure %in% c("avoid_AP","approach_AP")) %>%
  group_by(id, measure) %>%
  summarise(RT = median(latency, na.rm = TRUE), .groups = "drop")

neutral_summary <- data3 %>%
  filter(measure %in% c("avoid_neutral","approach_neutral")) %>%
  group_by(id, measure) %>%
  summarise(RT = median(latency, na.rm = TRUE), .groups = "drop")

# Recombine, pivot, compute IAPA
df_Ata2024_2 <- bind_rows(ap_summary, neutral_summary) %>%
  pivot_wider(
    names_from  = measure,
    values_from = RT
  ) %>%
  mutate(
    across(c(avoid_AP, approach_AP, avoid_neutral, approach_neutral), as.numeric),
    RT_avoid_AP         = avoid_AP,
    RT_approach_AP      = approach_AP,
    RT_avoid_neutral    = avoid_neutral,
    RT_approach_neutral = approach_neutral,
    IAPA = (RT_avoid_AP - RT_approach_AP) - (RT_avoid_neutral - RT_approach_neutral)
  ) %>%
  select(id, RT_approach_AP, RT_avoid_AP, RT_approach_neutral, RT_avoid_neutral, IAPA)

# MVPA calculation (in min/week)
mvpa_by_subject <- cleanlightdata1 %>%
  reframe(
    MVPA = (vigorous_d * vigorous_m) + (moderate_d * moderate_m),
    .by = id
  )
df_Ata2024_2 <- df_Ata2024_2 %>%
  mutate(
    MVPA = mvpa_by_subject$MVPA[ match(id, mvpa_by_subject$id) ]
  )

# Initialize columns with NA
df_Ata2024_2$Pearson_corr <- NA_real_
df_Ata2024_2$p_value <- NA_real_
df_Ata2024_2$Mean_MVPA <- NA_real_
df_Ata2024_2$Mean_Age <- NA_real_
df_Ata2024_2$Prop_women <- NA_real_

# Correlation MVPA <> Impulses toward physical activity
corr_result3 <- cor.test(
  df_Ata2024_2$MVPA,
  df_Ata2024_2$IAPA,
  method = "pearson",
  use = "complete.obs",
)

# Insert the value only in first row 
df_Ata2024_2$Pearson_corr[1] <- corr_result3$estimate
df_Ata2024_2$p_value[1] <- corr_result3$p.value

# MVPA mean calculation and display in first row
df_Ata2024_2$Mean_MVPA[1] <- mean(df_Ata2024_2$MVPA, na.rm = TRUE)

# Adding mean age & sex of the subjects
demog_by_subject <- cleanlightdata1 %>%
  select(id, age, sex) %>%
  distinct()

df_Ata2024_2 <- df_Ata2024_2 %>%
  left_join(demog_by_subject, by = "id") %>%
  relocate(age, sex, .after = id)

# Mean age calculation
df_Ata2024_2$Mean_Age[1] <- mean(df_Ata2024_2$age, na.rm = TRUE)

# Female proportion calculation
female_variants <- c("Femelle", "Female")

df_Ata2024_2$Prop_women[1] <- df_Ata2024_2 %>%
  summarise(prop = mean(sex %in% female_variants, na.rm = TRUE)) %>%
  pull(prop)

# Exportation
write_xlsx(df_Ata2024_2, file.path(output_dir, "2023-2025_Ata_Goubran_correlation_neutral.xlsx"))

# ─────────────────────────────────────────────────────────────────────────────
#                            #### Fessler 2024 ####
# ─────────────────────────────────────────────────────────────────────────────

## ===============================
## 1) Demographic data
## ===============================
df_demog_Fessler <- read.csv("Data_questionnaire_IMPACT_all_participants.csv")

parse_date_safe <- function(x) {
  parse_date_time(x, orders = c("ymd", "dmy", "mdy"), tz = "UTC")
}

df_demog_Fessler <- df_demog_Fessler %>%
  mutate(
    dob           = parse_date_safe(dob),
    date_sign_cons= parse_date_safe(date_sign_cons),
    Age_in_year   = as.numeric(difftime(date_sign_cons, dob, units = "days")) / 365.25,
    gender        = ifelse(gender == 0, "M", ifelse(gender == 1, "F", NA))
  ) %>%
  select(record_id, Age_in_year, gender, height, weight) %>%
  na.omit() %>%
  filter(Age_in_year >= 39, Age_in_year <= 110) %>%
  mutate(record_id = as.character(record_id),
         record_id = str_trim(record_id))

## ===============================
## 2) IPAQ data computation
## ===============================

## --- CONTROL ---
df_SRPA_control <- read_excel("Data_SRPA_CONTROL_IMPACT.xlsx", sheet = 1) %>%
  mutate(ID = as.character(ID), ID = str_trim(ID))

vig_cols_ctl <- paste0("PA_Log_J", 1:7, "_VIGOUROUS_TIME")
mod_cols_ctl <- paste0("PA_Log_J", 1:7, "_MODERATE_TIME")

df_SRPA_control <- df_SRPA_control %>%
  mutate(
    n_days_vig = rowSums(!is.na(as.data.frame(select(., all_of(vig_cols_ctl))))),
    n_days_mod = rowSums(!is.na(as.data.frame(select(., all_of(mod_cols_ctl))))),
    n_days_any = pmax(n_days_vig, n_days_mod, na.rm = TRUE),
    # sum of VIG + MOD in min
    MVPA_IPAQ  = rowSums(as.data.frame(select(., all_of(vig_cols_ctl))), na.rm = TRUE) +
      rowSums(as.data.frame(select(., all_of(mod_cols_ctl))), na.rm = TRUE)
  ) %>%
  filter(n_days_any >= 4) %>%  # criterion > 4 days
  transmute(ID, MVPA_CONTROL = MVPA_IPAQ)

## --- Cardiovascular Rehabilitation (CR) ---
df_SRPA_CR <- read_excel("Data_SRPA_IMPACT.xlsx", sheet = 1) %>%
  mutate(ID = as.character(ID), ID = str_trim(ID))

vig_cols <- paste0("PA_Log_J", 1:7, "_VIGOUROUS_TIME_1W")
mod_cols <- paste0("PA_Log_J", 1:7, "_MODERATE_TIME_1W")

df_SRPA_CR <- df_SRPA_CR %>%
  mutate(
    J1_DATE_1W_chr = as.character(J1_DATE_1W),
    n_days_vig = rowSums(!is.na(as.data.frame(select(., all_of(vig_cols))))),
    n_days_mod = rowSums(!is.na(as.data.frame(select(., all_of(mod_cols))))),
    n_days_any = pmax(n_days_vig, n_days_mod, na.rm = TRUE),
    MVPA_IPAQ  = rowSums(as.data.frame(select(., all_of(vig_cols))), na.rm = TRUE) +
      rowSums(as.data.frame(select(., all_of(mod_cols))), na.rm = TRUE)
  ) %>%
  filter(!is.na(J1_DATE_1W_chr) & J1_DATE_1W_chr != "") %>%
  filter(n_days_any >= 4) %>%  # criterion > 4 days
  transmute(ID, MVPA_CR = MVPA_IPAQ)


## --- Fusion IPAQ ---
df_mvpa <- full_join(df_SRPA_control, df_SRPA_CR, by = "ID") %>%
  mutate(MVPA_IPAQ = coalesce(MVPA_CR, MVPA_CONTROL)) %>%
  transmute(ID, MVPA_IPAQ) %>%
  mutate(ID = as.character(ID), ID = str_trim(ID))

## ===============================
## 3) Accelerometer data computation
## ===============================

# Function to compute data for 1w
compute_accelero_1w <- function(path) {
  readxl::read_excel(path, sheet = 1) %>%
    dplyr::mutate(
      PARTICIPANT = as.character(.data$PARTICIPANT),
      `Total MVPA` = as.numeric(gsub(",", ".", gsub("\\s+", "", as.character(`Total MVPA`))))
    ) %>%
    dplyr::filter(.data$CONDITION == "1w") %>%
    dplyr::filter(
      dplyr::if_all(
        dplyr::any_of(c("VALID_YES_NO_WEEK", "VALID_YES_NO_DAYS")),
        ~ toupper(as.character(.x)) == "YES"
      )
    ) %>%
    dplyr::group_by(.data$PARTICIPANT) %>%
    dplyr::summarise(MVPA_accelero = sum(`Total MVPA`, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(PARTICIPANT, MVPA_accelero)
}

# Files
path_control <- "Data_accelero_CONTROL_IMPACT.xlsx"
path_cr      <- "Data_accelero_IMPACT.xlsx"

# Calculation per file
acc_control_sum <- compute_accelero_1w(path_control)
acc_cr_sum      <- compute_accelero_1w(path_cr)

# Fusion 
acc_all <- dplyr::bind_rows(acc_control_sum, acc_cr_sum) %>%
  dplyr::mutate(
    PARTICIPANT = stringr::str_trim(as.character(.data$PARTICIPANT))
  ) %>%
  dplyr::group_by(.data$PARTICIPANT) %>%
  dplyr::summarise(MVPA_accelero = sum(.data$MVPA_accelero, na.rm = TRUE)) %>%
  dplyr::ungroup()

# Adding to demographic data
df_accelero_Fessler <- df_demog_Fessler %>%
  dplyr::mutate(record_id = as.character(.data$record_id)) %>%
  dplyr::left_join(acc_all, by = c("record_id" = "PARTICIPANT")) %>%
  dplyr::filter(!is.na(.data$MVPA_accelero))

## ===============================
## 4) VAAST data computation
## ===============================

library(plyr)       # for revalue()
library(reshape2)   # for melt/dcast

Impact_VAAST_data <- read_excel("Data_vaast_IMPACT_all_participants.xlsx")

Impact_VAAST_data_main <- Impact_VAAST_data %>%
  transmute(
    subject = as.character(subject),
    blockcode,
    session,
    blocknum,
    trialcode,
    trialnum,
    stimulinum = stimulusnumber1,
    stimuliname = stimulusitem2,
    accuracy = correct,
    RT = latency
  ) %>%
  filter(subject != "0", subject != "1111", subject != "2201") %>%
  mutate(subject = str_trim(subject))

## Remove useless rows
rm_codes <- c(
  "Consignes_training_V1_2","Consignes_training_V2",
  "correctfb_Approach_PA_training_V1_A","correctfb_Approach_PA_training_V1_B",
  "correctfb_Approach_PA_training_V2_A","correctfb_Approach_PA_training_V2_B",
  "correctfb_Approach_PA_trials_V1","correctfb_Approach_PA_trials_V2",
  "correctfb_Approach_SED_training_V1_A","correctfb_Approach_SED_training_V1_B",
  "correctfb_Approach_SED_training_V2_A","correctfb_Approach_SED_training_V2_B",
  "correctfb_Approach_SED_trials_V1","correctfb_Approach_SED_trials_V2",
  "correctfb_Avoid_PA_training_V1_A","correctfb_Avoid_PA_training_V1_B",
  "correctfb_Avoid_PA_training_V2_A","correctfb_Avoid_PA_training_V2_B",
  "correctfb_Avoid_PA_trials_V1","correctfb_Avoid_PA_trials_V2",
  "correctfb_Avoid_SED_training_V1_A","correctfb_Avoid_SED_training_V1_B",
  "correctfb_Avoid_SED_training_V2_A","correctfb_Avoid_SED_training_V2_B",
  "correctfb_Avoid_SED_trials_V1","correctfb_Avoid_SED_trials_V2"
)

Impact_VAAST_data_main_sub <- Impact_VAAST_data_main %>%
  filter(!(trialcode %in% rm_codes))

## Define Assessment session for exclusion by accuracy
assessment_map <- c(
  "Approach_PA_trials_V1"="assessment","Approach_PA_trials_V2"="assessment",
  "Avoid_PA_trials_V2"="assessment","Avoid_PA_trials_V1"="assessment",
  "Approach_SED_trials_V1"="assessment","Approach_SED_trials_V2"="assessment",
  "Avoid_SED_trials_V2"="assessment","Avoid_SED_trials_V1"="assessment"
)
Impact_VAAST_data_for_acc <- Impact_VAAST_data_main_sub %>%
  mutate(Assessment = plyr::revalue(trialcode, assessment_map, warn_missing = FALSE)) %>%
  filter(Assessment == "assessment")

## Exclusion of subjects with low-accuracy (p.ex. < 0.60) 
acc_by_subj <- Impact_VAAST_data_for_acc %>%
  dplyr::group_by(subject) %>%
  dplyr::summarise(acc_mean = mean(accuracy, na.rm = TRUE), .groups = "drop")
low_acc <- acc_by_subj %>% filter(acc_mean < 0.60) %>% pull(subject)

## Filter errors + RT + low-acc
Impact_VAAST_data_main_sub <- Impact_VAAST_data_main_sub %>%
  filter(!(subject %in% low_acc)) %>%
  filter(accuracy == 1, RT > 200, RT < 3000) %>%
  mutate(subject = str_trim(subject))

## Build Action/Condition/Assessment
Impact_VAAST_data_main_sub$type_image <- ifelse(Impact_VAAST_data_main_sub$trialcode %in% c(
  "Approach_PA_training_V1_A","Approach_PA_training_V1_B","Approach_PA_training_V2_A","Approach_PA_training_V2_B",
  "Approach_PA_trials_V1","Approach_PA_trials_V2","Avoid_PA_training_V1_A","Avoid_PA_training_V1_B",
  "Avoid_PA_training_V2_A","Avoid_PA_training_V2_B","Avoid_PA_trials_V2","Avoid_PA_trials_V1"
), "PA", "SED")

Impact_VAAST_data_main_sub$Action <- revalue(Impact_VAAST_data_main_sub$trialcode, c(
  "Approach_PA_training_V1_A"="Approach","Approach_PA_training_V1_B"="Approach",
  "Approach_PA_training_V2_A"="Approach","Approach_PA_training_V2_B"="Approach",
  "Approach_PA_trials_V1"="Approach","Approach_PA_trials_V2"="Approach",
  "Avoid_PA_training_V1_A"="Avoid","Avoid_PA_training_V1_B"="Avoid",
  "Avoid_PA_training_V2_A"="Avoid","Avoid_PA_training_V2_B"="Avoid",
  "Avoid_PA_trials_V2"="Avoid","Avoid_PA_trials_V1"="Avoid",
  "Approach_SED_training_V1_A"="Approach","Approach_SED_training_V1_B"="Approach",
  "Approach_SED_training_V2_A"="Approach","Approach_SED_training_V2_B"="Approach",
  "Approach_SED_trials_V1"="Approach","Approach_SED_trials_V2"="Approach",
  "Avoid_SED_training_V1_A"="Avoid","Avoid_SED_training_V1_B"="Avoid",
  "Avoid_SED_training_V2_A"="Avoid","Avoid_SED_training_V2_B"="Avoid",
  "Avoid_SED_trials_V2"="Avoid","Avoid_SED_trials_V1"="Avoid"
))

Impact_VAAST_data_main_sub$Condition <- revalue(Impact_VAAST_data_main_sub$trialcode, c(
  "Approach_PA_training_V1_A"="Approach_PA","Approach_PA_training_V1_B"="Approach_PA",
  "Approach_PA_training_V2_A"="Approach_PA","Approach_PA_training_V2_B"="Approach_PA",
  "Approach_PA_trials_V1"="Approach_PA","Approach_PA_trials_V2"="Approach_PA",
  "Avoid_PA_training_V1_A"="Avoid_PA","Avoid_PA_training_V1_B"="Avoid_PA",
  "Avoid_PA_training_V2_A"="Avoid_PA","Avoid_PA_training_V2_B"="Avoid_PA",
  "Avoid_PA_trials_V2"="Avoid_PA","Avoid_PA_trials_V1"="Avoid_PA",
  "Approach_SED_training_V1_A"="Approach_SED","Approach_SED_training_V1_B"="Approach_SED",
  "Approach_SED_training_V2_A"="Approach_SED","Approach_SED_training_V2_B"="Approach_SED",
  "Approach_SED_trials_V1"="Approach_SED","Approach_SED_trials_V2"="Approach_SED",
  "Avoid_SED_training_V1_A"="Avoid_SED","Avoid_SED_training_V1_B"="Avoid_SED",
  "Avoid_SED_training_V2_A"="Avoid_SED","Avoid_SED_training_V2_B"="Avoid_SED",
  "Avoid_SED_trials_V2"="Avoid_SED","Avoid_SED_trials_V1"="Avoid_SED"
))

Impact_VAAST_data_main_sub$Assessment <- revalue(Impact_VAAST_data_main_sub$trialcode, c(
  "Approach_PA_training_V1_A"="training","Approach_PA_training_V1_B"="training",
  "Approach_PA_training_V2_A"="training","Approach_PA_training_V2_B"="training",
  "Approach_PA_trials_V1"="assessment","Approach_PA_trials_V2"="assessment",
  "Avoid_PA_training_V1_A"="training","Avoid_PA_training_V1_B"="training",
  "Avoid_PA_training_V2_A"="training","Avoid_PA_training_V2_B"="training",
  "Avoid_PA_trials_V2"="assessment","Avoid_PA_trials_V1"="assessment",
  "Approach_SED_training_V1_A"="training","Approach_SED_training_V1_B"="training",
  "Approach_SED_training_V2_A"="training","Approach_SED_training_V2_B"="training",
  "Approach_SED_trials_V1"="assessment","Approach_SED_trials_V2"="assessment",
  "Avoid_SED_training_V1_A"="training","Avoid_SED_training_V1_B"="training",
  "Avoid_SED_training_V2_A"="training","Avoid_SED_training_V2_B"="training",
  "Avoid_SED_trials_V2"="assessment","Avoid_SED_trials_V1"="assessment"
))

## Only keep assessment
Impact_VAAST_Asses <- subset(Impact_VAAST_data_main_sub, Assessment == "assessment")

## Median RT per subject/condition/session
data_VAAST_averaged_long <- aggregate(RT ~ subject + Condition + session, data = Impact_VAAST_Asses, FUN = median)

## Correspondence sessions
Impact_VAAST_corresp_ass <- read_excel("20230418_Correspondance_assessment.xlsx")

data_VAAST_averaged_long_clean <- merge(
  data_VAAST_averaged_long,
  Impact_VAAST_corresp_ass,
  by = c("subject","session"),
  all.x = TRUE
) %>% na.omit()

## Long -> wide
keepVars <- c("subject","recode_session","Condition","RT")
data_VAAST_averaged_long_clean_short <- data_VAAST_averaged_long_clean[, keepVars]

data_VAAST_clean_session <- dcast(
  melt(data_VAAST_averaged_long_clean_short, id.vars = c("subject","recode_session","Condition")),
  subject ~ Condition + recode_session
)

## Columns order
keepVars <- c("subject","Approach_PA_baseline","Avoid_PA_baseline","Approach_SED_baseline", "Avoid_SED_baseline",
              "Approach_PA_middle","Avoid_PA_middle","Approach_SED_middle", "Avoid_SED_middle",
              "Approach_PA_end","Avoid_PA_end","Approach_SED_end", "Avoid_SED_end")
data_VAAST_clean_session <- data_VAAST_clean_session[, keepVars]

## Bias calculation (Avoid - Approach)
data_VAAST_clean_session$app_bias_PA_baseline  <- data_VAAST_clean_session$Avoid_PA_baseline  - data_VAAST_clean_session$Approach_PA_baseline
data_VAAST_clean_session$app_bias_SED_baseline <- data_VAAST_clean_session$Avoid_SED_baseline - data_VAAST_clean_session$Approach_SED_baseline
data_VAAST_clean_session$app_bias_PA_middle    <- data_VAAST_clean_session$Avoid_PA_middle    - data_VAAST_clean_session$Approach_PA_middle
data_VAAST_clean_session$app_bias_SED_middle   <- data_VAAST_clean_session$Avoid_SED_middle   - data_VAAST_clean_session$Approach_SED_middle
data_VAAST_clean_session$app_bias_PA_end       <- data_VAAST_clean_session$Avoid_PA_end       - data_VAAST_clean_session$Approach_PA_end
data_VAAST_clean_session$app_bias_SED_end      <- data_VAAST_clean_session$Avoid_SED_end      - data_VAAST_clean_session$Approach_SED_end

Impact_Inq_VAAST <- data_VAAST_clean_session

detach("package:plyr", unload = TRUE)
detach("package:reshape2", unload = TRUE)

## Bias choice (IMPACT => end ; CONTROL => baseline)
Impact_Inq_VAAST <- Impact_Inq_VAAST %>%
  mutate(
    subject = as.character(subject),
    subject = str_trim(subject),
    group = case_when(
      str_detect(subject, "^22") ~ "IMPACT",
      str_detect(subject, "^33") ~ "CONTROL",
      TRUE ~ NA_character_
    ),
    app_bias_PA_pred = case_when(
      group == "IMPACT"  ~ app_bias_PA_end,
      group == "CONTROL" ~ app_bias_PA_baseline,
      TRUE ~ NA_real_
    )
  )

vaast_bias <- Impact_Inq_VAAST %>%
  select(subject, app_bias_PA_pred)

## ===============================
## 5) Adding demographic data
## ===============================

## IPAQ
df_IPAQ_Fessler <- df_demog_Fessler %>%
  inner_join(df_mvpa %>% mutate(ID = as.character(ID), ID = str_trim(ID)),
             by = c("record_id" = "ID")) %>%
  inner_join(vaast_bias, by = c("record_id" = "subject")) %>%
  filter(!is.na(MVPA_IPAQ), !is.na(app_bias_PA_pred)) %>%
  distinct(record_id, .keep_all = TRUE)

## Accelerometer
df_accelero_Fessler <- df_demog_Fessler %>%
  inner_join(acc_all %>% dplyr::rename(record_id = PARTICIPANT), by = "record_id") %>%
  inner_join(vaast_bias, by = c("record_id" = "subject")) %>%
  filter(!is.na(MVPA_accelero), !is.na(app_bias_PA_pred)) %>%
  distinct(record_id, .keep_all = TRUE)

## ===============================
## 6) Split CR vs control
## ===============================

add_summary_cols <- function(df, mvpa_col) {
  if (!mvpa_col %in% names(df)) stop(paste("Column", mvpa_col, "not found"))
  
  dat_cor <- df %>% filter(!is.na(.data[[mvpa_col]]), !is.na(app_bias_PA_pred))
  cor_res <- if (nrow(dat_cor) >= 3) cor.test(dat_cor[[mvpa_col]], dat_cor$app_bias_PA_pred) else NULL
  
  mean_age   <- mean(df$Age_in_year, na.rm = TRUE)
  prop_women <- mean(df$gender == "F", na.rm = TRUE)
  cor_val    <- if (!is.null(cor_res)) unname(cor_res$estimate) else NA_real_
  p_val      <- if (!is.null(cor_res)) cor_res$p.value else NA_real_
  
  df <- df %>%
    mutate(
      Mean_age = NA_real_,
      Prop_women = NA_real_,
      Cor = NA_real_,
      p_value = NA_real_
    )
  df[1, c("Mean_age","Prop_women","Cor","p_value")] <- c(mean_age, prop_women, cor_val, p_val)
  df
}

## Tag group
tag_group <- function(df) {
  df %>%
    mutate(
      record_id = as.character(record_id),
      group = case_when(
        str_starts(record_id, "22") ~ "CR",
        str_starts(record_id, "33") ~ "control",
        TRUE ~ NA_character_
      )
    )
}

df_accelero_Fessler <- tag_group(df_accelero_Fessler)
df_IPAQ_Fessler     <- tag_group(df_IPAQ_Fessler)

df_accelero_Fessler_CR      <- df_accelero_Fessler %>% filter(group == "CR")      %>% add_summary_cols("MVPA_accelero")
df_accelero_Fessler_control <- df_accelero_Fessler %>% filter(group == "control") %>% add_summary_cols("MVPA_accelero")

df_IPAQ_Fessler_CR      <- df_IPAQ_Fessler %>% filter(group == "CR")      %>% add_summary_cols("MVPA_IPAQ")
df_IPAQ_Fessler_control <- df_IPAQ_Fessler %>% filter(group == "control") %>% add_summary_cols("MVPA_IPAQ")

## ===============================
## 7) Computation of mean MVPA for each df
## ===============================
add_mvpa_mean_col <- function(df, mvpa_col) {
  if (!mvpa_col %in% names(df)) {
    stop(paste("Column", mvpa_col, "not found in dataframe"))
  }
  
  mean_mvpa <- mean(df[[mvpa_col]], na.rm = TRUE)
  
  df <- df %>%
    mutate(Mean_MVPA = NA_real_)   
  
  df[1, "Mean_MVPA"] <- mean_mvpa  
  
  return(df)
}

# Exportation
write_xlsx(df_accelero_Fessler_control, file.path(output_dir, "2024_Fessler_accelero_control_correlation.xlsx"))
write_xlsx(df_accelero_Fessler_CR, file.path(output_dir, "2024_Fessler_accelero_CR_correlation.xlsx"))
write_xlsx(df_IPAQ_Fessler_control, file.path(output_dir, "2024_Fessler_IPAQ_control_correlation.xlsx"))
write_xlsx(df_IPAQ_Fessler_CR, file.path(output_dir, "2024_Fessler_IPAQ_CR_correlation.xlsx"))
