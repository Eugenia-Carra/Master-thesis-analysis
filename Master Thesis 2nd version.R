# Student: Eugenia Carra
# Student Number: 2742176
# Date: '25.06.2025'
# Master's thesis
# Supervisor: K. Weißmüller 

# ------------- R Script for Empirical Analysis: Thesis -----------------


# This script covers:
#   1. Data loading, inspection, and cleaning
#   2. Recoding of demographic/control variables
#   3. Scale construction (Intention, Technostress, Resistance, Trust)
#   4. Descriptive statistics and correlations
#   5. Hypothesis testing via multiple regression (with interactions)
#   6. Visualization of coefficients and interaction effects
#   7. Diagnostic checks (VIF, residuals, heteroscedasticity, normality)


#Packages
library(effsize)
library(tidyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(kableExtra)
library(summarytools)
library(interactions)
library(skimr)
library(corrplot)
library(caret)
library(mgcv)
library(mgcViz)
library(lavaan)
library(kableExtra)
library(psych)
library(Hmisc)
library(knitr)
library(FactoMineR)   
library(factoextra)  


# ---- 1. Loading Data ----

# 1.1 Read the Excel file using the first row as column names:
df <- read_excel("/Users/eugenia.carra/Desktop/Master thesis/Master_thesis_2025_15.37.xlsx", col_names = TRUE)
View(df)
setwd("/Users/eugenia.carra/Desktop/Master thesis")

# 1.2. Drop the second row (which holds the question‐text)
#    so that only actual respondent rows remain:
df <- df[-1, ]

unique(df$CONSENT)
unique(df$ATTCHK)

# 1.3 Inspect the first few rows, structure, summary
head(df)
summary(df)
str(df)

# Check the column names to verify survey question variables
colnames(df)

# 1.4 Keep only the respondents who agreed to participate
df <- df %>%
  filter(
    CONSENT == "Yes, I have read this information carefully and I agree to participate",
    # keep only those who got the attention check right
    ATTCHK == "22"
  )

# # check how many remain
nrow(df)
table(df$CONSENT)
table(df$ATTCHK)

# Therefore, there are 147 actual respondents #

# ---- STUDENT SUBSAMPLE ------------------------------------------------------
df_stud <- df %>% filter(STUDY == "Yes")   # N = 108

#We need students for the targeted sample
# Students represent 73.4% of the full sample (108/147).


# ----- 2. CLEANING and PREPPING DATA -----

# 2.1 Recoding demographics 

df_stud <- df_stud %>%
  mutate(
    # AGE_1 → numeric Age
    Age = as.numeric(AGE_1),
    
    # GENDER → factor 
    Gender = factor(
      GENDER,
      levels = c("Male", "Female", "Non-binary / third gender", "Prefer not to say")
    ),
    
    # EDUC → collapse to short codes:
    Education = case_when(
      grepl("Primary",    EDUC, ignore.case = TRUE) ~ "Primary or less",
      grepl("Secondary",  EDUC, ignore.case = TRUE) ~ "Secondary",
      grepl("Vocational", EDUC, ignore.case = TRUE) ~ "Vocational",
      grepl("Bachelor",   EDUC, ignore.case = TRUE) ~ "Bachelor’s",
      grepl("Master",     EDUC, ignore.case = TRUE) ~ "Master’s",
      grepl("Doctorate",  EDUC, ignore.case = TRUE) ~ "Doctorate+",
      TRUE                                           ~ NA_character_  # anything else
    ),
    # Turn them into a factor:
    Education = factor(
      Education,
      levels = c("Primary or less", "Secondary", "Vocational",
                 "Bachelor’s",       "Master’s",   "Doctorate+")
    ),
    
    MIGR = case_when(
      # if they wrote one of these, treat as “Native”
      MIGR_7_TEXT %in% c(
        "grootouders naar Nederland geëmigreerd",
        "Mijn opa en oma zijn naar nederland geemigreerd"
      ) ~ "My parents and I were born in the Netherlands",
      
      # if they answered one of these, treat as “I migrated…”
      MIGR_7_TEXT %in% c(
        "I'm planning on moving to the Netherlands permanently",
        "Im temporarily in the Netherlands",
        "Internationale student"
      ) ~ "I migrated to the Netherlands",
      
      # otherwise leave MIGR untouched
      TRUE ~ MIGR
    )
  ) %>%
    
    # MIGR → short codes for migration background:
  mutate(
  MigrationBG = case_when(
      MIGR == "I migrated to the Netherlands"               ~ "Self MIGR",
      MIGR == "One of my parents migrated to the Netherlands" ~ "OneParent MIGR",
      MIGR == "Two of my parents migrated to the Netherlands" ~ "TwoParents MIGR",
      MIGR == "My parents and I were born in the Netherlands" ~ "Native",
      # Anything else—including “Prefer not to say” or “Other (specify)”:
      TRUE                                                  ~ "Other"
  ),
    MigrationBG = factor(MigrationBG,
                         levels = c("Self MIGR","OneParent MIGR","TwoParents MIGR","Native", "Other")),
    
    # RESID (province) → factor with 12 province names in correct order:
    Province = factor(
      RESID,
      levels = c(
        "Groningen","Friesland","Drenthe","Overijssel","Flevoland",
        "Gelderland","Utrecht","Noord-Holland","Zuid-Holland",
        "Zeeland","Noord-Brabant","Limburg"
      )
    ),
    
    # STUDY → factor Yes/No
    Studying = factor(STUDY, levels = c("Yes", "No")),
    
    # WORK → factor with four options:
    Working = case_when(
      WORK == "Yes, part-time"     ~ "Part-time",
      WORK == "Yes, full-time"     ~ "Full-time",
      WORK == "No"                 ~ "No",
      TRUE                          ~ "Other"
    ),
    Working = factor(Working,
                     levels = c("Part-time","Full-time","No","Other"))
  )


# 2.2. Recode all Likert items to numeric, only keeping the variables that I need for my research:

# Define the Likert‐scale labels
likert_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

df_stud <- df_stud %>%
  mutate_at(vars(starts_with("TECH_STRESS_"),
                 starts_with("RES_CHANGE_"),
                 starts_with("OVERALL_TRUST_"),
                 starts_with("TRUST_SYS_"),
                 starts_with("INTENTION_")),
            ~ as.numeric(factor(., levels = likert_levels)))


# 2.3 Reverse-code positively phrased items

df_stud <- df_stud %>% 
  mutate(
    # Technostress items to reverse (on a 1–5 scale):
    TECH_STRESS_1  = 6 - TECH_STRESS_1,
    TECH_STRESS_3  = 6 - TECH_STRESS_3,
    TECH_STRESS_4  = 6 - TECH_STRESS_4,
    TECH_STRESS_5  = 6 - TECH_STRESS_5,
    TECH_STRESS_8  = 6 - TECH_STRESS_8,
    TECH_STRESS_10 = 6 - TECH_STRESS_10,
    
    # Resistance items to reverse:
    RES_CHANGE_2 = 6 - RES_CHANGE_2,
    RES_CHANGE_3 = 6 - RES_CHANGE_3,
    RES_CHANGE_8 = 6 - RES_CHANGE_8
  )


# 2.4 Recode Municipal AI use and AI context

# a) Define the labels
freq_labels <- c(
  "Never",
  "About once per year",
  "About once per month",
  "About once per week",
  "Several times per week or daily"
)
ctx_labels <- c(
  "Personal / Household reasons",
  "Work-related",
  "Study-related",
  "Helping someone else",
  "Other setting"
)

# b) recode
df_stud <- df_stud %>%
  mutate(across(
    starts_with("MUNIC_AI_"),
    ~ case_when(
      str_detect(.x, regex("^never$",             ignore_case=TRUE)) ~ 1L,
      str_detect(.x, regex("once per year",        ignore_case=TRUE)) ~ 2L,
      str_detect(.x, regex("once per month",       ignore_case=TRUE)) ~ 3L,
      str_detect(.x, regex("once per week",        ignore_case=TRUE)) ~ 4L,
      str_detect(.x, regex("several times per week|daily", ignore_case=TRUE)) ~ 5L,
      TRUE ~ NA_integer_
    )
  )) %>%
  # then convert to an ordered factor
  mutate(across(
    starts_with("MUNIC_AI_"),
    ~ factor(.x,
             levels = 1:5,
             labels = freq_labels,
             ordered = TRUE)
  ))

# same process for AI context
df_stud <- df_stud %>%
  mutate(across(
    starts_with("AI_CONTEXT_"),
    ~ case_when(
      str_detect(.x, regex("personal",      ignore_case=TRUE)) ~ 1L,
      str_detect(.x, regex("work",          ignore_case=TRUE)) ~ 2L,
      str_detect(.x, regex("study",         ignore_case=TRUE)) ~ 3L,
      str_detect(.x, regex("help",          ignore_case=TRUE)) ~ 4L,
      str_detect(.x, regex("other setting", ignore_case=TRUE)) ~ 5L,
      TRUE                                                     ~ NA_integer_
    )
  )) %>%
  mutate(across(
    starts_with("AI_CONTEXT_"),
    ~ factor(.x,
             levels = 1:5,
             labels = ctx_labels,
             ordered = TRUE)
  ))

# Verify
summary(df_stud$MUNIC_AI_1) 
summary(df_stud$AI_CONTEXT_1)

#---------- 3. Build composite scores -------

df_stud <- df_stud %>%
  rowwise() %>%
  mutate(
    
    # Technostress → here we take the overall average of all 10 TECH_STRESS items:
    TechStress = mean(c_across(TECH_STRESS_1:TECH_STRESS_10), na.rm = TRUE),
    
    # Resistance to change → here we take the mean of all 8 items
    Resistance = mean(c_across(RES_CHANGE_1:RES_CHANGE_8), na.rm = TRUE),
    
    # Overall trust in municipality → 4 items
    Trust_Municipality = mean(c_across(OVERALL_TRUST_1:OVERALL_TRUST_4), na.rm = TRUE),
    
    # System trust → 3 items
    Trust_System = mean(c_across(TRUST_SYS_1:TRUST_SYS_3), na.rm = TRUE),
    
    # Combined “perceived trustworthiness”
    Trust_Total = mean(c(Trust_Municipality, Trust_System), na.rm = TRUE),
    
    # Intention to use → 3 items
    Intention = mean(c_across(INTENTION_1:INTENTION_3), na.rm = TRUE)
  ) %>%
  ungroup()


# Composite score for Municipal AI use and Context

df_stud <- df_stud %>%
  # First convert the factor levels back to their underlying codes 1–5
  mutate(across(
    starts_with("MUNIC_AI_"),
    ~ as.numeric(.),   # factor → numeric automatically gives 1…5
    .names = "num_{col}"
  )) %>%
  # now compute UseIntensity on those numeric versions
  mutate(
    UseIntensity = rowMeans(select(., starts_with("num_MUNIC_AI_")), na.rm = TRUE)
  ) %>%
  # remove the helper columns
  select(-starts_with("num_MUNIC_AI_"))

# Verify
summary(df_stud$UseIntensity)
hist(df_stud$UseIntensity, main="UseIntensity", xlab="Average Frequency (1–5)")
#On a 1–5 scale of “how often you used any municipal AI service,” the typical student is clustered at the bottom.
# 25 % of students average exactly 1.0 (“Never”).
# The median is 1.4, and the upper quartile is 1.6, so even the “heavy” users only chose “about once a year” on average.
# A few outliers (max = 4) show weekly/daily users, but they’re rare.
#The histogram also shows that most of the students sits in the 1–2 range, with a long right tail towards the occasional power user.

df_stud <- df_stud %>%
  mutate(across(
    starts_with("AI_CONTEXT_"),
    ~ as.numeric(.),
    .names = "num_{col}"
  )) %>%
  mutate(
    ContextBreadth = rowSums(!is.na(select(., starts_with("num_AI_CONTEXT_"))), na.rm = TRUE)
  ) %>%
  select(-starts_with("num_AI_CONTEXT_"))

# Verify
summary(df_stud$ContextBreadth)
table(df_stud$ContextBreadth)

#median (2.00) and mean (1.796)
# 30 students (27%) have never used any AI service in zero contexts (25%)
# 36% of students have used AI in one or two contexts.
# 22% of students used AI in three contexts
# 8% of students have used AI in four different contexts.
# Six students used AI in all five context (study-related, work-related, helping somebody else, personal reasons and other setting).

## 1. Use-intensity (five frequency items) ------------------------------
freq_items <- df_stud %>%                                   # original data
  select(starts_with("MUNIC_AI_")) %>%                      # only the 5 items
  mutate(across(everything(), as.numeric)) %>%              # ensure numeric
  select(where(~ !all(is.na(.x)) & sd(.x, na.rm = TRUE) > 0))  # drop zero-var

useint_alpha <- alpha(freq_items, check.keys = FALSE)
round(useint_alpha$total$raw_alpha, 3)   # 0.718

## 2. Context-breadth (five items) -------------------------------
ctx_items <- df_stud %>% 
  select(starts_with("AI_CONTEXT_")) %>% 
  mutate(across(everything(), ~ as.numeric(!is.na(.))))      # 1 = used, 0 = never

ctx_alpha <- alpha(ctx_items, check.keys = FALSE)
round(ctx_alpha$total$raw_alpha, 3)   # 0.742 


# ----- 5. Descriptive statistics -------

# 5.1 Subset to the variables needed for my research
df2_stud <- df_stud %>%
  select(
    Intention, TechStress, Resistance, Trust_Total, Trust_Municipality, Trust_System,
    Age, Gender, Education, MigrationBG, Province, Studying, Working, UseIntensity, ContextBreadth
  )

# 5.2 Demographics: counts & percentages
demog_vars <- c("Gender","Education","MigrationBG","Province","Studying","Working")

# Function to get n & % for a factor
freq_pct <- function(df2_stud, var)
  df2_stud %>%
  count(!!sym(var)) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1)
  ) %>%
  rename(level = !!sym(var)) %>%
  kable(col.names = c(var, "Count", "Percent"), caption = paste("Distribution of", var))


# Print tables
for(v in demog_vars) {
  print(freq_pct(df2_stud, v))
}

# Gender: of 108 respondents, 61.1% are female, 37% are male, and 1.9% non binary or third gender.
# Education: the majority of respondents holds a bachelor's degree (61.1%), 13% a Master's degree, 24% secondary, 1.9% vocational, and nobody holds a doctorate or higher degree. Finally, nobody has only a primary level education.
# Migration background: 44.4 % are first-generation migrants, 14.8% have one migrant parent, 21.3% two migrant parents, 16.7% are native, and 2.8% fall in the “Other” category, which consits of respondents who did not disclose their migration background.
# Province:  Nearly 86 % live in North or South Holland (57.4% Noord-Holland, 28.7% Zuid-Holland); the rest are spread across Utrecht (4.6%), Flevoland (4.6%), and Noord-Brabant (3.7%), with Gelderland at 0.9%.
# Studying & Working: As expected, there are 108 current students; of all respondents, more than half work part-time (58.3%), 11.1% full-time, 26.9% not at all, and 3.7% in “Other” working arrangements.


# 5.3 Continuous summaries
cont_vars <- c("Age","UseIntensity") #for context it does not make sense to calculate a mean, rather I will analyse each context separately. 

# Coerce to a data.frame
cont_stats <- psych::describe(df2_stud[cont_vars]) %>%
  as.data.frame()

# Describe for mean, sd, etc.
cont_stats <- cont_stats %>%
  mutate(Variable = row.names(.)) %>%
  select(Variable, n, mean, sd, min, max, skew, kurtosis)

# Print the table with kable
kable(
  cont_stats,
  digits  = 2,
  caption = "Descriptive stats for continuous variables"
)

# Age: Mean age is 23.31 years (SD = 2.53), ranging from 18 to 33, with a moderate positive skew (1.28), suggesting a tail of older respondents.
# UseIntensity (municipal AI): Low overall (M = 1.46 on a 1–5 scale, SD = 0.50), strongly right-skewed (2.06), indicating most people rarely use these AI services, with a few more frequent users.

#### Plots ####
# 1) Demographics bar-charts with percentages on top
for (v in demog_vars) {
  p <- ggplot(df2_stud, aes_string(x = v)) +
    geom_bar(aes(y = after_stat(count) / sum(after_stat(count))),
             fill = "lightblue") +
    geom_text(
      aes(
        y     = after_stat(count) / sum(after_stat(count)),
        label = percent(after_stat(count) / sum(after_stat(count)),
                        accuracy = 0.1)
      ),
      stat = "count",
      vjust = -0.3
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste("Distribution of", v),
      x     = v,
      y     = "Percent of sample"
    ) +
    theme_minimal() +
    theme(
      plot.title  = element_text(face = "bold", size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(p)
}


#### Overall Likert‐scales variables descriptives ####
# 1)
comp_vars <- c("TechStress", "Resistance", "Trust_Total", "Intention")

# 2) Pivot to long form correctly:
comp_long <- df2_stud %>%
  select(all_of(comp_vars)) %>%       # now only 4 columns remain
  pivot_longer(
    cols      = everything(),         # these 4 get pivoted
    names_to  = "Scale",
    values_to = "Score"
  ) %>%
  filter(!is.na(Score))


# 3) Quick check that it worked:
head(comp_long)

# 4. Summaries
comp_summary <- comp_long %>%
  group_by(Scale) %>%
  summarise(
    N      = n(),
    Mean   = round(mean(Score), 2),
    Median = round(median(Score), 2),
    SD     = round(sd(Score),   2),
    Q1     = round(quantile(Score, 0.25), 2),
    Q3     = round(quantile(Score, 0.75), 2),
    Min    = min(Score),
    Max    = max(Score)
  )

kable(
  comp_summary,
  col.names = c("Scale","N","Mean","Median","SD","Q1","Q3","Min","Max"),
  caption   = "Descriptives for Composite Likert Scales"
)

#Interpretation:
# Intention: On average, respondents sit just above the neutral midpoint (3.33), with 50 % of scores between “Neither agree nor disagree” and “Somewhat agree.” The full range (1.0–5.0) shows some people are very reluctant while others are very willing, but most cluster around some agreement to reuse municipal AI.
# Resistance to change (M = 3.19, Mdn = 3.12, SD = 0.59, Q1–Q3 = 2.75–3.62). On average people are very close to neutral, with a slight tendency toward acceptance of change (scores > 3). The tighter SD and IQR indicate less variability here.
# Technostress (M = 2.34, Mdn = 2.30, SD = 0.58, Q1–Q3 = 1.98–2.80). Mean and median are well below 3.0, showing low overall technostress. Half of respondents score between “Somewhat disagree” (1.90) and about “Neither agree nor disagree” (2.80) on stress‐related items. Virtually no one reports extreme stress (max = 3.7).
# Trust_Total (M = 3.23, Mdn = 3.35, SD = 0.66, Q1–Q3 = 3.00–3.60). Trust is closely to neutral and slightly positive. The median above the mean hints at a slight skew toward higher trust among some.


# 5. Boxplot
ggplot(comp_long, aes(x = Scale, y = Score)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(
    title = "Distribution of Composite Scale Scores",
    x     = "Scale",
    y     = "Score (1 (Strongly Disagree)– 5 (Strongly Agree))"
  )

#more fancy plot
comp_long %>%
  ggplot(aes(x = Scale, y = Score)) +
  geom_violin(fill = "lightblue", alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.2, linewidth = 0.7, outlier.color = "red") +
  theme_minimal() +
  labs(
    title = "Composite Scale Distributions",
    x     = NULL,
    y     = "Score (1 (Strongly Disagree)– 5 (Strongly Agree))"
  ) +
  theme(
    plot.title  = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold")
  ) -> p3
print(p3)

#TechStress (third violin) is strongly concentrated at the bottom of the scale (around 1 and 3), confirming that most students report low technostress.
# Intention (first violin) and Trust_Total (fourth violin) both show a “bulge” around 3–4, indicating that most responses cluster in the “somewhat agree” range.
# Resistance (second violin) peaks right around 3, suggesting most are neutral or only mildly resistant to change.
# TechStress is left-skewed (long tail toward higher stress scores), whereas Trust_Total is slightly right-skewed (a few very high trust scores).
# Intention is fairly symmetric around the medians.
# Resistance has a light left-skew (more lower-resistance than very high resistance), indicating generally mild or no resistance.
# Trust_Total shows a strong density around 3.2–3.7 with a longer lower tail, meaning most students are neutral-to-trusting but a few are quite distrustful.
# A few extreme low Intention scores (1.0), extreme high Trust_Total scores (5.0) reflect students who very strongly agreed or disagreed on these scales.



#### Technostress descriptive ####
#To check if the results are correct, I also inspected each of the technostress items, resistance to change items and Perceived trustworthiness.

# 1. Identify the technostress items
tech_items <- df_stud %>%
  select(starts_with("TECH_STRESS_")) %>%
  names()

# 2. Pivot to long form
tech_long <- df_stud %>%
  select(all_of(tech_items)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "Item",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response))

# 3A. Medians & quartiles per item
tech_stats <- tech_long %>%
  group_by(Item) %>%
  summarise(
    N      = n(),
    Median = median(Response),
    Q1     = quantile(Response, .25),
    Q3     = quantile(Response, .75),
    Min    = min(Response),
    Max    = max(Response)
  ) %>%
  ungroup()

kable(
  tech_stats,
  digits    = 2,
  col.names = c("Item","N","Median","Q1","Q3","Min","Max"),
  caption   = "Median & Quartiles for Technostress Items"
)

# 3B. Counts and percentages
tech_counts <- tech_long %>%
  count(Item, Response) %>%
  group_by(Item) %>%
  mutate(Percent = round(n / sum(n) * 100, 1)) %>%
  ungroup()

kable(
  tech_counts,
  col.names = c("Item","Response","Count","Percent"),
  caption   = "Response Distribution for Technostress Items"
)

# 4. Facetted bar‐plot of counts
ggplot(tech_long, aes(x = factor(Response,1:5))) +
  geom_bar(fill = "tomato") +
  facet_wrap(~ Item, ncol = 5, scales = "free_y") +
  scale_x_discrete(labels = c("1\n(SD)","2\n(D)","3\n(N)","4\n(A)","5\n(SA)")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 7),
    strip.text   = element_text(size = 8)
  ) +
  labs(
    title = "Technostress Item Response Distributions",
    x     = "Response (1=Strongly Disagree … 5=Strongly Agree)",
    y     = "Count"
  )

# Most responses cluster at the bottom end of the scale
# Across nearly all items, the tallest bars are between 1 (“Strongly Disagree”) and 3 (“Nieither Agree or Disagree”), indicating that most students do not feel high levels of technostress on any single dimension.
# Neutral midpoint at 3 is still common for almost every item, meaning that students are often ambivalent rather than actively stressed.
# Very few extreme “5” stress reports, so very few students strongly agree that AI-driven municipal services overwhelm or invade them.
# Reverse-coded items (3, 4, 5, 8, 10) behave as expected: After reverse-recoding, these panels still peak at 1–2, confirming that even when the question was phrased positively (e.g. “My personal life remains unaffected”), the vast majority “strongly” or “somewhat” agree with low invasion/complexity.
# Items 2 (“additional and complex interfaces”) and 9 (“features change too often”) have slightly higher counts at 4 and 5, suggesting these are the most stressful aspects, but still well below the midpoint.
# Items 7 (“limited AI skills will block access to essential AI services”) and 8 (“I feel confident using the system”) show the lowest stress, with the highest bars at 1 and 2.


####  Resistance‐to‐Change descriptive ####

# 1A. Identify the resistance items
res_items <- df_stud %>%
  select(starts_with("RES_CHANGE_")) %>%
  names()

# 1B. Pivot to long form
res_long <- df_stud %>%
  select(all_of(res_items)) %>%
  pivot_longer(
    cols     = everything(),
    names_to = "Item",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response))

# 2A. Medians & quartiles per item
res_stats <- res_long %>%
  group_by(Item) %>%
  summarise(
    N      = n(),
    Median = median(Response),
    Q1     = quantile(Response, .25),
    Q3     = quantile(Response, .75),
    Min    = min(Response),
    Max    = max(Response)
  ) %>%
  ungroup()

kable(
  res_stats,
  digits    = 2,
  col.names = c("Item","N","Median","Q1","Q3","Min","Max"),
  caption   = "Median & Quartiles for Resistance Items"
)

# 2B. Counts & percentages
res_counts <- res_long %>%
  count(Item, Response) %>%
  group_by(Item) %>%
  mutate(Percent = round(n / sum(n) * 100, 1)) %>%
  ungroup()

kable(
  res_counts,
  col.names = c("Item","Response","Count","Percent"),
  caption   = "Response Distribution for Resistance Items"
)

# 2C. Facetted bar-plot of counts
ggplot(res_long, aes(x = factor(Response, 1:5))) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~ Item, ncol = 4, scales = "free_y") +
  scale_x_discrete(labels = c("1\n(SD)","2\n(D)","3\n(N)","4\n(A)","5\n(SA)")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 7),
    strip.text   = element_text(size = 8)
  ) +
  labs(
    title = "Resistance‐to‐Change Item Response Distributions",
    x     = "Response (1=Strongly Disagree … 5=Strongly Agree)",
    y     = "Count"
  )

# Across the eight resistance items, students show a slight tendency toward acceptance of change.
# Median responses range from 2 to 4.5, with most medians at 3 or 4. 
# The strongest endorsement is for Item 1 (mean ≈ 4, median = 4.5, and Q3= 5.00), indicating most students resist new AI processes in principle, 
# while Items 7–8 (median = 2) reveal a minority with more rigid or short-term–focused tendencies.
# Items 1 & 2: very tall bars at 4–5
# But also, item 3 and 4 show somewhat high bars at 4 and 5.
# Items 4 & 6: more balanced, tall bars around 3
# Items 7 & 8: peak at 1–2, but item 7 reports quite high levels of "Somewhat agree".



#### Perceived‐Trust descriptive ####

# 3A. Identify the trust items (institutional + system)
trust_items <- df_stud %>%
  select(starts_with("OVERALL_TRUST_"), starts_with("TRUST_SYS_")) %>%
  names()

# 3B. Pivot to long form
trust_long <- df_stud %>%
  select(all_of(trust_items)) %>%
  pivot_longer(
    cols     = everything(),
    names_to = "Item",
    values_to = "Response"
  ) %>%
  filter(!is.na(Response))

# 4A. Medians & quartiles per item
trust_stats <- trust_long %>%
  group_by(Item) %>%
  summarise(
    N      = n(),
    Median = median(Response),
    Q1     = quantile(Response, .25),
    Q3     = quantile(Response, .75),
    Min    = min(Response),
    Max    = max(Response)
  ) %>%
  ungroup()

kable(
  trust_stats,
  digits    = 2,
  col.names = c("Item","N","Median","Q1","Q3","Min","Max"),
  caption   = "Median & Quartiles for Trust Items"
)

# 4B. Counts & percentages
trust_counts <- trust_long %>%
  count(Item, Response) %>%
  group_by(Item) %>%
  mutate(Percent = round(n / sum(n) * 100, 1)) %>%
  ungroup()

kable(
  trust_counts,
  col.names = c("Item","Response","Count","Percent"),
  caption   = "Response Distribution for Trust Items"
)

# 4C. Facetted bar-plot of counts
ggplot(trust_long, aes(x = factor(Response, 1:5))) +
  geom_bar(fill = "forestgreen") +
  facet_wrap(~ Item, ncol = 4, scales = "free_y") +
  scale_x_discrete(labels = c("1\n(SD)","2\n(D)","3\n(N)","4\n(A)","5\n(SA)")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 7),
    strip.text   = element_text(size = 8)
  ) +
  labs(
    title = "Trust Item Response Distributions",
    x     = "Response (1=Strongly Disagree … 5=Strongly Agree)",
    y     = "Count"
  )

# For Municipality trust, for all four items the median is 4 and Q1=3, while Q3=4.
# From the bar-chart it becomes clear that most students tend to generally trust their municipalities, reflecting quite a high institutional trust.
# When asked about the AI services used by their municipalities, if the have safeguards etc... the median is 3 for all items, reflecting a lower, but still ambivalent, general trust in the AI system itself.
# Especially in Trust_sys_2, asking about legal and technological structures protecting from possible problems, students tend to score lower on the scale. 
# For Trust_sys_1 and 3, most students do not have a preference or are not informed enough, choosing to neither agree or disagree with the statement about safeguards in AI systems used by their municipality.



# 5.5 Descriptive Stats & Plots for each AI service

# Gather the five MUNIC_AI_* columns into long form
svc_long <- df_stud %>%
  select(starts_with("MUNIC_AI_")) %>%
  mutate(id = row_number()) %>%        # unique respondent ID
  pivot_longer(
    cols = starts_with("MUNIC_AI_"),
    names_to = "ServiceCode",
    values_to = "Frequency"
  ) %>%
  mutate(Service = case_when(
    ServiceCode == "MUNIC_AI_1" ~ "Chatbot",
    ServiceCode == "MUNIC_AI_2" ~ "Report",
    ServiceCode == "MUNIC_AI_3" ~ "Permit",
    ServiceCode == "MUNIC_AI_4" ~ "Appointments",
    ServiceCode == "MUNIC_AI_5" ~ "OtherAI",
    TRUE ~ NA_character_
  )) %>%
  select(id, Service, Frequency)

# Verify svc_long structure
head(svc_long)

# 1.3 Compute counts & percents
freq_table <- svc_long %>%
  count(Service, Frequency) %>%
  group_by(Service) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 1)
  )

# Inspect it
freq_table %>%
  arrange(Service, Frequency) %>%
  kable(
    col.names = c("Service", "Frequency", "Count", "Percent"),
    caption = "Usage frequency by AI service"
  )
# Overall, for each service, the majority of respondents report never using it.
# Tools for booking appointments are the most widely used municipal AI service with 34.3% using it at least once per year and 20.4% using them once per month.
# Chatbots as well score high with 34.3% of respondents using them at least once per year, but only 4.6% using them once per month.
# Permit applications are used from 33.3% of respondents at least once per year.
# Report and other AI services are less used with respectively 87% and 96.3% of respondents that never used them.
# In general, very few respondents use any service weekly or daily, indicating that even the most popular tools (Permit apps, Chatbots) remain primarily annual or monthly usages.

#Service usage frequency plot
ggplot(svc_long, aes(x = Service, fill = Frequency)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Service Usage Frequencies",
    x = "AI Service",
    y = "Count",
    fill = "Frequency"
  ) +
  coord_flip()

#In the plot it becomes even clearer that most people never used most of the applications.
#Still, Permit applications, chatbots and websites for booking appointments are used at least once a year from many students


# ------ 6. Cronbach’s α and Correlations ------------

# 6.1. Extract the item block:
tech_items <- df_stud %>% select(starts_with("TECH_STRESS_"))
res_items  <- df_stud %>% select(starts_with("RES_CHANGE_"))
trust_items <- df_stud %>% select(starts_with(c("OVERALL_TRUST_","TRUST_SYS_")))
int_items   <- df_stud %>% select(starts_with("INTENTION_"))


# 6.2. Compute Cronbach’s α:
tech_alpha <- psych::alpha(tech_items, check.keys=FALSE)
res_alpha  <- psych::alpha(res_items, check.keys=FALSE)
trust_alpha <- psych::alpha(trust_items, check.keys=TRUE)
int_alpha   <- psych::alpha(int_items,   check.keys=TRUE)

# 6.3. Print the results for Technostress:
cat("TechStress α = ", tech_alpha$total$raw_alpha, "\n")

#Cronbach’s α = 0.8, which is above the usual 0.70 cutoff for acceptable reliability.

print(tech_alpha$item.stats)    # see item‐total correlations
# Most items have good item–total correlations (r.drop > 0.40 for 8 of the 10 items), 
# indicating that they cohere well with the overall scale.
# TECH_STRESS_1 has the lowest corrected item–total correlation (r.drop ≈ 0.29), suggesting it contributes relatively little to the composite.
#Therefore, even when applying the Technostress scale to municipal AI system, the scale remains reliable. 
# we can then reasonably aggregate  the 10 items into a single Technostress score, or we could drop TECH_STRESS_1, but since α is already acceptable, it is also defensible to keep it for content coverage.

# 6.4. Print the results for Resistance to change
cat("Resistance α = ", res_alpha$total$raw_alpha, "\n")
print(res_alpha$item.stats)

#Cronbach’s α = 0.69, slightly below 0.70, suggesting the items only moderately hang together as a unidimensional scale.
# Items 5 and 6 (“Short-term focus”) have the lowest corrected item–total correlations (r.drop ≈ 0.24 and 0.26).
# All other items show r.drop ≥ 0.29, with items 2–4 and 7–8 around 0.39–0.48.
# we then might consider dropping one or both items 5 and 6 since they contribute relatively little to the internal consistency of the scale,
# but since it's very slightly below 0.70 we could still aggregate the items into one single scale.


# 6.5. Print the results for Perceived trustworthiness
cat("Trust α =", trust_alpha$total$raw_alpha, "\n")
print(trust_alpha$item.stats)

# The Cronbach's α is 0.87, so well above the conventional 0.70 cutoff, indicating high reliability.
# Moreover, every items correlates strongly with the rest of the scale and the lowest r.drop is 0.66 for Overall_trust_4 and 0.62 for Trust_sys_3, still solid.
# OVERALL_TRUST items cluster around mean ≈ 3.3–3.7 on a 1–5 scale, with SDs ≈ 0.7–1.00.
# TRUST_SYS items are slightly lower (means ≈ 2.8–3.0), but with comparable spread.

# 6.6. Print the results for Intention to use
cat("Intention α =",  int_alpha$total$raw_alpha, "\n")
print(int_alpha$item.stats)

# Here, the Cronbach's α is 0.87 demonstrating excellent reliability.
# INTENTION_3 (r.drop = 0.8) is the strongest contributor; the others are also very solid.
# INTENTION_2 has a higher mean (3.67) than INTENTION_1 (3.10) or INTENTION_3 (3.16), but all three have SDs ≈ 1.00–1.02.


# 6.7. Correlation matrices
cor(tech_items, use="pairwise.complete.obs")
cor(res_items,  use="pairwise.complete.obs")
cor(trust_items, use="pairwise.complete.obs")
cor(int_items,  use="pairwise.complete.obs")

# Technostress inter‐item correlations:
# Range: –0.18 (weak/negative) to +0.70 (strong) across the 10 items.
# Most correlations are positive and moderate (around 0.15–0.50), indicating the items tend to move together.
# A few negative or very low correlations correspond to the items we reverse‐coded, which is normal.
# The strongest links (.40–.70) occur among items 5–10, indicating a tight “core” of complexity/insecurity/uncertainty.
# Items 1 and 3 show very low (near zero) or slightly negative correlations with most others, suggesting they tap a different aspect.
# Overall, there is a clearly defined core technostress factor, but TS1 (and to a lesser extent TS3) do not align well and could be dropped or treated separately.

# Resistance‐to‐change inter‐item correlations:
# The inter‐item correlation matrix for the eight resistance to change items reveals two clearly separable clusters (and helps explain why the overall α is .69).
# Correlations among items 1–4 (routine-seeking/emotional) range from about r = .28 to .40, with the strongest link between items 2 and 3 (r = .62).
# This indicates a cohesive sub‐group tapping “disruption to familiar routines” and “emotional response”.
# Correlations within items 5–8 (short-term focus/rigidity): run from r = .26 to .47, strongest between items 7 and 8 (r = .47).
# They form a second sub‐group around “focus on immediate problems” and “cognitive rigidity.”
# Across clusters, correlations are very low or even negative (e.g. item 1 vs. item 5 is r = –.10), showing that the two dimensions don’t hang together.  
# Overall, the pattern supports dropping items that don't correlate well with the rest of the items or treating Resistance to change as two distinct subscales. 

# Trust inter‐item correlations:
# Overall trust items (1–4) correlate strongly (≈0.57–0.79), showing they tap a common municipality‐trust factor.
# System trust items (TRUST_SYS_1–3) also intercorrelate well (≈0.65–0.68) and also correlate reasonably with overall trust (≈0.29–0.46).
# This pattern supports combining all seven trust items into a single “perceived trustworthiness” scale.

# Intention‐to‐use inter‐item correlations:
# all three items correlate strongly (≈0.62–0.78), indicating a cohesive intention construct.
# High intercorrelations justify averaging these three items into one intention score.

# ----- 6.1 Pairwise correlations of composites for Results ------- 

# select the four composites
comp_vars <- c("Intention", "TechStress", "Resistance", "Trust_Total")

# compute pairwise correlations
pwcorr <- cor(
  df2_stud[comp_vars],
  use = "pairwise.complete.obs",
  method = "pearson"
)

# round and print
pwcorr_rounded <- round(pwcorr, 2)
print(pwcorr_rounded)

# add significance levels with Hmisc::rcorr
if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")
library(Hmisc)
rc <- Hmisc::rcorr(
  as.matrix(df2_stud[comp_vars]),
  type = "pearson"
)
# rc$r is the correlation matrix, rc$P the p-values
rc$r %>% round(2)     # correlations
rc$P %>% round(3)     # p-values


# ------------ 6.2  Table for composites and continuous variables ------------

comp_vars <- c("Intention", "TechStress", "Resistance", "Trust_Total")
cont_vars <- c("Age","UseIntensity")
all_vars  <- c (comp_vars, cont_vars)

# define the make_corr_tab function
make_corr_tab <- function(vars, data){
  # a) descriptive stats
  desc <- psych::describe(data[vars])
  # b) pearson correlations + p-values
  R <- Hmisc::rcorr(as.matrix(data[vars]), type="pearson")
  Rmat <- round(R$r, 2)
  Pmat <- R$P
  
  # build output data.frame
  out <- data.frame(
    Variable = vars,
    Mean     = round(desc$mean, 2),
    SD       = round(desc$sd,   2),
    Range    = paste0(round(desc$min,2),"–",round(desc$max,2)),
    stringsAsFactors = FALSE
  )
  # add one empty column per variable to hold the lower‐triangle correlations
  for(v in vars) out[[v]] <- ""
  
  # fill in “r (p)” only for i > j
  n <- length(vars)
  for(i in seq_len(n)) for(j in seq_len(n)){
    if(i > j){
      out[i, vars[j]] <- sprintf("%0.2f (%0.3f)", Rmat[i,j], Pmat[i,j])
    }
  }
  return(out)
}

# build & style the table
tab_all <- make_corr_tab(all_vars, df2_stud)

kable(
  tab_all,
  caption        = "All Numeric Variables: Descriptives & Correlations",
  booktabs       = TRUE,
  align          = "c",
  escape         = FALSE
) %>%
  kable_styling(
    full_width       = FALSE,
    bootstrap_options = c("striped","hover","condensed")
  ) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:4, width = "2.5cm") %>%
  row_spec(0, bold = TRUE)

# ------------ 6.3  Table for demographic factor-based variables ------------

demog_vars <- c("Gender","Education","MigrationBG","Province","Working")

# build one long table of counts + pct
demog_tab <- map_dfr(demog_vars, function(v) {
  df2_stud %>%
    count(!!sym(v)) %>%
    mutate(
      Percent  = round(n / sum(n) * 100, 1),
      Variable = v,
      Category = as.character(!!sym(v))
    ) %>%
    select(Variable, Category, Count = n, Percent)
})

# print with kableExtra
demog_tab %>%
  kable(
    caption        = "Table 1 – Sample demographics (N = 108)",
    col.names      = c("Variable","Category","Count","Percent"),
    booktabs       = TRUE,
    align          = c("l","l","r","r")
  ) %>%
  kable_styling(
    full_width       = FALSE,
    bootstrap_options = c("striped","condensed","hover")
  ) %>%
  pack_rows("Gender",       1,  length(unique(demog_tab$Category[demog_tab$Variable=="Gender"]))) %>%
  pack_rows("Education",    min(which(demog_tab$Variable=="Education")),
            max(which(demog_tab$Variable=="Education"))) %>%
  pack_rows("MigrationBG",  min(which(demog_tab$Variable=="MigrationBG")),
            max(which(demog_tab$Variable=="MigrationBG"))) %>%
  pack_rows("Province",     min(which(demog_tab$Variable=="Province")),
            max(which(demog_tab$Variable=="Province"))) %>%
  pack_rows("Working",      min(which(demog_tab$Variable=="Working")),
            max(which(demog_tab$Variable=="Working")))


# ----- 7. Visualize loading pattern with corrplot ----------

# With these codes I visualise correlations that I previously also checked. 
# Technostress
corrplot(cor(tech_items,   use="pairwise.complete.obs"), 
         is.corr=TRUE, 
         title="Technostress Item Correlations", 
         mar=c(0,0,1,0))

# The corrplot reveals that most TECH_STRESS items intercorrelate at moderate‐to‐high levels (r ≈ .25–.70; medium to dark blue), 
# especially those tapping complexity/insecurity/uncertainty (e.g. TS6–TS9 cluster very tightly).
# By contrast, TS1 stands out as much weaker—its correlations with other items hover near zero or even slightly negative (very light blue or light red), 
# matching its low item–total drop and suggesting it contributes little to the overall construct.
# Overall, the dense band of medium–dark blues among TS2–TS10 supports treating these items as a coherent technostress scale,
# but TS1 could be reconsidered or dropped to boost internal consistency further.

# Resistance
corrplot(cor(res_items,    use="pairwise.complete.obs"), 
         is.corr=TRUE, 
         title="Resistance Item Correlations", 
         mar=c(0,0,1,0))

# The heatmap is mostly light to medium blue, indicating generally modest positive correlations among the eight resistance items.
# The strongest blues (~ 0.4–0.6) appear between items 2 & 3 and between items 7 & 8.
# RES_CHANGE_5 stands out with very pale (even slightly negative) correlations against some items (e.g., r ≈ –.12 with RES_CHANGE_1), suggesting it may not align well with the rest.
# No item pair (aside from the RES_CHANGE_5 anomalies) shows zero or negative correlation overall, so the scale still holds together, but RES_CHANGE_5 could be reconsidered or reworded to boost coherence.


# Trust
corrplot(cor(trust_items,  use="pairwise.complete.obs"), 
         is.corr=TRUE, 
         title="Trust Item Correlations", 
         mar=c(0,0,1,0))

# The four OVERALL_TRUST items form a dense block of dark blue (r ≈ .61–.80), indicating very high coherence among institutional‐trust questions.
# The three TRUST_SYS items also cluster in deep blue (r ≈ .61–.68), showing strong internal consistency for system‐trust.
# The off‐diagonal correlations between OVERALL_TRUST and TRUST_SYS items are medium blue (r ≈ .30–.46), reflecting a moderate but lower linkage between institutional and system trust.
# Overall, all trust items correlate positively, supporting aggregation into a single perceived‐trustworthiness scale with two closely related sub‐dimensions. 

# Intention
corrplot(cor(int_items,    use="pairwise.complete.obs"), 
         is.corr=TRUE, 
         title="Intention Item Correlations", 
         mar=c(0,0,1,0))
# INTENTION_1 and INTENTION_3 show the strongest link (r ≈ .78; darkest blue), indicating very high agreement between those two items.
# Correlations of INTENTION_2 with INTENTION_1 (r ≈ .62) and INTENTION_3 (r ≈ .66) are slightly lower (medium-blue), but still strong.
# All three inter-item correlations exceed .60, confirming that the intention items form a coherent single scale.



# -------- 8.Confirmatory factor analysis (CFA) for each scale ---------

# 1. Define the CFA model according to the dimensions determined by the theory

# Since to run the model with five factors I would needs more participants (now I only have 108),
# I decided to run a confirmatory factor analysis to check for one technostress underlying factor.

# A. Technostress CFA
onefactor_tech <- "
  TS_factor =~ TECH_STRESS_1 + TECH_STRESS_2 + TECH_STRESS_3 + 
                 TECH_STRESS_4 + TECH_STRESS_5 + TECH_STRESS_6 +
                 TECH_STRESS_7 + TECH_STRESS_8 + TECH_STRESS_9 + TECH_STRESS_10
"

fit.tech <- cfa(
  model         = onefactor_tech,
  data          = df_stud,
  std.lv        = TRUE,
  meanstructure = TRUE,
  missing       = "fiml"
)

summary(fit.tech, fit.measures=TRUE, standardized=TRUE)
fitMeasures(fit.tech, c("rmsea","cfi","tli","srmr"))

# CFI = .807, TLI = .752 (both  below the .90 “acceptable” cut-off)
# RMSEA = .135 (well above .08) with 90% CI [105–.165]
# SRMR = .090 (above .08)
# Thus, as expected, a single technostress factor does not seem to explain well the data.
# Moreover, TS1 loads only weakly (TS1 = 0.078).
# According to these results, I will run and EFA to see the underlying factors of technostress and to decide if it's better to use a composite mean or different factors.

# B. Resistance to change CFA

res_model <- "
  RoutineSeek =~ RES_CHANGE_1 + RES_CHANGE_2
  Emotional   =~ RES_CHANGE_3 + RES_CHANGE_4
  ShortTerm   =~ RES_CHANGE_5 + RES_CHANGE_6
  Rigidity    =~ RES_CHANGE_7 + RES_CHANGE_8
"

fit.res <- cfa(
  model         = res_model,
  data          = df_stud,
  std.lv        = TRUE,
  meanstructure = TRUE,
  missing       = "fiml"
)

summary(fit.res, fit.measures=TRUE, standardized=TRUE)
fitMeasures(fit.res, c("rmsea","cfi","tli","srmr"))

# CFI=.893 →  just matching the “acceptable” .90 threshold.
# TLI=.79 → a bit below .90, indicating slighlty poor incremental fit.
# RMSEA=.108 (90% CI [.056–.143]) → well above the .08 cutoff for “reasonable” fit.
# SRMR=.061 → below the .08 treshold.
# However, two out of four indices fall in the poor range, so the 4 factor in resistance to change don't fit well.
# Still, all items load significantly on their theorized factors.
# RoutineSeek and Emotional form the same dimension, so they could be merged in one factor or I could still use a composite score with the mean of all items.


# C. Perceived Trustworthiness CFA

trust_model <- "
  InstTrust =~ OVERALL_TRUST_1 + OVERALL_TRUST_2 + OVERALL_TRUST_3 + OVERALL_TRUST_4
  SysTrust  =~ TRUST_SYS_1     + TRUST_SYS_2     + TRUST_SYS_3
"

fit.trust <- cfa(
  model         = trust_model,
  data          = df_stud,
  std.lv        = TRUE,
  meanstructure = TRUE,
  missing       = "fiml"
)

summary(fit.trust, fit.measures=TRUE, standardized=TRUE)
fitMeasures(fit.trust, c("rmsea","cfi","tli","srmr"))

##  CFI  = 1.00                 # excellent incremental fit
#   TLI  = 1.00                 # excellent incremental fit
#   RMSEA = 0.008 (90% CI [.000–.096]), p(RMSEA ≤ .05) = .675    #  absolute fit
#   SRMR  = 0.029               # residuals well below .08 threshold

# Standardized loadings (all p < .001):
# Institutional trust items load .59–.76  
# System trust items load .62–.88  
# Thus, each item strongly represents its factor.

# Latent correlation:
# InstTrust ↔ SysTrust = 0.55  
# (≈30% shared variance; factors distinct but related)
# This two‐factor structure fits the student data well.




# D. Intention to use CFA

intent_model <- "
  Intent =~ INTENTION_1 + INTENTION_2 + INTENTION_3
"

fit.intent <- cfa(
  model         = intent_model,
  data          = df_stud,
  std.lv        = TRUE,
  meanstructure = TRUE,
  missing       = "fiml"
)

summary(fit.intent, fit.measures=TRUE, standardized=TRUE)
fitMeasures(fit.intent, c("rmsea","cfi","tli","srmr"))

#CFI  = 1.00, TLI  = 1.00, representing maximal incremental fit
#RMSEA = 0.00 (no degrees of freedom), so zero residual error
#SRMR  = 0.00: no residual correlations

# Standardized loadings (all p < .001):
# INTENTION_1 → 0.86  
# INTENTION_2 → 0.72  
# INTENTION_3 → 0.91  
# All very high, showing each item strongly represents the single “Intention” factor

# Variances explained:
# INTENTION_1: 0.86² ≈ 0.74 (74% of variance)
# INTENTION_2: 0.72² ≈ 0.52 (52% of variance)
# INTENTION_3: 0.91² ≈ 0.83 (83% of variance)

# The three‐item intention scale is unidimensional and highly coherent in the student subsample,
#thus, using the simple mean of these items is fully justified.



# -------------- 9. EFA for technostress ---------------------------

# 1. pull out the 10 technostress columns
tech_items <- df_stud %>% select(starts_with("TECH_STRESS_"))

# 2. 3-factor EFA, orthogonal (varimax rotation)
efa3_var <- factanal(tech_items, factors = 3, rotation = "varimax")
print(efa3_var$loadings, cutoff = 0.30)

#Three factors (F1 27%, F2 14% and F3 10%) emerge but Item 1 only loads .337 on Factor 2, suggesting it doesn’t fit neatly into any one dimension.

# 3. 3-factor EFA, oblique (promax rotation)
efa3_obl <- factanal(tech_items, factors = 3, rotation = "promax")
print(efa3_obl$loadings, cutoff = 0.30)

#Three correlated factors together explain about 49% of the variance (23.6% + 14.9% + 10.7%):
# Factor 1 loads ≥.64 on TS_2, 0.73 on TS_6, 0.87 on TS_7, and 0.65 TS_9
#Factor 2 loads ≥.37 on TS_1, 0.32 on TS_5, 0.74 on TS_8, and 0.79 on TS_10
# Factor 3 – Invasion loads ≥.63 on TS_3 and 0.78 on TS_4.
# TS_1  and TS_5 only load modestly (~.32–.37) on Factor 2, suggesting they’re weaker indicators of the complexity/confidence dimension.

# 4. 2-factor EFA, oblique (promax rotation)
efa2_obl <- factanal(tech_items, factors = 2, rotation = "promax")
print(efa2_obl$loadings, cutoff = 0.30)

# F1 explains 25% of the total variance and loads on mostly technostress 2, 6, 7 and 9 whereas Factor 2 explains 15% of the variance for items 1, 5, 8 and 10.
# Items 3 and 4 are not explained by either of the two factors.
# This confirms that there are possibly three factors for technostress rather than 5.

#### Table of technostress items loadings  ####

# chosen model: 3-factor EFA (varimax rotation)
efa3_var <- factanal(tech_items, factors = 3, rotation = "varimax")

# Extract and round the loadings:
L <- round(efa3_var$loadings[, 1:3], 2)
colnames(L) <- paste0("F", 1:3)                # name the factors F1, F2, F3
rownames(L) <- rownames(efa3_var$loadings)         # TECH_STRESS_1 … TECH_STRESS_10

# 3. Compute variance‐explained stats
p        <- nrow(efa3_var$loadings)                     # number of items
ss_load  <- round(colSums(efa3_var$loadings[,1:3]^2), 3) # sum of squared loadings
prop_var <- round(ss_load / p, 3)                       # proportion of total var
cum_var  <- round(cumsum(prop_var), 3)                  # cumulative prop

# 4. Append them as extra rows
L2 <- rbind(
  L,
  "SS loadings"   = ss_load,
  "Prop. Var"     = prop_var,
  "Cum. Var"      = cum_var
)

# 5. Print with kable
library(kableExtra)
kable(
  L2,
  caption   = "Table X. 3-Factor Varimax Loadings for Technostress (cutoff |.30|) + Variance Explained",
  booktabs  = TRUE,
  align     = c("l","c","c","c"),
  row.names = TRUE
) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed")) %>%
  column_spec(1, bold = TRUE)



# --------- 10. PCA technostress (for descriptive purposes and confirm the number of factors) ----------------

# Technostress PCA
# extract as many components as there are items (10)
pca(tech_items, nfactors = ncol(tech_items), rotate="none") 

# PC1 xplains 3.94 units of variance (39% of total)
# PC2 explains 1.49 (15%), PC3 explains 1.17 (12%), PC4 explains 0.80 (8%)...
# By PC2, the model already captures 54% of the total variance; by PC3, 66%, etc.
# PC1 has strong positive loadings (≥ .64) on items 5–10 (especially TECH_STRESS_7 = .86), indicating a large, common technostress factor.
# PC2 is driven by items 1, 3, and 4 (loadings ≈ .49–.62), suggesting a secondary dimension (perhaps “overload/uncertainty”).
# Loadings on PC3–PC5 are all smaller (|loading| < .56) and less consistent, confirming that most structure lives in the first two components.
# Mean complexity ≈ 3.3 shows that items tend to spread their loadings across multiple components rather than cleanly on one—again pointing to a strongly general factor plus a smaller second factor.

# get eigenvalues
pca_tech <- pca(tech_items, nfactors = ncol(tech_items), rotate="none")
pca_tech$values

# Eigenvalues (Kaiser rule):
# PC1 = 3.94, PC2 = 1.49, PC3 = 1.17 (all > 1), suggesting up to three components could be retained by the eigenvalue >1 rule.

# scree plot
scree(tech_items, pc=TRUE, fa=FALSE)
# The plot shows a clear “elbow” after the second/third component, indicating that a two/three-factor solution may be most acceptable.


# ----------- 11. PCA with 2/3 factors for Technostress ----------------

# choose 2 components (per parallel analysis)
pca_tech2 <- pca(tech_items, nfactors = 2, rotate="none")
print(pca_tech2$loadings, digits = 2)

# PC1 explains 39% of total variance and appears to capture the complexity, uncertainty and insecurity aspects of technostress.
# whereas it does not really explain the item 1,3 and 4.
# PC2 adds another 15% of explained variance and mostly explains the overload and invasion aspects of technostress.

# Inspect the first few component scores
head(pca_tech2$scores)

#The first component (PC1) scores range roughly from –1.29 to +1.08; positive values indicate respondents with higher overall technostress, negative values lower.
#The second component (PC2) scores, smaller in magnitude (≈ –0.71 to +1.47), capture a secondary pattern.

# FactoMineR PCA (for plotting & eigenvalues)
fm_tech <- PCA(tech_items, graph = FALSE)

# Summary & eigenvalues
summary(fm_tech)
print(fm_tech$eig)

# A two-component solution captures over half the variance,
# However, a third dimension explain nearly 12 %, confirming a strong two-factor structure, but also a possibble third factor.

# Biplot of variable vectors
fviz_pca_var(fm_tech, repel = TRUE, 
             title = "Technostress items – PCA variable plot")

# Also this plot shows that there appear to be two primary dimensions in the technostress scale. 

# Try with 3 factors and rotation:

# Three‐factor PCA with varimax rotation
pca_tech_var <- pca(tech_items, nfactors = 3, rotate = "varimax", 
                    scores = FALSE, covar = FALSE, missing = TRUE)

# Rotated loadings (cutoff |0.30|)
print(pca_tech_var$loadings, digits = 2, cutoff = 0.3)


# Correlation‐style plot of the rotated loadings
corrplot(pca_tech_var$loadings, is.corr = TRUE, 
         tl.col = "black", 
         title = "Varimax-rotated loadings", mar = c(0,0,1,0))

#Interpretation: 
#RC1 (33% of variance) loads very strongly on  
# TECH_STRESS_2 (Overload)  
# TECH_STRESS_6 (Complexity)  
# TECH_STRESS_7 (Insecurity)  
# TECH_STRESS_9 (Uncertainty)  
# and moderately on TS5 (Complexity)
# This factor captures a more general anxiety and lack of confidence in respondents when AI adds demands.

# RC2 (18% of variance) loads strongly on  
# TECH_STRESS_1 (Overload – positive framing)  
# TECH_STRESS_8 (Insecurity – positive framing)  
# TECH_STRESS_10 (Uncertainty – positive framing)  
# (and moderately on TS5)  
# This factor reflects "ease" or "self-efficacy": how smoothly users feel they can keep up.

# RC3 (15% of variance) loads solely on  
# TECH_STRESS_3  
# TECH_STRESS_4  
# This cleanly represents “techno-invasion,” i.e. intrusion into personal life.

# Although the theory posits five subscales, both EFA and PCA point to three robust constructs:
#   1. General stress and complexity  (items 2, 5, 6, 7, 9)
#   2. Ease/Self-Efficacy   (items 1, 8, 10)
#   3. Invasion             (items 3, 4)


#CFA with three factors instead of five
tech2_model <- "
  GeneralStress =~ TECH_STRESS_2 + TECH_STRESS_5 + TECH_STRESS_6 + TECH_STRESS_7 + TECH_STRESS_9
  Difficulty    =~ TECH_STRESS_1 + TECH_STRESS_8 + TECH_STRESS_10
  Invasion      =~ TECH_STRESS_3 + TECH_STRESS_4 
"  
fit.tech <- cfa(
  model         = tech2_model,
  data          = df_stud,
  std.lv        = TRUE,
  meanstructure = TRUE,
  missing       = "fiml"
)

summary(fit.tech, fit.measures=TRUE, standardized=TRUE)
fitMeasures(fit.tech, c("rmsea","cfi","tli","srmr"))


# RMSEA is below 0.08 which is considered good
# CFI and TLI are above 0.90 which is considered also good
# SRMR is below 0.08 which is considered acceptable. 
# Therefore, three factors instead of five seems to explain technostress.
# However, for practical reasons and for the number of participants gathered, I decided to calculate still the mean of all items in one composite score, given that the Overall cronbach's α (~ 0.8) supports adequate unidimensional reliability.
# Still, in the discussion I will report how the analysis of the CFA and EFA revealed three underlining factors rather than 5.


#----------- 12. EFA and PCA for resistance to change --------------

# 1. Resistance to change unrotated PCA

# extract as many components as there are items (8)
pca(res_items, nfactors = ncol(res_items), rotate="none") 

#General Factor (PC1): 32% of total variance
# All 8 items load positively (0.35–0.70)
# This tells us there is one strong “resistance” factor driving the scale.
# However, the secondary dimension explains 21% of variance and items 1-4 load negatively while items 5-8 load positively.
# PC2 appears to split the four theory-driven sub-dimensions into two clusters.
# PC3 explains only another 14% of variance.

# Get eigenvalues
pca_res <- pca(res_items, nfactors = ncol(res_items), rotate="none")
pca_res$values
# Three components have eigenvalues >1 (2.74, 1.43, 1.10).

# Scree plot
scree(res_items, pc=TRUE, fa=FALSE)
#  the curve flattens after the second or third point, but the biggest drop is from 1→2 and 2→3.

# 2. EFA: 2 factors, varimax
efa2_var <- factanal(res_items, factors = 2, rotation = "varimax")
print(efa2_var$loadings, cutoff = 0.30)

#Factor 1, which explains 21% of the variance, represents items 1 to 4, so routine seeking and emotional reaction.
# Factor 2 (17% of variance) instead represents items 5 to 8 (short-term focus and cognitive rigidity).

# EFA: 2 factors, promax (oblique)
efa2_obl <- factanal(res_items, factors = 2, rotation = "promax")
print(efa2_obl$loadings, cutoff = 0.30)

# here it's exactly the same as the varimax rotation, with F1 describing items 1 to 4 and F2 describing items 5 to 8.
# Item 4 loading is weaker compared to varimax, but most items increase their loadings.
# Still together they explain only 38% of the variance.

# EFA: 3 factors, promax
efa3_obl <- factanal(res_items, factors = 3, rotation = "promax")
print(efa3_obl$loadings, cutoff = 0.30)

# With three factors and promax rotation, F1  describes items 2 and 3 and explains 17% of the total variance.
# Factor 2 now explains 17% of variance and specifically items 5 to 8.
# and Factor 3 adds 13% of variance and explains items 1 and 4.

# 2‐PC solution, varimax
pca2_var <- principal(res_items, nfactors = 2, rotate = "varimax")
print(pca2_var$loadings, cutoff = 0.30)
corrplot(pca2_var$loadings, is.corr=TRUE,
         title="Resistance PCA (2 PCs, varimax)", mar=c(0,0,1,0))

pca3_var <- principal(res_items, nfactors = 3, rotate = "varimax")
print(pca3_var$loadings, cutoff = 0.30)
corrplot(pca3_var$loadings, is.corr=TRUE,
         title="Resistance PCA (3 PCs, varimax)", mar=c(0,0,1,0))

#Although a three-factor solution is technically defensible (all three eigenvalues >1), the third factor is weaker and doesn’t map neatly onto the theoretical sub-dimensions.
# Thus, two factors results in a cleaner, more stable structure (explaining 53% of variance) and easier to conceptualise.

#Overall, while PCA without rotation suggested one strong component which underlies all eight items, 
# EFA with two factors suggests that there exists two factors that describe respectively item 1 to 4 and item 5 to 8.
# EFA even suggested that a third factor could be taken into consideration particularly regarding items 1 and 4.
# There are no clear four separate dimensions.
# For the next analysis, I will proceed with one single composite mean score for all eight items, for practical reasons (number of respondents and cronbach alpha very slightly below 0.70) but keeping in mind that there might be two underlying factors that can explain separately items 1-4 and items 5-8.

#### Table Factor Promax Loadings ####

# 1. run 2-factor EFA with varimax
efa2_obl <- factanal(res_items, factors = 2, rotation = "promax")

# 2. extract & round the loadings
Lres <- round(efa2_obl$loadings[, 1:2], 2)
colnames(Lres) <- paste0("F", 1:2)
rownames(Lres) <- rownames(efa2_obl$loadings)

# 3. blank out anything under the |.30| cutoff
Lres[ abs(Lres) < 0.30 ] <- ""

# 4. compute variance‐explained stats
p_res    <- nrow(efa2_obl$loadings)
ss_res   <- round(colSums(efa2_obl$loadings[,1:2]^2), 3)
prop_res <- round(ss_res / p_res, 3)
cum_res  <- round(cumsum(prop_res), 3)

# 5. append them as extra rows
Lres2 <- rbind(
  Lres,
  "SS loadings" = ss_res,
  "Prop. Var"   = prop_res,
  "Cum. Var"    = cum_res
)

# 6. print with kable
kable(
  Lres2,
  caption   = "Table X. 2-Factor Promax Loadings for Resistance to Change (cutoff |.30|) + Variance Explained",
  booktabs  = TRUE,
  align     = c("l", rep("c", ncol(Lres2))),
  row.names = TRUE
) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped","condensed")) %>%
  column_spec(1, bold = TRUE)


# --------------- 13. Multiple regression model ----------------

# 1) Start from cleaned df2_stud
df3_stud <- df2_stud %>% 
  
  # 2) Add simple binary controls
  mutate(
    # 1) Gender: reference = Female 
    Gender_bin = factor(
      ifelse(Gender == "Female", "Female", "Other"),
      levels = c("Female", "Other")
    ),
    
    # 2) Education: reference = BelowBachelor 
    Educ_bin = factor(
      ifelse(Education %in% c("Bachelor’s","Master’s","Doctorate+"),
             "BachelorPlus",
             "BelowBachelor"),
      levels = c("BelowBachelor","BachelorPlus")
    ),
    
    # 3) Work: reference = No 
    Work_bin = factor(
      ifelse(Working == "No", "No", "Yes"),
      levels = c("No","Yes")
    ),
    
    # 4) Use intensity: reference = Infrequent (never/≤1×yr), vs Frequent (≥monthly)
    UseFreq = factor(
      ifelse(UseIntensity <= 2, "Infrequent", "Frequent"),
      levels = c("Infrequent","Frequent")
    ),
    
    # 5) Migration 
    MigrationGen = case_when(
      MigrationBG %in% c("OneParent MIGR", "TwoParents MIGR") ~ "Second-gen migrant",
      MigrationBG == "Self MIGR"                            ~ "First-gen migrant",
      MigrationBG == "Native"                               ~ "Native-born",
      TRUE                                                  ~ "Other"
    ),
    MigrationGen = factor(
      MigrationGen,
      levels = c("Native-born", "Second-gen migrant", "First-gen migrant", "Other")
    ),
  
    # 6) Province → keep the two biggest vs Other
    Province2 = factor(
      ifelse(Province == "Noord-Holland", "Noord-Holland", "OtherProv"),
      levels = c("Noord-Holland", "OtherProv")
    )
  )

# 3) Inspect that the new *_bin columns look right:
table(df3_stud$Gender_bin)
table(df3_stud$Educ_bin)
table(df3_stud$MigrationGen)
table(df3_stud$Province2)
table(df3_stud$Work_bin)
table(df3_stud$UseFreq)


model_data_full <- df3_stud %>%
  select(
    Intention, 
    TechStress, Resistance, Trust_Total,             # IVs
    Age, Gender_bin, Educ_bin, Province2,            # controls
    Work_bin, MigrationGen, UseFreq, ContextBreadth                 # more controls
  ) %>%
  na.omit()

# check
nrow(model_data_full)
str(model_data_full)

# ------- Models ------
library(modelsummary)
library(car)
library(data.table)

# 1) Baseline model (controls only)
mod1 <- lm(
  Intention ~ 
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

# 2) Add main effects of interest (TechStress, Resistance, Trust)
mod2 <- lm(
  Intention ~ 
    TechStress + Resistance + Trust_Total +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

# 3a) Interaction technostress × trust
mod3_TSxTrust <- lm(
  Intention ~ 
    TechStress * Trust_Total + Resistance +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

# 3b) Interaction resistance × trust
mod3_RESxTrust <- lm(
  Intention ~ 
    Resistance * Trust_Total + TechStress +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

# 4) Full model with both interactions
mod4 <- lm(
  Intention ~ 
    TechStress * Trust_Total + Resistance * Trust_Total +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

# Quick check on sample size after listwise deletion
cat("Observations used in models:", nobs(mod1), "\n")

# 5) Tabulate all four models side by side
model_list <- list(
  Baseline      = mod1,
  MainEffects   = mod2,
  TS_x_Trust    = mod3_TSxTrust,
  RES_x_Trust   = mod3_RESxTrust,
  FullInteraction = mod4
)

modelsummary(
  model_list,
  stars = TRUE,
  gof_omit = "Adj\\.R\\^2|AIC|BIC",    # show only what you need
  coef_map = c(
    "TechStress"               = "Technostress",
    "Resistance"               = "Resistance",
    "Trust_Total"              = "Trust",
    "TechStress:Trust_Total"   = "TS × Trust",
    "Resistance:Trust_Total"   = "RES × Trust",
    "Age"                      = "Age",
    "Gender_binOther"          = "Gender = Other",
    "Educ_binBachelorPlus"     = "Education: Bachelor+",
    "Province2Zuid-Holland"    = "Province = ZH",
    "Province2Noord-Holland"   = "Province = NH",
    "Work_binYes"              = "Working = Yes",
    "MigrationGenSecond-gen migrant" = "2nd-gen migrant",
    "MigrationGenFirst-gen migrant"  = "1st-gen migrant",
    "UseFreqFrequent"          = "Use = Frequent",
    "ContextBreadth"           = "Context breadth"
  ),
  title = "Models Predicting Intention to Use AI Services"
)

#### Compute Models separately #####
# ---- 1) Model 1: Controls Only ----

# main controls variables only
mod1 <- lm(
  Intention ~ 
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + 
    MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

cat("### Model 1 (Controls only):\n")
print(summary(mod1))


# ---- 2) Model 2: + Main IVs (TechStress, Resistance, Trust) ----

m2_full <- lm(
  Intention ~ TechStress + Resistance + Trust_Total +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + 
    MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

cat("\n### Model 2 (+ main IVs):\n")
print(summary(m2_full))


# ---- 3) Model 3: + Interaction Terms ----

model_data_full <- model_data_full %>%
  mutate(
    TechStress_c = TechStress  - mean(TechStress),
    Resistance_c = Resistance  - mean(Resistance),
    Trust_c      = Trust_Total - mean(Trust_Total)
  )

# 3a) Interaction technostress × trust
mod3_TSxTrust <- lm(
  Intention ~ 
    TechStress * Trust_Total + Resistance +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

cat("\n### Model 3 (Technostress * Trust):\n")
print(summary(mod3_TSxTrust))


# 3b) Interaction resistance × trust
mod3_RESxTrust <- lm(
  Intention ~ 
    Resistance * Trust_Total + TechStress +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

cat("\n### Model 4 (Resistance * Trust):\n")
print(summary(mod3_RESxTrust))


# ------- 4) Full model with both interactions ---------
mod4 <- lm(
  Intention ~ 
    TechStress * Trust_Total + Resistance * Trust_Total +
    Age + Gender_bin + Educ_bin + Province2 + Work_bin + MigrationGen + UseFreq + ContextBreadth,
  data = model_data_full
)

cat("\n### Model 4 (Full model with both interaction):\n")
print(summary(mod4))


# ---- Compare Models ----


# List of models
models <- list(m1 = mod1, m2 = m2_full,
               m3a = mod3_TSxTrust, m3b = mod3_RESxTrust, m4 = mod4)

# Apply glance to each and bind results into a single dataframe
compare <- map_df(models, glance, .id = "model") %>%
  select(model, adj.r.squared, AIC)

compare

anova(mod1, m2_full,mod3_TSxTrust,mod3_RESxTrust, mod4)

#Model 1 (controls only) explains almost none of the variance in Intention (adj. R² = -0.0026). The AIC is 410, indicating poor fit.
#Model 2 (adds main predictors: Technostress, Resistance, Trust) improves the model:
# Adjusted R² increases to 0.314 — now explaining 31.4% of the variance.
#AIC drops to 358, showing better fit (lower AIC = better).
# Model 3 (adds interaction terms) shows further improvement:
#Adjusted R² increases slightly to 0.333 (explains 33.3% of the variance).
# AIC drops slightly to 356, showing marginally better fit than Model 2.

# Model 2 significantly improves over Model 1 (p < .001), showing that adding technostress, resistance, and trust contributes meaningfully to predicting intention to use AI municipal services.
# Model 3 improves slightly over Model 2, but the interaction terms (TechStress × Trust, Resistance × Trust) are only marginally significant (p = .074). This suggests a possible moderating effect, but it's not strong enough to be conclusive.




#---------Model with separate items for technostress and resistance to change --------------

model_data_full <- df_stud %>%
  select(
    Intention, Trust_Total,
    Age, Gender, Education, MigrationBG, Province,
    Studying, Working, UseIntensity, ContextBreadth,
    # bring in the raw items you need for subdimensions:
    starts_with("TECH_STRESS_"),
    starts_with("RES_CHANGE_")
  ) %>%
  na.omit()

# Compute the subdimensions:
model_data_full <- model_data_full %>%
  mutate(
    GeneralStress  = rowMeans(select(., TECH_STRESS_2, TECH_STRESS_6, TECH_STRESS_7, TECH_STRESS_9), na.rm = TRUE),
    Difficulty     = rowMeans(select(., TECH_STRESS_1, TECH_STRESS_5, TECH_STRESS_8, TECH_STRESS_10), na.rm = TRUE),
    Invasion       = rowMeans(select(., TECH_STRESS_3, TECH_STRESS_4), na.rm = TRUE),
    
    RoutineEmotion = rowMeans(select(., RES_CHANGE_1, RES_CHANGE_2, RES_CHANGE_3, RES_CHANGE_4), na.rm = TRUE),
    FocusRigidity  = rowMeans(select(., RES_CHANGE_5, RES_CHANGE_6, RES_CHANGE_7, RES_CHANGE_8), na.rm = TRUE)
  )

# Center all subdimensions + trust
model_data_full <- model_data_full %>%
  mutate(across(
    c(GeneralStress, Difficulty, Invasion, RoutineEmotion, FocusRigidity, Trust_Total),
    ~ . - mean(., na.rm = TRUE),
    .names = "{.col}_c"
  ))

# Run regression
model_sub <- lm(
  Intention ~ GeneralStress_c + Difficulty_c + Invasion_c +
    RoutineEmotion_c + FocusRigidity_c +
    Trust_Total_c +
    Age + Gender + Education + Province,
  data = model_data_full
)

summary(model_sub)


model_interact <- lm(
  Intention ~ (GeneralStress_c + Difficulty_c + Invasion_c +
                 RoutineEmotion_c + FocusRigidity_c) * Trust_Total_c +
    Age + Gender + Education + MigrationBG + Province + Working + UseIntensity,
  data = model_data_full
)

summary(model_interact)

#-------- Final regression model  -------

# 1. Start from df2_stud and recode collapsed controls _before_ any NA‐omission
df3_stud <- df2_stud %>%
  
  # — Collapse sparse “Other” levels into substantive groups —
  mutate(
    MigrationGen = case_when(
      MigrationBG %in% c("OneParent MIGR", "TwoParents MIGR") ~ "Second-gen",
      MigrationBG == "Self MIGR"                              ~ "First-gen",
      MigrationBG == "Native"                                 ~ "Native-born",
      TRUE                                                    ~ "Second-gen"
    ),
    Province2 = ifelse(Province == "Noord-Holland", "NH", "OtherProv"),
    Gender_bin = ifelse(Gender == "Female", "Female", "Other"),
    Educ_bin   = ifelse(Education %in% c("Bachelor’s","Master’s","Doctorate+"),
                        "BachelorPlus","BelowBachelor"),
    Work_bin   = ifelse(Working == "No","No","Yes"),
    UseFreq    = ifelse(UseIntensity <= 2, "Infrequent","Frequent")
  ) %>%
  # turn into factors with clear levels
  mutate_at(vars(MigrationGen, Province2, Gender_bin, Educ_bin, Work_bin, UseFreq),
            ~ factor(.x)) %>%
  
  # — compute raw Intention & IVs —
  select(
    Intention, TechStress, Resistance, Trust_Total,
    Age, ContextBreadth,
    MigrationGen, Province2, Gender_bin, Educ_bin, Work_bin, UseFreq
  ) %>%
  
  # drop any remaining NAs (should be minimal)
  na.omit() %>%
  
  # 2. Center all continuous predictors _once_
  mutate(
    TechStress_c  = TechStress  - mean(TechStress),
    Resistance_c  = Resistance  - mean(Resistance),
    Trust_c       = Trust_Total - mean(Trust_Total),
    Age_c         = Age         - mean(Age),
    Context_c     = ContextBreadth - mean(ContextBreadth)
  )

# Quick check of sample size
cat("N used in all models:", nrow(df3_stud), "\n")


# 3. Fit the five models
mod1 <- lm( Intention ~ Age_c + Context_c +
              MigrationGen + Province2 + Gender_bin + Educ_bin + Work_bin + UseFreq,
            data = df3_stud)

mod2 <- lm( Intention ~ TechStress_c + Resistance_c + Trust_c +
              Age_c + Context_c +
              MigrationGen + Province2 + Gender_bin + Educ_bin + Work_bin + UseFreq,
            data = df3_stud)

mod3_TSxT <- lm( Intention ~ TechStress_c * Trust_c + Resistance_c +
                   Age_c + Context_c +
                   MigrationGen + Province2 + Gender_bin + Educ_bin + Work_bin + UseFreq,
                 data = df3_stud)

mod3_RESxT <- lm( Intention ~ Resistance_c * Trust_c + TechStress_c +
                    Age_c + Context_c +
                    MigrationGen + Province2 + Gender_bin + Educ_bin + Work_bin + UseFreq,
                  data = df3_stud)

mod4 <- lm( Intention ~ TechStress_c * Trust_c + Resistance_c * Trust_c +
              Age_c + Context_c +
              MigrationGen + Province2 + Gender_bin + Educ_bin + Work_bin + UseFreq,
            data = df3_stud)


# 4. Check multicollinearity on the biggest model
print(vif(mod4))


# 5. Present them side by side
model_list <- list(
  Baseline      = mod1,
  MainEffects   = mod2,
  TS_x_Trust    = mod3_TSxT,
  RES_x_Trust   = mod3_RESxT,
  Full          = mod4
)

modelsummary(
  model_list,
  stars     = TRUE,
  coef_map  = c(
    "TechStress_c"             = "Technostress",
    "Resistance_c"             = "Resistance",
    "Trust_c"                  = "Trust",
    "TechStress_c:Trust_c"     = "TS × Trust",
    "Resistance_c:Trust_c"     = "RES × Trust",
    "Age_c"                    = "Age (centered)",
    "Context_c"                = " AI Context ",
    "Gender_binOther"          = "Gender = Other",
    "Educ_binBachelorPlus"     = "Educ = Bachelor+",
    "Province2OtherProv"       = "Province = Other",
    "MigrationGenSecond-gen"   = "2nd-gen migrant",
    "MigrationGenFirst-gen"    = "1st-gen migrant",
    "Work_binYes"              = "Working = Yes",
    "UseFreqFrequent"          = "Use ≥ monthly"
  ),
  gof_omit  = "AIC|BIC|Log.Lik|F|RMSE"
)



# ---- Diagnostics on the full model (mod4) ----

# 1. Multicollinearity
vif(mod4, type="predictor")
# All VIF values are between about 1.04 and 1.23, which is well below common “problem” thresholds, usually VIF > 2 or 3). 
# Technostress (TechStress_c) has the highest predictor‐level GVIF^(1/(2·Df)) at ~1.16 (interacting with Trust_c),
# Resistance_c comes in at ~1.15, Trust_c at ~1.04, and all of the controls (Age_c, Context_c, MigrationGen, Province2, Gender_bin, Educ_bin, Work_bin, UseFreq) are in the 1.08–1.23 range.
# Since none of these adjusted VIFs exceed about 2,there is not a serious multicollinearity problem. 
# All predictors, including the interaction terms, are sufficiently independent of one another.

# 2. Residuals vs Fitted
plot(mod4, which = 1)
# The points are more-or-less symmetrically scattered about the zero‐line, with no strong U– or inverted U–shape. That suggests no gross non-linearity in the model.
# Moreover, there’s no obvious funneling, so the homoscedasticity assumption is reasonably met.
# Lastly, there are a couple of points labeled (cases 33 and 102) lying farther from the rest. Those may be potential outliers, but they don’t appear to dominate the overall pattern.

# 3. Normal Q–Q
plot(mod4, which = 2)
# Almost all points hug the 45° reference line closely, especially through the middle 80 percent of the distribution.
# The most extreme residuals at both ends (cases 80 and 102 at the lower end, case 33 at the upper end) deviate slightly from the line, indicating mild heavy-tail behavior, but nothing too severe.
# The residuals are approximately normally distributed, with only minor departures in the extreme tails.

# 4. Breusch‐Pagan (heteroscedasticity test)
library(lmtest)
bptest(mod4)
# χ²(14) = 16.15 with p = 0.30. 
# Since p ≫ 0.05, we fail to reject the null of constant variance. This means that here’s no statistical evidence of heteroscedasticity in the full model.
# Residuals appear to have roughly equal variance across fitted values.

# 5. Shapiro–Wilk (normality of residuals)
shapiro.test(residuals(mod4))
# p = 0.036. Since it is < .05, this means that residuals tend to slightly deviate from normality distribution.
# The Q-Q plot already showed that a couple of points (e.g. cases 80 and 102) pull away at the extremes. Those are the ones driving the Shapiro p-value below .05.
# A potential remedy could be to investigate such cases to detect if they did have extreme responses. 

# 6. Cook’s distance (influential points)
library(olsrr)
ols_plot_cooksd_chart(mod4)
#Labeled points (25, 27, 72, 80, 89, 93, 96, 97, 102) are the cases whose Cook’s D exceeds that threshold. In particular, observation 80 (Cook’s D ≈ 0.10) and 102 (≈ 0.08) are the most influential.

# 7. 10-fold cross-validation (predictive performance)
library(caret)
set.seed(123)
cv_ctrl <- trainControl(method = "cv", number = 10)
cv_mod4 <- train(
  Intention ~ TechStress_c*Trust_c + Resistance_c*Trust_c +
    Age_c + Context_c +
    MigrationGen + Province2 + Gender_bin + Educ_bin + Work_bin + UseFreq,
  data     = df3_stud,
  method   = "lm",
  trControl = cv_ctrl
)
print(cv_mod4)
# On average, the model's prediction error is about 0.73 points on the intention scale
# On new (held-out) folds, the model explains about 35.5 % of the variance in Intention.
# The average absolute difference between predicted and actual Intention is about 0.57 points.
# 10-fold CV, then, tells us that the full model generalises moderately: it explains ~35 % of out-of-sample variance, with an average prediction error under 1 point on the 1–5 Intention scale.




