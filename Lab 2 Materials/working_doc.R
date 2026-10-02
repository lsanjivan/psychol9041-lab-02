library(psych)
library(tidyverse)
library(gt)
library(janitor)

data <- read_csv("Attachment_Anxiety_Data.csv")

data <- data |>
  mutate(Gender = case_when(
    Gender == 1 ~ "Male",
    Gender == 2 ~ "Female",
    TRUE        ~ NA_character_
  )) |>
  mutate(Relationship = case_when(
    Relationship == 1 ~ "Married",
    Relationship == 2 ~ "In a relationship",
    Relationship == 4 ~ "Divorced",
    Relationship == 5 ~ "In a relationship",
    Relationship == 6 ~ "Never been in a relationship",
    Relationship == 7 ~ "Prefer not to say",
    TRUE              ~ NA_character_
  )) |>
  mutate(across(SA_1:SEst_10, as.integer))

SIAS_key <- list(SocialAnxiety = c(
  "SA_1", "SA_2", "SA_3", "SA_4", "-SA_5",
  "SA_6", "SA_7", "SA_8", "-SA_9", "SA_10",
  "-SA_11", "SA_12", "SA_13", "SA_14", "SA_15",
  "SA_16", "SA_17", "SA_18", "SA_19", "SA_20"
))

ECR_key <- list(AttachmentAnxiety = c(
  "AA_1", "AA_2", "AA_3", "AA_4", "AA_5",
  "AA_6", "AA_7", "AA_8", "AA_9"
))

RSES_key <- list(SelfEsteem = c(
  "SEst_1", "-SEst_2", "SEst_3", "SEst_4", "-SEst_5",
  "-SEst_6", "SEst_7", "-SEst_8", "-SEst_9", "SEst_10"
))
  
SIAS_scores <- scoreItems(SIAS_key, data, totals = F, min = 1, max = 5)
ECR_scores <- scoreItems(ECR_key, data, totals = F, min = 1, max = 7)
RSES_scores <- scoreItems(RSES_key, data, totals = F, min = 1, max = 4)

SIAS_scaled <- as.data.frame(SIAS_scores$scores)
ECR_scaled <- as.data.frame(ECR_scores$scores)
RSES_scaled <- as.data.frame(RSES_scores$scores)

allscores_df <- cbind(SIAS_scaled, ECR_scaled, RSES_scaled)

merged_df <- cbind(data, allscores_df)

final_df <- merged_df |>
  select(URN, Gender, Relationship, SocialAnxiety, AttachmentAnxiety, SelfEsteem)
n <- nrow(final_df)


demographic_table <- final_df |> 
  tabyl(Gender, Relationship)

gttable_1 <- demographic_table |>
  gt() |>
  tab_header(
    title = md("*Distribution of Males and Females by Relationship Status*")
  )

summary <- describe(select(final_df, SocialAnxiety:SelfEsteem))

summary$Scales <- c("Social Interaction Anxiety", "Attachment Anxiety", "Self-Esteem")

alphas <- round(c(
  SIAS_scores$alpha,
  ECR_scores$alpha,
  RSES_scores$alpha
), 2)

summary <- summary |>
  select(Scales, mean, median, sd, range) |>
  mutate_if(is.numeric, round, 2)

score_table <- cbind(summary, alpha = alphas)

gttable_2 <- gt(score_table) |>
  tab_header(
    title = md("*Descriptive Statistics for the Social Anxiety, Attachment Anxiety and Self-Esteem Scales*")
  ) |>
  tab_spanner(label = "Central Tendency", columns = c(mean, median)) |>
  tab_spanner(label = "Variability", columns = c(sd, range)) |>
  tab_spanner(label = "Reliability (Cronbach's alpha)", columns = c(alpha))


female_df <- merged_df |>
  filter(Gender == "Female")

summary_female <- describe(select(female_df, SocialAnxiety:SelfEsteem))

n_female <- nrow(female_df)

summary_female$Scales <- c("Social Interaction Anxiety", "Attachment Anxiety", "Self-Esteem")

SIAS_scores_female <- scoreItems(SIAS_key, female_df, totals = F, min = 1, max = 5)
ECR_scores_female  <- scoreItems(ECR_key,  female_df, totals = F, min = 1, max = 7)
RSES_scores_female <- scoreItems(RSES_key, female_df, totals = F, min = 1, max = 4)

alphas_female <- round(c(
  SIAS_scores_female$alpha,
  ECR_scores_female$alpha,
  RSES_scores_female$alpha
), 2)

summary_female <- summary_female |>
  select(Scales, mean, median, sd, range) |>
  mutate_if(is.numeric, round, 2)

score_table_female <- cbind(summary_female, alpha = alphas_female)

gttable_3 <- gt(score_table_female) |>
  tab_header(
    title = md("*Descriptive Statistics for the Social Anxiety, Attachment Anxiety and Self-Esteem Scales in Female Participants*")
  ) |>
  tab_spanner(label = "Central Tendency", columns = c(mean, median)) |>
  tab_spanner(label = "Variability", columns = c(sd, range)) |>
  tab_spanner(label = "Reliability (Cronbach's alpha)", columns = c(alpha))
