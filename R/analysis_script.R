library(tidyverse)
library(haven)
library(psych)
library(apaTables)
library(lm.beta)
library(lmtest)
library(performance)
library(car)
library(lavaan)
library(DiagrammeR)
library(DiagrammeRsvg)

# ---- 1. DATA PREPARATION ----

## 1.1 Import data

data_raw <- read_sav("data/raw data - Jamie project.sav")

## 1.2 Copy data to leave raw data unchanged

data_clean <- data_raw

## 1.3 Inspect raw data

dim(data_raw)
names(data_raw)
str(data_raw)
head(data_raw)
summary(data_raw)
view(data_raw)

## 1.4 Clean data

### 1.4.1 Change variable names for consistency and conciseness

data_clean <- data_clean |>
  rename(ECRS_7avd = ECRS_7avoid)

data_clean <- data_clean |>
  rename_with(
    ~ sub("SIAS_straightforward_", "SIAS_", .x),
    starts_with("SIAS_straightforward_")
  )

data_clean <- data_clean |>
  rename_with(
    ~ sub("Revised_UCLALS_", "Rev_UCLALS_", .x),
    starts_with("Revised_UCLALS_")
  )

#### Variable names:

# "Rev_UCLALS_" = Revised UCLA Loneliness Scale 
# (20 items, 1-4 scale, sum total = score)

# "ECRS_" = Experiences in Close Relationships Scale Short Version 
# (12 items, 1-7 scale, "avd" measures attachment avoidance, 
# "anx" measures attachment anxiety, mean = score)

# "SIAS_" = Social Interaction Anxiety Scale Straightforward 
# (17 items, 0-5 scale, no reverse scored items, sum total = score)

# "PHQ2_" = Patient Health Questionnaire 2 
# (2 items, 0-3 scale, measures depression, sum total = score)

# "FFMQ15_" = Five Facet Mindfulness Questionnaire 
# (15 items, 1-5 scale, mean = score)

# Note: number indicates questionnaire item, "R" indicates reverse scored items

### 1.4.2 Change questionnaire items and age data type from character to numeric

data_clean[ , 1:67] <- lapply(data_clean[ , 1:67], as.numeric)

### 1.4.3 Apply exclusion criteria (exclude ages outside of 18-34)
data_clean <- subset(data_clean, Age >= 18 & Age <=34)
dim(data_clean)

#### Participants removed after applying age (18-34) criteria = 2

### 1.5 Create new variables for reverse scored items

#### Identify reverse scored items
names(data_clean)[endsWith(names(data_clean), "R")]

#### UCLALS: 1-4 scale reverse scored = 5 - original
data_clean$Rev_UCLALS_1 <- 5 - data_clean$Rev_UCLALS_1R
data_clean$Rev_UCLALS_4 <- 5 - data_clean$Rev_UCLALS_4R
data_clean$Rev_UCLALS_5 <- 5 - data_clean$Rev_UCLALS_5R
data_clean$Rev_UCLALS_6 <- 5 - data_clean$Rev_UCLALS_6R
data_clean$Rev_UCLALS_9 <- 5 - data_clean$Rev_UCLALS_9R
data_clean$Rev_UCLALS_10 <- 5 - data_clean$Rev_UCLALS_10R
data_clean$Rev_UCLALS_15 <- 5 - data_clean$Rev_UCLALS_15R
data_clean$Rev_UCLALS_16 <- 5 - data_clean$Rev_UCLALS_16R
data_clean$Rev_UCLALS_19 <- 5 - data_clean$Rev_UCLALS_19R
data_clean$Rev_UCLALS_20 <- 5 - data_clean$Rev_UCLALS_20R

#### ECRS: 1-7 scale reverse scored = 8 - original
data_clean$ECRS_1avd <- 8 - data_clean$ECRS_1avdR
data_clean$ECRS_5avd <- 8 - data_clean$ECRS_5avdR
data_clean$ECRS_8anx <- 8 - data_clean$ECRS_8anxR
data_clean$ECRS_9avd <- 8 - data_clean$ECRS_9avdR

#### FFMQ: 1-5 scale reverse scored = 6 - original
data_clean$FFMQ15_3 <- 6 - data_clean$FFMQ15_3R
data_clean$FFMQ15_4 <- 6 - data_clean$FFMQ15_4R
data_clean$FFMQ15_7 <- 6 - data_clean$FFMQ15_7R
data_clean$FFMQ15_8 <- 6 - data_clean$FFMQ15_8R
data_clean$FFMQ15_9 <- 6 - data_clean$FFMQ15_9R
data_clean$FFMQ15_13 <- 6 - data_clean$FFMQ15_13R
data_clean$FFMQ15_14 <- 6 - data_clean$FFMQ15_14R

### 1.6 Calculate questionnaire scores

#### 1.6.1 Loneliness (sum of 20 items)
data_clean$Loneliness <- rowSums(
  data_clean[paste0("Rev_UCLALS_", 1:20)])

#### 1.6.2 Attachment avoidance (mean of 6 items)
data_clean$Attachment_avoidance <- rowMeans(
  data_clean[c("ECRS_1avd", "ECRS_3avd", "ECRS_5avd", 
               "ECRS_7avd", "ECRS_9avd", "ECRS_11avd")]
)

#### 1.6.3 Attachment anxiety (mean of 6 items)
data_clean$Attachment_anxiety <- rowMeans(
  data_clean[c("ECRS_2anx", "ECRS_4anx", "ECRS_6anx", 
               "ECRS_8anx", "ECRS_10anx", "ECRS_12anx")]
)

#### 1.6.4 Social anxiety (sum of 17 items)
data_clean$Social_anxiety <- rowSums(
  data_clean[paste0("SIAS_", 1:17)])

#### 1.6.5 Depression (sum of 2 items)
data_clean$Depression <- rowSums(
  data_clean[c("PHQ2_1", "PHQ2_2")])

#### 1.6.6 Trait mindfulness (mean of 15 items)
data_clean$Trait_mindfulness <- rowMeans(
  data_clean[paste0("FFMQ15_", 1:15)])

### 1.7 Inspect clean data
dim(data_clean)
names(data_clean)
str(data_clean)
head(data_clean)
summary(data_clean)
view(data_clean)

# ---- 2. EXPLORATORY ANALYSIS ----

## 2.1 Scale reliability analysis: cronbach alpha's

### 2.1.1 Revised UCLALS Loneliness Scale
Rev_UCLALS_items <- data_clean[ , c(paste0("Rev_UCLALS_", 1:20))]
alpha(Rev_UCLALS_items)

### 2.1.2 Attachment avoidance
ECRS_avd_items <- data_clean[ , c("ECRS_1avd", "ECRS_3avd", "ECRS_5avd", 
                                  "ECRS_7avd", "ECRS_9avd", "ECRS_11avd")]
alpha(ECRS_avd_items)

### 2.1.3 Attachment anxiety
ECRS_anx_items <- data_clean[ , c("ECRS_2anx", "ECRS_4anx", "ECRS_6anx", 
                                  "ECRS_8anx", "ECRS_10anx", "ECRS_12anx")]
alpha(ECRS_anx_items)

### 2.1.4 Social anxiety
SIAS_items <- data_clean[ , c(paste0("SIAS_", 1:17))]
alpha(SIAS_items)

### 2.1.5 Depression
PHQ2_items <- data_clean[ , c("PHQ2_1", "PHQ2_2")]
alpha(PHQ2_items)

### 2.1.6 Trait mindfulness
FFMQ15_items <- data_clean[ , c(paste0("FFMQ15_", 1:15))]
alpha(FFMQ15_items)

## 2.2 Exploring data visually

scale_scores <- data_clean[
  , c("Loneliness", "Attachment_avoidance", "Attachment_anxiety", 
      "Social_anxiety", "Depression", "Trait_mindfulness")]

### 2.2.1 Distribution of loneliness scores
ggplot(scale_scores, aes(x = Loneliness)) +
  geom_histogram(bins = 20,
                 fill = "steelblue",
                 colour = "white") +
  labs(
    title = "Distribution of loneliness scores",
    x = "Loneliness", 
    y = "Number of participants"
    ) +
  theme_minimal()

### 2.2.2 Distributions of all scores
scale_long <- pivot_longer(
  scale_scores,
  cols = everything(),
  names_to = "Scale",
  values_to = "Score"
)

ggplot(scale_long, aes(x = Score)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    colour = "white"
  ) +
  facet_wrap(~ Scale, scales = "free") +
  labs(x = "Scale score",
       y = "Number of participants") +
  theme_minimal()

### 2.2.3 Visualising relationship between predictors and loneliness

scale_long_predictors <- pivot_longer(
  scale_scores,
  cols = c("Attachment_avoidance", "Attachment_anxiety", "Social_anxiety", 
           "Depression", "Trait_mindfulness"),
  names_to = "Predictor",
  values_to = "Score"
)

ggplot(scale_long_predictors, aes(x = Score, y = Loneliness)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Predictor, scales = "free") +
  labs(x = "Predictor",
       y = "Loneliness") +
  theme_minimal()

ggsave(
  "figures/predictors_loneliness_scatterplots.png",
  width = 8,
  height = 6,
  dpi = 300
)

## 2.3 Descriptive statistics and correlation matrix

### 2.3.1 Descriptive statistics
describe(scale_scores)
descriptives <- describe(scale_scores)[, c("n", "mean", "sd", "median", 
                                           "min", "max", "range")]
descriptives

### 2.3.2 Correlation matrix
correlation_matrix <- corr.test(scale_scores, use = "pairwise.complete.obs")
correlation_matrix

# ---- 3. STATISTICAL ANALYSIS ----

## 3.1 Fit initial hierarchical regression.
## Research question: is trait mindfulness an independent predictor of loneliness
## after accounting for established predictors?

### 3.1.1 List-wise deletion (to ensure same size models for comparison)
data_regression <- na.omit(data_clean)

### 3.1.2 Step 1 (with standardised beta coefficients)
reg_1 <- lm(
  Loneliness ~ Depression + Social_anxiety, 
  data = data_regression)
summary(reg_1)
reg_1_std <- lm.beta(reg_1)
summary(reg_1_std)

### 3.1.3 Step 2 (with standardised beta coefficients)
reg_2 <- lm(
  Loneliness ~ Depression + Social_anxiety + 
    Attachment_anxiety + Attachment_avoidance, 
  data = data_regression)
summary(reg_2)
reg_2_std <- lm.beta(reg_2)
summary(reg_2_std)

### 3.1.4 Comparing regression 1 to regression 2
anova(reg_1, reg_2)

### 3.1.5 Step 3 (with standardised beta coefficients)
reg_3 <- lm(
  Loneliness ~ Depression + Social_anxiety + 
    Attachment_anxiety + Attachment_avoidance + 
    Trait_mindfulness, 
  data = data_regression)
summary(reg_3)
reg_3_std <- lm.beta(reg_3)
summary(reg_3_std)

### 3.1.6 Comparing regression 2 with regression 3
anova(reg_2, reg_3)

## 3.2 Test accuracy of model (sources of bias and regression assumptions)

### 3.2.1 Outliers and influential cases

#### Diagnostic statistics
diagnostics <- data.frame(
  case = 1:nobs(reg_3),
  standardised = rstandard(reg_3),
  studentised = rstudent(reg_3),
  dfbeta = dfbeta(reg_3),
  dffit = dffits(reg_3),
  covariance_ratios = covratio(reg_3),
  cooks_distance = cooks.distance(reg_3),
  leverage = hatvalues(reg_3)
)

#### Identify unusually large studentised residuals (> |2|)
large_residuals <- diagnostics[abs(diagnostics$studentised) > 2, ]
large_residuals

#### Examine influence of unusually large studentised residuals
influence <- influence.measures(reg_3)
large_residuals_cases <- diagnostics |>
  subset(abs(studentised) > 2) |>
  pull(case)
influence$is.inf[large_residuals_cases, ]
influence$infmat[large_residuals_cases, ]

#### Summary: 
#### Of the unusually large standardised residuals, some showed elevated
#### DFFITS and/or covariance ratios. However, none demonstrated substantial 
#### influence on individual regression coefficients or the overall model.
#### ALL observations were therefore retained.

### 3.2.2 Assumption of independence
dwtest(reg_3)

### 3.2.3 Assumption of no multicollinearity
check_collinearity(reg_3)
mean(vif(reg_3))

### 3.2.4 Assumptions about the residuals 
### linearity and homoscedasticiy: residuals vs fitted plot inspection
### normality: Q-Q plot and studentised residuals histogram inspection
plot(reg_3)
hist(diagnostics$studentised)

## 3.4 Mediation model of cross-sectional associations
## Research question: Can attachment anxiety and avoidance statistically account
## for part of the association between trait mindfulness and loneliness?

parallel_model <- '
  # a paths
  Attachment_anxiety ~ a1*Trait_mindfulness
  Attachment_avoidance ~ a2*Trait_mindfulness

  # b paths and direct effect
  Loneliness ~ b1*Attachment_anxiety + b2*Attachment_avoidance + c_prime*Trait_mindfulness

  # Allow residuals of attachment anxiety and avoidance to covary
  Attachment_anxiety ~~ Attachment_avoidance

  # Indirect effects
  indirect_anx := a1*b1
  indirect_avoid := a2*b2

  # Total indirect effect
  indirect_total := indirect_anx + indirect_avoid

  # Total effect
  total := c_prime + indirect_total
'

fit <- sem(
  parallel_model,
  data = data_regression,
  se = "bootstrap",
  bootstrap = 5000
)

summary(
  fit,
  standardized = TRUE,
  ci = TRUE
)

# ---- 4. TABLES AND FIGURES ----

## 4.1 APA Table: Means, standard deviations, and correlations with confidence intervals
apa.cor.table(scale_scores,
              filename = "tables/correlations_descriptives.doc",
              table.number = 1, 
              show.conf.interval = TRUE, 
              show.sig.stars = TRUE
)

## 4.2 APA Table: Hierarchical regression

apa.reg.table(
  reg_1,
  reg_2,
  reg_3,
  filename = "tables/regression.doc"
)

?apa.reg.table

## 4.3 Mediation path diagram

mediation_diagram <- grViz("
digraph mediation {

  graph [layout = neato]

  node [shape = box]

  mindfulness [label = 'Trait mindfulness', pos = '0,1!']
  anxiety     [label = 'Attachment anxiety', pos = '2,2!']
  avoidance   [label = 'Attachment avoidance', pos = '2,0!']
  loneliness  [label = 'Loneliness', pos = '4,1!']

  mindfulness -> anxiety    [label = '-.475***']
  mindfulness -> avoidance  [label = '-.243**']
  anxiety -> loneliness     [label = '.285***']
  avoidance -> loneliness   [label = '.548***']
  mindfulness -> loneliness [label = '-.086']
}
")

svg <- export_svg(mediation_diagram)

writeLines(
  svg,
  "figures/mediation_path_diagram.svg"
)

## 4. Mediation indirect effects table

mediation_results <- parameterEstimates(
  fit,
  standardized = TRUE,
  ci = TRUE,
  boot.ci.type = "perc"
)

indirect_effects <- mediation_results[
  mediation_results$label %in% c(
    "indirect_anx",
    "indirect_avoid",
    "indirect_total"
  ),
]

indirect_table <- indirect_effects[, c(
  "label",
  "est",
  "se",
  "ci.lower",
  "ci.upper",
  "std.all"
)]

indirect_table$label <- c(
  "Attachment anxiety",
  "Attachment avoidance",
  "Total indirect effect"
)

names(indirect_table) <- c(
  "Indirect effect",
  "B",
  "SE",
  "95% CI lower",
  "95% CI upper",
  "β"
)

indirect_table

write.csv(indirect_table,
          "tables/mediation_indirect_effects.csv",
          row.names = FALSE
          )