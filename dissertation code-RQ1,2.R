#Dissertation
# Load necessary libraries
library(tidyverse)  # For data manipulation
library(psych)      # For descriptive statistics
library(car)        # For VIF (Variance Inflation Factor)
library(nnet)       # For multinomial logistic regression
library(broom)
library(ggplot2)

df <- read.csv("E:/desktop/postgraduate dissertation/ONLINE EDUCATION SYSTEM REVIEW.csv")
# View column names
colnames(df)
# Rename columns to standardized, coding-friendly names
df <- df %>%
  rename(
    Gender = Gender,
    Home = Home.Location,
    Education = Level.of.Education,
    Device = Device.type.used.to.attend.classes,
    Economic = Economic.status,
    InternetQuality = Internet.facility.in.your.locality,
    Sports = Are.you.involved.in.any.sports.,
    ElderlyMonitor = Do.elderly.people.monitor.you.,
    StudyTime = Study.time..Hours.,
    SleepTime = Sleep.time..Hours.,
    SocialMedia = Time.spent.on.social.media..Hours.,
    Gaming = Interested.in.Gaming.,
    SeparateRoom = Have.separate.room.for.studying.,
    GroupStudy = Engaged.in.group.studies.,
    Interaction = Your.interaction.in.online.mode,
    Performance = Performance.in.online,
    Satisfaction = Your.level.of.satisfaction.in.Online.Education
  )
# Convert necessary variables to factors
df <- df %>%
  mutate(across(c(Gender, Home, Education, Device, Economic, Sports,
                  ElderlyMonitor, Gaming, SeparateRoom, GroupStudy,
                  Satisfaction),
                as.factor))

# Convert numeric variables
df <- df %>%
  mutate(across(c(StudyTime, SleepTime, SocialMedia, Interaction,
                  InternetQuality,  Performance), as.numeric))

# Remove rows with missing key variables (in either outcome or predictors)
df <- df %>%
  drop_na(Performance, Satisfaction, StudyTime, SleepTime, SocialMedia,
          Interaction, InternetQuality)


#RQ1: What factors influence student’s academic performance and satisfaction in online education environments? 
#How do factors especially significant factors respectively influence students’ academic performance and satisfaction in online learning environments?

# -------------------------------
# MODEL 1: Linear regression for academic performance
# -------------------------------
model_perf <- lm(Performance ~ Gender + Home + Education + Device + Economic +
                   InternetQuality + Sports + ElderlyMonitor + StudyTime + SleepTime +
                   SocialMedia + Gaming + SeparateRoom + GroupStudy  +
                   Interaction,
                 data = df)

# View model summary
summary(model_perf)

# Check for multicollinearity
vif(model_perf)

# -------------------------------
# MODEL 2: Multinomial logistic regression for satisfaction
# -------------------------------
model_sat <- multinom(Satisfaction ~ Gender + Home + Education + Device + Economic +
                        InternetQuality + Sports + ElderlyMonitor + StudyTime + SleepTime +
                        SocialMedia + Gaming + SeparateRoom + GroupStudy  +
                        Interaction,
                      data = df)
# View summary
summary(model_sat)

# Compute z-values and p-values for the logistic model
z_vals <- summary(model_sat)$coefficients / summary(model_sat)$standard.errors
p_vals <- 2 * (1 - pnorm(abs(z_vals)))

cat("Z-values:\n")
print(z_vals)

cat("P-values:\n")
print(p_vals)


# Tidy model outputs
coef_perf <- tidy(model_perf) %>%
  filter(term != "(Intercept)") %>%
  mutate(model = "Performance")

coef_sat <- tidy(model_sat) %>%
  filter(term != "(Intercept)") %>%
  mutate(model = "Satisfaction")

# Combine both models
coef_all <- bind_rows(coef_perf, coef_sat)

# Reorder terms by absolute estimate for clarity
coef_all <- coef_all %>%
  mutate(term = fct_reorder(term, estimate))

# Plot
ggplot(coef_all, aes(x = estimate, y = term, fill = model)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Coefficient Comparison: Performance vs Satisfaction",
       x = "Coefficient Estimate", y = "Predictor",
       fill = "Model") +
  theme_minimal() +
  theme(text = element_text(size = 12))


#RQ2: How do academic performance and student satisfaction interact and influence one another in the context of online education?
#method1:
df$Satisfaction_ord <- factor(df$Satisfaction, levels = c("Bad", "Average", "Good"), ordered = TRUE)
df$Satisfaction_num <- as.numeric(df$Satisfaction_ord)
model_perf_on_sat <- lm(Performance ~ Satisfaction_num, data = df)
summary(model_perf_on_sat)

#method2:
model_sat_on_perf <- multinom(Satisfaction ~ Performance, data = df)
summary(model_sat_on_perf)
z_vals <- summary(model_sat_on_perf)$coefficients / summary(model_sat_on_perf)$standard.errors
p_vals <- 2 * (1 - pnorm(abs(z_vals)))
print(p_vals)

# Performance by Satisfaction boxplot
ggplot(df, aes(x = Satisfaction, y = Performance, fill = Satisfaction)) +
  geom_boxplot() +
  labs(title = "Performance Distribution Across Satisfaction Levels")








