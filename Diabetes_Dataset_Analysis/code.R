library(dplyr)
library(ggplot2)
library(knitr)
library(lubridate)
library(reshape2)
library(cluster)
library(tidyverse)
library(kableExtra)

setwd("C:/3rd Semester/Introduction to Data Science/DataScienceProject/")

diabetes_data <- read.csv("diabetes.csv")
View(diabetes_data)


#----------- CHECKING THE MISSING VALUES ------------
colSums(is.na(diabetes_data))
any(is.na(diabetes_data))


#------------ Check for duplicate rows -----------
duplicate_rows <- diabetes_data[duplicated(diabetes_data), ]

print(duplicate_rows)

any_duplicates <- any(duplicated(diabetes_data))
print(any_duplicates)
unique_diabetes_data <- unique(diabetes_data)



# ----------- EXPLORE THE DATASET ----------------
head(diabetes_data)
summary(diabetes_data)
str(diabetes_data)


# ----------- CORRELATION OF THE DATASETS -------------
numeric_variables <- sapply(diabetes_data, is.numeric)
numeric_data <- diabetes_data[, numeric_variables]

correlation_matrix <- cor(numeric_data)

correlation_long <- reshape2::melt(correlation_matrix)

ggplot(correlation_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#003366", high = "#001f3f", mid = "#66ccff", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
  geom_text(aes(x = Var1, y = Var2, label = sprintf("%0.2f", value)), color = "white", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 10, hjust = 1)) +
  labs(title = "Modified Correlation Heatmap")


# -------------- ANALYSING DIABETES ON THE BASIS OF AGE GROUP --------------
diabetes_data <- diabetes_data %>%
  mutate(Age_Group = cut(Age, breaks = c(20, 30, 40, 50, 60, Inf), labels = c("20-30", "30-40", "40-50", "50-60", "60+"), right = FALSE))

age_group_summary <- diabetes_data %>%
  group_by(Age_Group) %>%
  summarize(
    Total_Individuals = n(),
    Diabetes_Prevalence = mean(Outcome == 1) * 100  
  )

print(kable(age_group_summary, format = "markdown"))

ggplot(age_group_summary, aes(x = Age_Group, y = Diabetes_Prevalence, fill = Age_Group)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", Diabetes_Prevalence)), vjust = -0.5) +
  scale_fill_manual(values = c("20-30" = "skyblue", "30-40" = "lightgreen", "40-50" = "lightcoral", "50-60" = "gold", "60+" = "lightblue")) +
  labs(
    title = "Diabetes Prevalence by Age Group",
    x = "Age Group",
    y = "Diabetes Prevalence (%)"
  ) +
  theme_minimal()


#----------------- AVERAGE BLOOD PRESSURE BY AGE GROUP AND BP CATEGORY-----------
diabetes_data$Age_Group <- cut(diabetes_data$Age, breaks = c(20, 30, 40, 50, 60, Inf), labels = c("20-30", "30-40", "40-50", "50-60", "60+"), right = FALSE)

diabetes_data$BP_Category <- cut(diabetes_data$BloodPressure,
                                 breaks = c(-Inf, 90, 120, Inf),
                                 labels = c("Low BP", "Normal BP", "High BP"),
                                 right = FALSE)

bp_summary <- diabetes_data %>%
  group_by(Age_Group, BP_Category) %>%
  summarize(
    Avg_BloodPressure = mean(BloodPressure, na.rm = TRUE),
    Count = n()
  )

print(kable(bp_summary, format = "markdown"))

ggplot(bp_summary, aes(x = Age_Group, y = Avg_BloodPressure, fill = BP_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Blood Pressure by Age Group and Blood Pressure Category",
    x = "Age Group",
    y = "Average Blood Pressure",
    fill = "Blood Pressure Category"
  ) +
  scale_fill_manual(values = c("Low BP" = "lightgreen", "Normal BP" = "lightcoral", "High BP" = "skyblue")) +
  theme_minimal()


# -------------- INSIGHTS OF BLOOD PRESSURE CATEGORY AND DIABETES PREVALENCE --------------
diabetes_data <- diabetes_data %>%
  mutate(BP_Category = case_when(
    BloodPressure < 90 ~ "Low BP",
    BloodPressure >= 90 & BloodPressure <= 120 ~ "Normal BP",
    BloodPressure > 120 ~ "High BP"
  ))

bp_category_summary <- diabetes_data %>%
  group_by(BP_Category) %>%
  summarize(
    Total_Individuals = n(),
    Diabetes_Prevalence = mean(Outcome == 1) * 100
  )

print(kable(bp_category_summary, format = "markdown"))

ggplot(bp_category_summary, aes(x = BP_Category, y = Diabetes_Prevalence, fill = BP_Category)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Diabetes Prevalence by Blood Pressure Category",
    x = "Blood Pressure Category",
    y = "Diabetes Prevalence (%)"
  ) +
  theme_minimal()

# --------------- AGE GROUP VS GLUCOSE -------------
ggplot(diabetes_data, aes(x = Age_Group, y = Glucose, fill = Age_Group)) +
  geom_boxplot(outlier.shape = NA) +  
  labs(
    title = "Grouped Boxplot of Glucose by Age Group",
    x = "Age Group",
    y = "Glucose (in mg/dL)"
  ) +
  scale_fill_manual(values = c("20-30" = "skyblue", "30-40" = "lightgreen", "40-50" = "lightcoral", "50-60" = "gold", "60+" = "lightblue")) +
  theme_minimal()


# ------------- BMI GROUP VS DIABETES PREVALENCE -----------
diabetes_data <- diabetes_data %>%
  mutate(BMI_Group = cut(BMI, breaks = c(0, 18.5, 25, 30, Inf), labels = c("Underweight", "Normal Weight", "Overweight", "Obesity"), right = FALSE))

bmi_group_summary <- diabetes_data %>%
  group_by(BMI_Group) %>%
  summarize(
    Total_Individuals = n(),
    Diabetes_Prevalence = mean(Outcome == 1) * 100
  )

ggplot(bmi_group_summary, aes(x = BMI_Group, y = Diabetes_Prevalence, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Diabetes Prevalence by BMI Group",
    x = "BMI Group",
    y = "Diabetes Prevalence (%)"
  ) +
  theme_minimal()


# -------------- NUMBER OF PREGNANCIES AS PER AGE GROUP ----------------
pregnancies_age_summary <- diabetes_data %>%
  group_by(Age_Group, Pregnancies) %>%
  summarize(Total_Individuals = n())

ggplot(pregnancies_age_summary, aes(x = Pregnancies, y = Age_Group, fill = as.factor(Pregnancies))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Pregnancies as per Age Groups",
    x = "Number of Pregnancies",
    y = "Age Group",
    fill = "Number of Pregnancies"
  ) +
  theme_minimal()


# ----------------- DIABETES PREVALENCE BY NUMBER OF PREGNANCIES ---------------
pregnancies_group_summary <- diabetes_data %>%
  group_by(Pregnancies) %>%
  summarize(
    Total_Individuals = n(),
    Diabetes_Prevalence = mean(Outcome == 1) * 100
  )

print(kable(pregnancies_group_summary, format = "markdown"))

ggplot(pregnancies_group_summary, aes(x = Pregnancies, y = Diabetes_Prevalence, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Diabetes Prevalence by Number of Pregnancies",
    x = "Number of Pregnancies",
    y = "Diabetes Prevalence (%)"
  ) +
  theme_minimal()


# -------------- DIABETES PREVALENCE BY GLUCOSE LEVELS --------------
diabetes_data$Glucose_Group <- cut(diabetes_data$Glucose, breaks = c(0, 100, 150, Inf), labels = c("Low", "Medium", "High"), right = FALSE)

# Analyze diabetes prevalence for different glucose groups
glucose_summary <- diabetes_data %>%
  group_by(Glucose_Group) %>%
  summarize(
    Total_Individuals = n(),
    Diabetes_Prevalence = mean(Outcome == 1) * 100
  )

print(kable(glucose_summary, format = "markdown"))


ggplot(glucose_summary, aes(x = Glucose_Group, y = Diabetes_Prevalence, fill = Glucose_Group)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Diabetes Prevalence by Glucose Levels",
    x = "Glucose Group",
    y = "Diabetes Prevalence (%)",
    fill = "Glucose Group"
  ) +
  theme_minimal()


# ---------------- HIGH RISK OF DIABETES IN TERMS OF GLUCOSE AND BMI -----------------
high_glucose_threshold <- 150
high_bmi_threshold <- 30

ggplot(diabetes_data, aes(x = Glucose, y = BMI)) +
  geom_point(aes(color = ifelse(Glucose > high_glucose_threshold | BMI > high_bmi_threshold, "High Risk", "Low Risk"))) +
  geom_hline(yintercept = high_bmi_threshold, linetype = "dashed", color = "red") +
  geom_vline(xintercept = high_glucose_threshold, linetype = "dashed", color = "red") +
  labs(
    title = "High Risk of Diabetes - Glucose vs BMI",
    x = "Glucose",
    y = "BMI",
    color = "Diabetes Risk Level"
  ) +
  theme_minimal()


# ------------ K-MEANS CLUSTERING OF GLUCOSE AND BMI ---------------- 
features_for_clustering <- diabetes_data[, c("Glucose", "BMI", "Outcome")]

k <- 3

kmeans_model <- kmeans(features_for_clustering[, c("Glucose", "BMI")], centers = k)

diabetes_data$Cluster <- as.factor(kmeans_model$cluster)

diabetes_data$Outcome_Label <- ifelse(diabetes_data$Outcome == 1, "Yes", "No")

ggplot(diabetes_data, aes(x = Glucose, y = BMI, color = Cluster, shape = Outcome_Label)) +
  geom_point() +
  labs(
    title = "K-means Clustering of Glucose and BMI with Diabetes Outcome",
    x = "Glucose",
    y = "BMI",
    color = "Cluster",
    shape = "Diabetes Outcome"
  ) +
  theme_minimal()



# --------------- AVERAGE RISK FOR EACH FACTOR -------------------
average_risk <- colMeans(diabetes_data[, c("Pregnancies", "Glucose", "BloodPressure", 
                                           "SkinThickness", "Insulin", "BMI", 
                                           "DiabetesPedigreeFunction", "Age")])

plot_data <- data.frame(Average_Risk = average_risk, Factor = names(average_risk))

ggplot(plot_data, aes(x = Factor, y = Average_Risk, fill = Factor)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Average Risk of Diabetes by Factor",
    x = "Factors Affecting Diabetes",
    y = "Average Diabetes Risk"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
