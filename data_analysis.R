---
title: 'Community Health: Statistical Analysis Report'
author: "Andrew Tumwesigye"
date: "2026-05-12"
output:
  html_document: default
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
#step1: identify general folder where data is stored
getwd()

#step2: list file in specific folder
list.files("/cloud/project/data")
#step3: load dataset in the folder
community_health <- read.csv("/cloud/project/data/community_health_genetic_risk_dataset.csv")
head(community_health)

```

```{r}
#1. WHAT IS THE AVERAGE BMI ACROSS DIFFERENT AGE GROUPS?
# a: create age categories
age_groups <- cut(community_health$Age,
                  breaks=c(18,35,45,55,65,75),
                  labels=c("18-35","36-45","46-55","56-65","66-75"),
                  include.lowest = TRUE)

# b: install dplyr package
install.packages("dplyr")
library(dplyr)

# c: create a new column age groups
community_health <- community_health %>%
  mutate(
    Age_Groups = age_groups
  )

# d: aggregate and group by age groups
bmi_status <- community_health %>%
  group_by (Age_Groups)%>%
  summarise(AverageBMI = mean(BMI,na.rm=TRUE),.groups='drop')

# e: print results
print(bmi_status)

# f: visualize output
install.packages("ggplot2")
library(ggplot2)

ggplot(data=bmi_status,mapping=aes(x=Age_Groups,y=AverageBMI,size=AverageBMI))+
  geom_point(color = 'red')+ labs(title='Average BMI by Age Group')

```
```{r}
#2. WHAT ARE THE MINIMUM, MAXIMUM, AND QUARTILE VALUES FOR SYSTOLICBP?
summary(community_health$SystolicBP)
```
```{r}
#3. WHAT IS THE STANDARD DEVIATION OF GLUCOSE LEVELS? 
sd(community_health$GlucoseLevel)
```
```{r}
#4. WHICH AGE GROUP HAS THE HIGHEST AVERAGE CHOLESTEROL LEVEL? 
# a: aggregate and group by age groups
age_cholestrol <- community_health %>%
  group_by(Age_Groups)%>%
  summarise(Avg_Cholestrol = mean(CholesterolLevel,na.rm=TRUE),.groups='drop')
print(age_cholestrol)

# b: visualize output
ggplot(data=age_cholestrol,mapping=aes(x=Age_Groups,y=Avg_Cholestrol, fill=Age_Groups))+
  geom_col(color='black')+labs(title='Choletrol Level by Age Group')

```
```{r}
#5. WHAT IS THE DISTRIBUTION OF EYECOLOR AMONG PARTICIPANTS?

# a: count of eye color grouped by eye color types
eye_color_distribution <- community_health %>%
  group_by(EyeColor)%>%
  summarise(EyeColorCount=n(),.groups='drop')
print(eye_color_distribution)

# b: visualize output
ggplot(data=eye_color_distribution, mapping=aes(x=EyeColor,y=EyeColorCount))+
  geom_col(color='black',fill='grey') + labs(title='Distribution of EyeColor')

```
```{r}
#6. WHAT IS THE RATE/PROPORTION OF SMOKING STATUS PARTICIPANTS? 
# a: calculate the rate and group by smoker status
participant_proportion <- community_health %>%
  group_by(SmokingStatus)%>%
  summarise(smoke_status_count = n())%>%
  mutate (
    rate = smoke_status_count / sum(smoke_status_count)*100
  )
print(participant_proportion)

# b: visualize output
ggplot(data=participant_proportion,mapping=aes(x='',y=smoke_status_count,fill=SmokingStatus))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Distribution of Smoking Status",
    fill = "Smoking Status"
  ) +
  theme_void()
```
```{r}
#7. WHAT IS THE RATE OF PARTICIPANTS WHO HAVE AND DONT HAVE A FAMILYHISTORYDIABETES? 
# a: count and group by family history diabetes
diabetes_status <- community_health %>%
  group_by(FamilyHistoryDiabetes)%>%
  summarise(diabetes_count=n())%>%
  mutate(
    diabetes_rate = diabetes_count / sum(diabetes_count)*100
  )
print(diabetes_status)

# b:visualize output
ggplot(data=diabetes_status,mapping=aes(x='',y=diabetes_count,fill=FamilyHistoryDiabetes))+
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(diabetes_rate, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Distribution of Participants by Family History of Diabetes",
    fill = "Family History"
  ) +
  theme_void()
```
```{r}
#8. IS SMOKINGSTATUS ASSOCIATED WITH DIAGNOSISRISK? 
# a: build contingency table
tbl_1 <- table(community_health$SmokingStatus, community_health$DiagnosisRisk)

# b: calculate degree of association
smoking_diagnosis_association <-chisq.test(tbl_1)
print(smoking_diagnosis_association)
```
The above result indicates is a statistically significant association between the two categorical variables (p < 0.001).
The large chi-square value (98.95) indicates that the observed distribution is extremely unlikely to have occurred by chance alone.

```{r}
#9. IS GENDER ASSOCIATED WITH STRESSLEVEL? 
# a: create a contingency table
tbl_2 <- table(community_health$Gender,community_health$StressLevel)

# b: calculate degree of association
gender_stress_association <- chisq.test(tbl_2)
print(gender_stress_association)
```
There is no statistically significant association between the two variables 
(p = 0.2088). The chi-square value (3.13) suggests that any observed difference is likely due to chance, not a real relationship.

```{r}
#10. IS URBANRURAL ASSOCIATED WITH ALCOHOLCONSUMPTIONLEVEL? 
# a: create a contingency table
tbl3 <- table(community_health$UrbanRural,community_health$AlcoholConsumptionLevel)

# b: calculate degree of association between variables
rural_urban_alcohol_association <- chisq.test(tbl3)
print(rural_urban_alcohol_association)
```
There is no statistically significant association between the variables (p = 0.5105). 
The very low chi-square value (1.34) confirms that the observed distribution is very close to what would be expected by chance alone.
In conclusion, no relationship exists, the variables are independent.

```{r}
# 11. IS THERE A SIGNIFICANT DIFFERENCE IN BMI BETWEEN MALES AND FEMALES? 
t_result1 <- t.test(BMI ~ Gender, data = community_health, na.rm =TRUE)
print(t_result1)
```
There is a statistically significant difference in BMI between males and females (p = 0.0027).
Males have a significantly higher mean BMI (26.30) compared to females (25.11), with a mean difference of approximately negative 1.2 units.

```{r}
# 12. DO CURRENT SMOKERS HAVE HIGHER CHOLESTEROL LEVELS THAN NON-SMOKERS? 
anova_result <- aov(CholesterolLevel ~ SmokingStatus, data = community_health, na.rm = TRUE)
summary(anova_result)
```
Since p-value (0.678) > alpha (0.05), we fail to reject the null hypothesis.
In Conclusion, there is insufficient evidence to conclude that smoking status affects cholesterol levels. The observed differences in group means are likely due to random chance.

```{r}
# 13. DOES MONTHLYMEDICALCOST DIFFER SIGNIFICANTLY ACROSS REGIONS? 
aov_result2 <- aov(MonthlyMedicalCost ~ Region,data=community_health,na.rm=TRUE)
summary(aov_result2)
```
Since p-value (0.802 > alpha value 0.05, we fail to reject the null hypothesis)
There is no evidence that monthly medical costs differ across regions

```{r}
#14. DO COGNITIVETESTSCORES VARY ACROSS BLOODTYPE GROUPS? 
anova_result3 <- aov(CognitiveTestScore ~ BloodType,data=community_health,na.rm=TRUE)
summary(anova_result3)
```
Since p-value (0.0844) > alpha (0.05), we fail to reject the null hypothesis.
However, the result is marginally significant (p < 0.10), suggesting a weak trend that does not meet conventional statistical significance.

```{r}
#15. IS THERE A CORRELATION BETWEEN BMI AND BLOODPRESSURE? 
corr1 <- cor.test(community_health$BMI,community_health$SystolicBP,use = 'complete.obs')
print(corr1)
```
Since p-value (< 0.001) < alpha (0.05), we REJECT the null hypothesis.
There is a statistically significant, moderate-to-strong positive correlation 
between BMI and systolic blood pressure (r = 0.644, p < 0.001).

```{r}
#16. HOW STRONGLY IS DAILYSCREENTIMEHOURS RELATED TO DEPRESSIONSCORE? 
corr2 <- cor.test(community_health$DailyScreenTimeHours,community_health$DepressionScore,use='complete.obs')
print(corr2)
```
Since p-value (< 0.001) < alpha (0.05), we REJECT the null hypothesis.
There is a statistically significant, weak to moderate positive correlation 
between BMI and systolic blood pressure (r = 0.3777, p < 0.001).

```{r}
#17. IS EXERCISEHOURSPERWEEK NEGATIVELY CORRELATED WITH BMI? 
corr3 <- cor.test(community_health$ExerciseHoursPerWeek,community_health$BMI,use='complete.obs')
print(corr3)
```
Exercise hours per week is not significantly correlated with BMI(r = 0.0005, p = 0.9872), suggesting no linear relationship between these variables in this population.

```{r}
#18. DOES STRESSLEVEL SIGNIFICANTLY PREDICT DEPRESSIONSCORE? 
linear_model1 <- lm(DepressionScore~StressLevel,data=community_health)
summary(linear_model1)
```
The intercept value of 15.2928 represents the estimated depression score for the 
reference stress category (StressLevelHigh) Compared to the reference category, participants with low stress had depression scores lower by 2.6861 points, while participants with medium stress had depression scores lower by 2.2866 points. Both relationships were statistically significant (p<0.001).
In conclusion, stress level significantly affects depression score.

```{r}
#19. CAN EXERCISEHOURSPERWEEK PREDICT CHOLESTEROLLEVEL? 
linear_model2 <- lm(CholesterolLevel~ExerciseHoursPerWeek,data=community_health)
summary(linear_model2)

```
The predicted average cholesterol level for someone with 0 hours of exercise per
week is 217.9866.
For each additional hour of exercise per week, cholesterol level decreases by only 0.14, but this effect is NOT statistically significant (p = 0.793 > alpha 0.05)

```{r}
#20. CAN SLEEPHOURS AND SCREENTIME PREDICT COGNITIVETESTSCORE? 
linear_model3 <- lm(CognitiveTestScore~DailyScreenTimeHours+SleepHours,data=community_health)
summary(linear_model3)
```
The predicted average cognitive test score is 103.5 when Daily Screen Time is 0 hours and Sleep Hours is 0 though this is an extrapolation since sleep hours cannot be zero in reality.
Each additional hour of screen time decreases cognitive score by 1.45 points holding sleep constant
Each additional hour of sleep increases cognitive score by 1.68 points holding screen time constant
Since the p value (< 2.2e-16) is less than alpha (0.05), the overall regression model is statistically significant. 
In conclusion, both daily screen time and sleep hours are significant predictors of cognitive test scores.

```{r}
#21. DOES FAMILYHISTORYDIABETES SIGNIFICANTLY INCREASE DISEASE RISK? 
logistical_model1 <- glm(DiagnosisRisk ~ FamilyHistoryDiabetes, data=community_health,family='binomial')
summary(logistical_model1)
```
Intercept =  negative 5.4525 indicates that without family history, the chance of high diagnostic risk is very low.
Since p value (1.35e-11) < 0.05, we REJECT the null hypothesis
Having a family history of diabetes is associated with 56.6 times higher odds of 
being at high diagnosis risk (applied exponential to 4.04 to derive 56.6)

```{r}
#22. CAN A MODEL USING AGE, BMI, GLUCOSELEVEL, AND STRESSLEVEL ACCURATELY CLASSIFY DIAGNOSISRISK?
logistical_model2 <- glm(DiagnosisRisk~Age+BMI+GlucoseLevel, data=community_health,family='binomial')
summary(logistical_model2)

```
When Age = 0, BMI = 0, and GlucoseLevel = 0, the log-odds of the diagnostic outcome being true is -8.07, indicating a very low baseline probability of the outcome.

Age had a coefficient of -0.004017, meaning increasing age slightly decreased the odds of the outcome. Converting the coefficient using e−0.004017e^{-0.004017}e−0.004017 gives an odds ratio of approximately 0.996, indicating that each 1-year increase in age decreases the odds of the outcome by about 0.4\%. However, the p-value for Age was 0.623, which is greater than 0.05, showing that age was not a statistically significant predictor of the outcome.

BMI had a coefficient of 0.147874, meaning increasing BMI increased the odds of the outcome. Converting the coefficient using e0.147874e^{0.147874}e0.147874 gives an odds ratio of approximately 1.16, indicating that each 1-unit increase in BMI increases the odds of the outcome by about 16\%. The p-value for BMI was 9.83e-07, which is far below 0.05, showing a statistically significant relationship between BMI and the outcome.

GlucoseLevel had a coefficient of 0.011436, meaning increasing glucose level slightly increased the odds of the outcome. Converting the coefficient using e0.011436e^{0.011436}e0.011436 gives an odds ratio of approximately 1.012, indicating that each 1-unit increase in glucose level increases the odds of the outcome by about 1.2\%. However, the p-value for GlucoseLevel was 0.399, which is greater than 0.05, indicating that glucose level was not a statistically significant predictor of the outcome in this model.

```{r}
#23. HOW DOES MONTHLYMEDICALCOST TREND OVER FOLLOWUPMONTH? 

#a: install lubridate
install.packages("lubridate")
library(lubridate) 

#b: convert followupmonth column to date format
community_health$FollowUpMonth <- as.Date(community_health$FollowUpMonth)

#c: extract month from follow up month
community_health <- community_health %>%
  mutate(
    month = month(FollowUpMonth, label = TRUE)
  )

#d: calculate total cost by month
cost_trend <- community_health %>%
  group_by(month)%>%
  summarise(TotalCost=sum(MonthlyMedicalCost,na.rm=TRUE),.groups='drop')
print(cost_trend)

#e: visualize output
ggplot(data=cost_trend,mapping=aes(x=month, y=TotalCost,fill=month, group=1,color=month))+geom_line(size = 1)+ geom_point(size = 2)
  labs(title='How Monthly Medical Costs Trend over Followup Months')

```

```{r}
#24. IS THERE SEASONALITY IN SICKDAYSLASTYEAR REPORTING? 
# a: Extract month from FollowUpMonth
library(lubridate)

community_health <- community_health %>%
  mutate(
    month = month(FollowUpMonth, label = TRUE)
  )

# b: Calculate average sick days by month
seasonality_sick <- community_health %>%
  group_by(month) %>%
  summarise(
    AvgSickDays = mean(SickDaysLastYear, na.rm = TRUE),
    TotalSickDays = sum(SickDaysLastYear, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  )

# c: Print results
print(seasonality_sick)

# d: Visualize seasonality
ggplot(seasonality_sick, aes(x = month, y = AvgSickDays, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Seasonality in Sick Days Reporting",
       x = "Month",
       y = "Average Sick Days") +
  theme_minimal()
```

```{r}
#25. WHICH PARTICIPANTS HAVE ABNORMALLY LOW OR HIGH BMI VALUES? 
#a: calculate Q1, Q3 and IQR
Q1 <- quantile(community_health$BMI,0.25,na.rm = TRUE)
Q3 <- quantile(community_health$BMI,0.75,na.rm = TRUE)
IQR <- Q3 - Q1

#b: define bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

#c: visualize outliers using vline
ggplot(data=community_health, mapping=aes(x = BMI)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  geom_vline(xintercept = c(lower_bound, upper_bound), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of BMI Levels",
       subtitle = "Red dashed lines show outlier boundaries",
       x = "BMI",
       y = "Count") +
  theme_minimal()
```

