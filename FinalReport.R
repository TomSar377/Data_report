## Final report - Tomas Sarkozi 


##Loading of packages
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)


##Loading the dataset
data <- read_excel("C:\\Users\\42190\\Desktop\\data.xlsx", sheet = "Sheet1")
View(data)


##Summary of dataset

#Saving our data as data frame
data_df<-as.data.frame(data)
View(data_df)

#Summary statistics of males 
males_sum <- as.numeric(data_df[1:740, 3])
summary(males_sum, na.rm = TRUE)

#Summary statistics of females 
females_sum <- as.numeric(data_df[741:1480,3])
summary(females_sum, na.rm = TRUE)

#Identifying the missing values
sum(is.na(data))


##Data cleaning

#Drop years with too many NAs and Euro area
data_df1<- data_df[c(41:740,781:1480),]
view(data_df1)
data_df_cleaned <- na.omit(data_df1)
view(data_df_cleaned)


##Data exploration


#Question 1
#Linear regression 
my_model <- lm(EmpRates ~ Sex, data = data_df_cleaned)
summary(my_model)

#Ploting the regression
ggplot(data_df_cleaned, aes(x = Sex, y = EmpRates)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Gender(0=Males, 1=Females)", y = "Employment Rates") +
  ggtitle("Relationship between Gender and Employment Rates")


#Question 2
#Visualising employemnt rates for both genders in all years
data_df_cleaned$Sex <- as.factor(data_df_cleaned$Sex)
ggplot(data_df_cleaned, aes(x = Year, y = EmpRates, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Years") +
  ylab("Rates") +
  ggtitle("Employment Rates by Gender over Time") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Male", "Female")) +
  theme_minimal()




#Question 3
#Average employment rates for both gender in countries of western part of Europe and the rest 

#Calculating mean for males in western countries
males_western_df <- data_df_cleaned[15:243, 1:3]
view(males_western_df)
mean_males_western_df <- mean(males_western_df$EmpRates)
mean_males_western_df

#Calculating mean for males in other countries
males_other_df<- data_df_cleaned[244:486, 1:3]
View(males_other_df)
mean_males_other_df <- mean(males_other_df$EmpRates)
mean_males_other_df

#Calculating mean for females in western countries
females_western_df <- data_df_cleaned[501:729, 1:3]
view(females_western_df)
mean_females_western_df <- mean(females_western_df$EmpRates)
mean_females_western_df

#Calculating mean for females in other countries
females_other_df<- data_df_cleaned[730:972, 1:3]
View(females_other_df)
mean_females_other_df <- mean(females_other_df$EmpRates)
mean_females_other_df

#Gruping the means
means <- c(mean_males_western_df, mean_females_western_df, mean_males_other_df, mean_females_other_df)

# Create a vector of labels
labels <- c("Males Western","Females Western", "Males Other", "Females Other")

# Create a bar plot
barplot(means, names.arg = labels, ylab = "Mean Value", col = c("blue", "red", "blue", "red"))


#Graph on executive summary
#Create a data frame for males and females 
gender_means <- data.frame(
  gender = c("Male", "Female"),
  mean_height = c(males_df, females_df)
)

# Create a bar plot 
ggplot(gender_means, aes(x = gender, y = mean_height, fill = gender)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Gender", y = "Employement Rate (%)", title = "Employment by gender")
coord_flip()




