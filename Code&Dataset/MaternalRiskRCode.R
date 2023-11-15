
#LOADING THE PACKAGES
# list of packages required for our code
packages <- c("ggplot2", "tidyr", "nnet", "dplyr", "reshape2")

# function to check and install any missing packages for the user of our code
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}


# applying the function to each package needed as defined in packages
invisible(sapply(packages, install_if_missing))



#READING THE DATAET AND VISUALISING THE STRUCTURE 

# read the raw dataset and name it DataSet_Raw
DataSet_Raw <- read.csv("MaternalRisk4.csv", header=TRUE)

# Check for missing values in RiskLevel for our dataset
print(sum(is.na(DataSet_Raw$RiskLevel)))

# Check the structure of the original dataset
str(DataSet_Raw)



#DATA TRANSFORMATION

# Transforming the risk level column to be numerical 
DataSet_Raw$RiskLevel <- factor(DataSet_Raw$RiskLevel, levels = c("low risk", "mid risk", "high risk"))
DataSet_Raw$RiskLevel <- as.numeric(DataSet_Raw$RiskLevel) - 1 # 0 = low risk, 1= mid risk, 2= high risk 

#transform the column name BS to blood sugar
colnames(DataSet_Raw)[colnames(DataSet_Raw) == "BS"] <- "Blood Sugar"
#transform the diastolicBP to be DiastolicBloodPressure
colnames(DataSet_Raw)[colnames(DataSet_Raw) == "DiastolicBP"] <- "DiastolicBloodPressure"
#transform the SystolicBP to be SystolicBloodPressure 
colnames(DataSet_Raw)[colnames(DataSet_Raw) == "SystolicBP"] <- "SystolicBloodPressure"
#transform BodyTemp to be BodyTemperature for uniformity 
colnames(DataSet_Raw)[colnames(DataSet_Raw) == "BodyTemp"] <- "BodyTemperature"


# View column names now that we have made transformation to check they are correct
print(colnames(DataSet_Raw))

# Check the unique values in RiskLevel to ensure they are correct after transfromation
print(unique(DataSet_Raw$RiskLevel))



#DATA CLEANING PROCESS 


# Check for zero values in numeric columns
numeric_columns <- sapply(DataSet_Raw, is.numeric)
zero_counts <- sapply(DataSet_Raw[, numeric_columns], function(x) sum(x == 0, na.rm= TRUE))
#printing the zero coutns to visualise 
print(zero_counts)


# Convert RiskLevel back to a factor for plotting so that we can see the labels correctly
DataSet_Raw$RiskLevel <- factor(DataSet_Raw$RiskLevel, levels = c(2, 1, 0), 
                                labels = c("High Risk", "Mid Risk", "Low Risk"))


# Check the levels of RiskLevel to ensure they match our custom colors
print(table(DataSet_Raw$RiskLevel))

# convert the dataset to long format for visualisation
long_data <- gather(DataSet_Raw, key = "variable", value = "value", -RiskLevel)



#looking at the spread of the data set to decide how we will clean and more 

# Custom colorsfor each risk level for improved visualisation
risk_colors <- c("Low Risk" = "green", "Mid Risk" = "orange", "High Risk" = "red")

# Plot
#This line initializes a ggplot object
densityplot1 <- ggplot(long_data, aes(x = value, fill = RiskLevel)) + 
  #density layers 60% opaque and 40% transparent
  geom_density(alpha = 0.6) +
  #create a separate density plot for each variable in the dataset on the same page
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  #tells ggplot to use the risk_colors vector created earlier
  scale_fill_manual(values = risk_colors) +
  #set the labels for the y-axis, x-axis, and the legend title
  labs(y = "Density", x = "Value", fill = "Risk Level") +
  #minimalistic theme to the plot
  theme_minimal() +
  #ustomizes the appearance of the plot e.g. text size and bold text
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold")
  )

#print out the plot 
print(densityplot1)
#the graph show that the variables are skewed so this is not a normally distributed data set 




#Function to check for outliers in the dataset using IQR method due to the distribution of out dataset and categorical variables 
check_outliers <- function(data_col) 
  {
  #checks whether data_col is a numeric vector
  if (is.numeric(data_col)) {
    #calculate the first (Q1) and third (Q3) quartiles of the data, which are the 25th and 75th percentiles
    Q1 <- quantile(data_col, 0.25, na.rm = TRUE)
    Q3 <- quantile(data_col, 0.75, na.rm = TRUE)
    #computes the interquartile range (IQR), which is the difference between the third and first quartiles
    IQR <- Q3 - Q1
    #calculate the upper and lower bounds for outliers
    lower_bound <- Q1 - 1.5 * IQR
    #any point that is less that lower or greater than upper is an outlier 
    upper_bound <- Q3 + 1.5 * IQR
    
    # Return the outliers
    return(data_col[data_col < lower_bound | data_col > upper_bound])
  } else {
    return(NULL) # Return NULL for non-numeric columns
  }
}

# Applying the function to numeric columns of the dataset
outliers_list <- lapply(DataSet_Raw[, numeric_columns], check_outliers)

# Print the outliers for each column
print(outliers_list)

#in terms of outliers there  are two heart rates of 7 which are both associated with low risk patients therefore they will need to be removed or swapped out for the median as the heart rate variable is not normally distributed, age of 70 is also unlikely becuase most mothers are not 70 so this could also be a mistake etc.....
# Filter the long_data for only HeartRate variable
heart_rate_data <- long_data[long_data$variable == "HeartRate", ]

# Plot for HeartRate only to visualise the outliers and the skew which is abnormal 
p_heart_rate <- ggplot(heart_rate_data, aes(x = value, fill = RiskLevel)) + 
  #density layers 60% opaque and 40% transparent
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = risk_colors) +
  #setting the title and other labels of the graph x and y 
  labs(title = "Heart Rate Density by Risk Level",
       y = "Density", 
       x = "BPM", 
       fill = "Risk Level") +
  #using the minimal theme 
  theme_minimal() +
  #setting the text elemets 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))
#print the graph 
print(p_heart_rate)
#in terms of outliers there  are two heart rates of 7 which are both associated with low risk patients therefore they will need to be removed or swapped out for the median as the heart rate variable is not normally distributed, age of 70 is also unlikely becuase most mothers are not 70 so this could also be a mistake etc.....


#replacement using the median becuse of the data
#Create a data frame with the count of each Risk Level
risk_level_count <- as.data.frame(table(DataSet_Raw$RiskLevel))

# Calculate the median heart rate excluding outliers
median_heart_rate <- median(DataSet_Raw$HeartRate[DataSet_Raw$HeartRate != 7], na.rm = TRUE)

# Replace outliers with the median heart rate
DataSet_Raw$HeartRate[DataSet_Raw$HeartRate == 7] <- median_heart_rate




risk_level_count <- as.data.frame(table(DataSet_Raw$RiskLevel))

# Convert 'RiskLevel' into binary variables for each level
 DataSet_Raw$IsHighRisk <- as.numeric(DataSet_Raw$RiskLevel == "High Risk")
DataSet_Raw$IsMidRisk <- as.numeric(DataSet_Raw$RiskLevel == "Mid Risk")
DataSet_Raw$IsLowRisk <- as.numeric(DataSet_Raw$RiskLevel == "Low Risk")

# Calculate the point-biserial correlation matrix for 'High Risk' with other variables
 high_risk_cor <- sapply(DataSet_Raw[, sapply(DataSet_Raw, is.numeric)], function(x) {
       cor(x, DataSet_Raw$IsHighRisk, use = "complete.obs")
     })

# Repeat for 'Mid Risk'
 mid_risk_cor <- sapply(DataSet_Raw[, sapply(DataSet_Raw, is.numeric)], function(x) {
     cor(x, DataSet_Raw$IsMidRisk, use = "complete.obs")
  })
# Repeat for 'Low Risk'
   low_risk_cor <- sapply(DataSet_Raw[, sapply(DataSet_Raw, is.numeric)], function(x) {
       cor(x, DataSet_Raw$IsLowRisk, use = "complete.obs")
    })
   
 # Since we have separate vectors, let's combine them into a data frame for visualization
   cor_data <- data.frame(
       Variable = names(high_risk_cor),
       HighRisk = high_risk_cor,
       MidRisk = mid_risk_cor,
       LowRisk = low_risk_cor
    )
# Melt the data frame for ggplot2
   cor_melted <- melt(cor_data, id.vars = 'Variable')
 # Plot the heatmap using ggplot2
   ggplot(cor_melted, aes(x = Variable, y = variable, fill = value)) +
    geom_tile(color = "white") +
     scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
     theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
                   axis.text.y = element_text(size = 12)) +
    labs(x = "", y = "", title = "Correlation between Risk Levels and Other Variables")

   predictors <- DataSet_Raw[, sapply(DataSet_Raw, is.numeric)]
 # Remove the response variable if it's included in numeric columns, for example 'RiskLevel'
   predictors <- predictors[, !colnames(predictors) %in% c("RiskLevel", "IsHighRisk", "IsMidRisk", "IsLowRisk")]
 # Calculate the correlation matrix
   correlation_matrix <- cor(predictors, use = "complete.obs")  # 'use' parameter handles missing values but there arent any
 # View the correlation matrix
   print(correlation_matrix)




#VISUAL ANALYSIS 

# Create the Pie Chart
ggplot(risk_level_count, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Distribution of Risk Levels", y = NULL, x = NULL, fill = "Risk Level") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.title = element_blank())

# Create the Bar Chart
ggplot(DataSet_Raw, aes(x = RiskLevel, fill = RiskLevel)) +
  geom_bar() +
  scale_fill_manual(values = risk_colors) +
  labs(title = "Distribution of Risk Levels", y = "Count", x = "Risk Level", fill = "Risk Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))

#from this we can tell that our data is imbalanced 





#HYPOTHESIS TESTING FOR FURTHER EXPLORATION 


# Ensure that "High Risk" is not the baseline category and low risk would be more appropriate
DataSet_Raw$RiskLevel <- relevel(DataSet_Raw$RiskLevel, ref = "Low Risk")

# Hypothesis 1: Blood Pressure
# Fit the model for blood pressure
#multinom function from the nnet package is used to fit a multinomial logistic regression
multinom_model_bp <- multinom(RiskLevel ~ SystolicBloodPressure + DiastolicBloodPressure + Age + `Blood Sugar`, data = DataSet_Raw)
#stores a summary of the fitted multinomial model
summary_model_bp <- summary(multinom_model_bp)
#p value calculation done by dividing the coefficients by their standard errors, taking the absolute value, using the pnorm function to find the normal cumulative distribution at these values, and then calculating the two-tailed p-value
p_values_bp <- (1 - pnorm(abs(coef(summary_model_bp) / summary_model_bp$standard.errors), 0, 1)) * 2
#extracts the p-values for the coefficients associated with the "High Risk" outcome category 
p_values_bp_high_risk <- p_values_bp["High Risk", c("SystolicBloodPressure", "DiastolicBloodPressure")]

# Hypothesis 2: Blood Sugar
# Fit the model for blood sugar
multinom_model_bs <- multinom(RiskLevel ~ `Blood Sugar` + Age + SystolicBloodPressure + DiastolicBloodPressure, data = DataSet_Raw)
summary_model_bs <- summary(multinom_model_bs)
p_values_bs <- (1 - pnorm(abs(coef(summary_model_bs) / summary_model_bs$standard.errors), 0, 1)) * 2
p_values_bs_high_risk <- p_values_bs["High Risk", "`Blood Sugar`"]

# Hypothesis 3: Age
# Fit the model for age
multinom_model_age <- multinom(RiskLevel ~ Age + SystolicBloodPressure + DiastolicBloodPressure + `Blood Sugar`, data = DataSet_Raw)
summary_model_age <- summary(multinom_model_age)
p_values_age <- (1 - pnorm(abs(coef(summary_model_age) / summary_model_age$standard.errors), 0, 1)) * 2
p_values_age_high_risk <- p_values_age["High Risk", "Age"]

# Print the p-values
print("P-values for High Risk - Blood Pressure Hypothesis:")
print(p_values_bp_high_risk)

print("P-values for High Risk - Blood Sugar Hypothesis:")
print(p_values_bs_high_risk)

print("P-values for High Risk - Age Hypothesis:")
print(p_values_age_high_risk)




#displaying the accuracy of the model we chose

# Predicting the class labels for the dataset
predicted_classes <- predict(multinom_model_bp, DataSet_Raw)

# Calculating the accuracy
accuracy <- sum(predicted_classes == DataSet_Raw$RiskLevel) / nrow(DataSet_Raw)
print(paste("Accuracy of the multinomial logistic regression model:", accuracy))


#EXTRA PLOTTING FOR ANALYSIS OF HYPOTHESIS, INFERENCE, AND EXPLORATION


#creating the supporting plots for the hypothesis 

#setting the colors for the plot
risk_colors <- c("Low Risk" = "green", "Mid Risk" = "orange", "High Risk" = "red")

#blood pressure plot for visualising the hypothesis analysis 
ggplot(DataSet_Raw, aes(x = SystolicBloodPressure, y = DiastolicBloodPressure, color = RiskLevel)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = risk_colors) +
  #naming the plot 
  labs(title = "Blood Pressure and Risk Level",
       x = "Systolic Blood Pressure (mmHg)",
       y = "Diastolic Blood Pressure (mmHg)",
       color = "Risk Level") +
  #minimal theme 
  theme_minimal()

#blood sugar using age as the extra variable for plotting 
ggplot(DataSet_Raw, aes(x = `Blood Sugar`, y = Age, color = RiskLevel)) +
  geom_jitter(alpha = 0.5) +
  scale_color_manual(values = risk_colors) +
  labs(title = "Blood Sugar and Risk Level",
       x = "Blood Sugar Level (mmol/L)",
       y = "Age (years)",
       color = "Risk Level") +
  theme_minimal()

#age variable using blood sugar as the extra variable for plotting 
ggplot(DataSet_Raw, aes(x = Age, y = `Blood Sugar`, color = RiskLevel)) +
  geom_jitter(alpha = 0.5) +
  scale_color_manual(values = risk_colors) +
  labs(title = "Age and Risk Level",
       x = "Age (years)",
       y = "Blood Sugar mmol/L",
       color = "Risk Level") +
  theme_minimal()



# filtering the data set for the high risk category
high_risk_data <- DataSet_Raw %>% 
  filter(RiskLevel == "High Risk")

# Custom color for "High Risk"
high_risk_color <- "red"

# Density Plot for Age for "High Risk"
ggplot(high_risk_data, aes(x = Age, fill = RiskLevel)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("High Risk" = high_risk_color)) +
  labs(title = "Age Density for High Risk",
       x = "Age (years)",
       fill = "Risk Level") +
  theme_minimal()






