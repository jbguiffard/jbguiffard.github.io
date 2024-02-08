################################################################################
# M1S2 EPOLPRO - PROJET TUTORE
# SCRIPT 2: Introduction to R
# Date: 08/02/2024
################################################################################


# Install and load necessary packages
install.packages("questionr") # Install questionr package
library(questionr) # Load questionr package

# Load and explore the dataset
data(hdv2003) # Load the 'hdv2003' dataset from the 'questionr' package
View(hdv2003) # View the dataset in a spreadsheet-like viewer

# Basic data structure exploration
data <- hdv2003 # Assign the dataset to a new variable 'data' for ease of use
nrow(data) # Display the number of rows in the dataset
ncol(data) # Display the number of columns in the dataset
dim(data) # Display dimensions of the dataset (rows and columns)
rownames(data) # Display the row names
colnames(data) # Display the column names (variable names)
names(data) # Alternative way to display column names
str(data) # Display the structure of the dataset, including data types of each column

# Accessing specific variables
data$sexe # Access the 'sexe' (gender) column

# Exploring a quantitative variable
head(data$age) # Display the first few entries of the 'age' column
tail(data$age, 10) # Display the last 10 entries of the 'age' column

# More detailed exploration of another variable
head(data$heures.tv, 10) # Display the first 10 entries of the 'heures.tv' (TV hours) column

# Data manipulation: Creating a new variable
data$minutes.tv <- data$heures.tv * 60 # Convert TV hours to minutes

# Display the new variable
head(data$minutes.tv)

# Univariate analysis of a quantitative variable
min(data$age) # Minimum age
max(data$age) # Maximum age
range(data$age) # Range of age

mean(data$age) # Mean age
median(data$age) # Median age

max(data$age) - min(data$age) # Age range calculation

var(data$age) # Variance of age
sd(data$age) # Standard deviation of age

summary(data$age) # Summary statistics for age

quantile(data$age, prob = 0.25) # 25th percentile (Q1)
quantile(data$age, prob = 0.75) # 75th percentile (Q3)

# Visualization: Histograms
hist(data$age) # Basic histogram of age
hist(data$age, breaks = 10) # Histogram with 10 breaks
hist(data$age, breaks = 70) # Histogram with 70 breaks
hist(data$age, col = "green", main = "Age distribution of respondents", xlab = "Age", ylab = "Number") # Customized histogram

# Analysis of qualitative variables
table(data$sexe) # Frequency table for 'sexe'
table(data$qualif) # Frequency table for 'qualif' (qualification)

# Sorting and summarizing qualitative data
tab <- table(data$qualif) # Save frequency table to 'tab'
sort(tab) # Sort the frequency table
summary(data$qualif) # Summary of 'qualif' variable

# Advanced frequency analysis using 'questionr' functions
freq(data$qualif) # Frequency analysis with percentages
freq(data$qualif, valid = FALSE, total = TRUE, sort = "dec") # Frequency analysis including invalid and total counts, sorted in descending order

# Visualizing qualitative data
tab <- table(data$clso) # Frequency table for 'clso' variable
barplot(tab) # Basic bar plot
barplot(sort(tab)) # Bar plot with sorted frequencies
dotchart(table(data$qualif)) # Dot chart for 'qualif'
dotchart(sort(table(data$qualif))) # Sorted dot chart for 'qualif'
