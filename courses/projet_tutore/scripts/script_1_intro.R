################################################################################
# M1S2 EPOLPRO - PROJET TUTORE
# SCRIPT 1: Introduction to R
# Date: 08/02/2024
################################################################################

# Section 1: Basic Calculations with R
# R can be used as a simple calculator.
2 + 2.5
7 * 5
100 / 25

# Section 2: Getting Help in R
# How to find help on functions.
?mean
help(mean)

# Section 3: Setting the Working Directory
# It's essential to set your working directory to where your data files are stored.
setwd("C:/Users/.../Mon_dossier")  # Adjust the path according to your directory structure.



# Section 4: Installing and Loading Packages
# How to install and then load packages for use.
install.packages('UsingR')  # Corrected function name from install.package to install.packages
library(UsingR)
require(UsingR)  # Note: library() or require() can be used to load packages.

# Section 5: Downloading and Viewing Data
# Reading a dataset and viewing its content.
#data <- read.csv2('mes_donnees.csv')
data(galton)

View(galton)

# Section 6: Basic Objects in R
# Demonstrating different types of objects in R: scalars, vectors, and more.
# Scalar
a <- 1
b <- "Le Séminaire Automat"
b1 <- FALSE
class(b)
mode(b1)

# Vector
c <- c(3, 4, 5, 6)
d <- c(7, 8, 9, 10)
notes <- c(13, 16, 18, 12, 15)
bonus <- 1
nouvelles_notes <- notes + bonus
seq(1, 10, by = 1)
seq(1, 10, by = 0.5)
rep(1, 8)
e <- c("Initiation", "_", "R")

# Matrix
matrix_1 <- matrix(1, nrow = 4, ncol = 2)
length(matrix_1)
dim(matrix_1)
comb_c_d <- c(c, d)
matrix_2 <- matrix(comb_c_d, nrow = 4, ncol = 2, byrow = FALSE)
matrix_2[, 2]  # Accessing the 2nd column.
matrix_2[4, ]  # Accessing the 4th row.
matrix_2[1, 1]  # Accessing a specific element.

# List
i <- list(b, d, matrix_1, "h")
j <- list(i, "Nested list example")

# DataFrame
taille <- c(152, 156, 160, 160, 163, 167, 169, 173, 174, 174)
masse <- c(51, 51, 54, 60, 61, 64, 70, 71, 72, 73)
sexe <- c("M", "F", "F", "M", "M", "F", "F", "M", "F", "F")
df <- data.frame(taille, masse, sexe)
print(df)
nrow(df)
ncol(df)
mean(df$taille)
var(df$taille)
sd(df$taille)
median(df$taille)
cov(df$taille, df$masse)
colnames(df)
head(df, n = 4)
summary(df)
summary(df$taille)
hist(df$taille)

# Section 7: Using Functions in R
# Example of setting a seed for reproducibility and generating random numbers.
set.seed(140)  # Ensures reproducibility of random number generation.
rnorm(n = 4)

# Custom print function
monimpression <- function(maphrase) {
  print(maphrase)
}
bonjour <- "Bonjour ça va ?"

# Example of a custom function to calculate the mean.
my_function1 <- function(var_x) {
  sum_x <- sum(var_x)
  n_x <- length(var_x)
  mean_x <- sum_x / n_x
  return(mean_x)
}
my_function1(df$taille)
