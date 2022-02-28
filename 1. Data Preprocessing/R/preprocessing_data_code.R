# Data Preprocessing

# Importing the dataset
dataset = read.csv('Data.csv')

# Taking care of missing data 

#As a note, there are much more efficient ways to impute missing data, but this will not be discussed here

#Age
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
#this is the same as
dataset$Age <- ifelse(is.na(dataset$Age), mean(dataset$Age, na.rm = T), dataset$Age)
#or
dataset$Age[is.na(dataset$Age)] <- mean(dataset$Age, na.rm = T)

#Salary
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)
#this is the same as
dataset$Salary <- ifelse(is.na(dataset$Salary), mean(dataset$Salary, na.rm = T), dataset$Salary)
#or
dataset$Salary[is.na(dataset$Salary)] <- mean(dataset$Salary, na.rm = T)

# Encoding categorical data

#Country 
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))
#this is the same as
dataset$Country <- ifelse(dataset$Country == "France", 1, ifelse(dataset$Country == "Spain", 2, 3))

#Purchased
dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
#this is the same as
dataset$Purchased <- ifelse(dataset$Purchased == "No", 0, 1)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)

#setting the seed
set.seed(123)

#splitting the data 80% training and 20% testing
split = sample.split(dataset$Purchased, SplitRatio = (8/10))

#making the training set
training_set = subset(dataset, split == TRUE)
#making the testing set
test_set = subset(dataset, split == FALSE)

#there are many other ways to split the dataset without using a package, such as:
dataset$split <- sample(x = c(rep("train", 8), rep("test", 2)), size = 10, replace = F)
train <- dataset[dataset$split == "train", -5]
train <- dataset[dataset$split == "test", -5]

#or
smp_size <- floor(0.8 * nrow(dataset))
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size) #seq_len() is the same as 1:nrow(df)
train2 <- dataset[train_ind, ]
test2 <- dataset[-train_ind, ]