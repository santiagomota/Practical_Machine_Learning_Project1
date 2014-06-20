# Establecer el idioma de R en ingles
Sys.setenv(LANGUAGE="en")

install.packages("RWeka")
# install.packages("RWeka", lib="/path/to/library")

# Uso total de memoria
gc()

# Dimensions matrix z
dim(z)

# Show elements working directory
ls()

# Remove y from working directory 
rm(y)

# Installing package from Github 
devtools::install_github(c("hadley/testthat", "rstudio/shiny", "rstudio/ggvis"))
# Shinyapps
devtools::install_github("rstudio/shinyapps")
shinyapps::setAccountInfo(name='smota', token='ADD450AEA389719D3BE963A44A25C8CC', secret='CmN6t/1tqkawfyIYPoJS9xi39pSkOZmBszSywEdH')
deployApp()

# Demo of a function
library(lattice)
demo(lattice)

###############################################################################
## Maddison project in R 
## http://ropengov.github.io/general/2013/12/05/maddison-data/

# The Exercise

# Below is a stepwise explanation on how to visualize the data.
# Load the data

library(gdata)
# url <- "http://www.ggdc.net/maddison/maddison-project/data/mpd_2013-01.xlsx"
# dat <- read.xls(url, header = TRUE, skip = 1, stringsAsFactors = FALSE)
# Error por no tener instalado perl
setwd("E:/Varios/R/Archivos/varios")

# Como no quiero instalar perl, paso el fichero a csv con LibreOffice
dat <- read.csv("mpd_2013-01.csv", header = TRUE, stringsAsFactors = FALSE)

# Manipulating the data

# Excel-sheets usually require some processing in R. Below I'm removing row 
# not needed and melting the data in long format, and convert in to numerical.

# column.names <- as.character(dat[1, ])
column.names <- names(dat)
column.names[1] <- "year"
df.mad <- dat[-1, ]
names(df.mad) <- column.names
library(reshape2)
df.mad.l <- melt(df.mad, id.vars = "year")
df.mad.l$value <- as.numeric(df.mad.l$value)
df.mad.l <- df.mad.l[!is.na(df.mad.l$value), ]

# Subset the data

# My research emphasises in post-socialist space and therefore I will subset 
# the data. In addition to post-socialist countries I'm including some Western countries, too.

cntry.list <- c("Czech Republic", "Estonia", "Hungary", "Bulgaria", "Latvia", 
                "Lithuania", "Poland", "Slovakia", "Slovenia", "Romania", "Albania", "Bosnia", 
                "Bulgaria", "Croatia", "Macedonia", "Montenegro", "Kosovo", "Serbia", "Armenia", 
                "Azerbaijan", "Belarus", "Georgia", "Kazakhstan", "Kyrgyzstan", "Moldova", 
                "Mongolia", "Russia", "Tajikistan", "Ukraine", "Uzbekistan", "F. USSR", 
                "USA", "Japan", "Finland", "Sweden")
library(stringr)
df.mad.l$variable <- str_trim(df.mad.l$variable, side = "both")
df.mad.l2 <- df.mad.l[df.mad.l$variable %in% cntry.list, ]

# Then I will group the countries in one sensible way.

library(car)
df.mad.l2$group[df.mad.l2$variable %in% c("Czech Republic", "Estonia", "Hungary", 
                                          "Bulgaria", "Latvia", "Lithuania", "Poland", "Slovakia", "Slovenia", "Romania", 
                                          "Bulgaria")] <- "CEE"
df.mad.l2$group[df.mad.l2$variable %in% c("Albania", "Bosnia", "Croatia", "Macedonia", 
                                          "Montenegro", "Kosovo", "Serbia")] <- "Balkan"
df.mad.l2$group[df.mad.l2$variable %in% c("Armenia", "Azerbaijan", "Belarus", 
                                          "Georgia", "Kazakhstan", "Kyrgyzstan", "Moldova", "Mongolia", "Russia", 
                                          "Tajikistan", "Ukraine", "Uzbekistan")] <- "CIS"
df.mad.l2$group[df.mad.l2$variable %in% c("USA", "Japan", "Finland", "Sweden")] <- "WEST"
df.mad.l2$group[df.mad.l2$variable %in% c("F. USSR")] <- "USSR"

# Plotting the data from 1850 to 2010

# As I mentioned in the beginning the data extends for over 2000 years. My 
# interest is more of contemporary nature, so I will subset data to cover only 
# the last 160 years from 1850 to 2010. Also, I will add some major historical 
# events during that period that have had an effect on economic growth, too.

library(ggplot2)
ggplot(data = df.mad.l2[df.mad.l2$year > 1850, ], aes(x = year, y = value, group = variable, 
                                                      color = group)) + geom_line() + geom_point() + geom_text(data = df.mad.l2[df.mad.l2$year == 
                                                                                                                                  2010, ], aes(x = year, y = value, label = variable)) + annotate("rect", 
                                                                                                                                                                                                  xmin = 1914, xmax = 1918, ymin = 5, ymax = 20000, alpha = 0.2) + annotate("text", 
                                                                                                                                                                                                                                                                            x = 1916, y = 21000, label = "WW1") + annotate("rect", xmin = 1939, xmax = 1945, 
                                                                                                                                                                                                                                                                                                                           ymin = 5, ymax = 20000, alpha = 0.2) + annotate("text", x = 1942, y = 21000, 
                                                                                                                                                                                                                                                                                                                                                                           label = "WW2") + annotate("segment", x = 1991, xend = 1991, y = 0, yend = 30000, 
                                                                                                                                                                                                                                                                                                                                                                                                     colour = "blue") + annotate("text", x = 1991, y = 31000, label = "Dissolution of \n the Soviet Union")+ 
  theme_minimal() + theme(legend.position = "top")


###############################################################################

## Tricks to manage the available memory in an R session?
## https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()

# Create 50 ramdon numbers [0,1]
x=runif(50)

# Create 50 ramdon normalizad distribution numbers [-1,1]
y=rnorm(50)

# Make available data of dataset Boston. Do not use.
# Not elegant code.
attach(Boston)

# Open an editor of data
fix(Carseats)

# Writing R functions
RegPlot=function(x, y){
  fit <- lm(y~x)
  plot(x, y)
  abline(fit, col="red")
}

attach(Carseats)

RegPlot(Price,Sales)

RegPlot=function(x, y, ...){
  fit <- lm(y~x)
  plot(x, y, ...)
  abline(fit, col="red")
}

RegPlot(Price, Sales, xlab="Price", ylab="Sales", 
        col="blue", pch=20)



###############################################################################
### Machine Learning with R

## Vectors -----

# create vectors of data for three medical patients
subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

## Factors -----

# add gender factor
gender <- factor(c("MALE", "FEMALE", "MALE"))
gender

# add blood type factor
blood <- factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))
blood

## Lists -----

# display information for a patient
subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]

# create list for a patient
subject1 <- list(fullname = subject_name[1], temperature = temperature[1], flu_status = flu_status[1],
                 gender = gender[1], blood = blood[1])

# display the patient
subject1

## Data frames -----

# create a data frame from medical patient data

pt_data <- data.frame(subject_name, temperature, flu_status, gender, blood, stringsAsFactors = FALSE)

# display the data frame
pt_data

## accessing a data frame

# get a single column
pt_data$subject_name

# get several columns by specifying a vector of names
pt_data[c("temperature", "flu_status")]

# this is the same as above, extracting temperature and flu_status
pt_data[2:3]

# accessing by row and column
pt_data[1, 2]

# accessing several rows and several columns using vectors
pt_data[c(1, 3), c(2, 4)]

## Leave a row or column blank to extract all rows or columns

# column 1, all rows
pt_data[, 1]
# row 1, all columns
pt_data[1, ]
# all rows and all columns
pt_data[ , ]

# the following are equivalent to the above
pt_data[c(1, 3), c("temperature", "gender")]
pt_data[-2, c(-1, -3, -5)]

## Matrixes -----

# create a 2x2 matrix
m <- matrix(c('a', 'b', 'c', 'd'), nrow = 2)
m

# equivalent to the above
m <- matrix(c('a', 'b', 'c', 'd'), ncol = 2)
m

# extract values from matrixes
m[1, 1]
m[3, 2]

# extract rows
m[1, ]

# extract columns
m[, 1]

## data exploration example using used car data
usedcars <- read.csv("./chapter 2/usedcars.csv", stringsAsFactors = FALSE)

# get structure of used car data
str(usedcars)

# use quantile to calculate five-number summary
quantile(usedcars$price)

# the 99th percentile
quantile(usedcars$price, probs = c(0.01, 0.99))

# quintiles
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# boxplot of used car prices and mileage
boxplot(usedcars$price, main="Boxplot of Used Car Prices", ylab="Price ($)")

boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage", ylab="Odometer (mi.)")

# histograms of used car prices and mileage
hist(usedcars$price, main = "Histogram of Used Car Prices", xlab = "Price ($)")

hist(usedcars$mileage, main = "Histogram of Used Car Mileage", xlab = "Odometer (mi.)")

# variance and standard deviation of the used car data
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)

## Exploring numeric variables -----

# one-way tables for the used car data
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# compute table proportions
model_table <- table(usedcars$model)
prop.table(model_table)

# round the data
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

## Exploring relationships between variables -----

# scatterplot of price vs. mileage
plot(x = usedcars$mileage, y = usedcars$price, main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)", ylab = "Used Car Price ($)")

# new variable indicating conservative colors
usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")

# checking our variable
table(usedcars$conservative)

# Crosstab of conservative by model
CrossTable(x = usedcars$model, y = usedcars$conservative)
CrossTable(x = usedcars$model, y = usedcars$conservative,  chisq = TRUE)

### Machine Learning with R
### Chapter 3: Classification using Nearest Neighbors --------------------

## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
wbcd <- read.csv("./chapter 3/wisc_bc_data.csv", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# In plain English, this command applies the  normalize() function to columns  2
# through  31 in the  wbcd data frame, converts the resulting list to a data frame, and
# assigns it the name  wbcd_n . The  _n suffix is used here as a reminder that the values
# in  wbcd have been normalized.

# confirm that normalization worked
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)

table(wbcd_test_pred, wbcd_test_labels)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

##### Chapter 4: Classification using Naive Bayes --------------------

# Conditional probability with Bayes' theorem
# The relationships between dependent events can be described using Bayes'
# theorem, as shown in the following formula. The notation P(A|B) can be read as
# the probability of event A given that event B occurred. This is known as conditional
# probability, since the probability of A is dependent (that is, conditional) on what
# happened with event B.

## Example: Filtering spam SMS messages ----
## Step 2: Exploring and preparing the data ---- 

# read the sms data into the sms data frame
sms_raw <- read.csv("./chapter 4/sms_spam.csv", stringsAsFactors = FALSE)

# examine the structure of the sms data
str(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

# The first step in processing text data involves creating a corpus, which refers to a
# collection of text documents. In our project, a text document refers to a single SMS
# message. We'll build a corpus containing the SMS messages in the training data
# using the following command:

# build a corpus using the text mining (tm) package
library(tm)
print(vignette("tm"))
sms_corpus <- Corpus(VectorSource(sms_raw$text))

# This command uses two functions. First, the  Corpus() function creates an R object
# to store text documents. This function takes a parameter specifying the format of
# the text documents to be loaded. Since we have already read the SMS messages and
# stored them in an R vector, we specify  VectorSource() , which tells  Corpus() to use
# the messages in the vector  sms_train$text . The  Corpus() function stores the result
# in an object named  sms_corpus .
# The Corpus() function is extremely flexible and can read
# documents from many different sources such as PDFs and
# Microsoft Word documents. To learn more, examine the
# Data Import section in the tm package vignette using the
# command: print(vignette("tm"))

# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:3])

# The function  tm_map() provides a method for transforming (that is, mapping) a
# tm corpus. We will use this to clean up our corpus using a series of transformation
# functions, and save the result in a new object called  corpus_clean .
# First, we will convert all of the SMS messages to lowercase and remove any numbers:

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)

# A common practice when analyzing text data is to remove filler words such as to,
# and, but, and or. These are known as stop words. Rather than define a list of stop
# words ourselves, we will use the  stopwords() function provided by the  tm package.
# It contains a set of numerous stop words. To see them all, type stopwords() at
# the command line. As we did before, we'll use the tm_map() function to apply this
# function to the data:

corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())

# We'll also remove punctuation:

corpus_clean <- tm_map(corpus_clean, removePunctuation)

# Now that we have removed numbers, stop words, and punctuation, the text
# messages are left with blank spaces where these characters used to be. The last step
# then is to remove additional whitespace, leaving only a single space between words.

corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# examine the clean corpus
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# word cloud visualization
library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


##### Chapter 5: Classification using Decision Trees and Rules -------------------

#### Part 1: Decision Trees -------------------
  
## Understanding Decision Trees ----
# calculate entropy of a two-class segment
-0.60 * log2(0.60) - 0.40 * log2(0.40)

curve(-x * log2(x) - (1 - x) * log2(1 - x), col="red", xlab = "x", ylab = "Entropy", lwd=4)

## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
credit <- read.csv("./chapter 5/credit.csv")
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]

# compare the credit and credit_rand data frames
summary(credit$amount)
summary(credit_rand$amount)
head(credit$amount)
head(credit_rand$amount)

# split the data frames
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Step 5: Improving model performance ----

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# boosted decision tree with 100 trials (not shown in text)
credit_boost100 <- C5.0(credit_train[-17], credit_train$default,
                        trials = 100)
credit_boost_pred100 <- predict(credit_boost100, credit_test)
CrossTable(credit_test$default, credit_boost_pred100,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
summary(credit_boost100)

## Making some mistakes more costly than others
# create a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# The C5.0() function will generate a model using classification rules if you specify rules = TRUE
# when training the model.

#### Part 2: Rule Learners -------------------

## Example: Identifying Poisonous Mushrooms ----
## Step 2: Exploring and preparing the data ---- 
mushrooms <- read.csv("./chapter 5/mushrooms.csv", stringsAsFactors = TRUE)

# examine the structure of the data frame
str(mushrooms)

# drop the veil_type feature
mushrooms$veil_type <- NULL

# examine the class distribution
table(mushrooms$type)

## Step 3: Training a model on the data ----
library(RWeka)

# train OneR() on the data
mushroom_1R <- OneR(type ~ ., data = mushrooms)

## Step 4: Evaluating model performance ----
mushroom_1R
summary(mushroom_1R)

## Step 5: Improving model performance ----
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip)

# Rule Learner Using C5.0 Decision Trees (not in text)
library(C50)
mushroom_c5rules <- C5.0(type ~ odor + gill_size, data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)

##### Chapter 6: Regression Methods -------------------

#### Part 1: Linear Regression -------------------

## Understanding regression ----
## Example: Space Shuttle Launch Data ----
launch <- read.csv("./chapter 6/challenger.csv")

# estimate beta manually
b <- cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
b

# estimate alpha manually
a <- mean(launch$distress_ct) - b * mean(launch$temperature)
a

# calculate the correlation of launch data
r <- cov(launch$temperature, launch$distress_ct) / (sd(launch$temperature) * sd(launch$distress_ct))
r
cor(launch$temperature, launch$distress_ct)

# computing the slope using correlation
r * (sd(launch$distress_ct) / sd(launch$temperature))

# confirming the regression line using the lm function (not in text)
model <- lm(distress_ct ~ temperature, data = launch)
model
summary(model)

# creating a simple multiple regression function
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  solve(t(x) %*% x) %*% t(x) %*% y
}

# examine the launch data
str(launch)

# test regression model with simple linear regression
reg(y = launch$distress_ct, x = launch[3])

# use regression model with multiple regression
reg(y = launch$distress_ct, x = launch[3:5])

# confirming the multiple regression result using the lm function (not in text)
model <- lm(distress_ct ~ temperature + pressure + launch_id, data = launch)
model

## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("./chapter 6/insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable
summary(insurance$charges)

# histogram of insurance charges
hist(insurance$charges)

# table of region
table(insurance$region)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "charges")])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "charges")])

# more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

## Step 3: Training a model on the data ----
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model <- lm(charges ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
ins_model

# By default, R uses the first level of the factor variable as the reference. If you would prefer to use another level, 
# the relevel() function can be used to specify the reference group manually.

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)

summary(ins_model2)

#### Part 2: Regression Trees and Model Trees -------------------

## Understanding regression trees and model trees ----
## Example: Calculating SDR ----
# set up the data
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)

# compute the SDR
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))

# The SDR for the split on A was about 1.2 versus 1.4 for the split on B. Since the standard deviation was reduced more for B, 
# the decision tree would use B first. It results in slightly more homogeneous sets than does A.

# compare the SDR for each split
sdr_a
sdr_b

## Example: Estimating Wine Quality ----
## Step 2: Exploring and preparing the data ----
wine <- read.csv("./chapter 6/whitewines.csv")

# examine the wine data
str(wine)

# the distribution of quality ratings
hist(wine$quality)

# Compared to other types of machine learning models, one of the advantages of trees is that they can handle many types of 
# data without preprocessing. This means we do not need to normalize or standardize the features.

# summary statistics of the wine data
summary(wine)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

## Step 3: Training a model on the data ----
# regression tree using rpart
library(rpart)
m.rpart <- rpart(quality ~ ., data = wine_train)

# get basic information about the tree
m.rpart

# get more detailed information about the tree
summary(m.rpart)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

# a few adjustments to the diagram
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

## Step 4: Evaluate model performance ----

# generate predictions for the testing dataset
p.rpart <- predict(m.rpart, wine_test)

# compare the distribution of predicted values vs. actual values
summary(p.rpart)
summary(wine_test$quality)

# compare the correlation
cor(p.rpart, wine_test$quality)

# function to calculate the mean absolute error
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))  
}

# mean absolute error between predicted and actual values
MAE(p.rpart, wine_test$quality)

# mean absolute error between actual values and mean value
mean(wine_train$quality) # result = 5.87
mean_abserror(5.87, wine_test$quality)

## Step 5: Improving model performance ----
# train a M5' Model Tree
library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)

# display the tree
m.m5p

# get a summary of the model's performance
summary(m.m5p)

# generate predictions for the model
p.m5p <- predict(m.m5p, wine_test)

# summary statistics about the predictions
summary(p.m5p)

# correlation between the predicted and true values
cor(p.m5p, wine_test$quality)

# mean absolute error of predicted and true values
# (uses a custom function defined above)
MAE(wine_test$quality, p.m5p)

##### Chapter 7: Neural Networks and Support Vector Machines -------------------

##### Part 1: Neural Networks -------------------
## Example: Modeling the Strength of Concrete  ----

## Step 2: Exploring and preparing the data ----
# read in data and examine structure
concrete <- read.csv("./chapter 7/concrete.csv")
str(concrete)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# Any transformation applied to the data prior to training the model will have to be applied in reverse later on in order
# to convert back to the original units of measurement. To facilitate the rescaling, it is wise to save the original data, or
# at least the summary statistics of the original data.

# confirm that the range is now between zero and one
summary(concrete_norm$strength)

# compared to the original minimum and maximum
summary(concrete$strength)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Step 3: Training a model on the data ----
# train the neuralnet model
library(neuralnet)

# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
                            data = concrete_train)


# visualize the network topology
plot(concrete_model)

## Step 4: Evaluating model performance ----
# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

##### Part 2: Support Vector Machines -------------------
## Example: Optical Character Recognition ----

## Step 2: Exploring and preparing the data ----
# read in data and examine structure
letters <- read.csv("./chapter 7/letterdata.csv")
str(letters)

# SVM learners require all features to be numeric, and moreover, that each feature is scaled to a fairly small interval. 

# divide into training and test data
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

## Step 3: Training a model on the data ----
# begin by training a simple linear SVM
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")

# look at basic information about the model
letter_classifier

## Step 4: Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# look only at agreement vs. non-agreement
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## Step 5: Improving model performance ----
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

##### Chapter 8: Association Rules -------------------

## Example: Identifying Frequently-Purchased Groceries ----
## Step 2: Exploring and preparing the data ----

# Apriori algorithm employs a simple a priori belief as guideline for reducing the association rule search space: 
# all subsets of a frequent itemset must also be frequent. This heuristic is known as the Apriori property. 

# load the grocery data into a sparse matrix
library(arules)
groceries <- read.transactions("./chapter 8/groceries.csv", sep = ",")
summary(groceries)

# look at the first five transactions
inspect(groceries[1:5])

# examine the frequency of items
itemFrequency(groceries[, 1:3])

# plot the frequency of items
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# a visualization of the sparse matrix for the first five transactions
image(groceries[1:5])

# visualization of a random sample of 100 transactions
image(sample(groceries, 100))

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
apriori(groceries)


# set better support and confidence levels to learn more rules

# you could argue that if an item is purchased twice a day (about 60 times) then it may be worth taking a look at. 
# From there, it is possible to calculate the support level needed to find only rules matching at least that many
# transactions. Since 60 out of 9,835 equals 0.006, we'll try setting the support there first.
# We'll start with a confidence threshold of 0.25, which means that in order to be included in the results, the rule has 
# to be correct at least 25 percent of the time. This will eliminate the most unreliable rules while allowing some room 
# for us to modify behavior with targeted promotions.
# In addition to the minimum support and confidence, it is helpful to set  minlen = 2 to eliminate rules that contain 
# fewer than two items. 

groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules

## Step 4: Evaluating model performance ----
# summary of grocery association rules
summary(groceryrules)

# look at the first three rules
inspect(groceryrules[1:3])

# A large lift value is therefore a strong indicator that a rule is important, and reflects a true connection between the items.

## Step 5: Improving model performance ----

# sorting grocery rules by lift
inspect(sort(groceryrules, by = "lift")[1:5])

# finding subsets of rules containing any berry items
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

# The keyword  items , explained previously, matches an item appearing anywhere in the rule. To limit the subset to where 
# the match occurs only on the left or right-hand side, use  lhs and  rhs instead.
# .  The operator  %in% means that at least one of the items must be found in the list you defined. If you wanted any rules 
#    matching either berries or yogurt, you could write  items %in% c("berries", "yogurt") .
# .  Additional operators are available for partial matching ( %pin% ) and complete matching ( %ain% ). Partial matching 
#    allows you to find both  citrus fruit and  tropical fruit using one search:  items %pin% "fruit" . Complete matching 
#    requires that all listed items are present. For instance,  items %ain%   c("berries", "yogurt") finds only rules with 
#    both berries and yogurt.
# .  Subsets can also be limited by support, confidence, or lift. For instance, confidence > 0.50 would limit you to rules 
#    with confidence greater than 50 percent.
# .  Matching criteria can be combined with standard R logical operators such as and ( & ), or ( | ), and not ( ! ).

# writing the rules to a CSV file
write(groceryrules, file = "./chapter 8/groceryrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

##### Chapter 9: Clustering with k-means -------------------

## Example: Finding Teen Market Segments ----
## Step 2: Exploring and preparing the data ----

# it's a good idea to try a cluster analysis more than once to test the robustness of your findings.

# Choosing the number of clusters requires a delicate balance. Setting the k to be very large will improve the
# homogeneity of the clusters, and at the same time, it risks overfitting the data.

# In general, it may be wise to spend little time worrying about getting k exactly right.

teens <- read.csv("./chapter 9/snsdata.csv")
str(teens)

# look at missing data for female variable
table(teens$gender)
table(teens$gender, useNA = "ifany")

# look at missing data for age variable
summary(teens$age)

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)

summary(teens$age)

# reassign missing gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# The first statement assigns  teens$female the value  1 if gender is equal to  F and the gender is not equal to  NA , 
# otherwise it assigns the value  0 . The  is.na() function tests whether gender is equal to  NA . If  is.na() returns  
# TRUE , then the  teens$no_gender variable is assigned  1 , otherwise it is assigned the value  0 . 

# check our recoding work
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# finding the mean age by cohort
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) 

# age by cohort
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# calculating the expected age for each person
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))

# ave() function, which returns a vector with the group means repeated such that the result is equal in length to 
# the original vector:

# To impute these means onto the missing values, we need one more  ifelse() call to use the  ave_age value only if 
# the original age value was  NA :

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# check the summary results to ensure missing values are eliminated
summary(teens$age)

## Step 3: Training a model on the data ----
interests <- teens[5:40]

# z-score standardization rescales features such that they have a mean of zero and a standard deviation of one.
# As lapply() returns a matrix, it must be coaxed back to data frame form using the as.data.frame() function.

interests_z <- as.data.frame(lapply(interests, scale))

teen_clusters <- kmeans(interests_z, 5)

## Step 4: Evaluating model performance ----
# look at the size of the clusters
teen_clusters$size

# look at the cluster centers
teen_clusters$centers

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
teens$cluster <- teen_clusters$cluster

# look at the first five records
teens[1:5, c("cluster", "gender", "age", "friends")]

# mean age by cluster
aggregate(data = teens, age ~ cluster, mean)

# proportion of females by cluster
aggregate(data = teens, female ~ cluster, mean)

# mean number of friends by cluster
aggregate(data = teens, friends ~ cluster, mean)

##### Chapter 10: Evaluating Model Performance -------------------

## Confusion matrixes in R ----
sms_results <- read.csv("./chapter 10/sms_results.csv")
head(sms_results)

#  the predicted and actual values differ
head(subset(sms_results, actual_type != predict_type))

# specifying vectors
table(sms_results$actual_type, sms_results$predict_type)
# using the formula interface
xtabs(~ actual_type + predict_type, sms_results)
# using the CrossTable function
library(gmodels)
CrossTable(sms_results$actual_type, sms_results$predict_type)

# accuracy and error rate calculation --
# accuracy
(154 + 1202) / (154 + 1202 + 5 + 29)
# error rate
(5 + 29) / (154 + 1202 + 5 + 29)
# error rate = 1 - accuracy
1 - 0.9755396

## Beyond accuracy: other performance measures ----
library(caret)


# The  caret package adds yet another function for creating a confusion matrix. As
# shown in the following commands, the syntax is similar to  table() , but the positive
# outcome must be specified. Because the SMS classifier is intended to detect spam, we
# will set  positive = "spam" 
confusionMatrix(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# Kappa statistic
# example using SMS classifier
pr_a <- 0.865 + 0.111
pr_a

pr_e <- 0.868 * 0.886 + 0.132 * 0.114
pr_e

k <- (pr_a - pr_e) / (1 - pr_e)
k

library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type))

library(irr)
kappa2(sms_results[1:2])

# Sensitivity and specificity
# example using SMS classifier
sens <- 154 / (154 + 29)
sens

spec <- 1202 / (1202 + 5)
spec

# example using the caret package
library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

# Precision and recall
prec <- 154 / (154 + 5)
prec

rec <- 154 / (154 + 29)
rec

# example using the caret package
library(caret)
posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")

# F-measure
f <- (2 * prec * rec) / (prec + rec)
f

f2 <- (2 * 154) / (2 * 154 + 5 + 29)
f2

## Visualizing Performance Tradeoffs ----
library(ROCR)
pred <- prediction(predictions = sms_results$prob_spam, labels = sms_results$actual_type)

# ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)

# add a reference line to the graph
abline(a = 0, b = 1, lwd = 2, lty = 2)

# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
as.numeric(perf.auc@y.values)

# partitioning data
library(caret)
credit <- read.csv("./chapter 10/credit.csv")

# Holdout method
# using random IDs
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500],]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

# The first line creates a vector of randomly ordered row IDs from 1 to 1000. These
# IDs are then used to divide the credit data frame into 500, 250, and 250 records
# comprising the training, validation, and test datasets.

# Although, on average, a random sample will contain
# roughly the same proportion of class values as the full dataset, stratified random
# sampling ensures that the generated random partitions have approximately the same
# proportion of each class as the full dataset.
# The  caret package provides a  createDataPartition() function that will create
# partitions based on stratified holdout sampling. 

# using caret function
in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ]
credit_test <-  credit[-in_train, ]

# 10-fold CV
folds <- createFolds(credit$default, k = 10)
str(folds)
credit01_train <- credit[folds$Fold01, ]
credit01_test <-  credit[-folds$Fold01, ]

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----
library(caret)
library(C50)
library(irr)

set.seed(123)
folds <- createFolds(credit$default, k = 10)

# we will apply a series of identical steps to the list of folds using the  lapply()
# function. As shown in the following code, because there is no existing function that
# does exactly what we need, we must define our own function to pass to  lapply() .
# Our custom function divides the  credit data frame into training and test data, creates
# a decision tree using the  C5.0() function on the training data, generates a set of
# predictions from the test data, and compares the predicted and actual values using
# the  kappa2() function:
  
cv_results <- lapply(folds, function(x) {
  credit_train <- credit[x, ]
  credit_test <- credit[-x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))

##### Chapter 11: Improving Model Performance -------------------

# load the credit dataset
credit <- read.csv("./chapter 11/credit.csv")
library(caret)

## Creating a simple tuned model ----
# automated parameter tuning of C5.0 decision tree 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0")

# summary of tuning results
m

# apply the best C5.0 candidate model to make predictions
p <- predict(m, credit)
table(p, credit$default)

# obtain predicted classes
head(predict(m, credit, type = "raw"))

# obtain predicted probabilities
head(predict(m, credit, type = "prob"))


## Customizing the tuning process ----
# use trainControl() to alter resampling strategy
ctrl <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

# use expand.grid() to create grid of tuning parameters
grid <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

# look at the result of expand.grid()
grid

# customize train() with the control list and grid of parameters 
set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

## Bagging ----
# Using the ipred bagged decision trees
library(ipred)
set.seed(300)
mybag <- bagging(default ~ ., data = credit, nbagg = 25)
credit_pred <- predict(mybag, credit)
table(credit_pred, credit$default)

# estimate performance of ipred bagged trees
library(caret)
set.seed(300)
ctrl <- trainControl(method = "cv", number = 10)
train(default ~ ., data = credit, method = "treebag",
      trControl = ctrl)

# Using caret's more general bagging function

#  bag() function. It includes out-of-the-box support for a handful of models,
# though it can be adapted to more types with a bit of additional effort. The  bag()
# function uses a control object to configure the bagging process. It requires the
# specification of three functions: one for fitting the model, one for making predictions,
# and one for aggregating the votes.

# create a bag control object using svmBag

# The  bag() function requires us to provide functionality for training the SVMs, making predictions, and
# counting votes. the  caret package's built-in  svmBag list object supplies three functions we can use for this purpose:

str(svmBag)

# By looking at the  svmBag$fit function, we see that it simply calls the  ksvm()
# function from the  kernlab package and returns the result:

svmBag$fit

# The  pred and  aggregate functions for  svmBag are also similarly straightforward. By
# studying these functions and creating your own in the same format, it is possible to
# use bagging with any machine learning algorithm you would like.
# The caret package also includes example objects for bags
# of naive Bayes models (nbBag), decision trees (ctreeBag), and neural networks (nnetBag).
# Applying the three functions in the  svmBag list, we can create a bagging
# control object:

bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)

# fit the bagged svm model
set.seed(300)
svmbag <- train(default ~ ., data = credit, "bag",
                trControl = ctrl, bagControl = bagctrl)

svmbag

## Boosting ----
# Using AdaBoost.M1
# (please note that this example is not in the text) 
library(adabag)
set.seed(300)
bst <- boosting.cv(default ~ ., data = credit, mfinal = 50)
bst$confusion
bst$error

## Random Forests ----
# random forest with default settings
library(randomForest)
set.seed(300)
rf <- randomForest(default ~ ., data = credit)
rf

library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10)

# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)
m_rf

# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(.model = "tree",
                        .trials = c(10, 20, 30, 40),
                        .winnow = "FALSE")

set.seed(300)
m_c50 <- train(default ~ ., data = credit, method = "C5.0",
               metric = "Kappa", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50

############################################################################### 

## Computing for Data Analysis - Video [2-5]

x <- c(0.5, 0.6)      ## numeric
x <- c(TRUE, FALSE)   ## logical
x <- c(T, F)          ## logical
x <- c("a", "b", "c") ## character
x <- 9:29             ## integer
x <- c(1+0i, 2+4i)    ## complex

x <- c(1,3, 5)
y <- c(3, 2, 10)
class(rbind(x, y))

x <- 0:6
class(x)
# [1] "integer"
as.numeric(x)
# [1] 0 1 2 3 4 5 6
as.logical(x)
# [1] FALSE TRUE TRUE TRUE TRUE TRUE TRUE
as.character(x)
# [1] "0" "1" "2" "3" "4" "5" "6"
as.complex(x)
# [1] 0+0i 1+0i 2+0i 3+0i 4+0i 5+0i 6+0i

m <- matrix(nrow = 2, ncol = 3)
m
# [,1] [,2] [,3]
# [1,] NA NA NA
# [2,] NA NA NA
dim(m)
# [1] 2 3
attributes(m)
# $dim
# [1] 2 3

m <- matrix(1:6, nrow = 2, ncol = 3)
m
# [,1] [,2] [,3]
# [1,] 1 3 5
# [2,] 2 4 6

m <- 1:10
m
# [1] 1 2 3 4 5 6 7 8 9 10
dim(m) <- c(2, 5)
m
# [,1] [,2] [,3] [,4] [,5]
# [1,] 1 3 5 7 9
# [2,] 2 4 6 8 10

x <- 1:3
y <- 10:12
cbind(x, y) # Column bind
x y
# [1,] 1 10
# [2,] 2 11
# [3,] 3 12
rbind(x, y)
# [,1] [,2] [,3]
# x 1 2 3
# y 10 11 12


## Herramienta mineria de datos: rattle()

## Paquete googlevis()

x <- factor(c("yes", "yes", "no", "yes", "no"))
x
# [1] yes yes no yes no
# Levels: no yes
table(x)
# x
# no yes
# 2 3
unclass(x)
# [1] 2 2 1 2 1
# attr(,"levels")
# [1] "no" "yes"

x <- factor(c("yes", "yes", "no", "yes", "no"), levels = c("yes", "no"))
x
# [1] yes yes no yes no
# Levels: yes no

## is.na() is used to test objects if they are NA
## is.nan() is used to test for NaN
## NA values have a class also, so there are integer NA, character NA, etc.
## A NaN value is also NA but the converse is not true

## Using factors with labels is better than using integers because factors are 
## self-describing; having a variable that has values \Male" and \Female" is better
## than a variable that has values 1 and 2.

## Data frames are used to store tabular data
## They are represented as a special type of list where every element of the list has to have the same length
## Each element of the list can be thought of as a column and the length of each element of the list is the number of rows
## Unlike matrices, data frames can store dierent classes of objects in each column (just like lists); matrices must have every element be the same class
## Data frames also have a special attribute called row.names
## Data frames are usually created by calling read.table() or read.csv() Can be converted to a matrix by calling data.matrix()

x <- data.frame(foo = 1:4, bar = c(T, T, F, F))
x
# foo bar
# 1 1 TRUE
# 2 2 TRUE
# 3 3 FALSE
# 4 4 FALSE
nrow(x)
# [1] 4
ncol(x)
# [1] 2

x <- 1:3
names(x)
# NULL
names(x) <- c("foo", "bar", "norf")
x
# foo bar norf
# 1 2 3
names(x)
# [1] "foo" "bar" "norf"


m <- matrix(1:4, nrow = 2, ncol = 2)
dimnames(m) <- list(c("a", "b"), c("c", "d"))
m
# c d
# a 1 3
# b 2 4


# Subsetting
x <- c("a", "b", "c", "c", "d", "a")
x[1]
# [1] "a"
x[2]
# [1] "b"
x[1:4]
# [1] "a" "b" "c" "c"
x[x > "a"]
# [1] "b" "c" "c" "d"
u <- x > "a"
u
# [1] FALSE TRUE TRUE TRUE TRUE FALSE
x[u]
# [1] "b" "c" "c" "d"

# Subsetting a Matrix: Matrices can be subsetted in the usual way with (i ; j) type indices.
x <- matrix(1:6, 2, 3)
x
# [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
x[1, 2]
# [1] 3
x[2, 1]
# [1] 2
# Indices can also be missing.
x[1, ]

# By default, when a single element of a matrix is retrieved, it is returned as a vector of
# length 1 rather than a 11 matrix. This behavior can be turned o by setting drop = FALSE.
x <- matrix(1:6, 2, 3)
x[1, 2]
# [1] 3
x[1, 2, drop = FALSE]
# [,1]
# [1,] 3

# [1] 1 3 5
x[, 2]
# [1] 3 4

# Subsetting Lists
x <- list(foo = 1:4, bar = 0.6)
x[1]
# $foo
# [1] 1 2 3 4
x[[1]]
# [1] 1 2 3 4
x$bar
# [1] 0.6
x[["bar"]]
# [1] 0.6
x["bar"]
# $bar
# [1] 0.6

# Extracting multiple elements of a list.
x <- list(foo = 1:4, bar = 0.6, baz = "hello")
x[c(1, 3)]
# $foo
# [1] 1 2 3 4
# $baz
# [1] "hello"

# Partial matching of names is allowed with [[ and $.
x <- list(aardvark = 1:5)
x$a
# [1] 1 2 3 4 5
x[["a"]]
# NULL
x[["a", exact = FALSE]]
# [1] 1 2 3 4 5

# Removing NA Values: A common task is to remove missing values (NAs).
x <- c(1, 2, NA, 4, NA, 5)
bad <- is.na(x)
x[!bad]
# [1] 1 2 4 5


# What if there are multiple things and you want to take the subset with no missing values?
x <- c(1, 2, NA, 4, NA, 5)
y <- c("a", "b", NA, "d", NA, "f")
good <- complete.cases(x, y)
good
# [1] TRUE TRUE FALSE TRUE FALSE TRUE
x[good]
# [1] 1 2 4 5
y[good]
# [1] "a" "b" "d" "f"

airquality[1:6, ]
# Ozone Solar.R Wind Temp Month Day
# 1 41 190 7.4 67 5 1
# 2 36 118 8.0 72 5 2
# 3 12 149 12.6 74 5 3
# 4 18 313 11.5 62 5 4
# 5 NA NA 14.3 56 5 5
# 6 28 NA 14.9 66 5 6
good <- complete.cases(airquality)
airquality[good, ][1:6, ]
# Ozone Solar.R Wind Temp Month Day
# 1 41 190 7.4 67 5 1
# 2 36 118 8.0 72 5 2
# 3 12 149 12.6 74 5 3
# 4 18 313 11.5 62 5 4
# 7 23 299 8.6 65 5 7
# 8 19 99 13.8 59 5 8

# read.table, read.csv, for reading tabular data              vs write.table
# readLines, for reading lines of a text file                 vs writeLines
# source, for reading in R code files (inverse of dump)       vs dump
# dget, for reading in R code files (inverse of dput)         vs sput
# load, for reading in saved workspaces                       vs save
# unserialize, for reading single R objects in binary form    vs serialize

# The read.table function is one of the most commonly used functions for reading data. It has a few important arguments:
# file, the name of a file, or a connection
# header, logical indicating if the file has a header line
# sep, a string indicating how the columns are separated
# colClasses, a character vector indicating the class of each column in the dataset
# nrows, the number of rows in the dataset
# comment.char, a character string indicating the comment character
# skip, the number of lines to skip from the beginning
# stringsAsFactors, should character variables be coded as factors?

# Connections: In general, connections are powerful tools that let you navigate files or other external
# objects. In practice, we often don't need to deal with the connection interface directly.
con <- file("foo.txt", "r")
data <- read.csv(con)
close(con)
# is the same as
data <- read.csv("foo.txt")

# readLines can be useful for reading in lines of webpages
# This might take time
con <- url("http://www.jhsph.edu", "r")
x <- readLines(con)

head(x)
# [1] "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\">"
# [2] ""
# [3] "<html>"
# [4] "<head>"
# [5] "\t<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\"

# read.csv is identical to read.table except that the default separator is a comma.
# Set comment.char = "" if there are no commented lines in your file.
# Use the colClasses argument. Specifying this option instead of using the default
# can make 'read.table' run MUCH faster, often twice as fast. In order to use this
# option, you have to know the class of each column in your data frame. If all of
# the columns are "numeric", for example, then you can just set colClasses =
# "numeric". A quick an dirty way to figure out the classes of each column is the following:
initial <- read.table("datatable.txt", nrows = 100)
classes <- sapply(initial, class)
tabAll <- read.table("datatable.txt", colClasses = classes)
head(tabAll)
str(tabAll)
# Set nrows. This doesn't make R run faster but it helps with memory usage. A
# mild overestimate is okay. You can use the Unix tool wc to calculate the number
# of lines in a file.

version
str(.Platform)

# Interfaces to the Outside World
# Data are read in using connection interfaces. Connections can be made to files (most common) or to other more exotic things.
# file, opens a connection to a file
# gzfile, opens a connection to a file compressed with gzip
# bzfile, opens a connection to a file compressed with bzip2
# url, opens a connection to a webpage

str(lm)
str(str)

x <- rnorm(100,2,4)
summary(x)
str(x)

library(datasets)
head(airquality)
str(airquality)
s <- split(airquality, airquality$Month)
str(s)

## Computing for Data Analysis - Video [3-1]

# Control structures in R allow you to control the flow of execution of the program, depending on runtime conditions. Common structures are
# if, else: testing a condition
# for: execute a loop a xed number of times
# while: execute a loop while a condition is true
# repeat: execute an innite loop
# break: break the execution of a loop
# next: skip an interation of a loop
# return: exit a function

# if(<condition1>) {
  ## do something
# } else if(<condition2>) {
  ## do something different
# } else {
  ## do something different
# }

cube <- function(x, n) {
  x^3
}

pow <- function(x = 4, n = 3) {
  x^n
}

for(i in 1:10) {print(i)}

for(i in 1:10) {
  print(i)
}

# These three loops have the same behavior.
x <- c("a", "b", "c", "d")
for(i in 1:4) {
  print(x[i])
}
for(i in seq_along(x)) {
  print(x[i])
}
for(letter in x) {
  print(letter)
}
for(i in 1:4) print(x[i])

# for loops can be nested.
x <- matrix(1:6, 2, 3)
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }
}
# Be careful with nesting though. Nesting beyond 2{3 levels is often very dicult to read/understand.

# While loops begin by testing a condition. If it is true, then they execute the loop body.
# Once the loop body is executed, the condition is tested again, and so forth.
count <- 0
while(count < 10) {
  print(count)
  count <- count + 1
}
# While loops can potentially result in innite loops if not written properly. Use with care!

# Sometimes there will be more than one condition in the test.
z <- 5
while(z >= 3 && z <= 10) {
  print(z)
  coin <- rbinom(1, 1, 0.5)
  if(coin == 1) { ## random walk
    z <- z + 1
  } else {
    z <- z - 1
  }
}
# Conditions are always evaluated from left to right.

# Repeat initiates an innite loop; these are not commonly used in statistical applications
# but they do have their uses. The only way to exit a repeat loop is to call break.
x0 <- 1
tol <- 1e-8
repeat {
  x1 <- computeEstimate()
  if(abs(x1 - x0) < tol) {
    break
  } else {
    x0 <- x1
  }
}

## Computing for Data Analysis - Video [3-2]

# The formals function returns a list of all the formal arguments of a function

# R functions arguments can be matched positionally or by name. So the following calls
# to sd are all equivalent
mydata <- rnorm(100)
sd(mydata)
sd(x = mydata)
sd(x = mydata, na.rm = FALSE)
sd(na.rm = FALSE, x = mydata)
sd(na.rm = FALSE, mydata)
# Even though itÂs legal, I donÂt recommend messing around with the order of the
# arguments too much, since it can lead to some confusion.

x <- 1:10
if(x > 5) {
  x <- 0
}

args(lm)
# function (formula, data, subset, weights, na.action,
#          method = "qr", model = TRUE, x = FALSE,
#          y = FALSE, qr = TRUE, singular.ok = TRUE,
#          contrasts = NULL, offset, ...)
# The following two calls are equivalent.
lm(data = mydata, y ~ x, model = FALSE, 1:100)
lm(y ~ x, mydata, 1:100, model = FALSE)

# The ... argument indicate a variable number of arguments that are usually passed on
# to other functions.
# ... is often used when extending another function and you donÂt want to copy
# the entire argument list of the original function
myplot <- function(x, y, type = "l", ...) {
  plot(x, y, type = type, ...)
}

args(plot.default)

# When R tries to bind a value to a symbol, it searches through a series of
# environments to find the appropriate value. When you are working on the command
# line and need to retrieve the value of an R object, the order is roughly
# 1 Search the global environment for a symbol name matching the one requested.
# 2 Search the namespaces of each of the packages on the search list
# The search list can be found by using the search function.
search()

# The global environment or the userÂs workspace is always the first element of the
# search list and the base package is always the last.
# The order of the packages on the search list matters!
# UserÂs can configure which packages get loaded on startup so you cannot assume
# that there will be a set list of packages available.
# When a user loads a package with library the namespace of that package gets
# put in position 2 of the search list (by default) and everything else gets shifted
# down the list.
# Note that R has separate namespaces for functions and non-functions so itÂs
# possible to have an object named c and a function named c.

f <- function(x) {x*x}
f
environment(f)
parent.env(environment(f))
# <environment: package:RCurl>
#   attr(,"name")
# [1] "package:RCurl"
# attr(,"path")
# [1] "C:/Users/enero/Documents/R/win-library/3.0/RCurl"

make.power <- function(n) {
  pow <- function(x) {
    x^n
  }
  pow
}
# This function returns another function as its value.
cube <- make.power(3)
square <- make.power(2)
cube(3)
# [1] 27
square(3)
# [1] 9

ls(environment(cube))
# [1] "n" "pow"
get("n", environment(cube))
# [1] 3
ls(environment(square))
# [1] "n" "pow"
get("n", environment(square))
# [1] 2

## Computing for Data Analysis - Video [3-4]

# Maximizing a Normal Likelihood
# Write a ÂconstructorÂ function
make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)) {
  params <- fixed
  function(p) {
    params[!fixed] <- p
    mu <- params[1]
    sigma <- params[2]
    a <- -0.5*length(data)*log(2*pi*sigma^2)
    b <- -0.5*sum((data-mu)^2) / (sigma^2)
    -(a + b)
  }
}
# Note: Optimization functions in R minimize functions, so you need to use the
# negative log-likelihood.

set.seed(1); normals <- rnorm(100, 1, 2)
nLL <- make.NegLogLik(normals)
nLL
# function(p) {
#  params[!fixed] <- p
#  mu <- params[1]
#  sigma <- params[2]
#  a <- -0.5*length(data)*log(2*pi*sigma^2)
#  b <- -0.5*sum((data-mu)^2) / (sigma^2)
#  -(a + b)
#}
# <environment: 0x165b1a4>
ls(environment(nLL))
# [1] "data" "fixed" "params"

optim(c(mu = 0, sigma = 1), nLL)$par
# mu sigma
# 1.218239 1.787343
# Fixing sigma=2

nLL <- make.NegLogLik(normals, c(FALSE, 2))
optimize(nLL, c(-1, 3))$minimum
# [1] 1.217775

# Fixing mu=1

nLL <- make.NegLogLik(normals, c(1, FALSE))
optimize(nLL, c(1e-6, 10))$minimum
# [1] 1.800596

nLL <- make.NegLogLik(normals, c(1, FALSE))
x <- seq(1.7, 1.9, len = 100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = "l")

nLL <- make.NegLogLik(normals, c(FALSE, 2))
x <- seq(0.5, 1.5, len = 100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = "l")

library(datasets)
data(iris)
?iris

mean(iris$Sepal.Length)
mean(iris$Sepal.Length[iris$Species=="virginica"])

## Computing for Data Analysis - Video [3-5]

# Looping on the Command Line
# Writing for, while loops is useful when programming but not particularly easy when
# working interactively on the command line. There are some functions which implement
# looping to make life easier.
# lapply: Loop over a list and evaluate a function on each element
# sapply: Same as lapply but try to simplify the result
# apply: Apply a function over the margins of an array
# tapply: Apply a function over subsets of a vector
# mapply: Multivariate version of lapply
# An auxiliary function split is also useful, particularly in conjunction with lapply.

apply(iris[, 1:4], 2, mean)

# lapply takes three arguments: a list X, a function (or the name of a function) FUN,
# and other arguments via its ... argument. If X is not a list, it will be coerced to a list
# using as.list.
lapply
# function (X, FUN, ...)
# {
#  FUN <- match.fun(FUN)
#  if (!is.vector(X) || is.object(X))
#    X <- as.list(X)
#  .Internal(lapply(X, FUN))
#}

# lapply always returns a list, regardless of the class of the input.
x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)
# $a
# [1] 3
# $b
# [1] 0.0296824

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)

x <- 1:4
lapply(x, runif, min = 0, max = 10)

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
x

# An anonymous function for extracting the first column of each matrix.
lapply(x, function(elt) elt[,1])

# sapply will try to simplify the result of lapply if possible.
# If the result is a list where every element is length 1, then a vector is returned
# If the result is a list where every element is a vector of the same length (> 1), a matrix is returned.
# If it canÂt figure things out, a list is returned

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
lapply(x, mean)
sapply(x, mean)

library(datasets)
data(mtcars)

?mtcars

tapply(mtcars$mpg, mtcars$cyl, mean)
tapply(mtcars$cyl, mtcars$mpg, mean)

## Computing for Data Analysis - Video [3-6]

str(apply)
# function (X, MARGIN, FUN, ...)
# X is an array
# MARGIN is an integer vector indicating which margins should be ÂretainedÂ.
# FUN is a function to be applied
# ... is for other arguments to be passed to FUN

x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean)
# [1] 0.04868268 0.35743615 -0.09104379
# [4] -0.05381370 -0.16552070 -0.18192493
# [7] 0.10285727 0.36519270 0.14898850
# [10] 0.26767260
apply(x, 1, sum)

# For sums and means of matrix dimensions, we have some shortcuts.
# rowSums = apply(x, 1, sum)
# rowMeans = apply(x, 1, mean)
# colSums = apply(x, 2, sum)
# colMeans = apply(x, 2, mean)
# The shortcut functions are much faster, but you wonÂt notice unless youÂre using a large matrix.

tapply(mtcars$hp, mtcars$cyl, mean)[1]-tapply(mtcars$hp, mtcars$cyl, mean)[3]

x <- matrix(rnorm(200), 20, 10)
apply(x, 1, quantile, probs = c(0.25, 0.75))

## Computing for Data Analysis - Video [3-7]

x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10) 
# 3 gruops of numbers 1,2,3 repeated 10 times 111111111122222222223333333333
f
# [1] 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3 3
# [24] 3 3 3 3 3 3 3
# Levels: 1 2 3
tapply(x, f, mean)
# 1         2         3 
# 0.2633420 0.3647714 0.7799134
tapply(x, f, mean, simplify = FALSE)
# $`1`
# [1] 0.263342
# 
# $`2`
# [1] 0.3647714
# 
# $`3`
# [1] 0.7799134

tapply(x, f, range)

# split takes a vector or other objects and splits it into groups determined by a factor
# or list of factors.
str(split)
# function (x, f, drop = FALSE, ...)
# x is a vector (or list) or data frame
# f is a factor (or coerced to one) or a list of factors
# drop indicates whether empty factors levels should be dropped

x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
split(x, f)

# A common idiom is split followed by an lapply.
lapply(split(x, f), mean)

library(datasets)
head(airquality)

s <- split(airquality, airquality$Month)
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))

sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))

sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")], na.rm = TRUE))

## Computing for Data Analysis - Video [3-8]

# mapply is a multivariate apply of sorts which applies a function in parallel over a set of arguments.
str(mapply)
# function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE)
# FUN is a function to apply
# ... contains arguments to apply over
# MoreArgs is a list of other arguments to FUN.
# SIMPLIFY indicates whether the result should be simplified

# mapply
# The following is tedious to type
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))
# Instead we can do
mapply(rep, 1:4, 4:1)

noise <- function(n, mean, sd) {
  rnorm(n, mean, sd)
}
noise(5, 1, 2)
# [1] 2.4831198 2.4790100 0.4855190 -1.2117759
# [5] -0.2743532
noise(1:5, 1:5, 2)

mapply(noise, 1:5, 1:5, 2)

# Which is the same as
list(noise(1, 1, 2), noise(2, 2, 2),
     noise(3, 3, 2), noise(4, 4, 2),
     noise(5, 5, 2))

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)

## Computing for Data Analysis - Video [3-9]

# Indications that somethingÂs not right
# message: A generic notification/diagnostic message produced by the message function; execution of the function continues
# warning: An indication that something is wrong but not necessarily fatal; execution of the function continues; generated by the warning function
# error: An indication that a fatal problem has occurred; execution stops; produced by the stop function
# condition: A generic concept for indicating that something unexpected can occur; programmers can create their own conditions

## Computing for Data Analysis - Video [3-10]

# Debugging Tools in R
# The primary tools for debugging functions in R are
# traceback: prints out the function call stack after an error occurs; does nothing if thereÂs no error
# debug: flags a function for ÂdebugÂ mode which allows you to step through execution of a function one line at a time
# browser: suspends the execution of a function wherever it is called and puts the function in debug mode
# trace: allows you to insert debugging code into a function a specific places
# recover: allows you to modify the error behavior so that you can browse the function call stack
# These are interactive tools specifically designed to allow you to pick through a
# function. ThereÂs also the more blunt technique of inserting print/cat statements in the function.

## Computing for Data Analysis - Video [3-11]

mean(x)
# Error in mean(x) : object ÂxÂ not found
traceback()
# 1: mean(x)

lm(y ~ x)
# Error in eval(expr, envir, enclos) : object ÂyÂ not found
traceback()

options(error = recover)
read.csv("nosuchfile")

# Print class of all variables in dataset
sapply(loansData[1,],class)

## Computing for Data Analysis - Video [4-1]

# Generating Random Numbers
# Functions for probability distributions in R
# rnorm: generate random Normal variates with a given mean and standard deviation
# dnorm: evaluate the Normal probability density (with a given mean/SD) at a
# point (or vector of points)
# pnorm: evaluate the cumulative distribution function for a Normal distribution
# rpois: generate random Poisson variates with a given rate

# Probability distribution functions usually have four functions associated with them.
# The functions are prexed with a
# d for density
# r for random number generation
# p for cumulative distribution
# q for quantile function

# Working with the Normal distributions requires using these four functions
# dnorm(x, mean = 0, sd = 1, log = FALSE)
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# rnorm(n, mean = 0, sd = 1)

# Generating random Normal variates
x <- rnorm(10)
x
# [1] 1.38380206 0.48772671 0.53403109 0.66721944
# [5] 0.01585029 0.37945986 1.31096736 0.55330472
# [9] 1.22090852 0.45236742
x <- rnorm(10, 20, 2)
x
# [1] 23.38812 20.16846 21.87999 20.73813 19.59020
 #[6] 18.73439 18.31721 22.51748 20.36966 21.04371
summary(x)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# 18.32 19.73 20.55 20.67 21.67 23.39

# Setting the random number seed with set.seed ensures reproducibility
set.seed(1)

# Generating Random Numbers From a Linear Model
# Suppose we want to simulate from the following linear model
# y = b0 + b1x + e
# where e ~ N(0,  2^2). Assume x ~ N(0, 1^2), b0 = 0:5 and b1 = 2.
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# -6.4080 -1.5400 0.6789 0.6893 2.9300 6.5050
plot(x, y)

# What if x is binary?
set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# -3.4940 -0.1409 1.5770 1.4320 2.8400 6.9410
plot(x, y)

# Suppose we want to simulate from a Poisson model where
# Y  Poisson()
# log u = b0 + b1x
# and b0 = 0:5 and b1 = 0:3. We need to use the rpois function for this
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
# Min. 1st Qu. Median Mean 3rd Qu. Max.
# 0.00 1.00 1.00 1.55 2.00 6.00
plot(x, y)

# Random Sampling
# The sample function draws randomly from a specied set of (scalar) objects allowing you to sample from arbitrary distributions.
set.seed(1)
sample(1:10, 4)
# [1] 3 4 5 7
sample(1:10, 4)
# [1] 3 9 8 5
sample(letters, 5)
# [1] "q" "b" "e" "x" "p"
sample(1:10) ## permutation
# [1] 4 7 10 6 9 2 8 3 1 5
sample(1:10)
# [1] 2 3 4 1 9 5 10 8 6 7
sample(1:10, replace = TRUE) ## Sample w/replacement
# [1] 2 9 7 8 2 8 5 9 7 8

## Computing for Data Analysis - Video [4-2]

# The plotting and graphics engine in R is encapsulated in a few base and recommend packages:
# graphics: contains plotting functions for the ÂbaseÂ graphing systems, including plot, hist, boxplot and many others.
# lattice: contains code for producing Trellis graphics, which are independent of the ÂbaseÂ graphics system; includes functions like xyplot, bwplot, levelplot
# grid: implements a di???erent graphing system independent of the ÂbaseÂ system; the lattice package builds on top of grid; 
# we seldom call functions from the grid package directly
# grDevices: contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.

# When making a plot one must first make a few choices (not necessarily in this order):
# To what device will the plot be sent? The default in Unix is x11; on Windows it is windows; on Mac OS X it is quartz
# Is the plot for viewing temporarily on the screen, or will it eventually end up in a
# paper? Are you using it in a presentation? Plots included in a paper/presentation
# need to use a file device rather than a screen device.
# Is there a large amount of data going into the plot? Or is it just a few points?
# Do you need to be able to resize the graphic?

# Base graphics are usually constructed piecemeal, with each aspect of the plot
# handled separately through a series of function calls; this is often conceptually
# simpler and allows plotting to mirror the thought process
# Lattice/grid graphics are usually created in a single function call, so all of the
# graphics parameters have to specified at once; specifying everything at once
# allows R to automatically calculate the necessary spacings and font sizes.

# The base graphics system has many parameters that can set and tweaked; these parameters are documented in ?par;

# The par function is used to specify global graphics parameters that a???ect all plots in
# an R session. These parameters can often be overridden as arguments to specific plotting functions.
# pch: the plotting symbol (default is open circle)
# lty: the line type (default is solid line), can be dashed, dotted, etc.
# lwd: the line width, specified as an integer multiple
# col: the plotting color, specified as a number, string, or hex code; the colors function gives you a vector of colors by name
# las: the orientation of the axis labels on the plot
# bg: the background color
# mar: the margin size
# oma: the outer margin size (default is 0 for all sides)
# mfrow: number of plots per row, column (plots are filled row-wise)
# mfcol: number of plots per row, column (plots are filled column-wise)

# plot: make a scatterplot, or other type of plot depending on the class of the object being plotted
# lines: add lines to a plot, given a vector x values and a corresponding vector of y
# values (or a 2-column matrix); this function just connects the dots
# points: add points to a plot
# text: add text labels to a plot using specified x, y coordinates
# title: add annotations to x, y axis labels, title, subtitle, outer margin
# mtext: add arbitrary text to the margins (inner or outer) of the plot
# axis: adding axis ticks/labels

## Computing for Data Analysis - Video [4-3]

x <- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x,y)
par(mar=c(2,2,2,2)) # Set margins of plot
plot(x,y)
par(mar=c(4,4,2,2))
plot(x,y)
plot(x,y, pch=19)
example(points)   
plot(x,y, pch=20)
title("Scatterplot")
text(-2,-2,"Label")
legend("topleft", legend = "Data")
fit <- lm(y ~ x)
abline(fit)
abline(fit, lwd =3)
abline(fit, lwd =3, col="blue")
z <- rpois(100,2)
par(mfrom=c(2,1))
plot(x,y,pch=20)
plot(x,z,pch=19)
x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50, labels=c("Male", "Female"))
str(g)
plot(x,y)
plot(x,y,type="n") # prepare the plot buy do not print it
points(x[g=="Male"], y[g=="Male"],col="green")
points(x[g=="Female"], y[g=="Female"],col="blue")

## Computing for Data Analysis - Video [4-4]

# Lattice Functions
# xyplot: this is the main function for creating scatterplots
# bwplot: box-and-whiskers plots (ÂboxplotsÂ)
# histogram: histograms
# stripplot: like a boxplot but with actual points
# dotplot: plot dots on Âviolin stringsÂ
# splom: scatterplot matrix; like pairs in base graphics system
# levelplot, contourplot: for plotting ÂimageÂ data

# Lattice functions generally take a formula for their first argument, usually of the form
# y ~ x | f * g

# Lattice functions have a panel function which controls what happens inside each panel of the entire plot.
x <- rnorm(100)
y <- x + rnorm(100, sd = 0.5)
f <- gl(2, 50, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f)

# plot y vs. x conditioned on f with horizontal (dashed) line drawn at the median of y for each panel
xyplot(y ~ x | f,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.abline(h = median(y),
                      lty = 2)
       })

# Adding a regression line
xyplot(y ~ x | f,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, col = 2)
       })

## Computing for Data Analysis - Video [4-5]

package ? lattice
library(help = lattice)
data(environmental)
?env(environmental)
head(environmental)
xyplot(ozone ~ radiation, data = environmental)
xyplot(ozone ~ radiation, data = environmental, main="Ozone vs. Radiation")
xyplot(ozone ~ temperature, data = environmental)
summary(environmental$temperature)
temp.cut <- equal.count(environmental$temperature,4)
temp.cut
xyplot(ozone ~ radiation | temp.cut, data = environmental)
xyplot(ozone ~ radiation | temp.cut, data = environmental, layout = C(1,4))
xyplot(ozone ~ radiation | temp.cut, data = environmental, layout = C(1,4), as.table=TRUE)
xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table=TRUE)
xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table=TRUE,
       panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               fit <- lm(y ~ x)
               panel.abline(fit)
       })
xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table=TRUE, pch=20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         fit <- lm(y ~ x)
         panel.abline(fit, lmd=2)
       })
xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table=TRUE, pch=20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       })
xyplot(ozone ~ radiation | temp.cut, data = environmental, as.table=TRUE, pch=20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       }, xlab="Solar Radiation", ylab="Ozone (ppb)", 
       main="Ozone vs. Solar Radiation")

wind.cut <- equal.count(environmental$wind,4)
wind.cut

xyplot(ozone ~ radiation | temp.cut * wind.cut, data = environmental, as.table=TRUE, pch=20,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         panel.loess(x, y)
       }, xlab="Solar Radiation", ylab="Ozone (ppb)", 
       main="Ozone vs. Solar Radiation")

splom(~environmental)
histogram(~ temperature, data = environmental)
histogram(~ temperature | wind.cut, data = environmental)
histogram(~ ozone | wind.cut, data = environmental)
histogram(~ ozone | temp.cut * wind.cut, data = environmental)

## Computing for Data Analysis - Video [4-8]

# Mathematical Annotation

?plotmath

plot(0, 0, main = expression(theta == 0),
     ylab = expression(hat(gamma) == 0),
     xlab = expression(sum(x[i] * y[i], i==1, n)))
# Pasting strings together.
x <- rnorm(100)
hist(x,
     xlab=expression("The mean (" * bar(x) * ") is " *
                       sum(x[i]/n,i==1,n)))

# What if you want to use a computed value in the annotation?
x <- rnorm(100)
y <- x + rnorm(100, sd = 0.5)
plot(x, y,
     xlab=substitute(bar(x) == k, list(k=mean(x))),
     ylab=substitute(bar(y) == k, list(k=mean(y)))
)
# Or in a loop of plots
par(mfrow = c(2, 2))
for(i in 1:4) {
  x <- rnorm(100)
  hist(x, main=substitute(theta==num,list(num=i)))
}

?par
?plot
?xyplot
?plotmath
?axis

# A basic boxplot of the death rates by state can be made running the following code.
death <- outcome2[, 11]
state <- outcome2$State
boxplot(death ~ state)
# Add the following aspects to the plot
# Set the y-axis label to say "30-day Death Rate"
boxplot(death ~ state, ylab="30-day Death Rate")
# Set the title of the plot to be \Heart Attack 30-day Death Rate by State"
boxplot(death ~ state, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State")
# Set the x- and y-axis tick labels to be perpendicular to the axis so that the abbreviated names of all
# the states will appear on the plot. Use the par function to set this.
boxplot(death ~ state, ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State", las=2)


## Computing for Data Analysis - Video [5-1]

# The func1on colors()lists the names of colors you can use in any plotting function

## Computing for Data Analysis - Video [5-2]

# Dates are stored internally as the number of days since 1970-01-01
# Times are stored internally as the number of seconds since 1970-01-01

# Times in R
# Times can be coerced from a character string using the as.POSIXlt or as.POSIXct function.
x <- as.Date("1970-01-01")
x

x <- Sys.time()
x
## [1] "2013-01-23 15:19:11 EST"
p <- as.POSIXlt(x)
names(unclass(p))
## [1] "sec" "min" "hour" "mday" "mon"
## [6] "year" "wday" "yday" "isdst"
p$sec

# You can also use the POSIXct format.
x <- Sys.time()
x ## Already in `POSIXct' format
## [1] "2013-01-23 15:19:11 EST"
unclass(x)
## [1] 1358972352
x$sec
## Error: $ operator is invalid for atomic vectors
p <- as.POSIXlt(x)
p$sec
## [1] 11.88
## [1] 11.86

# Times in R
# Finally, there is the strptime function in case your dates are
# written in a dierent format
# datestring <- c("January 10, 2012 10:40", "December 9, 2011 x <- strptime(datestring, "%B %d, %Y %H:%M")
x
## [1] "2012-01-10 10:40:00" "2011-12-09 09:10:00"
class(x)
## [1] "POSIXlt" "POSIXt"
# I can never remember the formatting strings. Check ?strptime for details.


# Operations on Dates and Times
# You can use mathematical operations on dates and times. Well,
# really just + and -. You can do comparisons too (i.e. ==, <=)
x <- as.Date("2012-01-01")
y <- strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
x - y
## Warning: Incompatible methods ("-.Date",
## "-.POSIXt") for "-"
## Error: non-numeric argument to binary operator
x <- as.POSIXlt(x)
x - y
## Time difference of 356.3 days

# Operations on Dates and Times
# Even keeps track of leap years, leap seconds, daylight savings, and time zones.
x <- as.Date("2012-03-01")
y <- as.Date("2012-02-28")
x - y
## Time difference of 2 days
x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT")
y - x
## Time difference of 1 hours

## Computing for Data Analysis - Video [5-5]

# Regular Expression Functions
# The primary R functions for dealing with regular expressions are
# grep, grepl: Search for matches of a regular expression/pattern in a character
# vector; either return the indices into the character vector that match, the strings
# that happen to match, or a TRUE/FALSE vector indicating which elements match
# regexpr, gregexpr: Search a character vector for regular expression matches
# and return the indices of the string where the match begins and the length of the match
# sub, gsub: Search a character vector for regular expression matches and replace
# that match with another string
# regexec: Easier to explain through demonstration.

length(grep("iconHomicideShooting", homicides))
# [1] 228
length(grep("iconHomicideShooting|icon_homicide_shooting", homicides))
# [1] 1003
length(grep("Cause: shooting", homicides))
# [1] 228
length(grep("Cause: [Ss]hooting", homicides))
# [1] 1003
length(grep("[Ss]hooting", homicides))
# [1] 1005

i <- grep("[cC]ause: [Ss]hooting", homicides)
j <- grep("[Ss]hooting", homicides)
str(i)
# int [1:1003] 1 2 6 7 8 9 10 11 12 13 ...
str(j)
# int [1:1005] 1 2 6 7 8 9 10 11 12 13 ...
setdiff(i, j)
# integer(0)
setdiff(j, i)
# [1] 318 859
homicides[859]

# By default, grep returns the indices into the character vector where the regex pattern matches.
grep("^New", state.name)
# [1] 29 30 31 32
# Setting value = TRUE returns the actual elements of the character vector that match.
grep("^New", state.name, value = TRUE)
# [1] "New Hampshire" "New Jersey" "New Mexico" "New York"
# grepl returns a logical vector indicating which element matches.
grepl("^New", state.name)
# [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [25] FALSE FALSE FALSE FALSE TRUE TRUE TRUE TRUE FALSE FALSE FALSE FALSE
# [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
# [49] FALSE FALSE

# Some limitations of grep
# The grep function tells you which strings in a character vector match a certain
# pattern but it doesn't tell you exactly where the match occurs or what the match
# is (for a more complicated regex.
# The regexpr function gives you the index into each string where the match
# begins and the length of the match for that string.
# regexpr only gives you the rst match of the string (reading left to right).
# gregexpr will give you all of the matches in a given string.

# How can we nd the date of the homicide?
# Let's use the pattern
# <dd>[F|f]ound(.*)</dd>
# What does this look for?
regexpr("<dd>[F|f]ound(.*)</dd>", homicides[1:10])
# [1] 177 178 188 189 178 182 178 187 182 183
# attr(,"match.length")
# [1] 93 86 89 90 89 84 85 84 88 84
# attr(,"useBytes")
# [1] TRUE
substr(homicides[1], 177, 177 + 93 - 1)
# [1] "<dd>Found on January 1, 2007</dd><dd>Victim died at Shock Trauma</dd><dd>Cause: shooting</dd>"

# The previous pattern was too greedy and matched too much of the string. We need to
# use the ? metacharacter to make the regex \lazy".
regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:10])
# [1] 177 178 188 189 178 182 178 187 182 183
# attr(,"match.length")
# [1] 33 33 33 33 33 33 33 33 33 33
# attr(,"useBytes")
# [1] TRUE
substr(homicides[1], 177, 177 + 33 - 1)
# [1] "<dd>Found on January 1, 2007</dd>"

# One handy function is regmatches which extracts the matches in the strings for you
# without you having to use substr.
r <- regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:5])
regmatches(homicides[1:5], r)
# [1] "<dd>Found on January 1, 2007</dd>" "<dd>Found on January 2, 2007</dd>"
# [3] "<dd>Found on January 2, 2007</dd>" "<dd>Found on January 3, 2007</dd>"
# [5] "<dd>Found on January 5, 2007</dd>"

# sub/gsub
# Sometimes we need to clean things up or modify strings by matching a pattern and
# replacing it with something else. For example, how can we extract the data from this
# string?
x <- substr(homicides[1], 177, 177 + 33 - 1)
x
# [1] "<dd>Found on January 1, 2007</dd>"
# We want to strip out the stu surrounding the \January 1, 2007" piece.
sub("<dd>[F|f]ound on |</dd>", "", x)
# [1] "January 1, 2007</dd>"
gsub("<dd>[F|f]ound on |</dd>", "", x)
# [1] "January 1, 2007"

# sub/gsub can take vector arguments
r <- regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:5])
m <- regmatches(homicides[1:5], r)
m
# [1] "<dd>Found on January 1, 2007</dd>" "<dd>Found on January 2, 2007</dd>"
# [3] "<dd>Found on January 2, 2007</dd>" "<dd>Found on January 3, 2007</dd>"
# [5] "<dd>Found on January 5, 2007</dd>"
gsub("<dd>[F|f]ound on |</dd>", "", m)
# [1] "January 1, 2007" "January 2, 2007" "January 2, 2007" "January 3, 2007"
# [5] "January 5, 2007"
as.Date(d, "%B %d, %Y")
# [1] "2007-01-01" "2007-01-02" "2007-01-02" "2007-01-03" "2007-01-05"

# The regexec function works like regexpr except it gives you the indices for
# parenthesized sub-expressions.
regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1])
# [[1]]
# [1] 177 190
# attr(,"match.length")
# [1] 33 15
regexec("<dd>[F|f]ound on .*?</dd>", homicides[1])
# [[1]]
# [1] 177
# attr(,"match.length")
# [1] 33

# Now we can extract the string in the parenthesized sub-expression.
regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1])
# [[1]]
# [1] 177 190
# attr(,"match.length")
# [1] 33 15
substr(homicides[1], 177, 177 + 33 - 1)
# [1] "<dd>Found on January 1, 2007</dd>"
substr(homicides[1], 190, 190 + 15 - 1)
# [1] "January 1, 2007"

# Even easier with the regmatches function.
r <- regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1:2])
regmatches(homicides[1:2], r)
# [[1]]
# [1] "<dd>Found on January 1, 2007</dd>" "January 1, 2007"
# [[2]]
# [1] "<dd>Found on January 2, 2007</dd>" "January 2, 2007"

# Let's make a plot of monthly homicide counts
r <- regexec("<dd>[F|f]ound on (.*?)</dd>", homicides)
m <- regmatches(homicides, r)
dates <- sapply(m, function(x) x[2])
dates <- as.Date(dates, "%B %d, %Y")
hist(dates, "month", freq = TRUE)

## Computing for Data Analysis - Video [5-6]

# All objects in R have a class which can be determined by the class function
class(1)
# [1] "numeric"
class(TRUE)
# [1] "logical"
class(rnorm(100))
# [1] "numeric"
class(NA)
# [1] "logical"
class("foo")
# [1] "character"

head(getS3method("mean", "default"))


x[!is.na(x)] # All elements of x NOT NA

mat <- matrix(1:10, nrow=2, byrow=T)
mat
# [,1] [,2] [,3] [,4] [,5]
# [1,]    1    2    3    4    5
# [2,]    6    7    8    9   10
# SUMMING THE COLUMNS
apply(mat, 2, sum)
# [1]  7  9 11 13 15
# SUMMING THE ROWS
apply(mat, 1, sum)
# [1] 15 40
rowSums(mat) # the same
# [1] 15 40

# Multicore use
library(foreach)
library(doMC)
library(multicore)
ncore = multicore:::detectCores()
registerDoMC(cores = ncore)
results <- foreach(i = 1:5, .combine=c) %dopar% {
  i+i
}
results

# Euclidean distante function

Mr.Euclide <- function(x, y){
  dist <- sqrt(sum((x - y)^2))
  return(dist)
}

x <- c(1, 1)
y <- c(2, 2)
Mr.Euclide(x, y)
# [1] 1.414214

# Robust function

setClass("myObject", representation(vec = "numeric"), 
         prototype = prototype(vec = 0))
(vectorA <- new("myObject", vec=c(1, 1, 1)))


Mr.Euclide <- function(x, y){
  if(class(x)!="myObject" & class(y)!="myObject") stop("error") 
  dist <- sqrt(sum((x@vec - y@vec)^2))
  return(dist)
}

vectorB <- new("myObject", vec=c(2,2,2))
Mr.Euclide(vectorA,vectorB)

# Example of function and for. Iniciate the vector with fib <- rep(o,n)
fib <- function(n=20){
  if(n<3){
    return(c(1,1))
  } else{
    fib <- rep(0, n)
    for(i in 1:n){
      if(i <= 2){
        fib[i] <- 1
      } else{
        fib[i] <- fib[i-1] + fib[i-2]
      }
    }
  }
  return(fib)
}
fib(20)

# Some plots

library(ggplot2)
qplot(cyl, cty, data=mpg, geom="jitter")

# A very good plot with a standard desviation line with error
qplot(cyl, cty, data=mpg, geom="jitter") + geom_smooth(method="lm")

qplot(dose, len, data=ToothGrowth, geom="jitter", colour=factor(supp))

p <- ggplot(ToothGrowth, aes(as.factor(dose), len))
theme_set(theme_bw()) # theme_set(theme_grey())
p + geom_boxplot() + facet_grid(.~supp) + xlab("DOSE") + ylab('Tooth length')

# Welch T-test. Differences in distributions
t.test(MYC ~ condition, data = experiment, alternative = "two.sided")

# Test of the same variance in two groups
var.test(MYC ~ condition, data = experiment)

attach(intake)
head(intake)
# pre post
# 1 5260 3910
# 2 5470 4220
# 3 5640 3885
# 4 6180 5160
# 5 6390 5645
# 6 6515 4680
t.test(pre, post, paired = T)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(cystfibr[,3:5], lower.panel=panel.smooth, upper.panel=panel.cor)

# Obtener informacion local del sistema
Sys.getlocale()

beer = scan()
1: 3 4 1 1 3 4 3 3 1 3 2 1 2 1 2 3 2 3 1 1 1 1 4 3 1
26:
  Read 25 items
barplot(beer) # this isn't correct
barplot(table(beer)) # Yes, call with summarized data
barplot(table(beer)/length(beer)) # divide by n for proportion

table(beer)/length(beer)
# 1 2 3 4
# 0.40 0.16 0.32 0.12

# Pie charts
beer.counts = table(beer) # store the table result
pie(beer.counts) # first pie -- kind of dull
names(beer.counts) = c("domestic\n can","Domestic\n bottle",
                         "Microbrew","Import") # give names
pie(beer.counts) # prints out names
pie(beer.counts,col=c("purple","green2","cyan","white")) # now with colors

x=scan()
# 1: 29.6 28.2 19.6 13.7 13.0 7.8 3.4 2.0 1.9 1.0 0.7 0.4 0.4 0.3 0.3
# 16: 0.3 0.3 0.3 0.2 0.2 0.2 0.1 0.1 0.1 0.1 0.1
# 27:
#   Read 26 items
hist(x) # frequencies
hist(x,probability=TRUE) # proportions (or probabilities)
rug(jitter(x)) # add tick marks

library("Simple") # read in library for these notes
data(movies) # read in data set for gross.
names(movies)
# [1] "title" "current" "previous" "gross"
attach(movies) # to access the names above
boxplot(current,main="current receipts",horizontal=TRUE)
boxplot(gross,main="gross receipts",horizontal=TRUE)
detach(movies) # tidy up

# Frequency Polygons
x = c(.314,.289,.282,.279,.275,.267,.266,.265,.256,.250,.249,.211,.161)
tmp = hist(x) # store the results
lines(c(min(tmp$breaks),tmp$mids,max(tmp$breaks)),c(0,tmp$counts,0),type="l")

smokes = c("Y","N","N","Y","N","Y","Y","Y","N","Y")
amount = c(1,2,2,3,3,1,2,1,3,2)
table(smokes,amount)
# amount
# smokes 1 2 3
# N 0 2 2
# Y 3 2 1

# What would be nice to have are the marginal totals and the proportions. For example, what
# proportion of smokers study 5 hours or less. We know that this is 3 /(3+2+1) = 1/2, but how can we
# do this in R?
# The command prop.table will compute this for us. It needs to be told the table to work on, and
# a number to indicate if you want the row proportions (a 1) or the column proportions (a 2) the default
# is to just nd proportions.
tmp=table(smokes,amount) # store the table
old.digits = options("digits") # store the number of digits
options(digits=3) # only print 3 decimal places
prop.table(tmp,1) # the rows sum to 1 now
# amount
# smokes 1 2 3
# N 0.0 0.500 0.500
# Y 0.5 0.333 0.167
prop.table(tmp,2) # the columns sum to 1 now
# amount
# smokes 1 2 3
# N 0 0.5 0.667
# Y 1 0.5 0.333
prop.table(tmp)
# amount # all the numbers sum to 1
# smokes 1 2 3
# N 0.0 0.2 0.2
# Y 0.3 0.2 0.1
options(digits=old.digits) # restore the number of digits

# Plotting tabular data
barplot(table(smokes,amount))
barplot(table(amount,smokes))
smokes=factor(smokes) # for names
barplot(table(smokes,amount),
          + beside=TRUE, # put beside not stacked
          + legend.text=T) # add legend
barplot(table(amount,smokes),main="table(amount,smokes)",
            + beside=TRUE,
            + legend.text=c("less than 5","5-10","more than 10"))

# Handling bivariate data: categorical vs. numerical
x = c(5, 5, 5, 13, 7, 11, 11, 9, 8, 9)
y = c(11, 8, 4, 5, 9, 5, 10, 5, 4, 10)
boxplot(x,y)

# Creating new plots with plot and curve.
x=seq(0,4,by=.1) # create the x values
plot(x,x^2,type="l") # type="l" to make line
curve(x^2,0,4)

# Storing multivariate data in data frames
weight = c(150, 135, 210, 140)
height = c(65, 61, 70, 65)
gender = c("Fe","Fe","M","Fe")
study = data.frame(weight, height, gender) # make the data frame
study
study = data.frame(w=weight,h=height,g=gender)
row.names(study)<-c("Mary","Alice","Bob","Judy")
study[,1:2]
study['Mary',]

# Ways to view multivariate data

library(MASS);data(Cars93);attach(Cars93)
## make some categorical variables using cut
price = cut(Price,c(0,12,20,max(Price)))
levels(price)=c("cheap","okay","expensive"))
mpg = cut(MPG.highway,c(0,20,30,max(MPG.highway)))
levels(mpg) = c("gas guzzler","okay","miser"))
## now look at the relationships
table(Type)
# Type
# Compact Large Midsize Small Sporty Van
# 16 11 22 21 14 9
table(price,Type)
# Type
# price Compact Large Midsize Small Sporty Van
# cheap 3 0 0 18 1 0
# okay 9 3 8 3 9 8
# expensive 4 8 14 0 4 1
table(price,Type,mpg)

barplot(table(price,Type),beside=T) # the price by different types
barplot(table(Type,price),beside=T) # type by different prices

y=rnorm(1000) # 1000 random numbers
f=factor(rep(1:10,100)) # the number 1,2...10 100 times
boxplot(y ~ f,main="Boxplot of normal random data with model notation")

par(mfrow=c(1,3)) # 3 graphs per page
data(InsectSprays) # load in the data
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
simple.violinplot(count ~ spray, data = InsectSprays, col = "lightgray")
simple.densityplot(count ~ spray, data = InsectSprays)

data(emissions) # or read in from dataset
attach(emissions)
simple.scatterplot(perCapita,CO2)
title("GDP/capita vs. CO2 emissions 1999")
detach(emissions)

rankall <- function( outcome, num = "best") {
  
  outcomes<-data.frame( row.names=c("heart attack","heart failure","pneumonia"), n=c(11,17,23))
  if (!(outcome %in% row.names(outcomes)) ) stop("invalid outcome")
  
  rateh<-function(X) {
    vec<-data.frame(n=as.numeric(X[,outcomes[outcome,]]), name=X[,2])
    vec<-vec[complete.cases(vec),]
    if(num=="best") num<-1
    if(num=="worst") num<-dim(vec)[1]
    if(num>dim(vec)[1]) return (NA)
    as.character(vec[order(vec$n,vec$name),][num,2])
  }
  oh <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ohs<-split(oh, oh$State)
  s<-data.frame(hospital=sapply(ohs,rateh))
  s$state<-row.names(s)
  return(s)
}


attach(cars)
n=2
X= cars$speed 
Y=cars$dist
df=data.frame(X,Y)
vX=seq(min(X)-2,max(X)+2,length=n)
vY=seq(min(Y)-15,max(Y)+15,length=n)
mat=persp(vX,vY,matrix(0,n,n),zlim=c(0,.1),theta=-30,ticktype ="detailed", box = FALSE)
reggig=glm(Y~X,data=df,family=gaussian(link="identity"))
x=seq(min(X),max(X),length=501)
C=trans3d(x,predict(reggig,newdata=data.frame(X=x),type="response"),rep(0,length(x)),mat)
lines(C,lwd=2)
sdgig=sqrt(summary(reggig)$dispersion)
x=seq(min(X),max(X),length=501)
y1=qnorm(.95,predict(reggig,newdata=data.frame(X=x),type="response"), sdgig)
C=trans3d(x,y1,rep(0,length(x)),mat)
lines(C,lty=2)
y2=qnorm(.05,predict(reggig,newdata=data.frame(X=x),type="response"), sdgig)
C=trans3d(x,y2,rep(0,length(x)),mat)
lines(C,lty=2)
C=trans3d(c(x,rev(x)),c(y1,rev(y2)),rep(0,2*length(x)),mat)
polygon(C,border=NA,col="yellow")
C=trans3d(X,Y,rep(0,length(X)),mat)
points(C,pch=19,col="red")
n=8
vX=seq(min(X),max(X),length=n)
mgig=predict(reggig, newdata=data.frame(X=vX))
sdgig=sqrt(summary(reggig)$dispersion)
for(j in n:1){
  stp=251
  x=rep(vX[j],stp)
  y=seq(min(min(Y)-15,qnorm(.05,predict(reggig,newdata=data.frame(X=vX[j]),type="response"), sdgig)),max(Y)+15,length=stp)
  z0=rep(0,stp)
  z=dnorm(y, mgig[j], sdgig)
  C=trans3d(c(x,x),c(y,rev(y)),c(z,z0),mat)
  polygon(C,border=NA,col="light blue",density=40)
  C=trans3d(x,y,z0,mat)
  lines(C,lty=2)
  C=trans3d(x,y,z,mat)
  lines(C,col="blue")}

# Opening Large CSV Files in R

# Before heading home for the holidays, I had a large data set (1.6 GB with over 1.25 million rows) with columns of text 
# and integers ripped out of the company (Kwelia) Database and put into a .csv file since I was going to be offline a lot 
# over the break. I tried opening the csv file in the usual way:
  
all <- read.csv("file.csv")

# However it never finished even after letting it go all night. I also tried reading it into a SQLlite database first and 
# reading it out of that, but the file was so messy it kept coming back with errors. I finally got it read in to R by using 
# the ff package and the following code:
  
library("ff")
x <- read.csv.ffdf(file="file.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)

# Because the file was so messy, I had to turn off column classes (colClasses=NA) to have the read ignore giving each column 
# a class on the first 10,000. After reading the first 10,000 rows, the script then reads in chunks of 50,000 so as to not 
# completely overload the ram in my laptop. I also turned Verbose because it would drive me nuts to not be able to follow 
# the progress.

# Performance measurement and performance tuning

# As R is an interpreted language, if you don't write efficient functions, you could end up waiting a bit longer than expected, before any results are thrown back onto the console. It is not verbose and does not usually tell you what it is upto.
# We spent most of our two weeks performing this action as we came across performance bottlenecks in our scripts and could do with using the xapply() like functions. Applying them improved the performance of certain tasks from several hours to a reasonable number of minutes per execution.
# "Measure, don't guess." was the motto!
# Thanks to the sequence of calls to the proc.time() function, which we used voraciously to measure performances of the different blocks of code we thought needed attention.

startTimer <- proc.time()

# and 

proc.time() - startTimer

# This paid off at the end of the process as we were able to determine how much time it would take for the script to transform and validate the heaps of data we have been playing with.
# At the end of each such iteration we saw the stats in the below format. It got us excited if it was a low number and dejected if it wasn't to our liking:

# user  system elapsed 
# 87.085   0.694  87.877 

# Plot shp files

download.shapefile<-function(shape_url,layer,outfile=layer)
{
  #written by: jw hollister
  #Oct 10, 2012
  
  #set-up/clean-up variables
  if(length(grep("/$",shape_url))==0)
  {
    shape_url<-paste(shape_url,"/",sep="")
  }
  #creates vector of all possible shapefile extensions
  shapefile_ext<-c(".shp",".shx",".dbf",".prj",".sbn",".sbx",
                   ".shp.xml",".fbn",".fbx",".ain",".aih",".ixs",
                   ".mxs",".atx",".cpg")
  
  #Check which shapefile files exist
  if(require(RCurl))
  {
    xurl<-getURL(shape_url)
    xlogic<-NULL
    for(i in paste(layer,shapefile_ext,sep=""))
    {
      xlogic<-c(xlogic,grepl(i,xurl))
    }
    
    #Set-up list of shapefiles to download
    shapefiles<-paste(shape_url,layer,shapefile_ext,sep="")[xlogic]
    #Set-up output file names
    outfiles<-paste(outfile,shapefile_ext,sep="")[xlogic]   }
  #Download all shapefiles
  if(sum(xlogic)>0)
  {
    for(i in 1:length(shapefiles))
    {
      download.file(shapefiles[i],outfiles[i],
                    method="auto",mode="wb")
    }
  } else
  {
    stop("An Error has occured with the input URL
         or name of shapefile")
  }
  }

library(rgdal)
#Download the NH State Boundaries
download.shapefile("ftp://ftp.granit.sr.unh.edu/pub/GRANIT_Data/Vector_Data/Administrative_and_Political_Boundaries/d-nhsenatedists/2012",
                   "NHSenateDists2012")
#Read shapefiles in SpatialPolygonsDataFrame
NHBnd<-readOGR("E:/Varios/R/Archivos/varios","NHSenateDists2012")
#Plot it
plot(NHBnd)

# Histogramas

BMI<-rnorm(n=1000, m=24.2, sd=2.2) 
hist(BMI)
# It's useful to print out that information to understand the parameters of this histogram. 
histinfo<-hist(BMI)
histinfo
hist(BMI, breaks=20, main="Breaks=20")
hist(BMI, breaks=5, main="Breaks=5")
hist(BMI, breaks=c(17,20,23,26,29,32), main="Breaks is vector of breakpoints")
hist(BMI, breaks=seq(17,32,by=3), main="Breaks is vector of breakpoints")
# Instead of counting the number of datapoints per bin, R can give the probability densities using the freq=FALSE option:
hist(BMI, freq=FALSE, main="Density plot")

# we can make the histogram better looking by adjusting the x-axis, y-axis, axis labels, title, and color like this:
hist(BMI, freq=FALSE, xlab="Body Mass Index", main="Distribution of Body Mass Index", 
     col="lightgreen", xlim=c(15,35),  ylim=c(0, .20))

# Finally, I can add a nice normal distribution curve to this plot using the curve() function, in which I specify a normal 
# density function with mean and standard deviation that is equal to the mean and standard deviation of my data, and 
# I add this to my previous plot with a dark blue color and a line width of 2. You can play around with these options 
# to get the kind of line you want:
curve(dnorm(x, mean=mean(BMI), sd=sd(BMI)), add=TRUE, col="darkblue", lwd=2)

# 3D Mapping in R
# RGL is RÂs box of power-tool for 3D object rendering, with functionality for creating 3d mesh objects and curved surfaces, 
# and for using materials and directional lighting.  For example the line:
library(rgl)
plot3d(rnorm(100),rnorm(100),rnorm(100))

# OpenStreetMap provides a nice way to import map tiles via the OSM API (among others). 
# A helpful StackOverLoader (Spacedman) has provided this useful function for adding ÂzÂ values to OSM map objects, 
# enabling them to be plotted in 3d:

map3d <- function(map, ...){
  if(length(map$tiles)!=1){stop("multiple tiles not implemented") }
  nx = map$tiles[[1]]$xres
  ny = map$tiles[[1]]$yres
  xmin = map$tiles[[1]]$bbox$p1[1]
  xmax = map$tiles[[1]]$bbox$p2[1]
  ymin = map$tiles[[1]]$bbox$p1[2]
  ymax = map$tiles[[1]]$bbox$p2[2]
  xc = seq(xmin,xmax,len=ny)
  yc = seq(ymin,ymax,len=nx)
  colours = matrix(map$tiles[[1]]$colorData,ny,nx)
  m = matrix(0,ny,nx)
  surface3d(xc,yc,m,col=colours, ...)
}

# Improved Tree Maps with R
# Open R and install the following packages

install.packages("portfolio")
install.packages("RColorBrewer")

# now load them.

library(RColorBrewer)
library(portfolio)

# The next step is to load in the data file we are using. This is an edited version of the London Borough Profiles csv taken from the London Datastore. There are five columns of data. The three we are interested in are ÂpopÂ, ÂearningsÂ and ÂpartyÂ.

input<-read.csv("http://spatialanalysis.co.uk/wp-content/uploads/2011/08/tree_eg_data.csv")
attach(input)

# A treemap generally requires 4 pieces of information:
#   the item- in this case the London Borough's or "id"- each will be assigned a rectangle,
# a value to scale the size of the rectangle by- in this case the population or "pop",
# a value for assigning the colour- in this case the average earnings per person or "earnings",
# and a broader group to which the item belongs- in this case the ruling political party or "party".

# Armed with this we can simply used the map.market function from the portfolio package (installed earlier) to produce a treemap.

map.market(id, pop, party, earnings, lab = c(TRUE, TRUE), main="London Earnings, Population and Politics")

# shp Madrid mapas

rm(list=ls())
require(RCurl)
require(rjson)
require(igraph)
require(sp)
require(maptools)
require(rgdal)
require(plotrix)

# setwd("~/Dropbox/CIBBVA/Datathon/")
setwd("E:/Varios/R/Archivos/bbva/innovaRecommender")

# Codigo aÃ±adido para resolver un problema de conexiÃ³n
# error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed

library(RCurl) 
# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

# datos de proyecciones
# usados en datos cartogrÃÂ¡ficos
ED50 <- CRS(paste("+proj=utm +zone=30 +ellps=intl +units=m +no_defs"))
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# leer shapefile y transformar
# a longitud,latitud
map <- readShapeSpatial(fn="madrid/codigos_postales/200001414", proj4string=ED50)
map <- spTransform(map, WGS84)

# tomamos los centros y filtramos
# Madrid ciudad
labpts <- t(sapply(1:length(map), function(x) map@polygons[[x]]@labpt))
codenames <- map@data$GEOCODIGO
imadrid <- which((labpts[,1] > -3.833) & (labpts[, 1] < -3.523) & (labpts[, 2] < 40.564) & (labpts[, 2] > 40.316) )

#pintar mapa de CPS Madrid

mapMadrid <- map[imadrid, ]
plot(map)
plot(mapMadrid)

Mito solar plots for Mito PheWAS

# See also: https://github.com/hadley/ggplot2/wiki

# Import text file into R Studio with columns (at least): bp, pheno, p

setwd("E:/Varios/R/Archivos/varios")

# Load ggplot2
library(ggplot2)

# plink output files merged and pheno label added to specify results for cholesterol and diabetes
mitodata <- read.table(file="mitodata.txt", header=T)

# Sets gene names for bp ranges
addgenelabel <- function(bp,gene) { gene <- ifelse(bp < 577,gene <- "Control-Region", ifelse(bp < 648,gene <- "tRNA", ifelse(bp < 1602,gene <- "rRNA", ifelse(bp < 1671,gene <- "tRNA", ifelse(bp < 3230,gene <- "rRNA", ifelse(bp < 3305,gene <- "tRNA", ifelse(bp < 3307,gene <- "Non-Coding", ifelse(bp < 4263,gene<- "ND1", ifelse(bp < 4332,gene <- "tRNA", ifelse(bp < 4401,gene <- "tRNA", ifelse(bp < 4402,gene <- "Non-Coding", ifelse(bp < 4470,gene <- "tRNA", ifelse(bp < 5512,gene <- "ND2", ifelse(bp < 5580,gene <- "tRNA", ifelse(bp < 5587,gene <- "Non-Coding", ifelse(bp < 5656,gene <- "tRNA", ifelse(bp < 5657,gene <- "Non-Coding", ifelse(bp < 5730,gene <- "tRNA", ifelse(bp < 5826,gene <- "tRNA", ifelse(bp < 5892,gene <- "tRNA", ifelse(bp < 5904,gene <- "Non-Coding", ifelse(bp < 7446,gene <- "CO1", ifelse(bp < 7515,gene <- "tRNA", ifelse(bp < 7518,gene <- "Non-Coding", ifelse(bp < 7586,gene <- "tRNA", ifelse(bp < 8270,gene <- "CO2", ifelse(bp < 8295,gene <- "Non-Coding", ifelse(bp < 8365,gene <- "tRNA", ifelse(bp < 8366,gene <- "Non-Coding", ifelse(bp < 8573,gene <- "ATP8", ifelse(bp < 9208,gene <- "ATP6", ifelse(bp < 9991,gene <- "CO3", ifelse(bp < 10059,gene <- "tRNA", ifelse(bp < 10405,gene <- "ND3", ifelse(bp < 10470,gene <- "tRNA", ifelse(bp < 10767,gene <- "ND4L", ifelse(bp < 12138,gene <- "ND4", ifelse(bp < 12207,gene <- "tRNA", ifelse(bp < 12266,gene <- "tRNA", ifelse(bp < 12337,gene <- "tRNA", ifelse(bp < 14149,gene <- "ND5", ifelse(bp < 14674,gene <- "ND6", ifelse(bp < 14743,gene <- "tRNA", ifelse(bp < 14747,gene <- "Non-Coding", ifelse(bp < 15888,gene <- "CYB", ifelse(bp < 15954,gene <- "tRNA", ifelse(bp < 15956,gene <- "Non-Coding", ifelse(bp < 16024,gene <- "tRNA", ifelse(bp < 17000,gene <- "Control-Region") ))))))))))))))))))) ))))))))))))))))))) ))))))))) ) }

# Add gene names to each SNP
mitodata$gene <- addgenelabel(mitodata$bp,mitodata$gene)

# Display internal structure of mitodata
str(mitodata)

# Creates and stores negative log p as a new variable
mitodata$neglogp <- -1*log10(mitodata$p)

# Adds a significance threshold line at negative log of 0.05
mitodata$neglogpline <- -1*log10(0.05)

# Adds -3 label to y axis
mitodata$extraline <- -3

# Set colors for each gene
colours <- c("Control-Region" <- "lightblue4", "tRNA" <- "magenta4", "rRNA" <- "mediumaquamarine", "Non-Coding" <- "sienna4", "ND1" <- "magenta", "ND2" <- "mediumblue", "CO1" <- "olivedrab", "CO2" <- "orange2", "ATP8" <- "orchid4", "ATP6" <- "red3", "CO3" <- "royalblue2", "ND3" <- "palegreen4", "ND4L" <- "grey0", "ND4" <- "pink4", "ND5" <- "yellow4", "ND6" <- "steelblue4", "CYB" <- "tan","red")

# Create gene boundaries and lines
visibleboundaries <- c(1,576,1601,3229,4262,5511,7445,8269,9207,9990,10404,10766,12137,14148,14673,15887)

bdries <- data.frame(x = visibleboundaries, y=-.5)

bdries$gene <- addgenelabel(bdries$x,bdries$gene)

lines <- data.frame(x = seq(0,16567, by=1),y = 0)

lines$gene <- addgenelabel(lines$x, lines$gene)

# Plot everything and GO
ggplot(mitodata, aes(x = bp,y = neglogp,color = gene)) +
  geom_point()+ coord_polar(direction = -1) +
  geom_line(aes(x,1.30,color = "red"),data = lines) +
  #facet_grid(.~pheno) +
  geom_line(aes(y=extraline)) +
  geom_point(aes(x,y,color = gene),data=lines) +
  scale_colour_manual(values = colours,"Genes",breaks = c("Control-Region","tRNA","rRNA","Non-Coding","ND1","ND2","CO1","CO2","ATP8","ATP6","CO3","ND3","ND4L","ND4","ND5","ND6","CYB"),
                      labels = c("Control Region","tRNA","rRNA","Non-Coding","ND1","ND2","CO1","CO2","ATP8","ATP6","CO3","ND3","ND4L","ND4","ND5","ND6","CYB"))+
  xlab("Mitochondrial Base-Pair Location") +
  ylab("-log(p-value)") +
  ggtitle("Negative Log P-value of Mitochondrial Hits") +
  layer(geom="text",mapping =aes(x,y,label = x),data = bdries,size=2.5)

ggsave("solarplot.png", w=6, h=6, dpi=110)

## ggplot2 Version
# Data

# Let us begin by simulating our sample data of 3 factor variables and 4 numeric variables. 

## Simulate some data

## 3 Factor Variables
FacVar1 = as.factor(rep(c("level1", "level2"), 25))
FacVar2 = as.factor(rep(c("levelA", "levelB", "levelC"), 17)[-51])
FacVar3 = as.factor(rep(c("levelI", "levelII", "levelIII", "levelIV"), 13)[-c(51:52)])

## 4 Numeric Vars
set.seed(123)
NumVar1 = round(rnorm(n = 50, mean = 1000, sd = 50), digits = 2)  ## Normal distribution
set.seed(123)
NumVar2 = round(runif(n = 50, min = 500, max = 1500), digits = 2)  ## Uniform distribution
set.seed(123)
NumVar3 = round(rexp(n = 50, rate = 0.001))  ## Exponential distribution
NumVar4 = 2001:2050

simData = data.frame(FacVar1, FacVar2, FacVar3, NumVar1, NumVar2, NumVar3, NumVar4)

# Initialize the libraries used for this page

library(ggplot2)
library(reshape2)

# One Variable: Numeric Variable

ggplot(simData, aes(y = NumVar1, x = 1:nrow(simData), group = "NumVar1")) + 
  geom_point() + geom_line() + xlab("")  ## Index plot 

ggplot(simData, aes(x = NumVar1)) + geom_histogram()  ## histogram

ggplot(simData, aes(x = NumVar1)) + geom_density()  ## Kernel density plot

ggplot(simData, aes(x = factor(""), y = NumVar1)) + geom_boxplot() + xlab("")  ## box plot

# One Variable: Factor Variable

## barplot
ggplot(simData, aes(x = FacVar3)) + geom_bar()

## pie chart - Not the best graph --- use with caution
ggplot(simData, aes(x = factor(""), fill = FacVar3, label = FacVar3)) + geom_bar() + 
  coord_polar(theta = "y") + scale_x_discrete("")

# Two Variables: Two Numeric Variables

simtmp = simData[, c(4:5)]  ## 4th and 5th columns are NumVar1 and NumVar2
simtmp$index = 1:nrow(simtmp)
simtmpmelt = melt(simtmp, id = c("index"))

## line plots with observation number as index
ggplot(simtmpmelt, aes(y = value, x = index, color = variable)) + geom_point() + 
  geom_line() + xlab("")

## Let's draw density functions for NumVar1 & NumVar2
ggplot(simtmpmelt, aes(x = value, color = variable)) + geom_density()

## scatter plot
ggplot(simData, aes(x = NumVar1, y = NumVar2)) + geom_point()

# Two Variables: Two Factor Variables

## Mosaic plot: ggMMplot function - thanks to Edwin on Stackoverflow:
## http://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2

ggMMplot <- function(var1, var2) {
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))
  
  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq/plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 - 1]) + 
    plotData$marginVar1/2
  
  ggplot(plotData, aes(var1Center, var2Height)) + geom_bar(stat = "identity", 
                                                           aes(width = marginVar1, fill = var2), col = "Black") + geom_text(aes(label = as.character(var1), 
                                                                                                                                x = var1Center, y = 1.05))
}
ggMMplot(simData$FacVar2, simData$FacVar3)

## barplots

bartabledat = as.data.frame(table(simData$FacVar2, simData$FacVar3))  ## get the cross tab
ggplot(bartabledat, aes(x = Var2, y = Freq, fill = Var1)) + geom_bar(position = "dodge")  ## plot

ggplot(bartabledat, aes(x = Var2, y = Freq, fill = Var1)) + geom_bar()  ## stacked

bartableprop = as.data.frame(prop.table(table(simData$FacVar2, simData$FacVar3), 
                                        2) * 100)
ggplot(bartableprop, aes(x = Var2, y = Freq, fill = Var1)) + geom_bar()  ## Stacked 100%

# Two Variables: One Factor and One Numeric

## Box plots for the numeric var over the levels of the factor var
ggplot(simData, aes(x = FacVar1, y = NumVar1)) + geom_boxplot()

## density plot of numeric var across multiple levels of the factor var
ggplot(simData, aes(x = NumVar1, color = FacVar1)) + geom_density()

## Mean of one numeric var over levels of one factor var
meanagg = aggregate(simData$NumVar1, list(simData$FacVar3), mean)
ggplot(meanagg, aes(x = Group.1, y = x)) + geom_point() + coord_flip()  ## Dot Chart equivalent

ggplot(meanagg, aes(x = Group.1, y = x)) + geom_bar()  ## Bar plot

# Three Variables: Three Factor Variables

Threebartable = as.data.frame(table(simData$FacVar1, simData$FacVar2, simData$FacVar3))  ## CrossTab
ggplot(Threebartable, aes(x = Var3, y = Freq, fill = Var2)) + geom_bar(position = "dodge") + 
  facet_wrap(~Var1)  ## Bar plot with facetting

# Three Variables: One Numeric and Two Factor Variables

## boxplot of NumVar1 over an interaction of 6 levels of the combination of
## FacVar1 and FacVar2
ggplot(simData, aes(x = FacVar2, y = NumVar1, fill = FacVar1)) + geom_boxplot()

## Mean of 1 Numeric over levels of two factor vars
meanaggg = aggregate(simData$NumVar1, list(simData$FacVar1, simData$FacVar2), 
                     mean)
## Dot Chart equivalent
ggplot(meanaggg, aes(x = Group.2, y = x, color = Group.2)) + geom_point() + 
  coord_flip() + facet_wrap(~Group.1, ncol = 1)

## Interaction chart - line chart
ggplot(meanaggg, aes(x = Group.2, y = x, color = Group.1, group = Group.1)) + 
  geom_point() + geom_line()

## And bar plot
ggplot(meanaggg, aes(x = Group.2, y = x)) + geom_bar() + facet_wrap(~Group.1)

# Three Variables: Two Numeric and One Factor Variables

## Scatter plot with color identifying the factor variable
ggplot(simData, aes(x = NumVar1, y = NumVar2, color = FacVar1)) + geom_point()

# Three Variables: Three Numeric Variables

## NumVar4 is 2001 through 2050... possibly, a time variable - use that as
## the x-axis
simtmpp = simData[, c(4, 5, 7)]
simtmppmelt = melt(simtmpp, id = c("NumVar4"))
ggplot(simtmppmelt, aes(x = NumVar4, y = value, color = variable, group = variable)) + 
  geom_point() + geom_line()

## Extra: Stacked Area Graph
ggplot(simtmppmelt, aes(x = NumVar4, y = value, fill = variable)) + geom_area(position = "stack")

## Extra: 100% stacked area graph
ggplot(simtmppmelt, aes(x = NumVar4, y = value, fill = variable)) + geom_area(position = "fill")

## ## Bubble plot - scatter plot of NumVar1 and NumVar2 with individual
## observations sized by NumVar3
ggplot(simData, aes(x = NumVar1, y = NumVar2, size = NumVar3)) + geom_point()

# Scatterplot Matrix of all Numeric Vars, colored by a Factor variable

# Thanks to Gaston Sanchez for the function:
# http://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
makePairs <- function(data) {
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], x = data[, 
                                                                            xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, 
                                                                      i])
  }))
  list(all = all, densities = densities)
}

## expanding numeric columns for pairs plot
gg1 = makePairs(simData[, 4:7])

## new data frame
simDatabig = data.frame(gg1$all, simData[, 1:3])

## pairs plot
ggplot(simDatabig, aes_string(x = "x", y = "y")) + facet_grid(xvar ~ yvar, scales = "free") + 
  geom_point(aes(colour = FacVar2), na.rm = TRUE) + stat_density(aes(x = x, 
                                                                     y = ..scaled.. * diff(range(x)) + min(x)), data = gg1$densities, position = "identity", 
                                                                 colour = "grey20", geom = "line")

# rCharts Version
# Data

# Let us begin by simulating our sample data of 3 factor variables and 4 numeric variables. 

### Simulate some data

### 3 Factor Variables
FacVar1 = as.factor(rep(c("level1", "level2"), 25))
FacVar2 = as.factor(rep(c("levelA", "levelB", "levelC"), 17)[-51])
FacVar3 = as.factor(rep(c("levelI", "levelII", "levelIII", "levelIV"), 13)[-c(51:52)])

### 4 Numeric Vars
set.seed(123)
NumVar1 = round(rnorm(n = 50, mean = 1000, sd = 50), digits = 2)  ### Normal distribution
set.seed(123)
NumVar2 = round(runif(n = 50, min = 500, max = 1500), digits = 2)  ### Uniform distribution
set.seed(123)
NumVar3 = round(rexp(n = 50, rate = 0.001))  ### Exponential distribution
NumVar4 = 2001:2050

simData = data.frame(FacVar1, FacVar2, FacVar3, NumVar1, NumVar2, NumVar3, NumVar4)

# Initialize the libraries used for this page

library(rCharts)
library(reshape2)

# One Variable: Numeric Variable
# Index Plot using HighChart

simData$index = 1:nrow(simData)
h1 = hPlot(x = "index", y = "NumVar1", data = simData, type = "line")
# h1$publish('h1',host='gist') h1$save('h1.html',cdn=TRUE)

# Histogram using PolyChart

rp1 = rPlot(x = "bin(NumVar1,10)", y = "count(NumVar1)", data = simData, type = "bar")
# rp1$publish('rp1',host='gist') rp1$save('rp1.html',cdn=TRUE)

# Density plot using NVD3

dense = density(simData$NumVar1)
dense = data.frame(dense$x, dense$y)
n1 = nPlot(x = "dense.x", y = "dense.y", data = dense, type = "lineChart")
# n1$save('n1.html',cdn=TRUE) n1$publish('n1',host='gist')

# Boxplot using HighChart

### dummy variable created
simData$tmpFac = "tmp"
bwstats = setNames(as.data.frame(boxplot(NumVar1 ~ tmpFac, data = simData, plot = F)$stats), 
                   nm = NULL)
h2 = Highcharts$new()
h2$set(series = list(list(name = "NumVar1 Distribution", data = bwstats)))
h2$xAxis(categories = levels(simData$tmpFac), title = list(text = "Dummy Fac Var"))
h2$yAxis(title = list(text = "NumVar1"))
h2$chart(type = "boxplot")
# h2$publish('h2',host='gist') h2$save('h2.html',cdn=TRUE)

# One Variable: Factor Variable
# Bar plot using NVD3

bpf3 = data.frame(table(simData$FacVar3))
n2 = nPlot(x = "Var1", y = "Freq", data = bpf3, type = "discreteBarChart")
# n2$publish('n2',host='gist') n2$save('n2.html',cdn=TRUE)

# Pie Chart using NVD3

n3 = nPlot(x = "Var1", y = "Freq", data = bpf3, type = "pieChart")
# n3$publish('n3',host='gist') n3$save('n3.html',cdn=TRUE)

#Two Variables: Two Numeric Variables
# Index plots- line plots with observation number x-axis - Using HighCharts

simsub = simData[, c(4:5, 8)]
simsubmelt = melt(simsub, id = c("index"))

h2a = hPlot(x = "index", y = "value", group = "variable", data = simsubmelt, 
            type = "line")
# h2a$publish('h2a',host='gist') h2a$save('h2a.html',cdn=TRUE)

# Density plots for NumVar1 & NumVar2 using NVD3

dense$Var = "NumVar1"
names(dense) = c("x", "y", "Var")
dense2 = density(simData$NumVar2)
dense2 = data.frame(dense2$x, dense2$y)
dense2$Var = "NumVar2"
names(dense2) = c("x", "y", "Var")
densen1n2 = rbind(dense, dense2)
n4 = nPlot(x = "x", y = "y", group = "Var", data = densen1n2, type = "lineChart")
# n4$publish('n4',host='gist') n4$save('n4.html',cdn=TRUE)

# Scatter Chart using NVD3 - NumVar1 (x-Axis) versus NumVar2 (y-Axis)

n5 = nPlot(NumVar2 ~ NumVar1, data = simData, type = "scatterChart")
# n5$publish('n5',host='gist') n5$save('n5.html',cdn=TRUE)

# Two Variables: Two Factor Variables
# Mosaic plot/horizontal marimekko using Dimple

table = as.data.frame(table(simData$FacVar2, simData$FacVar3))  ### Crosstab

d1 = dPlot(Var2 ~ Freq, groups = "Var1", data = table, type = "bar")
d1$yAxis(type = "addAxis", measure = "Freq", showPercent = TRUE)
d1$xAxis(type = "addPctAxis")
d1$legend(x = 200, y = 10, width = 400, height = 20, horizontalAlign = "right")
# d1$publish('d1',host='gist') d1$save('d1.html',cdn=TRUE)

# Stacked Bar chart using UV Chart

u2 = uPlot("Var2", "Freq", data = bartabledat, group = "Var1", type = "StackedBar")
u2$config(graph = list(palette = "Lint"))
# u2$publish('u2',host='gist') u2$save('u2.html',cdn=TRUE)

# 100% Stacked Bar chart using UV Chart

u3 = uPlot("Var2", "Freq", data = bartabledat, group = "Var1", type = "PercentBar")
u3$config(graph = list(palette = "Soft"))
# u3$publish('u3',host='gist') u3$save('u3.html',cdn=TRUE)

# Two Variables: One Factor and One Numeric
# Box plots of numeric var over the levels of the factor var using HighCharts

bwstats = setNames(as.data.frame(boxplot(NumVar1 ~ FacVar1, data = simData, 
                                         plot = F)$stats), nm = NULL)
h3 = Highcharts$new()
h3$set(series = list(list(name = "NumVar1 Distribution", data = bwstats)))
h3$xAxis(categories = levels(simData$FacVar1), title = list(text = "FacVar1"))
h3$yAxis(title = list(text = "NumVar1"))
h3$chart(type = "boxplot")
# h3$publish('h3',host='gist') h3$save('h3.html',cdn=TRUE)

# Density plot numeric var across multiple levels of the factor var - NVD3

level1 = simData[simData$FacVar1 == "level1", ]
level2 = simData[simData$FacVar1 == "level2", ]

densen1 = density(level1$NumVar1)
densen1 = data.frame(densen1$x, densen1$y)
densen1$FacVar = "level1"
names(densen1) = c("x", "y", "FacVar")

densen2 = density(level2$NumVar1)
densen2 = data.frame(densen2$x, densen2$y)
densen2$FacVar = "level2"
names(densen2) = c("x", "y", "FacVar")

densen1and2 = rbind(densen1, densen2)
n6 = nPlot(x = "x", y = "y", group = "FacVar", data = densen1and2, type = "lineChart")
# n6$publish('n6',host='gist') n6$save('n6.html',cdn=TRUE)

##################

## scatterplots
plot(simData$NumVar1, simData$NumVar2)

# Two Variables: Two Factor Variables

## Mosaic plot
plot(table(simData$FacVar2, simData$FacVar3))

# Three Variables: Three Factor Variables

par(mfrow = c(1, 2))

bar1table = table(level1$FacVar2, level1$FacVar3)
barplot(bar1table, beside = TRUE, main = "FacVar1=level1")

bar2table = table(level2$FacVar2, level2$FacVar3)
barplot(bar2table, beside = TRUE, main = "FacVar1=level2", legend = levels(unique(level2$FacVar2)))

# Three Variables: One Numeric and Two Factor Variables

par(mfrow = c(1, 1))
## boxplot of NumVar1 over an interaction of 6 levels of the combination of
## FacVar1 and FacVar2
boxplot(NumVar1 ~ interaction(FacVar1, FacVar2), data = simData)

# Three Variables: Two Numeric and One Factor Variables

## Scatter plot with color identifying the factor variable
par(mfrow = c(1, 1))
plot(simData$NumVar1, simData$NumVar2, col = simData$FacVar1)
legend("topright", levels(simData$FacVar1), fill = simData$FacVar1)

## Bubble plot - scatter plot of NumVar1 and NumVar2 with individual
## observations sized by NumVar3
## http://flowingdata.com/2010/11/23/how-to-make-bubble-charts/
radius <- sqrt(simData$NumVar3/pi)
symbols(simData$NumVar1, simData$NumVar2, circles = radius, inches = 0.25, fg = "white", 
        bg = "red", main = "Sized by NumVar3")

# Scatterplot Matrix of all Numeric Vars, colored by a Factor variable

pairs(simData[, 4:7], col = simData$FacVar1)

# Plot axes with customized labels

dat = data.frame(label = sample(c(1, 2, 3), 150, replace = TRUE), val = rgamma(150, 50))

# A boxplot of the numeric variable val can be generated for each group.
boxplot(dat$val ~ dat$label)

# The simplest way to add a label for groups is treating the label variable as a factor. 
# This is the best choice also from a logical point of view.

dat$label = factor(dat$label, levels = 1:3, labels = c("First", "Second", "Third"))

# Wow! Now the plot show the group names.

boxplot(dat$val ~ dat$label)

# If labels change, factor can be modified in a one line.

levels(dat$label) = c("My First Group has a very long name",
  "My Second Group is a little bit more long",
  "The third one is as long as the first")

# And again, my plot!

boxplot(dat$val ~ dat$label)

# The labels are too long and the second one doesn't appear. Labels may be rotated, using the las parameter. 
# This argument specifies the style of axis labels. It can assume one of the following: 
# 0 (always parallel to the axis, which is the default), 1 (always horizontal), 2 (always perpendicular to the axis), 
# 3 (always vertical).

boxplot(dat$val ~ dat$label, las = 3)

# Labels have been rotated, but labels go outside the margin and disappears. Bottom margin should be increased. 
# This can be done using the mar argument of the par() function. The default is par(mar = c(5,4,4,2)) which means 
# that there are 5 lines at the bottom, 4 lines on the left, 4 lines in the top and 2 lines on the right. 
# The bottom margin can be increased to 20 lines.

op0 = par()    # Get current graphical parameters
op1 = op0$mar  # Get current margins in lines
op1[1] = 20    # Modify bottom margins to 20 (lines)
op1            # View bottom margins in lines

# If you want to specify margins in inches, use par(mai = c(bottom, left, top, right).

# Now I obtained the plot I was looking for!

par(mar = op1)
boxplot(dat$val ~ dat$label, las = 3)

install.packages("ggplot2")
library(ggplot2)
# http://had.co.nz/ggplot2

# qplot examples -------------------------------------------------------------

qplot(diamonds$cut, diamonds$carat)
qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, colour=clarity)
qplot(carat, price, data = diamonds, geom=c("point", "smooth"), method=lm)

qplot(carat, data = diamonds, geom="histogram")
qplot(carat, data = diamonds, geom="histogram", binwidth = 1)
qplot(carat, data = diamonds, geom="histogram", binwidth = 0.1)
qplot(carat, data = diamonds, geom="histogram", binwidth = 0.01)

# using ggplot() -------------------------------------------------------------
d <- ggplot(diamonds, aes(x=carat, y=price))
d + geom_point()
d + geom_point(aes(colour = carat))
d + geom_point(aes(colour = carat)) + scale_colour_brewer()

ggplot(diamonds) + geom_histogram(aes(x=price))

# Separation of statistcs and geometric elements -----------------------------

p <- ggplot(diamonds, aes(x=price))

p + geom_histogram()
p + stat_bin(geom="area")
p + stat_bin(geom="point")
p + stat_bin(geom="line")

p + geom_histogram(aes(fill = clarity))
p + geom_histogram(aes(y = ..density..))

# Setting vs mapping ---------------------------------------------------------
p <- ggplot(diamonds, aes(x=carat,y=price))

# What will this do?
p + geom_point(aes(colour = "green"))
p + geom_point(colour = "green")
p + geom_point(colour = colour)

#####################################
# http://www.r-bloggers.com/ggplot2-cheatsheet-for-scatterplots/
# ggplot2: Cheatsheet for Scatterplots

library(ggplot2)
library(gridExtra)
mtc <- mtcars

# Here's the basic syntax of a scatterplot. We give it a dataframe, mtc, and then in the aes() statement, 
# we give it an x-variable and a y-variable to plot. I save it as a ggplot object called p1, 
# because we are going to use this as the base and then layer everything else on top:

# Basic scatterplot
p1 <- ggplot(mtc, aes(x = hp, y = mpg))

# Now for the plot to print, we need to specify the next layer, which is how the symbols should look - 
# do we want points or lines, what color, how big. Let's start with points:

# Print plot with default points
p1 + geom_point()

# That's the bare bones of it. Now we have fun with adding layers. For each of the examples, 
# I'm going to use the grid.arrange() function in the gridExtra package to create multiple graphs in one panel to save space.
# >> Change color of points

# We start with options for colors just by adding how we want to color our points in the geom_point() layer:
  
p2 <- p1 + geom_point(color="red")            #set one color for all points
p3 <- p1 + geom_point(aes(color = wt))        #set color scale by a continuous variable
p4 <- p1 + geom_point(aes(color=factor(am)))  #set color scale by a factor variable

grid.arrange(p2, p3, p4, nrow=1)

#Change default colors in color scale
p1 + geom_point(aes(color=factor(am))) + scale_color_manual(values = c("orange", "purple"))

# plot of chunk unnamed-chunk-5 

# >> Change shape or size of points

# We're sticking with the basic p1 plot, but now changing the shape and size of the points:

p2 <- p1 + geom_point(size = 5)                   #increase all points to size 5
p3 <- p1 + geom_point(aes(size = wt))             #set point size by continuous variable
p4 <- p1 + geom_point(aes(shape = factor(am)))    #set point shape by factor variable    

grid.arrange(p2, p3, p4, nrow=1)

# Again, if we want to change the default shapes we can:

p1 + geom_point(aes(shape = factor(am))) + scale_shape_manual(values=c(0,2))

# >> Add lines to scatterplot

p2 <- p1 + geom_point(color="blue") + geom_line()                           #connect points with line
p3 <- p1 + geom_point(color="red") + geom_smooth(method = "lm", se = TRUE)  #add regression line
p4 <- p1 + geom_point() + geom_vline(xintercept = 100, color="red")         #add vertical line

grid.arrange(p2, p3, p4, nrow=1)

# You can also take out the points, and just create a line plot, and change size and color as before:

ggplot(mtc, aes(x = wt, y = qsec)) + geom_line(size=2, aes(color=factor(vs)))

# >> Change axis labels

# There are a few ways to do this. If you only want to quickly add labels you can use the labs() layer. 
# If you want to change the font size and style of the label, then you need to use the theme() layer. 
# More on this at the end of this post. If you want to change around the limits of the axis, 
# and exactly where the breaks are, you use the scale_x_continuous (and scale_y_continuous for the y-axis).

p2 <- ggplot(mtc, aes(x = hp, y = mpg)) + geom_point()
p3 <- p2 + labs(x="Horsepower", 
                y = "Miles per Gallon")                                  #label all axes at once
p4 <- p2 + theme(axis.title.x = element_text(face="bold", size=20)) + 
  labs(x="Horsepower")                                                   #label and change font size
p5 <- p2 + scale_x_continuous("Horsepower",
                              limits=c(0,400),
                              breaks=seq(0, 400, 50))                    #adjust axis limits and breaks

grid.arrange(p3, p4, p5, nrow=1)

# >> Change legend options

# We start off by creating a new ggplot base object, g1, which colors the points by a factor variable. 
# Then we show three basic options to modify the legend.

g1<-ggplot(mtc, aes(x = hp, y = mpg)) + geom_point(aes(color=factor(vs)))
g2 <- g1 + theme(legend.position=c(1,1),legend.justification=c(1,1))        #move legend inside                
g3 <- g1 + theme(legend.position = "bottom")                                #move legend bottom         
g4 <- g1 + scale_color_discrete(name ="Engine", 
                                labels=c("V-engine", "Straight engine"))    #change labels

grid.arrange(g2, g3, g4, nrow=1)

# If we had changed the shape of the points, we would use scale_shape_discrete() with the same options. 
# We can also remove the entire legend altogether by using theme(legend.position="none")

# Next we customize a legend when the scale is continuous:
  
g5<-ggplot(mtc, aes(x = hp, y = mpg)) + geom_point(size=2, aes(color = wt))
g5 + scale_color_continuous(name="Weight",                                     #name of legend
                            breaks = with(mtc, c(min(wt), mean(wt), max(wt))), #choose breaks of variable
                            labels = c("Light", "Medium", "Heavy"),            #label
                            low = "pink",                                      #color of lowest value
                            high = "red")                                      #color of highest value

# >> Change background color and style

# The look of the plot in terms of the background colors and style is the theme(). 
# I personally don't like the look of the default gray so here are some quick ways to change it. 
# I often the theme_bw() layer, which gets rid of the gray.

# All of the theme options can be found here.

g2<- ggplot(mtc, aes(x = hp, y = mpg)) + geom_point()

#Completely clear all lines except axis lines and make background white
t1<-theme(                              
plot.background = element_blank(), 
panel.grid.major = element_blank(), 
panel.grid.minor = element_blank(), 
panel.border = element_blank(), 
panel.background = element_blank(),
axis.line = element_line(size=.4)
)

#Use theme to change axis label style
t2<-theme(                              
axis.title.x = element_text(face="bold", color="black", size=10),
axis.title.y = element_text(face="bold", color="black", size=10),
plot.title = element_text(face="bold", color = "black", size=12)
)

g3 <- g2 + t1
g4 <- g2 + theme_bw()
g5 <- g2 + theme_bw() + t2 + labs(x="Horsepower", y = "Miles per Gallon", title= "MPG vs Horsepower")

grid.arrange(g2, g3, g4, g5, nrow=1)

# Finally, here's a nice graph using a combination of options:

g2<- ggplot(mtc, aes(x = hp, y = mpg)) + 
  geom_point(size=2, aes(color=factor(vs), shape=factor(vs))) +
  geom_smooth(aes(color=factor(vs)),method = "lm", se = TRUE) +
  scale_color_manual(name ="Engine", 
                     labels=c("V-engine", "Straight engine"),
                     values=c("red","blue")) +
  scale_shape_manual(name ="Engine", 
                     labels=c("V-engine", "Straight engine"),
                     values=c(0,2)) +
  theme_bw() + 
  theme(                              
    axis.title.x = element_text(face="bold", color="black", size=12),
    axis.title.y = element_text(face="bold", color="black", size=12),
    plot.title = element_text(face="bold", color = "black", size=12),
    legend.position=c(1,1),
    legend.justification=c(1,1)) +
  labs(x="Horsepower", 
       y = "Miles per Gallon", 
       title= "Linear Regression (95% CI) of MPG vs Horsepower by Engine type")

g2

##########################33
# http://adv-r.had.co.nz/memory.html

# One of the most useful tools for understanding memory usage in R is object.size(), 
# which tells you how much memory an object occupies. 
# This section uses object.size() to look at the size of some simple vectors. 
# By exploring some unusual findings, you'll start to understand some important aspects of memory allocation in R.

# We'll start with a suprising plot: a line plot of vector length vs. memory size (in bytes) for an integer vector. 
# You might have expected that the size of an empty vector would be 0 and that the memory usage would grow proportionately 
# with length. Neither of those things are true!
  
sizes <- sapply(0:50, function(n) object.size(seq_len(n)))
plot(0:50, sizes, xlab = "Length", ylab = "Bytes", type = "s")

# Total memory use

# object.size() tells you the size of a single object; gc() (among other things) tells you the total size of all objects in memory:
  
gc()

# R breaks down memory usage into Vcells (memory used by vectors) and Ncells (memory used by everything else). 
# But this distinction isn't usually important, and neither are the gc trigger and max used columns. 
# What you're usually most interested in is the total memory used. The function below wraps around gc() to return 
# the just amount of memory (in megabytes) that R is currently using.

mem <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }
  
  node_size <- if (bit == 32L) 28L else 56L
  
  usage <- gc()
  sum(usage[, 1] * c(node_size, 8)) / (1024 ^ 2)
}
mem()

# We can build a function of top of mem() that tells us how memory changes during the execution of a block of code. 
# We use a little special evaluation to make the code behave in the same way as running it directly. 
# Positive numbers represent an increase in the memory used by R, and negative numbers a decrease.

mem_change <- function(code) {
  start <- mem()
  
  expr <- substitute(code)
  eval(expr, parent.frame())
  rm(code, expr)
  
  round(mem() - start, 3)
}
# Need about 4 mb to store 1 million integers
mem_change(x <- 1:1e6)
# [1] 3.823
# We get that memory back when we delete it
mem_change(rm(x))

#####################3
# http://www.r-bloggers.com/visualizing-neural-networks-in-r-update/

# As usual, I'll simulate some data to use for creating the neural networks. The dataset contains eight input variables 
# and two output variables. The final dataset is a data frame with all variables, as well as separate data frames for the 
# input and output variables. I've retained separate datasets based on the syntax for each package.

library(clusterGeneration)

seed.val <- 2
set.seed(seed.val)

num.vars < -8
num.obs <- 1000

#input variables
cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
rand.vars <- mvrnorm(num.obs, rep(0,num.vars), Sigma=cov.mat)

#output variables
parms<-runif(num.vars,-10,10)
y1<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)
parms2<-runif(num.vars,-10,10)
y2<-rand.vars %*% matrix(parms2) + rnorm(num.obs,sd=20)

#final datasets
rand.vars<-data.frame(rand.vars)
resp<-data.frame(y1,y2)
names(resp)<-c('Y1','Y2')
dat.in<-data.frame(resp,rand.vars)

# The various neural network packages are used to create separate models for plotting.

#nnet function from nnet package
library(nnet)
set.seed(seed.val)
mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)

#neuralnet function from neuralnet package, notice use of only one response
library(neuralnet)
form.in<-as.formula('Y1~X1+X2+X3+X4+X5+X6+X7+X8')
set.seed(seed.val)
mod2<-neuralnet(form.in,data=dat.in,hidden=10)

#mlp function from RSNNS package
library(RSNNS)
set.seed(seed.val)
mod3<-mlp(rand.vars, resp, size=10,linOut=T)

# I've noticed some differences between the functions that could lead to some confusion. For simplicity, the above code 
# represents my interpretation of the most direct way to create a neural network in each package. Be very aware that direct 
# comparison of results is not advised given that the default arguments differ between the packages. A few key differences 
# are as follows, although many others should be noted. First, the functions differ in the methods for passing the primary 
# input variables. The nnet function can take separate (or combined) x and y inputs as data frames or as a formula, the 
# neuralnet function can only use a formula as input, and the mlp function can only take a data frame as combined or separate 
# variables as input. As far as I know, the neuralnet function is not capable of modelling multiple response variables, 
# unless the response is a categorical variable that uses one node for each outcome. Additionally, the default output for 
# the neuralnet function is linear, whereas the opposite is true for the other two functions.

# Specifics aside, here's how to use the updated plot function. Note that the same syntax is used to plot each model.

#import the function from Github
library(devtools)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')

#plot each model
plot.nnet(mod1)
plot.nnet(mod2)
plot.nnet(mod3)

###############################################################################
# http://www.r-bloggers.com/using-r-coloured-sizeplot-with-ggplot2/

# The sizeplot function in the plotrix package makes this type of scatterplot. However, it doesn't do the colouring easily. 
# I'm sure it's quite possible with a better knowledge of base graphics, but I tend to prefer ggplot2. 
# To construct the same type of plot we need to count the data points. For this, I use table( ), and then melt the 
# contingency table and remove the zeroes.

library(ggplot2)
library(reshape2)
data <- data.frame(x=c(0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4),
                   y=c(0, 0, 0, 3, 1, 1, 1, 2, 2, 1, 4, 4),
                   group=c(rep(1, 6), rep(2, 4), rep(3, 2)))
counts <- melt(table(data[1:2]))
colnames(counts) <- c(colnames(data)[1:2], "count")
counts <- subset(counts, count != 0)
sizeplot <- qplot(x=x, y=y, size=count, data=counts) + scale_size(range=c(5, 10))

# This is the first sizeplot. (The original scale makes single points very tiny. Hence the custom scale for size. 
# Play with the range values to taste!) To add colour, we merge the counts with the original data to get back the group 
# information - and, in true ggplot2 fashion, map the group variable to colour.

counts.and.groups <- merge(counts, unique(data))
sizeplot.colour <- qplot(x=x, y=y, size=count,
                         colour=factor(group), data=counts.and.groups) +
  scale_size(range=c(5, 10))

substr("abcdef", 2, 4)
# [1] "bcd"
substring("abcdef", 1:6, 1:6)
# [1] "a" "b" "c" "d" "e" "f"
substr(rep("abcdef", 4), 1:4, 4:5)
# [1] "abcd" "bcde" "cd"   "de"  

###############################################################################
# http://www.r-tutor.com/gpu-computing/clustering/distance-matrix
# Distance Matrix by GPU

# For example, the data frame mtcars consists of measurements from a collection of 32 automobiles. Since there are 11 
# measurement attributes for each automobile, the data set can be seen as a collection of 32 sample vectors in an 11 
# dimensional space. To find out the dissimilarity between two automobiles, say Honda Civic and Camaro Z28, we can calculate 
# the distance between them with the dist function: 

x <- mtcars["Honda Civic",] 
y <- mtcars["Camaro Z28",] 
dist(rbind(x, y)) 

#  Likewise, we can compute the distance between Camaro Z28 and Pontiac Firebird:
z <- mtcars["Pontiac Firebird",] 
dist(rbind(y, z))

#  As the distance between Camaro Z28 and Pontiac Firebird (86.267) is smaller than the distance between Camaro Z28 and 
# Honda Civic (335.89), we conclude that Camaro Z28 is more similar to Pontiac Firebird than to Honda Civic.

# If we apply the same distance computation between all possible pairs of automobiles in mtcars, and arrange the result 
# into a 32x32 symmetric matrix, with the element at the i-th row and j-th column being the distance between the i-th and 
# j-th automobiles in the the data set, we will have the so-called the distance matrix. It can be computed for mtcars as:
dist(as.matrix(mtcars)) 

#  In general, for a data sample of size M, the distance matrix is an M Ã M symmetric matrix with M Ã (M - 1)???2 distinct
# elements. Hence for a data sample of size 4,500, its distance matrix has about ten million distinct elements. Nevertheless, 
# depending on your application, a sample of size 4,500 may still to be too small to be useful.

# The following measures the time spent on finding the distance matrix for a collection of 4,500 random vectors in a 120 
# dimension space. On a desktop computer with AMD Phenom II X4 CPU, it takes about 14 seconds to finish.
test.data <- function(dim, num, seed=17) { 
       set.seed(seed) 
       matrix(rnorm(dim * num), nrow=num) 
} 
m <- test.data(120, 4500) 
system.time(dist(m)) 

# Now load the rpud package, and compute the same distance matrix using the rpuDist method. For rpud running on 
# NVIDIA GTX 460 GPU, the execution time is about 1 second. Even better, with the rpudplus add-on, you can compute distance 
# matrices for larger data sets that fit inside the system RAM available to R (about 2GB).
library(rpud)                  # load rpud with rpudplus 
system.time(rpuDist(m)) 

###############################################################################
# Breaking the rules with spatial correlation
# http://beckmw.wordpress.com/2013/01/07/breaking-the-rules-with-spatial-correlation/

# Let's start by importing the packages gstat, sp, and nlme. The first two packages have some nifty functions to check model 
# assumptions and the nlme package provides models that can be used to account for various correlation structures.

#import relevant packages
library(gstat)
library(sp)
library(nlme)  

# Next we create a simulated dataset, which allows us to verify the accuracy of our model if we know the 'true' parameter 
# values. The dataset contains 400 observations that are each spatially referenced by the 'lat' and 'lon' objects. 
# The coordinates were taken from a uniform distribution (from -4 to 4) using the runif function. 
# There are two explanatory variables that are both taken from the standard normal distribution using the rnorm function. 
# I've used the set.seed function so the results can be reproduced. R uses pseudo-random number generation, 
# which can be handy for sharing examples.

#create simulated data, lat and lon from uniformly distributed variable
#exp1 and exp2 from random normal
set.seed(2)
samp.sz<-400
lat<-runif(samp.sz,-4,4)
lon<-runif(samp.sz,-4,4)
exp1<-rnorm(samp.sz)
exp2<-rnorm(samp.sz)

# The response variable is a linear combination of the explanatory variables and follows the form of a standard linear model. 
# I've set the parameters for the linear model as 1, 4, -3, and -2 for the intercept and slope values. 
# The error term is also normally distributed. Note that I've included latitude in the model to induce a spatial correlation 
# structure.

#resp is linear combination of explanatory variables
#plus an error term that is normally distributed
resp<-1+4*exp1-3*exp2-2*lat+rnorm(samp.sz)

# Setting the parameter values is key because we hope that any model we use to relate the different variables will return 
# values that are very close to the actual and show minimal variation. In the real world, we do not know the actual parameter 
# values so we have to be really careful that we are using the models correctly.

# Next, we can plot the variables and run some diagnostics to verify we've created appropriate data. 
# Plotting the data frame produces scatterplots that indicate correlation among the simulated variables.

#pair plots of variables
plot(data.frame(resp,exp1,exp2,lat))

# The cor function verifies the info in the figure by returning Pearson correlation coefficients. Note the sign and magnitude 
# of the correlations of the response variable with all other variables. The explanatory variables and latitude are not 
# correlated with each other.

#correlation between variables
cor(data.frame(resp,exp1,exp2,lat))

resp       exp1        exp2         lat
resp  1.0000000 0.53416042 -0.44363892 -0.70131965
exp1  0.5341604 1.00000000  0.01703620  0.03419987
exp2 -0.4436389 0.01703620  1.00000000  0.04475536
lat  -0.7013196 0.03419987  0.04475536  1.00000000

# Now that we've simulated our data, we can create a linear model to see if the parameter coefficients are similar to the 
# actual. In this example, we're ignoring the spatial correlation structure. If we were savvy modelers, we would check for 
# spatial correlation prior to building our model. Spatial correlation structures in actual data may also be more complex 
# than a simple latitudinal gradient, so we need to be extra careful when evaluating the independence assumption. We create 
# the model using the basic lm function.

mod<-lm(resp~exp1+exp2)
coefficients(summary(mod))

Estimate Std. Error    t value     Pr(>|t|)
(Intercept)  1.298600  0.2523227   5.146587 4.180657e-07
exp1         3.831385  0.2533704  15.121673 4.027448e-41
exp2        -3.237224  0.2561525 -12.637879 5.276878e-31

# The parameter estimates are not incredibly different form the actual values. Additionally, the standard errors of the 
# estimates are not large enough to exclude the true parameter values. We can verify this using a t-test with the null 
# hypothesis for each parameter set at the actual (the default null in lm is zero). I've created a function for doing this 
# that takes the observed mean value as input rather than the normal t.test function that uses a numeric vector of values as 
# input. The arguments for the custom function are the observed mean (x.bar), null mean (mu), standard error (se), degrees of 
# freedom (deg.f), and return.t which returns the t-statistic if set as true.

#function for testing parameter values against actual
t_test<-function(x.bar,mu,se,deg.f,return.t=F){
  if(return.t) return(round((x.bar-mu)/se,3))
  pt(abs((x.bar-mu)/se),deg.f,lower.tail=F)*2
}

#for first explanatory variable
t_test(3.8314, 4, 0.2534, 397)
[1] 0.5062122

# The p-value indicates that the estimated parameter value for the first explanatory variable is not different from the actual. 
# We get similar results if we test the other parameters. So far we've shown that if we ignore the spatial correlation 
# structure we get fairly reasonable parameter estimates. Can we improve our model if we account for the spatial correlation 
# structure?

# Before we model spatial correlation we first have to identify its existence (remember, we're assuming we don't know the 
# response is correlated with latitude). Let's dig a little deeper by first describing some tools to evaluate the assumption 
# of independence. The easiest way to evaluate this assumption in a linear model is to plot the model residuals by their 
# spatial coordinates. We would expect to see some spatial pattern in the residuals if we have not accounted for spatial 
# correlation. The bubble plot function in the sp package can be used to plot residuals.

dat<-data.frame(lon,lat,resids=resid(mod))
coordinates(dat)<-c('lon','lat')
bubble(dat,zcol='resids')

# A nice feature of the bubble function is that it color codes the residuals based on positive and negative values and sets 
# the size of the points in proportion to the magnitude. An examination of the plot shows a clear residual pattern with 
# latitude such that smaller values are seen towards the north.

# The bubble plot provides a qualitative means of evaluating spatial correlation. A more quantitative approach is to evaluate 
# semivariance, which provides a measure of spatial correlation between points at different distances. Points closer to one 
# another are more likely to be similar if observations in our dataset are spatially correlated. The variety of statistics 
# that can be applied to spatial data is staggering and evaluations of semivariance are no exception. For this example, it is 
# sufficient to know that points closer to one another will have smaller values of semivariance (more similar) and those 
# farther apart will have larger values (less similar) if points are spatially correlated. We would also expect calculated 
# values of semivariance to vary depending on what direction we are measuring (i.e., anisotropy). For example, we might not 
# detect spatial correlation if we estimate semivariance strictly in an east to west direction, whereas semivariance would 
# vary quite dramatically along a north to south direction. We can apply the variogram function (gstat package) to estimate 
# semivariance and also account for potential differences depending on direction.

var.mod<-variogram(resids~1,data=dat,alpha=c(0,45,90,135))
plot(var.mod)

# When we use the variogram function we can specify directions in which semivariance is measured using the alpha argument. 
# Here we've specified that we'd like to measure semivariance in the residuals in a northern, north-eastern, eastern, and 
# south-eastern direction. This is the same as estimating semivariance in directions 180 degrees from what we've specified, 
# so it would be redundant if we looked at semivariance in a southern, south-western, western, and north-western direction. 
# The plot shows that spatial correlation is strongest in a north-south direction and weakest along an east-west direction, 
# which confirms the latitudinal change in our response variable.

# Our analyses have made it clear that the model does not account for spatial correlation, even though our parameter estimates 
# are not significantly different from the actual values. If we account for this spatial correlation can we improve our model? 
# An obvious solution would be to include latitude as an explanatory variable and obtain a parameter estimate that describes 
# the relationship. Although this would account for spatial correlation, it would not provide any explanation for why our 
# response variable changes across a latitudinal gradient. That is, we would not gain new insight into causation, rather we 
# would only create a model with more precise parameters. If we wanted to account for spatial correlation by including an 
# additional explanatory variable we would have to gather additional data based on whatever we hypothesized to influence our 
# response variable. Temperature, for example, may be a useful variable because it would likely be correlated with latitude 
# and its relationship with the response variable could be biologically justified.

# In the absence of additional data we can use alternative models to account for spatial correlation. The gls function (nlme 
# package) is similar to the lm function because it fits linear models, but it can also be used to accommodate errors that are 
# correlated. The implementation is straightforward with the exception that you have specify a form of the spatial correlation 
# structure. Identifying this structure is non-trivial and the form you pick will affect how well your model accounts for 
# spatial correlation. There are six options for the spatial correlation structure. Picking the optimal correlation structure 
# requires model comparison using AIC or similar tools.

# For our example, I've shown how we can create a model using only the Gaussian correlation structure. Also note the form 
# argument, which can be used to specify a covariate that captures the spatial correlation.

#refit model with correlation structure
mod.cor<-gls(resp~exp1+exp2,correlation=corGaus(form=~lat,nugget=TRUE))
summary(mod.cor)

# The gls function seems to have trouble estimating the intercept. If we ignore this detail, the obvious advantage of 
# accounting for spatial correlation is a reduction in the standard error for the slope estimates. More precisely, we've 
# reduced the uncertainty of our estimates by over 500% from those obtained using lm, even though our initial estimates were 
# not significantly different from the actual.

# The advantage of accounting for spatial correlation is a huge improvement in the precision of the parameter estimates. 
# Additionally, we do not sacrifice degrees of freedom if we use the gls function to model spatial correlation. Hopefully this 
# example has been useful for emphasizing the need to account for model assumptions. I invite further discussion on methods 
# for modeling spatial correlation.

# Information of the sesion
search()
ls()

# is random selection of 70% training set
train <- sample(nrow(weather), 0.7*nrow(weather))

# Get a random collection of 9 observations from the top of 
# the weatherAUS dataset - thet top is not likely to change as 
# weatherAUS grows.

# so we can replicate
set.seed(42)

# randomly select 9 numbers out of 300
obs <- sort(sample(300, 9))

# use the random sample to select 9 random rows
ds <- weather[obs,]

# change global graphic parameter setting
oldpar <- par(xpd=TRUE)

# send the output to a pdf
pdf(file="annotated_dataset.pdf")

# Do lots of stuff to create annotated data frame
plot(c(0,10), c(0,0), type="l", axes=FALSE, xlab="", ylab="",
     xlim=c(-5,12), ylim=c(-2,12))
lines(c(0,10), c(10,10))
lines(c(0,0), c(0,10))
lines(c(10,10), c(0,10))

text(1, 9.5, "Date")
lines(c(0,10), c(9,9))
text(1, seq(8.5, 0.5, -1), format(as.Date(ds$Date), "%d %b"))

text(3, 9.5, "Temp")
text(3, seq(8.5, 0.5, -1), round(ds$Temp3pm))

text(5, 9.5, "Wind Dir.")
text(5, seq(8.5, 0.5, -1), ds$WindDir9am)

text(7, 9.5, "Evap")
text(7, seq(8.5, 0.5, -1), sprintf("%.1f", ds$Evaporation))

text(9, 9.5, "Rain?")
text(9, seq(8.5, 0.5, -1), c("Y", "Y", "N", "N", "N", "Y", "N", "N", "Y"))

text(-4, 9.5, "Variables")
arrows(-2.5, 9.5, -0.2, 9.5, code=2, lwd=0.5, length=0.1)

text(3, 11.5, "Numeric")
arrows(3, 11, 3, 10.2, code=2, lwd=0.5, length=0.1)

text(5, 12, "Categoric")
arrows(5, 11.5, 5, 10.2, code=2, lwd=0.5, length=0.1)

text(7, 11.5, "Numeric")
arrows(7, 11, 7, 10.2, code=2, lwd=0.5, length=0.1)

text(9, 12, "Categoric")
arrows(9, 11.5, 9, 10.2, code=2, lwd=0.5, length=0.1)

text(-4, 5, "Observations")
arrows(-2, 5, -0.22, 5, code=2, lwd=0.5, length=0.1)

text(1, -1, "Identifier")
text(5, -1, "Input")
text(9, -1, "Output")

lines(c(2.2,2.2), c(0,10))
lines(c(8.2,8.2), c(0,10))

# this next line sends the graphic to the pdf
dev.off()

# this line returns the graphics settings to their original state
par(oldpar)

# resets defaults graphic device
dev.off()

###################################################
### Cumulative Distribution Plot displays proportion
### of data that has a value that is less than or
### equal to the value shown on x-axis
###################################################
library(rattle)
library(Hmisc)
su <- weather$Sunshine
sn <- su[weather$RainTomorrow=="No"]
sy <- su[weather$RainTomorrow=="Yes"]
Ecdf(su, col="#E495A5", xlab="Sunshine", subtitles=FALSE)
Ecdf(sn, col="#86B875", lty=2, add=TRUE, subtitles=FALSE)
Ecdf(sy, col="#7DB0DD", lty=3, add=TRUE, subtitles=FALSE)


###################################################
### Add a Legend
###################################################
legend("bottomright", c("All","No","Yes"), bty="n", 
       col=c("#E495A5", "#86B875", "#7DB0DD"), 
       lty=1:3, inset=c(0.05,0.05))

### Add a Title and time stamp at bottom
title(main=paste("Distribution of Sunshine (sample)",
                 "by RainTomorrow", sep="\n"),
      sub=paste("Rattle", format(Sys.time(), 
                                 "%Y-%b-%d %H:%M:%S")))

###################################################
### Creating panel functions for histogram
###################################################
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey90", ...)
}

##################################################

# 50 functions to clear a basic interview for Business Analytics #rstats
# http://decisionstats.com/2013/11/24/50-functions-to-clear-a-basic-interview-for-business-analytics-rstats/?goback=.gde_4066593_member_5810423548753235969#!

# Due respect to all cheat sheets and ref cards, but these are the functions that I use in a sequence to analyze a business 
# data set. interview

# Packages

install.packages("Hmisc")  # installs package Hmisc
library(Hmisc)             # loads package Hmisc
update.packages()          # Updates all packages

# Data Input

getwd()                    # Gets you the current working directory
setwd("C:/Path")           # Sets the working directory to the new path , here C:/Path
dir()                      # Lists all the files present in the current working directory
a=read.csv("1.csv",header=T,sep=",",stringsAsFactors=T)

# a= read.csv (assigns the object name a to whatever comes to the right side)
# You can also explicitly assign a character name to a data object using assign)
# read.csv is a type of input command derived from read.table to read in a rectangular object (rows and columns)
# header specifies whether the first line has variable names or not
# sep denotes seperator (generally a comma for CSV files but can be space or tab for text files)
# stringsAsFactors=T reads in strings as character values and not factor levels

# Object Inspection

str(a)    # Gives the structure of object named  including class, dimensions, type of variables , names of variables and a 
          # few values of variables as well. Only drawback is can throw up a lot of information for a big data set
names(a)  # Gives the names of variables of the object
class(a)  # Gives the class of a object like data.frame, list,matrix, vector etc
dim(a)    # Gives the dimension of object (rows column)
nrow(a)   # Gives the number of rows of object a- useful when used as an input for another function
ncol(a)   # Gives the number of columns of object a
length(a) # Gives the length of object- useful for vectors, for a data frame it is the same as ncol
a[i,j]    # Gives the value in ith row and jth column
a$var1    # Gives the variable named var1 in object a . This can be treated as a seperate object on it's own for inspection 
          # or analysis

# Data Inspection

head(a,10) # gives the first ten rows of object a
tail(a,20) # gives the last twenty rows of object a
b=ajay[sample(nrow(ajay),replace=F,size=0.05*nrow(ajay)),] 

# Lets get a 5 % random sample of the object ajay
# [] uses the subset to give value in the specified row
# Sample is the command for sample
# So out nrow(ajay) or total number to be sampled of ,size= Size of sample it is taking 5% numbers, and these are the row 
# numbers that are being returned. replace =F means each number is selected only once

# Math Functions

# Basic-
  
sum     # sum
sqrt    # square root
sd      # standard deviation
log     # log
mean    # mean
median  # median

# Additional-
  
cumsum  # Cumulative Sum for a column
diff    # Differencing
lag     # Lag

# Data Manipulation

paste(a$Var)       # converts Var from Factor/Numeric variable to Charachter Variable
as.numeric(a$Var2) # Converts a character variable into a numeric variable
is.na(a)           # retruns TRUE wheneve it encounters a Missing Value within the object
na.omit(a)         # Deletes all missing values (denoted by NA within R)
na.rm=T            # (this option enables you to calculate values Ignoring Missing Values)
nchar(abc)         # gives the values of characters in a character value
substr("ajay",1,3) # gives the sub string from starting place 1 to ending place 3. Note in R index starts from 1 for first object

# Date Manipulation  

library(lubridate)
a="20Mar1987???
dmy(a)
# [1] "1987-03-20 UTC"
b="20/7/89???
dmy(b)
# [1] "1989-07-20 UTC"
c="October 12 93???
mdy(c)
# [1] "1993-10-12 UTC"

# Data Analysis

summary(a)           # Gives summary of object including min,max,median,mean, 1st quartile, 3rd Quartile) for numeric objects 
                     # and frequency analysis of Factor variables
table(a)             # Gives Frequency Analysis of variable or obejct
table(a$var1,a$var2) # Gives cross tabs of Var1 with respect to Var 2 of object a

library(Hmisc) # loads HMisc which enables use to use describe and summarize function

describe(a$var1)              # gives a much more elaborate and concise summary of the variable Var 1- it's a better version of 
                              # summary
summarize(a$var1,a$var2,FUN)  # applies a function (like sum, median, summary or mean) on Var 1 , as GROUPED by Var2
cor(a)                        # gives corelation between all numeric variables of a

# Data Visualization

plot(a$var1,a$var2)  # Plots Var 1 with  Var 2
boxplot(a)           # boxplot
hist(a$Var1)         # Histogram
plot(density(a$Var1) # Density Plot
pie(pie chart)
     
# Modeling
     
a=lm(y~x)        # creates model
vif(a)           # gives Variance Inflation  (library(car) may help)
outlierTest(a)   # gives Outliers
     
summary(a)       # gives model summary including parameter estimates
     
# Write Output
     
write.csv(a)     # Write output as a csv file
png("graph.png") # Write plot as png file
q()              # Quits R Session
     
# Sys.time() and Sys.Date() gives current   time and date (note the change in case)
# while system.time(experession gives time taken to evaluate an expression)
     
# We are interested in: Whether, on the basis of their
# chemical compositions, the pots can be divided into
# distinct groups, and how these groups relate to the
# kiln site.

pottery

?dist
# get a distance (dissimilarity) matrix between the rows.
# dissimilarity is based on euclidean distance of the
# chemical measurements of all 45 pots:
pottery_dist <- dist(pottery[, colnames(pottery) != "kiln"])
library("lattice") # need to have lattice and grid installed
# associates each cell of the dissimilarity matrix with
# a color value. 
levelplot(as.matrix(pottery_dist), xlab = "Pot Number",
          ylab = "Pot Number")

# Can see at least three distance groups with small
# inter-cluster differences, whereas much larger 
# differences can be observed for all other cells.

###################################################
### Same plot in just greys
###################################################
trellis.par.set(standard.theme(color = FALSE))
plot(levelplot(as.matrix(pottery_dist), 
               xlab = "Pot Number", ylab = "Pot Number"))

###################################################
### Construct three series of partitions using
### single, complete, and average linkage
### hierarchical clustering with function hclust()
###################################################

# Function hclust() uses dissimilarity matrix and
# method argument to specify how distnace between
# two clusters is assessed.
pottery_single <- hclust(pottery_dist, method = "single")
pottery_complete <- hclust(pottery_dist, method = "complete")
pottery_average <- hclust(pottery_dist, method = "average")
layout(matrix(1:3, ncol = 3))
# plot method draws a dendogram; the three dendograms
# all indicate that three clusters fit the data best (!)
# I think mainly 'average linkage' shows that
plot(pottery_single, main = "Single Linkage",
     sub = "", xlab = "")
plot(pottery_complete, main = "Complete Linkage",
     sub = "", xlab = "")
plot(pottery_average, main = "Average Linkage",
     sub = "", xlab = "")
    
###################################################
### From the pottery_average object representing
### the average linkage hierarchical clustering,
### we derive the three-cluster solution by
### cutting the dendrogram at a height of four.
###################################################

# We are now interested in a comparison of the kiln
# sites at which the pottery was found

pottery_cluster <- cutree(pottery_average, h = 4)
xtabs(~ pottery_cluster + kiln, data = pottery)

# The contingency table shows that cluster 1 contains
# all pots at kiln site number one, cluster 2
# contains all pots from kiln site number two and
# three, and cluster three contains the ten pots
# from kiln sites four and five.
 
# In fact, the five kiln sites are from three different
# regions defined by one, two and three, and four and
# five, so the clusters actually correspond to pots
# from three different regions.

###################################################
### Model-based Clustering in R
###################################################

# mclust() selects both the most appropriate model
# for the data AND the optimal number of groups
# based on the values of the Bayesian Information
# Criterion (BIC) computed over several models.

library("mclust")
planet_mclust <- Mclust(planet.dat)

###################################################
### Examine plot of BIC values
###################################################
plot(planet_mclust, planet.dat, what = "BIC", col = "black",
     ylab = "-BIC", ylim = c(0, 350))

# Different plotting symbols refer to different
# model assumptions about the shape of clusters:
# EII: spherical, equal volume
# VII: spherical, unequal volume
# EEI: diagonal, equal volume and shape
# VEI: diagonal, varying volume, equal shape
# EVI: diagonal, equal volume, varying shape
# VVI: diagonal, varying volume and shape ** BEST HERE **
# EEE: ellipsoidal, equal volume, shape and distribution
# EEV: ellipsoidal, equal volume and equal shape
# VEV: ellipsoidal, equal shape
# VVV: ellipsoidal, varying volume, shape and orientation

###################################################
### Examine plot of BIC values
###################################################

# BIC selects model VVI with three clusters as best
# solution as can be seen from the print output:
print(planet_mclust)

###################################################
### Can show this solution graphically as a 
### scatterplot matrix.
###################################################
clPairs(planet.dat,
   classification = planet_mclust$classification,
        symbols = 1:3, col = "black")

###################################################
### Here is a clustering solution in the three-
### dimensional space
###################################################
scatterplot3d(log(planets$mass), log(planets$period),
              log(planets$eccen + ifelse(planets$eccen == 0, 0.001, 0)),
              type = "h", angle = 55, scale.y = 0.7,
              pch = planet_mclust$classification,
              y.ticklabs = seq(0, 10, by = 2), y.margin.add = 0.1,
              xlab = "log(mass)", ylab = "log(period)",
              zlab = "log(eccen)")

###################################################
### The number of planets in each cluster and the
### the mean vectors of the three clusters for
### the untransformed data can now be inspected:
###################################################
table(planet_mclust$classification)
ccent(planet_mclust$classification)

# These numbers are different from the solution we
# determined using k-means

#  1  2  3 
# 19 41 41 

# Cluster 1 consists of planets about the same size
# as Jupiter with very short periods and eccentricities
# (similar to the first cluster of k-means solution).
# Cluster 2 consists of slightly larger planets with
# moderate periods and large eccentricities, and
# cluster 3 contains the very large planets with very
# large periods. 

# The final two clusters do not match those found by
# the k-means approach.

#    1           2            3
#   mass   1.16652632   1.5797561    6.0761463
# period   6.47180158 313.4127073 1325.5310048
#  eccen   0.03652632   0.3061463    0.3704951
# Association analysis identifies relations or
# correlations between observations and/or
# between variables in our datasets.

# These relationships are then expressed
# as a collection of "association rules".

# Is a core technique of data mining. Is very
# useful for mining very large transactional
# databases, like shopping baskets and on-line
# customer purchases.

### Knowledge Representation: Assocation Rules

# General Format is A -> C    Can generalize
# A (the antecedent) to a specific variable
# or value combination so can apply to various
# datasets

### Search Heuristic

# Basis of an association analysis algorithm
# is the generation of frequent itemsets. Is
# an "apriori algorithm", a generate-and-test
# type of search algorithm. Only after exploring
# all of the possibilities of associations
# containing k items does it consider those
# contining K + 1 items. For each k, all candidates
# are tested to determine whether they have
# enough support.

# A frequent itemset is a set of items that
# occur together frequently enough to be 
# considered as a candidate for generating
# association rules.

### 3 Measures: Support, Confidence, Lift

# "Support" is a measure of how frequently the
# items must appear in the whole dataset before
# they can be considered as a candidate asso-
# ciation rule.

# "Support" for a collection of items is the
# proportion of all transactions in which the
# items appear together

# support(A -> C) = P(A U C)

# We use small values of "support" as are not
# looking for the obvious ones.

# The actual association rules that we retain
# are those that meet a criterion "confidence"
# "Confidence" calculates the proportion of 
# transactions containing A that also contain C.

# confidence(A -> C) = P(C|A) = P(A U C)/P(A)

# confidence(A -> C) = support(A -> C)/support(A)

# Typically looking for larger values of
# confidence

# Another measure: "Lift"
# "Lift" is the increased likelihood of C
# being in a transaction if A is included in
# the transaction:

# lift(A -> C) = confidence(A -> C)/support(C)

# Another measure: Leverage, which captures
# the fact that a higher frequency of A and C
# with a lower lift may be interesting:

# leverage(A->C)=support(A->C)-support(A)*support(C)

### Tutorial Example using Rattle: DVDs

# Two types of association rules were identified 
# corresponding to the type of data available. 
# The simplest case, known as "market basket
# analysis", is when we have a transaction 
# dataset that records just a transaction
# identifer. The identifer might identify a 
# single shopping basket containing multiple 
# items from shopping or a particular customer
# or patient and their associated purchases or 
# medical treatments over time.

# A simple example of a market basket dataset 
# might record the purchases of DVDs by customers
# (three customers in this case):

# ID, Item
# 1, Sixth Sense
# 1, LOTR1
# 1, Harry Potter1
# 1, Green Mile
# 1, LOTR2
# 2, Gladiator
# 2, Patriot
# 2, Braveheart
# 3, LOTR1
# 3, LOTR2

###############################################################################
## http://www.r-bloggers.com/r-syntax-for-ranked-choice-voting/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29&utm_content=Yahoo!+Mail

library(RCurl)
library(ggplot2)
# url = getURL("https://raw.github.com/tcrug/ranked-choice-vote-data/master/2013-mayor-cvr.csv")
# url = getURL("./2013-mayor-cvr.csv")
# vote = read.csv(text = url)
vote<- read.csv("./2013-mayor-cvr.csv")

# A quick look at the data reveal that the three ranked choices for the 80,101 voters are in columns 2, 3, and 4. 
# The values "undervote" and "overvote" are ballot also need to be converted to "NA" (missing). The syntax below reduces the 
# data frame to the second, third and fourth columns and replaces "undervote" and "over vote' with NAs.

vote = vote[ , 2:4]
vote[vote == "undervote"] = NA
vote[vote == "overvote"] = NA

# The syntax below is the main idea of the vote counting algorithm. (You will need to load the ggplot library.) 
# I will try to explain each line in turn.

nonMissing = which(vote[ , 1] != "")
candidates = vote[nonMissing, 1]
#print(table(candidates))

vote[ , 1] =  factor(vote[ , 1], levels = rev(names(sort(table(vote[ , 1]), decreasing=TRUE))))
mayor = levels(vote[ , 1])
candidates = vote[nonMissing, 1]

p = ggplot(data = data.frame(candidates), aes(x = factor(candidates, levels = mayor))) +
  geom_bar() +
  theme_bw() +
  ggtitle("Round 1") +
  scale_x_discrete(name = "", drop = FALSE) +
  ylab("Votes") +
  ylim(0, 40000) +
  coord_flip()

ggsave(p, file = "round1.png", width = 8, height = 6)

# Line 1: Examine the first column of the vote data frame to determine which rows are not missing.
# Line 2: Take the candidates from the first column and put them in an object
# Line 3: Count the votes for each candidate
# Line 5: Coerce the first column into a factor (it is currently a character vector) and create the levels of that factor so that they display in reverse order based on the number of votes. 
# This is important in the plot so that the candidates display in the same order every time the plot is created.
# Line 6: Store the levels we just created from Line #5 in an object
# Line 7: Recreate the candidates object (same as Line #2) but this time they are a factor. This is so we can plot them.
# Line 8-16: Create the bar plot
# Line 18: Save the plot onto your computer as a PNG file. In my case, I saved it to the desktop.

# Now, we will create an object to hold the round of counting (we just plotted the first round, so the next round is Round 2). 
# We will also coerce the first column back to characters.

j = 2
vote[ , 1] = as.character(vote[ , 1])

# The next part of the syntax is looped so that it repeats the remainder of the algorithm, which essentially is to determine 
# the candidate with the fewest votes, remove him/her from all columns, take the second and third choices of anyone who voted 
# for the removed candidate and make them the 'new' first and second choices, recount and continue.

while( any(table(candidates) >= 0.5 * length(candidates) + 1) == FALSE ){
  leastVotes = attr(sort(table(candidates))[1], "names")
  vote[vote == leastVotes] = NA
  rowNum = which(is.na(vote[ , 1]))
  vote[rowNum, 1] = vote[rowNum, 2]
  vote[rowNum, 2] = vote[rowNum, 3]
  vote[rowNum, 3] = NA
  nonMissing = which(vote[ , 1] != "")
  candidates = vote[nonMissing, 1]
  p = ggplot(data = data.frame(candidates), aes(x = factor(candidates, levels = mayor))) +
    geom_bar() +
    theme_bw() +
    ggtitle(paste("Round", j, sep =" ")) +
    scale_x_discrete(name = "", drop = FALSE) +
    ylab("Votes") +
    ylim(0, 40000) +
    coord_flip()
  ggsave(p, file = paste("./round", j, ".png", sep = ""), width = 8, height = 6)
  j = j + 1
  candidates = as.character(candidates)
  print(sort(table(candidates)))
}

# The while{} loop continues to iterate until the criterion for winning the election is met. Within the loop:

# Line 2: Determines the candidate with the fewest votes
# Line 3: Replaces the candidate with the fewest votes with NA (missing)
# Line 4: Stores the row numbers with any NA in column 1
# Line 5: Takes the second choice for the rows identified in Line #4 and stores them in column 1 (new first choice)
# Line 6: Takes the third choice for the rows identified in Line #4 and stores them in column 2 (new second choice)
# Line 7: Makes the third choice for the rows identified in Line #4 an NA
# Line 8-18: Are equivalent to what we did before (but this time they are in the while loop). The biggest difference is in the ggsave() function, the filename is created on the fly using the object we created called j.
# Line 19: Augment j by 1
# Line 20: Print the results

# Creating the Animated GIF

# There should now be 35 PNG files on your desktop (or wherever you saved them in the ggsave() function). These should be 
# called round1.png, round2.png, etc. The first thing I did was rename all of the single digit names so that they were 
# round01.png, round02.png, ., round09.png.

# Then I opened Terminal and used ImageMagick to create the animated GIF. Note that in Line #1 I move into the folder where 
# I saved the PNG files. In my case, the desktop.

cd ~/Desktop
convert -delay 50 round**.png animation.gif
   
     ## Logistic Regression with R
     ## http://pingax.com/logistic-regression-r-step-step-implementation-part-2/#!
     
     #Load data
     data <- read.csv("data.csv")
     
     #Create plot
     plot(data$score.1,data$score.2,col=as.factor(data$label),xlab="Score-1",ylab="Score-2",pch=16)
     
     # From, the plot you can see that red marked observations indicate they got admission and black marked observation indicate 
     # they did not get.  We can imagine clear cut classification boundary between these two groups.
     
     # So, in this case we have two predictor variables (two exams' scores) and label as response variable. 
     # Let us set predictor and response variables.
     
     #Predictor variables
     X <- as.matrix(data[,c(1,2)])
     
     #Add ones to X
     X <- cbind(rep(1,nrow(X)),X)
     
     #Response variable
     Y <- as.matrix(data$label)
     
     # Before, we start with actual cost function. Recall the logistic regression hypothesis is defined as: sigmoid_hypothesis
     # Where function g is the sigmoid function. Our first step is to implement sigmoid function.
     
     #Sigmoid function
     sigmoid <- function(z)
     {
       g <- 1/(1+exp(-z))
       return(g)
     }
     
     # Now we will implement cost function. Recall the cost function in logistic regression R code is as:
     
     #Cost Function
     cost <- function(theta)
     {
       m <- nrow(X)
       g <- sigmoid(X%*%theta)
       J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
       return(J)
     }
     
     # Let's test this cost function with initial theta parameters. We will set theta parameters equal to zero initially and 
     # check the cost.
     
     #Intial theta
     initial_theta <- rep(0,ncol(X))
     
     #Cost at inital theta
     cost(initial_theta)
     
     # You will find cost is 0.693 with initial parameters. Now, our objective is to minimize this cost and derive the optimal
     # value of the thetas. For that we will use gradient descent optimization. In blog post 'Linear regression with R:step by step 
     # implementation part-2', I implemented gradient descent and defined the update function to optimize the values of theta.
     # Here I will use inbuilt function of R optim() to derive the best fitting parameters. Ultimately we want to have optimal value 
     # of the cost function and theta.
     
     # Derive theta using gradient descent using optim function
     theta_optim <- optim(par=initial_theta,fn=cost)
     
     #set theta
     theta <- theta_optim$par
     
     #cost at optimal value of the theta
     theta_optim$value
     
     # We have optimal values of the theta and cost is about 0.2034 at optimal value of theta. We can now use this theta parameter 
     # for predicting for the admission probability for new applicant based on the score of two exams.
     # For example, a student is with exam-1 score of 45 and exam-2 score of 85.
     
     # probability of admission for student
     prob <- sigmoid(t(c(1,45,85))%*%theta)
     
     # You will find probability is 0.774, it is above 0.5 which means that student will get admission.

     ## 24 Days of R: Day 6
     ## http://pirategrunt.com/2013/12/06/24-days-of-r-day-6-2/
     
     # I've finally had some success at munging some HTML. For quite some time, I've wanted to render a county level choropleth for 
     # US presidential election results. The numbers are all there on Politico.com, but attempts to use readHTMLTable never returned 
     # the full set of data. It still doesn't, but I have sorted out how to get all of the results I want. It takes a fair bit of 
     # work, but- once the smoke clears- doesn't seem too crazy.
     
     # First, we'll fetch some raw HTML for North Carolina.
     
     library(XML)
     
     URL = "http://www.politico.com/2012-election/results/president/north-carolina/"
     content.raw = htmlParse(URL, useInternalNodes = TRUE)
     
     # Inspection of the tables which get returned tell us that the second element in the list has the data we need. 
     # Attempts to extract the information lead us to take a slightly different approach. First, we'll get all the nodes with 
     # a "tbody" element. Each of these nodes may be treated as a table.
     
     tables <- getNodeSet(content.raw, "//table")
     counties = getNodeSet(tables[[2]], "//tbody")
     counties = counties[-1]
     
     countyTables = lapply(counties, readHTMLTable, header = FALSE, stringsAsFactors = FALSE)
     
     # The table we get isn't quite what we want.
     
     head(countyTables[[1]])
     
     ##                          V1        V2    V3     V4     V5
     ## 1 Alamance 100.0% Reporting M. Romney   GOP  56.6% 37,712
     ## 2              B. Obama (i)       Dem 42.5% 28,341   <NA>
     ## 3                G. Johnson       Lib  0.9%    585   <NA>
     
     # A couple helper functions will fetch the county name and move the cells to a sensible location.
     
     GetCountyName = function(dfCounty) {
       strCounty = dfCounty[1, 1]
       strCounty = strsplit(strCounty, " ")
       strCounty[[1]][1]
     }
     
     MungeTable = function(dfCounty) {
       
       if (ncol(dfCounty) != 5) 
         return(data.frame())
       
       dfCounty[1, 1] = GetCountyName(dfCounty)
       
       dfCounty[-1, 2:5] = dfCounty[-1, 1:4]
       
       dfCounty[, 1] = dfCounty[1, 1]
       
       colnames(dfCounty) = c("CountyName", "Candidate", "Party", "Pct", "Votes")
       
       dfCounty$Votes = gsub(",", "", dfCounty$Votes)
       dfCounty$Votes = as.numeric(dfCounty$Votes)
       
       dfCounty$Pct = NULL
       
       dfCounty
     }
     
     correctTable = MungeTable(countyTables[[1]])
     head(correctTable)
     
     ##   CountyName    Candidate Party Votes
     ## 1   Alamance    M. Romney   GOP 37712
     ## 2   Alamance B. Obama (i)   Dem 28341
     ## 3   Alamance   G. Johnson   Lib   585
     
     # With that done, it's a simple thing to munge each data frame and then bind the results into a single data frame.
     
     counties = lapply(countyTables, MungeTable)
     dfNorthCarolina = do.call("rbind", counties)
     
     # A plot shows that Obama won in counties with a high population, but didn't do as well in smaller counties. 
     # I'll draw some better charts tomorrow.
     
     library(ggplot2)
     ggplot(dfNorthCarolina, aes(x = CountyName, y = Votes, fill = Party)) + geom_bar(stat = "identity")
     
     # This required getting very, very familiar with the underlying HTML structure. That's a hassle, but hardly impossible. 
     # Tomorrow, this will become a map and I'll make some inferences about voting patterns and demographics.
     
     sessionInfo()
     
## http://www.datanalytics.com/blog/2013/12/10/te-queda-lejos-el-aeropuerto/?utm_source=twitterfeed&utm_medium=linkedin#!

aeropuertos <- read.table("GlobalAirportDatabase.txt", sep = ":", header = F, quote = "")
aeropuertos <- subset(aeropuertos, V5 == "SPAIN" & V2 != "N/A")
     
aeropuertos$V7 <- aeropuertos$V7 + aeropuertos$V8 / 60
aeropuertos$V6 <- aeropuertos$V6 + aeropuertos$V7 / 60
   
aeropuertos$V11 <- aeropuertos$V11 + aeropuertos$V12 / 60
aeropuertos$V10 <- aeropuertos$V10 + aeropuertos$V11 / 60
aeropuertos$V10 <- aeropuertos$V10 * ifelse(aeropuertos$V13 == "E", 1, -1)
     
aeropuertos <- subset(aeropuertos, select = c("V3", "V6", "V10"))
     
colnames(aeropuertos) <- c("nombre", "lat", "lon")

# Luego he descargado y procesado el mapa que me proporciona el contorno de la España peninsular:
     
library(maptools)
tmp <- readShapePoly("./ESP_adm0.shp")
peninsula <- tmp@polygons[[1]]@Polygons[[187]]
aeropuestos.peninsula <- point.in.polygon(aeropuertos$lon,
                                         aeropuertos$lat,
                                         peninsula@coords[,1],
                                         peninsula@coords[,2])
aeropuertos <- aeropuertos[aeropuestos.peninsula == 1, ]
     
# El shapefile estÃ¡n descargado de GADM. La funcion point.in.polygon permite descartar aquellos aeropuertos 
# extrapeninsulares: indentifica si un punto esta dentro o fuera de un poligono.
     
# Luego he creado una malla de puntos a partir de los extremos de la penÃ­nsula y he utilizado el paquete geosphere para 
# calcular la distancia entre puntos expresados en tÃ©rminos de su latitud/longitud.

library(geosphere)
     
 extremos <- apply(peninsula@coords, 2, range)
 grid.lat <- seq(from = extremos[1,2], to = extremos[2,2], length.out = 1000)
 grid.lon <- seq(from = extremos[1,1], to = extremos[2,1], length.out = 1000)
     
 distancia <- function(lon, lat){
   lonlat <- cbind(lon, lat)
   aeropuertos.lon.lat <- cbind(aeropuertos$lon, aeropuertos$lat)
   distancias <- distm(lonlat, aeropuertos.lon.lat)
   apply(distancias, 1, min)
 }
    
res <- outer(grid.lon, grid.lat, distancia)
     
# Finalmente,
     
library(raster)
     
resk <- SpatialPoints(expand.grid(grid.lon, grid.lat))
resk <- SpatialPixelsDataFrame(resk, data.frame(dist = as.vector(res)))
     
sp.peninsula <- Polygon(peninsula@coords)
sp.peninsula <- Polygons(list(sp.peninsula), ID = "peninsula")
sp.peninsula <- SpatialPolygons(list(sp.peninsula))
     
seleccionados <- !is.na(over(resk, sp.peninsula))
     
final <- resk[seleccionados,]
     
image(final)
     
# Es decir, he usado primero la funcion over (de sp) para identificar (como antes, mÃ¡s arriba, usando point.in.polygon) 
# aquellos puntos de la malla que caen dentro del para­metro deseado. Para ello he tenido que hacer dos transformaciones previas:
# Convertir el poli­gon D ePÃ­metro de la EspaÃ±a peninsular en un objeto de la clase SpatialPolygons (es decir, un polÃ­gono 
# con informacion de tipo cartogrÃ¡fico)
# Convertir mi malla en un objeto de la clase SpatialPixelsDataFrame (es decir, de puntos tambien con base cartografica)
# Con el ultimo filtro me he quedado con los puntos deseados y con la llamada a image (sin mas argumentos) he liquidado 
# la entrada del dÃ­a.

## http://www.r-bloggers.com/plotting-y-and-logy-in-one-figure/

# Sometimes I have the desire to plot both on the linear and on the log scale. To save space just two figures is not my solution. I want to reuse the x-axis, legend, title. 
# This post examines possibilities to do so with standard plot tools, lattice and ggplot2.

# Data is completely artificial.
library(ggplot2)
library(lattice)

datastart <- data.frame(x=rep(1:5,2),
                        y=c(1,2,10,50,1, .1,9,8,20,19),
                        type=rep(c('a','b'),each=5))
datastart
# x    y type
# 1  1  1.0    a
# 2  2  2.0    a
# 3  3 10.0    a
# 4  4 50.0    a
# 5  5  1.0    a
# 6  1  0.1    b
# 7  2  9.0    b
# 8  3  8.0    b
# 9  4 20.0    b
# 10 5 19.0    b

# standard plot tools
# The trick here is to make two plots. The top plot has no x-axis, the bottom one no title. To make the two plot surfaces 
# equal in size the room reserved for x-axis and title is the same. 

par(mfrow=c(2,1),mar=c(0,4.1,4,2))
plot(y~x,
     data=datastart,
     axes=FALSE,
     frame.plot=TRUE,
     xlab='',
     main='bla bla',
     col=c('red','green')[datastart$type])
legend(x='topleft',
       legend=c('a','b'),
       col=c('red','green'),
       pch=1)
axis(side=2,las=1)
par(mar=c(4,4.1,0,2))
plot(y~x,
     data=datastart,
     axes=FALSE,
     frame.plot=TRUE,
     xlab='x',
     log='y',
     ylab='log(y)',
     col=c('red','green')[datastart$type]
)
axis(side=2,las=1)
axis(side=1)
# lattice
# As far as I understand, lattice does not have the tools to fiddle this extreme with axis. The trick then is to add a copy of
# the data on logarithmic scale and manually control the labels. Lattice does not understand that the x-axis are same, 
# but some suppression is possible.
data1=datastart
data2=datastart
data1$lab='linear'
data2$lab='log'
data2$y <- log10(data2$y)
dataplot <- rbind(data1,data2)

at2 <- axisTicks(range(data2$y),log=TRUE,nint=4)
at1 <- axisTicks(range(data1$y),log=FALSE,nint=5)
atx <- axisTicks(range(data1$x),log=FALSE,nint=5)
dataplot$lab <- factor(dataplot$lab,levels=c('linear','log'))
xyplot(y  ~ x | lab,groups=type,data=dataplot,layout=c(1,2),
       main='bla bla',
       as.table=TRUE,
       auto.key=TRUE,
       scales=list(relation='free',
                   x=list(at=list(NULL,atx)),
                   y=list(at=list(at1,log10(at2)),
                          labels=list(format(at1),format(at2))
                   )
       ))
# ggplot2
# The trick here is that ggplot2 can have a free y-axis, but you cannot set the labels per axis. Instead it is a common y-axis 
# which has adapted labels. ggplot chooses the range for the y-axis itself, you have to make sure that the labels you feed 
# it match that range. To make that fit in the end I just shifted the whole log part to a different range. Some of the code of 
# lattice is re-used.

dataplot2 <- dataplot
offset <- -10
breaks=c(-11,-10,-9)
dataplot2$y[dataplot2$lab=='log'] <- dataplot2$y[dataplot2$lab=='log'] +offset 
p <- ggplot(dataplot2, aes(x, y,colour=type)) + geom_point()
p + facet_grid(lab ~ .,
               scales='free_y') +
  labs(title = 'bla bla') +
  scale_y_continuous(
    breaks = c(breaks,at1),
    labels=c(format(10^(breaks-offset)),format(at1)))

###################################################
### code chunk number 24: unit2.Rnw:292-295
###################################################
oopar <- par(mar=c(1,1,1,1)+0.1)
library(maptools)
volcano_sl <- ContourLines2SLDF(contourLines(volcano))

volcano_sl$level1 <- as.numeric(volcano_sl$level)
pal <- terrain.colors(nlevels(volcano_sl$level))
plot(volcano_sl, bg="grey70", col=pal[volcano_sl$level1], lwd=3)
par(oopar)


###################################################
### code chunk number 25: unit2.Rnw:302-303
###################################################
options(width=35)


###################################################
### code chunk number 26: unit2.Rnw:306-307 (eval = FALSE)
###################################################
## library(maptools)
## volcano_sl <- ContourLines2SLDF(contourLines(volcano))
## 
## volcano_sl$level1 <- as.numeric(volcano_sl$level)
## pal <- terrain.colors(nlevels(volcano_sl$level))
## plot(volcano_sl, bg="grey70", col=pal[volcano_sl$level1], lwd=3)


###################################################
### code chunk number 27: unit2.Rnw:309-310
###################################################
options(width=60)


###################################################
### code chunk number 28: grdffreq (eval = FALSE)
###################################################
## meuseg1$ffreq1 <- as.numeric(meuseg1$ffreq)
## image(meuseg1, "ffreq1", col=cols)
## legend("topleft", legend=labs, fill=cols, bty="n")


###################################################
### code chunk number 29: unit2.Rnw:380-381
###################################################
options("width"=72)


###################################################
### code chunk number 30: unit2.Rnw:383-393
###################################################

storn2$LLoss<-log(storn2$Loss+.0001)

library(classInt)
library(RColorBrewer)
pal <- brewer.pal(3, "Blues")
q5 <- classIntervals(storn2$LLoss, n=5, style="quantile")
q5
fj5 <- classIntervals(storn2$LLoss, n=5, style="fisher")
fj5


###################################################
### code chunk number 31: unit2.Rnw:395-397 (eval = FALSE)
###################################################
## plot(q5, pal=pal)
## plot(fj5, pal=pal)


###################################################
### code chunk number 32: unit2.Rnw:399-400
###################################################
options("width"=36)


###################################################
### code chunk number 33: unit2.Rnw:411-414
###################################################
#oopar <- par(mar=c(3,3,3,1)+0.1)
plot(q5, pal=pal, main="Quantile", xlab="", ylab="")
#par(oopar)


###################################################
### code chunk number 34: unit2.Rnw:419-422
###################################################
#oopar <- par(mar=c(3,3,3,1)+0.1)
plot(fj5, pal=pal, main="Fisher-Jenks natural breaks", xlab="", ylab="")
#par(oopar)


###################################################
### code chunk number 35: unit2.Rnw:435-441
###################################################
#oopar <- par(mar=c(1,1,3,1)+0.1, bg="wheat1")
q5Colours <- findColours(q5, pal)
plot(storn2, col=q5Colours, pch=19)
title(main="Quantile")
legend("topleft", fill=attr(q5Colours, "palette"), legend=names(attr(q5Colours, "table")), bty="n")
#par(oopar)


###################################################
### code chunk number 36: unit2.Rnw:446-452
###################################################
#oopar <- par(mar=c(1,1,3,1)+0.1, bg="wheat1")
fj5Colours <- findColours(fj5, pal)
plot(storn2, col=fj5Colours, pch=19)
title(main="Fisher-Jenks natural breaks")
legend("topleft", fill=attr(fj5Colours, "palette"), legend=names(attr(fj5Colours, "table")), bty="n")
#par(oopar)


###################################################
### code chunk number 37: znbub (eval = FALSE)
###################################################
## library(lattice)
## print(bubble(storn2, "Loss", maxsize=2, key.entries=25*2^(0:4)))


###################################################
### code chunk number 38: unit2.Rnw:487-488
###################################################
library(lattice)
print(bubble(storn2, "Loss", maxsize=2, key.entries=25*2^(0:4)))


###################################################
### code chunk number 39: unit2.Rnw:495-496 (eval = FALSE)
###################################################
## library(lattice)
## print(bubble(storn2, "Loss", maxsize=2, key.entries=25*2^(0:4)))


###################################################
### code chunk number 40: distlev (eval = FALSE)
###################################################
## bpal <- colorRampPalette(pal)(6)
## print( spplot(storn2, "LLoss", col.regions=bpal, cuts=5) )


###################################################
### code chunk number 41: unit2.Rnw:514-515
###################################################
bpal <- colorRampPalette(pal)(6)
print( spplot(storn2, "LLoss", col.regions=bpal, cuts=5) )


## Probability and Monte Carlo methods
## http://www.r-bloggers.com/probability-and-monte-carlo-methods/

# Numerical integration

# A common use of the Monte Carlo method is to perform numerical integration on a function that may be difficult to integrate 
# analytically. This may seem surprising at first, but the intuition is rather straight forward. The key is to think about the 
# problem geometrically and connect this with probability. Let's take a simple polynomial function, say y = x^2 to illustrate 
# the idea.

# Suppose we want to find the integral of this function, but we don't know how to derive it analytically. All is not lost as 
# we can inscribe the graph into a confined box. Now if we randomly throw grains of rice (ideally points) into the box, the 
# ratio of the number of grains under the curve to the total area of the box will converge to the integral. Intuitively this 
# makes sense because if each point in the box has equal probability of being counted, then it is reasonable that the total 
# probability of the event (of a point being under the curve) is the same as the area under the curve. Indeed, plotting 10,000 
# random points seems to fill up the box uniformly.

n <- 10000
f <- function(x) x^2
plot(runif(n), runif(n), col='blue', pch=20)
curve(f, 0,1, n=100, col='white', add=TRUE)

# Now how do we get from a bunch of points uniformly distributed in a box to an approximation of the integral? To answer this, 
# let's think about that colloquial phrase "area under the curve". What this is telling us is that the points under the curve 
# are the important ones. Hence, for a given x value, the y value must be less than the function value at the same point.

ps <- matrix(runif(2*n), ncol=2)
g <- function(x,y) y <= x^2
z <- g(ps[,1], ps[,2])
plot(ps[!z,1], ps[!z,2], col='blue', pch=20)
points(ps[z,1], ps[z,2], col='green', pch=20)
curve(f, 0,1, n=100, col='white', add=TRUE)

# The punchline is that the integral is simply the count of all the points under the curve divided by the total number of 
# points, which is the probability that the point lands under the curve.

length(z[z]) / n
[1] 0.3325

# Note that this method is not limited to calculating the integral. It can even be used to approximate irrational numbers 
# like \pi. We'll explore this scenario a bit later.

# Approximation error and numerical stability

# Numerical approximation seems useful, but how do we know whether an approximation is good? To answer this we need to 
# consider the approximation error. Let's first look at how the approximation changes as we increase the number of points used.

ks <- 1:7
g <- function(k) {
  n <- 10^k
  f <- function(x,y) y <= x^2
  z <- f(runif(n), runif(n))
  length(z[z]) / n
}
a <- sapply(ks,g)

a
[1] 0.1000000 0.3300000 0.3220000 0.3293000 0.3317400 0.3332160 0.3334584

# Judging from this particular realization, it appears that approximation converges, albeit somewhat slowly. Remember that 
# each approximation above took an order of magnitude more samples to produce the result. With 1 million points, the error is 
# about 0.038%. How many points do we need to get to under 0.01%? From Grinstead & Snell we know that the error will be 
# within \frac{1}{\sqrt{n}} 95% of the time, which implies that a million trials should achieve a precision of 0.0003162278, 
# or 0.032%. Therefore we need to run 10 million trials to achieve this precision with 95% probability. As a sanity check, in 
# my first attempt I got 0.009071%, which looks good!

# When approximations improve as the number of iterations increase, this is known as numerical stability. Plotting out both 
# the theoretical limit and the actual errors shows that with enough terms the two appear to converge.

plot(ks, 1/sqrt(10^ks), type='l')
lines(ks, abs(1/3 - a), col='blue')

# Did we answer the question about knowing whether an approximation is good? No, not really. However, attempting to answer 
# this in the abstract is a bit of a fool's errand. The need for precision is case-specific so there are no fixed rules to 
# follow. It is akin to significance tests that are similarly case-specific. The key is to achieve enough precision that your 
# results don't create noise when they are used.

# Estimation of pi

# Now it's time to turn our attention to \pi. As mentioned earlier, it is possible to estimate \pi using a Monte Carlo approach
# and the same geometric insight. For this situation I use the equation of a circle to define the area. Since the unit circle 
# has area of \pi, a quarter of that will have area \frac{\pi}{4}. Hence, the end result will need to be multiplied by 4 to 
# get the final approximation.

g <- function(k) {
  n <- 10^k
  f <- function(x,y) sqrt(x^2 + y^2) <= 1
  z <- f(runif(n),runif(n))
  length(z[z]) / n
}
a <- sapply(1:7, g)
a*4
[1] 3.600000 3.160000 3.100000 3.142800 3.129320 3.142516 3.141445

# Similar to the approximation of \int_0^1 x^2 dx, the approximation of \pi appears to jump around despite converging to the 
# true value. The amount the value jumps around for a given number of trials is related to the approximation error. In addition
# to knowing how many iterations must be simulated to get a precise approximation, it is also important to know by how much a 
# given approximation can deviate. We can observe this by running the simulation with the same number of iterations over and 
# over.

trials <- 4 * sapply(rep(6,100), g)
e <- 1/sqrt(10^6)

mean(trials)
[1] 3.141468
length(trials[abs(trials - pi)/pi <= e])
[1] 95

# Approximation error aside, what is interesting about the Monte Carlo approach is that numerous problems can be solved by 
# transforming a problem into a form compatible with the Monte Carlo approach. Another such class of problems is optimization, 
# which is out of scope for this lecture.

# Simulations

A common use of Monte Carlo methods is for simulation. Rather than approximating a function or number, the goal is to 
# understand a distribution or set of outcomes based on simulating a number of paths through a process. As described in 
# Grinstead & Snell, a simple simulation is tossing a coin multiple times. Here we use a uniform distribution and transform 
# the real-valued output into the set \left\{ ]-1, 1 \right\} . (The sample function can do this directly, but this is more 
# illustrative.)

r <- runif(1000)
toss <- ifelse(r > .5, 1, -1)
plot(cumsum(toss), type='l')

# This simulation shows us what happens after randomly tossing a coin 1000 times. It is difficult to glean much information 
# from this, but if we do the same experiment 1000 times, now we can see a good representation of the possible outcomes. Given 
# this set of outcomes it is possible to compute statistics to characterize the properties of the distribution. This is useful 
# when a process doesn't follow a known distributions.

outcomes <- sapply(1:1000, function(i) sum(ifelse(runif(1000) > .5, 1, -1)))
hist(outcomes)

## Medley

## Library

library(Hmisc)
library(gbm)
library(stringr)
library(BMA)
library(devtools)
library(medley)
library(leaps)
library(tree)
library(randomForest)
library(splines)
library(boot)
library(ElemStatLearn)
library(medley)
library(e1071)
library(ipred)
library(party)

con <- file("./data/samsungDatab.csv")
samsungData <- read.csv2(con)
close(con)

## Summary stats
head(samsungData)
summary(samsungData)
sapply(samsungData[1,], class)

names(samsungData)
table(samsungData$activity)
table(samsungData$subject)

samsungData$activity.N <- as.numeric(samsungData$activity)
y <- samsungData$activity.N

# train <- sample(1:1000,size=750) # En las transparencias size=500
# trainData <- data[train,]; 
# testData <- data[-train,]

# trainData <- samsungData[1:sum(table(samsungData$subject)[1:14]),] # trains 2 <- samsungData[1:4694,]
# testData <- samsungData[-(1:4694),]

trainData <- samsungData[1:4694,]
testData <- samsungData[-(1:4694),]

# train2 <- samsungData[1:sum(table(samsungData$subject)[1:14]),] # trains 2 <- samsungData[1:4694,]
# test2 <- samsungData[-(1:4694),]

## Basic models

lm1 <- lm(activity.N ~., data=trainData)
rmse(predict(lm1, data=testData), testData$activity.N)

tree1 <- tree(activity.N ~., data=trainData)
rmse(predict(tree1, data=testData), testData$activity.N)

tree2 <- tree(activity.N ~.,data=trainData[sample(1:dim(trainData)[1]),])
rmse(predict(tree2, data=testData), testData$activity.N[sample(1:dim(trainData)[1]),])

## Combining models
combine1 <- predict(lm1, data=testData)/2 + predict(tree1, data=testData)/2
rmse(combine1, testData$activity.N)

combine2 <- (predict(lm1, data=testData)/3 + predict(tree1, data=testData)/3
             + predict(tree2, data=testData)/3)
rmse(combine2, testData$activity.N)

## Medley package

#library(devtools)
#install_github("medley","mewo2")
library(medley)
library(e1071)

x <- trainData[,-c(563,564)]
y <- trainData$activity.N
newx <- testData[,-c(563,564)]

## Blending models (part 1)
m <- create.medley(x, y, errfunc=rmse);
for (g in 1:10) {
  m <- add.medley(m, svm, list(gamma=1e-3 * g/10));
}


## Blending models (part 2)
for (mt in 1:10) {
  m <- add.medley(m, randomForest, list(mtry=mt));
}

m <- add.medley(m, bic.glm, glm.family="gaussian")
rmse(predict(m, newx), testData$y)

m <- prune.medley(m, 0.8);
rmse(predict(m, newx), testData$y)

## http://joelcadwell.blogspot.com.es/2013/12/feature-prioritization-multiple.html

library(psych)
d<-c(1.50,1.25,1.00,.25,0,
     -.25,-1.00,-1.25,-1.50)
set.seed(12413)
bar<-sim.poly.npl(nvar = 9, n = 200, 
                  low=-1, high=1, a=NULL, 
                  c=0, z=1, d=d, mu=0, 
                  sd=1, cat=4)
ratings<-bar$items+1

library(gplots)
feature<-apply(ratings,2,mean)
person<-apply(ratings,1,sum)
ratingsOrd<-ratings[order(person),
                    order(feature)]
heatmap.2(as.matrix(ratingsOrd), Rowv=FALSE, 
          Colv=FALSE, dendrogram="none", 
          col=redblue(16), key=FALSE, 
          keysize=1.5, density.info="none", 
          trace="none", labRow=NA)

F.ratings<-data.frame(ratings)
F.ratings[]<-lapply(F.ratings, factor)
str(F.ratings)

library(FactoMineR)
mca<-MCA(F.ratings)
summary(mca)

categories<-mca$var$coord[,1:2]
categories[,1]<--categories[,1]
categories

feature_label<-c(rep(1,4),rep(2,4),rep(3,4),
                 rep(4,4),rep(5,4),rep(6,4),
                 rep(7,4),rep(8,4),rep(9,4))
category_color<-rep(c("darkred","red",
                      "blue","darkblue"),9)
category_size<-rep(c(1.1,1,1,1.1),9)
plot(categories, type="n")
text(categories, labels=feature_label, 
     col=category_color, cex=category_size)

mca2<-mca
mca2$var$coord[,1]<--mca$var$coord[,1]
mca2$ind$coord[,1]<--mca$ind$coord[,1]
plot(mca2, choix="ind", label="none")


## http://joelcadwell.blogspot.com.es/2013/12/the-complexities-of-customer.html

library(psych)
d1<-c(1.50,1.25,1.00,.25,0,-.25,
      -1.00,-1.25,-1.50)
d2<-c(1.50,1.25,1.00,-1.50,-1.25,-1.00,
      .25,0,-.25)

set.seed(12413)
bar1<-sim.poly.npl(nvar = 9, n = 100, 
                   low=-1, high=1, a=NULL, 
                   c=0, z=1, d=d1, 
                   mu=0, sd=1.5, cat=4)
bar2<-sim.poly.npl(nvar = 9, n = 100, 
                   low=-1, high=1, a=NULL, 
                   c=0, z=1, d=d2, 
                   mu=0, sd=1.5, cat=4)
rating1<-data.frame(bar1$items+1)
rating2<-data.frame(bar2$items+1)
apply(rating1,2,table)
apply(rating2,2,table)
ratings<-rbind(rating1,rating2)

kcl<-kmeans(ratings, 2, nstart=25)
kcl

rowmean<-apply(ratings, 1, mean)
ipsative<-sweep(ratings, 1, rowmean, "-")
round(apply(ipsative,1,sum),8)
kcl_rc<-kmeans(ipsative, 2, nstart=25)
kcl_rc
table(c(rep(1,100),rep(2,100)),
      kcl_rc$cluster)

F.ratings<-data.frame(ratings)
F.ratings[]<-lapply(F.ratings, factor)

library(FactoMineR)
mca<-MCA(F.ratings)

categories<-mca$var$coord[,1:2]
categories[,1]<--categories[,1]
categories

feature_label<-c(rep(1,4),rep(2,4),rep(3,4),
                 rep(4,4),rep(5,4),rep(6,4),
                 rep(7,4),rep(8,4),rep(9,4))
category_color<-rep(c("darkred","red",
                      "blue","darkblue"),9)
category_size<-rep(c(1.1,1,1,1.1),9)
plot(categories, type="n")
text(categories, labels=feature_label, 
     col=category_color, cex=category_size)

mca2<-mca
mca2$var$coord[,1]<--mca$var$coord[,1]
mca2$ind$coord[,1]<--mca$ind$coord[,1]
plot(mca2$ind$coord[,1:2], col=kcl$cluster, pch="*")
plot(mca2$ind$coord[,1:2], col=kcl_rc$cluster, pch="*")

pca<-PCA(ipsative)
plot(pca$ind$coord[,1:2], col=kcl_rc$cluster, pch="*")
arrows(0, 0, 3.2*pca$var$coord[,1], 
       3.2*pca$var$coord[,2], col = "chocolate", 
       angle = 15, length = 0.1)
text(3.2*pca$var$coord[,1], 3.2*pca$var$coord[,2],
     labels=1:9)

## Continue
## http://joelcadwell.blogspot.com.es/2013/12/latent-variable-mixture-models-lvmm.html


library(mixRasch)

# need to set the seed only if
# we want the same result each
# time we run mixRasch
set.seed(20131218)
mR2<-mixRasch(ratings, max.iter=500,
              steps=3, model="PCM", n.c=2)
mR2

# shows the list structure 
# containing the output
str(mR2)

# latent cluster membership
# probility and max classification
round(mR2$class,2)
cluster<-max.col(mR2$class)

# comparison with simulated data
table(c(rep(1,100),rep(2,100)),cluster)

# comparison with row-centered
# kmeans from last post
table(cluster,kcl_rc$cluster)

# Split iris into new, different training (70%) and test (30%) subsets

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

trainData <- iris[ind==1,]
testData  <- iris[ind==2,]

# we load randomForest
library(randomForest)

# we train randomForest predict species using all variables

# two limitations of randomForest function:
# 1) cannot handle data with missing values so users must impute values for those missing
# 2) 32 maximum number of levels for each categorical variable; must transform them

# build same model as before; are predicting Species based on all other variables:

rf <- randomForest(Species ~ ., data=trainData, ntree=100)

# show conditional table cells' frequencies
table(predict(rf), trainData$Species)

# we take a look at the results
print(rf)
?gini
# show the components of the output 'rf' object
attributes(rf)

# plot error rates with various number of trees
plot(rf)

# find importance of variables
importance(rf)

# plots importance
varImpPlot(rf)

# test random forest using test data
irisPred <- predict(rf, newdata=testData)

# check the results
table(irisPred, testData$Species)

# check margins of data which is proportion of votes for the correct class minus maximum proportion of
# votes for other classes. Positive margin indicates correct classification

plot(margin(rf, testData$Species))

######################################
#####        RANDOM FOREST       #####
#####          IRIS DATA         #####
######################################

# iris is a flower
head(iris)

# 'species' is categorical, four numeric
str(iris)

# we split iris into training (70%) and test (30%) subsets

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))

trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# we load randomForest
library(randomForest)

# we train randomForest predict species using all variables
rf <- randomForest(Species ~ ., data=trainData, ntree=100)
table(predict(rf), trainData$Species)

# we take a look
print(rf)

attributes(rf)

# plot error rates with various number of trees
plot(rf)

# find importance of variables
importance(rf)

# plots importance
varImpPlot(rf)

# test random forest using test data
irisPred <- predict(rf, newdata=testData)

# check the results
table(irisPred, testData$Species)

# check margins of data which is proportion of votes for the correct class minus maximum proportion of
# votes for other classes. Positive margin indicates correct classification

plot(margin(rf, testData$Species))

# Make a random Forest model using system.time to calculate time
library(ElemStatLearn)
system.time(rf.spam <- randomForest(spam ~ ., data = spam)) 

# e) Suppose, we get a new email and want to predict the spam label. For simplicity, 
#    we refit the Random Forest on 2601 randomly chosen emails and save the remaining 2000 
#    emails as test set. How does the OOB error compare with the error on the test set? 
#    (use ntree = 100, and set.seed = 123)

set.seed(123)
idx <- sample(1:nrow(spam), 2601)
dTrain <- spam[idx,]
dTest <- spam[-idx,]
rf.train <- randomForest(spam ~ ., data = dTrain, ntree = 100)
rf.train ## OOB error: 4.8%
pred.rf <- predict(rf.train, newdata = dTest)
mean(pred.rf != dTest$spam) ## Test error: 5.4%

# i.e. OOB error is a very good approximation

# f) Suppose we don't want to compute all variables for each new incoming mail, but only use the 
#    best 5. Which 5 variables should we choose? (according to decrease in accuracy; use ntree = 100 and seed = 123).

set.seed(123)

rf.spam <- randomForest(spam ~ ., 
                        data = spam, 
                        importance = TRUE, 
                        ntree = 100)
tmp <- varImpPlot(rf.spam, n.var = ncol(spam)-1)

rf.spam ## OOB error: 2.8%

## http://www.r-bloggers.com/google-maps-and-ggmap/
## Google Maps and ggmap
## December 22, 2013
## By Ralph

# The ggmap package can be used to access maps from the Google Maps API and there are a number of examples on various statistics 
# related blogs. These include here, here and here.

# The ggmap package has a function get_map that can download maps from various sources including Google Maps.

require(ggmap)

# The first example specifies the longitude and latitude close to the London 2012 Olympic park from Google and selects the 
# satellite map type. The extent="device" argument stretches the map to fill the whole graphics device.

mapImageData1 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 17)

ggmap(mapImageData1,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")

# London 2012 Olympic Stadium Google Map Example 1

# The second example is based on the terrain map type which looks slightly odd.

mapImageData2 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "terrain",
                         zoom = 16)

ggmap(mapImageData2,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")

# London 2012 Olympic Stadium Google Map Example 2

# The third example is roadmap and is uncluttered and provides an overview of the surroundings.

mapImageData3 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "roadmap",
                         zoom = 16)

ggmap(mapImageData3,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")

# London 2012 Olympic Stadium Google Map Example 3

# The final example is a combination of the satellite image and some road and location names.

mapImageData4 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "hybrid",
                         zoom = 15)

ggmap(mapImageData4,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")

## Example: Old faithful eruptions
## Coursera Data Analysis 8.3 Prediction with regression models
## Sampling
data(faithful)
dim(faithful)
names(faithful)
str(faithful)

set.seed(333)
trainSamples <- sample(1:272,size=(272/2),replace=F)
trainFaith <- faithful[trainSamples,]
testFaith <- faithful[-trainSamples,]
head(trainFaith)

# Eruption duration versus waiting time
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

# Fit a linear model
# E = + W + D i b 0 b 1 T i e i
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

# Model fit
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

# Predict a new value
coef(lm1)[1] + coef(lm1)[2]*80
# (Intercept)
# 4.186
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

# Plot predictions - training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration",main="Train set")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration", main="Test set")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# Get training set/test set errors
# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# [1] 5.713
# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))
# [1] 5.827

# Prediction intervals
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",main="Test set")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

# Ravens Data
download.file("https://dl.dropbox.com/u/7710864/data/ravensData.rda", destfile="./data/ravensData.rda", method="curl")
load("./ravensData.rda")
head(ravensData)

# Fit a logistic regression
glm1 <- glm(ravenWinNum ~ ravenScore,family="binomial",data=ravensData)
par(mfrow=c(1,2))
boxplot(predict(glm1) ~ ravensData$ravenWinNum, col="blue")
boxplot(predict(glm1,type="response") ~ ravensData$ravenWinNum,col="blue")

# Choosing a cutoff (re-substitution)
xx <- seq(0,1,length=10); err <- rep(NA,10)
for(i in 1:length(xx)){
  err[i] <- sum((predict(glm1,type="response") > xx[i]) != ravensData$ravenWinNum)
}
plot(xx,err,pch=19,xlab="Cutoff",ylab="Error")

# Comparing models with cross validation
library(boot)
cost <- function(win, pred = 0) mean(abs(win-pred) > 0.5)
glm1 <- glm(ravenWinNum ~ ravenScore,family="binomial",data=ravensData)
glm2 <- glm(ravenWinNum ~ ravenScore,family="gaussian",data=ravensData)
cv1 <- cv.glm(ravensData,glm1,cost,K=3)
cv2 <- cv.glm(ravensData,glm2,cost,K=3)
cv1$delta
# [1] 0.350 0.365
cv2$delta
# [1] 0.40 0.42

## Coursera Data Analysis 8.4 Prediction with trees

# Example: Iris Data
data(iris)
names(iris)
# [1] "Sepal.Length" "Sepal.Width" "Petal.Length" "Petal.Width" "Species"
table(iris$Species)

# Iris petal widths/sepal width
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

# Iris petal widths/sepal width
# An alternative is library(rpart)
library(tree)
tree1 <- tree(Species ~ Sepal.Width + Petal.Width,data=iris)
summary(tree1)
plot(tree1)
text(tree1)

# Another way of looking at a CART model
# This separate every region of model
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE) 
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

# Predicting new values
set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20,0,2.5),Sepal.Width = runif(20,2,4.5))
pred1 <- predict(tree1,newdata)
pred1

# Overlaying new values
pred1 <- predict(tree1,newdata,type="class")
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19)
partition.tree(tree1,"Species",add=TRUE)

# Pruning trees example: Cars
data(Cars93,package="MASS")
head(Cars93)

# Build a tree
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags +
                   EngineSize + Width + Length + Weight + Price + Cylinders +
                   Horsepower + Wheelbase,data=Cars93)
plot(treeCars)
text(treeCars)

# Plot errors
par(mfrow=c(1,2))
plot(cv.tree(treeCars,FUN=prune.tree,method="misclass"))
plot(cv.tree(treeCars))

# Prune the tree
pruneTree <- prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)

# Show resubstitution error  ???
table(Cars93$DriveTrain,predict(pruneTree,type="class"))
table(Cars93$DriveTrain,predict(treeCars,type="class"))


## Coursera Data Analysis. 9.1 Smoothing

download.file("https://spark-public.s3.amazonaws.com/dataanalysis/cd4.data",
              destfile="./data/cd4.data", method="curl")
cd4Data <- read.table("cd4.data", col.names=c("time", "cd4", "age", "packs", "drugs", "sex", "cesd", "id"))
cd4Data <- cd4Data[order(cd4Data$time),]
head(cd4Data)

# CD4 over time
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)

# Average first 2 points
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
points(mean(cd4Data$time[1:2]),mean(cd4Data$cd4[1:2]),col="blue",pch=19)

# Average second and third points
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
points(mean(cd4Data$time[1:2]),mean(cd4Data$cd4[1:2]),col="blue",pch=19)
points(mean(cd4Data$time[2:3]),mean(cd4Data$cd4[2:3]),col="blue",pch=19)

# A moving average
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
aveTime <- aveCd4 <- rep(NA,length(3:(dim(cd4Data)[1]-2)))
for(i in 3:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-2):(i+2)])
  aveCd4[i] <- mean(cd4Data$cd4[(i-2):(i+2)])
}
lines(aveTime,aveCd4,col="blue",lwd=3)

# Average more points
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
aveTime <- aveCd4 <- rep(NA,length(11:(dim(cd4Data)[1]-10)))
for(i in 11:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-10):(i+10)])
  aveCd4[i] <- mean(cd4Data$cd4[(i-10):(i+10)])
}
lines(aveTime,aveCd4,col="blue",lwd=3)

# Average many more
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
aveTime <- aveCd4 <- rep(NA,length(201:(dim(cd4Data)[1]-200)))
for(i in 201:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-200):(i+200)])
  aveCd4[i] <- mean(cd4Data$cd4[(i-200):(i+200)])
}
lines(aveTime,aveCd4,col="blue",lwd=3)

# A faster way
filtTime <- as.vector(filter(cd4Data$time,filter=rep(1,200))/200)
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=rep(1,200))/200)
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1); lines(filtTime,filtCd4,col="blue",lwd=3)

# Averaging = weighted sums
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=rep(1,4))/4)
filtCd4[2]
# [1] 607.5
sum(cd4Data$cd4[1:4] * rep(1/4,4))

# Other weights -> should sum to one
ws = 10; tukey = function(x) pmax(1 - x^2,0)^2
filt= tukey(seq(-ws,ws)/(ws+1));filt=filt/sum(filt)
plot(seq(-(ws),(ws)),filt,pch=19)

# Other weights -> should sum to one
ws = 100; tukey = function(x) pmax(1 - x^2,0)^2
filt= tukey(seq(-ws,ws)/(ws+1));filt=filt/sum(filt)
filtTime <- as.vector(filter(cd4Data$time,filter=filt))
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=filt))
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1); lines(filtTime,filtCd4,col="blue",lwd=3)

# Lowess (loess)
lw1 <- loess(cd4 ~ time,data=cd4Data)
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
lines(cd4Data$time,lw1$fitted,col="blue",lwd=3)

# Span
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1,ylim=c(500,1500))
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.1)$fitted,col="blue",lwd=3)
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.25)$fitted,col="red",lwd=3)
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.76)$fitted,col="green",lwd=3)

# Predicting with loess
tme <- seq(-2,5,length=100); pred1 = predict(lw1,newdata=data.frame(time=tme),se=TRUE)
plot(tme,pred1$fit,col="blue",lwd=3,type="l",ylim=c(0,2500))
lines(tme,pred1$fit + 1.96*pred1$se.fit,col="red",lwd=3)
lines(tme,pred1$fit - 1.96*pred1$se.fit,col="red",lwd=3)
points(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)

# Splines in R
library(splines)
ns1 <- ns(cd4Data$time,df=3)
par(mfrow=c(1,3))
plot(cd4Data$time,ns1[,1]); plot(cd4Data$time,ns1[,2]); plot(cd4Data$time,ns1[,3])

# Regression with splines
lm1 <- lm(cd4Data$cd4 ~ ns1)
summary(lm1)

# Fitted values
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
points(cd4Data$time,lm1$fitted,col="blue",pch=19,cex=0.5)

## Coursera Data Analysis 9.2. Bootstraping

# Example

set.seed(333); x <- rnorm(30)
bootMean <- rep(NA,1000); sampledMean <- rep(NA,1000)
for(i in 1:1000){bootMean[i] <- mean(sample(x,replace=TRUE))}
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))}
plot(density(bootMean)); lines(density(sampledMean),col="red")

# Example with boot package
set.seed(333); x <- rnorm(30); sampledMean <- rep(NA,1000)
for(i in 1:1000){sampledMean[i] <- mean(rnorm(30))}
meanFunc <- function(x,i){mean(x[i])}
bootMean <- boot(x,meanFunc,1000)
bootMean

# Plotting boot package example
plot(density(bootMean$t)); lines(density(sampledMean),col="red")

# Nuclear costs
library(boot); data(nuclear)
nuke.lm <- lm(log(cost) ~ date,data=nuclear)
plot(nuclear$date,log(nuclear$cost),pch=19)
abline(nuke.lm,col="red",lwd=3)

par(mfrow=c(1,3))
for(i in 1:3){
  nuclear0 <- nuclear[sample(1:dim(nuclear)[1],replace=TRUE),]
  nuke.lm0 <- lm(log(cost) ~ date,data=nuclear0)
  plot(nuclear0$date,log(nuclear0$cost),pch=19)
  abline(nuke.lm0,col="red",lwd=3)
}

# Bootstrap distribution
bs <- function(data, indices,formula) {
  d <- data[indices,];fit <- lm(formula, data=d);return(coef(fit))
}
results <- boot(data=nuclear, statistic=bs, R=1000, formula=log(cost) ~ date)
plot(density(results$t[,2]),col="red",lwd=3)
lines(rep(nuke.lm$coeff[2],10),seq(0,8,length=10),col="blue",lwd=3)

# Bootstrap confidence intervals
boot.ci(results)

# Bootstrapping from a model
resid <- rstudent(nuke.lm)
fit0 <- fitted(lm(log(cost) ~ 1,data=nuclear))
newNuc <- cbind(nuclear,resid=resid,fit0=fit0)
bs <- function(data, indices) {
  return(coef(glm(data$fit0 + data$resid[indices] ~ data$date,data=data)))
}
results <- boot(data=newNuc, statistic=bs, R=1000)

# Results
plot(density(results$t[,2]),lwd=3,col="blue")
lines(rep(coef(nuke.lm)[2],10),seq(0,3,length=10),col="red",lwd=3)

# An empirical p-value
B <- dim(results$t)[1]
(1 + sum((abs(results$t[,2]) > abs(coef(nuke.lm)[2]))))/(B+1)
# [1] 0.1838

# Bootstrapping non-linear statistics
set.seed(555); x <- rnorm(30); sampledMed <- rep(NA,1000)
for(i in 1:1000){sampledMed[i] <- median(rnorm(30))}
medFunc <- function(x,i){median(x[i])}; bootMed <- boot(x,medFunc,1000)
plot(density(bootMed$t),col="red",lwd=3)
lines(density(sampledMed),lwd=3)

# Things you can't bootstrap (max)
set.seed(333); x <- rnorm(30); sampledMax <- rep(NA,1000)
for(i in 1:1000){sampledMax[i] <- max(rnorm(30))}
maxFunc <- function(x,i){max(x[i])}; bootMax <- boot(x,maxFunc,1000)
plot(density(bootMax$t),col="red",lwd=3,xlim=c(1,3))
lines(density(sampledMax),lwd=3)

## Coursera Data Analysis 9.3. Bootstraping for prediction

# Bootstrapping prediction errors
library(boot); data(nuclear)
nuke.lm <- lm(log(cost) ~ date,data=nuclear)
plot(nuclear$date,log(nuclear$cost),pch=19)
abline(nuke.lm,col="red",lwd=3)

newdata <- data.frame(date = seq(65,72,length=100))
nuclear <- cbind(nuclear,resid=rstudent(nuke.lm),fit=fitted(nuke.lm))
nuke.fun <- function(data,inds,newdata){
  lm.b <- lm(fit + resid[inds] ~ date,data=data)
  pred.b <- predict(lm.b,newdata)
  return(pred.b)
}
nuke.boot <- boot(nuclear,nuke.fun,R=1000,newdata=newdata)
head(nuke.boot$t)

pred <- predict(nuke.lm,newdata)
predSds <- apply(nuke.boot$t,2,sd)
plot(newdata$date,pred,col="black",type="l",lwd=3,ylim=c(0,10))
lines(newdata$date,pred + 1.96*predSds,col="red",lwd=3)
lines(newdata$date,pred - 1.96*predSds,col="red",lwd=3)

# Bagged loess

library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1],replace=T)
  ozone0 <- ozone[ss,] 
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
  ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

# Iris data
data(iris)
head(iris)

# Bagging a tree
library(ipred)
bagTree <- bagging(Species ~.,data=iris,coob=TRUE)
print(bagTree)

# Looking at bagged tree one
bagTree$mtrees[[1]]$btree

# Looking at bagged tree two
bagTree$mtrees[[2]]$btree

# Random forests
library(randomForest)
forestIris <- randomForest(Species~ Petal.Width + Petal.Length,data=iris,prox=TRUE)
forestIris

# Getting a single tree
getTree(forestIris,k=2)

# Class "centers"
iris.p <- classCenter(iris[,c(3,4)], iris$Species, forestIris$prox)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
     bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
     main="Iris Data with Prototypes")
points(iris.p[,1], iris.p[,2], pch=21, cex=2, bg=c("red", "blue", "green"))

# Combining random forests
forestIris1 <- randomForest(Species~Petal.Width + Petal.Length,data=iris,prox=TRUE,ntree=50)
forestIris2 <- randomForest(Species~Petal.Width + Petal.Length,data=iris,prox=TRUE,ntree=50)
forestIris3 <- randomForest(Species~Petal.Width + Petal.Length,data=iris,prox=TRUE,nrtee=50)
combine(forestIris1,forestIris2,forestIris3)

# Predicting new values
newdata <- data.frame(Sepal.Length <- rnorm(1000,mean(iris$Sepal.Length), sd(iris$Sepal.Length)),
                      Sepal.Width  <- rnorm(1000,mean(iris$Sepal.Width), sd(iris$Sepal.Width)),
                      Petal.Width  <- rnorm(1000,mean(iris$Petal.Width), sd(iris$Petal.Width)),
                      Petal.Length <- rnorm(1000,mean(iris$Petal.Length), sd(iris$Petal.Length)))
pred <- predict(forestIris,newdata)

plot(newdata[,4], newdata[,3], pch=21, xlab="Petal.Length",ylab="Petal.Width",
     bg=c("red", "blue", "green")[as.numeric(pred)],main="newdata Predictions")

## Coursera Data Analysis 9.3. Combining predictors

# Example
#library(devtools)
#install_github("medley","mewo2")
library(medley)
set.seed(453234)
y  <- rnorm(1000)
x1 <- (y > 0) 
x2 <- y*rnorm(1000)
x3 <- rnorm(1000,mean=y,sd=1)
x4 <- (y > 0) & (y < 3)
x5 <- rbinom(1000,size=4,prob=exp(y)/(1+exp(y)))
x6 <- (y < -2) | (y > 2)
data <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6)
train <- sample(1:1000,size=500)
trainData <- data[train,]
testData <- data[-train,]

# Basic models
library(tree)
lm1 <- lm(y ~.,data=trainData)
rmse(predict(lm1,data=testData),testData$y)
# [1] 1.294
tree1 <- tree(y ~.,data=trainData)
rmse(predict(tree1,data=testData),testData$y)
# [1] 1.299
tree2 <- tree(y~.,data=trainData[sample(1:dim(trainData)[1]),])

# Combining models
combine1 <- predict(lm1,data=testData)/2 + predict(tree1,data=testData)/2
rmse(combine1,testData$y)
# [1] 1.281
combine2 <- (predict(lm1,data=testData)/3 + predict(tree1,data=testData)/3
             + predict(tree2,data=testData)/3)
rmse(combine2,testData$y)
# [1] 1.175

# Medley package
# http://www.kaggle.com/users/10748/martin-o-leary

#library(devtools)
#install_github("medley","mewo2")
library(medley)
library(e1071)
library(randomForest)
x <- trainData[,-1]
y <- trainData$y
newx <- testData[,-1]

# Blending models
m <- create.medley(x, y, errfunc=rmse);
for (g in 1:10) {
  m <- add.medley(m, svm, list(gamma=1e-3 * g));
}

for (mt in 1:2) {
  m <- add.medley(m, randomForest, list(mtry=mt));
}
CV model 11 randomForest (mtry = 1) time: 2.015 error: 0.4668
CV model 12 randomForest (mtry = 2) time: 3.532 error: 0.4135
m <- prune.medley(m, 0.8);
rmse(predict(m,newx),testData$y)


## Coursera Data Analysis 10.1. Multiple testing

# Case study I: no true positives
set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
  y <- rnorm(20)
  x <- rnorm(20)
  pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
# Controls false positive rate
sum(pValues < 0.05)

# Controls FWER
sum(p.adjust(pValues,method="bonferroni") < 0.05)
# [1] 0
# Controls FDR
sum(p.adjust(pValues,method="BH") < 0.05)
# [1] 0

# Case study II: 50% true positives
set.seed(1010093)
pValues <- rep(NA,1000)
for(i in 1:1000){
  x <- rnorm(20)
  # First 500 beta=0, last 500 beta=2
  if(i <= 500){y <- rnorm(20)}else{ y <- rnorm(20,mean=2*x)}
  pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
trueStatus <- rep(c("zero","not zero"),each=500)
table(pValues < 0.05, trueStatus)

# Controls FWER
table(p.adjust(pValues,method="bonferroni") < 0.05,trueStatus)
# trueStatus
# not zero zero
# FALSE 23 500
# TRUE 477 0
# Controls FDR
table(p.adjust(pValues,method="BH") < 0.05,trueStatus)

# P-values versus adjusted P-values
par(mfrow=c(1,2))
plot(pValues,p.adjust(pValues,method="bonferroni"),pch=19)
plot(pValues,p.adjust(pValues,method="BH"),pch=19)

## Coursera Data Analysis 10.2. Simulation for model checking

# Simulating data from a model
# Suppose that you have a regression model
# Here is an example of generating data from this model where  and are normal:

set.seed(44333)
x  <- rnorm(50)
e  <- rnorm(50)
b0 <- 1 
b1 <- 2
y  <- b0 + b1*x + e

# Violating assumptions
set.seed(44333)
x  <- rnorm(50)
e  <- rnorm(50) 
e2 <- rcauchy(50)
b0 <- 1
b1 <- 2
y  <- b0 + b1*x + e
y2 <- b0 + b1*x + e2

par(mfrow=c(1,2))
plot(lm(y ~ x)$fitted,lm(y~x)$residuals,pch=19,xlab="fitted",ylab="residuals")
plot(lm(y2 ~ x)$fitted,lm(y2~x)$residuals,pch=19,xlab="fitted",ylab="residuals")

# Repeated simulations
set.seed(44333)
betaNorm <- betaCauch <- rep(NA,1000)
for(i in 1:1000){
  x  <- rnorm(50) 
  e  <- rnorm(50); e2 <- rcauchy(50) 
  b0 <- 1; b1 <- 2
  y  <- b0 + b1*x + e 
  y2 <- b0 + b1*x + e2
  betaNorm[i]  <- lm(y ~ x)$coeff[2] 
  betaCauch[i] <- lm(y2 ~ x)$coeff[2]
}
quantile(betaNorm)
# 0% 25% 50% 75% 100%
# 1.500 1.906 2.013 2.100 2.596
quantile(betaCauch)

# Monte Carlo Error
boxplot(betaNorm,betaCauch,col="blue",ylim=c(-5,5))

# Simulation based on a data set
library(UsingR) 
data(galton) 
nobs <- dim(galton)[1]
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

# Calculating means,variances
lm1 <- lm(galton$child ~ galton$parent)
parent0 <- rnorm(nobs,sd=sd(galton$parent),mean=mean(galton$parent))
child0 <- lm1$coeff[1] + lm1$coeff[2]*parent0 + rnorm(nobs,sd=summary(lm1)$sigma)
par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19)
plot(parent0,child0,pch=19,col="blue")

# Simulating more complicated scenarios
library(bootstrap)
data(stamp)
nobs <- dim(stamp)[1]
hist(stamp$Thickness,col="grey",breaks=100,freq=F)
dens <- density(stamp$Thickness)
lines(dens,col="blue",lwd=3)

# A simulation that is too simple
plot(density(stamp$Thickness),col="black",lwd=3)
for(i in 1:10){
  newThick <- rnorm(nobs,mean=mean(stamp$Thickness),sd=sd(stamp$Thickness))
  lines(density(newThick),col="grey",lwd=3)
}

# Simulating from the density estimate
plot(density(stamp$Thickness),col="black",lwd=3)
for(i in 1:10){
  newThick <- rnorm(nobs,mean=stamp$Thickness,sd=dens$bw)
  lines(density(newThick),col="grey",lwd=3)
}

# Increasing variability
plot(density(stamp$Thickness),col="black",lwd=3)
for(i in 1:10){
  newThick <- rnorm(nobs,mean=stamp$Thickness,sd=dens$bw*1.5)
  lines(density(newThick,bw=dens$bw),col="grey",lwd=3)
}

# Look at age patterns
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

which(combi$Embarked == '')

# Find the indexes for the tile piece of the name
strsplit(combi$name[1], split='[,.]')
strsplit(combi$name[1], split='[,.]')[[1]]
strsplit(combi$name[1], split='[,.]')[[1]][2]

# Delete a row

dat <- data.frame(A = 1:3, B = 1:3)
dat
# A B
# 1 1 1
# 2 2 2
# 3 3 3
dat <- dat[-1,]
dat
# A B
# 2 2 2
# 3 3 3

###############################################################################
## The traveling salesperson problem (TSP) salesman
## Problema del viajante
## http://tsp.r-forge.r-project.org/

# load library and read data
library("TSP")
data("USCA312")

# create a TSP object from the data 
tsp <- TSP(USCA312)
tsp

# find a 2-optimal solution 
tour <- solve_TSP(tsp, method = "2-opt")
tour

## Problema del viajante
## TSP Package

# Comparing some heuristics

library("TSP")
data("USCA50")
USCA50

methods <- c("nearest_insertion", "farthest_insertion", "cheapest_insertion",
             "arbitrary_insertion", "nn", "repetitive_nn", "2-opt")
tours <- sapply(methods, FUN = function(m) solve_TSP(USCA50, method = m),
                simplify = FALSE)

str(tours)
names(tours)
plot(tours$nn)
summary(tours)

## names(tours) <- methods
## tours$concorde <- solve_TSP(tsp, method = "concorde")
## tours$concorde <- solve_TSP(tsp, method = "linkern")

tours[[1]]
 
## opt <- 14497
dotchart(c(sapply(tours, FUN = attr, "tour_length"), optimal = 14497),
         xlab = "tour length", xlim = c(0, 20000))
## add optimum
# abline(v = opt, col = "red")
# mtext("optimum", at = opt, side = 3, col = "red")

# Finding the shortest Hamiltonian path

library("TSP")
data("USCA312")

tsp <- insert_dummy(USCA312, label = "cut")
tsp

tour <- solve_TSP(tsp, method="farthest_insertion")
# tour <- solve_TSP(tsp, method ="concorde")
tour

# Since the dummy city has distance zero to all other cities, the path length is equal to the tour
# length reported above. The path starts with the first city in the list after the 'dummy' city
# and ends with the city right before it. We use cut_tour() to create a path and show the first
# and last 6 cities on it.

path <- cut_tour(tour, "cut")
head(labels(path))
tail(labels(path))

library("maps")
library("sp")
library("maptools")
data("USCA312_map")
plot_path <- function(path){
  plot(as(USCA312_coords, "Spatial"), axes = TRUE)
  plot(USCA312_basemap, add = TRUE, col = "gray")
  points(USCA312_coords, pch = 3, cex = 0.4, col = "red")
  path_line <- SpatialLines(list(Lines(list(Line(USCA312_coords[path,])), ID="1")))
  plot(path_line, add=TRUE, col = "black")
  points(USCA312_coords[c(head(path,1), tail(path,1)),], pch = 19, col = "black")
}
plot_path(path)

# The map containing the path is presented in Figure 3. It has to be mentioned that the path
# found by the used heuristic is considerable longer than the optimal path found by Concorde
# with a length of 34928, illustrating the power of modern TSP algorithms.

# As an example, we choose New York as the starting city. We transform the data set into
# an ATSP and set the column corresponding to New York to zero before solving it. Thus,
# the distance to return from the last city in the path to New York does not contribute to the
# path length. We use the nearest neighbor heuristic to calculate an initial tour which is then
# improved using 2-Opt moves and cut at New York to create a path.

atsp <- as.ATSP(USCA312)
ny <- which(labels(USCA312) == "New York, NY")
atsp[, ny] <- 0
initial_tour <- solve_TSP(atsp, method="nn")
initial_tour

tour <- solve_TSP(atsp, method ="2-opt", control = list(tour = initial_tour))
tour
 
path <- cut_tour(tour, ny, exclude_cut = FALSE)
head(labels(path))
 
tail(labels(path))
 
plot_path(path)

# Concorde and many advanced TSP solvers can only solve symmetric TSPs. To use these
# solvers, we can formulate the ATSP as a TSP using reformulate_ATSP_as_TSP() which
# introduces a dummy city for each city (see Section 2.2).

tsp <- reformulate_ATSP_as_TSP(atsp)
tsp

# tour <- solve_TSP(tsp, method = "linkern")
tour <- solve_TSP(tsp, method = "concorde")
tour <- as.TOUR(tour[tour <= n_of_cities(atsp)])

# For the following example, we are only interested in paths starting in New York and ending
# in Los Angeles. Therefore, we remove the two cities from the distance matrix, create an
# asymmetric TSP and insert a dummy city called "LA/NY". The distances from this dummy
# city are replaced by the distances from New York and the distances towards are replaced by
# the distances towards Los Angeles.
m <- as.matrix(USCA312)
ny <- which(labels(USCA312) == "New York, NY")
la <- which(labels(USCA312) == "Los Angeles, CA")
atsp <- ATSP(m[-c(ny,la), -c(ny,la)])
atsp <- insert_dummy(atsp, label = "LA/NY")
la_ny <- which(labels(atsp) == "LA/NY")
atsp[la_ny, ] <- c(m[-c(ny,la), ny], 0)
atsp[, la_ny] <- c(m[la, -c(ny,la)], 0)

# We use the nearest insertion heuristic.
tour <- solve_TSP(atsp, method ="nearest_insertion")
tour

path_labels <- c("New York, NY", labels(cut_tour(tour, la_ny)), "Los Angeles, CA")
path_ids <- match(path_labels, labels(USCA312))
head(path_labels)

tail(path_labels)
 
plot_path(path_ids)

###############################################################################

require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)
# Logistic regression
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, type="response") 
glm.probs[1:5]
glm.pred <- ifelse(glm.probs>0.5, "Up","Down")

attach(Smarket)
table(glm.pred,Direction)
mean(glm.pred==Direction)
# Make training and test set
train = Year<2005
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket, family=binomial, subset=train)
glm.probs=predict(glm.fit,newdata=Smarket[!train,], type="response") 
glm.pred=ifelse(glm.probs >0.5, "Up", "Down")
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
#Fit smaller model
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)
glm.probs <- predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred <- ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(76+106)


require(MASS)

## Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 <- subset(Smarket, Year==2005)
lda.pred <- predict(lda.fit,Smarket.2005)
lda.pred[1:5, ]
class(lda.pred)
data.frame(lda.pred)[1:5, ]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## K-Nearest Neighbors
library(class)
?knn
attach(Smarket)
Xlag <- cbind(Lag1, Lag2)
train <- Year<2005
knn.pred <- knn(Xlag[train, ], Xlag[!train, ], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])



###############################################################################
## Boxplot with mean and standard deviation in ggPlot2 (plus Jitter)
## http://www.r-bloggers.com/boxplot-with-mean-and-standard-deviation-in-ggplot2-plus-jitter/

# When you create a boxplot in R, it automatically computes median, first and third quartile ("hinges") and 95% confidence 
# interval of median ("notches").

# But we would like to change the default values of boxplot graphics with the mean, the mean + standard deviation, the mean 
# - S.D., the min and the max values. Here is an example solved using ggplot2 package. Plus here are represented points 
# (the single values) jittered horizontally.

library(ggplot2)

# create fictitious data
a <- runif(10)
b <- runif(12)
c <- runif(7)
d <- runif(15)

# data groups
group <- factor(rep(1:4, c(10, 12, 7, 15)))


# dataframe
mydata <- data.frame(c(a,b,c,d), group)
names(mydata) <- c("value", "group")

# function for computing mean, DS, max and min values
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# ggplot code
p1 <- ggplot(aes(y = value, x = factor(group)), data = mydata)
p1 <- p1 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") + 
      geom_jitter(position=position_jitter(width=.2), size=3) + 
      ggtitle("Boxplot con media, 95%CI, valore min. e max.") + xlab("Gruppi") + ylab("Valori")

p1

## introducingR.pdf

# You can also use the  seq function to create a sequence given the starting and stopping points
# and an increment. For example here are eleven values between 0 and 1 in steps of 0.1:
seq(0, 1, 0.1)
# [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

# Another function that is useful in creating vectors is  rep for repeat or replicate. For
# example  rep(3,4) replicates the number three four times. The first argument can be a vector,
# so  rep(x,3) replicates the entire vector x three times. If both arguments are vectors of
# the same size, then each element of the first vector is replicated the number or times indicated
# by the corresponding element in the second vector. Consider this example:
rep(1:3, 2)
# [1] 1 2 3 1 2 3
rep(1:3, c(2,2,2))
# [1] 1 1 2 2 3 3

# The number of elements of a vector is returned by the function  length . Individual elements are
# addressed using subscripts in square brackets, so  x[1]  is the first element of x,  x[2] is the
# second, and  x[length(x)] is the last.
# The subscript can be a vector itself, so  x[1:3] is a vector consisting of the first three elements
# of x. A negative subscript excludes the corresponding element, so  x[-1] returns a vector with
# all elements of x except the first one.

# The number of rows and columns of a matrix are returned by the functions  nrow and  ncol . To
# transpose a matrix use the function  t . The matrix multiplication operator is  %*% . Matrix
# inversion is done by the function  solve .

# R has very extensive and powerful graphic facilities. In the example below we use  seq to create
# equally spaced points between -3 and 3 in steps of 0.1 (that's 61 points). Then we call the
# function  dnorm to calculate the standard normal density evaluated at those points, we plot it,
# and add a title in a nice shade of blue. Note that we are able to add the title to the current plot
# in a separate call.
z <- seq(-3,3,.1)
d <- dnorm(z)
plot(z, d, type="l")
title("The Standard Normal
      Density", col.main="cornflowerblue")

# The title function expects a character string with the title as the first argument. We also
# specified the optional argument  col.main="cornflowerblue" to set the color of the title.
# There are 657 named colors to choose from, type  colors() to see their names.

pie(rep(1,16),col=rainbow(16))

# Unlike a vector, whose elements must all be of the same type (all numeric, or all character), the
# elements of a list may have different types.

fpe <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat")

fpe <- read.table("noheader.dat", col.names=c("setting", "effort", "change"))
names(fpe) <- c("setting", "effort", "change")

# You can also define fpe as
# your default dataset by "attaching" it to your session:
attach(fpe)
# If you now type the name  effort by itself, R will look for it in the  fpe data frame. If you are
# done with a data frame you can detach it using  detach(fpe) 

boxplot(fpe$setting, col="lavender")
title("Boxplot of Setting", col.main="#3366CC")

plot(fpe$effort, fpe$change, pch=21, bg="gold")
title("Scatterplot of Change by Effort", col.main="#3366CC")

# To identify points in a scatterplot use the  identify function. Try the following (assuming the
# scatterplot is still the active graph):
identify(fpe$effort, fpe$change, row.names(fpe), ps=9)

pairs(fpe)

# 4.1 Fitting a Model
# To fit an ordinary linear model with fertility change as the response and setting and effort as
# predictors, try
lmfit = lm( change ~ setting + effort )
# Note first that lm is a function, and we assign the result to an object that we choose to call lmfit
# (for linear model fit). This stores the results of the fit for later examination.
# The argument to  lm is a model formula, which has the response on the left of the tilde  ~ ~ (read
# "is modeled as") and a Wilkinson-Rogers model specification formula on the right. R uses
# + to combine elementary terms, as in A+B
# : for interactions, as in A:B;
# * for both main effects and interactions, so A*B = A+B+A:B
# A nice feature of R is that it lets you create interactions between categorical variables, between
# categorical and continuous variables, and even between numeric variables (it just creates the
# cross-product).

lmfit
summary(lmfit)
anova(lmfit)
plot(lmfit)

par(mfrow=c(2,2))
plot(lmfit)

fitted(lmfit)
coef(lmfit)
residuals(lmfit)
names(lmfit)

# 4.4 Factors and Covariates
# So far our predictors have been continuous variables or covariates. We can also use categorical
# variables or factors. Let us group family planning effort into three categories:
effortg = cut(effort, breaks = c(-1, 4, 14, 100),
              label=c("weak","moderate","strong"))
# The function  cut creates a factor or categorical variable. The first argument is an input vector,
# the second is a vector of breakpoints, and the third is a vector of category labels. Note that
# there is one more breakpoint than there are categories. All values greater than the i-th
# breakpoint and less than or equal to the (i+1)-st breakpoint go into the i-th category. Any values
# below the first breakpoint or above the last one are coded  NA (a special R code for missing
# values). If the labels are omitted, R generates a suitable default of the form "a thru b".
# Try fitting the analysis of covariance model:
covfit = lm( change ~ setting + effortg )
covfit
anova(covfit)

# If you don't like this choice, R provides a special function to re-order levels, check out  
help(relevel) 

# S codes unordered factors using the Helmert contrasts by default, a choice that is useful in
# designed experiments because it produces orthogonal comparisons, but has baffled many a
# new user. Both R and S-Plus code ordered factors using polynomials. To change to the
# reference cell method for unordered factors use the following call
options(contrasts=c("contr.treatment","contr.poly"))

# 4.5 Regression Splines
# The real power of R begins to shine when you consider some of the other functions you can
# include in a model formula. First, you can include mathematical functions, for example
# log(setting) ) is a perfectly legal term in a model formula. You don't have to create a variable
# representing the log of setting and then use it, R will create it 'on the fly', so you can type
lm( change ~ log(setting) + effort)
# If you wanted to use orthogonal polynomials of degree 3 on setting, you could include a term of
# the form  poly(setting,3)
# You can also get R to calculate a well-conditioned basis for regression splines. First you must
# load the splines library (this step is not needed in S-Plus):
library(splines)
# This makes available the function  bs to generate B-splines. For example the call
setting.bs <- bs(setting, knots = c(66,74,84) + effort)

# The next couple of lines create a model matrix to
# represent the constant, setting and effort, and then calculate the OLS estimate of the
# coefficients as (X'X) -1 X'y:
X <- cbind(1, effort, setting)
solve( t(X) %*% X ) %*% t(X) %*% change

dat <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
summary(m.glm <- glm(num_awards ~ prog + math, family = "poisson", data = dat))

p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)
with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(p, aes(num_awards, fill = prog)) + geom_histogram(binwidth = 0.5, position = "dodge")

library(gbm)
summary(m.gbm <- gbm(num_awards ~ prog + math, distribution = "poisson", data = dat))

## ISLR P191
library(ISLR)
set.seed(1)

# We begin by using the sample() function to split the set of observations
# sample() into two halves, by selecting a random subset of 196 observations out of
# the original 392 observations. 
train <- sample (392,196)

# We then use the subset option in lm() to fit a linear regression using only
# the observations corresponding to the training set.
lm.fit <-lm(mpg???horsepower, data=Auto, subset=train)

# We now use the predict() function to estimate the response for all 392
# observations, and we use the mean() function to calculate the MSE of the
# 196 observations in the validation set. Note that the -train index below
# selects only the observations that are not in the training set.
attach(Auto)
mean((mpg -predict(lm.fit ,Auto))[-train]^2)

# Therefore, the estimated test MSE for the linear regression fit is 26.14. We
# can use the poly() function to estimate the test error for the polynomial
# and cubic regressions.
lm.fit2 <- lm(mpg???poly(horsepower, 2), data=Auto, subset=train)
mean((mpg -predict(lm.fit2,Auto))[-train]^2)
# [1] 19.82
lm.fit3 <- lm(mpg???poly(horsepower ,3), data=Auto, subset=train)
mean((mpg -predict(lm.fit3,Auto))[-train]^2)
# [1] 19.78
# These error rates are 19.82 and 19.78, respectively. If we choose a different
# training set instead, then we will obtain somewhat different errors on the
# validation set.
set.seed(2)
train <- sample (392,196)
lm.fit <- lm(mpg???horsepower ,subset=train)
mean((mpg -predict(lm.fit ,Auto))[-train]^2)
# [1] 23.30
lm.fit2=lm(mpg???poly(horsepower ,2),data=Auto,subset=train)
mean((mpg -predict(lm.fit2,Auto))[-train]^2)
# [1] 18.90
lm.fit3=lm(mpg???poly(horsepower ,3),data=Auto,subset=train)
mean((mpg -predict(lm.fit3,Auto))[-train]^2)
# [1] 19.26
# Using this split of the observations into a training set and a validation
# set, we find that the validation set error rates for the models with linear,
# quadratic, and cubic terms are 23.30, 18.90, and 19.26, respectively.


## Chapter 5 

require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower, data=Auto)

## LOOCV
glm.fit=glm(mpg~horsepower, data=Auto)
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)

##Lets write a simple function to use formula (5.2)
loocv=function(fit){
  h <- lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out
loocv(glm.fit)


cv.error <- rep(0,5)
degree <-1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV

cv.error10 <- rep(0,5)
for(d in degree){
  glm.fit <- glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b", col="red")


## Bootstrap
## Minimum risk investment - Section 5.2

alpha <- function(x,y){
  vx <- var(x)
  vy <- var(y)
  cxy <- cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X, Portfolio$Y)

## What is the standard error of alpha?

alpha.fn <- function(data, index){
  with(data[index,],alpha(X,Y))
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn (Portfolio,sample(1:100, 100, replace=TRUE))

boot.out <- boot(Portfolio, alpha.fn, R=1000)
boot.out
plot(boot.out)


## Chapter 6
## Model Selection

library(ISLR)
summary(Hitters)

# There are some missing values here, so before we proceed we will remove them:
Hitters <- na.omit(Hitters)
with(Hitters, sum(is.na(Salary)))

# Best Subset regression

# We will now use the package `leaps` to evaluate all the best-subset models.
library(leaps)
regfit.full <- regsubsets(Salary~., data=Hitters)
summary(regfit.full)

# It gives by default best-subsets up to size 8; lets increase that to 19, 
# i.e. all the variables

regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary <- summary(regfit.full)
names(reg.summary)
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], pch=20, col="red")

# There is a plot method for the `regsubsets`  object
plot(regfit.full, scale="Cp")
coef(regfit.full, 10)

# Forward Stepwise Selection
# Here we use the `regsubsets` function but specify the `method="forward" option:
regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, 
                         method="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="Cp")

regfit.bwd <- regsubsets(Salary???., data=Hitters, nvmax=19,
                         method="backward")
summary(regfit.bwd)

coef(regfit.full ,7)
coef(regfit.fwd ,7)
coef(regfit.bwd ,7)

# Model Selection Using a Validation Set

# Lets make a training and validation set, so that we can choose a good subset model.
# We will do it using a slightly different approach from what was done in the the book.

dim(Hitters)
set.seed(1)
# Choose 180 numbers of a sequence of 263, without replacement
train <- sample(seq(263), 180, replace=FALSE)
train
regfit.fwd=regsubsets(Salary~., data=Hitters[train, ], nvmax=19, method="forward")

# Now we will make predictions on the observations not used for training. We know 
# there are 19 models, so we set up some vectors to record the errors. We have to 
# do a bit of work here, because there is no predict method for `regsubsets`.
val.errors <- rep(NA, 19)
x.test <- model.matrix(Salary~., data=Hitters[-train, ]) 
# notice the -index! Negative indexing
for(i in 1:19){
# Extract the coefficients of model i
  coefi <- coef(regfit.fwd, id=i)
# ?
  pred <- x.test[, names(coefi)]%*%coefi
# Validation errors
  val.errors[i] <- mean((Hitters$Salary[-train]-pred)^2)
}
plot(sqrt(val.errors), ylab="Root MSE", ylim=c(300, 400), pch=19, type="b")
points(sqrt(regfit.fwd$rss[-1]/180), col="blue", pch=19, type="b")
legend("topright", legend=c("Training", "Validation"), col=c("blue", "black"), pch=19)

# As we expect, the training error goes down monotonically as the model gets bigger, 
# but not so # for the validation error.

# This was a little tedious - not having a predict method for `regsubsets`. 
# So we will write one!
  
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  mat[, names(coefi)]%*%coefi
}

# Model Selection by Cross-Validation

# We will do 10-fold cross-validation. Its really easy!

set.seed(11)
# Make a 10-fold of Hitters
folds <- sample(rep(1:10, length=nrow(Hitters)))
folds
table(folds)
# Number of folds, 10. Number of variables, 19 (nvmax)
cv.errors <- matrix(NA, 10, 19)
cv.errors
# For every fold, test=Hitters[folds==k, ], training=Hitters[folds!=k, ]
for(k in 1:10){
  best.fit <- regsubsets(Salary~., data=Hitters[folds!=k, ], nvmax=19, method="forward")
  for(i in 1:19){
# We need the function predict.regsubsets to be defined    
    pred <- predict(best.fit, Hitters[folds==k, ], id=i)
    cv.errors[k, i] <- mean((Hitters$Salary[folds==k]-pred)^2)
  }
}
pred
cv.errors
# apply(cv.errors, 2, mean) make the mean of colums (2) in cv.errors
rmse.cv <- sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=19, type="b")

# Ridge Regression and the Lasso

# We will use the package `glmnet`, which does not use the model formula 
# language, so we will set up an `x` and `y`.
# The glmnet() function has an alpha argument that determines what type
# of model is fit. If alpha=0 then a ridge regression model is fit, and if alpha=1
# then a lasso model is fit. We first fit a ridge regression model.

library(glmnet)
x <- model.matrix(Salary~.-1, data=Hitters) 
y <- Hitters$Salary

x
y

# First we will fit a ridge-regression model. This is achieved by calling 
# `glmnet` with `alpha=0` (see the helpfile). There is also a `cv.glmnet` 
# function which will do the cross-validation for us. 

fit.ridge <- glmnet(x, y, alpha=0)
plot(fit.ridge,xvar="lambda", label=TRUE)
cv.ridge <- cv.glmnet(x, y, alpha=0)
plot(cv.ridge)

# Now we fit a lasso model; for this we use the default `alpha=1`

fit.lasso <- glmnet(x, y)
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso <- cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)
plot(fit.lasso, xvar="dev", label=TRUE)

# Suppose we want to use our earlier train/validation division to select the `lambda` for the lasso.
# This is easy to do.

lasso.tr <- glmnet(x[train,], y[train])
lasso.tr
pred <- predict(lasso.tr, x[-train,])
dim(pred)
rmse <- sqrt(apply((y[-train]-pred)^2, 2, mean))
plot(log(lasso.tr$lambda), rmse,type="b", xlab="Log(lambda)")
lam.best <- lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr, s=lam.best)
rmse

## 6.7 Lab 3: PCR and PLS Regression
## 6.7.1 Principal Components Regression. Only in book

# Principal components regression (PCR) can be performed using the pcr()
# pcr() function, which is part of the pls library. We now apply PCR to the 
# Hitters data, in order to predict Salary.

library(pls)
set.seed(1)

library(ISLR)
summary(Hitters)

# There are some missing values here, so before we proceed we will remove them:
Hitters <- na.omit(Hitters)

x <- model.matrix(Salary~.-1, data=Hitters) 
y <- Hitters$Salary
train <- sample(seq(263), 180, replace=FALSE)
# train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
# test <- -train
y.test <-y[-train]


set.seed(2)
pcr.fit <- pcr(Salary???., data=Hitters, scale=TRUE,
               validation ="CV")
# The syntax for the pcr() function is similar to that for lm() , with a few
# additional options. Setting scale=TRUE has the effect of standardizing each
# predictor, using (6.6), prior to generating the principal components, so that
# the scale on which each variable is measured will not have an effect. Setting
# validation="CV" causes pcr() to compute the ten-fold cross-validation error
# for each possible value of M, the number of principal components used. The
# resulting fit can be examined using summary() .
summary(pcr.fit)

# The CV score is provided for each possible number of components, ranging
# from M = 0 onwards. (We have printed the CV output only up to M = 4.)
# Note that pcr() reports the root mean squared error; in order to obtain
# the usual MSE, we must square this quantity. For instance, a root mean
# squared error of 352.8 corresponds to an MSE of 352.8 2 = 124,468.
# One can also plot the cross-validation scores using the validationplot()
# Using val.type="MSEP" will cause the cross-validation MSE to be
# plotted.
validationplot(pcr.fit, val.type="MSEP")

# We see that the smallest cross-validation error occurs when M = 16 com-
# ponents are used. This is barely fewer than M = 19, which amounts to
# simply performing least squares, because when all of the components are
# used in PCR no dimension reduction occurs. 

# The summary() function also provides the percentage of variance explained
# in the predictors and in the response using different numbers of components. 
# For example, setting M = 1 only captures 38.31% of all the variance, or 
# information, in the predictors. In contrast, using M = 6 increases the value to 
# 88.63%. If we were to use all M = p = 19 components, this would increase to 100%.

# We now perform PCR on the training data and evaluate its test set performance.
set.seed(1)
pcr.fit <- pcr(Salary???., data=Hitters, subset=train, scale=TRUE,
               validation ="CV")
validationplot(pcr.fit, val.type="MSEP")
# Now we find that the lowest cross-validation error occurs when M = 7
# component are used. We compute the test MSE as follows.
pcr.pred <- predict(pcr.fit, x[-train, ], ncomp=7)
mean((pcr.pred-y.test)^2)

# Finally, we fit PCR on the full data set, using M = 7, the number of
# components identified by cross-validation.
pcr.fit <- pcr(y???x, scale=TRUE, ncomp=7)
summary(pcr.fit)


## 6.7.2 Partial Least Squares. Only in book

# We implement partial least squares (PLS) using the plsr() function, also
# in the pls library. The syntax is just like that of the pcr() function.
set.seed(1)
pls.fit <- plsr(Salary???., data=Hitters, subset=train, scale=TRUE,
                validation ="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")

# The lowest cross-validation error occurs when only M = 2 partial least
# squares directions are used. We now evaluate the corresponding test set
# MSE.
pls.pred <- predict(pls.fit, x[-train ,], ncomp=2)
mean((pls.pred-y.test)^2)
# [1] 101417

# The test MSE is comparable to, but slightly higher than, the test MSE
# obtained using ridge regression, the lasso, and PCR.
# Finally, we perform PLS using the full data set, using M = 2, the number
# of components identified by cross-validation.
pls.fit <- plsr(Salary???., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)

# Notice that the percentage of variance in Salary that the two-component
# PLS fit explains, 46.40%, is almost as much as that explained using the
# 6.8 Exercises 259 final seven-component model PCR fit, 46.69%. 
# This is because PCR only attempts to maximize the amount of variance 
# explained in the predictors, while PLS searches for directions that 
# explain variance in both the predictors and the response.

## Chapter 7 
## Nonlinear Models

# Here we explore the use of nonlinear models using some tools in R

require(ISLR)
attach(Wage)

# Polynomials
# First we will use polynomials, and focus on a single predictor age:
  
fit <- lm(wage~poly(age, 4), data=Wage)
summary(fit)

# The `poly()` function generates a basis of *orthogonal polynomials*.
# Lets make a plot of the fitted function, along with the standard errors of the fit.

agelims  <- range(age)
# To crate a sequence from minimu to maximum
age.grid <- seq(from=agelims[1], to=agelims[2])
preds    <- predict(fit, newdata=list(age=age.grid), se=TRUE)
# Standard Error bandas from 2se. We bind two columns in a matrix
se.bands <- cbind(preds$fit+2*preds$se, preds$fit-2*preds$se)
agelims
age.grid
preds
names(preds)
se.bands
plot(age,wage, col="darkgrey")
lines(age.grid, preds$fit, lwd=2, col="blue")
# matlines plots both columns of se bands. lty=2 broken line
matlines(age.grid, se.bands, col="blue", lty=2)

# There are other more direct ways of doing this in R. For example

fita <- lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
summary(fita)

# Here `I()` is a *wrapper* function; we need it because `age^2` means something 
# to the formula language, while `I(age^2)` is protected.
# The coefficients are different to those we got before! However, the fits are the same:
  
plot(fitted(fit), fitted(fita))

# By using orthogonal polynomials in this simple way, it turns out that we can separately test
# for each coefficient. So if we look at the summary again, we can see that the linear, quadratic
# and cubic terms are significant, but not the quartic.

summary(fit)

# This only works with linear regression, and if there is a single predictor. 
# In general we would use `anova()` as this next example demonstrates.

fita <- lm(wage~education, data=Wage)
fitb <- lm(wage~education+age, data=Wage)
fitc <- lm(wage~education+poly(age, 2), data=Wage)
fitd <- lm(wage~education+poly(age, 3), data=Wage)
anova(fita, fitb, fitc, fitd)

# Polynomial logistic regression

# Now we fit a logistic regression model to a binary response variable, 
# constructed from `wage`. We code the big earners (`>250K`) as 1, else 0.

fit <- glm(I(wage>250)~poly(age, 3), data=Wage, family=binomial)
summary(fit)
preds <- predict(fit, list(age=age.grid), se=T)
se.bands <- preds$fit + cbind(fit=0, lower=-2*preds$se, upper=2*preds$se)
se.bands[1:5, ]

# We have done the computations on the logit scale. To transform we need to apply 
# the inverse logit mapping $$p=\frac{e^\eta}{1+e^\eta}.$$
# (Here we have used the ability of MarkDown to interpret TeX expressions.) 
# We can do this simultaneously for all three columns of `se.bands`:
  
prob.bands <- exp(se.bands)/(1+exp(se.bands))
prob.bands
matplot(age.grid, prob.bands, col="blue", lwd=c(2, 1, 1), lty=c(1, 2, 2), 
        type="l", ylim=c(0, .1))
points(jitter(age), I(wage>250)/10, pch="|", cex=.5)


# Splines
-------
# Splines are more flexible than polynomials, but the idea is rather similar.
# Here we will explore cubic splines.

require(splines)
# A cubic spline with knots at 25, 40 & 60
fit <- lm(wage~bs(age, knots=c(25, 40, 60)), data=Wage)
plot(age, wage, col="darkgrey")
lines(age.grid, predict(fit, list(age=age.grid)), col="darkgreen", lwd=2)
abline(v=c(25, 40, 60), lty=2, col="darkgreen")

# The smoothing splines does not require knot selection, but it does have a smoothing parameter,
# which can conveniently be specified via the effective degrees of freedom or `df`.

fit <- smooth.spline(age, wage, df=16)
lines(fit, col="red", lwd=2)
summary(fit)

# Or we can use LOO cross-validation to select the smoothing parameter for us automatically:
  
fit <- smooth.spline(age, wage, cv=TRUE)
lines(fit, col="purple", lwd=2)
fit
summary(fit)

# Generalized Additive Models
---------------------------
  
# So far we have focused on fitting models with mostly single nonlinear terms.
# The `gam` package makes it easier to work with multiple nonlinear terms. In addition 
# it knows how to plot these functions and their standard errors.

require(gam)
gam1 <- gam(wage~s(age, df=4)+s(year, df=4)+education, data=Wage)
par(mfrow=c(1, 3))
plot(gam1, se=T)
gam2 <- gam(I(wage>250)~s(age, df=4)+s(year, df=4)+education, data=Wage, family=binomial)
plot(gam2)

# Lets see if we need a nonlinear terms for year

gam2a <- gam(I(wage>250)~s(age, df=4)+year+education, data=Wage, family=binomial)
anova(gam2a, gam2, test="Chisq")

# One nice feature of the `gam` package is that it knows how to plot the functions nicely,
# even for models fit by `lm` and `glm`.

par(mfrow=c(1,3))
lm1 <- lm(wage~ns(age, df=4)+ns(year, df=4)+education, data=Wage)
plot.gam(lm1, se=T)

## Chapter 8 
## Tree-based Methods

# Decision Trees
# We will have a look at the `Carseats` data using the `tree` package in R, as in the lab in the book.
# We create a binary response variable `High` (for high sales), and we include it in the same dataframe.

require(ISLR)
require(tree)
attach(Carseats)
hist(Sales)
# Make High a binary response with ifelse
High <- ifelse(Sales<=8, "No", "Yes")
High
Carseats <- data.frame(Carseats, High)

# Now we fit a tree to these data, and summarize and plot it. Notice that we 
# have to _exclude_ `Sales` from the right-hand side of the formula, because 
# the response is derived from it.

# Model al data of Carseats without Sales (-Sales)
tree.carseats <- tree(High~.-Sales, data=Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)

# For a detailed summary of the tree, print it:

tree.carseats

 # Lets create a training and test set (250,150) split of the 400 observations, 
# grow the tree on the training set, and evaluate its performance on the test set.

set.seed(1011)
train <- sample(1:nrow(Carseats),250)
tree.carseats <- tree(High~.-Sales, Carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.pred <- predict(tree.carseats, Carseats[-train, ], type="class")
# Use test data and make a trable prediction/real
with(Carseats[-train,], table(tree.pred, High))
(72+33)/150

# This tree was grown to full depth, and might be too variable. We now use CV to prune it.

# Cross Validation of tree
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats
plot(cv.carseats)
prune.carseats <- prune.misclass(tree.carseats, best=13)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# Now lets evaluate this pruned tree on the test data.

tree.pred <- predict(prune.carseats, Carseats[-train, ], type="class")
with(Carseats[-train,], table(tree.pred, High))
(72+32)/150

# It has done about the same as our original tree. So pruning did not hurt us 
# wrt misclassification errors, and gave us a simpler tree.

# Random Forests and Boosting

# These methods use trees as building blocks to build more complex models. Here we will 
# use the Boston housing data to explore random forests and boosting. These data are in the `MASS` package.
# It gives housing values and other statistics in each of 506 suburbs of Boston based on a 1970 census.

# Random Forests
# Random forests build lots of bushy trees, and then average them to reduce the variance.

require(randomForest)
require(MASS)
set.seed(101)
dim(Boston)
train <- sample(1:nrow(Boston), 300)
?Boston

# Lets fit a random forest and see how well it performs. We will use the response `medv`, 
# the median housing value (in \$1K dollars)

rf.boston <- randomForest(medv~., data=Boston, subset=train)
rf.boston

# The MSR and % variance explained are based on OOB  or _out-of-bag_ estimates, a very 
# clever device in random forests to get honest error estimates. The model reports that 
# `mtry=4`, which is the number of variables randomly chosen at each split. Since $p=13$ here, 
# we could try all 13 possible values of `mtry`. We will do so, record the results, and make a plot.

oob.err <- double(13)
test.err <- double(13)
for(mtry in 1:13){
  fit <- randomForest(medv~., data=Boston, subset=train, mtry=mtry, ntree=400)
  oob.err[mtry] <- fit$mse[400]
  pred <- predict(fit, Boston[-train, ])
  test.err[mtry] <- with(Boston[-train, ], mean((medv-pred)^2))
  cat(mtry, " ")
}
matplot(1:mtry, cbind(test.err, oob.err), pch=19, col=c("red", "blue"), type="b", ylab="Mean Squared Error")
legend("topright", legend=c("OOB", "Test"), pch=19, col=c("red", "blue"))


# Not too difficult! Although the test-error curve drops below the OOB curve, these are 
# estimates based on data, and so have their own standard errors (which are typically quite 
# large). Notice that the points at the end with `mtry=13` correspond to bagging.

# Boosting

# Boosting builds lots of smaller trees. Unlike random forests, each new tree in boosting 
# tries to patch up the deficiencies of the current ensemble.

require(gbm)

plot(boost.boston, i="lstat")
plot(boost.boston, i="rm")

# Lets make a prediction on the test set. With boosting, the number of trees is a tuning 
# parameter, and if we have too many we can overfit. So we should use cross-validation to 
# select the number of trees. We will leave this as an exercise. Instead, we will compute 
# the test error as a function of the number of trees, and make a plot.

n.trees <- seq(from=100, to=10000, by=100)
predmat <- predict(boost.boston, newdata=Boston[-train, ], n.trees=n.trees)
dim(predmat)
berr <- with(Boston[-train, ], apply((predmat-medv)^2, 2, mean))
plot(n.trees, berr, pch=19, ylab="Mean Squared Error", xlab="# Trees", 
     main="Boosting Test Error")
abline(h=min(test.err), col="red")
berr

# Fitting Classification Trees

library(tree)
library(ISLR)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

# Fitting Regression Trees

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Bagging and Random Forests

library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

# Boosting

library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)


## Chapter 9
## SVM

# To demonstrate the SVM, it is easiest to work in low dimensions, so we can see the data.

# Linear SVM classifier
# Lets generate some data in two dimensions, and make them a little separated.

set.seed(10111)
x <- matrix(rnorm(40),20,2)
y <- rep(c(-1,1), c(10,10))
x[y==1, ] <- x[y==1, ]+1
plot(x, col=y+3, pch=19)

# Now we will load the package `e1071` which contains the `svm` function we will use. 
# We then compute the fit. Notice that we have to specify a `cost` parameter, which is a tuning parameter. 

library(e1071)
dat <- data.frame(x, y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
print(svmfit)
plot(svmfit, dat)

# As mentioned in the the chapter, the plot function is somewhat crude, and plots X2 on the horizontal axis 
# (unlike what R would do automatically for a matrix). Lets see how we might make our own plot.

# The first thing we will do is make a grid of values for X1 and X2. We will write a function to do that,
# in case we want to reuse it. It uses the handy function `expand.grid`, and produces the coordinates of 
# `n*n` points on a lattice covering the domain of `x`. Having made the lattice, we make a prediction at 
# each point on the lattice. We then plot the lattice, color-coded according to the classification. Now 
# we can see the decision boundary.

# The support points (points on the margin, or on the wrong side of the margin) are indexed in the 
# `$index` component of the fit.


make.grid <- function(x, n=75){
  grange <- apply(x, 2, range)
  x1 <- seq(from=grange[1,1], to=grange[2,1], length=n)
  x2 <- seq(from=grange[1,2], to=grange[2,2], length=n)
  expand.grid(X1=x1, X2=x2)
}
xgrid <- make.grid(x)
xgrid
ygrid <- predict(svmfit, xgrid)
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=.2)
# Plot the points
points(x, col=y+3, pch=19)
# plot support points
points(x[svmfit$index,], pch=5, cex=2)

# The `svm` function is not too friendly, in that we have to do some work to get back the linear 
# coefficients, as described in the text. Probably the reason is that this only makes sense for linear 
# kernels, and the function is more general. Here we will use a formula to extract the coefficients; 
# for those interested in where this comes from, have a look in chapter 12 of 
# ESL ("Elements of Statistical Learning").

# We extract the linear coefficients, and then using simple algebra, we include the decision boundary and the two margins.

beta <- drop(t(svmfit$coefs)%*%x[svmfit$index, ])
beta0 <- svmfit$rho
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2)
abline(beta0/beta[2], -beta[1]/beta[2])
abline((beta0-1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0+1)/beta[2], -beta[1]/beta[2], lty=2)

# Just like for the other models in this book, the tuning parameter `C` has to be selected.
# Different values will give different solutions. Rerun the code above, but using `C=1`, and see what we mean. 
# One can use cross-validation to do this.

# Nonlinear SVM

# Instead, we will run the SVM on some data where a non-linear boundary is called for. We will use the mixture data from ESL

load(url("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/ESL.mixture.rda"))
names(ESL.mixture)
rm(x, y)
attach(ESL.mixture)

# These data are also two dimensional. Lets plot them and fit a nonlinear SVM, using a radial kernel.

plot(x, col=y+1)
dat <- data.frame(y=factor(y), x)
fit <- svm(factor(y)~., data=dat, scale=FALSE, kernel="radial", cost=5)

# Now we are going to create a grid, as before, and make predictions on the grid.
# These data have the grid points for each variable included on the data frame.

xgrid <- expand.grid(X1=px1, X2=px2)
ygrid <- predict(fit, xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20, cex=.2)
points(x, col=y+1, pch=19)

# We can go further, and have the predict function produce the actual function estimates at each of our grid points. 
# We can include the actual decision boundary on the plot by making use of the contour function. On the dataframe 
# is also `prob`, which is the true probability of class 1 for these data, at the gridpoints. If we plot its 0.5 
# contour, that will give us the _Bayes Decision Boundary_, which is the best one could ever do.

func <- predict(fit, xgrid, decision.values=TRUE)
func <- attributes(func)$decision
func
xgrid <- expand.grid(X1=px1, X2=px2)
ygrid <- predict(fit, xgrid)
plot(xgrid, col=as.numeric(ygrid), pch=20, cex=.2)
points(x, col=y+1, pch=19)

contour(px1, px2, matrix(func,69,99), level=0, add=TRUE)
contour(px1, px2, matrix(prob,69,99), level=.5, add=TRUE, col="blue", lwd=2)

# We see in this case that the radial kernel has done an excellent job.

# Support Vector Classifier

set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))
dat=data.frame(x=x, y=as.factor(y))
library(e1071)
svmfit=svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat)
svmfit$index
set.seed(1)
tune.out=tune(svm,y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)
svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit,dat)

# Support Vector Machine

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col=y)
train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])
summary(svmfit)
svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])
set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newx=dat[-train,]))

# ROC Curves

library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
svmfit.opt=svm(y~., data=dat[train,], kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")
svmfit.flex=svm(y~., data=dat[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted,dat[train,"y"],add=T,col="red")
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")

# SVM with Multiple Classes

set.seed(1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

# Application to Gene Expression Data

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)
table(Khan$ytrain)
table(Khan$ytest)
dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)
table(out$fitted, dat$y)
dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

## Chapter 10
## Unsupervised Learning

# Principal Components
# We will use the `USArrests` data (which is in R)

dimnames(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)


# We see that `Assault` has a much larger variance than the other variables. 
# It would dominate the principal components, so we choose to standardize the variables when we perform PCA

pca.out <- prcomp(USArrests, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)
biplot(pca.out, scale=0, cex=.6)

# K-Means Clustering
# K-means works in any dimension, but is most fun to demonstrate in two, because we can plot pictures.
# Lets make some data with clusters. We do this by shifting the means of the points around.

set.seed(101)
# Two colum matrix or random obs
x <- matrix(rnorm(100*2), 100, 2)
# Generate 4 cluster with sd=4
xmean <- matrix(rnorm(8, sd=4), 4, 2)
# Separate rows from each cluster
which <- sample(1:4, 100, replace=TRUE)
which
x <-x+xmean[which,]
plot(x, col=which, pch=19)

# We know the "true" cluster IDs, but we wont tell that to the `kmeans` algorithm.


km.out <- kmeans(x, 4, nstart=15)
km.out
# There is a 87.6% of variance explained
plot(x, col=km.out$cluster, cex=2, pch=1, lwd=2)
points(x, col=which, pch=19)
points(x, col=c(4,3,2,1)[which], pch=19)


# Hierarchical Clustering

# We will use these same data and use hierarchical clustering

hc.complete <- hclust(dist(x), method="complete")
plot(hc.complete)
hc.single <- hclust(dist(x), method="single")
plot(hc.single)
hc.average <- hclust(dist(x), method="average")
plot(hc.average)


# Lets compare this with the actualy clusters in the data. We will use the function 
# `cutree` to cut the tree at level 4.
# This will produce a vector of numbers from 1 to 4, saying which branch each observation is on. 
# You will sometimes see pretty plots where the leaves of the dendrogram are colored. I searched 
# a bit on the web for how to do this, and its a little too complicated for this demonstration.

# We can use `table` to see how well they match:

hc.cut <- cutree(hc.complete, 4)
table(hc.cut, which)
table(hc.cut, km.out$cluster)

# or we can use our group membership as labels for the leaves of the dendrogram:

plot(hc.complete, labels=which)









###############################################################################
## https://github.com/Chicago/osd-bike-routes/blob/master/examples/Importing%20GeoJSON%20R%20Demo.R

# TITLE: Importing GeoJSON Example in R
# AUTHOR: Tom Schenk Jr., City of Chicago
# CREATED: 2013-01-23
# UPDATED: 2013-01-31
# NOTES:
# LIBRARIES: rgdal, ggplot2

# Set working directory (e.g., "C:\\Users\\username\\downloads" or "~/downloads")
setwd("path\\to\\folder")

# Install and load libraries
## If you need to install the RGDAL and GGPLOT2 libraries, complete this step first, otherwise, skip:
install.packages(c("rgdal", "ggplot2"))

library(rgdal)  # Import data into a Spatial Data Frame in R
library(ggplot2)	# Transform data from Shapefile to Data Frame

# Import data to Spatial Dataframe
# ogrInfo("Bikeroutes.json", layer="OGRGeoJSON")
ogrInfo("./Bikeroutes.geojson", layer="OGRGeoJSON")

# bikeroutes.shapefile <- readOGR(dsn="data\\Bikeroutes.json", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84") # Imports data. Replace PATH\\TO with actual file path (e.g., C:\\Users\\username\\downloads)
bikeroutes.shapefile <- readOGR(dsn="./Bikeroutes.geojson", layer="OGRGeoJSON", p4s="+proj=tmerc +ellps=WGS84") # Imports data. Replace PATH\\TO with actual file path (e.g., C:\\Users\\username\\downloads)

head(bikeroutes.shapefile) # Inspect the data structure.

plot(bikeroutes.shapefile) # Test plot of spatial data frame.

# Fortify data to translate to Data Frame
bikeroutes.df <- fortify(bikeroutes.shapefile) # Caution, this is very memory intensive and may take several hours to complete

head(bikeroutes.df) # Inspect the data structure

ggplot(bikeroutes.df, aes(x=long, y=lat, group=group)) + geom_path() # Test plot of data frame.



# Now we would like to store this time series. This is done easily using

save(sunglasses,file="sunglasses.RData")

# Next time we open R, we just have to use

load("/Users/UQAM/sunglasses.RData")

## ggPlot2: Histogram with jittered stripchart
## http://www.r-bloggers.com/ggplot2-histogram-with-jittered-stripchart/

library(ggplot2)
movies <- movies[1:1000,]

# Here is an example of a Histogram plot, with a stripchart (vertically jittered) along the x side of the plot.

m <- ggplot(movies, aes(x=rating))
m + geom_histogram(binwidth=0.2, colour = "darkgreen", 
                   fill = "white", size=1) + geom_rug(aes(y=-2), position="jitter", sides="b")

# Alternatively, using the geom_rug function:

m <- ggplot(movies, aes(x=rating))
m + geom_histogram(binwidth=0.2, colour = "darkgreen", fill = "white", size=1) + 
  geom_rug(aes(y=-2), position="jitter", sides="b")

# Of course this simplicistic method need to be adjusted in vertical position 
# of the stripchart or rugchart (y=-2, here), 
# and the relative proportion of points jittering. 

#############################################################
## Creepypasta.com stories, Votes vs. Rating
## http://www.everydayanalytics.ca/2014/02/creepypasta-learning-ggplot.html

library(ggplot2)

# Read in the data
data <- read.csv(file = 'creepypasta_ratings.csv', header=T, sep=',')

# Base Package
# Plot
plot(data$Rating, data$Votes, pch=16, cex.main=1, cex.axis=0.8, cex=0.8, col=rgb(0,0,1,0.25), log='y',
     ylab='Votes', xlab='Rating', main='Creepypasta Stories, Votes vs. Ratings')

# Fit second order polynomial
l <- lm(data$Votes ~ data$Rating + I(data$Rating^2))
points(data$Rating, predict(l), type='l')

# equivalent in ggplot
gplot <- ggplot(data, aes(Rating, log(Votes))) +
  geom_point(col=rgb(0,0,1,0.25), pch=16, cex=2) +
  geom_smooth(method="lm", formula=y~poly(x,2)) +
  labs(title="Creepypasta Stories, Votes vs. Ratings") +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        plot.title=element_text(size=16, face="bold"))
gplot

# Data density with hexbin
hexbin <- ggplot(data, aes(Rating, log(Votes))) +
  stat_binhex() + theme_bw() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
hexbin


## Sochi Olympic Medals
## http://www.r-bloggers.com/sochi-olympic-medals/

# Packages to Load

packs <- c("knitr", "ggplot2", "XML", "reshape2", "rCharts")
lapply(packs, require, character.only = T)

# The Script

olympics <- 
  function(theurl = "http://www.sochi2014.com/en/medal-standings", include.zero = FALSE) {
    
    invisible(lapply(c('ggplot2', 'XML', 'reshape2') , require, character.only=TRUE))
    
    ## Grab Data, Clean and Reshape
    raw <- readHTMLTable(theurl, header=FALSE, 
                         colClasses = c(rep("factor", 2), rep("numeric", 4)))
    raw <- as.data.frame(raw)[, -1]
    colnames(raw) <- c("Country", "Bronze", "Silver", "Gold", "Total")
    raw <- raw[order(raw[, "Total"]), ]
    if (!include.zero) {
      raw <- raw[raw[, "Total"] != 0, ]
    }
    raw[, "Country"] <- factor(raw[, "Country"], levels = raw[, "Country"])
    rownames(raw) <- NULL
    mdat <- melt(raw, value.name = "Count", variable.name = "Place", id.var = "Country")
    
    ## Plot the Data
    plot1 <- ggplot(mdat, aes(x = Count, y = Country, colour = Place)) +
      geom_point() +
      facet_grid(.~Place) + theme_bw()+
      scale_colour_manual(values=c("#CC6600", "#999999", "#FFCC33", "#000000")) 
    print(plot1)
    
    return(invisible(raw))
  }

# The Visual Results

x <- olympics()

## Ejemplo de Isidro Hidalgo

rm(list=ls())
require(MASS)

set.seed(1)
grupo1 = data.frame(factor1 = rnorm(100, 2, 1), factor2 = rnorm(100, 5, 1), Color = rep("Azul", 100))
grupo2 = data.frame(factor1 = rnorm(100, 7, 1), factor2 = rnorm(100, 3, 1), Color = rep("Rojo", 100))
todos = rbind(grupo1, grupo2)
str(todos)

lda.fit = lda(Color~ factor1 + factor2, data = todos)
lda.fit
lda.fit$scaling
lda.fit$means
centroide = apply(lda.fit$means, 2, mean)
centroide

m = -lda.fit$scaling[1]/lda.fit$scaling[2]

plot(grupo1[,1:2], col = "lightblue", pch = 3,
     xlim = c(-2, 10), ylim = c(-2, 10),
     xlab = "factor_1", ylab = "factor_2")
points(grupo2[, 1:2], col = "orange", pch = 3)
points(lda.fit$means, col = c("blue", "red"), pch = 19)
lines(todos$factor1, 4.006897 + 2.31147 * (todos$factor1 - 4.569280), col = "green") 


###############################################################################
## BigData Challemge

# con <- url("https://api.dandelion.eu/datagem/milanotoday/data/v1/?$limit=10&$offset=0&$app_id=9adaea76&$app_key=16ed1bb0cae9d4173fc2576e12c458a8", "r")
con <- url("https://api.dandelion.eu/datagem/administrative-regions/data/v1/?$limit=10&$offset=0&$app_id=9adaea76&$app_key=16ed1bb0cae9d4173fc2576e12c458a8", "r")
# con <- url("https://api.dandelion.eu/datagem/administrative-regions/data/v1/?$limit=10&$offset=0&$app_id=a380624b&$app_key=ee6517c008b06e2271c875a9ca4edb8c", "r")
x <- readLines(con)

# url = getURL("https://raw.github.com/tcrug/ranked-choice-vote-data/master/2013-mayor-cvr.csv")
# url = getURL("./2013-mayor-cvr.csv")
# vote = read.csv(text = url)

url <- getURL("https://api.dandelion.eu/datagem/administrative-regions/data/v1/?$limit=10&$offset=0&$app_id=9adaea76&$app_key=16ed1bb0cae9d4173fc2576e12c458a8")
url <- getURL("https://api.dandelion.eu/datagem/administrative-regions/data/v1/?$limit=10&$offset=0&$app_id=a380624b&$app_key=ee6517c008b06e2271c875a9ca4edb8c")

library(XML)

URL = "https://api.dandelion.eu/datagem/administrative-regions/data/v1/?$limit=10&$offset=0&$app_id=9adaea76&$app_key=16ed1bb0cae9d4173fc2576e12c458a8"
content.raw = htmlParse(URL, useInternalNodes = TRUE)

###############################################################################
## Data Science with R
## MapsO.pdf

# 1 Google Maps: Geocoding
# One of the fundamental things about spatial data and mapping is the geographic coordinate
# system used to uniquely identify locations. We use longitude (x axis, abbreviated lon) and
# latitude (y axis, abbreviated lat) for locations on our planet. The longitude is the angle from
# the meridian through Greenwich and the latitude is the angle from the equator. We can use
# ggmap (Kahle and Wickham, 2013) to geocode() street addresses and locations. Here are a few
# examples.
library(ggmap)
geocode("New York")
geocode("Qiushi Road, Shenzhen")
geocode("Canberra")
geocode("Gus' Cafe, Garema Place, Canberra, Australia")
geocode("9 Bunda Street, Canberra")
geocode("11 Bunda Street, Canberra")

# For later use we will save some locations.
(syd <- as.numeric(geocode("Sydney")))
(cbr <- as.numeric(geocode("Canberra")))
sydbb <- c(151.15, -33.88, 151.25, -33.84)

# 2 World Map
# ----world_data
# The data here comes from the maps (Brownrigg, 2013) package. We load the vector data for
# plotting a world map using map data().
ds <- map_data("world")

# ----world_map
p <- ggplot(ds, aes(x=long, y=lat, group=group))
p <- p + geom_polygon()
p

# ----world_data_show
ds <- map_data("world")
class(ds)
str(ds)
head(ds)
names(ds)

# 3 World Map with Coloured Regions
# We can specify a fill for the map, based on the regions.
# ----world_colour
p <- ggplot(ds, aes(x=long, y=lat, group=group, fill=region))
p <- p + geom_polygon()
p <- p + theme(legend.position = "none")
p

# Note there are very many regions, and so the legend would be too large, so we turn that off.
length(unique(ds$region))

# 4 Obtain Map Data
# Visit http://www.gadm.org/country to download administrative vector data for any region of
# the world. The coordinate reference system is latitude/longitude and the WGS84 datum. The
# file formats include shapefiles, ESRI data files, Google Earth, and RData files.
# From the website: A âshapefileâ consist of at least three actual files. This is a commonly
# used format that can be directly used in Arc-anything, DIVA-GIS, and many other programs.
# Unfortunately, many of the non standard latin (roman / english) characters are lost in the
# shapefile. Even if you use the shapefile for mapping, you can use the .csv file that comes with
# the shapefiles, or the attribute data in the geodatabase for the correct attributes (the geodatabase
# is a MS Access database that (on windows) can be accessed via ODBC).
# An âESRI personal geodatabaseâ is a MS Access file that can be opened in ArcGIS (version
# 10). One of its advantages, compared to a shapefile, is that it can store non-latin characters
# (e.g. Cyrillic and Chinese characters). You can also query the (attribute) data in Access or via
# ODBC.
# An âESRI file geodatabaseâ can be opened in ArcGIS (version 10). It can also store non-latin
# characters (e.g. Cyrillic and Chinese characters).
# A âGoogle Earth .kmzâ file can be opened in Google Earth.
# A âRDataâ file can be used in R (with the sp package loaded). See the CRAN spatial task view

ds <- read.csv("ozdata.csv")
dim(ds)
head(ds, 2)
tail(ds, 2)
str(ds)
summary(ds)
names(ds)

# 6 Australian Map with States
# ----aus_map
# The map is drawn with a different fill colour for each state.
p <- ggplot(ds, aes(long, lat, fill=state))
p <- p + geom_polygon()
p <- p + coord_equal()
p <- p + ggtitle("States of Australia")
p
# Note the use coord equal() to ensure a properly proportioned map.
# This example was motivated by the example using qqplot() at 
# http://www.elaliberte.info/software.

7 Map a Subset â Australian Coastline
# ----aus_coast
p <- ggplot(subset(ds, border=="coast"), aes(long, lat, fill=state))
p <- p + geom_path()
p <- p + coord_equal()
p <- p + ggtitle("Coastline of Australia")
p

# 8 Map a Subset â New South Wales
# ----nsw
p <- ggplot(subset(ds, state=="NSW"), aes(long, lat, fill=state))
p <- p + geom_polygon()
p <- p + coord_equal()
p <- p + ggtitle("Map of New South Wales")
p <- p + theme(legend.position="none")
p

# 9 Google Map of Sydney
# Here we download the map data for Sydney from Google using get map() from ggmap (Kahle
# and Wickham, 2013). The data is transformed into a raster object for plotting.
# ----sydney_google_map
map <- get_map(location="Sydney", zoom=14, maptype="roadmap", source="google")
p <- ggmap(map)
p

# The zoom= option is an integer. A value of 0 returns a map of the whole world, centred around
# the location. The maximum value is 21 and returns a map down to the building. Choosing 4 is
# usually good for continents, 14 for a city.
# Notice that the object returned by ggmap() is a ggplot object, and so all the usual ggplot2
# (Wickham and Chang, 2013) functions apply.

class(p)

# 10 Google Map by Geocode of Sydney
# We will generally provide geo-codes to extract maps.
map <- get_map(location=syd, zoom=14, maptype="roadmap", source="google")
p <- ggmap(map)
p
# Notice we have previously saved the location of Sydney into the variable syd and we have used
# that here to extract the same map.
(syd <- as.numeric(geocode("Sydney")))

# 11 Map with Normal Extent of Sydney
map <- get_map(location="Sydney", zoom=14, maptype="roadmap", source="google")
p <- ggmap(map, extent="normal")
p
# The default is extent="panel".

# 12 Map with Device Extent of Sydney
map <- get_map(location="Sydney", zoom=14, maptype="roadmap", source="google")
p <- ggmap(map, extent="device")
p

# 13 Google Terrain Map of Sydney
# Here we see another type of map available from Google.
map <- get_map(location="Sydney", zoom=14, maptype="terrain", source="google")
p <- ggmap(map)
p

# 14 Google Satellite Map of Sydney
# ----sydney_google_map_satellite
map <- get_map(location="Sydney", zoom=14, maptype="satellite", source="google")
p <- ggmap(map)
p

# 15 Google Hybrid Map of Sydney
# ----sydney_google_map_hybrid
map <- get_map(location="Sydney", zoom=14, maptype="hybrid", source="google")
p <- ggmap(map)
p

# 16 OpenStreetMap of Sydney
# ----sydney_osm_map
map <- get_map(location="Sydney", zoom=14, source="osm")
p <- ggmap(map)
p

# 17 Stamen Watercolour Map of Sydney
# Here we downloaded the map data for Sydney from other sources. This watercolour comes from
# Stamen.
map <- get_map(location="Sydney", zoom=14, maptype="watercolor", source="stamen")
p <- ggmap(map)
p

# 18 Stamen Toner Map of Sydney
# ----sydney_toner
map <- get_map(location="Sydney", zoom=14, maptype="toner", source="stamen")
p <- ggmap(map)
p

# 19 Bounding Box to Specify Sydney Map
# Some sources also support bounding boxes rather than a centroid and zoom. Stamen, for exam-
# ple, supports bounding boxes, but Google does not. Here we use a bounding box (sydbb) that
# we defined earlier.
map <- get_map(location=sydbb, maptype="watercolor", source="stamen")
p <- ggmap(map)
p

# 20 Google Map of Canberra
map <- get_map(location="Canberra", zoom=15, maptype="roadmap", source="google")
p <- ggmap(map)
p

# 21 Annotating a Map with Text
# Here we add a blue ANU label (Australian National University).
map <- get_map(location="Canberra", zoom=15, maptype="roadmap", source="google")
dflbl <- data.frame(lon=149.1230, lat=-35.2775, text="ANU")
p <- ggmap(map)
p <- p + geom_text(data=dflbl, aes(x=lon, y=lat, label=text),
                   size=10, colour="blue", fontface="bold")
p

# 22 Annotating a Map with Landmarks
# Here we have geocoded some landmarks and then added them to the map.
landmarks <- c("Gus Cafe, Bunda Street, Canberra", "Canberra Centre, Canberra",
               "Canberra School of Music", "Jolimont Centre",
               "Australian National University")
lbls <- cbind(geocode(landmarks), text=landmarks)
p <- ggmap(map)
p <- p + geom_point(data=lbls, aes(x=lon, y=lat), size=5, colour="orange")
p <- p + geom_point(data=lbls, aes(x=lon, y=lat), size=3, colour="red")
p <- p + geom_text(data=lbls, aes(x=lon, y=lat, label=text),
                   size=3, colour="blue", hjust=0, vjust=0)
p

# 23 Google Map of Australia
map <- get_map(location=as.numeric(geocode("Australia")),
               zoom=4, source="google")
p <- ggmap(map)
p

# 24 OpenStreetMap of Australia
map <- get_map(location="Australia", zoom=4, source="osm")
p <- ggmap(map)
p

# 25 Google Map of Europe
map <- get_map(location="Europe", zoom=4)
p <- ggmap(map)
p

# 26 New York: Google Maps
map <- get_map(location=as.numeric(geocode("New York")),
               zoom=14, source="google")
p <- ggmap(map)
p

# 27 Google Maps: Shenzhen University Town
map <- get_map(location=as.numeric(geocode("Qiushi Road, Shenzhen")),
               zoom=14, source="google")
p <- ggmap(map)
p

# 28 Google Maps: Shenzhen Satellite
map <- get_map(location=as.numeric(geocode("Qiushi Road, Shenzhen")),
               zoom=14, maptype="satellite", source="google")
p <- ggmap(map)
p

# 29 Plot an Address
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
map <- get_map(location=loc, zoom=15, source="google")
p <- ggmap(map)
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=5, colour="orange")
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=3, colour="red")
p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text),
                   size=5, colour="blue", hjust=0.5, vjust=5)
p

addr <- "3 Carlos Munoz Ruiz, Alcobendas, Madrid, Spain"
loc <- as.numeric(geocode(addr))
lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
map <- get_map(location=loc, zoom=15, source="google")
p <- ggmap(map)
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=5, colour="orange")
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=3, colour="red")
p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text),
                   size=5, colour="blue", hjust=0.5, vjust=5)
p
# The location is pinpointed with a red dot overlaying an orange dot. We also add the actual
# address to the plot.

# 30 Plot an Address Using Circle
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
map <- get_map(location=loc, zoom=15, source="google")
p <- ggmap(map)
p <- p + geom_point(data=lbl, aes(x=lon, y=lat), size=5,
                    shape=1, colour="red")
p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text),
                   size=5, colour="blue", hjust=0.5, vjust=5)
p
# Here we have replaced the dot with a circle to pinpoint the location, using shape=1 to choose a
# circle rather than a filled dot.

# 31 Plot an Address on a Satellite Image
addr <- "21 Bambridge Street, Weetangera, ACT, Australia"
loc <- as.numeric(geocode(addr))
lbl <- data.frame(lon=loc[1], lat=loc[2], text=addr)
map <- get_map(location=loc, zoom=15, maptype="hybrid", source="google")
p <- ggmap(map)
p <- p + geom_point(data=lbl, aes(x=lon, y=lat),
                    alpha=I(0.5), size=I(5), colour="red")
p <- p + geom_text(data=lbl, aes(x=lon, y=lat, label=text),
                   size=5, colour="white", hjust=0.5, vjust=5)
p
# The underlying map is now a hybrid satellite and road map, with a transparent red dot to
# pinpoint the location. The transparency is controlled by alpha=I(0.5).

# 32 USA Arrests: Assaults per Murder Data
# This example comes from the help page for map data() from ggplot2 (Wickham and Chang,
# 2013). It shows the number of assaults per murder in each US state, thoug hit is quite easy to
# modify the code to display various statistics from the data.
# First we take a copy of the USArrests dataset and lowercase the variables and the state names
# to make the matching across different datasets uniform.
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))
head(arrests)
# murder assault urbanpop rape region
# Alabama 13.2 236 58 21.2 alabama
# Alaska 10.0 263 48 44.5 alaska
# Arizona 8.1 294 80 31.0 arizona
# Then we merge the statistics with the spatial data, in readiness for mapping.
states <- map_data("state")
head(states)
# long lat group order region subregion
# 1 -87.46 30.39 1 1 alabama <NA>
# 2 -87.48 30.37 1 2 alabama <NA>
# 3 -87.53 30.37 1 3 alabama <NA>
ds <- merge(states, arrests, sort=FALSE, by="region")
head(ds)
# region long lat group order subregion murder assault urbanpop rape
# 1 alabama -87.46 30.39 1 1 <NA> 13.2 236 58 21.2
# 2 alabama -87.48 30.37 1 2 <NA> 13.2 236 58 21.2
# 3 alabama -87.95 30.25 1 13 <NA> 13.2 236 58 21.2
ds <- ds[order(ds$order), ]
head(ds)

# 33 USA Arrests: Assaults per Murder Map
# Once we have the data ready, plotting it simply requires nominating the dataset, and identifying
# the x and y as long and lat respectively. We also need to identify the grouping, which is by state,
# and so the fill is then specified for each state to indicate the statistic of interest.
g <- ggplot(ds, aes(long, lat, group=group, fill=assault/murder))
g <- g + geom_polygon()
print(g)
# We might also be interested in plotting the percentage of urban population in each state. Exercise:
# How can we change the colour of the scale?
g <- ggplot(ds, aes(long, lat, group=group, fill=urbanpop))
g <- g + geom_polygon()
print(g)

# 34 Portland: Open Street Maps
library(OpenStreetMap)
stores <- data.frame(name=c("Commercial","Union","Bedford"),
                     lon=c(-70.25042295455, -70.26050806045, -70.27726650238),
                     lat=c(43.657471302616, 43.65663299041, 43.66091757424))
lat <- c(43.68093, 43.64278)
lon <- c(-70.29548, -70.24097)
portland <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]), zoom=15, 'osm')
plot(portland, raster=TRUE)

# 35 Portland: Annotated Maps
# From http://stackoverflow.com/questions/10686054/outlined-text-with-ggplot2
stores <- data.frame(name=c("Commercial", "Union", "Bedford"),
                     lon=c(-70.25042295455, -70.26050806045, -70.27726650238),
                     lat=c( 43.65747130261, 43.65663299041, 43.66091757424))
portland <- c(-70.2954, 43.64278, -70.2350, 43.68093)
library(ggmap)
map <- get_map(location=portland, source="osm")
g <- ggmap(map)
g <- g + geom_point(data=stores, aes(x=lon, y=lat), size=5)
g <- g + geom_text(data=stores, aes(label=name, x=lon+.001, y=lat), hjust=0)
print(g)

# 36 Portland: Standout Text Annotation
# This map uses outlined text to ensure we can better read text on the map.
g <- ggmap(map)
g <- g + geom_point(data=stores, aes(x=lon, y=lat), size=5)
theta <- seq(pi/16, 2*pi, length.out=32)
xo <- diff(portland[c(1,3)])/250
yo <- diff(portland[c(2,4)])/250
for(i in theta)
  g <- g + geom_text(data=stores, bquote(aes(x=lon + .001 + .(cos(i) * xo),
                                             y=lat + .(sin(i) * yo), label = name)),
                     size=5, colour='black', hjust=0)
g <- g + geom_text(data=stores, aes(x=lon+.001, y=lat, label=name),
                   size=5, colour='white', hjust=0)
print(g)

# Data information
print(object.size(transportation.mi.nov), units="Mb")
print(object.size(transportation.mi.dec), units="Mb")

###############################################################################
## Dealing with Big Data
## BigDataO.pdf

library(knitr)
library(xtable)
library(data.table)
library(plyr)
library(rbenchmark)
library(sqldf)

# 1 Reading from CSV
# Loading data from a CSV file is one of the simplest ways to load data into R. However,
# read.csv() performs a lot of analysis of the data it is reading, to determine the data types.
# We will illustrate this with a relatively small dataset of nearly 70 thousand rows. Even with a
# small dataset loading takes some timeâjust over 1 minute here.
system.time(ds <- read.csv(file="data/weatherAUS.csv"))
## user system elapsed
## 1.193 0.012 1.222

# 2 Reading from CSVâA Little Help
# We can help read.csv() quite a bit, and avoid a lot of extra processing and memory us-
# age by telling read.csv() the data types of the columns of the CSV file. We do this using
# colClasses=. 
# Often though we are not all that sure what the classes should be, and there may be many of
# them, and it would not be reasonable to have to manually specify each one of them. We can
# get a pretty good idea by reading only a part of the CSV file as to the data types of the various
# columns.
# A common approach is to make use of nrows= to read a limited number of rows. From the first
# 5000 rows, for example, we can extract the classes of the columns as automatically determined
# by R and then use that information to read the whole dataset.
# Note firstly the expected significant reduction in time in reading just the first 5000 rows.
system.time(ds <- read.csv(file="data/weatherAUS.csv", nrows=5000))
## user system elapsed
## 0.044 0.000 0.044
classes <- sapply(ds, class)
classes
## Date Location MinTemp MaxTemp Rainfall
## "factor" "factor" "numeric" "numeric" "numeric"
## Evaporation Sunshine WindGustDir WindGustSpeed WindDir9am
## "numeric" "numeric" "factor" "integer" "factor"

# We can check that the data types all look okay. Actually, perhaps we want to ignore the date,
# which is the first column:
classes[1] <- "NULL"
classes
## Date Location MinTemp MaxTemp Rainfall
## "NULL" "factor" "numeric" "numeric" "numeric"
## Evaporation Sunshine WindGustDir WindGustSpeed WindDir9am
## "numeric" "numeric" "factor" "integer" "factor"

# Now to read the full dataset without R having to do so much parsing.
system.time(ds <- read.csv(file="data/weatherAUS.csv", colClasses=classes))
## user system elapsed
## 0.610 0.016 0.627

# For a small dataset, as used here for illustration, the timing differences are in fractions of a
# second. For our 10GB dataset, with 2 million observations and 1000 variables, this approach
# reduces the read time from 30 minutes to 10 minutes.

# 3 Checking Object Sizes
# Explore the options for listing the memory used for storing data.
print(object.size(ds), units="Mb")

# 4 Reading a File in Chunks
# Demonstrate the approach of loading the data in chunks and piecing those chunks together.
ds <- read.csv(...., nrows=2e4)
ds <- rbind(ds, read.csv(...., skip=2e4, nrows=2e4))
ds <- rbind(ds, read.csv(...., skip=4e4, nrows=2e4))
ds <- rbind(ds, read.csv(...., skip=6e4))

# 5 Conversion to a Data Table
# Explore the data.table package.
# For our usual manpulation of data ddply() from plyr (Wickham, 2012) is particularly useful.
# However, as our datasets increase in size, it begins to struggle. The data.table (?) package uses
# similar syntax to manipulate data, but also introduces indexing to handle larger datasets more
# efficiently.

# 6 Reading from CSVâFast Reading
# Using fread() from data.table (?) can even more dramatically reduced load time. For our
# 10GB dataset, with 2 million observations and 1000 variables the load time is reduced to just 2
# minutes.
# Demonstrate the use of fread().

# 7 Interacting with Data Tables
# Whereas we index a data frame by the observation (i) and the columns (j), the basic syntax of
# the data table takes a very different view, and one more familiar to database users. In terms of
# the familiar SQL syntax, we might think of a data table access as specifying the condition to be
# met for the observations we wish to extract (a WHERE clause), the columns we wish to extract
# (a SELECT clause) and how the data is to be aggregated (a GROUP BY clause).
ds[WHERE, SELECT, by=GROUPBY]

# 8 Data Preparation using data.table
# Replicate the data loading template of the Data one pager, using data.table terminology.
# Required packages
library(data.table) # fread()
library(rattle) # normVarNames()
library(randomForest) # na.roughfix()
# Data setup
dspath <- system.file("csv", "weather.csv", package="rattle")
weather <- fread(dspath)
dsname <- "weather"
ds <- get(dsname)
class(ds)
setnames(ds, names(ds), normVarNames(names(ds))) # Optional lower case names.
vars <- names(ds)
target <- "rain_tomorrow"
risk <- "risk_mm"
id <- c("date", "location")
# Variables to ignore
mvc <- ds[,lapply(.SD, function(x) sum(is.na(x)))]
mvn <- names(ds)[which(mvc == nrow(ds))]
ignore <- union(ignore, mvn)
vars <- setdiff(vars, ignore)
# Observations to omit
omit <- NULL
omit <- union(omit, which(is.na(ds[,target])) # Remove all with a missing target.
              # SOME WAY TO GET THIS TO WORK?
              ds[vars] <- na.roughfix(ds[vars]) # Optional impute missing values.
              mo <- attr(na.omit(ds[vars]), "na.action")
              omit <- union(omit, mo)
              if (length(omit)) ds <- ds[-omit,] # Optional remove ommited observations.
              # Finalise TO FIX FROM HERE
              ds[target] <- as.factor(ds[[target]])
              inputs <- setdiff(vars, target)
              nobs <- nrow(ds)
              numerics <- intersect(inputs, vars[which(sapply(ds[vars], is.numeric))])
              categorics <- intersect(inputs, vars[which(sapply(ds[vars], is.factor))])


## Parallel Execution
## Paralel.pdf

library(parallel)
library(rpart)

# 1 Weather Data
# We will model the weatherAUS dataset. We choose this dataset since it is reasonably large,
# and takes quite a few seconds to build a decision tree.
# We have a CSV version of the dataset available in the local data folder.
dir(path="data", pattern="*.csv")
# [1] "heart.csv" "stroke.csv" "weatherAUS.csv"
# The data is directly read into a data frame.
ds <- read.csv(file="data/weatherAUS.csv")
# As always, we first check the contents of the dataset to ensure everything looks okay:
dim(ds)
# [1] 66672 24
head(ds)
tail(ds)
str(ds)
summary(ds)

# 2 Prepare for Modelling
# Following the template presented in the Models module, we continue with setting up some fo the
# modelling parameters.
target <- "RainTomorrow"
risk <- "RISK_MM"
dsname <- "weather"
ds[target] <- as.factor(ds[[target]])
summary(ds[target])

vars <- colnames(ds)
ignore <- vars[c(1, 2, if (exists("risk")) which(risk==vars))]
vars <- setdiff(vars, ignore)
(inputs <- setdiff(vars, target))
nobs <- nrow(ds)
dim(ds[vars])
# [1] 66672 21
(form <- formula(paste(target, "~ .")))
# RainTomorrow ~ .
(seed <- sample(1:1000000, 1))
# [1] 954262
set.seed(seed)
length(train <- sample(nobs, 0.7*nobs))
# [1] 46670
length(test <- setdiff(seq_len(nobs), train))
# [1] 20002

# 3 Build a Model
set.seed(42)
system.time(model <- wsrpart(form, ds[train, vars], ntrees=1))
# user system elapsed
# 0.268 0.004 2.851
model[[1]]$model
# n=45860 (810 observations deleted due to missingness)
#
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
model[[1]]$vars
# [1] "Rainfall" "Humidity3pm" "Cloud3pm" "Evaporation" "MaxTemp"
# [6] "Pressure3pm" "Cloud9am" "Temp3pm" "Pressure9am" "RainToday"
# [11] "MinTemp"
model[[1]]$accuracy

# 4 Build a Second Model
# We can call it again to obtain another model:
set.seed(84)
system.time(model <- wsrpart(form, ds[train, vars], ntrees=1))
# user system elapsed
# 2.732 0.056 2.676
model[[1]]$model
# n=45761 (909 observations deleted due to missingness)
#
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
model[[1]]$vars
# [1] "Humidity9am" "Temp3pm" "Sunshine" "Pressure9am"
# [5] "Humidity3pm" "RainToday" "Cloud9am" "WindGustSpeed"
# [9] "Pressure3pm" "WindGustDir" "Cloud3pm"
model[[1]]$oob.error
# [1] 0.1767

# 5 Build Models in Parallel
# The parallel (?) package provides functions to distribute the computation across multiple cores
# and servers.
# We first determine the number of cores available on the computer we are processing our data on:
cores <- detectCores()
cores
## [1] 4
# We can then start a parallel run of building models using mcparallel(). This command forks
# the current process to build the tree (and hence will not work on MS/Windows). Here we build
# one tree for each core.
jobs <- lapply(1:cores,
               function(x) mcparallel(wsrpart(form, ds[train,vars], ntrees=1),
                                      name=sprintf("dt%02d", x)))

# 6 Collect Results
# We now wait for the jobs to finish:
system.time(model <- mccollect(jobs, wait=TRUE))
# user system elapsed
# 3.404 0.232 6.218
# The decision trees will then be available in the resulting list:
length(model)
# [1] 4
model[[1]][[1]]$model
# n=45832 (838 observations deleted due to missingness)
#
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
#
# 1) root 45832 10610 No (0.7685 0.2315)
# 2) Humidity3pm< 69.5 36727 5143 No (0.8600 0.1400) *
# 3) Humidity3pm>=69.5 9105 3636 Yes (0.3993 0.6007)
# 6) Humidity3pm< 82.5 5482 2496 No (0.5447 0.4553)
# 12) Rainfall< 2.3 3620 1353 No (0.6262 0.3738) *
model[[2]][[1]]$model
# n=45843 (827 observations deleted due to missingness)
#
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
#
# 1) root 45843 10530 No (0.7702 0.2298)
# 2) Humidity3pm< 70.5 37372 5297 No (0.8583 0.1417) *
# 3) Humidity3pm>=70.5 8471 3235 Yes (0.3819 0.6181)
# 6) Humidity3pm< 82.5 4976 2391 No (0.5195 0.4805)
# 12) Rainfall< 1.7 3068 1194 No (0.6108 0.3892) *


## Exploring Data with GGPlot2
## GGplot2O.pdf

library(ggplot2) # Grammar of graphics.
library(scales) # Include commas in numbers.
library(rattle) # Weather dataset.
library(randomForest) # Use na.roughfix() to deal with missing data.
library(gridExtra) # Layout multiple plots.
library(wq) # Regular grid layout.
library(xkcd) # Some xkcd fun.
library(extrafont) # Fonts for xkcd.
library(GGally) # Parallel coordinates.

# 1 Preparing the Dataset: Reviewing the Data
# We use the relatively large weatherAUS dataset from rattle (Williams, 2014) to illustrate the
# capabilities of ggplot2.
library(rattle)
dsname <- "weatherAUS"
ds <- get(dsname)
# The dataset is summarised below.
dim(ds)
names(ds)
head(ds)
tail(ds)
str(ds)
summary(ds)

# 2 Preparing the Dataset: Collecting Information
names(ds) <- normVarNames(names(ds)) # Optional lower case variable names.
vars <- names(ds)
target <- "rain_tomorrow"
id <- c("date", "location")
ignore <- id
inputs <- setdiff(vars, target)
numi <- which(sapply(ds[vars], is.numeric))
numi
# min_temp max_temp rainfall evaporation
# 3 4 5 6
# sunshine wind_gust_speed wind_speed_9am wind_speed_3pm
# 7 9 12 13
numerics <- names(numi)
numerics
# [1] "min_temp" "max_temp" "rainfall"
# [4] "evaporation" "sunshine" "wind_gust_speed"
# [7] "wind_speed_9am" "wind_speed_3pm" "humidity_9am"
# [10] "humidity_3pm" "pressure_9am" "pressure_3pm"
cati <- which(sapply(ds[vars], is.factor))
cati
# location wind_gust_dir wind_dir_9am wind_dir_3pm rain_today
# 2 8 10 11 22
# rain_tomorrow
# 24
categorics <- names(cati)
categorics
# [1] "location" "wind_gust_dir" "wind_dir_9am" "wind_dir_3pm"
# [5] "rain_today" "rain_tomorrow"

# We perform missing value imputation simply to avoid warnings from ggplot2, ignoring whether
# this is appropriate to do so from a data integrity point of view.
library(randomForest)
sum(is.na(ds))
# [1] 172050
ds[setdiff(vars, ignore)] <- na.roughfix(ds[setdiff(vars, ignore)])
sum(is.na(ds))
# [1] 0

# 3 Histogram: Displaying Frequencies
p <- ggplot(data=ds, aes(x=wind_dir_3pm))
p <- p + geom_bar()
p

# 4 Histogram: Narrow Bars
p <- ggplot(ds, aes(wind_dir_3pm))
p <- p + geom_bar(width=0.5)
p

# 5 Histogram: Full Width Bars
p <- ggplot(ds, aes(wind_dir_3pm))
p <- p + geom_bar(width=1)
p
# Going the other direction, the bars can be made to touch by specifying a full width with
# width=1.

# 6 Histogram: Full Width Bars with Borders
p <- ggplot(ds, aes(wind_dir_3pm))
p <- p + geom_bar(width=1, colour="blue", fill="grey")
p

# 7 Histogram: Coloured Histogram Without a Legend
p <- ggplot(ds, aes(wind_dir_3pm, fill=wind_dir_3pm))
p <- p + geom_bar()
p <- p + theme(legend.position="none")
p
# Now we really add a flamboyant streak to our plot by adding quite a spread of colour. To do so
# we simply specify a fill= aesthetic to be controlled by the values of the variable wind dir 3pm
# which of course is the variable being plotted on the x-axis. A good set of colours is chosen by
# default.
# We add a theme() to remove the legend that would be displayed by default, by indicating that
# the legend.position= is none.

# 8 Histogram: Comma Formatted Labels
p <- ggplot(ds, aes(wind_dir_3pm, fill=wind_dir_3pm))
p <- p + geom_bar()
p <- p + scale_y_continuous(labels=comma)
p <- p + theme(legend.position="none")
p

# 9 Histogram: Too Many Bars
p <- ggplot(data=ds, aes(x=location, y=temp_3pm, fill=location))
p <- p + stat_summary(fun.y="mean", geom="bar")
p <- p + theme(legend.position="none")
p

# 10 Histogram: Rotated Labels
p <- ggplot(ds, aes(location, temp_3pm, fill=location))
p <- p + stat_summary(fun.y="mean", geom="bar")
p <- p + theme(legend.position="none",
               axis.text.x=element_text(angle=90))
p
# The obvious solution is to rotate the labels. We achieve this through modifying the theme(),
# setting the axis.text= to be rotated 90 degrees.

# 11 Histogram: Horizontal Histogram
p <- ggplot(ds, aes(location, temp_3pm, fill=location))
p <- p + stat_summary(fun.y="mean", geom="bar")
p <- p + theme(legend.position="none")
p <- p + coord_flip()
p
# Alternatively perhaps it would be better to flip the coordinates and produce a horizontal 
# histogram:

# 12 Histogram: Reorder the Levels
dslr <- within(ds, location <- factor(location, levels=rev(levels(location))))
p <- ggplot(dslr, aes(location, temp_3pm, fill=location))
p <- p + stat_summary(fun.y="mean", geom="bar")
p <- p + theme(legend.position="none")
p <- p + coord_flip()
p
# We also want to have the labels in alphabetic order which makes the plot more accessible. This
# requires we reverse the order of the levels in the original dataset. We do this and save the result
# into another dataset so as to revert to the original dataset when appropriate below.

# 13 Histogram: Plot the Mean with CI
p <- ggplot(dslr, aes(location, temp_3pm, fill=location))
p <- p + stat_summary(fun.y="mean", geom="bar")
p <- p + stat_summary(fun.data="mean_cl_normal",
                      geom="errorbar",
                      conf.int=0.95,
                      width=0.35)
p <- p + coord_flip()
p <- p + theme(legend.position="none")
p
# Here we add a confidence interval around the mean.

# 14 Histogram: Text Annotations
p <- ggplot(dslr, aes(location, fill=location))
p <- p + geom_bar(width=1, colour="white")
p <- p + theme(legend.position="none")
p <- p + coord_flip()
p <- p + geom_text(stat="bin",
                   color="white",
                   hjust=1.0,
                   size=3,
                   aes(y=..count.., label=..count..))
p
# It would be informative to also show the actual numeric values on the plot. This plot shows the
# counts.

# 15 Histogram: Text Annotations with Commas
p <- ggplot(dslr, aes(location, fill=location))
p <- p + geom_bar(width=1, colour="white")
p <- p + theme(legend.position="none")
p <- p + coord_flip()
p <- p + geom_text(stat="bin", color="white", hjust=1.0, size=3,
                   aes(y=..count.., label=scales::comma(..count..)))
p


# 16 Histogram: Multiple Text Annotations
p <- ggplot(dslr, aes(location, fill=location))
p <- p + geom_bar(width=1, colour="white")
p <- p + theme(legend.position="none")
p <- p + coord_flip()
p <- p + geom_text(stat="bin", color="white", hjust=1.0, size=3,
                   aes(y=..count.., label=scales::comma(..count..)))
p <- p + geom_text(stat="identity", colour="white", hjust=-2.5, size=3,
                   aes(y=0, label=location))
p <- p + theme(axis.text.y=element_blank())
p
# We can add location as a label in the bar rather than on the axis.

# 19 Distributions: Transparent Categoric Density Plot
cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")
p <- ggplot(subset(ds, location %in% cities),
            aes(temp_3pm, colour = location, fill = location))
p <- p + geom_density(alpha = 0.55)
p

# 21 Parallel Coordinates Plot: Labels Aligned
library(GGally)
p <- ggparcoord(subset(ds, location %in% cities & rainfall>75), columns=numi)
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
# We notice the labels are by default aligned by their centres. Rotating 45 â¦ causes the labels to
# sit over the plot region. We can ask the labels to be aligned at the top edge instead, using
# hjust=1.

# 22 Parallel Coordinates Plot: Colour by Location
p <- ggparcoord(subset(ds, location %in% cities & rainfall>75),
                columns=numi, group="location")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

# 26 Scatter Plot: Comparing Individual Changes Over Time
load(file.path("data", "tolerance.RData"))
dim(tolerance)
# [1] 80 6
names(tolerance)
# [1] "id" "age" "tolerance" "male" "exposure" "time"
length(unique(tolerance$id))
# [1] 16
# Here we use facet to separately plot each entity by their id. The code for the plot comes from
# http://heuristically.wordpress.com/2012/03/14/plotting-individual-growth-charts/.
p <- ggplot(tolerance, aes(age, tolerance))
p <- p + geom_point()
p <- p + geom_smooth(method=lm)
p <- p + facet_wrap(~id)
p

# 27 Scatter Plot: Using ggpairs()
wds <- na.omit(weather[c(3,4,6,7,10,19,22,24)])
ggpairs(wds, params = c(shape = I("."), outlier.shape = I(".")))
# This scatter plot uses ggpairs() from GGally (Schloerke et al., 2014) to plot the diamond

# 28 Using grid.arrange(): Multiple Plots Code
# Here we illustrate the ability to layout multiple plots in a regular grid using grid.arrange()
# from gridExtra (Auguie, 2012). We illustrate this with the weatherAUS dataset from rattle. We
# generate a number of informative plots, using the plyr (Wickham, 2012a) package to aggregate
# the cumulative rainfall. The scales (Wickham, 2012b) package is used to provide labels with
# commas.
library(rattle)
library(plyr)
library(ggplot2)
library(scales)
cities <- c("Adelaide", "Canberra", "Darwin", "Hobart")
dss <- subset(ds, location %in% cities & date >= "2009-01-01")
dss <- ddply(dss, .(location), transform, cumRainfall=cumsum(rainfall))
p <- ggplot(dss, aes(x=date, y=cumRainfall, colour=location))
p1 <- p + geom_line()
p1 <- p1 + ylab("millimetres")
p1 <- p1 + scale_y_continuous(labels=comma)
p1 <- p1 + ggtitle("Cumulative Rainfall")
p2 <- ggplot(dss, aes(x=date, y=max_temp, colour=location))
p2 <- p2 + geom_point(alpha=.1)
p2 <- p2 + geom_smooth(method="loess", alpha=.2, size=1)
p2 <- p2 + ggtitle("Fitted Max Temperature Curves")
p3 <- ggplot(dss, aes(x=pressure_3pm, colour=location))
p3 <- p3 + geom_density()
p3 <- p3 + ggtitle("Air Pressure at 3pm")
p4 <- ggplot(dss, aes(x=sunshine, fill=location))
p4 <- p4 + facet_grid(location ~ .)
p4 <- p4 + geom_histogram(colour="black", binwidth=1)
p4 <- p4 + ggtitle("Hours of Sunshine")
p4 <- p4 + theme(legend.position="none")

# 29 Using grid.arrange(): Multiple Plots
library(gridExtra)
grid.arrange(p1, p2, p3, p4)
# The actual plots are arranged by grid.arrange().
# A collection of plots such as this can be quite informative and effective in displaying the informa-
# tion efficiently. We can see that Darwin is quite a stick out. It is located in the tropics, whereas
# the remaining cities are in the southern regions of Australia.

# 30 Using grid.arrange(): Arranging Plots
# We are able to arrange the plots in quite a flexible manner.
grid.arrange(p4, arrangeGrob(p1, p2, p3, ncol=1), ncol=2, widths=c(1,1.2))

# 31 Using grid.arrange(): Sharing a Legend
plegend<-function(p)
{
  tmp <- ggplot_gtable(ggplot_build(p))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- plegend(p1)
grid.arrange(p1 + theme(legend.position="none"),
             p2 + theme(legend.position="none"),
             p3 + theme(legend.position="none"),
             legend)

# 32 Using grid.arrange(): 2D Histogram Code
# The following code is based on an example from Michael Kuhn. In this code block we generate
# the ggplot2 objects that we will then arrange and print on the next page.
# The data we use comes from rattle. From the full weatherAUS dataset we select a subset()
# covering just three cities. A basic scatter plot is built displaying the minimum and maximum
# daily temperatures. Two density plots are then generated, one for each of the variables in the
# scatter plot. The fourth object is the legend for the scatter plot.
library(rattle)
library(ggplot2)
dss <- subset(ds, location %in% c("Canberra", "Adelaide", "Darwin"))
dss$location <- ordered(dss$location)
p <- ggplot(dss, aes(min_temp, max_temp, colour=location))
p <- p + geom_point()
p5 <- p + theme(legend.position="none")
p6 <- ggplot(dss, aes(x=min_temp, group=location, colour=location))
p6 <- p6 + stat_density(fill=NA, position="dodge")
p6 <- p6 + theme(legend.position="none",
                 axis.title.x=element_blank(),
                 axis.text.x=element_blank())
p7 <- ggplot(dss, aes(x=max_temp, group=location, colour=location))
p7 <- p7 + stat_density(fill=NA, position="dodge")
p7 <- p7 + coord_flip()
p7 <- p7 + theme(legend.position="none",
                 axis.title.y=element_blank(),
                 axis.text.y=element_blank())
legend <- plegend(p)


# 33 Using grid.arrange(): 2D Histogram Plot
# Having generated a number of graphical objects, we arrange them using grid.arrange() from
# gridExtra.
library(gridExtra)
grid.arrange(arrangeGrob(p6, legend, p5, p7,
                         widths= unit.c(unit(0.75, "npc"), unit(0.25, "npc")),
                         heights=unit.c(unit(0.25, "npc"), unit(0.75, "npc")),
                         nrow=2))

# 34 Using layOut(): Multiple Plots
# Here we illustrate the ability to layout multiple plots in a regular grid using layOut() from wq
# (Jassby and Cloern, 2012).
library(wq)
layOut(list(p1, 1, 1), list(p2, 1, 2), list(p3, 2, 1), list(p4, 2, 2))

# 35 Using layOut(): Arranging Plots
# Here we illustrate the ability to layout multiple plots in a grid of differing sizes using layOut()
# from wq.
library(wq)
layOut(list(p4, 1:3, 1), list(p1, 1, 2), list(p2, 2, 2), list(p3, 3, 2))

# 36 Visually Weighted Regression
# From Nicrebread www.nicebread.de (and posted on Bloggers on R) by Felix Schoenbrodt 30
# August 2012 addressing Solomon Hsiangâs proposal of an appealing method for visually displaying
# the uncertainty in regressions and using shading in response to Gelmanâs note that traditional
# statistical summaries such as 95% intervals give too much weight to the edges.

f1 <- read.csv("F1 2010 - JB McLaren Telemetry, Australia.csv", sep=";", dec=",")
head(f1)

# 38 F1: Simple Map
# We can draw the particular F1 circuit using the longitude and latitude as the x and y coordinates,
# and using the sign of the latitudinal g-force on the driver. We believe that a positive value of
# gLat indicates force to the left and a negative value indicates a force to the right.
p <- ggplot(f1, aes(NGPSLongitude, NGPSLatitude))
p <- p + geom_point(aes(col=sign(gLat), size=abs(gLat)))
p

# We should be able to see from the plot the forces on the driver on the left and right hand corners,
# and see how tight the corner is based on the size of the dots.

# 39 F1: Driver Behaviour
# We can explore the driverâs behaviour in using low gear and throttle. The distance around
# the track is plotted on the x-axis and the lap number on y axis. The node size is inversely
# proportional to gear number (low gear, large point size) and the colour is the relative amount of
# throttle pedal depression.
library(scales)
p <- ggplot(f1, aes(sLap, Lap))
p <- p + geom_point(aes(col=rThrottlePedal, size=-NGear))
p <- p + scale_colour_gradient(low="blue", high="red")
p <- p + scale_x_continuous(labels=comma)
p

# 40 F1: Gear Usage Around the Track
p <- ggplot(f1, aes(sLap, NGear))
p <- p + geom_line()
p

# 41 F1: Trace a Single Lap
# We can trace a single lap to display the speed (y-axis) coloured by gear as the vehicle travels
# around the circuit:
ggplot(subset(f1, Lap==2), aes(sLap, vCar)) +
geom_line(aes(colour=NGear))

# 42 F1: Box Plot of Speed by Gear
# Statistical graphics provide important insights. The box plot here makes sense, in that higher
# gears correspond to higher speeds.
ggplot(f1, aes(factor(NGear), vCar)) + geom_boxplot()

# 43 F1: Footwork
# How busy are the feet? We can summarise the brake (red) and throttle (green) depression based
# on gear.
ggplot(f1, aes(factor(NGear))) +
  geom_jitter(aes(y=rThrottlePedal), colour='darkgreen') +
  geom_jitter(aes(y=pBrakeF), colour='darkred')

# 44 F1: Forces on the Driver
ggplot(f1, aes(factor(NGear), gLong)) +
  geom_jitter(aes(col=pBrakeF)) +
  scale_colour_gradient(low='red', high='green')

# 45 F1: More Forces
ggplot(f1, aes(factor(NGear), gLong)) +
  geom_jitter(aes(col=rThrottlePedal)) +
  scale_colour_gradient(low='red', high="green")

# 46 F1: Box Plot of Forces
# We can use a box plot to investigate the longitudinal g-forceâs relationship with acceleration or
# braking by gear. Note that a random jitter is used to scatter points around their actual integer
# values.
ggplot(f1, aes(factor(NGear), gLong)) +
  geom_boxplot() +
  geom_jitter(size=1)

# 47 F1: RPM and Speed in Relation to Gear
ggplot(f1, aes(nEngine, vCar)) +
  geom_point(aes(col=factor(NGear)))

## Scatterplot with marginal histograms in ggplot2
## https://stackoverflow.com/questions/8545035/scatterplot-with-marginal-histograms-in-ggplot2

require(ggplot2)
x  <- rnorm(300)
y  <- rt(300, df=2)
xy <- data.frame(x, y)

library(gridExtra)
hist_top <- ggplot()+geom_histogram(aes(rnorm(100)))
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
  opts(axis.ticks=theme_blank(), 
       panel.background=theme_blank(), 
       axis.text.x=theme_blank(), axis.text.y=theme_blank(),           
       axis.title.x=theme_blank(), axis.title.y=theme_blank())

scatter <- ggplot()+geom_point(aes(rnorm(100), rnorm(100)))
hist_right <- ggplot()+geom_histogram(aes(rnorm(100)))+coord_flip()

grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

# Another scatterplot

scatter <- qplot(x, y, data=xy)  + 
  scale_x_continuous(limits=c(min(x), max(x))) + 
  scale_y_continuous(limits=c(min(y), max(y))) + 
  geom_rug(col=rgb(.5, 0, 0, alpha=.2))
scatter

scatter <- qplot(x, y, data=xy)  + 
  scale_y_continuous(limits=c(min(y), max(y))) + 
  geom_rug(col=rgb(.5, 0, 0, alpha=.2)) +
  theme(legend.position = "none",          
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        plot.margin = unit(c(3, -5.5, 4, 3), "mm")) +
  scale_x_continuous(breaks = 0:6,
                     limits = c(0,6),
                     expand = c(.05, .05))
scatter

x  <- rnorm(300)
y  <- rt(300,df=10)
xy <- data.frame(x,y)

require(ggplot2); require(grid)
# make the basic plot object
ggplot(xy, aes(x, y)) +        
  # set the locations of the x-axis labels as Tukey's five numbers   
  scale_x_continuous(limit=c(min(x), max(x)), 
                     breaks=round(fivenum(x),1)) +     
  # ditto for y-axis labels 
  scale_y_continuous(limit=c(min(y), max(y)),
                     breaks=round(fivenum(y),1)) +     
  # specify points
  geom_point() +
  # specify that we want the rug plot
  geom_rug(size=0.1) +   
  # improve the data/ink ratio
  theme_set(theme_minimal(base_size = 18))

## ggplot2: Cheatsheet for Visualizing Distributions
## www.r-bloggers.com/ggplot2-cheatsheet-for-visualizing-distributions/

# In the third and last of the ggplot series, this post will go over interesting ways to visualize the distribution 
# of your data. I will make up some data, and make sure to set the seed.

library(ggplot2)
library(gridExtra)
set.seed(10005)

xvar <- c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
yvar <- c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
zvar <- as.factor(c(rep(1, 1500), rep(2, 1500)))
xy <- data.frame(xvar, yvar, zvar)

# Histograms

#counts on y-axis
g1 <- ggplot(xy, aes(xvar)) + geom_histogram()                                      #horribly ugly default
g2 <- ggplot(xy, aes(xvar)) + geom_histogram(binwidth=1)                            #change binwidth
g3 <- ggplot(xy, aes(xvar)) + geom_histogram(fill=NA, color="black") + theme_bw()   #nicer looking

#density on y-axis
g4 <- ggplot(xy, aes(x=xvar)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()

grid.arrange(g1, g2, g3, g4, nrow=1)

## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust
## this. stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to
## adjust this. stat_bin: binwidth defaulted to range/30. Use 'binwidth = x'
## to adjust this.

# Notice the warnings about the default binwidth that always is reported unless you specify it yourself. 
# I will remove the warnings from all plots that follow to conserve space.

# Density plots
# We can do basic density plots as well. Note that the default for the smoothing kernel is gaussian, 
# and you can change it to a number of different options, including kernel="epanechnikov" and kernel="rectangular" 
# or whatever you want. 

# basic density
p1 <- ggplot(xy, aes(xvar)) + geom_density()

# histogram with density line overlaid
p2 <- ggplot(xy, aes(x=xvar)) + 
      geom_histogram(aes(y = ..density..), color="black", fill=NA) +
      geom_density(color="blue")

# split and color by third variable, alpha fades the color a bit
p3 <- ggplot(xy, aes(xvar, fill = zvar)) + geom_density(alpha = 0.2)

grid.arrange(p1, p2, p3, nrow=1)

# Boxplots and more
# We can also look at other ways to visualize our distributions. Boxplots are probably the most useful in order to 
# describe the statistics of a distribution, but sometimes other visualizations are nice. I show a jitter plot and 
# a volcano plot. More on boxplots here. Note that I removed the legend from each one because it is redundant.

# boxplot
b1 <- ggplot(xy, aes(zvar, xvar)) + 
      geom_boxplot(aes(fill = zvar)) +
      theme(legend.position = "none")

# jitter plot
b2 <- ggplot(xy, aes(zvar, xvar)) + 
      geom_jitter(alpha=I(1/4), aes(color=zvar)) +
      theme(legend.position = "none")

# volcano plot
b3 <- ggplot(xy, aes(x = xvar)) +
      stat_density(aes(ymax = ..density..,  ymin = -..density..,
                   fill = zvar, color = zvar),
                   geom = "ribbon", position = "identity") +
      facet_grid(. ~ zvar) +
      coord_flip() +
      theme(legend.position = "none")

grid.arrange(b1, b2, b3, nrow=1)

## ggplot2: Cheatsheet for Visualizing Distributions

# In the third and last of the ggplot series, this post will go over interesting ways to visualize the 
# distribution of your data. I will make up some data, and make sure to set the seed.

library(ggplot2)
library(gridExtra)
set.seed(10005)

xvar <- c(rnorm(1500, mean = -1), rnorm(1500, mean = 1.5))
yvar <- c(rnorm(1500, mean = 1), rnorm(1500, mean = 1.5))
zvar <- as.factor(c(rep(1, 1500), rep(2, 1500)))
xy <- data.frame(xvar, yvar, zvar)

# >> Histograms
# I've already done a post on histograms using base R, so I won't spend too much time on them. Here are the basics of doing them in ggplot. More on all options for histograms here. The R cookbook has a nice page about it too: http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/ Also, I found this really great aggregation of all of the possible geom layers and options you can add to a plot. In general the site is a great reference for all things ggplot.

#counts on y-axis
g1<-ggplot(xy, aes(xvar)) + geom_histogram()                                      #horribly ugly default
g2<-ggplot(xy, aes(xvar)) + geom_histogram(binwidth=1)                            #change binwidth
g3<-ggplot(xy, aes(xvar)) + geom_histogram(fill=NA, color="black") + theme_bw()   #nicer looking

#density on y-axis
g4<-ggplot(xy, aes(x=xvar)) + geom_histogram(aes(y = ..density..), color="black", fill=NA) + theme_bw()

grid.arrange(g1, g2, g3, g4, nrow=1)

## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust
## this. stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to
## adjust this. stat_bin: binwidth defaulted to range/30. Use 'binwidth = x'
## to adjust this.

plot of chunk unnamed-chunk-2 Notice the warnings about the default binwidth that always is reported unless you specify it yourself. I will remove the warnings from all plots that follow to conserve space.
>> Density plots
We can do basic density plots as well. Note that the default for the smoothing kernel is gaussian, and you can change it to a number of different options, including kernel="epanechnikov" and kernel="rectangular" or whatever you want. You can find all of those options here.

#basic density
p1<-ggplot(xy, aes(xvar)) + geom_density()

#histogram with density line overlaid
p2<-ggplot(xy, aes(x=xvar)) + 
  geom_histogram(aes(y = ..density..), color="black", fill=NA) +
  geom_density(color="blue")

#split and color by third variable, alpha fades the color a bit
p3<-ggplot(xy, aes(xvar, fill = zvar)) + geom_density(alpha = 0.2)

grid.arrange(p1, p2, p3, nrow=1)

plot of chunk unnamed-chunk-3
>> Boxplots and more
We can also look at other ways to visualize our distributions. Boxplots are probably the most useful in order to describe the statistics of a distribution, but sometimes other visualizations are nice. I show a jitter plot and a volcano plot. More on boxplots here. Note that I removed the legend from each one because it is redundant.

#boxplot
b1<-ggplot(xy, aes(zvar, xvar)) + 
  geom_boxplot(aes(fill = zvar)) +
  theme(legend.position = "none")

#jitter plot
b2<-ggplot(xy, aes(zvar, xvar)) + 
  geom_jitter(alpha=I(1/4), aes(color=zvar)) +
  theme(legend.position = "none")

#volcano plot
b3<-ggplot(xy, aes(x = xvar)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..,
                   fill = zvar, color = zvar),
               geom = "ribbon", position = "identity") +
  facet_grid(. ~ zvar) +
  coord_flip() +
  theme(legend.position = "none")

grid.arrange(b1, b2, b3, nrow=1)

# Putting multiple plots together
# Finally, it's nice to put different plots together to get a real sense of the data. We can make a scatterplot of the data, 
# and add marginal density plots to each side. Most of the code below I adapted from this StackOverflow page. One way to do 
# this is to add distribution information to a scatterplot as a "rug plot". It adds a little tick mark for every point in 
# your data projected onto the axis.

# rug plot
ggplot(xy,aes(xvar,yvar)) + geom_point() + geom_rug(col="darkred", alpha=.1)

# Another way to do this is to add histograms or density plots or boxplots to the sides of a scatterplot. I followed the 
# stackoverflow page, but let me know if you have suggestions on a better way to do this, especially without the use of 
# the empty plot as a place-holder. I do the density plots by the zvar variable to highlight the differences in the two groups.

# placeholder plot - prints nothing at all
empty <- ggplot()+geom_point(aes(1,1), colour="white") +
theme(                              
plot.background = element_blank(), 
panel.grid.major = element_blank(), 
panel.grid.minor = element_blank(), 
panel.border = element_blank(), 
panel.background = element_blank(),
axis.title.x = element_blank(),
axis.title.y = element_blank(),
axis.text.x = element_blank(),
axis.text.y = element_blank(),
axis.ticks = element_blank()
)

# scatterplot of x and y variables
scatter <- ggplot(xy,aes(xvar, yvar)) + 
geom_point(aes(color=zvar)) + 
scale_color_manual(values = c("orange", "purple")) + 
theme(legend.position=c(1, 1), legend.justification=c(1, 1)) 

# marginal density of x - plot on top
plot_top <- ggplot(xy, aes(xvar, fill=zvar)) + 
geom_density(alpha=.5) + 
scale_fill_manual(values = c("orange", "purple")) + 
theme(legend.position = "none")

# marginal density of y - plot on the right
plot_right <- ggplot(xy, aes(yvar, fill=zvar)) + 
geom_density(alpha=.5) + 
coord_flip() + 
scale_fill_manual(values = c("orange", "purple")) + 
theme(legend.position = "none") 

# arrange the plots together, with appropriate height and width for each row and column
grid.arrange(plot_top, empty, scatter, plot_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

# It's really nice that grid.arrange() clips the plots together so that the scales are automatically the same. 
# You could get rid of the redundant axis labels by adding in theme(axis.title.x = element_blank()) in the density 
# plot code. I think it comes out looking very nice, with not a ton of effort. You could also add linear regression 
# lines and confidence intervals to the scatterplot. Check out my first ggplot2 cheatsheet for scatterplots if you 
# need a refresher. 


library(MCMCpack)
breg <- MCMCregress(incumbent.vote ~ gdp.growth + net.approval + two.terms, dat)
summary(breg); plot(breg)


## Bayesian analysis of sensory profiling data III
## www.r-bloggers.com/bayesian-analysis-of-sensory-profiling-data-iii/

# Data
# The data includes a treatment and a time variable. The time is in weeks and crossed with the other variables, 
# suggesting that this is systematically varied and hence in itself a factor of interest. In the end I decided 
# to cross treatments and time to generate a product factor with 30 levels. In addition I crossed time with a 
# repetition factor so this effect could function as a random variable. As bonus, there are some missing data, 
# which Stan does not like and are removed before analysis.

data(french_fries,package='reshape')
head(french_fries)
vars <- names(french_fries)
fries <- reshape(french_fries,
                 idvar=vars[1:4],
                 varying=list(vars[5:9]),
                 v.names='Score',
                 direction='long',
                 timevar='Descriptor',
                 times=vars[5:9])
head(fries)
fries$Product <- interaction(fries$treatment,fries$time)
fries$Session <- interaction(fries$time,fries$rep)
fries$Descriptor <- factor(fries$Descriptor)
fries <- fries[complete.cases(fries),]

# Model
# The model is a variation on last week, because I there is no rounds information but there are sessions. 
# In addition, I made the range for the shift variable a bit larger, because it appeared some panelists have extreme behavior. 
model1 <-
  '
data {
int<lower=0> npanelist;
int<lower=0> nobs;
int<lower=0> nsession;
int<lower=0> ndescriptor;
int<lower=0> nproduct;
vector[nobs] y;
int<lower=1,upper=npanelist> panelist[nobs];
int<lower=1,upper=nproduct> product[nobs];
int<lower=1,upper=ndescriptor> descriptor[nobs];
int<lower=1,upper=nsession> session[nobs];
real maxy;
}
parameters {
matrix<lower=0,upper=maxy> [nproduct,ndescriptor] profile;
vector<lower=-maxy/2,upper=maxy/2>[npanelist] shift[ndescriptor];
vector<lower=-maxy/2,upper=maxy/2>[npanelist] sitting[nsession];  
vector<lower=-1,upper=1> [npanelist] logsensitivity;
real<lower=0,upper=maxy/5> varr;
vector [npanelist] lpanelistvar;
vector [ndescriptor] ldescriptorvar;
real<lower=0,upper=maxy/5> sigmashift;
real<lower=0,upper=maxy/5> sigmasitting;
}
transformed parameters {
vector [nobs] expect;
vector[npanelist] sensitivity;
real mlogsens;
real mlpanelistvar;
real mldescriptorvar;
real meanshift[ndescriptor];
real meansit[nsession];
vector [nobs] sigma;
mlogsens <- mean(logsensitivity);
for (i in 1:ndescriptor) {
meanshift[i] <- mean(shift[i]);
}
mlpanelistvar <- mean(lpanelistvar);
mldescriptorvar <- mean(ldescriptorvar);   
for (i in 1:nsession) {
meansit[i] <- mean(sitting[i]);
}
for (i in 1:npanelist) {
sensitivity[i] <- pow(10,logsensitivity[i]-mlogsens);
}
for (i in 1:nobs) {
expect[i] <- profile[product[i],descriptor[i]]
*sensitivity[panelist[i]]
+ shift[descriptor[i],panelist[i]]-meanshift[descriptor[i]]
+ sitting[session[i],panelist[i]]-meansit[session[i]];
sigma[i] <- sqrt(varr
*exp(lpanelistvar[panelist[i]]-mlpanelistvar)
*exp(ldescriptorvar[descriptor[i]]-mldescriptorvar));
}
}
model {
logsensitivity ~ normal(0,0.1);
for (i in 1: ndescriptor) {
shift[i] ~ normal(0,sigmashift);
ldescriptorvar[i] ~ normal(0,1);
}
for (i in 1:nsession)
sitting[i] ~ normal(0,sigmasitting);
for (i in 1:npanelist)
lpanelistvar[i] ~ normal(0,1);
y ~ normal(expect,sigma);
} 
generated quantities    {
vector [npanelist] panelistsd;
vector [ndescriptor] descriptorsd;
for (i in 1:npanelist) {
panelistsd[i] <- sqrt(exp(lpanelistvar[i]));
}
for (i in 1:ndescriptor) {
descriptorsd[i] <- sqrt(exp(ldescriptorvar[i]));
}
}
'
# Additional code
# This is just the stuff needed to get Stan running
datainfries <- with(fries,list(
  panelist=(1:nlevels(subject))[subject],
  npanelist=nlevels(subject),
  session=(1:nlevels(Session))[Session],
  nsession=nlevels(Session),
  product=(1:nlevels(Product))[Product],
  nproduct=nlevels(Product),
  descriptor=(1:nlevels(Descriptor))[Descriptor],
  ndescriptor=nlevels(Descriptor),
  y=Score,
  nobs=length(Score),
  maxy=15))
pars <- c('profile','sensitivity','shift','sitting',
          'panelistsd','descriptorsd')
library(rstan)
fitfries <- stan(model_code = model1,
                 data = datainfries,
                 pars=pars,
                 iter = 1100,
                 warmup=100,
                 chains = 4)
# Results
# Traceplot is not shown, but the general plot provides some overview.
# Plot of the profile plot has been adapted given the design. This plot should convey the message the data contains. 
# As time progresses the flavor becomes more rancid and pointy, less potato. This process starts at circa 4 weeks.
# Treatment 3 seems least affected, but the difference is minute.

samples <- extract(fitfries,'profile')$profile
nsamp <- dim(samples)[1]
profile <- expand.grid(Product=levels(fries$Product),
                       Descriptor=levels(fries$Descriptor))
profile$des <- as.numeric(profile$Descriptor)
profile$prod <- as.numeric(profile$Product)
profs <- as.data.frame(t(sapply(1:nrow(profile),function(i){
  subsamples <-c(samples[1:nsamp,
                         profile$prod[i],profile$des[i]])
  c(mean=mean(subsamples),quantile(subsamples,c(.1,.9)))
})))
names(profs) <- c('score','lmin','lmax')
profile <- cbind(profile,profs)
profile <- merge(profile,
                 unique(fries[,c('Product','treatment','time')])
)

library(ggplot2)

profile$timen <- c(1:12)[profile$time]
profile <- profile[order(profile$timen),]

p1 <- ggplot(profile, aes(y=score, x=timen,color=treatment))

p1 + geom_path() +
  scale_x_continuous(
    breaks=1:nlevels(profile$time),
    labels=levels(profile$time)) +
  xlab('') +
  geom_ribbon(aes(ymin=lmin, ymax=lmax,fill=treatment),
              alpha=.15,linetype='blank') +
  facet_grid(  Descriptor ~ . )

## 

library(lattice)

#Build the data
nrowcol <- 1000
dat <- matrix(ifelse(runif(nrowcol*nrowcol) > 0.5, 1, 0), nrow=nrowcol)

#Build the palette and plot it
pal <- colorRampPalette(c("red", "yellow"), space = "rgb")
levelplot(dat, main="1000 X 1000 Levelplot", xlab="", ylab="", col.regions=pal(4), cuts=3, at=seq(0,1,0.5))






###############################################################################
## Face To Face With Marilyn Monroe
## http://www.r-bloggers.com/face-to-face-with-marilyn-monroe/

library("biOps")
library("abind")
rm(list=ls())
#############################################################
#Read Original Image
#############################################################
x     <- readJpeg("marilyn-monroe3.jpg")
plot(x)
#############################################################
#1. Swap
#############################################################
plot(imagedata(abind(x[, (ncol(x)/2):ncol(x), ], x[, 1:(ncol(x)/2), ], along=2)))
dev.copy(jpeg, filename="IMG01-Swap.jpg");
dev.off ();
#############################################################
#2. Artificial Symmetrical faces
#############################################################
plot(imagedata(abind(x[, 1:(ncol(x)/2), ], x[, (ncol(x)/2):1, ], along=2)))
dev.copy(jpeg, filename="IMG02-Symmetric1.jpg");
dev.off ();
plot(imagedata(abind(x[, ncol(x):(ncol(x)/2),], x[, (ncol(x)/2):ncol(x), ], along=2)))
dev.copy(jpeg, filename="IMG03-Symmetric2.jpg");
dev.off ();
#############################################################
#3. Uniform sampling over axis points
#############################################################
x2   <- aperm(array(255, dim = c(3, ncol(x), nrow(x))))
rows <- sample(1:nrow(x), round(nrow(x)*0.80), replace = FALSE)
cols <- sample(1:ncol(x), round(ncol(x)*0.80), replace = FALSE)
for (i in 1:length(rows))
{
  for (j in 1: length(cols))
  {
    x2[rows[i], cols[j], 1]<-x[rows[i], cols[j], 1]
    x2[rows[i], cols[j], 2]<-x[rows[i], cols[j], 2]
    x2[rows[i], cols[j], 3]<-x[rows[i], cols[j], 3]
  }
}
plot(imagedata(x2))
dev.copy(jpeg, filename="IMG04-Uniform1.jpg");
dev.off ();
#############################################################
#4. Uniform sampling over pixels
#############################################################
m2 <- matrix(rbinom(nrow(x)*ncol(x), 1, 0.5), nrow(x), ncol(x))
x4 <- do.call(abind, c(list(x[, , 1]*m2+(m2==0)*255, x[, , 2]*m2+(m2==0)*255, 
                            x[, , 3]*m2+(m2==0)*255), along=3))
plot(imagedata(x4))
dev.copy(jpeg, filename="IMG05-Uniform2.jpg");
dev.off ();
#############################################################
#6. Jittering
#############################################################
x1 <- mapply(as.matrix(x[, , 1]), FUN=function(x)
{z <- round(x+jitter(0, amount=50))
 if(z<0|z>255) x else z})
x1 <- matrix(x1, nrow=nrow(x), ncol=ncol(x))
x2 <- mapply(as.matrix(x[, , 2]), FUN=function(x)
{z <- round(x+jitter(0, amount=50))
 if(z<0|z>255) x else z})
x2 <- matrix(x2, nrow=nrow(x), ncol=ncol(x))
x3 <- mapply(as.matrix(x[, , 3]), FUN=function(x)
{z <- round(x+jitter(0, amount=50))
 if(z<0|z>255) x else z})
x3 <- matrix(x3, nrow=nrow(x), ncol=ncol(x))
x4 <- do.call(abind, c(list(x1, x2, x3), along=3))
plot(imagedata(x4))
dev.copy(jpeg,filename="IMG06-Jitter.jpg");
dev.off ();
#############################################################
#7. Mosaic
#############################################################
sptr <- 6 #Row splits
rnkr <- sample(1:sptr, size=sptr, replace=FALSE)
wthr <- floor(nrow(x)/sptr) #Splits width (row)
rnkr <- as.vector(sapply(rnkr, function (x) rep(x, wthr)))
rnkr <- rnkr*10E6+seq(1, length(rnkr), by=1)
rnkr <- rank(rnkr)
sptc <- round(ncol(x)/wthr)
rnkc <- sample(1:sptc, size=sptc, replace=FALSE)
wthc <- floor(ncol(x)/sptc) #Splits width (row)
rnkc <- as.vector(sapply(rnkc, function (x) rep(x, wthc)))
rnkc <- rnkc*10E6+seq(1, length(rnkc), by=1)
rnkc <- rank(rnkc)
x2 <- x[1:length(rnkr), 1:length(rnkc), ]
x2 <- x[rank(rnkr),rank(rnkc), ]
plot(imagedata(x2))
dev.copy(jpeg, filename="IMG07-Mosaic.jpg");
dev.off ();

#############################################################
## Wind Maps
## https://stat.ethz.ch/pipermail/r-help/2008-May/161067.html

u <- array(NA,c(5,8))
t <- seq(from=0.5, to=0.11,length=15)
t2 <- seq(from=(-0.7),to=(-0.1),length=25)
u[1:15] <- t
u[16:40] <- t2
v <- array(NA,c(5,8))
y <- seq(from=(-0.9), to=(-0.01),length=40) 
v[1:40] <- y

library(TeachingDemos)
library(maps)
map('state',col='grey')

lats <- seq( 27.5, 48.7, length=5 )
longs <- seq( -123, -73, length=8 )

x <- longs[ col(u) ]
y <- lats[ row(u) ]

speed <- sqrt( u*u+v*v )
dir <- atan2(v,u)

my.symbols(x, y, ms.arrows, angle=dir, r=speed, add=TRUE, length=.05)

## En fichero linea_aerea de contesta
# Separar codigo INE e id. Caracter. Estan separados por un caracter ":"
sapply(codigos[1,], class)
codigos$x <- as.character(codigos$x)
library(stringr)
temp <- str_split(codigos$x, ":")
temp5 <- str_locate(codigos$x, ":")
temp.id <- substr(codigos$x, (temp5+1), nchar(codigos$x))
temp.cod.ine <- substr(codigos$x, 1, temp5)
temp.cod.ine[temp.cod.ine==":"] <- "0000000000:"
temp.cod.ine <- substr(temp.cod.ine, 1, nchar(temp.cod.ine)-1)
codigos$cod.ine <- temp.cod.ine
codigos$id <- temp.id

# Todos los clientes con id dentro de temp11$id
clientes[(clientes$id %in% temp11$id), ]




## Measuring predictive performance and screening with ROC in R (ROCR, pROC).pdf
## Pedro concejero. Presentacion Grupo R

library(ROCR)
library(pROC)
library(sqldf)
library(gmodels)

datos <- read.table(file = "http://labs.fhcrc.org/pepe/book/data/psa2b.csv",
                    sep = ",", header = TRUE)

# PSA data
# http://labs.fhcrc.org/pepe/book/data/psa2b_desc.txt
# d - diagnostic | fpsa - free PSA | tpsa - total PSA

str(datos)

# Apart from that we have repeated PSA measures in time -t- and age at the moment of test. 
# But we are not going to use the time dimension for our ROC -will use simplest model: 
# obtain the first PSA measure but you must know diagnostic
# might be done at the end of the process

datos.originales <- datos
datos <- sqldf("select id, d, min(t), fpsa, tpsa, age from 'datos.originales'\n group by id")
?sqldf

# This study is an age-matched case-control design: each diagnosed case was 
# assigned a control matched to case by date of birth. There are 70 of each 
# group. One of the main advantages of ROC: it is applicable to any study
# independently of base rate. In this case: 50%.

barplot(table(datos$d))

# Diagnosed = 1
hist(datos$age[datos$d == 1], main = "Histogram of ages for diagnosed = 1")

# Diagnosed = 0
hist(datos$age[datos$d == 0], main = "Histogram of ages for diagnosed = 0")

boxplot(datos$fpsa ~ datos$d, main = "Boxplot of FPSA per diagnostic group")
boxplot(datos$tpsa ~ datos$d, main = "Boxplot of TPSA per diagnostic group")

plot(density((datos$fpsa[datos$d == 0])),
     col = "blue", xlim = c(0, 4),
     main = "FPSA density for diagnosed = 0 (blue) and 1 (red)")
lines(density((datos$fpsa[datos$d == 1])), 
      col = "red")

plot(density((datos$tpsa[datos$d == 0])),
     col = "blue", xlim = c(0, 20),
     main = "TPSA density for diagnosed = 0 (blue) and 1 (red)")
lines(density((datos$tpsa[datos$d == 1])), 
      col = "red")

# More usually, we will use a log transformation 

plot(density(log(datos$fpsa[datos$d == 0])),
     col = "blue", xlim = c(-5, 5),
     main = "log(FPSA) density for diagnosed = 0 (blue) and 1 (red)")
lines(density(log(datos$fpsa[datos$d == 1])), 
      col = "red")

plot(density(log(datos$tpsa[datos$d == 0])),
     col = "blue", xlim = c(-5, 5),
     main = "log(TPSA) density for diagnosed = 0 (blue) and 1 (red)")
lines(density(log(datos$tpsa[datos$d == 1])), 
      col = "red")

# The 2 x 2 table for each cutoff point
# Thanks english wikipedia!
# http://en.wikipedia.org/wiki/Receiver_operating_characteristic#Basic_concept

# TPSA cutoff point 2
# Performance at different cutoff points. Let's see 2 for tpsa

datos$scree <- "b-test neg"
datos$scree[datos$tpsa >= 2] <- "a-test pos"
datos$cond <- "a-cond pos"
datos$cond[datos$d == 0] <- "b-cond neg"

CrossTable(datos$scree, datos$cond, 
           prop.c = TRUE,
           prop.chisq = FALSE,
           prop.r = FALSE,
           prop.t = FALSE)

# TPSA cutoff point 4
# Performance at different cutoff points. Let's see 4 for tpsa

datos$scree <- "b-test neg"
datos$scree[datos$tpsa >= 4] <- "a-test pos"
datos$cond <- "a-cond pos"
datos$cond[datos$d == 0] <- "b-cond neg"

CrossTable(datos$scree, datos$cond, 
           prop.c = TRUE,
           prop.chisq = FALSE,
           prop.r = FALSE,
           prop.t = FALSE)

# Plot ROC with ROCR
# ROC is **a set of points** in the square space (0, 1) x (0, 1) where each 
# point is the pair (FPR: FALSE Positives Ratio - x axis, TPR: TRUE Positives 
# Ratio - y axis) 

plot(c(0, 1), c(0, 1), type= "n", xlab = "TPR", ylab = "FPR")
points(.171, .732)
points(.029, .465)

# ROC with ROCR - example participant's id
# prediction creates the predictor + criterion object
# performance creates the object with measures **from prediction object**
# Let's use this for the id - just in case it predicts anything

pred.z.01 <- prediction(datos$id, datos$d)

# uso: performance(prediction_object, "tpr", "fpr") creates the object with performance metrics
# TPR: True Positive Ratio
# FPR: False Positive Ratio

perf.z.01 <- performance(pred.z.01, "tpr", "fpr")

# ROC for participant's id
# We also plot the null predictive performance (when TPR = FPR; or diagonal in ROC space).
# AUC = Area Under Curve (0.5 = null predictive perf. 1 = perfect predictive perf)

plot.new()
plot(perf.z.01, col = "green") 
abline(0, 1, 
       col = "grey")

auc.z.01 <- performance(pred.z.01, "auc")

legend("bottomright", 
       paste(round(as.numeric(auc.z.01@y.values), digits = 2)), 
       col = c("green"),
       pch = c(3))

# ROC for participant's id
# AUC = 0.45 -> null predictive perf. (as could be expected)

plot.new()
plot(perf.z.01, col = "green") 
abline(0, 1, 
       col = "grey")

auc.z.01 <- performance(pred.z.01, "auc")

legend("bottomright", 
       paste(round(as.numeric(auc.z.01@y.values), digits = 2)), 
       col = c("green"),
       pch = c(3))

# ROC for fpsa, tpsa and age

pred.z.01 <- prediction(datos$age, datos$d)
pred.z.02 <- prediction(datos$fpsa, datos$d)
pred.z.03 <- prediction(datos$tpsa, datos$d)

perf.z.01 <- performance(pred.z.01, "tpr", "fpr")
perf.z.02 <- performance(pred.z.02, "tpr", "fpr")
perf.z.03 <- performance(pred.z.03, "tpr", "fpr")

plot.new()
plot(perf.z.01, col = "green") 
abline(0, 1, 
       col = "grey")
plot(perf.z.02, col = "blue", add = TRUE) 
plot(perf.z.03, col = "brown", add = TRUE) 

auc.z.01 <- performance(pred.z.01, "auc")
auc.z.02 <- performance(pred.z.02, "auc")
auc.z.03 <- performance(pred.z.03, "auc")

for_legend <- c(paste("Age ", round(as.numeric(auc.z.01@y.values), digits = 2)),
                paste("fpsa ", round(as.numeric(auc.z.02@y.values), digits = 2)),
                paste("tpsa ", round(as.numeric(auc.z.03@y.values), digits = 2)))

legend("bottomright", 
       for_legend, 
       col = c("green", "blue", "brown"),
       pch = c(3))

# ROC with pROC
# Far easier!

pROC::plot.roc(datos$d, datos$tpsa, print.auc = TRUE)


# ROC with pROC - with age groups
# green 45-55 blue 55-65 red >65

datos$age_gr <- cut(datos$age, 
                    breaks = c(45, 55, 65, 100))
pROC::plot.roc(datos$d[datos$age_gr == "(45,55]"], 
               datos$tpsa[datos$age_gr == "(45,55]"],
               print.auc = TRUE, col = "green", print.auc.col = "green", print.auc.y = 0.97, print.auc.x = 0.7)
pROC::plot.roc(datos$d[datos$age_gr == "(55,65]"], 
               datos$tpsa[datos$age_gr == "(55,65]"],
               print.auc = TRUE, col = "blue", add = TRUE, print.auc.col = "blue", print.auc.y = 0.82, print.auc.x = 0.7)
pROC::plot.roc(datos$d[datos$age_gr == "(65,100]"], 
               datos$tpsa[datos$age_gr == "(65,100]"],
               print.auc = TRUE, col = "red", add = TRUE, print.auc.col = "red", print.auc.y = 0.6, print.auc.x = 0.8)


CONCLUSIONS
========================================================
  - ROC is standard in medicine - huge experience in using this for screening and also for diagnostic performance of many types of tests
- We have seen one direct measure (tPSA), but you can measure predictive performance of any output of predictive models though you need a binary criterion
- Do not make a confusion with AUC -it is no percentage!! It is an abstract measure of performance
- You can use statistical contrasts to test differences between predictors, as well as predictors versus null predictive performance
- Make your choice pROC is more recent, many useful functions, ROCR maybe makes fancier plots
- R is **absolutely** the best option to make ROC analysis


# APPENDIX - save workspace

save.image("roc_psa_wkspace.rdata")

# APPENDIX - Origins in Signal Theory
# Two normal distributions, same sd different mean

s <- seq(-4, 6, 0.01)
mean1 <- 0
mean2 <- 2
sd <- 1

plot(s, dnorm(s, mean1, sd), type = "l", col = "blue")
lines(s, dnorm(s,mean2, sd), col = "red")
segments(1, 0, 1, 0.5, col = "green")

# APPENDIX - ROC with pROC - many useful options
# auc.polygon


pROC::plot.roc(datos$d, datos$tpsa,
               print.auc = TRUE,
               grid = TRUE,
               #               partial.auc = c(100, 90),
               auc.polygon = TRUE)

###############################################################################
## Creating publication quality graphs in R
## http://teachpress.environmentalinformatics-marburg.de/2013/07/creating-publication-quality-graphs-in-r-7/

# here's a rather handy way of loading all packages that you need
# for your code to work in one go
# First things first, as always in R, we load the necessary packages first (and 
# this should always happen at the beginning of a script, not somewhere in the 
# middle or towards the end!)
libs <- c('ggplot2', 'latticeExtra', 'gridExtra', 'MASS', 
          'colorspace', 'plyr', 'Hmisc', 'scales')
lapply(libs, require, character.only = T)

# load the diamonds data set (comes with ggplot2)

data(diamonds)

# The diamonds data set is structured as follows

str(diamonds)

# 1.1. sub-setting our data

# Suppose we're a stingy person and don't want to spend too much money on the 
# wedding ring for our loved one, we could create a data set only including 
# diamonds that cost less than 1000 US$ (though 1000 US$ does still seem very 
# generous to me).

diamonds.cheap <- subset(diamonds, price < 1000)

# Then our new data set would look like this

str(diamonds.cheap)

# Now the new diamonds.cheap subset is a reduced data set of the original 
# diamonds data set only having 14499 entries instead of the original 53940 entries.

# In case we were interested in a subset only including all diamonds of quality 
# (cut) 'Premium' the command would be

diamonds.premium <- subset(diamonds, cut == "Premium")
str(diamonds.premium)

# Any combinations of these subset commands are valid, e.g.

diamonds.premium.and.cheap <- subset(diamonds, cut == "Premium" & 
                                       price <= 1000)

# produces a rather strict subset only allowing diamonds of premium quality 
# that cost less than 1000 US$

# In case we want ANY of these, meaning all diamonds of premium quality OR 
# cheaper than 1000 US$, we would use the | operator to combine the two 
# specifications

diamonds.premium.or.cheap <- subset(diamonds, cut == "Premium" | 
                                      price <= 1000)

# The OR specification is much less rigid than the AND specification which will 
# result in a larger data set:
  
# diamonds.premium.and.cheap has 3204 rows, while
# diamonds.premium.or.cheap has 25111 rows

str(diamonds.premium.and.cheap)
str(diamonds.premium.or.cheap)

# 1.2. aggregating our data

# Suppose we wanted to calculate the average price of the diamonds for each 
# level of cut, i.e. the average price for all diamonds of "Ideal" quality, for 
# all diamonds of "Premium" quality and so on, this would be done like this

ave.price.cut <- aggregate(diamonds$price, by = list(diamonds$cut), 
                           FUN = mean)

# by = ... needs a list, even if there is only one entry
# and will look like this

ave.price.cut

# Note, that the original column names are not carried over to the newly created 
# table of averaged values. Instead these get the generic names Group.1 and x
# The Group.1 already indicates that we are not limited to aggregate just over 
# one factorial variable, more are also possible. Furthermore, any function to 
# compute the summary statistics which can be applied to all data subsets is 
# allowed, e.g. to compute the number of items per category we could use length

ave.n.cut.color <- aggregate(diamonds$price, 
                             by = list(diamonds$cut,
                                       diamonds$color), 
                             FUN = length)
ave.n.cut.color

# Given that as a result of aggregating this way we loose our variable names, 
# it makes sense to set them afterwards, so that we can easily refer to them later

names(ave.n.cut.color) <- c("cut", "color", "n")
str(ave.n.cut.color)

# 1.3. sorting our data

# Sorting our data according to one (or more) of the variables in our data can 
# also be very handy.
# Sorting of a vector can be achieved using sort()

sort(ave.n.cut.color$n)

# sorting of an entire dataframe is done using order() as follows
# for sorting according to one variable

ave.n.cut.color <- ave.n.cut.color[order(ave.n.cut.color$cut), ]
ave.n.cut.color

# for sorting according to two variables

ave.n.cut.color <- ave.n.cut.color[order(ave.n.cut.color$cut,
                                         ave.n.cut.color$n), ]
ave.n.cut.color

# 1.4. merging our data

# Often enough we end up with multiple data sets on our hard drive that contain 
# useful data for the same analysis. In this case we might want to amalgamate 
# our data sets so that we have all the data in one set.
# R provides a function called merge() that does just that

ave.n.cut.color.price <- merge(ave.n.cut.color, ave.price.cut, 
                               by.x = "cut", by.y = "Group.1")
ave.n.cut.color.price

# As the variable names of our two data sets differ, we need to specifically 
# provide the names for each by which the merging should be done (by.x & by.y). 
# The default of merge() tries to find variable names which are identical.
# Note, in order to merge more that two data frames at a time, we need to call 
# a powerful higher order function called Reduce(). This is one mighty function 
# for doing all sorts of things iteratively.

names(ave.price.cut) <- c("cut", "price")

set.seed(12)

df3 <- data.frame(cut = ave.price.cut$cut,
                  var1 = rnorm(nrow(ave.price.cut), 10, 2),
                  var2 = rnorm(nrow(ave.price.cut), 100, 20))

ave.n.cut.color.price <- Reduce(function(...) merge(..., all=T), 
                                list(ave.n.cut.color, 
                                     ave.price.cut,
                                     df3))
ave.n.cut.color.price

# 2. Data visualisation

# 2.1. Scatter plots

# If, from our original diamonds data set we wanted to see the relation between 
# price and carat of the diamonds (or more precisely how price is influenced by 
# carat), we would use a scatter plot.

# 2.1.1. The lattice way

scatter.lattice <- xyplot(price ~ carat, data = diamonds)

scatter.lattice

# What we see is that generally lower carat values tend to be cheaper.
# we can easily show the same relationship from figure 1, but this time for each 
# of the different quality levels (the variable cut in the diamonds data set) 
# into which diamonds can be classified. These conditional subsets are called 
# panels in lattice.
# This is done using the | character just after the formula expression. So the 
# complete formula would read:
  
y ~ x | g

# y is a function of x conditional to the values of g (where g is usually a 
# factorial variable)

# The code below shows how all of this is achieved.
# plot price ~ carat conditional to cut
# draw the regression line for each panel
# also provide the R2 value for each panel

scatter.lattice <- xyplot(price ~ carat | cut, 
                          data = diamonds, 
                          panel = function(x, y, ...) {
                            panel.xyplot(x, y, ...)
                            lm1 <- lm(y ~ x)
                            lm1sum <- summary(lm1)
                            r2 <- lm1sum$adj.r.squared
                            panel.abline(a = lm1$coefficients[1], 
                                         b = lm1$coefficients[2])
                            panel.text(labels = 
                                         bquote(italic(R)^2 == 
                                                  .(format(r2, 
                                                           digits = 3))),
                                       x = 4, y = 1000)
                          },
                          xscale.components = xscale.components.subticks,
                          yscale.components = yscale.components.subticks,
                          as.table = TRUE)

scatter.lattice

# As lattice is geared towards providing plots in small multiples (as E. Tufte calls them) or panels it provides a 
# panel = argument to which we can assign certain functions that will be evaluated separately for each of the panels. 
# There's a variety of pre-defined panel functions (such as the ones we used here - panel.xyplot, panel.abline, 
# panel.text & many more) but we can also define out own panel functions. This is why lattice is so versatile and 
# powerful. Basically, writing panel functions is just like writing any other function in R (though some limitations 
# do exist).

# The important thing to note here is that x and y in the context of panel functions refer to the x and y variables we 
# define in the plot definition, i.e. x = carat and y = price. So, for the panel functions we can use these shorthands, 
# like as we are doing in defining our linear model lm1 <- lm(y ~ x). This linear model will be calculated separately 
# for each of the panels, which are basically nothing else than subsets of our data corresponding to the different 
# levels of cut. Maybe it helps to think of this as a certain type of for loop:

for (level in levels(cut)) lm1 <- lm(price ~ carat)

# This then enables us to access the outcome of this linear model separately for each panel and we can use panel.abline 
# to draw a line in each panel corresponding to the calculated model coefficients a = Intercept and b = slope. Hence, 
# we are drawing the regression line which represents the line of least squares for each panel separately.

# The same holds true for the calculation and plotting of the adjusted R2 value for each linear model per panel. In this 
# case we use the panel.text function to 'write' the corresponding value into each of the panel boxes. The location of 
# the text is determined by the x = and y = arguments. This is where some car needs to be taken, as in the panel.text 
# call x and y don't refer to the x and y variables of the global plot function (xyplot) anymore, but rather represent 
# the locations of where to position the text within each of the panel boxes in the units used for each of the axes (in 
# our case x = 4 and y = 1000).

# Right, so now we have seen how to use panel functions to calculate and draw things specific to each panel. 
# One thing I really dislike about lattice is the default graphical parameter settings (such as colours etc). 
# However, changing these is rather straight forward. We can easily create our own themes by replacing the 
# default values for each of the graphical parameters individually and saving these as our own themes. 
# The function that let's us access the default (or already modified) graphical parameter settings is called 
# trellis.par.get(). Assigning this to a new object, we can modify every entry of the default settings to our 
# liking (remember str() provides a 'road map' for accessing individual bits of an object).

my.theme <- trellis.par.get()
my.theme$strip.background$col <- "grey80"
my.theme$plot.symbol$pch <- 16
my.theme$plot.symbol$col <- "grey60"
my.theme$plot.polygon$col <- "grey90"

l.sc <- update(scatter.lattice, par.settings = my.theme, 
               layout = c(3, 2),
               between = list(x = 0.3, y = 0.3))

print(l.sc)

# Apart from showing us how to change the graphical parameter settings, the above code chunk also highlights one of 
# the very handy properties of lattice (which is also true for ggplot2). We are able to store any plot we create in 
# an object and can refer to this object later. This enables us to simply update the object rather than having to 
# define it over (and over) again.

# Like many other packages, lattice has a companion called latticeExtra. This package provides several additions/extensions 
# to the core functionality of lattice. One of the very handy additions is a panel function called panel.smoother which 
# enables us to evaluate several linear and non-linear models for each panel individually. This means that we actually 
# don't need to calculate these models 'by hand' for each panel, but can use this pre-defined function to evaluate them.
# This is demonstrated in the next code chunk.

# Note that we are still calculating the linear model in order to be able to provide the R2 value for each panel. 
# We don't need the panel.abline anymore to draw the regression line anymore. Actually, this is done using panel.smoother 
# which also provides us with an estimation of the standard error related to the mean estimation of y for each x. This may 
# be hard to see, but there is a confidence band of the standard error plotted around the regression line in each panel. 
# Note, unfortunately, the the confidence intervals are very narrow so that they are hard to see in the plot below.

# For an overview of possible models to be specified using panel.smoother see ?panel.smoother (in library latticeExtra)

?panel.smoother
scatter.lattice <- xyplot(price ~ carat | cut, 
                          data = diamonds, 
                          panel = function(x, y, ...) {
                            panel.xyplot(x, y, ...)
                            lm1 <- lm(y ~ x)
                            lm1sum <- summary(lm1)
                            r2 <- lm1sum$adj.r.squared
                            panel.text(labels = 
                                         bquote(italic(R)^2 == 
                                                  .(format(r2, 
                                                           digits = 3))),
                                       x = 4, y = 1000)
                            panel.smoother(x, y, method = "lm", 
                                           col = "black", 
                                           col.se = "black",
                                           alpha.se = 0.3)
                          },
                          xscale.components = xscale.components.subticks,
                          yscale.components = yscale.components.subticks,
                          as.table = TRUE)

l.sc <- update(scatter.lattice, par.settings = my.theme)

print(l.sc)

# Before we go there, though, we need to spend some time to have a closer look at colour representation of certain 
# variables. A careful study of colour-spaces (e.g HERE AND HERE AND HERE) leads to the conclusion that the hcl colour 
# space is preferable when mapping a variable to colour (be it factorial or continuous).

# This colour space is readily available in R through the package colorspace and the function of interest is called hcl().

# In the code chunk that follows we will create a colour palette that varies in both colour (hue) and also luminance 
# (brightness) so that this can be distinguished even in grey-scale (printed) versions of the plot. As a reference 
# colour palette we will use the 'Spectral' palette from clorobrewer.org which is also a multi-hue palette but represents 
# a diverging colour palette. Hence, each end of the palette will look rather similar when converted to grey-scale.

clrs.spec <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
clrs.hcl <- function(n) {
  hcl(h = seq(230, 0, length.out = n), 
      c = 60, l = seq(10, 90, length.out = n), 
      fixup = TRUE)
}

# function to plot a colour palette
pal <- function(col, border = "transparent", ...)
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

# So here's the Spectral palette from colorbrewer.org interpolated over 100 colours

pal(clrs.spec(100))

# And this is what it looks like in grey-scale

pal(desaturate(clrs.spec(100)))

# We see that this palette varies in lightness from a very bright centre to darker ends on each side. Note, that the red 
# end is slightly darker than the blue end

# This is quite ok in case we want to show some diverging data (deviations from a zero point - e.g. the mean). If we, 
# however, are dealing with a sequential measure, such as temperature, or in our case the density of points plotted per 
# some grid cell, we really need to use a sequential colour palette. There are two common problems with sequential palettes:
# we need to create a palette that maps the data accurately. This means that the perceived distances between the different 
# hues utilised needs to reflect the distances between our data points. AND this distance needs to be constant, no matter 
# between which two point of the palette we want to estimate their distance. Let me give you an example. Consider the 
# classic 'rainbow' palette (Matlab refers to this as 'jet colors')

pal(rainbow(100))

# It becomes obvious that there are several thin bands in this colour palette (yellow, aquamarine, purple) which do not 
# map the distances between variable values accurately. That is, the distance between two values located in/around the 
# yellow region of the palette will seem to change faster than, for example somewhere in the green region (and red and blue).

# When converted to grey-scale this palette produces a hell of a mess.

pal(desaturate(rainbow(100)))

# Note, that this palette is maybe the most widely used colour coding palette for mapping a sequential continuous variable 
# to colour. We will see further examples later on.

# I hope you get the idea that this is not a good way of mapping a sequential variable to colour!
  
# hcl() produces so-called perceptually uniform colour palettes and is therefore 
# much better suited to represent sequential data.

pal(clrs.hcl(100))

# an hcl based multi-hue colour palette with increasing luminence towards the red end

# We see that the different hues are spread constantly over the palette and therefore it is easy to estimate distances 
# between data values from the changes in colour.

# The fact that we also vary luminance here means that we get a very light colour at one end (here the red end - which 
# is a more of a pink tone than red). This might, at first sight, not seem very aesthetically pleasing, yet it enables 
# us to encode our data even in grey-scale.

pal(desaturate(clrs.hcl(100)))

# As a general suggestion I encourage you to make use of the hcl colour space whenever you can. But most importantly, 
# it is essential to do some thinking before mapping data to colour. The most basic question you always need to ask 
# yourself is what is the nature of the data that I want to show? - sequential, diverging or qualitative? Once you know 
# this, it is easy to choose the appropriate colour palette for the mapping. A good place to start for choosing perceptually 
# well thought through palettes is the colorbrewer website.

# we need a so-called '2 dimensional kernel density estimator'. I won't go into much detail about the density estimation 
# here. What is important for our purpose is that we actually need to estimate this twice. Once globally, meaning for the 
# whole data set, in order to find the absolute extremes (minimum and maximum) of our data distribution. This is important 
# for the colour mapping, because the values of each panel need to be mapped to a common scale in order to interpret them. 
# In other words, this way we are making sure that the similar values of our data are represented by similar shades of, 
# let's say red.

# However, in order to be able to estimate the density for each of our panels we also need to do the same calculation in 
# our panel function.

# Essentially what we are creating is a gridded data set (like a photo) of the density of points within each of the defined 
# pixels. The lattice function for plotting gridded data is called levelplot().

# Here's the code

xy <- kde2d(x = diamonds$carat, y = diamonds$price, n = 100) 
xy.tr <- con2tr(xy)
offset <- max(xy.tr$z) * 0.2
z.range <- seq(min(xy.tr$z), max(xy.tr$z) + offset, offset * 0.01)

l.sc <- update(scatter.lattice, aspect = 1, par.settings = my.theme, 
between = list(x = 0.3, y = 0.3),
panel=function(x,y) {
xy <- kde2d(x,y, n = 100) 
xy.tr <- con2tr(xy)                 
panel.levelplot(xy.tr$x, xy.tr$y, xy.tr$z, asp = 1,
subscripts = seq(nrow(xy.tr)), 
contour = FALSE, region = TRUE, 
col.regions = c("white", 
rev(clrs.hcl(10000))),
at = z.range)
lm1 <- lm(y ~ x)
lm1sum <- summary(lm1)
r2 <- lm1sum$adj.r.squared
panel.abline(a = lm1$coefficients[1], 
b = lm1$coefficients[2])
panel.text(labels = 
bquote(italic(R)^2 == 
.(format(r2, digits = 3))),
x = 4, y = 1000)
#panel.xyplot(x,y) 
} 
) 

print(l.sc)

# It should not go unnoticed that there is a panel function in lattice that does this for you. The function is called 
# panel.smoothScatter() and unless we need to specify a custom colour palette, this is more than sufficient.

# As a hint, if you want to use this panel function with your own colour palette you need to make sure that your 
# palette starts with white as otherwise things will look really weird.

l.sc.smooth <- update(scatter.lattice, aspect = 1, 
                      par.settings = my.theme, 
                      between = list(x = 0.3, y = 0.3),
                      panel = panel.smoothScatter)

print(l.sc.smooth)

# This representation of our data basically adds another dimension to our plot which enables us to see that no matter 
# what the quality of the diamond, most of the diamonds are actually of low carat and low price. Whether this is good 
# or bad news, depends on your interpretation (and the size of your wallet, of course).

# 2.1.2. The ggplot2 way

# Now let's try to recreate what we have achieved with lattice using ggplot2.

# ggplot2 is radically different from the way that lattice works. lattice is much closer to the traditional way of 
# plotting in R. There are different functions for different types of plots. In ggplot2 this is different. Every plot 
# we want to draw, is, at a fundamental level, created in exactly the same way. What differs are the subsequent calls 
# on how to represent the individual plot components (basically x and y). This means a much more consistent way of 
# building visualisations, but it also means that things are rather different from what you might have learned about 
# syntax and structure of (plotting) objects in R. But not to worry, even I managed to understand how thing are done 
# in ggplot2 (and prior to writing this I had almost never used it before).

# Before I get carried away too much let's jump right into our first plot using ggplot2

scatter.ggplot <- ggplot(aes(x = carat, y = price), data = diamonds)

g.sc <- scatter.ggplot + geom_point()

print(g.sc)

# Let's look at the above code in a little more detail. The first line is the fundamental definition of what we want to 
# plot. We provide the 'aesthetics' for the plot (aes()) We state that we want the values on the x-axis to represent 
# carat and the y-values are price. The data set to take these variables from is the diamonds data set. That's basically 
# it, and this will not change a hell of a lot in the subsequent plotting routines.

# What will change in the plotting code chunks that follow is how we want the relationship between these variables to be 
# represented in our plot. This is done by defining so-called geometries (geom_...()). In this first case we stated that 
# we want the relationship between x and y to be represented as points, hence we used geom_point().

# If we wanted to provide a plot showing the relationship between price and carat in panels representing the quality of 
# the diamonds, we need what in gglot2 is called facetting (panels in lattice). To achieve this, we simply repeat our 
# plotting call from earlier and add another layer to the call which does the facetting.

g.sc <- scatter.ggplot + 
  geom_point() +
  facet_wrap(~ cut)

print(g.sc)

# One thing that some people dislike about the default settings in ggplot2 is the grey background of the plots. This 
# grey background is, in my opinion, a good idea when colors are involved as it tends to increase the contrast of the 
# colours. If, however, the plot is a simple black-and-white scatter plot as in our case here, a white panel, or better 
# facet background seems more reasonable. We can easily change this using a pre-defined theme called theme_bw().

# In order to provide the regression line for each panel like we did in lattice, we need a function called stat_smooth(). 
# This is fundamentally the same function that we used earlier, as the panel.smoother() in lattice is based on stat_smooth().

# Putting this together we could do something like this (note that we also change the number of rows and columns into 
# which the facets should be arranged):
  
  g.sc <- scatter.ggplot + 
  #geom_tile() +
  geom_point(colour = "grey60") +
  facet_wrap(~ cut, nrow = 2, ncol = 3) +
  theme_bw() +
  stat_smooth(method = "lm", se = TRUE, 
              fill = "black", colour = "black")

print(g.sc)

# Simple and straight forward, and the result looks rather similar to the lattice version we created earlier.

# Creating a point density scatter plot in ggplot2 is actually a fair bit easier than in lattice, as gglot2 provides 
# several predefined stat_...() function calls. One of these is designed to create 2 dimensional kernel density 
# estimations, just what we want. However, this is where the syntax of ggplot2 really becomes a bit abstract. he 
# definition of the fill argument of this call is ..density.., which, at least to me, does not seem very intuitive.

# Furthermore, it is not quite sufficient to supply the stat function, we also need to state how to map the colours to 
# that stat definition. Therefore, we need yet another layer which defines what colour palette to use. As we want a 
# continuous variable (density) to be filled with a gradient of n colours, we need to use scale_fill_gradientn() in 
# which we can define the colours we want to be used.

g.sc <- scatter.ggplot + 
  geom_tile() +
  #geom_point(colour = "grey60") +
  facet_wrap(~ cut, nrow = 3, ncol = 2) +
  theme_bw() +
  stat_density2d(aes(fill = ..density..), n = 100,
                 geom = "tile", contour = FALSE) +
  scale_fill_gradientn(colours = c("white",
                                   rev(clrs.hcl(100)))) +
  stat_smooth(method = "lm", se = FALSE, colour = "black") +
  coord_fixed(ratio = 5/30000)

print(g.sc)

# 2.2. Box an whisker plots

# I honestly don't have a lot to say about box and whisker plots. They are probably the most useful plots for showing 
# the nature/distribution of your data and allow for some easy comparisons between different levels of a factor for example.

# see http://upload.wikimedia.org/wikipedia/commons/1/1a/Boxplot_vs_PDF.svg for a visual representation of the standard 
# R settings of BW plots in relation to mean and standard deviation of a normal distribution.

# So without further ado, here's a basic lattice box and whisker plot.

# 2.2.1. The lattice way

bw.lattice <- bwplot(price ~ color, data = diamonds)

bw.lattice

# Not so very beautiful. So, let's again modify the standard par.settings so that we get an acceptable box and whisker plot.

bw.theme <- trellis.par.get()
bw.theme$box.dot$pch <- "|"
bw.theme$box.rectangle$col <- "black"
bw.theme$box.rectangle$lwd <- 2
bw.theme$box.rectangle$fill <- "grey90"
bw.theme$box.umbrella$lty <- 1
bw.theme$box.umbrella$col <- "black"
bw.theme$plot.symbol$col <- "grey40"
bw.theme$plot.symbol$pch <- "*"
bw.theme$plot.symbol$cex <- 2
bw.theme$strip.background$col <- "grey80"

l.bw <- update(bw.lattice, par.settings = bw.theme)

print(l.bw)

# Much better, isn't it?

bw.lattice <- bwplot(price ~ color | cut, data = diamonds,
                     asp = 1, as.table = TRUE, varwidth = TRUE)
l.bw <- update(bw.lattice, par.settings = bw.theme, xlab = "color", 
               fill = clrs.hcl(7),
               xscale.components = xscale.components.subticks,
               yscale.components = yscale.components.subticks)

print(l.bw)

# In addition to the rather obvious provision of a color palette to to fill the boxes, in this final box & whisker plot 
# we have also told lattice to adjust the widths of the boxes so that they reflect the relative sizes of the data samples 
# for each of the factors (colours). This is a rather handy way of providing insight into the data distribution along the 
# factor of the x-axis. We can show this without having to provide any additional plot to highlight that some of the factor 
# levels (i.e. colours) are much less represented than others ('J' compared to 'G', for example, especially for the 'Ideal' 
# quality class).

# 2.2.2. The ggplot2 way

# As much as I love lattice, I always end up drawing box and whisker plots with ggplot2 because they look so much nicer 
# and there's no need to modify so many graphical parameter settings in order to get an acceptable result.

# You will see what I mean when we plot a ggplot2 version using the default settings.

bw.ggplot <- ggplot(diamonds, aes(x = color, y = price))

g.bw <- bw.ggplot + geom_boxplot()

print(g.bw)

# This is much much better straight away!!

# And, as we've already seen, the facetting is also just one more line.

bw.ggplot <- ggplot(diamonds, aes(x = color, y = price))

g.bw <- bw.ggplot + 
geom_boxplot(fill = "grey90") +
theme_bw() +
facet_wrap(~ cut)

print(g.bw)

# So far, you may have gotten the impression that pretty much everything is a little bit easier the ggplot2 way. Well, 
# a lot of things are, but some are not. If we wanted to highlight the relative sample sizes of the different colour 
# levels like we did earlier in lattice (using varwidth = TRUE) we have to put a little more effort into ggplot2. Meaning, 
# we have to calculate this ourselves. There is no built in functionality for this feature (yet), at least none that 
# I am aware of.

# But, it is not too complicated. The equation for this adjustment is rather straight forward, we simply take the square 
# root of the counts for each colour and divide it by the overall number of observations. Then we standardise this relative 
# to the maximum of this calculation. As a final step, we need to break this down to each of the panels of the plot. This 
# is the toughest part of it. I won't go into any detail here, but the llply pat of the following code chunk is basically 
# the equivalent of what is going on behind the scenes of lattice (though lattice most likely does not use llply).

# Anyway, it does not require too many lines of code to achieve the box width adjustment in ggplot2.

w <- sqrt(table(diamonds$color)/nrow(diamonds))
# standardise w to maximum value
w <- w / max(w)

g.bw <- bw.ggplot + 
facet_wrap(~ cut) +
llply(unique(diamonds$color), 
function(i) geom_boxplot(fill = clrs.hcl(7)[i],
width = w[i], outlier.shape = "*",
outlier.size = 3,
data = subset(diamonds, color == i))) +
theme_bw()

print(g.bw)

# The result is very very similar to what we have achieved earlier with lattice. In summary, lattice needs a little more 
# care to adjust the standard graphical parameters, whereas ggplot2 requires us to manually calculate the width of the 
# boxes. I leave it up to you, which way suits you better. I have already made my choice a few years ago ;-)

# Boxplots are, as mentioned above, a brilliant way to visualise data distribution(s). Their strength lies in the 
# comparability of different classes as they are plotted next to each other using a common scale. Another, more classical 
# - as parametric - way are histograms (and densityplots).

# 2.2. Histograms & densityplots

# The classic way to visualise the distribution any data are histograms. They are closely related to density plots, where 
# the individual data points are not binned into certain classes but a continuous density function is calculated to show 
# the distribution. Both approaches reflect a certain level of abstraction (binning vs. functional representation), 
# therefore a general formulation of which of them is more accepted is hard. In any case, the both achieve exactly the 
# same result, they will show us the distribution of our data.

# 2.2.1. The lattice way

# As is to be expected with lattice, the default plotting routine does not really satisfy the (or maybe better my) 
# aesthetic expectations.

hist.lattice <- histogram(~ price, data = diamonds)
hist.lattice

# This is even worse for the default density plot.

dens.lattice <- densityplot(~ price, data = diamonds)
dens.lattice

# Yet, as we've already adjusted our global graphical parameter settings, we can now easily modify this.

hist.lattice <- histogram(~ price | color, 
                          data = diamonds,
                          as.table = TRUE,
                          par.settings = my.theme)

l.his <- hist.lattice

print(l.his)

# Now, this is a plot that every journal editor will very likely accept.

# Until now, we have seen how to condition our plots according to one factorial variable (diamonds$cut). It is, in theory, 
# possible to condition plots on any number of factorial variable, though more than two is seldom advisable. Two, however, 
# is definitely acceptable and still easy enough to perceive and interpret. In lattice this is generally done as follows.

y ~ x | g + f

# where g and f are the factorial variables used for the conditioning.

# In the below code chunk we are first creating out plot object and the we are using a function called useOuterStrips() 
# which makes sure that the strips that correspond to the conditioning variables are plotted on both the top and the left 
# side of our plot. The default lattice setting is to plot both at the top, which makes the navigation through the plot by 
# the viewer a little more difficult.

# Another default setting for densityplots in lattice is to plot a point (circle) for each observation of our variable 
# (price) at the bottom of the plot along the x-axis. In our case, as we have a lot of data points, this is not desirable, 
# so we set the argument plot.points to FALSE.

dens.lattice <- densityplot(~ price | cut + color, 
                            data = diamonds,
                            as.table = TRUE,
                            par.settings = my.theme,
                            plot.points = FALSE,
                            between = list(x = 0.2, y = 0.2),
                            scales = list(x = list(rot = 45)))

l.den <- useOuterStrips(dens.lattice)

print(l.den)

# You may have noticed that the lines of the densityplot are plotted in a light shade of blue (cornflowerblue to be 
# precise). I it up to you to change this yourself.

# Another thing you may notice when looking at the above plot is that the x-axis labels are rotated by 45 degrees. This 
# one I also leave up to you to figure out. ;-)

# 2.2.2. The ggplot2 way

# Much like with the box and whisker plot, the default settings of ggplot2 are quite a bit nicer for both histogram and 
# densityplot.

hist.ggplot <- ggplot(diamonds, aes(x = price))

g.his <- hist.ggplot +
  geom_histogram()

print(g.his)

# stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.

# One thing that is really nice about the ggplot2 densityplots that it is so easy to fill the area under the curve which 
# really helps the visual representation of the data.

dens.ggplot <- ggplot(diamonds, aes(x = price))

g.den <- dens.ggplot +
  geom_density(fill = "black", alpha = 0.5)

print(g.den)

# Just as before, we are encountering again the rather peculiar way of ggplot2 to adjust certain default settings to 
# suit our needs (likes). In we wanted to show percentages instead of counts for the histograms, we again need to use 
# the strange ..something.. syntax.

# Another thing I want to highlight in the following code chunk is the way to achieve binary conditioning in ggplot2. 
# This is done as follows:
  
  facet_grid(g ~ f)

# where, again, g and f are the two variables used for the conditioning.

g.his <- hist.ggplot +
  geom_histogram(aes(y = ..ncount..)) +
  scale_y_continuous(labels = percent_format()) +
  facet_grid(color ~ cut) + 
  ylab("Percent")

print(g.his)

# Similar to our lattice approach we're going to rotate the x-axis labels by 45 degrees.

dens.ggplot <- ggplot(diamonds, aes(x = price))

g.den <- dens.ggplot +
  geom_density(fill = "black", alpha = 0.5) +
  facet_grid(color ~ cut) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g.den)

# Ok, another thing we might want to show is the value of a certain estimated value (like the mean of our sample) 
# including error bars.

# 2.3. Plotting error bars

# 2.3.1. The lattice way

# Honestly, lattice sucks at plotting error bars. Therefore, we will only explore one way of achieving this. In case you 
# really want to explore this further, I refer you to Stackoverflow and other R related forums/lists, where you will find 
# a solution, but I doubt that you will like it. Error bars are much easier plotted using ggplot2.

my.theme$dot.symbol$col <- "black"
my.theme$dot.symbol$cex <- 1.5
my.theme$plot.line$col <- "black"
my.theme$plot.line$lwd <- 1.5

dmod <- lm(price ~ cut, data = diamonds)
cuts <- data.frame(cut = unique(diamonds$cut), 
                   predict(dmod, data.frame(cut = unique(diamonds$cut)), 
                           se = TRUE)[c("fit", "se.fit")])

errbar.lattice <- Hmisc::Dotplot(cut ~ Cbind(fit, 
                                             fit + se.fit, 
                                             fit - se.fit),
                                 data = cuts, 
                                 par.settings = my.theme)

l.err <- errbar.lattice

print(l.err)

# 2.3.2. The ggplot2 way

# As mentioned above, when plotting error bars ggplot2 is much easier. Whether this is because of the general ongoing 
# discussion about the usefulness of these plots I do not want to judge.

# Anyway, plotting error bars in gglplot2 is as easy as everything else.

errbar.ggplot <- ggplot(cuts, aes(cut, fit, ymin = fit - se.fit, 
                                  ymax=fit + se.fit))
g.err <- errbar.ggplot + 
  geom_pointrange() +
  coord_flip() +
  theme_classic()

print(g.err)

# Especially, when plotting them as part of a bar plot.

g.err <- errbar.ggplot + 
  geom_bar(fill = "grey80") +
  geom_errorbar(width = 0.2) +
  coord_flip() +
  theme_classic()

print(g.err)

# Just as before (with the box widths) though, applying this to each panel is a little more complicated.

errbar.ggplot.facets <- ggplot(diamonds, aes(x = color, y = price))

# function to calculate the standard error of the mean
se <- function(x) sd(x)/sqrt(length(x))

# function to be applied to each panel/facet
my.fun <- function(x) {
  data.frame(ymin = mean(x) - se(x), 
             ymax = mean(x) + se(x), 
             y = mean(x))
}

g.err.f <- errbar.ggplot.facets + 
  stat_summary(fun.y = mean, geom = "bar", 
               fill = clrs.hcl(7)) + 
  stat_summary(fun.data = my.fun, geom = "linerange") + 
  facet_wrap(~ cut) +
  theme_bw()

print(g.err.f)

# Still, trust me on this, much much easier than to achieve the same result in lattice.

# 3. Manipulating plots with the grid package

# Both, lattice and ggplot2 are based upon the grid package. This means that we can use this package to fundamentally 
# modify whatever we've produced (remember, we're always storing our plots in objects) in a much more flexible way that 
# provided by any pf these packages.

# n order to fully appreciate the possibilities of the grid package, it helps to think of this package as a package for 
# drawing things. Yes, we're not producing statistical plots as such (for this we have lattice and ggplot2), we're actually 
# drawing* things!

# The fundamental features of the grid package are the viewports. By default, the whole plotting area, may it be the 
# standard R plotting device or the png plotting device, is considered as the root viewport (basically like the 
# home/<username> folder on your linux system or the Users\<username> folder in windows). In this viewport we now have 
# the possibility to specify other viewports which are relative to the root viewport (just like the Users<username>\Documents 
# folder in windows or - to provide a different example - the home/<username>/Downloads folder in linux).

# The very very important thing to realise here is that in order to do anything in this folder (be it creating another 
# sub-folder or simply saving a file or whatever), we need to first create the folder and then we also need to 
# navigate/change/go into the folder. If you keep this in mind, you will quickly understand the fundamental principle of grid.

# When we start using the grid package, we always start with the 'root' viewport. This is already available, it is created 
# for us, we don't need to do anything. This is our starting point. The really neat thing about grid is that each viewport 
# is, by default, defined as both x and y ranging from 0 - 1. In the lower left corner x = 0 and y = 0. The lower right 
# corner is x = 1 & y = 0, the upper right corner x = 1 & y = 1 and so on. It is, however, possible to specify a myriad of 
# different unit systems (type, with the grid package loaded, ?unit to get an overview of what is available). I usually 
# stick to these default settings called npc - natural parent coordinates which range from 0 - 1 in each direction, as 
# this makes setting up viewports very intuitive.

# A viewport needs some basic specifications for it to be located somewhere in the plotting area (the current viewport). 
# These are:
  
# x - the location along the x-axis
# y - the location along the y -axis
# width - the width of the viewport
# height - the height of the viewport
# just - the justification of the viewport in both x and y directions

# width and height should be rather self-explanatory.

# x, y and just are a bit more mind-bending. As default, x and y are 0.5 and just is c(centre, centre). This means that 
# the new viewport will be positioned at x = 0.5 and y = 0.5. As the default of just is centre this means that a new 
# viewport will be created at the midpoint of the current viewport (0.5 & 0.5) and it will be centered on this point. 
# It helps to think of the just specification in the same way that you provide your text justification in Word (left, 
# right, centre & justified). Let us try a first example which should highlight the way this works

grid.newpage()

grid.rect()
grid.text("this is the root vp", x = 0.5, y = 1, 
          just = c("centre", "top"))

our.first.vp <- viewport(x = 0.5, y = 0.5, 
                         height = 0.5, width = 0.5,
                         just = c("centre", "centre"))

pushViewport(our.first.vp)

grid.rect(gp = gpar(fill = "pink"))
grid.text("this is our first vp", x = 0.5, y = 1, 
          just = c("centre", "top"))

# Ok, so now we have created a viewport in the middle of the root viewport at x = 0.5 and y = 0.5 - just = c("centre", 
# ,"centre") that is half the height and half the width of the original viewport - height = 0.5 and width = 0.5.

# Afterwards we navigated into this viewport - pushViewport(our.first.vp) and then we have drawn a rectangle that fills 
# the entire viewport and filled it in pink colour - grid.rect(gp = gpar(fill = "pink"))

# Note that we didn't leave the viewport yet. This means, whatever we do now, will happen in the currently active viewport 
# (the pink one). To illustrate this, we will simply repeat the exact same code from above once more.

our.first.vp <- viewport(x = 0.5, y = 0.5, 
height = 0.5, width = 0.5,
just = c("centre", "centre"))

pushViewport(our.first.vp)

grid.rect(gp = gpar(fill = "cornflowerblue"))

# This means, that whatever viewport we are currently in, this defines our reference system (0 - 1). In case you don't 
# believe me, we can repeat this 10 times more.

for (i in 1:10) {
  our.first.vp <- viewport(x = 0.5, y = 0.5, 
                           height = 0.5, width = 0.5,
                           just = c("centre", "centre"))
  
  pushViewport(our.first.vp)
  
  grid.circle(gp = gpar(fill = "cornflowerblue"))
}

# I hope this is proof enough now! We are cascading down viewports always creating a rectangle that fills half the 
# 'mother' viewport each time. Yet, as the 'mother' viewport becomes smaller and smaller, our rectangles also become 
# smaller at each step along the way (programmers would actually call these steps iterations, but we won't be bothered here.)

# So, how do we navigate back?

# If I counted correctly, we went down 12 rabbit holes. In order to get out of these again, we need to upViewport(12) 
# and in order to see whether we are correct, we ask grid what viewport we are currently in.

upViewport(12)

current.viewport()

# Sweet, we're back in the 'root' viewport.

# Now, let's see how this just parameter works. As you have seen we are now in the root viewport. Let's try to draw 
# another rectangle that sits right at the top left corner of the pink one. In theory the lower right corner of this 
# viewport should be located at x = 0.25 and y = 0.75. If we specify it like this, we need to adjust the justification, 
# because we do not want to centre it on these coordinates. If these coordinates are the point of origin, this viewport 
# should be justified right horizontally and bottom vertically. And the space we have to plot should be 0.25 vertically 
# and 0.25 horizontally. Let's try this.

top.left.vp <- viewport(x = 0.25, y = 0.75, 
height = 0.25, width = 0.25,
just = c("right", "bottom"))

pushViewport(top.left.vp)

grid.rect(gp = gpar(fill = "grey", alpha = 0.5))

# I hope that you have understood 2 things now:

# how to create and navigate between viewports
# why I said earlier that grid is a package for drawing.

# Assuming that you have understood these two points, lets make use of the first one and use this incredibly flexible 
# plotting framework for arranging multiple plots on one page.

# 3.1. multiple plots on one page

# In order to succeed plotting several of our previoulsly created plots on one page, there's two things of importance:

# the lattice/ggplot2 plot objects need to be printed using print(latticeObject)/print(ggplot2Object)
# we need to set newpage = FALSE in the print call so that the previously drawn elements are not deleted

# Let's try and plot the lattice and ggplot2 versions of our box and whisker plots and scatter plots next to each 
# other on one page by setting up a suitable viewport structure.

# clear plot area
grid.newpage()

# define first plotting region (viewport)
vp1 <- viewport(x = 0, y = 0, 
                height = 0.5, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")

# enter vp1 
pushViewport(vp1)

# show the plotting region (viewport extent)
grid.rect(gp = gpar(fill = "red", alpha = 0.2))

# plot a plot - needs to be printed (and newpage set to FALSE)!!!
print(l.bw, newpage = FALSE)

# leave vp1 - up one level (into root vieport)
upViewport(1)

# define second plot area
vp2 <- viewport(x = 1, y = 0, 
                height = 0.5, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")

# enter vp2
pushViewport(vp2)

# show the plotting region (viewport extent)
grid.rect(gp = gpar(fill =  "white", alpha = 0.2))

# plot another plot
print(g.bw, newpage = FALSE)

# leave vp2
upViewport(1)


vp3 <- viewport(x = 0, y = 1, 
                height = 0.5, width = 0.5,
                just = c("left", "top"),
                name = "upper left")

pushViewport(vp3)

# show the plotting region (viewport extent)
grid.rect(gp = gpar(fill =  "green", alpha = 0.2))

print(l.sc, newpage = FALSE)

upViewport(1)


vp4 <- viewport(x = 1, y = 1, 
                height = 0.5, width = 0.5,
                just = c("right", "top"),
                name = "upper right")

pushViewport(vp4)

# show the plotting region (viewport extent)
grid.rect(gp = gpar(fill =  "black", alpha = 0.2))

print(g.sc, newpage = FALSE)

upViewport(1)

# 3.2. manipulating existing plots

# Another application of grid is to manipulate an existing plot object. You may have noted that our version of the 2D 
# density scatter plot produced with lattice lacks a colour key. This can be easily added using grid. As lattice is 
# built upon grid, it produces a lot of viewports in the creation of the plots (like our scatter plot). We can, after 
# they have been set up, navigate to each of these and edit them or delete them or add new stuff. Looking at the ggplot2 
# version of the density scatter, we see that this has a colour key which is placed to right of the main plot. Given that 
# we have 5 panels, we actually have some 'white' space that we could use for the colour key placement, thus making better 
# use of the available space.

# In order to do so, we need to know into which of the viewports created by lattice we need to navigate. Therefore, we 
# need to know their names. lattice provides a function for this. We can use trellis.vpname() to extract the name(s) of 
# the viewport(s) we are interested in. lattice provides a structured naming convention for its viewports. We can use this 
# to specify what viewport name(s) we want to extract. As we are interested in the viewport that comprises the main figure, 
# we will tell lattice to extract this name (see below in the code). Then we can navigate into this viewport downViewport() 
# and set up a new viewport in the main plotting area (the figure viewport) to make use of the existing 'white' space. 
# Remember that the default units of grid are 0 - 1. This means that we can easily calculate the necessary viewport 
# dimensions. Let's see how this is done (Note, we are actually creating 2 new viewports in the figure area, one for the 
# colour key and another one for the colour key label).

grid.newpage()
#grid.rect()
print(l.sc, newpage = FALSE)
#grid.rect()
downViewport(trellis.vpname(name = "figure"))
#grid.rect()
vp1 <- viewport(x = 1, y = 0, 
height = 0.5, width = 0.3,
just = c("right", "bottom"),
name = "legend.vp")

pushViewport(vp1)
#grid.rect()

vp1.1 <- viewport(x = 0.2, y = 0.5, 
height = 0.7, width = 0.5,
just = c("left", "centre"),
name = "legend.key")

pushViewport(vp1.1)
#grid.rect()

xy <- kde2d(x = diamonds$carat, y = diamonds$price, n = 100) 
xy.tr <- con2tr(xy)
offset <- max(xy.tr$z) * 0.01
z.range <- seq(min(xy.tr$z), max(xy.tr$z) + 50 * offset, offset)

key <- draw.colorkey(key = list(col = c("white", rev(clrs.hcl(10000))),
at = z.range), draw = TRUE)

seekViewport("legend.vp")
#grid.rect()
vp1.2 <- viewport(x = 1, y = 0.5, 
height = 1, width = 0.3,
just = c("right", "centre"),
name = "legend.text", angle = 0)

pushViewport(vp1.2)
#grid.rect()

grid.text("estimated point density", 
x = 0, y = 0.5, rot = 270, 
just = c("centre", "bottom"))

upViewport(3)

# Not too complicated, ey. And, in comparison to the ggplot2 version, we are utilising the available space a bit 
# more efficiently. Though, obviously, we could also manipulate the ggplot2 density scatter plot (or any other plot) 
# in a similar manner.

# I hope it became clear just how useful grid can be and how it provides us with tools which enable us to produce 
# individual graphics that satisfy our needs. This may become a little clearer in section 5, where we will see how 
# to use grid for map creation.

# So far, we have put our efforts into plot creation. I think, by now, we have a few tools handy to achieve what we want, 
# so let's see how we can save our graphics to our hard drive.

# 4. Saving your visualisations

# Saving graphics in R is very straight forward. We simply need to call a suitable device. There are a number of 
# different plotting devices available. Here, we will introduce 2 examples, postscript and png. postscript should be 
# used for vector graphics (e.g. line plots, point plots, polygon plots etc.), whereas png is preferable for raster 
# graphics (e.g. photos, our density scatter plot or anything pixel based).

# All the graphics devices in R basically work the same way:

# we open the respective device (postscript, png, jpeg, svg.)
# we plot (or in our case print our plot objects)
# we close the device using dev.off() - otherwise the file will not be written to the hard drive.

# That's it. Simple and straight forward. Here's the two examples using postscript and png:

postscript("test.ps")
print(g.bw)
dev.off()

png("test.png", width = 14.7, height = 18.8, units = "cm", res = 600, pointsize = 12)

grid.newpage()

print(l.sc, newpage = FALSE)

downViewport(trellis.vpname(name = "figure"))

vp1 <- viewport(x = 1, y = 0, height = 0.5, width = 0.3, just = c("right", "bottom"), name = "legend.vp")

pushViewport(vp1)
# grid.rect()

vp1.1 <- viewport(x = 0.2, y = 0.5, height = 0.7, width = 0.5, just = c("left", "centre"), name = "legend.key")

pushViewport(vp1.1)
# grid.rect()
xy <- kde2d(x = diamonds$carat, y = diamonds$price, n = 100)
xy.tr <- con2tr(xy)
offset <- max(xy.tr$z) * 0.01
z.range <- seq(min(xy.tr$z), max(xy.tr$z) + 50 * offset, offset)

key <- draw.colorkey(key = list(col = c("white", rev(clrs.hcl(10000))), at = z.range), draw = TRUE)

seekViewport("legend.vp")
# grid.rect()
vp1.2 <- viewport(x = 1, y = 0.5, height = 1, width = 0.3, just = c("right", "centre"), name = "legend.text", angle = 0)

pushViewport(vp1.2)
# grid.rect()
grid.text("estimated point density", x = 0, y = 0.5, rot = 270, just = c("centre", "bottom"))

upViewport(3)

dev.off()

# 5. Spatial data in R

# In order to use R for spatial data we need to prepare the computer environment so that we can make use of some spatial 
# libraries (as in computer libraries, not R libraries). In particular, we need to install and set up the so-called 
# geographical data abstraction library - gdal. This is the backbone for any analysis that involves geographical 
# coordinates (i.e. projected spatial data). I cannot go into any detail here, but without these libraries installed 
# on your machine, you will not be able to do any sort of geographical analysis.

# In case you're on a windows machine, the easiest solution (in my opinion) is to install OSGEO4W, a bundle of various 
# spatial tools and programs including gdal. In case you are asked during the installation progress whether to include 
# the gdal libraries in the windows path, click yes (or mark the respective option). In case you are not asked, you have 
# to set this yourself. Here's a little tutorial how to set the global windows path to gdal installation. This is necessary 
# so that windows always knows where to find gdal in case any program (like R) executes a gdal call.

# Then, the R binding to the gdal libraries, called rgdal, should install just like any other package. The other package 
# that we will need to install for any spatial task in R is sp (for spatial). This is the centre of the spatial universe 
# in R and any other package for spatial analysis is dependent on it. A final package that we will need here is raster 
# which is, as the name already implies, focussed towards the spatial analysis of raster data.

# 5.1. Polygon data

# As an example, let's try to produce a graphic that uses colour to represent a certain continuous variable mapped to each 
# of the federal states of Germany. In our case here, we create a random variable based on a poisson distribution and assume 
# it represents the number of breweries in each German federal state.

# The spatial data (polygons) for German federal states can be downloaded from the gadm project site or with aid of the 
# raster package they can be downloaded straight into our R session using getData().

# For Germany, level1 is the data set that provides the outlines of the German federal states (level0 is country outline, 
# level2 is 'Bezirke'). The neat thing about this online data base is that all data is provided in several formats including 
# R spatial data format. This means that we can use this straight away in R and no conversion is necessary.

# As we have already seen how to use grid to arrange multiple plots on one page, we will do this now with spatial 
# data - maps. In order to draw multiple plots we will obviously need several variables to be displayed. Therefore, in 
# addition to the number of breweries in each state, we will create some additional variables such as the number of 
# micro-/corporate breweries and the total amount of beer they sell in a year - in the arbitrary unit fantastiliters ;-) .

# Here we go.

# load the necessary spatial packages
library(sp)
library(raster)
library(fields) # only needed for the hideous tim.colors() palette

# load the German federal state polygons
gadm <- getData('GADM', country = 'DEU', level = 1)

# load('/media/windows/tappelhans/uni/marburg/lehre/2013/ss/Sp-VpPh/data/DEU_adm1.RData')

# create the random variables
set.seed(12345)
breweries <- rpois(16, 3) * 100
micro <- breweries * rnorm(16, 0.4, 0.1)
corporate <- breweries - micro
micro.fl <- rnorm(16, 100000L, 20000L)
corporate.fl <- rnorm(16, 100000L, 10000L)

# calculate global minima and maxima to standardise the colors 
# in each plot. We need to add a little offset at the upper end!
brew.min <- range(breweries)[1]
brew.max <- range(breweries)[2] + 10
litres.min <- min(micro.fl, corporate.fl)
litres.max <- max(micro.fl, corporate.fl) + 1000

# German language hick-ups need to be resolved
enamessp <- gsub("ü", "ue", gadm@data$NAME_1)
gadm@data$NAME_1 <- enamessp

# insert the newly created brewery variables into the spatial data frame
gadm$breweries <- breweries
gadm$micro <- micro
gadm$corp <- corporate
gadm$micro.fl <- micro.fl
gadm$corp.fl <- corporate.fl

# create colour palettes for the plots
clrs.breweries <- colorRampPalette(brewer.pal(9, "YlGnBu"))
clrs.litres <- colorRampPalette(brewer.pal(9, "YlOrBr"))

# create the plot objects with fixed ranges for the z variable (at = ...)
p.total <- spplot(gadm, zcol = "breweries", 
                  main = "Total number of breweries", 
                  col.regions = clrs.breweries(1000),
                  at = seq(brew.min, brew.max, 10),
                  par.settings = list(axis.line = list(col = 0)),
                  colorkey = list(space = "top", width = 1, height = 0.75))

p.micro <- spplot(gadm, zcol = "micro", 
                  main = "Number of micro-breweries", 
                  col.regions = clrs.breweries(1000),
                  at = seq(brew.min, brew.max, 10)) 

p.corpo <- spplot(gadm, zcol = "corp", 
                  main = "Number of corporate breweries", 
                  col.regions = clrs.breweries(1000),
                  at = seq(brew.min, brew.max, 10))

p.micro.fl  <- spplot(gadm, zcol = "micro.fl", 
                      main = "Micro-breweries production (fantastilitres)", 
                      col.regions = clrs.litres(1000),
                      at = seq(litres.min, litres.max, 1000))

p.corpo.fl <- spplot(gadm, zcol = "corp.fl", 
                     main = "Corporate breweries production (fantastilitres)", 
                     col.regions = clrs.litres(1000),
                     at = seq(litres.min, litres.max, 1000))

# Now we can set up our plotting device to plot all five plots on the one device. This is done by defining a viewport for 
# all of them. The first viewport we create will be the upper left viewport. It will be half the height and a quarter the 
3 width of the complete plotting device (called the root viewport). It will be drawn at x = 0 and y = 1 and will be 
# left-aligned horizontally and top-aligned vertically. Once it is set up, we need to navigate to this viewport by pushing 
# into it (pushviewport). Once we are 'in' the viewport, we can draw our plot.

# NOTE: there are two things of importance here (I know this is repetition, but didactically, repetition is not such a bad 
# thing)

# the lattice plot objects need to be printed using print(latticeObject)
# we need to set newpage = FALSE so that the previously drawn elements are not deleted

# Then we create another viewport for the next plot and so on.

# Here's the code:

# grid.newpage() clears whatever has been drawn on the device before
grid.newpage()

vp1 <- viewport(x = 0, y = 1, 
height = 0.5, width = 0.25,
just = c("left", "top"),
name = "upper left")
pushViewport(vp1)
print(p.micro, newpage = FALSE)

upViewport(1)

vp2 <- viewport(x = 0, y = 0, 
height = 0.5, width = 0.25,
just = c("left", "bottom"),
name = "lower left")
pushViewport(vp2)
print(p.micro.fl, newpage = FALSE) 

upViewport(1)

vp3 <- viewport(x = 0.25, y = 0, 
height = 1, width = 0.5,
just = c("left", "bottom"),
name = "centre")
pushViewport(vp3)
print(p.total, newpage = FALSE) 

upViewport(1)

vp4 <- viewport(x = 1, y = 1, 
height = 0.5, width = 0.25,
just = c("right", "top"),
name = "upper right")
pushViewport(vp4)
print(p.corpo, newpage = FALSE) 

upViewport(1)
vp5 <- viewport(x = 1, y = 0, 
height = 0.5, width = 0.25,
just = c("right", "bottom"),
name = "lower right")
pushViewport(vp5)
print(p.corpo.fl, newpage = FALSE) 

upViewport(1)

# Now let's do something similar with raster data.

# 5.2. Raster data

# To highlight the use of the raster package, we will use some temperature data for Germany. This can, again, be 
# downloaded straight into the current session with getData().

# The steps we are taking below to produce maps of average August temperatures in Germany are as follows:
  
# we get the global long-term mean temperatures from worldclim (these are so-called raster stacks with 12 layers, 
#  one for each month of the year - see below)
# we crop the global data set to the extent of Germany using the gadm data set from above
# we need to divide the temperature (which are in degrees celcius) by a factor of 10 (in order to save space, these 
#  data are stored as integers and hence multiplied by 10)
# we download the outline of Germany from gadm

# to highlight the problems related to colour representation we already touched upon in section 2.1.1. we plot 5 different 
# representations of the same data on one page (similar to the polygon data from section 5.1.)

Tmean <- getData('worldclim', var = 'tmean', res = 5)
ext <- extent(gadm)

Tmean.ger <- crop(Tmean, ext)

# worldclim stores temepratures as degrees celcius * 10
Tmean.ger <- Tmean.ger / 10

Tmean.ger

# The default way of plotting raster data using spplot is, again, not so nice - see lattice defaults.

Tplot <- spplot(Tmean.ger, zcol = "tmean8")
print(Tplot)

# So, let's manipulate the colours a little.

ger <- getData('GADM', country = 'DEU', level = 0)

Tplot.spec <- spplot(Tmean.ger, zcol = "tmean8", 
                     col.regions = clrs.spec(1001), 
                     cuts = 1000, 
                     colorkey = list(space = "top"),
                     main = "Average August temperatures in Germany") +
  as.layer(spplot(ger, zcol = "NAME_ISO", col.regions = "transparent"))

Tplot.rnbw <- spplot(Tmean.ger, zcol = "tmean8", 
                     col.regions = tim.colors(1001), 
                     cuts = 1000) +
  as.layer(spplot(ger, zcol = "NAME_ISO", col.regions = "transparent"))

clrs.hcl2 <- function(n) {
  hcl(h = seq(270, 0, length.out = n), 
      c = 60, l = seq(90, 40, length.out = n), 
      fixup = TRUE)
}

Tplot.hcl <- spplot(Tmean.ger, zcol = "tmean8", 
                    col.regions = clrs.hcl2(1001), 
                    cuts = 1000) +
  as.layer(spplot(ger, zcol = "NAME_ISO", col.regions = "transparent"))

clrs.ylorrd <- colorRampPalette(brewer.pal(9, "YlOrRd"))

Tplot.ylorrd <- spplot(Tmean.ger, zcol = "tmean8", 
                       col.regions = clrs.ylorrd(1001), 
                       cuts = 1000) +
  as.layer(spplot(ger, zcol = "NAME_ISO", col.regions = "transparent"))

clrs.grey <- colorRampPalette(brewer.pal(9, "Greys"))

Tplot.greys <- spplot(Tmean.ger, zcol = "tmean8", 
                      col.regions = clrs.grey(1001), 
                      cuts = 1000) +
  as.layer(spplot(ger, zcol = "NAME_ISO", col.regions = "transparent"))

# Now, we can plot the different versions and compare the usefulness of the different colour palettes.

grid.newpage()


vp1 <- viewport(x = 0, y = 1, 
                height = 0.5, width = 0.25,
                just = c("left", "top"),
                name = "upper left")
pushViewport(vp1)
print(Tplot.rnbw, newpage = FALSE)

upViewport(1)

vp2 <- viewport(x = 0, y = 0, 
                height = 0.5, width = 0.25,
                just = c("left", "bottom"),
                name = "lower left")
pushViewport(vp2)
print(Tplot.ylorrd, newpage = FALSE) 

upViewport(1)

vp3 <- viewport(x = 0.25, y = 0, 
                height = 1, width = 0.5,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp3)
print(Tplot.spec, newpage = FALSE) 

upViewport(1)

vp4 <- viewport(x = 1, y = 1, 
                height = 0.5, width = 0.25,
                just = c("right", "top"),
                name = "upper right")
pushViewport(vp4)
print(Tplot.hcl, newpage = FALSE) 

upViewport(1)
vp5 <- viewport(x = 1, y = 0, 
                height = 0.5, width = 0.25,
                just = c("right", "bottom"),
                name = "lower right")
pushViewport(vp5)
print(Tplot.greys, newpage = FALSE) 

upViewport(1)

###############################################################################
## Caret

library(caret)
library(mlbench)
library(klaR)
library(pROC)
library(MASS)

data(Sonar)
set.seed(107)

# To demonstrate this function, the Sonar data from the mlbench package will be used.
# The Sonar data consist of 208 data points collected on 60 predictors. The goal 
# is to predict the two classes (M for metal cylinder or R for rock).
# First, we split the data into two groups: a training set and a test set. To do this, 
# the createDataPartition function is used:

inTrain <- createDataPartition(y = Sonar$Class,
                               # the outcome data are needed
                               p = .75,
                               # The percentage of data in the training set
                               list = FALSE)
# The format of the results
# The output is a set of integers for the rows of Sonar
# that belong in the training set.
str(inTrain)

# Bt default, createDataPartition does a stratified random split of the data. 
# To partition the data:

training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]
nrow(training)
# [1] 157
nrow(testing)
# [1] 51

# To tune a model using Algorithm 1, the train function can be used. More details 
# on this function can be found at:
# http://caret.r-forge.r-project.org/training.html
# Here, a partial least squares discriminant analysis (PLSDA) model will be tuned 
# over the number of PLS components that should be retained. The most basic 
# syntax to do this is:

plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                # Center and scale the predictors for the training
                # set and all future samples.
                preProc = c("center", "scale"))

# However, we would probably like to customize it in a few ways:
# . expand the set of PLS models that the function evaluates. By default, the 
#   function will tune over three values of each tuning parameter.
# . the type of resampling used. The simple bootstrap is used by default. We 
#   will have the function use three repeats of 10-fold cross-validation.
# . the methods for measuring performance. If unspecified, overall accuracy and 
#   the Kappa statistic are computed. For regression models, root mean squared 
#   error and R 2 are computed.
# Here, the function will be altered to estimate the area under the ROC curve, 
# the sensitivity and specificity
# To change the candidate values of the tuning parameter, either of the 
# tuneLength or tuneGrid arguments can be used. The train function can generate 
# a candidate set of parameter values and the tuneLength argument controls how 
# many are evaluated. In the case of PLS, the function uses a sequence of 
# integers from 1 to tuneLength . If we want to evaluate all integers between 
# 1 and 15, setting tuneLength = 15 would achieve this. The tuneGrid argument 
# is used when specific values are desired. A data frame is used where each 
# row is a tuning parameter setting and each column is a tuning parameter. 
# An example is used below to illustrate this.
# The syntax for the model would then be:

plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15,
                preProc = c("center", "scale"))

# To modify the resampling method, a trainControl function is used. The option 
# method controls the type of resampling and defaults to "boot" . Another 
# method, "repeatedcv" , is used to specify repeated K-fold cross-validation 
# (and the argument repeats controls the number of repetitions). K is 
# controlled by the number argument and defaults to 10. The new syntax is then:

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3)
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                preProc = c("center", "scale"))

# Finally, to choose different measures of performance, additional arguments 
# are given to trainControl. The summaryFunction argument is used to pas in a 
# function that takes the observed and predicted values and estimate some 
# measure of performance. Two such functions are already included in the 
# package: defaultSummary and twoClassSummary . The latter will compute 
# measures specific to two-class problems, such as the area under the ROC 
# curve, the sensitivity and specificity. Since the ROC curve is based on the 
# predicted class probabilities (which are not computed automatically), another
# option is required. The classProbs = TRUE option is used to include these 
# calculations.
# Lastly, the function will pick the tuning parameters associated with the best 
# results. Since we are using custom performance measures, the criterion that 
# should be optimized must also be specified.
# In the call to train, we can use metric = "ROC" to do this.
# The final model fit would then be:

ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale"))
plsFit

# In this output the grid of results are the average resampled estimates of 
# performance. The note at the bottom tells the user that PLS components were 
# found to be optimal. Based on this value, a final PLS model is fit to the 
# whole data set using this specification and this is the model that is used 
# to predict future samples.
# The package has several functions for visualizing the results. One method 
# for doing this is the plot function for train objects. The command 
# plot(plsFit) produced the results seen in Figure 1 and shows the relationship 
# between the resampled performance values and the number of PLS components.
# To predict new samples, predict.train can be used. For classification models, 
# the default behavior is to calculated the predicted class. Using the option 
# type = "prob" can be used to compute class probabilities from the model. 
# For example:

plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

# caret contains a function to compute the confusion matrix and associated 
# statistics for the model fit:

confusionMatrix(data = plsClasses, testing$Class)

# To fit an another model to the data, train can be invoked with minimal 
# changes. Lists of models available can be found at:
# http://caret.r-forge.r-project.org/modelList.html
# http://caret.r-forge.r-project.org/bytag.html
# For example, to fit a regularized discriminant model to these data, the 
# following syntax can be used:

# To illustrate, a custom grid is used
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ .,
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")
rdaFit

rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

# How do these models compare in terms of their resampling results? The 
# resamples function can be used to collect, summarize and contrast the 
# resampling results. Since the random number seeds were initialized to the 
# same value prior to calling train , the same folds were used for each model.
# To assemble them:

resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

# There are several functions to visualize these results. For example, a 
# Bland-Altman type plot can be created using xyplot(resamps, what = "BlandAltman") 
# (see Figure 2). The results look similar. Since, for each resample, there are 
# paired results a paired t-test can be used to assess whether there is a difference 
# in the average resampled area under the ROC curve. The diff.resamples function
# can be used to compute this:

diffs <- diff(resamps)
summary(diffs)

xyplot(resamps, what = "BlandAltman")

# Based on this analysis, the difference between the models is -0.006 ROC units 
# (the RDA model is slightly higher) and the two-sided p-value for this difference 
# is 0.67846.

###############################################################################
# Citar R

citation() 

# Citar una libreria
citation("BMA")

###############################################################################

##  r_contours_for_google_earth
## https://github.com/gimoya/theBioBucket-Archives/blob/master/R/r_contours_for_google_earth.R

library(rgdal)
library(raster)
library(maptools)
library(rgeos)
library(RCurl)

# dem in hgt-format downloaded from http://www.viewfinderpanoramas.org/dem3.html#alps
# as done here:
# http://thebiobucket.blogspot.co.at/2013/06/use-r-to-bulk-download-digital.html

require(XML)

# dir.create("D:/GIS_DataBase/DEM/")
# setwd("D:/GIS_DataBase/DEM/")

doc <- htmlParse("http://www.viewfinderpanoramas.org/dem3.html#alps")
urls <- paste0("http://www.viewfinderpanoramas.org", xpathSApply(doc,'//*/a[contains(@href,"/dem1/N4")]/@href'))
names <- gsub(".*dem1/(\\w+\\.zip)", "\\1", urls)

for (i in 1:length(urls)) download.file(urls[i], names[i]) 

# unzip all files in dir and delete them afterwards
sapply(list.files(pattern = "*.zip"), unzip)
unlink(list.files(pattern = "*.zip"))

# splitted into tiles for easier handling and saved to tiff as done here:
# http://thebiobucket.blogspot.co.at/2014/03/use-gdal-from-r-console-to-split-raster.html

## get filesnames (assuming the datasets were downloaded already. 
## please see http://thebiobucket.blogspot.co.at/2013/06/use-r-to-bulk-download-digital.html 
## on how to download high-resolution DEMs)

# setwd("D:/GIS_DataBase/DEM")
files <- dir(pattern = ".hgt")

## function for single file processing mind to replace the PATH to gdalinfo.exe!
## s = division applied to each side of raster, i.e. s = 2 gives 4 tiles, 3 gives 9, etc.
split_raster <- function(file, s = 2) {
  
  filename <- gsub(".hgt", "", file)
  gdalinfo_str <- paste0("\"C:/Program Files/QGIS Dufour/bin/gdalinfo.exe\" ", file)
  
  # pick size of each side
  x <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system(gdalinfo_str, intern = T)[3], ", "))))[1]
  y <- as.numeric(gsub("[^0-9]", "", unlist(strsplit(system(gdalinfo_str, intern = T)[3], ", "))))[2]
  
  # t is nr. of iterations per side
  t <- s - 1
  for (i in 0:t) {
    for (j in 0:t) {
      # [-srcwin xoff yoff xsize ysize] src_dataset dst_dataset
      srcwin_str <- paste("-srcwin ", i * x/s, j * y/s, x/s, y/s)
      gdal_str <- paste0("\"C:/Program Files/QGIS Dufour/bin/gdal_translate.exe\" ", srcwin_str, " ", "\"", file, "\" ", "\"", filename, "_", i, "_", j, ".tif\"")
      system(gdal_str)
    }
  }
}

## process all files and save to same directory
mapply(split_raster, files, 2) 

setwd("E:/Varios/R/Archivos/varios/DEM")
(filenames <- gsub(".tif", "", dir(pattern = ".tif")))

## make folder for output and set directory
# dir.create('E:/Varios/R/Archivos/varios/DEM/contours')
setwd("E:/Varios/R/Archivos/varios/DEM/contours")

## funtion make_kml_contours
## arguments
## step: altitude inbetween contours, starting at 0 m
## simplify: 1-0, 1 is no generalization, 0 is straight line
## ftp: optional ftp uload

make_kml_contours <- function(filename, step = 100, simplify = 0.001, ftp = F)
  
{
  ## coerce into SpatialGridDataFrame
  dem <- readGDAL(paste0("E:/Varios/R/Archivos/varios/DEM/", filename, ".tif"))
  
  ## make image object for contourLines function
  im <- as.image.SpatialGridDataFrame(dem)
  # check: summary(im$z)
  cl <- contourLines(im, levels = seq(0, max(im$z), step))
  
  ## back convert to SpatialLinesDataFrame
  SLDF <- ContourLines2SLDF(cl)
  proj4string(SLDF) <- CRS("+proj=longlat +datum=WGS84")
  
  ## simplify
  simplSLDF <- gSimplify(SLDF, tol = simplify)
  
  ## view results
  # image(dem, col = gray.colors(20))
  # plot(simplSLDF, add = T)
  
  ## convert simplified SLDF to KML (btw, that's how to extract IDs unlist(lapply(slot(simplSLDF, 'lines'), function(x) slot(x, 'ID'))) )
  out <- sapply(slot(simplSLDF, "lines"), function(x) {
    # get meter level, by picking from sequence by ID: ID = 1 -> 1*step m, ID = 2, 2*step m, etc.
    m <- seq(0, max(im$z), step)[as.numeric(gsub("C_", "", slot(x, "ID")))]
    # make thicker lines at 100 and 500 m Isolines
    kmlLine(x, name = m, description = paste0(m, "m-Isoline"), col = "#FCCD47", lwd = ifelse(m%%100 == 0, ifelse(m%%500, 2, 1.25), 0.75))
  })
  
  # write KML
  tf <- tempfile()
  kmlFile <- file(tf, "w")
  cat(kmlLine(kmlname = "Contour Lines", kmldescription = "<i>Contour lines by Kay Cichini, see <a href=\"htp://gimoya.bplaced.net/terrain-overlays.blogspot.co.at\">Terrain-Overlays</a> for details</i>")$header,
      file = kmlFile, sep = "\n")
  cat(unlist(out["style", ]), file = kmlFile, sep = "\n")
  cat(unlist(out["content", ]), file = kmlFile, sep = "\n")
  cat(kmlLine()$footer, file = kmlFile, sep = "\n")
  close(kmlFile)
  
  kmlName <- paste0("CONTOURS_", filename, ".kml")
  file.rename(tf, kmlName)
  if (ftp == T) ftpUpload(kmlName, paste0('ftp://gimoya:password@gimoya.bplaced.net/Terrain-Overlays/downloads/', kmlName))
}

for (filename in filenames[2:136])
{
  tryCatch(make_kml_contours(filename, step = 25, simplify = 0.0001, ftp = T),
           error = function(e) message(paste0("\n..something happend with dataset ", filename, ":\n", e)))
  cat("File ", filename, " done!..\n")
}

###############################################################################
## Forecasting weekly data
## http://www.statsblogs.com/2014/03/04/forecasting-weekly-data/

# Regression with ARIMA errors

# The simplest approach is a regression with ARIMA errors. Here is an example using 
# weekly data on US finished motor gasoline products supplied (in thousands of 
# barrels per day) from February 1991 to May 2005. An updated version of the data 
# is available from the EIA website. I select the number of Fourier terms by 
# minimizing the AICc. The order of the ARIMA model is also selected by minimizing 
# the AICc, although that is done within the auto.arima() function.

library(forecast)
gas <- ts(read.csv("http://robjhyndman.com/data/gasoline.csv", header=FALSE)[,1], 
          freq=365.25/7, start=1991+31/7/365.25)
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(gas, xreg=fourier(gas, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
fc <- forecast(bestfit, xreg=fourierf(gas, K=12, h=104))
plot(fc)

# There are 24 parameters to capture the seasonality which is rather a lot, but 
# apparently required according to the AIC selection. (BIC would have given fewer.) 
# The total number of degrees of freedom is 31 (the other seven coming from the 6 
# ARMA parameters and the drift parameter).

# TBATS

# An alternative approach is the TBATS model introduced by De Livera et al (JASA, 2011). 
# This uses a state space model that is a generalization of those underpinning 
# exponential smoothing. It also allows for automatic Box-Cox transformation and 
# ARMA errors. The modelling algorithm is entirely automated:
  
gastbats <- tbats(gas)
fc2 <- forecast(gastbats, h=104)
plot(fc2, ylab="thousands of barrels per day")

###############################################################################
## Warholing Grace With Clara
## http://www.r-bloggers.com/warholing-grace-with-clara/

library("biOps")
library("abind")
library("reshape")
library("reshape2")
library("cluster")
library("sp")

# Initialization

setwd("E:/Varios/R/Archivos/varios")
# Load jpf file
x     <- readJpeg("./936full-grace-kelly.jpg")
plot(x)

# Data

data <- merge(merge(melt(x[, , 1]), melt(x[, , 2]), by=c("X1", "X2")), 
              melt(x[, , 3]), by=c("X1", "X2"))
colnames(data) <- c("X1", "X2", "r", "g", "b")

# Clustering

colors <- 5
clarax <- clara(data[,3:5], colors)
datacl   <- data.frame(data, clarax$cluster)
claradf2 <- as.data.frame(clarax$medoids)
claradf2$id <- as.numeric(rownames(claradf2))
colnames(claradf2) <- c("r", "g", "b", "clarax.cluster")
claradf <- merge(datacl, claradf2, by=c("clarax.cluster"))
colnames(claradf) <-c("clarax.cluster", "X1", "X2", "r.x", "g.x", "b.x", "r.y", "g.y", "b.y")
datac <- claradf[do.call("order", claradf[c("X1", "X2")]), ]
x1 <- acast(datac[,c(2,3,7)], X1~X2, value.var="r.y")
x2 <- acast(datac[,c(2,3,8)], X1~X2, value.var="g.y")
x3 <- acast(datac[,c(2,3,9)], X1~X2, value.var="b.y")
warhol <- do.call(abind, c(list(x1,x2,x3), along = 3))
plot(imagedata(warhol))
writeJpeg(paste("Warholing", as.character(colors), ".jpg", sep=""), imagedata(warhol))

###############################################################################

library("biOps")
library("cluster")

# leo una foto usando readJpeg de biOps
# el objeto devuelto es un array mxnx3 dimensional
# la última dimensión es el rgb de cada pixel

tmp <- tempfile()
download.file("http://blog.guiasenior.com/images/Retrato_Garber.jpg", tmp)
x <- readJpeg(tmp)

# si quieres mostrar la foto como un gráfico...
#plot(x)

# convertimos el array 3D nxmx3 en uno 2D (nm)x3
# luego buscamos 5 clústers
# esencialmente, buscamos 7 "píxels representativos"
d <- dim(x)
clarax <- clara(array(x, dim = c(d[1] * d[2], d[3])), 7)

# reemplazamos cada rgb de cada cluster por su 
# "píxel representativo" (medioide) correspondiente
rgb.clusters <- clarax$medoids[clarax$cluster,]

# convertimos la matriz resultante en un array 3D 
# (invirtiendo la transformación anterior)
# y representamos gráficamente
plot(imagedata(array(rgb.clusters, dim = d)))

###############################################################################

library("biOps")
library("cluster")

# leo una foto usando readJpeg de biOps
# el objeto devuelto es un array mxnx3 dimensional
# la última dimensión es el rgb de cada pixel

tmp <- tempfile()
download.file("http://blog.guiasenior.com/images/Retrato_Garber.jpg", tmp)
x <- readJpeg(tmp)

# si quieres mostrar la foto como un gráfico...
#plot(x)

# convertimos el array 3D nxmx3 en uno 2D (nm)x3
# luego buscamos 5 clústers
# esencialmente, buscamos 7 "píxels representativos"
d <- dim(x)
clarax <- clara(array(x, dim = c(d[1] * d[2], d[3])), 7)

# reemplazamos cada rgb de cada cluster por su 
# "píxel representativo" (medioide) correspondiente
rgb.clusters <- clarax$medoids[clarax$cluster,]

# convertimos la matriz resultante en un array 3D 
# (invirtiendo la transformación anterior)
# y representamos gráficamente
plot(imagedata(array(rgb.clusters, dim = d)))


#########################################################################################
#   An R function to make a personalized map of people you follow and who follow you on twitter. 
#   R functions Copyright (C) 2011 Jeff Leek (jtleek@gmail.com), and the Simply Statistics Blog
#   (http://simplystatistics.tumblr.com, http://twitter.com/simplystats)
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details, see <http://www.gnu.org/licenses/>.
#
#
#   These functions depend on the packages: twitteR, maps, geosphere, and RColorBrewer. It will
#   attempt to install them if they are not installed when you source this function. Care
#   should be used when using this function since the twitteR API has rate limiting in place.
#   If you have a large number of followers, or run the function many times, you may be
#   rate limited. 
#
#
#   How to use: 
#       # Source the function
#       source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")
#
#      # Make your twittermap
#      twitterMap("simplystats")
#
#      #If your location can't be found or latitude longitude can't be calculated
#      #choose a bigger city near you. The list of cities used by twitterMap
#      #can be found like so:
#      data(world.cities)
#      grep("Baltimore",world.cities[,1])
#
#      # Then make the map using that big city
#      twitterMap("simplystats",userLocation="Baltimore")
#   
#      #If you want both your followers and people you follow in a plot you can do:
#      twitterMap("simplystats",plotType="both")
#      
########################################################################################
getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

pckg = try(require(twitteR))
if(!pckg) {
  cat("Installing 'twitteR' from CRAN\n")
  getPckg("twitteR")
  require("twitteR")
}

pckg = try(require(maps))
if(!pckg) {
  cat("Installing 'maps' from CRAN\n")
  getPckg("maps")
  require("maps")
}

pckg = try(require(geosphere))
if(!pckg) {
  cat("Installing 'geosphere' from CRAN\n")
  getPckg("geosphere")
  require("geosphere")
}

pckg = try(require(RColorBrewer))
if(!pckg) {
  cat("Installing 'RColorBrewer' from CRAN\n")
  getPckg("RColorBrewer")
  require("RColorBrewer")
}

twitterMap <- function(userName, userLocation=NULL, fileName="twitterMap.pdf", 
                       nMax = 1000, plotType=c("followers", "both", 
                                               "following")){
  
  # Get location data
  cat("Getting data from Twitter, this may take a moment.\n")
  tmp = getUser(userName)
  if(is.null(userLocation)){
    userLocation = location(tmp)
    userLocation = trim(userLocation)
    if(nchar(userLocation) < 2){stop("We can not find your location from Twitter")}
  }
  
  followers = tmp$getFollowers(n=nMax)
  followersLocation = sapply(followers, function(x){location(x)})
  following = tmp$getFriends(n=nMax)
  followingLocation = sapply(following, function(x){location(x)})
  
  
  # Load the geographic data
  data(world.cities)
  data(us.cities)
  data(canada.cities)
  
  # Find the latitude and longitude of the user
  cat("Getting geographic (latitude/longitude) of Twitter users.\n")
  userLL <- findLatLon(userLocation)$latlon
  if(any(is.na(userLL))){stop("We can't find the latitude and longitude of your location from Twitter")}
  
  
  # Find the latitude and longitude of each of the followers/following
  # and calcualte the distance to the user
  
  followersLL = matrix(NA,nrow=length(followers),ncol=4)
  followingLL = matrix(NA,nrow=length(following),ncol=4)
  
  for(i in 1:length(followers)){
    if(length(followersLocation[[i]]) > 0){
      tmpLL = findLatLon(trim(followersLocation[[i]]))
      if(any(!is.na(tmpLL$latlon))){
        followersLL[i,] = c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
      }
    }
  }
  
  for(i in 1:length(following)){
    if(length(followingLocation[[i]]) > 0){
      tmpLL = findLatLon(trim(followingLocation[[i]]))
      if(any(!is.na(tmpLL$latlon))){
        followingLL[i,] =  c(unlist(tmpLL$latlon),distCosine(userLL,tmpLL$latlon),unlist(tmpLL$cont))
      }
    }
  }
  
  followingLL = followingLL[order(-followingLL[,3]),]
  followersLL = followersLL[order(-followersLL[,3]),]
  
  followingLL = followingLL[!is.na(followingLL[,1]),]
  followersLL = followersLL[!is.na(followersLL[,1]),]
  
  
  cat("Plotting results.\n")
  # Set up the colors
  cols = brewer.pal(7,"Set2")
  
  # Both followers and following
  if(plotType=="both"){
    pdf(fileName,height=12,width=10)
    data(worldMapEnv)
    par(mfrow=c(2,1),mar=rep(0,4))
    map('world',col="#191919",bg="black",fill=T,mar=rep(0,4),border=0)
    
    mtext(paste("@",userName," Follower Map",sep=""),col="lightgrey")
    nFollowers = dim(followersLL)[1]
    for(i in 1:nFollowers){
      greatC = getGreatCircle(userLL,followersLL[i,1:2])
      lines(greatC,col=cols[followersLL[i,4]],lwd=0.8)
    }
    
    legend(-180,0,legend = c(paste("Asia",sum(followersLL[,4]==1)),paste("Africa",sum(followersLL[,4]==2)),paste("N. America",sum(followersLL[,4]==3)),paste("S. America",sum(followersLL[,4]==4)),paste("Australia/N.Z.",sum(followersLL[,4]==5)),paste("Europe",sum(followersLL[,4]==6))),text.col=cols[1:6],bg="black",cex=0.75)
    
    
    map('world',col="#191919",bg="black",fill=T,mar=rep(0,4),border=0)
    mtext(paste("@",userName," Following Map",sep=""),col="lightgrey")
    nFollowing = dim(followingLL)[1]
    for(i in 1:nFollowing){
      greatC = getGreatCircle(userLL,followingLL[i,1:2])
      lines(greatC,col=cols[followingLL[i,4]],lwd=0.8)
    }
    
    legend(-180,0,legend = c(paste("Asia",sum(followingLL[,4]==1)),paste("Africa",sum(followingLL[,4]==2)),paste("N. America",sum(followingLL[,4]==3)),paste("S. America",sum(followingLL[,4]==4)),paste("Australia/N.Z.",sum(followingLL[,4]==5)),paste("Europe",sum(followingLL[,4]==6))),text.col=cols[1:6],bg="black",cex=0.75)
    
    mtext("Created by @simplystats twitterMap",side=1,adj=1,cex=0.8,col="grey")
    dev.off()
  }
  
  ## Just followers
  if(plotType=="followers"){
    pdf(fileName,height=6,width=10)
    data(worldMapEnv)
    map('world',col="#191919",bg="black",fill=T,mar=rep(0,4),border=0)
    
    mtext(paste("@",userName," Follower Map",sep=""),col="lightgrey")
    nFollowers = dim(followersLL)[1]
    for(i in 1:nFollowers){
      greatC = getGreatCircle(userLL,followersLL[i,1:2])
      lines(greatC,col=cols[followersLL[i,4]],lwd=0.8)
    }
    
    legend(-180,0,legend = c(paste("Asia",sum(followersLL[,4]==1)),paste("Africa",sum(followersLL[,4]==2)),paste("N. America",sum(followersLL[,4]==3)),paste("S. America",sum(followersLL[,4]==4)),paste("Australia/N.Z.",sum(followersLL[,4]==5)),paste("Europe",sum(followersLL[,4]==6))),text.col=cols[1:6],bg="black",cex=0.75)
    mtext("Created by @simplystats twitterMap",side=1,adj=1,cex=0.8,col="grey")
    dev.off()
    
  }
  
  ## Just following
  if(plotType=="following"){
    pdf(fileName,height=6,width=10)
    data(worldMapEnv)
    map('world',col="#191919",bg="black",fill=T,mar=rep(0,4),border=0)
    mtext(paste("@",userName," Following Map",sep=""),col="lightgrey")
    nFollowing = dim(followingLL)[1]
    for(i in 1:nFollowing){
      greatC = getGreatCircle(userLL,followingLL[i,1:2])
      lines(greatC,col=cols[followingLL[i,4]],lwd=0.8)
    }
    
    legend(-180,0,legend = c(paste("Asia",sum(followingLL[,4]==1)),paste("Africa",sum(followingLL[,4]==2)),paste("N. America",sum(followingLL[,4]==3)),paste("S. America",sum(followingLL[,4]==4)),paste("Australia/N.Z.",sum(followingLL[,4]==5)),paste("Europe",sum(followingLL[,4]==6))),text.col=cols[1:6],bg="black",cex=0.75)
    
    mtext("Created by @simplystats twitterMap",side=1,adj=1,cex=0.8,col="grey")
    dev.off()
    
  }
  
}


findLatLon <- function(loc){
  latlon = NA
  cont = NA
  
  # Asia = 1, Africa = 2, North America = 3, South America = 4, Australia/New Zealand = 5, Europe = 6
  continents = matrix(NA,nrow=length(unique(world.cities[,2])),ncol=2)
  continents[,1] = unique(world.cities[,2])
  continents[1:10,2] = c(1,1,1,2,1,1,1,1,1,1)
  continents[11:20,2]= c(1,1,2,1,1,2,1,2,2,2)
  continents[21:30,2] = c(2,1,6,6,6,6,6,6,6,6)
  continents[31:40,2] = c(6,6,6,6,2,4,4,1,2,1)
  continents[41:50,2] = c(4,6,1,4,6,1,3,1,6,6)
  continents[51:60,2] = c(3,2,4,2,6,1,6,1,3,2)
  continents[61:70,2] = c(1,2,2,2,3,6,3,3,6,6)
  continents[71:80,2] = c(1,1,2,6,3,4,3,4,6,1)
  continents[81:90,2] = c(3,3,3,2,2,6,6,6,6,4)
  continents[91:100,2] = c(2,5,2,2,3,1,1,1,1,1)
  continents[101:110,2] = c(1,2,1,1,1,3,2,5,1,6)
  continents[111:120,2] = c(1,6,1,1,2,6,1,1,6,2)
  continents[121:130,2] = c(6,6,6,1,1,3,4,3,4,2)
  continents[131:140,2] = c(6,6,2,2,1,1,1,4,1,1)
  continents[141:150,2] = c(1,2,2,1,1,1,4,6,6,2)
  continents[151:160,2] = c(4,1,1,1,1,2,4,6,2,2)
  continents[161:170,2] = c(1,2,2,1,6,2,1,1,6,1)
  continents[171:180,2] = c(1,1,1,2,6,2,2,6,1,1)
  continents[181:190,2] = c(2,6,2,1,6,6,3,3,3,3)
  continents[191:200,2] = c(2,2,2,2,3,2,3,2,3,1)
  continents[201:210,2] = c(3,2,2,2,2,2,2,1,6,2)
  continents[211:220,2] = c(1,3,1,6,2,4,3,6,3,4)
  continents[221:230,2] = c(1,1,1,3,2,3,3,6,1,6)
  continents[231:232,2] = c(2,1)
    
  # Get the first element of the location
  # firstElement = strsplit(loc,"[^[:alnum:]]")[[1]][1]
  firstElement = strsplit(loc,",")[[1]][1]
  if(is.na(firstElement)){firstElement="zzzzzzzzz"}
  
  # See if it is a city
  tmp = grep(firstElement,world.cities[, 1], fixed=TRUE)
  tmp2 = grep(firstElement, state.name, fixed=TRUE)
  tmp3 = grep(firstElement, world.cities[, 2], fixed=TRUE)
  
  if(length(tmp) == 1){
    latlon = world.cities[tmp,c(5,4)]
    cont = continents[which(world.cities[tmp, 2]==continents[, 1]), 2]
  }else if(length(tmp) > 1){
    tmpCities = world.cities[tmp,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }else if(length(tmp2) == 1){
    latlon = c(state.center$x[tmp2],state.center$y[tmp2])
    cont = 3
  }else if(length(tmp3) > 0){
    tmpCities = world.cities[tmp3,]
    latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
    cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
  }
  
  return(list(latlon=latlon, cont=as.numeric(cont)))
  
}


getGreatCircle = function(userLL, relationLL){
  tmpCircle = greatCircle(userLL, relationLL)
  start = which.min(abs(tmpCircle[, 1] - userLL[1, 1]))
  end = which.min(abs(tmpCircle[, 1] - relationLL[1]))
  greatC = tmpCircle[start:end, ]
  return(greatC)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


###############################################################################
## Valores repetidos en una tabla

library(plyr)

n <- 100

dat <- data.frame(id = 1:n, a=rpois(n, lambda=3), b=rpois(n, lambda=5))
dat
dat2 <- dlply(dat, .(a, b), function(x) x$id)
dat2

# Otro modo
tabla <- table(dat$a, dat$b)
repetidos <- which(tabla>1)
expand.grid(row.names(tabla), colnames(tabla))[repetidos, ]

class(dat)
class(dat2)

# Otro mas

Lines <- "ID FECHA VALOR
1 01/04/1965 25578
2 01/09/1988 23456
3 01/04/2004 76578
4 01/04/1965 25578
5 01/05/1992 33724
6 01/04/1965 65789
7 01/09/1988 23456
"

dat.in <- read.table(textConnection(Lines), header=T, as.is = TRUE)

dupli.val <- dat.in[duplicated(dat.in[, 2:3]), ]
res.val <- merge(dat.in, dupli.val, by.x=c(2, 3), by.y=c(2, 3))
res.val <- res.val[res.val[, 3]!=res.val[, 4], ]

res.val
###############################################################################


###############################################################################
## caret package webminar

library(caret)
library(C50)
data(churn)

summary(churnTest)
dim(churnTest)
dim(churnTrain)

str(churnTrain)
str(churnTest)

# We want to predict "yes". Churn, not retention.
predictors <- names(churnTrain)[names(churnTrain)!="churn"]

allData <- rbind(churnTrain, churnTest)
set.seed(1)

# A simple, stratified random split is used here:
inTrainingSet <- createDataPartition(allData$churn, p=.57, list=FALSE)
# This functions maintain the chrun proportion

churnTrain <- allData[inTrainingSet, ]
churnTest  <- allData[-inTrainingSet, ]

# preProcess calculates values that can be used to apply to any data set
# (e.g. training, set, unknowns).
# Current methods: centering, scaling, spatial sign transformation, PCA or
# ICA"signal extraction", imputation, Box-Cox transformations and others.

numerics <- c("account_length", "total_day_calls", "total_night_calls")
# Determine means and sd's
procValues <- preProcess(churnTrain[, numerics],
                         method = c("center", "scale", "YeoJohnson"))

procValues
# Use the predict methods to do the adjustments
trainScaled <- predict(procValues, churnTrain[, numerics])
testScaled  <- predict(procValues, churnTest[, numerics])

# Using the gbm Package
# The gbm function in the gbm package can be used to fit the model, then
# predict.gbm and other functions are used to predict and evaluate the
# model.

library(gbm)
# The gbm function does not accept factor response values so we
# will make a copy and modify the outcome variable
forGBM <- churnTrain
# Change yes by 1 and no by 0
forGBM$churn <- ifelse(forGBM$churn == "yes", 1, 0)

gbmFit <- gbm(formula = churn ~ ., # Use all predictors
              distribution = "bernoulli", # For classification
              data = forGBM,
              n.trees = 2000, # 2000 boosting iterations
              interaction.depth = 7, # How many splits in each tree
              shrinkage = 0.01, # learning rate
              verbose = FALSE) # Do not print the details

# Tuning the gbm Model
# foreach resampled data set do
#   Hold-out samples ;
#   foreach combination of tree depth, learning rate and number of trees
#   do
#     Fit the model on the resampled data set;
#     Predict the hold-outs and save results;
#   end
#   Calculate the average AUC ROC across the hold-out sets of predictions
# end

# Determine tuning parameters based on the highest resampled ROC AUC;

# Model Tuning using train
# In a basic call to train, we specify the predictor set, the outcome data
# and the modeling technique (eg. boosting via the gbm package):
gbmTune <- train(x = churnTrain[, predictors],
                 y = churnTrain$churn,
                 method = "gbm")

# or, using the formula interface
gbmTune <- train(churn ~ ., data = churnTrain, method = "gbm")

# Note that train uses the original factor input for classification. For binary
# outcomes, the function models the probability of the first factor level
# (churn for these data).
# A numeric object would indicate we are doing regression.
# One problem is that gbm spits out a lot of information during model fitting.

# By default, train uses the bootstrap for resampling. We'll switch to 5
# repeats of 10-fold cross-validation instead.
# We can then change the type of resampling via the control function.
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5)
gbmTune <- train(churn ~ ., data = churnTrain,
                 method = "gbm",
                 verbose = FALSE,
                 trControl = ctrl)

# Using Different Performance Metrics
# The twoClassSummary function is defined in caret and calculates the
# sensitivity, specificity and ROC AUC. Other custom functions can be used
# (see ?train).
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
gbmTune <- train(churn ~ ., data = churnTrain,
                 method = "gbm",
                 metric = "ROC",
                 verbose = FALSE,
                 trControl = ctrl)
# Expanding the Search Grid
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
grid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 1000, by = 50),
                    shrinkage = c(0.01, 0.1))

set.seed(1)
gbmTune <- train(churn ~ ., data = churnTrain,
                 method = "gbm",
                 metric = "ROC",
                 tuneGrid = grid,
                 verbose = FALSE,
                 trControl = ctrl)

# Plotting the Results
ggplot(gbmTune) + theme(legend.position = "top")

# The predict method can be used to get results for other data sets:
gbmPred <- predict(gbmTune, churnTest)
str(gbmPred)
head(gbmPred)

#Factor w/ 2 levels "yes","no": 2 2 2 2 2 2 2 2 2 2 ...
gbmProbs <- predict(gbmTune, churnTest, type = "prob")
str(gbmProbs)

# Predction and Performance Assessment
confusionMatrix(gbmPred, churnTest$churn)

# Test Set ROC curve via the pROC Package
rocCurve <- roc(response = churnTest$churn,
                predictor = gbmProbs[, "yes"],
                levels = rev(levels(churnTest$churn)))
rocCurve
plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5, .2),
     print.thres.pch = 16,
     print.thres.cex = 1.2)

# Switching To Other Models
# Minimal changes are needed to fit different models to the same data:
# Using the same seed prior to train() will ensure that
# the same resamples are used (even in parallel)
set.seed(1)
svmTune <- train(churn ~ . , data = churnTrain,
                 # Tell it to fit a SVM model and tune
                 # over Cost and the RBF parameter
                 method = "svmRadial",
                 # This pre-processing will be applied to
                 # these data and new samples too.
                 preProc = c("center", "scale"),
                 # Tune over different values of cost
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")
plot(svmTune)
svmTune$bestTune
svmTune$results
svmTune$finalModel

set.seed(1)
fdaTune <- train(churn ~ . , data = churnTrain,
                 # Now try a flexible discriminant model
                 # using MARS basis functions
                 method = "fda",
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")

names(fdaTune)
plot(fdaTune)
fdaTune$results
fdaTune$finalModel
fdaTune$bestTune

###############################################################################
## Getting Started with Hidden Markov Models in R
## http://www.r-bloggers.com/getting-started-with-hidden-markov-models-in-r/

#
# Hiden Markov Model of S&P 500 log returns
# See documentation for depmixS4 package 
# http://cran.r-project.org/web/packages/depmixS4/depmixS4.pdf and presentation 
# on Singapore R Users Group Site on HMM February 14, 2014
# http://www.meetup.com/R-User-Group-SG/files/

library(depmixS4)
library(TTR)
library(ggplot2)
library(reshape2)

## Bull and Bear Markets ##
# Load S&P 500 returns from Yahoo
Sys.setenv(tz = "UTC")
sp500 <- getYahooData("^GSPC", start = 19500101, end = 20120909, freq = "daily")
head(sp500)
tail(sp500)

# Preprocessing
# Compute log Returns
ep <- endpoints(sp500, on = "months", k = 1)
sp500LR <- sp500[ep[2:(length(ep)-1)]]
sp500LR$logret <- log(sp500LR$Close) - lag(log(sp500LR$Close))
sp500LR <- na.exclude(sp500LR)
head(sp500LR)

# Build a data frame for ggplot
sp500LRdf <- data.frame(sp500LR)
sp500LRdf$Date <-as.Date(row.names(sp500LRdf),"%Y-%m-%d")

# Plot the S&P 500 returns
ggplot( sp500LRdf, aes(Date) ) + 
  geom_line( aes( y = logret ) ) +
  labs( title = "S&P 500 log Returns")


# Construct and fit a regime switching model
mod <- depmix(logret ~ 1, family = gaussian(), nstates = 4, data = sp500LR)
set.seed(1)
fm2 <- fit(mod, verbose = FALSE)
#
summary(fm2)
print(fm2)

# Classification (inference task)
probs <- posterior(fm2)             # Compute probability of being in each state
head(probs)
rowSums(head(probs)[,2:5])          # Check that probabilities sum to 1

pBear <- probs[,2]                  # Pick out the "Bear" or low volatility state
sp500LRdf$pBear <- pBear            # Put pBear in the data frame for plotting

# Pick out an interesting subset of the data or plotting and
# reshape the data in a form convenient for ggplot
df <- melt(sp500LRdf[400:500, 6:8], id="Date", measure=c("logret", "pBear"))
#head(df)

# Plot the log return time series along withe the time series of probabilities
qplot(Date,value,data=df,geom="line",
      main = "SP 500 Log returns and 'Bear' state probabilities",
      ylab = "") + 
  facet_grid(variable ~ ., scales="free_y")

###############################################################################
#----------------------------------------------------------------------

Lines <- "ROW ID      FECHA YEAR CANTIDAD
1 100 2005-08-02 2005        1
2 100 2005-10-19 2005        2
3 100 2007-02-09 2007        1
4 100 2007-10-25 2007        1
5 100 2007-10-29 2007        1
6 120 2006-05-11 2006        1
7 120 2006-08-17 2006        5
8 120 2006-10-15 2006        1
9 120 2007-04-16 2007        3
"

DF <- read.table(textConnection(Lines), header=T, as.is = TRUE)

library(sqldf)
df.tmp <- sqldf("select ID,YEAR, sum(CANTIDAD) as cusum from DF group by
                ID,YEAR order by ID,YEAR")

#----------------------- quitar estas filas
# Pruebo que el salto en años sea mayor que dos..
# En vez de "100 2007" lo cambio a "100 2008"..
df.tmp[2,2] <- c(2008)
# Y que la última fila "120 2007" pasa a ser "120 2014"
df.tmp[4,2] <- c(2014)
#----------------------- quitar filas anteriores

#------- Primer bucle para detectar los saltos en los años
for(i in 1:nrow(df.tmp)) {
  if(i==1) {
    df.tmp$difID[i] <- 0
    df.tmp$difYE[i] <- 0
    
  }
  else{
    
    if(df.tmp$ID[i]!=df.tmp$ID[i-1] & (df.tmp$YEAR[i]-df.tmp$YEAR[i-1] <
                                         0)) {
      df.tmp$difID[i] <- 0
      df.tmp$difYE[i] <- 0
    } else {
      df.tmp$difID[i] <- df.tmp$ID[i] - df.tmp$ID[i-1]
      df.tmp$difYE[i] <- df.tmp$YEAR[i] - df.tmp$YEAR[i-1]
    }
  }
}
df.tmp

#------- Segundo bucle para introducir filas en los saltos de años
# Introduzco filas cuando el salto de años sea mayor que 2...
df.new <- 0
for(i in 1:nrow(df.tmp)) {
  
  #Copio la fila tal cual cuando la diferencia en años es 0 o menor que dos.
  if(df.tmp$difYE[i] < 2) {
    
    df.new <- rbind(df.new, c(df.tmp$ID[i] , df.tmp$YEAR[i],
                              df.tmp$cusum[i]))
    
  } else {
    # Si la diferencia en años es mayor que dos, ciclo en años y teniendo
    # en cuenta que cusum se acumula...
    cusum.cont <- df.tmp$cusum[i-1]
    for(j in 1:(df.tmp$difYE[i]-1) ) {
      df.new <- rbind(df.new, c(df.tmp$ID[i] , df.tmp$YEAR[i-1]+j,
                                df.tmp$cusum[i-1]))
      cusum.cont <- cusum.cont + df.tmp$cusum[i-1]
    }
    # Y tras ciclar copio la fila en la que estaba
    df.new <- rbind(df.new, c(df.tmp$ID[i], df.tmp$YEAR[i],
                              df.tmp$cusum[i]+cusum.cont))
  }
  
}
df.tmp
df.new <- df.new[2:nrow(df.new),]
row.names(df.new) <- NULL
df.new <- as.data.frame(df.new)
names(df.new) <- c('ID', 'YEAR', 'CUSUM')
df.new

#---------------------------------------------------------------------

#----------------------------- FIN DE PROGRAMA -------------------------
###############################################################################
## https://stackoverflow.com/questions/13219387/world-map-map-halves-of-countries-to-different-colors/13269287#13269287
## world map - map halves of countries to different colors
library(rgdal)
library(ggplot2)
library(maptools)

# Data from http://thematicmapping.org/downloads/world_borders.php.
# Direct link: http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip
# Unpack and put the files in a dir 'data'


gpclibPermit()
world.map <- readOGR(dsn="data", layer="TM_WORLD_BORDERS_SIMPL-0.3")
world.ggmap <- fortify(world.map, region = "NAME")

n <- length(unique(world.ggmap$id))
df <- data.frame(id = unique(world.ggmap$id),
                 growth = 4*runif(n),
                 category = factor(sample(1:5, n, replace=T)))

## noise
df[c(sample(1:100,40)),c("growth", "category")] <- NA


ggplot(df, aes(map_id = id)) +
  geom_map(aes(fill = growth, color = category), map =world.ggmap) +
  expand_limits(x = world.ggmap$long, y = world.ggmap$lat) +
  scale_fill_gradient(low = "red", high = "blue", guide = "colorbar")


# -----------------------------------

library(rgeos)
library(RColorBrewer)

# Get centroids of countries
theCents <- coordinates(world.map)

# extract the polygons objects
pl <- slot(world.map, "polygons")

# Create square polygons that cover the east (left) half of each country's bbox
lpolys <- lapply(seq_along(pl), function(x) {
  lbox <- bbox(pl[[x]])
  lbox[1, 2] <- theCents[x, 1]
  Polygon(expand.grid(lbox[1,], lbox[2,])[c(1,3,4,2,1),])
})

# Slightly different data handling
wmRN <- row.names(world.map)

n <- nrow(world.map@data)
world.map@data[, c("growth", "category")] <- list(growth = 4*runif(n),
                                                  category = factor(sample(1:5, n, replace=TRUE)))

# Determine the intersection of each country with the respective "left polygon"
lPolys <- lapply(seq_along(lpolys), function(x) {
  curLPol <- SpatialPolygons(list(Polygons(lpolys[x], wmRN[x])),
                             proj4string=CRS(proj4string(world.map)))
  curPl <- SpatialPolygons(pl[x], proj4string=CRS(proj4string(world.map)))
  theInt <- gIntersection(curLPol, curPl, id = wmRN[x])
  theInt
})

# Create a SpatialPolygonDataFrame of the intersections
lSPDF <- SpatialPolygonsDataFrame(SpatialPolygons(unlist(lapply(lPolys,
                                                                slot, "polygons")), proj4string = CRS(proj4string(world.map))),
                                  world.map@data)

##########
## EDIT ##
##########
# Create a slightly less harsh color set
s_growth <- scale(world.map@data$growth,
                  center = min(world.map@data$growth), scale = max(world.map@data$growth))
growthRGB <- colorRamp(c("red", "blue"))(s_growth)
growthCols <- apply(growthRGB, 1, function(x) rgb(x[1], x[2], x[3],
                                                  maxColorValue = 255))
catCols <- brewer.pal(nlevels(lSPDF@data$category), "Pastel2")

# and plot
plot(world.map, col = growthCols, bg = "grey90")

plot(lSPDF, col = catCols[lSPDF@data$category], add = TRUE)

###############################################################################

## https://github.com/beamartinez/fuertehack/blob/master/IntroductionR.R
## IntroductionR.R

# DATA IMPORT

# Set the working directory (where the files are being to be loaded from and where they are going to be saved)

getwd()
setwd("E:/Varios/R/Archivos/varios/Maps_with_R")


# Download the home page http://contributors.rubyonrails.org/ and save it as *.txt
url <- "http://contributors.rubyonrails.org/"
download.file(url, destfile = "Data/contributors00.txt")

# Load the contributors00.txt file into R
contributors <- readLines("Data/contributors00.txt")
contributors[60:70] # Have a look at some lines



# Once we have the homepage saved and loaded into R, we need to extract a vector with the URLs for every contributor page.
# We will find the URLs in the homepage code by using regular expressions.

contributorsLines <- grep("highlight", contributors, fixed = TRUE, value = TRUE) # Extract the lines where the contributors' names are, formatted to create the URL to their pages.

r <- gregexpr("/contributors/(.*)/commits", contributorsLines) # Get two indices: where the match starts and the match's length.
contributorsURL <- regmatches(contributorsLines, r) # Get the registered matches.
contributorsURL <- paste("http://contributors.rubyonrails.org", contributorsURL, sep="") # Paste the registered matches to the main URL.

head(contributorsURL) # We have now the URLs vector, containing every URL for each contributor page.





# Now, download every contributor page as *.txt, load into R and get the data we want (contributor name, rank, date, message)

# This may take a looong time, so you'd better download the resultant *.csv

urlCsv <- "https://github.com/beamartinez/fuertehack/blob/master/Data/contributorsdf.csv"
download.file(urlCsv, destfile = "contributorsdf.csv", method = "curl") # method must be set to 'curl' as it is a secure URL (https)
contributorsdf <- read.csv("contributorsdf.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


# Uncomment the code below to run the whole process (just remember it takes a time; it depends on your internet connection, but it took me more than half an hour)

# contributorsdf <- data.frame() # Create an empty data.frame where store the data as it is being extracting from the contributors' pages.

# for (i in (1:length(contributorsURL))){ # For every contributors' page

## Download it
# url2 <- contributorsURL[i]
# download.file(url2, destfile = paste("Data/contributor",i,".txt", sep = ""))
# contributor <- readLines(paste("Data/contributor",i,".txt", sep = ""))

## Generate a vector with the commits dates.
# date <- grep("commit-date", contributor, fixed = TRUE, value=TRUE)
# r <- gregexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", date)
# date <- unlist(regmatches(date,r))

## Another one with the commit messages.
# message <- grep("commit-message", contributor, fixed = TRUE, value = TRUE)
# r <- regexec(">(.*)<", message)
# message <- regmatches(message, r)
# message <- sapply(message, function(x) x[2])

## Extract the contributors' names and ranks.
# r <- regexec("Rails Contributors - #(.*?) (.*) -", contributor)
# m <- unlist(regmatches(contributor, r))
# rank <- m[2]
# name <- m[3]

## Join the four vectors (date, message, name and rank) in a data.frame
# tableContributor <- as.data.frame(cbind(date, message))
# tableContributor$name <- name
# tableContributor$rank <- rank

## Store this data in the empty data.frame created before the for loop.
# contributorsdf <- rbind(tableContributor, contributorsdf)
#}


# The data.frame with the data we wanted is ready.
head(contributorsdf)

# Now you can save it (new data have been generated in R !)
write.csv(contributorsdf, file = "Data/contributorsdf.csv", row.names = FALSE)

# And load the file again to use it as input data.
contributorsdf <- read.csv("Data/contributorsdf.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)



# DATA TREATMENT

# Have a look at the data and get information about the object.
str(contributorsdf)
summary(contributorsdf)


# Change an object class
contributorsdf$date <- as.Date(contributorsdf$date, "%Y-%m-%d")
summary(contributorsdf)


# Remove one register
contributorsdf <- contributorsdf[which(contributorsdf$date != "1970-01-01"), ] # The date was wrong, so we remove the whole observation.
nrow(contributorsdf)

# Add some information from other data.frame.
# E.g.: Assign a country to each contributor (not the real one this time).

# Download the countries list (with their codes) and load it into R.
urlCountries <- "https://dl.dropboxusercontent.com/s/c8nlins4davcnz8/ISOcountries.csv?token_hash=AAGb1r0-10fvR-q_eLY6-f5XtshKkYR0jXXFDHQ1mdOY3A&dl=1"
download.file(urlCountries, destfile = "Data/ISOcountries.csv", method = "curl")
ISOcountries <- read.csv("Data/ISOcountries.csv")
head(ISOcountries)

# Generate a vector which will have the same length that the number of contributors
codes <- sample(ISOcountries$codes, length(unique(contributorsdf$name)), replace = TRUE)

# Get a vector containing the contributors names without repetitions
uniqueContributors <- unique(as.character(contributorsdf$name))

# Join both vectors into a data.frame and merge to ISOcountries to add the countries' names.
countries <- data.frame(uniqueContributors, codes)
countries <- merge(countries, ISOcountries, by = "codes")
head(countries)

# Merge the 'countries' data.frame with the contributorsdf
contributorsdf <- merge(contributorsdf, countries, by.x = "name", by.y = "uniqueContributors")
str(contributorsdf) # Two new variables have been added to the data.frame, 'codes' and 'countries'

# Split the 'date' variable in other two: 'year' and 'month'
dates <- strsplit(as.character(contributorsdf$date), "-")
contributorsdf$year <- sapply(dates, function(x) x[1])
contributorsdf$month <- sapply(dates, function(x) x[2])
rm(dates)

# Subset the top 10 contributors
freqCommits <- data.frame(table(contributorsdf$name))  # Get the frequency table of commits per contributor.
names(freqCommits) # The given names are not very intuitive.
names(freqCommits) <- c("name", "Freq") # Change the columns names.

freqCommits <- freqCommits[order(-freqCommits$Freq), ] # Sort the values in decreasing order.
head(freqCommits) # Have a look at the firsts rows

top10 <- as.character(freqCommits$name[1:10]) # Get a vector with the top 10 contributors names (from a factor, that is why 'as.character' should be used).

top10contributors <- contributorsdf[contributorsdf$name %in% top10, ] # Subset the observations related to the top 10 contributors from the main data.frame.

unique(top10contributors$name)
identical(sort(unique(top10contributors$name)), sort(top10))

rm(freqCommits, top10) # Remove the unnecessary objects.




# DATA ANALYSIS

# Get frequency tables

# number of commits per year
table(contributorsdf$year)
mean(table(contributorsdf$year)) # mean of commits per year for the period 2004-2013


# number of commits per month and year
freqYM <- table(contributorsdf$year, contributorsdf$month)
freqYM

margin.table(freqYM, 1) # 'year' frequencies (summed over 'month')
margin.table(freqYM, 2) # 'month' frequencies (summed over 'year')

prop.table(freqYM)    # Cell percentages
prop.table(freqYM, 1) # Row percentages
prop.table(freqYM, 2) # Column percentages

colMeans(freqYM) # Monthly means
rowMeans(freqYM) # Yearly means

# Commits by contributor per year (only for the top ten contributors)
freqYC <- table(top10contributors$year, top10contributors$name)
colMeans(freqYC) # Mean of commits per year by contributor

# Cross tables

xtabs(~name+year, data=top10contributors) # Commits by contributor per year
xtabs(~name+month+year, data=top10contributors) # Commits by contributor per year and month
ftable(xtabs(~name+month+year, data=top10contributors)) # flat table: easy to read table

# Correlation
nContributors <- tapply(contributorsdf$name, contributorsdf$year, function(X) length(unique(X))) # Get the number of unique contributors per year
nCommits <- tapply(contributorsdf$name, contributorsdf$year, function(X) length(X)) # Get the number of commits per year
e <- data.frame(cbind(nCommits, nContributors))
e
cor(e$nContributors, e$nCommits)



# GRAPHICS


# Plot every commit
plot(contributorsdf$date, factor(contributorsdf$name))


# Plot every commit formating the plot
plot(contributorsdf$date, factor(contributorsdf$name),
     main = "Total Commits",
     xlab = "Date",
     ylab = "contributors",
     col = rgb(0,100,0,40,maxColorValue=255),
     pch = 18
)
# See ?par



#Library ggplot2 for nicer graphics (based on layers)
library(ggplot2)

# Plot the top ten contributors' commits
p <- ggplot(top10contributors, aes(date, name))
p + geom_point()
p + geom_point(alpha = 0.2, size = 3, aes(colour = factor(name)), show_guide = FALSE)

# Plot commits per month
# Bargraph
m <- ggplot(contributorsdf, aes(month))
m + geom_bar(fill = "darkblue")

# Stacked bars
m <- ggplot(contributorsdf, aes(month, fill=year))
m + geom_bar() + coord_flip()

# Plot them in a grid
m <- ggplot(contributorsdf, aes(month)) + geom_bar(aes(fill=year))
m + facet_grid(year ~ .)+ theme(legend.position = "none")

# Lines plot
m <- ggplot(contributorsdf, aes(month, colour = year, group = year))
m+geom_freqpoly()

# Plot the number of commits ~ number of contributors and its regression line
p <- ggplot(e, aes(nContributors, nCommits))
p + geom_smooth(method='lm', colour = "#CC79A7", se = FALSE) +
  geom_point(alpha = 0.8, size = 5, colour = "#009E73") +
  xlab("Number of contributors") + # Set x-axis label
  ylab("Number of commits") +
  annotate("text", label = "Correlation = 0.88", x = 700, y = 150, colour = "#CC79A7") + # Prints the correlation index
  theme(axis.title = element_text(size = rel(1.4), colour = "#006348"), # Change the appearance
        axis.title.x = element_text(vjust = 0.1),
        axis.title.y = element_text(vjust = 0.25),
        axis.text = element_text(size = rel(1.2)),
        panel.background = element_rect(fill = "#F5F6CE")
  )



# Plot a map

# Download the necessary libraries
if (!"sp" %in% installed.packages()) install.packages("sp")
if (!"maptools" %in% installed.packages()) install.packages("maptools")

# Load them into R
library(sp)
library(maptools)


data(wrld_simpl) # Get a World map where to plot the variable (number of commits by country this time)
commitsMap <- wrld_simpl
class(commitsMap)

# Get the frequency table for the countries
countriesFreq <- as.data.frame(table(contributorsdf$codes))
head(countriesFreq)


# Merge the countries frequency table with the commitsMap@data, the 'data.frame' inside the SpatialPolygonsDataFrame
commitsMap@data <- merge(commitsMap@data, countriesFreq, by.x = "ISO2", by.y = "Var1", all.x=T)

head(commitsMap@data) # A new variable has been added

# Set a bunch of colors. (So many by default in the "RColorBrewer" library)
colors <- c("#1D3140", "#1C3D4D","#194A58","#125862","#09666A","#047370","#0B8174","#1C8F76","#309D77","#46AA75","#5DB872","#76C46E","#91D069","#AEDB64","#CDE660","#EDEF5D") # Copy pasted from http://tristen.ca/hcl-picker/

# Plot the SpatialPolygonsDataFrame
spplot(commitsMap, "Freq", col.regions = rev(colors),
       par.settings = list(
         panel.background = list(col="#CEE3F6"),
         add.line = list(col = "#F5F6CE", lwd = .2)))



# Save any plot as *.png
png(file = "Images/commits_map.png", height = 480, width = (480*2))
spplot(commitsMap, "Freq", col.regions = rev(colors),
       par.settings = list(
         panel.background = list(col="#CEE3F6"),
         add.line = list(col = "#F5F6CE", lwd = .2)))
dev.off()



# Plot Fuertehack contributors

people <- c("Fernando Guillén", "Juanjo Bazán", "Fernando Blat","Paco Guzman","Christos Zisopoulos","Alberto Perdomo")

peopleData <- contributorsdf[contributorsdf$name %in% people, ]

p <- ggplot(peopleData, aes(date, name))
p + geom_point(alpha = 0.8, size = 4, aes(colour = factor(name)), show_guide = FALSE) 

###############################################################################
## Less wordy R
## http://www.statsblogs.com/2014/03/11/less-wordy-r/

require(Quandl)
require(ggplot2)

Quandl.auth("k44p2KxamP8YkocdXDDM")

# Basic Use

# Just pick a dataset and enter its Quandl code, like this:

#  mydata = Quandl("FRED/GDP")

# The Quandl package is able to return data in 4 very usable formats: data frame ("raw"), 
# ts ("ts"), zoo ("zoo") and xts ("xts"). The default is "raw". Here's how to get your data in ts format:
# mytimeseries = Quandl("NSE/OIL", type="ts")

# You can get multiple datasets in one call by passing an array of Quandl codes, like this:
# mydata = Quandl(c("NSE/OIL.4","WIKI/AAPL.1"))



# This grabs the 4th column of dataset NSE/OIL and the 1st column of dataset WIKI/AAPL, and returns them in a single call.

# Data Manipulations

# Just like the basic API, the R package offers various ways to manipulate or transform the data prior to download:
# Specific Date Range:

# mydata = Quandl("NSE/OIL", start_date="yyyy-mm-dd", end_date="yyyy-mm-dd")
# Frequency Change:
# mydata = Quandl("NSE/OIL", collapse="annual")
# ("weekly"|"monthly"|"quarterly"|"annual")

# Transformations:
# mydata = Quandl("NSE/OIL", transformation="rdiff")
# ("diff"|"rdiff"|"normalize"|"cumulative")

# (These transformations are documented more fully on our API page.)

# For further transformations, you might find our Cheat Sheet on time series analysis using R and Quandl useful.

# Search
# You can search Quandl for datasets from within the R console. Here is an example search:
# Quandl.search(query = "crude oil", page = 2, source = "DOE", silent = TRUE)

# The only mandatory parameter is "query". "page" specifies which page of search results you wish to see (default=1). "source" specifies that you want to search within a particular data source (default=all data sources). "silent" specifies whether or not you want to see the first 3 results on the console.


# Load data from Quandl
my.data <- Quandl("TPC/HIST_RECEIPT", 
                  start_date = "1945-12-31", 
                  end_date = "2013-12-31")

# The whole example relies on only three variables and-as I am not great at typing-I tend 
# to work with shorter variable names. I directly changed the names for variables 1 to 3:

# Display first lines of the data frame
# and set short names for first three columns
head(my.data)
names(my.data)[1:3] <- c('year', 'indtax', 'corptax')

# Change shape to fit both regressions simultaneously
mdlong <- reshape(my.data[, 1:3], 
                  idvar = 'year', times = c('Individual', 'Corporate'), 
                  varying = list(2:3), direction = 'long')

mdlong$taxtype <- factor(mdlong$time)

# And now we are ready to produce the plots. The first one can be a 
# rough cut to see if we get the right elements:

ggplot(mdlong, aes(x = year, y = indtax, color = taxtype)) + 
  geom_point() + geom_line() + geom_smooth(method = 'lm')

# Plotting the graph (first match color palette) and put the regression
# lines as well
serious.palette = c('#AD3333', '#00526D')
ggplot(mdlong, aes(x = year, y = indtax, color = taxtype)) + 
  geom_point() + geom_line() + geom_smooth(method = 'lm', aes(fill = taxtype)) + 
  theme_bw() + 
  scale_y_continuous('Income taxes (% of GDP)', breaks = seq(0, 12, 2), 
                     minor_breaks = NULL) + 
  scale_x_date('Fiscal year', minor_breaks = NULL) + 
  scale_colour_manual(values=serious.palette) + scale_fill_manual(values=serious.palette)

# Fitting a regression with dummy variables
m1 <- lm(indtax ~ year*taxtype, data = mdlong)
summary(m1)


###############################################################################
## Hidden Markov Models
## http://a-little-book-of-r-for-bioinformatics.readthedocs.org/en/latest/src/chapter10.html

# you may want to create a vector variable for storing the square of a number, 
# and then store numbers in its elements afterwards:

myvector <- numeric()                  # Create a vector "myvector" for storing numbers
for (i in 1:10) { myvector[i] <- i*i } # Fill in the values in the vector "myvector"
myvector                               # Print out the vector "myvector"

# Note that if you try to store numbers in the elements of a vector that you have not yet 
# created, you will get an error message, for example:

for (i in 1:10) { avector[i] <- i*i }  # Try to store values in the vector "avector"

# Matrix
# For example, say you have the heights and weights of eight patients in a hospital in two different vectors:

heights <- c(180, 170, 175, 160, 183, 177, 179, 182)
weights <- c(90, 88, 100, 68, 95, 120, 88, 93)

# To store this data in a matrix that has one column per person, and one row 
# for heights and one row for weights, we type:
  
mymatrix <- matrix(c(heights,weights), 2, 8, byrow=TRUE)
mymatrix # Print out the matrix

# We needed to use the argument "byrow=TRUE" to tell the matrix() command to 
# fill the matrix row-by-row (ie. to put the values from the vector heights 
# into the first row of the matrix, and the values from the vector weights 
# into the second row of the matrix).

# You can assign names to the rows and columns of a matrix using the rownames() 
# and colnames() commands, respectively. For example, to assign names to the 
# rows and columns of matrix mymatrix, you could type:
  
rownames(mymatrix) <- c("height", "weight")
colnames(mymatrix) <- c("patient1", "patient2", "patient3", "patient4", "patient5", "patient6", "patient7", "patient8")
mymatrix # Print out the matrix now

# Once you have created a matrix, you can access the values in the elements of 
# the matrix by using square brackets containing the indices of the row and 
# column of the element. For example, if you want to access the value in the 
# second row and fourth column of matrix mymatrix, you can type:

mymatrix[2,4]

# if you want to access all the values in a particular row of the matrix, 
# you can just type the index for the row, and leave out the index for the 
# column. For example, if you want to get the values in the second row of 
# the matrix mymatrix, type:

mymatrix[2,]

# Likewise, if you want to get the values in a particular column of a matrix, 
# leave out the index for the row, and just type the column index. For example, 
# if you want to get the values in the fourth row of the mymatrix, type:

mymatrix[,4]

# A multinomial model of DNA sequence evolution

# The simplest model of DNA sequence evolution assumes that the sequence has been produced by a random process that 
# randomly chose any of the four nucleotides at each position in the sequence, where the probability of choosing any 
# one of the four nucleotides depends on a predetermined probability distribution. That is, the four nucleotides are 
# chosen with pA, pC, pG, and pT respectively. This is known as the multinomial sequence model.

# A multinomial model for DNA sequence evolution has four parameters: the probabilities of the four nucleotides 
# pA, pC, pG, and pT. For example, say we may create a multinomial model where pA=0.2, pC=0.3, pG=0.3, and pT=0.2. 
# This means that the probability of choosing a A at any particular sequence position is set to be 0.2, the probability 
# of choosing a C is 0.3, of choosing a G is 0.3, and of choosing a T is 0.2. Note that pA + pC + pG + pT = 1, as the 
# sum of the probabilities of the four different types of nucleotides must be equal to 1, as there are only four possible 
# types of nucleotide.

# The multinomial sequence model is like having a roulette wheel that is divided into four different slices labelled 
# "A", "T", "G" and "C", where the pA, pT, pGand pC are the fractions of the wheel taken up by the slices with these four 
# labels. If you spin the arrow attached to the centre of the roulette wheel, the probability that it will stop in the 
# slice with a particular label (eg. the slice labelled "A") only depends on the fraction of the wheel taken up by that 
# slice 

# Generating a DNA sequence using a multinomial model

# We can use R to generate a DNA sequence using a particular multinomial model. First we need to set the values of the 
# four parameters of the multinomial model, the probabilities pA, pC, pG, and pT of choosing the nucleotides 
# A, C, G and T, respectively, at a particular position in the DNA sequence. For example, say we decide to set 
# pA=0.2, pC=0.3, pG=0.3, and pT=0.2. We can use the function sample() in R to generate a DNA sequence of a certain 
# length, by selecting a nucleotide at each position according to this probability distribution:
  
nucleotides    <- c("A", "C", "G", "T") # Define the alphabet of nucleotides
probabilities1 <- c(0.2, 0.3, 0.3, 0.2) # Set the values of the probabilities
seqlength      <- 30                    # Set the length of the sequence
sample(nucleotides, seqlength, rep=TRUE, prob=probabilities1) # Generate a sequence

# if you look at the help page for the function(), you will find that its inputs are the vector to sample from 
# (nucleotides here), the size of the sample (seqlength here), and a vector of probabilities for obtaining the 
# elements of the vector being sampled (probabilities1 here). If we use the sample() function to generate a 
# sequence again, it will create a different sequence using the same multinomial model:

sample(nucleotides, seqlength, rep=TRUE, prob=probabilities1) # Generate another sequence

# In the same way, we can generate a sequence using a different multinomial model, where 
# pA=0.1, pC=0.41, pG=0.39, and pT=0.1:

probabilities2 <- c(0.1, 0.41, 0.39, 0.1) # Set the values of the probabilities for the new model
sample(nucleotides, seqlength, rep=TRUE, prob=probabilities2) # Generate a sequence

# As you would expect, the sequences generated using this second multinomial model have a higher fraction of Cs and Gs 
# compared to the sequences generated using the first multinomial model above. This is because pC and GT are higher for 
# this second model than for the first model (pC=0.41 and GT=0.39 in the second model, versus pC=0.3 and GT=0.3 in the 
# first model). That is, in the second multinomial model we are using a roulette wheel that has large slices labelled 
# "C" and "G", while in the first multinomial model we were using a roulette wheel with relatively smaller slices labelled 
# "C" and "G" 

# A type of DNA sequence model called a Markov sequence model is a more accurate representation of the evolution of the 
# sequence.

# A Markov sequence model assumes that the sequence has been produced by a process that chose any of the four nucleotides 
# in the sequence, where the probability of choosing any one of the four nucleotides at a particular position depends on 
# the nucleotide chosen for the previous position. That is, if "A" was chosen at the previous position, then the probability 
# of choosing any one of the four nucleotides at the current position depends on a predetermined probability distribution. 
# That is, given that "A" was chosen at the previous position, the four nucleotides are chosen at the current position with 
# probabilities of pA, pC, pG, and pT of choosing "A", "C", "G", or "T", respectively (eg. pA=0.2, pC=0.3, pG=0.3, and 
# pT=0.2). In contrast, if "C" was chosen at the previous position, then the probability of choosing any one of the four 
# nucleotides at the current position depends on a different predetermined probability distribution, that is, the 
# probabilities of choosing "A", "C", "G", or "T" at the current position are now different (eg. pA=0.1, pC=0.41, pG=0.39, 
# and pT=0.1).

# A Markov sequence model is like having four different roulette wheels, labelled "afterA", "afterT", "afterG", and "afterC", 
# for the cases when "A", "T", "G", or "C" were chosen at the previous position in a sequence, respectively. Each of the four 
# roulette wheels has four slices labelled "A", "T", "G", and "C", but in each roulette wheel a different fraction of the 
# wheel is taken up by the four slices. That is, each roulette wheel has a different pA, pT, pG and pC. If we are generating 
# a new DNA sequence using a Markov sequence model, to decide what nucleotide to choose at a particular position in the 
# sequence, you spin the arrow at the centre of a roulette wheel, and see in which slice the arrow stops. There are four 
# roulette wheels, and the particular roulette wheel we use at a particular position in the sequence depends on the nucleotide 
# chosen for the previous position in the sequence. For example, if "T" was chosen at the previous position, we use the 
# "afterT" roulette wheel to choose the nucleotide for the current position. The probability of choosing a particular 
# nucleotide at the current position (eg. "A") then depends on the fraction of the "afterT" roulette wheel taken up by the 
# the slice labelled with that nucleotide

# The transition matrix for a Markov model

# A multinomial model of DNA sequence evolution just has four parameters: the probabilities pA, pC, pG, and pT. 
# In contrast, a Markov model has many more parameters: four sets of probabilities pA, pC, pG, and pT, that differ 
# according to whether the previous nucleotide was "A", "G", "T" or "C". The symbols pAA, pAC, pAG, and pAT are usually 
# used to represent the four probabilities for the case where the previous nucleotide was "A", the symbols pCA, pCC, pCG, 
# and pCT for the case when the previous nucleotide was "C", and so on.

# It is common to store the probability parameters for a Markov model of a DNA sequence in a square matrix, which is known 
# as a Markov transition matrix. The rows of the transition matrix represent the nucleotide found at the previous position 
# in the sequence, while the columns represent the nucleotides that could be found at the current position in the sequence. 
# In R, you can create a matrix using the matrix() command, and the rownames() and colnames() functions can be used to label 
# the rows and columns of the matrix. For example, to create a transition matrix, we type:
  
nucleotides         <- c("A", "C", "G", "T") # Define the alphabet of nucleotides
afterAprobs <- c(0.2, 0.3, 0.3, 0.2)         # Set the values of the probabilities, where the previous nucleotide was "A"
afterCprobs <- c(0.1, 0.41, 0.39, 0.1)       # Set the values of the probabilities, where the previous nucleotide was "C"
afterGprobs <- c(0.25, 0.25, 0.25, 0.25)     # Set the values of the probabilities, where the previous nucleotide was "G"
afterTprobs <- c(0.5, 0.17, 0.17, 0.17)      # Set the values of the probabilities, where the previous nucleotide was "T"
mytransitionmatrix <- matrix(c(afterAprobs, afterCprobs, afterGprobs, afterTprobs), 4, 4, byrow = TRUE) # Create a 4 x 4 matrix
mytransitionmatrix
rownames(mytransitionmatrix) <- nucleotides
colnames(mytransitionmatrix) <- nucleotides
mytransitionmatrix                           # Print out the transition matrix

# Rows 1, 2, 3 and 4 of the transition matrix give the probabilities pA, pC, pG, and pT for the cases where the previous 
# nucleotide was "A", "C", "G", or "T", respectively. That is, the element in a particular row and column of the transition 
# matrix (eg. the row for "A", column for "C") holds the probability (pAC) of choosing a particular nucleotide ("C") at the 
# current position in the sequence, given that was a particular nucleotide ("A") at the previous position in the sequence.

# Generating a DNA sequence using a Markov model

# Just as you can generate a DNA sequence using a particular multinomial model, you can generate a DNA sequence using a 
# particular Markov model. When you are generating a DNA sequence using a Markov model, the nucleotide chosen at each position 
# at the sequence depends on the nucleotide chosen at the previous position. As there is no previous nucleotide at the first 
# position in the new sequence, we need to define the probabilities of choosing "A", "C", "G" or "T" for the first position. 
# The symbols ??A, ??C, ??G, and ??T are used to represent the probabilities of choosing "A", "C", "G", or "T" at the first 
# position.

# We can define an R function generatemarkovseq() to generate a DNA sequence using a particular Markov model:
  
generatemarkovseq <- function(transitionmatrix, initialprobs, seqlength)
{
  nucleotides     <- c("A", "C", "G", "T") # Define the alphabet of nucleotides
  mysequence      <- character()           # Create a vector for storing the new sequence
  # Choose the nucleotide for the first position in the sequence:
  firstnucleotide <- sample(nucleotides, 1, rep=TRUE, prob=initialprobs)
  mysequence[1]   <- firstnucleotide       # Store the nucleotide for the first position of the sequence
  for (i in 2:seqlength)
  {
    prevnucleotide <- mysequence[i-1]     # Get the previous nucleotide in the new sequence
    # Get the probabilities of the current nucleotide, given previous nucleotide "prevnucleotide":
    probabilities  <- transitionmatrix[prevnucleotide,]
    # Choose the nucleotide at the current position of the sequence:
    nucleotide     <- sample(nucleotides, 1, rep=TRUE, prob=probabilities)
    mysequence[i]  <- nucleotide          # Store the nucleotide for the current position of the sequence
  }
  return(mysequence)
}

# The function generatemarkovseq() takes as its arguments (inputs) the transition matrix for the particular Markov model; 
# a vector containing the values of ??A, ??C, ??G, and ??T; and the length of the DNA sequence to be generated.

# The probabilities of choosing each of the four nucleotides at the first position in the sequence are 
# ??A, ??C, ??G, and ??T. The probabilities of choosing each of the four nucleotides at the second position in the sequence 
# depend on the particular nucleotide that was chosen at the first position in the sequence. The probabilities of choosing 
# each of the four nucleotides at the third position depend on the nucleotide chosen at the second position, and so on.

# We can use the generatemarkovseq() function to generate a sequence using a particular Markov model. For example, to create 
# a sequence of 30 nucleotides using the Markov model described in the transition matrix mytransitionmatrix, using uniform 
# starting probabilities (ie. ??A = 0.25, ??C = 0.25, ??G = 0.25, and ??T = 0.25) , we type:
  
myinitialprobs <- c(0.25, 0.25, 0.25, 0.25)
generatemarkovseq(mytransitionmatrix, myinitialprobs, 30)

# As you can see, there are many "A"s after "T"s in the sequence. This is because pTA has a high value (0.5) in the Markov 
# transition matrix mytransitionmatrix. Similarly, there are few "A"s or "T"s after "C"s, which is because pCA and pCT have 
# low values (0.1) in this transition matrix.

###############################################################################
## R in action.pdf

# R represents missing values using the symbol  N A (not available) and impossible values by
# the symbol  N aN (not a number). In addition, the symbols  Inf and  -Inf represent positive
# infinity and negative infinity, respectively. The functions  i s.na() ,  i s.nan() , and
# is.infinite() can be used to identify missing, impossible, and infinite values respectively. 
# Each returns either  TRUE or  FALSE . 

library(VIM)       # analyze missing values
# load the dataset
data(sleep, package="VIM")
# list the rows that do not have missing values
sleep[complete.cases(sleep),]
# list the rows that have one or more missing values
sleep[!complete.cases(sleep),]
# Examining the output reveals that 42 cases have complete data and 20 cases have one
# or more missing values.

# Because the logical values  TRUE and  FALSE are equivalent to the numeric values
# 1 and 0, the sum() and mean() functions can be used to obtain useful information
# about missing data. Consider the following:
sum(is.na(sleep$Dream))
[1] 12
mean(is.na(sleep$Dream))
[1] 0.19
mean(!complete.cases(sleep))
[1] 0.32

# The results indicate that there are 12 missing values for the variable Dream. Nineteen
# percent of the cases have a missing value on this variable. In addition, 32 percent of the
# cases in the dataset contain one or more missing values.

# There are two things to keep in mind when identifying missing values. First, the
# complete.cases() function only identifies NA and NaN as missing. Infinite values (Inf
# and  -Inf ) are treated as valid values. Second, you must use missing values functions,
# like those in this section, to identify the missing values in R data objects. Logical
# comparisons such as  myvar == NA are never true.

# The  m d.pattern() function in the  m ice package will produce a tabulation of the
# missing data patterns in a matrix or data frame. 

library(mice)
data(sleep, package="VIM")
md.pattern(sleep)

# The  a ggr() function plots the number of missing values for each variable alone
# and for each combination of variables. For example, the code
library("VIM")
aggr(sleep, prop=FALSE, numbers=TRUE)
matrixplot(sleep)
# The  m arginplot() function produces a scatter plot between two variables with infor-
# mation about missing values shown in the plot's margins. 
marginplot(sleep[c("Gest","Dream")], pch=c(20),
           col=c("darkgray", "red", "blue"))

# You can replace the data in a dataset with indicator variables, coded 1 for missing and 
# 0 for present. The resulting matrix is sometimes called a shadow matrix. Correlating these 
# indicator variables with each other and with the original (observed) variables can help you 
# to see which variables tend to be missing together, as well as relationships between a 
# variable's "miss  ingness" and the values of the other variables.

x <- as.data.frame(abs(is.na(sleep)))

# The elements of data frame  x are 1 if the corresponding element of  sleep is missing
# and 0 otherwise. You can see this by viewing the first few rows of each:
head(sleep, n=5)
head(x, n=5)

# The statement
y <- x[which(sd(x) > 0)]
# extracts the variables that have some (but not all) missing values, and
cor(y)
# gives you the correlations among these indicator variables

###############################################################################
## Totales agregados por bloques en tablas
## http://www.datanalytics.com/2014/03/25/totales-agregados-por-bloques-en-tablas/?utm_source=twitterfeed&utm_medium=linkedin

# En ocasiones uno quiere añadir un total calculado en ciertos bloques a una tabla. 
# Por ejemplo, en la tabla

set.seed(1234)
ventas.orig <- data.frame(cliente = rep(1:10, each = 5), 
                          producto = rep(letters[1:5], times = 10), 
                          importe = rlnorm(50))

# tenemos clientes, productos e importes. Y nos preguntamos por el porcentaje 
# en términos de importe que cada producto supone para cada cliente.

# Una manera natural pero torpe de realizar este cálculo consiste en usar un 
# objeto intermedio y merge:

library(plyr)
tmp <- ddply(ventas.orig, .(cliente), summarize, total = sum(importe))
ventas <- merge(ventas.orig, tmp)
ventas$pct.producto <- 100 * ventas$importe / ventas$total

# No os asustéis, se puede hacer aún peor (p.e., usando sqldf). Pero existen 
# dos maneras, cuando menos, de hacerlo mejor. La primera es usando data.table.

library(data.table)

ventas <- data.table(ventas.orig)
ventas[, total.cliente := sum(importe), by = cliente]
ventas$pct.producto <- 100 * ventas$importe / ventas$total.cliente

# El operador := es el que hace la magia en la segunda línea. Una ventaja de 
# data.table es que vuela literalmente con conjuntos de datos semigrandes.

# También es posible hacerlo todavía más sucintamente con plyr:

library(plyr)
ventas <- ddply(ventas.orig, .(cliente), transform, pct.producto = 100 * importe / sum(importe))

# Una única línea. El problema de plyr, sin embargo, es que es ineficiente 
# con conjuntos de datos grandecitos.

###############################################################################

### useR13_spatial_tutorial.R
### R code from vignette source 'useR13_spatial_tutorial.Rnw'
### Encoding: UTF-8
# packages: sp rgdal RColorBrewer classInt DCluster INLA rgeos maptools spdep
# raster CARBayes
#
# data: Prem_mort_sim setor1 UTM_dem.tif DEM_out.tif stream1 L8s.tif
# L_ETMps.tif 
###################################################
### code chunk number 17: useR13_spatial_tutorial.Rnw:151-157
###################################################
setwd("E:/Varios/R/Archivos/varios")

library(rgdal)
sm <- readOGR(".", "Prem_mort_sim")
sm1 <- sm[!is.na(sm$Value),]
library(RColorBrewer)
at <- c(200, 257, 268, 283, 505)
pal <- rev(brewer.pal(5, "RdYlGn")[-5])


###################################################
### code chunk number 18: useR13_spatial_tutorial.Rnw:165-168
###################################################
spplot(sm1, "Value", at=at, col.regions=pal, main="Premature mortalities per 100,000")


###################################################
### code chunk number 19: useR13_spatial_tutorial.Rnw:183-187
###################################################
library(classInt)
PHE <- classIntervals(sm1$Value, n=4, style="fixed", fixedBreaks=at)
Sd <- classIntervals(sm1$Value, n = 4, style = "sd")
fish <- classIntervals(sm1$Value, n=length(Sd$brks)-1, style="fisher")


###################################################
### code chunk number 20: useR13_spatial_tutorial.Rnw:191-198
###################################################
oopar <- par(mfrow=c(3,1), mar=c(2,1,3,1))
plot(PHE, pal=pal, main="PHE", xlab="", ylab="")
plot(Sd, pal=pal, main="S.D.", xlab="", ylab="")
plot(fish, pal=pal, main="Fisher", xlab="", ylab="")
par(oopar)


###################################################
### code chunk number 21: useR13_spatial_tutorial.Rnw:207-213
###################################################
at1 <- fish$brks
at1[1] <- at1[1] - at1[1]/100
at1[length(at1)] <- at1[length(at1)] + at1[length(at1)]/100
spplot(sm1, "Value", at=at1, col.regions=colorRampPalette(pal)(length(at1)-1), 
       main="Premature mortalities per 100,000")


###################################################
### code chunk number 22: useR13_spatial_tutorial.Rnw:227-231
###################################################
at2 <- c(185, 257, 268, 283, 470)
spplot(sm1, c("Lowr_CI", "Value", "Uppr_CI"), at=at2, col.regions=pal, 
       main="Premature mortalities per 100,000")


###################################################
### code chunk number 23: useR13_spatial_tutorial.Rnw:254-257
###################################################
plot(I(Uppr_CI-Lowr_CI) ~ Denmntr, data=sm1)

###################################################
olinda <- readOGR(".", "setor1")
names(olinda)
spplot(olinda, "DEPRIV", col.regions = grey.colors(20, 0.9, 0.3))

# Coordinate reference systems (CRS) are at the heart of
# geodetics and cartography: how to represent a bumpy ellipsoid
# on the plane

# We can speak of geographical CRS expressed in degrees and
# associated with an ellipse, a prime meridian and a datum, and
# projected CRS expressed in a measure of length, and a chosen
# position on the earth, as well as the underlying ellipse, prime
# meridian and datum.

# The EPSG list among other sources is used in the workhorse
# PROJ.4 library, which as implemented by Frank Warmerdam
# handles transformation of spatial positions between different
# CRS
# This library is interfaced with R in the rgdal package, and the
# CRS class is defined partly in sp, partly in rgdal
# A CRS object is defined as a character NA string or a valid
# PROJ.4 CRS definition
# The validity of the definition can only be checked if rgdal is
# loaded

# Spatial points
# The most basic spatial data object is a point, which may have 2 or 3 dimensions
# A single coordinate, or a set of such coordinates, may be used to define a SpatialPoints object; 
# coordinates should be of mode double and will be promoted if not
# The points in a SpatialPoints object may be associated with a row of attributes to create a 
# SpatialPointsDataFrame object
# The coordinates and attributes may, but do not have to be keyed to each other using ID values

names(olinda)
olinda$Expected <- olinda$POP * sum(olinda$CASES, na.rm = TRUE)/sum(olinda$POP, na.rm = TRUE)
names(olinda)
str(olinda, max.level = 2)
str(as(olinda, "data.frame"))

# The object also works within model fitting functions, like glm; note the number of rows.
str(model.frame(CASES ~ DEPRIV + offset(log(POP)), olinda), give.attr = FALSE)

# Creating objects within R
# maptools includes ContourLines2SLDF() to convert contour lines to SpatialLinesDataFrame objects
# maptools also allows lines or polygons from maps to be used as sp objects
# maptools can export sp objects to PBSmapping
# maptools uses gpclib to check polygon topology and to dissolve polygons
# maptools converts some sp objects for use in spatstat
# maptools can read GSHHS high-resolution shoreline data into SpatialPolygon objects

# The GridTopology class is the key constituent of raster representations,
# giving the coordinates of the south-west raster cell, the two cell sizes in the
# metric of the coordinates, giving the step to successive centres, and the numbers
# of cells in each dimension.
library(sp)
getClass("GridTopology")

# A SpatialGridDataFrame object
# The space shuttle flew a radar topography mission in 2000, giving 90m
# resolution elevation data for most of the world. The data here have been
# warped to a UTM projection, but for the WGS84 datum - we'll see below
# how to project and if need be datum transform Spatial objects:
DEM <- readGDAL("UTM_dem.tif")
str(DEM)
summary(DEM)
names(DEM)
DEM$band1
# Replace values low than 1 to NA
is.na(DEM$band1) <- DEM$band1 < 1
DEM$band1
image(DEM, "band1", axes = TRUE, col = terrain.colors(20))

# Methods provided by sp
# This table summarises the methods provided by sp:
# method           what it does
# [                select spatial items (points, lines, polygons, or
#                  rows/cols from a grid) and/or attributes variables
# $, $<-, [[, [[<- retrieve, set or add attribute table columns
# spsample         sample points from a set of polygons, on a set of
#                  lines or from a gridded area
# bbox             get the bounding box
# proj4string      get or set the projection (coordinate reference system)
# coordinates      set or retrieve coordinates
# coerce           convert from one class to another
# over             combine two different spatial objects

# The original data set CRS
# Very often, a good deal of work is needed to find the actual coordinate
# reference system of a data set. Typically, researchers use what is to hand,
# without thinking that incomplete CRS specifications limit their ability to
# integrate other data. Here we were told that the map was from the 1990
# census, that it was most likely UTM zone 25 south, but not its datum.
# Investigation showed that until very recently, the most used datum in
# Brazil has been Corrego Alegre:
proj4string(olinda)
# [1] NA
EPSG <- make_EPSG()
EPSG[grep("Corrego Alegre", EPSG$note), 1:2]

# In order to insert the parameters for the transformation to the WGS84
# datum, we have to override the initial imported values:
set_ReplCRS_warn(FALSE)
# [1] FALSE
proj4string(olinda) <- CRS("+init=epsg:22525")
proj4string(olinda)

proj4string(DEM)
# [1] "+proj=utm +zone=25 +south +ellps=WGS84 +units=m +no_defs"
proj4string(DEM) <- CRS("+init=epsg:31985")
proj4string(DEM)
# [1] "+init=epsg:31985 +proj=utm +zone=25 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
set_ReplCRS_warn(TRUE)

# Getting olinda to WGS84
# As we see, although both olinda and DEM are UTM zone 25 south, they
# differ in their ellipsoid and datum. Using spTransform methods in rgdal
# we can undertake a datum shift for olinda, making it possible to
# overplot in register:
olinda1 <- spTransform(olinda, CRS(proj4string(DEM)))
image(DEM, "band1", axes = TRUE, col = terrain.colors(20))
plot(olinda1, add = TRUE, lwd = 0.5)

library(spgrass6)
# myGRASS <- "/home/rsb/topics/grass/g642/grass-6.4.2"
# myGRASS <- "C:/Program Files/QGIS Valmiera/apps/grass/grass-6.4.3"
myGRASS <- "C:\\Program Files\\QGIS Valmiera\\apps\\grass\\grass-6.4.3\\bin"
initGRASS(myGRASS, tempdir(), SG = DEM, override = TRUE)

WPATH <- Sys.getenv("PATH")
WPATH1 <- paste("C:\\Program Files\\QGIS Valmiera\\apps\\grass\\grass-6.4.3\\bin", WPATH, sep=";")
# Note: Users with 64bit installation should put "C:\\OSGeo4W64\\bin"
#Sys.setenv(PATH=WPATH1)
initGRASS("C:/Program Files/QGIS Valmiera/apps/grass/grass-6.4.3", tempdir(), override=TRUE, SG = DEM)
# Note: Users with 64bit installation should put C:/OSGeo4W64/apps/grass/grass-6.4.3

writeRAST6(DEM, "dem", flags = "o")
execGRASS("g.region", rast = "dem")
execGRASS("r.resamp.rst", input = "dem",
           ew_res = 14.25, ns_res = 14.25,
           elev = "DEM_resamp", slope = "slope",
           aspect = "aspect")
execGRASS("g.region", rast = "DEM_resamp")
DEM_out <- readRAST6(c("DEM_resamp",
                       "slope", "aspect"))

execGRASS("r.watershed", elevation = "DEM_resamp",
          stream = "stream", threshold = 1000L,
          convergence = 5L, memory = 300L)
execGRASS("r.thin", input = "stream",
          output = "stream1", iterations = 200L)

# First we vectorise the raster stream network, and read it into the R session for display:
execGRASS("r.to.vect", input = "stream1",
          output = "stream", feature = "line")
stream1 <- readVECT6("stream")

# Population density in Olinda
# So let's see how things go with the 1993 data for different tracts:
olinda1$area <- sapply(slot(olinda1, "polygons"), slot, "area")
km2 <- olinda1$area/1e+06
olinda1$dens <- olinda1$POP/km2
library(RColorBrewer)
tt <- "Population density per square km"
spplot(olinda1, "dens", at = c(0,8000, 12000, 15000, 20000, 60000), 
       col.regions = brewer.pal(6,"YlOrBr")[-1], main = tt, col = "grey", lwd = 0.5)

# The rgeos package interfaces the GEOS/JTS topology suite providing predicates and operations for geometries.
# The simplest are measures for planar geometries - only these are supported. A complication is that computational
# geometry may represent objects using different scaling factors, leading to geometries becoming"unclean"; GEOS uses
# a fixed precision representation.

library(rgeos)
getScale()
pols <- as(olinda1, "SpatialPolygons")
Area <- gArea(pols, byid = TRUE)
all.equal(unname(Area), olinda1$area)

lns0 <- as(stream1, "SpatialLines")
length(lns0)
summary(gLength(lns0, byid = TRUE))
t0 <- gTouches(lns0, lns0, byid = TRUE)
any(t0)

library(spdep)
lw <- mat2listw(t0)
is.symmetric.nb(lw$neighbours)
nComp <- n.comp.nb(lw$neighbours)
IDs <- as.character(nComp$comp.id)
lns <- gLineMerge(lns0, id = IDs)
length(lns)
summary(gLength(lns, byid = TRUE))

GI <- gIntersects(lns, pols, byid = TRUE)
unname(which(GI[2, ]))
res <- numeric(length = nrow(GI))
for (i in seq(along = res)) {
  if (any(GI[i, ])) {
    resi <- gIntersection(lns[GI[i, ]], pols[i])
    res[i] <- gLength(resi)
    }
  }
olinda1$stream_len <- res

tree <- gBinarySTRtreeQuery(lns, pols)
tree[[2]]
res1 <- numeric(length = length(tree))
for (i in seq(along = res1)) {
  if (!is.null(tree[[i]])) {
    gi <- gIntersection(lns[tree[[i]]], pols[i])
    res1[i] <- ifelse(is.null(gi), 0, gLength(gi))
    }
  }
all.equal(res, res1)


uf50m <- gBuffer(lns, width = 50)
GI1 <- gIntersects(buf50m, pols, byid = TRUE)
res <- numeric(length = nrow(GI1))
for (i in seq(along = res)) {
  if (any(GI1[i, ])) {
    out <- gIntersection(buf50m[GI1[i, ]], pols[i])
    res[i] <- gArea(out)
    }
  }
olinda1$buf_area <- res
prop <- olinda1$buf_area/olinda1$area
olinda1$prop_50m <- prop

bounds <- gUnaryUnion(pols)
stream_inside <- gIntersection(lns, bounds)
bl <- colorRampPalette(brewer.pal(5, "Blues"))
tt <- "Prop. of area < 50m of streams"
spplot(olinda1, "prop_50m",
       col.regions = bl(20), col = "transparent",
       sp.layout = list("sp.lines", stream_inside), main = tt)






###################################################
### code chunk number 24: useR13_spatial_tutorial.Rnw:276-287
###################################################
library(DCluster)
grt <- sum(sm1$Count)/sum(sm1$Denmntr)
sm1$Expected <- grt*(sm1$Denmntr)
sm1$EB_SMR <- empbaysmooth(sm1$Count, sm1$Expected)$smthrr
if(!("INLA" %in% .packages(all = TRUE))) source("http://www.math.ntnu.no/inla/givemeINLA.R")
library(INLA)
sm1$AREAID <- 1:nrow(sm1)
pginla <- inla(Count ~ offset(log(Expected)) -1 + f(AREAID, model = "iid"), family = "poisson", data = as(sm1, "data.frame"), control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE))
sm1$PGINLA <- pginla$summary.fitted.values$mean/sm1$Expected
sm1$PGINLA_L <- pginla$summary.fitted.values[[3]]/sm1$Expected
sm1$PGINLA_U <- pginla$summary.fitted.values[[5]]/sm1$Expected
str(sm1, max.level=2)
summary(sm1)

###################################################
### code chunk number 25: useR13_spatial_tutorial.Rnw:291-296
###################################################
at <- seq(0.5, 1.9, 0.1)
pal <- c(rev(brewer.pal(5, "Blues")), brewer.pal(9, "Reds"))
spplot(sm1, "EB_SMR", at=at, col.regions=pal, main="ML empirical Bayes SMR")


###################################################
### code chunk number 26: useR13_spatial_tutorial.Rnw:306-309
###################################################
spplot(sm1, "PGINLA", at=at, col.regions=pal, main="INLA Poisson-Gamma SMR")


###################################################
### code chunk number 27: useR13_spatial_tutorial.Rnw:325-328
###################################################
spplot(sm1, c("PGINLA_L", "PGINLA_U"), at=at, col.regions=pal, main="INLA CIs")


###################################################
### code chunk number 28: useR13_spatial_tutorial.Rnw:372-373
###################################################
library(rgdal)


###################################################
### code chunk number 29: useR13_spatial_tutorial.Rnw:385-387
###################################################
olinda <- readOGR(".", "setor1")
names(olinda)


###################################################
### code chunk number 32: useR13_spatial_tutorial.Rnw:404-407
###################################################
spplot(olinda, "DEPRIV", col.regions=grey.colors(20, 0.9, 0.3))


###################################################
### code chunk number 34: useR13_spatial_tutorial.Rnw:668-671
###################################################
olinda$Expected <- olinda$POP * sum(olinda$CASES, na.rm=TRUE) / sum(olinda$POP, na.rm=TRUE)
str(olinda, max.level=2)
str(as(olinda, "data.frame"))


###################################################
### code chunk number 37: useR13_spatial_tutorial.Rnw:694-695
###################################################
str(model.frame(CASES ~ DEPRIV + offset(log(POP)), olinda), give.attr=FALSE)


###################################################
### code chunk number 39: useR13_spatial_tutorial.Rnw:760-762
###################################################
library(sp)
getClass("GridTopology")


###################################################
### code chunk number 40: useR13_spatial_tutorial.Rnw:809-810
###################################################
options("width"=100)


###################################################
### code chunk number 41: useR13_spatial_tutorial.Rnw:812-815
###################################################
DEM <- readGDAL("UTM_dem.tif")
summary(DEM)
is.na(DEM$band1) <- DEM$band1 < 1


###################################################
### code chunk number 44: useR13_spatial_tutorial.Rnw:833-836
###################################################
image(DEM, "band1", axes=TRUE, col=terrain.colors(20))


###################################################
### code chunk number 46: useR13_spatial_tutorial.Rnw:969-972
###################################################
proj4string(olinda)
EPSG <- make_EPSG()
EPSG[grep("Corrego Alegre", EPSG$note), 1:2]


###################################################
### code chunk number 49: useR13_spatial_tutorial.Rnw:993-1000
###################################################
set_ReplCRS_warn(FALSE)
proj4string(olinda) <- CRS("+init=epsg:22525")
proj4string(olinda)
proj4string(DEM)
proj4string(DEM) <- CRS("+init=epsg:31985")
proj4string(DEM)
set_ReplCRS_warn(TRUE)


###################################################
### code chunk number 52: useR13_spatial_tutorial.Rnw:1024-1025
###################################################
olinda1 <- spTransform(olinda, CRS(proj4string(DEM)))


###################################################
### code chunk number 55: useR13_spatial_tutorial.Rnw:1043-1047
###################################################
image(DEM, "band1", axes=TRUE, col=terrain.colors(20))
plot(olinda1, add=TRUE, lwd=0.5)


###################################################
### code chunk number 56: useR13_spatial_tutorial.Rnw:1220-1223 (eval = FALSE)
###################################################
## library(spgrass6)
## myGRASS <- "/home/rsb/topics/grass/g642/grass-6.4.2"
## initGRASS(myGRASS, tempdir(), SG=DEM, override=TRUE)


###################################################
### code chunk number 57: useR13_spatial_tutorial.Rnw:1248-1253 (eval = FALSE)
###################################################
## writeRAST6(DEM, "dem", flags="o")
## execGRASS("g.region", rast="dem")
## execGRASS("r.resamp.rst", input="dem", ew_res=14.25, ns_res=14.25, elev="DEM_resamp", slope="slope", aspect="aspect")
## execGRASS("g.region", rast="DEM_resamp")
## DEM_out <- readRAST6(c("DEM_resamp", "slope", "aspect"))


###################################################
### code chunk number 58: useR13_spatial_tutorial.Rnw:1257-1264
###################################################
DEM_out <- readGDAL("DEM_out.tif")
names(DEM_out) <- c("DEM_resamp", "slope", "aspect")
set_ReplCRS_warn(FALSE)
proj4string(DEM_out) <- CRS("+init=epsg:31985")
stream1 <- readOGR(".", "stream1")
proj4string(stream1) <- CRS("+init=epsg:31985")
set_ReplCRS_warn(TRUE)


###################################################
### code chunk number 59: useR13_spatial_tutorial.Rnw:1288-1290 (eval = FALSE)
###################################################
## execGRASS("r.watershed", elevation="DEM_resamp", stream="stream", threshold=1000L, convergence=5L, memory=300L)
## execGRASS("r.thin", input="stream", output="stream1", iterations=200L)


###################################################
### code chunk number 60: useR13_spatial_tutorial.Rnw:1310-1312 (eval = FALSE)
###################################################
## execGRASS("r.to.vect", input="stream1", output="stream", feature="line")
## stream1 <- readVECT6("stream")


###################################################
### code chunk number 61: useR13_spatial_tutorial.Rnw:1321-1326
###################################################
image(DEM_out, "DEM_resamp", col=terrain.colors(20), axes=TRUE)
plot(olinda1, add=TRUE, lwd=0.5)
plot(stream1, add=TRUE, col="blue")


###################################################
### code chunk number 62: useR13_spatial_tutorial.Rnw:1359-1363
###################################################
olinda1$area <- sapply(slot(olinda1, "polygons"), slot, "area")
km2 <- olinda1$area/1000000
olinda1$dens <- olinda1$POP/km2
library(RColorBrewer)


###################################################
### code chunk number 64: useR13_spatial_tutorial.Rnw:1374-1377
###################################################
spplot(olinda1, "dens", at=c(0, 8000, 12000, 15000, 20000, 60000), 
       col.regions=brewer.pal(6, "YlOrBr")[-1], main="Population density per square km", col="grey", lwd=0.5)


###################################################
### code chunk number 65: useR13_spatial_tutorial.Rnw:1391-1392
###################################################
library(rgeos)


###################################################
### code chunk number 66: useR13_spatial_tutorial.Rnw:1394-1398
###################################################
getScale()
pols <- as(olinda1, "SpatialPolygons")
Area <- gArea(pols, byid=TRUE)
all.equal(unname(Area), olinda1$area)


###################################################
### code chunk number 67: useR13_spatial_tutorial.Rnw:1419-1424
###################################################
lns0 <- as(stream1, "SpatialLines")
length(lns0)
summary(gLength(lns0, byid=TRUE))
t0 <- gTouches(lns0, lns0, byid=TRUE)
any(t0)


###################################################
### code chunk number 68: useR13_spatial_tutorial.Rnw:1444-1445
###################################################
library(spdep)


###################################################
### code chunk number 69: useR13_spatial_tutorial.Rnw:1447-1454
###################################################
lw <- mat2listw(t0)
is.symmetric.nb(lw$neighbours)
nComp <- n.comp.nb(lw$neighbours)
IDs <- as.character(nComp$comp.id)
lns <- gLineMerge(lns0, id=IDs)
length(lns)
summary(gLength(lns, byid=TRUE))


###################################################
### code chunk number 70: useR13_spatial_tutorial.Rnw:1474-1483
###################################################
GI <- gIntersects(lns, pols, byid=TRUE)
unname(which(GI[2,]))
res <- numeric(length=nrow(GI))
for (i in seq(along=res)) {
  if (any(GI[i,])) {
    resi <- gIntersection(lns[GI[i,]], pols[i])
    res[i] <- gLength(resi)
  }}
olinda1$stream_len <- res


###################################################
### code chunk number 71: useR13_spatial_tutorial.Rnw:1505-1515
###################################################
tree <- gBinarySTRtreeQuery(lns, pols)
tree[[2]]
res1 <- numeric(length=length(tree))
for (i in seq(along=res1)) {
  if (!is.null(tree[[i]])) {
    gi <- gIntersection(lns[tree[[i]]], pols[i])
    res1[i] <- ifelse(is.null(gi), 0, gLength(gi))
  }
}
all.equal(res, res1)


###################################################
### code chunk number 72: useR13_spatial_tutorial.Rnw:1536-1548
###################################################
buf50m <- gBuffer(lns, width=50)
GI1 <- gIntersects(buf50m, pols, byid=TRUE)
res <- numeric(length=nrow(GI1))
for (i in seq(along=res)) {
  if (any(GI1[i,])) {
    out <- gIntersection(buf50m[GI1[i,]], pols[i])
    res[i] <- gArea(out)
  }
}
olinda1$buf_area <- res
prop <- olinda1$buf_area/olinda1$area
olinda1$prop_50m <- prop


###################################################
### code chunk number 73: useR13_spatial_tutorial.Rnw:1574-1576
###################################################
bounds <- gUnaryUnion(pols)
stream_inside <- gIntersection(lns, bounds)


###################################################
### code chunk number 75: useR13_spatial_tutorial.Rnw:1588-1591
###################################################
spplot(olinda1, "prop_50m", col.regions=colorRampPalette(brewer.pal(5, "Blues"))(20), col="transparent", sp.layout=list("sp.lines", stream_inside), main="Prop. of area < 50m of streams")


###################################################
### code chunk number 76: useR13_spatial_tutorial.Rnw:1631-1637
###################################################
pan <- readGDAL("L8s.tif")
bb0 <- set_ReplCRS_warn(FALSE)
proj4string(pan) <- CRS("+init=epsg:31985")
bb0 <- set_ReplCRS_warn(TRUE)
brks <- quantile(pan$band1, seq(0,1,1/255))
pan$lut <- findInterval(pan$band1, brks, all.inside=TRUE)


###################################################
### code chunk number 78: useR13_spatial_tutorial.Rnw:1651-1658
###################################################
image(pan, "lut", col=grey.colors(20))
plot(olinda1, add=TRUE, border="brown", lwd=0.5)
title(main="Landsat panchromatic channel 15m")


###################################################
### code chunk number 79: useR13_spatial_tutorial.Rnw:1680-1688
###################################################
letm <- readGDAL("L_ETMps.tif")
bb0 <- set_ReplCRS_warn(FALSE)
proj4string(letm) <- CRS("+init=epsg:31985")
bb0 <- set_ReplCRS_warn(TRUE)
letm$ndvi <- (letm$band4 - letm$band3)/(letm$band4 + letm$band3)
library(RColorBrewer)
mypal <- brewer.pal(5, "Greens")
greens <- colorRampPalette(mypal)


###################################################
### code chunk number 81: useR13_spatial_tutorial.Rnw:1701-1709
###################################################
library(RColorBrewer)
greens <- colorRampPalette(brewer.pal(5, "Greens"))
image(letm, "ndvi", col=greens(20))
plot(olinda1, add=TRUE, lwd=0.5)


###################################################
### code chunk number 83: useR13_spatial_tutorial.Rnw:1753-1755
###################################################
olinda_ll <- spTransform(olinda1, CRS("+proj=longlat +datum=WGS84"))
writeOGR(olinda_ll, dsn="olinda_ll.kml", layer="olinda_ll", driver="KML", overwrite_layer=TRUE)


###################################################
### code chunk number 85: useR13_spatial_tutorial.Rnw:1787-1791
###################################################
library(maptools)
o_ndvi <- over(olinda1, letm[,,"ndvi"], fn=median)
o_alt <- over(olinda1, DEM_out[,,"DEM_resamp"], fn=median)
olinda1A <- spCbind(olinda1, cbind(o_ndvi, o_alt))
olinda2 <- olinda1A[!is.na(olinda1A$POP),]


###################################################
### code chunk number 86: useR13_spatial_tutorial.Rnw:1798-1801
###################################################
spplot(olinda2, "ndvi", col.regions=greens(20), main="Normalized difference vegetation index", col="transparent")


###################################################
### code chunk number 88: useR13_spatial_tutorial.Rnw:1826-1827
###################################################
library(raster)


###################################################
### code chunk number 89: useR13_spatial_tutorial.Rnw:1829-1832
###################################################
TMrs <- stack(letm)
e1 <- extract(TMrs, as(olinda2, "SpatialPolygons"))
all.equal(sapply(e1, function(x) median(x[,"ndvi"])), olinda2$ndvi)


###################################################
### code chunk number 91: useR13_spatial_tutorial.Rnw:1871-1873
###################################################
library(spdep)
ch <- choynowski(olinda2$CASES, olinda2$POP)


###################################################
### code chunk number 92: useR13_spatial_tutorial.Rnw:1880-1890
###################################################
cols <- rep("white", length(ch$pmap))
cols[(ch$pmap < 0.05) & (ch$type)] <- "grey35"
cols[(ch$pmap < 0.05) & (!ch$type)] <- "grey75"
plot(olinda2, col=cols)
title(main="Choynowski probability map")
legend("topleft", fill=c("grey35", "white", "grey75"), legend=c("low", "N/S", "high"), bty="n")


###################################################
### code chunk number 93: useR13_spatial_tutorial.Rnw:1906-1909
###################################################
pm <- probmap(olinda2$CASES, olinda2$POP)
names(pm)
olinda2$SMR <- pm$relRisk


###################################################
### code chunk number 94: useR13_spatial_tutorial.Rnw:1916-1931
###################################################
brks_prob <- c(0,0.05,0.1,0.2,0.8,0.9,0.95,1)
library(classInt)
fixed_prob <- classIntervals(pm$pmap, style="fixed", fixedBreaks=brks_prob)
library(RColorBrewer)
pal_prob <- rev(brewer.pal(5, "RdBu"))
cols <- findColours(fixed_prob, pal_prob)
plot(olinda2, col=cols)
title(main="Poisson probability map")
table <- attr(cols, "table")
legtext <- paste(names(table), " (", table, ")", sep="")
legend("topleft", fill=attr(cols, "palette"), legend=legtext, bty="n", cex=0.7, y.inter=0.8)


###################################################
### code chunk number 95: useR13_spatial_tutorial.Rnw:1947-1948
###################################################
table(findInterval(pm$pmap, seq(0,1,1/10)))


###################################################
### code chunk number 96: useR13_spatial_tutorial.Rnw:1955-1958
###################################################
hist(pm$pmap, breaks=8, col="grey", main="Poisson probability map")


###################################################
### code chunk number 97: useR13_spatial_tutorial.Rnw:1979-1982
###################################################
library(DCluster)
olinda2$RR <- olinda2$CASES/olinda2$Expected
olinda2$EB_ml <- empbaysmooth(olinda2$CASES, olinda2$Expected)$smthrr


###################################################
### code chunk number 99: useR13_spatial_tutorial.Rnw:1994-1997
###################################################
spplot(olinda2, c("RR", "EB_ml"), col.regions=brewer.pal(10, "RdBu"), at=c(seq(0,1,0.25), seq(1,6,1)))


###################################################
### code chunk number 100: useR13_spatial_tutorial.Rnw:2183-2185
###################################################
nb <- poly2nb(olinda2)
nb


###################################################
### code chunk number 101: useR13_spatial_tutorial.Rnw:2192-2198
###################################################
plot(olinda2, border="grey")
plot(nb, coordinates(olinda2), col="blue3", add=TRUE)


###################################################
### code chunk number 102: useR13_spatial_tutorial.Rnw:2214-2217
###################################################
olinda2$Observed <- olinda2$CASES
eb2 <- EBlocal(olinda2$Observed, olinda2$Expected, nb)
olinda2$EB_mm_local <- eb2$est


###################################################
### code chunk number 103: useR13_spatial_tutorial.Rnw:2224-2227
###################################################
spplot(olinda2, c("RR", "EB_ml", "EB_mm_local"), col.regions=brewer.pal(10, "RdBu"), at=c(seq(0,1,0.25), seq(1,6,1)))


###################################################
### code chunk number 104: useR13_spatial_tutorial.Rnw:2243-2246
###################################################
lw <- nb2listw(nb)
set.seed(130709)
moran.boot <- boot(as(olinda2, "data.frame"), statistic=moranI.boot, R=999, listw=lw, n=length(nb), S0=Szero(lw))


###################################################
### code chunk number 105: useR13_spatial_tutorial.Rnw:2253-2256
###################################################
plot(moran.boot)


###################################################
### code chunk number 106: useR13_spatial_tutorial.Rnw:2271-2272
###################################################
moran.pgboot <- boot(as(olinda2, "data.frame"), statistic=moranI.pboot, sim="parametric", ran.gen=negbin.sim, R=999, listw=lw, n=length(nb), S0=Szero(lw))


###################################################
### code chunk number 107: useR13_spatial_tutorial.Rnw:2279-2282
###################################################
plot(moran.pgboot)


###################################################
### code chunk number 109: useR13_spatial_tutorial.Rnw:2298-2299
###################################################
EBImoran.mc(olinda2$CASES, olinda2$Expected, lw, nsim=999)


###################################################
### code chunk number 112: useR13_spatial_tutorial.Rnw:2326-2333
###################################################
m0p <- glm(CASES ~ 1 + offset(log(Expected)), data=olinda2, family=poisson)
DeanB(m0p)
m0 <- glm(CASES ~ 1 + offset(log(Expected)), data=olinda2, family=quasipoisson)
m1 <- update(m0, . ~ . + DEPRIV)
m2 <- update(m1, . ~ . + ndvi)
m3 <- update(m2, . ~ . + DEM_resamp)
anova(m0, m1, m2, m3, test="F")


###################################################
### code chunk number 114: useR13_spatial_tutorial.Rnw:2356-2359
###################################################
olinda2$f1 <- fitted(m1)
olinda2$f2 <- fitted(m2)
olinda2$f3 <- fitted(m3)


###################################################
### code chunk number 116: useR13_spatial_tutorial.Rnw:2371-2374
###################################################
spplot(olinda2, c("CASES", "f1", "f2", "f3"), col.regions=brewer.pal(8, "Reds"), at=seq(0,40,5))


###################################################
### code chunk number 117: useR13_spatial_tutorial.Rnw:2396-2399
###################################################
olinda2$r1 <- residuals(m1, type="response")
olinda2$r2 <- residuals(m2, type="response")
olinda2$r3 <- residuals(m3, type="response")


###################################################
### code chunk number 119: useR13_spatial_tutorial.Rnw:2411-2414
###################################################
spplot(olinda2, c("r1", "r2", "r3"), col.regions=brewer.pal(8, "RdBu"), at=c(-30,-15,-10,-5,0,5,10,15,31))


###################################################
### code chunk number 120: useR13_spatial_tutorial.Rnw:2472-2478
###################################################
if(!("INLA" %in% .packages(all = TRUE))) source("http://www.math.ntnu.no/inla/givemeINLA.R")
library(INLA)
olinda2$INLA_ID <- 1:nrow(olinda2)
INLA_BYM <- inla(CASES ~ DEPRIV + ndvi + DEM_resamp + f(INLA_ID, model="bym",
                                                        graph=nb2mat(nb, style="B")) + offset(log(Expected)), family="poisson",
                 data=as(olinda2, "data.frame"), control.predictor=list(compute=TRUE))


###################################################
### code chunk number 122: useR13_spatial_tutorial.Rnw:2493-2494
###################################################
summary(INLA_BYM)


###################################################
### code chunk number 125: useR13_spatial_tutorial.Rnw:2516-2517
###################################################
olinda2$INLA <- exp(INLA_BYM$summary.linear.predictor[,1]-log(olinda2$Expected))


###################################################
### code chunk number 128: useR13_spatial_tutorial.Rnw:2530-2533
###################################################
spplot(olinda2, c("INLA"), col.regions=brewer.pal(10, "RdBu"), at=c(seq(0,1,0.25), seq(1,6,1)))


###################################################
### code chunk number 130: useR13_spatial_tutorial.Rnw:2552-2553
###################################################
library(CARBayes)


###################################################
### code chunk number 131: useR13_spatial_tutorial.Rnw:2555-2559 
###################################################
df <- as(olinda2, "data.frame")
attach(df)
obj <- poisson.bymCAR(CASES ~ DEPRIV + ndvi + DEM_resamp + offset(log(Expected)), 
                      W=nb2mat(nb, style="B"), n.sample=30000, burnin=20000, thin=10)
detach(df)


###################################################
### code chunk number 133: useR13_spatial_tutorial.Rnw:2564-2565
###################################################
olinda2$CARBayes <- obj$fitted.values[,1]/olinda2$Expected


###################################################
### code chunk number 138: useR13_spatial_tutorial.Rnw:2607-2610
###################################################
library(lattice)
splom(as(olinda2, "data.frame")[,c("RR", "INLA", "CARBayes")])

###############################################################################

library(MASS)
data <- rnorm(100)
fit <- fitdistr(data, 'Normal')
fit
names(fit)
fit$estimate[1]
fit$estimate[2]
x <- fit$estimate[1] + 2*fit$estimate[2]
x
# Eliminar atributos de un elemento
attributes(x) <- c()
x
y <- fit
y
names(y)

names(y) <- c()
names(y)
y
y <- fit
y
dimnames(y)
?dimnames

#  Learn unclass().

# Here is an example of fitting a linear model (from the help(glm) documentation)
# Dobson (1990) Page 93: Randomized Controlled Trial :
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())

# Want to get the model coefficients and don't feel like suffering through the 
# documentation/help system? You can't inspect the glm.D93 object because it 
# has overridden the print() and summary() methods to hide details (in 
# particular you can't find the member data). No problem, type this:

glm.D93
model <- unclass(glm.D93)

names(model)
names(glm.D93)

# The model is now a harmless list without a bunch of pesky methods hiding the information.
model

# learn how to list class and methods.

# Often one of methods(), showMethods() or getS3Method() can show you what 
# methods are on a class or object. Be prepared to try them all as they apply 
# in different contexts.

# lets make a tricky function
fe <- function(x) UseMethod("fe")
fe.formula <- function(x) { print('formula')}
fe.numeric <- function(x) { print('numeric')}

# How will anyone figure out what we have done?

class(fe)
[1] "function"

methods(fe)
# [1] fe.formula fe.numeric

getS3method('fe','numeric')
# fe.numeric <- function(x) { print('numeric')}

# Learn to stomp out attributes.

# Ever have this crud follow you around?

m <- summary(c(1,2))[4]
m
# Mean 
# 1.5 

# Ah that's cute: a little "Mean" tag is following the data around. But what if we try to use this value:
  
m*m
# Mean 
# 2.25 

# Okay, now the "Mean" tag has outstayed its welcome. The fix:
  
attributes(m) <- c()
m*m
[1] 2.25

# MUCH better. 




## Your Data is Never the Right Shape
## http://www.win-vector.com/blog/2011/07/your-data-is-never-the-right-shape/

# First let us enter some example data:
d <- data.frame(id=c(1,2,3,1,2,3), score1=c(17,5,6,10,13,7), score2=c(13,10,5,13,10,5))

# This gives us our example data. Each row is numbered (1 through 6) has an id and both our scores:
d

# We said our only task was to characterize how well score2 works at predicting 
# score1 (or how good a substitute score2 is for score1). We could compute 
# correlation, RMS error, info-gain or some such. But instead lets look at this 
# graphically. We will prepare a graph showing how well score1 is represented by 
# score2. For this we choose to place score1 on the y-axis (as it is the outcome) 
# and score2 on the x-axis (as it is the driver).

library(ggplot2)
ggplot(d) + geom_point(aes(x=score2,y=score1))

# This does not look good. We would liked to have seen all of the dots falling 
# on the line "y=x." This plot shows score2 is not predicting score1 very well. 
# Part of this is that we missed an important feature of the data (and because 
# we missed it the feature becomes a problem): the ids repeat. First we re-order 
# by id to make this more obvious.

dsort <- d[order(d$id),]
dsort
d

# The easiest (on us) way do fix up our data is to make the computer work hard 
# and use the powerful melt/cast technique. These functions are found in the 
# libraries reshape and plyr (which were automatically loaded with we loaded ggplot2 library).

# melt is a bit abstract. What it does convert your data into a "narrow" 
# format where rows are split into many rows each carrying just one result column 
# of the original row. For example we can melt our data by id as follows:

library(plyr)
library(reshape)
dmelt <- melt(d, id.vars=c('id'))
dmelt

# Each of the two facts (score1, score2) from our original row is split into its 
# own row. The id column plus the new variable column are now considered to be keys. 
# This format is not used directly but used because it is easy to express important 
# data transformations in terms of it. For instance we wanted our table to have 
# duplicate rows collected and score1 replaced by its average (to attempt to remove 
# the unexplainable variation). That is now easy:

dmean <- cast(dmelt, fun.aggregate=mean)
dmean
d

# We used cast() in its default mode, where it assumes all columns not equal to 
# "value" are the keyset. It then collects all rows with identical keying and 
# combines them back into wide rows using mean or average as the function to 
# deal with duplicates. Notice score1 is now the desired average, and score2 is 
# as before (as it was a function of the keys or inputs, so it is not affected 
# by averaging). With this new smaller data set we can re-try our original graph:

ggplot(dmean) + geom_point(aes(x=score2,y=score1))

# As before will will just make a graph, but the data preparation steps would be 
# exactly the same if we were using a quantitive summary (like correlation, or 
# any other). And, of course, our data is still not the right shape for this 
#step. Luckily there is another tool ready to fix this: join().

dsort
djoin <- join(dsort, dsort,'id')
djoin

fixnames <- function(cn) {
  n <- length(cn);
  for(i in 2:((n+1)/2)) { cn[i] <- paste('a',cn[i],sep='') };
  for(i in ((n+3)/2):n) { cn[i] <- paste('b',cn[i],sep='') };
  cn
}

colnames(djoin)
colnames(djoin) <- fixnames(colnames(djoin))
djoin

# With this table we can now plot how score1 varies for rows with the same id:

ggplot(djoin) + geom_point(aes(x=ascore1,y=bscore1))


d$rowNumber <- 1:(dim(d)[1])
djoin <- join(d,d,'id')
colnames(djoin) <- fixnames(colnames(djoin))
djoin <- djoin[djoin$arowNumber!=djoin$browNumber,]
# This gives us a table that shows only values of score1 from different rows:
djoin
# And only plots points on the diagonal if "you have really earned them":

###############################################################################
## http://biostatmatt.com/page/11
# convert degree, minute, second to decimal
dms2dec <- function(deg, min, sec) deg + (min + sec/60)/60

# convert degrees to radians
d2r <- function(d) d * pi / 180

# compute great circle distance (km) between two coordinates
# given in radians using the Haversine formula
hgcd <- function(lon1, lat1, lon2, lat2) {
  erth <- 6371 # average radius of earth (km)
  dlon <- (lon2 - lon1)
  dlat <- (lat2 - lat1)
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  erth * c
}
###############################################################################

# Given a particular area say 500sq units .. I need to generate random shapes for a particular area

# The area of an n-sided regular polygon, with a circumscribed circle of radius R is
# A = 1/2 nR^2 * sin((2pi)/n)
# Therefore, knowing n and A you can easily find R
# R = sqrt((2*A)/(n*sin((2pi)/n))
# So, you can pick the center, go at distance R and generate n points at 2pi/n angle increments.

regular.poly <- function(nSides, area)
{
  # Find the radius of the circumscribed circle
  radius <- sqrt((2*area)/(nSides*sin((2*pi)/nSides)))
  
  # I assume the center is at (0;0) and the first point lies at (0; radius)
  points <- list(x=NULL, y=NULL)
  angles <- (2*pi)/nSides * 1:nSides
  
  points$x <- cos(angles) * radius
  points$y <- sin(angles) * radius
  
  return (points);
}


# Some examples
par(mfrow=c(3,3))

for (i in 3:11)
{
  p <- regular.poly(i, 100)
  plot(0, 0, "n", xlim=c(-10, 10), ylim=c(-10, 10), xlab="", ylab="", main=paste("n=", i))
  polygon(p)
}

# We can extrapolate to a generic convex polygon.
# The area of a convex polygon can be found as: 
# A = 1/2 * [(x1*y2 + x2*y3 + ... + xn*y1) - (y1*x2 + y2*x3 + ... + yn*x1)]
# We generate the polygon as above, but deviate angles and radii from those of the regular polygon.
# We then scale the points to get the desired area.

convex.poly <- function(nSides, area)
{
  # Find the radius of the circumscribed circle, and the angle of each point if this was a regular polygon
  radius <- sqrt((2*area)/(nSides*sin((2*pi)/nSides)))
  angle <- (2*pi)/nSides
  
  # Randomize the radii/angles
  radii <- rnorm(nSides, radius, radius/10)
  angles <- rnorm(nSides, angle, angle/10) * 1:nSides
  angles <- sort(angles)
  
  points <- list(x=NULL, y=NULL)
  points$x <- cos(angles) * radii
  points$y <- sin(angles) * radii
  
  # Find the area of the polygon
  m <- matrix(unlist(points), ncol=2)
  m <- rbind(m, m[1,])
  current.area <- 0.5 * (sum(m[1:nSides,1]*m[2:(nSides+1),2]) - sum(m[1:nSides,2]*m[2:(nSides+1),1]))
  
  points$x <- points$x * sqrt(area/current.area)
  points$y <- points$y * sqrt(area/current.area)
  
  return (points)
}

###############################################################################

# I have a cloud of points scattered in a 2D Euclidean space. I would like to 
# calculate the area inside the polygon linking the most extreme (=peripheral) 
# points of the cloud. In other words, I would like to estimate the area covered 
# by the cloud in this space. Is there a formula in R? Thanks a lot for any response Julien

# This is called the convex-hull problem; R built-in chull function should do the work. 
# To count area, you may use a formula from here.

# Even better; splancs package has areapl function. So the function solving your problem should look like this:
  
cha<-function(x,y){
  chull(x,y)->i
  return(areapl(cbind(x[i],y[i])))
}

# For instance:
  
library(splancs);
x <- rnorm(20); rnorm(20)->y;

# Some visualization
i <- chull(x,y) 
plot(x,y)
polygon(x[i],y[i]);

# The area
cha(x,y);

###############################################################################

## Convex hull in Linear Discriminant Analysis in R
## http://stackoverflow.com/questions/20002699/convex-hull-in-linear-discriminant-analysis-in-r

library(MASS)
library(ggplot2)
library(gridExtra)
library(data.table)
library(plyr)


Raw <- data.frame(V1 = rnorm(100), V2 = rnorm(100),V3 = rnorm(100),V4 = rnorm(100), Group = sample(letters[1:5], 100, rep = T))
my.lda<-lda(Group ~ V1+V2+V3+V4, data = Raw )

LD1<-predict(my.lda)$x[,1]
LD2<-predict(my.lda)$x[,2]
Group=predict(my.lda)$class
dt<-data.table(Group,LD1,LD2)

#LDA plot
p1<-ggplot(Raw, aes(x=LD1, y=LD2, col=Raw$Group))+ geom_point(size = 5,aes(color = Raw$Group)) 


#Convex hull plot
find_hull <- function(dt) dt[chull(dt$LD1, dt$LD2), ]
hulls2 <- ddply(dt , "Group", find_hull)

p2<-ggplot(dt, aes(x=LD1, y=LD2, color = Group, fill=Group)) + geom_point(size = 5) + geom_polygon(data=hulls2, alpha=.2) 

#plot p1 and p2 in one graph
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend <- g_legend(p1)
lwidth <- sum(legend$width)

p3 <- grid.arrange(arrangeGrob(p1  + theme(legend.position="none"),
                               p2  + theme(legend.position="none"),
                               ncol=2), legend ,
                   widths=unit.c(unit(1, "npc") - lwidth, lwidth), nrow=1)

library(ggplot2)
library(data.table)
# You have to set the seed _before_ you generate random data, not after
set.seed(1) 
dt <- data.table(xdata=runif(15), ydata=runif(15), level=rep(c("a","b","c"), each=5),
                 key="level")

# Here is where the magic happens:
  
hulls <- dt[, .SD[chull(xdata, ydata)], by = level]

# Plotting the result:
ggplot(dt,aes(x=xdata,y=ydata,color=level)) +
geom_point() +
geom_polygon(data = hulls,aes(fill=level,alpha = 0.5))



###############################################################################
# R programming
# https://class.coursera.org/rprog-002

m <- 1:10
m
dim(m) <- c(2,5)
m

# Subsetting
x <- c(3,5,1,10,12,6)
x
x[x<6]==0

x <- matrix(1:6, 2, 3)
x[1,2]
x[1,2, drop=FALSE]

# Removing NA values
x <- c(1,2,NA,4,NA,6)
bad <- is.na(x)
bad
x[!bad]

# Reading Larger Datasets
initial <- read.table("datatable.txt", nrows=100)
classes <- sapply(initial, class)
tabAll <- read.table("datatable.txt", colClasses=classes)

# dump and dput
y <- data.frame(a=1, b="a")
dput(y)
dput(y, file="y.R")
new.y <- dget("y.R")
new.y

x <- "foo"
y <- data.frame(a=1, b="a")
dump(c("x", "y"), file="data.R")
rm(x, y)
source("data.R")
y
x

# File connections
str(file)

# Conections

con <- url("http://www.jhsph.edu", "r")
str(con)
x <- readLines(con)
head(x)
close(con)

# Control Structures: if

if(<condition>) {
  ## do something
} else {
  ## do something else
}
if(<condition1>) {
  ## do something
} else if(<condition2>)  {
  ## do something different
} else {
  ## do something different
}

# This is a valid if/else structure.

if(x > 3) {
  y <- 10
} else {
  y <- 0
}

# So is this one.

y <- if(x > 3) {
  10
} else { 
  0
}

# for
# These three loops have the same behavior.

x <- c("a", "b", "c", "d")

for(i in 1:4) {
  print(x[i])
}

for(i in seq_along(x)) {
  print(x[i])
}

for(letter in x) {
  print(letter)
}

for(i in 1:4) print(x[i])

# Nested for loops
# for loops can be nested.

x <- matrix(1:6, 2, 3)
x
# Print all elements of a matrix
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}

# while
# While loops begin by testing a condition. If it is true, then they execute 
# the loop body. Once the loop body is executed, the condition is tested again, 
# and so forth.

count <- 0
while(count < 10) {
  print(count)
  count <- count + 1
}


# while
# Sometimes there will be more than one condition in the test.

# random walk
z <- 5
while(z >= 3 && z <= 10) {
  print(z)
  coin <- rbinom(1, 1, 0.5)
  
  if(coin == 1) {  ## random walk
    z <- z + 1
  } else {
    z <- z - 1
  } 
}


# repeat
# Repeat initiates an infinite loop; these are not commonly used in statistical applications but they do have 
# their uses. The only way to exit a repeat loop is to call break.

x0 <- 1
tol <- 1e-8
repeat {
  x1 <- computeEstimate()
  
  if(abs(x1 - x0) < tol) {
    break
  } else {
    x0 <- x1
  } 
}

# Arguments of a funtion
args(lm)

# Formals of a funtion
formals(lm)

# Lazy Evaluation
# Arguments to functions are evaluated lazily, so they are evaluated only as needed.

f <- function(a, b) {
  a^2
}
f(2)

f <- function(a, b) {
  print(a)
  print(b)
}
f(45)

## [1] 45

## Error: argument "b" is missing, with no default

# Notice that "45" got printed first before the error was triggered. This is because 
# b did not have to be evaluated until after print(a). Once the function tried to 
# evaluate print(b) it had to throw an error.

add2 <- function(x, y) {
  x+y
}

above10 <- function(x) {
  use <- x>10
  x[use]
}

above <- function(x, n=10) {
  use <- x>n
  x[use]
}

above(c(1,15,10,2,6,7), 2)
y <- 1:30
above(y, 4)
above(y)

# Function to calcule means of every column in a matrix
columnmean <- function(y, removeNA=TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for (i in 1:nc) {
    means[i] <- mean(y[, i], na.rm=removeNA)
  }
  means
}

columnmean(airquality)
columnmean(airquality, FALSE)

# The search list can be found by using the search function.

search()


# Lexical Scoping

make.power <- function(n) {
      pow <- function(x) {
            x^n 
      }
      pow 
}

# This function returns another function as its value.

cube <- make.power(3)
square <- make.power(2)
cube(3)
# [1] 27
square(3)
# [1] 9

# What's in a function's environment?

ls(environment(cube))
# [1] "n"   "pow"
get("n", environment(cube))
# [1] 3

ls(environment(square))
# [1] "n"   "pow"
get("n", environment(square))
# [1] 2


# Lexical vs. Dynamic Scoping

# When a function is defined in the global environment and is subsequently called 
# from the global environment, then the defining environment and the calling 
# environment are the same. This can sometimes give the appearance of dynamic scoping.

g <- function(x) { 
      a <- 3
      x+a+y 
      }
g(2)
# Error in g(2) : object "y" not found
y <- 3
g(2)
# [1] 8


# Vectorized Matrix Operations

x <- matrix(1:4, 2, 2); y <- matrix(rep(10, 4), 2, 2)
x
y
x * y       ## element-wise multiplication
[,1] [,2]
[1,]   10   30
[2,]   20   40
x / y
[,1] [,2]
[1,]  0.1  0.3
[2,]  0.2  0.4
x %*% y     ## true matrix multiplication
[,1] [,2]
[1,]   40   40
[2,]   60   60

# Sequences of numbers
pi:10
15:1
?`:`
seq(0,10,by=0.5)
seq(5,10,length=30)
seq(along = my_seq)
seq_along(my_seq)
rep(c(0, 1, 2), times = 10)
Try rep(c(0, 1, 2), each = 10)

# Vectors
paste(LETTERS, 1:4, sep = "-")

# Missing values
y <- rnorm(1000)
z <- rep(NA, 1000)
myData <- sample(c(y, z), 100)
myNA <- is.na(myData)
myNA
myData == NA

# The reason you got a vector of all NAs is that NA is not really a value, but just a placeholder for a quantity that is not
# available. Therefore the logical expression is incomplete and R has no choice but to return a vector of the same length as myData
# that contains all NAs.

# R represents TRUE as the number 1 and FALSE as the number 0. Therefore, if
# we take the sum of a bunch of TRUEs and FALSEs, we get the total number of TRUEs.
sum(myNA)

# Subsetting Vectors
# Recall that `!` gives us the negation of a logical expression, so !is.na(x) can be read as 'is not NA'. Therefore, if we want to
# create a vector called y that contains all of the non-NA values from x, we can use y <- x[!is.na(x)].
y <- x[!is.na(x)]

# we request only values of x that are both non-missing AND greater than zero
x[!is.na(x) & x > 0]

x[c(-2, -10)]
x[-c(2, 10)]

identical(vect, vect2)

# Matrices and Data Frames

myVector <- 1:20
dim(myVector)
length(myVector)
dim(myVector) <- c(4, 5)
dim(myVector)
attributes(myVector)
class(myVector)
myMatrix2 <- matrix(c(1:20), nrow=4, ncol=5)
patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, myMatrix2)
# matrices can only contain ONE class of data. Therefore, when | we tried to combine a character vector with a numeric 
# matrix, R was forced to 'coerce' the numbers to characters, hence the double quotes.

myData <- data.frame(patients, myMatrix)
myData
cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(myData) <- cnames
myData

?'`'
?'('
?'['
?'...'
?'+'
?'%*%'
?'%x%'
?'%o%'
?'%%'
?'%/%'
?'$'
?'^'
?'~'
?'<-'
?'='
?'<<-'

# lapply
# lapply takes three arguments: (1) a list X; (2) a function (or the name of a 
# function) FUN; (3) other arguments via its ... argument. If X is not a list, 
# it will be coerced to a list using as.list.

lapply
## function (X, FUN, ...) 
## {
##     FUN <- match.fun(FUN)
##     if (!is.vector(X) || is.object(X)) 
##         X <- as.list(X)
##     .Internal(lapply(X, FUN))
## }
## <bytecode: 0x7ff7a1951c00>
## <environment: namespace:base>


# lapply
# lapply always returns a list, regardless of the class of the input.

x <- list(a = 1:5, b = rnorm(10))
x
lapply(x, mean)

x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
x
lapply(x, mean)

x <- 1:4
x
# Generate 1:4 random variables
lapply(x, runif)

x <- 1:4
lapply(x, runif, min = 0, max = 10)

# lapply and friends make heavy use of anonymous functions.
x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2)) 
x

# An anonymous function for extracting the first column of each matrix.
lapply(x, function(elt) elt[,1])

# sapply will try to simplify the result of lapply if possible.
# If the result is a list where every element is length 1, then a vector is returned
# If the result is a list where every element is a vector of the same length (> 1), a matrix is returned.
# If it can't figure things out, a list is returned


# sapply
x <- list(a = 1:4, b = rnorm(10), c = rnorm(20, 1), d = rnorm(100, 5))
x
lapply(x, mean)
sapply(x, mean) 
mean(x)

# apply
# apply is used to a evaluate a function (often an anonymous one) over the margins of an array.
# It is most often used to apply a function to the rows or columns of a matrix
# It can be used with general arrays, e.g. taking the average of an array of matrices
# It is not really faster than writing a loop, but it works in one line!
      
str(apply)
function (X, MARGIN, FUN, ...)
      
# X is an array
# MARGIN is an integer vector indicating which margins should be "retained".
# FUN is a function to be applied
# ... is for other arguments to be passed to FUN

x <- matrix(rnorm(200), 20, 10)
x
# Mean of each column
apply(x, 2, mean)
# Sum of each row
apply(x, 1, sum)

# For sums and means of matrix dimensions, we have some shortcuts.
rowSums = apply(x, 1, sum)
rowMeans = apply(x, 1, mean)
colSums = apply(x, 2, sum)
colMeans = apply(x, 2, mean)

# The shortcut functions are much faster, but you won't notice unless you're using a large matrix.

# Quantiles of the rows of a matrix.
x <- matrix(rnorm(200), 20, 10)
x
apply(x, 1, quantile, probs = c(0.25, 0.75))

# Average matrix in an array
# a is a 3 dimension matrix 2,2,10
a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
a
summary(a)
# Mean of a matrix in an array
apply(a, c(1, 2), mean)
rowMeans(a, dims = 2)

# tapply is used to apply a function over subsets of a vector. 
str(tapply)
# function (X, INDEX, FUN = NULL, ..., simplify = TRUE)
# X is a vector
# INDEX is a factor or a list of factors (or else they are coerced to factors)
# FUN is a function to be applied
# ... contains other arguments to be passed FUN
# simplify, should we simplify the result?

# Take group means.

x <- c(rnorm(10), runif(10), rnorm(10, 1))
x
# Generate a vector with 3 levels (1:3), repeated 10 times each
f <- gl(3, 10)
f
tapply(x, f, mean)

# Take group means without simplification.
tapply(x, f, mean, simplify = FALSE)

# Find group ranges.
tapply(x, f, range)

# split
# split takes a vector or other objects and splits it into groups determined by a factor or list of factors.
str(split)
# function (x, f, drop = FALSE, ...)
# x is a vector (or list) or data frame
# f is a factor (or coerced to one) or a list of factors
# drop indicates whether empty factors levels should be dropped

x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
x
f
split(x, f)

# A common idiom is split followed by an lapply.
lapply(split(x, f), mean)

# Splitting a Data Frame
library(datasets)
head(airquality)
airquality
# Separate data frame by Month
s <- split(airquality, airquality$Month)
s
lapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")]))

sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")])) 
sapply(s, function(x) colMeans(x[, c("Ozone", "Solar.R", "Wind")],
                                 na.rm = TRUE))

# Splitting on More than One Level
x <- rnorm(10)
f1 <- gl(2, 5)
f2 <- gl(5, 2)
x
f1
f2
interaction(f1, f2)

# Interactions can create empty levels.
str(split(x, list(f1, f2)))
split(x, list(f1, f2))

# Empty levels can be dropped.
str(split(x, list(f1, f2), drop = TRUE))

# mapply
# mapply is a multivariate apply of sorts which applies a function in parallel over a set of arguments.

str(mapply)
# function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
#           USE.NAMES = TRUE)
# FUN is a function to apply
# ... contains arguments to apply over
# MoreArgs is a list of other arguments to FUN.
# SIMPLIFY indicates whether the result should be simplified

# The following is tedious to type
list(rep(1, 4), rep(2, 3), rep(3, 2), rep(4, 1))

# Instead we can do
mapply(rep, 1:4, 4:1)

# Vectorizing a Function
noise <- function(n, mean, sd) {
      rnorm(n, mean, sd)
      }
noise(5, 1, 2)
noise(1:5, 1:5, 2)

# Instant Vectorization
mapply(noise, 1:5, 1:5, 2)

# Which is the same as
list(noise(1, 1, 2), noise(2, 2, 2), noise(3, 3, 2), noise(4, 4, 2), 
     noise(5, 5, 2))

# Indications that something's not right

# message: A generic notification/diagnostic message produced by the message function; execution of the function continues
# warning: An indication that something is wrong but not necessarily fatal; execution of the function continues; generated by the warning function
# error: An indication that a fatal problem has occurred; execution stops; produced by the stop function
# condition: A generic concept for indicating that something unexpected can occur; programmers can create their own conditions

# Warning
log(-1)

printmessage <- function(x) {
      if(x > 0)
            print("x is greater than zero")
      else
            print("x is less than or equal to zero")
      invisible(x)
}

printmessage(1)
printmessage(NA)

printmessage2 <- function(x) {
      if(is.na(x))
            print("x is a missing value!")
      else if(x > 0)
            print("x is greater than zero")
      else
            print("x is less than or equal to zero")
      invisible(x)
}

x <- log(-1)
printmessage2(x)

# traceback
mean(x)
# Error in mean(x) : object 'x' not found
traceback()

lm(y ~ x)
# Error in eval(expr, envir, enclos) : object 'y' not found
traceback()

# debug
debug(lm)
lm(y ~ x)

# Recover
options(error = recover)
read.csv("nosuchfile")

# Generating Random Numbers
# Working with the Normal distributions requires using these four functions

dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)

# The sample function draws randomly from a specified set of (scalar) objects 
# allowing you to sample from arbitrary distributions.

set.seed(1)
sample(1:10, 4)
[1] 3 4 5 7
sample(1:10, 4)
[1] 3 9 8 5
sample(letters, 5)
[1] "q" "b" "e" "x" "p"
sample(1:10)  ## permutation
[1] 4 710 6 9 2 8 3 1 5 
sample(1:10)
[1]  2  3  4  1  9  5 10  8  6  7
sample(1:10, replace = TRUE)  ## Sample w/replacement
[1] 2 9 7 8 2 8 5 9 7 8

system.time()

## Elapsed time > user time
system.time(readLines("http://www.jhsph.edu"))
user  system elapsed 
0.004   0.002   0.431 

## Elapsed time < user time
hilbert <- function(n) { 
      i <- 1:n
      1 / outer(i - 1, i, "+")
}
x <- hilbert(1000)
system.time(svd(x))

# Timing Longer Expressions
system.time({
      n <- 1000
      r <- numeric(n)
      for (i in 1:n) {
            x <- rnorm(n)
            r[i] <- mean(x)
      }
})

###############################################################################

# I'd like to have the same width of all values which means for me to fill the values with zeros like this:

# "name_001"  "name_026"  "name_051"  "name_076"  "name_101"

# There are several solutions to this.

# One of them is to use sprintf. This uses C style formatting codes embedded in
# a character string to indicate the format of any other arguments passed to it. 
# For example, the formatting code %3d means format a number as integer of width 3:

a <- seq(1,101,25)
sprintf("name_%03d", a)
[1] "name_001" "name_026" "name_051" "name_076" "name_101"

# Another is formatC and paste:

paste("name", formatC(a, width=3, flag="0"), sep="_")
[1] "name_001" "name_026" "name_051" "name_076" "name_101"


# Dates are represented by the Date class and can be coerced from a character 
# string using the as.Date() function.

x <- as.Date("1970-01-01")
x
## [1] "1970-01-01"
unclass(x)
## [1] 0
unclass(as.Date("1970-01-02"))
## [1] 1

# Times can be coerced from a character string using the as.POSIXlt or as.POSIXct function.

x <- Sys.time()
x
## [1] "2013-01-24 22:04:14 EST"
class(x)
p <- as.POSIXlt(x)
p
names(unclass(p))
## [1] "sec"   "min"   "hour"  "mday"  "mon"
## [6] "year"  "wday"  "yday"  "isdst"
p$sec
class(p)
## [1] 14.34
q <- as.POSIXct(x)
q
names(q)
class(q)

# You can also use the POSIXct format.
x <- Sys.time()
x  ## Already in 'POSIXct' format
## [1] "2013-01-24 22:04:14 EST"
unclass(x)
## [1] 1359083054
x$sec
## Error: $ operator is invalid for atomic vectors
p <- as.POSIXlt(x)
p$sec
## [1] 14.37

# Finally, there is the strptime function in case your dates are written in a different format

datestring <- c("Enero 10, 2012 10:40", "Diciembre 9, 2011 09:10")
x <- strptime(datestring, "%B %d, %Y %H:%M")
x
## [1] "2012-01-10 10:40:00" "2011-12-09 09:10:00"
class(x)
## [1] "POSIXlt" "POSIXt"
                
# I can never remember the formatting strings. Check ?strptime for details.

# You can use mathematical operations on dates and times. Well, really just + 
# and -. You can do comparisons too (i.e. ==, <=)

x <- as.Date("2012-01-01")
y <- strptime("9 Ene 2011 11:34:21", "%d %b %Y %H:%M:%S") 
x-y
## Warning: Incompatible methods ("-.Date",
## "-.POSIXt") for "-"
## Error: non-numeric argument to binary operator
x <- as.POSIXlt(x) 
x-y
x
y
## Time difference of 356.3 days

# Even keeps track of leap years (años bisiestos), leap seconds, daylight savings, and time zones.

x <- as.Date("2012-03-01")
y <- as.Date("2012-02-28") 
x-y
## Time difference of 2 days
x <- as.POSIXct("2012-10-25 01:00:00")
y <- as.POSIXct("2012-10-25 06:00:00", tz = "GMT") 
y-x
## Time difference of 1 hours
as.numeric(y-x)

d1 <- date()
d1
# [1] "Sun Jan 12 17:48:33 2014"
class(d1)

d2 <- Sys.Date()
d2
# [1] "2014-01-12"
class(d2)

# Formatting dates
# %d = day as number (0-31), %a = abbreviated weekday,%A = unabbreviated weekday, 
# %m = month (00-12), %b = abbreviated month, %B = unabbrevidated month, 
# %y = 2 digit year, %Y = four digit year

format(d2,"%a %b %d")
# [1] "Sun Jan 12"


# Creating dates
x <- c("1ene1960", "2ene1960", "31mar1960", "30jul1960") 
z = as.Date(x, "%d%b%Y")
x
z
# [1] "1960-01-01" "1960-01-02" "1960-03-31" "1960-07-30"

z[1] - z[2]
# Time difference of -1 days

as.numeric(z[1]-z[2])
# [1] -1

# Converting to Julian
weekdays(d2)
[1] "Sunday"

months(d2)
[1] "January"

julian(d2)
[1] 16082
attr(,"origin")
[1] "1970-01-01"


# Lubridate

library(lubridate)
ymd("20140108")
[1] "2014-01-08 UTC"

mdy("08/04/2013")
[1] "2013-08-04 UTC"

dmy("03-04-2013")
[1] "2013-04-03 UTC"

# Dealing with times

ymd_hms("2011-08-03 10:15:03")
[1] "2011-08-03 10:15:03 UTC"

ymd_hms("2011-08-03 10:15:03", tz="Pacific/Auckland")
[1] "2011-08-03 10:15:03 NZST"
ymd_hms("2011-08-03 10:15:03", tz="CET")

?Sys.timezone

# Some functions have slightly different syntax

x <- dmy(c("1ene2013", "2ene2013", "31mar2013", "30jul2013"))
wday(x[1])

# [1] 3

wday(x[1], label=TRUE)

setwd("E:/Varios/R/Archivos/Coursera/Getting and Cleaning Data")
df <- read.csv("getdata_data_ss06hid.csv")

table(df$VAL)

# if(!file.exists("data")){dir.create("data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./gDATA.gov_NGAP.xlsx",method="curl")
dateDownloaded <- date()

library(xlsx)
colIndex <- 7:15
rowIndex <- 18:23
# dat <- read.xlsx("./getdata_data_DATA.gov_NGAP.xlsx",sheetIndex=1,header=TRUE)
dat <- read.xlsx("./getdata_data_DATA.gov_NGAP.xlsx", sheetIndex=1, header=TRUE,
                 colIndex=colIndex, rowIndex=rowIndex)
head(dat)

sum(dat$Zip*dat$Ext,na.rm=T) 


# Read the file into R

library(XML)
# fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
fileUrl4<-"http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl4, useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)

[1] "breakfast_menu"

names(rootNode)

xpathSApply(rootNode,"//name",xmlValue)
zipcode <- xpathSApply(rootNode,"//zipcode",xmlValue)
table(zipcode)

library(data.table)
df2 <- read.csv("getdata_data_ss06pid.csv")
names(df2)
DT = data.table(df2)
tables()

getwd()

# List files from a directory
list.files("./data")

# file.exists("directoryName") will check to see if the directory exists
# dir.create("directoryName") will create a directory if it doesn't exist

# Create a subdirectory data if not exists
if (!file.exists("data")) {
      dir.create("data")
}

# Download a file from the web
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv")
list.files("./data")

## [1] "cameras.csv"

dateDownloaded <- date()
dateDownloaded

cameraData <- read.table("./data/cameras.csv", sep = ",", header = TRUE)
head(cameraData)

# read.csv sets sep="," and header=TRUE

cameraData <- read.csv("./data/cameras.csv")
head(cameraData)

# Read excel files
if(!file.exists("data")){dir.create("data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.xlsx")
dateDownloaded <- date()
library(xlsx)
cameraData <- read.xlsx("./data/cameras.xlsx",sheetIndex=1,header=TRUE)
head(cameraData)

# Reading specific rows and columns
colIndex <- 2:3
rowIndex <- 1:4
cameraDataSubset <- read.xlsx("./data/cameras.xlsx", sheetIndex=1,
                              colIndex=colIndex, rowIndex=rowIndex)
cameraDataSubset

# The XLConnect package has more options for writing and manipulating Excel files


# XML. Tags, elements and attributes

# Tags correspond to general labels
# Start tags <section>
#       End tags </section>
#       Empty tags <line-break />
#       Elements are specific examples of tags
# <Greeting> Hello, world </Greeting>
#       Attributes are components of the label
# <img src="jeff.jpg" alt="instructor"/>
#       <step number="3"> Connect A to B. </step>
      

# Read XML files into R
library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl, useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
# [1] "breakfast_menu"

names(rootNode)

# Directly access parts of the XML document
rootNode[[1]]

# <food>
#       <name>Belgian Waffles</name>
#       <price>$5.95</price>
#       <description>Two of our famous Belgian Waffles with plenty of real maple syrup</description>
#       <calories>650</calories>
#       </food> 
      
rootNode[[1]][[1]]

xmlSApply(rootNode, xmlValue)

# Get the items on the menu and prices
xpathSApply(rootNode, "//name", xmlValue)

# [1] "Belgian Waffles"             "Strawberry Belgian Waffles"  "Berry-Berry Belgian Waffles"
# [4] "French Toast"                "Homestyle Breakfast"        

xpathSApply(rootNode, "//price", xmlValue)

# Extract content by attributes
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl,useInternal=TRUE)
scores <- xpathSApply(doc,"//li[@class='score']",xmlValue)
teams <- xpathSApply(doc,"//li[@class='team-name']",xmlValue)
scores

# [1] "49-27"    "14-6"     "30-9"     "23-20"    "26-23"    "19-17"    "19-16"    "24-18"   
# [9] "20-17 OT" "23-20 OT" "19-3"     "22-20"    "29-26"    "18-16"    "41-7"     "34-17"   

teams

# Reading data from JSON {jsonlite package}
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)

# Nested objects in JSON
names(jsonData$owner)

# [1] "login"               "id"                  "avatar_url"          "gravatar_id"        
# [5] "url"                 "html_url"            "followers_url"       "following_url"      
# [9] "gists_url"           "starred_url"         "subscriptions_url"   "organizations_url"  
# [13] "repos_url"           "events_url"          "received_events_url" "type"               
# [17] "site_admin"         

jsonData$owner$login

# Writing data frames to JSON
myjson <- toJSON(iris, pretty=TRUE)
cat(myjson)

# Convert back to JSON
iris2 <- fromJSON(myjson)
head(iris2)

# Create data tables just like data frames
library(data.table)
DF <- data.frame(x=rnorm(9), y=rep(c("a","b","c"), each=3), z=rnorm(9))
head(DF,3)

# x y        z
# 1 0.4159 a -0.05855
# 2 0.8433 a  0.13732
# 3 1.0585 a  2.16448

DT <- data.table(x=rnorm(9), y=rep(c("a","b","c"), each=3), z=rnorm(9))
head(DT,3)
system.time(DT <- data.table(x=rnorm(9), y=rep(c("a","b","c"), each=3), z=rnorm(9)))
system.time(DF <- data.frame(x=rnorm(9), y=rep(c("a","b","c"), each=3), z=rnorm(9)))

# See all the data tables in memory
tables()

#Subsetting rows
DT[2, ]

# x y     z
# 1: 1.002 a 1.509

DT[DT$y=="a", ]

# Subsetting number 2 and 3 rows
DT[c(2,3)]

# Subsetting columns!?
DT[,c(2,3)]

# Calculating values for variables with expressions
DT[,list(mean(x), sum(z))]

# V1     V2
# 1: 0.05637 0.5815

DT[, table(y)]

# Adding new columns
DT[, w:=z^2]
DT

# Multiple operations
DT[, m:= {tmp <- (x+z); log2(tmp+5)}]
DT

# plyr like operations
DT[, a:=x>0]
DT

# Mean of (x+w) gruped by a
DT[, b:= mean(x+w), by=a] 
DT

# Special variables

# .N An integer, length 1, containing the number of times that a particur group appears

set.seed(123)
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
DT
DT[, .N, by=x]

# x     N
# 1: a 33387
# 2: c 33201
# 3: b 33412

# Keys
DT <- data.table(x=rep(c("a","b","c"), each=100), y=rnorm(300))
DT
setkey(DT, x)
DT['a']

# Joins
DT1 <- data.table(x=c('a', 'a', 'b', 'dt1'), y=1:4)
DT2 <- data.table(x=c('a', 'b', 'dt2'), z=5:7)
DT1
DT2
setkey(DT1, x) 
setkey(DT2, x)
merge(DT1, DT2)

# Fast reading
big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6))
file <- tempfile()
write.table(big_df, file=file, row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
system.time(fread(file))

# user  system elapsed 
# 0.312   0.015   0.326 

system.time(read.table(file, header=TRUE, sep="\t"))

#  user  system elapsed 
# 5.702   0.048   5.755 

# Connecting and listing databases

ucscDb <- dbConnect(MySQL(), user="genome", 
                    host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb,"show databases;"); dbDisconnect(ucscDb);

# [1] TRUE

result

# R HDF5 package
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)
created <- h5createFile("./data/example.h5")
created


# Create groups
created = h5createGroup("./data/example.h5","foo")
created = h5createGroup("./data/example.h5","baa")
created = h5createGroup("./data/example.h5","foo/foobaa")
h5ls("./data/example.h5")


# Write to groups
A = matrix(1:10,nr=5,nc=2)
h5write(A, "./data/example.h5","foo/A")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "example.h5","foo/foobaa/B")
h5ls("./data/example.h5")

# Write a data set
df = data.frame(1L:5L,seq(0,1,length.out=5),
                c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
h5write(df, "./data/example.h5","df")
h5ls("./data/example.h5")

# Reading data
readA = h5read("./data/example.h5","foo/A")
readB = h5read("./data/example.h5","foo/foobaa/B")
readdf= h5read("./data/example.h5","df")
readA

# Writing and reading chunks
h5write(c(12,13,14),"./data/example.h5","foo/A",index=list(1:3,1))
h5read("./data/example.h5","foo/A")

# Getting data off webpages - readLines()
con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode

# Parsing with XML
library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes=T)

xpathSApply(html, "//title", xmlValue)

# [1] "Jeff Leek - Google Scholar Citations"

xpathSApply(html, "//td[@id='col-citedby']", xmlValue)

# GET from the httr package
library(httr) 
html2 = GET(url)
content2 = content(html2,as="text")
parsedHtml = htmlParse(content2,asText=TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

# [1] "Jeff Leek - Google Scholar Citations"

# Accessing websites with passwords
pg1 = GET("http://httpbin.org/basic-auth/user/passwd")
pg1

# Accessing websites with passwords
pg2 = GET("http://httpbin.org/basic-auth/user/passwd",
          authenticate("user","passwd"))
pg2

# Using handles
google = handle("http://google.com")
pg1 = GET(handle=google, path="/")
pg2 = GET(handle=google, path="search")
names(pg2)
pg2$cockies

# Accessing Twitter from R
myapp = oauth_app("twitter",
                  key="gXIh8Uf7NABVxvRSFK9jeb0QHKlVr1HULVf9I0qrAysE47LxfD", 
                  secret="gXIh8Uf7NABVxvRSFK9jeb0QHKlVr1HULVf9I0qrAysE47LxfD")
sig = sign_oauth1.0(myapp,
                    # token = "310714574-ChraWdXHEIPSz8OITKuJuYU6QaLHBYax9jbBPga9",
                    token = "310714574-ChraWdXHEIPSz8OITKuJuYU6QaLHBYax9jbBPga9",
                    token_secret = " dmJfGon6eULbuxVALyKikpHE4TqMbzRINk1LIoqQ2ypuD")
homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)

# Converting the json object
json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1, 1:4]



################################################################################

# Listar ficheros de un directorio
list.files(path="./specdata", pattern="*.csv")

# Import and rbind multiple csv files with common name in R
# http://stackoverflow.com/questions/15527720/import-and-rbind-multiple-csv-files-with-common-name-in-r

# I have multiple CSV files with 4 common character in their names. I want to know how I can rbind the files with the 
# same common character. For example, "AM-25" is common in name of 3 csv files and "BA-35" in the name of another 2.

# The files are like this AM-25.myfiles.2000.csv, AM-25.myfiles.2001.csv ,AM-25.myfiles.2002.csv,BA-35.myfiles.2000.csv, 
# BA-35.myfiles.2001.csv, I use this to read in all the files:

do.call(rbind, lapply(list.files(path=".", pattern="AM-25"), read.table, header=TRUE, sep=","))

# This would rbind together the matrices read from your csv files which contain the characters "AM-25". 
# The arguments for read.table could be different, depending on your csv files.

# Leer todos los csv de una carpeta
data <- do.call(rbind, lapply(list.files(path=".", pattern="*.csv"), read.csv))

################################################################################
## Efficiency of Importing Large CSV Files in R
## http://statcompute.wordpress.com/2014/02/11/efficiency-of-importing-large-csv-files-in-r/ 

### size of csv file: 689.4MB (7,009,728 rows * 29 columns) ###

system.time(read.csv('../data/2008.csv', header = T))
#   user  system elapsed
# 88.301   2.416  90.716

library(data.table)
system.time(fread('../data/2008.csv', header = T, sep = ','))
#   user  system elapsed
#  4.740   0.048   4.785

library(bigmemory)
system.time(read.big.matrix('../data/2008.csv', header = T))
#   user  system elapsed
# 59.544   0.764  60.308

library(ff)
system.time(read.csv.ffdf(file = '../data/2008.csv', header = T))
#   user  system elapsed
# 60.028   1.280  61.335

library(sqldf)
system.time(read.csv.sql('../data/2008.csv'))
#   user  system elapsed
# 87.461   3.880  91.447

################################################################################
## General Regression Neural Network with R
## http://statcompute.wordpress.com/2013/06/16/general-regression-neural-network-with-r/

pkgs <- c('MASS', 'doParallel', 'foreach', 'grnn')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 8)

data(Boston)
# PRE-PROCESSING DATA
X <- Boston[-14]
st.X <- scale(X)
Y <- Boston[14]
boston <- data.frame(st.X, Y)

# SPLIT DATA SAMPLES
set.seed(2013)
rows <- sample(1:nrow(boston), nrow(boston) - 200)
set1 <- boston[rows, ]
set2 <- boston[-rows, ]

# DEFINE A FUNCTION TO SCORE GRNN
pred_grnn <- function(x, nn){
      xlst <- split(x, 1:nrow(x))
      pred <- foreach(i = xlst, .combine = rbind) %dopar% {
            data.frame(pred = guess(nn, as.matrix(i)), i, row.names = NULL)
      }
}

# SEARCH FOR THE OPTIMAL VALUE OF SIGMA BY THE VALIDATION SAMPLE
cv <- foreach(s = seq(0.2, 1, 0.05), .combine = rbind) %dopar% {
      grnn <- smooth(learn(set1, variable.column = ncol(set1)), sigma = s)
      pred <- pred_grnn(set2[, -ncol(set2)], grnn)
      test.sse <- sum((set2[, ncol(set2)] - pred$pred)^2)
      data.frame(s, sse = test.sse)
}

cat("\n### SSE FROM VALIDATIONS ###\n")
print(cv)
jpeg('grnn_cv.jpeg', width = 800, height = 400, quality = 100)
with(cv, plot(s, sse, type = 'b'))

cat("\n### BEST SIGMA WITH THE LOWEST SSE ###\n")
print(best.s <- cv[cv$sse == min(cv$sse), 1])

# SCORE THE WHOLE DATASET WITH GRNN
final_grnn <- smooth(learn(set1, variable.column = ncol(set1)), sigma = best.s)
pred_all <- pred_grnn(boston[, -ncol(set2)], final_grnn)
jpeg('grnn_fit.jpeg', width = 800, height = 400, quality = 100)
plot(pred_all$pred, boston$medv)
dev.off()

################################################################################
## Aventuras de "web scraping": cómo bajarse todo el BOE
## http://www.datanalytics.com/2014/04/24/aventuras-de-web-scraping-como-bajarse-todo-el-boe/

setwd("E:/Varios/R/Archivos/varios/boe/boes")

library(RCurl)

h = getCurlHandle()

for( i in 1:3231){
      mi.url <- paste("http://www.boe.es/diario_boe/xml.php?id=BOE-A-2013-", i, sep = "")
      nom.fich <- paste("2013-A-", formatC(i, width = 6, format = "d", flag = "0"),  ".xml", sep = "")
      res <- getURI(mi.url, curl = h)
      cat(res, file = nom.fich)
}

for( i in 1:3212){
      mi.url <- paste("http://www.boe.es/diario_boe/xml.php?id=BOE-B-2013-", i, sep = "")
      nom.fich <- paste("2013-B-", formatC(i, width = 6, format = "d", flag = "0"),  ".xml", sep = "")
      res <- getURI(mi.url, curl = h)
      cat(res, file = nom.fich)
}

################################################################################
## Getting and cleaning data
## https://class.coursera.org/getdata-002/quiz/attempt?quiz_id=69
# Subsetting - quick review

set.seed(13435)
X <- data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X
X <- X[sample(1:5),]
X
X$var2[c(1,3)] = NA
X

X[,1]
X[,"var1"]
X[1:2,"var2"]

# Logical AND
X[(X$var1 <= 3 & X$var3 > 11),]
# Logical OR
X[(X$var1 <= 3 | X$var3 > 15),]

# Dealing with missing values (NA)
X[which(X$var2 > 8),]

# Sorting

sort(X$var1)
# [1] 1 2 3 4 5

sort(X$var1, decreasing=TRUE)
# [1] 5 4 3 2 1

sort(X$var2, na.last=TRUE)
# [1]  6  9 10 NA NA

# Ordering

X[order(X$var1),]

# By multiple variables
X[order(X$var1, X$var3),]

# Ordering with plyr

library(plyr)
arrange(X, var1)
arrange(X, desc(var1))

# Adding rows and columns
X$var4 <- rnorm(5)
X

Y <- cbind(X, rnorm(5))
Y

# Getting the data from the web
# Download csv form url
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/restaurants.csv")
restData <- read.csv("./data/restaurants.csv")
head(restData, n=3)
tail(restData, n=3)
summary(restData)
str(restData)

# Quantiles of quantitative variables
quantile(restData$councilDistrict, na.rm=TRUE)
quantile(restData$councilDistrict, probs=c(0.5, 0.75, 0.9))

# Make table
table(restData$zipCode, useNA="ifany")
table(restData$councilDistrict, restData$zipCode)

# Check for missing values
sum(is.na(restData$councilDistrict))
# [1] 0

any(is.na(restData$councilDistrict))
# [1] FALSE

all(restData$zipCode > 0)

# Row and column sums. To look for any NA values in all columns 
colSums(is.na(restData))

all(colSums(is.na(restData))==0)

# Values with specific characteristics
table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212","21213"))
restData[restData$zipCode %in% c("21212","21213"),]

# Cross tabs
data(UCBAdmissions)
DF <- as.data.frame(UCBAdmissions)
summary(DF)
DF
xt <- xtabs(Freq ~ Gender + Admit, data=DF)
xt

# Flat tables
warpbreaks$replicate <- rep(1:9, len = 54)
summary(warpbreaks)
xt <- xtabs(breaks ~., data=warpbreaks)
xt
ftable(xt)

# Size of a data set

fakeData <- rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData), units="Mb")

# Creating sequences
# Sometimes you need an index for your data set
s1 <- seq(1, 10, by=2)
s1
#[1] 1 3 5 7 9

s2 <- seq(1, 10, length=3)
s2
# [1]  1.0  5.5 10.0

x <- c(1,3,8,25,100)
seq(along = x)
# [1] 1 2 3 4 5

# Subsetting variables
restData$nearMe <- restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearMe)

# Creating binary variables
restData$zipWrong <- ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong, restData$zipCode < 0)

# Creating categorical variables
restData$zipGroups <- cut(restData$zipCode, breaks=quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups, restData$zipCode)

# Easier cutting
library(Hmisc)
restData$zipGroups <- cut2(restData$zipCode, g=4)
table(restData$zipGroups)

# Creating factor variables
restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf)

# Levels of factor variables
yesno <- sample(c("yes","no"), size=10, replace=TRUE)
yesno
yesnofac <- factor(yesno, levels=c("yes","no"))
yesnofac
# To change levels of factors
relevel(yesnofac, ref="yes")
yesnofac
as.numeric(yesnofac)

# Using the mutate function
library(Hmisc)
library(plyr)
restData2 <- mutate(restData, zipGroups=cut2(zipCode, g=4))
table(restData2$zipGroups)

# Start with reshaping
library(reshape2)
head(mtcars)

# Melting data frames
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id=c("carname","gear","cyl"), measure.vars=c("mpg","hp"))
head(carMelt)
tail(carMelt)

# Casting data frames
cylData <- dcast(carMelt, cyl ~ variable)
cylData
cylData <- dcast(carMelt, cyl ~ variable,mean)
cylData

# Averaging values
head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

# Another way - split
spIns <- split(InsectSprays$count, InsectSprays$spray)
spIns

sprCount <- lapply(spIns, sum)
sprCount

unlist(sprCount)
sapply(spIns, sum)

ddply(InsectSprays, .(spray), summarize, sum=sum(count))

# Creating a new variable
spraySums <- ddply(InsectSprays, .(spray), summarize, sum=ave(count, FUN=sum))
dim(spraySums)
head(spraySums)

# Peer review data
if(!file.exists("./data")){dir.create("./data")}
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews.csv")
download.file(fileUrl2,destfile="./data/solutions.csv")
reviews <- read.csv("./data/reviews.csv")
solutions <- read.csv("./data/solutions.csv")
head(reviews)
head(solutions)

names(reviews)
names(solutions)

mergedData <- merge(reviews, solutions, by.x="solution_id", by.y="id", all=TRUE)
head(mergedData)


# Default - merge all common column names
intersect(names(solutions), names(reviews))
mergedData2 <- merge(reviews, solutions, all=TRUE)
head(mergedData2)

# Using join in the plyr package
# Faster, but less full featured - defaults to left join, see help file for more
df1 <- data.frame(id=sample(1:10), x=rnorm(10))
df2 <- data.frame(id=sample(1:10), y=rnorm(10))
arrange(join(df1, df2), id)

# If you have multiple data frames
df1 <- data.frame(id=sample(1:10), x=rnorm(10))
df2 <- data.frame(id=sample(1:10), y=rnorm(10))
df3 <- data.frame(id=sample(1:10), z=rnorm(10))
dfList <- list(df1, df2, df3)
dfList
join_all(dfList)

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv")
list.files("./data")
cameraData <- read.table("./data/cameras.csv", sep = ",", header = TRUE)
str(cameraData)
names(cameraData)

# Fixing character vectors - tolower(), toupper()
tolower(names(cameraData))

# Fixing character vectors - strsplit()
# Good for automatically splitting variable names
# Important parameters: x, split
splitNames <- strsplit(names(cameraData),"\\.")
splitNames
splitNames[[5]]
splitNames[[6]]

# Quick aside - lists
mylist <- list(letters = c("A", "b", "c"), numbers = 1:3, matrix(1:25, ncol = 5))
head(mylist)

# Fixing character vectors - sapply()
# Applies a function to each element in a vector or list
# Important parameters: X,FUN

splitNames[[6]][1]

# Select first value of every element in a list
firstElement <- function(x){x[1]}
sapply(splitNames, firstElement)

# Peer review data
fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
# download.file(fileUrl1,destfile="./data/reviews.csv", method="curl")
download.file(fileUrl1,destfile="./data/reviews.csv")
download.file(fileUrl2,destfile="./data/solutions.csv")
reviews <- read.csv("./data/reviews.csv")
solutions <- read.csv("./data/solutions.csv")
head(reviews)
head(solutions)

names(reviews)
sub("_", "", names(reviews), )

# Fixing character vectors - gsub()
testName <- "this_is_a_test"
sub("_", "", testName)
gsub("_", "", testName)

# Finding values - grep(),grepl()
grep("Alameda", cameraData$intersection)
table(grepl("Alameda", cameraData$intersection))
cameraData2 <- cameraData[!grepl("Alameda", cameraData$intersection), ]
cameraData2

grep("Alameda", cameraData$intersection, value=TRUE)
grep("JeffStreet", cameraData$intersection)
length(grep("JeffStreet", cameraData$intersection))

library(stringr)
nchar("Jeffrey Leek")
substr("Jeffrey Leek", 1, 7)
paste("Jeffrey", "Leek")
paste0("Jeffrey","Leek")
str_trim("Jeff      ")

# Regular expressions
# Metacharacters

# dates
d1 <- date()
d1
class(d1)

d2 <- Sys.Date()
d2
class(d2)

# Formatting dates
# %d = day as number (0-31), %a = abbreviated weekday,%A = unabbreviated weekday, 
# %m = month (00-12), %b = abbreviated month, %B = unabbrevidated month, 
# %y = 2 digit year, %Y = four digit year

format(d2,"%a %b %d")

# Creating dates

x <- c("1ene1960", "2ene1960", "31mar1960", "30jul1960")
z <- as.Date(x, "%d%b%Y")
z

[1] "1960-01-01" "1960-01-02" "1960-03-31" "1960-07-30"

z[1] - z[2]

Time difference of -1 days

as.numeric(z[1]-z[2])

[1] -1

# Converting to Julian
weekdays(d2)

[1] "Sunday"

months(d2)

[1] "January"

julian(d2)

# Lubridate
library(lubridate) 
ymd("20140108")

[1] "2014-01-08 UTC"

mdy("08/04/2013")

[1] "2013-08-04 UTC"

dmy("03-04-2013")

[1] "2013-04-03 UTC"

# Dealing with times
ymd_hms("2011-08-03 10:15:03")

[1] "2011-08-03 10:15:03 UTC"

ymd_hms("2011-08-03 10:15:03",tz="Pacific/Auckland")

[1] "2011-08-03 10:15:03 NZST"

?Sys.timezone

# Some functions have slightly different syntax
x <- dmy(c("1ene2013", "2ene2013", "31mar2013", "30jul2013"))
wday(x[1])
wday(x[1], label=TRUE)


################################################################################
## https://github.com/mages/googleVis/blob/master/demo/WorldBank.R

## This demo shows how country level data can be accessed from
## the World Bank via their API and displayed with a Motion Chart.
## Inspired by Google's Public Data Explorer, see
## http://www.google.com/publicdata/home
##
## For the World Bank Data terms of use see:
## http://data.worldbank.org/summary-terms-of-use
##
## To run this demo an internet connection and Flash are required.
## This demo is part of the googleVis R package.
##
## See also: http://lamages.blogspot.com/2011/09/accessing-and-plotting-world-bank-data.html
## Markus Gesmann, 24 September 2011
##
## Thanks to John Maindonald for a simplified version of this
## demo using the WDI package.
##
## Distributed under GPL 2 or later

## This demo requires the 'WDI' package
if( !is.element("WDI", installed.packages()[,1]) )
      install.packages("WDI")

library(WDI)
inds <- c('SP.DYN.TFRT.IN','SP.DYN.LE00.IN', 'SP.POP.TOTL',
          'NY.GDP.PCAP.CD', 'SE.ADT.1524.LT.FE.ZS')
indnams <- c("fertility.rate", "life.expectancy", "population",
             "GDP.per.capita.Current.USD", "15.to.25.yr.female.literacy")
wdiData <- WDI(country="all", indicator=inds,
               start=1960, end=format(Sys.Date(), "%Y"), extra=TRUE)
colnum <- match(inds, names(wdiData))

names(wdiData)[colnum] <- indnams
## Create a motion chart
library(googleVis)
WorldBank <- droplevels(subset(wdiData, !region %in% "Aggregates"))
M <- gvisMotionChart(WorldBank,
                     idvar="country", timevar="year",
                     xvar="life.expectancy", yvar="fertility.rate",
                     colorvar="region", sizevar="population",
                     options=list(width=700, height=600))
## Display the chart in the browser
plot(M)

################################################################################
## What If You Dig A Hole Through The Earth?
## http://www.r-bloggers.com/what-if-you-dig-a-hole-through-the-earth/

library(xlsx)
library(ggmap)
library(mapdata)
library(ggplot2)
#The xls file is in http://esa.un.org/unpd/wup/CD-ROM/WUP2011-F13-Capital_Cities.xls
CapitalCities <- read.xlsx("WUP2011-F13-Capital_Cities.xls", sheetName="Capital_Cities", startRow=13, header=TRUE)
names(CapitalCities) = gsub("\\.", "", names(CapitalCities))
#Obtain symmetric coordinates for each capital
CapitalCities$LatitudeSym <- -CapitalCities$Latitude
CapitalCities$LongitudeSym <- -sign(CapitalCities$Longitude)*(180-abs(CapitalCities$Longitude))
CapitalCities$DigResult <- apply(CapitalCities, 1, function(x) {unlist(revgeocode(c(as.numeric(x[11]),as.numeric(x[10]))))})
CapitalCities$Drowned <- is.na(CapitalCities$DigResult)*1
#Percentage of population saved
sum(CapitalCities$Drowned*CapitalCities$Populationthousands)/sum(CapitalCities$Populationthousands)
world <- map_data("world")
opt <- theme(legend.position="none",
             axis.ticks=element_blank(),
             axis.title=element_blank(),
             axis.text =element_blank(),
             plot.title = element_text(size = 35),
             panel.background = element_rect(fill="turquoise1"))
p <- ggplot()
p <- p + geom_polygon(data=world, aes(x=long, y=lat, group = group),colour="white", fill="lightgoldenrod2" )
p <- p + geom_point(data=CapitalCities, aes(x=Longitude, y=Latitude, color=Drowned, size = Populationthousands)) + scale_size(range = c(2, 20), name="Population (thousands)")
p <- p + labs(title = "What if you dig a hole through the Earth?")
p <- p + scale_colour_gradient(low = "brown", high = "blue")
p <- p + annotate("rect", xmin = -135, xmax = -105, ymin = -70, ymax = -45, fill = "white")
p <- p + annotate("text", label = "Drowned", x = -120, y = -60, size = 6, colour = "blue")
p <- p + annotate("text", label = "Saved", x = -120, y = -50, size = 6, colour = "brown")
p <- p + geom_point(aes(x = -120, y = -65), size=8, colour="blue")
p <- p + geom_point(aes(x = -120, y = -55), size=8, colour = "brown")
p + opt
# Get a map of Spain, centered and signed in Madrid
madrid <- geocode('Madrid, Spain')
map.madrid <- get_map(location = as.numeric(madrid), color = "color", maptype = "roadmap", scale = 2, zoom = 6)
ggmap(map.madrid) + geom_point(aes(x = lon, y = lat), data = madrid, colour = 'red', size = 4)
# Get a map of New Zealand, centered and signed in Weber (the antipode of Madrid)
weber <- geocode('Weber, New Zealand')
map.weber <- get_map( location = as.numeric(weber), color = "color", maptype = "roadmap", scale = 2, zoom = 6)

################################################################################
## Converting shapefiles to rasters in R
## http://www.r-bloggers.com/converting-shapefiles-to-rasters-in-r/

shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
                       proj.to = NA, map = TRUE) {
      require(raster, rgdal)
      
      # use transform==TRUE if the polygon is not in the same coordinate system as
      # the output raster, setting proj.from & proj.to to the appropriate
      # projections
      if (transform == TRUE) {
            proj4string(shp) <- proj.from
            shp <- spTransform(shp, proj.to)
      }
      
      # convert the shapefile to a raster based on a standardised background
      # raster
      r <- rasterize(shp, mask.raster)
      # set the cells associated with the shapfile to the specified value
      r[!is.na(r)] <- value
      # merge the new raster with the mask raster and export to the working
      # directory as a tif file
      r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
                overwrite = T)
      
      # plot map of new raster
      if (map == TRUE) {
            plot(r, main = label, axes = F, box = F)
      }
      
      names(r) <- label
      return(r)
}

library(maptools)
library(raster)

## example: import world raster from package biomod2 and set the background
## values to zero
library(biomod2)
worldRaster <- raster(system.file("external/bioclim/current/bio3.grd", package = "biomod2"))
worldRaster[!is.na(worldRaster)] <- 0
plot(worldRaster, axes = F, box = F, legend = F, main = "The world")

# import world polygon shapefile from package maptools
data(wrld_simpl, package = "maptools")
plot(wrld_simpl, add = T)

# extract all Australian polygons and convert to a world raster where cells
# associated with Australia have a value of 1 and everything else has a
# value of 0.
australia <- shp2raster(shp = wrld_simpl[grepl(c("Australia";), wrld_simpl$NAME), ],
                        mask.raster = worldRaster, label = "Where Amy currently lives", transform = FALSE, value = 1)

## Found 1 region(s) and 97 polygon(s)

# extract Australia, NZ & USA and convert to a world raster where cells
# associated with these countries have a value of 3 and everything
# else has a value of 0.
aus.nz.us <- shp2raster(shp = wrld_simpl[grepl(c("Australia|New Zealand|United States"),
                                               wrld_simpl$NAME), ], mask.raster = worldRaster, label = "All countries Amy has lived in",
                        transform = FALSE, value = 3)

## Found 5 region(s) and 384 polygon(s)

# set relevant projections
GDA94.56 <- CRS("+proj=utm +zone=56 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
GDA94 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

LH.mask <- raster("LH.mask.tif")
# set the background cells in the raster to 0
LH.mask[!is.na(LH.mask)] <- 0

NPWS.reserves <- readShapePoly("NPWSReserves.shp", proj4 = GDA94)

# convert the NPWS.reserves polygon data for National Parks &amp; Nature
# Reserves to a raster, after changing the projection.
NPWS.raster <- shp2raster(shp = NPWS.reserves[grepl(c("NP|NR"), NPWS.reserves$reservetyp),],
                          mask.raster = LH.mask, label = "National Parks & Nature Reserves", value = 3,
                          transform = TRUE, proj.from = GDA94, proj.to = GDA94.56)

## Found 111 region(s) and 837 polygon(s)

################################################################################
## Coursera - Exploratory Data Analysis
## https://class.coursera.org/exdata-002/lecture

setwd("E:/Varios/R/Archivos/Coursera/Exploratory Data Analysis")

pollution <- read.csv("./data/avgpm25.csv", colClasses=c("numeric", "character", 
                                                       "factor", "numeric", 
                                                       "numeric"))
head(pollution)

# Five Number Summary
summary(pollution$pm25)

# Boxplot
boxplot(pollution$pm25, col="blue")

# Histogram
hist(pollution$pm25, col="green")

hist(pollution$pm25, col="green")
rug(pollution$pm25)

hist(pollution$pm25, col="green", breaks=100)
rug(pollution$pm25)

# Overlaying Features
boxplot(pollution$pm25, col="blue")
abline(h=12)

# Overlaying Features
hist(pollution$pm25, col="green")
abline(v=12, lwd=2)
abline(v=median(pollution$pm25), col="magenta", lwd=4)

# Multiple Boxplots
boxplot(pm25 ~ region, data=pollution, col="red")

# Multiple Histograms
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
hist(subset(pollution, region=="east")$pm25, col="green")
hist(subset(pollution, region=="west")$pm25, col="green")

# Scatterplot
with(pollution, plot(latitude, pm25))
abline(h = 12, lwd = 2, lty = 2)

with(pollution, plot(latitude, pm25, col=region))
abline(h = 12, lwd = 2, lty = 2)

# Multiple Scatterplots
par(mfrow = c(2, 1), mar = c(5, 4, 2, 1))
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))

# Base Plot
library(datasets)
data(cars)
with(cars, plot(speed, dist))

# Lattice Plot
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))


# ggplot2 Plot
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

# Simple Base Graphics: Boxplot

library(datasets)
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

# Default values for global graphics parameters

par("lty")
## [1] "solid"

par("col")
## [1] "black"

par("pch")
## [1] 1

par("bg")
## [1] "transparent"

par("mar")
## [1] 5.1 4.1 4.1 2.1

par("mfrow")
## [1] 1 1

# Base Plot with Annotation

library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")  ## Add a title

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", 
                      type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

# Base Plot with Regression Line

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City", 
                      pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)


# Multiple Base Plots

par(mfrow = c(1, 2))
with(airquality, {
      plot(Wind, Ozone, main = "Ozone and Wind")
      plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
      plot(Wind, Ozone, main = "Ozone and Wind")
      plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
      plot(Temp, Ozone, main = "Ozone and Temperature")
      mtext("Ozone and Weather in New York City", outer = TRUE)
})

pdf(file = "myplot.pdf")  ## Open PDF device; create 'myplot.pdf' in my working directory
## Create plot and send to a file (no plot appears on screen)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")  ## Annotate plot; still nothing on screen
dev.off()  ## Close the PDF file device
## Now you can view the file 'myplot.pdf' on your computer

# Copying Plots
# Copying a plot to another device can be useful because some plots require a lot 
# of code and it can be a pain to type all that in again for a different device.

# dev.copy: copy a plot from one device to another
# dev.copy2pdf: specifically copy a plot to a PDF file 

# NOTE: Copying a plot is not an exact operation, so the result may not be identical to the original.
library(datasets)
with(faithful, plot(eruptions, waiting))  ## Create plot on screen device
title(main = "Old Faithful Geyser data")  ## Add a main title
dev.copy(png, file = "geyserplot.png")  ## Copy my plot to a PNG file
dev.off()  ## Don't forget to close the PNG device!

library(lattice)

# Simple Lattice Plot
library(lattice)
library(datasets)
# Simple scatterplot
xyplot(Ozone ~ Wind, data = airquality)
# To "see" dataset
data(airquality)

library(datasets)
library(lattice)
# Convert 'Month' to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))

p <- xyplot(Ozone ~ Wind, data = airquality)
## Nothing happens!
print(p)
## Plot appears
xyplot(Ozone ~ Wind, data = airquality)
## Auto-printing

# Lattice Panel Functions
set.seed(10)
x <- rnorm(100)
x
f <- rep(0:1, each = 50)
f
y <- x + f - f * x + rnorm(100, sd =0.5)
y
f <- factor(f, labels = c("Group 1", "Group 2"))
f
xyplot(y ~ x | f, layout = c(2, 1))
## Plot with 2 panels

## Custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) {
             panel.xyplot(x, y, ...)
             ## First call the default panel function for 'xyplot'
             panel.abline(h = median(y), lty = 2)
             ## Add a horizontal line at the median
       })

xyplot(y ~ x | f, panel = function(x, y, ...) {
      panel.xyplot(x, y, ...)
      ## First call the default panel function for 'xyplot'
      panel.lmline(x, y, col = 2)
      ## Add a horizontal line at the median
})

library(datasets)
data(airquality)

airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

library(ggplot2)
str(mpg)

# Basic ggplot 2
qplot(displ, hwy, data=mpg)

# With color
qplot(displ, hwy, data=mpg, color=drv)

# Adding a geom. Confidence interval. Intervalo de confianza. 
qplot(displ, hwy, data=mpg, geom=c("point", "smooth"))

# ggplot histogram
qplot(hwy, data=mpg, fill=drv)

# ggplot facets
qplot(displ, hwy, data=mpg, facets = .~drv)
qplot(hwy, data=mpg, facets = .~drv, binwidth=2)
qplot(hwy, data=mpg, facets = drv~., binwidth=2)

setwd("E:/Varios/R/Archivos/Coursera/Exploratory Data Analysis")
library(lattice)
env <- readRDS("maacs_env.rds")
str(env)

# Histogram  of  eNO
qplot(log(eno),  data  =  maacs)
qplot(log(eno),  data  =  maacs,  fill  =  mopos)

# Density  Smooth
qplot(log(eno),  data  =  maacs,  geom  =  "density")
qplot(log(eno),  data  =  maacs,  geom  =  "density",  color  = mopos)

# Scatterplots in ggplot
qplot(log(pm25), log(eno), data = maacs)
qplot(log(pm25), log(eno), data = maacs, shape = mopos)
qplot(log(pm25), log(eno), data = maacs, color = mopos)
qplot(log(pm25), log(eno), data=maacs, color=mopos, geom=c("point", "smooth"), 
      method="lm")
qplot(log(pm25), log(eno), data=maacs, color=mopos, geom=c("point", "smooth"), 
      method="lm", facets=.~mopos)
qplot(log(pm25), log(eno), data=maacs, geom=c("point", "smooth"), method="lm", 
      facets=.~mopos)

head(maacs)

# Initial call to ggplot
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
# Summary of ggplot object
summary(g)
# save and then plt
p <- g + geom_point()
print(p)
# plot without save
g + geom_point()
# First  Plot  with  Point 
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
g + geom_point()
# Adding  More  Layers:  Smooth
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method = "lm")
# Adding  More  Layers:  Facets
g + geom_point() + facet_grid(. ~ bmicat) + geom_smooth(method = "lm")
# Modifying  Aesthetics 
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)
g + geom_point(aes(color = bmicat), size = 4, alpha = 1/2)
# Modifying  Labels 
g + geom_point(aes(color = bmicat)) 
  + labs(title = "MAACS Cohort") 
  + labs(x = expression("log " * PM[2.5]), y = "Nocturnal Symptoms")
# Customizing  the  Smooth 
g + geom_point(aes(color = bmicat), size = 2, alpha = 1/2) 
  + geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)
# Changing  the  Theme  
g + geom_point(aes(color = bmicat)) + theme_bw(base_family = "Times")

# A  Notes  about  Axis 
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100 ## Outlier!
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()

# Outlier missing
g + geom_line() + ylim(-3, 3)
# Outlier included
g + geom_line() + coord_cartesian(ylim = c(-3, 3))

# Code  for  Final  Plot  
## Calculate the deciles of the data
cutpoints <- quantile(maacs$logno2_new, seq(0, 1, length = 11), na.rm = TRUE)
## Cut the data at the deciles and create a new factor variable
maacs$no2dec <- cut(maacs$logno2_new, cutpoints)
## See the levels of the newly created factor variable
levels(maacs$no2dec)


## Setup ggplot with data frame
g <- ggplot(maacs, aes(logpm25, NocturnalSympt))
## Add layers
g + geom_point(alpha = 1/3)                           # Add points
+ facet_wrap(bmicat ~ no2dec, nrow = 2, ncol = 4)     # Make panels
+ geom_smooth(method="lm", se=FALSE, col="steelblue") # Add smoother
+ theme_bw(base_family = "Avenir", base_size = 10)    # Change theme
+ labs(x = expression("log " * PM[2.5]))              # 
+ labs(y = "Nocturnal Symptoms")                      # Add labels
+ labs(title = "MAACS Cohort")                        #  
             

# Initial call to ggplot
g <- ggplot(maacs, aes(log(pm25), eno))
# Summary of ggplot object
summary(g)
# save and then plt
p <- g + geom_point()
print(p)
# plot without save
g + geom_point()
# First  Plot  with  Point 
g <- ggplot(maacs, aes(log(pm25), eno))
g + geom_point()
# Adding  More  Layers:  Smooth. Add trend.
g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method = "lm")
# Adding  More  Layers:  Facets. Facet variable: mopos
g + geom_point() + facet_grid(. ~ mopos) + geom_smooth(method = "lm")
# Modifying  Aesthetics 
g + geom_point(color = "steelblue", size = 4, alpha = 1/2)
g + geom_point(aes(color = mopos), size = 4, alpha = 1/2)
# Modifying  Labels 
g + geom_point(aes(color = mopos)) 
+ labs(title = "MAACS Cohort") 
+ labs(x = expression("log " * PM[2.5]), y = "ENO")
# Customizing  the  Smooth 
g + geom_point(aes(color = mopos), size = 2, alpha = 1/2) 
+ geom_smooth(size = 4, linetype = 3, method = "lm", se = FALSE)
# Changing  the  Theme  
g + geom_point(aes(color = mopos)) + theme_bw(base_family = "Times")

# A  Notes  about  Axis 
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100 ## Outlier!
plot(testdat$x, testdat$y, type = "l", ylim = c(-3,3))

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()

# Outlier missing
g + geom_line() + ylim(-3, 3)
# Outlier included
g + geom_line() + coord_cartesian(ylim = c(-3, 3))

# Code  for  Final  Plot  
## Setup ggplot with data frame

g <- ggplot(maacs, aes(log(pm25), eno))
## Add layers
g + geom_point(alpha = 1/3) +                         # Add points
facet_wrap(mopos ~ no2dec, nrow = 2, ncol = 4) +      # Make panels
geom_smooth(method="lm", se=FALSE, col="steelblue") + # Add smoother
theme_bw(base_family = "Avenir", base_size = 10) +    # Change theme
labs(x = expression("log " * PM[2.5])) +              # 
labs(y = "Nocturnal Symptoms") +                      # Add labels
labs(title = "MAACS Cohort")                          #  


# Annota:on  
# Labels:  xlab(),  ylab(),  labs(),  gg:tle()  
# Each  of  the  "geom"  func:ons  has  op:ons  to  modify    
# For  things  that  only  make  sense  globally,  use  theme()    
# Example:  theme(legend.posi:on  =  "none")    
# Two  standard  appearance  themes  are  included  
# theme_gray():  The  default  theme  (gray  background)  
# theme_bw():  More  stark/plain       


# Hierarchical clustering - example

set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

# Hierarchical clustering - dist
# Important parameters: x,method
dataFrame <- data.frame(x = x, y = y)
dist(dataFrame)

# Hierarchical clustering - hclust
dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)


# Prettier dendrograms
myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)), 
                      hang = 0.1, ...) {
      ## modifiction of plclust for plotting hclust objects *in colour*!  Copyright
      ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
      ## of labels of the leaves of the tree lab.col: colour for the labels;
      ## NA=default device foreground colour hang: as in hclust & plclust Side
      ## effect: A display of hierarchical cluster with coloured leaf labels.
      y <- rep(hclust$height, 2)
      x <- as.numeric(hclust$merge)
      y <- y[which(x < 0)]
      x <- x[which(x < 0)]
      x <- abs(x)
      y <- y[order(x)]
      x <- x[order(x)]
      plot(hclust, labels = FALSE, hang = hang, ...)
      text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order], 
           col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))


# heatmap()
dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
library(ggplot2)
heatmap(dataMatrix)

# K-means clustering - example
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

# kmeans()
# Important parameters: x, centers, iter.max, nstart
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
kmeansObj$cluster

par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

# Heatmaps
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")

# Matrix data
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

# Cluster the data
par(mar = rep(0.2, 4))
heatmap(dataMatrix)

# What if we add a pattern?
set.seed(678910)
for (i in 1:40) {
      # flip a coin
      coinFlip <- rbinom(1, size = 1, prob = 0.5)
      # if coin is heads add a common pattern to that row
      if (coinFlip) {
            dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
      }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

# Patterns in rows and columns
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

# PCA - Princical Components Analysis
# SVD - Singular Value Decomposition
# Components of the SVD - u and v
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector", 
     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

# Components of the SVD - Variance explained
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", 
     pch = 19)

# Relationship to principal components
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", 
     ylab = "Right Singular Vector 1")
abline(c(0, 1))

# Components of the SVD - variance explained
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)

# What if we add a second pattern?
set.seed(678910)
for (i in 1:40) {
      # flip a coin
      coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
      coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
      # if coin is heads add a common pattern to that row
      if (coinFlip1) {
            dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
      }
      if (coinFlip2) {
            dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
      }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

# Singular value decomposition - true patterns
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

# v and patterns of variance in rows
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")

#d and variance explained
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained", 
     pch = 19)

# Missing values
dataMatrix2 <- dataMatrixOrdered
## Randomly insert some missing data
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))  ## Doesn't work!

# Imputing {impute}
source("http://bioconductor.org/biocLite.R")
biocLite()
library(impute)  ## Available from http://bioconductor.org
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)

# Face example
load("data/face.rda")
image(t(faceData)[, nrow(faceData):1])

# Face example - variance explained
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")

# Face example - create approximations
svd1 <- svd(scale(faceData))
## Note that %*% is matrix multiplication

# Here svd1$d[1] is a constant
approx1 <- svd1$u[, 1] %*% t(svd1$v[, 1]) * svd1$d[1]

# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[, 1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[, 1:5])
approx10 <- svd1$u[, 1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[, 1:10])

# Face example - plot approximations
par(mfrow = c(1, 4))
image(t(approx1)[, nrow(approx1):1], main = "(a)")
image(t(approx5)[, nrow(approx5):1], main = "(b)")
image(t(approx10)[, nrow(approx10):1], main = "(c)")
image(t(faceData)[, nrow(faceData):1], main = "(d)")  ## Original data

# colorRamp
# [,1] [,2] [,3]
# corresponds to
# [Red] [Blue] [Green]
pal <- colorRamp(c("red", "blue")) 
pal(0)
pal(1)
pal(0.5)
str(pal)
pal(seq(0, 1, len = 10))

# colorRampPalette
pal <- colorRampPalette(c("red", "yellow"))
pal(2)
pal(10)

# RColorBrewer and colorRampPalette
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

# The smoothScatter function
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

# Scatterplot with no transparency
plot(x,y, pch=19)

# Scatterplot with transparency
plot(x,y, col=rgb(0, 0, 0, 0.2), pch=19)

load("data/samsungData.rda")
names(samsungData)[1:12]
table(samsungData$activity)

# Plotting average acceleration for first subject
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1)
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), 
       pch = 1)

# Clustering based just on average acceleration
source("myplclust.R")
distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

# Plotting max acceleration for the first subject
par(mfrow = c(1, 2))
plot(sub1[, 10], pch = 19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[, 11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])

# Clustering based on maximum acceleration
source("myplclust.R")
distanceMatrix <- dist(sub1[, 10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

# Singular Value Decomposition
svd1 = svd(scale(sub1[, -c(562, 563)]))
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = sub1$activity, pch = 19)
plot(svd1$u[, 2], col = sub1$activity, pch = 19)

# Find maximum contributor
plot(svd1$v[, 2], pch = 19)

# New clustering with maximum contributer
maxContrib <- which.max(svd1$v[, 2])
distanceMatrix <- dist(sub1[, c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

# New clustering with maximum contributer
names(samsungData)[maxContrib]

# K-means clustering (nstart=1, first try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6)
table(kClust$cluster, sub1$activity)

# K-means clustering (nstart=1, second try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 1)
table(kClust$cluster, sub1$activity)

# K-means clustering (nstart=100, first try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

# K-means clustering (nstart=100, second try)
kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

# Cluster 1 Variable Centers (Laying)
plot(kClust$center[1, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")

# Cluster 2 Variable Centers (Walking)
plot(kClust$center[4, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")


## setwd("~/CourseraModules/04_ExploratoryAnalysis/CaseStudy/pm25_data")

## Has fine particle pollution in the U.S. decreased from 1999 to
## 2012?

## Read in data from 1999

pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
dim(pm0)
head(pm0)
# Take names of the head of file
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
print(cnames)
# Split names
cnames <- strsplit(cnames, "|", fixed = TRUE)
print(cnames)
# Convert variable names. Replace spaces by dots
names(pm0) <- make.names(cnames[[1]])
names(pm0)
head(pm0)
x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
# Missing values
mean(is.na(x0)) ## Are missing values important here?

## Read in data from 2012

pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "", nrow = 1304290)
names(pm1) <- make.names(cnames[[1]])
head(pm1)
dim(pm1)
x1 <- pm1$Sample.Value
class(x1)

## Five number summaries for both periods
summary(x1)
summary(x0)
mean(is.na(x1)) ## Are missing values important here?

## Make a boxplot of both 1999 and 2012
boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

## Check negative values in 'x1'
summary(x1)
negative <- x1 < 0
# Number
sum(negative, na.rm = T)
# Porcentaje
mean(negative, na.rm = T)
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, "month") ## Check what's going on in months 1--6
hist(dates[negative], "month")

## Plot a subset for one monitor at both times

## Find a monitor for New York State that exists in both datasets
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
head(site0)
str(site0)
str(site1)
both <- intersect(site0, site1)
print(both)

## Find how many observations available at each monitor
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

## Choose county 63 and side ID 2008
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm1sub)
dim(pm0sub)

## Plot data for 2012
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
str(dates1)
plot(dates1, x1sub)

## Plot data for 1999
dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

## Plot data for both years in same panel
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20) ## Whoa! Different ranges
abline(h = median(x1sub, na.rm = T))

## Find global range both variables
rng <- range(x0sub, x1sub, na.rm = T)
rng
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))

## Show state-wide means and make a plot showing trend
head(pm0)
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)

## Make separate data frames for states / years
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)

## Connect lines
# Reset par
par(mfrow = c(1, 1))
with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5)))
# Points 
with(mrg, points(rep(2, 52), mrg[, 3]))
# Lines to conect points
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])










################################################################################

# Plotting all possible likelihoods for a small n
n <- 5
pvals <- seq(0, 1, length = 1000)
plot(c(0, 1), c(0, 1.2), type = "n", frame = FALSE, xlab = "p", ylab = "likelihood")
text((0 : n) /n, 1.1, as.character(0 : n))
sapply(0 : n, function(x) {
      phat <- x / n
      if (x == 0) lines(pvals, ( (1 - pvals) / (1 - phat) )^(n-x), lwd = 3)
      else if (x == n) lines(pvals, (pvals / phat) ^ x, lwd = 3)
      else lines(pvals, (pvals / phat ) ^ x * ( (1 - pvals) / (1 - phat) ) ^ (n-x), lwd = 3)
}
)
title(paste("Likelihoods for n = ", n))

# Example
# Suppose a friend has 8 children (oh my!), 7 of which are girls and none are twins
# If each gender has an independent 50% probability for each birth, what's the 
# probability of getting 7 or more girls out of 8 births?

choose(8, 7) * .5 ^ 8 + choose(8, 8) * .5 ^ 8
# [1] 0.03516
pbinom(6, size = 8, prob = .5, lower.tail = FALSE)
plot(pvals, dbinom(7, 8, pvals) / dbinom(7, 8, 7/8) ,
     lwd = 3, frame = FALSE, type = "l", xlab = "p", ylab = "likelihood")

zvals <- seq(-3, 3, length = 1000)
plot(zvals, dnorm(zvals),
     type = "l", lwd = 3, frame = FALSE, xlab = "z", ylab = "Density")
sapply(-3 : 3, function(k) abline(v = k))

# Example
# The number of people that show up at a bus stop is Poisson with a mean of 2.5 per hour.
# If watching the bus stop for 4 hours, what is the probability that 3 or fewer people show up for the
# whole time?

ppois(3, lambda = 2.5 * 4)

# We flip a coin with success probablity 0.01 five hundred times.
# What's the probability of 2 or fewer successes?

pbinom(2, size = 500, prob = .01)

# Law of large numbers in action
n <- 10000; means <- cumsum(rnorm(n)) / (1 : n)
plot(1 : n, means, type = "l", lwd = 2,
     frame = FALSE, ylab = "cumulative means", xlab = "sample size")
abline(h = 0)

## Simulation of mean of $n$ dice
par(mfrow = c(1, 3))
for (n in c(1, 2, 6)){
      temp <- matrix(sample(1 : 6, n * 10000, replace = TRUE), ncol = n)
      temp <- apply(temp, 1, mean)
      temp <- (temp - 3.5) / (1.71 / sqrt(n)) 
      dty <- density(temp)
      plot(dty$x, dty$y, xlab = "", ylab = "density", type = "n", xlim = c(-3, 3), ylim = c(0, .5))
      title(paste("sample mean of", n, "obs"))
      lines(seq(-3, 3, length = 100), dnorm(seq(-3, 3, length = 100)), col = grey(.8), lwd = 3)
      lines(dty$x, dty$y, lwd = 2)
}

## Simulations of mean of a coin
par(mfrow = c(2, 3))
for (n in c(1, 10, 20)){
      temp <- matrix(sample(0 : 1, n * 10000, replace = TRUE), ncol = n)
      temp <- apply(temp, 1, mean)
      temp <- (temp - .5) * 2 * sqrt(n)
      dty <- density(temp)
      plot(dty$x, dty$y, xlab = "", ylab = "density", type = "n", xlim = c(-3, 3), ylim = c(0, .5))
      title(paste("sample mean of", n, "obs"))
      lines(seq(-3, 3, length = 100), dnorm(seq(-3, 3, length = 100)), col = grey(.8), lwd = 3)
      lines(dty$x, dty$y, lwd = 2)
}

# Give a confidence interval for the average height of sons in Galton's data
library(UsingR);data(father.son); x <- father.son$sheight
(mean(x) + c(-1, 1) * qnorm(.975) * sd(x) / sqrt(length(x))) / 12

################################################################################
## Efficiency of Importing Large CSV Files in R
## http://statcompute.wordpress.com/2014/02/11/efficiency-of-importing-large-csv-files-in-r/

### size of csv file: 689.4MB (7,009,728 rows * 29 columns) ###

system.time(read.csv('../data/2008.csv', header = T))
#   user  system elapsed
# 88.301   2.416  90.716

library(data.table)
system.time(fread('../data/2008.csv', header = T, sep = ','))
#   user  system elapsed
#  4.740   0.048   4.785

library(bigmemory)
system.time(read.big.matrix('../data/2008.csv', header = T))
#   user  system elapsed
# 59.544   0.764  60.308

library(ff)
system.time(read.csv.ffdf(file = '../data/2008.csv', header = T))
#   user  system elapsed
# 60.028   1.280  61.335

library(sqldf)
system.time(read.csv.sql('../data/2008.csv'))
#   user  system elapsed
# 87.461   3.880  91.447

################################################################################
## http://statcompute.wordpress.com/2013/09/08/generate-and-retrieve-many-objects-with-sequential-names/
## Generate and Retrieve Many Objects with Sequential Names

# While coding ensemble methods in data mining with R, e.g. bagging, we often need to generate many data 
# and models objects with sequential names. Below is a quick example how to use assign() function to 
# generate many prediction objects on the fly and then retrieve these predictions with mget() to do the model averaging. 

data(Boston, package = "MASS")

for (i in 1:10) {
      set.seed(i)
      smp <- Boston[sample(1:nrow(Boston), nrow(Boston), replace = TRUE), ]
      glm <- glm(medv ~ ., data = smp)
      prd <- predict(glm, Boston)
      ### ASSIGN A LIST OF SEQUENTIAL NAMES TO PREDICTIONS ###
      assign(paste("p", i, sep = ""), prd)
}

### RETURN NAMED OBJECTS TO A LIST ###
plist <- mget(paste('p', 1:i, sep = ''))
### AGGREGATE ALL PREDICTIONS ###
pcols <- do.call('cbind', plist)
pred_medv <- rowSums(pcols) / i

### A SIMPLE FUNCTION CALCULATION R-SQUARE ###
r2 <- function(y, yhat) {
      ybar <- mean(y)
      r2 <- sum((yhat - ybar) ^ 2) / sum((y - ybar) ^ 2)
      return(r2)
}
print(r2(Boston$medv, pred_medv))
# OUTPUT:
# [1] 0.7454225


################################################################################
## Shakespeare Is More Monkey-Friendly Than Cervantes
## http://www.r-bloggers.com/shakespeare-is-more-monkey-friendly-than-cervantes/

library(ggplot2)
library(scales)
esp.dic=data.frame(LANG="ESP", WORD=readLines("ES.dic"))
eng.dic=data.frame(LANG="ENG", WORD=readLines("UK.dic"))
df.lang=do.call("rbind", list(esp.dic, eng.dic))
df.lang$WORD=tolower(iconv(df.lang$WORD, to="ASCII//TRANSLIT"))
df.lang=unique(df.lang)
results=data.frame(LANG=character(0), OCCURRENCES=numeric(0), SIZE=numeric(0), LENGTH=numeric(0))
for (i in 2:5)
{
      df.monkey=data.frame(WORD=replicate(20000, paste(sample(c(letters), i, replace = TRUE), collapse='')))
      results=rbind(results, data.frame(setNames(aggregate(WORD ~ ., data = merge(df.lang, df.monkey, by="WORD"), FUN=length), c("LANG","OCCURRENCES")), SIZE=20000, LENGTH=i))
}
opt=theme(panel.background = element_rect(fill="gray92"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color="white", size=1.5),
          plot.title = element_text(size = 35),
          axis.title = element_text(size = 20, color="gray35"),
          axis.text = element_text(size=16),
          axis.ticks = element_blank(),
          axis.line = element_line(colour = "white"))
ggplot(data=results, aes(x=LENGTH, y=OCCURRENCES/SIZE, colour=LANG))+
      geom_line(size = 2)+
      scale_colour_discrete(guide = FALSE) +
      geom_point(aes(fill=LANG),size=10, colour="gray92",pch=21)+
      scale_x_continuous("word length", labels=c("two chars", "three chars", "four chars", "five chars"))+
      scale_y_continuous("probability of existence", limits=c(0, 0.4), labels = percent)+
      labs(title = "What if you put a monkey in front of a typewriter?")+
      opt + scale_fill_discrete(name="Dictionary", breaks=c("ESP", "ENG"), labels=c("Spanish", "English"))

################################################################################
## Downloading weather, sea ice, and wave model data with the rNOMADS package in R
## http://www.r-bloggers.com/downloading-weather-sea-ice-and-wave-model-data-with-the-rnomads-package-in-r/


# Getting wind speed at a specific point

library(rNOMADS)

#A location near my house
# lat <- 35.828304
# lon <- -79.107467

library(ggmap)
madrid <- geocode('Madrid, Spain')
madrid

lat <- 40.41678
lon <- -3.70379

#Find the latest Global Forecast System model run
model.urls <- GetDODSDates("gfs_hd")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

#Get nearest model nodes
lons <- seq(0, 359.5, by = 0.5)
lats <- seq(-90, 90, by = 0.5)
lon.diff <- abs(lon + 360 - lons)
lat.diff <- abs(lat - lats)
model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 #NOMADS indexes at 0
model.lat.ind <- which(lat.diff == min(lat.diff)) - 1

#Subset model
time <- c(0,0) #Model status at initialization
lon.inds <- c(model.lon.ind - 2, model.lon.ind + 2)
lat.inds <- c(model.lat.ind - 2, model.lat.ind + 2)
variable <- "ugrd10m" #First get east-west wind

u.grd.data <- DODSGrab(latest.model, latest.model.run, variable,
                       time, lon.inds, lat.inds)

variable <- "vgrd10m" #Then get north-south wind

v.grd.data <- DODSGrab(latest.model, latest.model.run, variable,
                       time, lon.inds, lat.inds)

#Reformat the data
u.grd.grid <- ModelGrid(u.grd.data, c(0.5, 0.5))
v.grd.grid <- ModelGrid(v.grd.data, c(0.5, 0.5))

#Interpolate it to the point of interest
u.point <- BuildProfile(u.grd.grid, lon, lat, TRUE)
v.point <- BuildProfile(v.grd.grid, lon, lat, TRUE)

#Present results!
print(paste("The East-West winds are going", sprintf("%.2f", u.point),
            "meters per second, and the north-south winds are going", sprintf("%.2f", v.point),
            "meters per second."))

#How did I know all these strange parameter names?
info <- GetDODSModelRunInfo(latest.model, latest.model.run)
print(info)

# Getting a temperature profile from 0 to 40 km above a specific point

#Find the latest Global Forecast System model run
model.urls <- GetDODSDates("gfs_hd")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

#Get nearest model nodes
lons <- seq(0, 359.5, by = 0.5)
lats <- seq(-90, 90, by = 0.5)
lon.diff <- abs(lon + 360 - lons)
lat.diff <- abs(lat - lats)
model.lon.ind <- which(lon.diff == min(lon.diff)) - 1 #NOMADS indexes at 0
model.lat.ind <- which(lat.diff == min(lat.diff)) - 1 

#Subset model
time <- c(0,0) #Model status at initialization
lon.inds <- c(model.lon.ind - 2, model.lon.ind + 2)
lat.inds <- c(model.lat.ind - 2, model.lat.ind + 2)
levels <- c(0, 46) #All pressure levels
variable <- "tmpprs" #First get temperature

tmpprs.data <- DODSGrab(latest.model, latest.model.run, variable,
                        time, lon.inds, lat.inds, levels)

variable <- "hgtprs" #Now get elevation of each pressure level
hgtprs.data <- DODSGrab(latest.model, latest.model.run, variable,
                        time, lon.inds, lat.inds, levels)

#Reformat the data
tmpprs.grid <- ModelGrid(tmpprs.data, c(0.5, 0.5))
hgtprs.grid <- ModelGrid(hgtprs.data, c(0.5, 0.5))

#Interpolate it to the point of interest
tmpprs.point <- BuildProfile(tmpprs.grid, lon, lat, TRUE)
hgtprs.point <- BuildProfile(hgtprs.grid, lon, lat, TRUE)

#Plot it!
plot(tmpprs.point - 272.15, hgtprs.point, xlab = "Temperature (C)",
     ylab = "Elevation (m)", main = paste("Temperature above Chapel Hill, NC, at",
                                          tmpprs.grid$model.run.date)) 

# A world map of surface temperature

library(GEOmap)
library(rNOMADS)

model.urls <- GetDODSDates("gfs_hd")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

time <- c(0,0) #Analysis model
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points

tmp2m.data <- DODSGrab(latest.model, latest.model.run,
                       "tmp2m", time, lon, lat)
atmos <- ModelGrid(tmp2m.data, c(0.5, 0.5), "latlon")
colormap <- rev(rainbow(500, start = 0 , end = 5/6))
image(atmos$x, sort(atmos$y), atmos$z[1,1,,], col = colormap,
      xlab = "Longitude", ylab = "Latitude",
      main = paste("World Temperature at Ground Level:", atmos$model.run.date))
plotGEOmap(coastmap, border = "black", add = TRUE,
           MAPcol = NA)

################################################################################
# External data
library("weatherData")
# Station at Alcobendas
# id_station <- "ICOMUNID127"

location <- "Madrid"

getStationCode(location)
id_station <- "LEMD"
checkDataAvailabilityForDateRange(id_station, "2014-05-28", "2014-05-29", 
                                  "airportCode")
showAvailableColumns(id_station, "2014-05-28", "2014-05-29",
                     station_type="airportCode", opt_detailed=TRUE, 
                     opt_verbose=TRUE)
getWeatherForDate(id_station, "2014-05-29", opt_detailed=TRUE)
wheather <- getDetailedWeather(id_station, "2014-05-29", "airportCode", 
                               opt_temperature_columns=FALSE, 
                               opt_all_columns=TRUE, opt_verbose=TRUE)

madrid_2014 <- getWeatherForDate(id_station, "2014-01-01", "2014-05-29", "airportCode", 
                                 opt_detailed=TRUE, opt_temperature_columns=FALSE, 
                                 opt_all_columns=TRUE, opt_verbose=TRUE)

## http://www.r-bloggers.com/r-and-the-weather/
## http://cran.r-project.org/web/packages/weatherData/index.html
## Some Code to Get Average Temperature at SFO
# JB Rickert
library(weatherData)
library(ggplot2)
library(scales)
library(plyr)

w2014 <- getWeatherForYear(id_station, 2014, station_type="airportCode",
                           opt_detailed=TRUE)

plot(w2014)

w2014$shortdate <- strftime(w2014$Time, format="%m-%d")

meanTemp <- ddply(w2014, .(shortdate), summarize, mean_T=mean(TemperatureC))
meanTemp$shortdate <- as.Date(meanTemp$shortdate, format="%m-%d")

ggplot(meanTemp, aes(shortdate, mean_T)) + geom_line() +
      scale_x_date(labels=date_format("%m/%d")) + xlab("") + ylab("Mean Temp C") +
      ggtitle("2014 Average Daily Temperature at Madrid")


################################################################################
## Practical Machine Learning
## https://class.coursera.org/predmachlearn-002/lecture

# SPAM Example
library(kernlab)
data(spam)
head(spam)

plot(density(spam$your[spam$type=="nonspam"]),
     col="blue",main="", xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]), col="red")
abline(v=0.5, col="black")

prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction, spam$type)/length(spam$type)

# In sample versus out of sample errors
library(kernlab); data(spam); set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size=10), ]
spamLabel <- (smallSpam$type=="spam")*1 + 1
plot(smallSpam$capitalAve,col=spamLabel)

# Apply Rule 1 to smallSpam
rule1 <- function(x){
      prediction <- rep(NA, length(x))
      prediction[x > 2.7] <- "spam"
      prediction[x < 2.40] <- "nonspam"
      prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
      prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
      return(prediction)
}
table(rule1(smallSpam$capitalAve), smallSpam$type)

# Apply Rule 2 to smallSpam
rule2 <- function(x){
      prediction <- rep(NA,length(x))
      prediction[x > 2.8] <- "spam"
      prediction[x <= 2.8] <- "nonspam"
      return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)

# Apply to complete spam data
table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)
mean(rule1(spam$capitalAve)==spam$type)

# Look at accuracy
sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)

# SPAM Example: Data splitting
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# SPAM Example: Fit a model
set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
modelFit
modelFit$finalModel

# SPAM Example: Prediction
predictions <- predict(modelFit, newdata=testing)
predictions

# SPAM Example: Confusion Matrix
confusionMatrix(predictions, testing$type)

# SPAM Example
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,data=training, method="glm")

# SPAM Example: K-fold
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
folds
str(folds)
# To see length of each fold
sapply(folds, length)
folds[[1]][1:10]

# SPAM Example: Return test
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE)
sapply(folds,length)
folds[[1]][1:10]

# SPAM Example: Resampling
set.seed(32323)
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds,length)
folds[[1]][1:10]

# SPAM Example: Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

# Example: Wage data
library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

# Get training/test sets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)
summary(training)
summary(testing)

# Feature plot (caret package)
featurePlot(x=training[,c("age", "education", "jobclass")],
            y = training$wage,
            plot="pairs")

# Qplot (ggplot2 package)
qplot(age, wage, data=training)

# Qplot with color (ggplot2 package)
qplot(age, wage, colour=jobclass, data=training)

# Add regression smoothers (ggplot2 package)
qq <- qplot(age, wage, colour=education, data=training)
qq +  geom_smooth(method='lm', formula=y~x)

# cut2, making factors (Hmisc package)
library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

# Boxplots with cut2
p1 <- qplot(cutWage,age, data=training, fill=cutWage, geom=c("boxplot"))
p1

# Boxplots with points overlayed
library(grid)
library(ggplot2)
library(gridExtra)
p2 <- qplot(cutWage,age, data=training,fill=cutWage, geom=c("boxplot","jitter"))
grid.arrange(p1, p2, ncol=2)

# Tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1,1)

# Density plots
qplot(wage,colour=education,data=training,geom="density")

# Why preprocess?
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

# Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS)

# Standardizing - test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(testCapAveS)
sd(testCapAveS)

# Standardizing - preProcess function
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(32343)
modelFit <- train(type ~.,data=training,preProcess=c("center","scale"),method="glm")
modelFit

# Standardizing - Box-Cox transforms
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# Standardizing - Imputing data
set.seed(13343)

# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

# Level 2: Transforming tidy covariates
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

# Load example data
library(ISLR); library(caret)
data(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,] 
testing <- Wage[-inTrain,]

# Common covariates to add, dummy variables
# Basic idea - convert factor variables to indicator variables

table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))

# Removing zero covariates
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

# Spline basis
library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis

# Fitting curves with splines
lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

# Splines on the test set
predict(bsBasis,age=testing$age)

# Correlated predictors
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
# Eliminate diagonal 
diag(M) <- 0
# Search for corralation > 0.8 (high)
which(M > 0.8, arr.ind=T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

# We could rotate the plot
# X=0.71×num415+0.71×num857
# Y=0.71×num415−0.71×num857
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

# Principal components in R - prcomp
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
str(prComp)
prComp$rotation
names(prComp)
summary(prComp)

# PCA on SPAM data
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

# PCA with caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

# Preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

# Alternative (sets # of PCs)
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

## Example: Old faithful eruptions
library(caret);data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

## Eruption duration versus waiting time
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue",
     xlab="Waiting", ylab="Duration")

## Fit a linear model 
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

## Model fit
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue",
     xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting, lm1$fitted, lwd=3)

## Predict a new value
coef(lm1)[1] + coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
newdata
predict(lm1, newdata)

## Plot predictions - training and test
par(mfrow=c(1,2))
# Training set
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting, predict(lm1), lwd=3)
# Test set
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting, predict(lm1, newdata=testFaith), lwd=3)
             
## Get training set/test set errors
# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

## Prediction intervals
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

## Same process with caret
modFit <- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)

# Example: Wage data
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

# Get training/test sets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

# Feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

# Plot age versus wage
qplot(age, wage, data=training)

# Plot age versus wage colour by jobclass
qplot(age, wage, colour=jobclass, data=training)

# Plot age versus wage colour by education
qplot(age, wage, colour=education, data=training)

# Fit a linear model
modFit<- train(wage ~ age + jobclass + education, method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

# Diagnostics
plot(finMod, 1, pch=19, cex=0.5, col="#00000010")

# Color by variables not used in the model
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

# Plot by index
plot(finMod$residuals, pch=19)
plot(finMod$residuals, pch=19, col=as.factor(finMod$xNames))

# Predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)

# If you want to use all covariates
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)

# Example: Iris Data
data(iris)
library(ggplot2)
names(iris)
table(iris$Species)

# Create training and test sets
library(caret)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

# Iris petal widths/sepal width
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)

# Plot tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

# Prettier plots
library(rattle)
fancyRpartPlot(modFit$finalModel)

# Predicting new values
predict(modFit, newdata=testing)

# Ozone data
library(ElemStatLearn)
data(ozone, package="ElemStatLearn")
# Order the data set by ozone
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess
ll <- matrix(NA, nrow=10, ncol=155)
for(i in 1:10){
      # Sample with replacement
      ss <- sample(1:dim(ozone)[1], replace=T)
      ozone0 <- ozone[ss,]
      ozone0 <- ozone0[order(ozone0$ozone),]
      loess0 <- loess(temperature ~ ozone,data=ozone0, span=0.2)
      ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for(i in 1:10){lines(1:155, ll[i,], col="grey", lwd=2)}
lines(1:155, apply(ll, 2, mean), col="red", lwd=2)

# More bagging in caret
predictors <- data.frame(ozone=ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone, temperature, col='lightgrey', pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone, predict(treebag, predictors), pch=19, col="blue")

# Iris data
data(iris)
library(ggplot2)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

# Random forests
library(caret)
modFit <- train(Species~ ., data=training, method="rf", prox=TRUE)
modFit

# Getting a single tree
getTree(modFit$finalModel, k=2)

# Class "centers"
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

# Predicting new values
pred <- predict(modFit, testing)
testing$predRight <- pred==testing$Species
table(pred, testing$Species)

qplot(Petal.Width, Petal.Length, colour=predRight, data=testing, 
      main="newdata Predictions")

# Wage example
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

# Fit the model
modFit <- train(wage ~ ., method="gbm", data=training, verbose=FALSE)
print(modFit)

# Plot the results
qplot(predict(modFit, testing), wage, data=testing)

# Example: Iris Data
data(iris); library(ggplot2)
names(iris)
table(iris$Species)

# Create training and test sets
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Build predictions
modlda = train(Species ~ ., data=training, method="lda")
modnb = train(Species ~ ., data=training, method="nb")
plda = predict(modlda, testing)
pnb = predict(modnb, testing)
table(plda, pnb)

# Comparison of results
equalPredictions = (plda==pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)

# Prostate cancer
library(ElemStatLearn); data(prostate)
str(prostate)

####
# regression subset selection in the prostate dataset
library(ElemStatLearn)
data(prostate)

covnames <- names(prostate[-(9:10)])
y <- prostate$lpsa
x <- prostate[,covnames]

form <- as.formula(paste("lpsa~", paste(covnames, collapse="+"), sep=""))
summary(lm(form, data=prostate[prostate$train,]))

set.seed(1)
train.ind <- sample(nrow(prostate), ceiling(nrow(prostate))/2)
y.test <- prostate$lpsa[-train.ind]
x.test <- x[-train.ind,]

y <- prostate$lpsa[train.ind]
x <- x[train.ind,]

p <- length(covnames)
rss <- list()
for (i in 1:p) {
      cat(i)
      Index <- combn(p,i)
      
      rss[[i]] <- apply(Index, 2, function(is) {
            form <- as.formula(paste("y~", paste(covnames[is], collapse="+"), sep=""))
            isfit <- lm(form, data=x)
            yhat <- predict(isfit)
            train.rss <- sum((y - yhat)^2)
            
            yhat <- predict(isfit, newdata=x.test)
            test.rss <- sum((y.test - yhat)^2)
            c(train.rss, test.rss)
      })
}

png("Plots/selection-plots-01.png", height=432, width=432, pointsize=12)
plot(1:p, 1:p, type="n", ylim=range(unlist(rss)), xlim=c(0,p), 
     xlab="number of predictors", ylab="residual sum of squares", 
     main="Prostate cancer data")
for (i in 1:p) {
      points(rep(i-0.15, ncol(rss[[i]])), rss[[i]][1, ], col="blue")
      points(rep(i+0.15, ncol(rss[[i]])), rss[[i]][2, ], col="red")
}
minrss <- sapply(rss, function(x) min(x[1,]))
lines((1:p)-0.15, minrss, col="blue", lwd=1.7)
minrss <- sapply(rss, function(x) min(x[2,]))
lines((1:p)+0.15, minrss, col="red", lwd=1.7)
legend("topright", c("Train", "Test"), col=c("blue", "red"), pch=1)
dev.off()

##
# ridge regression on prostate dataset
library(MASS)
lambdas <- seq(0,50,len=10)
M <- length(lambdas)
train.rss <- rep(0,M)
test.rss <- rep(0,M)
betas <- matrix(0,ncol(x),M)
for(i in 1:M){
      Formula <-as.formula(paste("y~",paste(covnames,collapse="+"),sep=""))
      fit1 <- lm.ridge(Formula,data=x,lambda=lambdas[i])
      betas[,i] <- fit1$coef
      
      scaledX <- sweep(as.matrix(x),2,fit1$xm)
      scaledX <- sweep(scaledX,2,fit1$scale,"/")
      yhat <- scaledX%*%fit1$coef+fit1$ym
      train.rss[i] <- sum((y - yhat)^2)
      
      scaledX <- sweep(as.matrix(x.test),2,fit1$xm)
      scaledX <- sweep(scaledX,2,fit1$scale,"/")
      yhat <- scaledX%*%fit1$coef+fit1$ym
      test.rss[i] <- sum((y.test - yhat)^2)
}

png(file="Plots/selection-plots-02.png", width=432, height=432, pointsize=12) 
plot(lambdas,test.rss,type="l",col="red",lwd=2,ylab="RSS",ylim=range(train.rss,test.rss))
lines(lambdas,train.rss,col="blue",lwd=2,lty=2)
best.lambda <- lambdas[which.min(test.rss)]
abline(v=best.lambda+1/9)
legend(30,30,c("Train","Test"),col=c("blue","red"),lty=c(2,1))
dev.off()


png(file="Plots/selection-plots-03.png", width=432, height=432, pointsize=8) 
plot(lambdas,betas[1,],ylim=range(betas),type="n",ylab="Coefficients")
for(i in 1:ncol(x))
      lines(lambdas,betas[i,],type="b",lty=i,pch=as.character(i))
abline(h=0)
legend("topright",covnames,pch=as.character(1:8))
dev.off()


#######
# lasso
library(lars)
lasso.fit <- lars(as.matrix(x), y, type="lasso", trace=TRUE)

png(file="Plots/selection-plots-04.png", width=432, height=432, pointsize=8) 
plot(lasso.fit, breaks=FALSE)
legend("topleft", covnames, pch=8, lty=1:length(covnames), col=1:length(covnames))
dev.off()

# this plots the cross validation curve
png(file="Plots/selection-plots-05.png", width=432, height=432, pointsize=12) 
lasso.cv <- cv.lars(as.matrix(x), y, K=10, type="lasso", trace=TRUE)
dev.off()

# Another issue for high-dimensional data
small = prostate[1:5,]
lm(lpsa ~ .,data =small)

# Example with Wage data
# Create training, test and validation sets
library(ISLR)
data(Wage)
Wage
str(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select=-c(logwage))

# Create a building data set and validation set
inBuild <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]
str(training)
str(validation)
str(testing)

dim(training)
dim(testing)
dim(validation)

# Build two different models
mod1 <- train(wage ~., method="glm", data=training)
mod2 <- train(wage ~., method="rf",
              data=training, 
              trControl=trainControl(method="cv"), number=3)

# Predict on the testing set
pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
qplot(pred1, pred2, colour=wage, data=testing)

# Fit a model that combines predictors
predDF <- data.frame(pred1, pred2, wage=testing$wage)
predDF
str(predDF)
combModFit <- train(wage ~., method="gam", data=predDF)
str(combModFit)
summary(combModFit)
combPred <- predict(combModFit, predDF)
str(combPred)
summary(combPred)

# Testing errors
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

# Predict on validation data set
pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1=pred1V, pred2=pred2V)
combPredV <- predict(combModFit, predVDF)

# Evaluate on validation
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))

# Google data
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.dat, to = to.dat)
head(GOOG)

# Summarize monthly and store as time series
# Get timezone
Sys.getenv("TZ")

# Change the timezone
Sys.setenv(TZ="America/Chicago")
Sys.setenv(TZ="Europe/Madrid")

# mGoog <- to.monthly(GOOG)
mGoog <- to.monthly(GOOG[,-5])
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1, xlab="Years+1", ylab="GOOG")

# Decompose a time series into parts
plot(decompose(ts1), xlab="Years+1")

# Training and test sets
ts1Train <- window(ts1, start=1, end=5)
ts1Test  <- window(ts1, start=5, end=(7-0.01))
ts1Train
ts1Test

# Simple moving average
plot(ts1Train)
lines(ma(ts1Train, order=3),col="red")

# Exponential smoothing
ets1 <- ets(ts1Train, model="MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Test, col="red")

# Get the accuracy
accuracy(fcast, ts1Test)

# Iris example ignoring species labels
data(iris)
library(ggplot2)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

# Cluster with k-means
kMeans1 <- kmeans(subset(training, select=-c(Species)), centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour=clusters, data=training)

# Compare to real labels
table(kMeans1$cluster, training$Species)

# Build predictor
modFit <- train(clusters ~., data=subset(training, select=-c(Species)), 
                method="rpart")
table(predict(modFit, training), training$Species)

# Apply on test
testClusterPred <- predict(modFit, testing) 
table(testClusterPred, testing$Species)








################################################################################
## Developing Data Products
## https://class.coursera.org/devdataprod-002/lecture

# Shinyapps
devtools::install_github("rstudio/shinyapps")
shinyapps::setAccountInfo(name='smota', token='ADD450AEA389719D3BE963A44A25C8CC', secret='CmN6t/1tqkawfyIYPoJS9xi39pSkOZmBszSywEdH')
deployApp()
runApp()

## Example from the regression class
library(manipulate)
myHist <- function(mu){
      hist(galton$child,col="blue",breaks=100)
      lines(c(mu, mu), c(0, 150),col="red",lwd=5)
      mse <- mean((galton$child - mu)^2)
      text(63, 150, paste("mu = ", mu))
      text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))


################################################################################

mtcars2 <- mtcars[order(-mtcars$mpg), ]
par(cex.lab=1, cex.axis=.6, 
    mar=c(6.5, 3, 2, 2) + 0.1, xpd=NA) #shrink axis text and increas bot. mar.

barX <- barplot(mtcars2$mpg,xlab="Cars", main="MPG of Cars", 
                ylab="", names=rownames(mtcars2), mgp=c(5,1,0),
                ylim=c(0, 35), las=2, col=mtcars2$cyl)

mtext(side=2, text="MPG", cex=1, padj=-2.5)
text(cex=.5, x=barX, y=mtcars2$mpg+par("cxy")[2]/2, mtcars2$hp, xpd=TRUE)

###############################################################################

library(devtools)
install_github("slidify", "ramnathv")
install_github("slidifyLibraries", "ramnathv")
library(slidify)
author("Practical_Machine_Learning_Project1")
slidify("index.Rmd")
library(knitr)
browseURL("index.html")
public_github()

