setwd("E:/Varios/GitHub/Practical_Machine_Learning_Project1")

## Santiago Mota
## Practical Machine Learning
## Course Project 1
## https://class.coursera.org/predmachlearn-002/human_grading/view/courses/972090/assessments/4/submissions
## https://github.com/santiagomota/

library(caret)

# Create data directory
if(!file.exists("./data")){dir.create("./data")}

# Download train file
urls <- ("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
download.file(urls, "./data/pml-training.csv")

# Download test file
urls <- ("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
download.file(urls, "./data/pml-testing.csv")

# Change to data directory
setwd("./data/")

# Load datasets
training <- read.csv("pml-training.csv", header=TRUE, sep=",",
                     stringsAsFactors=FALSE)
testing  <- read.csv("pml-testing.csv", header=TRUE, sep=",",
                     stringsAsFactors=FALSE)

################################################################################
# Some analysis
str(training)
summary(training)

# Classe class
table(training$classe)
table(testing$classe)
summary(training$classe)
str(training$classe)

hist(as.numeric(as.factor(training$classe)))
boxplot(as.numeric(as.factor(training$classe)))
featurePlot(x=training[,c("user_name","new_window","num_window", "X")],
            y = training$classe,
            plot="pairs")

names(training)
sapply(training[1,], class)

# Print class of all variables in dataset
sapply(training[1,], class)
classes1 <- sapply(training[1,], class)
table(classes1)
classes2 <- sapply(testing[1,], class)
table(classes2)

################################################################################
# Change some classes
training$classe         <- as.factor(training$classe)
training$user_name      <- as.factor(training$user_name)
training$new_window     <- factor(training$new_window, labels=c("no", "yes"), 
                                  levels=c("no", "yes"))
# training$cvtd_timestamp <- as.POSIXct(strptime(training$cvtd_timestamp, 
#                                                "%d/%m/%Y %H:%M"))

testing$user_name      <- as.factor(testing$user_name)
testing$new_window     <- factor(testing$new_window, labels=c("no", "yes"), 
                                 levels=c("no", "yes"))
# testing$cvtd_timestamp <- as.POSIXct(strptime(testing$cvtd_timestamp, 
#                                               "%d/%m/%Y %H:%M"))

classes1 <- sapply(training[1,], class)
table(classes1)
classes2 <- sapply(testing[1,], class)
table(classes2)

classes1[classes1=="character"]
names(classes1[classes1=="character"])
classes_character <- names(classes1[classes1=="character"])
summary(training[, classes_character])

for (i in 1:34) {
  print(classes_character[i])
  print(table(training[, classes_character[i]]))
}

for (i in 2:34) {
  training[, classes_character[i]][training[, classes_character[i]]==""] <- NA
  training[, classes_character[i]][training[, classes_character[i]]=="#DIV/0!"] <- Inf
  training[, classes_character[i]] <- as.numeric(as.character(training[, classes_character[i]]))
  print(classes_character[i])
  print(table(training[, classes_character[i]]))
  print(class(training[, classes_character[i]]))
}

sapply(training[, classes_character], as.numeric)
classes1 <- sapply(training[1,], class)
table(classes1)

table(training$classe)
table(training$user_name)
table(training$new_window)
table(training$classe, training$new_window)
table(training$classe, training$num_window)
plot(training$classe, training$num_window)

table(training$min_yaw_forearm)
table(training$max_yaw_forearm)
table(training$cvtd_timestamp)
table(training$new_window)

summary(training)
sapply(training[1,], class)

classes2[classes2=="logical"]
names(classes2[classes2=="logical"])
classes2_logical <- names(classes2[classes2=="logical"])
summary(testing[, classes2_logical])

for (i in 1:100) {
  print(classes2_logical[i])
  print(table(testing[, classes2_logical[i]]))
}

for (i in 1:100) {
  # testing[, classes2_logical[i]] <- as.numeric(as.character(testing[, classes2_logical[i]]))
  testing[, classes2_logical[i]] <- as.numeric(testing[, classes2_logical[i]])
  print(classes2_logical[i])
  print(table(testing[, classes2_logical[i]]))
  print(class(testing[, classes2_logical[i]]))
}

summary(testing)
sapply(testing[1,], class)

classes2 <- sapply(testing[1,], class)
table(classes2)
table(classes1)

table(classes1, classes2)

save(training, file="training.RData")
save(testing,  file="testing.RData")


# apply normalization to entire data frame
# library(BiocGenerics)
# training_norm <- as.data.frame(lapply(training, normalize))

################################################################################
# Extract variables with values not NA in a new DF
mean0 <- sapply(training, mean)
# training_mod <- training[, na.omit(names(mean0)[as.integer(as.numeric(!(mean0=="NA"))==1]]
training_mod <- training[, na.omit(names(mean0)[as.numeric(!(mean0=="NA"))==1])]
testing_mod <-  testing[, names(training_mod)]

# Include user_name and classe
training_mod$user_name <- training$user_name
testing_mod$user_name  <- testing$user_name
training_mod$classe    <- training$classe

# Complete DF with training and testing data. We change classe variable to numeric
# Values of test set are classe = 6
temp <- training
temp$classe <- as.numeric(temp$classe)
temp2 <- testing
temp2$problem_id <- NULL
temp2$classe <- 6

all <- rbind(temp, temp2)

# Delete temp DF
rm(temp)
rm(temp2)

names(all)

# One DF only with 4 columns 
minimum <- all[, c(2,5,6,160)]

minimum$cvtd_timestamp <- as.POSIXct(strptime(minimum$cvtd_timestamp, "%d/%m/%Y %H:%M"))
minimum <- minimum[order(minimum$user_name, minimum$cvtd_timestamp),]

# Some plots to study data
plot(minimum$cvtd_timestamp, minimum$classe, col=minimum$user_name, pch=19)
qplot(cvtd_timestamp, classe, data=all, geom="jitter", colour=factor(user_name), 
      main="Classe by time", ylab="Classe", xlab="Time")

qplot(cvtd_timestamp, classe, data=all, geom="jitter", colour=factor(user_name), 
      main="Classe by time", ylab="Classe", xlab="Time") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Principal componets analysis
pca.out <- prcomp(training_mod[, -c(57, 58)], scale.=TRUE)
pca.out
summary(pca.out)
biplot(pca.out, scale=0)
biplot(pca.out, scale=0, cex=.6)

plot(pca.out$x[,1], pca.out$x[,2], xlab="PC1", ylab="PC2")

str(pca.out)
pca.out$rotation
names(pca.out)


# Correlation
M_cor <- abs(cor(training[,-c(2, 5, 6, 160)]))
M_cor
diag(M_cor) <- 0
which(M_cor>0.8, arr.ind=T)

# variables with cor > 0.8
new_names <- row.names(which(M_cor>0.8, arr.ind=T))
new_names
# New Df with variables with cor > 0.8. We also include timestamp and classe
training_mod3 <- training[,new_names]
training_mod3$cvtd_timestamp <- training$cvtd_timestamp
training_mod3$classe         <- training$classe

testing_mod3 <- testing[,new_names]
testing_mod3$cvtd_timestamp <- testing$cvtd_timestamp
testing_mod3$classe         <- testing$classe
################################################################################
# Models. We use the training_mod3 dataset. 57 + classe variables

# Random Forest
model30 <- train(classe ~., method="rf", data=training_mod3, 
                 trControl=trainControl(method="cv"), number=3)
pred30_train <- predict(model30, training_mod3)
table(pred30_train, training_mod3$classe)
pred30 <- predict(model30, testing_mod3)
print(model30)
pred30
confusionMatrix(pred30_train, training_mod3$classe)

### Best one
# Bagging 
library(ipred)
model31 <- bagging(classe ~., data=training_mod3, nbagg=25)
pred31_train <- predict(model31, training_mod3)
table(pred31_train, training_mod3$classe)
pred31 <- predict(model31, testing_mod3)
print(model31)
pred31
confusionMatrix(pred31_train, training_mod3$classe)

# C5.0
model32 <- train(classe ~., method = "C5.0", data=training_mod3)
pred32_train <- predict(model32, training_mod3)
table(pred32_train, training_mod3$classe)
pred32 <- predict(model32, testing_mod3)
print(model32)
pred32
confusionMatrix(pred32_train, training_mod3$classe)

################################################################################
# Prediction Assignment Submission: Instructions Help

# Please apply the machine learning algorithm you built to each of the 20 test 
# cases in the testing data set. For more information and instructions on how to 
# build your model see the prediction assignment writeup. For each test case you 
# should submit a text file with a single capital letter (A, B, C, D, or E) 
# corresponding to your prediction for the corresponding problem in the test 
# data set. You get 1 point for each correct answer. You may submit up to 2 
# times for each problem. I know it is a lot of files to submit. It may be 
# helpful to use the following function to create the files. If you have a 
# character vector with your 20 predictions in order for the 20 problems. So 
# something like (note these are not the right answers!):

# answers = rep("A", 20)
answers <- as.character(pred30)
# B A B A A E D B A A B C B A E E A B B B

# then you can load this function by copying and pasting it into R:
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i], file=filename, quote=FALSE, row.names=FALSE, 
                col.names=FALSE)
  }
}

# then create a folder where you want the files to be written. Set that to be 
# your working directory and run:

pml_write_files(answers)

# and it will create one file for each submission. Note: if you use this script, 
# please make sure the files that get written out have one character each with 
# your prediction for the corresponding problem ID. I have noticed the script 
# produces strange results if the answers variable is not a character vector. 



################################################################################
# Change to main directory
setwd("./")
# Create figure directory
if(!file.exists("./figure")){dir.create("./figure")}
# Print to png file
dev.copy(png, file="./figure/plot1.png", width=480, height=480)  ## Copy my plot to a PNG file
dev.off()