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

################################################################################
# Change some classes
training$classe         <- as.factor(training$classe)
training$user_name      <- as.factor(training$user_name)
training$new_window     <- factor(training$new_window, labels=c("no", "yes"), 
                                  levels=c("no", "yes"))
# training$cvtd_timestamp <- as.POSIXct(strptime(training$cvtd_timestamp, 
#                                               "%d/%m/%Y %H:%M"))

testing$user_name      <- as.factor(testing$user_name)
testing$new_window     <- factor(testing$new_window, labels=c("no", "yes"), 
                                 levels=c("no", "yes"))
# testing$cvtd_timestamp <- as.POSIXct(strptime(testing$cvtd_timestamp, 
#                                                "%d/%m/%Y %H:%M"))

# Print class of all variables in dataset
sapply(training[1,], class)
classes <- sapply(training[1,], class)
table(classes)
classes[classes=="character"]
names(classes[classes=="character"])
classes_character <- names(classes[classes=="character"])
summary(training[, classes_character])
# temp  <- training
# temp2 <- testing
sapply(training[, classes_character], as.double(as.character))
# summary(temp[, names(classes[classes=="character"])])

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

summary(training)
sapply(training[1,], class)

classes2 <- sapply(testing[1,], class)
table(classes2)
classes2[classes2=="logical"]
names(classes2[classes2=="logical"])
classes2_logical <- names(classes2[classes2=="logical"])
summary(testing[, classes2_logical])

for (i in 1:100) {
      print(classes2_logical[i])
      print(table(testing[, classes2_logical[i]]))
}

for (i in 2:100) {
      # testing[, classes2_logical[i]] <- as.numeric(as.character(testing[, classes2_logical[i]]))
      testing[, classes2_logical[i]] <- as.numeric(testing[, classes2_logical[i]])
      print(classes2_logical[i])
      print(table(testing[, classes2_logical[i]]))
      print(class(testing[, classes2_logical[i]]))
}

summary(testing)
sapply(testing[1,], class)

classes2b <- sapply(testing[1,], class)
table(classes2b)



save(training, file="training.RData")
save(testing,  file="testing.RData")


# apply normalization to entire data frame
# library(BiocGenerics)
# training_norm <- as.data.frame(lapply(training, normalize))

################################################################################
# Models

################################################################################

################################################################################
# Change to main directory
setwd("./")
# Create figure directory
if(!file.exists("./figure")){dir.create("./figure")}
# Print to png file
dev.copy(png, file="./figure/plot1.png", width=480, height=480)  ## Copy my plot to a PNG file
dev.off()