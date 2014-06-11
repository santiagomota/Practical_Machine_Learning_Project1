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
#                                               "%d/%m/%Y %H:%M"))

testing$user_name      <- as.factor(testing$user_name)
testing$new_window     <- factor(testing$new_window, labels=c("no", "yes"), 
                                 levels=c("no", "yes"))
# testing$cvtd_timestamp <- as.POSIXct(strptime(testing$cvtd_timestamp, 
#                                                "%d/%m/%Y %H:%M"))

classes1 <- sapply(training[1,], class)
table(classes1)
classes2 <- sapply(testing[1,], class)
table(classes2)

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

classes1[classes1=="character"]
names(classes1[classes1=="character"])
classes_character <- names(classes1[classes1=="character"])
summary(training[, classes_character])

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
# Models

model1 <- train(classe ~., method="glm", data=training, 
                preProcess=c("center", "scale"))
model2 <- train(classe ~., method="rf",
                data=training, 
                trControl = trainControl(method="cv"), number=3)
model3 <- train(classe ~., method="lda", data=training)
model4 <- train(classe ~., method="nb", data=training)
model5 <- train(classe ~., method = "C5.0", data=training)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 5)
model6 <- train(classe ~., method="gbm", data=training, trControl=ctrl)
names(model6)
pred6 <- predict(model6, newdata=testing)
ctrl <- trainControl(method="cv", number=10)
model7 <- train(classe ~., data=training, method="treebag", trControl=ctrl)
names(model7)
pred7 <- predict(model7, newdata=testing)
# Here, a partial least squares discriminant analysis (PLSDA) model 
model8  <- train(classe ~., data=training, method="pls",
                 # Center and scale the predictors for the training
                 # set and all future samples.
                 preProc = c("center", "scale"))
model9  <- train(classe ~., data=training, method="bag")

model10 <- train(classe ~., data=training, method="svmRadial",
                 # This pre-processing will be applied to
                 # these data and new samples too.
                 preProc=c("center", "scale"),
                 # Tune over different values of cost
                 tuneLength=10, trControl=ctrl, metric="ROC")

library(ipred)
model20 <- bagging(classe ~., data=training, nbagg=25)
pred20_train <- predict(model20, training)
table(pred20_train, training$classe)



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





vmbag <- train(default ~ ., data = credit, "bag",
               trControl = ctrl, bagControl = bagctrl)

# auto-tune a random forest
grid_rf <- expand.grid(.mtry = c(2, 4, 8, 16))

set.seed(300)
m_rf <- train(default ~ ., data = credit, method = "rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)



rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ .,
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")

gbmTune <- train(x = churnTrain[, predictors],
                 y = churnTrain$churn,
                 method = "gbm")

# or, using the formula interface
gbmTune <- train(churn ~ ., data = churnTrain, method = "gbm")

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
gbmTune <- train(churn ~ ., data = churnTrain,
                 method = "gbm",
                 metric = "ROC",
                 verbose = FALSE,
                 trControl = ctrl)

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

modFit <- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)

modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)

modFit <- train(Species~ ., data=training, method="rf", prox=TRUE)
modFit

modFit <- train(wage ~ ., method="gbm", data=training, verbose=FALSE)
print(modFit)

# Plot the results
qplot(predict(modFit, testing), wage, data=testing)




################################################################################

################################################################################
# Change to main directory
setwd("./")
# Create figure directory
if(!file.exists("./figure")){dir.create("./figure")}
# Print to png file
dev.copy(png, file="./figure/plot1.png", width=480, height=480)  ## Copy my plot to a PNG file
dev.off()