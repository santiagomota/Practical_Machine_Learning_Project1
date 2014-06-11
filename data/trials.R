# classes0 <- classes2
# classes0[classes2=="logical"] <- "numeric"
# classes0[1] <- "character"
# table(classes0)

# testing  <- read.table("pml-testing.csv", header=TRUE, sep=",",
#                        stringsAsFactors=FALSE, colClasses=classes0)

# testing  <- read.csv("pml-testing.csv", header=TRUE, sep=",",
#                      stringsAsFactors=FALSE, colClasses=classes0)
