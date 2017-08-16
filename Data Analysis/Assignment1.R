# 3 Create a vector of positive odd integers less than 100

Vector1 <-  seq(1,100,by = 2)
Vector1
summary(Vector1)
mean(Vector1)
sd(Vector1)
library(modeest)
mfv(Vector1)


# 4 Remove the values greater than 60 and less than 80

Vector2 <-  Vector1[ (Vector1 > 0 & Vector1 <=60 | Vector1 >=80) ]
Vector2


# 5 Find the five number summary of the remaining set of values

summary(Vector2)

# 6 Consider the following vector of values. X <-{8, 14, 9, 15, NA, 8,13, 2,9, NA}
#   Write an R program to return the positions ofthe missing values

Vector3 <- c(8, 14, 9, 15, NA, 8,13, 2,9, NA)
summary(Vector3)
Vector3
mean(Vector3)
length(which(!is.na(Vector3)))

# 7 Write an R program to count the number of non-missing values

is.na.data.frame(Vector3)
which(is.na(Vector3))

# 8 Write an R program to replace the missing values with the mean
# of the non-missing values.

Vector3[which(is.na(Vector3))] <- mean(Vector3,na.rm = TRUE)
Vector3


# 9 Load mtcars data. Write an R program that
#   will rearrange the rows of the data frame so
#   that they are sorted by the value of
#   Horsepower.

DB_Cars<-mtcars
new_DB_Cars_Ascendent<-DB_Cars[order(DB_Cars$hp),]
head(new_DB_Cars_Ascendent)
new_DB_Cars_Descendent<-DB_Cars[order(decreasing = TRUE,DB_Cars$hp),]
head(new_DB_Cars_Descendent)

## 10 Write an R program to count the number of
##    observations with cylinders greater than 4 and
##    gear greater than 3.

DB_Cars_cylandgear <- DB_Cars[DB_Cars$cyl>4 & DB_Cars$gear>3,]
DB_Cars_cylandgear

