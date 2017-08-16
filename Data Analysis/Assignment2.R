# 1.  Create two matrices from the given set of numbers X1 = {2,3,7,1,6,2,3,5,1}
#     and x2={3,2,9,0,7,8,5,8,2}

X1 <- matrix (c(2,3,7,1,6,2,3,5,1), nrow = 3, ncol = 3)
X2 <- matrix (c(3,2,9,0,7,8,5,8,2), nrow = 3, ncol = 3) 

X1
X2

# 2. Find the matrix product.

matrixMultiplication <- X1*X2
matrixMultiplication


matrixMultiplication2 <- X1%*%X2
matrixMultiplication2

# 3. Find the inverse of the matrix and prove that it is correct.

matrixInverseX1 <- X1 %*% solve(X1)
matrixInverseX1

solve(X1)

matrixInverseX2 <- X2 %*% solve(X2)
matrixInverseX2

matrixInverseX3 <- matrixMultiplication %*% solve(matrixMultiplication)
matrixInverseX3

matrixInverseX4 <- matrixMultiplication2 %*% solve(matrixMultiplication2)
matrixInverseX4

# 4. Load Animals from MASS Package

library(MASS)
dataAnimals = Animals
dataAnimals

# 5. Find the correlation coefficient of brain and body in this data set and
#    comment on the relation between them

(animalsCorrelation <- cor(dataAnimals))


#6 load USArrest data set. Comment on the distribution of the Variables. Is there
#   Any relation between UrbanPop and three crimes

library(MASS)
dataUSArrests <- USArrests
dataUSArrests
str(dataUSArrests)
summary(dataUSArrests)

dataUSArrestsCorrelation <- cor(dataUSArrests)
cor(dataUSArrests$UrbanPop, dataUSArrests$Murder)


# The correlation coefficient 0.06957262  
# Since it is rather close to 1, we can conclude that the variables are positively 
# linearly related, so if the states have more population the crime is related positively, 
# hence will increase.

#7 Which states has most and least assault, murder, rape and arrests.

maxMurder <- dataUSArrests[which.max(dataUSArrests$Murder),]
maxMurder
maxAssault <- dataUSArrests[which.max(dataUSArrests$Assault),]
maxAssault
maxRape <- dataUSArrests[which.max(dataUSArrests$Rape),]
maxRape

minMurder <- dataUSArrests[which.min(dataUSArrests$Murder),]
minMurder
minAssault <- dataUSArrests[which.min(dataUSArrests$Assault),]
minAssault
minRape <- dataUSArrests[which.min(dataUSArrests$Rape),]
minRape

# 8 List the states which have assault arrests more than median of the country

dataMeanMoreThanAssault <- dataUSArrests[dataUSArrests$Assault > (mean(dataUSArrests$Assault)),]
dataMeanMoreThanAssault

# 9  Which states are in the bottom 25% of the murder

totalMurder <- sum(USArrests$Murder)
summary(dataUSArrests)

more25percenTotalMurder <- dataUSArrests[which(dataUSArrests$Murder >= 11.250),]
more25percenTotalMurder
str(more25percenTotalMurder)
str(dataUSArrests)


#10 Which states are in the top 25% of the murder

less25percenTotalMurder <- dataUSArrests[which(dataUSArrests$Murder <= 4.075),]
less25percenTotalMurder
str(less25percenTotalMurder)
str(dataUSArrests)
