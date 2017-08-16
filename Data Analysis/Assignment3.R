# 1 import dataset .csv
#load package dplyr from library

library(dplyr)

#1. aread forestfires.csv from lbrary
dfForest = read.csv("D:/UCSC Extension Sillicon Valley/Data Analysis/Assignments/forestfires.csv", header = TRUE , sep = ',')
nrow(dfForest)
str(dfForest)     #how many observations str(dfForest)
tbl_df(dfForest)    #how is the df

# 1.b how many observations are there with a fire (i.e., area>0)
f_dfForestAreaGreater0 <- filter(dfForest, area > 0)
str(f_dfForestAreaGreater0)
tbl_df(f_dfForestAreaGreater0)

# 1.c how many observations are there with a rain ((i.e.,rain>0))
f_dfForestRainGreater0 <- filter(dfForest, rain > 0)
str(f_dfForestRainGreater0)
head(f_dfForestRainGreater0)
tbl_df(f_dfForestRainGreater0)

# 1.d How many observations are there with both a fire and rain
f_dfForestAreaAndRainGreater0 <- filter(dfForest, rain > 0 &  area > 0)
str(f_dfForestAreaAndRainGreater0)
head(f_dfForestAreaAndRainGreater0)
tbl_df(f_dfForestAreaAndRainGreater0)

#2 Show the colunms month, day, area, of the all the observations
df_Forest_Month_Day_area <- select(dfForest,month , area, day)
str(df_Forest_Month_Day_area)
head(df_Forest_Month_Day_area)
tbl_df(df_Forest_Month_Day_area)

#3 Show the columns month, day, area of the observations with a fire.
df_Forest_Month_Day_area_With_A_Fire <- df_Forest_Month_Day_area[df_Forest_Month_Day_area$area >0,]
str(df_Forest_Month_Day_area_With_A_Fire)
head(df_Forest_Month_Day_area_With_A_Fire)
tbl_df(df_Forest_Month_Day_area_With_A_Fire)

df_FilterAreaGreater0 <- filter(dfForest, area >0)
str(df_FilterAreaGreater0)


#4 How large are the five largest fires (i.e. having largest area)? 
top5ByArea <- top_n(dfForest[order(decreasing = TRUE, dfForest$area),],5)
tbl_df(top5ByArea)

#4.a What are the corresponding month, temp, RH, wind, rain, area?
select(top5ByArea,month , temp, RH, wind, rain, area)
top5ByMonthTempRHWindRainarea <- top_n(select(top5ByArea,month , temp, RH, wind, rain, area),5)
tbl_df(top5ByMonthTempRHWindRainarea)

#4.b Reorder factor levels of month to be from Jan to Dec.
levels(dfForest$month)
dfForest$month = factor(dfForest$month, levels = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))
levels(dfForest$month)

# Add one column to the data indicating whether a fire occurred for each observation ('TRUE' for area>0 and 'FALSE' for area==0).

dfForestFireOcurred <- dfForest %>% mutate(fire=area>0)
tbl_df(dfForestFireOcurred)


