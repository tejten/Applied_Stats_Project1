#Read in the data---------------------------------
df.orig <- read.csv("data/AMES_train.csv", stringsAsFactors=FALSE)

#Check number of NA's
missing <- colSums(is.na(df.orig))
missing

#CLEAN THE DATA-----------------------------------
#df.clean is where we store the cleaned data
df.clean <- df.orig

#The actual cleaning of the data that has NA's
df.clean$LotFrontage[is.na(df.clean$LotFrontage)] <- 1  #1 in case we take the log
df.clean$Alley[is.na(df.clean$Alley)] <- "NoAlley"
df.clean$MasVnrType[is.na(df.clean$MasVnrType)] <- "None"
df.clean$MasVnrArea[is.na(df.clean$MasVnrArea)] <- 0
df.clean$BsmtQual[is.na(df.clean$BsmtQual)] <- "NoBsmt"
df.clean$BsmtCond[is.na(df.clean$BsmtCond)] <- "NoBsmt"
df.clean$BsmtExposure[is.na(df.clean$BsmtExposure)] <- "NoBsmt"
df.clean$BsmtFinType1[is.na(df.clean$BsmtFinType1)] <- "NoBsmt"
df.clean$BsmtFinType2[is.na(df.clean$BsmtFinType2)] <- "NoBsmt"
df.clean$Electrical[is.na(df.clean$Electrical)] <- "Unknown"
df.clean$FireplaceQu[is.na(df.clean$FireplaceQu)] <- "NoFireplace"
df.clean$GarageType[is.na(df.clean$GarageType)] <- "NoGarage"
#Sets NA's ty the average between the two dates: YearBuilt, YearRemodAdd
df.clean$GarageYrBlt <- ifelse( is.na(df.clean$GarageYrBlt), 
  round((df.clean$YearBuilt + df.clean$YearRemodAdd)/2),
  df.clean$GarageYrBlt )
df.clean$GarageFinish[is.na(df.clean$GarageFinish)] <- "NoGarage"
df.clean$GarageQual[is.na(df.clean$GarageQual)] <- "NoGarage"
df.clean$GarageCond[is.na(df.clean$GarageCond)] <- "NoGarage"
df.clean$PoolQC[is.na(df.clean$PoolQC)] <- "NoPool"
df.clean$Fence[is.na(df.clean$Fence)] <- "NoFence"
df.clean$MiscFeature[is.na(df.clean$MiscFeature)] <- "None"

#Print out number of NA's per row to ensure no NA's
colSums(is.na(df.clean))

#50% of the total number of rows
length(df.orig$Id)/2

#Column names with 50% or more data missing:
#Alley, PoolQC, Fence, MiscFeature
#Remove those columns?... I did
df.clean$Alley <- NULL
df.clean$PoolQC <- NULL
df.clean$Fence <- NULL
df.clean$MiscFeature <- NULL

#OBJECTIVE 1-------------------------------------------------------

#Gets only the numeric values for the scatterplots
df.clean.numeric <- df.clean[, sapply(df.clean, is.numeric)]
#Plots all the pairs TAKES FOREVER
pairs(df.clean.numeric[c(38, 2:37)], gap = 0)#, pch=".")
#In plots tab Export-Image, Save as a png: 2048x2048
#If you don't it is way too small to see

#Gets the correlation of some strength (0.7 = all 70%+ correlation )
getcorr <- function(data.clean.numeric, corrStr)
{
  library(tibble)
  library(dplyr)
  library(tidyr)

  #Saves the correlation pairs and value 
  df.clean.cor <- data.clean.numeric %>% 
    as.matrix %>%
    cor %>%
    as.data.frame %>%
    rownames_to_column(var = 'var1') %>%
    gather(var2, value, -var1)
  names(df.clean.cor) <- c("Parameter1", "Parameter2", "CorStrength" )
  
  #Get the ones with only 'corrStr' amount of correlation
  #Saves them into res
  res <- filter(df.clean.cor, (CorStrength >= corrStr | CorStrength <= -corrStr ) & CorStrength != 1)
  
  #Finds rows [A,B,123], [B,A,123] and removes them (duplicates)
  rmrow <- numeric()
  len <- length(res$Parameter1)
  for( i in c(1:(len-1)) )
  {
    num <- i+1
    for(j in c(num:len))
    {
      if( (res[i,1] == res[j,2]) & (res[i,2] == res[j,1]) )
      {
        rmrow <- c(rmrow, j)
      }
    }
  }
  res <- res[-rmrow,]

  #Returns res
  res
}
#Creates a vector of indices of the correlated columns
corrcolindex <- function(getcorr.data, d)
{
  #getcorr.data is the result of getcorr
  #d is the numeric data that you input into getcorr
  
  vec <- unique( c(as.vector(getcorr.data$Parameter1), as.vector(getcorr.data$Parameter2)) )
  nm <- names(d)
  rownm <- numeric()
  for(i in c(1:length(vec)))
  {
    for(j in c(1:length(nm)))
    {
      if(vec[i] == nm[j])
      {
        rownm <- c(rownm, j)
      }
    }
  }
  rownm
}

#Get the correlations stronger than some number
df.clean.cor <- getcorr(df.clean.numeric[,2:37], 0.7)
df.clean.cor.indexes <- corrcolindex(df.clean.cor, df.clean.numeric)

#Shows the pairs of only the correlated values and saleprice
pairs(df.clean.numeric[,c(38,df.clean.cor.indexes)], gap = 0, pch = ".")



#RANDOM STUFF IGNORE EVERYTHING BELOW THIS LINE-----------------------
#---------------------------------------------------------------------
#Regression Model
#No: Blocking, Blinding, Stratification
#Unit 2 slides 25-27 have workflow for this

#Forward, Backward, Stepwise, Lasso, Custom
#Check: Normality, Independence, Constant Variance
#Check: Levarage, VIF, Cook's D
#Compare AIC, ADJ R2, BIC, CVPress, External Cross Validation
#Interpretation

#OBJECTIVE 2-------------------------------------------------------
#Categorical Factors: econ__economic_typology, area__urban_influence, area__rucc
#2 way ANOVA, NOT Time series


