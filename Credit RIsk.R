library(ade4)
library(glmnet)
library(randomForest)

set.seed(1234)

training_file = "./GiveMeSomeCredit/cs-training.csv"
training_data = read.csv(training_file,header=TRUE,stringsAsFactors=FALSE,row.names=1)

training_dlqin_percent = mean(training_data["SeriousDlqin2yrs"]==1)
#since training_dlqin_percent = 0.06684, the data set is heavily imbalanced.


############# Data Analysis and Cleaning ################
training_data_mod = training_data
#Checking missing data in the data set
for (col in names(training_data)){
  print(c(col,mean(is.na(training_data[col]))))
}
#The columns with missing values are MonthlyIncome (19.82%) and NumberOfDependents ("2.61%")
#Simply fill the missing values with NA


training_data_mod[is.na(training_data["MonthlyIncome"]), "MonthlyIncome"] = -1 # the median is 5400
training_data_mod[is.na(training_data["NumberOfDependents"]), "NumberOfDependents"] = -1
#median(training_data$NumberOfDependents,na.rm=T)


#Look at RevolvingUtilizationOfUnsecuredLines column
summary(training_data["RevolvingUtilizationOfUnsecuredLines"])
bins = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1, 1.2, Inf)
training_data_mod["RevolvingUtilizationOfUnsecuredLinesGrouped"] = cut(training_data_mod$RevolvingUtilizationOfUnsecuredLines, bins , right=FALSE)

#Look at Age column
summary(training_data_mod["age"])
bins = c(0, 25, 35, 55, 65, 80, Inf)
training_data_mod["ageGrouped"] = cut(training_data_mod$age, bins , right=FALSE)



#Look at DebtRatio column
summary(training_data_mod["DebtRatio"])
bins = c(0 ,0.2, 0.4,  0.6, 0.8, 1, 1.2, 1.5, 2, 3, 5, 10, Inf)
training_data_mod["DebtRatioGrouped"] = cut(training_data_mod$DebtRatio, bins , right=FALSE)

#Look at MonthlyIncome column
summary(training_data_mod["MonthlyIncome"])
hist(training_data_mod$MonthlyIncome[training_data_mod$SeriousDlqin2yrs==1 & training_data_mod$MonthlyIncome<10000])
hist(training_data_mod$MonthlyIncome[training_data_mod$SeriousDlqin2yrs==0& training_data_mod$MonthlyIncome<10000])
bins = c(-1,0, 1, 1000,2000,3000,4000,5000,6000,8000, 10000, Inf)
training_data_mod["MonthlyIncomeGrouped"] = cut(training_data_mod$DebtRatio, bins , right=FALSE)



#Look at NumberOfOpenCreditLinesAndLoans
summary(training_data_mod["NumberOfOpenCreditLinesAndLoans"])
hist(training_data_mod$NumberOfOpenCreditLinesAndLoans[training_data_mod$SeriousDlqin2yrs==0 & training_data_mod$NumberOfOpenCreditLinesAndLoans<15])
hist(training_data_mod$NumberOfOpenCreditLinesAndLoans[training_data_mod$SeriousDlqin2yrs==1 & training_data_mod$NumberOfOpenCreditLinesAndLoans<15])
bins = c(0, 1, 3, 8, 10,20, Inf)
training_data_mod["NumberOfOpenCreditLinesAndLoansGrouped"] = cut(training_data_mod$NumberOfOpenCreditLinesAndLoans, bins , right=FALSE)


#Look at NumberRealEstateLoansOrLines
summary(training_data_mod["NumberRealEstateLoansOrLines"])
hist(training_data_mod$NumberRealEstateLoansOrLines[training_data_mod$SeriousDlqin2yrs==0 & training_data_mod$NumberRealEstateLoansOrLines<5])
hist(training_data_mod$NumberRealEstateLoansOrLines[training_data_mod$SeriousDlqin2yrs==1 & training_data_mod$NumberRealEstateLoansOrLines<5])
bins = c(0, 1, Inf)
training_data_mod["NumberRealEstateLoansOrLinesGrouped"] = cut(training_data_mod$NumberRealEstateLoansOrLines, bins , right=FALSE)


#Look at NumberOfDependents
summary(training_data_mod["NumberOfDependents"])
hist(training_data_mod$NumberOfDependents[training_data_mod$SeriousDlqin2yrs==0 & training_data_mod$NumberOfDependents<5])
hist(training_data_mod$NumberOfDependents[training_data_mod$SeriousDlqin2yrs==1 & training_data_mod$NumberOfDependents<5])
bins = c(-1, 0, 1, 4, Inf)
training_data_mod["NumberOfDependentsGrouped"] = cut(training_data_mod$NumberOfDependents, bins , right=FALSE)

#NumberOfTimes90DaysLate
hist(training_data_mod$NumberOfTimes90DaysLate[training_data_mod$SeriousDlqin2yrs==0 & training_data_mod$NumberOfTimes90DaysLat<10])
hist(training_data_mod$NumberOfTimes90DaysLate[training_data_mod$SeriousDlqin2yrs==1 & training_data_mod$NumberOfTimes90DaysLate<10])
bins = c(0,1,Inf)
training_data_mod["NumberOfTimes90DaysLateGruped"] = cut(training_data_mod$NumberOfTimes90DaysLate, bins , right=FALSE)





############### Helper function for cross validating Random Forest to obtain optimal number of trees ########

cv_rf <- function(data,k,ntree_range=c(10,200)){
  indices = sample(x=1:nrow(data), size=nrow(data), replace = TRUE)
  batch_size = nrow(data)/k
  CV_score = rep(0,length(ntree_range))
  for (i in 1:length(ntree_range)){
    ntree = ntree_range[i]
    print(ntree)
    predict_rate_vec = rep(0,k)
    for (j in 1:k){
      test_index = ((k-1)*batch_size+1):(k*batch_size)
      
      cv_train = data[-(test_index),]
      cv_test = data[test_index,]
      
      rf_model = randomForest(SeriousDlqin2yrsFactor ~ RevolvingUtilizationOfUnsecuredLinesGrouped + ageGrouped  +    DebtRatioGrouped  + MonthlyIncomeGrouped +            
                                                 NumberOfOpenCreditLinesAndLoansGrouped    +  NumberRealEstateLoansOrLinesGrouped + NumberOfDependentsGrouped +           
                                                 NumberOfTimes90DaysLateGruped, data =cv_train, na.action = na.roughfix, ntree=ntree)
      rf_pred = predict(rf_model, newdata=cv_test)
      class_table = table(rf_pred, cv_test$SeriousDlqin2yrs)
      print(class_table)
      predict_rate_vec[k] = sum(diag(class_table))/sum(class_table)
    }
    CV_score[i] = mean(predict_rate_vec)
  }
  return (cbind(ntree_range, CV_score))
} 


dummy <- function(df) {  
  
  NUM <- function(dataframe)dataframe[,sapply(dataframe,is.numeric)]
  FAC <- function(dataframe)dataframe[,sapply(dataframe,is.factor)]
  
  require(ade4)
  if (is.null(ncol(NUM(df)))) {
    DF <- data.frame(NUM(df), acm.disjonctif(FAC(df)))
    names(DF)[1] <- colnames(df)[which(sapply(df, is.numeric))]
  } else {
    DF <- data.frame(NUM(df), acm.disjonctif(FAC(df)))
  }
  return(DF)
} 

################## Fit Model ######################
#training_data_mod[,"SeriousDlqin2yrs"] = factor(training_data_mod[,"SeriousDlqin2yrs"], levels = c(0,1))

training_data_mod_dummy = dummy(training_data_mod)
logit_data = training_data_mod_dummy[,c(1,12:66)]
logit_model= glm(formula = SeriousDlqin2yrs ~ .,
               family = binomial,
               data = logit_daya)

summary(logit_model)



training_data_mod["SeriousDlqin2yrsFactor"] =  as.factor(training_data_mod$SeriousDlqin2yrs)
rf_CV = cv_rf(training_data_mod,10,ntree_range=5:20)
rf_model = randomForest(SeriousDlqin2yrsFactor ~ RevolvingUtilizationOfUnsecuredLinesGrouped + ageGrouped  +    DebtRatioGrouped  + MonthlyIncomeGrouped +            
                          NumberOfOpenCreditLinesAndLoansGrouped    +  NumberRealEstateLoansOrLinesGrouped + NumberOfDependentsGrouped +           
                          NumberOfTimes90DaysLateGruped , data =training_data_mod, na.action = na.roughfix, ntree=7)



###############################################


################## Proceeds with producing predicted values on Test set #########################
test_file = "./GiveMeSomeCredit/cs-test.csv"
test_data = read.csv(test_file,header=TRUE,stringsAsFactors=FALSE,row.names=1)

test_data_mod = test_data
#Checking missing data in the data set
for (col in names(test_data)){
  print(c(col,mean(is.na(test_data[col]))))
}
#The columns with missing values are MonthlyIncome (19.82%) and NumberOfDependents ("2.61%")
#Simply fill the missing values with NA

test_data_mod[is.na(test_data["NumberOfDependents"]), "NumberOfDependents"] = -1

bins = c(0, 0.2, 0.4, 0.6, 0.8, 0.9, 1, 1.2, Inf)
test_data_mod["RevolvingUtilizationOfUnsecuredLinesGrouped"] = cut(test_data_mod$RevolvingUtilizationOfUnsecuredLines, bins , right=FALSE)
bins = c(0, 25, 35, 55, 65, 80, Inf)
test_data_mod["ageGrouped"] = cut(test_data_mod$age, bins , right=FALSE)
bins = c(0 ,0.2, 0.4,  0.6, 0.8, 1, 1.2, 1.5, 2, 3, 5, 10, Inf)
test_data_mod["DebtRatioGrouped"] = cut(test_data_mod$DebtRatio, bins , right=FALSE)
bins = c(-1,0, 1, 1000,2000,3000,4000,5000,6000,8000, 10000, Inf)
test_data_mod["MonthlyIncomeGrouped"] = cut(test_data_mod$DebtRatio, bins , right=FALSE)
bins = c(0, 1, 3, 8, 10,20, Inf)
test_data_mod["NumberOfOpenCreditLinesAndLoansGrouped"] = cut(test_data_mod$NumberOfOpenCreditLinesAndLoans, bins , right=FALSE)
bins = c(0, 1, Inf)
test_data_mod["NumberRealEstateLoansOrLinesGrouped"] = cut(test_data_mod$NumberRealEstateLoansOrLines, bins , right=FALSE)
bins = c(-1, 0, 1, 4, Inf)
test_data_mod["NumberOfDependentsGrouped"] = cut(test_data_mod$NumberOfDependents, bins , right=FALSE)
bins = c(0,1,Inf)
test_data_mod["NumberOfTimes90DaysLateGruped"] = cut(test_data_mod$NumberOfTimes90DaysLate, bins , right=FALSE)


rf_pred = predict(rf_model, newdata=test_data_mod)
output_file = "./GiveMeSomeCredit/cs-output.csv"
write.csv(rf_pred, output_file)