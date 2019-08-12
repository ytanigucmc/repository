# repository

My machinelearning solution to the GiveMeSomeCredit problem from kaggle.


Running the code
The Credit RIsk.R file runs all the calculation. In order to run the file, the structure is such that
Directory -- Credit RIsk.R
          -- GiveMeSomeCredit folder
             -- cs-training.csv
             -- cs-test.csv
          
Note that necessary R packages need to be installed for successful run.          


Data Cleaning
Missing values (MonthlyIncome and NumberOfDependents) are replaced with -1.
In general, explanatory variables (continuious are integers) are replaced with categorical variables representing cetain range.


Model Fitting
Random Forest implemented in randomFOrest pakcages is used to fit to the data.
10-fold coress validation is used to determine the optimal number of trees, which was 7.


Model Accuracy
The model however is deemed to have a poor prediction accuracy. 
This is because the model classifies significatly lower number of positive label (SeriousDlqin2yrs = 1) than the actual count in the training data.
More specifically about 6.7% of training data has entries with SeriousDlqin2yrs = 1 while the fittedmodel
model lassifies SeriousDlqin2yrs = 1 for about 1% of training data.

