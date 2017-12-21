# Titanic-Project   
   
The data is given as two separate files, training and testing data.    
## Getting values from Names
Get the titles from the names of the passengers. Store them in a separate variable 'Title'.   
   
## Missing Values   
Deletion of rows is not an option since there are already few data elements. Some values are filled by analysis of statistics like medians. Other are filled using Linear regression based imputation.   
  
## Scaling of Attributes   
Scale the numeric attributes to be between 0 and 1 so that no particular attribute dominates in the prediction model.   
   
## Prediction   
Fit the training data into a model using Support Vector Machines(SVM). Test the fiited model on the testing data and check the accuracy of the model.   
    
## K-fold Validation   
The model is checked for its accuracy by a 10-fold validation wherein the data set is divided into training and testing data in a 9:1 ratio respectively 10 times. Every time a separate set is taken for testing. The accuracies for all the tests are calcualted and the mean is taken to get the accuracy of the model. It is found to be 86.83%   
