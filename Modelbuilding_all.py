import pandas as pd
import numpy as np
import matplotlib as plt
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.metrics import mean_squared_error
from math import sqrt


def preprocessing(train,test):
    #Converting categorical variables to factors
    train['Open.Date'] = train['Open.Date'].astype("category").cat.codes
    train['City'] = train['City'].astype("category").cat.codes
    train['City.Group'] = train['City.Group'].astype("category").cat.codes
    train['Type'] = train['Type'].astype("category").cat.codes

    test['Open.Date'] = test['Open.Date'].astype("category").cat.codes
    test['City'] = test['City'].astype("category").cat.codes
    test['City.Group'] = test['City.Group'].astype("category").cat.codes
    test['Type'] = test['Type'].astype("category").cat.codes

    #Choosing the best of features for model training and building
    X = train[['City.Group', 'P26', 'P6', 'P21', 'P23', 'P28']]
    new_test = test[['City.Group', 'P26', 'P6', 'P21', 'P23', 'P28']]
    new_test2 = test[['City.Group', 'P26', 'P6', 'P21', 'P23', 'P28']]
    return X,new_test, new_test2


def linearreg(X,train,new_test):
    lm = LinearRegression()
    #Code block to compute RMSE

    #lm.fit(X,train['revenue'])
    #pred = lm.predict(X)
    #train['Prediction'] = pred
    #rms = sqrt(mean_squared_error(train['revenue'],train['Prediction']))
    #print("rmse is", rms)

    #Code block to compute kaggle prediction
    lm.fit(X,train['revenue'])
    Prediction = lm.predict(new_test)
    Prediction = np.exp(Prediction)
    new_test['Prediction'] = Prediction
    test1 = new_test[['Prediction']]
    Testfilelinear = test1.to_csv("Test_kaggle_lin.csv")
    return


def knn(X,train,new_test):
    classifier = KNeighborsRegressor(n_neighbors=49, weights="distance")
    classifier.fit(X, train['revenue'])
    #Code block to compute RMSE
    
    #Prediction = classifier.predict(X)
    #train['Prediction'] = Prediction
    #rms = sqrt(mean_squared_error(train['revenue'],train['Prediction']))
    #print("rmse is", rms)

    #Code block to compute kaggle prediction
    Prediction = classifier.predict(new_test)
    Prediction = np.exp(Prediction)
    new_test['Prediction'] = Prediction
    test2 = new_test[['Prediction']]
    Testfileknn = test2.to_csv("Test_kaggle_knn.csv")
    return

train = pd.read_csv("train_python.csv")
test = pd.read_csv("test_python.csv")
X,new_test, new_test2 = preprocessing(train,test)
#new_test2 = pd.DataFrame(new_test)
#new_test2 = new_test
linearreg(X,train,new_test)
knn(X,train,new_test2)
