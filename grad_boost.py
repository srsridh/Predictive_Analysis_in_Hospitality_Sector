import pandas
import numpy
import lightgbm as lgb
from xgboost import XGBRegressor
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.metrics import mean_squared_error

def predict_lgb(train, test):
    train_lgb, test_lgb = train_test_split(train,test_size=0.2,random_state=42, shuffle=False)
    params = {}
    params['application'] = 'regression'
    params['boosting'] = 'gbdt'
    params['learning_rate'] = 0.01
    params['max_bin'] = 6
    params['num_leaves'] = 64
    params['max_depth'] = 20
    params['n_estimators'] = 280
    params['min_sum_hessian_in_leaf'] = 1e-2
    params['bagging_fraction'] = 0.9
    params['feature_fraction'] = 0.2
    params['bagging_freq'] = 3
    params['num_threads'] = 4
    params['metric'] = 'rmse'

    d_train = lgb.Dataset(train_lgb[features], train_lgb['revenue'])
    d_test = lgb.Dataset(test_lgb[features], test_lgb['revenue'])

    watchlist = [d_train, d_test]
    model = lgb.train(params, train_set=d_train, num_boost_round=50000, valid_sets=watchlist)

    new_predict_list = [model.predict(test[features])]

    expval = numpy.exp(new_predict_list)
    print("lgboost", expval)
    columns = ['Id', 'Prediction']
    df = pandas.DataFrame(columns=columns)
    df['Id'] =  test['Id']
    df['Prediction'] = expval[0]
    df.to_csv('lgbm_prediction.csv')
    return

def predict_xgb(train, test):
    model = XGBRegressor(booster = 'gbtree',learning_rate=0.2, n_estimators=280, subsample=0.8, 
                      colsample_bytree=0.8, max_depth =20, seed=3, gamma=0.00001,
                      reg_alpha = 0.0000001, max_delta_step=4)
    model.fit(train[features], train['revenue'])
    #predict_list = model.predict(test[features])
    #rms = numpy.sqrt(mean_squared_error(test['revenue'], predict_list))
    #print(rms)

    new_predict_list = [model.predict(test[features])]
    expval = numpy.exp(new_predict_list)
    print("gboost", expval)
    columns = ['Id', 'Prediction']
    df = pandas.DataFrame(columns=columns)
    df['Id'] =  test['Id']
    df['Prediction'] = expval[0][0]
    df.to_csv('xgb_prediction.csv')
    return

train = pandas.read_csv("TRAIN_python.csv")
test = pandas.read_csv("TEST_python.csv")

############################# FACTORIZATION #################################
train.Type = pandas.factorize(train.Type)[0]
train['City.Group'] = pandas.factorize(train['City.Group'])[0]
train['City'] = pandas.factorize(train['City'])[0]
train['Open.Date'] = pandas.factorize(train['Open.Date'])[0]

test.Type = pandas.factorize(test.Type)[0]
test['City.Group'] = pandas.factorize(test['City.Group'])[0]
test['City'] = pandas.factorize(test['City'])[0]
test['Open.Date'] = pandas.factorize(test['Open.Date'])[0]
############################# FACTORIZATION ENDS HERE #################################

features = ['City.Group', 'P6', 'P23', 'P28', 'P21', 'P26']

predict_lgb(train, test)
predict_xgb(train, test)
