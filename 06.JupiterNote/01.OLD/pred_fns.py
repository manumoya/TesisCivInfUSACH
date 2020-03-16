import statsmodels.api as sm
from sklearn.ensemble import RandomForestClassifier, GradientBoostingRegressor


def pred_with_OLS(train_y, train_x, test_x):
	my_ols 		= sm.OLS(train_y, train_x).fit()
	predictions = my_ols.predict(test_x)
	return predictions

def pred_with_gbm(train_y, train_x, test_x):
	my_gbm = GradientBoostingRegressor(loss="ls", learn_rate=.05, 
				n_estimators=250, max_depth=3, min_samples_leaf=25).fit(train_x, train_y)
	predictions = my_gbm.predict(test_x)
	return predictions

def pred_with_rf(train_y, train_x, test_x):
	rf 	= RandomForestClassifier(n_estimators=300, max_depth=6, min_samples_leaf=30).fit(train_x, train_y)
	predictions = rf.predict_proba(test_x)
	return(predictions)
