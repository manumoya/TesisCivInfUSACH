from __future__ import division
import pandas as pd
import numpy as np
from datetime import datetime
import dateutil
import statsmodels.api as sm
import re
from pandas import DataFrame
from numpy import nan
from pred_fns import *

def read_and_clean(filename, path="/Users/dan2/Desktop/kaggle/wind_forecast/raw_data/"):
	"""read file located at /path + filename.  Return DataFrame containing file contents 
		(with date converted from string to date object) """
	new_frame = pd.read_csv(path+filename)
	new_frame['date'] = map(lambda x: datetime.strptime(str(x), '%Y%m%d%H'), new_frame.date)
	new_frame = new_frame.set_index('date')
	return new_frame

def get_site_forecast(site, hors_to_keep = [1]):
	"""Read forecast file for site_num. Return DataFrame with forecast data from last three hours.
	   Indexed by time forecast is for.  Column names include forecast field + number of hours ahead 
	   the forecast occurs"""
	forecast_file = "windforecasts_wf" + str(site) + ".csv"
	forecast = read_and_clean(forecast_file)
	forecast = forecast[forecast.hors.apply(lambda x: x in hors_to_keep)]
	forecast = forecast.pivot(index=forecast.index, columns='hors')	 #unstack to 1 obs per outcome-time
	forecast.columns = [x[0] + "_" + str(x[1]) for x in forecast.columns]
	forecast['site'] = site
	forecast.set_index('site', drop=True, append=True, inplace=True)
	return forecast

def get_site_outcome(site, all_outcomes_df):
	"""Take site number and df of all outcome.  Return df with wind speeds at specified site"""
	site_dat = DataFrame(data={'outcome': all_outcomes_df["wp"+str(site)], 'site': site, 'id': all_outcomes_df.id}, index=all_outcomes_df.index)
	site_dat.set_index('site', drop=True, append=True, inplace=True)
	return site_dat

def stack_frames_from_list(df_list):
	"""Convert list of dataframes to a single data frame by appending"""
	return reduce(lambda x,y: x.append(y), df_list)	

def interpolate_missing(df):
	"""use linear interpolation to fill missing values in each column (except id)
	The order of observations is very important.
	################################
	###AREA FOR MAJOR IMPROVEMENT###
	################################"""
	backup_id = df.id
	output = df.apply(pd.Series.interpolate)
	output.id = backup_id
	return output

def make_basic_df():
	""""Create single data frame containing 1 observation for each site-time pair.
	The times included are the union of the times in the benchmark and training file.
	The columns include the observed wind speed where available, and variables from the
	forecast files."""
	site_list 	= range(1,8)
	train		= read_and_clean("train.csv")
	bench 		= read_and_clean("benchmark.csv")
	base_data 	= train.append(bench)
	all_outcomes  = stack_frames_from_list(get_site_outcome(x, base_data) for x in site_list)
	all_forecasts = stack_frames_from_list([get_site_forecast(x) for x in site_list])
	all_data	  = all_outcomes.merge(all_forecasts, left_index=True, right_index=True, how="left")
	output		  = interpolate_missing(all_data)
	return output

def split_for_prediction(basic_df):
	"""takes data frame as given in output of make_basic_df and returns a series containing
	training outcomes, a df containing the explanatory data in the training set, and a df
	containing training data for the test set"""
	output = basic_df.reset_index()		#move date and site to 
	output['month'] = map(lambda x: x.month, output.date)
	output['hour']  = map(lambda x: x.hour, output.date)
	output['sitenum']  = output.site
	training 	= basic_df.id.isnull()
	output.set_index(['date', 'sitenum'], drop=True, append=False, inplace=True)
	x_vars = output.columns.drop(['id', 'outcome'])
	train_x	= output.ix[training, x_vars] 
	train_y	= output.outcome[training]
	test_x	= output.ix[training==False, x_vars]
	index_vals = DataFrame(output.ix[training==False, 'id'])
	return (train_y, train_x, test_x, index_vals)


def write_preds(pred_frame, output_path="/Users/dan2/Desktop/kaggle/wind_forecast/output/"):
	extra_text_for_fname = raw_input("String to add to filename:  ")
	filename = str(datetime.now())[:10]+"_"+extra_text_for_fname+".csv"
	pred_frame.to_csv(output_path + filename, header=True)
	return

def preds_to_output(predictions, test_x, index_vals):
	unstacked_preds = DataFrame(data={'prediction': predictions, 'id': index_vals.id}, index=test_x.index).unstack()
	output = DataFrame(data = {	'id':	map(int, unstacked_preds.ix[:,1]),
								'date':	map(lambda x: x.strftime('%Y%m%d%H'), unstacked_preds.index),
								'wp1':  unstacked_preds.ix[:,7],
								'wp2':  unstacked_preds.ix[:,8],
								'wp3':  unstacked_preds.ix[:,9],
								'wp4':  unstacked_preds.ix[:,10],
								'wp5':  unstacked_preds.ix[:,11],
								'wp6':  unstacked_preds.ix[:,12],
								'wp7':  unstacked_preds.ix[:,13]})
	output.set_index('id', inplace=True)
	return output

def do_all():
	basic_data = make_basic_df()
	train_y, train_x, test_x, index_vals = split_for_prediction(basic_data)
	predictions	= pred_with_OLS(train_y, train_x, test_x)
	output_frame = preds_to_output(predictions, test_x, index_vals)
	write_preds(output_frame)