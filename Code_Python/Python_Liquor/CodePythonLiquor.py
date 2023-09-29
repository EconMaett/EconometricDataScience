'''###########################################################################
This python script is to produce textbook figures in Undergraduate Econometrics 
textbook of Professor Diebold. 

Author: Theodore Caputi, Donato Onorato, JoonYup Park, Sarah Winton
Date: 6/18/2017
Project: Undergraduate Econometrics Textbook (Liquor)
Contact: joonyup@wharton.upenn.edu

Updates: 
    Finalized 8/15/2017

Instructions:
    
    1. Before running the script, please change your working directory in the prelim.
    
    2. In the "House Keeping" section, the required packages are listed. Please run those 
        lines before running the script. 
    
    3. This python scipt is written under Python version 3.6.0. 
    
Notes:
    -Chow Test and Breusch-Godfrey Test missing 

###########################################################################'''

# House Keeping ---------------------------------------------------------------

#%reset #Run this command before the run if you wish to clear directory

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.formula.api as sm
import statsmodels.api as stm
import statsmodels.tsa as tsa #for correlograms


# Declare Directory -----------------------------------------------------------

##Directory 
directory = 'C:/Users/USER/Documents/jyp/extra_curricular/research/Diebold_RA/Python_Liquor' # <- input your working directory here

##Input files 
liquorfile = directory + '/DataLiquor.csv'

##Output files
#None

# Load Data -------------------------------------------------------------------

data = pd.read_csv(liquorfile)
liquor_sales_data = data.as_matrix().ravel()

##Attach time information to the data
months = pd.date_range('1987-01', '2015-01', freq='M')
ls = pd.Series(liquor_sales_data, index=months)


##Various Linear Trends
time = np.arange(0,101,0.5)
trend1 = 10 - 0.25 * time
trend2 = -50 + 0.8 * time
plt.plot(time,trend1,color='r',label="Trend = 10 - 0.25*Time")
plt.plot(time,trend2,color='b',label="Trend = -50 + 0.8*Time")
plt.legend()
plt.show()


##Liquor Sales
ls.plot()
plt.xlabel("Time") 
plt.ylabel("Liquor Sales")
plt.show()

##Log Liquor Sales
ln_ls = np.log(ls)

ln_ls.plot()
plt.xlabel("Time")
plt.ylabel("Log Liquor Sales")
plt.show()


##Linear Trend Estimation
n = len(ln_ls)
time = np.arange(1,n+1,1)
X = time
X = stm.add_constant(X)
y = ln_ls
reg_lin_trend = sm.OLS(y,X).fit()
reg_lin_trend.summary()


##Residual Plot and Linear Trend Estimation
ln_ls_act = ln_ls
ln_ls_pred = reg_lin_trend.fittedvalues
ln_ls_resid = ln_ls_act - ln_ls_pred
ln_ls_resid_sd = np.std(ln_ls_resid)

plt.figure()
f, axes = plt.subplots(2,1)
axes[0].plot(ln_ls_act, label="Actual", color="red")
axes[0].plot(ln_ls_pred, label="Fitted", color="green")
axes[0].legend(loc="upper left")
axes[1].plot(ln_ls_resid, label="Residual", color="blue")
axes[1].axhline(y=0, color="black", linestyle='-')
axes[1].axhline(y=ln_ls_resid_sd, color="black", linestyle=':')
axes[1].axhline(y=-ln_ls_resid_sd, color="black", linestyle=':')
axes[1].legend(loc="upper left")
plt.show()


##Linear Trend Estimation with Seasonal Dummies
#Add monthly dummies to the data
list_months = months.month
seas_dummies = pd.get_dummies(list_months, prefix="month")

time = pd.DataFrame(time)
X = pd.concat([time,seas_dummies], axis=1)
X = X.rename(columns={0:'time'})
y = np.log(liquor_sales_data)
reg_lin_trend_seas = sm.OLS(y,X).fit()
reg_lin_trend_seas.summary()


ls = pd.Series(liquor_sales_data, index=months)

ln_ls_seas_act = ln_ls
ln_ls_seas_pred = reg_lin_trend_seas.fittedvalues
ln_ls_seas_pred = ln_ls_seas_pred.as_matrix().ravel()
ln_ls_seas_pred = pd.Series(ln_ls_seas_pred, index=months)
ln_ls_seas_resid = ln_ls_seas_act - ln_ls_seas_pred
ln_ls_seas_resid_sd = np.std(ln_ls_seas_resid)

plt.figure()
f, axes = plt.subplots(2,1)
axes[0].plot(ln_ls_seas_act, label="Actual", color="red")
axes[0].plot(ln_ls_seas_pred, label="Fitted", color="green")
axes[0].legend(loc="upper left")
axes[1].plot(ln_ls_seas_resid, label="Residual", color="blue")
axes[1].axhline(y=0, color="black", linestyle='-')
axes[1].axhline(y=ln_ls_seas_resid_sd, color="black", linestyle=':')
axes[1].axhline(y=-ln_ls_seas_resid_sd, color="black", linestyle=':')
axes[1].legend(loc="upper left")
plt.show()


##Seasonal Pattern
seas_coef = reg_lin_trend_seas.params
seas_coef = seas_coef.drop(['time'])

n = len(seas_coef)
month_list = np.arange(1,n+1,1)

plt.plot(month_list, seas_coef)
plt.xlabel("Month")
plt.ylabel("Factor")
plt.title("Estimated Seasonal Factors")
plt.show()


##Log-Quadratic Trend Estimation
time = time.rename(columns={0:'time'})
time_sq = time**2
time_sq = time_sq.rename(columns={'time':'time2'})
X = pd.concat((time,time_sq),axis=1)
X = stm.add_constant(X)
y = np.log(liquor_sales_data)
reg_quad_trend = sm.OLS(y,X).fit()
reg_quad_trend.summary()

##Residual Plot, Log-Quadratic Trend Estimation
ln_ls_quad_act = ln_ls
ln_ls_quad_pred = reg_quad_trend.fittedvalues
ln_ls_quad_pred = ln_ls_quad_pred.as_matrix().ravel()
ln_ls_quad_pred = pd.Series(ln_ls_quad_pred, index=months)
ln_ls_quad_resid = ln_ls_quad_act - ln_ls_quad_pred
ln_ls_quad_resid_sd = np.std(ln_ls_quad_resid)

plt.figure()
f, axes = plt.subplots(2,1)
axes[0].plot(ln_ls_quad_act, label="Actual", color="red")
axes[0].plot(ln_ls_quad_pred, label="Fitted", color="green")
axes[0].legend(loc="upper left")
axes[1].plot(ln_ls_quad_resid, label="Residual", color="blue")
axes[1].axhline(y=0, color="black", linestyle='-')
axes[1].axhline(y=ln_ls_quad_resid_sd, color="black", linestyle=':')
axes[1].axhline(y=-ln_ls_quad_resid_sd, color="black", linestyle=':')
axes[1].legend(loc="upper left")
plt.show()


##Log-Quadratic Trend Estimation with Seasonal Dummies
X = pd.concat([time,time_sq,seas_dummies], axis=1)
y = np.log(liquor_sales_data)
reg_quad_seas = sm.OLS(y,X).fit()
reg_quad_seas.summary()


##Residual Plot, Log-Quadratic Trend Estimation with Seasonal Dummies
ln_ls_quad_seas_act = ln_ls
ln_ls_quad_seas_pred = reg_quad_seas.fittedvalues
ln_ls_quad_seas_pred = ln_ls_quad_seas_pred.as_matrix().ravel()
ln_ls_quad_seas_pred= pd.Series(ln_ls_quad_seas_pred, index=months)
ln_ls_quad_seas_resid = ln_ls_quad_seas_act - ln_ls_quad_seas_pred
ln_ls_quad_seas_resid_sd = np.std(ln_ls_quad_seas_resid)

plt.figure()
f, axes = plt.subplots(2,1)
axes[0].plot(ln_ls_quad_seas_act, label="Actual", color="red")
axes[0].plot(ln_ls_quad_seas_pred, label="Fitted", color="green")
axes[0].legend(loc="upper left")
axes[1].plot(ln_ls_quad_seas_resid, label="Residual", color="blue")
axes[1].axhline(y=0, color="black", linestyle='-')
axes[1].axhline(y=ln_ls_quad_seas_resid_sd, color="black", linestyle=':')
axes[1].axhline(y=-ln_ls_quad_seas_resid_sd, color="black", linestyle=':')
axes[1].legend(loc="upper left")
plt.show()


##Expanding Window Regression

#Create a full dataframe that will be used in the expanding window regression
X_full = pd.concat([time,seas_dummies], axis=1)
y_full = np.log(liquor_sales_data)

#Define parameters for looping
n = len(y_full)-1
k = 120 #We will start from Jan. 1997 (120th row of the data refers to Jan. 1997)

#Initialize empty vector to store coefficients as we expand our window
coef_expand = np.zeros(shape=(n-k,1))

for i in range(k,n):
    
    X = X_full[0:i]
    y = y_full[0:i]
    
    reg_expand = sm.OLS(y,X).fit()
    
    coef_expand_index = i-k
    coef_expand[coef_expand_index] = reg_expand.params['time']
    
plt.plot(coef_expand)
plt.title('Expanding Window Regression')
plt.xlabel('Time Index')
plt.ylabel('Coefficient of Trend')
plt.show()


##Rolling Window Regression

#Create a full dataframe that will be used in the expanding window regression
X_full = pd.concat([time,seas_dummies], axis=1)
y_full = np.log(liquor_sales_data)

#Define parameters for looping
n = len(y_full)-1
w = 120 #We fix the window length to be 10 years

#Initialize empty vector to store coefficients as we roll through our window
coef_rolling = np.zeros(shape=(n-w+2,1))

for i in range(0,n-w+2):
    
    #Define cut-off
    w_start = i
    w_end = i+w-1
    
    X = X_full[w_start:w_end]
    y = y_full[w_start:w_end]
    
    reg_rolling = sm.OLS(y,X).fit()
    
    coef_rolling[i] = reg_rolling.params['time']

plt.plot(coef_rolling)
plt.title('Rolling Window Regression')
plt.xlabel('Window Cut-off Start Time Index')
plt.ylabel('Coefficient of Time')
plt.text(0,0,'Window Length is' + str(w) + 'months')
plt.show()


##Scatter Plot of e_t against e_t-1

#Let's review regression result of log(liquor sales) ~ trend + trend^2 + seasons
reg_quad_seas.summary()

#Create a lag of our residuals from the above regression
ln_ls_quad_seas_resid_lag = ln_ls_quad_seas_resid.shift(1)

plt.scatter(ln_ls_quad_seas_resid_lag, ln_ls_quad_seas_resid)
plt.xlabel('RESID L1')
plt.ylabel('RESID')
plt.show()

##Durbin-Watson Test for AR(1) Distrubance

stm.stats.stattools.durbin_watson(ln_ls_quad_seas_resid)
#Note that the result of DW test is also listed in the OLS window


##Regress on trend + trend^2 + season + p-lags
P = 4 #<- change this value to show different results for different p

X_full = pd.concat([time,time_sq,seas_dummies], axis=1)
y_full = np.log(liquor_sales_data)
y_full = pd.DataFrame(y_full)
y_full = y_full.rename(columns={0:'ln_liquorsales'})


for i in range(1,P+1):
    
    varname = 'ln_liquorsales_L' + str(i)
    
    y_p_lag = y_full.shift(i)
    y_p_lag = y_p_lag.rename(columns={'ln_liquorsales':varname})
    X_full = pd.concat([X_full,y_p_lag], axis=1)

X = X_full[P:]
y = y_full[P:]
reg_quad_seas_lagP = sm.OLS(y,X).fit()
reg_quad_seas_lagP.summary()


##Check AIC(=Akaiake's Information Criterion) and BIC(=Schwarz's Bayesian Criterion)
#AIC and BIC values are listed in the regression output
reg_quad_seas_lagP.summary()


##Regression with Serially-Correlated Disturbances (ACF & PACF)
acf = tsa.stattools.acf(ln_ls_quad_seas_resid)
pacf = tsa.stattools.pacf(ln_ls_quad_seas_resid)
