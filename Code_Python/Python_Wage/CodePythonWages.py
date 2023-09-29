'''###########################################################################
This python script is to produce textbook figures in Undergraduate Econometrics 
textbook of Professor Diebold. 

Author: Theodore Caputi, Donato Onorato, JoonYup Park, Sarah Winton
Date: 5/20/2017
Project: Undergraduate Econometrics Textbook (Wages)
Contact: joonyup@wharton.upenn.edu

Updates: 
    Finalized 8/15/2017

Instructions:
    
    1. Before running the script, please change your working directory in the prelim.
    
    2. In the "House Keeping" section, the required packages are listed. Please run those 
        lines before running the script. 
    
    3. This python scipt is written under Python version 3.6.0. 
    
Notes:

###########################################################################'''

# House Keeping ---------------------------------------------------------------

# %reset #Run this command before the run if you wish to clear directory

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import statsmodels.formula.api as sm
import statsmodels.api as stm
import scipy.stats as stats
import matplotlib.mlab as mlab #for drawing kernel density


# Declare Directory -----------------------------------------------------------

##Directory 
directory = '' # <- input your working directory here

##Input files 
wagefile = directory + '/DataWage.txt'

##Output files


# Load Data -------------------------------------------------------------------

data = pd.read_csv(wagefile)


# Main Working Space ---------------------------------------------------------

##Distributions of Wages 
hist_wage = plt.hist(data['WAGE'])
plt.show()

##Distribution of Log Wages
hist_lnwage = plt.hist(data['LNWAGE'])
plt.show()

##Kernel density plot for Wages 
mu = np.mean(data['WAGE'])
sigma = np.sqrt(np.var(data['WAGE']))
x = np.linspace(-10,70,10001)

kden_wage = stats.gaussian_kde(data['WAGE']).evaluate(x)

plt.plot(x, mlab.normpdf(x,mu,sigma), label="K-Density of Wages")
plt.plot(x, kden_wage, label="Normal Distribution")
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.show()

##Kernel density plot for Log Wages
mu = np.mean(data['LNWAGE'])
sigma = np.sqrt(np.var(data['LNWAGE']))
x = np.linspace(-1,5,101)

kden_lnwage = stats.gaussian_kde(data['LNWAGE']).evaluate(x)

plt.plot(x, mlab.normpdf(x,mu,sigma), label="K-Density of Log Wages")
plt.plot(x, kden_lnwage, label="Normal Distribution")
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.show()


##QQ-plot for Wages and Log Wages
stats.probplot(data['WAGE'], dist="norm", plot=plt)
plt.show()

stats.probplot(data['LNWAGE'], dist="norm", plot=plt)
plt.show()

##Histograms for Log Wages, Education, and Experience
hist_lnwage = plt.hist(data['LNWAGE'])
plt.show()

hist_educ = plt.hist(data['EDUC'])
plt.show()

hist_exper = plt.hist(data['EXPER'])
plt.show()


##Scatter plot for Log Wage on Education
plt.scatter(data['EDUC'], data['LNWAGE'])
plt.xlabel('EDUC')
plt.ylabel('LNWAGE')
plt.show()


##Fitted line through scatter plot for Log Wage on Education
m,b = np.polyfit(data['EDUC'],data['LNWAGE'],deg=1)
plt.scatter(data['EDUC'], data['LNWAGE'], s=10)
plt.plot(data['EDUC'], m*data['EDUC'] + b, color='r')
plt.show()


##Regression1: LNWAGE ~ EDUC + EXPER
X = data[['EDUC','EXPER']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg1 = sm.OLS(y,X).fit()
reg1.summary()


##Plot fitted values from Regression1
reg1_fit = reg1.fittedvalues
plt.scatter(reg1_fit, data['LNWAGE'], facecolors='none', edgecolors='b')
plt.xlim(0,5)
plt.ylim(0,5)
plt.show()

##Plot of residuals and Fitted values superimposed on #residuals
reg1_actual = data['LNWAGE']
reg1_resid = reg1.resid

plt.plot(reg1_actual, color="red", linewidth=0.5, label="Actual")
plt.plot(reg1_fit, color="green", linewidth=0.5, label="Fitted")
plt.plot(reg1_resid, color="blue", linewidth=0.5, label="Residual")
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.show()


##Histograms for EDUC, EXPER, NONWHITE, and UNION
hist_wage = plt.hist(data['EDUC'])
plt.show()

hist_wage = plt.hist(data['EXPER'])
plt.show()

hist_wage = plt.hist(data['NONWHITE'])
plt.show()

hist_wage = plt.hist(data['UNION'])
plt.show()

##Regression2: LNWAGE ~ FEMALE, NONWHITE, UNION, EDUC, EXPER
X = data[['FEMALE','NONWHITE','UNION','EDUC','EXPER']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg2 = sm.OLS(y,X).fit()
reg2.summary()

##Plot fitted values from Regression2
reg2_fit = reg2.fittedvalues
plt.scatter(reg2_fit, data['LNWAGE'], facecolors='none', edgecolors='b')
plt.xlim(0,5)
plt.ylim(0,5)
plt.show()


##Time and Trend
time = np.arange(0,101,0.5)
trend = 10 - 0.25 * time
BB = -50 + 0.8 * time
plt.plot(time,trend,color='r')
plt.plot(time,BB,color='b')
plt.show()


##Regression3: LNWAGE ~ EDUC, EDUC^2, EXP, EXP^2, EDUCxEXPER, FEMALE, UNION, NONWHITE
data['EDUC2'] = data['EDUC'] ** 2
data['EXPER2'] = data['EXPER'] ** 2
data['EDUC_X_EXPER'] = data['EDUC'] * data['EXPER']
X = data[['EDUC','EDUC2','EXPER','EXPER2','EDUC_X_EXPER','FEMALE','UNION','NONWHITE']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg3 = sm.OLS(y,X).fit()
reg3.summary()

##Regression3 w/ dummy interactions: LNWAGE ~ EDUC, EDUC^2, EXP, EXP^2, EDUCxEXPER, FEMALE, UNION, NONWHITE, FEMALExUNION, FEMALExNONWHITE, UNIONxNONWHITE 
data['FEMALE_X_UNION'] = data['FEMALE'] * data['UNION']
data['FEMALE_X_NONWHITE'] = data['FEMALE'] * data['NONWHITE']
data['UNION_X_NONWHITE'] = data['UNION'] * data['NONWHITE']
X = data[['EDUC','EXPER','FEMALE','UNION','NONWHITE','FEMALE_X_UNION','FEMALE_X_NONWHITE','UNION_X_NONWHITE']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg3_wDummyInt = sm.OLS(y,X).fit()
reg3_wDummyInt.summary()

##Regression4: combining several of the previous regressions
X = data[['EDUC','EDUC2','EXPER','EXPER2','EDUC_X_EXPER','FEMALE','UNION','NONWHITE','FEMALE_X_UNION','FEMALE_X_NONWHITE','UNION_X_NONWHITE']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg4 = sm.OLS(y,X).fit()
reg4.summary()

##Plots of different time trends
fig_a = 10 + 0.3*time + 0.3*(time**2)
plt.plot(time,fig_a)
plt.show()

fig_b = 10 + 30*time - 0.3*(time**2)
plt.plot(time,fig_b)
plt.show()

fig_c = 10 - 0.4*time - 0.4*(time**2)
plt.plot(time,fig_c)
plt.show()

fig_d = 10 - 0.25*time + 0.3*(time**2)
plt.plot(time,fig_d)
plt.show()

##Regression5: LNWAGE~EDUC, EXPER, EXPER^2, EDUCxEXPER, FEMALE, UNION, NONWHITE
X = data[['EDUC','EXPER','EXPER2','EDUC_X_EXPER','FEMALE','UNION','NONWHITE']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg5 = sm.OLS(y,X).fit()
reg5.summary()


##Plot residual^2 from Regression5
reg5_resid = reg5.resid
reg5_resid2 = reg5_resid**2
plt.scatter(data['EDUC'],reg5_resid2, facecolors='none', edgecolors='b')
plt.xlabel('EDUC')
plt.ylabel('RESID2')
plt.show()


##OLS Regression: LNWAGE ~ EDUC
X = data[['EDUC']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg_OLS = sm.OLS(y,X).fit()
reg_OLS.summary()

reg_OLS_fit = reg_OLS.fittedvalues
plt.scatter(data['EDUC'], data['LNWAGE'])
plt.plot(data['EDUC'], reg_OLS_fit, color='green')
plt.show()


##LAD Regression: LNWAGE ~ EDUC
reg_LAD = sm.quantreg('LNWAGE ~ EDUC', data=data).fit(q=0.5)
reg_LAD.summary()

reg_LAD_fit = reg_LAD.fittedvalues
plt.scatter(data['EDUC'], data['LNWAGE'])
plt.plot(data['EDUC'], reg_LAD_fit, color='red')
plt.show()

##10-percentile Regression: LNWAGE ~ EDUC
reg_10pct = sm.quantreg('LNWAGE ~ EDUC', data=data).fit(q=0.1)
reg_10pct.summary()

reg_10pct_fit = reg_10pct.fittedvalues
plt.scatter(data['EDUC'], data['LNWAGE'])
plt.plot(data['EDUC'], reg_10pct_fit, color='orange')
plt.show()

##90-percentile Regression: LNWAGE ~ EDUC
reg_90pct = sm.quantreg('LNWAGE ~ EDUC', data=data).fit(q=0.9)
reg_90pct.summary()

reg_90pct_fit = reg_90pct.fittedvalues
plt.scatter(data['EDUC'], data['LNWAGE'])
plt.plot(data['EDUC'], reg_90pct_fit, color='purple')
plt.show()

##Comparison of regressions with different loss functions
plt.scatter(data['EDUC'], data['LNWAGE'], label="Log(Wage)")
plt.plot(data['EDUC'], reg_OLS_fit, color='green', label="OLS")
plt.plot(data['EDUC'], reg_LAD_fit, color='red', label="LAD")
plt.plot(data['EDUC'], reg_10pct_fit, color='orange', label="10th Percentile")
plt.plot(data['EDUC'], reg_90pct_fit, color='purple', label="90th Percentile")
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.show()

##Leave-one-out Plot of Coefficients 
#Get original regression coefficient of EDUC
X = data[['EDUC']]
X = stm.add_constant(X)
y = data['LNWAGE']
reg_LOO = sm.OLS(y,X).fit()
b_orig = reg_LOO.params['EDUC']

n = len(data)

#Initiate a vector to store LOO coefficients
b_LOO_coef_change = np.zeros(shape=(n,1))

#Loop over all observations, excluding one by one
for i in range(0,n-1):
    
    short_data = data.drop(data.index[[i]])    
    
    X = short_data[['EDUC']]
    X = stm.add_constant(X)
    y = short_data['LNWAGE']   
    
    reg_short_LOO = sm.OLS(y,X).fit()
    b_LOO_coef_change[i] = reg_short_LOO.params['EDUC'] - b_orig

    
plt.scatter(range(0,1323), b_LOO_coef_change)   
    
##leverage 
stm.graphics.influence_plot(reg_LOO, size=0)


