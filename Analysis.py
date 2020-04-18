import pandas as pd
import numpy as np
import matplotlib as plt
import math as math
#pip install gspread oauth2client

uscases = pd.read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
n=len(uscases.columns)

uslocs = uscases["Province_State"].unique()

m = len(uslocs)
sumc = []
for i in range(0,m):
    temp = uscases.values[uscases.Province_State == uslocs[i],:]
    sumc.append(sum(temp[:,n-1]))

sumroot = []  
for i in range(0,m):
    sumroot.append(math.sqrt(sumc[i]))
    
logroot = []
for i in range(0,m):
    logroot.append(math.log(sumc[i]+1))

plt.pyplot.hist(sumc)
plt.pyplot.title("Histogram of US cases by Total Number of Cases")

plt.pyplot.hist(sumroot)
plt.pyplot.title("Histogram of US cases by Square Root of Total Number of Cases")

plt.pyplot.hist(logroot)
plt.pyplot.title("Histogram of US cases by Log of Total Number of Cases")






m = len(uscases.columns)
final = uscases.iloc[:,m-1]
plt.pyplot.hist(final,100)
plt.pyplot.title("Histogram of Case Numbers by City/County")
cases=uscases.iloc[:,m-1]

usdeaths = pd.read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
n=len(usdeaths.columns)
population=usdeaths.iloc[:,11]
deaths=usdeaths.iloc[:,n-1]

percentcases = cases / population *100
percentdeaths = deaths / population *100
percentcasesdeaths = deaths / cases *100

plt.pyplot.hist(np.isfinite(percentcases),100)
#plt.pyplot.format(0,100)
plt.pyplot.title("Histogram of the percentage of people infected by city/county")