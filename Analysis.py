import pandas as pd
import numpy as np
import matplotlib as plt
#pip install gspread oauth2client

uscases = pd.read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
n=len(uscases.columns)
#cols = [5,6,int(str(list(range(11,n+1))))]
uslocs = uscases.iloc[:,5:7]
uscases = uscases.iloc[:,11:n]
#uscases = uscases.iloc
#sum = int(str([0] * 10))
#for i in range(1,n):
#    sum[i] = sum(uscases[:,1:n])

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