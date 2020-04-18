import pandas as pd
import numpy as np
import matplotlib as plt
import math as math

cases = pd.read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
deaths = pd.read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

n = len(cases.columns) - 11
uscases = []
for i in range(11,n+11):      
    sumus = sum(cases.iloc[:,i])
    uscases.append(sumus)
    
usday = []
for i in range(1,n):
    temp = uscases[i] - uscases[i-1]
    usday.append(usday)