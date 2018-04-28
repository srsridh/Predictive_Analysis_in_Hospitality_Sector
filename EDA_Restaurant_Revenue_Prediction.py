from __future__ import division
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import re
import matplotlib.patches as mpatches

"""
from nltk import DecisionTreeClassifier
from nltk.corpus import stopwords
from sklearn.cross_validation import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import KNeighborsClassifier
from sklearn.tree import DecisionTreeClassifier
"""


train = pd.read_csv("train.csv")

#sns.stripplot(x = "City Group", y = "Type", data = train)
#train_plots = train.loc[train['City'].value.counts() >1]
#print(train_plots)
cities = train['City'].value_counts()
cities.columns = ['Name', 'count']
#print("cities are:", cities)

#print(type(cities))


cities_count = pd.DataFrame()
cities_count['count'] = cities
cities_count = cities_count[cities_count['count'] > 1]
#print("what", cities_count)
#print(type(cities_count))

color1 = sns.color_palette("husl", 10)
pd.Series(cities_count.plot(kind= "bar",colors = color1,
fontsize = 8 , figsize = (7,5), rot=0, title = "Number of restaurants opened per city"))
plt.xlabel("Cities")
plt.ylabel("Number of restaurants")
plt.show()



#Types of cities

city_types = train['City Group'].value_counts()
pd.Series(city_types.plot(kind= "bar",
fontsize = 8 , figsize = (7,5), rot=0, title = "Types of Cities"))
plt.xlabel("Types")
plt.ylabel("Count")
plt.show()

#Type of restaurants
#leg1 = mpatches.Patch(label='FC - Food Court', "IL - Inline", 'DT - Drive Thru')
#leg2 = mpatches.Patch(color='orange', label='IL - Inline')
#leg3 = mpatches.Patch(color='green', label='DT - Drive Thru')


restaurant_types = train['Type'].value_counts()
pd.Series(restaurant_types.plot(kind= "bar",
fontsize = 8 , figsize = (7,5), rot=0, title = "Types of Restaurants"))
plt.xlabel("Types")
plt.ylabel("Count")
#plt.legend("Food Court","ss","tt")
#plt.plot(label = "FC - Food Court")
#plt.plot(label = "IL - Inline")
#plt.plot(label = "DT - Drive Thru")

#plt.legend(handles=[leg1])
#plt.legend(handles=[leg2])
#plt.legend(handles=[leg3])
plt.show()


#d = {'cities' : "cities_count", }

