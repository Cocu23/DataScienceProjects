# -*- coding: utf-8 -*-
"""
Created on Wed Jan 11 14:14:19 2017

#this is a working copy of the ml titanic project you will find on kaggle
"""
# manually cleaning workspace
#%reset
# clearing workspace with every run 
from IPython import get_ipython
get_ipython().magic('reset -sf')


import os
#os.chdir("C:\\Users\kht\Documents\Data_Analytics\Workspaces\data_samples")
print("lets get started")
print (os.getcwd() )# to print the path name in CLI


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
# I only use seaborn to avoid getting graphs with 90ish colors
import seaborn
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn import tree
from sklearn import svm
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import SGDClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import AdaBoostClassifier
from sklearn.model_selection import StratifiedKFold
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV
from sklearn.preprocessing import MinMaxScaler
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import make_scorer
from sklearn.metrics import fbeta_score
from sklearn.metrics import cohen_kappa_score
from sklearn.model_selection import train_test_split
import warnings
warnings.filterwarnings('ignore')

# QC:


# Read the data:
data =  pd.read_csv('titanic.csv', delimiter=";")
print("everything was imported")
# split randomly into train and test datasets
train, test = train_test_split(data, test_size = 0.2)

#print(train.head(10))

# Passenger's Gender
def sex(value):
    if value == "male":
        return 0
    else:
        return 1
        
# Function to create a graph that groups, counts and check survivors per group
def survival_rate(column,t):
    # define data frame as ouput table
    df=pd.DataFrame()
    # group by column (input) and total, survieved and percentage
    df['total']=train.groupby(column).size()
    df['survived'] = train.groupby(column).sum()['survived']
    df['percentage'] = round(df['survived']/df['total']*100,2)
    print(df)
    # plotting bar charts
    df['survived'].plot(kind=t)
    df['total'].plot(kind=t,alpha=0.5,title="Survivors per "+str(column))
    plt.show()
    
# Count the number of rows
print("*** Number of rows: " + str(train.shape[0]))
total = train.shape[0]
print("\n")

# List all the columns
print("*** Columns: " + str(train.columns.values), end="\n")
colnames = list(train.columns.values)
# Count the number of NaNs each column has
print("\n*** NaNs per column:")
print(pd.isnull(train).sum())






train["sex"] = train["sex"].apply(sex)

print(survival_rate("sex","barh"))

print (survival_rate("pclass","barh"))


"""
Passengers' Age
The following section checks the age distribution. First some data manipulation will be done:
The data set contain ages less than 1 for those people with only months of life. Those will be changed to 1.
Create new column with buckets of ages, in 10 years ranges.
Then, the usual graph representation showing the ages in groups and the survival rate.
"""


print("*** Number of people with age less than 1 (months):")
print(train[train["age"] < 1.0].shape[0])

# If age is less than 1, we return 1. Else, we return the original age.
def normalize_age_below_one(age):
    if age < 1:
        return 1
    else:
        return age
        
train['age'] = train['age'].apply(normalize_age_below_one)

# Group ages in buckets
def group_age(value):
    if value <= 10:
        return "0-10"
    elif value <= 20:
        return "10-20"
    elif value <= 30:
        return "20-30"
    elif value <= 40:
        return "30-40"
    elif value <= 50:
        return "40-50"
    elif value <= 60:
        return "50-60"
    elif value <= 70:
        return "60-70"
    elif value <= 80:
        return "70-80"
    elif value <= 90:
        return "80-90"
    else:
        return "No data"

train["AgeGroup"] = train["age"].apply(group_age)

survival_rate("AgeGroup","bar")



"""
Passengers' Fare
The following section checks the fare distribution and the average fare per class. 
The analysis shows that fare is mostly related with "Class", so no need to check the survival rate since survival per class has been already analysed.
"""

print("*** Fare statistics:")
print(train["fare"].describe())

# Seems that some people paid nothing:
print("\n*** People with fare 0:")
nothing = train[train["fare"] == 0]
print(nothing[["name","sex","age","pclass","survived"]])

train.groupby("pclass").mean()['fare'].plot(kind="bar",title="Average Fare per Class")
plt.show()




"""
Passengers' Port of Embarkation
The following section checks the port distribution and the survival percentage.
"""

def embarked(value):
    if value == "C":
        return 0
    elif value =="Q":
        return 1
    else:
        return 2

train["embarked"] = train["embarked"].apply(embarked)
survival_rate("embarked","bar")



"""
Family members
The following section checks the family distribution. First some data manipulation will be done:
New column, FamilyMembers will be added. It counts the number of family members of that particular passenger.
I will also check large families (more than 5 members) and see if there are families where no member survived at all.
Then, the usual graph representation showing the family members and the survival rate.
"""
train["FamilyMembers"]=train["sibsp"]+train["parch"]
print("*** Family statistics, members:")
print("Min: " + str(train["FamilyMembers"].min()))
print("Average: " + str(round(train["FamilyMembers"].mean(),2)))
print("Max: " + str(train["FamilyMembers"].max()), end="\n\n")

print("*** Average family members per Class:")
print(train.groupby("pclass").mean()['FamilyMembers'], end="\n\n")

# Families with more than 5 members
large_families=train[train["FamilyMembers"]>= 5]
large_families_by_ticket=large_families.groupby("ticket").sum()['survived']
print("*** Large families by ticket. Did all family die?:")
print(large_families_by_ticket==0, end="\n\n")

# Largest family where all members died
largest_family_ticket=train["ticket"][train["FamilyMembers"]==10].iloc[0]
name=train["name"][train["ticket"]==largest_family_ticket].iloc[0]
print("*** Largest family, all members died: "+ name.split(",")[0], end="\n\n")
# More info: http://www.bbc.com/news/uk-england-cambridgeshire-17596264

survival_rate("FamilyMembers","bar")




###############################################################################
"""


# passengers tickets

train["ticket"].head()

train["TicketClean"] = train["ticket"].str.extract('(\d{2,})', expand=True)
train["TicketClean"].head()

print("Rows with NaN: " + str(pd.isnull(train["TicketClean"]).nonzero()[0]))
print("Ticket number: ")
print(str(train["ticket"].ix[179]))
print(str(train["ticket"].ix[271]))
print(str(train["ticket"].ix[302]))
print(str(train["ticket"].ix[597]))
print(str(train["ticket"].ix[772]))
print(str(train["ticket"].ix[841]))


train["TicketClean"] = train["ticket"].str.extract('(\d{3,})', expand=True)
train["TicketClean"] = train["TicketClean"].apply(pd.to_numeric)
med1=train["TicketClean"].median()
med2=train["TicketClean"].median()+train["TicketClean"].std()
train.set_value(179, 'TicketClean', int(med1))
train.set_value(271, 'TicketClean', int(med1))
train.set_value(302, 'TicketClean', int(med1))
train.set_value(597, 'TicketClean', int(med1))
train.set_value(772, 'TicketClean', int(med2))
train.set_value(841, 'TicketClean', int(med2))
train["TicketClean"].head()




"""
Passenger's Name
Names also hide some valuable information. Like family name, and also tittle abbreviations like Mr., Miss. and so on. I am going to extract those titles and examine them to see if there is something useful we can do with it.
"""
train["TitleClean"] = train["name"].str.extract('(\w*\.)', expand=True)
train.groupby(train["TitleClean"]).size()




# Clean and convert to numeric.
train["TitleClean"] = train["name"].str.extract('(\w*\.)', expand=True)
def title_to_int(value):
    if value == "Capt.":
        return 0
    elif value == "Col.":
        return 1
    elif value == "Countess.":
        return 2
    elif value == "Don.":
        return 3
    elif value == "Dr.":
        return 4
    elif value == "Jonkheer.":
        return 5
    elif value == "Lady.":
        return 6
    elif value == "Major.":
        return 7
    elif value == "Master.":
        return 8
    elif value == "Miss.":
        return 9
    elif value == "Mlle.": #Same as miss
        return 9
    elif value == "Mme.":
        return 11
    elif value == "Mr.":
        return 12
    elif value == "Mrs.":
        return 13
    elif value == "Ms.":
        return 14
    elif value == "Rev.":
        return 15
    elif value == "Sir.":
        return 16
    elif value == "Dona.": # Same as Mrs
        return 13
    else:
        return np.nan
train["TitleClean"] = train["TitleClean"].apply(title_to_int)



###############################################################################
"""
Machine Learning
Data Balance
It is important to check if our data is balanced. That means that in this binary classification problem we should have the same number of rows for each possible outcome. If the data is imbalanced, our accuracy score could be wrong and the model could have problems to generalise.
The graph shows that our data is imbalanced, so the things we can do are:
Use a different score. Apart from Accuracy and F-Score, I will also check Cohen's Kappa.
Cohen’s kappa: Classification accuracy normalized by the imbalance of the classes in the data. http://machinelearningmastery.com/tactics-to-combat-imbalanced-classes-in-your-machine-learning-dataset/
Use a stratified cross validation method like StratifiedKFold or StratifiedShuffleSplit.
"""

df=pd.DataFrame()
df['total']=train.groupby("survived").size()
df=df['total']/train.shape[0]
df.plot(kind="bar",title="Label's Balance")
axes = plt.gca()
axes.set_ylim([0,1])
plt.show()
print("till here its fine")

"""
Data Preparation
First some data cleaning:
Drop useless columns.
Fill NaN ages and Fare with average from "Title" and "Pclass" groups.
Separate features and labels
"""

train.head(10) 



remove=['name','cabin','ticket', "PassengerId", 'AgeGroup']
for column in remove:
    train = train.drop(column, 1)

# Add missing ages. If there is a NaN, change it with the average for that title group.
list_nan=pd.isnull(train["age"]).nonzero()
# Get a pd with the mean age for each title
means = train.groupby("TitleClean").mean()['age']
# for each row with NaN, we write the average
for i in list_nan[0]:
    temp_title = train["TitleClean"].ix[i]
    train.set_value(i, 'age', int(means[temp_title]))
    

# Add missing fare. If there is a NaN, change it with the average for that Pclass.
list_nan=pd.isnull(train["fare"]).nonzero()
# Get a pd with the mean age for each title
means = train.groupby("pclass").mean()['fare']
# for each row with NaN, we write the average
for i in list_nan[0]:
    temp_class = train["pclass"].ix[i]
    train.set_value(i, 'fare', int(means[temp_class]))

# Separate features and labels

labels=train["survived"]
train = train.drop("survived", 1)
features=train

features.head()


# First try to check some models
def lets_try(NL):
    results={}
    def test_model(clf):
        
        cv = StratifiedKFold(n_splits=10)
        fbeta_scorer = make_scorer(fbeta_score, beta=1)
        cohen_scorer = make_scorer(cohen_kappa_score)
        accu = cross_val_score(clf, features, labels, cv=cv)
        fbeta = cross_val_score(clf, features, labels, cv=cv,scoring=fbeta_scorer)
        cohen = cross_val_score(clf, features, labels, cv=cv,scoring=cohen_scorer)
        scores=[accu.mean(),fbeta.mean(),cohen.mean()]
        return scores



    # Decision Tree
    clf = tree.DecisionTreeClassifier()
    results["Decision Tree"]=test_model(clf)
    # Logistic Regression
    clf = LogisticRegression()
    results["Logistic Regression"]=test_model(clf)
    # SVM Linear
    clf = svm.LinearSVC()
    results["Linear SVM"]=test_model(clf)
    # SVM RBF
    clf = svm.SVC()
    results["RBF SVM"]=test_model(clf)
    # Gaussian Bayes
    clf = GaussianNB()
    results["Gaussian Naive Bayes"]=test_model(clf)
    # Random Forest
    clf=RandomForestClassifier()
    results["Random Forest"]=test_model(clf)
    # AdaBoost with Decision Trees
    clf=AdaBoostClassifier()
    results["AdaBoost"]=test_model(clf)
    clf=SGDClassifier()
    results["SGDC"]=test_model(clf)
    if NL:
        clf=MLPClassifier()
        results["Neural Network"]=test_model(clf)
    
    results = pd.DataFrame.from_dict(results,orient='index')
    results.columns=["Accuracy","F-Score", "Cohen Kappa"] 
    results=results.sort(columns=["Accuracy","F-Score", "Cohen Kappa"],ascending=False)
    results.plot(kind="bar",title="Model Scores")
    axes = plt.gca()
    axes.set_ylim([0,1])
    return plt

lets_try(NL=True).show()



def draw_best_features():
    clf=RandomForestClassifier()
    clf.fit(features,labels)
    importances = clf.feature_importances_
    names=features.columns.values

    pd.Series(importances*100, index=names).plot(kind="bar")
    plt.show()
    
draw_best_features()


# Now let's test only with relevant features
best_features=["Pclass","Sex","Age","Fare","FamilyMembers", "TicketClean", "TitleClean"]
features=features[best_features]
features.head()

scaler = MinMaxScaler()
features = scaler.fit_transform(features)
pd.DataFrame(features).head()


lets_try(NL=True).show()


cv = StratifiedKFold(n_splits=10)

parameters = {'n_estimators': [10,20,30,40,50,60,70],
               'min_samples_split' :[2,3,4,5,6,7,8],
               'min_samples_leaf' : [1,2,3,4,5]
             }

clf = RandomForestClassifier()
grid_obj = GridSearchCV(clf, parameters, cv=cv)
grid_fit = grid_obj.fit(features, labels)
best_clf = grid_fit.best_estimator_ 

best_clf.fit(features,labels)




"""
