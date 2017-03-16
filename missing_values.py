## missing values

import pandas as pd

df = pd.read_csv('mpg.csv')
print(df.head())


print(list(df.columns.values))
#['Unnamed: 0', 'speed', 'dist']


#rename cols
df.columns=["Car","Speed", "Dist"]
print(df.head())


df = df.set_index('Car')
df = df.sort_index()
print(df.head())

df = df.reset_index()
df = df.set_index(['Car', 'Speed'])
print(df.head())

df["New_Col"]=pd.Series(None, index=df.index)
print("this is with new NA column")
print(df.head())
df["New_Col"][0]=1
df["New_Col"][3]=5
print("manipulate first col entry for new_col")
print(df.head())
df = df.fillna(method='ffill')
print("now fill NA values appropriately, here with latest value")
print(df.head())


