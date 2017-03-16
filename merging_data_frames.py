# Panda data analysis library

# there are two-core data structures which are very similar:
# the one-dimensional series object and the two-dimensional DataFrame object

#Querying these two data structures is done in a few different ways,
# such as using the iloc or loc attributes for row-based querying,
# or using the square brackets on the object itself for column-based querying.




import pandas as pd

df = pd.DataFrame([{'Name': 'Chris', 'Item Purchased': 'Sponge', 'Cost': 22.50},
                   {'Name': 'Kevyn', 'Item Purchased': 'Kitty Litter', 'Cost': 2.50},
                   {'Name': 'Filip', 'Item Purchased': 'Spoon', 'Cost': 5.00}],
                  index=['Store 1', 'Store 1', 'Store 2'])
print(df)
df['Date'] = ['December 1', 'January 1', 'mid-May']
print("now added new column to df")
print(df)


df['Delivered'] = True
df['Feedback'] = ['Positive', None, 'Negative']
print("also added")
print(df)


adf = df.reset_index()
adf['Date'] = pd.Series({0: 'December 1', 2: 'mid-May'})
print(adf)



##### JOIN

staff_df = pd.DataFrame([{'Name': 'Kelly', 'Role': 'Director of HR'},
                         {'Name': 'Sally', 'Role': 'Course liasion'},
                         {'Name': 'James', 'Role': 'Grader'}])
staff_df = staff_df.set_index('Name')
student_df = pd.DataFrame([{'Name': 'James', 'School': 'Business'},
                           {'Name': 'Mike', 'School': 'Law'},
                           {'Name': 'Sally', 'School': 'Engineering'}])
student_df = student_df.set_index('Name')
print(staff_df.head())
print("this was staff, next student")
print(student_df.head())


######### merge

# outer join (person is either student OR stuff- hence all elements are considered/ Union/ full join)
print ("outer /full join / union based on index and best knowledge")
print(pd.merge(staff_df, student_df, how='outer', left_index=True, right_index=True))

# inner join (person is student AND stuff - intersectoin
pd.merge(staff_df, student_df, how='inner', left_index=True, right_index=True)

# left join (students (staff- could also be a student)
pd.merge(staff_df, student_df, how='left', left_index=True, right_index=True)

#right join ( the other way round of left join )
pd.merge(staff_df, student_df, how='right', left_index=True, right_index=True)


################################################

# new example


staff_df = staff_df.reset_index()
student_df = student_df.reset_index()
pd.merge(staff_df, student_df, how='left', left_on='Name', right_on='Name')



staff_df = pd.DataFrame([{'Name': 'Kelly', 'Role': 'Director of HR', 'Location': 'State Street'},
                         {'Name': 'Sally', 'Role': 'Course liasion', 'Location': 'Washington Avenue'},
                         {'Name': 'James', 'Role': 'Grader', 'Location': 'Washington Avenue'}])
student_df = pd.DataFrame([{'Name': 'James', 'School': 'Business', 'Location': '1024 Billiard Avenue'},
                           {'Name': 'Mike', 'School': 'Law', 'Location': 'Fraternity House #22'},
                           {'Name': 'Sally', 'School': 'Engineering', 'Location': '512 Wilson Crescent'}])
print("new staff set")
print(staff_df)
print("new student set")
print(student_df)
print("left join of these two- based on Name (on - condition")
print(pd.merge(staff_df, student_df, how='left', left_on='Name', right_on='Name'))




staff_df = pd.DataFrame([{'First Name': 'Kelly', 'Last Name': 'Desjardins', 'Role': 'Director of HR'},
                         {'First Name': 'Sally', 'Last Name': 'Brooks', 'Role': 'Course liasion'},
                         {'First Name': 'James', 'Last Name': 'Wilde', 'Role': 'Grader'}])
student_df = pd.DataFrame([{'First Name': 'James', 'Last Name': 'Hammond', 'School': 'Business'},
                           {'First Name': 'Mike', 'Last Name': 'Smith', 'School': 'Law'},
                           {'First Name': 'Sally', 'Last Name': 'Brooks', 'School': 'Engineering'}])
print("new staff set 2")
print(staff_df)
print("new student set 2")
print(student_df)
print( "inner  join of these two based on First and Lst Name- on condtion ")
print(pd.merge(staff_df, student_df, how='inner', left_on=['First Name','Last Name'], right_on=['First Name','Last Name']))

