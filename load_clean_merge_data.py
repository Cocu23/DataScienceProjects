def answer_one():
    
    import pandas as pd
    import numpy as np
    
    ### ENERGY
    energy = pd.read_excel("Energy Indicators.xls")
    energy.columns=['ColA','ColB','Country', 'Energy Supply', 'Energy Supply per Capita', '% Renewable']
    energy = energy.ix[16:,['Country', 'Energy Supply', 'Energy Supply per Capita', '% Renewable']]
    
    #conert missing data to NaN and drop NA rows
    energy = energy.dropna()
    energy = energy.apply(lambda x: x.replace('...', np.nan))
    
    #convert energy supply to gigajoules
    energy['Energy Supply']= energy['Energy Supply']*1000000
    
    # remove digits from countries
    energy["Country"] = energy["Country"].replace('[0-9]', "", regex=True)
    # if i want to get rid of all non-numeric values i have to include ^ to the number statement, such as:
    # energy["Country"] = energy["Country"].replace('[^0-9]', "", regex=True)
    
    #remove brackets (apply to country, split string by "(" and take the first string part )
    energy['Country'] = energy['Country'].apply(lambda x: x.split("(")[0])

    # rename countries
    energy["Country"] = energy["Country"].replace("Republic of Korea", "South Korea")
    energy["Country"] = energy["Country"].replace("China, Hong Kong Special Administrative Region", "Hong Kong")
    energy["Country"] = energy["Country"].replace("United States of America", "United States")
    energy["Country"] = energy["Country"].replace("United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
    
    

    ### GDP
    GDP = pd.read_csv("world_bank.csv", delimiter=",",header=4)
    # rename countries
    GDP["Country Name"] = GDP["Country Name"].replace("Korea, Rep", "South Korea")                  
    GDP["Country Name"] = GDP["Country Name"].replace("Iran, Islamic Rep.", "Iran")     
    GDP["Country Name"] = GDP["Country Name"].replace("Hong Kong SAR, China", "Hong Kong") 

    ### ScimEn
    ScimEn = pd.read_excel("scimagojr-3.xlsx")    
    #test: get only top 15
    #ScimEn = ScimEn[:15]  #works fine
    
    ### MERGE (intersection by country)
    GDP.rename(columns={'Country Name':'Country'}, inplace=True)
    #GDP = GDP[["Country", '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015']]
    df = pd.merge(energy, GDP[["Country", '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015']],  on="Country")
    df2= pd.merge(df, ScimEn[:15], how= "right",  left_on="Country", right_on="Country")
    
    #df2 = pd.merge(pd.merge(GDP,energy,on='Country'),ScimEn[:15],on='Country')
    
    return df2.set_index("Country")
#df2.set_index("Country")
answer_one()