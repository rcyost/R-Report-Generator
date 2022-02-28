

#%%
import imf_datatools

import pandas as pd

import numpy as np



#%%

# get all series
ser = (
        imf_datatools.worldbank_utilities.get_all_worldbank_metadata()
        .replace(' ', '-', regex=True)
)

# look at databases availiable
ser.source.unique()


#%%

# Worldwide-Bureaucracy-Indicators
# Doing-Business

# filter datasets
ofinterest = ser[ser.source.str.contains('Doing-Business')]
ofinterest = ofinterest[ofinterest.name.str.contains('tax')]

# search indicators for containing a string
#ofinterest = ser[ser.name.str.contains('tax')]
# with a second string
#ofinterest = ofinterest[ofinterest.name.str.contains('male')]


#%%
db_string = 'Doing-Business'
str_filter = 'tax'

db = ser[ser.source == db_string]
db = db[db.name.str.contains(str_filter)].reset_index().rename(columns = {'index': "code"})


#%%
# get country codes to query with
countries = (
            imf_datatools.worldbank_utilities.get_worldbank_countries()
            .query('region != "Aggregates"')
            .reset_index().rename(columns = {'index': 'iso3Code'})
)

# get codes in lists to query with
seriesList = [series for series in db.code]
iso3List = [code for code in countries.iso3Code]


#%%


WB_data = imf_datatools.get_worldbank_data(
                                            seriesname=seriesList,
                                            country=iso3List,
                                            long=True
                                            )


#%%

# clean up a bit and format then save

wb_long = pd.melt(WB_data, id_vars = ['countrycode', 'countryname', 'dates'])

# merge with metadata
wb_long = pd.merge(left=wb_long, right=db, left_on='variable', right_on='code')

wb_long.to_csv(r'C:\Users\ryost\Box Sync\00- Main\Internal\02 - Work\Projects\RA - Primer\git_RA_primer\draft_7\data\02-Clean\doingBusiness.csv')

