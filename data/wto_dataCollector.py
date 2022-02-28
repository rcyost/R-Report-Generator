


#%%

import pandas as pd
import os
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import concurrent

import http.client, urllib.request, urllib.parse, urllib.error, base64
import json

# custom functions for querying api
import time_series_datapoints_df as wto

import logging


#%%
key = ''

# only required field other than key
indicators = wto.get_indicators(key=key)
reportEcon = wto.get_reporting_economies(key=key)
products = wto.get_products(key=key)

# look for datasets
code = "HS_M"
of_interest = indicators[indicators.code.str.contains(code)]



#%%

############ BOP6 product sector codes

# ITS_MTV_AX : Merchandise exports by product group – annual (Million US dollar)
# ITS_MTV_AM : Merchandise imports by product group – annual (Million US dollar)

# ITS_CS_AX6 Commercial services exports by sector and partner – annual
# ITS_CS_AM6 Commercial services imports by sector and partner – annual

indicators = ['ITS_MTV_AX', 'ITS_MTV_AM', 'ITS_CS_AX6', 'ITS_CS_AM6']

BOP6_products = products[(products['hierarchy'].str.len()==6) & (products['productClassification']=="BOP6")]

# extract productSector codes in a string seperated by commas to query with
ps_query_string = ''

for code in BOP6_products.code:
    ps_query_string = ps_query_string + code + ','

# this adds extra comma for last code so we remove
bop6_product_string = ps_query_string[:-1]

list_of_data = []

for indicator in indicators:
        
    data = wto.get_wto_data(indicator = 'HS_M_0010',
                        reportEcon=reportEcon,
                        ps_query_string=bop6_product_string,
                        key=key)

    list_of_data.append(data)

BOP6_data = pd.concat(list_of_data)

#%%

############ Harmonized system product sector codes

# HS_M_0010 Bilateral imports by detailed HS codes (2,4,6 digit)
# HS_M_0020 Bilateral imports by MTN product category


indicators = ['HS_M_0010', 'HS_M_0020']

# only want high level 100 top HS codes
HS_products = products[(products['hierarchy'].str.len()==2) & (products['productClassification']=="HS")]

# extract productSector codes in a string seperated by commas to query with
ps_query_string = ''

for code in HS_products.code:
    ps_query_string = ps_query_string + code + ','

# this adds extra comma for last code so we remove
hs_product_string = ps_query_string[:-1]

list_of_data = []

for indicator in indicators:

    data = wto.get_wto_data(indicator = indicator,
                        reportEcon=reportEcon,
                        ps_query_string=hs_product_string,
                        key=key)
    
    list_of_data.append(data)

HM_data = pd.concat(list_of_data)

#HM_data.to_csv('wto_hm_data.csv', index=False)

#%%

##### Duties Downloader

list_of_data = []

of_interest = indicators[indicators.name.str.contains('duty range')]

for code in of_interest.code:

    data = wto.get_wto_data(indicator = code,
                        reportEcon=reportEcon,
                        ps_query_string='default',
                        key=key)

    list_of_data.append(data)

data = pd.concat(list_of_data)


###### Clean up the data


reportEcon = wto.get_reporting_economies(key=key)
# merge on country codes
data_format = pd.merge(left = data, right=reportEcon, left_on='ReportingEconomy', right_on='name')


# replace indicator names with shorter strings

# https://stackoverflow.com/questions/39768547/replace-whole-string-if-it-contains-substring-in-pandas
data_format.loc[data_format['Indicator'].str.contains('0 <= 5', case=False), 'Indicator'] = '0 <= 5'
data_format.loc[data_format['Indicator'].str.contains('5 <= 10', case=False), 'Indicator'] = '5 <= 10'
data_format.loc[data_format['Indicator'].str.contains('10 <= 15', case=False), 'Indicator'] = '10 <= 15'
data_format.loc[data_format['Indicator'].str.contains('15 <= 25', case=False), 'Indicator'] = '15 <= 25'
data_format.loc[data_format['Indicator'].str.contains('25 <= 50', case=False), 'Indicator'] = '25 <= 50'
data_format.loc[data_format['Indicator'].str.contains('50 <= 100', case=False), 'Indicator'] = '50 <= 100'
data_format.loc[data_format['Indicator'].str.contains('> 100', case=False), 'Indicator'] = '> 100'

data_format.loc[data_format['Indicator'].str.contains('5% <= 10', case=False), 'Indicator'] = '5 <= 10'
data_format.loc[data_format['Indicator'].str.contains('10% <= 15', case=False), 'Indicator'] = '10 <= 15'
data_format.loc[data_format['Indicator'].str.contains('15% <= 25', case=False), 'Indicator'] = '15 <= 25'
data_format.loc[data_format['Indicator'].str.contains('25% <= 50', case=False), 'Indicator'] = '25 <= 50'
data_format.loc[data_format['Indicator'].str.contains('50% <= 100', case=False), 'Indicator'] = '50 <= 100'


data_format.to_csv('wto_duties_data.csv', index=False)

#%%



#%%


################ Metadata 

# only required field other than key
indicators = wto.get_indicators(key=key)
reportEcon = wto.get_reporting_economies(key=key)
products = wto.get_products(key=key)

#%%
classif = wto.get_classifications(key=key)
topics = wto.get_topics(key=key)
freq = wto.get_frequencies(key=key)
periods = wto.get_periods(key=key)
units = wto.get_units(key=key)
cate = wto.get_indicator_categories(key=key)
regions = wto.get_regions(key=key)
econGroups = wto.get_economic_groups(key=key)

partnerEcon = wto.get_partner_economies(key=key)

years = wto.get_years(key=key)
valueFlag = wto.get_value_flags(key=key)








