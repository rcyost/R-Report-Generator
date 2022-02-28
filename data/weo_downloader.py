

#%%
import imf_datatools

import pandas as pd

import numpy as np

from pathlib import Path

from functools import reduce

#%%

# all vintages
weoDB = (
        imf_datatools.ecos_sdmx_utilities.get_weo_databases()
        .reset_index()
        .rename({'index':'weo'}, axis=1)
)



#%%


# weo single vintage download

indicatorList = ([
                # GDP
                'NGDP', # nominal gdp in lcu
                'NGDP_R', # real gdp in lcu
                'NGDPDPC', # nominal gdp per capita usd
                'NGDPRPC', # real gdp per capita usd
                'NGDPPCH', # nominal gdp year over year change
                'NGDP_RPCH', # real percent change
                'NGDP_RPCHY', # real gdp percent change quarter y/y
                'NGDPDPC', # gdp per capita usd
                'NGDPRPC_PCH', # gdp per cap percent change

                # Gen Gov Revenue
                'GGRS', # social contributions gen gov rev
                'GGRG', # grants gen gov rev
                'GGRO', # other gen gov rev
                'GGRT_GDP', # tax to gdp

                'GGRC', # commodity-related revenue
 
                # Gen Gov Tax Revenue
                'GGRTI', # Taxes on Income, Profits, Capital Gains
                'GGRTII', # Taxes on Income, Profits, Capital Gains, Individuals
                'GGRTIC', # Taxes on Income, Profits, Capital Gains, Corporations

                'GGRTGS', # Taxes and Goods and Services
                'GGRTT', # Taxes on International Trade and Transactions
                'GGRTOE', # Other Taxes

                # Gen Gov Expense
                "GGE", # Gen Gov Expense LCU

                # Fiscal position
                "GGXWDG_GDP", # General government gross debt, percent of Fiscal year GDP
                "GGXOFB_GDP", # General government overall fiscal balance, percent of Fiscal year GDP
                "DSI_GDP", # Interest paid, percent of GDP in U.S. dollars

                # Monetary
                'PCPI', # consumer prices, period average
                'PCPIE', # consumer prices, end-of-period
                'NGDP_D', # gdp deflator

                # Labor
                'LP', # Population, millions
                'LFE', # Full employment, millions
                'LP_PCH', # population year over year change
                'LLF', # Labor Force
                'LUR', # Unemployment rate
                'LHEM', # Hourly compensation manufacturing
                
                # Balance of Payments
                'BCA_GDP_BP6', # balance on current account 

                'BG_GDP_BP6', # balance on goods 
                'BS_GDP_BP6', # balance on services

                'BIP_GDP_BP6', # primary income
                'BIS_GDP_BP6', # secondary income

                # Investment 
                'NFI_RPCH', # Gross Fixed Capital Formation Real Percent Change
                'NFI_R', # Gross Fixed Capital Formation Real

                # Demand
                'NFDD_R', # Total Domestic Demand
                'NFDD_RPCH', # Total Domestic Demand Pct Change

                'NCP_R', # Private Consumption Expenditure
                'NCG_R', # Public Consumption Expenditure
                'NCP_RPCH', # Private Consumption Expenditure Pct Change
                'NCG_RPCH', # Public Consumption Expenditure Pct Change

                # National Accounts
                'NX_R', # exports
                'NM_R', # imports
                'NMG_R', # imports of goods 
                'NMS_R', # imports of  services 
                'NXG_R', # exports of goods 
                'NXS_R', # exports of  services
                'PCPI_PCH', # Consumer Prices, period average, percent change


])

data_list= []
for indicator in indicatorList:
        # get data
        data = (
                imf_datatools.get_ecos_sdmx_data(database='WEO_WEO_PUBLISHED',
                                                country='All',
                                                var = indicator,
                                                longformat=True)
        )
        data_list.append(data)

data = pd.concat(data_list)

#%%

# remove instances of empty df's
data_list2 = [x for x in data_list if x is not None]
# merge all 
data_1 = reduce(lambda df1, df2: pd.merge(df1, df2, on = ['COUNTRY', 'dates'], how='outer'), data_list2)



#%%

# read in master country list
mcl = pd.read_excel(r'02-Clean\CountryClassifications.xlsx', \
        sheet_name='data',
        # only read in country id cols
        usecols=['Value', 'WEO_ISO_2', 'WEO_Code', 'WEO_ISO_3'],
        dtype={
                'WEO_Code':str
        })

# drop countries without a weo code
mcl.dropna(subset = ["WEO_Code"], inplace=True)

# pivot to long
weoLong = pd.melt(data, id_vars=['COUNTRY', 'dates'])
# get year out of date
weoLong['dates'] = pd.to_datetime(weoLong.dates, format='%Y-%M-%D').dt.year

# merge on country identifiers
weoFormat = pd.merge(left=weoLong, right=mcl, left_on='COUNTRY', right_on='WEO_Code')

# pivot indicators from long to wide, no aggregation
# this is so we can do easy column-wise operations below
weoFormat = (
        weoFormat.pivot_table(
                        index = [col for col in weoFormat.columns if col not in ['value', 'variable']],
                        values = ['value'],
                        columns = ['variable'])
                        .reset_index()

)
 
# clean up ugly multiindexes, all my homies dislike multiindexes
weoFormat.columns = [''.join(col).replace('value', '').replace('.A', '') for col in weoFormat.columns.values]



# convert LCU data into percent of gdp, drop lcu data
weoFormat = (weoFormat
        .assign(
                GGRCgdp=(weoFormat['GGRC']/weoFormat['NGDP'])*100,
                GGRSgdp=(weoFormat['GGRS']/weoFormat['NGDP'])*100,
                GGRGgdp=(weoFormat['GGRG']/weoFormat['NGDP'])*100,
                GGROgdp=(weoFormat['GGRO']/weoFormat['NGDP'])*100,
                GGRTIgdp=(weoFormat['GGRTI']/weoFormat['NGDP'])*100,
                GGRTIIgdp=(weoFormat['GGRTII']/weoFormat['NGDP'])*100,
                GGRTICgdp=(weoFormat['GGRTIC']/weoFormat['NGDP'])*100,
                GGRTGSgdp=(weoFormat['GGRTGS']/weoFormat['NGDP'])*100,
                GGRTTgdp=(weoFormat['GGRTT']/weoFormat['NGDP'])*100,
                GGRTOEgdp=(weoFormat['GGRTOE']/weoFormat['NGDP'])*100,
                GGEgdp=(weoFormat['GGE']/weoFormat['NGDP'])*100,
                NMGgdp=(weoFormat['NMG_R']/weoFormat['NGDP'])*100,
                NMSgdp=(weoFormat['NMS_R']/weoFormat['NGDP'])*100,
                NXGgdp=(weoFormat['NXG_R']/weoFormat['NGDP'])*100,
                NXSgdp=(weoFormat['NXS_R']/weoFormat['NGDP'])*100,
                NXgdp=(weoFormat['NX_R']/weoFormat['NGDP'])*100,
                NMgdp=(weoFormat['NM_R']/weoFormat['NGDP'])*100,
                NFIgdp=(weoFormat['NFI_R']/weoFormat['NGDP'])*100,
                NFDDgdp=(weoFormat['NFDD_R']/weoFormat['NGDP'])*100,
                NCPgdp=(weoFormat['NCP_R']/weoFormat['NGDP'])*100,
                NCGgdp=(weoFormat['NCG_R']/weoFormat['NGDP'])*100
                )

        .drop(['GGRS', 'GGRG', 'GGRO', 'GGRTI', 'GGRTII', 'GGRTIC', 'GGRTGS', 'GGRTT', 'GGRTOE', 'GGE',
        'NMG_R', 'NMS_R', 'NXS_R', 'NXG_R', 'NX_R', 'NM_R', 'NFI_R', 'NFDD_R', 'NCP_R', 'NCG_R'], axis=1)
)

weoFormat = pd.melt(weoFormat, id_vars=['COUNTRY', 'dates', 'Value', 'WEO_ISO_3', 'WEO_ISO_2', 'WEO_Code'])



#%%

# save dataset

dataPath = Path('02-Clean/')

weoFormat.to_csv(dataPath / 'weo.csv')


# %%
