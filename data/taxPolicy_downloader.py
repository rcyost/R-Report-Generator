

#%%
import imf_datatools

import pandas as pd

import numpy as np

#%%

#tpCountries = imf_datatools.ecos_sdmx_utilities.get_countries('WEO_WoRLD_FADTP')
# WEO_Rates_FADTP
# WEO_WoRLD_FADTP

dataset = 'WEO_WoRLD_FADTP'
tpStructure = imf_datatools.ecos_sdmx_utilities.get_data_structure(dataset)

tpCountryList = [c for c in tpStructure['Country'].keys()]

tpCountryList = pd.DataFrame([c for k, c in tpStructure['Country'].items()])

tpIndicators = [c for k, c in tpStructure['Indicator'].items()]


dataList = []

for indicator in tpIndicators:

        try:
                # query for data
                data = (imf_datatools.get_ecos_sdmx_data(
                                database = dataset,
                                country = 'All',
                                var=indicator,
                                longformat=True))
                # make data long so we can concat easier
                data = pd.melt(data, id_vars=['COUNTRY', 'dates', 'UNIT_1'])
                # save in List to concat
                dataList.append(data)
        except:
                pass

data = pd.concat(dataList)


mcl = pd.read_excel(r'C:\Users\ryost\Box Sync\00- Main\Internal\02 - Work\Projects\RA - Primer\git_RA_primer\draft_7\data\02-Clean\CountryClassifications.xlsx', \
        sheet_name='data')

mcl = mcl[['Value', 'TPCountryNames', 'WEO_ISO_3', 'WEO_ISO_2', 'WEO_Code', 'Country_Code']]

# we lose Aruba, Dominica and EU, this is ok
world = pd.merge(left=data, right=mcl, left_on = 'COUNTRY', right_on='TPCountryNames')
np.setdiff1d(data['COUNTRY'].unique(), world['COUNTRY'].unique())

# turn dates to just year
world['dates'] = pd.to_datetime(world.dates, format='%D-%M-%Y').dt.year

world.to_csv(r'C:\Users\ryost\Box Sync\00- Main\Internal\02 - Work\Projects\RA - Primer\git_RA_primer\draft_7\data\02-Clean\world.csv')



dataset = 'WEO_Rates_FADTP'
tpStructure = imf_datatools.ecos_sdmx_utilities.get_data_structure(dataset)

tpCountryList = [c for c in tpStructure['Country'].keys()]

tpCountryList = pd.DataFrame([c for k, c in tpStructure['Country'].items()])

tpIndicators = [c for k, c in tpStructure['Indicator'].items()]


dataList = []

for indicator in tpIndicators:

        try:
                # query for data
                data = (imf_datatools.get_ecos_sdmx_data(
                                database = dataset,
                                country = 'All',
                                var=indicator,
                                longformat=True))
                # make data long so we can concat easier
                data = pd.melt(data, id_vars=['COUNTRY', 'dates', 'TAX_TYPE'])
                # save in List to concat
                dataList.append(data)
        except:
                pass


data = pd.concat(dataList)


mcl = pd.read_excel(r'C:\Users\ryost\Box Sync\00- Main\Internal\02 - Work\Projects\RA - Primer\git_RA_primer\draft_7\data\02-Clean\CountryClassifications.xlsx', \
        sheet_name='data')

mcl = mcl[['Value', 'TPCountryNames', 'WEO_ISO_3', 'WEO_ISO_2', 'WEO_Code', 'Country_Code']]

# we lose Aruba, Dominica and EU, this is ok
rates = pd.merge(left=data, right=mcl, left_on = 'COUNTRY', right_on='TPCountryNames')
np.setdiff1d(data['COUNTRY'].unique(), rates['COUNTRY'].unique())



# turn dates to just year
rates['dates'] = pd.to_datetime(rates.dates, format='%D-%M-%Y').dt.year

rates.to_csv(r'C:\Users\ryost\Box Sync\00- Main\Internal\02 - Work\Projects\RA - Primer\git_RA_primer\draft_7\data\02-Clean\rates.csv')

