


#%%

import pandas as pd
import os
import time
from concurrent.futures import ThreadPoolExecutor, as_completed

import concurrent

import http.client, urllib.request, urllib.parse, urllib.error, base64
import json

import time_series_datapoints_df as wto

import logging

from  dbnomics import fetch_series


#%%

mcl = pd.read_csv('mcl.csv', usecols = ['Country Name', 'WEO_ISO_2']).dropna()

#%%

listOfData = []

okList = []
failList = []
fail = 0
ok = 0

for country in sorted(mcl.WEO_ISO_2):

# 2943421 is the max number of series

    try:
        data = fetch_series(provider_code="IMF",
                            dataset_code="DOT",
                            dimensions={"REF_AREA": [country]},
                            max_nb_series = 2943421)
        
        print(country,"-", "ok")

        listOfData.append(data)

    except:
        print(country,"-", "fail")

dots = pd.concat(listOfData)


# %%


