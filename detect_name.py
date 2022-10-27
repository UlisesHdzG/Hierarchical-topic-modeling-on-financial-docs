# -*- coding: utf-8 -*-
"""
Function that returns True if the provided string contains a name (for US present in top 3 countries)
r: Minimum position in the ranking of popular names/last names in the US
c: Minimum ranking of popularity of the word as a name/last name in the US
It requires names_dataset module
"""

from names_dataset import NameDataset, NameWrapper
names = NameDataset()

def detect_name(string, r = 5000, c = 3):
    '''
    Function that returns True if the provided string contains a name (for US present in top 3 countries)
    r: Minimum position in the ranking of popular names/last names in the US
    c: Minimum ranking of popularity of the word as a name/last name in the US
    '''
    words = string.split(" ")
    l = False
    for w in words:

        x = names.search(w)['first_name']
        if x is None:# word not found in names dictionary
            f = False
        elif x['rank']['United States'] is None:# Name no frequent in US
            f = False
        elif x['rank']['United States'] > r:# Name unfrequent in US 
            f = False
        else:
            y = x['country']
            countries_first_name = list({k: v for k, v in sorted(y.items(), key=lambda item: item[1], reverse = True)}.keys())[0:c]
            f = 'United States' in countries_first_name
        
        x = names.search(w)['last_name']
        if x is None:# word not found in names dictionary
            p = False
        elif x['rank']['United States'] is None:# Last name no frequent in US
            p = False
        elif x['rank']['United States'] > r:# Last name unfrequent in US 
            p = False
        else:
            y = x['country']
            countries_last_name = list({k: v for k, v in sorted(y.items(), key=lambda item: item[1], reverse = True)}.keys())[0:c]
            p = 'United States' in countries_last_name     
        
        l = l | ( f | p )
    return l
