from cmath import nan
import os
import re
from numpy import NaN
import pandas as pd
from collections import defaultdict

#### READ IN THE DATA
data = pd.read_excel('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_1/data/sys_review_data_7.20.2022.xlsx', dtype=str)
# data = pd.read_excel('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_1/data/test_linkage_switch.xlsx', dtype=str)


#### NOTES

# The purpose of this script is to switch the linkage numbers recorded in the data sheet. I did this because I wanted the linkage numbers to match the way they are displayed in figure 1 in the paper.
# With each iteration of the figure, the numbers assigned to each linkage have changed. 


def switch_linkages(r):
    if pd.notna(r['linkages']):

        linkages_column = r['linkages']
        linkages_column = linkages_column.strip()
        fields = linkages_column.split(",")

        count = 0

        for linkage in fields:
            # print(linkage)
            print(count)
            
            if linkage == "1":
                fields[count] = "1"
            if linkage == "2":
                fields[count] = "2"
            if linkage == "3":
                fields[count] = "7"
            if linkage == "4":
                fields[count] = "3"
            if linkage == "5":
                fields[count] = "8"
            if linkage == "6":
                fields[count] = "9"
            if linkage == "7":
                fields[count] = "10"
            if linkage == "8":
                fields[count] = "4"
            if linkage == "9":
                fields[count] = "5"

                
            count += 1
            
     
        r['linkages'] = ",".join(fields)
        print(r['linkages'])


# order the linkages in 'linkages' column sequentially
data.apply(switch_linkages, axis = 1)

data.to_excel('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_1/data_outfiles/sys_review_numbers_switched.xlsx', index = False)
# data.to_excel('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_1/data_outfiles/test_linkage_switch_result.xlsx', index = False)



# NOTES

# linkage 3 --> 4 and linkage 4 --> 3 on 4.7.2022