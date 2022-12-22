from cmath import nan
import os
import re
from numpy import NaN
import pandas as pd
from collections import defaultdict

# the purpose of this script is data transformation--taking the raw data gathered from the reviewed articles and making it ready for data visualization in R.


# note: if the linkages in figure 1 get changed, the linkage_8 function needs to be changed. 

# ES typology according to MEA
def es_typology(es):
    # Natural Capital
    biodiversity = ["biodiversity", "species diversity", "forest diversity"]
    habitat = ["habitat", "habitat quality", "wildlife habitat", "habitat provision"]

    # Provisioning
    food = ["food", "food production", "livestock", "livestock production", "agricultural production", "forage quality", "crop production", "fish production", "forage production", "food provision", "fishing", "grain production", "fodder"]
    fiber = ["fiber", "fiber products", "fiber production", "wool","timber production", "timber products", "timber", "forest commodities", "construction materials"]
    fuel = ["fuel", "fuelwood", "fuel production", "bioenergy", "bioenergy production", "bioenergy yield", "biofuel production", "biomass based energy", "biofuel", "hydroelectricity generation"]
    freshwater = ["freshwater", "water supply", "water resources", "water yield", "freshwater provision", "water provision", "water", "provision of irrigation water", "drinking water", "water conservation", "drinking water provision", "water quantity", "water storage"]
    genetic_resources = ["genetic resources"]
    ornamental_resources = ["ornamental resources"]
    medicinal_resources = ["medicinal resources", "medicinal", "medicinal plants", "medical plants", "natural medicine"]

    # Regulating
    air_quality_maintenance = ["air quality maintenance", "air quality regulation", "air purification"]
    global_climate_regulation = ["global climate regulation", "GHG emission regulation", "C emission regulation", "climate regulation", "gas regulation" , "carbon sequestration", "greenhouse gas emission", "carbon storage"]
    regional_climate_regulation = ["regional climate regulation", "microclimate regulation", "local climate regulation"]
    water_regulation = ["water conservation", "water regulation", "water yield", "water resources", "water resource use", "water flow regulation", "flood management", "rain water absorption", "water infiltration", "groundwater recharge", "water cycle support", "flood regulation", "flood protection", "flood potential", "agricultural irrigation", "flood", "flood risk", "stream flow", "streamflow", "water flow", "water retention", "runoff", "runoff attenuation", "flood attenuation", "water storage", "water discharge", "water runoff"]
    erosion_control = ["erosion control", "erosion regulation", "sand fixation", "erosion prevention", "soil erosion", "soil retention", "sediment retention", "soil conservation", "sediment export", "soil loss", "erosion", "sediment control", "soil export"]
    water_purification = ["water purification", "water quality regulation", "water quality"]
    biological_control = ["biological control"]
    pollination = ["pollination", "pollination regulation"]

    # Cultural
    cultural_heritage = ["cultural heritage", "cultural site", "indigenous heritage", "sense of place", "cultural", "cultural values", ""]
    spiritual_value = ["spiritual", "spiritual values", "spiritual value", "religious values", "religious value", "religious", "spiritual services"]
    educational_value = ["education", "educational values", "educational value", "science education", "research", "teaching"]
    aesthetic_value = ["aesthetic value", "aesthetic values"]
    recreation = ["tourism", "urban green space", "recreation", "recreational values"]
    # social_relations = ["social health benefits", "social relations"]
    sense_of_place = ["sense_of_place", "sense of place"]
    inspirational_value = ["inspirational_value", "inspirational value"]

    if es in biodiversity:
        return "biodiversity"
    elif es in habitat:
        return "habitat"
    elif es in food:
        return "food"
    elif es in fiber:
        return "fiber"
    elif es in fuel:
        return "fuel"
    elif es in freshwater:
        return "freshwater"
    elif es in genetic_resources:
        return "genetic_resources"
    elif es in ornamental_resources:
        return "ornamental_resources"
    elif es in medicinal_resources:
        return "medicinal_resources"
    elif es in air_quality_maintenance:
        return "air_quality_maintenance"
    elif es in global_climate_regulation:
        return "global_climate_regulation"
    elif es in regional_climate_regulation:
        return "regional_climate_regulation"
    elif es in water_regulation:
        return "water_regulation"
    elif es in erosion_control:
        return "erosion_control"
    elif es in water_purification:
        return "water_purification"
    elif es in biological_control:
        return "biological_control"
    elif es in pollination:
        return "pollination"
    elif es in cultural_heritage:
        return "cultural_heritage"
    elif es in spiritual_value:
        return "spiritual_value"
    elif es in educational_value:
        return "educational_value"
    elif es in aesthetic_value:
        return "aesthetic_value"
    elif es in recreation:
        return "recreation"
    # elif es in social_relations:
    #     return "social_relations"
    elif es in sense_of_place:
        return "sense_of_place"
    elif es in inspirational_value:
        return "inspirational_value"
    else:
        # # capitalize the entire
        # return es.upper()
        # print(es)
        return "other"

# apply the ES typology from MEA
def clean_es(cell_contents):
    
    if pd.isna(cell_contents):
        return cell_contents

    # studies with ecosystem_services filled in
    else:
        new_list = []
        cell_contents = cell_contents.replace(";", ",")
        cell_contents = cell_contents.strip()
        fields = cell_contents.split(",")

        for es in fields:
            es = es.strip()
            new_list.append(es_typology(es))
        updated_typology = ",".join(new_list)
        return(updated_typology)


def add_es_columns(data):
    data["biodiversity"] = ""
    data["habitat"] = ""

    data["food"] = ""
    data["fiber"] = ""
    data["fuel"] = ""
    data["freshwater"] = ""
    data["genetic_resources"] = ""
    data["ornamental_resources"] = ""
    data["medicinal_resources"] = ""

    data["air_quality_maintenance"] = ""
    data["global_climate_regulation"] = ""
    data["regional_climate_regulation"] = ""
    data["water_regulation"] = ""
    data["erosion_control"] = ""
    data["water_purification"] = ""
    data["biological_control"] = ""
    data["pollination"] = ""

    # NOTE: the ES that are part of the MEA 2003 framework (chapter 2) but are not here are "social relations", "cultural diversity"
    data["cultural_heritage"] = ""
    data["spiritual_value"] = ""
    data["educational_value"] = ""
    data["aesthetic_value"] = ""
    data["recreation"] = ""
    # data["social_relations"] = ""
    data["sense_of_place"] = ""
    data["inspirational_value"] = ""

    data["other"] = ""

    data["es_category"] = ""

    data["provisioning"] = ""
    data["regulating"] = ""
    data["cultural"] = ""

    data.apply(restructure_es, axis = 1)

    return data


# set up so that an ecosystem service type (e.g., food, freshwater) is only counted once per study
def restructure_es(r):
    if pd.notna(r['ecosystem_services']):
        # print('valid')

        es_column = r['ecosystem_services']
        es_column = es_column.strip()
        fields = es_column.split(",")

        for es in fields:
            if es == 'biodiversity':
                r['biodiversity'] = 'yes'
                
            elif es == 'habitat':
                r['habitat'] = 'yes'
                
            elif es == "food":
                r['food'] = 'yes'
                r['provisioning'] = 'yes'
            elif es == 'fiber':
                r['fiber'] = 'yes'
                r['provisioning'] = 'yes'
            elif es == 'fuel':
                r['fuel'] = 'yes'
                r['provisioning'] = 'yes'
            elif es == 'freshwater':
                r['freshwater'] = 'yes'
                r['provisioning'] = 'yes'
            elif es == 'genetic_resources':
                r['genetic_resources'] = 'yes'
                r['provisioning'] = 'yes'
            elif es == 'ornamental_resources':
                r['ornamental_resources'] = 'yes'
                r['provisioning'] = 'yes'
            elif es == 'medicinal_resources':
                r['medicinal_resources'] = 'yes'
                r['provisioning'] = 'yes'
            elif es == 'air_quality_maintenance':
                r['air_quality_maintenance'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'global_climate_regulation':
                r['global_climate_regulation'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'regional_climate_regulation':
                r['regional_climate_regulation'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'water_regulation':
                r['water_regulation'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'erosion_control':
                r['erosion_control'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'water_purification':
                r['water_purification'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'biological_control':
                r['biological_control'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'pollination':
                r['pollination'] = 'yes'
                r['regulating'] = 'yes'
            elif es == 'cultural_heritage':
                r['cultural_heritage'] = 'yes'
                r['cultural'] = 'yes'
            elif es == 'spiritual_value':
                r['spiritual_value'] = 'yes'
                r['cultural'] = 'yes'
            elif es == 'educational_value':
                r['educational_value'] = 'yes'
                r['cultural'] = 'yes'
            elif es == 'aesthetic_value':
                r['aesthetic_value'] = 'yes'
                r['cultural'] = 'yes'
            elif es == 'recreation':
                r['recreation'] = 'yes'
                r['cultural'] = 'yes'
            # elif es == 'social_relations':
            #     r['social_relations'] = 'yes'
            #     r['cultural'] = 'yes'
            elif es == "sense_of_place":
                r['sense_of_place'] = 'yes'
                r['cultural'] = 'yes'
            elif es == "inspirational_value":
                r['inspirational_value'] = 'yes'
                r['cultural'] = 'yes'
            else:
                # print(es)
                r['other'] = 'yes'

        return r

    else:
        return r


# this will skip rows where 'ecosystem_services' column is populated and 'linkages' column is not populated; or vice-versa
def linkage_8(r):
    if pd.notna(r['ecosystem_services']) and pd.notna(r['linkages']):
        # print('valid')

        linkages_column = r['linkages']
        linkages_column = linkages_column.strip()
        linkages_fields = linkages_column.split(",")

        es_column = r['ecosystem_services']
        es_column = es_column.strip()
        fields = es_column.split(",")

        for es in fields:
            if es == "global_climate_regulation":
                if "4" not in linkages_fields:
                    linkages_fields.append("5")
                    r['linkages'] = ",".join(linkages_fields)

    return r

def add_num_linkages_column(data):
    data['num_linkages'] = ""
    data.apply(num_linkages, axis = 1)
    return data

def num_linkages(r):
    if pd.notna(r['linkages']):

        linkages_column = r['linkages']
        linkages_column = linkages_column.strip()
        fields = linkages_column.split(",")
        num_linkages = len(fields)
        
        r['num_linkages'] = num_linkages

    return r

def papers_per_linkage_columns(data):
    data["linkage_1"] = ""
    data["linkage_2"] = ""
    data["linkage_3"] = ""
    data["linkage_4"] = ""
    data["linkage_5"] = ""
    data["linkage_6"] = ""
    data["linkage_7"] = ""
    data["linkage_8"] = ""
    data["linkage_9"] = ""
    data["linkage_10"] = ""

    data.apply(papers_per_linkage, axis = 1)

    return data

def papers_per_linkage(r):
    if pd.notna(r['linkages']):

        linkages_column = r['linkages']
        linkages_column = linkages_column.strip()
        fields = linkages_column.split(",")

        for linkage in fields:
            if linkage == "1":
                r['linkage_1'] = "yes"
            elif linkage == "2":
                r['linkage_2'] = "yes"
            elif linkage == "3":
                r['linkage_3'] = "yes"
            elif linkage == "4":
                r['linkage_4'] = "yes"
            elif linkage == "5":
                r['linkage_5'] = "yes"
            elif linkage == "6":
                r['linkage_6'] = "yes"
            elif linkage == "7":
                r['linkage_7'] = "yes"
            elif linkage == "8":
                r['linkage_8'] = "yes"
            elif linkage == "9":
                r['linkage_9'] = "yes"
            elif linkage == "10":
                r['linkage_10'] = "yes"
            else:
                continue
            
        return r

    else:
        return r

def sort_linkages(r):
    if pd.notna(r['linkages']):

        linkages_column = r['linkages']
        linkages_column = linkages_column.strip()
        fields = linkages_column.split(",")

        fields_sorted = sorted(fields,key = int)

        r['linkages'] = ",".join(fields_sorted)

    return r

def group_linkages(r):
    if pd.notna(r['linkages']):

        linkages_column = r['linkages']
        linkages_column = linkages_column.strip()
        fields = linkages_column.split(",")

        if '7' in fields:
            r['decision_making'] = "yes"
        if '5' in fields or '6' in fields or '7' in fields:
            r['adaptation'] = 'yes'
        if '1' in fields or '2' in fields:
            r['landscape_climate_interactions'] = "yes"
        if '4' in fields:
            r['LUCC_ES'] = 'yes'
        if '3' in fields:
            r['CC_ES'] = 'yes'
        if '3' in fields and '4' in fields:
            r['CC_LUCC_ES'] = 'yes'
        return r

    return r


def add_group_columns(data):

    data['LUCC_ES'] = ""
    data['CC_ES'] = ""
    data['CC_LUCC_ES'] = ""
    data['landscape_climate_interactions'] = ""
    data['adaptation'] = ""
    data['decision_making'] = ""
    
    data.apply(group_linkages, axis = 1)

    return data

def add_article_type_columns(data):
    
    data['case_study'] = "" 
    data['review'] = ""
    data['methodological'] = ""
    data['book_chapter'] = ""

    data.apply(article_type, axis = 1)

    return data

def article_type(r):
    if pd.notna(r['article_type']):

        article_type_column = r['article_type']
        article_type_column = article_type_column.strip()
        fields = article_type_column.split(",")
        print(article_type_column.split(","))

        if 'review' in fields or ' review' in fields:
            r['review'] = "yes"
        if 'case_study' in fields or ' case_study' in fields:
            r['case_study'] = 'yes'
        if 'methodological' in fields or ' methodological' in fields:
            r['methodological'] = "yes"
        if 'book_chapter' in fields or ' book_chapter' in fields:
            r['book_chapter'] = "yes"

    return r

def add_es_column(data):

    data['tradeoffs'] = ""
    data['synergies'] = ""
    data['no_es_interaction'] = ""

    data.apply(add_es_interaction, axis = 1)

    return data

def add_es_interaction(r):

    if pd.notna(r['ES_interaction']):

        es_interaction_column = r['ES_interaction']
        es_interaction_column = es_interaction_column.strip()
        fields = es_interaction_column.split(",")
        # print(fields)

        # if 'synergies' in fields or 'synergy' in fields or 'cobenefits' in fields or 'co_benefits' in fields:
        if 'synergies' in fields or ' synergies' in fields or 'cobenefits' in fields or ' cobenefits' in fields or 'co_benefits' in fields or ' co_benefits' in fields: 
            r['synergies'] = "yes"
        if 'tradeoffs' in fields:
            r['tradeoffs'] = "yes"
 
        return r
    else:
        if r['status'] == 'reviewed':
            r['no_es_interaction'] = "yes"
        return r

# TESTING PURPOSES ONLY
# data = pd.read_excel('/Users/joshuagilman/Documents/code/dissertation/dissertation_chap_1/data/test_data_2.7.2022.xlsx', dtype=str)

#### READ IN THE DATA
data = pd.read_excel('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_1/data_outfiles/sys_review_numbers_switched.xlsx', dtype=str)
# data = pd.read_excel('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_1/data/sys_review_data_7.20.2022.xlsx', dtype=str)

#### CLEAN THE DATA
# Apply MEA typology to ecosystem services column
data['ecosystem_services'] = data['ecosystem_services'].apply(clean_es)

# add es columns, populate with "yes" if the es is addressed by a paper
data = add_es_columns(data)

# make sure that any row with 'global_climate_regulation' ES also has 'linkage_8' included under 'linkages' column
data.apply(linkage_8, axis = 1)

# order the linkages in 'linkages' column sequentially
data.apply(sort_linkages, axis = 1)

# make a new column that is num linkages, populate with number of linkages addressed /study
data = add_num_linkages_column(data)

# make new columns for each linkage, populate with "yes" if the es is addressed by a paper
data = papers_per_linkage_columns(data)

# create arbitrary groups for linkages, populate with "yes" if addressed by the paper
data = add_group_columns(data)

# make new columns for article_type, populate with "yes if addressed by the paper"
data = add_article_type_columns(data)

# make new columns for ES interactions
data = add_es_column(data)

#### OUTPUT data
data.to_csv('/Users/joshuagilman/Documents/code/PhD/dissertation_chap_1/data_outfiles/sr_cleaned.csv', index = False)


