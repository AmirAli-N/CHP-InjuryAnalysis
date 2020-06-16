# -*- coding: utf-8 -*-
"""
Created on Mon Mar 23 18:40:28 2020

@author: Amir Ali
"""
import os
import sys
import regex
import pandas as pd
import spacy

!runas /user:administrator python -m spacy download en_core_web_sm
!runas /user:administrator python -m spacy download en

nltk.download('wordnet')
from nltk.stem import WordNetLemmatizer

from more_itertools import locate
from itertools import islice

os.getcwd()
os.chdir("C:/Users/anasr/Desktop")
injury_file=open("cleaned_injury_text.txt",  "r")
injury_text=injury_file.readlines()
injury_file.close()

#set of numerous but irrelevant words
irrelevant_words=set(['address', 'take', 'transport', 'injure', 'injury', 'describe',
                      'telephone', 'number', 'victim', 'preparer', 'year', 'day',
                      'reviewer', 'pace', 'medical', 'internationally', 'accredit',
                      'agency', 'seek', 'old', 'tim', 'aid', 'treat', 'preparers',
                      'eject', 'officer', 'name', 'passenger', 'ave', 'cop', 'witness',
                      'bag', 'party', 'san', 'air', 'seat', 'ooo', 'equip', 'center', 
                      'scene', 'wet', 'view', 'extent', 'safety', 'area', 'emergency',
                      'room', 'samll', 'hospital', 'solo', 'severe', 'rim', 'vic',
                      'vie', 'staff', 'page', 'mojo', 'doctor', 'vier', 'time',
                      'minor', 'pas', 'case', 'coroner', 'collision', 'body',  'loll',
                      'pronounce', 'eee', 'seatbelt', 'nice', 'would', 'valley',
                      'street', 'sor', 'refuse', 'hour', 'bicycle', 'viet', 'precautionary',
                      'fool', 'los', 'county', 'tool', 'paramedic', 'shin', 'attention',
                      'logo', 'physician', 'job', 'lee'])

descriptive_words=set(['complaint', 'complain', 'compliant', 'pain', 'laceration',
                       'abrasion', 'cut', 'fracture', 'fractured', 'scrape',
                       'visible', 'bruise', 'sore', 'fatal',
                       'contusion', 'break', 'headache', 'scratch', 'deceased',
					   'burn', 'bloody', 'trauma', 'fire', 'headache', 'broken',
                       'deceased', 'bleed', 'bleed', 'swell', 'swollen', 'rash',
                       'concussion', 'consciousness', 'dizziness', 'numbness', 
                       'stiffness', 'redness', 'ache', 'dislocate', 'bleeding',
                       'soreness', 'lacerate', 'dead', 'nausea', 'hurt', 'death',
                       'dizzy', 'sprain', 'blood', 'aceration'])

body_parts_words=set(['neck', 'head', 'chest', 'shoulder', 'arm', 'knee', 'leg',
                      'hand', 'rib', 'face', 'foot', 'elbow', 'forehead', 'hip',
                      'nose', 'jaw', 'chin', 'wrist', 'femur', 'abdomen', 'stomach',
                      'finger', 'ear', 'facial', 'toe', 'thumb', 'eye', 'ankle',
                      'forearm', 'lip', 'bone', 'paw', 'collar', 'abdominal', 'torso',
                      'tooth', 'mouth', 'spine', 'pelvi', 'liver', 'scalp', 'cheek',
                      'abdoman', 'thigh', 'lung', 'calf', 'eyebrow', 'tongue', 'brain',
                      'back', 'skull', 'heart', 'spinal', 'knuckle', 'collarbone'])

#set of body part positions; leave=left
body_pos_words=set(['right', 'left', 'leave', 'lower', 'low', 'upper'])

lemmatizer=WordNetLemmatizer()
spacy_parser=spacy.load('en', disable=['parser', 'ner'])
spacy_parser.max_length=1500000

def find_order(descriptive_set, bodyPos_set, bodyPart_set, tokens):
    ind_descr=[i for word in descriptive_set for i, x in enumerate(tokens) if x==word]
    ind_part=[i for word in bodyPart_set for i, x in enumerate(tokens) if x==word]
    ind_pos=[i for word in bodyPos_set for i, x  in enumerate(tokens) if x==word]
    
    ind=[]
    if len(ind_descr) !=0:
        ind.extend(ind_descr)
    if len(ind_part) !=0:
        ind.extend(ind_part)
    if len(ind_pos) !=0:
        ind.extend(ind_pos)
    
    ind.sort()
    return ind

def separate_descr_part(ind, tokens):
    df=pd.DataFrame(columns=['description', 'body_part'])
    description=str()
    body_part=str()
    ind_iter=iter(ind)
    for i in ind_iter:
        if tokens[i] in descriptive_words:
            description=tokens[i]
            j=i
            if j+1 < len(tokens)-1:
                while tokens[j+1] in descriptive_words:
                    description=description+' '+tokens[j+1]
                    j+=1
                    if j+1==len(tokens):
                        break
                    next(ind_iter)
        if tokens[i] in body_parts_words:
            if tokens[i-1] in body_pos_words:
                if tokens[i-1]=="leave":
                    body_part='left' + ' ' + tokens[i]
                elif tokens[i]=="low":
                    body_part='lower' + ' ' + tokens[i]
                else:
                    body_part=tokens[i-1] + ' ' + tokens[i]
            else:
                body_part=tokens[i]
            df=df.append({'description':description, 'body_part':body_part}, ignore_index=True)
    return df
        
df=pd.DataFrame(columns=['description', 'body_part'])
for line in injury_text:
    #clean up the line
    temp_line=line.lower()
    doc=spacy_parser(temp_line)
    tokens=[token.lemma_ for token in doc if not token.is_punct | 
                                                 token.is_space | 
                                                 token.is_stop]
    tokens=[lemmatizer.lemmatize(token) for token in tokens]
    #end of clean up
    word_set=set(tokens) #create a set of tokens
    descriptive_set=word_set.intersection(descriptive_words) #extract injury description
    bodyPart_set=word_set.intersection(body_parts_words) #extract injured body parts
    bodyPos_set=word_set.intersection(body_pos_words) #extract injured body parts position
    
    ind_lst=find_order(descriptive_set, bodyPart_set, bodyPos_set, tokens) #find the order of the above sets in the originial description
    df=df.append(separate_descr_part(ind_lst, tokens), ignore_index=True) #create a pair of description and body part
    
    
df.to_csv("pair_freq.csv")
