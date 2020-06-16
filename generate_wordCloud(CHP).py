# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 14:58:54 2020

@author: anasr
"""


# -*- coding: utf-8 -*-
"""
Created on Wed Mar 25 17:30:45 2020

@author: Amir Ali
"""

import sys
import os
import regex
import pandas as pd
import csv
from PIL import Image
import wordcloud
from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
import matplotlib.pyplot as plt
%matplotlib qt5

os.getcwd()
os.chdir("C:/Users/anasr/Desktop")

file=open('injury_text.txt', 'r')
injury_text=file.read()
file.close()

file=open('word_freq.csv', 'r')
dic_reader=csv.DictReader(file, fieldnames=('word', 'count'))
next(dic_reader, None)
word_dic={}
for row in dic_reader:
    key=row.pop('word')
    if key in word_dic:
        pass
    word_dic[key]=row.pop('count')
file.close()

word_dic={k:float(v) for (k,v) in word_dic.items()}

stopwords=set(STOPWORDS)
irrelevant_words=['address', 'take', 'transport', 'injure', 'injury', 'describe',
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
                      'logo', 'physician', 'job', 'lee']

stopwords.update(irrelevant_words)
for word in stopwords:
    word_dic.pop(word, None)
#wordcloud=WordCloud(background_color='white').generate(injury_text)
wordcloud=WordCloud(width=1000, height=500, background_color=None, stopwords=stopwords, mode='RGBA')
fig=wordcloud.generate_from_frequencies(word_dic)
plt.imshow(fig, interpolation='bilinear')
plt.axis('off')
plt.show()
