# -*- coding: utf-8 -*-
"""
Created on Mon Mar 23 14:31:10 2020

@author: Amir Ali
"""

import sys
import os
import regex
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
nltk.download('punkt')
from nltk.tokenize import word_tokenize, RegexpTokenizer
nltk.download('wordnet')
from nltk.stem import PorterStemmer, WordNetLemmatizer
import matplotlib as plt
import spacy

!runas /user:administrator python -m spacy download en_core_web_sm
!runas /user:administrator python -m spacy download en

import pandas as pd
from spellchecker import SpellChecker
import pkg_resources
from symspellpy.symspellpy import SymSpell, Verbosity


os.getcwd()
os.chdir("C:/Users/anasr/Desktop")

injury_file=open("injury_text.txt",  "r")
injury_text=injury_file.read()
injury_file.close()

injury_text=injury_text.lower()
injury_text=regex.sub(r'\d+', '', injury_text)
#injury_text=regex.sub(r'(\n+)', '', injury_text)
injury_text=regex.sub(r'(\n+)', '\n', injury_text)

"""
spell checking: first with symspellpy package, then with spellchecker package, then remove the wrongly spelled word
"""
tokenizer=RegexpTokenizer(r'\w+')
spell=SpellChecker()

injury_tokens=tokenizer.tokenize(injury_text)
misspelled=spell.unknown(injury_tokens)

max_edit_distance_dictionary = 2
prefix_length = 7
sym_spell = SymSpell(max_edit_distance_dictionary, prefix_length)

dictionary_path = pkg_resources.resource_filename("symspellpy", "frequency_dictionary_en_82_765.txt")
bigram_path = pkg_resources.resource_filename("symspellpy", "frequency_bigramdictionary_en_243_342.txt")
sym_spell.load_dictionary(dictionary_path, term_index=0, count_index=1)
sym_spell.load_bigram_dictionary(bigram_path, term_index=0, count_index=2)

max_edit_distance_lookup = 2
suggestion_verbosity = Verbosity.CLOSEST

for word in misspelled:
    spell_corrected_obj=sym_spell.lookup_compound(word, max_edit_distance_lookup)
    if len(spell_corrected_obj) > 0:
        spell_correction= spell_corrected_obj[0].term
        injury_text=regex.sub(pattern=" "+word+" ", repl=" "+spell_correction+" ", string=injury_text)

injury_tokens=tokenizer.tokenize(injury_text)
misspelled=spell.unknown(injury_tokens)

for word in misspelled:
    spell_correction = spell.correction(word)
    injury_text=regex.sub(pattern=" "+word+" ", repl=" "+spell_correction+" ", string=injury_text)
    
injury_tokens=tokenizer.tokenize(injury_text)
misspelled=spell.unknown(injury_tokens)

for word in misspelled:
    injury_text=regex.sub(pattern=" "+word+" ", repl=" "+''+" ", string=injury_text)

lemmatizer=WordNetLemmatizer()
stemmer=PorterStemmer()
spacy_parser=spacy.load('en', disable=['parser', 'ner'])
spacy_parser.max_length=1500000
stop_words=set(stopwords.words('english'))

injury_doc=spacy_parser(injury_text)
injury_tokens=[token.lemma_ for token in injury_doc if not token.is_punct | 
                                                           token.is_space | 
                                                           token.is_stop]

injury_tokens=[lemmatizer.lemmatize(word) for word in injury_tokens]
clean_tokens=[word for word in injury_tokens if not word in stop_words]
clean_tokens=[word for word in clean_tokens if len(word) > 2]

clean_text=" ".join(clean_tokens)

freq=nltk.FreqDist(clean_tokens)
freq.plot(30, cumulative=False)

lst_freq=pd.DataFrame.from_dict(freq, orient='index', columns=['count'])
lst_freq=lst_freq.sort_values(by=['count'], ascending=False)

lst_freq.to_csv('word_freq.csv')
file=open("cleaned_words.txt", "w")
file.write(clean_text)
file.close()

file=open("cleaned_injury_text.txt", "w")
file.writelines(injury_text)
file.close()
