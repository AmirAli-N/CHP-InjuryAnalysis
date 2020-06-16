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

from collections import defaultdict
import multiprocessing
from joblib import Parallel, delayed
import joblib.parallel
import glob


os.getcwd()
os.chdir("//AHMCT-065/teams/PMRF/Amir/bin/Injury project/Injury description files")

descriptive_words=set(['complaint', 'complain', 'compliant', 'pain', 'laceration',
                       'abrasion', 'cut', 'fracture', 'fractured', 'scrape',
                       'bruise', 'sore', 'fatal', 'fatality', 
                       'contusion', 'break', 'headache', 'scratch', 'deceased',
					   'burn', 'bloody', 'trauma', 'fire', 'broken',
                      'bleed', 'swell', 'swollen', 'rash',
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
                      'back', 'skull', 'heart', 'spinal', 'knuckle', 'collarbone',
                      'pelvis'])

#set of body part positions; leave=left
body_pos_words=set(['right', 'left', 'leave', 'lower', 'low', 'upper'])

tokenizer=RegexpTokenizer(r'\w+')
spell=SpellChecker()
lemmatizer=WordNetLemmatizer()
spacy_parser=spacy.load('en', disable=['parser', 'ner'])
spacy_parser.max_length=1500000
stop_words=set(stopwords.words('english'))

max_edit_distance_dictionary = 2
prefix_length = 7
sym_spell = SymSpell(max_edit_distance_dictionary, prefix_length)

dictionary_path = pkg_resources.resource_filename("symspellpy", "frequency_dictionary_en_82_765.txt")
bigram_path = pkg_resources.resource_filename("symspellpy", "frequency_bigramdictionary_en_243_342.txt")
sym_spell.load_dictionary(dictionary_path, term_index=0, count_index=1)
sym_spell.load_bigram_dictionary(bigram_path, term_index=0, count_index=2)

max_edit_distance_lookup = 2
suggestion_verbosity = Verbosity.CLOSEST

injury_file_lst=glob.glob('*.txt')

num_cores=multiprocessing.cpu_count()
class CallBack(object):
    completed = defaultdict(int)

    def __init__(self, index, parallel):
        self.index = index
        self.parallel = parallel

    def __call__(self, index):
        CallBack.completed[self.parallel] += 1
        print("done with {}".format(CallBack.completed[self.parallel]))
        if self.parallel._original_iterable:
            self.parallel.dispatch_next()

joblib.parallel.CallBack = CallBack

#write a function to get txt_file name and its text
    #send the text string to another function for cleanup    
    #extract the discription of injuries    
    #send the cleaned text to another function for pair extraction
    #get the list of injury description and their corresponding body part in a list, add a column of file name to each row

def cleanUp_text(string):
    string = string.strip()
    string=string.lower()
    string=regex.sub(r'\d+', '', string)
    string = string.replace('\n', ' ')
    while '  ' in string:
        string = string.replace('  ', ' ')
    return string

def injury_extraction(string):
    injury_descriptions=list()
    injury_field_start=regex.search(r'(DESCRIBE INJURIES:){e<2}', string, flags=regex.IGNORECASE)
    while injury_field_start is not None:
        injury_field_end=regex.search(r'(DESCRIBE INJURIES:){e<2}', string[injury_field_start.end():], flags=regex.IGNORECASE)
        if injury_field_end is not None:
            injury_descriptions.append(string[injury_field_start.end():injury_field_start.end()+injury_field_end.start()])
            string=string.replace(string[:injury_field_start.end()], '', 1)
        else:
            injury_descriptions.append(string[injury_field_start.end():])
        injury_field_start=injury_field_end
    return injury_descriptions

def spell_correction(line):
    spell_corrected_obj=sym_spell.lookup_compound(line, max_edit_distance_lookup)
    if len(spell_corrected_obj) > 0:
        line= spell_corrected_obj[0].term
    
    tokens=tokenizer.tokenize(line)
    misspelled=spell.unknown(tokens)
    for word in misspelled:
        spell_correction = spell.correction(word)
        line=regex.sub(pattern=" "+word+" ", repl=" "+spell_correction+" ", string=line)
    
    tokens=tokenizer.tokenize(line)
    misspelled=spell.unknown(tokens)
    for word in misspelled:
        line=regex.sub(pattern=" "+word+" ", repl=" "+''+" ", string=line)
    return line

def lemmatization(line):
    doc=spacy_parser(line)
    tokens=[token.lemma_ for token in doc if not token.is_punct | token.is_space | token.is_stop]
    tokens=[lemmatizer.lemmatize(word) for word in tokens]
    clean_tokens=[word for word in tokens if not word in stop_words]
    clean_tokens=[word for word in clean_tokens if len(word) > 2]
    return clean_tokens

def unify_words(tokens):
    for i, token in enumerate(tokens):
        if token == "complain" or token =="compliant":
            tokens[i]="complaint"
        if token == "lacerate" or token=="aceration":
            tokens[i]="laceration"
        if token=="fractured":
            tokens[i]="fracture"
        if token=="fatal" or token=="dead" or token=="deceased" or token=="death":
            tokens[i]="fatality"
        if token=="sore":
            tokens[i]="soreness"
        if token=="break":
            tokens[i]="broken"
        if token=="sewll":
            tokens[i]="sowllen"
        if token=="bloody" or token=="bleed" or token=="blood":
            tokens[i]="bleeding"
        if token=="dizzy":
            tokens[i]="dizziness"
        if token=="facial":
            tokens[i]="face"
        if token=="abdominal" or token=="abdoman":
            tokens[i]="abdomen"
        if token=="pelvi":
            tokens[i]="pelvis"
        if token=="spinal":
            tokens[i]="spine"
        if token=="leave":
            tokens[i]="left"
        if token=="low":
            tokens[i]="lower"
    return tokens
        
def find_order(descriptive_set, bodyPos_set, bodyPart_set, tokens):
    ind_descr=[i for word in descriptive_set for i, x in enumerate(tokens) if x==word]
    ind_part=[i for word in bodyPart_set for i, x in enumerate(tokens) if x==word]
    ind_pos=[i for word in bodyPos_set for i, x  in enumerate(tokens) if x==word]
    
    ind=[]
    if len(ind_pos) !=0:
        ind.extend(ind_pos)
    if len(ind_descr) !=0:
        ind.extend(ind_descr)
    if len(ind_part) !=0:
        ind.extend(ind_part)
        
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
                body_part=tokens[i-1] + ' ' + tokens[i]
            else:
                body_part=tokens[i]
            df=df.append({'description':description, 'body_part':body_part}, ignore_index=True)
    return df

def pair_extraction(injury_list):
    df=pd.DataFrame(columns=['field', 'description', 'body_part'])
    field_num=1
    for line in injury_list:
        line=spell_correction(line)
        tokens=lemmatization(line)
        tokens=unify_words(tokens)
        
        word_set=set(tokens)
        descriptive_set=word_set.intersection(descriptive_words) #extract injury description
        bodyPart_set=word_set.intersection(body_parts_words) #extract injured body parts
        bodyPos_set=word_set.intersection(body_pos_words) #extract injured body parts position
    
        ind_lst=find_order(descriptive_set, bodyPart_set, bodyPos_set, tokens) #find the order of the above sets in the originial description
        temp_df=separate_descr_part(ind_lst, tokens)
        temp_df['field']=field_num
        df=df.append(temp_df, ignore_index=True) #create a pair of description and body part
        field_num=field_num+1
    return df

def file_pair_extraction(txt_file):
    df=pd.DataFrame(columns=['file_num', 'field', 'description', 'body_part'])
    file_num=txt_file[0:(len(txt_file)-15)]
    f=open(txt_file, "r")
    string=f.read()
    f.close()
    string=cleanUp_text(string)
    injury_list=injury_extraction(string)
    temp_df=pair_extraction(injury_list)
    temp_df['file_num']=file_num
    df=temp_df
    return df
    
if __name__ == "__main__":
    processed_list=Parallel(n_jobs=1, backend='multiprocessing', verbose=len(injury_file_lst))(delayed(file_pair_extraction)(txt_file) for txt_file in injury_file_lst)
    df=pd.concat(processed_list)
    df.to_csv('Injury_description_pair.csv', sep=",", index=False)
