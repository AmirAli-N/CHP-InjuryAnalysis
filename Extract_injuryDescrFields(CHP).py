# -*- coding: utf-8 -*-
"""
Created on Fri Feb 28 15:58:20 2020

@author: anasr
"""

import pytesseract
import glob
import PyPDF2
from PIL import Image
import sys
from pdf2image import convert_from_path
import os
import regex
import multiprocessing
from joblib import Parallel, delayed

os.getcwd()
os.chdir("C:/Users/anasr/Desktop/wzsafety4")
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'

#clean up field description:    removes white space at the start and end of field
#                               replaces new line with space
#                               replaces non-alphanumeric characters with empty space except for : or ;
#                               removes more than one white space                           
def cleanUp_text(string):
    string = string.strip()
    string = string.replace('\n', ' ')
    string = regex.sub(r'[^a-zA-Z0-9:; ]', '', string)
    while '  ' in string:
        string = string.replace('  ', ' ')
    return string

#checks if the page includes a string matching to 'INJURED / WITNESSES / PASSENGERS' within some threshold
#if so, removes the text before the matching string
def indicate_injury_description_page(text):
    similarity_threshold=5
    indicator=regex.search('(INJURED / WITNESSES / PASSENGERS){e<='+str(similarity_threshold)+'}', text, flags=regex.IGNORECASE)
    if indicator is not None:
        text=text[indicator.start():]
        return text
    else:
        return None

#finds the begining of the injury description field by looking for word describe
def start_of_injury_field(text):
    similarity_threshold=1
    indicator=regex.search('(DESCRIBE){e<='+str(similarity_threshold)+'}', text, flags=regex.IGNORECASE)
    if indicator is not None:
        return indicator.start()
    else:
        return None

#finds the end of the injury description field by looking for words 'VICTIM, VIOLENT, CRIME, NOTIFIED'
#tolerance of similarity for these words is different. For example 'crime' with one character tolerance will be found in `cribe` part 'describe'
#out of these four words, the minimum identified index is selected
def end_of_injury_field(text):
    similarity_threshold=1
    victim_searchObj=regex.search('(VICTIM){e<'+str(similarity_threshold)+'}', text, flags=regex.IGNORECASE)
    violent_searchObj=regex.search('(VIOLENT){e<='+str(similarity_threshold)+'}', text, flags=regex.IGNORECASE)
    crime_searchObj=regex.search('(CRIME){e<'+str(similarity_threshold)+'}', text, flags=regex.IGNORECASE)
    notified_searchObj=regex.search('(NOTIFIED){e<='+str(similarity_threshold)+'}', text, flags=regex.IGNORECASE)
    
    ind_end_description_lst=[victim_searchObj.start() if victim_searchObj is not None else -1, 
                             violent_searchObj.start() if violent_searchObj is not None else -1,
                             crime_searchObj.start() if crime_searchObj is not None else -1,
                             notified_searchObj.start() if notified_searchObj is not None else -1]
    
    if (ind_end_description_lst.count(-1) != len(ind_end_description_lst)):
        ind_end=min([i for i in ind_end_description_lst if i != -1])
        return ind_end
    else:
        return None

#if the page indicates injury description, 
    #find the start of each injury field, 
    #find the end, 
    #extract the field text, 
    #remove the field text, and loop again
def parse_text_for_field_description(text):
    injury_descriptions=str()
    text=indicate_injury_description_page(text)
    if text is not None:
        injury_field_start=start_of_injury_field(text)
        injury_field_counter=0
        while injury_field_start is not None:
            injury_field_counter+=1
            text=text[injury_field_start:]
            injury_field_end=end_of_injury_field(text)
            if injury_field_end is not None:
                field_description=text[:injury_field_end]
                text=text.replace(field_description, '', 1)
                field_description=cleanUp_text(field_description)
                injury_descriptions+=field_description+"\n"
            else:
                injury_descriptions+="The end of injury field "+str(injury_field_counter)+" was not found"+"\n"
                return injury_descriptions
            injury_field_start=start_of_injury_field(text)
        else:
            if injury_field_counter==0:
                injury_descriptions+="No injury description field was found!"+"\n"
            else:
                injury_descriptions+="No more injury description field was found!"+" Number of total fields: "+str(injury_field_counter)
        return injury_descriptions
    else:
        return None

def pdf_file_to_text(pdf_file):
    pdf_object=PyPDF2.PdfFileReader(stream=pdf_file, strict=False)
    page_num=pdf_object.getNumPages()
    pages=convert_from_path(pdf_file, last_page=page_num)
    out_file=pdf_file[0:(len(pdf_file)-4)]+"_injuryDesc.txt"
    page_counter=0
    response_string=out_file+"\n"
    f=open(out_file, "a")
    for page in pages:
        page_counter+=1
        text=str(pytesseract.image_to_string(page))
        text=text.replace('-\n', '')
        res=parse_text_for_field_description(text)
        if res is None:
            response_string+="Page "+str(page_counter)+" of "+str(page_num)+" had no injury description"+"\n"
            f.write("Page "+str(page_counter)+" of "+str(page_num)+" had no injury description"+"\n")
        else:
            response_string+="Page "+str(page_counter)+":"+"\n"+res+"\n"
            f.write("Page "+str(page_counter)+":"+"\n"+res+"\n")
    f.close()
    return response_string

#read every pdf file in the current folder
pdf_lst=glob.glob('*.pdf')
num_cores=multiprocessing.cpu_count()

if __name__ == "__main__":
    processed_list=Parallel(n_jobs=num_cores, backend='multiprocessing')(delayed(pdf_file_to_text)(pdf_file) for pdf_file in pdf_lst)
    
#for every file, 
    #get the number of pages,
    #convert them to images,
    #create a text file with the same name,
    #use pytesseract to convert the image to string
    #parse the string
    #write the result
#for pdf_file in pdf_lst:
#    pdf_object=PyPDF2.PdfFileReader(stream=pdf_file, strict=False)
#    page_num=pdf_object.getNumPages()
#    pages=convert_from_path(pdf_file, last_page=page_num)
#    out_file=pdf_file[0:(len(pdf_file)-4)]+"_injuryDesc.txt"
#    f=open(out_file, "a")
#    page_counter=0
#    for page in pages:
#        page_counter+=1
#        text=str(pytesseract.image_to_string(page))
#        text=text.replace('-\n', '')
#        res=parse_text_for_field_description(text)
#        if res is None:
#            f.write("Page "+str(page_counter)+" of "+str(page_num)+" had no injury description"+"\n")
#        else:
#            f.write("Page "+str(page_counter)+":"+"\n"+res+"\n")
#    f.close()

#f=open("13907.txt", "r")
#text=f.read()
#print(parse_text_for_injury_description(text))




