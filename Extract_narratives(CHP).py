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
import pandas as np

os.getcwd()
os.chdir("//ahmct-065/teams/PMRF/Amir/bin/Collision reports")
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'

#clean up field description:    removes white space at the start and end of field
#                               replaces new line with space
#                               replaces non-alphanumeric characters with empty space except for : or ;
#                               removes more than one white space                           
def cleanUp_text(string):
    string = string.strip()
    string = string.replace('|', 'I')
    while '  ' in string:
        string = string.replace('  ', ' ')
    return string

def indicate_narrative_page(text):
    similarity_threshold=5
    indicator=regex.search('(NARRATIVE/SUPPLEMENTAL){e<='+str(similarity_threshold)+'}', text, flags=regex.IGNORECASE)
    if indicator is not None:
        text=text[indicator.start():]
        return text
    else:
        return None
    
def parse_text_for_narratives(text):
    text=indicate_narrative_page(text)
    if text is not None:
        text=cleanUp_text(text)
        return text
    else:
        return None
    
def remove_line_numbers(line):
    one_char_numeric_obj=regex.match(r'^\d ', line)
    two_char_numeric_obj=regex.match(r'^\d. ', line)
    three_char_numeric_obj=regex.match(r'^\d\d ', line)
    four_char_numeric_obj=regex.match(r'^\d\d. ', line)
    if one_char_numeric_obj is not None:
        line=line[one_char_numeric_obj.end():]
    elif two_char_numeric_obj is not None:
        line=line[two_char_numeric_obj.end():]
    elif three_char_numeric_obj is not None:
        line=line[three_char_numeric_obj.end():]
    elif four_char_numeric_obj is not None:
        line=line[four_char_numeric_obj.end():]
    
    one_char_numeric_obj=regex.fullmatch(r'^\d', line)
    two_char_numeric_obj=regex.fullmatch(r'^\d.', line)
    three_char_numeric_obj=regex.fullmatch(r'^\d\d', line)
    four_char_numeric_obj=regex.match(r'^\d\d.', line)
    if one_char_numeric_obj is not None:
        line=line[one_char_numeric_obj.end():]
    elif two_char_numeric_obj is not None:
        line=line[two_char_numeric_obj.end():]
    elif three_char_numeric_obj is not None:
        line=line[three_char_numeric_obj.end():]
    elif four_char_numeric_obj is not None:
        line=line[four_char_numeric_obj.end():]
    
    return line
        
def cleanUp_line_by_line(lines):
    filtered=list(map(remove_line_numbers, lines))
    filtered=list(filter(lambda x: not regex.match(r'^\s*$', x), filtered))
    return filtered

#read every pdf file in the current folder
pdf_lst=glob.glob('*.pdf')

for pdf_file in pdf_lst:
    pdf_object=PyPDF2.PdfFileReader(stream=pdf_file, strict=False)
    page_num=pdf_object.getNumPages()
    pages=convert_from_path(pdf_file, last_page=page_num)
    out_file=pdf_file[0:(len(pdf_file)-4)]+"_narratives.txt"
    f=open(out_file, "a+")
    page_counter=0
    for page in pages:
        page_counter+=1
        text=str(pytesseract.image_to_string(page))
        text=text.replace('-\n', '')
        res=parse_text_for_narratives(text)
        if res is None:
            f.write("Page "+str(page_counter)+" of "+str(page_num)+" had no narrative/supplemental information"+"\n")
        else:
            res=res.splitlines()
            res=cleanUp_line_by_line(res)
            f.write("Page "+str(page_counter)+":"+"\n")
            f.writelines(s + '\n' for s in res)
            f.write("\n")
    f.close()