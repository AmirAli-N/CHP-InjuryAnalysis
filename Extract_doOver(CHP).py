# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import glob
import sys
import os
import regex
import pandas as pd

os.getcwd()
os.chdir("C:/Users/anasr/Desktop/Injury")
file_lst=glob.glob('*.txt')

def parse_text(txt_file):
    f=open(txt_file, "r")
    txt_lines=f.readlines()
    injury_description=str()
    file_incomplete=None
    for line in txt_lines:
        injury_field=regex.search(r'(DESCRIBE INJURIES:){e<2}', line, flags=regex.IGNORECASE)
        if injury_field is not None:
            injury_description=injury_description+line[injury_field.end():]
        
        num_of_fields=regex.search(r'Number of total fields: \d*', line)
        if num_of_fields is not None:
            num=regex.search(r'\d+', line[num_of_fields.start():num_of_fields.end()])
            if num is not None:
                num=num.group()
                if num !='6':
                    file_incomplete=txt_file
            else:
                file_incomplete=txt_file
    f.close()
    return([file_incomplete, injury_description])                               

def cleanUp_text(string):
    string = string.strip()
    string = string.replace('\n', ' ')
    while '  ' in string:
        string = string.replace('  ', ' ')
    return string

do_over_lst=[]
injury_text=str()

for txt_file in file_lst:
    res=parse_text(txt_file)
    if res[0] is not None:
        do_over_lst.append(res[0])
    if res[1] is not None:
        injury_text=injury_text+" "+cleanUp_text(res[1])+"\n"

os.chdir("C:/Users/anasr/Desktop")
injury_file=open("injury_text.txt", "w")
injury_file.write(injury_text)
do_over_file=open("do_over.txt", "w")
do_over_file.writelines(["%s\n" % item  for item in do_over_lst])
injury_file.close()
do_over_file.close()