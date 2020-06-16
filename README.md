# CHP_InjuryAnalysis
Extraction of injuries to body parts from Caltrans CHP collision reports

Count_word(CHP).py\
count the words in the extracted injury string

Extract_doOver(CHP).py\
idenitfy reports for which pytesseract has not been able to find 6 fields per injury description page

Extract_injuryDescrFields(CHP).py\
extract the injury fields in each injury description page using pytesseract and regex matching

Extract_injuryDescrBodyPartPairs(CHP).py\
idenitfy injury description wording and injured body part

Extract_injuryDescrBodyPartPairs(CHP).py\
identify injury description wording and injured body part per report

Extract_narratives(CHP).py\
extract CHP report narrative text

generate_wordCloud(CHP).py\
create a word cloud based on the Count_word(CHP).py

Sankey(injuryDescr_bodypart).R
plot a Sankey diagram showing the flow volume between specific description and body parts
