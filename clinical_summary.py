#!/usr/bin/env python
import requests
import json
patient_req = {                                             #data for patient cohort token
    'token' : 'token here',                    
    'content': 'report',
    'format': 'json',
    'report_id': '621',
    'rawOrLabel': 'label',
    'rawOrLabelHeaders': 'label',
    'exportCheckboxLabel': 'false',
    'returnFormat': 'json'
}

r = requests.post('https://dmsc.mind.uci.edu/redcap/api/', data = patient_req)      #make request for patient cohort
                  
patientc = r.json()                                         #Get JSON from patient cohort which is a list of dictionaries
#print (type(patientc))                                     list
#print (type(patientc[0]))                                  dictionary
#print (patientc[0].get('age'))                             Get age of first patient
#print (patientc[0]['age'])                                 Same as previous



patientCnt = 0                                              #initialize counts and averages
maleCnt, femaleCnt, noSexCnt = 0, 0, 0
avgAge, totalAge = 0, 0
avgEduYear, totalEduYear = 0, 0
ethnicList = []

for i in range(len(patientc)):                              #loop through the JSON
    patientCnt += 1                                         #count total patients
    if (patientc[i]['sex'] == 'F'):                         #count total males and females
        femaleCnt += 1
    elif (patientc[i]['sex'] == 'M'):
        maleCnt +=1
    else:
        noSexCnt +=1

    totalAge += int(patientc[i]['age'])                     #get total age to find avg
    totalEduYear += int(patientc[i]['eduyears'])            #get total edu year to find avg

    ethnicList.append(patientc[i]['ethnicitymod'])          #add ethnicity to empty list

    
    

avgAge = round(totalAge / patientCnt)                       #find avg age
avgEduYear = round(totalEduYear / patientCnt)               #find avg education year

print ("Total Patients:\t " +   str(patientCnt))            #print out everything
print ("Total Males:\t " +      str(maleCnt))
print ("Total Females:\t " +    str(femaleCnt))
print ("Other Sex\t " +         str(noSexCnt))
#print (totalAge)
print ("Avg Age:\t " +          str(avgAge))
print ("Avg Education:\t " +     str(avgEduYear))
#for i in ethnicList:
#        print (i)
    


