
import sys

# total arguments
n = len(sys.argv)
#print("Total arguments passed:", n)
 
# Arguments passed
#print("\nName of Python script:", sys.argv[0])
 
#print("\nArguments passed:", end = " ")
#for i in range(1, n):
#    print()
#    print(sys.argv[i], end = " ")
     
api_url = sys.argv[1] #First Command Line Argument: URL
api_key = sys.argv[2] #Second Command Line Argument: KEY
api_pro = sys.argv[3] #Third Command Line Argument: REDCap Project Name
api_out = sys.argv[4] #Fourth Command Line Argument: Path and Name of Output File
api_flt = sys.argv[5].replace("?", " ") #Fifth Command Line Argument: Query Filter.
                                        #Cannot have a space " " in the command line argument,
                                        #so use a "?" in place of space " " and change
                                        #it to space " " using replace("?", " ") here.

from redcap import Project


project = Project(api_url, api_key)

data = project.export_records()

import requests

if api_pro == 'Self_Report':
	data = {
		'token': api_key,
		'content': 'record',
		'action': 'export',
		'format': 'csv',
		'type': 'flat',
		'csvDelimiter': '',
		'rawOrLabel': 'label',
		'rawOrLabelHeaders': 'raw',
		'exportCheckboxLabel': 'true',
		'exportSurveyFields': 'true',
		'exportDataAccessGroups': 'false',
		'returnFormat': 'csv',
		'filterLogic': api_flt,
		'exportBlankForGrayFormStatus': 'false',

		'fields[0]': 'record_id',
		'fields[1]': 'subject_id',
		'fields[2]': 'head_size',
		'fields[3]': 'presession_date',
		'fields[4]': 'presession_exp_initials',
		'fields[5]': 'session1_date',
		'fields[6]': 'team_id',
		'fields[7]': 'session1_exp_initials',
		'fields[8]': 'session2_date',
		'fields[9]': 'session2_exp_initials',
		'fields[10]': 'subject_information_sheet_complete',

		'fields[11]': 'consent_date',
		'fields[12]': 'informed_consent_form_complete',
		
		'fields[13]': 'covid_symptoms',
		'fields[14]': 'covid_close_contact',
		'fields[15]': 'covid19_screening_complete',
		
		'fields[16]': 'age',
		'fields[17]': 'sex',
		'fields[18]': 'hisp',
		'fields[19]': 'race',
		'fields[20]': 'income',
		'fields[21]': 'edu',
		'fields[22]': 'exp',
		'fields[23]': 'exp_mc',
		'fields[24]': 'handedness',
		'fields[25]': 'trackpad_preference',
		'fields[26]': 'sph_label',
		'fields[27]': 'shl_impairements',
		'fields[28]': 'shl_impairment_specify',
		'fields[29]': 'shl_impairment_agediagnosis',
		'fields[30]': 'shl_impairment_therapy',
		'fields[31]': 'first_language',
		'fields[32]': 'languages_spoken',
		'fields[33]': 'language_age_learned',
		'fields[34]': 'countries_live_one_year',
		'fields[35]': 'major_schooling_country',
		'fields[36]': 'health_label',
		'fields[37]': 'health_concussion',
		'fields[38]': 'health_seizure',
		'fields[39]': 'health_trauma',
		'fields[40]': 'health_other_trauma_specify',
		'fields[41]': 'health_medications',
		'fields[42]': 'health_vision',
		'fields[43]': 'health_vision_specify',
		'fields[44]': 'demographics_survey_complete',

		'fields[45]': 'bfi2_q1',
		'fields[46]': 'bfi2_q2',
		'fields[47]': 'bfi2_q3',
		'fields[48]': 'bfi2_q4',
		'fields[49]': 'bfi2_q5',
		'fields[50]': 'bfi2_q6',
		'fields[51]': 'bfi2_q7',
		'fields[52]': 'bfi2_q8',
		'fields[53]': 'bfi2_q9',
		'fields[54]': 'bfi2_q10',
		'fields[55]': 'bfi2_q11',
		'fields[56]': 'bfi2_q12',
		'fields[57]': 'bfi2_q13',
		'fields[58]': 'bfi2_q14',
		'fields[59]': 'bfi2_q15',
		'fields[60]': 'bfi2_q16',
		'fields[61]': 'bfi2_q17',
		'fields[62]': 'bfi2_q18',
		'fields[63]': 'bfi2_q19',
		'fields[64]': 'bfi2_q20',
		'fields[65]': 'bfi2_q21',
		'fields[66]': 'bfi2_q22',
		'fields[67]': 'bfi2_q23',
		'fields[68]': 'bfi2_q24',
		'fields[69]': 'bfi2_q25',
		'fields[70]': 'bfi2_q26',
		'fields[71]': 'bfi2_q27',
		'fields[72]': 'bfi2_q28',
		'fields[73]': 'bfi2_q29',
		'fields[74]': 'bfi2_q30',
		'fields[75]': 'big_five_inventory_2_short_form_bfi2s_complete',

		'fields[76]': 'attach_q1',
		'fields[77]': 'attach_q2',
		'fields[78]': 'attach_q3',
		'fields[79]': 'attach_q4',
		'fields[80]': 'attach_q5',
		'fields[81]': 'attach_q6',
		'fields[82]': 'attach_q7',
		'fields[83]': 'attach_q8',
		'fields[84]': 'attach_q9',
		'fields[85]': 'attach_q10',
		'fields[86]': 'attach_q11',
		'fields[87]': 'attach_q12',
		'fields[88]': 'attachment_style_questionnaire_complete',

		'fields[89]': 'notes_presession_date',
		'fields[90]': 'notes_presession_exp',
		'fields[91]': 'notes_presession_consentedby',
		'fields[92]': 'notes_credit_type',
		'fields[93]': 'notes_credit_granted',
		'fields[94]': 'notes_speech_baseline',
		'fields[95]': 'notes_other',
		'fields[96]': 'presession_notes_for_research_team_only_complete',

		'fields[97]': 'session1notes_session_date',
		'fields[98]': 'notes_session_exp_v2',
		'fields[99]': 'notes_credit_type_v2',
		'fields[100]': 'notes_credit_granted_v2',
		'fields[101]': 'notes_other_v2',
		'fields[102]': 'note_session_observations_v2',
		'fields[103]': 'session_1_notes_for_research_team_only_complete',

                'fields[104]': 'consent_date_2',
                'fields[105]': 'informed_consent_form_multiple_subjecttwo_sessions_complete'
	}

if api_pro == 'Post_Game_Survey':
	data = {
		'token': api_key,
		'content': 'record',
		'action': 'export',
		'format': 'csv',
		'type': 'flat',
		'csvDelimiter': '',
		'rawOrLabel': 'label',
		'rawOrLabelHeaders': 'raw',
		'exportCheckboxLabel': 'true',
		'exportSurveyFields': 'true',
		'exportDataAccessGroups': 'false',
		'returnFormat': 'csv',
		'filterLogic': api_flt,
		'exportBlankForGrayFormStatus': 'false'
	}

if api_pro == 'Team_Data':
	data = {
		'token': api_key,
		'content': 'record',
		'action': 'export',
		'format': 'csv',
		'type': 'flat',
		'csvDelimiter': '',
		'rawOrLabel': 'label',
		'rawOrLabelHeaders': 'raw',
		'exportCheckboxLabel': 'true',
		'exportSurveyFields': 'true',
		'exportDataAccessGroups': 'false',
		'returnFormat': 'csv',
		'filterLogic': api_flt,
		'exportBlankForGrayFormStatus': 'false'
	}

if api_pro == 'Verify_Team_Data':
        data = {
                'token': api_key,
                'content': 'record',
                'action': 'export',
                'format': 'csv',
                'type': 'flat',
                'csvDelimiter': '|',
		'rawOrLabel': 'label',
                'rawOrLabelHeaders': 'raw',
                'exportCheckboxLabel': 'false',
                'exportSurveyFields': 'false',
                'exportDataAccessGroups': 'false',
                'returnFormat': 'csv',
                'filterLogic': api_flt,
                'exportBlankForGrayFormStatus': 'false'
        }

# Send Post Request to Redcap API
r = requests.post(url=api_url,data=data)

import os, os.path

# Write request contents out to CSV file
def safe_open_w(path):
    ''' Open "path" for writing, creating any parent directories as needed.
    '''
    os.makedirs(os.path.dirname(path), exist_ok=True)
    return open(path, 'w')

f = safe_open_w(api_out)

#f = open(api_out, "w")
f.write(r.text)
f.close()

if api_pro != 'Verify_Team_Data':
	print()
	print("OUTPUTED: " + api_out)

