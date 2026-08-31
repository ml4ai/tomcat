# CMU TA2  - Profiling Analytic Component Event Message Format

This data message subtype, `AC:BEARD`, contains data from the CMU TA2 Background of Experience, Affect, and Resources Diagnostic (BEARD) Analytic Component.

## TOPIC

agent/ac/ac_cmu_ta2_beard/beard

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From Event Header Format section 
| data.team | number | TODO |
| data.team.anger_mean | number | TODO |
| data.team.anger_sd | number | TODO |
| data.team.anxiety_mean | number | TODO |
| data.team.anxiety_sd | number | TODO |
| data.team.rmie_mean | number | TODO |
| data.team.rmie_sd | number | TODO |
| data.team.mission_knowledge_mean | number | TODO |
| data.team.mission_knowledge_sd | number | TODO |
| data.team.sbsod_mean | number | TODO |
| data.team.sbsod_sd | number | TODO |
| data.team.gaming_experience_mean | number | TODO |
| data.team.gaming_experience_sd | number | TODO |
| data.team.walking_skill_mean | number | TODO |
| data.team.walking_skill_sd | number | TODO |
| data.team.marking_skill_mean | number | TODO |
| data.team.marking_skill_sd | number | TODO |
| data.team.transporting_skill_mean | number | TODO |
| data.team.transporting_skill_sd | number | TODO |
| data.{RED\|BLUE\|GREEN}_ASISTX.anxiety | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.rmie | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.mission_knowledge | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.sbsod | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.gaming_experience | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.walking_skill | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.marking_skill | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.transporting_skill | number | TODO | 
| data.{RED\|BLUE\|GREEN}_ASISTX.role | string | TODO | 

## Message Example

```json
{
  "header": {
    "timestamp": "2021-12-15T20:10:54.166980Z",
    "message_type": "agent",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "AC:BEARD",
    "version": "0.1",
    "source": "ac_cmu_ta2_beard",
    "timestamp": "2021-12-15T20:10:54.166980Z",
    "experiment_id": "aef73f4e-0741-41ec-85f2-9e2728e6fe7c",
    "trial_id": "d921be79-df09-4404-a90a-f2ad0e10f4f4",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75"
},
  "data": {
    "team": {
        "gaming_experience_mean": 2.83,
        "gaming_experience_sd": 0.85,
        "mission_knowledge_mean": 26.7,
        "mission_knowledge_sd": 9.43,
        "anger_mean": 0.0,
        "anger_sd": 0.0,
        "anxiety_mean": 0.0,
        "anxiety_sd": 0.0,
        "rmie_mean": 11.7,
        "rmie_sd": 0.943,
        "sbsod_mean": 4.04,
        "sbsod_sd": 0.0629,
        "walking_skill_mean": 0.222,
        "walking_skill_sd": 0.0549,
        "marking_skill_mean": 3.41,
        "marking_skill_sd": 0.166,
        "transporting_skill_mean": 2.31,
        "transporting_skill_sd": 0.43
    },
    "RED_ASIST1": {
        "role": "Medical_Specialist",
        "gaming_experience": 2.5,
        "mission_knowledge": 40.0,
        "anger": -1.0,
        "anxiety": -1.0,
        "rmie": 11.0,
        "sbsod": 4.0,
        "walking_skill": 0.153,
        "marking_skill": 3.49,
        "transporting_skill": 2.91
    },
    "GREEN_ASIST1": {
        "role": "Transport_Specialist",
        "gaming_experience": 2.0,
        "mission_knowledge": 20.0,
        "anger": -1.0,
        "anxiety": -1.0,
        "rmie": 13.0,
        "sbsod": 4.0,
        "walking_skill": 0.287,
        "marking_skill": 3.18,
        "transporting_skill": 2.08
    },
    "BLUE_ASIST1": {
        "role": "Engineering_Specialist",
        "gaming_experience": 4.0,
        "mission_knowledge": 20.0,
        "anger": -1.0,
        "anxiety": -1.0,
        "rmie": 11.0,
        "sbsod": 4.13,
        "walking_skill": 0.225,
        "marking_skill": 3.57,
        "transporting_skill": 1.94
    }
  },
  "topic": "agent/ac/ac_cmu_ta2_beard/beard"
}
```