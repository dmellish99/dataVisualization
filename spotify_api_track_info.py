import requests 

import pandas as pd

import json



client_id='e0d8fc6d0f2d49d78fe9e55d44c18a5a'
client_secret='c323467a79fc4ca381a37edcf16a3e3e'


token_url='https://accounts.spotify.com/api/token'


token_data={
    'grant_type': 'client_credentials',
    'client_id': client_id,
    'client_secret': client_secret
}



token_response=requests.request('POST',url=token_url,data=token_data)


token=token_response.json()['access_token']




## Get songs from latest TikTok/Instagram Playlist (Viral Hits)


playlist_id='37i9dQZF1DX2L0iB23Enbq'

playlist_url='https://api.spotify.com/v1/playlists/'+playlist_id+'/tracks'

headers={'Authorization': 'Bearer '+token}


playlist_response= requests.get(playlist_url, headers=headers)


playlist_response=playlist_response.json()


track_info=pd.DataFrame()

## Iterate through each item in the response and collect the relevant features
for item in playlist_response['items']:
    cur_track_name=item['track']['name']
    cur_track_id=item['track']['id']


    release_date=item['track']['album']['release_date']
    release_date_precision=item['track']['album']['release_date_precision']

    duration_ms=item['track']['duration_ms']
    is_explicit=item['track']['explicit']

    popularity=item['track']['popularity']


    artists=[]
    for an_artist in item['track']['artists']:
        cur_id=an_artist['id']
        cur_name=an_artist['name']
        
        artists.append({'artist':cur_name,'id':cur_id})


    cur_track_info={'track_name':[cur_track_name],
     'track_id':[cur_track_id],
     'artists':[artists],
     'release_date':[release_date],
     'release_date_precision':[release_date_precision],
     'duration_ms':[duration_ms],
     'is_explicit':[is_explicit],
     'popularity':[popularity]}
    
    temp_df=pd.DataFrame(cur_track_info)

    track_info=pd.concat([track_info,temp_df])



## Initialize null columns for features we are interested in

features_of_interest=["acousticness",
            "danceability",
            "duration_ms",
            "energy",
            "id",
            "instrumentalness"
            "key",
            "liveness",
            "loudness",
            "mode",
            "speechiness",
            "tempo",
            "time_signature",
            "type",
            "valence"]
  

for a_feature in features_of_interest:

    track_info[a_feature]=None



track_info=track_info.reset_index(drop=True)

track_info.to_csv('track_info.csv',index=False)


## Now go to the track audio info endpoint and collect the desired features



features_df=pd.DataFrame()

for idx in range(0,len(track_info)):

    temp_features_df=pd.DataFrame()
    
    
    track_id=track_info['track_id'][idx]

    print(track_id)

    features_url='https://api.spotify.com/v1/audio-features/'+track_id

    headers={'Authorization': 'Bearer '+token}


    track_response= requests.get(features_url, headers=headers)

    features_lib=track_response.json()   



    ## Now go through each feature and add it to track_info df
    for cur_key in features_lib.keys():

        if cur_key in features_of_interest:

            cur_val=features_lib[cur_key]

            temp_features_df[cur_key]=cur_val

    features_df=pd.concat([features_df,temp_features_df])


features_df.to_csv('features.csv',index=False)



## Now Collect audio analysis response from each track


sections=pd.DataFrame()

segments=pd.DataFrame()

## Now go to the track analysis endpoint and collect the segments and sections


for idx in range(0,len(track_info)):

 
    
    
    track_id=track_info['track_id'][idx]

    

    analysis_url='https://api.spotify.com/v1/audio-analysis/'+track_id

    headers={'Authorization': 'Bearer '+token}


    response= requests.get(analysis_url, headers=headers)

    segments_lib=response.json()['segments']

    
    sections_lib=response.json()['sections']


    # print(sections_lib)


    a_section_df=pd.DataFrame()
    a_segment_df=pd.DataFrame()

    ## Now go through each item and add it to sections df
    for a_section in sections_lib:


        temp_section_df=pd.DataFrame()

        temp_section_lib={}
        for section_key in a_section.keys():
            
            cur_section_val=a_section[section_key]
            temp_section_lib[section_key]=[cur_section_val]

        
        temp_section_lib['track_id']=track_id
        temp_section_df=pd.DataFrame(temp_section_lib)


        
        sections=pd.concat([sections,temp_section_df])


        # print(temp_sections_df)






     ## Now go through each item and add it to segments df
    for a_segment in segments_lib:
        print(a_segment)
        temp_segment_df=pd.DataFrame()

        temp_segment_lib={}
        for segment_key in a_segment.keys():
            
            cur_segment_val=a_segment[segment_key]
            temp_segment_lib[segment_key]=[cur_segment_val]

        
        temp_segment_lib['track_id']=track_id
        temp_segment_df=pd.DataFrame(temp_segment_lib)


        segments=pd.concat([segments,temp_segment_df])

sections.to_csv('sections.csv',index=False)

segments.to_csv('segments.csv',index=False)















