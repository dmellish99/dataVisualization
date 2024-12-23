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



## Load Cleaned List of Artists


import os 


path=os.getcwd()

path=path.replace('\\','/')

artists=pd.read_csv(path+'/csv/'+'artists.csv')


## Get songs from latest TikTok/Instagram Playlist (Viral Hits)

artists['genres']=''

for idx in range(0,len(artists['id'])):
    artist_id=artists['id'][idx]

    artist_url='https://api.spotify.com/v1/artists/'+str(artist_id)

    headers={'Authorization': 'Bearer '+token}

    response=requests.get(artist_url, headers=headers)

    artists['genres'][idx]=response.json()['genres']



artists.to_csv('artists.csv',index=False)