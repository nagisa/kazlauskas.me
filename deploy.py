#!/usr/bin/env python
import urllib.request as rq
import json
import zipfile
import shutil
import os
import datetime

gh_token = os.environ['GH_TOKEN']
headers={
    'Accept': 'application/vnd.github.v3+json',
    'Authorization': f'token {gh_token}'
}
req = rq.Request(
    url='https://api.github.com/repos/nagisa/kazlauskas.me/actions/artifacts',
    headers=headers
)
print(f"Figuring out what to download...")
with rq.urlopen(req, timeout=20) as artifact:
    artifacts = json.load(artifact)['artifacts']
    artifact = max(
        artifacts,
        key=lambda o: datetime.datetime.fromisoformat(o["created_at"].replace("Z", "+00:00"))
    )
    artifact = artifact["archive_download_url"]
print(f"Downloading {artifact}...")
req = rq.Request(url=artifact, headers=headers)
with rq.urlopen(req, timeout=20) as input, open("artifact.zip", "wb") as output:
    shutil.copyfileobj(input, output)
print(f"Downloaded. Extracting...")
with zipfile.ZipFile("artifact.zip") as zip:
    zip.extractall("site_out")
print(f"Done")
