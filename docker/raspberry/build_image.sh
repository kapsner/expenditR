#!/bin/bash

# create folder to add to Dockerfile
mkdir addfolder
cd addfolder

# clone repository
git clone https://github.com/kapsner/expenditR.git

# build image
cd ..
docker build -f Dockerfile -t expenditr .

# remove addfolder
rm -rf ./addfolder

docker-compose -f docker-compose.yml up -d
