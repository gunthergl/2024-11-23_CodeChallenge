# Code Challenge Code 

This repository holds the code for the code challenge. All plots and results are saved in the "res" folder after running `main.R`. 
The gitlab-ci is part of my local gitlab runner whose results I will show during the discussion. I cannot provide a public link to the gitlab-repository, but will show the results. 

Effectively, the gitlab-ci runs the following steps everytime a new commit is pushed:

 - Downloading data from some server (not Internet)
 - Cleaning the local directory
 - Run main.R
 - Store all results from "res" folder
 - Compress the "res" folder
 - Upload the compressed folder to some server (not Internet)

Excited to discuss the results with you!