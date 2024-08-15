# The code required for pre-processing NBA play-by-play data to use in VI-DPP model. 
This is a Repository to help with the preprocessing of hoopR data obtained from https://hoopr.sportsdataverse.org. The model is not my own. 
The original model is described in the AISTATS-23 paper [Scalable Marked Point Processes for Exchangeable and
Non-Exchangeable Event Sequences](https://arxiv.org/pdf/2105.14574.pdf) and the implementation is originally described in 
https://github.com/aresPanos/Interpretable-Point-Processes . I have added the python file URSS_data_preproccessing
to help with the preprocessing of the data and a couple useful print statements. 
## Guidance. 
First use the R code to download the data and to perform initial edits. The edits include adding columns of required values such as 

*--time intervals 

*--Mark values 

*-- The sequence which each event is tied to. In this game the game each event is part of

*-- Timestamps in seconds. 

One can also use the R code to produce the transition matrix. Various heatmaps may be used. 

Note however these heatmaps must be installed. I used "viridis".

Use the R code to produce a CSV file with the required columns. One then passes this CSV file into the URSS_data_preprocessing file in python to extract the final dataframe. 

Secondly use the python code to perform final edits and to transform the data into the format required to feed into the model. 

The python code contains various functions to inspect the data and to check that the dataframe created is the of the correct form to be fed into the model. 
# NOTE
Do not download any pickle files from the internet and attempt to train the model on these. Pickle files are dangerous. More can be read at 
https://www.synopsys.com/blogs/software-security/python-pickling.html
