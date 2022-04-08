
OVERVIEW:

In this repository are contained all the R scripts and the initial data necessary to reproduce the pre-processing, the analyses and the results supporting the publication "Effect of tag attachment on flight parameters across five raptor species", Longarini et al. 2022.
To reproduce the workflow save the repository to your computer, and run in sequence the scripts "gitStep0", "gitStep1", "gitStep2" and "gitStep3".
The path to the repository folder in the user's computer needs to be set at the beginning of each of the four scripts, by replacing the argument of the "setwd()" function.

----------

DATA:

In the folder "RawData" are contained the original unmanipulated data needed to start running the R workflow that reproduces all the data processing and analyses of the study. 
Within "RawData", in each daily subfolder the single files are compressed. Important: do not manipulate/unzip these data files. The files are uncompressed within the workflow of "gitStep0", so the user can directly start running the R code.

Some of these files (the raw accelerometry data) required some manual processing to identify the calibration time and re-assign a corrected timestamp to each datapoint. This procedure is partially automatized in steps 0.1 and 0.2 of the script "gitStep0". However, to avoid this manual work, the accelerometry data with corrected timestamp are also provided in this repository (as single zip files in the folder "formatData"); therefore these two initial steps of script "gitStep0" can be skipped and the user can start from step 0.3. These files are also uncompressed within the R workflow.

At the end of "gitStep1" the variables are summarised per behavioural segment and uploaded to the Movebank platform to associate each segment to the local wind conditions, which are then downloaded and used to run the models in "gitStep2". Therefore this dataset is also provided in the folder "finalData".

