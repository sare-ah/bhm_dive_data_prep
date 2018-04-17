Dive Surveys data prep

1. ExtractDataFromMSAccess.R
Read in MS Access data; csv files will be created in the ExtractedData folder

2. Substrate_summarySCUBA.R & Substrate_summaryDropCam.R
Build figures for substrate by proportion of sites

3. CreateUpdatedSppObs.R
Edit known errors/typos in the extracted dataset; build separate algae and invert tables 

4. Build_AlgaeMtrx.R & Build_InvertMtrx.R
Build species by site matrices: 
* Species X Quadrat table
* Species X Depth category table
* Species X Substrate table
* Species X Substrate & Depth table

5. Algae_summarySCUBA.R & Invert_summarySCUBA.R
Build final output figures and tables

6. MakeSiteSHP.R
Build a shapefile for the survey data