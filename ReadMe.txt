Dive Surveys data prep

1.ExtractDataFromMSAccess.R
Read in MS Access data; csv files will be created in the ExtractedData folder

2.CreateUpdatedSppObs.R
Edit known errors/typos in the extracted dataset; build separate algae and invert tables 

3a.Algae_BuildMtrx.R & 3b.Invert_BuildMtrx.R
Build species by site matrices: 
* Species X Quadrat table
* Species X Depth category table
* Species X Substrate table
* Species X Substrate & Depth table

5. MakeSiteSHP.R
Build a shapefile for the survey data

6. CalculateSppPrevalence.R
Calculates species prevalence using the Species X Depth category table

7. JoinSpatialPoints.R
Joins species observation data to "spatialized points" from Bottom Patches

8. BuildInSituObs.R
Extracts depth and slope by quadrat; creates matrix with percent cover for all substrate categories by quadrat