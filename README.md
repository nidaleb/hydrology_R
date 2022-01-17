# hydrology_R
Hydrological analysis in R
1 Hydro_File
It should be evident from the lectures that digital elevation models (DEMs) have a great deal of potential for modelling surface
water hydrology and related phenomena in drainage basins. Much of this work is based on the assumptions that (1) we 
can trace the path that water will take from any point in an elevation grid to a basin outlet, and (2) the discharge of water from a particular
location is directly related to its catchment area. This is a fairly reasonable assumption in most drainage basins.

Overview
In this lab exercise, we will use a DEM of the Upper Eskdale catchment to model flow-related phenomena, and will have gained experience in:

DEM pre-processing;
Calculating flow parameters (e.g. pointers and contributing areas);
Landcover
We know from the lecture that some of the factors which influence river hydrochemistry include land cover, soil type and bedrock geology.
Information on these characteristics for the Mersey region under study is contained in the categorical files mersey_LC (based on LCM2000 data),
mersey_HOST (Hydrology of Soil Types) and mersey_bedrock respectively. These datasets contain many different detailed classes, some of which are not 
applicable to the study region. Therefore, the datasets need to be simplified by aggregating some classes and omitting unnecessary classes.
We’ll illustrate the process for the land cover raster, which you can then repeat for the soil type and bedrock rasters.

2 Stat_file
In this final chapter, we will compare the information about catchment characteristics with the water quality data collected at each of the 70 monitoring stations.
To begin, load the csv file created at the end of Task 6 (mersey_watersheds_ea.csv), saving to a new variable called watersheds_df:
