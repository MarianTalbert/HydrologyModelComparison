To work with the precompiled data load the workspace from within the shiny app: 
load("HydrologyProblem/HydroCode/HydroMapApp/data/ShinyDatStacksPrettyNames.RData")

changing the way the data is compiled would be quite a bit more difficult 
get the data off my computer: 
VIC 4.0.3 many files stored in:
E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\wwcra\\monthly_observed

VIC 4.1.2 many files stored in:
E:\\ClimateCache\\GDO\\BCSD\\ftp_mirror\\gdo-dcp.ucllnl.org\\pub\\dcp\\archive\\cmip5\\hydro\\historical_mon_VIC

Satellite Data from Candida:
E:\\ClimateCache\\SatelliteSWE

Snotel Data from Colin:
C:\\Users\\mtalbert\\Desktop\\HydrologyProblem\\SnotelLocs.csv

University of Washington SWE:
E:\\ClimateCache\\UWHydrology\\SWE.monthly.1950-2000.nc

Start with NCDFVicsSatelliteToRaster.R this summarizes the VIC 4.2.1, 4.0.7 and satelite data to yearly
next run the PrepareUWHydroData.R which does the same with the university of Washington data - this might eventually be removed
because it matches nearly identically the VIC 4.2.1
then SnotelFormatting.R pulls in the Snotel data, extracts from the rasters the pixel values and generates the curves
this file also saves the output and there are some example plots at the end 