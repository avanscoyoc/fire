# Fire

Analysis of historic California fire sizes and cover type. An idea and analysis led by all members of the Brashares Lab. 

## Data Team Members: 

- Amy Van Scoyoc
- Kendall Calhoun
- Millie Chapman
- Alex McInturff

## Common Files

Our team repository includes the files found here:

- 'README.md' this file is a general overview of the repository in markdown format. 
- 'CA_fire.Rproj', an R-Project file created by RStudio for it's own configuration.
- 'data' folder includes all data files for analyses. 
- 'analyses' folder includes analyses to format and merge data, conduct the cover type analysis, and produce the figures. 

## Data Files

Our team repository data files are described below: 

- 'all_fire_data.csv': (shared by Alex) This file is a datatable exported from CalFire's shapefile??? and includes all available fires, ids, names, locations. 
- 'fires_millie.csv': (shared by Millie) This file is a datatable exported from CalFire's shapefile??? and includes all available fires, cleaner names, ids, locations that will be used in analysis to clipped to covertype shapefile. 
- ICS_209_reports.csv': (shared by Amy) This file is a compiled datatable (retrieved 2/19/19). Data were copied from (https://fam.nwcg.gov/fam-web/hist_209/report_list_209) website tables into .xlsx document from Northern and Southern California regions between all available years (2002-2013).  Data from 2007 and after had different column structure, so 'State Unit', 'Incident Type' and 'Measurement' columns were added and left blank for 2002-2006. Data were then saved to .csv file as appears here. This source includes fire costs.
- 'LF_00_16.csv': (shared by Amy) This file is a webscraped datatable (retrieved 11/16/18). Data were compiled into .csv file using a script and by manual entry from CalFire's Annual Large Fire lists (http://cdfdata.fire.ca.gov/incidents/incidents_statsevents) 2000-2016.  The source PDFs were categorized as all reported fires in california greater than 300 acres and originally put in PDFs from the ICS-209 database. This source includes fire-related deaths. 

# Infrastructure for Testing

- `.travis.yml`: A configuration file for automatically running [continuous integration](https://travis-ci.com) checks to verify reproducibility of all `.Rmd` notebooks in the repo.  If all `.Rmd` notebooks can render successfully, the "Build Status" badge above will be green (`build success`), otherwise it will be red (`build failure`).  
- `DESCRIPTION` a metadata file for the repository, based on the R package standard. It's main purpose here is as a place to list any additional R packages/libraries needed for any of the `.Rmd` files to run.
