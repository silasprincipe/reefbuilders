
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="figures/logo.jpg" align="right" width="240" /> Species Distribution Modeling of reef building coral species of the Western Atlantic

## Introduction

This project assessed how climate change will impact the distribution of
three important reef builders of the Western Atlantic. This was done
using SDM tools in an ensemble framework. For more details, reader is
referred to the publication “Predicted shifts in the distributions of
Atlantic reef-building corals in the face of climate change” (under
consideration for publishing). All codes and data used in the study are
available in this folder, except original climatic layers, which are
available online. NOTE: some data will be available uppon publishing of
the article.

## Interactive results

An interactive version of the results of this work is availabe at
<http://silasprincipe.github.io/reefbuilders>

## Files included

**data**: all data used in the study  
– **env**: environmental data  
— **bath\_layers**: bathymetry layers  
— **change\_factor**: change factor applied to obtain future climatic
layers  
— **crop\_layers**: Bio-ORACLE layers cropped to the extent of the
study  
— **proj\_layers**: future scenarios layers — Other files used in
processing environmental data (auxiliary).  
– **muhi**: *Mussismilia hispida* data  
– **moca**: *Montastraea cavernosa* data  
– **side**: *Siderastrea* data  
– **thinning**: files produced in the thinning procedure (presence
data)  
**codes**: all codes used in the study  
– **graphs**: codes used in the production of the maps  
– **climate**: codes used for producing the future climate layers using
CDO (external) and R  
**functions**: all functions that were used either directly or as
support in the study  
**data\_cleaning**: codes used in the data cleaning procces, including
download of data from databases  
**gis**: this folder includes shapefiles used to create maps and as
support  
**boot**: this folder stores bootstrap results  
**Folders with species acronyms**: stores BIOMOD2 results for each
species  
**figures**: contains the logo of the page. Stores maps when code is
executed.  
**auxiliary\_codes**: other codes used to obtain certain data
(auxiliary).

### Reproducible paper

A reproducible version of the results section (that is, all the
analysis) of the article is available here as an Rmd file (results.Rmd).

### Codes

All codes are supplied as individual files, so it’s possible to run each
one separately. However, note that for some codes to work they need to
be executed in the same order as stated here.

1.  download of species data (these codes for species data are in the
    data\_cleaning folder).

*IMPORTANT: this part of the code involve steps of data cleaning. As
global datasets are in continuous change, if you run this code now, you
will probably end with different data than ours (new points may have
been added). Thus, these codes are supplied here just for the sake of
transparency of how data was obtained and cleaned. Final datasets used
in our study are provided with the codes.*  
**species name\_gbif\_cleaning.R**: this file is used to retrieve and
clean GBIF data on species occurrence.  
**species name\_obis\_cleaning.R**: this file is used to retrieve and
clean OBIS data on species occurrence.  
**species name\_data\_preparation.R**: this file is used to merge data
from GBIF, OBIS and bibliographic sources, and to prepare for use with
other codes.  
**tocell\_species name.R**: convert to 1 point per cell.

2.  Environmental layers preparing

**bath\_layer\_prep.R**: used to prepare bathymetry layer, based on
GEBCO.  
**variables\_prep\_current.R**: this file is used to prepare the
environmental layers from BIO-Oracle to be used in the modeling
proccess.  
**wind\_layer\_prep.R**: used to prepare the wind speed layer, from
Copernicus.  
**change\_factor\_apply.R**: used to prepare future layers applying the
change factor obtained from CMIP5 files (note that change factor was
obtained using the Climate Data Operators program. See note on the file
for more information).

3.  Pseudo-absence generation

**kernel\_density.R** and **occ\_thinning**: used to thin presence data.
Note that to `occ_thinning` to work you need to first download
OccurrenceThinner v.1.04 from
<https://github.com/hverbruggen/OccurrenceThinner>. Also, note that
OccThinning need to be run in an “american format” language (due to
differences in the decimal signal).  
**pseudoabGen.R** *(function)*: this file is used to generate
pseudo-absences, required by the modeling procedure. Pseudo absences are
generated based on Mahalonobis distance and a buffer.  
**blockGen.R** *(function)*: used to create cross validation blocks for
the modeling, based on the blockCV package.

4.  Modelling

**biomodModelling.R** *(function)*: code to run the ecological niche
modelling of reef building species.  
**cv\_bootstrap.R**: this code performs a bootstrap procedure for
obtaining coefficent of variations for the niche maps.

NOTE: the code file **modeling\_steps.R** is provided just to organize
the pseudo absence generation, the blockCV procedure and the final
modeling with BIOMOD, which are provided as individual functions.

6.  Graphs and other analysis

**plot\_maps\_mapname.R**: generate maps using ggplot2.  
**areaChange.R** and **getDiff.R**: get area differences between current
and future layers.  
**metricsTable.R** and **varimpTable.R**: get tables of metrics and
variable importance.  
**Varload.R**: load variables for use in each step of the modeling.

7.  Other codes (these are provided in the *climate folder* inside
    *codes*): if you want to execute this, you will first need to
    download CMIP5 files and the CDO program. See note on the
    *change\_factor\_apply.R* code.

**log\_cdo.txt**: code steps used in the CDO program. Note that this is
not executable. You need to write each line once on the program.  
**interpolation.R**: interpolation of CMIP5 layers to obtain the change
factor to be applied to current layers.

-----

Please, contact us if you have any questions on how to use the codes.
