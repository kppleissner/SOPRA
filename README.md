
## _Single Object Profiles Regression Analysis (SOPRA): A novel method for analyzing high content cell-based screens_ 

### Authors:
Rajendra Kumar Gurumurthy2#, Klaus-Peter Pleissner1#, Cindrilla Chumduri 2, Thomas F. Meyer2, André P. Mäurer1*

#These authors contributed equally to the manuscript  
1Steinbeis Center for Systems Biomedicine, Steinbeis Innovation gGmbH, 14612 Falkensee, Germany  
2Max Planck Institute for Infection Biology, Department of Molecular Biology, 10117 Berlin, Germany  

 ***  
### Single Object Profiles Regression Analysis - SOPRA  
 The Single Object Profiles Regression Analysis (SOPRA) workflow enables researchers to identify cell populations with statistically significant changes
 of normalized frequency distribution profiles (histograms) of measured cellular features based on the regression analysis. 
 The regression-based approach was performed using maSigPro from Bioconductor, an R-package originally applied for time-course microarray analysis.
 We defined a regression model where the dependent variable was the bin-wise normalized frequency distribution profile and the independent variable 
 was the measured cellular feature within predefined binning intervals. Our experimental design was based on the single series time-course approach of maSigPro.
 Shortly, maSigPro is a 2-step regression-based method for the analysis of single and multiple time series microarray experiments.
 The first step is a gene selection step that applies the least-square technique to estimate the parameters of regression models and to calculate 
 the variance  for each gene. The p-value associated with F statistics for each gene is used to select significant genes. 
 The p-value is corrected for the multiple comparisons using Benjamini & Hochberg, a false discovery rate method.
 The second step is a variable selection step that applies the stepwise regression approach to identify statistically significant profiles 
 based on the R-square-value of the second regression model between the experimental groups. These explanations are given in the [maSigPro user’s guide](https://bioconductor.org/packages/release/bioc/vignettes/maSigPro/inst/doc/maSigProUsersGuide.pdf).

*** 

### Description of software
The SOPRA software workflow (see Fig.2  in SOPRA project description) is realized as a _**R shiny application**_ with an user interface ui.R and application server.R and mainly consists of four parts, (SOPRA 1 of 4) preprocessing, (SOPRA 2 of 4) data gathering and normalization, (SOPRA 3 of 4) identification of significantly changed cell populations and clustering and (SOPRA 4 of 4) conversion of these significant findings into genes with their cluster membership.  

_**Please read the project description in the Manual folder of the SOPRA  project !**_    

*** 

### How to get the software  
* Go to kppleissner/SOPRA   ->  green Code button ->  Download ZIP
* Extract ZIP file on your local computer. (You should see a folder SOPRA-master) 
* Open file SOPRA_as_Project.Rproj with RStudio
* Open ui.R and server.R
* RunApp  

***

### Software modification for Windows OS   
If you have Windows OS and drive A: is the first drive exlude it in **server.R** following way:    

```{r}
  server <-  function(input, output, session) { 


if(.Platform$OS.type == "windows") {  
    
    volumes <- system("wmic logicaldisk get name", intern = T)
    volumes <- sub(" *\\r$", "", volumes)
    keep <- !tolower(volumes) %in% c("name", "")
    volumes <- volumes[keep]
  #  volumes <- volumes[-1]  #  exclusion of volume A: in Windows OS, i.e. uncomment this line
    names(volumes) <- volumes  
    
}
```
***
### Data for testing  
For testing you can use the data given in the folder __Test-Data-4x96__.  
In the folder __Test-Data-4x96/Files4x96/__ you can see the structure of describing files such as PlateList-Area-4x96.txt or PlateConf_LookUp.txt and ScreenLog-4x96.txt.  
In the folder __Test-Data-4x96/InData4x96/__ the measured data from 96-wells plates are given.

***
### Contact:   
Dr. Klaus-Peter Pleissner  
pleissner@gmail.com
 
--------

### License
This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.  
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this program. If not, see http://www.gnu.org/licenses/.  
