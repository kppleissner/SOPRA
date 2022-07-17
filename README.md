
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

### Different operating systems  
The software was tested for Windows OS, Linux and MAC OS.
Especially, the function getVolumes() in the server-side file system viewer for Shiny (package: shinyFiles - https://cran.r-project.org/web/packages/shinyFiles/shinyFiles.pdf) is used to access the OS specific volumes/drives.
In Windows getVolumes()() returns all drives mapped to a letter.
Mac OSX looks in /Volumes/ and lists the directories therein.
In Linux getVolumes()() returns the system root.  
If the function does not recognize the system under which it is running it will throw an error.  

__Recommendations:__     
If error occurs then install the most recent github version via remotes:   

remotes::install_github("thomasp85/shinyFiles")

Check also,  if __shinyFiles::shinyFilesExample()__ works on your system !


See following code as example:

```{r}
  server <-  function(input, output, session) { 

   volumes <- c(getVolumes()())

  # use shinyFile functions
  shinyFileChoose(input, "PlateLi", roots = volumes, session = session)
  shinyDirChoose(input, "outdirectory", roots = volumes, session = session, restrictions = system.file(package = "base"))
...
...
...

}



```
***
### Data for testing  
For testing you can use the data given in the folder __Test-Data-4x96__.  
In the folder __Test-Data-4x96/Files4x96/__ you can see the structure of describing files such as __PlateList-Area-4x96.txt__ or __PlateConf_LookUp.txt__ and __ScreenLog-4x96.txt__.   
The plate list file __PlateList-Area-4x96.txt__ contains 2 plates with 2 replicates for each plate.   
The plate configuration file __PlateConf_LookUp.txt__ describes the plate configuration, for instance GeneSymbols, well annotation and well content (sample wells, control wells, Outerwell etc.).   
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
