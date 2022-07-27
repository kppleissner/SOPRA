# SOPRA
# server.R V.0.15 last edited by KPP: 2022/07

# The software has been implemented in R http://www.r-project.org.
# Software License:
# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.




# - Libraries.

# all libraries are invoked in ui.R


# -Initialization.


script_dir = getwd()
cat("\n Script directory:",script_dir,"\n\n")
options(error = expression(NULL))



# --- Start Server:

server <-  function(input, output, session) { 
  
  volumes <- c(getVolumes()()) #The function is valid for Windows OS, Linux and MAC OS specific and looks for volumes/drives 
  
  # use shinyFile functions
  shinyFileChoose(input, "PlateLi", roots = volumes, session = session)
  shinyFileChoose(input, "PlateCon", roots = volumes, session = session)
  shinyFileChoose(input, "ScreenLo", roots = volumes, session = session)
  shinyDirChoose(input, "indirectory", roots = volumes, session = session, restrictions = system.file(package = "base"))
  shinyDirChoose(input, "outdirectory", roots = volumes, session = session, restrictions = system.file(package = "base"))


  ########## Selection of Directories ########################
  
  observe({
 #   cat("\ninput$indirectory value:\n\n")
    assign("InputFolderName", parseDirPath(volumes, input$indirectory), pos=".GlobalEnv")
    print(input$indirectory)
  })
  
  observe({
   # cat("\ninput$outdirectory value:\n\n")
    assign("OutputFolderName",  parseDirPath(volumes, input$outdirectory), pos=".GlobalEnv")
    print(input$outdirectory)
  })
  
  output$directorypath1 <- renderPrint({
    parseDirPath(volumes, input$indirectory)
  })
  
  output$directorypath2 <- renderPrint({
    parseDirPath(volumes, input$outdirectory)
  })
  
  ########## End of selection of Directories ############
  
  ############# Platelist ###################
  observe({
   # cat("\ninput$PlateLi value:\n\n")
    print(input$PlateLi)
  })
  
  ## print to browser
  output$PlateListOut <- renderPrint({
    parseFilePaths(volumes, input$PlateLi)$datapath # access via datapath  to column datapath in data.frame(KP)
  })
  ######### end PlateList ###############
  
  ######### PlateConfLookUp ################### 
  
  observe({
   # cat("\ninput$PlateCon value:\n\n")
    print(input$PlateCon)
  })
  
  ## print to browser
  output$PlateConfLookUpOut <- renderPrint({
    parseFilePaths(volumes, input$PlateCon)$datapath
  })
  ######### end PlateConfLookUp ###############  
  
  
  ######### ScreenLog ################### 
  
  observe({
  #  cat("\ninput$ScreenLo value:\n\n")
    print(input$ScreenLo)
  })
  
  ## print to browser
  output$ScreenLogOut <- renderPrint({
    parseFilePaths(volumes, input$ScreenLo)$datapath
    
  })
  ######### end ScreenLog  ###############   
  
  
  # - Author Info. 
  observeEvent(input$showauthorinfo, {
    showModal(modalDialog(
      title = "Software and author info.",
      "This software is free software. See the GNU General Public License for more details. If you like it, think about donating for www.worldwildlife.org or www.dkms.org. If used in a publication please cite: Single Object Profiles Regression Analysis (SOPRA): A novel method for analyzing high content cell based screens'.Authors: 'Rajendra Kumar Gurumurthy, Klaus-Peter Pleissner, Cindrilla Chumduri, Thomas F.Meyer, Andre P. Maeurer'"
    ))
  })
  
 
  
  # - Version Info. 
  observeEvent(input$showversioninfo, {
    showModal(modalDialog(
      title = "Software version info.",
      HTML("Single Object Profiles Regression Analysis with interactive selection of folders, files, order of processing,
       parameters of significance, clusters etc. using packages as shiny, shinyFiles, maSigPro, data.table, wordcloud, Mclust, fs etc.<br>
      version 0.15    <br>  Last edit: by Klaus-P. Pleissner- July 26th, 2022")
    ))
  })
  

  
  
  # - 1of4 Info. 
  observeEvent(input$show1of4info, {
    showModal(modalDialog(
      title = "SOPRA 1of4 info.",
     HTML( "SOPRA 1 of 4 performs a preprocessing step, in which all features of the
      indicated wells are flagged by NA. <br>The specified wells are listed in the manually-created
      'ScreenLog' file")
    ))
  })
  
  # - 2of4 Info. 
  observeEvent(input$show2of4info, {
    showModal(modalDialog(
      title = "SOPRA 2of4 info.",
      HTML("SOPRA 2 of 4 performs data gathering and normalization by collecting all data according to a given PlateList,
      annotates the objects (single cells) by a PlateConfiguration file, calculates a common discrete interval (bin) axis for all probability
      distribution profiles (histograms) of a selected feature, performs a median-of-
      control normalization per plate for raw data (plate-wise) and a median-of-control normalization of histograms per bin (called: bin-wise). 
      <br>Subsequently, the density distribution (histogram) of a selected feature (test distribution) is divided
      by the distribution (histogram) of that selected feature for control wells (reference distribution) in a bin-wise manner.<br> 
      Taking the logarithm of the ratio test/reference one gets a zero-profile if the test distribution equals the reference
      distribution. <br> The larger the deviation from the reference distribution the more likely the test distribution would be statistically significant. 
      <br>Finally, an AllDataTable.txt-file containing all normalized and annotated data is generated. This AllDataTable.txt-file serves as input for the determination 
      of statistically significant different histogram profiles using the R-package maSigPro")
    ))
  })
  
  # - 3of4 Info. 
  observeEvent(input$show3of4info, {
    showModal(modalDialog(
      title = "SOPRA 3of4 info.",
     HTML( "SOPRA 3 of 4  finds and clusters statistically significant different density profiles (histograms) of median-of-control normalized (bin-wise)
      feature of siRNAs using the R-package  maSigPro (microarray Significant Profiles).<br>
      maSigPro is a 2-step regression-based method for the analysis of single and multiple time series microarray experiments.
      <br>The first step is a gene selection step that applies the least-square technique to estimate the parameters of regression models
      and to calculate the variance  for each gene. The p-value associated with F statistics for each gene is used to select 
      significant genes. The p-value is corrected for the multiple comparisons using Benjamini & Hochberg, a false discovery rate method.
     <br> The second step is a variable selection step that applies the stepwise regression approach to identify statistically 
      significant profiles based on the R-square-value of the second regression model between the experimental groups.
      <br>This package comprises five functions:
      
      make.design.matrix();  p.vector(); T.fit(); get.siggenes(); see.genes().
      
      <br>Please note, that hclust-method for clustering should not apply if data have been preprocessed. 
      <br>Preprocessing introduces NAs which leads to an error in hclust!! 
      
     <br> For further details see the documentation of maSigPro (https://bioconductor.org/packages/release/bioc/html/maSigPro.html)")
    ))
  })
  
  # - 4of4 Info. 
  observeEvent(input$show4of4info, {
    showModal(modalDialog(
      title = "SOPRA 4of4 info.",
      "SOPRA 4 of 4 converts the statistically significant siRNAs found by the maSigPro-analysis into a file containing the gene
      name, cluster and frequency of genes in a cluster. This step always follows the \"SOPRA 3 of 4 part\" immediately. "
    ))
  })
  
  # - Read ScreenLog.
  ScreenLog  <- reactive({
    inFile <- parseFilePaths(volumes, input$ScreenLo)$datapath
    if(is.null(inFile)) {return(NULL)}
    else{
    return(read.table(inFile, header= TRUE,row.names = NULL, sep="\t",quote = "",stringsAsFactors=F))
    }
    
  })
  
  
  
  # - Read PlateList.
  PlateList  <- reactive({
    inFile2 <- parseFilePaths(volumes, input$PlateLi)$datapath
    if (is.null(inFile2)){return(NULL)}
    else{
    return(read.table(inFile2, header= TRUE,row.names = NULL, sep="\t",quote = "",stringsAsFactors=F))
    }
  }) 
  
  # - Read PlateConfLookUp.
  PlateConfLookUp  <- reactive({
    inFile3 <- parseFilePaths(volumes, input$PlateCon)$datapath
    if (is.null(inFile3)){ return(NULL) }
    else{
    return(read.table(inFile3, header= TRUE,row.names = NULL, sep="\t",quote = "",stringsAsFactors=F))
    }
  }) 
  

  # --- This is the START when the 'GO' button is pressed:
  
  observeEvent(input$GO,{
    
   

    ## Measurement of running time  is obsolete ##
    #
    # start_time <- Sys.time()
    #
    
#############################################################################    
    # --- This is SOPRA 1 of 4:
    
    if(input$preprocess=="TRUE") {
      
      cat("\n\nStart: SOPRA 1 of 4")
      
      # - Read files.
      
      sl = ScreenLog() # Read ScreenLog file.
      pl = PlateList()  # Read Platelist file. 
      
     
      
  
        
        # -- Set Path.
        
        Path_to_Input_Data  <<- InputFolderName  
        Path_to_Output_Data <<- OutputFolderName
        Path_to_Output_Data_Processed = paste(Path_to_Output_Data, "Preprocessing_Results", sep=.Platform$file.sep)
        
        if (!file.exists(Path_to_Output_Data_Processed)) dir.create(Path_to_Output_Data_Processed, recursive=T)
        
        
        # -- Determine plate names. 
        
        pnames = as.character(unique(pl$Filename))
        cat("\n\nData files to be preprocessed: \n\n")
        
        for(i in seq(along=pnames)){
          cat(pnames[i],"\n")
        }
        
        
        # -- Loop.
        
        progresswert = 1/length(pnames)
        withProgress(message = 'SOPRA 1of4.Processing ', value = 0, { # Progress bar
          
          for( i in seq(along=pnames)){
            
            incProgress(progresswert, detail = paste("part", i, "of", length(pnames))) # Progress bar
            
            cat("\nPreprocessing file ",i,"of",length(pnames),"files:\nFile (or Plate): ",pnames[i])
            sub <- subset(sl, sl$Barcode == pnames[i]) # Determine the NA wells.
            wells.to_be_replaced.by.NA = sub$WellNo
            Flag = sl$Flag
            
            #cat(" \n Read plate. ")
            
            file = fread(file=paste(Path_to_Input_Data,"/", pnames[i],sep=""),data.table=FALSE,showProgress=FALSE, stringsAsFactors=FALSE, check.names=TRUE) ## Read data file by fread
            
            clength = colnames(file)
            for (j in 1:length(clength)){
              if (clength[j]=="Well"){wellno=j}else{}
            }
            wellneg = wellno-1
            wellpos = wellno+1
            maxwell=length(clength)
            
            #cat("\n Processing well:")
           
             for (k in seq(along=(wells.to_be_replaced.by.NA))){ 
              file[file$Well == wells.to_be_replaced.by.NA[k],c(2:wellneg)] <- Flag[i]} #filter columns remain
            
            #cat("\nWrite preprocessed table . ")
            filename = paste(Path_to_Output_Data_Processed,"/",pnames[i],sep="")
            filesplit = unlist(strsplit (filename, ".txt", fixed = T))
            filenamenew = paste(filesplit[1],".txt",sep="")
            
            fwrite(file=filenamenew, file, na="NA",sep="\t") # Write NA-modified table using fwrite
            
            cat("\n")
          } # End Loop.
        })  # End Progress
        
        # -- Set Path.
        
        #Path_to_Input_Data = Path_to_Output_Data_Processed
        Path_to_Input_Data <<- Path_to_Output_Data_Processed #KPP
        Path_to_Output_Data <<- OutputFolderName
        
        cat("\n\nEnd: SOPRA 1 of 4")
        
        
      
      
    }else{  
      
      cat("\n No: SOPRA 1 of 4")
      
      # creation of empty tabs in the output
      output$Edesign <- renderTable({return()})
      output$sig_siRNA <- renderTable({return()})
      output$image3 <- renderPlot({return()})
      output$sig_genes <- renderTable({return()})
      output$plot1 <-renderPlot({return()})
      #output$image2 <-renderPlot({return()})  # The QC plot should always be visible 
      
      
      # - Set Path.
      
      Path_to_Output_Data <<- OutputFolderName
      Path_to_Input_Data  <<- InputFolderName  
      
      
    } # --- End of SOPRA 1 of 4.
    
    # --- This is SOPRA 2 of 4:
    
    if(input$dataprocess=="TRUE") {
      
      cat("\n\nStart: SOPRA 2 of 4")
      
      
      # -- Calculating AllDataTable:
      
      # - Assign PlateList and PlateConfLookUp.
      
      Nobi<-as.numeric(input$number_of_bins)
      Nowp<-as.numeric(input$number_of_wells)
      left.quantile<-as.numeric(input$left.quantile)
      right.quantile<-as.numeric(input$right.quantile)

      Gating.Filter = as.numeric(input$gating.filter)
      PlateList = PlateList()
      PlateConf = PlateConfLookUp()
      
      table.out = as.character(input$table.out)  #  T or F
      #t.Test = as.character(input$t.Test) # T or F
      t.Test = as.character("FALSE") # t.Test will  not be performed 
      #
      #t.Testfull = as.character(input$t.Testfull) # T or F
      t.Testfull = as.character("FALSE") # t.Testfull will  not be performed 
      normalize.plate = as.character(input$normalize.plate) #T or F
      
      Platename = PlateList$Platename 
      WellRange=PlateList$WellRange 
      ObjectType=PlateList$ObjectType 
      Filenames=PlateList$Filename 
      Feature=PlateList$Feature 
      firstfeature=Feature[1] # Takes the first feature out of the feature list
      
      Filter1 = PlateList$Filter1[1]  
      Filter2 = PlateList$Filter2[1]
      Filter3 = PlateList$Filter3[1]
      
      Plate = PlateList$Plate
      Replicate =PlateList$Replicate
      
      # The information for 'Platename', 'WellRange','ObjectType','InputData-filenames', 'Feature of interest',
      # three'Gate Filters' (object active=1/inactive=0), plate and replicate is extracted. 
      # Several parameters are determined: No of plates (Nopl)v * No of plate parts (Nopp)v = Total No of plate parts (TNopp).
      # No of plates (Nopl)v * No of plate replicates (Nopr)v = Total No of plates (TNopl)v. Unique plate part names (Pp.names)v.Unique 
      # plate names (Pl.names)v
      
      # Nopp: No of plate parts (Nopp):
      #Nopp = Nowp/as.numeric(substring(PlateList$WellRange[1],first= 4,last = 1000000L))
      
      # Nopp: No of plate parts (Nopp):
      # Nopp = Nowp/as.numeric(substring(PlateList$WellRange[1],first= 4,last = 1000000L))
      #
      
      
      ##### New remarks for the interactive selection of the right well number ##
      # Processing of 96-well plates (by KPP)  :
      #
      # The number of wells which should be processed is given as the max. number of wells in the file "PlateConfLookUp.txt" 
      # If we have 96-plates, but a PlateConfLookUp which describes the configuration of a 384-plate, then 4 x 96-plates
      # must be read (according the file "PlateList.txt"  for 96-well plates) to create a 384-well plate. 
      # The processing of wells and the assembling of data to  the AllDataTable.txt is based on 384 wells given
      # as max. number in "PlateConfLookUp.txt" given in the column "Well_Annotation_2".
      
      Nowp <- max(PlateConf$Well_Annotation_2) # Find maximum of number of wells of PlateConfLookUp (in our case 384)
      # This Nowp is not the number of wells which are selected but the number of maximum number 384 in our PlateConfLookUp
      Nopp = Nowp/as.numeric(input$number_of_wells)  # 96 or 384
      # if 96 is selected then Nopp = 4; if 384 is selected  then Nopp = 1
      
      # Test if the interactively selected number of wells corresponds the plate dimension (96 or 384) 
      # if 96  plates are read according 96. PlateList,  then the interactively selected wells per plate must also be 96, else (STOP)
      # if 384 plates are read according 384. PlateList,  then the interactively selected wells per plate must also be 384, else (STOP) 
      
      # Number of wells taken from PlateList
      NoW <-  as.numeric(substring(PlateList$WellRange[1],first= 4,last = 1000000L))
      # Number of wells interactively selected
      NoWis <- as.numeric(input$number_of_wells) 
      
      
      
      if ( !(NoW == NoWis)){stop("\n\n !! Choose  the suitable number of wells that must be matched to the selected PlateList !!") }
      
      ####################### end of new remarks  ###############################
      
      
      
      # Pp.names:
      Pp.names = unique(Replicate)
      # TNopl:  Total No of plates (TNopl):
      TNopl = length(Replicate)/Nopp
      # Pl.names:
      Pl.names = as.character(unique(Plate))
      # Nopl: No of plates (Nopl):
      Nopl = max(unique(Plate)) 
      # Nopr: No of plate replicates (Nopr):
      Nopr = max(unique(Replicate))
      
      # - Define vector maxima, minima, maximaraw and minimaraw. 
      maxima = vector() 
      minima = vector() 
      maximaraw = vector() 
      minimaraw = vector() 
      
      dfsraw =list (data.frame())
      dfsnz =list (data.frame())
      
      # -- Start Main Loop:
      
      progresswert = 1/TNopl
      withProgress(message = 'SOPRA 2 of 4. Processing ', value = 0, { # Progress bar
        
        for( l in 1:TNopl ){ # - Main-loop. # Total No of plates (TNopl)
          
          incProgress(progresswert, detail = paste("part", l, "of", TNopl)) # Progress bar
          Total_of_all_wells = list() # Initialize object.
          cat("\n\nProcessing of plate",PlateList$Plate[1+((l-1)*Nopp)], "replicate",PlateList$Replicate[1+((l-1)*Nopp)])
          
          for( k in 1:Nopp ){ # - Inner-loop1. # Number of plate parts. 
            
            # - Read file.
            filename=paste(Path_to_Input_Data,"/",Filenames[k+((l-1)*Nopp)],sep="")
            cat("\nFile ", filename, "is read.\n")
            
            file = fread(file=filename,data.table=FALSE,showProgress=FALSE, stringsAsFactors=FALSE, check.names=TRUE) ## Read data file by fread

            # - Replace komma with dot.
            file[,Feature[k+((l-1)*Nopp)]] = gsub(",",".",file[,Feature[l]]) 
            # - Replace -Inf by NA.
            file[,Feature[k+((l-1)*Nopp)]] = gsub("-Inf","NA",file[,Feature[k+((l-1)*Nopp)]]) 
            # - Replace Inf by NA.
            file[,Feature[k+((l-1)*Nopp)]] = gsub("Inf","NA",file[,Feature[k+((l-1)*Nopp)]]) 
            # - Extract columns: Object.ID, feature of interest, well and filters.
            nm <- c("Object.ID", Feature[k+((l-1)*Nopp)],"Well",Filter1, Filter2, Filter3) 
            file=file[,colnames(file) %in% nm] 
            
            # - Concatenate dataframe.
            Total_of_all_wells = rbind(Total_of_all_wells,file) #This contains all 4 parts of a plate
            
          } # - End Inner-loop1.
          
          # -- Annotation of each object with additional information:
          
          rm(file) # Remove object.
          
          # - Sort according to well number.
          All.data.ordered = Total_of_all_wells[order(Total_of_all_wells$Well, decreasing =F),] 
          rm (Total_of_all_wells) # Remove Object.
          
          # - Generate vector with platenumber, replicatenumber and wellnumber.
          plate= rep(PlateList$Plate[k+((l-1)*Nopp)],dim(All.data.ordered)[1]) 
          replicate = rep(PlateList$Replicate[k+((l-1)*Nopp)],dim(All.data.ordered)[1])
          Well=All.data.ordered$Well 
          nm <- c("Well")
          
          All.data.ordered$Well <- NULL  # Remove Well column.
          
          # - Regenerate data.frame with new column order (Replicate,Plate,Well,data).
          All.data.ordered= cbind(plate,replicate,Well,All.data.ordered)  
          Control = PlateList$Control[k+((l-1)*Nopp)] # Extract control.
          
          # -- Generate annotated data file:

          # - Extract data. 
          PlateConfSub = subset(PlateConf, PlateConf$Plate == PlateList$Plate[k+((l-1)*Nopp)])  
          
          # - Merge dataframes.
          #cat("\n Merge dataframes. ")
          All.data.annotated = merge(All.data.ordered, PlateConfSub, by.x="Well", by.y="Well_Annotation_2" ) 
          rm (All.data.ordered) # Removes 'All.data.ordered' from memory.
          
          # -- Objects which are not in defined gates are dismissed:
          
          x = list(data.frame()) # Definition of temporary list x[i].

          # - User feedback  depending on filter setting.
          
          if(Gating.Filter==1){
            #cat("(Gating Filter: 1)")
          }
          
          if(Gating.Filter==2){
          }
          
          if(Gating.Filter==3){
          }
          
          if(Gating.Filter==4){
          } 
          
          if(Gating.Filter==5){
          } 
          
          for( i in seq(along=PlateConfSub$Well_Annotation_2)){ # Inner Loop 2. 
          
            # - Filter data  depending on filter setting:.
            
            if(Gating.Filter==1){
              # - per well filtering: in each x[[i]] are the filtered value for each well.
              
              all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
              x[[i]]   =all.objects.in.well[all.objects.in.well[[Filter1]] == 1,] }
            
            if(Gating.Filter==2){
              # - per well filtering: in each x[[i]] are the filtered value for each well.
              
              all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
              x[[i]]   =all.objects.in.well[all.objects.in.well[[Filter2]] == 1,] }
            
            if(Gating.Filter==3){
              # - per well filtering: in each x[[i]] are the filtered value for each well. 
              
              all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
              x[[i]]   =all.objects.in.well[all.objects.in.well[[Filter3]] == 1,] }
            
            if(Gating.Filter==4){
              
              # - F1+F2
              if(PlateConfSub$Filter[i] == "F1&F2"){
                # - per well filtering: in each x[[i]] are the filtered value for each well
                
                all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
                x[[i]]   =all.objects.in.well[ (all.objects.in.well[[Filter1]] == 1
                                                & all.objects.in.well[[Filter2]] == 1),]}# filtering
              
              # - F1+F3
              if(PlateConfSub$Filter[i] == "F1&F3"){
                # - per well filtering: in each x[[i]] are the filtered value for each well 
                
                all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
                x[[i]]   =all.objects.in.well[ (all.objects.in.well[[Filter1]] == 1 
                                                & all.objects.in.well[[Filter3]] == 1),] }# filtering
              
              # - F1+F3
              if(PlateConfSub$Filter[i] == "F2&F3"){
                # - per well filtering: in each x[[i]] are the filtered value for each well 
                
                all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
                x[[i]]   =all.objects.in.well[ (all.objects.in.well[[Filter2]] == 1 
                                                & all.objects.in.well[[Filter3]] == 1),] }# filtering
              
              # - F1
              if(PlateConfSub$Filter[i] == "F1"){
                # - per well filtering: in each x[[i]] are the filtered value for each well 
                
                all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
                x[[i]]   =all.objects.in.well[all.objects.in.well[[Filter1]] == 1,] }# filtering
              
              # - F2
              if(PlateConfSub$Filter[i] == "F2"){
                # - per well filtering: in each x[[i]] are the filtered value for each well 
                
                all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
                x[[i]]   =all.objects.in.well[all.objects.in.well[[Filter1]] == 2,] }# filtering
              
              # - F3
              if(PlateConfSub$Filter[i] == "F3"){
                # - per well filtering: in each x[[i]] are the filtered value for each well 
                
                all.objects.in.well = All.data.annotated[All.data.annotated$Well == i,]
                x[[i]]   =all.objects.in.well[all.objects.in.well[[Filter1]] == 3,] }# filtering
              
            }
            
            if(Gating.Filter==5){}
            
          } # -- End Inner-loop2.
          
          # -- Concatenate data frames depending on filter setting:
          
          if(Gating.Filter==4){
            All.data.annotated= (do.call("rbind", x))
          }
          
          rm(x) # Remove object 'x' from memory.
          
          # -- Generate dfsraw:
          
          # - Generate data frame dfsraw (All.data.annotated ->dfraw->dfsraw[]).

          nmraw <- c("Well","plate","replicate","Object.ID", Feature[k+((l-1)*Nopp)],"Content","GeneSymbol")
          # - Generation of dataframe.
          dfraw=All.data.annotated[,names(All.data.annotated) %in% nmraw] 
          colnames (dfraw) <- c("Well","plate","replicate","Object.ID", "Feature","Content","GeneSymbol")
          dfsraw[[l]] =dfraw
          
          # -- Export raw data:
          
          # - Save raw data.
          setwd(Path_to_Output_Data) # Set path for output.

          if(table.out){ 
            fwrite(file=paste(Path_to_Output_Data,"/","Data_rawdata_Plate",PlateList$Plate[k+((l-1)*Nopp)],"Replicate",PlateList$Replicate[1+((l-1)*Nopp)],".txt",sep=""),dfraw, na="NA",sep="\t")  # write Data_rawdata_Plate using fwrite
          }
          
          # -- Calculate quantiles and median raw values of controls:
          
          maximaraw[l] = quantile(as.numeric(dfraw[,grep( Feature[k+((l-1)*Nopp)],nmraw)]),
                                  probs = c(right.quantile/100), na.rm=TRUE)[[1]][1]
          minimaraw[l] = quantile(as.numeric(dfraw[,grep( Feature[k+((l-1)*Nopp)],nmraw)]),
                                  probs = c(left.quantile/100),  na.rm=TRUE)[[1]][1]
          
          # -- Object wise median of control (moc) normalization for each plate:
          
          # -Generate dfsnz. (Normalize->Round->Rename columns).
          dfnz = dfraw
          nmnz <- c("Well","plate","replicate","Object.ID", "Feature.normalized","Content","GeneSymbol") 
          
          # - Median/Mean normalize. Data frame.
          if (normalize.plate==TRUE){
            median_of_control_wells = median(as.numeric(subset(dfraw$Feature,dfraw$Content == Control), na.rm=T))

            dfnz$Feature=100*(as.numeric(dfnz$Feature)/abs(median_of_control_wells))  # median normalization
          }else{
            mean_of_control_wells = mean(as.numeric(subset(dfraw$Feature,dfraw$Content == Control), na.rm=T))
            dfnz$Feature=100*(as.numeric(dfnz$Feature)/abs(mean_of_control_wells))  # mean normalization
          }
          
          median_of_control_wells_pnz = median(as.numeric(subset(dfnz$Feature,dfnz$Content == Control), na.rm=T))
          
          # - From this point .. no specific featurename any more.
          colnames (dfnz) <- c("Well","plate","replicate","Object.ID", "Feature.normalized","Content","GeneSymbol") 
          
          dfsnz[[l]] = dfnz
          
   
          
          # --- Export nz data:
          
          # - Set path for output.
          setwd(Path_to_Output_Data) 
          #cat("\n Output table: Data_nzdata. ")
          
          # - Save nz data.
          if(table.out){
            
            fwrite(file=paste(Path_to_Output_Data,"/","Data_nzdata_Plate",PlateList$Plate[1+((l-1)*Nopp)], "Replicate",PlateList$Replicate[1+((l-1)*Nopp)],".txt",sep=""), dfnz, na="NA",sep="\t") # Write  file using fwrite
            
          }
          
          maxima[l] = quantile(as.numeric(dfnz[,grep("Feature.normalized",nmnz)]),probs = c(right.quantile/100), na.rm=TRUE)[[1]][1]
          minima[l] = quantile(as.numeric(dfnz[,grep("Feature.normalized",nmnz)]),probs = c(left.quantile/100),  na.rm=TRUE)[[1]][1]
          
        } # -- End of Loop.
      }) # Progress bar
      
      # -- Calculation of the common binning axis:
      
      # - Determination of feature max. and min. for all plates.
      max.ap = max(maxima, na.rm=T) 
      min.ap = min(minima, na.rm=T) 
      
      # - Calculation of bin-width b and breaks of common binning axis.
      b = ceiling((ceiling(max.ap)-floor(min.ap))/Nobi) 
      bre = floor(min.ap) + c(seq(0,b*(Nobi),by=b)) 
      
      cat("\n\nCommon binning axis is calculated! \n\n")
      
      # -- QC-plots:
      
      Pic.out = as.character(input$pic.out)
      
      if (Pic.out==TRUE){
        cat("\n QC plots are generated. \n")
        
        ldfraw <- vector()
        ldfnz <- vector()
        
        # - Calculate max vector length. 
        for (i in 1:l){ # Start loop.
          
          ldfraw[i]<- length(dfsraw[[i]]$Feature)
          ldfnz[i]<-length(dfsnz[[i]]$Feature) 
        } # End loop.
        
        maxldfraw <- max (ldfraw) 
        maxldfnz <- max (ldfnz)
        
        if (maxldfraw != maxldfnz){cat("\n Warning: Different length for raw and nz data!")}
        
        # - Generate matrix. 
        progresswert = 1/l
        withProgress(message = 'SOPRA 2 of 4. Plotting ', value = 0, { # Progress bar
          
          for (i in 1:l){ # Main loop.
            
            incProgress(progresswert, detail = paste("part", i, "of", l)) # Progress bar
            
            lengthtofill <- maxldfraw-length(dfsraw[[i]]$Feature)
            if (lengthtofill > 0 ){ # Start if.
              lengthtofill = lengthtofill +1
              lengthtofillwNA <- seq (length=lengthtofill)
              is.na(lengthtofillwNA) <- c (1:lengthtofill)
              
              featureraw <- as.numeric(dfsraw[[i]]$Feature) # Cal.4rawdata.
              featurenz <- as.numeric(dfsnz[[i]]$Feature) # Cal.4nzdata.
              
              NAs <- as.numeric (lengthtofillwNA)
              dfsrawwNA <- c(featureraw,NAs,seq="")
              dfsnzwNA <-c(featurenz,NAs,Seq="")} # End if.
            
            if (i==1){ # Start if.
              plotdataraw <- as.data.frame (matrix(as.numeric(dfsrawwNA)))
              plotdatanz <- as.data.frame (matrix(as.numeric(dfsnzwNA)))
            } else {
              plotdataraw = cbind(plotdataraw,as.numeric(dfsrawwNA))
              plotdatanz = cbind(plotdatanz,as.numeric(dfsnzwNA))}# End if.
          } # End main loop.
        }) # End progress bar.
        
        colnames(plotdataraw)<-(c(1:l))
        pdffilenamerawall <- c(paste("BoxplotsRaw-AllPlates.pdf",sep=""))
        pdf (pdffilenamerawall)
        boxplot (plotdataraw)
        dev.off()
        
        colnames(plotdatanz)<-(c(1:l))
        pdffilenamenzall <- c(paste("BoxplotsNz-AllPlates.pdf",sep=""))
        pdf (pdffilenamenzall)
        boxplot (plotdatanz)
        dev.off()
        
        pdffilenamecount <- c(paste("ObjectCount-AllPlates.pdf",sep=""))
        pdf (pdffilenamecount)
        plot (ldfraw, type = "h", col = "grey", lwd = 10,main = "SOPRA QC",xlab ="Plate",ylab="Total Object Count")
        dev.off()
        
  
        output$image2 <- renderPlot({
          boxplot (plotdatanz,  col = "lightblue",xlab = "Plate")
          title("Boxplots of normalized data")
        })
        
      } else {cat("\n No QC plots are generated. \n")
        
        
      }
      
      # --- Calculation of histograms and bin-wize normalization by median-of-controls:
      
      # -- Initialization:
      
      # - Creating dataframe 'lookup' for counting l , plate and rep.  
      lookup = data.frame(index_l= numeric(0), plate=character(0), replicate=character(0), stringsAsFactors=F) 
      
      bin.table = list()    
      y = list(data.frame()) # Delete?
      
      # The data is read and a histogram for the absolute frequency is calculated for each well using the common binning axis. 
      # A histogram for the relative frequency ([%]=countperbin/populationsize) is calculated, 
      # followed by a bin-wize median of control normalization.
      
      progresswert = 1/TNopl
      withProgress(message = 'SOPRA 2 of 4.Calculating ', value = 0, { # Progress bar
        
        for( l in 1:TNopl ){  # Main-loop. # TNopl Total number of plates 
          
          incProgress(progresswert, detail = paste("part", l, "of", TNopl)) # Progress bar
          
          # - Initialization.  
          x = list(data.frame()) 
          Total_of_all_wells = list() 
          cat( "\n\nProcessing of plate",PlateList$Plate[1+((l-1)*Nopp)], "replicate",PlateList$Replicate[1+((l-1)*Nopp)],".\n ") 
          # - Extract control.
          control = PlateList$Control[1+((l-1)*Nopp)]  # Extract control.
          # - Extract data.
          PlateConfSub = subset(PlateConf, PlateConf$Plate ==   PlateList$Plate[k+((l-1)*Nopp)])
          
          # -- Calculate Absolute frequencies:
          
          histos.abs=character()  
          df <-dfsnz[[l]]

          for(i in  1:Nowp){ # Inner-loop1.  #Nowp Number of wells per plate
            
            # - Extract feature values for the i-th well.
            f.well = subset(df[,grep("Feature.normalized",nmnz)],df$Well == i )  
            # - Generate histogram.
            if(!all(is.na( f.well))){ # If well != NA =TRUE.
              # - Only values > min.ap and < max.ap.
              f.well= f.well[f.well >= min.ap & f.well <= max.ap]
              h = hist(f.well,breaks=bre, plot=F)  
              # - Add pseudocount to avoid counts=0.
              h$counts= h$counts + 1 
              # - Write data to variable 'zeile' +sum alldata + line number.
              zeile = c(as.character(h$counts),as.character(sum(h$counts, na.rm=T)),as.character(PlateConfSub$Content[i]))}
            else{zeile = c(rep("NA", Nobi+1 ), as.character(PlateConfSub$Content[i]))} 
            
            # - Add histogram data.
            histos.abs=rbind(histos.abs,zeile) 
            
          } # - End Inner-loop1
          
          # - Add row, column names and extend by plate name and well annotation.  
          row.names(histos.abs) = as.character(1:Nowp)
          colnames(histos.abs) <- c(as.character(floor(h$mids)),"Number of objects", "Well_content")
          
          # - Generate vectors containing plate replicate and number wiht length = wells number. 
          replicate.number =as.character(rep(PlateList$Replicate[k+((l-1)*Nopp)],Nowp))
          plate.number= as.character(rep(PlateList$Plate[k+((l-1)*Nopp)],Nowp))
          Well.number <- (1:Nowp) # /!\ Check for redundancy!
          histos.abs=cbind(plate.number,replicate.number,Well.number,histos.abs)
          
          # - Rename the first three columns.
          colnames(histos.abs)[1:3] <- c("Plate", "Replicate", "Well") 
          
          # - Calculate plate number (pn) and repilcate numerb (rn)
          pn=PlateList$Plate[k+((l-1)*Nopp)] 
          rn=PlateList$Replicate[k+((l-1)*Nopp)] 
          
          # - Save absolute frequencies.
          if(table.out){
           fwrite(file=paste("Absolute_frequencies_of_Plate", pn,"Replicate",rn,".txt",sep=""), as.data.frame(histos.abs), na="NA",sep="\t") # Write file using fwrite
          } 
          
          # -- Calculate relative frequencies: 
          
          histos.rel= matrix(0,Nowp,length(bre)-1)
          for(r in 1:Nowp){
           
            for(rr in 1:(length(bre)-1))      
            {    
              histos.rel[r,rr] = as.numeric(histos.abs[r,rr+3])/as.numeric(histos.abs[r,(dim(histos.abs)[2]-1)]) *100  # generates NA errors
            }
          }
     
          
          # - Create data.frame with rel.frequencies and descriptive columns.  
          histos.rel = as.data.frame(unlist(histos.rel))
          row.names(histos.rel) = as.character(1:Nowp)
          colnames(histos.rel) <- c(as.character(floor(h$mids)))      
          
          # - Extend and change column names. 
          histos.rel=cbind(plate.number,replicate.number,Well.number, histos.rel)
          colnames(histos.rel)[1:3] <- c("Plate","Replicate","Well")      
        
          temp = merge(histos.rel, PlateConfSub, by.x="Well",by.y="Well_Annotation_2" )
          Well_content = temp$Content
          
          # - Extend dataframe by Well_content.
          histos.rel=cbind(histos.rel,Well_content) 
          
          # - Calculate plate number(pn) and replicate number(rn).
          pn=PlateList$Plate[k+((l-1)*Nopp)]
          rn=PlateList$Replicate[k+((l-1)*Nopp)]
          
          # - Output of relative frequencies as table.    
          if(table.out){
            fwrite(file=paste("Relative_frequencies_of_Plate", pn,"Replicate_",rn,".txt",sep=""), as.data.frame(histos.rel), na="NA",sep="\t") # Write file using fwrite
          }
          
          # -- Calculate Normalized frequencies:
          
          # - Extract data rows.
          relfreqctrlrow = subset(histos.rel,Well_content == control )
          
          # - Extract data columns.
          relfreqctrlcols=relfreqctrlrow[,4:(length(bre)+2)] 
          
          # - Calculate median of controls for each bin column.
          medianvalctrls=vector()
          for(r in 1:Nobi) {medianvalctrls[r]=abs(median(as.numeric(relfreqctrlcols[,r]),na.rm = TRUE))}
          
          # - Extract all data. 
          alldatacols=histos.rel[,4:(length(bre)+2)] 
          
          # - Create matrix 'bin' and 'binround'.
          bin = matrix(0, nrow=Nowp, ncol= dim(alldatacols)[2]) 
          binround = matrix(0, nrow=Nowp, ncol= dim(alldatacols)[2])
          
          # - Normalize by the median of the control wells for the same bin.
          for(r in 1:Nobi) { 
            for (rr in 1: Nowp) {
              # - Generate log2 data.
              bin[rr,r] = log2((as.numeric(alldatacols[rr,r])/medianvalctrls[r])) 
            } 
          }
          histos.nz= as.data.frame(unlist(bin)) #without rounding
          row.names(histos.nz) = as.character(1:Nowp)
          
          colnames(histos.nz)<-c(paste("Bin",as.character(floor(h$mids)),"rep",as.character(rn),sep="_"))
          
          replicate.number =as.character(rep(PlateList$Replicate[k+((l-1)*Nopp)],Nowp))
          plate.number= as.character(rep(PlateList$Plate[k+((l-1)*Nopp)],Nowp))
          
          histos.nz=cbind(plate.number,replicate.number, Well.number,histos.nz)
          colnames(histos.nz)[1:3] <- c("Plate", "Replicate","Well")
          
          # - Merge dataframe 'da' with 'PlateConfSub'.
          histos.nz = merge(PlateConfSub, histos.nz, by.x="Well_Annotation_2",by.y="Well",  all=TRUE ) 
          
          Well=histos.nz$Well_Annotation_2 # /?\ Cant we use Well.number?
          Plate=histos.nz$Plate.x # /?\ Cant we use plate.number?
          GeneSymbol=histos.nz$GeneSymbol
          Well_content=histos.nz$Conten
          
          # - Create RNA.ID consisiting of GeneSymbol+Plate+Well.
          RNA.ID = paste(GeneSymbol, Plate, Well, sep="#")
          histos.nz = subset(histos.nz, select= -c(Well_Annotation_2,Plate.x,Content,Replicate,Well_Annotation_1, GeneSymbol,Filter,FilterComment,Plate.y))
          
          # - Create 'AllTopTable' structure.
          histos.nz = cbind(RNA.ID, plate.number, replicate.number, Well, histos.nz, Well_content,GeneSymbol)
          colnames(histos.nz)[2:4] <- c("Plate", "Replicate","Well") 
          
          # - Write each bin-wise normalized table into the list 'bin.table'.
          bin.table[[l]]=histos.nz 
          
          # - Extend dataframe lookup.
          lookup[l,] = c(l, pn, rn)
          
          # - Save normalized frequencies.
          if(table.out){
            fwrite(file=paste("Binwisenz_frequencies__of_Plate", pn,"Replicate_",rn,".txt",sep=""), as.data.frame(histos.nz), na="NA",sep="\t") # Write file using fwrite
          }
          
        } # - End of Main-loop.
        
      }) # - Progress bar.
      
      # -- Assembly of 'AllDataTable.txt':
      
      # - Generate void data.frame.
      cl = Nobi + 7 
      
      # - Generate void data.frame with NAs. 
      void <- data.frame(matrix(ncol = cl, nrow = Nowp))
      
      progresswert = 1/Nopl
      withProgress(message = 'SOPRA 2 of 4.Bin. table ', value = 0, { # Progress bar
        
        # - Rearrange bin.table with new indexation.
        bin.table.new=list()
        for (s in 1 : Nopl){ 
          incProgress(progresswert, detail = paste("part", s, "of", Nopl)) # Progress bar
          for( k in 1: Nopr){
            
            # - Generate index of lookup table.
            index.bin.table =  subset(lookup, lookup$plate == s & lookup$replicate == k)$index_l
            
            # - Create necessary index lind for bin.table.new.
            lind= k + Nopr*(s-1) 
            
            # - If index=0 then void data.frame=NAs.    
            if (length(index.bin.table) != 0) { bin.table.new[[lind]] = bin.table[[index.bin.table]]}   
            else {    
              # - Replace replicate with real replicate.
              sub=paste(colnames(da), collapse=" ")
              sub1 = gsub("rep_(.)", paste("rep_",k, sep=""), sub) 
              colnames(void) = unlist(strsplit(sub1, " "))
              # - Fill describing columns.
              void$RNA.ID=RNA.ID
              void$Plate = rep(s,nrow(PlateConfSub) )
              void$Well =  rep(1:nrow(PlateConfSub) )
              void$Replicate = rep(k,nrow(PlateConfSub) )
              # - Assignment.
              bin.table.new[[lind]]= void      
            } # - End if.
          }   # - End Inner-loop.
        }     # - End Outer-loop and end of rearrangement.
      }) # - Progress bar
      
      # -- Creating  AllDataTable:
      
      AllDataTable  = list() 
      AllDataList  = list() 
      
      # - Concatenate bin.tables which later will be vertically rbinded.
      for( r in 1:Nopl){  
        # - Start of horizontal index. 
        start = 2 + Nopr * (r-1)  
        # - End of horizontal index.
        end   = Nopr * r          
        # - Index of first dataframe to which other are cbinded.
        ei    = 1 + Nopr * (r-1) 
        # - List of horizontally concatenated bin.tables.
        bin.hori = bin.table.new[[ei]]      
        for (rr in start: end) {
          # - Concatenate horizontally.
          bin.hori = cbind( bin.hori, bin.table.new[[rr]]) 
        }   
        AllDataList[[r]] =  bin.hori 
      }
      
      #- Concatenate list vertically. 
      AllDataTable = do.call("rbind", AllDataList) 
      
      # - Save 'AllDataTable'.  

      fwrite(file=paste("AllDataTable.txt", sep=""), AllDataTable, na="NA",sep="\t") # Write file using fwrite
      
      
      # -- The file 'binning_template.txt' is generated and saved:
      
      # - Initialization.
      
      proc = t(floor(h$mids)) # Save binvalues (h$mids) in proc.
      binning.temp <- c(47) # these numbers are just symbolic
      setwd(Path_to_Output_Data)
      binning.file = paste(proc[1,],sep="", collapse=",")
      Path_to_Input_Data <<- Path_to_Output_Data
      
      cat("\n\nEnd: SOPRA 2 of 4")
      
    }else{
      
      cat("\n\n No: SOPRA 2 of 4")
  
         } # --- END of SOPRA 2of4.
    
    
    # --- This is SOPRA 3 of 4:
    
    if(input$dataanalysis=="TRUE") {
      
      cat("\n\nStart: SOPRA 3 of 4")
      
      # - Read platelist and feature of interest.
      
      PlateList = PlateList()
      Feature=PlateList$Feature 
      firstfeature=Feature[1] # Takes the first feature out of the feature list
      
      # - Define AllDataTable  file
      AllDataTable_name  = c("AllDataTable.txt")
      
      # - Create filename with path file 
      InputFile = paste(Path_to_Output_Data, AllDataTable_name , sep=.Platform$file.sep)
      
      if (file.exists(InputFile) ==0){ #Check if filename already exists
        showModal(modalDialog(
          title = "Error message.SOPRA 3 of 4.",
          "...can not find 'AllDataTable.txt."
        ))
      }else{
        
        withProgress(message = 'SOPRA 3 of 4.Calculating ', value = 0.25, { # Progress bar
          
          # - Read AllDataTable.txt. 
          tab= read.delim(InputFile, sep="\t",header=TRUE, check.names = F) 
          
          top=tab # top as intermediate variable
          
          tab=tab[tab$Processing == TRUE,] 
          
          # - Extraction of information from 'AllDataTable' to use maSigPro.
          h=colnames(tab)
          
          incProgress(0.5, detail = paste("")) # Progress bar
          
          # - Extract data. 
          ext = tab[grep("^Bin_(.*)_rep",h,perl=T)] 
          b = strsplit(colnames(ext), "_")
          binning.file=vector()
          for (r in 1:length(b)){binning.file[r]=as.numeric(b[[r]][2])}
          binning.file = unique(binning.file)
          b=binning.file
          
          # - Create a list with dataframes for each binnig interval
          d=list(data.frame())
          for(j in 1:length(b)){d[[j]]=tab[grep(paste("_",b[j],"(_)", sep=""),h,perl=T )]} 
          
          # - Concatenate the dataframes from the list using cbind-function
          tub = cbind.data.frame(d) 
          h = colnames(tub) 
          
          # - Extract data. 
          ext = tub[grep("^Bin_(.*)_rep",h,perl=T)]   # variable ext contains extracted values 
          
          # - Extract RNA.ID. 
          rna.ID=as.character(tab$RNA.ID)
          
          # - Extract x-axtime from header columns.
          T = strsplit(colnames(ext), "_")
          Time=vector()
          for (r in 1:length(T)){Time[r]=as.numeric(T[[r]][2])}
          
          # - Extract replicates from header columns
          R = strsplit(colnames(ext), "_") # extract replicate
          Replicate = vector()
          for (r in 1:length(R)) { Replicate[r]=as.numeric( R[[r]][4])}  
          
          # - Generation of dummy variables
          Ind=rep(0,length(Time))
          Ind[1:dim(ext)[2]] <-1
          Hpy=Ind
          
          incProgress(0.75, detail = paste("")) # Progress bar
          
          # - Edesign 
          edesign=cbind(as.integer(Time), as.integer(Replicate), as.integer(Ind))
          colnames(edesign)=c("Time","Replicate","Bin")  # The last column Bin is the dummy variable
          rownames(edesign)=colnames(ext)
          
          # - Sort replicate in such a way that each time point has a unique number
          re=rep(c(1:length(b)),each=max(unique(Replicate)))
          edesign[,2] <- re
          
          # - Assignment , i.e.only new names. 
          edesign.rna = edesign
          
          # - Extension of edesign.rna and data.rna with rownames and colnames 
          data.rna= ext
          rownames(edesign.rna) <- paste(colnames(data.rna))
          rownames(data.rna) <- paste(rna.ID)  # assign extended unique RNAi symbols to rna.DATA as rownames
          
          
          incProgress(1, detail = paste("")) # Progress bar
        }) # Progress bar end
        
        # -- End of preparing data.
        
        # -- Library used.
        withProgress(message = '!!!CHECK R-STUDIO!!!! ', value = 0.25, { # Progress bar
       
        }) # Progress bar.
        
        cat("\n Start: maSigPro")
        withProgress(message = 'SOPRA 3 of 4.maSigPro ', value = 0.25, { # Progress bar
          
          # Define experimental design matrix.
          degreevalue = as.numeric(input$degree)
          edesign <- make.design.matrix(edesign.rna, degree = degreevalue)  
          
          Qvalue = as.numeric(input$FDR)
          
          fit <- p.vector(data.rna, edesign, Q=Qvalue, counts = F, MT.adjust = "BH")
          
          # - Shiny Output.
          output$Edesign <- renderTable({ 
            return(edesign.rna) # output of a part of Edesign
          })
          
          # - Best regression model for each histogram profile of siRNA  using stepwise regression.
          alfavalue = as.numeric(input$alpha)
          
          tstep <- T.fit(fit, nvar.correction = FALSE, step.method = "backward", alfa= alfavalue)
          filenametstep = paste(Path_to_Output_Data, "/All_p-rsq_values_after_function_tfit_wDegree",degreevalue,"Q",Qvalue,"_walfa",alfavalue,".txt", sep="")
          
          write.table(file=filenametstep,tstep$sol[,1:2], sep="\t",  col.names =NA)
          
          incProgress(0.75, detail = paste("")) # Progress bar
          cat("\n Done: T.fit Export.")
          
          
          # - Extract significant histogram profile.
          rsqvalue = as.numeric(input$RSQ)
          sigs <- get.siggenes (tstep, vars = "groups", rsq = rsqvalue) 
          
          cat("\n\nEnd: SOPRA 3 of 4")
          
          
        }) # Progress bar
      } # End of check if file AllDataTable exists.
      
      
      
      
      # -- This is SOPRA 3 of 4Clustering: 
      
      # -- Define function PlotProfiles.SOPRA: # CAN THIS BE DELETED?
      
      
      PlotProfiles.SOPRA <- function (data, cond, main = NULL, cex.xaxis = 0.5, ylim = NULL, 
                                      repvect, sub = NULL, color.mode = "rainbow") 
      {
        pos.vline <- repvect[1:length(repvect)] - c(0, repvect[1:(length(repvect) - 
                                                                    1)])
        for (i in 1:length(pos.vline)) {
          if (pos.vline[i] != 0) 
            pos.vline[i] <- i
          else pos.vline[i] <- pos.vline[i - 1]
        }
        if (is.null(ylim)) {
          ylim = c(min(as.matrix(data), na.rm = TRUE) * 1.1, max(as.matrix(data), 
                                                                 na.rm = TRUE) * 1.1)
        }
        if (!is.vector(data)) {
          n = dim(data)[2]
          m = dim(data)[1]
          if (m == 1) 
            nom <- rownames(data)
          else nom <- NULL
          plot(x = c(1:n), y = data[1, ], type = "l", col = 1, 
               ylim = ylim, ylab = "normalized relative frequency", xlab = " ", 
               main = paste("Cluster ", main, "(", m, " genes )", 
                            nom), xaxt = "n")
          axis(1, at = 1:n, labels = substr(cond, 1, 26), cex.axis = cex.xaxis, 
               las = 2)
          if (color.mode == "rainbow") {
            abline(v = pos.vline, col = "light gray")
            for (i in 1:dim(data)[1]) {
              lines(x = c(1:n), y = data[i, ], col = i)
            }
          }
          else if (color.mode == "gray") {
            for (i in 1:dim(data)[1]) {
              lines(x = c(1:n), y = data[i, ], col = "light gray")
            }
            yy <- apply(as.matrix(data), 2, median, na.rm = TRUE)
            lines(x = c(1:n), y = yy, col = "black")
          }
          else stop("Invalid mode, must be one of rainbow, gray")
        }
        else {
          n = length(data)
          plot(x = c(1:n), y = data, type = "l", col = 1, ylim = ylim, 
               ylab = "normalized relative frequency", sub, xaxt = "n", xlab = " ")
          axis(1, at = 1:n, labels = cond, cex.axis = cex.xaxis, 
               las = 2)
          abline(v = pos.vline, col = "light gray")
        }
      }
      
      
      # --  Define function PlotGroups.SOPRA:
      
      PlotGroups.SOPRA <-function (data, edesign = NULL, time = edesign[, 1], groups = edesign[, 
                                                                                               c(3:ncol(edesign))], repvect = edesign[, 2], show.fit = FALSE, 
                                   dis = NULL, step.method = "backward", min.obs = 2, alfa = 0.05, 
                                   nvar.correction = FALSE, summary.mode = "median", show.lines = TRUE, 
                                   groups.vector = NULL, xlab = firstfunction, ylab = "normalized relative frequency", 
                                   cex.xaxis = 1, ylim = NULL, main = NULL, cexlab = 0.8, legend = TRUE, 
                                   sub = NULL) 
      {
        if (!is.vector(data)) {
          if (summary.mode == "representative") {
            distances <- apply(as.matrix(dist(data, diag = TRUE, 
                                              upper = TRUE)), 1, sum)
            representative <- names(distances)[distances == min(distances)]
            yy <- as.numeric(data[rownames(data) == representative, 
                                  ])
            sub <- paste("Representative:", representative)
          }
          else if (summary.mode == "median") {
            yy <- apply(as.matrix(data), 2, median, na.rm = TRUE)
            sub <- paste("Median profile of ", nrow(data), " genes")
          }
          else stop("not valid summary.mode")
          if (dim(data)[1] == 1) {
            sub <- rownames(data)
          }
        }
        else if (length(data) != 0) {
          yy <- as.numeric(data)
          sub <- rownames(data)
        }
        else stop("empty data")
        if (is.null(ncol(groups))) {
          ncol = 1
          legend = FALSE
          codeg = "group"
        }
        else {
          ncol = ncol(groups)
          codeg <- as.character(colnames(groups))
        }
        reps <- i.rank(repvect)
        y <- vector(mode = "numeric", length = length(unique(reps)))
        x <- vector(mode = "numeric", length = length(unique(reps)))
        g <- matrix(nrow = length(unique(reps)), ncol = ncol)
        for (k in 1:length(y)) {
          y[k] <- mean(yy[reps == k], na.rm = TRUE)
          x[k] <- mean(time[reps == k])
          for (j in 1:ncol) {
            g[k, j] <- mean(groups[reps == k, j])
          }
        }
        if (is.null(ylim)) 
          ylim = c(min(as.numeric(yy), na.rm = TRUE), max(as.numeric(yy), 
                                                          na.rm = TRUE))
        abcissa <- x
        xlim = c(min(abcissa, na.rm = TRUE), max(abcissa, na.rm = TRUE) * 
                   1.3)
        color1 <- as.numeric(sort(factor(colnames(groups)))) + 1
        color2 <- groups
        for (j in 1:ncol) {
          color2[, j] <- color2[, j] * j
        }
        color2 <- as.vector(apply(color2, 1, sum) + 1)
        plot(x = time, y = yy, pch = 21, xlab = xlab, ylab = "normalized relative frequency", 
             xaxt = "n", main = main, sub = sub, ylim = ylim, xlim = xlim, 
             cex = cexlab, col = color2)
        axis(1, at = unique(abcissa), labels = unique(abcissa), cex.axis = cex.xaxis)
        if (show.fit) {
          rm <- matrix(yy, nrow = 1, ncol = length(yy))
          rownames(rm) <- c("ratio medio")
          colnames(rm) <- rownames(dis)
          fit.y <- T.fit(rm, design = dis, step.method = step.method, 
                         min.obs = min.obs, alfa = alfa, nvar.correction = nvar.correction)
          betas <- fit.y$coefficients
        }
        for (i in 1:ncol(groups)) {
          group <- g[, i]
          if ((show.fit) && !is.null(betas)) {
            li <- c(2:6)
            a <- reg.coeffs(coefficients = betas, groups.vector = groups.vector, 
                            group = colnames(groups)[i])
            a <- c(a, rep(0, (7 - length(a))))
            curve(a[1] + a[2] * x + a[3] * (x^2) + a[4] * (x^3) + 
                    a[5] * (x^4) + a[6] * (x^5) + a[7] * (x^5), from = min(time), 
                  to = max(time), col = color1[i], add = TRUE, 
                  lty = li[i])
          }
          if (show.lines) {
            lx <- abcissa[group != 0]
            ly <- y[group != 0]
            ord <- order(lx)
            lxo <- lx[ord]
            lyo <- ly[ord]
            lines(lxo, lyo, col = color1[i])
          }
        }
        op <- par(bg = "white")
        if (legend) 
          legend(max(abcissa, na.rm = TRUE) * 1.02, ylim[1], legend = codeg, 
                 text.col = color1, col = color1, cex = cexlab, lty = 1, 
                 yjust = 0)
        par(op)
      }
      
      
      
      
      
      
      # -- Define function See.genes.SOPRA:  
      
      see.genes.SOPRA <-function (data, edesign = data$edesign, time.col = 1, repl.col = 2, 
                                  group.cols = c(3:ncol(edesign)), names.groups = colnames(edesign)[3:ncol(edesign)], 
                                  cluster.data = 1, groups.vector = data$groups.vector, k = 9, 
                                  k.mclust = FALSE, cluster.method = "hclust", distance = "cor", 
                                  agglo.method = "ward.D", show.fit = FALSE, dis = NULL, step.method = "backward", 
                                  min.obs = 1, alfa = 0.05, nvar.correction = FALSE, show.lines = TRUE, 
                                  iter.max = 500, summary.mode = "median", color.mode = "rainbow", 
                                  cexlab = 1, legend = TRUE, newX11 = TRUE, ylim = NULL, main = NULL, ...) 
      {
        time = edesign[, time.col]
        repvect = edesign[, repl.col]
        groups = edesign[, group.cols]
        narrays <- length(time)
        if (!is.null(dim(data))) {
          dat <- as.data.frame(data)
          clusterdata <- data
        }
        else {
          clusterdata <- data[[cluster.data]]
          dat <- as.data.frame(data$sig.profiles)
        }
        if (nrow(dat) > 1) {
          dat <- as.data.frame(dat[, (ncol(dat) - length(time) + 
                                        1):ncol(dat)])
          count.noNa <- function(x) (length(x) - length(x[is.na(x)]))
          dat <- dat[which(apply(as.matrix(dat), 1, count.noNa) >= 
                             length(unique(repvect))), ]
          clusterdata <- dat
          if (any(is.na(clusterdata))) {
            if (cluster.method == "kmeans" || cluster.method == 
                "Mclust") {
              if (all(cluster.data != 1, cluster.data != "sig.profiles")) {
                clusterdata[is.na(clusterdata)] <- 0
              }
              else {
                mean.replic <- function(x) {
                  tapply(as.numeric(x), repvect, mean, na.rm = TRUE)
                }
                MR <- t(apply(clusterdata, 1, mean.replic))
                if (any(is.na(MR))) {
                  row.mean <- t(apply(MR, 1, mean, na.rm = TRUE))
                  MRR <- matrix(row.mean, nrow(MR), ncol(MR))
                  MR[is.na(MR)] <- MRR[is.na(MR)]
                }
                data.noNA <- matrix(NA, nrow(clusterdata), 
                                    ncol(clusterdata))
                u.repvect <- unique(repvect)
                for (i in 1:nrow(clusterdata)) {
                  for (j in 1:length(u.repvect)) {
                    data.noNA[i, repvect == u.repvect[j]] = MR[i, 
                                                               u.repvect[j]]
                  }
                }
                clusterdata <- data.noNA
              }
            }
          }
          if (!is.null(clusterdata)) {
            k <- min(k, nrow(dat), na.rm = TRUE)
            if (cluster.method == "hclust") {
              if (distance == "cor") {
                
                dcorrel <- matrix(rep(1, nrow(clusterdata)^2), 
                                  nrow(clusterdata), nrow(clusterdata)) - cor(t(clusterdata), use = "pairwise.complete.obs")
                
                clust <- hclust(as.dist(dcorrel), method = agglo.method)
                c.algo.used = paste(cluster.method, "cor", 
                                    agglo.method, sep = "_")
              }
              else {
                clust <- hclust(dist(clusterdata, method = distance), 
                                method = agglo.method)
                c.algo.used = paste(cluster.method, distance, 
                                    agglo.method, sep = "_")
              }
              cut <- cutree(clust, k = k)
            }
            else if (cluster.method == "kmeans") {
              cut <- kmeans(clusterdata, k, iter.max)$cluster
              c.algo.used = paste("kmeans", k, iter.max, sep = "_")
            }
            else if (cluster.method == "Mclust") {
              if (k.mclust) {
                my.mclust <- Mclust(clusterdata)
                k = my.mclust$G
              }
              else {
                my.mclust <- Mclust(clusterdata, k)
              }
              cut <- my.mclust$class
              c.algo.used = paste("Mclust", k, sep = "_")
            }
            else stop("Invalid cluster algorithm")
            if (newX11) 
              X11()
            groups <- as.matrix(groups)
            colnames(groups) <- names.groups
            if (k <= 4) 
              par(mfrow = c(2, 2))
            else if (k <= 6) 
              par(mfrow = c(3, 2))
            else if (k > 6) 
              par(mfrow = c(3, 3))
            for (i in 1:(k)) {
              PlotProfiles.SOPRA(data = dat[cut == i, ], repvect = repvect, 
                                 main = i, ylim = ylim, color.mode = color.mode,
                                 #xlab = firstfeature, ylab="normalized relative frequency",
                                 cond = rownames(edesign))
            }
            if (newX11) 
              X11()
            if (k <= 4) {
              par(mfrow = c(2, 2))
              cexlab = 0.6
            }
            else if (k <= 6) {
              par(mfrow = c(3, 2))
              cexlab = 0.6
            }
            else if (k > 6) {
              par(mfrow = c(3, 3))
              cexlab = 0.35
            }
            for (j in 1:(k)) {
              #PlotGroups(data = dat[cut == j, ], show.fit = show.fit,
              PlotGroups.SOPRA(data = dat[cut == j, ], show.fit = show.fit,
                               dis = dis, step.method = step.method, min.obs = min.obs, 
                               alfa = alfa, nvar.correction = nvar.correction, 
                               show.lines = show.lines, time = time, groups = groups, 
                               repvect = repvect, summary.mode = summary.mode, 
                               #xlab = "feature", main = paste("Cluster", j, sep = " "), 
                               #xlab = firstfeature, main = paste("Cluster", j, sep = " "), 
                               xlab = firstfeature, ylab="normalized relative frequency", main = paste("Cluster", j, sep = " "), 
                               ylim = ylim, cexlab = cexlab, legend = legend, 
                               groups.vector = groups.vector)
            }
          }
          else {
            print("warning: impossible to compute hierarchical clustering")
            c.algo.used <- NULL
            cut <- 1
          }
        }
        else if (nrow(dat) == 1) {
          if (newX11) 
            X11()
          PlotProfiles.SOPRA(data = dat, repvect = repvect, main = NULL, 
                             #ylim = ylim, color.mode = color.mode, cond = rownames(edesign))
                             #xlab = firstfeature, ylab="normalized relative frequency",
                             ylim = ylim, color.mode = color.mode, cond = rownames(edesign))
          if (newX11) 
            X11()
         
          PlotGroups.SOPRA(data = dat, show.fit = show.fit, dis = dis,
                           step.method = step.method, min.obs = min.obs, alfa = alfa, 
                           nvar.correction = nvar.correction, show.lines = show.lines, 
                           time = time, groups = groups, repvect = repvect, 
                           #summary.mode = summary.mode, 
                           #xlab = "time", main = main,
                           # summary.mode = summary.mode,
                           summary.mode = "median", 
                           xlab = firstfeature, 
                           ylab="normalized relative frequency", 
                           main = main,
                           ylim = ylim,
                           cexlab = cexlab, 
                           legend = legend, 
                           groups.vector = groups.vector)
          c.algo.used <- NULL
          cut <- 1
        }
        else {
          print("warning: NULL data. No visualization possible")
          c.algo.used <- NULL
          cut <- NULL
        }
        OUTPUT <- list(cut, c.algo.used, groups)
        names(OUTPUT) <- c("cut", "cluster.algorithm.used", "groups")
        OUTPUT
      }
      
      
      # -- Get input values.:
      
      # - Input Clustermethod.
      if (input$clustermethod == 1){
        clustermethodvalue = c("hclust")
      }else{
        if (input$clustermethod == 2){
          clustermethodvalue = c("kmeans")
        }
        if (input$clustermethod == 3){
          clustermethodvalue =("Mclust")
        }}
      
      # - Input Stepmethod.
      if (input$stepmethod == 1){
        stepmethodvalue =c("two.steps.backward")
      }else{
        if (input$stepmethod == 2){
          stepmethodvalue =c("backward")
        }else{
          if (input$stepmethod == 3){
            stepmethodvalue =c("forward")
          }else{
            if (input$stepmethod == 4){
              stepmethodvalue =c("two.steps.forward")
            }
          }
        }
      }
      
      # - Input Distmethod.
      if (input$distmethod == 1){
        distmethodvalue =c("cor")
      }else{
        if (input$distmethod == 2){
          distmethodvalue =c("euclidean")
        }
      }
      
      
      # -  Input hclustmethod. 
      if (input$hclustmethod == 1){
        hclustmethodvalue =c("ward.D")
      }else{
        if (input$hclustmethod == 2){
          hclustmethodvalue =c("ward.D2")
        }else{
          if (input$hclustmethod == 3){
            hclustmethodvalue =c("single")
          }else{
            if (input$hclustmethod == 4){
              hclustmethodvalue =c("complete")
            }else{
              if (input$hclustmethod == 5){
                hclustmethodvalue =c("average")
              }else{
                if (input$hclustmethod == 6){
                  hclustmethodvalue =c("mcquitty")
                }else{
                  if (input$hclustmethod == 7){
                    hclustmethodvalue =c("median")
                  }else{
                    if (input$hclustmethod == 8){
                      hclustmethodvalue =c("centroid")
                      
                    }
                  }
                }
              }
            }
          }
        }
      }
      
      
      
      # - Input NVAR cor. 
      if (input$nvar.cor == 1){
        nvar.corvalue = c("TRUE")
      }else{
        if (input$nvar.cor == 2){
          nvar.corvalue = c("FALSE")
        }
      }
      
      # --- Start Clustering: 
      
      cat("\n\n Start: Clustering Export\n")
      
      rsqvalue = as.numeric(input$RSQ)   # remove RSQ line later?
      alfavalue = as.numeric(input$alpha)
      Qvalue = as.numeric(input$FDR)
      degreevalue = as.numeric(input$degree)
      clustervalue = as.numeric (input$cluster)
      itervalue = as.numeric(input$maxiterations)
 
      
      filenameclusterimage = paste(Path_to_Output_Data, "/Clustered_Sign_Samples_wd",degreevalue,"_Q",Qvalue,
                                   "_a",alfavalue,"_rsq",rsqvalue,"_c",clustervalue,"_", clustermethodvalue, 
                                   stepmethodvalue, distmethodvalue,
                                   format(Sys.time(),'_%Y_%m_%d_%H_%M_%S'),".pdf",sep="") #kp mit Sys.time stamp
      
   
      # output as PDF
      
      pdf(filenameclusterimage)
      
      # - Calculate ylow and y high for global coordinates.
      ylow= min( apply(sigs$sig.genes$Bin$sig.profiles, 1, min, na.rm=T))
      yhigh= max( apply(sigs$sig.genes$Bin$sig.profiles, 1, max, na.rm=T))
      
      
     
      
      cluster = see.genes.SOPRA(sigs$sig.genes$Bin, newX11=FALSE,
                                # color.mode = plotcolorvalue, 
                                k= clustervalue, 
                                step.method= stepmethodvalue,
                                agglo.method = hclustmethodvalue,
                                cluster.method = clustermethodvalue, 
                                alfa=alfavalue,
                                summary.mode="median", 
                                ylim=c(ylow,yhigh),
                                legend=F,
                                iter.max = itervalue, 
                                distance= distmethodvalue,
                                nvar.correction = nvar.corvalue,
                                min.obs = 1)
      

      cat("\n Done: Clustering")
      
      
      # - Export.
      # cluster=image
      
      XportSignsiRNAs<-cbind(sigs$summary,cluster$cut)
      colnames(XportSignsiRNAs)<-c("siRNA","Cluster")
      rownames(XportSignsiRNAs)=XportSignsiRNAs$siRNA
      
      out.sig.siRNAs.table <<- XportSignsiRNAs  # kp: for output of table and make global variable
      
      XportSignsiRNAs<-XportSignsiRNAs[-1]
      
      filenamesisigncluster = paste(Path_to_Output_Data, "/All_significant_siRNA_in_which_cluster__wd",degreevalue,"_Q",Qvalue,"_alfa",alfavalue,"_rsq",rsqvalue,"_cluster",clustervalue,"_",clustermethodvalue,stepmethodvalue,distmethodvalue,".txt", sep="")
      
      write.table(file=filenamesisigncluster, XportSignsiRNAs, sep="\t",col.names =NA)
      
      cat("\n Done: Clustering Export")
      dev.off()
   
      
      #######################################################################      
      
      #output of table of sig. siRNAs using renderTable 
      
      output$sig_siRNA <- renderTable({   
        return( out.sig.siRNAs.table) # output of sig.siRNAs   #kp
      })
      ########################################################################     
      
      # - Shiny Output of clustered profile images
      output$image3 <- renderPlot({
        see.genes.SOPRA(sigs$sig.genes$Bin, 
                        newX11=FALSE,
                        #color.mode = plotcolorvalue, 
                        k= clustervalue, 
                        step.method= stepmethodvalue,
                        agglo.method = hclustmethodvalue,
                        cluster.method = clustermethodvalue, 
                        alfa=alfavalue,
                        summary.mode="median", 
                        ylim=c(ylow,yhigh),
                        legend=F,
                        iter.max = itervalue, 
                        distance= distmethodvalue,
                        nvar.correction = nvar.corvalue,
                        min.obs = 1)
      })
      
    } # end of dataanalysis (SOPRA 3 0f 4)
    
    
    # -- This is Sopra 4 of 4:
  
     
    if(input$dataanalysis == "TRUE") { 
     
      
      cat("\n\nStart: SOPRA 4 of 4")  
      
      InputFileName = paste(Path_to_Output_Data, "/All_significant_siRNA_in_which_cluster__wd",
                            degreevalue, "_Q", Qvalue,"_alfa",alfavalue,"_rsq",rsqvalue,"_cluster",
                            clustervalue,"_",clustermethodvalue,stepmethodvalue,distmethodvalue,
                            ".txt", sep="") 
      
      
      if (file.exists(InputFileName) ==0){ #Check if filename already exists
        showModal(modalDialog(
          title = "Error message.SOPRA 4of4.",
          "...can not load Input File 'ALL_significant-SiRNA_in_which_cluster'. Please check that this file is located in the input folder."
        ))
      }else{
        x =  read.table(file=InputFileName, sep="\t", header=FALSE,skip=1,na.strings = "NaN",stringsAsFactors = F,comment.char="")
        a=strsplit(as.character(x$V1),"_" ) # split data at first underscore 
        
        aa = vector()
        for (j in seq(along=a)) { aa[j]=a[[j]][1]}
        
        # - Split data at first #. 
        bb = strsplit(as.character(aa),"#" ) 
        
        # - Extract human gene names found.
        genes_found =vector()
        
        for (j in seq(along=a)) { genes_found[j]=bb[[j]][1]}
        
        # - Combine siRNA, cluster,genes in a working table wt. 
        wt =cbind(subset(x, x$V1 %in% grep("(.*)", x$V1, value=TRUE)), genes_found)
        
        # - Remove duplicates.
        genes_unique = unique(genes_found) 
        
        # Now the working table wt is processed in order to determine how often (Frequency) a gene occurs in a cluster.  
        # An output file ordered by frequency and with the prefix post_processed is written.
        
        # -- Processing of wt table:
        
        # - Extract filename from.
        tn=strsplit(InputFileName,"/")
        
        # - Create  filename with suffix post_processed. 
        fn = gsub(pattern=".txt", replacement="_post_processed.txt", InputFileName )
        
        
        # - Define header of table.
        Frequency_Table <- data.frame(Gene=character(0),   
                                      Cluster=character(0),  
                                      Frequency=character(0),  
                                      stringsAsFactors=F) 
        
        # - Determine how often  a gene is in a cluster.
        for( j in genes_unique) {
          sub = subset(wt,wt$genes_found %in% grep(glob2rx(j), as.character(wt$genes_found), value=TRUE )) # exact finding 
          
          f=as.data.frame(table(sub$V2))  # for genes
          
          for(l in 1:dim(f)[1]){    
            Frequency_Table[nrow(Frequency_Table)+1, ] <- c(j,as.character(f$Var1[l]),as.character(f$Freq[l]))}     
        }
        
        # - Order by Frequency.
        ordered.by.Freq = Frequency_Table[order(as.numeric(Frequency_Table$Frequency),decreasing = TRUE), ]
        
        write.table(ordered.by.Freq,file = fn,row.names=F,col.names=TRUE , sep="\t",quote=F)
        
        cat("\n\nEnd: SOPRA 4of4")
        cat("\n\nOutput-File:\n",fn) 
        
        
        
        #####################################################################   
        # shiny output  of sig. genes with frequency and cluster as table#
        
        output$sig_genes <- renderTable({
          return(ordered.by.Freq) # output of sig. genes with frequency and cluster #kp
        })
        
        ## -- WordCloud:
        
        #####################################################################   
        # shiny output  of wordcloud as image #
        
        
        wc <- reactive({
          
          dataWC<-data.frame(ordered.by.Freq$Gene,as.numeric(ordered.by.Freq$Frequency))
          colnames(dataWC)<-c("word","freq")
          set.seed(32)
          wordcloud(dataWC$word,dataWC$freq,min.freq=1,scale=c(4,0.5),max.words=100,random.order=FALSE,colors=brewer.pal(8, "Dark2"))
        })
        
        
        output$plot1 <- renderPlot({
          wc()
        })  # end reactive
        
      } # End of Check if filename already exists
      
      
    } # end of SOPRA 4 of 4. 
      #  SOPRA 4 of 4 is always running together with SOPRA3 of 4
    

    
 ######################################################   
  
    showModal(modalDialog(
      title = "Analysis finished.",
      HTML("The current analysis has been finished. You can find all resulting files in the selected output folder you have chosen.  
      You can end the script or choose different conditions to re-run the analysis.<br>   
      Please note : <br> 
      For setting different papameters in the SOPRA 2 of 4 step you have to uncheck SOPRA 1 of 4. <br>
      For setting different parameters in the SOPRA 3 of 4 step you have to uncheck SOPRA 1 of 4 and SOPRA 2 of 4.<br>
      Otherwise the analysis re-runs from the very beginning!! ")
    ))
    
    
    
    
    
    
    ### measurement  of time elapsed and processing time per plate  is obsolete #######
   
   #end_time <- Sys.time()
   #cat("\n\n=== Total time elapsed ===\n")
   #print(end_time - start_time)
   #cat("\nProcessing time per plate = ", (end_time - start_time)/TNopl, "mins." )  
   
    ##################################################################
    
    
    
      } # end of observe event
 
 
 
    )  # --- END GO Button. 

 
    
  #--- STOP:
  observeEvent(
    input$STOP,{ stopApp(returnValue = invisible())
    })
  
  
  }    # --- END Server.
