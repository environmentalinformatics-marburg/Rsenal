#' Read climate station data from "Climate Data Online"
#' 
#' @param file The name of the file the data are to be read from.
#' @param data The name of the data source downloaded at http://gis.ncdc.noaa.gov/map/viewer/. Currently Hourly Global ("HourlyGlobal"), Global summary of the day ("GSOD") and Global Historical Climatology Network-Daily "GHCN" are supported
#' @param advanced logical. Indicated whether advanced download option for "HourlyGlobal" was used or not.
#' @param sep The field separator character. Note that the separation character of the the header might differ in some cases which however can be ignored since the function will account on this.
#' 
#' @return
#' A data frame containing a representation of the data in the file.
#' 
#' @note
#' Space separated data are only supported with the options data="HourlyGlobal" and advanced==FALSE.
#' 
#' @author
#' Hanna Meyer
#' 
#' @seealso
#' \code{\link{read.table}}
#' 
#' @examples
#' \dontrun{
#' #### Example 1: Read a table of hourly global data downloaded using advanced download option
#' 
#' file=system.file("ClimateStationData.txt", package="Rsenal")
#' readCDO(file)
#' }
#' 
#' @aliases readCDO
#' @export readCDO

readCDO <- function(file="",data="HourlyGlobal",advanced=TRUE,sep=","){
  ###########
  if(data=="GSOD"||data=="GHCN"&sep==","){
    result <- read.csv(file,header=TRUE)
  }
  ############
  else if(data=="GSOD"||data=="GHCN"||(data=="HourlyGlobal"&advanced==TRUE)&sep==" "){
    stop ("format is not supported. Please download data in comma separated format")
  }  
  ##########
  else if (data=="HourlyGlobal"&advanced==FALSE&sep==","){
    result <- read.csv(file,header=TRUE)
  }
  else if (data=="HourlyGlobal"&advanced==FALSE&sep==" "){
    data <- readLines(file,n=-1) 
    header <- data[1]
    data <- data[-1]
    for (i in 1:length(data)){ 
      while (grepl(pattern="  ",data[i])) {#subcategory contains " "
        data[i]=gsub(pattern="  "," ",data[i])
      }
    }
      while (grepl(pattern="  ",header)) {
        header <- gsub(pattern="  "," ",header)
      }
    acc <- 1
    while(substr(header,0,acc)==" "){
      acc <- acc+1
      header <- substr(header,acc,nchar(header))
    }
    data <- gsub(pattern=" ",",",data)
    data <- strsplit(data, ",")
    header <- gsub(pattern=" ",",",header)
    header <- strsplit(header, ",")
    data <- do.call(rbind, data)
    colnames(data) <- header[[1]]
    result <- data
    print ("Warning: Class determination might be wrong reading a file with the specified options. Consider that some columns should now be converted to e.g numeric!")
  }
  ##########
  else if (data=="HourlyGlobal"&advanced&sep==",")
  { 
    category <- readLines(file,n=1) 
    subcategory <- readLines(file,n=2) 
    subcategory <- subcategory[2]
    #################################################
    ###Assign the category for each subcategory
    #################################################
    #split categories
    CSplit <- strsplit(category,"")[[1]] #split at every character
    CSplitRecl <- c()
    counter <- 1
    for (i in 1:(length(CSplit))){#merge words plus blanks
      if (i==length(CSplit)){
        CSplitRecl <- c(CSplitRecl,paste(CSplit[counter: i],collapse=""))    
      }  
      if (i<length(CSplit)&CSplit[i]==" "&CSplit[i+1]!=" "){
        CSplitRecl <- c(CSplitRecl,paste(CSplit[counter: i],collapse=""))
        counter <- i+1
      }
    }
    ##Calculate length of characters of the categories
    CLength <- c()
    for (i in 1:length(CSplitRecl)){
      CLength[i] <- sum(nchar(CSplitRecl[1:i])) 
    }
    #format Category Names
    while (grepl(pattern="  ",category)) {
      category <- gsub(pattern="  "," ",category)
    }
    category <- gsub(pattern=" ",",",category)
    category <- strsplit(category, ",")
    category <- category[[1]]
    #################################################################
    ###split subcategory
    SCSplit <- strsplit(subcategory,"")[[1]]
    SCSplitRecl <- c()
    counter <- 1
    for (i in 1:(length(SCSplit))){
      if (i==length(SCSplit)){
        SCSplitRecl <- c(SCSplitRecl,paste(SCSplit[counter: i],collapse=""))    
      }
      if (i<length(SCSplit)&SCSplit[i]==" "&SCSplit[i+1]!=" "){
        SCSplitRecl <- c(SCSplitRecl,paste(SCSplit[counter: i],collapse=""))
        counter <- i+1
      }
    }
    #Calculate length of characters of the subcategory
    SCLength <- c()
    for (i in 1:length(SCSplitRecl)){
      SCLength[i] <- sum(nchar(SCSplitRecl[1:i])) 
    }
    ###Assign the category for each subcategory
    acc <- 1
    HeaderCategory <- ""
    for (i in SCLength[-length(SCLength)]){
      HeaderCategory[acc] <- category[which(CLength==min(CLength[i<=CLength]))]
      acc <- acc+1
    }
    HeaderCategory <- c(HeaderCategory,category[length(category)])
    ######################################################
    ######################################################
    #format subcategory names
    while (grepl(pattern="  ",subcategory)) {
      subcategory <- gsub(pattern="  "," ",subcategory)
    }
    subcategory <- gsub(pattern=" ",",",subcategory)
    subcategory <- strsplit(subcategory, ",")
    subcategory <- subcategory[[1]]
    ########################################################
    result <- read.csv(file,skip=2,blank.lines.skip=TRUE,header=FALSE)
    result <- result[,-ncol(result)]
    colnames(result) <- paste(HeaderCategory,"_",subcategory,sep="")
    #Add index to duplicated category names
    for (i in 1: length(names(result))){
      if (sum(names(result)==names(result)[i])>1){
        match <- names(result)==names(result)[i]
        names(result)[which(match)] <- paste(names(result)[match],"_",1:length(names(result)[match]),sep="")                                               
      }
    }
  }
  else stop ("Format was not correctly spcified or is not supported")
  return (result)
}