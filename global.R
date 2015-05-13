### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

Sys.setenv(TZ="America/Los_Angeles")

#install missing packages.
list.of.packages <- c("shiny","dygraphs","reshape2","plyr","lubridate","data.table","httr","xts","rvest")
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))(print(paste("The following packages are not installed: ", new.packages, sep="")))else(print("All packages installed"))
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

#global functions
alt.diff <- function (x, n = 1, na.pad = TRUE) {
  NAs <- NULL
  if (na.pad) {NAs <- rep(NA, n)}
  diffs <- c(NAs, diff(x, n))
}

round.minutes <- function(x, noOfMinutes=5){
	tz <- tz(x[1])
	class <- class(x[1])
	structure((noOfMinutes*60) * (as.numeric(x + (noOfMinutes*60*0.5)) %/% (noOfMinutes*60)), class=class,tz=tz)
}

read.sum <- function(x, fileName, saveFile=F, tzone="America/Los_Angeles", dataTable=T, makeLong=F, roundTime=T){
	fileCheck <- file.info(x)$size>0
	if(fileCheck){
		#get device type from SUMS header; clean/format
		notes <- head(read.csv(x, header=F),21)[,1]
		notes <- as.character(notes[1:21])
		device <- substring(strsplit(notes[1],":")[[1]][2],2,9)
		if(device=="DS1921G-"){device <- "DS1921G"}
		if(device=="DS1922/D"){device <- "DS1922X"}
	 
		#find line at which to begin import
		startimport <- grep("Value",notes)
	 
		#import SUMs file
		sums <- as.data.table(read.csv(x, skip=startimport, header=F, stringsAsFactors=F))

		hhid.start <- gregexpr(pattern ='KS[0-9]{4}', fileName)[[1]][1]
		hhid.stop <- gregexpr(pattern ='KS[0-9]{4}', fileName)[[1]][1] + 5
		sums[,hhid:=substring(fileName, hhid.start, hhid.stop)]
		device.start <- gregexpr(pattern ='SUM[A-Z0-9]{4,}', fileName)[[1]][1]
		device.stop <- device.start + 6
		sums[,device_id:=substring(fileName, device.start, device.stop)]
		sums[,loc:=strsplit(fileName, '-')[[1]][3]]
		setnames(sums,1:3, c('datetime','unit','temp'))
 
		#correct for fahrenheit files
		tempunit <- unique(as.character(sums$unit))
		if(tempunit==FALSE){sums[, temp:=round((5/9)*(sums$temp - 32),3)]}
		#drop unit
		sums[,unit:=NULL]
		sums[,datetime:=mdy_hms(datetime, tz=tzone)]

		if(roundTime){
			interval <- as.numeric(sums[2, datetime] - sums[1, datetime])
			sums[,datetime:=round.minutes(datetime,interval)]
		}

		if(makeLong){sums <- melt(sums, id.var=c('datetime','hhid','device_id','loc'))}
		if(saveFile & makeLong){write.csv(sums, row.names=F, file=paste(substring(x, 1, nchar(x)-4),'.import.long.csv',sep=''))}
		if(saveFile & !makeLong){write.csv(sums, row.names=F, file=paste(substring(x, 1, nchar(x)-4),'.import.csv',sep=''))}
		if(dataTable){sums} else (as.data.frame(sums))
	}else{warning(paste("File", x, "does not contain valid iButton data", sep=" "))}
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

	read.patsplus <- function(x, saveFile=F, tzone="America/Los_Angeles", dataTable=T, makeLong=F, logDT='logpats'){
		#confirm file contains data
			#read in without regard for delimiters
			raw <- read.delim(x)
			#use a regular expression to identify lines that are data, denote the line number
			kLines <- as.numeric(sapply(raw, function(x) grep('[0-9/0-9/0-9]{2,} [0-9:]{6,},[0-9.,]{3,}',x)))
			#convert to character
			rare <- as.character(raw[kLines,])
			#create a tempfile and write to it
			fn <- tempfile()
			write(rare, file=fn)
			#read in using fread
			mediumwell <- fread(fn)
			#remove cruft
			unlink(fn)
			setnames(mediumwell, c('datetime','V_power','degC_sys','degC_air','RH_air','degC_thermistor','usb_pwr','fanSetting','filterSetting','ref_sigDel','low20','high320'))
			mediumwell[,datetime:=ymd_hms(datetime, tz=tzone)]
			#filename string extraction madness
			hhid.start <- gregexpr(pattern ='[A-C]{1,}[0-9]{2,}',x)[[1]][1]
			hhid.stop <- gregexpr(pattern ='[A-C]{1,}[0-9]{2,}',x)[[1]][1] + 2
			mediumwell[,hhid:=substring(x, hhid.start, hhid.stop)]
			device.start <- gregexpr(pattern ='[A-T]{3,}[0-9]{4}',x)[[1]][1]
			device.stop <- gregexpr(pattern ='[A-T]{3,}[0-9]{4}',x)[[1]][1] + 6
			mediumwell[,device_id:=gsub("PAT","",substring(x, device.start, device.stop))]
			loc.start <- gregexpr("\\-[A-Z]{4}\\-",x, perl=T)[[1]][1] + 1
			loc.stop <- loc.start + 3
			mediumwell[,loc:=tolower(substring(x,loc.start,loc.stop))]
			# mediumwell[,rollingMin:=rollapply(high320, width=60, FUN='quantile', p=0.2)]
			# mediumwell[,rollingMin:=rollapply(high320, width=30, FUN='min', fill=NA)]

			#check for a datatable/frame with log information
			if(exists(logDT)){
				logframe <- try(get(logDT))
				logframe <- as.data.table(logframe)
				log <- logframe[hhid==mediumwell[,unique(hhid)] & loc==mediumwell[,unique(loc)] & device_id==mediumwell[,unique(device_id)]]
				mediumwell[datetime>=log[,zero1.start] & datetime<=log[,zero1.stop], period:='zero1']
				mediumwell[datetime>=log[,zero2.start] & datetime<=log[,zero2.stop], period:='zero2']
				mediumwell[datetime>=log[,sample.start] & datetime<=log[,sample.stop], period:='sample']
			}

			#function flags
			if(makeLong){mediumwell <- melt(mediumwell, id.var=c('datetime','hhid','device_id','loc','period'))}
			if(saveFile & makeLong){write.csv(mediumwell, row.names=F, file=paste(substring(x, 1, nchar(x)-4),'.long.csv',sep=''))}
			if(saveFile & !makeLong){write.csv(mediumwell, row.names=F, file=paste(substring(x, 1, nchar(x)-4),'.csv',sep=''))}
			# if(toMemory){assign(strsplit(x,'/')[[1]][3],mediumwell,envir=.GlobalEnv)}
			if(dataTable){mediumwell} else (as.data.frame(mediumwell))
	}  

	readPATSdb <- function(URL="https://www.dropbox.com/s/bvqw45uppkdt7jd/Filter%20room%20daignostic%20real%20time%20trace_2.txt?dl=0"){
		temp_file <- tempfile()
	    on.exit(unlink(temp_file))
	    request <- GET(URL)
	    writeBin(content(request, type = "raw"), temp_file)
	    data <- read.patsplus(temp_file)
	 	data[,hhid:=NULL]
	 	data[,device_id:=NULL]
	 	data[,loc:=NULL]
	 	data[,rollingMin:=NULL]
	    data
	}   

	readPATSlog <- function(URL='https://www.dropbox.com/s/fse1s0niaabt28i/log.txt?dl=0'){
		temp_file <- tempfile()
	    on.exit(unlink(temp_file))
	    request <- GET(URL)
	    writeBin(content(request, type = "raw"), temp_file)
	    data <- fread(temp_file, header=F)
	    data[,V1:=ymd_hms(V1, tz="America/Los_Angeles")]
	}
