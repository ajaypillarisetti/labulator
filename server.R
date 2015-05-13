### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0N

library(reshape2)
library(plyr)
library(lubridate)
library(data.table)
library(rvest)
library(httr)
library(dygraphs)
library(xts)


shinyServer(function(input, output) {

	#read in data
	datasetInput <- reactive({
		datas <- readPATSdb()
		datas.2 <- readPATSdb(URL="https://www.dropbox.com/s/b8dyxurmf1or2xr/Filter%20room%20daignostic%20real%20time%20trace.txt?dl=0")
		datas.3 <- readPATSdb(URL="https://www.dropbox.com/s/ap6wfdy86w3f957/Filter%20room%20daignostic%20real%20time%20trace_3.txt?dl=0")
		datas.4 <- readPATSdb(URL="https://www.dropbox.com/s/r7g9z844t1rtcl1/Filter%20room%20daignostic%20real%20time%20trace_4.txt?dl=0")
		datas.5 <- readPATSdb(URL="https://www.dropbox.com/s/0eg3jgcvetuo7ws/Filter%20room%20daignostic%20real%20time%20trace_5.txt?dl=0")
		datas <- rbind(datas.4, datas.5)
		datas <- arrange(datas, datetime)		
		maxdate <- datas[,max(datetime)]
		datas <- datas[datetime>maxdate-(5*24*60*60)]
		datas[,tempLow:=20]
		datas[,tempMid:=21.5]
		datas[,tempHigh:=23]
		datas[,rhHigh:=40]
		datas[,rhMid:=35]
		datas[,rhLow:=30]
		datas
	})

	logInput <- reactive({
		log <- readPATSlog()
	})

	summaryDataTable <- reactive({
		dta <- datasetInput()
		dta[,round.dt:=datetime-(minute(datetime)*60)-second(datetime)]
		dta[RH_air>rhHigh | RH_air<rhLow, rhFlag:=1]
		dta[rhFlag!=1 | is.na(rhFlag), rhFlag:=0]
		dta[degC_air>tempHigh | degC_air<tempLow, tempFlag:=1]
		dta[tempFlag!=1 | is.na(tempFlag), tempFlag:=0]
		summary <- dta[,list(
			`Minutes Temp out of Bounds`=sum(tempFlag),
			`Minutes RH out of Bounds`=sum(rhFlag),
			`No Obs`=length(tempFlag)
			),by='round.dt']
		summary2 <- summary[,list(
			`% Temp Violations per Hour`= round(`Minutes Temp out of Bounds`/`No Obs`, 3)*100,
			`% RH Violations per Hour`= round(`Minutes RH out of Bounds`/`No Obs`, 3) *100
			), by='round.dt']
		setnames(summary2,'round.dt','Datetime')
		arrange(summary2,Datetime)
		summary2
	})

	output$allDataTable<-renderDataTable({
		# data_cleaned()[,with=F]}, 
		datasetInput()},
		options = list(paging=FALSE, searching=FALSE))

	#SUMMARY DATA TABLE
	output$summaryDataTable <- renderDataTable({
		summaryDataTable()},
		options = list(paging=FALSE, searching=FALSE))

	#DOWNLOAD OUTPUTS
	output$downloadFull <- downloadHandler(
    	filename = function() {paste(
    		'lab.temp.rh',
    		'-',
			Sys.Date(),
    		'.csv', sep='') },
	    content = function(file) {
    		write.csv(datasetInput(), file, row.names=F)
    	}
	)

	#PLOT OUTPUTS
	  output$temp <- renderDygraph({
		dygraph(as.xts(datasetInput()[,c('degC_air', 'tempLow', 'tempMid','tempHigh'), with=F], order.by=datasetInput()$datetime), group='lab')%>% 
		# dyRangeSelector() %>%
		dySeries(c("tempLow", "tempMid", "tempHigh"), label = "Temp Bounds", strokePattern = "dotted", strokeWidth=0.5) %>%
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=TRUE) %>%
	    dyAxis("y", label = "Temp C") %>%
	    dyAxis("x", label = "") %>%
		dyEvent(date = "2015-04-17 23:50", "New Humidifier", labelLoc = "top")
	  })
	  output$rh <- renderDygraph({
		dygraph(as.xts(datasetInput()[,c('RH_air', 'rhLow', 'rhMid','rhHigh'), with=F], order.by=datasetInput()$datetime), group='lab')%>% 
		dyRangeSelector() %>%
		dySeries(c("rhLow", "rhMid", "rhHigh"), label = "RH Bounds", strokePattern = "dotted", strokeWidth=0.5) %>%
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=TRUE) %>%
	    dyAxis("y", label = "RH") %>%
	    dyAxis("x", label = "datetime")
		# dyEvent(date = "2015-04-17 23:50", "New Humidifier", labelLoc = "top")
	  })
})