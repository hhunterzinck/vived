##############################################################
# Description:  Functions required to run VIVED.
# Usage: source(Vived.fxns.r)
# Author: Haley Hunter-Zinck
# Date: September 29, 2016
# Dependencies:  ggplot2, Vived.param.r, 
#				Vived.calc.fxns.r, Vived.plot.fxns.r,
#				Vived.read.fxns.r, Vived.report.fxns.r
##############################################################

# packages
source("Vived.param.r")
source("Vived.calc.fxns.r")
source("Vived.plot.fxns.r")
source("Vived.read.fxns.r")

# for testing
testing=0
if(testing==1)
{
	efile="../data/1-3.edis.csv"
	dateRange=c("3001-01-01","3001-01-31")
	metric=OCC_DISP
	agg=HIST
	md="All"
	disp="All"
	acuity="All"
	sHour="00"
	eHour="23"
	sMin="00"
	eMin="59"
	dow=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
	#dow=c("Mon","Tue","Wed","Thu","Fri")
	#dow=c("Mon")
	excludeHolidays=FALSE
}

######################################################
#				MAIN FUNCTIONS						#
#####################################################

# plot according to criteria
createPlot=function(efile, dateRange, metric, agg, md, dow, disp, acuity, 
	sHour, eHour, sMin, eMin, excludeHolidays)
{	
	if(DEBUG)
	{
		cat("startDate=",as.character(dateRange[1]),"\n",sep="")
		cat("endDate=",as.character(dateRange[2]),"\n",sep="")
	}
	
	# check input
	if(!is.na(as.character(dateRange[1])))
	{
		# format input
		startDate=as.POSIXct(as.character(dateRange[1]),tz=TZ)
		endDate=as.POSIXct(as.character(dateRange[2]),tz=TZ)+24*3600-1
	
		msg=illegal(startDate, endDate, metric, agg, disp)
		if(msg!="")
		{
			return(plotInvalid(msg))
		}
	}

	# read data and process for validity
	raw=readVisitData(efile)
	if(length(raw$ti==1) && is.na(raw$ti[1]))
	{
		msg="Data file lacks required column(s) named \"PatientID\", \"TimeIn\", and/or \"TimeOut\".  Please check data file format."
		return(plotInvalid(msg))
	}
	
	# preprocess for validity
	preproc=preprocess(raw)
	
	# filter data according to metric
	filtered=filter(preproc, startDate, endDate, md, dow, disp, acuity, excludeHolidays, metric=metric)
	
	# generate plot
	locName=NA
	plotted=plotMetric(preproc,filtered, locName, metric, agg, startDate, endDate, dow, md, disp,
			sHour, eHour, sMin, eMin, excludeHolidays)
	
	# default file name downloads
	plotted$fileNamePlot=defaultFileName(locName, dateRange, metric, agg, "plot")
	plotted$fileNameTable=defaultFileName(locName, dateRange, metric, agg, "table")
	
	return(plotted)
}

# get list of aggregate views for currently selected metric
getViews=function(metric)
{
	# by day
	if(is.element(metric,METRICS$Day) || is.element(metric,METRICS$DayPercent))
	{
		return(AGG$AGG_DAY)
	}
	
	# by visit
	if(is.element(metric,METRICS$Visit))
	{
		return(AGG$AGG_VISIT)
	}
	
	# by minute
	if(metric==OCC)
	{
		return(AGG$AGG_MIN)
	}
	
	# by minute but subdivided into subgroups
	if(metric==OCC_BY)
	{
		return(AGG$AGG_OCC_BY)
	}
	
	# ED census subdivided into subgroups
	if(is.element(metric,METRICS_OCC_SPLIT))
	{
		return(AGG$AGG_OCC_SPLIT)
	}
		
	# default: return all
	return(AGG$AGG_ALL)
}

# preproces raw data
preprocess=function(obj)
{
	preproc=obj
	
	# filtering by validity
	toRemove=which(is.na(preproc$ti) | is.na(preproc$to) | preproc$ti>=preproc$to)
	toRemove=unique(toRemove)
	if(length(toRemove)>0)
	{
		preproc=updateObj(preproc,-toRemove)
	}
		
	# make sure we have no missing timestamps
	labels=names(INTERVAL_MAX)
	for(i in (length(labels)-1):1)
	{
		nIndeces=NA
		if(is.na(preproc[[labels[i]]][1]))
		{
			preproc[[labels[i]]]=preproc[[labels[i+1]]]
		} else
		{
			nIndeces=which(is.na(preproc[[labels[i]]]))
			preproc[[labels[i]]][nIndeces]=preproc[[labels[i+1]]][nIndeces]
		}
		
		if(DEBUG)
		{
			if(length(nIndeces)==1 && is.na(nIndeces))
			{
				cat("All ",labels[i]," are missing.\n",sep="")
			} else
			{
				perc=round(length(nIndeces)/length(preproc[[labels[i]]])*100,2)
				cat("Number of missing ",labels[i]," filled in: ",
					length(nIndeces)," / ",length(preproc[[labels[i]]]),
					" (",perc,"%)","\n",sep="")
			}
		}
	}
	
	# make sure progression of time stamps is consistent
	labels=names(INTERVAL_MAX)
	for(i in (length(labels)-1):1)
	{
		# get difference between timestamps in each interval
		diff=as.double(preproc[[labels[i+1]]])-as.double(preproc[[labels[i]]])
		
		# if timestamp is out of order, set it equal to the next timestamp in the progression
		wIndeces=which(diff<0)
		preproc[[labels[i]]][wIndeces]=preproc[[labels[i+1]]][wIndeces]
		diff=as.double(preproc[[labels[i+1]]])-as.double(preproc[[labels[i]]])
		
		if(DEBUG)
		{
			
			perc=round(length(wIndeces)/length(preproc[[labels[i]]])*100,2)
			cat("Number of inconsistent ",labels[i]," modified: ",
				length(wIndeces)," / ",length(preproc[[labels[i]]]),
				" (",perc,"%)","\n",sep="")
				cat("Number of inconsistent labels:",length(which(diff<0)),"\n",sep="")
		}
	}
	
	# make sure td is consistent as well
	preproc$td=preproc$tdisp
	preproc$td[which(!is.element(preproc$disp,DISP_ADMIT))]=NA
	
	# debug
	if(DEBUG)
	{
		cat("EDIS visits removed during preprocessing: ",length(obj$ti)-length(preproc$ti)," / ",length(obj$ti)," visits.\n",sep="")
	}
	
	return(preproc)
}

# filter functions
filter=function(obj, start, end, md, dow, disp, acuity, excludeHolidays, metric="")
{
	# filter by date range
	if(metric==OCC || is.element(metric,METRICS_OCC_SPLIT))
	{
		# keep all overlapping intervals for occupancy metrics
		indeces=which(!(obj$to<start | obj$ti>end))
	} else
	{
		# filter by start time of visit for other metrics
		indeces=which(obj$ti>=start & obj$ti<=end)	
	}
	fil=updateObj(obj,indeces)
	
	# filter by day of the week
	if(length(dow)<length(LABEL_DOW) & !(metric==OCC || is.element(metric,METRICS_OCC_SPLIT)))
	{
		visitDow=as.double(format(fil$ti,"%w"))
		dowNum=match(dow,LABEL_DOW)-1
		iDow=is.element(visitDow, dowNum)
		fil=updateObj(fil,iDow)
	}
	
	# exclude holidays
	if(excludeHolidays & !(metric==OCC || is.element(metric,METRICS_OCC_SPLIT)))
	{
		holidays=getFedHolidays(start,end)
		visitDate=as.character(format(fil$ti,"%Y-%m-%d"))
		iHoliday=which(is.element(visitDate, holidays))
		if(length(iHoliday)>0)
		{
			fil=updateObj(fil,-iHoliday)
		}
	}
	
	# filter by provider
	if(md!=ALL_MD)
	{
		iMd=which(fil$md==md)
		fil=updateObj(fil,iMd)
	}
	
	# filter by disposition
	if(disp!=ALL_DISP)
	{
		visitAdmit=which(!is.na(fil$td))
		if(disp==ALL_ADMIT)
		{
			fil=updateObj(fil, visitAdmit)
		} else if(disp==ALL_NOT_ADMIT)
		{
			fil=updateObj(fil, -visitAdmit)
		} else
		{
			fil=updateObj(fil, which(fil$disp==disp))
		}
	}
	
	# filter by acuity
	if(acuity!=ALL_ACUITY)
	{
		aIndeces=which(fil$acuity==acuity)
		fil=updateObj(fil,aIndeces)		
	}

	# debug
	if(DEBUG)
	{
		cat("EDIS visits removed during filtering: ",length(obj$ti)-length(fil$ti)," / ",length(obj$ti)," visits.\n",sep="")
	}

	return(fil)
}

# calculate the metric of interest on the filtered data
plotMetric=function(preproc,obj, locName, metric, agg, start, end, dow, md, disp, 
	sHour, eHour, sMin, eMin, excludeHolidays)
{
	# initialize
	myPlot=NULL
	msg=list()
	myTable=NA
	params=getPlottingParameters(locName, metric, obj)
	
	# format input
	sHourNum=as.double(sHour)
	eHourNum=as.double(eHour)
	sMinNum=as.double(sMin)
	eMinNum=as.double(eMin)
	
	# holidays
	if(excludeHolidays)
	{
		holidays=getFedHolidays(start,end)
	} else
	{
		holidays=c()
	}

	# store appropriate plot for metric - aggregation pair
	if(is.element(metric,METRICS$DayPercent) || is.element(metric,METRICS$Day))
	{
		# day metric
		nday=ceiling(hours(start,end)/24)
		if(metric==NVISIT)
		{
			# nvisit
			dayMetric=calcNumVisit(obj,start, nday, dow, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
		} else if(metric==PVISIT)
		{
			# percentage of visits for preprocessed and filtered
			dayMetricFil=calcNumVisit(obj,start, nday, dow, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
			dayMetricTotal=calcNumVisit(preproc,start, nday, LABEL_DOW, 0, 23, 0, 59, holidays, agg=agg)
			
			# calculate percentage over vector or matrix
			if(agg!=HOUR)
			{
				# align and divide to get percentage and account for dividing by 0
				indeces=match(names(dayMetricFil),names(dayMetricTotal))
				dayMetric=dayMetricFil/dayMetricTotal[indeces]*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				names(dayMetric)=names(dayMetricFil)
			} else
			{
				# align and divide to get percentage and account for dividing by 0
				indeces=match(rownames(dayMetricFil),rownames(dayMetricTotal))
				dayMetric=dayMetricFil/dayMetricTotal[indeces,]*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				rownames(dayMetric)=rownames(dayMetricFil)
			}
		} else if(metric==PBOARD_4PLUS)
		{
			# get indeces of visits that boarded > 4 hours
			indeces=which(as.double(obj$to-obj$td)>4*3600)
		
			# percentage of visits for preprocessed and filtered
			dayMetricSubset=calcNumVisit(updateObj(obj,indeces),start, nday, dow, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
			dayMetricFil=calcNumVisit(obj,start, nday, LABEL_DOW, 0, 23, 0, 59, holidays, agg=agg)
			
			# calculate percentage over vector or matrix
			if(agg!=HOUR)
			{
				# align and divide to get percentage and account for dividing by 0 for vector
				indeces=match(names(dayMetricFil),names(dayMetricSubset))
				dayMetric=dayMetricSubset[indeces]/dayMetricFil*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				names(dayMetric)=names(dayMetricFil)
			} else
			{
				# align and divide to get percentage and account for dividing by 0 for matrix
				indeces=match(rownames(dayMetricFil),rownames(dayMetricSubset))
				dayMetric=dayMetricSubset[indeces,]/dayMetricFil*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				rownames(dayMetric)=rownames(dayMetricFil)
			}
		} else if(metric==NBOARD_4PLUS)
		{
			# get indeces of visits that boarded > 4 hours
			indeces=which(as.double(obj$to-obj$td)>4*3600)
		
			# number of visits boarded > 4 hours
			dayMetric=calcNumVisit(updateObj(obj,indeces),start, nday, dow, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
		} else if(metric==PRETURN72)
		{
			# get only the returned with the same filters
			filForReturn=filter(preproc, start, end+RETURN_HOUR_MAX*3600, md=ALL_MD, dow=LABEL_DOW, 
				disp=ALL_DISP, acuity=ALL_ACUITY, excludeHolidays=FALSE)
			iIndeces=returnedIndex(filForReturn, RETURN_HOUR_MAX, returnInitial=TRUE, rAdmit=FALSE)
			objRet=filter(updateObj(filForReturn,iIndeces), start, end, md=md, dow=dow, 
				disp=disp, acuity=ALL_ACUITY, excludeHolidays=excludeHolidays)
			
			# calculate metrics for numerator and denominator
			dayMetricRet=calcNumVisit(objRet,start, nday, LABEL_DOW, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
			dayMetricFil=calcNumVisit(obj,start, nday, LABEL_DOW, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
			
			# calculate percentage over vector or matrix
			if(agg!=HOUR)
			{
				# align and divide to get percentage and account for dividing by 0 for vector
				indeces=match(names(dayMetricFil),names(dayMetricRet))
				dayMetric=dayMetricRet[indeces]/dayMetricFil*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				names(dayMetric)=names(dayMetricFil)
			} else
			{
				# align and divide to get percentage and account for dividing by 0 for matrix
				cIndeces=match(colnames(dayMetricFil),colnames(dayMetricRet))
				rIndeces=match(rownames(dayMetricFil),rownames(dayMetricRet))
				dayMetric=dayMetricRet[rIndeces,cIndeces]/dayMetricFil*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				colnames(dayMetric)=colnames(dayMetricFil)
			}
		} else if(metric==NRETURN72)
		{
			# get only the returned with the same filters
			filForReturn=filter(preproc, start, end+7*24*3600, md="All", dow=LABEL_DOW, disp="All", acuity=ALL_ACUITY, excludeHolidays=FALSE)
			iIndeces=returnedIndex(filForReturn,RETURN_HOUR_MAX,returnInitial=TRUE, rAdmit=FALSE)
			objRet=filter(updateObj(filForReturn,iIndeces), start, end, md, dow, disp, acuity=ALL_ACUITY, excludeHolidays)
			
			# nvisit that are quick returns
			dayMetric=calcNumVisit(objRet,start, nday, dow, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
		} else if(metric==NRETURN72ADMIT)
		{
			# get only the returned with the same filters
			filForReturn=filter(preproc, start, end+7*24*3600, md="All", dow=LABEL_DOW, disp="All", acuity=ALL_ACUITY, excludeHolidays=FALSE)
			iIndeces=returnedIndex(filForReturn,RETURN_HOUR_MAX,returnInitial=TRUE, rAdmit=TRUE)
			objRet=filter(updateObj(filForReturn,iIndeces), start, end, md, dow, disp, acuity=ALL_ACUITY, excludeHolidays)
			
			# nvisit that are quick returns
			dayMetric=calcNumVisit(objRet,start, nday, dow, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
		} else if(metric==PRETURN72ADMIT)
		{
			# get only the returned with the same filters
			filForReturn=filter(preproc, start, end+7*24*3600, md="All", dow=LABEL_DOW, disp="All", acuity=ALL_ACUITY, excludeHolidays=FALSE)
			iIndeces=returnedIndex(filForReturn,RETURN_HOUR_MAX,returnInitial=TRUE, rAdmit=TRUE)
			objRet=filter(updateObj(filForReturn,iIndeces), start, end, md, dow, disp, acuity=ALL_ACUITY, excludeHolidays)
			
			# calculate metrics for numerator and denominator
			dayMetricRet=calcNumVisit(objRet,start, nday, LABEL_DOW, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
			dayMetricFil=calcNumVisit(obj,start, nday, LABEL_DOW, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg=agg)
			
			# calculate percentage over vector or matrix
			if(agg!=HOUR)
			{
				# align and divide to get percentage and account for dividing by 0 for vector
				indeces=match(names(dayMetricFil),names(dayMetricRet))
				dayMetric=dayMetricRet[indeces]/dayMetricFil*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				names(dayMetric)=names(dayMetricFil)
			} else
			{
				# align and divide to get percentage and account for dividing by 0 for matrix
				cIndeces=match(colnames(dayMetricFil),colnames(dayMetricRet))
				rIndeces=match(rownames(dayMetricFil),rownames(dayMetricRet))
				dayMetric=dayMetricRet[rIndeces,cIndeces]/dayMetricFil*100
				dayMetric[which(dayMetric==Inf | dayMetric==-Inf)]=NA
				colnames(dayMetric)=colnames(dayMetricFil)
			}
		}
		msg[[length(msg)+1]]=paste(" Number of visits analyzed = ",length(which(!is.na(obj$ti)))," / ",length(obj$ti),sep="")
		
		# check for no days
		if(length(dayMetric)==0)
		{
			return(plotInvalid("No valid days to plot after filtering"))
		}
		
		# plot day metric
		if(agg==HIST)
		{
			# day metric, hist
			myPlot=plotHistogram(dayMetric, metric)
			myTable=makeTable(dayMetric,agg)
		} else if(agg==YM)
		{
			# day metric, ym
			yearMonth=format(as.POSIXlt(names(dayMetric),tz=TZ),"%Y-%m")
			myPlot=plotBoxplot(dayMetric, yearMonth, metric)
			myTable=makeTable(dayMetric,agg,yearMonth)
		} else if(agg==DATE)
		{
			# day metric, date
			yearMonthDay=format(as.POSIXlt(names(dayMetric),tz=TZ),"%Y-%m-%d")
			myPlot=plotLine(vec=dayMetric, labels=yearMonthDay, ylab=metric)
			myTable=makeTable(dayMetric,agg,yearMonthDay)
		} else if(agg==YEAR)
		{
			# day metric, year
			date=format(as.POSIXlt(names(dayMetric),tz=TZ),"%Y-%m-%d",tz=TZ)
			year=factor(format(as.POSIXlt(date,tz=TZ),"%Y"))
			myPlot=plotBoxplot(dayMetric, year, metric)
			myTable=makeTable(dayMetric,agg,year)
		} else if(agg==MONTH)
		{
			# day metric, month
			date=format(as.POSIXlt(names(dayMetric),tz=TZ),"%Y-%m-%d")
			month=as.double(format(as.POSIXlt(date,tz=TZ),"%m"))
			month=factor(mapLabel(month, c(1:12), LABEL_MONTH), levels=LABEL_MONTH)
			myPlot=plotBoxplot(dayMetric, month, metric)
			myTable=makeTable(dayMetric,agg,month)
		} else if(agg==DOW)
		{
			# day metric, dow
			date=format(as.POSIXlt(names(dayMetric),tz=TZ),"%Y-%m-%d")
			dow=format(as.POSIXlt(date,tz=TZ),"%w")
			dow=factor(mapLabel(dow, c(0:6), LABEL_DOW), levels=LABEL_DOW)
			myPlot=plotBoxplot(dayMetric, dow, metric)
			myTable=makeTable(dayMetric,agg,dow)
		} else if(agg==HOUR)
		{
			# add missing hours
			missingHours=setdiff(c(0:23),as.double(colnames(dayMetric)))
			if(length(missingHours)>0)
			{
				aIndeces=(ncol(dayMetric)+1):(ncol(dayMetric)+length(missingHours))
				tmat=matrix(c(dayMetric,rep(0,length(missingHours))),nrow=nrow(dayMetric),ncol=ncol(dayMetric)+length(missingHours))
				colnames(tmat)=c(colnames(dayMetric),missingHours)
				rownames(tmat)=rownames(dayMetric)
				dayMetric=tmat
			}
		
			# day metric, hour (arrivals by hour)
			matHour=c()
			for(i in 1:ncol(dayMetric))
			{
				matHour=append(matHour,rep(colnames(dayMetric)[i],nrow(dayMetric)))
			}
			hour=factor(mapLabel(matHour, c(0:23), LABEL_HOUR), levels=LABEL_HOUR)
			
			
			# get plot and table
			myPlot=plotBoxplot(c(dayMetric), hour, metric)
			myTable=makeTable(c(dayMetric),agg,hour)
		} else if(agg==BY_ACUITY)
		{
			# plot day metric by acuity
			
		} else if(agg==WEEK)
		{
			# day metric, week
			week=weekLabel(as.POSIXlt(names(dayMetric),tz=TZ),dowStart=DOW_START) 
			myPlot=plotBoxplot(dayMetric, week, metric)
			myTable=makeTable(dayMetric,agg,week)
		}
		
		# update message
		msg[[length(msg)+1]]=paste(" Number of days analyzed = ",length(which(!is.na(dayMetric)))," / ",length(dayMetric),sep="")
		msg[[length(msg)+1]]=paste(metric," median = ",round(median(dayMetric, na.rm=TRUE),digits=2),sep="")
		
	} else if(is.element(metric,METRICS$Visit))
	{	
		# calculate visit metric
		if(metric==LOS)
		{
			visitMetric=calcTimeIntervals(obj$ti,obj$to,LOS_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		} else if(metric==WAIT_IT)
		{
			visitMetric=calcTimeIntervals(obj$ti,obj$tt,TRIAGE_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		} else if(metric==WAIT_IS)
		{
			visitMetric=calcTimeIntervals(obj$ti,obj$ts,SEEN_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		} else if(metric==WAIT_ID)
		{
			visitMetric=calcTimeIntervals(obj$ti,obj$tdisp,DISP_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		} else if(metric==BOARD_DO)
		{
			visitMetric=calcTimeIntervals(obj$td,obj$to,BOARD_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		} else if(metric==BOARD_RO)
		{
			visitMetric=calcTimeIntervals(obj$tr,obj$to,BOARD_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		} else if(metric==BOARD_RA)
		{
			visitMetric=calcTimeIntervals(obj$tr,obj$ta,BOARD_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		} else if(metric==BOARD_AO)
		{
			visitMetric=calcTimeIntervals(obj$ta,obj$to,BOARD_MAX,dow,sHourNum,eHourNum, sMinNum, eMinNum, holidays)
		}			

		# update message
		msg[[length(msg)+1]]=paste(" Number of visits analyzed = ",length(which(!is.na(visitMetric)))," / ",length(visitMetric),sep="")
		msg[[length(msg)+1]]=paste(metric," median = ",round(median(visitMetric, na.rm=TRUE),digits=2),sep="")
		
		# check for no valid visits
		if(length(visitMetric)==0)
		{
			return(plotInvalid("No valid visits to plot after filtering"))
		}
		
		# plot visit metric
		if(agg==HIST)
		{
			# visit metric, hist
			myPlot=plotHistogram(visitMetric, metric)
			myTable=makeTable(visitMetric,agg)
		} else if(agg==YM)
		{
			# visit metric, ym
			visYearMonth=format(as.POSIXlt(names(visitMetric),tz=TZ),"%Y-%m")
			myPlot=plotBoxplot(visitMetric, visYearMonth, metric)
			myTable=makeTable(visitMetric,agg,visYearMonth)
		} else if(agg==DATE)
		{
			# visit metric, date
			visYearMonthDay=format(as.POSIXlt(names(visitMetric),tz=TZ),"%Y-%m-%d")
			myPlot=plotBoxplot(visitMetric, visYearMonthDay, metric)
			myTable=makeTable(visitMetric,agg,visYearMonthDay)
		} else if(agg==YEAR)
		{
			# visit metric, year
			visYear=factor(format(as.POSIXlt(names(visitMetric),tz=TZ),"%Y"))
			myPlot=plotBoxplot(visitMetric, visYear, metric)
			myTable=makeTable(visitMetric,agg,visYear)
		} else if(agg==MONTH)
		{
			# visit metric, month
			visMonth=format(as.POSIXlt(names(visitMetric),tz=TZ),"%m")
			month=factor(mapLabel(as.double(visMonth), c(1:12), LABEL_MONTH), levels=LABEL_MONTH)
			myPlot=plotBoxplot(visitMetric, month, metric)
			myTable=makeTable(visitMetric,agg,month)
		} else if(agg==DOW)
		{
			# visit metric, dow
			visDayOfWeek=format(as.POSIXlt(names(visitMetric),tz=TZ),"%w")
			dow=factor(mapLabel(visDayOfWeek, c(0:6), LABEL_DOW), levels=LABEL_DOW)
			myPlot=plotBoxplot(visitMetric, dow, metric)
			myTable=makeTable(visitMetric,agg,dow)
		} else if(agg==HOUR)
		{
			# visit metric, hour
			visHour=as.character(as.double(format(as.POSIXlt(names(visitMetric),tz=TZ),"%H")))
			hour=factor(mapLabel(visHour, as.character(c(0:23)), LABEL_HOUR), levels=LABEL_HOUR)
			myPlot=plotBoxplot(visitMetric, hour, metric)
			myTable=makeTable(visitMetric,agg,hour)
		} else if(agg==BY_ACUITY)
		{
			# visit metric, acuity
			indeces=match(obj$acuity,names(MAP_ACUITY))
			visAcuity=factor(MAP_ACUITY[indeces],levels=as.character(MAP_ACUITY))
			if(metric==BOARD_DO)
			{
				visAcuity=visAcuity[which(!is.na(obj$td))]
			}
			myPlot=plotBoxplot(visitMetric, visAcuity, metric)
			myTable=makeTable(visitMetric, agg, visAcuity)
		} else if(agg==BY_DISP)
		{
			# visit metric, acuity
			visDisp=obj$disp
			visDisp[which(is.na(visDisp))]=obj$disp[which(is.na(visDisp))]
			visDisp=factor(visDisp)
			if(metric==BOARD_DO)
			{
				visDisp=visDisp[which(!is.na(obj$td))]
			}
			myPlot=plotBoxplot(visitMetric, visDisp, metric)
			myTable=makeTable(visitMetric, agg, visDisp)
		} else if(agg==BY_PROV)
		{
			# visit metric, acuity
			visProv=factor(obj$md)
			if(metric==BOARD_DO)
			{
				visProv=visProv[which(!is.na(obj$td))]
			}
			myPlot=plotBoxplot(visitMetric, visProv, metric)
			myTable=makeTable(visitMetric, agg, visProv)
		} else if(agg==WEEK)
		{
			# visit metric, week
			week=weekLabel(as.POSIXlt(names(visitMetric),tz=TZ),dowStart=DOW_START)
			myPlot=plotBoxplot(visitMetric, week, metric)
			myTable=makeTable(visitMetric,agg,week)
		}
	} else if(metric==OCC)
	{
		# occupancy per minute
		occ=calcOcc(obj$ti, obj$to, start, end)
		occTime=start+c(0:(length(occ)-1))*60
		
		# remove dow if date range is less than a week
		dow=intersect(dow,names(table(format(occTime,"%a"))))
		
		# filter by day of the week if necessary
		if(length(dow)<length(LABEL_DOW))
		{
		  dowNum=match(dow,LABEL_DOW)-1
		  indeces=which(is.element(format(occTime,"%w"),dowNum))
		  occ=occ[indeces]
		  occTime=occTime[indeces]
		}
		
		# filter by holidays if necessary
		if(excludeHolidays)
		{
		  indeces=which(is.element(format(occTime,"%Y-%m-%d"),holidays))
		  if(length(indeces)>0)
		  {
			occ=occ[-indeces]
			occTime=occTime[-indeces]
		  }
		}

		# filter by minute if necessary
		myMinutes=getMinutes(sHourNum,eHourNum,sMinNum,eMinNum)
		if(agg!=MD)
		{
			# filter occupancy by hours
			indeces=which(is.element(format(occTime,"%H:%M"),myMinutes))
			occ=occ[indeces]
			occTime=occTime[indeces]
		}
		
		# check for no valid occupancy time slots
		if(length(occ)==0)
		{
			return(plotInvalid("No valid minutes to plot after filtering"))
		}

		if(agg==HIST)
		{
			# occ, hist
			myPlot=plotHistogram(occ, metric)
			myTable=makeTable(occ,agg)
		} else if(agg==YM)
		{
			# occ, ym
			occYearMonth=format(occTime,"%Y-%m")
			myPlot=plotBoxplot(occ, occYearMonth, metric)
			myTable=makeTable(occ,agg, occYearMonth)
		} else if(agg==DATE)
		{
			# occ, date
			occYearMonthDay=format(occTime,"%Y-%m-%d")
			myPlot=plotBoxplot(occ, occYearMonthDay, metric)
			myTable=makeTable(occ,agg, occYearMonthDay)
		} else if(agg==YEAR)
		{
			# occ, year
			occYear=factor(format(occTime,"%Y"))
			myPlot=plotBoxplot(occ, occYear, metric)
			myTable=makeTable(occ,agg, occYear)
		} else if(agg==MONTH)
		{
			# occ, month
			occMonth=as.double(format(occTime,"%m"))
			month=factor(mapLabel(occMonth, c(1:12), LABEL_MONTH), levels=LABEL_MONTH)
			myPlot=plotBoxplot(occ, month, metric)
			myTable=makeTable(occ,agg, month)
		} else if(agg==DOW)
		{
			# occ, dow
			occDayOfWeek=format(occTime,"%w")
			dow=factor(mapLabel(occDayOfWeek, c(0:6), LABEL_DOW), levels=LABEL_DOW)
			myPlot=plotBoxplot(occ, dow, metric)
			myTable=makeTable(occ,agg, dow)
		} else if(agg==HOUR)
		{
			# occ, hour
			occHour=as.character(as.double(format(occTime,"%H")))
			hour=factor(mapLabel(occHour, as.character(c(0:23)), LABEL_HOUR), levels=LABEL_HOUR)
			myPlot=plotBoxplot(occ, hour, metric)
			myTable=makeTable(occ,agg, hour)
		} else if(agg==WEEK)
		{
			# occ, date
			occWeek=weekLabel(occTime,dowStart=DOW_START)
			myPlot=plotBoxplot(occ, occWeek, metric)
			myTable=makeTable(occ,agg, occWeek)
		} else if(agg==MD)
		{
			# occ, min x dow
			nweek=round(as.double(table(format(occTime,"%w")))/(60*24))
			dowNum=match(dow,LABEL_DOW)-1
			dowTime=factor(mapLabel(format(occTime,"%w"), dowNum, dow), levels=dow)
			minute=factor(as.character(as.double(format(occTime,"%H"))*60+as.double(format(occTime,"%M"))),levels=as.character(c(0:(24*60-1))))
			
			# aggregate by day of the week and minute of the day
			df=data.frame(occ=occ, dow=dowTime, minute=minute)
			meltMD=aggregate(occ ~ dow + minute, df, FUN=sum)
			
			# setup for multiple lines
			#occHourMin=format(occTime,"%H:%M")
			matMD=matrix(meltMD$occ/as.double(nweek), nrow=length(dow), ncol=24*60, byrow=FALSE)
			colnames(matMD)=as.character(c(0:(24*60-1)))
			if(length(which(nweek>1))>0)
			{
				rownames(matMD)=dow
			} else
			{
				oIndeces=match(as.character(format(occTime[seq(from=1,to=length(occTime),by=24*60)],"%a")),dow)
				matMD=matrix(matMD[oIndeces,],nrow=nrow(matMD))
				rownames(matMD)=as.character(format(occTime[seq(from=1,to=length(occTime),by=24*60)],"%Y-%m-%d\n%a"))
			}
			
			# get minutes to keep based on time of day filter
			mIndeces=which(is.element(HOUR_MINUTES,myMinutes))
			if(length(mIndeces)<ncol(matMD))
			{
				matMD[,-mIndeces]=NA
			} 

			# define xlimits
			if(sHourNum <= eHourNum)
			{
				xlimits=c(sHourNum*60+sMinNum, eHourNum*60+eMinNum)
			} else
			{
				xlimits=c(0,24*60)
			}
			
			# plot 
			myPlot=plotMultiLine(matMD, nbed=getNumBed(locName), xlab="", ylab=metric, label=metric, xlimits=xlimits)
			
			# table
			myTable=t(rbind(getMinLabel(c(1:ncol(matMD))),matMD))
			colnames(myTable)[1]="Minute"
		}
	} else if(is.element(metric,METRICS_OCC_SPLIT))
	{
		# get occupancy subset 
		occ=calcOccSubset(obj,metric,start,end)
		occTime=start+c(0:(length(occ[[1]])-1))*60
		
		# remove dow if date range is less than a week
		dow=intersect(dow,names(table(format(occTime,"%a"))))
	
		# filter by day of the week if necessary
		if(length(dow)<length(LABEL_DOW))
		{
		  dowNum=match(dow,LABEL_DOW)-1
		  indeces=which(is.element(format(occTime,"%w"),dowNum))
		  for(i in 1:length(occ))
		  {
			occ[[i]]=occ[[i]][indeces]
		  }
		  occTime=occTime[indeces]
		}
		
		# filter by holidays if necessary
		if(excludeHolidays)
		{
		  indeces=which(is.element(format(occTime,"%Y-%m-%d"),holidays))
		  if(length(indeces)>0)
		  {
			for(i in 1:length(occ))
			{
				occ[[i]]=occ[[i]][-indeces]
			}
			occTime=occTime[-indeces]
		  }
		  
		  # remove dow if holiday was the only representation of that day
		  dow=intersect(dow,names(table(format(occTime,"%a"))))
		}

		# get minutes to keep based on time of day filter
		myMinutes=getMinutes(sHourNum,eHourNum,sMinNum,eMinNum)
		mIndeces=which(is.element(HOUR_MINUTES,myMinutes))
		
		if(agg!=MD)
		{
			# initialize
			occDate=format(occTime,"%Y-%m-%d")
			occFil=list()		
			
			# for each occ, get mean across each day for requested hours
			for(i in 1:length(occ))
			{
				# only retain hours of the day
				omat=matrix(occ[[i]],ncol=length(unique(occDate)),byrow=FALSE)
				occFil[[i]]=c(omat[mIndeces,])
			}
			
			# filter occTime by minutes
			dmat=matrix(as.character(occTime),ncol=length(unique(occDate)),byrow=FALSE)
			occTimeFil=as.POSIXlt(c(dmat[mIndeces,]),tz=TZ)
			
			# make sure labels are still present
			names(occFil)=names(occ)
		}
		
		# check for no valid occupancy time slots
		if(length(which(unlist(lapply(occ,length))>0))==0)
		{
			return(plotInvalid("No valid minutes to plot after filtering"))
		}

		# format and plot split occupancy
		if(agg==HIST)
		{
			# plot occupancy means for each date as a stacked barplot
			msg=params$msg
			myPlot=plotMultiDensity(occFil, xlab=metric, myParams=params)
			
			# table
			myTable=makeTable(occFil,agg)
			if(length(names(params$myLevels))>0)
			{
				myTable[,1]=params$myLevels[myTable[,1]]
			}

		} else if(agg==YEAR)
		{
			# get plot and table for aggregation type year
			results=edCensusPlotAndTable(occFil=occFil,occAggFil=format(occTimeFil,"%Y"), metric=metric, myParams=params)
			myTable=results$myTable
			myPlot=results$myPlot
		} else if(agg==YM)
		{
			# get plot and table for aggregation type year
			results=edCensusPlotAndTable(occFil=occFil,occAggFil=format(occTimeFil,"%Y-%m"), metric=metric, myParams=params)
			myTable=results$myTable
			myPlot=results$myPlot
		} else if(agg==MONTH)
		{
			# get plot and table for aggregation type year
			results=edCensusPlotAndTable(occFil=occFil,occAggFil=factor(format(occTimeFil,"%b"),levels=LABEL_MONTH), metric=metric, myParams=params)
			myTable=results$myTable
			myPlot=results$myPlot
		} else if(agg==DATE)
		{
			# get plot and table for aggregation type date
			results=edCensusPlotAndTable(occFil=occFil,occAggFil=format(occTimeFil,"%Y-%m-%d"), metric=metric, myParams=params)
			myTable=results$myTable
			myPlot=results$myPlot
		} else if(agg==DOW)
		{
			# get plot and table for aggregation type year
			results=edCensusPlotAndTable(occFil=occFil,occAggFil=factor(format(occTimeFil,"%a"),levels=LABEL_DOW), metric=metric, myParams=params)
			myTable=results$myTable
			myPlot=results$myPlot
		} else if(agg==HOUR)
		{
			# get plot and table for aggregation type year
			results=edCensusPlotAndTable(occFil=occFil,occAggFil=LABEL_HOUR[as.double(format(occTimeFil,"%H"))+1], metric=metric, myParams=params)
			myTable=results$myTable
			myPlot=results$myPlot
		} else if(agg==WEEK)
		{
			# get plot and table for aggregation type date
			occAggWeek=weekLabel(occTimeFil,dowStart=DOW_START)
			results=edCensusPlotAndTable(occFil=occFil,occAggFil=occAggWeek, metric=metric, myParams=params)
			myTable=results$myTable
			myPlot=results$myPlot
		} else if(agg==MD)
		{
			# occ, min x dow
			nweek=round(as.double(table(format(occTime,"%w")))/(60*24))
			dowNum=match(dow,LABEL_DOW)-1
			dowTime=factor(mapLabel(format(occTime,"%w"), dowNum, dow), levels=dow)
			minute=factor(as.character(as.double(format(occTime,"%H"))*60+as.double(format(occTime,"%M"))),levels=as.character(c(0:(24*60-1))))

			# create dow x min matrix for each type of occ calculation
			toPlotOcc=list()
			for(i in 1:length(occ))
			{
				# aggregate by day of the week and minute of the day
				df=data.frame(occType=occ[[i]], dowTime=dowTime, minute=minute)
				meltMD=aggregate(occType ~ dowTime + minute, df, FUN=sum)
			
				# setup for multiple lines
				matMD=matrix(meltMD$occType/as.double(nweek), nrow=length(dow), ncol=24*60, byrow=FALSE)
				colnames(matMD)=as.character(c(0:(24*60-1)))
				if(length(which(nweek>1))>0)
				{
					rownames(matMD)=dow
				} else
				{
					oIndeces=match(as.character(format(occTime[seq(from=1,to=length(occTime),by=24*60)],"%a")),dow)
					matMD=matrix(matMD[oIndeces,],nrow=nrow(matMD))
					rownames(matMD)=as.character(format(occTime[seq(from=1,to=length(occTime),by=24*60)],"%Y-%m-%d\n%a"))
				}
				
				# set indeces to 0 if not in time range
				if(length(mIndeces)<ncol(matMD))
				{
					matMD[,-mIndeces]=NA
				} 
				toPlotOcc[[i]]=matMD
			}
			names(toPlotOcc)=names(occ)
						
			# define xlimits
			if(sHourNum <= eHourNum)
			{
				xlimits=c(sHourNum*60+sMinNum, eHourNum*60+eMinNum)
			} else
			{
				xlimits=c(0,24*60)
			}

			# plot as multi-barplot
			myPlot=plotMultiBar(myList=toPlotOcc, xlab="", ylab=metric, label=metric, xlimits=xlimits, myParams=params, metric=metric)
		
			# table
			myTable=t(rbind(getMinLabel(c(1:ncol(toPlotOcc[[1]]))), rep(names(toPlotOcc)[1],,ncol(toPlotOcc[[i]])), toPlotOcc[[1]]))
			if(length(toPlotOcc)>1)
			{
				for(i in 2:length(toPlotOcc))
				{
					tmat=t(rbind(getMinLabel(c(1:ncol(toPlotOcc[[i]]))), rep(names(toPlotOcc)[i],ncol(toPlotOcc[[i]])), toPlotOcc[[i]]))
					myTable=rbind(myTable,tmat)
				}
			}
			colnames(myTable)[1:2]=c("Minute",tail(strsplit(metric," ")[[1]],1))
			occLabels=MAP_ALL_OCC[myTable[,2]]
			occLabels[which(is.na(occLabels))]=myTable[,2]
			myTable[,2]=occLabels
		}
	}

	# return plot and msg
	result=list()
	result$plot=myPlot
	result$msg=params$msg
	result$table=cleanTable(myTable,agg)
	return(result)
}

# create plot and table for split ED census metrics for
#	non-histogram and non-minutexday views
edCensusPlotAndTable=function(occFil,occAggFil, metric, myParams)
{
	# initialize
	toPlotOcc=matrix(NA,nrow=length(occFil), ncol=length(unique(occAggFil)), 
		dimnames=list(rownames=names(occFil), colnames=sort(unique(occAggFil))))

	# for each occ, get mean across each day for requested hours
	for(i in 1:length(occFil))
	{
		toPlotOcc[i,]=aggregate(occFil[[i]]~occAggFil,FUN=mean, na.rm=TRUE)$occFil
	}
	
	# plot occupancy means for each date as a stacked barplot
	myPlot=plotStackedBar(toPlotOcc, xlab="", ylab=metric, myParams=myParams)
	
	# table
	myTable=t(round(toPlotOcc, digits=NDIGITS))
	cNames=c("Category",MAP_ALL_OCC[colnames(myTable)])
	cNames[which(is.na(cNames))]=colnames(myTable)[which(is.na(cNames))-1]
	myTable=cbind(rownames(myTable),myTable)
	colnames(myTable)=cNames
			
	# return
	toreturn=list()
	toreturn$myPlot=myPlot
	toreturn$myTable=myTable
	return(toreturn)
}
