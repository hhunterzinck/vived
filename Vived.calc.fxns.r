##############################################################
# Description:   Functions that do calculations for VIVED.
# Usage: source("Vived.calc.fxns.r")
# Author: Haley Hunter-Zinck
# Date: October 6, 2016
# Dependencies:
##############################################################

# determine if a parameter combo is illegal
illegal=function(start, end, metric, agg, disp)
{
	msg=""
	ADJUST_RANGE="\nPlease adjust date range."
	SELECT_DIFFERENT="\nPlease select another metric or aggregation view."
	
	# check parameters
	if(seconds(start,end)<0)
	{
		# bad date range
		msg=paste("End date is before start date.",ADJUST_RANGE,"\n",sep="")
	} else if((metric==BOARD_RO || metric==BOARD_RA || metric==BOARD_AO) && disp==DISCHARGE)
	{
		# can't calculate boarding time of discharged patients
		msg=paste("Cannot calculate boarding time for discharged patients.",SELECT_DIFFERENT,"\n",sep="")
	}  	
	return(msg)
}

# update an list object to include only the indeces requested
updateObj=function(obj, indeces)
{
	upt=list()
	for(i in 1:length(obj))
	{
		upt[[i]]=obj[[i]][indeces]
	}
	names(upt)=names(obj)
	return(upt)
}

# calculate occupancy for the given object
calcOcc=function(vStart,vEnd, iStart, iEnd)
{
	# setup
	MAX=1e3
	nmin=round(minutes(iStart,iEnd))
	breaks=unique(c(seq(from=1,to=nmin,by=MAX),nmin+1))
	sums=list()

	# get visit intervals, expressing start and end of interval as minutes since filter start time
	visitIntervals=round((cbind(as.double(vStart),as.double(vEnd))-as.double(iStart))/60)

	# calculate occ for each large time period	
	subIntervals=c()
	for(i in 2:length(breaks))
	{
		# look at any visit intervals that overlap break interval
		indeces=which(!(visitIntervals[,2]<breaks[i-1] | visitIntervals[,1]>=breaks[i]))
		if(length(indeces)>0)
		{
			# determine occupancy in the break interval
			matOcc=apply(matrix(visitIntervals[indeces,],nrow=length(indeces)), 1, occInInterval, interval=c(breaks[i-1],breaks[i]))
			subIntervals[[i-1]]=rowSums(matOcc)
		} else
		{
			subIntervals[[i-1]]=rep(0,breaks[i]-breaks[i-1])
		}
	}

	# collapse all into a single vector
	return(unlist(subIntervals))
}

# calculate occupancy for a given interval of minutes
# NOTE: both visit and interval have two elements
# representing the start end end in minutes from
# a standard reference point
occInInterval=function(visit, interval)
{
	# convert timestamps to hours since start
	vStart=visit[1]
	vEnd=visit[2]
	iStart=interval[1]
	iEnd=interval[2]

	# truncate visit if necessary
	if(vStart < iStart)
	{
		vStart=iStart
	}
	if(vEnd>=iEnd)
	{
		vEnd=iEnd
	}
	
	if(DEBUG)
	{
		if(vStart-iStart<0)
		{
			cat("vStart-iStart\n")
			print(visit)
		}
		
		if(vEnd-vStart<0)
		{
			cat("vEnd-vStart\n")
			print(visit)
		}
		if(iEnd-vEnd<0)
		{
			cat("iEnd-vEnd\n")
			print(visit)
		}
	}
	
	# update intervals
	visitOcc=c(rep(0,vStart-iStart),rep(1,vEnd-vStart),rep(0,iEnd-vEnd))
	return(visitOcc)
}

# calculate the number of visit for each day in the requested time period
calcNumVisit=function(obj, start, nday, dow, sHourNum, eHourNum, sMinNum, eMinNum, holidays, agg)
{
	# get visits that start within time range
	indeces=which(is.element(format(obj$ti,"%H:%M"),getMinutes(sHourNum, eHourNum, sMinNum, eMinNum)))

	# format all days
	if(agg==HOUR)
	{
		# get day by hour counts of visits
		nvisit=table(format(obj$ti[indeces],"%Y-%m-%d"),as.double(format(obj$ti[indeces],"%H")))
		
		# add missing hours
		missingHours=setdiff(c(0:23), as.double(colnames(nvisit)))
		colLabels=c(colnames(nvisit),missingHours)
		nvisit=cbind(nvisit,matrix(0,nrow=nrow(nvisit),ncol=length(missingHours)))
		colnames(nvisit)=colLabels
	} else
	{
		nvisit=table(format(obj$ti[indeces],"%Y-%m-%d"))
	}
	days=format(start+24*3600*c(0:(nday-1)),"%Y-%m-%d")
	
	# determine missing days
	missing=setdiff(days,rownames(nvisit))
	
	# only keep missing days if they are within user's dow selection
	if(length(dow)<length(LABEL_DOW))
	{
	  dowNum=match(dow,LABEL_DOW)-1
	  indeces=which(is.element(as.double(format(as.POSIXlt(missing),"%w",tz=TZ)),dowNum))
	  missing=missing[indeces]
	}
	
	# only keep missing holidays if they are not excluded
	if(length(holidays)>0)
	{
		iHoliday=which(is.element(missing, holidays))
		if(length(iHoliday)>0)
		{
			missing=missing[-iHoliday]
		}
	}
	
	# add missing days
	if(length(missing)>0)
	{
		if(agg==HOUR)
		{
			toAdd=matrix(0,nrow=length(missing), ncol=ncol(nvisit))
			rownames(toAdd)=missing
			nvisit=rbind(nvisit,toAdd)
			nvisit[order(rownames(nvisit)),]
		} else
		{
			toAdd=rep(0,length(missing))
			names(toAdd)=missing
			nvisit=append(nvisit,toAdd)
			nvisit=nvisit[order(names(nvisit))]
		}
	}
	
	# return number of visits per day
	return(nvisit)
}

# calculate the amount of time in intervals between pairs of timestamps
calcTimeIntervals=function(t1, t2, maxTime)
{
	# calculate time interval in minutes
	visitMetric=(as.double(t2)-as.double(t1))/60
	
	# set unrealistic time intervals to missing
	if(length(which(visitMetric>maxTime))>0)
	{
		visitMetric[which(visitMetric>maxTime)]=NA
	}
	
	# label by initial timestamp and return
	names(visitMetric)=t1
	return(visitMetric)
}

# calculate the requested occupancy subsetted by intervals
calcOccSubset=function(obj, metric, start, end)
{
	# initialize
	occ=list()

	if(metric==OCC_SPLIT)
	{		
		# get required metrics for status
		tBoard=(as.double(obj$to)-as.double(obj$td))
		iDis=which(is.na(obj$td))
		iAdmFast=which(!is.na(obj$td) & tBoard<=BOARD_THRESH)
		iAdmSlow=which(!is.na(obj$td) & tBoard>BOARD_THRESH)
		
		# occupancy per minute for each visit interval type
		occ$io=calcOcc(obj$ti[iDis], obj$to[iDis], start, end)
		occ$id=calcOcc(obj$ti[c(iAdmFast,iAdmSlow)], obj$td[c(iAdmFast,iAdmSlow)], start, end)
		occ$do=calcOcc(obj$td[c(iAdmFast,iAdmSlow)], c(obj$to[iAdmFast],(obj$td[iAdmSlow]+BOARD_THRESH)), start, end)
		occ$bo=calcOcc(obj$td[iAdmSlow]+BOARD_THRESH+1, obj$to[iAdmSlow], start, end)
	} else if(metric==OCC_ACUITY)
	{
		# occupancy per minute for each visit by acuity level
		occ=list()
		uValue=arrangeUnknown(sort(unique(obj$acuity)),uValue=UNKNOWN_STRING,uLast=FALSE)
		for(i in 1:length(uValue))
		{
			indeces=which(obj$acuity==uValue[i])
			occ[[i]]=calcOcc(obj$ti[indeces], obj$to[indeces], start, end)
		}
		names(occ)=uValue
	} else if(metric==OCC_DISP)
	{
		# get occupancy by disposition
		uDisp=names(sort(table(obj$disp),decreasing=FALSE,na.last=FALSE))
		uDisp=arrangeUnknown(uDisp,uValue=UNKNOWN_STRING,uLast=FALSE)
		for(disp in uDisp)
		{
			indeces=which(obj$disp==disp)
			occ[[disp]]=calcOcc(obj$ti[indeces], obj$to[indeces], start, end)
		}
	} else if(metric==OCC_AGE)
	{ 
		# get occupancy by age group
		uAge=as.character(sort(factor(unique(obj$age),levels=as.character(MAP_AGE)),decreasing=FALSE))
		uAge=arrangeUnknown(uAge,uValue=UNKNOWN_STRING,uLast=FALSE)
		for(age in uAge)
		{
			indeces=which(obj$age==age)
			if(length(indeces)>0)
			{
				occ[[as.character(age)]]=calcOcc(obj$ti[indeces], obj$to[indeces], start, end)
			}
		}
	} else if(metric==OCC_PROG)
	{
		occ$it=calcOcc(obj$ti, obj$tt, start, end)
		occ$ts=calcOcc(obj$tt, obj$ts, start, end)
		occ$sd=calcOcc(obj$ts, obj$tdisp, start, end)
		occ$dop=calcOcc(obj$tdisp, obj$to, start, end)
	}

	return(occ)
}	


# calculate the number of hours between two timestamps
weeks=function(t1,t2)
{
	return(days(t1,t2)/7)
}

# for each timestamp in a vector, get the date
# that corresponds to the start that timestamps
# week where the day of the week that is the start
# of the week is determined by the user
weekLabel=function(vec, dowStart="Mon")
{
	# get dow numbers
	dowStartNum=which(LABEL_DOW==dowStart)-1
	dowVecNum=as.double(format(vec,"%w"))
	
	# get start of the week date for each
	dowDiffNum=dowVecNum-dowStartNum
	dowDiffNum[which(dowDiffNum<0)]=7+dowDiffNum[which(dowDiffNum<0)]
	vecWeek=format(vec-dowDiffNum*24*3600,"%Y-%m-%d")
	return(vecWeek)
}

# calculate the number of hours between two timestamps
days=function(t1,t2)
{
	return(hours(t1,t2)/24)
}

# calculate the number of hours between two timestamps
hours=function(t1,t2)
{
	return(minutes(t1,t2)/60)
}

# calculate the number of hours between two timestamps
minutes=function(t1,t2)
{
	return(seconds(t1,t2)/60)
}

# calculate seconds between two timestamps
seconds=function(t1,t2)
{
	return(as.double(t2)-as.double(t1))
}

# map labels to a another label set
mapLabel=function(vec, from, to)
{
	map=setNames(to,from)
	return(map[vec])
}

# return the number of non-NA elements in a vector
numNotNa=function(vec)
{
	return(length(which(!is.na(vec))))
}

getNumBed=function(locName)
{
	# just return NA if locName unknown
	if(is.na(locName))
	{
		return(NA)
	}

	# get number of beds
	eHeader=scan(FILE_ED_FACTS, what="character", nlines=1, sep=",", quiet=QUIET)
	emat=matrix(scan(FILE_ED_FACTS, what="character", skip=1, sep=",", quiet=QUIET), ncol=length(eHeader), byrow=TRUE)
	cIndex=which(eHeader==COL_NBED)
	rIndex=which(emat[,which(eHeader==COL_LOC)]==locName)
	if(length(rIndex)==1)
	{
		if(emat[rIndex,cIndex]=="")
		{
			if(DEBUG)
			{
				cat("Number of beds for location ", locName, " is blank in file ", FILE_ED_FACTS, "\n",sep="")
			}
			nbed=NA
		} else
		{
			if(DEBUG)
			{
				cat("Number of beds for location ", locName, " is ",emat[rIndex,cIndex]," in file ", FILE_ED_FACTS, "\n",sep="")
			}
			nbed=as.double(emat[rIndex,cIndex])
		}
	} else
	{
		if(DEBUG)
		{
			cat(length(rIndex), " row(s) matching location ",locName," in file ", FILE_ED_FACTS, "\n", sep="")
		}
		nbed=NA
	}

	return(nbed)
}

# get list of valid hours in the hour time range
getHours=function(sHourNum, eHourNum)
{
	# initialize
	hours=c()
	
	# get valid hours in time range
	if(sHourNum<eHourNum)
	{
		hours=c(sHourNum:(eHourNum-1))
	} else
	{
		hours=c(c(sHourNum:23),c(0:(eHourNum-1)))
	}

	return(hours)
}

# get list of valid hours in the hour time range
getMinutes=function(sHourNum, eHourNum, sMinNum, eMinNum)
{
	# initialize
	minutes=c()
	startTime=paste(sprintf("%02d",sHourNum),sprintf("%02d",sMinNum),sep=":")
	endTime=paste(sprintf("%02d",eHourNum),sprintf("%02d",eMinNum),sep=":")
	
	# get valid hours in time range
	if(startTime==endTime)
	{
		minutes=startTime
	}else if(startTime<endTime)
	{
		minutes=HOUR_MINUTES[which(HOUR_MINUTES==startTime):which(HOUR_MINUTES==endTime)]
	} else
	{
		minutes=c(HOUR_MINUTES[which(HOUR_MINUTES==startTime):length(HOUR_MINUTES)], HOUR_MINUTES[1:which(HOUR_MINUTES==endTime)])
	}

	return(minutes)
}

# construct a data frame to display on the dashboard from metric data and the given aggregation view
makeTable=function(tabData,agg,cats=NA)
{ 
	# initialize
	df=c()
	
	# make table according to aggregation view
	if(typeof(tabData)=="list")
	{
		smat=c()
		for(i in 1:length(tabData))
		{
			stats=calcStats(tabData[[i]],agg, cats)
			smat=rbind(smat,c(names(tabData)[i],stats[,2:ncol(stats)]))
			colnames(smat)=c("Category",colnames(stats)[2:ncol(stats)])
		}
	} else
	{
		smat=calcStats(tabData, agg, cats)
	}
	
	# return as data frame
	df=as.data.frame(smat)
	return(df)
}

# calculate all statistics for the given categories
calcStats=function(vec, agg, cats=NA)
{	
	# adjust column names
	myColNames=TABLE_COL_NAMES
	mat=c()
	
	# get non-zero indeces
	nonZeroIndeces=which(vec!=0)
	nonNaIndeces=which(!is.na(vec))

	if(is.na(cats[1]))
	{	
		# initialize
		mat=matrix(NA,ncol=length(TABLE_COL_NAMES), nrow=1, dimnames=list(rownames=c(),colnames=myColNames))
		rownames(mat)="All"
		
		# if non non-missing values, just return NA
		if(length(nonNaIndeces)==0)
		{
			return(mat)
		}
		
		# calculate statistics over whole vector
		mat[,which(TABLE_COL_NAMES==COUNT)]=length(which(!is.na(vec)))
		mat[,which(TABLE_COL_NAMES==MED)]=round(median(vec,na.rm=TRUE),digits=NDIGITS)
		mat[,which(TABLE_COL_NAMES==MEAN)]=round(mean(vec,na.rm=TRUE),digits=NDIGITS)
		mat[,which(TABLE_COL_NAMES==MAX)]=round(max(vec,na.rm=TRUE),digits=NDIGITS)
		mat[,which(TABLE_COL_NAMES==MIN)]=round(min(vec,na.rm=TRUE),digits=NDIGITS)
		mat[,which(TABLE_COL_NAMES==VAR)]=round(var(vec,na.rm=TRUE),digits=NDIGITS)
		
		# non-zero stats
		if(length(nonZeroIndeces)>0)
		{
			mat[,which(TABLE_COL_NAMES==MED_NON_ZERO)]=round(median(vec[which(vec!=0)],na.rm=TRUE),digits=NDIGITS)
			mat[,which(TABLE_COL_NAMES==MEAN_NON_ZERO)]=round(mean(vec[which(vec!=0)],na.rm=TRUE),digits=NDIGITS)
		} else
		{
			mat[,which(TABLE_COL_NAMES==MED_NON_ZERO)]=NA
			mat[,which(TABLE_COL_NAMES==MEAN_NON_ZERO)]=NA
		}
	} else
	{
		# get ordering of unique categories
		if(is.factor(cats))
		{
			uCats=intersect(as.character(levels(cats)),unique(as.character(cats)))
			cats=as.character(cats)
		} else
		{
			uCats=sort(unique(as.character(cats)))
		}
					
		# initialize to calculate statistics over categories
		mat=matrix(NA,ncol=length(TABLE_COL_NAMES), nrow=length(uCats), dimnames=list(rownames=uCats,colnames=myColNames))
		mat=matrix(NA,ncol=length(TABLE_COL_NAMES), nrow=length(uCats), dimnames=list(rownames=uCats,colnames=myColNames))
		
		# calculate count
		amat=aggregate(vec~cats,FUN=numNotNa)
		aIndeces=match(uCats,amat[,1])
		mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==COUNT)]=amat[aIndeces[which(!is.na(aIndeces))],2]
		
		# calculate median over categories
		amat=aggregate(vec~cats,FUN=median, na.rm=TRUE)
		aIndeces=match(uCats,amat[,1])
		mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MED)]=round(amat[aIndeces[which(!is.na(aIndeces))],2],digits=NDIGITS)
		
		# calculate mean over categories
		amat=aggregate(vec~cats,FUN=mean, na.rm=TRUE)
		aIndeces=match(uCats,amat[,1])
		mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MEAN)]=round(amat[aIndeces[which(!is.na(aIndeces))],2],digits=NDIGITS)
		
		# calculate max over categories
		amat=aggregate(vec~cats,FUN=max, na.rm=TRUE)
		aIndeces=match(uCats,amat[,1])
		mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MAX)]=round(amat[aIndeces[which(!is.na(aIndeces))],2],digits=NDIGITS)
		
		# calculate min over categories
		amat=aggregate(vec~cats,FUN=min, na.rm=TRUE)
		aIndeces=match(uCats,amat[,1])
		mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MIN)]=round(amat[aIndeces[which(!is.na(aIndeces))],2],digits=NDIGITS)
		
		# calculate variance over categories
		amat=aggregate(vec~cats,FUN=var, na.rm=TRUE)
		aIndeces=match(uCats,amat[,1])
		mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==VAR)]=round(amat[aIndeces[which(!is.na(aIndeces))],2],digits=NDIGITS)
				
		if(length(nonZeroIndeces)>0)
		{
			# calculate median over non-zero elements
			amat=aggregate(vec[nonZeroIndeces]~cats[nonZeroIndeces],FUN=median, na.rm=TRUE)
			aIndeces=match(uCats,amat[,1])
			mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MED_NON_ZERO)]=round(amat[aIndeces[which(!is.na(aIndeces))],2],digits=NDIGITS)
			
			# calculate mean over non-zero elements
			amat=aggregate(vec[nonZeroIndeces]~cats[nonZeroIndeces],FUN=mean, na.rm=TRUE)
			aIndeces=match(uCats,amat[,1])
			mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MEAN_NON_ZERO)]=round(amat[aIndeces[which(!is.na(aIndeces))],2],digits=NDIGITS)
		} else
		{
			mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MED_NON_ZERO)]=NA
			mat[which(!is.na(aIndeces)),which(TABLE_COL_NAMES==MEAN_NON_ZERO)]=NA
		}
	}
	
	return(mat)
}

# get the user-friendly minute of the day label (hh:mm)
getMinLabel=function(vec)
{
	hour=sprintf("%02d",floor((vec-1)/60))
	minute=sprintf("%02d",(vec-1)%%60)
	formatted=apply(cbind(hour,minute),1,paste,collapse=":")
	return(formatted)
}

# descretize age by decade
discretizeAge=function(vec)
{
	# get decades for ages
	decades=floor(as.double(vec)/10)*10
	descritizedAge=as.character(decades)
	
	# set boundary cases
	descritizedAge[which(decades<0 | is.na(decades))]=UNKNOWN_STRING
	descritizedAge[which(decades<20 & decades>=0)]="<20"
	descritizedAge[which(decades>=100)]=">=100"
	
	# set non-boundary cases
	indeces=which(decades>=20 & decades<100)
	descritizedAge[indeces]=paste(decades[indeces],"s",sep="")
	
	return(descritizedAge)
}

# extract location name from location label
getLocName=function(loc)
{
	return(strsplit(loc,INSTITUTION_DELIM)[[1]][3])
}

# return indeces of visits that represent patient returns in less than nhour
returnedIndex=function(obj,nhour,returnInitial,rAdmit)
{
	# initialize
	tIndeces=c()

	# identify repeat patients
	ids=apply(cbind(obj$station,obj$psid),1,paste,collapse="-")
	tab=table(ids)
	returns=names(tab)[which(tab>1)]
	
	# for returns, get those returning in less than nhour
	if(length(returns)>0)
	{
		for(i in 1:length(returns))
		{	
			# get intervals
			rIndeces=which(ids==returns[i])
			intervals=as.double(obj$ti[rIndeces][2:length(rIndeces)])-as.double(obj$to[rIndeces][1:(length(rIndeces)-1)])
			
			# append visits that are quick returns
			if(rAdmit)
			{
				# require that return visit be an admit
				iIndeces=which(intervals<nhour*3600 & intervals>RETURN_HOUR_MIN*3600 & is.element(obj$disp[rIndeces[2:length(rIndeces)]],DISP_ADMIT))
			} else
			{
				# no requirements other than interval of time
				iIndeces=which(intervals<nhour*3600 & intervals>RETURN_HOUR_MIN*3600)
			}
			if(length(iIndeces)>0)
			{
				if(returnInitial)
				{
					# return index of initial visit in quick return
					tIndeces=append(tIndeces,rIndeces[iIndeces])
				} else
				{
					# return index of return visit in quick return
					tIndeces=append(tIndeces,rIndeces[iIndeces+1])
				}
			}
		}
	}
	
	return(tIndeces)
}

# return the fraction of the vector's elements that are NA
fracNA=function(vec)
{
	if(length(vec)==0)
	{
		return(NA)
	}
	return(length(which(is.na(vec)))/length(vec))
}

# remove rows and columns that are all NA
cleanTable=function(myTable, agg)
{
	# initialize
	cleaned=myTable
	
	# get rows that are all NA
	if(is.element(agg,MD))
	{
		if(ncol(myTable)>2)
		{
			mat=myTable[,2:ncol(myTable)]
		} else
		{
			mat=matrix(myTable[,2:ncol(myTable)],ncol=1,byrow=FALSE)
		}
		
	} else
	{
		if(ncol(myTable)>1)
		{
			mat=myTable
		} else
		{
			mat=matrix(myTable[,2:ncol(myTable)],ncol=1,byrow=FALSE)
		}
	}
	rNas=apply(mat,1,fracNA)
	rIndeces=which(rNas==1)
	
	# remove empty rows
	if(length(rIndeces)>0)
	{
		cleaned=myTable[-rIndeces,]
	}

	return(cleaned)
}

# get all federal holidays within a given time span
# start and end should be POSIXct timestamps#
getFedHolidays=function(start,end)
{
	# initialize
	holidays=list()
	start=as.POSIXct(start,tz=TZ)
	end=as.POSIXct(end,tz=TZ)

	# new year's day (date)
	holidays=as.character(getDateHoliday(month=1, day=1, start, end))
	
	# mlk (pattern)
	holidays=append(holidays, getPatternHoliday(month=1, week=3, dow=1, start, end))
	
	# washington's bday (pattern)
	holidays=append(holidays, getPatternHoliday(month=2, week=3, dow=1, start, end))
	
	# memorial day (pattern)
	holidays=append(holidays, getPatternHoliday(month=5, week=0, dow=1, start, end))
	
	# independence day (date)
	holidays=append(holidays, getDateHoliday(month=7, day=4, start, end))
	
	# labor day (pattern)
	holidays=append(holidays, getPatternHoliday(month=9, week=1, dow=1, start, end))
	
	# columbus day (pattern)
	holidays=append(holidays, getPatternHoliday(month=10, week=2, dow=1, start, end))
	
	# veterans day (date)
	holidays=append(holidays, getDateHoliday(month=11, day=11, start, end))
	
	# thanksgiving (pattern)
	holidays=append(holidays, getPatternHoliday(month=11, week=4, dow=4, start, end))
	
	# christmas (date)
	holidays=append(holidays, getDateHoliday(month=12, day=25, start, end))

	return(holidays)
}

# get holidays for a holiday that always occurs on a particular date
getDateHoliday=function(month, day, start, end)
{
	# initialize
	holidays=list()

	# get year of start and end
	sYear=as.double(format(start,"%Y"))
	eYear=as.double(format(end,"%Y"))
	
	# add all holidays within years
	holidays=as.POSIXct(paste(sYear:eYear,"-",sprintf("%02d",month),"-",sprintf("%02d",day),sep=""), tz=TZ)
	if(length(holidays)==0)
	{
		return(c())
	}
	
	# check for boundary years
	if(start>holidays[[1]])
	{
		if(length(holidays)==1)
		{
			return(c())
		}
		holidays=holidays[2:length(holidays)]
	}
	if(end<holidays[[length(holidays)]])
	{
		if(length(holidays)==1)
		{
			return(c())
		}
		holidays=holidays[1:(length(holidays)-1)]
	}

	# saturday and sunday exceptions
	mDows=as.double(format(holidays,"%w"))
	satIndeces=which(mDows==6)
	sunIndeces=which(mDows==0)
	if(length(satIndeces)>0)
	{
		for(i in 1:length(satIndeces))
		{
			holidays[[satIndeces[i]]]=holidays[[satIndeces[i]]]-24*3600
		}
	}
	if(length(sunIndeces)>0)
	{
		for(i in 1:length(sunIndeces))
		{
			holidays[[sunIndeces[i]]]=holidays[[sunIndeces[i]]]+24*3600
		}
	}
		
	return(as.character(holidays))
}

# get holidays for a holiday that occurs on the nth -day of every month
# month, week, dow should all be numeric
# dow=0 is Sunday, dow=6 is Saturday
# for last week of the month, use week=0
getPatternHoliday=function(month, week, dow, start, end)
{
	# initialize
	holidays=list()

	# get year of start and end
	sYear=as.double(format(start,"%Y"))
	eYear=as.double(format(end,"%Y"))
	
	# for each year
	for(y in sYear:eYear)
	{
		# calculate week dow of month
		mDates=as.POSIXct(paste(y,"-",sprintf("%02d",month),"-",c(1:numDayInMonth(y,month)),sep=""), tz=TZ)
		mDows=as.double(format(mDates,"%w"))
		indeces=which(mDows==dow)
		
		# get actual holiday
		if(week==0)
		{
			# last -day of the month
			holidays[[y-sYear+1]]=mDates[indeces[length(indeces)]]
		} else
		{
			# other -day of the month
			holidays[[y-sYear+1]]=mDates[indeces[week]]
		}
	}
		
	# check for boundary years
	if(start>holidays[[1]])
	{
		if(length(holidays)==1)
		{
			return(c())
		}
		holidays=holidays[2:length(holidays)]	
	}
	if(end<holidays[[length(holidays)]])
	{
		if(length(holidays)==1)
		{
			return(c())
		}

		holidays=holidays[1:(length(holidays)-1)]
	}
	
	# return holidays within time span
	return(unlist(lapply(holidays,as.character)))
}

# return the number of days in a month of a given year
numDayInMonth=function(year,month)
{
	nday=NDAY_PER_MONTH[month]
	
	# check for February in leap year
	if(month==2 && year%%4==0)
	{
		nday=29
	}
	
	return(nday)
}

# rearrange a vector so that the given value is last or first
arrangeUnknown=function(vec,uValue,uLast)
{
	# get location of uValue
	index=which(vec==uValue)
	
	# uValue not present in vector
	if(length(index)==0)
	{
		return(vec)
	}
	
	# return with uValue last
	if(uLast)
	{
		return(c(vec[-index],vec[index]))
	}
	
	# return with uValue first
	return(c(vec[index],vec[-index]))
}

# get default file name
defaultFileName=function(dateRange, metric, agg, dataType)
{
	if(dataType=="plot")
	{
		suffix="pdf"
	} else if (dataType=="table")
	{
		suffix="csv"
	} else
	{
		cat("Warning: Data type ", dataType, " is not accounted for in defaultFileName().  Assuming file with suffix txt.\n", sep="")
	}

	# format parameter choices
	sDate=gsub("-","", format(dateRange[1],format="%Y-%m-%d"))
	eDate=gsub("-","", format(dateRange[2],format="%Y-%m-%d"))
	aMetric=METRICS_ABBREV[metric]
	aAgg=AGG$AGG_ALL_ABBREV[agg]

	# concatenate and return
	filename=paste(sDate,eDate,aMetric,aAgg,"vived",suffix,sep=".")
	return(filename)
}




