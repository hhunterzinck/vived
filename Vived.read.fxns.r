##############################################################
# Description:  Functions that read files for VIVED.
# Usage: source(Vived.read.fxns.r)
# Author: Haley Hunter-Zinck
# Date: March 21, 2017
# Dependencies:
##############################################################

# read visit data information
readVisitData=function(efile)
{
	# initialize
	obj=list()
	myFormat=FORMAT_TIMESTAMP_STD

	# read edis data, in rds or csv format
	delim=","
	header=scan(efile, what="character", nlines=1,sep=delim, quiet=QUIET)
	mat=matrix(scan(efile, what="character", skip=1,sep=delim, quiet=QUIET), ncol=length(header), byrow=TRUE)
		
	# check for essential data elements
	if(length(intersect(header,ESSENTIALS))!=length(ESSENTIALS))
	{
		obj$psid=NA
		obj$ti=NA
		obj$to=NA
		return(obj)
	}
	
	# determine date format
	if(length(which(is.na(as.POSIXlt(mat[,which(header==TI)],format=myFormat,tz=TZ))))==nrow(mat))
	{
		myFormat=FORMAT_TIMESTAMP_ALT
	}
	
	# format essential data elements
	obj$psid=as.double(mat[,which(header==PATIENT_SID)])
	obj$ti=as.POSIXlt(mat[,which(header==TI)],format=myFormat,tz=TZ)
	obj$to=as.POSIXlt(mat[,which(header==TO)],format=myFormat,tz=TZ)
	
	# check for station 
	if(length(which(is.element(STATION,header)))>0)
	{
		# get real station number
		obj$station=as.double(mat[,which(header==STATION)])
	} else
	{
		# fill in fake station number
		obj$station=rep(1,nrow(mat))
	}
	
	# check for acuity
	if(length(which(is.element(ACUITY,header)))>0)
	{
		obj$acuity=mat[,which(header==ACUITY)]
		obj$acuity[which(obj$acuity=="NULL" | is.na(obj$acuity) | obj$acuity=="-1")]=UNKNOWN_STRING
	} else
	{
		obj$acuity=rep(UNKNOWN_STRING,nrow(mat))
	}
	
	# check for disposition
	if(length(which(is.element(DISP,header)))>0)
	{
		obj$disp=mat[,which(header==DISP)]
		obj$disp[which(obj$disp=="NULL" | is.na(obj$disp) | obj$disp=="-1")]=UNKNOWN_STRING
	} else
	{
		obj$disp=rep(UNKNOWN_STRING,nrow(mat))
	}
	
	# check for age
	if(length(which(is.element(AGE,header)))>0)
	{
		obj$age=discretizeAge(mat[,which(header==AGE)])
		obj$age[which(is.na(obj$age))]=UNKNOWN_STRING
	} else
	{
		obj$age=rep(UNKNOWN_STRING,nrow(mat))
	}
	
	# check for triage time
	if(length(which(is.element(TT,header)))>0)
	{
		obj$tt=as.POSIXlt(mat[,which(header==TT)],format=myFormat,tz=TZ)
	} else
	{
		obj$tt=NA
	}
	
	# check for time seen
	if(length(which(is.element(TS,header)))>0)
	{
		obj$ts=as.POSIXlt(mat[,which(header==TS)],format=myFormat,tz=TZ)
	} else
	{
		obj$ts=NA
	}
	
	# check for disposition time
	if(length(which(is.element(TD,header)))>0)
	{
		obj$tdisp=as.POSIXlt(mat[,which(header==TD)],format=myFormat,tz=TZ)
	} else
	{
		obj$tdisp=NA
	}
	
	# get mds as id numbers
	if(length(which(is.element(MA,header)))>0)
	{
		obj$md=mat[,which(header==MA)]
		obj$md[which(obj$md=="NULL" | is.na(obj$md) | obj$md=="-1")]=UNKNOWN_STRING
	} else
	{
		obj$md=rep(UNKNOWN_STRING,nrow(mat))
	}
	
	# determine bed request time from disposition admitted list
	obj$td=obj$tdisp
	obj$td[which(!is.element(obj$disp,DISP_ADMIT))]=NA
		
	return(obj)
}

# get list of unique providers at currently selected location
getProviders=function(obj)
{
	# read and extract unique providers
	#obj=readVisitData(efile)
	mds=sort(unique(obj$md))
	return(mds)
}

# get list of unique dispositions at currently selected location
getDispositions=function(obj)
{
	# read and extract unique providers
	#obj=readVisitData(efile)
	disps=sort(unique(obj$disp))
	return(disps)
}

# get list of unique acuity values at currently selected location
getAcuity=function(obj)
{
	# read and extract unique providers
	#obj=readVisitData(efile)
	acuity=sort(unique(obj$acuity), na.last=TRUE)
	return(acuity)
}

# get max and min time in for institution
#getDefaultDateRange=function(efile)
getDefaultDateRange=function(obj)
{
	# read data and get max and min time in
	#obj=readVisitData(efile)
	if(!is.na(obj$ti[1]))
	{
		dateRange=c(as.character(format(min(obj$ti,na.rm=TRUE),"%Y-%m-%d")),as.character(format(max(obj$ti,na.rm=TRUE),"%Y-%m-%d")))
	} else
	{
		dateRange=c("","")
	}
	return(dateRange)
}

# determine first date that has starts an interval of 30
# days where at least 20 of those days has at least one visit
getStartDate=function(dates)
{
	# initiate
	startDate=NA
	index=1

	# determine first 30 days span with visits on at least 20 of those days...
	tab=table(dates)
	while(is.na(startDate) && index<=length(tab))
	{
		# get month range
		t1=names(tab)[index]
		t2=as.character(as.POSIXct(names(tab)[index],tz=TZ)+30*24*3600)
		
		# determine number of dates with at least one visit in the range
		indeces=which(names(tab)>=t1 & names(tab)<=t2)
		if(length(indeces)>20)
		{
			startDate=t1
		}
		
		# update
		index=index+1
	}
	
	return(startDate)
}

