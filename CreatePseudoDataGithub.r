##############################################################
# Description:   Create pseudo-data for the VIVED app.
# Usage: R -f CreatePseudoDataGithub.r --args <output_prefix> <start> <end> <frac_missing>
# Author: Haley Hunter-Zinck
# Date: April 28, 2017
# Dependencies:
# Input: optional: .params
# Output: .edis.csv
##############################################################

# user input
args=commandArgs(trailingOnly=TRUE)
if(length(args)!=4)
{
	print("Usage: R -f CreatePseudoDataGithub.r --args <output_prefix> <start> <end> <frac_missing>")
	q(save="no")
}
# args=c("example_dataset", "3001-01-01", "3003-12-31","0.05")

# get start time
t1Total=as.double(format(Sys.time(),"%s"))

# packages

# parameters
outprefix=args[1]
start=as.POSIXct(args[2])
end=as.POSIXct(args[3])
fmiss=as.double(args[4])

# files
pfile=paste(outprefix,".params", sep="")
eOutfile=paste(outprefix,".edis.csv", sep="")

# constants
ADMIT=c("Admit", "Telemetry", "ICU", "Psych")
ADMIT_PROB=c(0.7,0.1,0.1,0.1)
DISCHARGE=c("Home", "Eloped", "AMA")
DISCHARGE_PROB=c(0.98,0.01,0.01)
ACUITY=c(1:5)
ACUITY_PROB=c(0.01,0.1,0.69,0.1,0.1)
PROVIDERS=c("Provider1", "Provider2", "Provider3", "Provider4")
PSIDS=c(1:10000)
WARD=paste("Ward",c(1:50),sep="")
LOC=sort(rep(paste("Location",c(1:5),sep=""),5))
STATION=3

# set parameters to default or given a parameter file
setParam=function(file)
{
	# initialize
	params=list()	

	if(!file.exists(file))
	{
		# default parameters for edis
		params$intervalMean=30
		params$fracAdmit=0.3
		params$ttMean=5
		params$tsMean=15
		params$tdMean=60
		params$maxProb=1
		params$ageMean=65
		params$ageSd=15
		
		# default parameters for inpat
		params$inpatIntervalMean=20
		params$itoMean=2*24*60
		
	} else
	{
		params=readRDS(file)
	}
	return(params)
}

# probability that visit should be removed
probRemove=function(x,maxProb=1)
{
	scaled=maxProb/720^2
	return(scaled*(x-1440/2)^2)
}

# read params
params=setParam(pfile)


################### EDIS #######################

# create time in for all visits
totalMin=(as.double(end+24*3600-1)-as.double(start))/60
intervals=rexp(ceiling(totalMin/params$intervalMean), rate=1/params$intervalMean)
ti=as.POSIXct(start+cumsum(intervals)*60)

# remove ti if beyond data range
indeces=which(ti>=end+24*3600)
if(length(indeces)>0)
{
	ti=ti[-indeces]
} 

# remove ti with probability a function of time of day
tiMin=as.double(format(ti,"%H"))*60+as.double(format(ti,"%M"))
rIndeces=which(runif(length(ti))<probRemove(tiMin%%1440,maxProb=params$maxProb))
ti=ti[-rIndeces]

# get dispositions
disp=rep(NA,length(ti))
iAdmit=sample(c(1:length(ti)),round(params$fracAdmit*length(ti)))
disp[iAdmit]=sample(ADMIT,length(iAdmit),replace=TRUE,prob=ADMIT_PROB)
disp[-iAdmit]=sample(DISCHARGE,length(disp)-length(iAdmit),replace=TRUE, prob=DISCHARGE_PROB)

# get providers
md=sample(PROVIDERS,length(ti),replace=TRUE)

# get acuity
acuity=sample(ACUITY,length(ti), replace=TRUE, prob=ACUITY_PROB)

# get age
ageAtVisit=round(rnorm(length(ti),mean=params$ageMean, sd=params$ageSd))

# create time triage
intervals=rexp(length(ti), rate=1/params$ttMean)
tt=as.POSIXct(ti+intervals*60)

# create time seen
intervals=rexp(length(ti), rate=1/params$tsMean)
ts=as.POSIXct(tt+intervals*60)

# create time disposition
intervals=rexp(length(ti), rate=1/params$tdMean)
td=as.POSIXct(ts+intervals*60)

# create time out (time betwen disp and to should be longer)
intervals=rexp(length(ti), rate=1/params$tdMean)
to=as.POSIXct(td+intervals*60)
to[iAdmit]=as.POSIXct(to[iAdmit]+intervals[iAdmit]*60)

# setup for writing
header=c("VisitId","Station","PatientID","TimeIn","TimeOut",
	"TimeDisposition","MdAssigned","Acuity","Disposition",
	"AgeAtVisit","TimeAcuity","TimeSeen")
towrite=cbind(c(1:length(ti)),rep(STATION,length(ti)),sample(PSIDS,length(ti), replace=TRUE),format(ti), format(to),
	format(td), md, acuity,disp,
	ageAtVisit, format(tt), format(ts))
	
# incorporate missingness if required
if(fmiss>0)
{
	indeces=sample(c(1:length(towrite)),length(towrite)*fmiss)
	towrite[indeces]=NA
}
	
# write to file
towrite=rbind(header,towrite)
write(t(towrite),sep=",", file=eOutfile, ncolumns=ncol(towrite))

# get end time
t2Total=as.double(format(Sys.time(),"%s"))

# debug
printSumm=function()
{
    cat("#############################################\n")
	cat("Outfile: ",eOutfile,"\n",sep="")
    cat("Runtime: ",t2Total-t1Total," s\n",sep="")
    cat("#############################################\n")
}
printSumm()
