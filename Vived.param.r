##############################################################
# Description:  Constant parameters for VIVED.
# Usage: source("Vived.param.r")
# Author: Haley Hunter-Zinck
# Date: October 6, 2016
# Input: FILE_DISP_ADMIT
# Output: 
##############################################################

# debug
DEBUG=TRUE
QUIET=TRUE

# software
VERSION="0.5g"

# files
FILE_DISP_ADMIT="disp_admit.txt"

# messages
FORMAT_TIME="%a %b %d, %Y at %H:%M:%S"
FORMAT_TIMESTAMP_STD="%Y-%m-%d %H:%M:%S"
FORMAT_TIMESTAMP_ALT="%m/%d/%Y %H:%M"

# unknown data
UNKNOWN_STRING="Unknown"
UNKNOWN_INTEGER="-1"
UNKNOWN_COLOR="gray"

# metrics
NVISIT="Number of visits per day"
PVISIT="Percentage of visits per day (%)"
NBOARD_4PLUS="Number per day boarding > 4 hours"
PBOARD_4PLUS="Percentage per day boarding > 4 hours"
NRETURN72="Number per day returned < 72 hours"
PRETURN72="Percentage per day returned < 72 hours"
NRETURN72ADMIT="Number per day returned-admits < 72 hours"
PRETURN72ADMIT="Percentage per day returned-admits < 72 hours"
LOS="Length of stay (min)"
WAIT_IT="Door to triage (min)"
WAIT_IR="Waiting time: in-room (min)"
WAIT_IS="Door to doc (min)"
WAIT_ID="Door to disposition (min)"
WAIT_RS="Waiting time: room-seen (min)"
TREAT_SA="Treatment time: seen-decision (min)"
TREAT_SO="Treatment time: seen-out (min)"
BOARD_DO="Boarding time: disposition-out (min)"
BOARD_RO="Boarding time: request-out (min)"
BOARD_RA="Boarding time: request-assignment (min)"
BOARD_AO="Boarding time: assignment-out (min)"
OCC="Census per minute"
OCC_BY="Census per minute by..."
OCC_SPLIT="Census per minute by status"
OCC_ACUITY="Census per minute by acuity"
OCC_DISP="Census per minute by disposition"
OCC_AGE="Census per minute by age"
OCC_PROG="Census per minute by progression"
METRICS_OCC_SPLIT=c(OCC_SPLIT,OCC_ACUITY,OCC_DISP,OCC_AGE,OCC_PROG)
METRICS=list(Day=c(NVISIT, NBOARD_4PLUS,NRETURN72,NRETURN72ADMIT),
			DayPercent=c(PVISIT,PBOARD_4PLUS,PRETURN72,PRETURN72ADMIT),
			Visit=c(LOS,WAIT_IT,WAIT_IS,WAIT_ID,BOARD_DO),
			Census=c(OCC,OCC_ACUITY,OCC_AGE,OCC_DISP,OCC_PROG,OCC_SPLIT))
METRICS_ABBREV=setNames(c("nvisit","pvisit","los","board_do","occ",
							"occ_status","occ_acuity","occ_disp",
							"occ_age","door_triage","door_doc","door_disp",
							"nboard4plus","pboard4plus","nreturn72","preturn72",
							"nreturn72admit","preturn72admit","occ_prog"), 
						c(NVISIT,PVISIT,LOS,BOARD_DO,OCC
							,OCC_SPLIT,OCC_ACUITY,OCC_DISP,
							OCC_AGE,WAIT_IT,WAIT_IS,WAIT_ID,
							NBOARD_4PLUS,PBOARD_4PLUS,NRETURN72,PRETURN72,
							NRETURN72ADMIT,PRETURN72ADMIT,OCC_PROG))

# aggregation views
HIST="Histogram"
YEAR="Year"
YM="Month"
MONTH="Month of the year"
WEEK="Week"
DATE="Day"
DOW="Day of the week"
HOUR="Hour of the day"
HD="Hour of the day by day of the week"
MD="Minute of the day by day of the week"
BY_STATUS="Status"
BY_ACUITY="Acuity"
BY_DISP="Disposition"
BY_AGE="Age"
BY_PROG="Progression"
BY_PROV="Provider"
AGG=list()
AGG$AGG_ALL=list(Summary=c(HIST,""), 
					Temporal=c(YEAR, YM, MONTH, WEEK, DATE, DOW, HOUR, MD),
					Categorical=c(BY_ACUITY,BY_DISP, BY_PROV))
AGG$AGG_ALL_ABBREV=setNames(c("hist","","year","ym","month","week","date","dow","hour","md","acuity","disp","prov"),unlist(AGG$AGG_ALL))
AGG$AGG_DAY=list(Summary=c(HIST,""), 
					Temporal=c(YEAR, YM, MONTH, WEEK, DATE, DOW, HOUR))
AGG$AGG_VISIT=list(Summary=c(HIST,""),  
					Temporal=c(YEAR, YM, MONTH, WEEK, DATE, DOW, HOUR),
					Categorical=c(BY_ACUITY,BY_DISP, BY_PROV))
AGG$AGG_MIN=list(Summary=c(HIST,""),  
					Temporal=c(YEAR, YM, MONTH, WEEK, DATE, DOW, HOUR, MD))
AGG$AGG_OCC_BY=list(Split=c(BY_STATUS, BY_ACUITY, BY_DISP, BY_AGE,BY_PROG))
AGG$AGG_OCC_SPLIT=list(Summary=c(HIST,""),  
					Temporal=c(YEAR, YM, MONTH, WEEK, DATE, DOW, HOUR, MD))

# reports
SUMMARY="Summary"
PROVIDERS="Providers"
DISPOSITIONS="Dispositions"
REPORTS=c(SUMMARY,PROVIDERS)
R_TVISIT="Total visits"
R_NDAY="Number of days with visits"
R_NVISIT="Number of visits per day"
R_ADMIT="Patients admitted"
R_NADMIT="Number admitted"
R_PADMIT="Percentage admitted"
R_BOARD_4PLUS="Boarding > 4 hours"
R_NBOARD_4PLUS="Number boarding > 4 hours"
R_PBOARD_4PLUS="Percentage boarding > 4 hours"
R_RETURN72="Returned < 72 hours"
R_RETURN72ADMIT="Returned-admits < 72 hours"
R_NRETURN72="Number returned < 72 hours"
R_PRETURN72="Percentage returned < 72 hours"
R_NRETURN72ADMIT="Number returned-admits < 72 hours"
R_PRETURN72ADMIT="Percentage returned-admits < 72 hours"
R_LOS="LOS"
R_LOS_HOME="LOS (not admitted)"
R_LOS_ADMIT="LOS (admitted)"
R_MAX_OCC="Median of maximum occupancy per day"
R_NVISIT_ACUITY=paste("Visits (ESI ",c(1:5), ")", sep="")
R_NADMIT_ACUITY=paste("Admitted (ESI ",c(1:5), ")", sep="")
R_PADMIT_ACUITY=paste("Percentage admitted (ESI ",c(1:5), ")", sep="")
R_LOS_ACUITY=paste("LOS (ESI ",c(1:5), ")", sep="")
REPORT_SUMMARY_METRICS=c(R_TVISIT,R_NDAY,R_NVISIT,
	R_ADMIT, R_BOARD_4PLUS, R_RETURN72,R_RETURN72ADMIT,
	R_LOS, R_LOS_ADMIT, R_LOS_HOME,
	WAIT_IT, WAIT_IS, WAIT_ID, BOARD_DO,
	R_MAX_OCC,
	R_NVISIT_ACUITY,R_NADMIT_ACUITY, R_LOS_ACUITY)
REPORT_PROVIDER_METRICS=c(R_TVISIT, R_NVISIT, 
	R_PADMIT,  
	R_NRETURN72, R_PRETURN72, 
	R_NRETURN72ADMIT, R_PRETURN72ADMIT, 
	WAIT_IS, WAIT_ID, 
	R_LOS_HOME, R_LOS_ADMIT, 
	R_PADMIT_ACUITY, R_LOS_ACUITY)

# time of day
HOURS_LABELS=paste(sprintf("%02d",c(0:24)),":00",sep="")
HOURS=sprintf("%02d",c(0:23))
MINUTES=sprintf("%02d",c(0:59))
HOUR_MINUTES=c()
for(i in 1:length(HOURS))
{
	HOUR_MINUTES=append(HOUR_MINUTES,paste(HOURS[i],MINUTES,sep=":"))
}

# disposition
ALL_DISP="All"
ALL_ADMIT="All admitted"
ALL_NOT_ADMIT="All not admitted"
DISP_ADMIT=scan(FILE_DISP_ADMIT,what="character", quiet=QUIET)

# metric maps creating levels in ggplot2
MAP_ACUITY=setNames(c("1","2","3","4","5","Unknown"), c("1","2","3","4","5","Unknown"))
MAP_AGE=setNames(c(UNKNOWN_STRING,"<20","20s","30s","40s","50s","60s","70s","80s","90s",">=100"),
	c(UNKNOWN_STRING,"<20","20s","30s","40s","50s","60s","70s","80s","90s",">=100"))
MAP_PROG=setNames(c("In - triage","Triage - doc","Doc - disp","Disp - out"),c("it","ts","sd","dop"))
MAP_STATUS=rev(setNames(c("Home", "Admit before disposition", "Admit after disposition","Boarding > 4h"),c("io","id","do","bo")))
MAP_ALL_OCC=c(MAP_ACUITY, MAP_AGE,MAP_PROG, MAP_STATUS)

# preference tab defaults
TZ="GMT"
LOS_MAX=20*60
BOARD_MAX=20*60
TRIAGE_MAX=4*60
SEEN_MAX=7*60
DISP_MAX=14*60
BOARD_THRESH=4*3600
RETURN_HOUR_MIN=6
RETURN_HOUR_MAX=72
DOW_START="Mon"
TZ="GMT"
SEASON_SPRING="March"
SEASON_SUMMER="June"
SEASON_FALL="September"
SEASON_WINTER="December"
INTERVAL_MAX=setNames(c(TRIAGE_MAX,SEEN_MAX-TRIAGE_MAX,DISP_MAX-SEEN_MAX,BOARD_MAX,NA),c("ti","tt","ts","tdisp","to"))

# column headers
STATION="Station"
PATIENT_SID="PatientID"
TI="TimeIn"
TO="TimeOut"
TR="BedRequest"
TA="BedAssng"
TD="TimeDisposition"
ACUITY="Acuity"
MA="MdAssigned"
TM="TimeMdAssigned"
DISP="Disposition"
AGE="AgeAtVisit"
TT="TimeAcuity"
TS="TimeSeen"
ITI="AdmitDateTime"
ITO="DischargeDateTime"
IWARD="WardLocationName"
ILOC="PrimaryLocation"
IBSEC="BedSection" 
INSTITUTION_NAME="InstitutionName"
INSTITUTION_SID="InstitutionSID"
INSTITUTION_STATION="Sta3n"
COL_NBED="ED_UCC_OP_BEDS"
COL_LOC="InstitutionName"
ESSENTIALS=c(PATIENT_SID, TI, TO)

# constants
LABEL_DOW=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
LABEL_HOUR=paste(c(paste("0",c(0:9),sep=""),c(10:23)),":00", sep="")
LABEL_HD=paste(c(paste("0",c(0:9),sep=""),c(10:23)),":00", sep="")
LABEL_MONTH=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
LABEL_STATUS=c("io","ir","ra","ao")
START_DAY=6
END_DAY=18
ACUITY_MAP=setNames(c("1","2","3","4","5"),c("edp.acuity.esi1","edp.acuity.esi2","edp.acuity.esi3","edp.acuity.esi4","edp.acuity.esi5"))
ALL_MD="All"
FILL_STATION="FILL_STATION"
FILL_ISID="FILL_INSTITUTION_SID"
INSTITUTION_DELIM=" - "
NDAY_PER_MONTH=c(31,28,31,30,31,30,31,31,30,31,30,31)

# limits
DATE_LIMIT=30
NBED=12

# acuity
ALL_ACUITY="All"
CHOICES_ACUITY=c(ALL_ACUITY,as.character(ACUITY_MAP))

# plotting parameters for both
COLOR_BARS=rev(c("blue","cyan","gold","red"))
COLOR_ACUITY=setNames(c("red","orange","yellow","green","blue",UNKNOWN_COLOR), c("1","2","3","4","5",UNKNOWN_STRING))
COLOR_PROG=c("green","purple","gray","black")
COLOR_AGE=setNames(c(brewer.pal(9,"GnBu"),"black",UNKNOWN_COLOR),c("10","20","30","40","50","60","70","80","90","100","-1"))
PAL_DISP="Set3"
PAL_DISP_BACKUP="Dark2"
PAL_ACUITY="Spectral"
PAL_ACUITY_BACKUP="Set3"
PAL_AGE="GnBu"
PAL_AGE_BACKUP="Purples"

# plotting parameters for ggplot2
X_ANGLE=55
HJUST=1
VJUST=1
NOTCH=FALSE
SIZE_AXIS_TEXT=14
SIZE_AXIS_TEXT_SMALL=8
SIZE_AXIS_TITLE=16
SIZE_AXIS_TITLE_SMALL=8
OCC_COLORS=c("green","gold","red")
OCC_BREAKS=c(-Inf,12,15,Inf)
NBREAK=25
NBIN=50
#NBIN=30
BREWER_NCOLOR_MIN=3

# plotting parameters for base plotting package
MAGNIFY=1.25
MARGIN_HEATMAP=c(7,5)
MARGIN_DEFAULT=c(5, 4, 4, 2) + 0.1
MARGIN_BOXPLOT=MARGIN_DEFAULT+c(1,0,0,0)
MARGIN_LINE=MARGIN_DEFAULT+c(1,0,0,0)
MARGIN_SMALL=c(2,4,2,4)
MARGIN_NONE=c(0,0,0,0)
LINE_COLOR="red"
LINE_TYPE=2

# table parameters
MED="Median"
MED_NON_ZERO="Non-zero median"
MEAN="Mean"
MEAN_NON_ZERO="Non-zero mean"
MAX="Maximum"
MIN="Minimum"
VAR="Variance"
COUNT="Count"
TABLE_COL_NAMES=c(COUNT,MED,MED_NON_ZERO,MEAN,MEAN_NON_ZERO,MIN,MAX,VAR)
NDIGITS=2