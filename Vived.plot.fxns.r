##############################################################
# Description:   Plotting functions for VIVED.
# Usage: source("Vived.plot.fxns.r")
# Author: Haley Hunter-Zinck
# Date: October 6, 2016
# Dependencies: ggplot2, reshape, RColorBrewer
##############################################################

# plot text message on an empty ggplot2 
plotInvalid=function(msg)
{
	# with ggplot2
	df=data.frame(msg=msg, x=1, y=1)
	myPlot=ggplot(df, aes(x=x, y=y)) +
		geom_point(color="white") +
		annotate("text", x=1, y=1, label=msg) +
		theme_bw()
	
	# format for return
	plotted=list()
	plotted$plot=myPlot
	plotted$table=matrix(NA,ncol=length(TABLE_COL_NAMES), nrow=1, dimnames=list(rownames=c(),colnames=TABLE_COL_NAMES))
	plotted$myMsg=""

	return(plotted)
}

# plot a histogram with ggplot2
plotHistogram=function(vec, xlab, ylab="Count")
{
	# make sure numeric and not NA
	myPlot=NULL
	vec=as.double(vec[which(!is.na(vec))])
	
	# calculate bin width
	if(length(unique(vec))==1)
	{
		myBinWidth=NULL
	} else if(length(unique(vec))<NBIN)
	{
		myBinWidth=1
	} else
	{
		if(identical(floor(vec),vec))
		{
			myBinWidth=floor((max(vec)-min(vec))/NBIN)
		} else
		{
			myBinWidth=(max(vec)-min(vec))/NBIN
		}
	} 
	
	# with ggplot2
	df=data.frame(data=vec)
	myPlot=ggplot(df,aes(x=data)) +
		geom_histogram(binwidth=myBinWidth) +
		ylab(ylab) +
		xlab(xlab) +
		theme_bw() +
		theme(
			axis.text.x=element_text(size=SIZE_AXIS_TEXT), 
			axis.text.y=element_text(size=SIZE_AXIS_TEXT),
			axis.title.x=element_text(size=SIZE_AXIS_TITLE),
			axis.title.y=element_text(size=SIZE_AXIS_TITLE))
	
	return(myPlot)
}

# plot multiple density curves
plotMultiDensity=function(occ, xlab, ylab="Density", myParams)
{
	# initialize
	occFormatted=occ
	
	# set rownames if necessary
	if(length(names(myParams$myLevels))>0)
	{
		names(occFormatted)=myParams$myLevels[names(occ)]
	}

	# plot with ggplot2
	df=melt(occFormatted,as.is=TRUE)
	df$L1=factor(df$L1,levels=myParams$myLevels)
	myPlot=ggplot(df,aes(x=value,colour=L1)) +
		geom_density(size=2) +
		labs(y=ylab, x=xlab, colour="") +
		scale_colour_manual(values=myParams$myColors)+
		theme_bw() +
		theme(
			axis.text.x=element_text(size=SIZE_AXIS_TEXT), 
			axis.text.y=element_text(size=SIZE_AXIS_TEXT),
			axis.title.x=element_text(size=SIZE_AXIS_TITLE),
			axis.title.y=element_text(size=SIZE_AXIS_TITLE))
	
	return(myPlot)
}

# plot line with given data
plotLine=function(vec, labels, xlab="", ylab)
{
	# prep data to plot
	myPlot=NULL
	indeces=which(!is.na(vec))
	vec=as.double(vec[indeces])
	labels=factor(as.character(labels[indeces]))

	# setup labeling breaks
	uLabel=sort(unique(labels))
	if(length(unique(labels))>NBREAK)
	{
		byNum=ceiling(length(uLabel)/NBREAK)
		labelBreaks=as.character(uLabel[seq(from=1,to=length(uLabel), by=byNum)])
	} else
	{
		labelBreaks=uLabel
	}

	# plot line with ggplot2
	df=data.frame(x=labels, y=vec)
	myPlot=ggplot(df, aes(x=x, y=y, group=1)) +
		geom_point() +
		geom_line() +
		geom_hline(yintercept=median(vec), col=LINE_COLOR, linetype=2) +
		labs(x=xlab, y=ylab) +
		scale_x_discrete(breaks=labelBreaks) +
		theme_bw() +
		theme(
			axis.text.x=element_text(size=SIZE_AXIS_TEXT,angle=X_ANGLE, hjust=HJUST), 
			axis.text.y=element_text(size=SIZE_AXIS_TEXT),
			axis.title.x=element_text(size=SIZE_AXIS_TITLE),
			axis.title.y=element_text(size=SIZE_AXIS_TITLE))
				
	return(myPlot)
}

# plot a boxplot with ggplot2
plotBoxplot=function(values, labels, ylab, xlab="",threshold=NA)
{
	# make sure numeric and not NA
	myPlot=NULL
	indeces=which(!is.na(values) & !is.na(labels))
	values=as.double(values[indeces])
	labels=labels[indeces]
	
	# set threshold
	if(is.na(threshold))
	{
		threshold=median(values)
	} 

	# setup labeling breaks
	uLabel=sort(unique(labels))
	if(length(unique(labels))>NBREAK)
	{
		byNum=ceiling(length(uLabel)/NBREAK)
		labelBreaks=as.character(uLabel[seq(from=1,to=length(uLabel), by=byNum)])
	} else
	{
		labelBreaks=uLabel
	}

	# boxplot with ggplot2
	df=data.frame(labels=labels, values=values)
	myPlot=ggplot(df,aes(x=labels,y=values)) +
		geom_boxplot(notch=NOTCH, outlier.shape=1) +
		geom_hline(yintercept=threshold, col=LINE_COLOR, linetype=2) +
		stat_summary(fun.y=mean, geom="point", show.legend=FALSE) +
		labs(x=xlab, y=ylab) +
		scale_x_discrete(breaks=labelBreaks) +
		theme_bw() +
		theme(
			axis.text.x=element_text(size=SIZE_AXIS_TEXT,angle=X_ANGLE, hjust=HJUST), 
			axis.text.y=element_text(size=SIZE_AXIS_TEXT),
			axis.title.x=element_text(size=SIZE_AXIS_TITLE),
			axis.title.y=element_text(size=SIZE_AXIS_TITLE))
			
	return(myPlot)
}

# plot a heatmap with ggplot2
plotHeatmap=function(melted, myBreaks="", myColors="", label="", xlab="", ylab="")
{
	# initialize
	myPlot=NULL

	if(myBreaks[1]=="")
	{
		# plot continuous graident by default
		myPlot=ggplot(melted, aes(x=dow, y=hour)) +
			geom_tile(aes(fill=value, width=0.9, height=0.9)) +
			scale_fill_gradient(low="white",high="steelblue") +
			labs(fill=label, x=xlab, y=ylab) +
			theme_bw() +
			theme(
				axis.ticks=element_blank(),
				panel.grid.major=element_blank(),
				panel.grid.minor=element_blank(),
				axis.text.x=element_text(size=SIZE_AXIS_TEXT,angle=X_ANGLE, hjust=HJUST),
				axis.title.x=element_text(size=SIZE_AXIS_TITLE),
				axis.title.y=element_text(size=SIZE_AXIS_TITLE),
				legend.text=element_text(size=SIZE_AXIS_TITLE),
				legend.title=element_text(size=SIZE_AXIS_TITLE))
	} else
	{
		# plot custom color gradient
		fillValue=cut(melted$value, breaks=myBreaks, right=FALSE)
		melted$fillValue=fillValue
		myPlot=ggplot(melted, aes(x=dow, y=hour)) +
			geom_tile(aes(fill=fillValue, width=0.9, height=0.9)) +
			scale_fill_manual(breaks=levels(fillValue), values=myColors) +
			labs(fill=label, x=xlab, y=ylab) +
			theme_bw() +
			theme(
				axis.ticks=element_blank(),
				panel.grid.major=element_blank(),
				panel.grid.minor=element_blank(),
				axis.text.x=element_text(size=SIZE_AXIS_TEXT,angle=X_ANGLE, hjust=HJUST),
				axis.title.x=element_text(size=SIZE_AXIS_TITLE),
				axis.title.y=element_text(size=SIZE_AXIS_TITLE),
				legend.text=element_text(size=SIZE_AXIS_TITLE),
				legend.title=element_text(size=SIZE_AXIS_TITLE))
	}
	
	return(myPlot)
}

# plot multiple line plots on 
plotMultiLine=function(mat, nbed, label="", xlab="", ylab="", xlimits=c(0,24))
{
	# intialize
	myPlot=NULL

	# get limits
	ylimits=c(min(mat, na.rm=TRUE), max(nbed,max(mat, na.rm=TRUE)))
				
	# get side panel labels
	sideLabels=c()
	if(length(intersect(rownames(mat),LABEL_DOW))>0)
	{
		sideLabels=LABEL_DOW
	} else
	{
		sideLabels=sort(rownames(mat))
	}
	
	# create x-axis hour labels
	xTickBreaks=seq(from=0,to=24, by=1)*60
	xTickLabels=HOURS_LABELS[xTickBreaks/60+1]

	# plot all facets
	melted=melt(mat)
	melted$X2=melted$X2
	melted$X1=factor(melted$X1, levels=sideLabels)
	myPlot=ggplot(melted, aes(x=X2, y=value)) + 
		geom_line() +
		labs(x=xlab, y=ylab) +
		scale_x_continuous(limits=xlimits, breaks=xTickBreaks, labels=xTickLabels) +
		scale_y_continuous(limits=ylimits) +
		theme_bw() +
		facet_grid(X1~.) +
		theme(
			strip.text.y = element_text(size = SIZE_AXIS_TITLE_SMALL),
			axis.title.x=element_text(size=SIZE_AXIS_TITLE),
			axis.text.x=element_text(size=SIZE_AXIS_TEXT,angle=X_ANGLE, hjust=HJUST),
			axis.title.y=element_text(size=SIZE_AXIS_TITLE),
			axis.text.y=element_text(size=SIZE_AXIS_TITLE_SMALL))
			
	# only display line if i have a valid number of beds
	if(!is.na(nbed))
	{
		myPlot=myPlot+geom_hline(yintercept=nbed, col=LINE_COLOR, linetype=2) 
	} 
	
	return(myPlot)
}

# plot multiple bar plots
plotMultiBar=function(myList, label="", xlab="", ylab="", myParams, xlimits=c(0,24*60), metric)
{
	# intialize
	myPlot=NA
	myColors=myParams$myColors
	myLevels=myParams$myLevels
	nbed=myParams$nbed

	# get limits
	totalMat=myList[[1]]
	if(length(myList)>1)
	{
		for(i in 2:length(myList))
		{
			totalMat=totalMat+myList[[i]]
		}
	}
	ylimits=c(0, max(max(totalMat, na.rm=TRUE),nbed,na.rm=TRUE))
	
	# create x-axis hour labels
	xTickBreaks=seq(from=0,to=24, by=2)*60
	xTickLabels=HOURS_LABELS[xTickBreaks/60+1]
		
	# get all plots
	melted=c()
	for(i in 1:length(myList))
	{
		# gather relevant rows across types
		meltedType=melt(myList[[i]])
		if(length(names(myLevels))==0)
		{
			catLabel=myLevels[i]
		} else
		{
			catLabel=myLevels[names(myList)[i]]
		}
		meltedType$Category=factor(rep(catLabel,length(meltedType$value)),levels=myLevels)
		melted=rbind(melted, meltedType)
	}
	
	# get side panel labels
	sideLabels=c()
	if(length(setdiff(unique(melted$X1),LABEL_DOW))==0)
	{
		sideLabels=LABEL_DOW
	} else
	{
		sideLabels=sort(unique(melted$X1))
	}

	# gather all plots to arrange later
	melted$X2=melted$X2
	melted$X1=factor(melted$X1, levels=sideLabels)
	myPlot=ggplot(melted, aes(x=X2, y=value, fill=Category)) + 
			geom_bar(stat="identity",linetype=0, width=1) +
			labs(x=xlab, y=ylab, fill="") +
			scale_fill_manual(values=myColors) +
			scale_x_continuous(limits=xlimits, breaks=xTickBreaks, labels=xTickLabels) +
			scale_y_continuous(limits=ylimits) +
			theme_bw() +
			facet_grid(X1~.) +
			theme(
				strip.text.y = element_text(size = SIZE_AXIS_TITLE_SMALL),
				axis.title.x=element_text(size=SIZE_AXIS_TITLE),
				axis.text.x=element_text(size=SIZE_AXIS_TEXT,angle=X_ANGLE, hjust=HJUST),
				axis.title.y=element_text(size=SIZE_AXIS_TITLE),
				axis.text.y=element_text(size=SIZE_AXIS_TITLE_SMALL)
	)
	
	# only display line if i have a valid number of beds
	if(!is.na(nbed))
	{
		myPlot=myPlot+geom_hline(yintercept=nbed, col=LINE_COLOR, linetype=2) 
	} 

	return(myPlot)
}

# given a matrix, plot stacked barplot where each row is a different color bar in each stack
# columns represent a different bar
plotStackedBar=function(mat, xlab="", ylab, myParams, nbedLine=TRUE)
{
	# set rownames if necessary
	if(length(names(myParams$myLevels))>0)
	{
		rownames(mat)=myParams$myLevels[rownames(mat)]
	}

	# organize data to plot for ggplot2
	melted=melt(mat,as.is=TRUE,varnames=c("type","dates"))
	melted$type=factor(melted$type,levels=myParams$myLevels)
	melted$dates=factor(melted$dates, levels=colnames(mat))
	
	# get limits
	ylimits=c(0, max(max(colSums(mat), na.rm=TRUE),myParams$nbed))
	
	# setup labeling breaks
	labels=melted$dates
	uLabel=sort(unique(labels))
	if(length(unique(labels))>NBREAK)
	{
		byNum=ceiling(length(uLabel)/NBREAK)
		labelBreaks=as.character(uLabel[seq(from=1,to=length(uLabel), by=byNum)])
	} else
	{
		labelBreaks=uLabel
	}

	# plot stacked barplot
	myPlot=ggplot(melted,aes(x=dates,y=value,fill=type)) +
		geom_bar(stat="identity") +
		scale_fill_manual(values=myParams$myColors) +
		labs(x=xlab, y=ylab, fill="") +
		scale_x_discrete(breaks=labelBreaks) +
		scale_y_continuous(limits=ylimits) +
		theme_bw() +
		theme(
			axis.title.x=element_text(size=SIZE_AXIS_TITLE),
			axis.text.x=element_text(size=SIZE_AXIS_TEXT,angle=X_ANGLE, hjust=HJUST),
			axis.title.y=element_text(size=SIZE_AXIS_TITLE),
			axis.text.y=element_text(size=SIZE_AXIS_TITLE)
			#, legend.position = "right",  legend.box = "vertical"
			)
			
	# only display line if i have a valid number of beds
	if(!is.na(myParams$nbed) && nbedLine)
	{
		myPlot=myPlot+geom_hline(yintercept=myParams$nbed, col=LINE_COLOR, linetype=2) 
	} 

	return(myPlot)
}

# return colors, labels, etc for plotting a particular metric
getPlottingParameters=function(locName, metric, obj)
{
	# initialize
	params=list()
	pal="Set3"
	
	# get number of beds
	params$nbed=getNumBed(locName)	
	
	# set colors and levels by metric
	if(metric==OCC_SPLIT)
	{
		#params$myColors=COLOR_BARS[1:3]
		params$myColors=COLOR_BARS[1:4]
		params$myLevels=MAP_STATUS
		params$msg=""
	} else if(metric==OCC_ACUITY)
	{
		# get colors, national dispositions should be consistent
		uAcuity=arrangeUnknown(sort(unique(obj$acuity)),uValue=UNKNOWN_STRING,uLast=FALSE)
		myColors=assignColorFromPal(PAL_ACUITY,PAL_ACUITY_BACKUP,uAcuity)
		
		params$myColors=myColors
		params$myLevels=uAcuity
		params$msg=""
	} else if(metric==OCC_DISP)
	{
		# get colors, national dispositions should be consistent
		uDisp=names(sort(table(obj$disp),decreasing=FALSE,na.last=FALSE))
		uDisp=arrangeUnknown(uDisp,uValue=UNKNOWN_STRING,uLast=FALSE)
		myColors=assignColorFromPal(PAL_DISP,PAL_DISP_BACKUP,uDisp)
					
		# store in parameter object
		params$myColors=myColors
		params$myLevels=uDisp
		params$msg=""
	} else if(metric==OCC_AGE)
	{	
		# get colors, national dispositions should be consistent
		uAge=as.character(sort(factor(unique(obj$age),levels=as.character(MAP_AGE)),decreasing=TRUE))
		uAge=arrangeUnknown(uAge,uValue=UNKNOWN_STRING,uLast=FALSE)
		myColors=assignColorFromPal(PAL_AGE,PAL_AGE_BACKUP,uAge, reverse=TRUE)

		# green-blue gradient
		params$myColors=myColors
		params$myLevels=uAge
		
		# no message
		params$msg=""
	} else if(metric==OCC_PROG)
	{
		params$myColors=rev(COLOR_PROG)
		params$myLevels=rev(MAP_PROG)
		params$msg=""
	}
	
	# return parameters
	return(params)
}

# given a default palette and backup palette, get colors
# for unique values
assignColorFromPal=function(pal,palBackup,values,reverse=FALSE)
{
	# initialize
	myColors=rep(NA,length(values))

	# get number of colors
	ncolor=brewer.pal.info[pal,1]
	ncolorBackup=brewer.pal.info[PAL_DISP_BACKUP,1]
	
	# account for unknown values
	index=which(values==UNKNOWN_STRING | values==UNKNOWN_INTEGER)
	if(length(index)>0)
	{
		myColors[index]=UNKNOWN_COLOR
		vIndeces=c(1:length(values))[-index]
	} else
	{
		vIndeces=c(1:length(values))
	}
	
	# assign colors to non-missing values
	vColors=c()
	if(length(vIndeces)<BREWER_NCOLOR_MIN)
	{
		# very few colors (less than min that RColorBrewer outputs)
		vColors=brewer.pal(BREWER_NCOLOR_MIN,pal)[1:length(vIndeces)]
	} else if(length(vIndeces)<=ncolor)
	{
		# only need one color palette
		vColors=brewer.pal(length(vIndeces),pal)
	} else if(length(vIndeces)<=ncolor+ncolorBackup)
	{
		# need two color palettes
		vColors=c(brewer.pal(ncolor,pal),brewer.pal(length(vIndeces)-ncolor,palBackup))
	} else
	{
		# too many colors needed, just give up with palettes
		vColors=c(1:length(vIndeces))
	}
	
	# assign remaining, non-unknown colors
	if(reverse)
	{
		myColors[vIndeces]=rev(vColors)
	} else
	{
		myColors[vIndeces]=vColors
	}
	
	# return assigned colors
	myColors=setNames(myColors,values)
	return(myColors)
}

