##############################################################
# Description:  VIVED: Visit Information Visualization 
#					for the Emergency Department.
#				Visualization tool to visualize trends in visit 
#				length of stay, fraction of admitted patients,
#				number of visits, and other summary statistics
#				regarding visits to the emergency department.
# Usage: runApp("Vived.r")
# Author: Haley Hunter-Zinck
# Date: September 29, 2016
# Dependencies: shiny, Vived.fxns.r, reshape, RColorBrewer, 
#					ggplot2, DT
##############################################################


# packages
library(shiny)
library(RColorBrewer)
library(reshape)
library(ggplot2)
library(DT)
source("Vived.fxns.r")

# page
ui = navbarPage(
	title=paste("VIVED v",VERSION,sep=""),
	# dashboard tab
	tabPanel("Dashboard",
		sidebarPanel(
		
			# location
			fileInput(inputId="efile", label="File: "),
					
			# date range
			uiOutput("rangeControls"),
			
			# visualization options
			selectInput(inputId="metric", label="Metric: ", choices=METRICS),
			uiOutput("aggControls"),
			
			# visit property filters
			fluidRow(
				column(8,
					uiOutput("dispControls")),
				column(4,
					uiOutput("acuityControls"))
					),
			uiOutput("mdControls"),
			
			# day/time filters
			fluidRow(
				column(3,
					selectInput(inputId="sHour", label="Start hour: ", choices=HOURS, selected=HOURS[1])),
				column(3,
					selectInput(inputId="sMin", label="Start min: ", choices=MINUTES, selected=MINUTES[1])),
				column(3,
					selectInput(inputId="eHour", label="End hour: ", choices=HOURS, selected=HOURS[length(HOURS)])),
				column(3,
					selectInput(inputId="eMin", label="End min: ", choices=MINUTES, selected=MINUTES[length(MINUTES)]))
					),
			checkboxGroupInput(inputId="dow", label="Days of the week: ", choices=LABEL_DOW, select=LABEL_DOW, inline=TRUE),
			
			# federal holiday filter
			checkboxInput(inputId="holiday", label="Exclude federal holidays",value=FALSE),
			
			# action buttons
			actionButton(inputId = "updatePlot", label = "Run"),
			downloadButton(outputId="downloadPlot", label="Download plot"),
			downloadButton(outputId="downloadTable", label="Download table")
		),
		mainPanel(
			htmlOutput('myChecked'),
			htmlOutput('myUpdated'),
			plotOutput('myPlot'),
			dataTableOutput('myTable')
		)
	)
)

# server
server <- function(input, output) 
{	
	# setup aggregate views
	output$aggControls <- renderUI({
		views=getViews(input$metric)
		if(length(input$agg)>0 && is.element(input$agg,unlist(views)))
		{
			mySelected=input$agg
		} else
		{
			mySelected=NULL
		}
		selectInput(inputId="agg", label="View: ", choices=views, selected=mySelected)
	})
	
	# setup default date range
	output$rangeControls <- renderUI({
		if(length(input$efile$datapath)==0)
		{
			dateRangeForLoc=c(NA,NA)
		} else
		{
			dateRangeForLoc=getDefaultDateRange(input$efile$datapath)
		}
		dateRangeInput(inputId="dateRange", label="Date range: ", start=dateRangeForLoc[1], end=dateRangeForLoc[2])
	})
	
	# setup disposition filter selection button
	output$dispControls <- renderUI({
		if(length(input$efile$datapath)==0)
		{
			myChoices=c(ALL_DISP)
		} else
		{
			dispositions=getDispositions(input$efile$datapath)
			if(length(intersect(DISP_ADMIT,dispositions))>0)
			{
				myChoices=c(ALL_DISP,ALL_ADMIT,dispositions)
			} else
			{
				myChoices=c(ALL_DISP,dispositions)
			}
		}
		selectInput(inputId="disp", label="Disposition: ", choices=myChoices)
	})
	
	# setup acuity filter selection button
	output$acuityControls <- renderUI({
		if(length(input$efile$datapath)==0)
		{
			myChoices=c(ALL_ACUITY)
		} else
		{
			acuity=getAcuity(input$efile$datapath)
			myChoices=c(ALL_ACUITY, acuity)
		}
		selectInput(inputId="acuity", label="Acuity: ", choices=myChoices)
	})

	# setup providers filter selection button
	output$mdControls <- renderUI({
		if(length(input$efile$datapath)==0)
		{
			myChoices=ALL_MD
		} else
		{
			providers=getProviders(input$efile$datapath)
			myChoices=c(ALL_MD,providers)
		}
		selectInput(inputId="md", label="Provider: ", choices=myChoices)
	})
	
	# setup run button
	data=eventReactive(input$updatePlot, 
	{
		withProgress(message="Creating plot...",detail="This may take a few seconds", style="notification", value=NULL, 
		{
			results=createPlot(efile=input$efile$datapath, dateRange=input$dateRange, metric=input$metric, 
				agg=input$agg, md=input$md, dow=input$dow, disp=input$disp, acuity=input$acuity, 
				sHour=input$sHour, eHour=input$eHour, sMin=input$sMin, eMin=input$eMin,
				excludeHolidays=input$holiday)
		})
		return(results)
	})

	# output plot
	output$myPlot=renderPlot(plot(data()$plot))
	
	# output data table
	output$myTable=renderDataTable(
	{
		datatable(data()$table, rownames=FALSE, options=list(pageLength=5))
	})
	
	# setup download button for plot
	output$downloadPlot=downloadHandler(
		filename=function()
		{
			if(length(data()$fileNamePlot)>0)
			{
				return(data()$fileNamePlot)
			}
			return("vived.pdf")
		},
		content=function(file)
		{
			device=function(..., width, height) 
			{
				grDevices::pdf(..., width=width, height=height)
			}
			ggsave(file, plot=data()$plot, device=device)
		} 
	)
	
	# setup download button for table
	output$downloadTable=downloadHandler(
		filename=function()
		{
			if(length(data()$fileNameTable)>0)
			{
				return(data()$fileNameTable)
			}
			return("vived.csv")
		},
		content=function(file)
		{
			write.csv(data()$table,file,row.names=TRUE)	
		}		
	)
}

# start app
shinyApp(ui = ui, server = server)


