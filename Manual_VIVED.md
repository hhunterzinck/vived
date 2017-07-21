#Manual for VIVED v0.5
##Description
The Visit Information Visualization for the Emergency Department (VIVED) tool is a HTML dashboard implemented in R with the Shiny package to examine patient flow through an emergency department (ED) or urgent care center (UCC).  Users can filter data to their specifications including by date, day of the week, time of the day, patient disposition, and provider.  Users can then plot metrics such as number of visits per day, fraction of patients admitted per day, length of stay, boarding time, and ED census information.

##Installation 
* Install R version 3.3.1 or greater
    * https://www.r-project.org/
* Install required R packages
    * Open an R terminal and install the following packages with the command install.packages("package_name")
        * shiny
        * ggplot2
        * gridExtra
        * reshape
        * RColorBrewer
* Download and decompress VIVED package
* Modify the disp_admit.txt file in the VIVED package.
    * The disp_admit.txt file contains a list of dispositions types that result in admission to the relevant inpatient facility.
    * Users should modify this file so that each unique disposition type resulting in admission is listed for each row.  For example
        * "Admit"
        * "Telemetry"
        * "ICU"
    * If no dispositions result in admission or dispositions are unknown, leave this file empty.
    
##Running VIVED
* After installation, open an R terminal.
* Set the working directory to the decompressed VIVED package directory. 
    * setwd("/path/to/files/vived/")
* Type the following two commands:
    * library(shiny)
    * runApp("Vived.r")
* The app should open in your default web browser.

##Input file
The input file containing ED or UCC visit information must be in csv format with one header row and each remaining row representing a unique ED visit.  Columns, along with their header labels, are described in the following table: 

Header label | Required | Date type | Description
-------------|---------|---------|---------------------------
PatientID | Yes | Integer | Patient identifier
TimeIn | Yes | Timestamp | Time at which the patient enters the ED or UCC
TimeOut | Yes | Timestamp | Time at which the patient exists the ED or UCC
Station | No | Integer | ED or UCC identifier.  Used in combination with the PatientID to identify unique patients.
TimeTriage | No | Timestamp | Time at which the patient is assigned an acuity score
TimeSeen | No | Timestamp | Time at which the patient is first seen by a physician
TimeDisposition | No | Timestamp | Time at which the patient is assigned a disposition
Acuity | No | Integer | Acuity score (e.g. Emergency Severity Index)
Disposition | No | String | Disposition of the patient 
MdAssigned | No | String | Name or identifier of the physician assigned to the patient during the visit
AgeAtVisit | No | Integer | Age of the patient on the day of the visit

All timestamps must be in the form "YYYY-MM-DD HH:MM:SS".  Columns do not have to be in order and additional columns may be included, but the column headers must be exact, including capitalization.  Columns that are not required do not have to be included in the input file.  However, visualizations involving non-essential data elements will not be available if the respective columns are not included.


##Dashboard tab
###File
Users can select an input file (format described above) using the "Browse" button.  

###Date range
This dropdown allows users to select a start and end date for filtering the dataset.  Users may type in the dates (format is "YYYY-MM-DD") or click on the boxes and select dates in a dropdown calendar.  The default range is set to the timestamp of patient arrival for the first and last visit associated with each dataset.  

###Metric
This dropdown menu allows users to select a metric to display.  Currently implemented metrics are as follows:

Metric | Unit | Description
---------------|-------|-------------------------------------
Number of visits per day | Day | Calculates the number of visits per day based on patient arrival time
Number per day boarding > 4 hours | Day | Number of patients per day who boarded (defined as the time between the disposition time and the patient departure from the emergency department) for more than 4 hours.  Patient visits are assigned to each day by the patient arrival time.
Number per day returned < 72 hours | Day | Number of visits per day that correspond to a patient who visited the same emergency department within 6 and 72 hours previously.  A minimum threshold of 6 hours is included to reduce data input errors.  Any filters applied to this metric only affect the initial visit, not the return visit.
Number per day returned-admitted < 72 hours | Day | Number of visits per day that correspond to a patient who visited the same ED within 6 and 72 hours previously and was admitted on the second visit.  A minimum threshold of 6 hours is included to reduce data input errors.  Any filters applied to this metric only affect the initial visit, not the return visit.
Percent of patients per day | Day percent | Calculates the fraction of visits per day that correspond to the user-specified filters for disposition, provider, hour of the day, or day of the week.  The visit is associated with each day by the patient arrival time.  If default filter options are selected, this metric will always return 100%. The denominator corresponds to all valid visits.
Percent per day boarding > 4 hours | Day percent | Percentage of patients per day who boarded (defined as the time between the disposition time and the patient departure from the emergency department) for more than 4 hours.  Patient visits are assigned to each day by the patient arrival time. The denominator corresponds only to those visits as specified by the user filters.
Percent per day returned < 72 hours | Day percent | Percentage of visits per day that correspond to a patient who visited the same emergency department within 6 and 72 hours previously.  A minimum threshold of 6 hours is included to reduce data input errors. The denominator corresponds only to those visits as specified by the user filters.
Percent per day returned-admitted < 72 hours  | Day percent | Percentage of visits per day that correspond to a patient who visited the same ED within 6 and 72 hours previously and was admitted on the second visit.  A minimum threshold of 6 hours is included to reduce data input errors.  Any filters applied to this metric only affect the initial visit, not the return visit.
Length of stay (min) | Visit | Calculates the length of stay of a patient in minutes as the difference between the time out and time in 
Door to triage (min) | Visit | The time in minutes between the patient arrival time and the first time the acuity score is set for the patient during his or her visit.
Door to doc (min) | Visit | The time in minutes between the patient arrival time and the first time the patient is assigned to an MD provider.
Door to disp (min) | Visit | The time in minutes between the patient arrival time and the first time the disposition is set for the patient visit.
Boarding time: disposition-out (min) | Visit | Calculates the boarding time of admitted patients.  Admitted patients are those with dipositions matching those in the disp_admit.txt file, modified upon installation.  The boarding time is calculated as the difference between the disposition time and time out.
Census per minute | Minute | Calculates the number of patients in the ED for each minute of the day as assessed by the number of visit intervals overlapping that minute.
Census per minute by acuity | Minute | Calculates the number of patients in the ED for each minute of the day as assessed by the number of visit intervals overlapping that minute and subsets the ED census by patient acuity.
Census per minute by age | Minute | Calculates the number of patients in the ED for each minute of the day as assessed by the number of visit intervals overlapping that minute and subsets the ED census by patient age.
Census per minute by disposition | Minute | Calculates the number of patients in the ED for each minute of the day as assessed by the number of visit intervals overlapping that minute and subsets the ED census by patient disposition.
Census per minute by progression | Minute | Calculates the number of patients in the ED for each minute of the day as assessed by the number of visit intervals overlapping that minute and subsets the ED census by patient progression. ???	Progression indicates the stage of the patient visits.  The first stage is between the visit time in and the time of triage.  The second state is the time between triage and being seen by the doctor.  The third stage is between being seen by the doctor and having the disposition set.  The fourth stage is between having a disposition set and leaving the ED.
Census per minute by status | Minute | Calculates the number of patients in the ED for each minute of the day as assessed by the number of visit intervals overlapping that minute and subsets the ED census by patient status. ???	Patient status is split into 4 categories: patients who will be home, patients who will be admitted but their disposition has not yet been set, and patients who will be admitted and their disposition has been set who have been boarding for at most 4 hours and more than 4 hours.

### View
This dropdown allows users to select an aggregate view to display the metrics described above.  The available aggregation views differ by selected metric and will dynamically update depending on the metric selected by the user.  The views are described below:

View | Description
----------|--------------------------------------------------
Histogram | Plots metric distribution as a block histogram
Year | Groups the metrics by year associated with each visit and displays the distribution associated with each year as a boxplot for relevant years.
Month | Groups the metrics by year and month for each unique year and month combination.  The distribution of each group is shown as a boxplot for relevant year-months.
Month of the year | Groups the metrics by month of the year.  Metrics in different years but the same month will be placed in the same group.  The distribution for each group is shown as a boxplot for relevant months.
Week | Groups metrics by week where weeks begin on Monday.
Day | Groups metrics by the date.  If only one metric per date is available (e.g. number of visits per day), the plot is a line plot with points indicating each unique date.  If more than one metric per date is available, each unique date's distribution is displayed as a boxplot for relevant dates.
Day of the week | Groups metrics by the day of the week.  Metrics in different weeks but the same day of the week will be placed in the same group.  The distribution for each group is shown as a boxplot for relevant days of the week.
Hour of the day | Groups metrics by the hour of the day.  Metrics from different days but occurring in the same hour of the day will be placed in the same group.  The distribution for each group is shown as a boxplot for relevant hours of the day.
Minute by day | Groups metrics by minute of the day and day of the week.  Metrics are displayed as a bar plot showing the metric's value for that minute on a particular day.  Multiple bar plots are created if more than one day of the week is specified.  If the data represents more than one week, then data is averaged for overlapping minutes.

The views available for each metric by unit type is delineated below:

Metric unit / View | Histogram | Year | Month | Month of the year | Week | Day | Day of the week | Hour of the day | Minute by day
-------|-----|-----|-----|-----|-----|-----|-----|-----|-----
Day|X|X|X|X|X|X|X|X||
Day percent|X|X|X|X|X|X|X|X||
Visit|X|X|X|X|X|X|X|X||
Census|X|X|X|X|X|X|X|X|X


###Disposition
This dropdown allows the user to filter patient visits by the patient disposition assigned during the visit.  This dropdown updates dynamically to display all unique dispositions in the full dataset.  The default is "All".  

###Acuity
This dropdown allows the user to filter patient visits by the assigned acuity (e.g. Emergency Severity Index).  The default is "All".

###Provider
This dropdown allows the user to filter patient visits by the provider assigned to the patient during the visit.  This dropdown updates dynamically to display all unique providers in the full dataset.  The default is "All".

###Start hour/minute and end hour/minute
These four dropdowns allow the user to filter patient visits within a particular time of day.  The start time and end time are inclusive of the selected minute.  For census metrics, the census is set to missing during these periods and so is not displayed or included in any visualization.  For other metrics, patient visits are excluded if the time in associated with the visit lies outside the requested time interval.  The default is to include all hours (start time: "00:00" and end time "23:59").

###Days of the week
This group of checkboxes allows the user to select any combination of days of the week to filter patient visits.  Any patient visit's time in which lies in a checked day of the week will be retained for analysis and visualization.  The default is to include all days of the week.

###Exclude federal holidays
When checked, this option allows users to display visualizations and tables calculated without visits occurring on the ten annual federal holidays.  A list of federal holidays is available on the U.S. Office of Personnel Management website.

###Run 
Having selected all appropriate filters and options, the user can click the "Run" button to produce the visualization.  Depending on the size of the dataset and the metric selected, the visualization may take as much as several seconds to produce.  ED census plots are particular time consuming when calculated over long ranges of time.

###Download plot
Having produced a plot, the user can click the "Download plot" button to save a pdf of the respective visualization.  A file explorer box will appear asking the user to select a folder in which to save the plot.   

###Download table
Having produced a plot and table, the user can click the "Download table" button to save a csv file of the respective table.  A file explorer box will appear asking the user to select a folder in which to save the table.    


##Reports tab

###File
Users can select an input file (format described above) using the "Browse" button.  

### Date range
This dropdown allows users to select a start and end date for filtering data.  Users may type in the dates (format is "YYYY-MM-DD"") or click on the boxes and select dates in a dropdown calendar.  The default range is set to the timestamp of patient arrival for the first and last visit associated with each dataset. 

### Report
This dropdown allows users to select a particular report type to generate.  The default is the "Summary" report.

Currently implemented reports are as follows:

* Summary: this report displays a table of metrics as rows and two columns.  The first column indicates the count or median value for the respective metric and the second column displays a percentage value, if appropriate.  If there are no associated visits with a given metric or the percentage does not apply to the metric, the element of the table is left blank. Quality metrics are as follows:
    * Total number of visits
    * Number of days with visits
    * Number of visits per day
    * Patients admitted (count and percent)
    * Boarding > 4 hours (count and percent)
    * Returned < 72 hours (count and percent)
    * Returned-admits < 72 hours (count and percent)
    * Median length of stay (LOS) in minutes
    * Median length of stay for admitted patients
    * Median length of stay for patients with disposition "Home"
    * Median door to triage time in minutes
    * Median door to doc time in minutes
    * Median door to disposition time in minutes
    * Median boarding time in minutes
    * Median of the maximum occupancy per day
    * Number of visits by ESI level 
    * Patients admitted by ESI level (count and percentage)
    * Median length of stay for patient by ESI level
* Providers: this report displays a table of median quality metrics over the given time range for each provider with an associated visit in that time range.  Rows of the table can be sorted by each column value and the table can also be searched.  Quality metrics that have no corresponding visits for that provider are left blank.  Quality metrics are as follows:
    * Total number of visits
    * Median number of visits per day
    * Median percentage admitted
    * Median number of patients who return within 72 hours
    * Median percentage of patients who return within 72 hours
    * Median number of patients who return and are admitted within 72 hours
    * Median percentage of patients who return and are admitted within 72 hours
    * Median door to doc time in minutes
    * Median door to disposition time in minutes
    * Median length of stay (LOS) in minutes
    * Median length of stay for patients with disposition "Home"
    * Median length of stay for admitted patients
    * Percentage of patients admitted by ESI level
    * Median length of stay for patient by ESI level

###Run
Having selected the appropriate options, the user can click the "Run" button to produce the report.  Depending on the type of report and the data range selected, the report may take as long as a minute to produce.  The report generation will be optimized in future versions of VIVED.

###Download
Having produced a report, the user can click the download button to save a csv file of the respective table.  A file explorer box will appear asking the user to select a folder in which to save the table.
