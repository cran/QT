\name{QT}
\alias{QT}
\title{QT knowledge management system}
\description{This package performs QTc-RR, QT-time, and concentration-QTc analyses.}
\usage{
	QT(data,info)
}

\arguments{
	\item{data}{Data specifications and controlled terms:
		\describe{
			\item{\code{subjid} or \code{id}}{Unique subject identifier (numeric or character) (required)}
			\item{\code{treat} or \code{trt}}{Treatment group (numeric or character) (required)}
			\item{\code{period}}{Treatment period for crossover studies (numeric or character) (optional)}
			\item{\code{day}}{Day relative to first dose (within period for crossover studies) (numeric) (required)}
			\item{\code{time}}{Time (hr) relative to dose (numeric) (required)}
			\item{\code{rr}}{RR (ms) interval (numeric) (required)}
			\item{\code{hr}}{Heart rate (bpm) (numeric) (required)}
			\item{\code{qt/qtc}}{QT/QTc interval (ms) for (numeric) (QT, QTcB, and QTcF are required)}
			\item{\code{qt.bs/qtc.bs}}{Baseline QT/QTc interval (ms) (numeric) (QT, QTcB, and QTcF are required)}
			\item{\code{qt.cfb/qtc.cfb}}{Change from baseline QT/QTc (ms) (numeric) (QT, QTcB, and QTcF are required)}
			\item{\code{conc}}{Parent drug concentration (numeric) (optional)}
			\item{\code{meta}}{Metabolite concentration (numeric) (optional)}
			\item{\code{moxi}}{Moxifloxacin concentrationHeart rate (numeric) (optional)}
			\item{\code{weight}}{Body weight (kg) (numeric) (optional)}
			\item{\code{height}}{Height (cm) (numeric) (optional)}
			\item{\code{age}}{Age (years) (numeric) (optional)}
			\item{\code{Race}}{Race (numeric or character) (optional)}
			\item{\code{sex}}{Gender (controlled term, M=male, F=female) (required)}
		}
	}
	\item{info}{A list including the following elements:
	\describe{
		\item{\code{alpha}}{Alpha level for mean in quantile plot (default is 0.1, not implemented yet).}
		\item{\code{bins}}{Cut-off points to group concentrations by. 
					The minumum and maximum value of the covariate will automatically be added and values outside the range of the concentrations will be removed (default is to use quantiles).}
		\item{\code{cex}}{Size of symbols (default is 1).}
		\item{\code{col}}{Colors for symbols (default is 'black', 'blue', 'red', 'green', 'orange', 'yellow', 'brown').}
		\item{\code{conc}}{Concentration to be used for concentration-QTc analysis (Default: 'conc').}
		\item{\code{corr}}{Correlation structure for inter-individual random effects (Options: 'un' (default), 'diag').}
		\item{\code{correction}}{QT correction to use for analyses (Default: 'qtcf'). The derived QTcI from QTcorrections function is used if 'qtci' argument is TRUE.}
		\item{\code{days}}{Vector of unique days in study design.}
		\item{\code{delta}}{Dependent variable for concentration-QTc analysis (Options: 'double' (default), 'single').}
		\item{\code{design}}{Study design (Required: 'parallel' or 'crossover').}
		\item{\code{device}}{Graphical device to print to (options: 'wmf', 'bmp', 'jpg', 'pdf', 'png', or 'tiff' (pdf is default)).}
		\item{\code{digits}}{Number of significant digits to be used in tables (default is 4, not implemented yet).}
		\item{\code{gof}}{Quantile concentration to be used in quantile plots (Options: 'median' (default), 'mid').}
		\item{\code{intercept}}{Which concentration-QTc model should be used for predictions and graphs (Options: 'Yes' (default), 'No', or 'Fix').}
		\item{\code{label}}{Label for treatment arms (Optional, default is labels=trt).}
		\item{\code{lty}}{Line types for parameter-covariate equations (default is 1 (solid)).}
		\item{\code{lwd}}{Line width (default is 1).}
		\item{\code{output}}{Output folder for graphs and tables (default is NA which prints on screen).}
		\item{\code{pch}}{Symbols (default is 20,1,15,22,17,2,215,3).}
		\item{\code{pk}}{List of PK information (Options: 'drugname', 'drugunit', 'metaname', and 'metaunit').}
		\item{\code{primary}}{List of primary endpoint days for the different treatment arms in trt (except Placebo).}
		\item{\code{qtci}}{Calculate QTcI using baseline (parallel studies) or placebo (crossover studies) data by QTcI=QT/(RR)^beta where log(QT)=alpha+beta*log(RR/1000) (Options: TRUE or FALSE).}
		\item{\code{quantiles}}{Number of bins to group the covariates in (default is 10).}
		\item{\code{report}}{Create summary report in Word. This feature only works on Windows and requires R2wd package is installed along with statconnDCOM) (default is FALSE).}
		\item{\code{scale}}{Concentration scale for concentration-QTc analysis (Options: 'normal' (default) or 'log').}
		\item{\code{trt}}{List of treatment arms (Required: 'Placebo', 'Moxi', 'Therapeutic', and 'Supra', use "NA" when missing).}
		\item{\code{visit}}{Vector  of names for each day the same length as 'days' argument (Required: 'Primary Endpoint' and 'Baseline' (only for parallel studies)).}

	}
	}
}

\examples{

#Path of SAS executable should be in global path before trying to run the example

data(qtpk)
data <- qtpk

info <- list(
	#Output folder (Graphs are shown on screen when NA)
	output=NA,
	
	#Study design, options: 'parallel' or 'crossover'
	design = "crossover",
	
	#Treatment information (Match treatment names in data set to "Placebo", "Therapeutic", "Supra", and "Moxi" (Remember spelling) 
	trt = data.frame(Placebo="Placebo",Therapeutic="Therapeutic",Supra="Supra",Moxi="Moxifloxacin"),

	#Optional graph legend information
	label = data.frame(Placebo="Placebo",Therapeutic="Drug X 10 mg",Supra="Drug X 100 mg",Moxi="Moxifloxacin 400 mg"),

	#Primary endpoint days
	primary = data.frame(Therapeutic=1,Supra=1,Moxi=1),

	#Unique days in design
	days = c(1),

	#Visit names, "Primary Endpoint" (required) (for parallel studies "Baseline" is also required)
	visit  = c("Primary Endpoint"),

	#PK information about therapeutic and supra-therapeutic dose levels (tdose, stdose)
	# drug name and concentration unit and metabolite name and concentration unit
	pk = data.frame(drugname="Drug X",drugunit="ng/mL",metaname=NA,metaunit=NA),

	#QT Correction method, "qtcf" (default), "qtcb", "qtci" (or anything starting with qtc)
	correction = "qtcf",

	####Concentration for Concentration-QTc analysis
	conc = "conc",

	#Should log concentrations be used for the concentration-QT analysis, Options "normal" (default) or "log"
	scale = "normal", 

	#Which mixed-model should be used for plotting, Options "Yes" (default),"No", or "Fix"
	intercept = "Yes",

	#Number of concentration quantiles for goodness of fit plots (default = 10)
	quantiles =10,
	
	#Dependent variable for CQT analysis, options "double" (default), and "single"
	delta="double", 
	
	#Perform QT-RR analysis to calculate QTcI by log QT = alpha + beta * log RR + epsilon using off-treatment data
	qtci=TRUE,
	
	#Quantile concentration to be used in GOF plots, options "median" (default), "mid"
	gof = "median",
	
	#IIV structure, options "diag" (vc), "un" (default)
	corr = "un", 

	#Write summary analysis report to Word (requires R2wd package)
	report=TRUE
)

\dontrun{
	#Drug X Analysis
	QTcorrections(data,info)
	DataCheck(data,info)
	MeanData(data,info)
	QTtime(info)
	QTconc(info)

	#Moxifloxacin Analysis
	data <- qtpk
	info$conc <- "moxi"

	info$trt[,c("Therapeutic","Supra")] <- NA 
	info$label[,c("Therapeutic","Supra")] <- NA 
	info$primary[,c("Therapeutic","Supra")] <- NA
	info$pk$drugname <- "Moxifloxacin"
	info$report <- FALSE

	MeanData(data,info)
	QTconc(info)}
}

\value{
  Creates standard graphs and tables to summarize TQT analyses.
}
\author{Christoffer W. Tornoe <\email{pharmacometrics@fda.hhs.gov}>}
\note{DISCLAIMER: This R package does not reflect the official policy of the FDA. No official endorsement by the FDA is intended or should be inferred. Use of the QT package is at your own risk.}

\keyword{models}
