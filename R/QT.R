require(SASxport)
require(Hmisc)
require(grid)
require(lattice)

####################################################################################
###   QT script (Copyright: Christoffer Wenzel Tornoe, FDA 2010)                 ###
###   Last edited: 3/25-2010                                                     ###
####################################################################################
#ScriptPath <- paste(R.home(),"/library/QT/data/",sep="")

#=========================================function for QT analyses          ===========================================================#
#Function Name: QT(data,info)                                                                                                          #
#                                                                                                                                      #
#======================================================================================================================================#

QT <- function(data,info){
	InfoCreate(info)
	QTcorrections(data,info)
	DataCheck(data,info)
	MeanData(data,info)
	QTtime(info)
	QTconc(info)
}

#=========================================function for adding to info list ===========================================================#
#Function Name: outputGraph()                                                                                                          #
#                                                                                                                                      #
#======================================================================================================================================#

outputGraph <- function(name,device,height=9,width=9){
	if(device=="pdf") pdf(file=paste(name,".pdf",sep=""),height=height,width=width)
	if(device=="wmf") win.metafile(file=paste(name,".wmf",sep=""),height=height,width=width)
	if(device=="bmp") tiff(filename=paste(name,".bmp",sep=""),width=480/9*width,height=480/9*height)
	if(device=="tiff") tiff(filename=paste(name,".tif",sep=""),width=480/9*width,height=480/9*height)
	if(device=="png") png(filename=paste(name,".png",sep=""),width=480/9*width,height=480/9*height)
	if(device=="jpg") jpeg(filename=paste(name,".jpg",sep=""),quality=100,width=480/9*width,height=480/9*height)
}


#=========================================function for adding to info list ===========================================================#
#Function Name: InfoCreate()                                                                                                           #
#                                                                                                                                      #
#======================================================================================================================================#
 InfoCreate <- function(info){

	if(sum(c("design","trt","label","primary","output","digits","device",
		          	"days","visit","pk","correction","saspath","report",       
				"conc","scale","intercept","quantiles","delta",
				"qtci","gof","corr",        
				"col","bin",    
				"cex","lty","lwd","pch","alpha")%in%names(info)==FALSE)>0){
		
		cat("\nThe following default settings were added to your info list:\n")

		if(is.null(info$label)){
			info$label <- info$trt
			cat(paste("\n   label=",info$label))
		}

		if(is.null(info$report)){
			info$report <- FALSE
			cat(paste("\n   report=",info$report))
		}

		if(is.null(info$first)){
			info$first <- TRUE
		}

		if(info$report==TRUE){
			if(length(grep("R2wd",row.names(installed.packages())))>0){
				require(R2wd)

				if(info$first==TRUE){	
					wdGet()
					wdNewDoc("QT")
					wdTitle("Summary of QT Analysis",label="QT")
					wdTitle("",label="blankTitle")
	
					wdSection("QT Corrections",label="corrections",newpage=TRUE)
					wdSection("",label="blankcorrection")

					wdSection("Mean Data",label="mean",newpage=TRUE)
					wdSection("",label="blankMean",newpage=FALSE)
					wdSection("",label="blankMean2",newpage=FALSE)

					wdSection("QTc-Time Analysis",label="time",newpage=TRUE)
					wdSection("",label="blankTime",newpage=FALSE)
	
					wdSection("Concentration-QTc Analysis",label="conc",newpage=TRUE)
					wdSection("",label="blankConc",newpage=FALSE)

					wdSection("Data Check",label="check",newpage=TRUE)
					wdSection("",label="blankCheck",newpage=FALSE)

					wdSave("QT")

				}
				info$first <- FALSE
			}else{
				cat("\nYou need to install the 'R2wd' package to generate the report\n")
				info$report <- FALSE
			}
		}


		if(is.null(info$pk)){
			info$pk <- list(drugname="Drug",drugunit=NA,metaname=NA,metaunit=NA)
			cat(paste("\n   pk=",info$pk))
		}
		if(is.null(info$correction)){
			info$correction <- "qtcf"
			cat(paste("\n   correction=",info$correction))
		}
		if(is.null(info$saspath)){
			info$saspath <- ""
			cat(paste("\n   saspath=",info$saspath))
		}
		if(is.null(info$conc)){
			info$conc <- "conc"
			cat(paste("\n   conc=",info$conc))
		}
		if(is.null(info$scale)){
			info$scale <- "normal"
			cat(paste("\n   scale=",info$scale))
		}
		if(is.null(info$intercept)){
			info$intercept <- "Yes"
			cat(paste("\n   intercept=",info$intercept))
		}
		if(is.null(info$delta)){
			info$delta <- "double"
			cat(paste("\n   delta=",info$delta))
		}
		if(is.null(info$qtci)){
			info$qtci <- TRUE
			cat(paste("\n   qtci=",info$qtci))
		}
		if(is.null(info$gof)){
			info$gof <- "median"
			cat(paste("\n   gof=",info$gof))
		}
		if(is.null(info$corr)){
			info$corr <- "un"
			cat(paste("\n   corr=",info$corr))
		}
		if(is.null(info$quantiles)){
			info$quantiles <- 4
			cat(paste("\n   quantiles=",info$quantiles))
		}
		if(is.null(info$bin)){
			info$bin <- NA
			cat(paste("\n   bin=",info$bin))
		}

		if(is.null(info$col)){
			info$col <- c("black","blue","red","green","orange","yellow","brown")
			cat(paste("\n   col=",paste(info$col,collapse=",")))
		}

		if(is.null(info$device)){
			info$device <- "pdf"
			cat(paste("\n   device=",info$device,sep=""))
		}

		if(is.null(info$pch)){
			info$pch<- c(20,1,15,22,17,2,215,3)
			cat(paste("\n   pch=",paste(info$pch,collapse=",")))
		}

		if(is.null(info$cex)){
			info$cex <- c(1,1,1,1,1,1,1,1)
			cat(paste("\n   cex=",paste(info$cex,collapse=",")))
		}
		if(is.null(info$lty)){
			info$lty <- rep(1,length(info$col))
			cat(paste("\n   lty=",paste(info$lty,collapse=",")))
		}
		if(is.null(info$lwd)){
			info$lwd<- rep(1,length(info$col))
			cat(paste("\n   lwd=",paste(info$lwd,collapse=",")))

		}

		if(is.null(info$digits)){
			info$digits <- 4
			cat(paste("\n   digits=",info$digits))
		}

		if(is.null(info$output)){
			info$output <- NA
			cat(paste("\n   output=",info$output))
		}

		if(is.null(info$alpha)){
			info$alpha <- 0.1
			cat(paste("\n   alpha=",info$alpha))
		}
		cat("\n")
	}
	#assign("info",info,pos=sys.parent(),inherits=TRUE)
	assign("info",info,envir=.GlobalEnv)
	return(info)

}

####################################################################################
###### Calculate QTcI and plot QTc vs. RR
####################################################################################

QTcorrections <- function(data,info){
	options(warn = -1)
	names(info) <- casefold(names(info),upper=FALSE)

	if(sum(c("design","trt","primary","days","visit")%in%names(info)==FALSE)>0){

		cat(paste("\nYou forgot the following required items in info: \"",c("design","trt","primary","days","visit")[which(c("design","trt","primary","days","visit")%in%names(info)==FALSE)],
				"\" to your info list\n",sep=""))

		cat("The QT script will terminate now")
		stop()
	}

	if(sum(c("design","trt","label","primary","output","digits","device",
		          	"days","visit","pk","correction","saspath","report",       
				"conc","scale","intercept","quantiles","delta",
				"qtci","gof","corr",        
				"col","bin",    
				"cex","lty","lwd","pch","alpha")%in%names(info)==FALSE)>0){
		
		cat("QTcorrections function: The info list has been changed and does not include all options\n")
		cat("InfoCreate function is being called from within the QTcorrections function\n")
		info <- InfoCreate(info)
	}


	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	if(!is.na(info$output)){
		opath <- getwd()
		dir.create(info$output,showWarnings=FALSE)
		setwd(info$output)
	}

	names(data) <- casefold(names(data), upper=FALSE)
	if(is.null(data$id)) data$id <- data$subjid
	if(is.null(data$trt)) data$trt <- data$treat
							
	if(length(which(names(data)%in%c("id","trt","day","time","qt","qtcb","qtcf","hr","rr","qt.bs","qtcb.bs","qtcf.bs",
						"qt.cfb","qtcb.cfb","qtcf.cfb","hr.cfb","rr.cfb","age","sex",
						"race","weight","height")))!=22){
							
			print(paste("The following columns are not in the data: ",c("id","trt","day","time","qt","qtcb","qtcf","hr","rr","qt.bs","qtcb.bs","qtcf.bs",
					"qt.cfb","qtcb.cfb","qtcf.cfb","hr.cfb","rr.cfb","age","sex",
					"race","weight","height")[!c("id","trt","day","time","qt","qtcb","qtcf","hr","rr","qt.bs","qtcb.bs","qtcf.bs",
					"qt.cfb","qtcb.cfb","qtcf.cfb","hr.cfb","rr.cfb","age","sex",
					"race","weight","height")%in%names(data)]),sep="")
					#stop()
	}

	if(!is.numeric(data$id)) data$id <- as.character(data$id)
	data <- data[!is.na(data$id),]
	if(!is.numeric(data$trt)) data$trt <- as.character(data$trt)

	data$qt <- as.numeric(as.character(data$qt))
	data$qtcb <- as.numeric(as.character(data$qtcb))
	data$qtcf <- as.numeric(as.character(data$qtcf))
	
	CorrectionSponsor <- unique(substring(names(data)[grep("qtc",names(data))],1,4))[!c(unique(substring(names(data)[grep("qtc",names(data))],1,4))%in%c("qtcb","qtcf"))]
	if(length(CorrectionSponsor)>0){
		for(i in 1:length(CorrectionSponsor)){
			eval(parse(text=paste("data$",CorrectionSponsor[i]," <- as.numeric(as.character(data$",CorrectionSponsor[i],"))",sep="")))	
		}
	}
	
	if(length(grep("qtci",CorrectionSponsor))>0) CorrectionSponsor[grep("qtci",CorrectionSponsor)] <- "sqtci"

	if(!is.null(data$qtci))	data$qtci <- as.numeric(as.character(data$qtci))
	if(info$correction!="qtci" & info$qtci==FALSE) eval(parse(text=paste("data$",info$correction," <- as.numeric(as.character(data$",info$correction,"))",sep="")))

	data$rr <- as.numeric(as.character(data$rr))
	data$gender <- rep(NA,nrow(data))
	data$sex <- casefold(data$sex, upper=FALSE)

	data$gender[data$sex=="male" | data$sex=="m"] <- 1
	data$gender[data$sex=="female" | data$sex=="f"] <- 0

	data$qbase <- rep(0,nrow(data))
	data$qbase[data$day%in%info$days[which(info$visit=="Baseline")]] <- 1

	if(info$qtci==TRUE){
		if(!is.null(data$qtci)){
			data$sqtci <- data$qtci
			data$sqtci.bs <- data$qtci.bs
			data$sqtci.cfb <- data$qtci.cfb
			data <- data[,-which(names(data)%in%c("qtci","qtci.bs","qtci.cfb"))]
		}	
	}

	datarange <- data[1:100,]
	datarange[1:100,] <- NA
	datarange$qbase <- rep(1,nrow(datarange))
	datarange$id <- c(rep(-999,50),rep(-9999,50))
	datarange$trt <- c(rep(-999,50),rep(-9999,50))
	datarange$rr <- c(seq(min(data$rr,na.rm=TRUE),max(data$rr,na.rm=TRUE),length=50),seq(min(data$rr,na.rm=TRUE),max(data$rr,na.rm=TRUE),length=50))
	datarange$gender <- c(rep(1,50),rep(0,50))
	
	dataexport <- rbind(datarange,data)
	dataexport$lnqt <- log(dataexport$qt)
	dataexport$lnrr <- log(dataexport$rr/1000)

	if(info$qtci==TRUE){
		if(length(which(info$visit=="Baseline"))>0){
				sas <- dataexport[,c("id","trt","qbase","gender","qt","qtcb","qtcf","rr","lnqt","lnrr")]
				write.xport(sas, file="qtcdata.xpt")	

				if(length(unique(data$gender))>1){
					sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTc.sas",sep=""),sep="\n",what="character")	
				}else{
					sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTcmale.sas",sep=""),sep="\n",what="character")						
				}
				if(info$corr=="diag"){
					for(i in which(regexpr("TYPE=UN",sasscript)!=-1)){
						substring(sasscript[i],first=regexpr("TYPE=UN",sasscript[i])[1],last=regexpr("TYPE=UN",sasscript[i])[1]+6) <- "TYPE=VC"
					}
				}
				write(sasscript,file="QTc.sas")	

				if(info$saspath==""){	
					system("sas QTc.sas")			
				}else{
					system(paste(info$saspath,"/sas QTc.sas",sep=""))
				}

				if(length(CorrectionSponsor)>0){
					sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTcSponsor.sas",sep=""),sep="\n",what="character")	
					for(i in 1:length(CorrectionSponsor)){
						sasscript[20] <- paste("TITLE '",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i])," ANALYSIS';",sep="")
						sasscript[23] <- paste("MODEL ", ifelse(CorrectionSponsor[i]=="sqtci","SQTCI",CorrectionSponsor[i])," =GENDER NEWRR / SOLUTION CL COVB ALPHA=0.05 ALPHAP=0.05 DDFM=BW",sep="")
						sasscript[28] <- paste("data pred",i,"; set pred; lnrr=NEWRR; baseline= 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; if id=\"-9999\" or id=\"-999\"; tused=(upper-pred)/StdErrPred;keep tused trt gender lnrr pred StdErrPred DF alpha lower upper baseline correction; run;",sep="")
						sasscript[29] <- paste("data parameter",i,"; set parameter; baseline= 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[30] <- paste("data fit",i,"; set fit; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[31] <- paste("data random",i,"; set random; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[32] <- paste("data predblup",i,"; set predblup; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[33] <- paste("data iiv",i,"; set iiv; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						if(i==1){
							sasscriptcomb <- sasscript[1:33]
						}else{
							sasscriptcomb <- c(sasscriptcomb,sasscript[20:33])
						}
					}

					sasscript[38] <- paste("data pred; set ",paste("pred",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[39] <- paste("data predblup; set ",paste("predblup",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[40] <- paste("data parameter; set ",paste("parameter",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[41] <- paste("data eta; set ",paste("random",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[42] <- paste("data fit; set ",paste("fit",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[43] <- paste("data iiv; set ",paste("iiv",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")

					sasscriptcomb <- c(sasscriptcomb,sasscript[34:71])
						
					if(info$corr=="diag"){
						for(i in which(regexpr("TYPE=UN",sasscriptcomb)!=-1)){
							substring(sasscriptcomb[i],first=regexpr("TYPE=UN",sasscriptcomb[i])[1],last=regexpr("TYPE=UN",sasscriptcomb[i])[1]+6) <- "TYPE=VC"
						}
					}
					sas <- dataexport[,c("id","trt","qbase","gender","rr",CorrectionSponsor)]
					write.xport(sas, file="qtcsponsordata.xpt")	
					write(sasscriptcomb,file="QTcSponsor.sas")	

					if(info$saspath==""){	
						system("sas QTcSponsor.sas")			
					}else{
						system(paste(info$saspath,"/sas QTcSponsor.sas",sep=""))
					}
				}

		}else{
			data$qbase[data$trt%in%info$trt$Placebo] <- 1
			dataexport$qbase[dataexport$trt%in%info$trt$Placebo] <- 1
			sas <- dataexport[,c("id","trt","qbase","gender","qt","qtcb","qtcf","rr","lnqt","lnrr")]
			write.xport(sas, file="qtcdata.xpt")	

			if(length(unique(data$gender))>1){
				sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTc.sas",sep=""),sep="\n",what="character")	
			}else{
				sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTcmale.sas",sep=""),sep="\n",what="character")						
			}
				
			if(info$corr=="diag"){
						for(i in which(regexpr("TYPE=UN",sasscript)!=-1)){
							substring(sasscript[i],first=regexpr("TYPE=UN",sasscript[i])[1],last=regexpr("TYPE=UN",sasscript[i])[1]+6) <- "TYPE=VC"
						}
			}

			write(sasscript,file="QTc.sas")	

			if(info$saspath==""){	
				system("sas QTc.sas")			
			}else{
				system(paste(info$saspath,"/sas QTc.sas",sep=""))
			}			
			
			if(length(CorrectionSponsor)>0){
					sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTcSponsor.sas",sep=""),sep="\n",what="character")	
					for(i in 1:length(CorrectionSponsor)){
						sasscript[20] <- paste("TITLE '",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i])," ANALYSIS';",sep="")
						sasscript[23] <- paste("MODEL ", ifelse(CorrectionSponsor[i]=="sqtci","SQTCI",CorrectionSponsor[i])," =GENDER NEWRR / SOLUTION CL COVB ALPHA=0.05 ALPHAP=0.05 DDFM=BW",sep="")
						sasscript[28] <- paste("data pred",i,"; set pred; lnrr=NEWRR; baseline= 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; if id=\"-9999\" or id=\"-999\"; tused=(upper-pred)/StdErrPred;keep tused trt gender lnrr pred StdErrPred DF alpha lower upper baseline correction; run;",sep="")
						sasscript[29] <- paste("data parameter",i,"; set parameter; baseline= 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[30] <- paste("data fit",i,"; set fit; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[31] <- paste("data random",i,"; set random; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[32] <- paste("data predblup",i,"; set predblup; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						sasscript[33] <- paste("data iiv",i,"; set iiv; baseline = 'No'; correction='",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"'; run;",sep="")
						if(i==1){
							sasscriptcomb <- sasscript[1:33]
						}else{
							sasscriptcomb <- c(sasscriptcomb,sasscript[20:33])
						}
					}
					
					sasscript[38] <- paste("data pred; set ",paste("pred",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[39] <- paste("data predblup; set ",paste("predblup",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[40] <- paste("data parameter; set ",paste("parameter",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[41] <- paste("data eta; set ",paste("random",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[42] <- paste("data fit; set ",paste("fit",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")
					sasscript[43] <- paste("data iiv; set ",paste("iiv",1:length(CorrectionSponsor),sep="",collapse=" "),"; run;",sep="")

					sasscriptcomb <- c(sasscriptcomb,sasscript[34:71])
					
					if(info$corr=="diag"){
						for(i in which(regexpr("TYPE=UN",sasscriptcomb)!=-1)){
							substring(sasscriptcomb[i],first=regexpr("TYPE=UN",sasscriptcomb[i])[1],last=regexpr("TYPE=UN",sasscriptcomb[i])[1]+6) <- "TYPE=VC"
						}
					}
					sas <- dataexport[,c("id","trt","qbase","gender","rr",CorrectionSponsor)]
					write.xport(sas, file="qtcsponsordata.xpt")	
					write(sasscriptcomb,file="QTcSponsor.sas")	

					if(info$saspath==""){	
						system("sas QTcSponsor.sas")			
					}else{
						system(paste(info$saspath,"/sas QTcSponsor.sas",sep=""))
					}
			}
		}

		qtci <- read.table(file="qtci.csv",header=TRUE,sep=",")
		qtpredsas <- read.table("qtcipred.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
		if(length(CorrectionSponsor)>0) qtpredsasSponsor <- read.table("qtcsponsorpred.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")

		#Merge QTcI data with original data
		qtci <- qtci[match(unique(qtci$ID),qtci$ID),c("ID","exponent")]
		names(qtci) <- c("id","exponent")

		if(is.numeric(qtci$id)){
			data$tempid <- as.numeric(data$id)
			data <- merge(data,qtci,by.x="tempid",by.y="id",all.x=TRUE)
			data <- data[,-1]
		}else{
			data <- merge(data,qtci,by="id",all.x=TRUE)
		}

		data$qtci <- round(data$qt/(data$rr/1000)^data$exponent,0)
		
		#if(length(which(info$visit=="Baseline"))==0) data$qbase <- rep(1,nrow(data))
		#data$qbase <- rep(1,nrow(data))
		
		####Plot QTc results
		probs <- 0:info$quantiles/info$quantiles
	
		qCPtherapeutic <- quantile(data$rr[!is.na(data$rr) & data$gender==1 & data$qbase==1],probs,na.rm=TRUE)
		qCPsupra <- quantile(data$rr[!is.na(data$rr) & data$gender==0 & data$qbase==1],probs,na.rm=TRUE)

		qtpred <- qtpredsas[qtpredsas$baseline=="Yes" & qtpredsas$correction=="QTcI",]	
		qtpred$rr <- exp(qtpred$LNRR)*1000

		ExposureResponse <- data.frame(CpMid=rep(0,100+length(probs)*4-2),Low=rep(0,100+length(probs)*4-2),High=rep(0,100+length(probs)*4-2),Response=rep(0,100+length(probs)*4-2),Lower=rep(NA,100+length(probs)*4-2),Upper=rep(NA,100+length(probs)*4-2))
		ExposureResponse$Quantile <- 100*c(probs[1:info$quantiles],probs,probs[1:info$quantiles],probs,rep(NA,100))
		ExposureResponse$Low <- c(as.numeric(qCPtherapeutic[-length(qCPtherapeutic)]),
											as.numeric(qCPtherapeutic),
											as.numeric(qCPsupra[-length(qCPsupra)]),
											as.numeric(qCPsupra),
											qtpred$rr[qtpred$TRT==-999],qtpred$rr[qtpred$TRT==-9999])
											
		ExposureResponse$High <- c(as.numeric(qCPtherapeutic[-1]),
											as.numeric(qCPtherapeutic),
											as.numeric(qCPsupra[-1]),
											as.numeric(qCPsupra),
											qtpred$rr[qtpred$TRT==-999],qtpred$rr[qtpred$TRT==-9999])

		ExposureResponse$CpMid <- (ExposureResponse$High-ExposureResponse$Low)/2 + ExposureResponse$Low
		ExposureResponse$logCpMid <- exp((log(ExposureResponse$High)-log(ExposureResponse$Low))/2 + log(ExposureResponse$Low))
		ExposureResponse$CpMedian <- ExposureResponse$CpMid
		ExposureResponse$logCpMedian <- ExposureResponse$logCpMid

		ExposureResponse$N <- rep(NA,nrow(ExposureResponse))

		ExposureResponse$trt <- c(rep(1,length(probs)*2-1),rep(0,length(probs)*2-1),rep(-999,50),rep(-9999,50))			

		ExposureResponse$type <- c(rep(1,length(probs)-1),rep(2,length(probs)),rep(3,length(probs)-1),rep(4,length(probs)),rep(5,50),rep(6,50))

		for(j in c("qt","qtcb","qtcf","qtci",CorrectionSponsor)){
			if(j=="qt"){
				qtpred <- qtpredsas[qtpredsas$baseline=="Yes" & qtpredsas$correction=="QTcI",]	
				qtpred$rr <- exp(qtpred$LNRR)*1000
				qtpred$Pred <- exp(qtpred$Pred)
				qtpred$Lower <- exp(qtpred$Lower)
				qtpred$Upper <- exp(qtpred$Upper)
			}else{
				if(j%in%c("qtcb","qtcf","qtci")){
					if(j=="qtcb") qtpred <- qtpredsas[qtpredsas$baseline=="No" & qtpredsas$correction=="QTcB",]
					if(j=="qtcf") qtpred <- qtpredsas[qtpredsas$baseline=="No" & qtpredsas$correction=="QTcF",]
					if(j=="qtci") qtpred <- qtpredsas[qtpredsas$baseline=="No" & qtpredsas$correction=="QTcI",]
					qtpred$rr <- qtpred$LNRR*1000 +1000
				}else{
					if(j=="sqtci"){
						 qtpred <- qtpredsasSponsor[qtpredsasSponsor$baseline=="No" & qtpredsasSponsor$correction=="qtci",]
					}else{
						 qtpred <- qtpredsasSponsor[qtpredsasSponsor$baseline=="No" & qtpredsasSponsor$correction==j,]						
					}
					qtpred$rr <- qtpred$lnrr*1000+1000
				}	

			}

			if(j=="qt"){
				qCPtherapeutic <- quantile(data$rr[!is.na(data$rr) & data$gender==1 & data$qbase==1],probs,na.rm=TRUE)
				qCPsupra <- quantile(data$rr[!is.na(data$rr) & data$gender==0 & data$qbase==1],probs,na.rm=TRUE)
			}else{
				qCPtherapeutic <- quantile(data$rr[!is.na(data$rr) & data$gender==1 & data$qbase==0],probs,na.rm=TRUE)
				qCPsupra <- quantile(data$rr[!is.na(data$rr) & data$gender==0 & data$qbase==0],probs,na.rm=TRUE)
			}

			ExposureResponse$Low <- c(as.numeric(qCPtherapeutic[-length(qCPtherapeutic)]),
											as.numeric(qCPtherapeutic),
											as.numeric(qCPsupra[-length(qCPsupra)]),
											as.numeric(qCPsupra),
											qtpred$rr[qtpred$TRT==-999],qtpred$rr[qtpred$TRT==-9999])
											
			ExposureResponse$High <- c(	as.numeric(qCPtherapeutic[-1]),
											as.numeric(qCPtherapeutic),
											as.numeric(qCPsupra[-1]),
											as.numeric(qCPsupra),
											qtpred$rr[qtpred$TRT==-999],qtpred$rr[qtpred$TRT==-9999])

			ExposureResponse$CpMid <- (ExposureResponse$High-ExposureResponse$Low)/2 + ExposureResponse$Low
			ExposureResponse$logCpMid <- exp((log(ExposureResponse$High)-log(ExposureResponse$Low))/2 + log(ExposureResponse$Low))
			ExposureResponse$CpMedian <- ExposureResponse$CpMid
			ExposureResponse$logCpMedian <- ExposureResponse$logCpMid

			qtpred$TRT <- as.character(qtpred$TRT)
			eval(parse(text=paste("data$qtcorrection <- as.numeric(as.character(data$",j,"))",sep="")))

			for(i in 1:(length(probs)-1)){
				if(j=="qt"){
					if(i==1) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & !is.na(data$rr)  & !is.na(data$qtcorrection) & data$qbase==1,]
					if(i>1 & i<(length(probs)-1)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==1,]
					if(i==(length(probs)-1)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==1,]	
				}else{
					if(i==1) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & !is.na(data$rr)  & !is.na(data$qtcorrection) & data$qbase==0,]
					if(i>1 & i<(length(probs)-1)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==0,]
					if(i==(length(probs)-1)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==0,]
				}

				ddqtc <- as.numeric(unlist(by(temp$qtcorrection,temp$id,mean,na.rm=TRUE)))
				conc <- as.numeric(unlist(by(temp$rr,temp$id,mean,na.rm=TRUE)))

				ExposureResponse$Response[i] <- mean(ddqtc)
				ExposureResponse$Upper[i] <- ExposureResponse$Response[i] + 1.645*sd(ddqtc)/sqrt(length(ddqtc))
				ExposureResponse$Lower[i] <- ExposureResponse$Response[i] - 1.645*sd(ddqtc)/sqrt(length(ddqtc))
				ExposureResponse$CpMedian[i] <- median(conc,na.rm=TRUE)
				ExposureResponse$N[i] <- length(conc)
			}

			for(i in (length(probs)*2):(length(probs)*3-2)){
				if(j=="qt"){
					if(i==(length(probs)*2)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==1,]
					if(i>(length(probs)*2) & i<(length(probs)*3-2)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==1,]
					if(i==(length(probs)*3-2)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==1,]
				}else{
					if(i==(length(probs)*2)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==0,]
					if(i>(length(probs)*2) & i<(length(probs)*3-2)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr<ExposureResponse$High[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==0,]
					if(i==(length(probs)*3-2)) temp <- data[data$gender==ExposureResponse$trt[i] & data$rr>=ExposureResponse$Low[i] & !is.na(data$rr) & !is.na(data$qtcorrection) & data$qbase==0,]
				}

				ddqtc <- as.numeric(unlist(by(temp$qtcorrection,temp$id,mean,na.rm=TRUE)))
				conc <- as.numeric(unlist(by(temp$rr,temp$id,mean,na.rm=TRUE)))
	
				ExposureResponse$Response[i] <- mean(ddqtc)
				ExposureResponse$Upper[i] <- ExposureResponse$Response[i] + 1.645*sd(ddqtc)/sqrt(length(ddqtc))
				ExposureResponse$Lower[i] <- ExposureResponse$Response[i] - 1.645*sd(ddqtc)/sqrt(length(ddqtc))
				ExposureResponse$CpMedian[i] <- median(conc,na.rm=TRUE)
				ExposureResponse$N[i] <- length(conc)
			}

			ExposureResponse$logCpMedian <- ExposureResponse$CpMedian

			#Range of concentrations at therapeutic and supra therapeutic
			ExposureResponse$Response[(length(probs)*4-1):(length(probs)*4+98)] <- c(qtpred$Pred[qtpred$TRT==-999],qtpred$Pred[qtpred$TRT==-9999])
			ExposureResponse$Lower[(length(probs)*4-1):(length(probs)*4+98)] <- c(qtpred$Lower[qtpred$TRT==-999],qtpred$Lower[qtpred$TRT==-9999])
			ExposureResponse$Upper[(length(probs)*4-1):(length(probs)*4+98)] <- c(qtpred$Upper[qtpred$TRT==-999],qtpred$Upper[qtpred$TRT==-9999])
			if(j%in%c("qtcf","qtci",CorrectionSponsor)){
				ExposureResponse$Response[length(probs):(length(probs)*2-1)] <- min(ExposureResponse$Lower,na.rm=TRUE)-1			
				ExposureResponse$Response[(3*length(probs)-1):(length(probs)*4-2)] <- min(ExposureResponse$Lower,na.rm=TRUE)+1	
			}else{
				ExposureResponse$Response[length(probs):(length(probs)*2-1)] <- min(ExposureResponse$Lower,na.rm=TRUE)-5			
				ExposureResponse$Response[(3*length(probs)-1):(length(probs)*4-2)] <- min(ExposureResponse$Lower,na.rm=TRUE)+5	
			}
			ExposureResponse$Conc <- ExposureResponse$CpMedian
			ExposureResponse$logConc <- ExposureResponse$logCpMedian
	
			if(j=="qt") QTcorrection <- "QT"
			if(j=="qtcb") QTcorrection <- "QTcB"
			if(j=="qtcf") QTcorrection <- "QTcF"
			if(j=="qtci") QTcorrection <- "QTcI"	
			if(j=="sqtci") QTcorrection <- "QTcISponsor"	
			if(!j%in%c("qt","qtcb","qtcf","qtci","sqtci")) QTcorrection <- j	
				
			ExposureResponse$QTcorrection <- factor(rep(QTcorrection,nrow(ExposureResponse)))
			ExposureResponse$type <- ordered(ExposureResponse$type,levels=c(1,2,3,4,5,6))	
			assign("ExposureResponse",ExposureResponse,envir=.GlobalEnv)
			assign("ExposureResponseQTc",ExposureResponse,envir=.GlobalEnv)

			assign("probs",probs,envir=.GlobalEnv)
	
			plot <- xYplot(Cbind(Response,Lower,Upper)~Conc|QTcorrection,subset=!is.na(Response),
						  	data=ExposureResponse,
						  	layout=c(1,1),
							aspect=2/3,
							groups=type,
							xlim=c(min(ExposureResponse$Low,na.rm=TRUE),max(ExposureResponse$High,na.rm=TRUE)),
							#ylim=c(250,550),
							#ylim=c(floor(min(data$qt[data$qbase==1],na.rm=TRUE)),ceiling(max(data$qt[data$qbase==1],na.rm=TRUE))),
							key=list(x=0.0,y=1.08,corner=c(0,1),border=FALSE,transparent=TRUE,columns=2,between=1,between.columns=2,text.width.multiplier=1,
                        			text = list(c("Male median RR quantiles",
											"Female median RR quantiles","Mean (90% CI) predicted"),cex=c(1)),
							lines=list(type=c("p","p","l"),lty=c(1),pch=c(15,16,1),cex=c(1.2),col=c("blue","red","black"),lwd=c(3,3,3))),
                      			 	xlab=list("RR (ms)",cex=1.5),
       						ylab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),
                       				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       				scales=list(cex=1.5),
                       				panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>4*length(probs)],y[subscripts>4*length(probs)],subscripts[subscripts>4*length(probs)],groups,type="l",method="filled bands",label.curves=FALSE, col=c(1),col.fill=c("lightblue","lightpink"), lty=c(1),pch=c(1),cex=c(2), lwd=c(3),...)
								if(!is.na(ExposureResponse$Response[1])) panel.xYplot(x[subscripts<length(probs)],y[subscripts<length(probs)],subscripts[subscripts<length(probs)],groups[subscripts<length(probs)],type=c("p"),methods="bars",label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(15),cex=c(0.9), lwd=c(1.5),...)
								if(!is.na(ExposureResponse$Response[1])) panel.xYplot(x[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],y[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],subscripts[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],type=c("o"),label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
								if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],y[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],subscripts[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],type=c("p"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(16),cex=c(0.9), lwd=c(1.5),...)
								if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],y[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],subscripts[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],type=c("o"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)
							}, 
							par.strip.text=list(cex=1.5))	
	
			if(!is.na(info$output)){
				outputGraph(paste(QTcorrection,"RRzoom",sep=""),info$device,height=9,width=12)
				#win.metafile(height=9,width=12,file=paste(QTcorrection,"RRzoom.wmf",sep=""))	
				par(oma=c(4,0,8,0))
				print(plot)
				dev.off()
			}else{
				print(plot)
			}	

			if(j=="qt"){
				plot <- xYplot(Cbind(Response,Lower,Upper)~Conc|QTcorrection,subset=!is.na(Response),
						  	data=ExposureResponse,
						  	layout=c(1,1),
                       				aspect=2/3,
							groups=type,
							xlim=c(min(ExposureResponse$Low,na.rm=TRUE),max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(exp(qtpredsas$Lower[qtpredsas$TRT%in%c(-999,-9999) & qtpredsas$baseline=="Yes" & qtpredsas$correction%in%c("QTcI")]),na.rm=TRUE)-10,max(exp(qtpredsas$Lower[qtpredsas$TRT%in%c(-999,-9999) & qtpredsas$baseline=="Yes" & qtpredsas$correction%in%c("QTcI")]),na.rm=TRUE)+25),
							#ylim=c(floor(min(data$qt[data$qbase==1],na.rm=TRUE)),ceiling(max(data$qt[data$qbase==1],na.rm=TRUE))),
							key=list(x=0.0,y=1.08,corner=c(0,1),border=FALSE,transparent=TRUE,columns=2,between=1,between.columns=2,text.width.multiplier=1,
                       				text = list(c("Male median RR quantiles",
											"Female median RR quantiles","Mean (90% CI) predicted"),cex=c(1)),
							lines=list(type=c("p","p","l"),lty=c(1),pch=c(15,16,1),cex=c(1.2),col=c("blue","red","black"),lwd=c(3,3,3))),
                       				xlab=list("RR (ms)",cex=1.5),
       						ylab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),
                      	 			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       				scales=list(cex=1.5),
                       				panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>4*length(probs)],y[subscripts>4*length(probs)],subscripts[subscripts>4*length(probs)],groups,type="l",method="filled bands",label.curves=FALSE, col=c(1),col.fill=c("lightblue","lightpink"), lty=c(1),pch=c(1),cex=c(2), lwd=c(3),...)
								if(!is.na(ExposureResponse$Response[1])) panel.xYplot(x[subscripts<length(probs)],y[subscripts<length(probs)],subscripts[subscripts<length(probs)],groups[subscripts<length(probs)],type=c("p"),methods="bars",label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(15),cex=c(0.9), lwd=c(1.5),...)
								if(!is.na(ExposureResponse$Response[1])) panel.xYplot(x[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],y[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],subscripts[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],type=c("o"),label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
								if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],y[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],subscripts[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],type=c("p"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(16),cex=c(0.9), lwd=c(1.5),...)
								if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],y[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],subscripts[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],type=c("o"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)
							}, 
							par.strip.text=list(cex=1.5))	
	

			}else{
				ExposureResponse$Response[length(probs):(length(probs)*2-1)] <- min(qtpredsas$Lower[qtpredsas$TRT%in%c(-999,-9999) & qtpredsas$baseline=="No" & qtpredsas$correction%in%c("QTcF","QTcB","QTcI")],na.rm=TRUE)-5
				ExposureResponse$Response[(3*length(probs)-1):(length(probs)*4-2)] <- min(qtpredsas$Lower[qtpredsas$TRT%in%c(-999,-9999) & qtpredsas$baseline=="No" & qtpredsas$correction%in%c("QTcF","QTcB","QTcI")],na.rm=TRUE)	
				
				plot <- xYplot(Cbind(Response,Lower,Upper)~Conc|QTcorrection,subset=!is.na(Response),
						  	data=ExposureResponse,
						  	layout=c(1,1),
                       				aspect=2/3,
							groups=type,
							xlim=c(min(ExposureResponse$Low,na.rm=TRUE),max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(qtpredsas$Lower[qtpredsas$TRT%in%c(-999,-9999) & qtpredsas$baseline=="No" & qtpredsas$correction%in%c("QTcF","QTcB","QTcI")],na.rm=TRUE)-10,max(qtpredsas$Lower[qtpredsas$TRT%in%c(-999,-9999) & qtpredsas$baseline=="No" & qtpredsas$correction%in%c("QTcF","QTcB","QTcI")],na.rm=TRUE)+25),
							#ylim=c(floor(min(data$qt[data$qbase==1],na.rm=TRUE)),ceiling(max(data$qt[data$qbase==1],na.rm=TRUE))),
							key=list(x=0.0,y=1.08,corner=c(0,1),border=FALSE,transparent=TRUE,columns=2,between=1,between.columns=2,text.width.multiplier=1,
                        			text = list(c("Male median RR quantiles",
											"Female median RR quantiles","Mean (90% CI) predicted"),cex=c(1)),
							lines=list(type=c("p","p","l"),lty=c(1),pch=c(15,16,1),cex=c(1.2),col=c("blue","red","black"),lwd=c(3,3,3))),
                       				xlab=list("RR (ms)",cex=1.5),
       						ylab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),
                       				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       				scales=list(cex=1.5),
                       				panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>4*length(probs)],y[subscripts>4*length(probs)],subscripts[subscripts>4*length(probs)],groups,type="l",method="filled bands",label.curves=FALSE, col=c(1),col.fill=c("lightblue","lightpink"), lty=c(1),pch=c(1),cex=c(2), lwd=c(3),...)
								if(!is.na(ExposureResponse$Response[1])) panel.xYplot(x[subscripts<length(probs)],y[subscripts<length(probs)],subscripts[subscripts<length(probs)],groups[subscripts<length(probs)],type=c("p"),methods="bars",label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(15),cex=c(0.9), lwd=c(1.5),...)
								if(!is.na(ExposureResponse$Response[1])) panel.xYplot(x[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],y[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],subscripts[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],type=c("o"),label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
								if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],y[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],subscripts[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],type=c("p"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(16),cex=c(0.9), lwd=c(1.5),...)
								if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],y[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],subscripts[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],type=c("o"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)
							}, 
							par.strip.text=list(cex=1.5))	
			}
			
			if(!is.na(info$output)){
				outputGraph(paste(QTcorrection,"RR",sep=""),info$device,height=9,width=12)
				#win.metafile(height=9,width=12,file=paste(QTcorrection,"RR.wmf",sep=""))	
				par(oma=c(4,0,8,0))
				print(plot)
				dev.off()
			}else{
				par(oma=c(0,0,8,0))
				print(plot)
			}

			eval(parse(text=paste(j,"plotQuantile <- plot",sep="")))
		}
	
		#Check whether qtci is used as correction method
		data$qtci.bs <- round(data$qt.bs/(data$rr.bs/1000)**data$exponent,0)
		data$qtci.cfb <- data$qtci-data$qtci.bs 
		
		assign("data",data,envir=.GlobalEnv)
		
		paraest <- read.table(file="qtciparaest.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
		iivest <- read.table(file="qtciiiv.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
		random <- read.table(file="qtcirandom.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
		if(length(CorrectionSponsor)>0){
			paraestSponsor <- read.table("qtcsponsorparaest.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
			iivSponsor <- read.table("qtcsponsoriiv.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")				
			randomSponsor <- read.table("qtcsponsorrandom.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")				
		} 	

		table <- data.frame(matrix(nrow=6,ncol=4))
		names(table) <- c("Parameter","Estimate","pvalue","IIV")
		
		table[1,1] <- "Model: log QT = QT1000 + slope*log RR + epsilon"
		table[2,1] <- "QT1000 Male (ms)"
		table[3,1] <- "QT1000 Female (ms)"
		table[4,1] <- "Slope Male"
		table[5,1] <- "Slope Female"
		table[6,1] <- "Residual Variability (CV%)"

		table[2,2] <- paste(round(exp(paraest[1,3]),0)," (",round(exp(paraest[1,9]),0),"; ",round(exp(paraest[1,10]),0),")",sep="")
		table[3,2] <- paste(round(exp(paraest[1,3]+paraest[2,3]),0)," (",round(exp(paraest[1,9]+paraest[2,9]),0),"; ",round(exp(paraest[1,10]+paraest[2,10]),0),")",sep="")

		table[4,2] <- paste(round(paraest[4,3],3)," (",round(paraest[4,9],3),"; ",round(paraest[4,10],3),")",sep="")
		table[5,2] <- paste(round(paraest[4,3]+paraest[5,3],3)," (",round(paraest[4,9]+paraest[5,9],3),"; ",round(paraest[4,10]+paraest[5,10],3),")",sep="")

		table[2,3] <- as.character(paraest[1,7])
		table[3,3] <- as.character(paraest[2,7])

		table[4,3] <- as.character(paraest[4,7])
		table[5,3] <- as.character(paraest[5,7])

		if(length(unique(data$gender))==1){
			table[3,2] <- paste(round(exp(paraest[1,3]+paraest[2,3]),0)," (",round(exp(paraest[1,9]),0),"; ",round(exp(paraest[1,10]),0),")",sep="")
			table[4,2] <- paste(round(paraest[3,3],3)," (",round(paraest[3,9],3),"; ",round(paraest[3,10],3),")",sep="")
			table[5,2] <- paste(round(paraest[3,3],3)," (",round(paraest[3,9],3),"; ",round(paraest[3,10],3),")",sep="")
		
			table[2,3] <- as.character(paraest[1,7])
			table[3,3] <- as.character(paraest[1,7])

			table[4,3] <- as.character(paraest[3,7])
			table[5,3] <- as.character(paraest[3,7])
		}
	
		if(info$corr=="diag"){
			table[6,2] <- round(100*sqrt(iivest[3,3]),2)			
		}else{
			table[6,2] <- round(100*sqrt(iivest[4,3]),2)
		}
		if(info$corr=="diag"){
			table[2,4] <- round(sqrt(iivest[1,3]),2)
			table[4,4] <- round(sqrt(iivest[2,3]),2)
		}else{
			table[2,4] <- round(sqrt(iivest[1,3]),2)
			table[4,4] <- round(sqrt(iivest[3,3]),2)	
		}
		
		table[1,2] <- ""
		table[c(1,6),3] <- ""
		table[c(1,3,5,6),4] <- ""
		
		if(!is.na(info$output)){
			write.table(table, file=paste("tableqtciparms.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
		}else{
			cat("\nQT-RR parameter estimates using off-treatment data\n")
			print(table)
		}

		tableqtci <- table

		table <- data.frame(matrix(nrow=13,ncol=4))
		names(table) <- c("Parameter","Estimate","pvalue","IIV")
		
		table[1,1] <- "Model: QTc = QT1000 + slope* RR + epsilon"
		table[2,1] <- "QTcI1000 Male (ms)"
		table[3,1] <- "QTcI1000 Female difference (ms)"
		table[4,1] <- "QTcI Slope"
		table[5,1] <- "QTcI Residual Variability (SD)"

		table[6,1] <- "QTcB1000 Male (ms)"
		table[7,1] <- "QTcB1000 Female difference (ms)"
		table[8,1] <- "QTcB Slope"
		table[9,1] <- "QTcB Residual Variability (SD)"

		table[10,1] <- "QTcF1000 Male (ms)"
		table[11,1] <- "QTcF1000 Female difference (ms)"
		table[12,1] <- "QTcF Slope"
		table[13,1] <- "QTcF Residual Variability (SD)"

		#QTcI
		table[2,2] <- paste(round(paraest[7,3],0)," (",round(paraest[7,9],0),"; ",round(paraest[7,10],0),")",sep="")
		table[3,2] <- paste(round(paraest[8,3],0)," (",round(paraest[8,9],0),"; ",round(paraest[8,10],0),")",sep="")

		table[4,2] <- paste(round(paraest[10,3],3)," (",round(paraest[10,9],3),"; ",round(paraest[10,10],3),")",sep="")

		if(info$corr=="diag"){
			table[5,2] <- round(100*sqrt(iivest[6,3]),2)
		}else{
			table[5,2] <- round(100*sqrt(iivest[8,3]),2)			
		}
		
		table[2,3] <- as.character(paraest[7,7])
		table[3,3] <- as.character(paraest[8,7])

		table[4,3] <- as.character(paraest[10,7])

		if(info$corr=="diag"){
			table[2,4] <- round(sqrt(iivest[4,3]),2)
			table[4,4] <- round(sqrt(iivest[5,3]),2)
		}else{
			table[2,4] <- round(sqrt(iivest[5,3]),2)
			table[4,4] <- round(sqrt(iivest[7,3]),2)			
		}

		#QTcB
		table[6,2] <- paste(round(paraest[11,3],0)," (",round(paraest[11,9],0),"; ",round(paraest[11,10],0),")",sep="")
		table[7,2] <- paste(round(paraest[12,3],0)," (",round(paraest[12,9],0),"; ",round(paraest[12,10],0),")",sep="")
		table[8,2] <- paste(round(paraest[14,3],3)," (",round(paraest[14,9],3),"; ",round(paraest[14,10],3),")",sep="")

		if(info$corr=="diag"){
			table[9,2] <- round(100*sqrt(iivest[9,3]),2)
		}else{
			table[9,2] <- round(100*sqrt(iivest[12,3]),2)			
		}
		
		table[6,3] <- as.character(paraest[11,7])
		table[7,3] <- as.character(paraest[12,7])

		table[8,3] <- as.character(paraest[14,7])

		if(info$corr=="diag"){
			table[6,4] <- round(sqrt(iivest[7,3]),2)
			table[8,4] <- round(sqrt(iivest[8,3]),2)
		}else{
			table[6,4] <- round(sqrt(iivest[9,3]),2)
			table[8,4] <- round(sqrt(iivest[11,3]),2)			
		}
	

		#QTcF
		table[10,2] <- paste(round(paraest[15,3],0)," (",round(paraest[15,9],0),"; ",round(paraest[15,10],0),")",sep="")
		table[11,2] <- paste(round(paraest[16,3],0)," (",round(paraest[16,9],0),"; ",round(paraest[16,10],0),")",sep="")

		table[12,2] <- paste(round(paraest[18,3],3)," (",round(paraest[18,9],3),"; ",round(paraest[18,10],3),")",sep="")

		if(info$corr=="diag"){
			table[13,2] <- round(100*sqrt(iivest[12,3]),2)
		}else{
			table[13,2] <- round(100*sqrt(iivest[16,3]),2)			
		}
		
		table[10,3] <- as.character(paraest[15,7])
		table[11,3] <- as.character(paraest[16,7])

		table[12,3] <- as.character(paraest[18,7])

		if(info$corr=="diag"){
			table[10,4] <- round(sqrt(iivest[10,3]),2)
			table[12,4] <- round(sqrt(iivest[11,3]),2)
		}else{
			table[10,4] <- round(sqrt(iivest[13,3]),2)
			table[12,4] <- round(sqrt(iivest[15,3]),2)			
		}
		
		table[1,2] <- ""
		table[c(1,5,9,13),3] <- ""
		table[c(1,3,5,7,9,11,13),4] <- ""

		if(!is.na(info$output)){
			write.table(table, file=paste("tableqtcparms.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
		}else{
			cat("\nQTc-RR parameter estimates using on-treatment data\n")
			print(table)
		}
		
		random <- random[random$Effect=="newrr",]
		datainfo <- data[match(unique(data$id),data$id),c("id","trt","gender")]

		if(is.numeric(random$ID)){
			datainfo$tempid <- as.numeric(datainfo$id)
			random <- merge(random,datainfo,by.x="ID",by.y="tempid",all.x=TRUE)
		}else{
			random <- merge(random,datainfo,by.x="ID",by.y="id",all.x=TRUE)
		}
		random <- random[!c(random$ID%in%c(-999,-9999)),]

		random$qtci <- paraest$Estimate[paraest$Effect=="newrr" & paraest$baseline=="No" & paraest$correction=="QTcI"]+ random$Estimate
		random$qtcf <- paraest$Estimate[paraest$Effect=="newrr" & paraest$baseline=="No" & paraest$correction=="QTcF"]+ random$Estimate
		random$qtcb <- paraest$Estimate[paraest$Effect=="newrr" & paraest$baseline=="No" & paraest$correction=="QTcB"]+ random$Estimate	
		
		if(length(CorrectionSponsor)>0){
			randomSponsor <- randomSponsor[randomSponsor$Effect=="newrr",]
			if(is.numeric(randomSponsor$ID)){
				datainfo$tempid <- as.numeric(datainfo$id)
				randomSponsor <- merge(randomSponsor,datainfo,by.x="ID",by.y="tempid",all.x=TRUE)
			}else{
				randomSponsor <- merge(randomSponsor,datainfo,by.x="ID",by.y="id",all.x=TRUE)
			}

			randomSponsor <- randomSponsor[!c(randomSponsor$ID%in%c(-999,-9999)),]
			for(i in 1:length(CorrectionSponsor)){
				eval(parse(text=paste("randomSponsor$",CorrectionSponsor[i]," <- paraestSponsor$Estimate[paraestSponsor$Effect==\"newrr\" & paraestSponsor$baseline==\"No\" & paraestSponsor$correction==\"",ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),"\"]+ randomSponsor$Estimate",sep="")))		
			}
		}

		table <- data.frame(matrix(nrow=(4+length(CorrectionSponsor)),ncol=2))
		names(table) <- c("Correction","SSS")
		table[,1] <- c("N","QTcB","QTcF","QTcI",CorrectionSponsor)
		table[1,2] <- nrow(random[random$correction=="QTcB",])
		table[2,2] <- mean((random$qtcb[random$correction=="QTcB"]/1000)**2)
		table[3,2] <- mean((random$qtcf[random$correction=="QTcF"]/1000)**2)
		table[4,2] <- mean((random$qtci[random$correction=="QTcI"]/1000)**2)

		if(length(CorrectionSponsor)>0){
			for(i in 1:length(CorrectionSponsor)){
				table[(4+i),2] <- mean((randomSponsor[randomSponsor$correction==ifelse(CorrectionSponsor[i]=="sqtci","qtci",CorrectionSponsor[i]),CorrectionSponsor[i]]/1000)**2)
			}
		}

		#table[2,] <- cbind(sum((random$qtcb[random$gender==1 & random$correction=="QTcB"]/1000)**2)/nrow(random[random$gender==1 & random$correction=="QTcB",]),sum((random$qtcb[random$gender==0 & random$correction=="QTcB"]/1000)**2)/nrow(random[random$gender==0 & random$correction=="QTcB",]),sum((random$qtcb[random$correction=="QTcB"]/1000)**2)/nrow(random[random$correction=="QTcB",]))
		#table[3,] <- cbind(sum((random$qtcf[random$gender==1 & random$correction=="QTcF"]/1000)**2)/nrow(random[random$gender==1 & random$correction=="QTcF",]),sum((random$qtcf[random$gender==0 & random$correction=="QTcF"]/1000)**2)/nrow(random[random$gender==0 & random$correction=="QTcF",]),sum((random$qtcf[random$correction=="QTcF"]/1000)**2)/nrow(random[random$correction=="QTcF",]))
		#table[4,] <- cbind(sum((random$qtci[random$gender==1 & random$correction=="QTcI"]/1000)**2)/nrow(random[random$gender==1 & random$correction=="QTcI",]),sum((random$qtci[random$gender==0 & random$correction=="QTcI"]/1000)**2)/nrow(random[random$gender==0 & random$correction=="QTcI",]),sum((random$qtci[random$correction=="QTcI"]/1000)**2)/nrow(random[random$correction=="QTcI",]))
		table$SSS[2:(4+length(CorrectionSponsor))] <- signif(table$SSS[2:(4+length(CorrectionSponsor))],digits=4)

		if(!is.na(info$output)){
			write.table(table[1:4,2], file=paste("tableqtcSSS.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
			if(length(CorrectionSponsor)>0){
				write.table(table[5:(4+length(CorrectionSponsor)),], file=paste("tableqtcSSSsponsor.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
			}else{
				tableSponsor <- data.frame(matrix(" ",nrow=1,ncol=2))
				write.table(tableSponsor, file=paste("tableqtcSSSsponsor.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
			}	
		}else{
			cat("\nSum of individual squared QTc-RR slopes using on-treatment data\n")
			print(table[1:4,1:2])

			if(length(CorrectionSponsor)>0){
				cat("\nSum of individual squared QTc-RR slopes using on-treatment data (Sponsor's correction methods)\n")
				print(table[5:(4+length(CorrectionSponsor)),])	
			}
		}
				
	}

	####################################################################################
	###### Plot QT,QTcF,QTcI vs RR
	####################################################################################

	if(!is.numeric(data$id)) data$id <- as.character(data$id)
	data <- data[order(data$id,data$rr),]
	#data <- sort.col(data,columns.to.sort="@ALL", columns.to.sort.by=c("id","rr"),ascending=TRUE)

	data$plotQT <- rep(0,nrow(data))
	data$strip <- rep(0,nrow(data))

	dataQT <- data
	dataQT$plotQT <- dataQT$qt
	dataQT$strip <- rep("QT",nrow(dataQT))

	dataQTcB <- data
	dataQTcB$plotQT <- dataQTcB$qtcb
	dataQTcB$strip <- rep("QTcB",nrow(dataQTcB))

	dataQTcF <- data
	dataQTcF$plotQT <- dataQTcF$qtcf
	dataQTcF$strip <- rep("QTcF",nrow(dataQTcF))

	dataQTcI <- data
	if(!is.null(data$qtci)){
		dataQTcI$plotQT <- data$qtci
		dataQTcI$strip <-  rep("QTcI",nrow(dataQTcI)) 
		
		dataplot <- rbind(dataQT,dataQTcB,dataQTcF,dataQTcI)
		dataplot$strip <- ordered(dataplot$strip,levels=c("QT","QTcB","QTcF","QTcI"))
		
		if(sum(which(info$visit=="Baseline")==TRUE)>0){
			plot0 <- xyplot(plotQT~rr|strip,data=dataplot,subset=day%in%info$days[which(info$visit=="Baseline")] & strip%in%c("QT","QTcB","QTcF","QTcI"),aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
			
			if(!is.na(info$output)){
				outputGraph("QTcorrectionBaseline",info$device,height=9,width=9)
				#win.metafile(height=9,width=9,file="QTcorrectionBaseline.wmf")	
				print(plot0)
				dev.off()
			}else{
				print(plot0)
			}
		}
		
		plotAll <- xyplot(plotQT~rr|strip,data=dataplot,subset=strip%in%c("QT","QTcB","QTcF","QTcI"),aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		if(!is.na(info$output)){
			outputGraph("QTcorrectionAll",info$device,height=9,width=9)
			#win.metafile(height=9,width=9,file="QTcorrectionAll.wmf")
			print(plotAll)
			dev.off()
		}else{
			print(plotAll)
		}
		
		plot0 <- xyplot(plotQT~rr|strip,data=dataplot,subset=day%in%info$days[which(info$visit!="Baseline")] & strip%in%c("QT","QTcB","QTcF","QTcI"),aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		if(!is.na(info$output)){
			outputGraph("QTcorrectionPostDose",info$device,height=9,width=9)
			#win.metafile(height=9,width=9,file="QTcorrectionPostDose.wmf")
			print(plot0)
			dev.off()
		}else{
			print(plot0)
		}

	}else{
		#dataQTc <- data
		#dataQTc$plotQT <- eval(parse(text=paste("dataQTc$",info$correction,sep="")))
		#dataQTc$strip  <- rep(paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep=""),nrow(dataQTc))

		dataplot <- rbind(dataQT,dataQTcB,dataQTcF)#,dataQTc)
		dataplot$strip <- ordered(dataplot$strip,levels=c("QT","QTcB","QTcF"))#,paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")))
		
		if(sum(which(info$visit=="Baseline")==TRUE)>0){
			plot0 <- xyplot(plotQT~rr|strip,data=dataplot,subset=day%in%info$days[which(info$visit=="Baseline")] & strip%in%c("QT","QTcB","QTcF"),aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
			if(!is.na(info$output)){
				outputGraph("QTcorrectionBaseline",info$device,height=9,width=9)
				#win.metafile(height=9,width=9,file="QTcorrectionBaseline.wmf")	
				print(plot0)
				dev.off()	
			}else{
				print(plot0)
			}
		}
	
		plotAll <- xyplot(plotQT~rr|strip,data=dataplot,subset=strip%in%c("QT","QTcB","QTcF"),aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))

		if(!is.na(info$output)){
			outputGraph("QTcorrectionAll",info$device,height=9,width=9)
			#win.metafile(height=9,width=9,file="QTcorrectionAll.wmf")	
			print(plotAll)
			dev.off()	
		}else{
			print(plotAll)
		}			
	}
	
	if(info$qtci==TRUE){
		corrections <- c("qt","qtcb","qtcf","qtci",CorrectionSponsor)	
	}else{
		if(length(which(CorrectionSponsor=="sqtci"))>0){
			CorrectionSponsor[which(CorrectionSponsor=="sqtci")] <- "qtci"
		}
		corrections <- c("qt","qtcb","qtcf",CorrectionSponsor)	
					
	}
	
	for(j in corrections){
			temp <- data
			temp$plotQT <- eval(parse(text=paste("temp$",j,sep="")))

			if(j=="qt") QTcorrection <- "QT"
			if(j=="qtcb") QTcorrection <- "QTcB"
			if(j=="qtcf") QTcorrection <- "QTcF"
			if(j=="qtci") QTcorrection <- "QTcI"	
			if(j=="sqtci") QTcorrection <- "QTcISponsor"	
			if(!j%in%c("qt","qtcb","qtcf","qtci","sqtci")) QTcorrection <- j	
			
			temp$strip <- rep(QTcorrection,nrow(temp))
			plot0 <- xyplot(plotQT~rr|strip,data=temp,aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
				
			if(!is.na(info$output)){
				outputGraph(paste(QTcorrection,"RRIndividualAll",sep=""),info$device,height=9,width=9)
				#win.metafile(height=9,width=9,file=paste(QTcorrection,"RRIndividualAll.wmf",sep=""))	
				print(plot0)
				dev.off()	
			}else{
				print(plot0)
			}
	}
	
	if(!info$correction%in%c("qtcf","qtcb","qtci")){
		dataQTc <- data
		dataQTc$plotQT <- eval(parse(text=paste("dataQTc$",info$correction,sep="")))
		dataQTc$strip  <- rep(paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep=""),nrow(dataQTc))

		dataplot <- rbind(dataQT,dataQTcB,dataQTcF,dataQTc)
		dataplot$strip <- ordered(dataplot$strip,levels=c("QT","QTcB","QTcF",paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")))
			
		if(sum(which(info$visit=="Baseline")==TRUE)>0){
			plot1 <- xyplot(plotQT~rr|strip,data=dataplot,layout=c(2,2),subset=strip%in%c("QT","QTcB","QTcF",paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")) & day%in%info$days[which(info$visit=="Baseline")],aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
			
			if(!is.na(info$output)){
				outputGraph("QTcorrectionSponsorBaseline",info$device,height=9,width=9)
				#win.metafile(height=9,width=9,file="QTcorrectionSponsorBaseline.wmf")	
				print(plot1)
				dev.off()
			}else{
				print(plot1)
			}
		}
		
		plot1 <- xyplot(plotQT~rr|strip,data=dataplot,layout=c(2,2),subset=strip%in%c("QT","QTcB","QTcF",paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")),aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))

		if(!is.na(info$output)){
			outputGraph("QTcorrectionSponsorAll",info$device,height=9,width=9)
			#win.metafile(height=9,width=9,file="QTcorrectionSponsorAll.wmf")	
			print(plot1)
			dev.off()			
		}else{
			print(plot1)
		}	

		plot1 <- xyplot(plotQT~rr|strip,data=dataplot,layout=c(2,2),subset=day%in%info$days[which(info$visit!="Baseline")] & strip%in%c("QT","QTcB","QTcF",paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")),aspect="fill",groups=id,panel=panel.superpose.2,type="l",ylab=list("QT interval (ms)",cex=1.5),xlab=list("RR interval (ms)",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))

		if(!is.na(info$output)){
			outputGraph("QTcorrectionSponsorPostDose",info$device,height=9,width=9)
			#win.metafile(height=9,width=9,file="QTcorrectionSponsorPostDose.wmf")	
			print(plot1)
			dev.off()			
		}else{
			print(plot1)
		}
	}

	if(!is.na(info$output)){
		setwd(opath)
	}

	if(info$report==TRUE){
		wdGoToBookmark("blankcorrection")
		wdNormal("")

		wdBody("The observed relationship between QT and RR interval is presented in Figure 1 together with the Bazett's (QTcB), Fridericia (QTcF), and individual correction (QTcI).")
		wdBody("Figure 1: QT, QTcB, QTcF, and QTcI vs. RR (Each subject's data points are connected with a line).")
		wdPlot(plotAll,width=6,height=6)

		wdBody("Table 1: Individual Correction Method (QTcI) Parameter Estimates.")
		wdTable(format(tableqtci))

		wdBody("Table 2: Average Sum of Squared Slopes (SSS) for Different QT-RR Correction Methods.")
		wdTable(format(table))

		wdBody("The mean (90% CI) estimated linear mixed-effects slope between QT/QTc and RR is shown in Figure 2 together with the associated mean (90% CI) observed QT/QTc-RR quantiles.")
		wdBody("Figure 2: Mean (90% CI) Predicted QTc vs RR Relationship (solid line and shaded area) for male (blue) and female (red). The dots represent the observed median RR quantiles and associated mean (90% CI) QT/QTc. The RR quantile ranges for males (blue) and females (red) are shown along the x-axis.")
		for(j in corrections){
			eval(parse(text=paste("wdPlot(",j,"plotQuantile,width=9,height=6)",sep="")))
		}

		wdSave("QT")
	}
      options(warn = -1)
}


####################################################################################
###### Check data
####################################################################################

DataCheck <- function(data,info){

	names(data) <- casefold(names(data), upper=FALSE)

	if(sum(c("design","trt","primary","days","visit")%in%names(info)==FALSE)>0){

		cat(paste("\nYou forgot the following required items in info: \"",c("design","trt","primary","days","visit")[which(c("design","trt","primary","days","visit")%in%names(info)==FALSE)],
				"\" to your info list\n",sep=""))

		cat("The QT script will terminate now")
		stop()
	}

	if(sum(c("design","trt","label","primary","output","digits","device",
		          	"days","visit","pk","correction","saspath","report",       
				"conc","scale","intercept","quantiles","delta",
				"qtci","gof","corr",        
				"col","bin",    
				"cex","lty","lwd","pch","alpha")%in%names(info)==FALSE)>0){
		
		cat("DataCheck function: The info list has been changed and does not include all options\n")
		cat("InfoCreate function is being called from within the DataCheck function\n")
		info <- InfoCreate(info)
	}
	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	if(!is.na(info$output)){
		opath <- getwd()
		dir.create(info$output,showWarnings=FALSE)
		setwd(info$output)
	}

	if(is.null(data$id)) data$id <- data$subjid
	if(is.null(data$trt)) data$trt <- data$treat

	data$trt <- as.character(data$trt)
	data <- data[data$trt%in%info$trt[!is.na(info$trt)],]
	if(!is.numeric(data$id)) data$id <- as.character(data$id)

	if(info$correction%in%c("qtcf","qtcb","qtci")){
		if(info$correction=="qtcb") QTcorrection <- "QTcB"
		if(info$correction=="qtcf") QTcorrection <- "QTcF"
		if(info$correction=="qtci") QTcorrection <- "QTcI"	
	}else{
		QTcorrection <- paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")
	}

	if(info$conc=="meta"){
		info$pk$drugname <- info$pk$metaname
		info$pk$drugunit <- info$pk$metaunit
		data$conc <- data$meta
	} 

	if(!info$conc%in%c("conc","meta")) 	data$conc <- eval(parse(text=paste("data$",info$conc,sep="")))
	data$conc <- as.numeric(as.character(data$conc))

	data$qt <- as.numeric(as.character(data$qt))
	data$qtc <- eval(parse(text=paste("as.numeric(as.character(data$",info$correction,"))",sep="")))

	data$rr <- as.numeric(as.character(data$rr))
	data$hr <- as.numeric(as.character(data$hr))
	data$time <- as.numeric(as.character(data$time))
	data$day <- as.numeric(as.character(data$day))

	noDay <- length(unique(data$day))
	noTRT <- length(unique(data$trt))

	if(is.null(data$period) | info$design=="parallel"){
		noPeriod <- 1	
	}else{
		noPeriod <- length(unique(data$period))	
	}

	data$daytxt <- ordered(paste("Day",as.character(data$day)),levels=c(paste("Day",sort(unique(data$day)))))
	if(info$design=="crossover") data$periodtxt <- ordered(paste("Period",as.character(data$period)),levels=c(paste("Period",sort(unique(data$period)))))

	if(info$design=="parallel"){	
		plot7 <- xyplot(time~as.integer(as.factor(id))|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("Time (hours)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot8 <- xyplot(conc~as.integer(as.factor(id))|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot9 <- xyplot(rr~as.integer(as.factor(id))|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("RR Interval (ms)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot10 <- xyplot(hr~as.integer(as.factor(id))|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("Heart Rate (bpm)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot11 <- xyplot(qt~as.integer(as.factor(id))|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("QT (ms)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot12 <- xyplot(qtc~as.integer(as.factor(id))|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list(paste(QTcorrection,"(ms)"),cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot13 <- bwplot(trt~conc|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Treatment group",cex=1.5),xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot14 <- bwplot(day~conc|as.factor(paste("Treatment",as.character(trt))),data=data,aspect="fill",layout=c(noPeriod,noTRT),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Day",cex=1.5),xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot15 <- bwplot(trt~qtc|daytxt,data=data,aspect="fill",layout=c(1,noDay),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Treatment group",cex=1.5),xlab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),xlim=c(min(data$qtc,na.rm=TRUE),max(data$qtc,na.rm=TRUE)),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot16 <- bwplot(day~qtc|as.factor(paste("Treatment",as.character(trt))),data=data,aspect="fill",layout=c(1,noTRT),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Day",cex=1.5),xlab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),xlim=c(min(data$qtc,na.rm=TRUE),max(data$qtc,na.rm=TRUE)),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
	}else{
		plot7 <- xyplot(time~as.integer(as.factor(id))|daytxt*periodtxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("Time (hours)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot8 <- xyplot(conc~as.integer(as.factor(id))|daytxt*periodtxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot9 <- xyplot(rr~as.integer(as.factor(id))|daytxt*periodtxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("RR Interval (ms)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot10 <- xyplot(hr~as.integer(as.factor(id))|daytxt*periodtxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("Heart Rate (bpm)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot11 <- xyplot(qt~as.integer(as.factor(id))|daytxt*periodtxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list("QT (ms)",cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot12 <- xyplot(qtc~as.integer(as.factor(id))|daytxt*periodtxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.xyplot,type="p",pch=16,col=1,cex=.3,ylab=list(paste(QTcorrection,"(ms)"),cex=1.5),xlab=list("Subject",cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot13 <- bwplot(trt~conc|daytxt*periodtxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Treatment group",cex=1.5),xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot14 <- bwplot(day~conc|periodtxt*as.factor(as.character(trt)),data=data,aspect="fill",layout=c(noPeriod,noTRT),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Day",cex=1.5),xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
		plot15 <- bwplot(trt~qtc|periodtxt*daytxt,data=data,aspect="fill",layout=c(noPeriod,noDay),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Treatment group",cex=1.5),xlab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),xlim=c(min(data$qtc,na.rm=TRUE),max(data$qtc,na.rm=TRUE)),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1.2),scales=list(cex=1.2))
		plot16 <- bwplot(day~qtc|periodtxt*as.factor(as.character(trt)),data=data,aspect="fill",layout=c(noPeriod,noTRT),panel=panel.bwplot,pch=16,col=1,cex=1,ylab=list("Day",cex=1.5),xlab=list(paste(QTcorrection," (ms)",sep=""),cex=1.5),xlim=c(min(data$qtc,na.rm=TRUE),max(data$qtc,na.rm=TRUE)),strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),par.strip.text=list(cex=1),scales=list(cex=1.2))
	}

	if(!is.na(info$output)){
		outputGraph("IDtime",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="IDtime.wmf")
		#par(oma=c(-0,-1.2,-1,-1.6),mgp=c(3,1,.3))
		print(plot7)
		dev.off()

		outputGraph("IDconc",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="IDconc.wmf")
		#par(oma=c(-0,-1.2,-1,-1.6),mgp=c(3,1,.3))
		print(plot8)
		dev.off()

		outputGraph("IDrr",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="IDrr.wmf")
		#par(oma=c(-0,-1.2,-1,-0.6),mgp=c(3,1,.3))
		print(plot9)
		dev.off()

		outputGraph("IDhr",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="IDhr.wmf")
		#par(oma=c(-0,-1.2,-1,-0.6),mgp=c(3,1,.3))
		print(plot10)
		dev.off()

		outputGraph("IDqt",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="IDqt.wmf")
		#par(oma=c(-0,-1.2,-1,-1.6),mgp=c(3,1,.3))
		print(plot11)
		dev.off()

		outputGraph("IDqtc",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="IDqtc.wmf")
		#par(oma=c(-0,-1.2,-1,-1.6),mgp=c(3,1,.3))
		print(plot12)
		dev.off()

		outputGraph("TRTconc",info$device,height=12,width=8)
		#win.metafile(height=12,width=9,file="TRTconc.wmf")
		#par(oma=c(-0,-1.2,-1,0),mgp=c(3,1,.3))
		print(plot13)
		dev.off()

		outputGraph("Dayconc",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="Dayconc.wmf")
		#par(oma=c(-0,-1.2,-1,-1.6),mgp=c(3,1,.3))
		print(plot14)
		dev.off()

		outputGraph("TRTqtc",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="TRTqtc.wmf")
		#par(oma=c(-0,-1.2,-1,-1.6),mgp=c(3,1,.3))
		print(plot15)
		dev.off()

		outputGraph("Dayqtc",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="Dayqtc.wmf")
		#par(oma=c(-0,-1.2,-1,-1.6),mgp=c(3,1,.3))
		print(plot16)
		dev.off()

		setwd(opath)
	}else{
		print(plot7)
		print(plot8)
		print(plot9)
		print(plot10)
		print(plot11)
		print(plot12)
		print(plot13)
		print(plot14)
		print(plot15)
		print(plot16)
	}	

	if(info$report==TRUE){
		wdGoToBookmark("blankCheck")
		wdNormal("")

		wdPlot(plot7,width=6,height=6)
		wdPlot(plot8,width=6,height=6)
		wdPlot(plot9,width=6,height=6)
		wdPlot(plot10,width=6,height=6)
		wdPlot(plot11,width=6,height=6)
		wdPlot(plot12,width=6,height=6)
		wdPlot(plot13,width=6,height=6)
		wdPlot(plot14,width=6,height=6)
		wdPlot(plot15,width=6,height=6)
		wdPlot(plot16,width=6,height=6)

		wdSave("QT")
	}
	
}

####################################################################################
###### Create mean data
####################################################################################

MeanData <- function(data,info){
	options(warn = -1)

	names(data) <- casefold(names(data), upper=FALSE)

	if(sum(c("design","trt","primary","days","visit")%in%names(info)==FALSE)>0){

		cat(paste("\nYou forgot the following required items in info: \"",c("design","trt","primary","days","visit")[which(c("design","trt","primary","days","visit")%in%names(info)==FALSE)],
				"\" to your info list\n",sep=""))

		cat("The QT script will terminate now")
		stop()
	}

	if(sum(c("design","trt","label","primary","output","digits","device",
		          	"days","visit","pk","correction","saspath","report",       
				"conc","scale","intercept","quantiles","delta",
				"qtci","gof","corr",        
				"col","bin",    
				"cex","lty","lwd","pch","alpha")%in%names(info)==FALSE)>0){
		
		cat("MeanData function: The info list has been changed and does not include all options\n")
		cat("InfoCreate function is being called from within the MeanData function\n")
		info <- InfoCreate(info)
	}
	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	if(!is.na(info$output)){
		opath <- getwd()
		dir.create(info$output,showWarnings=FALSE)
		setwd(info$output)
	}

	if(	is.null(data$id)) data$id <- data$subjid
	if(	is.null(data$trt)) data$trt <- data$treat

	if(!is.numeric(data$id)) data$id <- as.character(data$id)
	if(!is.numeric(data$trt)) data$trt <- as.character(data$trt)

	for(i in names(info$trt)){
			eval(parse(text=paste("info$trt$",i," <- as.character(info$trt$",i,")",sep="")))
	}

	if(is.null(info$label)) info$label <- info$trt
	for(i in names(info$trt)){
			eval(parse(text=paste("info$label$",i," <- as.character(info$label$",i,")",sep="")))
	}
	
	if(info$conc=="meta"){
		info$pk$drugname <- info$pk$metaname
		info$pk$drugunit <- info$pk$metaunit
		data$conc <- data$meta
	} 

	if(is.null(info$pk$metaname)) info$pk$metaname <- "Metabolite"
	if(is.null(info$pk$metaunit)) info$pk$metaunit <- "amount/volume"
	if(!is.null(data$meta) & is.na(info$pk$metaname)) info$pk$metaname <- "Metabolite"
	if(!is.null(data$meta) & is.na(info$pk$metaunit)) info$pk$metaunit <- "amount/volume"
	if(!is.null(data$meta) & is.na(info$pk$metaname)) info$pk$metaname <- "Metabolite"
	if(!is.null(data$meta) & is.na(info$pk$metaunit)) info$pk$metaunit <- "amount/volume"

	if(is.null(info$pk$drugname)) info$pk$drugname <- "Drug"
	if(is.null(info$pk$drugunit)) info$pk$drugunit <- "amount/volume"

	data$time <- as.numeric(as.character(data$time)) 
	data$day <- as.numeric(as.character(data$day))
	data$qt <- as.numeric(as.character(data$qt)) 
	data <- data[!is.na(data$time),]
	#data <- data[data$trt%in%info$trt[info$trt!="NA"],]
	data <- data[data$trt%in%unlist(info$trt[which(!is.na(info$trt))]),]


	if(!info$conc%in%c("conc","meta")) 	data$conc <- eval(parse(text=paste("data$",info$conc,sep="")))
	if(info$conc=="moxi" & !is.na(info$trt$Moxi)){
		data$conc <- data$moxi
	}

	data$conc <- as.numeric(as.character(data$conc))
	if(!is.null(data$meta)) data$meta <- as.numeric(as.character(data$meta))
	if(!is.null(data$moxi)) data$moxi <- as.numeric(as.character(data$moxi))
	
	if(info$correction%in%c("qtcf","qtcb","qtci")){
		if(info$correction=="qtcb") QTcorrection <- "QTcB"
		if(info$correction=="qtcf") QTcorrection <- "QTcF"
		if(info$correction=="qtci") QTcorrection <- "QTcI"	
	}else{
		QTcorrection <- paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")
	}

	assign("info",info,envir=.GlobalEnv)

	first <- TRUE

	data$qtc <- eval(parse(text=paste("as.numeric(as.character(data$",info$correction,"))",sep="")))
	data$qtc.bs <- eval(parse(text=paste("as.numeric(as.character(data$",info$correction,".bs))",sep="")))
	data$qtc.cfb <- eval(parse(text=paste("as.numeric(as.character(data$",info$correction,".cfb))",sep="")))
	
	data$hr <- as.numeric(as.character(data$hr)) 
	data$hr.cfb <- as.numeric(as.character(data$hr.cfb)) 

	data$rr <- as.numeric(as.character(data$rr)) 
	data$rr.cfb <- as.numeric(as.character(data$rr.cfb)) 

	if(info$design=="crossover"){
		placebo <- data[data$trt==info$trt[[which(names(info$trt)=="Placebo")]],c("id","day","time","qtc","qtc.cfb")]
		names(placebo)[4:5] <- c("plac.qtc","plac.qtc.cfb")
	
		data <- merge(data,placebo,by=c("id","day","time"),all.x=TRUE)

	}

	meandata <- aggregate(data$qt,list(data$time,data$day,data$trt),mean,na.rm=TRUE)
	names(meandata) <- c("time","day","trt","qt")

	meandata$trtm <- rep(NA,nrow(meandata))
	for(i in unique(meandata$trt)){
		meandata$trtm[meandata$trt==i] <- names(info$trt[which(info$trt==i)])	
	}

	meandata$trtm <- ordered(meandata$trtm,levels=names(info$trt)[which(!is.na(info$trt))])

	meandata$visit <- rep(NA,nrow(meandata))
	for(i in 1:length(sort(unique(meandata$day)))){
		meandata$visit[meandata$day==sort(unique(meandata$day))[i]] <- info$visit[i]	
	}

	meandata$qtc <- aggregate(data$qtc,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	meandata$qt.bs <- aggregate(data$qt.bs,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	meandata$qtc.bs <- aggregate(data$qtc.bs,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	meandata$qtc.cfb <- aggregate(data$qtc.cfb,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	meandata$hr <- aggregate(data$hr,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	meandata$hr.cfb <- aggregate(data$hr.cfb,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	meandata$rr <- aggregate(data$rr,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	meandata$rr.cfb <- aggregate(data$rr.cfb,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x

	##Calculate mean placebo QTc (plac.qtc) and mean individual QTcF change from baseline (plac.qtc.cfb)
	## mean drug (parent) concentration
	## mean metabolite concentration

	meandata$plac.qtc <- rep(NA,nrow(meandata))
	meandata$plac.qtc.cfb <- rep(NA,nrow(meandata))

	meandata$conc <- rep(NA,nrow(meandata))
	meandata$conc.se <- rep(NA,nrow(meandata))
	if(!is.null(data$meta)){
		meandata$meta <- rep(NA,nrow(meandata))
		meandata$meta.se <- rep(NA,nrow(meandata))
	} 
	if(!is.null(data$moxi)){
		meandata$moxi <- rep(NA,nrow(meandata))
		meandata$moxi.se <- rep(NA,nrow(meandata))
	} 

	if(info$design=="crossover"){
		meandata$plac.qtc <- aggregate(data$plac.qtc,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
		meandata$plac.qtc.cfb <- aggregate(data$plac.qtc.cfb,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	}

	for(i in unique(meandata$trt)){
		for(j in unique(meandata$day[meandata$trt==i & !is.na(meandata$day)])){
			for(k in unique(meandata$time[meandata$trt==i & meandata$day==j  & !is.na(meandata$time)])){
				
				if(info$design=="parallel"){
					meandata$plac.qtc[meandata$trt==i & meandata$day==j & meandata$time==k] <- meandata$qtc[meandata$trt==info$trt$Placebo & meandata$day==j & meandata$time==k]
					meandata$plac.qtc.cfb[meandata$trt==i & meandata$day==j & meandata$time==k] <- meandata$qtc.cfb[meandata$trt==info$trt$Placebo & meandata$day==j & meandata$time==k]	
				}
				
				meandata$conc[meandata$trt==i & meandata$day==j & meandata$time==k] <- mean(data$conc[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)
				meandata$conc.se[meandata$trt==i & meandata$day==j & meandata$time==k] <- ifelse(length(unique(data$id[!is.na(data$conc) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$conc[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$conc) & data$trt==i & data$day==j & data$time==k]))))
				if(!is.na(info$trt$Moxi)){
					if(i==info$trt[[which(names(info$trt)=="Moxi")]] & !is.null(data$moxi)){
						meandata$moxi[meandata$trt==i & meandata$day==j & meandata$time==k] <- mean(data$moxi[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)				
						meandata$moxi.se[meandata$trt==i & meandata$day==j & meandata$time==k] <- ifelse(length(unique(data$id[!is.na(data$moxi) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$moxi[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$moxi) & data$trt==i & data$day==j & data$time==k]))))
					}
				} 
				
				if(!is.null(meandata$meta)){
					meandata$meta[meandata$trt==i & meandata$day==j & meandata$time==k] <- mean(data$meta[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)
					meandata$meta.se[meandata$trt==i & meandata$day==j & meandata$time==k] <- ifelse(length(unique(data$id[!is.na(data$meta) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$meta[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$meta) & data$trt==i & data$day==j & data$time==k]))))
				}
			}
		}
	}

	## Calculate mean time-matched placebo corrected QTc (qtc.cfp)
	if(info$design=="parallel"){
		meandata$qtc.cfp <- meandata$qtc - meandata$plac.qtc 			
	}else{
		meandata$qtc.cfp <- aggregate(data$qtc - data$plac.qtc,list(data$time,data$day,data$trt),mean,na.rm=TRUE)$x
	}

	## Calculate mean time-matched placebo substracted baseline corrected QTc (ddqtc)
	meandata$ddqtc <- meandata$qtc.cfb - meandata$plac.qtc.cfb 

	## Calculate individual qtc.cfb and ddQTc by subtracting mean placebo QTc and QTc.cfb from individual 
	## QTc and QTc.cfb at each time point
	if(info$design=="parallel"){
		data$plac.qtc <- rep(NA,nrow(data))
		data$plac.qtc.cfb <- rep(NA,nrow(data))

		if(!is.na(info$trt$Placebo)){
			for(i in 1:nrow(data)){
				if(!is.na(data$day[i]) & !is.na(data$time[i])){
					data$plac.qtc[i] <- meandata$qtc[meandata$trtm=="Placebo" & meandata$day==data$day[i] & meandata$time==data$time[i]] 
					data$plac.qtc.cfb[i] <- meandata$qtc.cfb[meandata$trtm=="Placebo" & meandata$day==data$day[i] & meandata$time==data$time[i]] 
				}
			}
		}
	}	

	data$qtc.cfp <- data$qtc - data$plac.qtc 
	data$ddqtc <- data$qtc.cfb - data$plac.qtc.cfb 
	
	meandata$qtc.se <- rep(0,nrow(meandata))
	meandata$dqtc.se <- rep(0,nrow(meandata))
	meandata$ddqtc.se <- rep(0,nrow(meandata))
	meandata$hr.se <- rep(NA,nrow(meandata))
	meandata$dhr.se <- rep(NA,nrow(meandata))
	meandata$rr.se <- rep(NA,nrow(meandata))
	meandata$drr.se <- rep(NA,nrow(meandata))

	for(i in unique(meandata$trt)){
		for(j in unique(meandata$day[meandata$trt==i])){
			for(k in unique(meandata$time[meandata$trt==i & meandata$day==j])){
				meandata$qtc.se[meandata$trt==i & meandata$day==j & meandata$time==k]   <- ifelse(length(unique(data$id[!is.na(data$qtc) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$qtc[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$qtc) & data$trt==i & data$day==j & data$time==k]))))
				meandata$dqtc.se[meandata$trt==i & meandata$day==j & meandata$time==k]  <- ifelse(length(unique(data$id[!is.na(data$qtc.cfb) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$qtc.cfb[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$qtc.cfb) & data$trt==i & data$day==j & data$time==k]))))
				meandata$ddqtc.se[meandata$trt==i & meandata$day==j & meandata$time==k] <- ifelse(length(unique(data$id[!is.na(data$ddqtc) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$ddqtc[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$ddqtc) & data$trt==i & data$day==j & data$time==k]))))
				meandata$hr.se[meandata$trt==i & meandata$day==j & meandata$time==k]   <- ifelse(length(unique(data$id[!is.na(data$hr) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$hr[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$hr) & data$trt==i & data$day==j & data$time==k]))))
				meandata$dhr.se[meandata$trt==i & meandata$day==j & meandata$time==k]  <- ifelse(length(unique(data$id[!is.na(data$hr.cfb) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$hr.cfb[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$hr.cfb) & data$trt==i & data$day==j & data$time==k]))))
				meandata$rr.se[meandata$trt==i & meandata$day==j & meandata$time==k]   <- ifelse(length(unique(data$id[!is.na(data$hr) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$rr[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$rr) & data$trt==i & data$day==j & data$time==k]))))
				meandata$drr.se[meandata$trt==i & meandata$day==j & meandata$time==k]  <- ifelse(length(unique(data$id[!is.na(data$hr.cfb) & data$trt==i & data$day==j & data$time==k]))<2,0,sd(data$rr.cfb[data$trt==i & data$day==j & data$time==k],na.rm=TRUE)/sqrt(length(unique(data$id[!is.na(data$rr.cfb) & data$trt==i & data$day==j & data$time==k]))))
			}
		}
	}

	MeanCmax <- data.frame(trt=names(info$trt),Cmax=rep(NA,length(names(info$trt))))
	for(i in names(info$trt)){
		if(length(eval(parse(text=paste("data$conc[!is.na(data$conc) & data$day==info$primary$",i," & data$trt==info$trt$",i,"]",sep=""))))>0){
			eval(parse(text=paste("MeanCmax$Cmax[MeanCmax$trt==\"",i,"\"] <- ","exp(mean(log(unlist(by(data$conc[!is.na(data$conc) & data$day==info$primary[[i]] & data$trt==info$trt[[i]]],list(data$id[!is.na(data$conc) & data$day==info$primary[[i]] & data$trt==info$trt[[i]]]),max,na.rm=TRUE))),na.rm=TRUE))",sep="")))	
		}
	}

	MeanCmax$trt <- as.character(MeanCmax$trt) #unlist(info$trt,use.names=FALSE)

	expdata <- data

	expdata$QTCONC <- rep(1,nrow(expdata))
	
	if(is.null(info$delta)){
		expdata$QTCONC[expdata$trt==info$trt$Placebo] <- 0
	}else{
		if(info$delta!="single") expdata$QTCONC[expdata$trt==info$trt$Placebo] <- 0
	}

	if(!is.na(info$trt$Therapeutic)){
		expdata$QTCONC[expdata$trt==info$trt$Moxi] <- 0
	}

	expdata$QTCONC[expdata$day%in%info$days[which(info$visit=="Baseline")]] <- 0

	if(is.null(info$delta)){
		expdata$QTCONC[is.na(expdata$ddqtc)] <- 0	
	}else{
		if(info$delta=="single"){
			expdata$QTCONC[is.na(expdata$qtc.cfb)] <- 0
		}else{
			expdata$QTCONC[is.na(expdata$ddqtc)] <- 0	
		}
	}

	expdata$CTthera <- rep(0,nrow(expdata))
	expdata$CTsupra <- rep(0,nrow(expdata))
	expdata$CTmoxi <- rep(0,nrow(expdata))
	
	expdata$CTthera[!is.na(expdata$qtc.cfb) & expdata$trt%in%c(as.character(info$trt$Placebo),as.character(info$trt$Therapeutic)) & expdata$time%in%unique(expdata$time[expdata$trt==as.character(info$trt$Therapeutic)])] <- 1
	expdata$CTsupra[!is.na(expdata$qtc.cfb) & expdata$trt%in%c(as.character(info$trt$Placebo),as.character(info$trt$Supra)) & expdata$time%in%unique(expdata$time[expdata$trt==as.character(info$trt$Supra)])] <- 1
	expdata$CTmoxi[!is.na(expdata$qtc.cfb) & expdata$trt%in%c(as.character(info$trt$Placebo),as.character(info$trt$Moxi)) & expdata$time%in%unique(expdata$time[expdata$trt==as.character(info$trt$Moxi)])] <- 1

	if(length(info$days[which(info$visit=="Baseline")])>0){
		expdata$CTthera[expdata$day%in%info$days[which(info$visit=="Baseline")]] <- 0
		expdata$CTsupra[expdata$day%in%info$days[which(info$visit=="Baseline")]] <- 0
		expdata$CTmoxi[expdata$day%in%info$days[which(info$visit=="Baseline")]] <- 0
	} 

	#temp <- names(info$trt)[which(!c(names(info$trt)%in%c("Placebo","Therapeutic","Supra","Moxi")))]
	#if(length(temp)>0){
	#	temp <- temp[!is.na(unique(unlist(info$trt[names(info$trt)%in%temp])))]
	#	colNoStart <- ncol(expdata)+1
	#	colNoEnd <- ncol(expdata)+length(temp)
	#}else{
		colNoStart <- ncol(expdata)+1
		colNoEnd <- ncol(expdata)+1		
	#}

	for(i in names(info$trt)[which(!is.na(info$trt))]){
		if(!i%in%c("Placebo","Therapeutic","Supra","Moxi")){
			eval(parse(text=paste("expdata$CT",substring(i,1,min(5,nchar(i)))," <- rep(0,nrow(expdata))",sep="")))
			eval(parse(text=paste("expdata$CT",substring(i,1,min(5,nchar(i))),"[!is.na(expdata$qtc.cfb) & expdata$trt%in%c(as.character(info$trt$Placebo),as.character(info$trt$",i,")) & expdata$time%in%unique(expdata$time[expdata$trt==as.character(info$trt$",i,")])] <- 1",sep="")))	
			eval(parse(text=paste("expdata$CT",substring(i,1,min(5,nchar(i))),"[expdata$day%in%info$days[which(info$visit==\"Baseline\")]] <- 0",sep="")))
			colNoEnd <- colNoEnd+1
		}
	}
	
	if(info$conc=="meta"){
		expdata$parent <- expdata$conc
		expdata$conc <- expdata$meta
	}

	Cmax <- expdata[1:nrow(MeanCmax[!is.na(MeanCmax$Cmax),]),]
	Cmax[1:nrow(MeanCmax[!is.na(MeanCmax$Cmax),]),] <- NA
	Cmax$id <- rep(-999,nrow(MeanCmax[!is.na(MeanCmax$Cmax),]))
	Cmax$trt <- MeanCmax$trt[!is.na(MeanCmax$Cmax)]
	Cmax$conc <- MeanCmax$Cmax[!is.na(MeanCmax$Cmax)]
	Cmax$QTCONC <- rep(1,nrow(MeanCmax[!is.na(MeanCmax$Cmax),]))
	Cmax$CTthera <- rep(0,nrow(MeanCmax[!is.na(MeanCmax$Cmax),]))
	Cmax$CTsupra <- rep(0,nrow(MeanCmax[!is.na(MeanCmax$Cmax),]))
	Cmax$CTmoxi <- rep(0,nrow(MeanCmax[!is.na(MeanCmax$Cmax),]))
	for(i in names(info$trt)[which(!is.na(info$trt)) | which(info$trt != "NA")]){
		if(!i%in%c("Placebo","Therapeutic","Supra","Moxi")){
			eval(parse(text=paste("Cmax$CT",substring(i,1,min(5,nchar(i)))," <- rep(0,nrow(MeanCmax[!is.na(MeanCmax$Cmax),]))",sep="")))	
		}
	}

	Crange <- expdata[1:100,]
	Crange[1:100,] <- NA
	Crange$id <- rep(-9999,100)
	Crange$trt <- rep(-9999,100)
	Crange$conc <- seq(min(data$conc[data$conc>0],na.rm=TRUE),max(data$conc,na.rm=TRUE),length=100)	

	Crange$QTCONC <- rep(1,100)
	Crange$CTthera <- rep(0,100)
	Crange$CTsupra <- rep(0,100)
	Crange$CTmoxi <- rep(0,100)

	for(i in names(info$trt)[which(!is.na(info$trt)) | which(info$trt != "NA")]){
		if(!i%in%c("Placebo","Therapeutic","Supra","Moxi")){
			eval(parse(text=paste("Crange$CT",substring(i,1,min(5,nchar(i)))," <- rep(0,100)",sep="")))	
		}
	}


	expdata <- rbind(Crange,Cmax,expdata)

	if(max(MeanCmax$Cmax,na.rm=TRUE)>400){
		if(info$scale=="log"){
			expdata$Scale <- rep(0,nrow(expdata))
		}else{
			expdata$conc <- expdata$conc/1000
			expdata$Scale <- rep(1,nrow(expdata))
		}
	}else{
		expdata$Scale <- rep(0,nrow(expdata))	
	}

	expdata <- expdata[order(expdata$id,expdata$trt,expdata$day,expdata$time),]

	#sas <- expdata[,-which(names(expdata)=="plac.qtc.cfb")]

	if(colNoStart!=colNoEnd){
		sas <- expdata[,c(which(names(expdata)%in%c("id","day","time","trt","conc","qtc","qtc.bs","qtc.cfb","ddqtc","CTthera","CTsupra","CTmoxi","QTCONC","Scale")),colNoStart:(colNoEnd-1))]
	}else{
		sas <- expdata[,c(which(names(expdata)%in%c("id","day","time","trt","conc","qtc","qtc.bs","qtc.cfb","ddqtc","CTthera","CTsupra","CTmoxi","QTCONC","Scale")))]
	}
	## Export data to info$path folder for SAS analysis
	write.xport(sas, file=paste("data.xpt",sep=""))

	## Check for hysteresis and decide whether to use drug conc or metabolite
	meanconc <- meandata[!is.na(meandata$conc) & meandata$conc>0,]

	if(!is.null(meandata$meta)) meanmeta <- meandata[!is.na(meandata$meta) & meandata$meta>0,]
	
	if(!is.null(meandata$moxi)) meanmoxi <- meandata[!is.na(meandata$moxi) & meandata$moxi>0,]

	meanplot <- rbind(meandata,meanconc)
	meanplot$type <- ordered(c(rep(info$correction,nrow(meandata)),rep(as.character(info$pk$drugname),nrow(meanconc))),levels=c(as.character(info$pk$drugname),info$correction))
	meanplot$plot <- meanplot$qtc
	meanplot$plot[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
	meanplot$plotD <- meanplot$qtc.cfb
	meanplot$plotD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
	meanplot$plotDD <- meanplot$ddqtc
	meanplot$plotDD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)] 
	meanplot$plotHys <- meanplot$conc
	
	if(!is.null(meandata$meta) & info$conc!="meta" & info$conc!="moxi"){
		meanplot <- rbind(meandata,meanconc,meanmeta)
		meanplot$type <- ordered(c(rep(info$correction,nrow(meandata)),rep(as.character(info$pk$drugname),nrow(meanconc)),rep(as.character(info$pk$metaname),nrow(meanmeta))),levels=c(as.character(info$pk$metaname),as.character(info$pk$drugname),info$correction))

		meanplot$plot <- meanplot$qtc
		meanplot$plot[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
		meanplot$plot[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]

		meanplot$plotD <- meanplot$qtc.cfb
		meanplot$plotD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
		meanplot$plotD[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]

		meanplot$plotDD <- meanplot$ddqtc
		meanplot$plotDD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)] 
		meanplot$plotDD[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]

		meanplot$plotHys <- meanplot$conc
		meanplot$plotHys[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]
	}

	if(!is.null(meandata$moxi) & info$conc!="moxi"){
		meanplot <- rbind(meandata,meanconc,meanmoxi)
		meanplot$type <- ordered(c(rep(info$correction,nrow(meandata)),rep(as.character(info$pk$drugname),nrow(meanconc)),rep("Moxifloxacin",nrow(meanmoxi))),levels=c("Moxifloxacin",as.character(info$pk$drugname),info$correction))

		meanplot$plot <- meanplot$qtc
		meanplot$plot[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
		meanplot$plot[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]

		meanplot$plotD <- meanplot$qtc.cfb
		meanplot$plotD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
		meanplot$plotD[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]

		meanplot$plotDD <- meanplot$ddqtc
		meanplot$plotDD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)] 
		meanplot$plotDD[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]

		meanplot$plotHys <- meanplot$conc
		meanplot$plotHys[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]
	}

	if(!is.null(meandata$moxi) & info$conc!="moxi" & info$conc!="meta" & !is.null(meandata$meta)){
		meanplot <- rbind(meandata,meanconc,meanmeta,meanmoxi)
		meanplot$type <- ordered(c(rep(info$correction,nrow(meandata)),rep(as.character(info$pk$drugname),nrow(meanconc)),rep(as.character(info$pk$metaname),nrow(meanmeta)),rep("Moxifloxacin",nrow(meanmoxi))),levels=c("Moxifloxacin",as.character(info$pk$metaname),as.character(info$pk$drugname),info$correction))

		meanplot$plot <- meanplot$qtc
		meanplot$plot[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
		meanplot$plot[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]
		meanplot$plot[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]

		meanplot$plotD <- meanplot$qtc.cfb
		meanplot$plotD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)]
		meanplot$plotD[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]
		meanplot$plotD[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]

		meanplot$plotDD <- meanplot$ddqtc
		meanplot$plotDD[meanplot$type==as.character(info$pk$drugname)] <- meanplot$conc[meanplot$type==as.character(info$pk$drugname)] 
		meanplot$plotDD[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]
		meanplot$plotDD[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]

		meanplot$plotHys <- meanplot$conc
		meanplot$plotHys[meanplot$type=="Moxifloxacin"] <- meanplot$moxi[meanplot$type=="Moxifloxacin"]
		meanplot$plotHys[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta[meanplot$type==as.character(info$pk$metaname)]
	}

	meanplot$factor <- ordered(paste(meanplot$trtm,meanplot$day,sep="/"),levels=sort(unique(paste(meanplot$trtm,meanplot$day,sep="/"))))

	meanplot$daytxt <- paste("Day ",as.character(meanplot$day))#,ifelse(meanplot$visit!="",paste(" (",meanplot$visit,")",sep=""),""),sep="")
	meanplot$daytxt <- ordered(meanplot$daytxt,unique(meanplot$daytxt[order(meanplot$day)]))

	maxtime <- ceiling(max(meanplot$time,na.rm=TRUE))
	
	repfactor <- data.frame(trt=names(info$trt),
									days=rep(NA,length(names(info$trt))),
									label=rep(NA,length(names(info$trt))),
									conc=rep(NA,length(names(info$trt))),
									dd=rep(NA,length(names(info$trt))),
									color=rep(NA,length(names(info$trt))),
									symbol=rep(NA,length(names(info$trt))),
									scolor=rep(NA,length(names(info$trt)))
								)
								
	for(i in names(info$trt)){
		eval(parse(text=paste("repfactor$days[repfactor$trt==\"",i,"\"] <- ","ifelse(!is.na(info$trt$",i,"),length(unique(meanplot$day[meanplot$trtm==\"",i,"\"])),0)",sep="")))	
		eval(parse(text=paste("repfactor$label[repfactor$trt==\"",i,"\"] <- ","ifelse(!is.na(info$trt$",i,"),1,0)",sep="")))	
		eval(parse(text=paste("repfactor$conc[repfactor$trt==\"",i,"\"] <- ","ifelse(!is.na(info$trt$",i,") & length(meanplot$conc[!is.na(meanplot$conc) & meanplot$trtm==\"",i,"\"])>0,1,0)",sep="")))
		eval(parse(text=paste("repfactor$dd[repfactor$trt==\"",i,"\"] <- ","ifelse(!is.na(info$trt$",i,") & i!=\"Placebo\",1,0)",sep="")))	
	}
	
	repfactor$trt <- as.character(repfactor$trt)
	repfactor$color <- info$col[1:length(info$trt)] #c("black","blue","red","green","orange",3,7,9,2)[1:length(names(info$trt))]
	repfactor$symbol <- info$pch[1:length(info$trt)] #c(2,15,1,16,4,3,5,6,7)[1:length(names(info$trt))]
	repfactor$scolor <- c(0,1,16,1,16,1,16,1,16,1)[1:length(info$trt)]

	repfactor$conc[repfactor$trt=="Placebo"] <- 0
	repfactor$conc[repfactor$trt=="Moxi"] <- 0

	if(info$conc=="moxi" & !is.na(info$trt$Moxi)){
		repfactor$conc[repfactor$trt=="Moxi"] <- 1
	}

	repfactor$order <- 1:nrow(repfactor)
	repfactor <- repfactor[order(repfactor$trt),]
	#assign("repfactor",repfactor,frame=1)
	assign("repfactor",repfactor,envir=.GlobalEnv)

	################## PK Plots #####################################################

	if(!is.na(info$trt$Therapeutic)){
		plotPK <- xYplot(plot~time|daytxt,
			groups=factor,
			data=meanplot,subset=c(type==as.character(info$pk$drugname) & !is.na(plot)),
			layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==as.character(info$pk$drugname)]))),
			aspect="fill",
			xlim=c(min(meanplot$time[meanplot$type==as.character(info$pk$drugname)])-1,max(meanplot$time[meanplot$type==as.character(info$pk$drugname)])+1),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean ",as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=3,text.width.multiplier=1, 
			text = list(unlist(unique(info$label[!is.na(info$trt) & names(info$trt)!="Placebo" & names(info$trt)!="Moxi"])),cex=c(1)),
			#text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]] & info$trt!=info$trt[[which(names(info$trt)=="Moxi")]]])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))==1) outputGraph("MeanPK",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="MeanPK.wmf")
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))==2) outputGraph("MeanPK",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="MeanPK.wmf")
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))>=3) outputGraph("MeanPK",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="MeanPK.wmf")	
			par(oma=c(0,0,5,0))
			print(plotPK)
			dev.off()
		}else{
			print(plotPK)
		}

		plotPKlog <- xYplot(plot~time|daytxt,
			groups=factor,
			data=meanplot,subset=c(type==as.character(info$pk$drugname) & !is.na(plot)),
			layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==as.character(info$pk$drugname)]))),
			aspect="fill",
			xlim=c(min(meanplot$time[meanplot$type==as.character(info$pk$drugname)])-1,max(meanplot$time[meanplot$type==as.character(info$pk$drugname)])+1),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean ",as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=3,between=1,between.columns=2,text.width.multiplier=1, 
			text  = list(unlist(unique(info$label[!is.na(info$trt) & names(info$trt)!="Placebo" & names(info$trt)!="Moxi"])),cex=c(1)),
			#text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]] & info$trt!=info$trt[[which(names(info$trt)=="Moxi")]]])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)),y=list(log=10))
		)

		if(!is.na(info$output)){
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))==1) outputGraph("MeanlogPK",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="MeanlogPK.wmf")
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))==2) outputGraph("MeanlogPK",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="MeanlogPK.wmf")
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))>=3) outputGraph("MeanlogPK",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="MeanlogPK.wmf")	
			par(oma=c(0,0,5,0))
			print(plotPKlog)
			dev.off()
		}else{
			print(plotPKlog)
		}

		plotPKCI <- xYplot(Cbind(plot,1.64*conc.se)~time|daytxt,
			groups=factor,
			data=meanplot,subset=c(type==as.character(info$pk$drugname) & !is.na(plot)),
			layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==as.character(info$pk$drugname)]))),
			aspect="fill",
			xlim=c(min(meanplot$time[meanplot$type==as.character(info$pk$drugname)])-1,max(meanplot$time[meanplot$type==as.character(info$pk$drugname)])+1),
			ylim=c(0.95*min(meanplot$plot[!is.na(meanplot$plot) & meanplot$type==as.character(info$pk$drugname)]-1.64*meanplot$conc.se[!is.na(meanplot$plot) & meanplot$type==as.character(info$pk$drugname)]),1.03*max(meanplot$plot[!is.na(meanplot$plot) & meanplot$type==as.character(info$pk$drugname)]+1.64*meanplot$conc.se[!is.na(meanplot$plot) & meanplot$type==as.character(info$pk$drugname)])),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean (90% CI) ",as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
					text  = list(unlist(unique(info$label[!is.na(info$trt) & names(info$trt)!="Placebo" & names(info$trt)!="Moxi"])),cex=c(1)),
					 #text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]] & info$trt!=info$trt[[which(names(info$trt)=="Moxi")]]])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))==1) outputGraph("MeanPKCI",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="MeanPK90CI.wmf")
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))==2) outputGraph("MeanPKCI",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="MeanPK90CI.wmf")
			if(length(unique(meanplot$day[meanplot$type==as.character(info$pk$drugname) & !is.na(meanplot$plot)]))>=3) outputGraph("MeanPKCI",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="MeanPK90CI.wmf")	
			par(oma=c(0,0,5,0))
			print(plotPKCI)
			dev.off()
		}else{
			par(oma=c(0,0,5,0))
			print(plotPKCI)
		}

	}

	################## Moxi Plots #####################################################
	if(!is.null(meandata$moxi) & !is.na(info$trt$Moxi)){	
		plotMoxi <- xYplot(plot~time|daytxt,
			groups=factor,
			data=meanplot,subset=c(type=="Moxifloxacin" & !is.na(plot)),
			layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type=="Moxifloxacin"]))),
			aspect="fill",
			xlim=c(min(meanplot$time[meanplot$type=="Moxifloxacin"])-1,max(meanplot$time[meanplot$type=="Moxifloxacin"])+1),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean Moxifloxacin concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1, 
				text = list(unlist(unique(info$label[names(info$trt)=="Moxi"])),cex=c(1)),
				#text = list(unlist(unique(info$label[[which(names(info$trt)=="Moxi")]])),cex=c(1)),
			 	lines=list(type=c("l"),lty=c(1),pch=c(1),col=repfactor$color[which(repfactor$trt=="Moxi")],cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=repfactor$color[which(repfactor$trt=="Moxi")],...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(y=list(cex=1.2),x=list(cex=1.5,at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			if(length(unique(meanplot$day[meanplot$type=="Moxifloxacin" & !is.na(meanplot$plot)]))==1) outputGraph("MeanMoxiPK",info$device,height=9,width=12)#win.metafile(height=9,width=12,file="MeanMoxiPK.wmf")
			if(length(unique(meanplot$day[meanplot$type=="Moxifloxacin" & !is.na(meanplot$plot)]))==2) outputGraph("MeanMoxiPK",info$device,height=12,width=12)#win.metafile(height=12,width=12,file="MeanMoxiPK.wmf")
			if(length(unique(meanplot$day[meanplot$type=="Moxifloxacin" & !is.na(meanplot$plot)]))>=3) outputGraph("MeanMoxiPK",info$device,height=12,width=9)#$win.metafile(height=12,width=9,file="MeanMoxiPK.wmf")	
			par(oma=c(0,0,5,0))
			print(plotMoxi)
			dev.off()
		}else{
			print(plotMoxi)
		}

		plotMoxiCI <- xYplot(Cbind(plot,1.64*moxi.se)~time|daytxt,
			groups=factor,
			data=meanplot,subset=c(type=="Moxifloxacin" & !is.na(plot)),
			layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type=="Moxifloxacin"]))),
			aspect="fill",
			xlim=c(min(meanplot$time[meanplot$type=="Moxifloxacin"])-1,max(meanplot$time[meanplot$type=="Moxifloxacin"])+1),
			ylim=c(0.95*min(meanplot$plot[!is.na(meanplot$plot) & meanplot$type=="Moxifloxacin"]-1.64*meanplot$moxi.se[!is.na(meanplot$plot) & meanplot$type=="Moxifloxacin"]),1.03*max(meanplot$plot[!is.na(meanplot$plot) & meanplot$type=="Moxifloxacin"]+1.64*meanplot$moxi.se[!is.na(meanplot$plot) & meanplot$type=="Moxifloxacin"])),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean (90% CI) Moxifloxacin concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
					text = list(unlist(unique(info$label[names(info$trt)=="Moxi"])),cex=c(1)),
					 #text = list(unlist(unique(info$label[[which(names(info$trt)=="Moxi")]])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=repfactor$color[which(repfactor$trt=="Moxi")],cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=repfactor$color[which(repfactor$trt=="Moxi")],...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(y=list(cex=1.2),x=list(cex=1.5,at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){	
			if(length(unique(meanplot$day[meanplot$type=="Moxifloxacin" & !is.na(meanplot$plot)]))==1) outputGraph("MeanMoxiPKCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file="MeanMoxiPK90CI.wmf")
			if(length(unique(meanplot$day[meanplot$type=="Moxifloxacin" & !is.na(meanplot$plot)]))==2) outputGraph("MeanMoxiPKCI",info$device,height=12,width=12)#win.metafile(height=12,width=12,file="MeanMoxiPK90CI.wmf")
			if(length(unique(meanplot$day[meanplot$type=="Moxifloxacin" & !is.na(meanplot$plot)]))>=3) outputGraph("MeanMoxiPKCI",info$device,height=12,width=9)#win.metafile(height=12,width=9,file="MeanMoxiPK90CI.wmf")	
			par(oma=c(0,0,5,0))
			print(plotMoxiCI)
			dev.off()
		}else{
			print(plotMoxiCI)
		}
	}
	
	################## QT Plots #####################################################
	plot1 <- xYplot(plot~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(plot)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$time[meanplot$type==info$correction])-1,max(meanplot$time[meanplot$type==info$correction])+1),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean ",QTcorrection," (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)
	

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("Meanqtc",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("Meanqtc.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("Meanqtc",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("Meanqtc.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("Meanqtc",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("Meanqtc.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plot1)
		dev.off()
	}else{
		print(plot1)
	}

	plotQTcCI <- xYplot(Cbind(plot,1.64*qtc.se)~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(plot)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$time[meanplot$type==info$correction])-1,max(meanplot$time[meanplot$type==info$correction])+1),
		ylim=c(0.99*min(meanplot$plot[!is.na(meanplot$plot) & meanplot$type==info$correction]-1.64*meanplot$qtc.se[!is.na(meanplot$plot) & meanplot$type==info$correction]),1.01*max(meanplot$plot[!is.na(meanplot$plot) & meanplot$type==info$correction]+1.64*meanplot$qtc.se[!is.na(meanplot$plot) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean (90% CI) ",QTcorrection," (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("MeanqtcCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("Meanqtc90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("MeanqtcCI",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("Meanqtc90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("MeanqtciCI",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("Meanqtc90CI.wmf",sep=""))		
		par(oma=c(0,0,5,0))
		print(plotQTcCI)
		dev.off()
	}else{
		print(plotQTcCI)
	}
	
	plotdQTc <- xYplot(plotD~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(plotD) & visit!="Baseline"),
		layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$plotD) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$plotD) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$plotD) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$plotD) & meanplot$type==info$correction])+1),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean ",QTcorrection," Change from Baseline (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
		        text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
               #text = list(c(info$label$Placebo,info$label$Therapeutic,info$label$Supra,info$label$Moxi),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==1) outputGraph("Meandqtc",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("Meandqtc.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==2) outputGraph("Meandqtc",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("Meandqtc.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))>=3) outputGraph("Meandqtc",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("Meandqtc.wmf",sep=""))		
		par(oma=c(0,0,5,0))
		print(plotdQTc)
		dev.off()
	}else{
		print(plotdQTc)
	}

	plotdQTcCI <- xYplot(Cbind(plotD,1.64*dqtc.se)~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(plotD) & visit!="Baseline"),
		layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$plotD) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$plotD) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$plotD) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$plotD) & meanplot$type==info$correction])+1),
		ylim=c(0.95*min(meanplot$plotD[!is.na(meanplot$plotD) & meanplot$type==info$correction]-1.64*meanplot$dqtc.se[!is.na(meanplot$plotD) & meanplot$type==info$correction]),1.03*max(meanplot$plotD[!is.na(meanplot$plotD) & meanplot$type==info$correction]+1.64*meanplot$dqtc.se[!is.na(meanplot$plotD) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean (90% CI) ",QTcorrection," Change from Baseline (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==1) outputGraph("MeandqtcCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("Meandqtc90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==2) outputGraph("MeandqtcCI",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("Meandqtc90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))>=3) outputGraph("MeandqtcCI",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("Meandqtc90CI.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plotdQTcCI)
		dev.off()
	}else{
		print(plotdQTcCI)
	}

	if(is.null(info$delta) | info$delta!="single"){
		plot3 <- xYplot(plotDD~time|daytxt,
			groups=factor,
			data=meanplot,subset=c(!is.na(plotDD) & type==info$correction & trtm!="Placebo" & visit!="Baseline"),
			layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$plotDD) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$plotDD) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$type==info$correction])+1),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean ",QTcorrection," Change from Baseline and Placebo Adjusted (ms)",sep=""),cex=1.3),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
      	         text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
				panel.abline(h=c(10),lty=4,lwd=1,col=1)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & meanplot$trtm!="Placebo" & !is.na(meanplot$plotDD)]))==1) outputGraph("Meanddqtc",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("Meanddqtc.wmf",sep=""))
			if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & meanplot$trtm!="Placebo" & !is.na(meanplot$plotDD)]))==2) outputGraph("Meanddqtc",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("Meanddqtc.wmf",sep=""))
			if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & meanplot$trtm!="Placebo" & !is.na(meanplot$plotDD)]))>=3) outputGraph("Meanddqtc",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("Meanddqtc.wmf",sep=""))	
			par(oma=c(0,0,5,0))
			print(plot3)
			dev.off()
		}else{
			print(plot3)
		}

		plot3 <- xYplot(Cbind(plotDD,1.64*ddqtc.se)~time|daytxt,
			groups=factor,
			data=meanplot,subset=c(!is.na(plotDD) & type==info$correction & trtm!="Placebo" & visit!="Baseline"),
			layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$plotDD) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$plotDD) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$type==info$correction])+1),
			ylim=c(0.95*min(meanplot$plotDD[!is.na(meanplot$plotDD) & meanplot$type==info$correction]-1.64*meanplot$ddqtc.se[!is.na(meanplot$plotDD) & meanplot$type==info$correction]),1.03*max(meanplot$plotDD[!is.na(meanplot$plotDD) & meanplot$type==info$correction]+1.64*meanplot$ddqtc.se[!is.na(meanplot$plotDD) & meanplot$type==info$correction])),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean (90% CI) ",QTcorrection," Change from Baseline and Placebo Adjusted (ms)",sep=""),cex=1.1),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
     		          text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),               
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
				panel.abline(h=c(10),lty=4,lwd=1,col=1)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & meanplot$trtm!="Placebo" & !is.na(meanplot$plotDD)]))==1) outputGraph("MeanddqtcCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("Meanddqtc90CI.wmf",sep=""))
			if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & meanplot$trtm!="Placebo" & !is.na(meanplot$plotDD)]))==2) outputGraph("MeanddqtcCI",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("Meanddqtc90CI.wmf",sep=""))
			if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & meanplot$trtm!="Placebo" & !is.na(meanplot$plotDD)]))>=3) outputGraph("MeanddqtcCI",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("Meanddqtc90CI.wmf",sep=""))		
			par(oma=c(0,0,5,0))
			print(plot3)
			dev.off()
		}else{
			print(plot3)
		}
	}

	################## HR plots ######################
	plotHR <- xYplot(hr~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(plot)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$time[meanplot$type==info$correction])-1,max(meanplot$time[meanplot$type==info$correction])+1),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean Heart Rate (bpm)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("MeanHR",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanHR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("MeanHR",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanHR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("MeanHR",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanHR.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plotHR)
		dev.off()
	}else{
		print(plotHR)
	}

	plotHRCI <- xYplot(Cbind(hr,1.64*hr.se)~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(plot)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$time[meanplot$type==info$correction])-1,max(meanplot$time[meanplot$type==info$correction])+1),
		ylim=c(0.99*min(meanplot$hr[!is.na(meanplot$hr) & meanplot$type==info$correction]-1.64*meanplot$hr.se[!is.na(meanplot$hr) & meanplot$type==info$correction]),1.01*max(meanplot$hr[!is.na(meanplot$hr) & meanplot$type==info$correction]+1.64*meanplot$hr.se[!is.na(meanplot$hr) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean (90% CI) Heart Rate (bpm)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("MeanHRCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanHR90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("MeanHRCI",info$device,height=9,width=12)#win.metafile(height=12,width=12,file=paste("MeanHR90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("MeanHRCI",info$device,height=9,width=12)#win.metafile(height=12,width=9,file=paste("MeanHR90CI.wmf",sep=""))		
		par(oma=c(0,0,5,0))
		print(plotHRCI)
		dev.off()
	}else{
		print(plotHRCI)
	}

	plotHRcfb <- xYplot(hr.cfb~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(hr.cfb) & visit!="Baseline"),
		layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction])+1),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean Change from Baseline Heart Rate (bpm)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
		        text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==1) outputGraph("MeanHRcfb",info$device,height=9,width=12) #win.metafile(height=9,width=12,file=paste("MeanHRcfb.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==2) outputGraph("MeanHRcfb",info$device,height=12,width=12) #win.metafile(height=12,width=12,file=paste("MeanHRcfb.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))>=3) outputGraph("MeanHRcfb",info$device,height=12,width=9) #win.metafile(height=12,width=9,file=paste("MeanHRcfb.wmf",sep=""))		
		par(oma=c(0,0,5,0))
		print(plotHRcfb)
		dev.off()
	}else{
		print(plotHRcfb)
	}

	plotHRcfbCI <- xYplot(Cbind(hr.cfb,1.64*dhr.se)~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(hr.cfb) & visit!="Baseline"),
		layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction])+1),
		ylim=c(0.95*min(meanplot$hr.cfb[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction]-1.64*meanplot$dhr.se[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction]),1.03*max(meanplot$hr.cfb[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction]+1.64*meanplot$dhr.se[!is.na(meanplot$hr.cfb) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list("Mean (90% CI) Change from Baseline HR (bpm)",cex=1.4),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$hr.cfb)]))==1) outputGraph("MeanHRcfbCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanHRcfb90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$hr.cfb)]))==2) outputGraph("MeanHRcfbCI",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanHRcfb90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$hr.cfb)]))>=3) outputGraph("MeanHRcfbCI",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanHRcfb90CI.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plotHRcfbCI)
		dev.off()
	}else{
		print(plotHRcfbCI)
	}

	################## RR plots ######################
	plotRR <- xYplot(rr~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(plot)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$time[meanplot$type==info$correction])-1,max(meanplot$time[meanplot$type==info$correction])+1),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean RR Interval (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("MeanRR",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanRR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("MeanRR",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanRR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("MeanRR",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanRR.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plotRR)
		dev.off()
	}else{
		print(plotRR)
	}

	plotRRCI <- xYplot(Cbind(rr,1.64*rr.se)~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(rr)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$rr) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$time[meanplot$type==info$correction])-1,max(meanplot$time[meanplot$type==info$correction])+1),
		ylim=c(0.99*min(meanplot$rr[!is.na(meanplot$rr) & meanplot$type==info$correction]-1.64*meanplot$rr.se[!is.na(meanplot$rr) & meanplot$type==info$correction]),1.01*max(meanplot$rr[!is.na(meanplot$rr) & meanplot$type==info$correction]+1.64*meanplot$rr.se[!is.na(meanplot$rr) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean (90% CI) RR Interval (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("MeanRRCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanRR90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("MeanRRCI",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanRR90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("MeanRRCI",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanRR90CI.wmf",sep=""))		
		par(oma=c(0,0,5,0))
		print(plotRRCI)
		dev.off()
	}else{
		print(plotRRCI)
	}

	plotRRcfb <- xYplot(rr.cfb~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(rr.cfb) & visit!="Baseline"),
		layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction])+1),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean Change from Baseline RR Interval (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
		        text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==1) outputGraph("MeanRRcfb",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanRRcfb.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==2) outputGraph("MeanRRcfb",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanRRcfb.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))>=3) outputGraph("MeanRRcfb",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanRRcfb.wmf",sep=""))		
		par(oma=c(0,0,5,0))
		print(plotRRcfb)
		dev.off()
	}else{
		print(plotRRcfb)
	}

	plotRRcfbCI <- xYplot(Cbind(rr.cfb,1.64*drr.se)~time|daytxt,
		groups=factor,
		data=meanplot,subset=c(type==info$correction & !is.na(rr.cfb) & visit!="Baseline"),
		layout=c(1,ifelse(length(unique(meanplot$day[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction]))>1,length(unique(meanplot$day[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction]))-length(which(info$visit=="Baseline")),1)),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction])-1,max(meanplot$time[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction])+1),
		ylim=c(1.1*min(meanplot$rr.cfb[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction]-1.64*meanplot$drr.se[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction]),0.9*max(meanplot$rr.cfb[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction]+1.64*meanplot$drr.se[!is.na(meanplot$rr.cfb) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list("Mean (90% CI) Change from Baseline RR Interval (ms)",cex=1.4),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==1) outputGraph("MeanRRcfbCI",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanRRcfb90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))==2) outputGraph("MeanRRcfbCI",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanRRcfb90CI.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & meanplot$visit!="Baseline" & !is.na(meanplot$plotD)]))>=3) outputGraph("MeanRRcfbCI",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanRRcfb90CI.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plotRRcfbCI)
		dev.off()
	}else{
		print(plotRRcfbCI)
	}

	############# QT/QTc vs RR plots ####################################
	meanplotsort <- meanplot[order(meanplot$factor,meanplot$rr),]
	#meanplotsort <- sort.col(meanplot,columns.to.sort="@ALL", columns.to.sort.by=c("factor","rr"),ascending=TRUE)
	
	plotQTRR <- xYplot(qt~rr|daytxt,
		groups=factor,
		data=meanplotsort,subset=c(type==info$correction & !is.na(rr) & !is.na(qt)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$rr[meanplot$type==info$correction],na.rm=TRUE)-10,max(meanplot$rr[meanplot$type==info$correction],na.rm=TRUE)+1),
		ylab=list("Mean QT (ms)",cex=1.5),
		xlab=list("Mean RR Interval (ms)",cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,pch=16,type="b",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5)
 	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("MeanQTRR",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanQTRR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("MeanQTRR",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanQTRR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("MeanQTRR",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanQTRR.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plotQTRR)
		dev.off()
	}else{
		print(plotQTRR)
	}

	plotQTcRR <- xYplot(qtc~rr|daytxt,
		groups=factor,
		data=meanplotsort,subset=c(type==info$correction & !is.na(plot)),
		layout=c(1,length(unique(meanplot$day[!is.na(meanplot$plot) & meanplot$type==info$correction]))),
		aspect="fill",
		xlim=c(min(meanplot$rr[meanplot$type==info$correction],na.rm=TRUE)-10,max(meanplot$rr[meanplot$type==info$correction],na.rm=TRUE)+1),
		ylab=list(paste("Mean ",QTcorrection," (ms)",sep=""),cex=1.5),
		xlab=list("Mean RR Interval (ms)",cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,pch=16,type="b",col=rep(repfactor$color,repfactor$days),...)
			#panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5)
	)

	if(!is.na(info$output)){
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==1) outputGraph("MeanqtcRR",info$device,height=9,width=12)#win.metafile(height=9,width=12,file=paste("MeanqtcRR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))==2) outputGraph("MeanqtcRR",info$device,height=12,width=12)#win.metafile(height=12,width=12,file=paste("MeanqtcRR.wmf",sep=""))
		if(length(unique(meanplot$day[meanplot$type==info$correction & !is.na(meanplot$plot)]))>=3) outputGraph("MeanqtcRR",info$device,height=12,width=9)#win.metafile(height=12,width=9,file=paste("MeanqtcRR.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plotQTcRR)
		dev.off()
	}else{
		print(plotQTcRR)
	}


	############# ddQTc plots ####################################
	layout <- c(1,2)
	if(!is.null(meandata$meta) & info$conc!="meta") layout <- c(1,3)	
	if(!is.null(meandata$moxi) & info$conc!="moxi") layout <- c(1,3)	
	if(!is.null(meandata$moxi) & !is.null(meandata$meta) & info$conc!="moxi" & info$conc!="meta") layout <- c(1,4)	

	stripnames <- c(paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""))
	if(!is.null(meandata$meta) & info$conc!="meta") stripnames <- c(paste(as.character(info$pk$metaname),"concentration"),paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""))
	if(!is.null(meandata$moxi) & info$conc!="moxi") stripnames <- c("Moxifloxacin concentration",paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""))
	if(!is.null(meandata$moxi) & !is.null(meandata$meta) & info$conc!="moxi" & info$conc!="meta") stripnames <- c("Moxifloxacin concentration",paste(as.character(info$pk$metaname),"concentration"),paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""))
	assign("stripnames",stripnames,envir=.GlobalEnv)

	if(is.null(info$delta) | info$delta!="single"){
		plot4 <- xYplot(plotDD~time|type,
			groups=factor,
			data=meanplot,subset=!is.na(plotDD) & trtm!="Placebo" & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
			layout=layout,
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])+1),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean Profiles",sep=""),cex=1.5),
			key=list(x=0,y=1.04,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
           	 	   text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),               
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...,factor.levels) strip.default(...,factor.levels=stripnames, strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,y=list(relation="free"),x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			if(layout[2]==4) outputGraph("MeanPKddqtc",info$device,height=16,width=12) #win.metafile(height=16,width=12,file=paste("MeanPKddqtc.wmf",sep=""))
			if(layout[2]<4) outputGraph("MeanPKddqtc",info$device,height=12,width=12) #win.metafile(height=12,width=12,file=paste("MeanPKddqtc.wmf",sep=""))	
			par(oma=c(0,0,5,0))
			print(plot4)
			dev.off()
		}else{
			print(plot4)
		}
	}

	meanplot$plotDD.se <- meanplot$conc.se
	meanplot$plotDD.se[meanplot$type==info$correction] <- meanplot$ddqtc.se[meanplot$type==info$correction]
	if(!is.null(meandata$meta) & info$conc!="meta") meanplot$plotDD.se[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta.se[meanplot$type==as.character(info$pk$metaname)]
	if(!is.na(info$trt$Moxi)) meanplot$plotDD.se[meanplot$type=="Moxifloxacin"] <- meanplot$moxi.se[meanplot$type=="Moxifloxacin"]

	meanplot$plotD.se <- meanplot$conc.se
	meanplot$plotD.se[meanplot$type==info$correction] <- meanplot$dqtc.se[meanplot$type==info$correction]
	if(!is.null(meandata$meta) & info$conc!="meta") meanplot$plotD.se[meanplot$type==as.character(info$pk$metaname)] <- meanplot$meta.se[meanplot$type==as.character(info$pk$metaname)]
	if(!is.na(info$trt$Moxi)) meanplot$plotD.se[meanplot$type=="Moxifloxacin"] <- meanplot$moxi.se[meanplot$type=="Moxifloxacin"]

	meanplot$type2 <- as.character(meanplot$type)
	meanplot$type2[meanplot$type2==info$correction] <- paste(QTcorrection,"Change from Baseline and Placebo Adjusted")
	meanplot$type2[meanplot$type2==as.character(info$pk$drugname)] <- paste(as.character(info$pk$drugname),"concentration")
	
	if(!is.na(info$pk$metaname) & !is.null(meandata$meta)) meanplot$type2[meanplot$type2==as.character(info$pk$metaname)] <- paste(as.character(info$pk$metaname),"concentration")
	if(!is.null(meandata$moxi) & !is.na(info$trt$Moxi)) meanplot$type2[meanplot$type2=="Moxifloxacin"] <- "Moxifloxacin concentration"

	if(!is.null(meandata$moxi) & !is.na(info$trt$Moxi)){
		meanplot$type2 <- ordered(meanplot$type2,levels=c("Moxifloxacin concentration",if(!is.na(info$pk$metaname)){paste(as.character(info$pk$metaname),"concentration")},paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,"Change from Baseline and Placebo Adjusted")))	
	}else{
		meanplot$type2 <- ordered(meanplot$type2,levels=c(if(!is.na(info$pk$metaname)){paste(as.character(info$pk$metaname),"concentration")},paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,"Change from Baseline and Placebo Adjusted")))
	}

	meanplot$type3 <- as.character(meanplot$type)
	meanplot$type3[meanplot$type3==info$correction] <- paste(QTcorrection,"Change from Baseline")
	meanplot$type3[meanplot$type3==as.character(info$pk$drugname)] <- paste(as.character(info$pk$drugname),"concentration")

	if(!is.na(info$pk$metaname)) meanplot$type3[meanplot$type3==as.character(info$pk$metaname)] <- paste(as.character(info$pk$metaname),"concentration")
	if(!is.null(meandata$moxi) & !is.na(info$trt$Moxi)) meanplot$type3[meanplot$type3=="Moxifloxacin"] <- "Moxifloxacin concentration"

	if(!is.null(meandata$moxi) & !is.na(info$trt$Moxi)){
		meanplot$type3 <- ordered(meanplot$type3,levels=c("Moxifloxacin concentration",if(!is.na(info$pk$metaname)){paste(as.character(info$pk$metaname),"concentration")},paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,"Change from Baseline")))
	}else{
		meanplot$type3 <- ordered(meanplot$type3,levels=c(if(!is.na(info$pk$metaname)){paste(as.character(info$pk$metaname),"concentration")},paste(as.character(info$pk$drugname),"concentration"),paste(QTcorrection,"Change from Baseline")))
	}


	if(info$delta!="single" | is.null(info$delta)){
		plot4 <- xYplot(Cbind(plotDD,1.64*plotDD.se)~time,#|type2,
			groups=factor,
			data=meanplot,subset=type==info$correction & !is.na(plotDD) & !is.na(plotDD.se) & trtm!="Placebo" & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
			layout=c(1,1),
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])+1),
			ylim=c(ifelse(min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction])<0,1.1*min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]),0.9*min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction])),1.1*max(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]+1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction])),
			xlab=list("Time (hours)",cex=1.5),
    			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               	text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),               
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
				panel.abline(h=c(10),lty=4,lwd=1,col=1)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(...,strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			outputGraph("MeanPrimaryddqtcCI",info$device,height=9,width=12) 
			#win.metafile(height=9,width=12,file=paste("MeanPrimaryddqtc90CI.wmf",sep=""))	
			par(oma=c(0,0,5,0))
			print(plot4)
			dev.off()
		}else{
			print(plot4)
		}

		plot4 <- xYplot(plotDD~time,#|type2,
			groups=factor,
			data=meanplot,subset=type==info$correction & !is.na(plotDD) & !is.na(plotDD.se) & trtm!="Placebo" & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
			layout=c(1,1),
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])+1),
			ylim=c(ifelse(min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction])<0,1.1*min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]),0.9*min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction])),1.1*max(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction]+1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==info$correction])),
			xlab=list("Time (hours)",cex=1.5),
    			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
	            text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),               
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
				panel.abline(h=c(10),lty=4,lwd=1,col=1)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(...,strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			outputGraph("MeanPrimaryddqtc",info$device,height=9,width=12) 
			#win.metafile(height=9,width=12,file=paste("MeanPrimaryddqtc.wmf",sep=""))	
			par(oma=c(0,0,5,0))
			print(plot4)
			dev.off()
		}else{
			print(plot4)
		}

		layout <- c(1,2)
		
		if(!is.null(meandata$meta) & info$conc!="meta") layout <- c(1,3)	
		if(!is.null(meandata$moxi) & info$conc!="moxi") layout <- c(1,3)	
		if(!is.null(meandata$moxi) & !is.null(meandata$meta) & info$conc!="moxi" & info$conc!="meta") layout <- c(1,4)	
	
		plot4 <- xYplot(plotDD~time|type2,#Cbind(plotDD,1.64*plotDD.se)
			groups=factor,
			data=meanplot,subset=!is.na(plotDD) & !is.na(plotDD.se) & trtm!="Placebo" & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
			layout=layout,
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])+1),
			#ylim=c(0.9*min(meanplot$plotDD[!is.na(meanplot$plotDD) & meanplot$type==as.character(info$pk$drugname)]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & meanplot$type==as.character(info$pk$drugname)]),1.1*max(meanplot$plotDD[!is.na(meanplot$plotDD) & meanplot$type==as.character(info$pk$drugname)]+1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD) & meanplot$type==as.character(info$pk$drugname)])),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste("Mean (90% CI) Profiles",sep=""),cex=1.5),
			key=list(x=0,y=1.04,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               	text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),               
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,y=list(relation="free"),x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			outputGraph("MeanPrimaryddqtcPK",info$device,height=12,width=12) 
			#win.metafile(height=12,width=12,file=paste("MeanPrimaryddqtcPK.wmf",sep=""))	
			par(oma=c(0,0,5,0))
			print(plot4)
			dev.off()
		}else{
			print(plot4)
		}
		
	}

	plot4 <- xYplot(Cbind(plotD,1.64*plotD.se)~time,#|type2,
		groups=factor,
		data=meanplot,subset=type==info$correction & !is.na(plotD) & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
		layout=c(1,1),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$plotD) & as.character(meanplot$trtm)!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotD) & as.character(meanplot$trtm)!="Placebo"])+1),
		ylim=c(ifelse(min(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]-1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction])>0,0.9,1.1)*min(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]-1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]),1.1*max(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]+1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
    		ylab=list(paste(QTcorrection," change from baseline (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=1,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(...,strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		outputGraph("MeanPrimarydqtcCI",info$device,height=9,width=12) 
		#win.metafile(height=9,width=12,file=paste("MeanPrimarydqtc90CI.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plot4)
		dev.off()
	}else{
		print(plot4)
	}


	plot4 <- xYplot(plotD~time,#|type2,
		groups=factor,
		data=meanplot,subset=type==info$correction & !is.na(plotD) & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
		layout=c(1,1),
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$plotD) & as.character(meanplot$trtm)!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotD) & as.character(meanplot$trtm)!="Placebo"])+1),
		ylim=c(ifelse(min(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]-1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction])>0,0.9,1.1)*min(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]-1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]),1.1*max(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction]+1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==info$correction])),
		xlab=list("Time (hours)",cex=1.5),
    		ylab=list(paste(QTcorrection," change from baseline (ms)",sep=""),cex=1.5),
		key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
			panel.abline(h=c(10),lty=4,lwd=1,col=1)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(...,strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		outputGraph("MeanPrimarydqtc",info$device,height=9,width=12) 
		#win.metafile(height=9,width=12,file=paste("MeanPrimarydqtc.wmf",sep=""))	
		par(oma=c(0,0,5,0))
		print(plot4)
		dev.off()
	}else{
		print(plot4)
	}
	
	layout <- c(1,2)
	
	if(!is.null(meandata$meta) & info$conc!="meta") layout <- c(1,3)	
	if(!is.null(meandata$moxi) & info$conc!="moxi") layout <- c(1,3)	
	if(!is.null(meandata$moxi) & !is.null(meandata$meta) & info$conc!="moxi" & info$conc!="meta") layout <- c(1,4)	
	
	plot4 <- xYplot(plotD~time|type3,
		groups=factor,
		data=meanplot,subset=!is.na(plotD) & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
		layout=layout,
		aspect="fill",
		xlim=c(min(meanplot$time[!is.na(meanplot$plotD) & as.character(meanplot$trtm)!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotD) & as.character(meanplot$trtm)!="Placebo"])+1),
		#ylim=c(ifelse(min(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==as.character(info$pk$drugname)]-1.64*meanplot$plotD.se[!is.na(meanplot$plotD)  & !is.na(meanplot$plotD.se) & meanplot$type==as.character(info$pk$drugname)])>0,0.9,1.1)*min(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==as.character(info$pk$drugname)]-1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==as.character(info$pk$drugname)]),1.1*max(meanplot$plotD[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==as.character(info$pk$drugname)]+1.64*meanplot$plotD.se[!is.na(meanplot$plotD) & !is.na(meanplot$plotD.se) & meanplot$type==as.character(info$pk$drugname)])),
		xlab=list("Time (hours)",cex=1.5),
		ylab=list(paste("Mean (90% CI) Profiles",sep=""),cex=1.5),
		key=list(x=0,y=1.04,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
		panel=function(x,y,groups,...){
			panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",col=rep(repfactor$color,repfactor$days),...)
		},
		par.strip.text=list(cex=1.5),
		strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		scales=list(cex=1.5,y=list(relation="free"),x=list(at=seq(0,maxtime,4)))
	)

	if(!is.na(info$output)){
		outputGraph("MeanPrimarydqtcPK",info$device,height=12,width=12) 
		#win.metafile(height=12,width=12,file=paste("MeanPrimarydqtcPK.wmf",sep=""))
		par(oma=c(0,0,5,0))
		print(plot4)
		dev.off()
	}else{
		print(plot4)
	}

	if(info$delta!="single" | is.null(info$delta)){
		plot4 <- xYplot(Cbind(plotDD,1.64*plotDD.se)~time|daytxt,#|type2,
			groups=factor,
			data=meanplot,subset=type==as.character(info$pk$drugname) & !is.na(plotDD) & !is.na(plotDD.se) & trtm!="Placebo" & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
			layout=c(1,1),
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & as.character(meanplot$trtm)!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & as.character(meanplot$trtm)!="Placebo"])+1),
			ylim=c(0.9*min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)]),1.1*max(meanplot$plotDD[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)]+1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)])),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
     		         text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			outputGraph("MeanPrimaryPKCI",info$device,height=9,width=12) 
			#win.metafile(height=9,width=12,file="MeanPrimaryPK90CI.wmf")	
			par(oma=c(0,0,5,0))
			print(plot4)
			dev.off()
		}else{
			print(plot4)
		}

		plot4 <- xYplot(plotDD~time|daytxt,#|type2,
			groups=factor,
			data=meanplot,subset=type==as.character(info$pk$drugname) & !is.na(plotDD) & !is.na(plotDD.se) & trtm!="Placebo" & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
			layout=c(1,1),
			aspect="fill",
			xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])+1),
			#ylim=c(0.9*min(meanplot$plotDD[!is.na(meanplot$plotDD) & !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)]),1.1*max(meanplot$plotDD[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)]+1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$drugname)])),
			xlab=list("Time (hours)",cex=1.5),
			ylab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
			key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
	               text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel=function(x,y,groups,...){
				panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
		)

		if(!is.na(info$output)){
			outputGraph("MeanPrimaryPK",info$device,height=9,width=12) 
			#win.metafile(height=9,width=12,file="MeanPrimaryPK.wmf")	
			par(oma=c(0,0,5,0))
			print(plot4)
			dev.off()
		}else{
			print(plot4)
		}


		if(!is.na(info$pk$metaname) & !is.null(meanplot$meta)){
			plot4 <- xYplot(Cbind(plotDD,1.64*plotDD.se)~time,#|type2,
				groups=factor,
				data=meanplot,subset=type==as.character(info$pk$metaname) & !is.na(plotDD) & !is.na(plotDD.se) & trtm!="Placebo" & visit=="Primary Endpoint",#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
				layout=c(1,1),
				aspect="fill",
				xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])+1),
				ylim=c(0.9*min(meanplot$plotDD[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$metaname)]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$metaname)]),1.1*max(meanplot$plotDD[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$metaname)]+1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type==as.character(info$pk$metaname)])),
				xlab=list("Time (hours)",cex=1.5),
				ylab=list(paste(as.character(info$pk$metaname)," concentration (",as.character(info$pk$metaunit),")",sep=""),cex=1.5),
				key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
      		      text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
				panel=function(x,y,groups,...){
					panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
				},
				par.strip.text=list(cex=1.5),
				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
				scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
			)

			if(!is.na(info$output)){
				outputGraph("MeanPrimaryMetaCI",info$device,height=9,width=12) 
				#win.metafile(height=9,width=12,file="MeanPrimaryMeta90CI.wmf")
				par(oma=c(0,0,5,0))
				print(plot4)
				dev.off()
			}else{
				print(plot4)
			}
		}

		if(!is.null(meandata$moxi) & !is.na(info$trt$Moxi)){

			plot4 <- xYplot(Cbind(plotDD,1.64*plotDD.se)~time,#|type2,
				groups=factor,
				data=meanplot,subset=type=="Moxifloxacin" & !is.na(plotDD) & !is.na(plotDD.se) & trtm!="Placebo" & day==info$primary$Moxi,#day%in%unique(unlist(info$primary[!is.na(info$primary)])),# & visit=="Primary Endpoint",
				layout=c(1,1),
				aspect="fill",
				xlim=c(min(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])-1,max(meanplot$time[!is.na(meanplot$plotDD) & meanplot$trtm!="Placebo"])+1),
				ylim=c(0.9*min(meanplot$plotDD[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type=="Moxifloxacin"]-1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type=="Moxifloxacin"]),1.1*max(meanplot$plotDD[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type=="Moxifloxacin"]+1.64*meanplot$plotDD.se[!is.na(meanplot$plotDD)& !is.na(meanplot$plotDD.se) & meanplot$type=="Moxifloxacin"])),
				xlab=list("Time (hours)",cex=1.5),
				ylab=list("Moxifloxacin concentration",cex=1.5),
				key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
     		            text = list(unlist(unique(info$label[!is.na(info$trt)])),cex=c(1)),
					 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
				panel=function(x,y,groups,...){
					panel.xYplot(x,y,groups,label.curves=FALSE,cex=1.5,lwd=4,lty=1,type="l",method="bars",col=rep(repfactor$color,repfactor$days),...)
				},
				par.strip.text=list(cex=1.5),
				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
				scales=list(cex=1.5,x=list(at=seq(0,maxtime,4)))
			)

			if(!is.na(info$output)){
				outputGraph("MeanPrimaryMoxiCI",info$device,height=9,width=12) 
				#win.metafile(height=9,width=12,file="MeanPrimaryMoxi90CI.wmf")
				par(oma=c(0,0,5,0))
				print(plot4)
				dev.off()
			}else{
				print(plot4)
			}
	
		}

		layout <- c(1,1)
		relation <- "same"
		
		if(!is.null(meandata$meta) & info$conc!="meta"){
			layout <- c(2,1)
			relation <- "free"
		} 

		if(!is.null(meandata$moxi) & info$conc!="moxi"){
			layout <- c(2,1)
			relation <- "free"
		}	 

		if(!is.null(meandata$moxi) & !is.null(meandata$meta) & info$conc!="moxi" & info$conc!="meta"){
			layout <- c(3,1)
			relation <- "free"
		} 

		###PLOT Mean Conc vs. Mean QTcF for delay effects with line showing the time direction
		plot5 <- xYplot(ddqtc~plotHys|type,
			groups=trtm,
			data=meanplot,subset=!is.na(ddqtc) & !is.na(plotHys) & type!=info$correction & trtm!="Placebo",# & visit=="Primary Endpoint",
			layout=layout,
			xlim=c(floor(min(meanplot$plotHys[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc)])),ceiling(max(meanplot$plotHys[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc)]))),
			ylim=c(floor(min(meanplot$ddqtc[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)]-1.645*meanplot$ddqtc.se[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)])),ceiling(max(meanplot$ddqtc[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)]+1.645*meanplot$ddqtc.se[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)]))),		
			aspect="fill",
			xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
      	 	ylab=list(paste(QTcorrection," change from placebo and baseline adjusted (ms)   ",sep=""),cex=1.4),
			key=list(x=0,y=1.04,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               	text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel.data = meanplot,
			panel=function(x,y,subscripts,groups,...,panel.data){
				panel.xYplot(x,y,subscripts,groups,cex=1.5,lwd=4,lty=c(1),type="b",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),pch=" ",...) #repfactor$label
				panel.text(x,y, labels = panel.data$time[subscripts])
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(relation=relation),y=list(alternating=FALSE))
			)

		if(!is.na(info$output)){
			if(layout[1]==1) outputGraph("ddqtcPKdelay",info$device,height=8,width=8)  #win.metafile(height=8,width=8,file="ddqtcPKdelay.wmf")
			if(layout[1]==2) outputGraph("ddqtcPKdelay",info$device,height=8,width=12) #win.metafile(height=8,width=12,file="ddqtcPKdelay.wmf")
			if(layout[1]==3) outputGraph("ddqtcPKdelay",info$device,height=8,width=16)  #win.metafile(height=8,width=12,file="ddqtcPKdelay.wmf")	
			par(oma=c(0,0,8,0))
			print(plot5)
			dev.off()
		}else{
			print(plot5)
		}

		plothysCI <- xYplot(Cbind(ddqtc,1.645*ddqtc.se)~plotHys|type,
			groups=trtm,
			data=meanplot,subset=!is.na(ddqtc) & !is.na(plotHys) & type!=info$correction & trtm!="Placebo",# & visit=="Primary Endpoint",
			layout=layout,
			xlim=c(floor(min(meanplot$plotHys[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc)])),ceiling(max(meanplot$plotHys[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc)]))),
			ylim=c(floor(min(meanplot$ddqtc[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)]-1.645*meanplot$ddqtc.se[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)])),ceiling(max(meanplot$ddqtc[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)]+1.645*meanplot$ddqtc.se[meanplot$type!=info$correction & meanplot$trtm!="Placebo" & !is.na(meanplot$plotHys) & !is.na(meanplot$ddqtc) & !is.na(meanplot$ddqtc.se)]))),		
			aspect="fill",
			xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
      	 	ylab=list(paste(QTcorrection," change from placebo and baseline adjusted (ms)     ",sep=""),cex=1.4),
			key=list(x=0,y=1.04,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=1,between.columns=2,text.width.multiplier=1,
               	text = list(unlist(unique(info$label[!is.na(info$trt) & info$trt!=info$trt[[which(names(info$trt)=="Placebo")]]])),cex=c(1)),
				 lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
			panel.data = meanplot,
			panel=function(x,y,subscripts,groups,...,panel.data){
				panel.xYplot(x,y,subscripts,groups,cex=1.5,lwd=4,lty=c(1),type="b",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),pch=" ",...) #repfactor$label
				panel.text(x,y, labels = panel.data$time[subscripts])
			},
			par.strip.text=list(cex=1.5),
			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
			scales=list(cex=1.5,x=list(relation=relation),y=list(alternating=FALSE))
			)

		if(!is.na(info$output)){
			if(layout[1]==1) outputGraph("ddqtcPKdelayCI",info$device,height=8,width=8) #win.metafile(height=8,width=8,file="ddqtcPKdelay90CI.wmf")
			if(layout[1]==2) outputGraph("ddqtcPKdelayCI",info$device,height=8,width=12) #win.metafile(height=8,width=12,file="ddqtcPKdelay90CI.wmf")
			if(layout[1]==3) outputGraph("ddqtcPKdelayCI",info$device,height=8,width=16) #win.metafile(height=8,width=16,file="ddqtcPKdelay90CI.wmf")
			par(oma=c(0,0,8,0))
			print(plothysCI)
			dev.off()
		}else{
			print(plothysCI)
		}
		
	}

	## Export data for SAS analysis
	if(!is.na(info$output)){
		write.table(meanplot, file="meandata.csv",quote=FALSE,row.names=FALSE,sep=",")
		setwd(opath)
	}

	if(info$report==TRUE){
		wdGoToBookmark("blankMean")
		wdNormal("")

		wdBody(paste("The mean (90% CI) PK, QT, ",QTcorrection,", HR, and RR-time profiles are shown below together with the corresponding change from baseline profiles.",sep=""))
		wdBody("Figure 3: Mean (90% CI) time profiles for PK, QT/QTc, HR, and RR.")
		if(!is.na(info$trt$Therapeutic)){
			wdPlot(plotPKCI,width=9,height=6)
		}
		if(!is.null(meandata$moxi) & !is.na(info$trt$Moxi)){
			wdPlot(plotMoxiCI,width=9,height=6)
		}

		wdPlot(plotQTcCI,width=9,height=6)
		wdPlot(plotHRCI,width=9,height=6)
		wdPlot(plotRRCI,width=9,height=6)

		wdPlot(plotdQTcCI,width=9,height=6)
		wdPlot(plotHRcfbCI,width=9,height=6)
		wdPlot(plotRRcfbCI,width=9,height=6)

		wdBody(paste("Figure 4: Mean dd",QTcorrection," vs. mean ",as.character(info$pk$drugname)," concentrations connected in chronological order. The numbers in the hysteresis plot represent the nominal sampling times.",sep=""))
		wdPlot(plothysCI,width=9,height=6)
		wdSave("QT")
	}

	options(warn = 1)
}

########################################################################################
## Perform Central Tendency analysis in SAS
########################################################################################
QTtime <- function(info){
	options(warn = -1)
	names(data) <- casefold(names(data), upper=FALSE)
	
	if(sum(c("design","trt","primary","days","visit")%in%names(info)==FALSE)>0){

		cat(paste("\nYou forgot the following required items in info: \"",c("design","trt","primary","days","visit")[which(c("design","trt","primary","days","visit")%in%names(info)==FALSE)],
				"\" to your info list\n",sep=""))

		cat("The QT script will terminate now")
		stop()
	}

	if(sum(c("design","trt","label","primary","output","digits","device",
		          	"days","visit","pk","correction","saspath","report",       
				"conc","scale","intercept","quantiles","delta",
				"qtci","gof","corr",        
				"col","bin",    
				"cex","lty","lwd","pch","alpha")%in%names(info)==FALSE)>0){
		
		cat("QTtime function: The info list has been changed and does not include all options\n")
		cat("InfoCreate function is being called from within the QTtime function\n")
		info <- InfoCreate(info)
	}

	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	if(!is.na(info$output)){
		opath <- getwd()
		dir.create(info$output,showWarnings=FALSE)
		setwd(info$output)
	}

	if(!is.na(info$trt$Placebo)){

		CTnames <- names(info$trt)[!is.na(info$trt)]	

		sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTtime.sas",sep=""),sep="\n",what="character")
		j <- length(sasscript)
	
		for(i in CTnames[-which(CTnames=="Placebo")]){
			textname <- as.character(eval(parse(text=paste("info$trt$",i,sep=""))))
			shortname <- substring(i,1,min(5,nchar(i)))	

			sasscript[j+1] <- "data oldt; set old1;"
			sasscript[j+2] <- paste("where CT",shortname," = 1;",sep="")
			sasscript[j+3] <- "run;"

			sasscript[j+4] <- "data oldt; set oldt;" 
			sasscript[j+5] <- paste("if trt=\"",textname,"\" then trtc=\"DRUG\";",sep="")
			sasscript[j+6] <- paste("if trt=\"",as.character(info$trt$Placebo),"\" then trtc=\"PBO\";",sep="")
			sasscript[j+7] <- "run;"

			sasscript[j+8] <- "data oldt; set oldt;"
			sasscript[j+9] <- "where trtc=\"DRUG\" | trtc=\"PBO\";"
			sasscript[j+10] <- "run;"

			sasscript[j+11] <- "proc sort data=oldt; by day time;run;"
			sasscript[j+12] <- "proc mixed data=oldt covtest;"
			sasscript[j+13] <- "class id trtc;"
			sasscript[j+14] <- "model dqtcobs= trtc/Solution ddfm=KENWARDROGER;"
			sasscript[j+15] <- "repeated trtc/subject=id type=un;"
			sasscript[j+16] <- "lsmeans trtc/cl pdiff=control(\"PBO\") alpha=0.1;"
			sasscript[j+17] <- "by day time;"
			sasscript[j+18] <- paste("ods output LSMeans=CTsum",shortname," Diffs=CTa",shortname," CovParms=cov",shortname,";",sep="")
			sasscript[j+19] <- "run;"
			sasscript[j+20] <- paste("data CTsum",shortname,"; set CTa",shortname," CTsum",shortname,";run;",sep="")	

			sasscript[j+21] <- paste("proc sort data=CTsum",shortname,"; by day time;run;",sep="")
			sasscript[j+22] <- paste("proc sort data=CTa",shortname,"; by descending Upper ; run;",sep="")
			sasscript[j+23] <- paste("data CTa",shortname,"; set CTa",shortname,"; if _n_=1; when=1; keep day time when;run;",sep="")
			sasscript[j+24] <- paste("data CT",shortname,"; merge CTa",shortname," CTsum",shortname,"; by day time; if when=1;method=\"",shortname,"\"; run;",sep="")
			sasscript[j+25] <- paste("data CTsum",shortname,"; set CTsum",shortname,"; method=\"",shortname,"\"; run;",sep="")
			sasscript[j+26]  <- ""
			j <- j+26 
		}
	
		sasscript[j+1] <- paste("data CT; set ",paste("CT",substring(CTnames[-which(CTnames=="Placebo")],1,5),sep="",collapse=" "),"; run;",sep="")
		sasscript[j+2] <- paste("data CTsum; set ",paste("CTsum",substring(CTnames[-which(CTnames=="Placebo")],1,5),sep="",collapse=" "),"; run;",sep="")

		sasscript[j+3] <- "ods pdf close;"
		sasscript[j+4] <- "ods trace off;"
		sasscript[j+5] <- "quit;"

		sasscript[j+6] <- "PROC EXPORT DATA=CTsum"
		sasscript[j+7] <- "OUTFILE= \"CT.csv\"" 
		sasscript[j+8] <- "DBMS=CSV REPLACE;"
		sasscript[j+9] <- "RUN;"

		write(sasscript,file="QTtime.sas")	
	
		if(info$saspath==""){
			system("sas QTtime.sas")
		}else{
			system(paste(info$saspath,"/sas QTtime.sas",sep=""))
		}

		if(is.null(info$label)) info$label <- info$trt

		for(i in 1:2){
			CT <- read.table("CT.csv",header=TRUE,stringsAsFactors=FALSE,sep=",")	
			data <- read.xport("data.xpt")
			
			if(i==1){		
				for(j in CTnames[-which(CTnames=="Placebo")]){
					textname <- as.character(eval(parse(text=paste("info$trt$",j,sep=""))))
					shortname <- substring(j,1,min(5,nchar(j)))

					CTdata <- CT[CT$method==shortname & CT$DAY==info$primary[[j]],]

					if(nrow(CTdata)>0){		
	
						tabledata <- data.frame(cbind(
									CTdata$TIME[!is.na(CTdata$TIME) & CTdata$trtc=="DRUG" & CTdata$X_trtc!="PBO"],
									CTdata$DF[!is.na(CTdata$TIME) & CTdata$trtc=="DRUG" & CTdata$X_trtc!="PBO"],round(CTdata$Estimate[!is.na(CTdata$TIME) & CTdata$trtc=="DRUG" & CTdata$X_trtc!="PBO"],2),round(CTdata$StdErr[!is.na(CTdata$TIME) & CTdata$trtc=="DRUG" & CTdata$X_trtc!="PBO"],2),
									CTdata$DF[!is.na(CTdata$TIME) & CTdata$trtc=="PBO" & CTdata$X_trtc!="PBO"],round(CTdata$Estimate[!is.na(CTdata$TIME) & CTdata$trtc=="PBO" & CTdata$X_trtc!="PBO"],2),round(CTdata$StdErr[!is.na(CTdata$TIME) & CTdata$trtc=="PBO" & CTdata$X_trtc!="PBO"],2),
									CTdata$DF[!is.na(CTdata$TIME) & CTdata$trtc=="DRUG" & CTdata$X_trtc=="PBO"],round(CTdata$Estimate[!is.na(CTdata$TIME) & CTdata$trtc=="PBO"],2),round(CTdata$StdErr[CTdata$trtc=="PBO"],2),
									paste(round(CTdata$Lower[!is.na(CTdata$TIME) & CTdata$X_trtc=="PBO"],2),round(CTdata$Upper[!is.na(CTdata$TIME) & CTdata$X_trtc=="PBO"],2),sep=";")
									))
						names(tabledata) <- c("Time","DF_D","Mean_D","SE_D","DF_P","Mean_P","SE_P","DF_DD","Mean_DD","SE_DD","90CI_DD")
	
					}else{
						tabledata <- data.frame(matrix(rep(0,11),ncol=11))
						names(tabledata) <- c("Time","DF_D","Mean_D","SE_D","DF_P","Mean_P","SE_P","DF_DD","Mean_DD","SE_DD","90CI_DD")				
					}
				
					eval(parse(text=paste("table",shortname," <- CTdata",sep="")))
					eval(parse(text=paste("write.table(table",shortname,", file=\"table",shortname,"CT.csv\",sep=\",\",row.names=FALSE,quote=FALSE)",sep="")))	
		
				}

			}
			if(i==1){ 
				CT <- CT[CT$X_trtc=="PBO",]
			}else{
				CT <- CT[CT$X_trtc!="PBO",]		
			}

			CT$trtm <- rep(NA,nrow(CT))
			for(j in unique(CT$method)){
				CT$trtm[CT$method==j] <- as.character(info$label[[which(substring(names(info$trt),1,nchar(j))==j)]])
			}

			CT$trtm <- ordered(CT$trtm,levels=unique(CT$trtm))
				
			if(i==2){
				CT$trtm2 <- paste(as.character(CT$trtm),as.character(CT$trtc),sep="/")
				CT$trtm2 <- ordered(CT$trtm2,levels=c(paste(unique(as.character(CT$trtm)),"DRUG",sep="/"),paste(unique(as.character(CT$trtm)),"PBO",sep="/")))
			}	

			CT$visit <- rep(NA,nrow(CT))
			for(j in 1:length(sort(unique(CT$DAY)))){
				CT$visit[CT$DAY==sort(unique(CT$DAY))[j]] <- info$visit[which(info$days==sort(unique(CT$DAY))[j])]	
			}

			CT$daytxt <- paste("Day ",as.character(CT$DAY))#,ifelse(CT$visit!="",paste(" (",CT$visit,")",sep=""),""),sep="")
			CT$daytxt <- ordered(CT$daytxt,unique(CT$daytxt[order(CT$DAY)]))

			if(info$correction%in%c("qtcf","qtcb","qtci")){
				if(info$correction=="qtcb") QTcorrection <- "QTcB"
				if(info$correction=="qtcf") QTcorrection <- "QTcF"
				if(info$correction=="qtci") QTcorrection <- "QTcI"	
			}else{
				QTcorrection <- paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")
			}

			maxtime <- ceiling(max(CT$TIME,na.rm=TRUE))

			if(i==1){
				plotCTCI <- xYplot(Cbind(Estimate,Lower,Upper)~TIME|daytxt,
							data=CT,
                       				layout=c(1,length(unique(CT$DAY))),
							groups=trtm,
                       				aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline and Placebo Adjusted (ms)  ",sep=""),cex=1.4),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(levels(CT$trtm),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       			strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
		                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
            		           panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),lwd=5,cex=1.3,...)
								panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))
					
		
						if(!is.na(info$output)){
							if(length(unique(CT$DAY))==1) outputGraph("QTtimedayCI",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CTday90CI.wmf")
							if(length(unique(CT$DAY))==2) outputGraph("QTtimedayCI",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CTday90CI.wmf")
							if(length(unique(CT$DAY))>=3) outputGraph("QTtimedayCI",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CTday90CI.wmf")	
							par(oma=c(0,0,5,0))
							print(plotCTCI)
							dev.off()
						}else{
							print(plotCTCI)
						}

				plotCT <- xYplot(Estimate~TIME|daytxt,
							data=CT,
                       				layout=c(1,length(unique(CT$DAY))),
							groups=trtm,
                       				aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline and Placebo Adjusted (ms)  ",sep=""),cex=1.4),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(levels(CT$trtm),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
                       panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),lwd=5,cex=1.3,...)
								panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))
		
						if(!is.na(info$output)){
							if(length(unique(CT$DAY))==1) outputGraph("QTtimeday",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CTday.wmf")
							if(length(unique(CT$DAY))==2) outputGraph("QTtimeday",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CTday.wmf")
							if(length(unique(CT$DAY))>=3) outputGraph("QTtimeday",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CTday.wmf")	
							par(oma=c(0,0,5,0))
							print(plotCT)
							dev.off()
						}else{
							print(plotCT)
						}


				plotCTprimaryCI <- xYplot(Cbind(Estimate,Lower,Upper)~TIME|daytxt,subset=DAY%in%unique(unlist(info$primary)),#c(info$primary$Therapeutic,info$primary$Supra,info$primary$Moxi),		
							data=CT,
			                       layout=c(1,length(unique(unlist(info$primary[!is.na(unique(info$primary))])))),
							groups=trtm,
                  	     			aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline and Placebo Adjusted (ms)  ",sep=""),cex=1.4),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(levels(CT$trtm),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
                       panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),lwd=5,cex=1.3,...)
								panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))
						
						if(!is.na(info$output)){
							if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==1) outputGraph("QTtimeCI",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CT90CI.wmf")
							if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==2) outputGraph("QTtimeCI",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CT90CI.wmf")
							if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))>=3) outputGraph("QTtimeCI",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CT90CI.wmf")	
							par(oma=c(0,0,5,0))
							print(plotCTprimaryCI)
							dev.off()	
						}else{
							print(plotCTprimaryCI)
						}
						
				plotCTprimary <- xYplot(Estimate~TIME|daytxt,subset=DAY%in%unique(unlist(info$primary)),#$Therapeutic,info$primary$Supra,info$primary$Moxi),		
							data=CT,
                       				layout=c(1,length(unique(unlist(info$primary[!is.na(unique(info$primary))])))),
							groups=trtm,
                       				aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline and Placebo Adjusted (ms)  ",sep=""),cex=1.4),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(levels(CT$trtm),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
                       panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),lwd=5,cex=1.3,...)
								panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))
						
						if(!is.na(info$output)){
							if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==1) outputGraph("QTtime",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CT.wmf")
							if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==2) outputGraph("QTtime",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CT.wmf")
							if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))>=3) outputGraph("QTtime",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CT.wmf")	
							par(oma=c(0,0,5,0))
							print(plotCTprimary)
							dev.off()							
						}else{
							print(plotCTprimary)
						}
			}else{
				plotCTsingleCI <- xYplot(Cbind(Estimate,Lower,Upper)~TIME|daytxt,
							data=CT,
                       				layout=c(1,length(unique(CT$DAY))),
							groups=trtm2,
                      				aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline (ms)",sep=""),cex=1.5),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(c(as.character(info$label$Placebo),levels(CT$trtm)),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
                       panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=c(rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),rep(repfactor$color[repfactor$trt=="Placebo"],length(rep(repfactor$color,repfactor$dd)))),lwd=5,cex=1.3,...)
								#panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=c(rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),rep(1,length(rep(repfactor$color,repfactor$dd)))),lwd=5,cex=1.3,...)
								#panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))

				if(!is.na(info$output)){
					if(length(unique(CT$DAY))==1) outputGraph("QTtimesingledeltadayCI",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CTsingledeltaday90CI.wmf")
					if(length(unique(CT$DAY))==2) outputGraph("QTtimesingledeltadayCI",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CTsingledeltaday90CI.wmf")
					if(length(unique(CT$DAY))>=3) outputGraph("QTtimesingledeltadayCI",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CTsingledeltaday90CI.wmf")	
					par(oma=c(0,0,5,0))
					print(plotCTsingleCI)
					dev.off()
				}else{
					print(plotCTsingleCI)
				}
			
				plotCTsingle <- xYplot(Estimate~TIME|daytxt,
							data=CT,
                    	   			layout=c(1,length(unique(CT$DAY))),
							groups=trtm2,
                       				aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline (ms)",sep=""),cex=1.5),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(c(as.character(info$label$Placebo),levels(CT$trtm)),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
                       panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=c(rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),rep(repfactor$color[repfactor$trt=="Placebo"],length(rep(repfactor$color,repfactor$dd)))),lwd=5,cex=1.3,...)
								#panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))

				if(!is.na(info$output)){
					if(length(unique(CT$DAY))==1) outputGraph("QTtimesingledeltaday",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CTsingledeltaday.wmf")
					if(length(unique(CT$DAY))==2) outputGraph("QTtimesingledeltaday",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CTsingledeltaday.wmf")
					if(length(unique(CT$DAY))>=3) outputGraph("QTtimesingledeltaday",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CTsingledeltaday.wmf")	
					par(oma=c(0,0,5,0))
					print(plotCTsingle)
					dev.off()
				}else{
					print(plotCTsingle)
				}

				plotCTsingleprimaryCI <- xYplot(Cbind(Estimate,Lower,Upper)~TIME|daytxt,subset=DAY%in%unique(unlist(info$primary)),#c(info$primary$Therapeutic,info$primary$Supra,info$primary$Moxi),		
							data=CT,
                       				layout=c(1,length(unique(unlist(info$primary[!is.na(unique(info$primary))])))),
							groups=trtm2,
                       				aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline (ms)",sep=""),cex=1.5),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(c(as.character(info$label$Placebo),levels(CT$trtm)),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
                       panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=c(rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),rep(repfactor$color[repfactor$trt=="Placebo"],length(rep(repfactor$color,repfactor$dd)))),lwd=5,cex=1.3,...)
								#panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))

				if(!is.na(info$output)){
					if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==1) outputGraph("QTtimesingledeltaCI",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CTsingledelta90CI.wmf")
					if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==2) outputGraph("QTtimesingledeltaCI",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CTsingledelta90CI.wmf")
					if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))>=3) outputGraph("QTtimesingledeltaCI",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CTsingledelta90CI.wmf")	
					par(oma=c(0,0,5,0))
					print(plotCTsingleprimaryCI)
					dev.off()
				}else{
					print(plotCTsingleprimaryCI)
				}		
				plotCTsingleprimary <- xYplot(Estimate~TIME|daytxt,subset=DAY%in%unique(unlist(info$primary)),#c(info$primary$Therapeutic,info$primary$Supra,info$primary$Moxi),		
							data=CT,
                       				layout=c(1,length(unique(unlist(info$primary[!is.na(unique(info$primary))])))),
							groups=trtm2,
                       				aspect="fill",#10/2,
							ylim=c(min(CT$Lower,na.rm=TRUE),max(CT$Upper,na.rm=TRUE)),
							xlim=c(min(CT$TIME,na.rm=TRUE)-1,max(CT$TIME,na.rm=TRUE)+1),
						  ylab=list(paste(QTcorrection," Change from Baseline (ms)",sep=""),cex=1.5),
						xlab=list("Time (hours)",cex=1.5),
						key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=3,text.width.multiplier=1,
               			text = list(c(as.character(info$label$Placebo),levels(CT$trtm)),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(1),pch=c(1),col=rep(repfactor$color[order(repfactor$order)],repfactor$label[order(repfactor$order)]),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,maxtime,4))),
                       panel = function(x,y,subscripts,...) {
								panel.xYplot(x,y,subscripts,type="l",label.curves=FALSE,col=c(rep(repfactor$color[order(repfactor$order)],repfactor$dd[order(repfactor$order)]),rep(repfactor$color[repfactor$trt=="Placebo"],length(rep(repfactor$color,repfactor$dd)))),lwd=5,cex=1.3,...)
								#panel.abline(h=c(10),col=1,lty=4,lwd=1)
							},
						 par.strip.text=list(cex=1.5))

				if(!is.na(info$output)){
					if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==1) outputGraph("QTtimesingledelta",info$device,height=9,width=12) #win.metafile(height=9,width=12,file="CTsingledelta.wmf")
					if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))==2) outputGraph("QTtimesingledelta",info$device,height=12,width=12) #win.metafile(height=12,width=12,file="CTsingledelta.wmf")
					if(length(unique(unlist(info$primary[!is.na(unique(info$primary))])))>=3) outputGraph("QTtimesingledelta",info$device,height=12,width=9) #win.metafile(height=12,width=9,file="CTsingledelta.wmf")	
					par(oma=c(0,0,5,0))
					print(plotCTsingleprimary)
					dev.off()	
				}else{
					print(plotCTsingleprimary)
				}

			}
		}
	}else{
		print("No placebo arm available for Central Tendency analysis")
	}
	if(!is.na(info$output)){
		setwd(opath)
	}

	if(info$report==TRUE){
		wdGoToBookmark("blankTime")
		wdNormal("")

		wdBody(paste("Figure 5: Mean (90% CI) d",QTcorrection," (change from baseline) and dd",QTcorrection," (placebo-adjusted change from baseline) vs. time profiles.",sep=""))
		wdPlot(plotCTCI,width=9,height=6)
		wdPlot(plotCTsingleCI,width=9,height=6)
		wdSave("QT")
	}

	options(warn = 1)	
}

########################################################################################
## Perform Concentration-QT analysis in SAS
########################################################################################

QTconc <- function(info){
	options(warn = -1)
	names(data) <- casefold(names(data), upper=FALSE)
	
	if(sum(c("design","trt","primary","days","visit")%in%names(info)==FALSE)>0){

		cat(paste("\nYou forgot the following required items in info: \"",c("design","trt","primary","days","visit")[which(c("design","trt","primary","days","visit")%in%names(info)==FALSE)],
				"\" to your info list\n",sep=""))

		cat("The QT script will terminate now")
		stop()
	}

	if(sum(c("design","trt","label","primary","output","digits","device",
		          	"days","visit","pk","correction","saspath","report",       
				"conc","scale","intercept","quantiles","delta",
				"qtci","gof","corr",        
				"col","bin",    
				"cex","lty","lwd","pch","alpha")%in%names(info)==FALSE)>0){
		
		cat("QTconc function: The info list has been changed and does not include all options\n")
		cat("InfoCreate function is being called from within the QTconc function\n")
		info <- InfoCreate(info)
	}
	if(!is.na(info$output)) while(dev.cur()!=1) dev.off()
	if(is.na(info$output)){
		if(length(grep(":",R.home()))>0) windows(rec=TRUE)
	}

	if(!is.na(info$output)){
		opath <- getwd()
		dir.create(info$output,showWarnings=FALSE)
		setwd(info$output)
	}	

	for(i in names(info$trt)){
			eval(parse(text=paste("info$trt$",i," <- as.character(info$trt$",i,")",sep="")))
	}

	if(is.null(info$label)) info$label <- info$trt
	for(i in names(info$trt)){
			eval(parse(text=paste("info$label$",i," <- as.character(info$label$",i,")",sep="")))
	}

	if(info$conc=="meta"){
		info$pk$drugname <- info$pk$metaname
		info$pk$drugunit <- info$pk$metaunit
	} 

	assign("info",info,envir=.GlobalEnv)
	
	if(info$scale=="normal") sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTconc.sas",sep=""),sep="\n",what="character")	
	if(info$scale=="log") sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTlogconc.sas",sep=""),sep="\n",what="character")	
	if(info$scale=="normal" & info$delta=="single") sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTconcSingle.sas",sep=""),sep="\n",what="character")	
	if(info$scale=="log" & info$delta=="single") sasscript <- scan(file=paste(R.home(),"/library/QT/extdata/","QTlogconcSingle.sas",sep=""),sep="\n",what="character")	
	
	if(info$scale!="normal" & info$scale!="log"){
		print("Scale argument in info should be either \"normal\" or \"log\" (case-sensitive)")
		stop()
	}

	
	if(info$corr=="diag"){
		for(i in which(regexpr("TYPE=UN",sasscript)!=-1)){
			substring(sasscript[i],first=regexpr("TYPE=UN",sasscript[i])[1],last=regexpr("TYPE=UN",sasscript[i])[1]+6) <- "TYPE=VC"
		}
	}
	
	write(sasscript,file="QTconc.sas")	

	if(info$saspath==""){
		system("sas QTconc.sas")
	}else{
		system(paste(info$saspath,"/sas QTconc.sas",sep=""))
	}
	
	data <- read.xport(file="data.xpt")	
	names(data) <- casefold(names(data), upper=FALSE)

	if(info$delta=="single") data$ddqtc <- data$qtc.cfb
	
	if(!is.numeric(data$id)) data$id <- as.character(data$id)
	if(!is.numeric(data$trt)) data$trt <- as.character(data$trt)

	if(info$correction%in%c("qtcf","qtcb","qtci")){
		if(info$correction=="qtcb") QTcorrection <- "QTcB"
		if(info$correction=="qtcf") QTcorrection <- "QTcF"
		if(info$correction=="qtci") QTcorrection <- "QTcI"	
	}else{
		QTcorrection <- paste("QTc",casefold(substring(info$correction,4),upper=TRUE),sep="")
	}

	predblup <- read.table("predblup.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")	
	predblup$ID <- as.character(predblup$ID)

	predblup <- predblup[predblup$intercept==info$intercept,]
	if(!is.numeric(predblup$TRT)) predblup$TRT <- as.character(predblup$TRT)
	if(info$scale=="log") predblup$CONC <- exp(predblup$CONC)
	
	Scale <- FALSE
	if(predblup$SCALE[1]==1) Scale <- TRUE

	if(Scale) predblup$CONC <- predblup$CONC*1000

	predblup$trtm <- rep(NA,nrow(predblup))
	for(i in unique(predblup$TRT)){
		if(i%in%names(info$trt)) predblup$trtm[predblup$TRT==i] <- info$trt[[which(names(info$trt)==i)]]
	}

	if(info$delta=="single") predblup$DDQTCOBS <- predblup$DQTCOBS
	
	predblup$visit <- rep(NA,nrow(predblup))
	for(i in 1:length(sort(unique(predblup$DAY)))){
		predblup$visit[predblup$DAY==sort(unique(predblup$DAY))[i]] <- info$visit[which(info$days==sort(unique(predblup$DAY))[i])]	
	}

	qtpred <- read.table("qtpred.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
	qtpred <- qtpred[qtpred$intercept==info$intercept,]
	if(!is.numeric(qtpred$TRT)) qtpred$TRT <- as.character(qtpred$TRT)
	if(info$scale=="log") qtpred$CONC <- exp(qtpred$CONC)
	if(Scale) qtpred$CONC <- qtpred$CONC*1000
	
	predblup$eblup[predblup$ID==-999] <- qtpred$Pred[qtpred$TRT!="-9999"]
	predblup$Lower[predblup$ID==-999] <- qtpred$Lower[qtpred$TRT!="-9999"]
	predblup$Upper[predblup$ID==-999] <- qtpred$Upper[qtpred$TRT!="-9999"]

	predblup$eblup[predblup$ID==-9999] <- qtpred$Pred[qtpred$TRT=="-9999"]
	predblup$Lower[predblup$ID==-9999] <- qtpred$Lower[qtpred$TRT=="-9999"]
	predblup$Upper[predblup$ID==-9999] <- qtpred$Upper[qtpred$TRT=="-9999"]
	
	param <- read.table("paraest.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")	
	param <- param[param$intercept==info$intercept & param$method=="approx",]

	if(Scale){
		param$Estimate[param$Effect=="CONC"] <- param$Estimate[param$Effect=="CONC"]/1000
		param$Lower[param$Effect=="CONC"] <- param$Lower[param$Effect=="CONC"]/1000
		param$Upper[param$Effect=="CONC"] <- param$Upper[param$Effect=="CONC"]/1000
	}

	random <- read.table("random.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
	random$ID <- as.character(random$ID)

	random <- random[random$intercept==info$intercept,]
	if(Scale)	random$Estimate[random$Effect=="CONC"] <- random$Estimate[random$Effect=="CONC"]/1000

	random$Effect <- as.character(random$Effect)
	random <- random[order(random$Effect),]
	#random <- sort.col(random,columns.to.sort="@ALL", columns.to.sort.by=c("Effect"),ascending=TRUE)
	random$Effect[random$Effect!="Intercept"] <- "Slope" 
	random$Slope <- rep(random$Estimate[random$Effect=="Slope"],2) + param$Estimate[param$Effect=="CONC"]

	popSlope <- param$Estimate[param$Effect=="CONC"]

	popInt <- 0
	if(info$intercept=="Yes") popInt <- param$Estimate[param$Effect=="Intercept"]	

	assign("popSlope",popSlope,envir=.GlobalEnv)
	assign("popInt",popInt,envir=.GlobalEnv)

	if(info$intercept!="No") random$Intercept <- rep(random$Estimate[random$Effect=="Intercept"],2) + popInt

	random$type <- rep(1,nrow(random))
	random$type[random$ID==-999] <- 2
	random$type[random$ID==-9999] <- 3

	random <- random[order(random$Effect,random$type),]
	#random <- sort.col(random,columns.to.sort="@ALL", columns.to.sort.by=c("Effect","type"),ascending=TRUE)
	temp <- predblup[match(unique(predblup$ID),predblup$ID),c("ID","TRT")]

	random <- merge(random,temp,by="ID",all.x=TRUE)

	random$TRT[random$ID=="-999"] <- "-999"
	random$TRT[random$ID=="-9999"] <- "-9999"

   ################################################################################
	paraest <- read.table(file="paraest.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")
	covest <- read.table(file="iiv.csv",header=TRUE,stringsAsFactor=FALSE,sep=",")

	table <- data.frame(matrix(nrow=11,ncol=4))
	names(table) <- c("Parameter","Estimate","pvalue","IIV")
	
	#Model with intercept
	table[1,1] <- paste("Model 1: dd",QTcorrection," = Intercept + slope * ",as.character(info$pk$drugname)," Concentration",sep="")
	table[2,1] <- "Intercept (ms)"
	table[3,1] <- paste("Slope (ms per",ifelse(info$scale=="log"," log "," "),as.character(info$pk$drugunit),")",sep="")
	table[4,1] <- "Residual Variability (ms)"

	table[2,2] <- paste(round(paraest[1,2],2)," (",round(paraest[1,8],2),"; ",round(paraest[1,9],2),")",sep="")
	table[3,2] <- paste(signif(ifelse(Scale,paraest[2,2]/1000,paraest[2,2]),3)," (",ifelse(Scale,signif(paraest[2,8]/1000,3),signif(paraest[2,8],3)),"; ",ifelse(Scale,signif(paraest[2,9]/1000,3),signif(paraest[2,9],3)),")",sep="")
	if(info$corr=="diag"){
		table[4,2] <- round(sqrt(covest[3,3]),2)
	}else{
		table[4,2] <- round(sqrt(covest[4,3]),2)
	}
	
	table[2,3] <- as.character(paraest[1,6])
	table[3,3] <- as.character(paraest[2,6])

	if(info$corr=="diag"){
		table[2,4] <- round(sqrt(covest[1,3]),2)
		table[3,4] <- round(sqrt(covest[2,3]),2)
	}else{
		table[2,4] <- round(sqrt(covest[1,3]),2)
		table[3,4] <- round(sqrt(covest[3,3]),2)	
	}	

	#Model with intercept fixed to 0 and IIV
	table[5,1] <- paste("Model 2: dd",QTcorrection," = Intercept + slope * ",as.character(info$pk$drugname)," Concentration (Fixed Intercept)",sep="")
	table[6,1] <- "Intercept (ms)"
	table[7,1] <- paste("Slope (ms per",ifelse(info$scale=="log"," log "," "),as.character(info$pk$drugunit),")",sep="")
	table[8,1] <- "Residual Variability (ms)"

	table[5,2] <- ""
	table[6,2] <- "0"
	table[7,2] <- paste(signif(ifelse(Scale,paraest[4,2]/1000,paraest[4,2]),3)," (",ifelse(Scale,signif(paraest[4,8]/1000,3),signif(paraest[4,8],3)),"; ",ifelse(Scale,signif(paraest[4,9]/1000,3),signif(paraest[4,9],3)),")",sep="")

	if(info$corr=="diag"){
		table[8,2] <- round(sqrt(covest[8,3]),2)
	}else{
		table[8,2] <- round(sqrt(covest[10,3]),2)		
	}
	
	table[7,3] <- as.character(paraest[4,6])

	if(info$corr=="diag"){
		table[6,4] <- round(sqrt(covest[6,3]),2)
		table[7,4] <- round(sqrt(covest[7,3]),2)
	}else{
		table[6,4] <- round(sqrt(covest[7,3]),2)
		table[7,4] <- round(sqrt(covest[9,3]),2)		
	}
	
	#Model without intercept
	table[9,1] <- paste("Model 3: dd",QTcorrection," = slope * ",as.character(info$pk$drugname)," Concentration (No Intercept)",sep="")
	table[10,1] <- paste("Slope (ms per",ifelse(info$scale=="log"," log "," "),as.character(info$pk$drugunit),")",sep="")
	table[11,1] <- "Residual Variability (ms)"

	table[10,2] <- paste(signif(ifelse(Scale,paraest[3,2]/1000,paraest[3,2]),3)," (",ifelse(Scale,signif(paraest[3,8]/1000,3),signif(paraest[3,8],3)),"; ",ifelse(Scale,signif(paraest[3,9]/1000,3),signif(paraest[3,9],3)),")",sep="")
	if(info$corr=="diag"){
		table[11,2] <- round(sqrt(covest[5,3]),2)
	}else{
		table[11,2] <- round(sqrt(covest[6,3]),2)		
	}
	
	table[10,3] <- as.character(paraest[3,6])

	if(info$corr=="diag"){
		table[10,4] <- round(sqrt(covest[4,3]),2)
	}else{
		table[10,4] <- round(sqrt(covest[5,3]),2)
	}

	table[c(1,5,9),2] <- ""
	table[c(1,4:6,8:9,11),3] <- ""
	table[c(1,4:5,8:9,11),4] <- ""
	
	if(!is.na(info$output)){
		write.table(table, file=paste("tableparms.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
	}else{
		cat("\n Concentration-QT parameter estimates\n")
		print(table)
	}
	#################################################################


	j <- 1 
	if(info$conc=="moxi" & !is.na(info$trt$Moxi)){
		tablepred <- data.frame(matrix(nrow=length(unique(unlist(info$label[which(names(info$trt)%in%c("Moxi"))]))[!is.na(unique(unlist(info$label[which(names(info$trt)%in%c("Moxi"))])))]),ncol=4))
		names(tablepred) <- c("Treatment","Conc","Pred","CI")

		info$trt$Therapeutic <- info$trt$Moxi
		info$label$Therapeutic <- info$label$Moxi

		tablepred[,1] <- unique(unlist(info$label[which(names(info$trt)%in%c("Moxi"))]))[!is.na(unique(unlist(info$label[which(names(info$trt)%in%c("Moxi"))])))]
		for(i in unique(names(info$label[which(names(info$trt)%in%c("Moxi"))]))[!is.na(unique(unlist(info$label[which(names(info$trt)%in%c("Moxi"))])))]){
			tablepred[j,2] <- paste(signif(qtpred$CONC[qtpred$TRT==i & qtpred$intercept==info$intercept],3)," ",as.character(info$pk$drugunit),sep="")
			tablepred[j,3] <- signif(qtpred$Pred[qtpred$TRT==i & qtpred$intercept==info$intercept],3)
			tablepred[j,4] <- paste("(",signif(qtpred$Lower[qtpred$TRT==i & qtpred$intercept==info$intercept],3),"; ",signif(qtpred$Upper[qtpred$TRT==i & qtpred$intercept==info$intercept],3),")",sep="")
			j <- j+1
		}
	}else{
		tablepred <- data.frame(matrix(nrow=length(unique(unlist(info$label[-which(names(info$trt)%in%c("Placebo","Moxi"))]))[!is.na(unique(unlist(info$label[-which(names(info$trt)%in%c("Placebo","Moxi"))])))]),ncol=4))
		names(tablepred) <- c("Treatment","Conc","Pred","CI")

		tablepred[,1] <- unique(unlist(info$label[-which(names(info$trt)%in%c("Placebo","Moxi"))]))[!is.na(unique(unlist(info$label[-which(names(info$trt)%in%c("Placebo","Moxi"))])))]
		for(i in unique(names(info$label[-which(names(info$trt)%in%c("Placebo","Moxi"))]))[!is.na(unique(unlist(info$label[-which(names(info$trt)%in%c("Placebo","Moxi"))])))]){
			tablepred[j,2] <- paste(signif(qtpred$CONC[qtpred$TRT==i & qtpred$intercept==info$intercept],3)," ",as.character(info$pk$drugunit),sep="")
			tablepred[j,3] <- signif(qtpred$Pred[qtpred$TRT==i & qtpred$intercept==info$intercept],3)
			tablepred[j,4] <- paste("(",signif(qtpred$Lower[qtpred$TRT==i & qtpred$intercept==info$intercept],3),"; ",signif(qtpred$Upper[qtpred$TRT==i & qtpred$intercept==info$intercept],3),")",sep="")
			j <- j+1
		}
	} 
	
	if(!is.na(info$output)){
		write.table(tablepred, file=paste("tablepred.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
	}else{
		cat("\n Concentration-QT predictions\n")
		print(tablepred)
	}
   #################################################################

	tableinfo <- data.frame(matrix(nrow=1,ncol=6))
	names(tableinfo) <- c("Drug","Correction","Therapeutic","Supra","Model","ModelTXT")
	
	tableinfo[1,1] <- as.character(info$pk$drugname)
	tableinfo[1,2] <- as.character(QTcorrection)
	tableinfo[1,3] <- as.character(info$label$Therapeutic)
	if(!is.na(info$trt$Supra)){
		tableinfo[1,4] <- as.character(info$label$Supra)	
	}else{
		tableinfo[1,4] <- "NA"
	}
	if(info$intercept=="Yes") tableinfo[1,5] <- "1"
	if(info$intercept=="Fix") tableinfo[1,5] <- "2"
	if(info$intercept=="No") tableinfo[1,5] <- "3"
	if(info$intercept=="Yes") tableinfo[1,6] <- "with intercept"
	if(info$intercept=="Fix") tableinfo[1,6] <- "with intercept fixed to zero"
	if(info$intercept=="No") tableinfo[1,6] <- "without intercept"

	if(!is.na(info$output)){
		write.table(tableinfo, file=paste("info.csv",sep=""),sep=",",row.names=FALSE,quote=FALSE)	
	}
   ################################################################
	plot5 <- xyplot(DDQTCOBS~eblup,data=predblup,subset=ID!=-999 & ID!=-9999,
                       layout=c(1,1),
                       aspect="fill",#10/2,
						  ylab=list(paste("Observed ",ifelse(info$delta=="single","d","dd"),QTcorrection," (ms)",sep=""),cex=1.5),
						xlab=list(paste("Predicted ",ifelse(info$delta=="single","d","dd"),QTcorrection," (ms)",sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                       panel = function(x,y,...) {
								panel.xyplot(x,y,type="p",col=c(1),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)								
								panel.abline(a=0,b=1,col=1,lwd=3)
								panel.loess(x,y,col="red",lty=4,lwd=5,...)
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("obspred",info$device,height=9,width=9)
		#win.metafile(height=9,width=9,file="obspred.wmf")	
		par(oma=c(0,1,0,0))
		print(plot5)
		dev.off()
	}else{
		print(plot5)
	}

	predblup$FLAG <- paste("ID ",predblup$ID," (",predblup$TRT,")", sep="")
	predblup$FLAG <- ordered(predblup$FLAG,levels=unique(predblup$FLAG[order(as.character(predblup$TRT))]))
	predblup$PAT <- paste("ID ",predblup$ID, sep="")
	predblup$PAT <- ordered(predblup$PAT,levels=unique(predblup$PAT[order(predblup$ID)]))

	for(i in 1:((length(unique(predblup$ID))-2)%/%9 + ifelse((length(unique(predblup$ID))-2)%%9==0,0,1))){
		assign("number",i,envir=.GlobalEnv)
		plot6 <- xyplot(DDQTCOBS~eblup|PAT,data=predblup,subset=ID!=-999 & ID!=-9999 & PAT%in%levels(PAT)[(number*9-8):(number*9)],
                      	layout=c(3,3),
                      	aspect="fill",#10/2,
				ylab=list(paste("Observed ",ifelse(info$delta=="single","d","dd"),QTcorrection," (ms)",sep=""),cex=1.5),
				#xlab=list(substitute(paste("Predicted ",ifelse(info$delta=="single",expression(Delta),expression(DeltaDelta)),QTcorrection," (ms)",sep=""),list(QTcorrection=QTcorrection)),cex=1.5),
				xlab=list(paste("Predicted ",ifelse(info$delta=="single","d","dd"),QTcorrection," (ms)",sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(relation="same")),
                       panel = function(x,y,...) {
								panel.xyplot(x,y,type="p",col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)								
								panel.abline(a=0,b=1,col="black",lwd=3)
								#panel.loess(x,y,col=8,lty=4,lwd=5,...)
							},
						 par.strip.text=list(cex=1.5))
						
		if(!is.na(info$output)){
			outputGraph(paste("obspredID",number,sep=""),info$device,height=9,width=9)
			#win.metafile(height=9,width=9,file=paste("obspredID",number,".wmf",sep=""))
			par(oma=c(0,1,0,0))
			print(plot6)
			dev.off()
		}else{
			print(plot6)
		}
	}
	

	plot7 <- xyplot(Resid~eblup,data=predblup,subset=ID!=-999 & ID!=-9999,
                       layout=c(1,1),
                       aspect="fill",#10/2,
						  ylab=list("Residuals (ms)",cex=1.5),
						xlab=list(paste("Predicted ",ifelse(info$delta=="single","d","dd"),QTcorrection," (ms)",sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                       panel = function(x,y,...) {
								panel.xyplot(x,y,type="p",col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)								
								panel.abline(h=0,col="black",lwd=3)
								panel.loess(x,y,col="red",lty=4,lwd=5,...)
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("respred",info$device,height=9,width=9)
		#win.metafile(height=9,width=9,file="respred.wmf")
		par(oma=c(0,1,0,0))
		print(plot7)
		dev.off()
	}else{
		print(plot7)
	}

	plot8 <- xyplot(Resid~TIME|as.factor(ifelse(visit!="",paste("Day ",DAY,sep=""),paste("Day",DAY))),
							data=predblup,subset=ID!=-999 & ID!=-9999,
                       layout=c(1,length(unique(predblup$DAY[!is.na(predblup$DAY)]))),
                       aspect="fill",#10/2,
							xlim=c(min(predblup$TIME,na.rm=TRUE)-1,max(predblup$TIME,na.rm=TRUE)+1),
						  ylab=list("Residuals (ms)",cex=1.5),
						xlab=list("Time (hours)",cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(at=seq(0,24,6))),
                       panel = function(x,y,...) {
								panel.xyplot(x,y,type="p",col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)								
								panel.abline(h=0,col="black",lwd=3)
								panel.loess(x,y,col="red",lty=4,lwd=5,...)
							},
						 par.strip.text=list(cex=1))

	if(!is.na(info$output)){
		outputGraph("restime",info$device,height=9,width=9)
		#win.metafile(height=9,width=9,file="restime.wmf")	
		par(oma=c(0,1,0,0))
		print(plot8)
		dev.off()
	}else{
		print(plot8)
	}

	plot9 <- xyplot(Resid~CONC,data=predblup,subset=ID!=-999 & ID!=-9999,
                       layout=c(1,1),
                       aspect="fill",#10/2,
							xlim=c(0,max(predblup$CONC,na.rm=TRUE)),
						  ylab=list(paste("Residuals (ms)",sep=""),cex=1.5),
						  xlab=list(paste(as.character(info$pk$drugname)," concentration"," (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                       panel = function(x,y,...) {
								panel.xyplot(x,y,type="p",col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)								
								panel.loess(x,y,col="red",lty=4,lwd=5,...)
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("residconc",info$device,height=9,width=9)
		#win.metafile(height=9,width=9,file="residconc.wmf")	
		par(oma=c(0,1,0,0))
		print(plot9)
		dev.off()
	}else{
		print(plot9)
	}

	plot10 <- bwplot(ID~Resid,data=predblup,subset=ID!=-999 & ID!=-9999,
                       layout=c(1,1),
                       aspect="fill",#10/2,
						  xlab=list("Residuals (ms)",cex=1.5),
						ylab=list("Subjects",cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,y=list(cex=.1)),
                       panel = function(x,y,...) {
								panel.bwplot(x,y,col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)								
								panel.abline(v=0,col="black",lwd=3)
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("residID",info$device,height=9,width=9)
		#win.metafile(height=9,width=9,file="residID.wmf")	
		par(oma=c(0,1,0,0))
		print(plot10)
		dev.off()
	}else{
		print(plot10)
	}

	dataplot <- rbind(predblup,predblup)
	dataplot$QTplot <- dataplot$eblup
	dataplot$QTplot[1:nrow(predblup)] <- dataplot$DDQTCOBS[1:nrow(predblup)]
	dataplot$TYPE <- ordered(c(rep("OBS",nrow(predblup)),rep("IPRED",nrow(predblup))),levels=c("OBS","IPRED"))

	dataplot$Min <- dataplot$Lower
	dataplot$Max <- dataplot$Upper

	dataplot$Min[dataplot$ID!="-999" & dataplot$ID!="-9999"] <- NA
	dataplot$Max[dataplot$ID!="-999" & dataplot$ID!="-9999"] <- NA

	dataplot <- dataplot[!c(dataplot$ID=="-999" & dataplot$ID=="-9999" & dataplot$TYPE=="IPRED"),]
	dataplot$TRT[dataplot$ID=="-999"] <- "999"
	dataplot$TRT[dataplot$ID=="-9999"] <- "9999"

	dataplot$TRT <- ordered(as.character(dataplot$TRT),levels=c(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE)[!is.na(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE))],"999","9999"))#  sort(unique(dataplot$TRT))

	dataplot$QTplot[as.character(dataplot$TRT)=="9999"] <- dataplot$eblup[as.character(dataplot$TRT)=="9999"]
	dataplot$FLAG <- paste("ID ",dataplot$ID," (",as.character(dataplot$TRT),")", sep="")
	dataplot$FLAG <- ordered(dataplot$FLAG,levels=unique(dataplot$FLAG[order(as.character(dataplot$TRT))]))
	dataplot$PAT <- paste("ID ",dataplot$ID, sep="")
	dataplot$PAT <- ordered(dataplot$PAT,levels=unique(dataplot$PAT[order(dataplot$ID)]))
	dataplotID <- dataplot[order(dataplot$ID,dataplot$CONC),]
	#dataplotID <- sort.col(dataplot,columns.to.sort="@ALL", columns.to.sort.by=c("ID","CONC"),ascending=FALSE)
	assign("dataplot",dataplot,envir=.GlobalEnv)

	repfactor <- repfactor[order(repfactor$order),]

	keytext <- list(c(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE)[!is.na(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE))],"Mean predicted"),cex=c(1))
	keyline <- list(type=c(rep("p",length(unique(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE)[!is.na(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE))]))),"l"),lty=c(1),pch=c(rep(repfactor$symbol[repfactor$conc!=0],repfactor$conc[repfactor$conc!=0]),1),col=c(rep(repfactor$scolor[repfactor$conc!=0],repfactor$conc[repfactor$conc!=0]),"red"),
						cex=1,lwd=c(rep(1,length(unique(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE)[!is.na(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE))]))),5))

	assign("keytext",keytext,envir=.GlobalEnv)
	assign("keyline",keyline,envir=.GlobalEnv)

	
	for(i in 1:((length(unique(dataplot$ID))-2)%/%9 + ifelse((length(unique(dataplot$ID))-2)%%9==0,0,1))){
		assign("number",i,envir=.GlobalEnv)
		plot11 <- xyplot(QTplot~CONC|PAT,data=dataplotID,subset=ID!=-999 & ID!=-9999 & PAT%in%levels(PAT)[(number*9-8):(number*9)],
                       	layout=c(3,3),
				groups=TYPE,
                       	aspect="fill",#10/2,
				#xlim=c(-10,ceiling(max(dataplotID$CONC,na.rm=TRUE))),
       			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
				xlab=list(paste(as.character(info$pk$drugname)," concentration ","(",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
				  	key=list(x=-0.1,y=1.01,corner=c(0,0),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
               			text = list(c("Individual predictions","Observations"),cex=c(1)),
				 			lines=list(type=c("l","p"),lty=c(1),pch=c(1,16),col=c("red","black"),cex=1.3,lwd=c(4))),                   
						strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                       panel = function(x,y,...) {
								panel.superpose.2(x,y,type=c("p","l"),col=c("black","red"),cex=c(1.5),lty=c(1),lwd=5,pch=c(16),...)
							},
						 par.strip.text=list(cex=1.1))

		if(!is.na(info$output)){
			outputGraph(paste("QTconcID",number,sep=""),info$device,height=10,width=10)
			#win.metafile(height=10,width=10,file=paste("QTconcID",number,".wmf",sep=""))
			par(oma=c(0,1,0,0))
			print(plot11)
			dev.off()
		}else{
			print(plot11)
		}
	}

	#& CONC>=min(dataplot$CONC[!c(dataplot$TRT%in%c(999,9999))],na.rm=TRUE)
	plot12normal <- xYplot(QTplot~CONC,data=dataplot,subset=TYPE=="OBS",
                       		layout=c(1,1),
					groups=TRT,
                        	aspect="fill",#10/2,
					#xlim=c(-10,ceiling(max(dataplot$CONC))),
       				ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
					xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
					key=list(x=0.,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=2,text.width.multiplier=1,
               				text = keytext,
						lines=keyline),               
					strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                     		scales=list(cex=1.5),
                       		panel = function(x,y,groups,subscripts,...) {
    							panel.xYplot(x[subscripts>(100+sum(repfactor$conc))],y[subscripts>(100+sum(repfactor$conc))],groups[subscripts>(100+sum(repfactor$conc))],subscripts[subscripts>(100+sum(repfactor$conc))],type="p",method="bars",col=rep(repfactor$scolor[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),cex=rep(1.3,(sum(repfactor$conc)+1)),lty=c(1),lwd=c(rep(1,sum(repfactor$conc)),rep(5,2)),pch=rep(repfactor$symbol[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),...)
    							panel.xYplot(x[subscripts<(99+sum(repfactor$conc))],y[subscripts<(99+sum(repfactor$conc))],subscripts[subscripts<(99+sum(repfactor$conc))],groups[subscripts<(99+sum(repfactor$conc))],label.curves=FALSE,type="l",col="red",col.fill=c("grey"),cex=c(1,1.3),lty=c(1),lwd=c(5),pch=c(15,1),...)
								panel.abline(h=10,lty=4)
								#panel.abline(b=popSlope,a=popInt,col="red",lwd=5)	
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTconc",info$device,height=10,width=12)
		#win.metafile(height=9,width=12,file="QTconc.wmf")
		print(plot12normal)
		dev.off()
	}else{
		print(plot12normal)
	}

	plot12log <- xYplot(QTplot~CONC,data=dataplot,subset=TYPE=="OBS",
                       	layout=c(1,1),
				groups=TRT,
                        aspect="fill",#10/2,
				#xlim=c(min(dataplot$CONC,na.rm=TRUE),ceiling(max(dataplot$CONC,na.rm=TRUE))),
       			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
				xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
				key=list(x=0.0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=2,text.width.multiplier=1,
               			text = keytext,
					lines=keyline),               
				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       	scales=list(cex=1.5,x=list(log=10,at=c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),
																		seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000)),
																		labels=c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),
																		"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000"))),
                       	panel = function(x,y,groups,subscripts,...) {
    							panel.xYplot(x[subscripts>(100+sum(repfactor$conc))],y[subscripts>(100+sum(repfactor$conc))],groups[subscripts>(100+sum(repfactor$conc))],subscripts[subscripts>(100+sum(repfactor$conc))],type="p",method="bars",col=rep(repfactor$scolor[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),cex=rep(1.3,(sum(repfactor$conc)+1)),lty=c(1),lwd=c(rep(1,sum(repfactor$conc)),rep(5,2)),pch=rep(repfactor$symbol[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),...)
    							panel.xYplot(x[subscripts<(99+sum(repfactor$conc))],y[subscripts<(99+sum(repfactor$conc))],subscripts[subscripts<(99+sum(repfactor$conc))],groups[subscripts<(99+sum(repfactor$conc))],label.curves=FALSE,type="l",col="red",col.fill=c("grey"),cex=c(1,1.3),lty=c(1),lwd=c(5),pch=c(15,1),...)
								panel.abline(h=10,lty=4)
								#panel.abline(b=popSlope,a=popInt,col="red",lwd=5)	
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTlogconc",info$device,height=10,width=12)
		#win.metafile(height=9,width=12,file="QTlogconc.wmf")
		par(oma=c(0,1,0,0))
		print(plot12log)
		dev.off()
	}else{
		print(plot12log)
	}


	if(!is.na(info$output)){
		outputGraph("QTconcReport",info$device,height=10,width=12)
		#win.metafile(height=9,width=12,file="QTconcReport.wmf")
		par(oma=c(0,1,0,0))
		if(info$scale=="normal") print(plot12normal)
		if(info$scale=="log") print(plot12log)
		dev.off()
	}

	keytext <- list(c(unique(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE)[!is.na(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE))])),cex=c(1))
	keyline <- list(type=c(rep("p",length(unique(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE)[!is.na(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE))])))),lty=c(1),pch=c(rep(repfactor$symbol,repfactor$conc)),col=c(rep(repfactor$scolor,repfactor$conc)),
						cex=1,lwd=c(rep(1,length(unique(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE)[!is.na(unlist(info$trt[-which(names(info$trt)%in%c("Placebo","Moxi"))],use.names=FALSE))])))))


	assign("keytext",keytext,envir=.GlobalEnv)
	assign("keyline",keyline,envir=.GlobalEnv)
	
	plot12 <- xYplot(QTplot~CONC,data=dataplot,subset=TYPE=="OBS",
                       	layout=c(1,1),
				groups=TRT,
                        aspect="fill",#10/2,
				#xlim=c(-10,ceiling(max(dataplot$CONC))),
       			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
				xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
				key=list(x=0.0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=4,between.columns=2,text.width.multiplier=1,
               			text = keytext,
					lines=keyline),               
				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       	scales=list(cex=1.5),
                       	panel = function(x,y,groups,subscripts,...) {
    							#panel.xYplot(x[subscripts<101],y[subscripts<101],groups[subscripts<101],subscripts[subscripts<101],label.curves=FALSE,method="filled bands",type="l",col=8,col.fill=c(160),cex=c(1.3),lty=c(1),lwd=c(5),pch=c(15),...)
    							panel.xYplot(x[subscripts>(100+sum(repfactor$conc))],y[subscripts>(100+sum(repfactor$conc))],groups[subscripts>(100+sum(repfactor$conc))],subscripts[subscripts>(100+sum(repfactor$conc))],type="p",method="bars",col=rep(repfactor$scolor[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),cex=rep(1.3,(sum(repfactor$conc)+1)),lty=c(1),lwd=c(rep(1,sum(repfactor$conc)),rep(5,2)),pch=rep(repfactor$symbol[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),...)
								panel.abline(h=10,lty=4)
								#panel.abline(b=popSlope,a=popInt,col=8,lwd=5)	
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTconcRaw",info$device,height=10,width=12)
		#win.metafile(height=9,width=12,file="QTconcRaw.wmf")
		par(oma=c(0,1,0,0))
		print(plot12)
		dev.off()
	}else{
		print(plot12)
	}

	# & CONC>=min(dataplot$CONC[!c(dataplot$TRT%in%c(999,9999))],na.rm=TRUE)
	plot12 <- xYplot(QTplot~CONC,data=dataplot,subset=TYPE=="OBS",
                       	layout=c(1,1),
				groups=TRT,
                        aspect="fill",#10/2,
				#xlim=c(min(dataplot$CONC,na.rm=TRUE),ceiling(max(dataplot$CONC))),
       			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
				xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
				key=list(x=0.0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=4,between=2,between.columns=2,text.width.multiplier=1,
               			text = keytext,
					lines=keyline),               
				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       	scales=list(cex=1.5,x=list(log=10,at=c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),
																		seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000)),
																		labels=c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),
																		"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000"))),
                       	panel = function(x,y,groups,subscripts,...) {
    							#panel.xYplot(x[subscripts<101],y[subscripts<101],groups[subscripts<101],subscripts[subscripts<101],label.curves=FALSE,method="filled bands",type="l",col=8,col.fill=c(16),cex=c(1.3),lty=c(1),lwd=c(5),pch=c(15),...)
    							panel.xYplot(x[subscripts>(100+sum(repfactor$conc))],y[subscripts>(100+sum(repfactor$conc))],groups[subscripts>(100+sum(repfactor$conc))],subscripts[subscripts>(100+sum(repfactor$conc))],type="p",method="bars",col=rep(repfactor$scolor[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),cex=rep(1.3,(sum(repfactor$conc)+1)),lty=c(1),lwd=c(rep(1,sum(repfactor$conc)),rep(5,2)),pch=rep(repfactor$symbol[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),...)
								panel.abline(h=10,lty=4)
								#panel.abline(b=popSlope,a=popInt,col="red",lwd=5)	
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTlogconcRaw",info$device,height=10,width=12)
		#win.metafile(height=9,width=12,file="QTlogconcRaw.wmf")	
		par(oma=c(0,1,0,0))
		print(plot12)
		dev.off()
	}else{
		print(plot12)
	}

	#ddqtc vs. conc with mid-quartile concentration ranges
	if(Scale)	data$conc <- data$conc*1000

	probs <- 0:info$quantiles/info$quantiles

	for(i in 1:sum(repfactor$conc)){
			eval(parse(text=paste("qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]," <- quantile(data$conc[data$conc>0 & !is.na(data$conc) & data$qtconc==1 & data$trt%in%c(info$trt$",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i],")],probs,na.rm=TRUE)",sep="")))
	}		
	


	qCP <- as.numeric(eval(parse(text=paste("c(",paste("qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(info$label[-which(names(info$trt)%in%c("Placebo","Moxi"))])],sep="",collapse=","),")",sep=""))))

	for(i in 1:sum(repfactor$conc)){
		if(i==1){
			qCPLow <- as.numeric(eval(parse(text=paste("qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i],"[-length(qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i],")]",sep=""))))
			qCPHigh <- as.numeric(eval(parse(text=paste("qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i],"[-1]",sep=""))))
		}else{
			qCPLow <- c(qCPLow,as.numeric(eval(parse(text=paste("qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i],"[-length(qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i],")]",sep="")))))
			qCPHigh <- c(qCPHigh,as.numeric(eval(parse(text=paste("qCP",names(info$label)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i],"[-1]",sep="")))))
		}
	}		

	if(info$conc=="moxi" & !is.na(info$trt$Moxi)){
		CmaxPred <- qtpred$CONC[qtpred$TRT==names(info$trt)[which(names(info$trt)%in%c("Moxi"))][i]]
		QTPred <- qtpred$Pred[qtpred$TRT==names(info$trt)[which(names(info$trt)%in%c("Moxi"))][i]]
		QTLower <- qtpred$Lower[qtpred$TRT==names(info$trt)[which(names(info$trt)%in%c("Moxi"))][i]]
		QTUpper <- qtpred$Upper[qtpred$TRT==names(info$trt)[which(names(info$trt)%in%c("Moxi"))][i]]
	}else{
		for(i in 1:sum(repfactor$conc)){
			if(i==1){
				CmaxPred <- qtpred$CONC[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]]
				QTPred <- qtpred$Pred[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]]
				QTLower <- qtpred$Lower[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]]
				QTUpper <- qtpred$Upper[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]]
				
			}else{
				CmaxPred <- c(CmaxPred,qtpred$CONC[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]])
				QTPred <- c(QTPred,qtpred$Pred[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]])
				QTLower <- c(QTLower,qtpred$Lower[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]])
				QTUpper <- c(QTUpper,qtpred$Upper[qtpred$TRT==names(info$trt)[-which(names(info$trt)%in%c("Placebo","Moxi"))][i]])
			}
		}
	}

	ExposureResponse <- data.frame(CpMid=rep(0,(100+sum(repfactor$conc)+2*sum(repfactor$conc)*length(probs)-sum(repfactor$conc))))
	ExposureResponse$Low 		<- ExposureResponse$CpMid
	ExposureResponse$High		<- ExposureResponse$CpMid
	ExposureResponse$Response 	<- ExposureResponse$CpMid
	ExposureResponse$Lower 		<- rep(NA,nrow(ExposureResponse))
	ExposureResponse$Upper 		<- rep(NA,nrow(ExposureResponse))			

	ExposureResponse$Quantile <- 100*c(rep(c(probs[1:info$quantiles],probs),sum(repfactor$conc)),rep(NA,(100+sum(repfactor$conc))))
	ExposureResponse$Low <- c(qCPLow,qCP,CmaxPred,
									qtpred$CONC[qtpred$TRT==-9999])
	ExposureResponse$High <- c(qCPHigh,qCP,CmaxPred,
									qtpred$CONC[qtpred$TRT==-9999])
											
	ExposureResponse$CpMid <- (ExposureResponse$High-ExposureResponse$Low)/2 + ExposureResponse$Low
	ExposureResponse$logCpMid <- exp((log(ExposureResponse$High)-log(ExposureResponse$Low))/2 + log(ExposureResponse$Low))
	ExposureResponse$CpMedian <- ExposureResponse$CpMid
	ExposureResponse$logCpMedian <- ExposureResponse$logCpMid


	ExposureResponse$N <- rep(NA,nrow(ExposureResponse))
	ExposureResponse$trt <- c(rep(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])],each=length(probs)-1),rep(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])],each=length(probs)),rep(-999,sum(repfactor$conc)),rep(-9999,100))			

	ExposureResponse$type <- c(rep(1:(2*sum(repfactor$conc)),rep(c(length(probs)-1,length(probs)),each=sum(repfactor$conc))),(2*sum(repfactor$conc)+1):(2*sum(repfactor$conc)+sum(repfactor$conc)),rep((2*sum(repfactor$conc)+sum(repfactor$conc))+1,100))

	for(i in 1:(sum(repfactor$conc)*(length(probs)-1))){
		if(i%%(length(probs)-1)!=0){
			temp <- data[!is.na(data$time) & data$qtconc==1 & data$trt==ExposureResponse$trt[i] & data$conc<ExposureResponse$High[i] & data$conc>=ExposureResponse$Low[i] & data$conc>0 & !is.na(data$conc),]
		}else{
			temp <- data[!is.na(data$time) & data$qtconc==1 & data$trt==ExposureResponse$trt[i] & data$conc>=ExposureResponse$Low[i] & data$conc>0 & !is.na(data$conc),]
		} 

		ddqtc <- as.numeric(unlist(by(temp$ddqtc,temp$id,mean,na.rm=TRUE)))
		conc <- as.numeric(unlist(by(temp$conc,temp$id,mean,na.rm=TRUE)))

		ExposureResponse$Response[i] <- mean(ddqtc)
		ExposureResponse$Upper[i] <- ExposureResponse$Response[i] + 1.645*sd(ddqtc)/sqrt(length(ddqtc))
		ExposureResponse$Lower[i] <- ExposureResponse$Response[i] - 1.645*sd(ddqtc)/sqrt(length(ddqtc))
		ExposureResponse$CpMedian[i] <- median(conc)
		ExposureResponse$N[i] <- length(conc)
		
	} 
	
	ExposureResponse$logCpMedian <- ExposureResponse$CpMedian

	#Range of concentrations at therapeutic and supra therapeutic
	ExposureResponse$Response[(sum(repfactor$conc)*(2*length(probs)-1)+1):(sum(repfactor$conc)*(2*length(probs)))] <- QTPred
	ExposureResponse$Lower[(sum(repfactor$conc)*(2*length(probs)-1)+1):(sum(repfactor$conc)*(2*length(probs)))] <- QTLower
	ExposureResponse$Upper[(sum(repfactor$conc)*(2*length(probs)-1)+1):(sum(repfactor$conc)*(2*length(probs)))] <- QTUpper
	ExposureResponse$Response[((sum(repfactor$conc)*(2*length(probs)))+1):((sum(repfactor$conc)*(2*length(probs)))+100)] <- qtpred$Pred[qtpred$TRT=="-9999"]
	ExposureResponse$Lower[((sum(repfactor$conc)*(2*length(probs)))+1):((sum(repfactor$conc)*(2*length(probs)))+100)] <- qtpred$Lower[qtpred$TRT=="-9999"]
	ExposureResponse$Upper[((sum(repfactor$conc)*(2*length(probs)))+1):((sum(repfactor$conc)*(2*length(probs)))+100)] <- qtpred$Upper[qtpred$TRT=="-9999"]

	for(j in 1:sum(repfactor$conc)){
		for(i in (sum(repfactor$conc)*(length(probs)-1)+(j-1)*length(probs)+1):(sum(repfactor$conc)*(length(probs)-1)+(j)*length(probs))){
			ExposureResponse$Response[i] <- min(ExposureResponse$Lower,na.rm=TRUE)-j			
		}
	}

	ExposureResponse$Conc <- ExposureResponse$CpMedian
	ExposureResponse$logConc <- ExposureResponse$logCpMedian

	if(!is.null(info$gof)){
		if(info$gof=="mid"){
			ExposureResponse$Conc <- ExposureResponse$CpMid	
			ExposureResponse$logConc <- ExposureResponse$logCpMid		
		}
	}


	assign("ExposureResponse",ExposureResponse,envir=.GlobalEnv)
	assign("ExposureResponse1",ExposureResponse,envir=.GlobalEnv)

	assign("probs",probs,envir=.GlobalEnv)

	keytext <- list(c(paste(unlist(info$label,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])]," median concentration quantiles",sep=""),paste("Mean (90% CI) Predicted ",QTcorrection," Prolongation",sep="")),cex=c(1))
	keyline <- list(type=c(rep("p",length(unlist(info$label,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])])),"l"),lty=c(1),pch=c(rep(15,sum(repfactor$conc)),1),col=c(rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),"black"),
						cex=1,lwd=c(rep(1,length(unlist(info$label,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])])),5))


	assign("keytext",keytext,envir=.GlobalEnv)
	assign("keyline",keyline,envir=.GlobalEnv)

	plot13normal <- xYplot(Cbind(Response,Lower,Upper)~Conc,subset=!is.na(Response),
						  data=ExposureResponse,
						  layout=c(1,1),
                       aspect=2/3,
							groups=type,
							#xlim=c(-10,max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),ceiling(max(data$ddqtc,na.rm=TRUE))),
							key=list(x=0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=3,between.columns=2,text.width.multiplier=1,
                        text = keytext,
							lines=keyline),
                       xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       					ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                       panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],groups,type="l",method="filled bands",label.curves=FALSE, col=c("black"),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)
								panel.xYplot(x[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],y[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],subscripts[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],groups[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],type=c("p"),methods="bars",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]), lty=c(1),pch=c(15),cex=c(1.5), lwd=c(5),...)
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+1)],y[subscripts>(sum(repfactor$conc)*(length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+1)],subscripts[subscripts>(sum(repfactor$conc)*(length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+1)],groups,type=c("o"),label.curves=FALSE,col=c(rep(1,sum(repfactor$conc)),rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)])),lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
								
								panel.abline(h=10,col="black",lty=4,lwd=2)	
							}, 
							par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTconcQuant",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTconcQuant.wmf")
		par(oma=c(0,1,5,0))
		print(plot13normal)
		dev.off()
	}else{
		print(plot13normal)
	}

	#& logCpMedian>=(min(ExposureResponse$Low[ExposureResponse$type%in%c(1,3) & !is.na(ExposureResponse$logCpMedian)],na.rm=TRUE)-0.0001)
	plot13log <- xYplot(Cbind(Response,Lower,Upper)~logConc,
						  data=ExposureResponse,subset=!is.na(Response),
						  layout=c(1,1),
                       aspect=2/3,
							groups=type,
							#xlim=c(min(ExposureResponse$Low[!is.na(ExposureResponse$logCpMid)],na.rm=TRUE),max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),max(data$ddqtc,na.rm=TRUE)),
                       	key=list(x=0.0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
							text = keytext,
							lines=keyline),
                       xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       					ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(log=10,at=c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),
																		seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000)),
																		labels=c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),
																		"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000"))),
                       panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],groups,type="l",method="filled bands",label.curves=FALSE, col=c("black"),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)
								panel.xYplot(x[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],y[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],subscripts[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],groups[subscripts<(sum(repfactor$conc)*(length(probs)-1)+1)],type=c("p"),methods="bars",label.curves=FALSE,col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]), lty=c(1),pch=c(15),cex=c(1.5), lwd=c(5),...)
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+1)],y[subscripts>(sum(repfactor$conc)*(length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+1)],subscripts[subscripts>(sum(repfactor$conc)*(length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+1)],groups,type=c("o"),label.curves=FALSE,col=c(rep(1,sum(repfactor$conc)),rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)])),lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
								
								panel.abline(h=10,col="black",lty=4,lwd=2)	
							}, 
							par.strip.text=list(cex=1.5))


	if(!is.na(info$output)){
		outputGraph("QTlogconcQuant",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTlogconcQuant.wmf")	
		par(oma=c(0,1,5,0))
		print(plot13log)
		dev.off()
	}else{
		print(plot13log)
	}
	
	if(!is.na(info$output)){
		outputGraph("QTconcQuantReport",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTconcQuantReport.wmf")
		par(oma=c(0,1,5,0))
		if(info$scale=="normal") print(plot13normal)
		if(info$scale=="log") print(plot13log)
		dev.off()
	}


	keytext <- list(c(paste("Mean (90% CI) Predicted ",QTcorrection," Prolongation at ",unlist(info$label,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])]," Mean Cmax",sep=""),paste("Mean (90% CI) Predicted ",QTcorrection," Prolongation",sep="")),cex=c(1))
	keyline <- list(type=c(rep("p",length(unlist(info$label,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])])),"l"),lty=c(1),pch=c(rep(15,sum(repfactor$conc)),1),col=c(rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),"black"),
						cex=1,lwd=c(rep(1,length(unlist(info$label,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])])),5))

	assign("keytext",keytext,envir=.GlobalEnv)
	assign("keyline",keyline,envir=.GlobalEnv)

	plot13cmaxnormal <- xYplot(Cbind(Response,Lower,Upper)~Conc,subset=!is.na(Response),
						  data=ExposureResponse,
						  layout=c(1,1),
                       aspect=2/3,
							groups=type,
							#xlim=c(-10,max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),ceiling(max(data$ddqtc,na.rm=TRUE))),
							key=list(x=0.0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
                        text = keytext,
							lines=keyline),
                       xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       					ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.4),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                       panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],groups,type="l",method="filled bands",label.curves=FALSE, col=c("black"),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)

								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],groups,type="p",method="bars",label.curves=FALSE, col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]), lty=c(1),pch=c(16),cex=c(.1), lwd=c(5),...)
								panel.abline(h=10,col=1,lty=4,lwd=2)
							}, 
							par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTconcCmax",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTconcCmax.wmf")	
		par(oma=c(0,1,5,0))
		print(plot13cmaxnormal)
		dev.off()
	}else{
		print(plot13cmaxnormal)
	}


	plot13cmaxnewnormal <- xYplot(Cbind(Response,Lower,Upper)~Conc,subset=!is.na(Response),
						  data=ExposureResponse,
						  layout=c(1,1),
                       				aspect=2/3,
							groups=type,
							#xlim=c(-10,max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),ceiling(max(data$ddqtc,na.rm=TRUE))),
							key=list(x=0.0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=1,text.width.multiplier=1,
                        				text = keytext,
								lines=keyline),
                       				xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       						ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.4),
                       				strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       				scales=list(cex=1.5),
                       				panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],groups,type="l",method="filled bands",label.curves=FALSE, col=c("black"),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)
								panel.abline(h=10,col=1,lty=4,lwd=2)

								panel.arrows(x0=x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],x1=x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],y0=min(ExposureResponse$Lower,na.rm=TRUE)-3,y1=y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),lwd=5)
								panel.arrows(x0=0.9*x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],y0=y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],x1=0,y1=y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),lwd=5)									
							}, 
							par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTconcCmaxNew",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTconcCmaxNew.wmf")
		par(oma=c(0,1,5,0))
		print(plot13cmaxnewnormal)
		dev.off()
	}else{
		print(plot13cmaxnewnormal)
	}


	# & logCpMid>=(min(ExposureResponse$Low[ExposureResponse$type%in%c(1,3) & !is.na(ExposureResponse$logCpMid)],na.rm=TRUE)-0.001)
	plot13cmaxlog <- xYplot(Cbind(Response,Lower,Upper)~logConc,
						  data=ExposureResponse,subset=!is.na(Response),
						  layout=c(1,1),
                       aspect=2/3,
							groups=type,
							#xlim=c(min(ExposureResponse$Low[!is.na(ExposureResponse$logCpMid)],na.rm=TRUE),max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),ceiling(max(data$ddqtc,na.rm=TRUE))),
							key=list(x=0.0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
                        text = keytext,
							lines=keyline),
                       xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       					ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.4),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(log=10,at=c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),
																		seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000)),
																		labels=c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),
																		"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000"))),
                       panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],groups,type="l",method="filled bands",label.curves=FALSE, col=c("black"),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)

								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],groups,type="p",method="bars",label.curves=FALSE, col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]), lty=c(1),pch=c(16),cex=c(.1), lwd=c(5),...)
								panel.abline(h=10,col=1,lty=4,lwd=2)
							}, 
							par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTlogconcCmax",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTlogconcCmax.wmf")
		par(oma=c(0,1,5,0))
		print(plot13cmaxlog)
		dev.off()
	}else{
		print(plot13cmaxlog)
	}

	plot13cmaxnewlog <- xYplot(Cbind(Response,Lower,Upper)~logConc,
						  data=ExposureResponse,subset=!is.na(Response),
						  layout=c(1,1),
                       aspect=2/3,
							groups=type,
							#xlim=c(min(ExposureResponse$Low[!is.na(ExposureResponse$logCpMid)],na.rm=TRUE),max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),ceiling(max(data$ddqtc,na.rm=TRUE))),
							key=list(x=0.0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
                        text = keytext,
							lines=keyline),
                       xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       					ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.4),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(log=10,at=c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),
																		seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000)),
																		labels=c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),
																		"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000"))),
                       panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],subscripts[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc))],groups,type="l",method="filled bands",label.curves=FALSE, col=c("black"),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)
								panel.abline(h=10,col=1,lty=4,lwd=2)

								panel.arrows(x0=x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],x1=x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],y0=min(ExposureResponse$Lower,na.rm=TRUE)-3,y1=y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),lwd=5)
								panel.arrows(x0=0.9*x[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],y0=y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],x1=log10(min(ExposureResponse$Low[!is.na(ExposureResponse$logCpMid)],na.rm=TRUE)),y1=y[subscripts>(sum(repfactor$conc)*(2*length(probs)-1)) & subscripts<(sum(repfactor$conc)*(2*length(probs)-1)+sum(repfactor$conc)+1)],col=rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),lwd=5)
							}, 
							par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTlogconcCmaxNew",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTlogconcCmaxNew.wmf")
		par(oma=c(0,1,5,0))
		print(plot13cmaxnewlog)
		dev.off()
	}else{
		print(plot13cmaxnewlog)
	}

	if(!is.na(info$output)){
		outputGraph("QTconcCmaxReport",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTconcCmaxReport.wmf")
		par(oma=c(0,1,5,0))

		if(info$scale=="normal") print(plot13cmaxnewnormal)
		if(info$scale=="log") print(plot13cmaxnewlog)
		dev.off()
	}


	#############GOF combined
	probs <- 0:info$quantiles/info$quantiles

	unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])]

	if(is.na(info$bin[1])){
		qCPtherapeutic <- quantile(data$conc[data$trt%in%unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))][!is.na(unlist(info$trt,use.names=FALSE)[-which(names(info$trt)%in%c("Placebo","Moxi"))])] & data$conc>0 & !is.na(data$conc) & data$qtconc==1],probs,na.rm=TRUE)
	}else{
		Bin <- sort(c(min(data$conc[data$conc>0 & !is.na(data$conc) & data$qtconc==1],na.rm=TRUE),info$bin,max(data$conc[data$conc>0 & !is.na(data$conc) & data$qtconc==1],na.rm=TRUE)))			
		Bin <- Bin[Bin>=min(data$conc[data$conc>0 & !is.na(data$conc) & data$qtconc==1],na.rm=TRUE) & Bin<=max(data$conc[data$conc>0 & !is.na(data$conc) & data$qtconc==1],na.rm=TRUE)]
		qCPtherapeutic <- Bin
		probs <- Bin					
	}
	
	ExposureResponse <- data.frame(CpMid=rep(0,102+length(probs)*4-2),Low=rep(0,102+length(probs)*4-2),High=rep(0,102+length(probs)*4-2),Response=rep(0,102+length(probs)*4-2),Lower=rep(NA,102+length(probs)*4-2),Upper=rep(NA,102+length(probs)*4-2))
	ExposureResponse$Quantile <- 100*c(probs[-length(probs)],probs,rep(NA,2*(length(probs)-1)+1),rep(NA,102))
	ExposureResponse$Low <- c(as.numeric(qCPtherapeutic[-length(qCPtherapeutic)]),
											as.numeric(qCPtherapeutic),
											as.numeric(rep(NA,(length(probs)-1))),
											as.numeric(rep(NA,length(probs))),
											ifelse(length(qtpred$CONC[qtpred$TRT==info$trt$Therapeutic])==0,NA,qtpred$CONC[qtpred$TRT==info$trt$Therapeutic]),
											ifelse(length(qtpred$CONC[qtpred$TRT==info$trt$Supra])==0,NA,qtpred$CONC[qtpred$TRT==info$trt$Supra]),
											qtpred$CONC[qtpred$TRT==-9999])
											
	ExposureResponse$High <- c(	as.numeric(qCPtherapeutic[-1]),
											as.numeric(qCPtherapeutic),
											as.numeric(rep(NA,(length(probs)-1))),
											as.numeric(rep(NA,length(probs))),
											ifelse(length(qtpred$CONC[qtpred$TRT==info$trt$Therapeutic])==0,NA,qtpred$CONC[qtpred$TRT==info$trt$Therapeutic]),
											ifelse(length(qtpred$CONC[qtpred$TRT==info$trt$Supra])==0,NA,qtpred$CONC[qtpred$TRT==info$trt$Supra]),
											qtpred$CONC[qtpred$TRT==-9999])

	ExposureResponse$CpMid <- (ExposureResponse$High-ExposureResponse$Low)/2 + ExposureResponse$Low
	ExposureResponse$logCpMid <- exp((log(ExposureResponse$High)-log(ExposureResponse$Low))/2 + log(ExposureResponse$Low))
	ExposureResponse$CpMedian <- ExposureResponse$CpMid
	ExposureResponse$logCpMedian <- ExposureResponse$logCpMid

	ExposureResponse$N <- rep(NA,nrow(ExposureResponse))

	if(!is.na(info$trt$Supra)){
		ExposureResponse$trt <- c(rep(info$trt$Therapeutic,length(probs)*2-1),rep(info$trt$Supra,length(probs)*2-1),-999,-999,rep(-9999,100))			
	}else{
		ExposureResponse$trt <- c(rep(info$trt$Therapeutic,length(probs)*2-1),rep("Supra",length(probs)*2-1),-999,-999,rep(-9999,100))				
	}

	ExposureResponse$type <- c(rep(1,length(probs)-1),rep(2,length(probs)),rep(3,length(probs)-1),rep(4,length(probs)),5,5,rep(6,100))

	for(i in 1:(length(probs)-1)){
		if(i==1) temp <- data[!is.na(data$time) & data$qtconc==1 & data$conc<ExposureResponse$High[i] & data$conc>0 & !is.na(data$conc),]
		if(i>1 & i<(length(probs)-1)) temp <- data[!is.na(data$time) & data$qtconc==1 & data$conc<ExposureResponse$High[i] & data$conc>=ExposureResponse$Low[i] & data$conc>0 & !is.na(data$conc),]
		if(i==(length(probs)-1)) temp <- data[!is.na(data$time) & data$qtconc==1 & data$conc>=ExposureResponse$Low[i] & data$conc>0 & !is.na(data$conc),]


		ddqtc <- as.numeric(unlist(by(temp$ddqtc,temp$id,mean,na.rm=TRUE)))
		conc <- as.numeric(unlist(by(temp$conc,temp$id,mean,na.rm=TRUE)))

		ExposureResponse$Response[i] <- mean(ddqtc)
		ExposureResponse$Upper[i] <- ExposureResponse$Response[i] + 1.645*sd(ddqtc)/sqrt(length(ddqtc))
		ExposureResponse$Lower[i] <- ExposureResponse$Response[i] - 1.645*sd(ddqtc)/sqrt(length(ddqtc))
		ExposureResponse$CpMedian[i] <- median(conc)
		ExposureResponse$N[i] <- length(conc)
	}

	for(i in (length(probs)*2):(length(probs)*3-2)){
		ExposureResponse$Response[i] <- NA
		ExposureResponse$Upper[i] <- NA
		ExposureResponse$Lower[i] <- NA
		ExposureResponse$CpMedian[i] <- NA
		ExposureResponse$N[i] <- NA
	}

	ExposureResponse$logCpMedian <- ExposureResponse$CpMedian

	#Range of concentrations at therapeutic and supra therapeutic
	ExposureResponse$Response[length(probs)*4-1] <- ifelse(length(qtpred$Pred[qtpred$TRT==info$trt$Therapeutic])==0,NA,qtpred$Pred[qtpred$TRT==info$trt$Therapeutic]) #popInt
	ExposureResponse$Response[length(probs)*4] <- ifelse(length(qtpred$Pred[qtpred$TRT==info$trt$Supra])==0,NA,qtpred$Pred[qtpred$TRT==info$trt$Supra]) #popInt
	ExposureResponse$Lower[length(probs)*4-1] <- ifelse(length(qtpred$Lower[qtpred$TRT==info$trt$Therapeutic])==0,NA,qtpred$Lower[qtpred$TRT==info$trt$Therapeutic]) 
	ExposureResponse$Lower[length(probs)*4] <- ifelse(length(qtpred$Lower[qtpred$TRT==info$trt$Supra])==0,NA,qtpred$Lower[qtpred$TRT==info$trt$Supra]) 
	ExposureResponse$Upper[length(probs)*4-1] <- ifelse(length(qtpred$Upper[qtpred$TRT==info$trt$Therapeutic])==0,NA,qtpred$Upper[qtpred$TRT==info$trt$Therapeutic]) 
	ExposureResponse$Upper[length(probs)*4] <- ifelse(length(qtpred$Upper[qtpred$TRT==info$trt$Supra])==0,NA,qtpred$Upper[qtpred$TRT==info$trt$Supra]) 
	ExposureResponse$Response[(length(probs)*4+1):(length(probs)*4+100)] <- qtpred$Pred[qtpred$TRT==-9999]
	ExposureResponse$Lower[(length(probs)*4+1):(length(probs)*4+100)] <- qtpred$Lower[qtpred$TRT==-9999]
	ExposureResponse$Upper[(length(probs)*4+1):(length(probs)*4+100)] <- qtpred$Upper[qtpred$TRT==-9999]
	ExposureResponse$Response[length(probs):(length(probs)*2-1)] <- min(ExposureResponse$Lower,na.rm=TRUE)-1			
	ExposureResponse$Response[(3*length(probs)-1):(length(probs)*4-2)] <- min(ExposureResponse$Lower,na.rm=TRUE)-2	

	ExposureResponse$Conc <- ExposureResponse$CpMedian
	ExposureResponse$logConc <- ExposureResponse$logCpMedian
	if(!is.null(info$gof)){
		if(info$gof=="mid"){
			ExposureResponse$Conc <- ExposureResponse$CpMid	
			ExposureResponse$logConc <- ExposureResponse$logCpMid		
		}
	}

	assign("ExposureResponse",ExposureResponse,envir=.GlobalEnv)
	assign("ExposureResponse2",ExposureResponse,envir=.GlobalEnv)
	assign("probs",probs,envir=.GlobalEnv)

	plot13 <- xYplot(Cbind(Response,Lower,Upper)~Conc,subset=!is.na(Response),
						  data=ExposureResponse,
						  layout=c(1,1),
                       aspect=2/3,
							groups=type,
							#xlim=c(-10,max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),ceiling(max(data$ddqtc,na.rm=TRUE))),
							key=list(x=0.0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
							text = list(c(paste(as.character(info$pk$drugname)," median concentration quantiles",sep=""),"Mean (90% CI) predicted"),cex=c(1)),
							lines=list(type=c("p","l"),lty=c(1),pch=c(15),cex=c(1.2),col=c("blue","black"),lwd=c(3,5))),
                       xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       					ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                       panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>4*length(probs)],y[subscripts>4*length(probs)],subscripts[subscripts>4*length(probs)],groups,type="l",method="filled bands",label.curves=FALSE, col=c("black"),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)
								if(!is.na(ExposureResponse$Response[2])) panel.xYplot(x[subscripts<length(probs)],y[subscripts<length(probs)],subscripts[subscripts<length(probs)],groups[subscripts<length(probs)],type=c("p"),methods="bars",label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(15),cex=c(1.5), lwd=c(5),...)
								if(!is.na(ExposureResponse$Response[2])) panel.xYplot(x[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],y[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],subscripts[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],type=c("o"),label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
								#if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],y[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],subscripts[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],type=c("p"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(16),cex=c(1.5), lwd=c(5),...)
								#if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],y[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],subscripts[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],type=c("o"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)
								#if(!is.na(ExposureResponse$Response[4*length(probs)-1])) panel.xYplot(x[subscripts==(4*length(probs)-1)],y[subscripts==(4*length(probs)-1)],subscripts[subscripts==(4*length(probs)-1)],groups,type="p",method="bars",label.curves=FALSE, col=c("blue"), lty=c(1),pch=c(16),cex=c(.1), lwd=c(5),...)
   	 							#if(!is.na(ExposureResponse$Response[4*length(probs)])) panel.xYplot(x[subscripts==(4*length(probs))],y[subscripts==(4*length(probs))],subscripts[subscripts==(4*length(probs))],groups,type="p",method="bars",label.curves=FALSE, col=c(8), lty=c(1),pch=c(16),cex=c(.1), lwd=c(5),...)
								#panel.abline(b=popSlope,a=popInt,col=1,lwd=5)	
								panel.abline(h=10,col="black",lty=4,lwd=2)	
							}, 
							par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTconcQuantCombined",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTconcQuantCombined.wmf")	
		par(oma=c(0,1,5,0))
		print(plot13)
		dev.off()
	}else{
		print(plot13)
	}

	# & logCpMedian>=(min(ExposureResponse$Low[ExposureResponse$type%in%c(1,3) & !is.na(ExposureResponse$logCpMedian)],na.rm=TRUE)-0.0001)
	plot13 <- xYplot(Cbind(Response,Lower,Upper)~logConc,
						  data=ExposureResponse,subset=!is.na(Response),
						  layout=c(1,1),
                       aspect=2/3,
							groups=type,
							#xlim=c(min(ExposureResponse$Low[!is.na(ExposureResponse$logCpMid)],na.rm=TRUE),max(ExposureResponse$High,na.rm=TRUE)),
							ylim=c(min(ExposureResponse$Lower,na.rm=TRUE)-3,max(ExposureResponse$Upper,na.rm=TRUE)+3),
							#ylim=c(floor(min(data$ddqtc,na.rm=TRUE)),max(data$ddqtc,na.rm=TRUE)),
							key=list(x=0.0,y=1.1,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
							text = list(c(paste(as.character(info$pk$drugname)," median concentration quantiles",sep=""),"Mean (90% CI) predicted"),cex=c(1)),
							lines=list(type=c("p","l"),lty=c(1),cex=1.2,pch=c(15),col=c("blue","black"),lwd=c(3,5))),
                       xlab=list(paste(as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
       					ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,x=list(log=10,at=c(seq(0.001,0.01,0.001),seq(0.02,0.1,0.01),seq(0.2,1,0.1),seq(2,10,1),
																		seq(20,100,10),seq(200,1000,100),seq(2000,10000,1000),seq(20000,100000,10000)),
																		labels=c("0.001",rep("",8),"0.01",rep("",8),"0.1",rep("",8),"1",rep("",8),"10",rep("",8),
																		"100",rep("",8),"1000",rep("",8),"10000",rep("",8),"100000"))),
                       panel = function(x,y,subscripts,groups,...) {
								panel.xYplot(x[subscripts>4*length(probs)],y[subscripts>4*length(probs)],subscripts[subscripts>4*length(probs)],groups,type="l",method="filled bands",label.curves=FALSE, col=c(1),col.fill="grey", lty=c(1),pch=c(1),cex=c(2), lwd=c(5),...)
								if(!is.na(ExposureResponse$Response[2])) panel.xYplot(x[subscripts<length(probs)],y[subscripts<length(probs)],subscripts[subscripts<length(probs)],groups[subscripts<length(probs)],type=c("p"),methods="bars",label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(15),cex=c(1.5), lwd=c(5),...)
								if(!is.na(ExposureResponse$Response[2])) panel.xYplot(x[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],y[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],subscripts[subscripts>(length(probs)-1) & subscripts<(2*length(probs))],type=c("o"),label.curves=FALSE,col=c("blue"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)   	 							
								#if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],y[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],subscripts[subscripts>(2*length(probs)-1) & subscripts<(3*length(probs)-1)],type=c("p"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(16),cex=c(1.5), lwd=c(5),...)
								#if(!is.na(ExposureResponse$Response[2*length(probs)])) panel.xYplot(x[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],y[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],subscripts[subscripts>(3*length(probs)-2) & subscripts<(4*length(probs)-1)],type=c("o"),label.curves=FALSE,col=c("red"), lty=c(1),pch=c(124),cex=c(1.5), lwd=c(5),...)
								#if(!is.na(ExposureResponse$Response[4*length(probs)-1])) panel.xYplot(x[subscripts==(4*length(probs)-1)],y[subscripts==(4*length(probs)-1)],subscripts[subscripts==(4*length(probs)-1)],groups,type="p",method="bars",label.curves=FALSE, col=c("blue"), lty=c(1),pch=c(16),cex=c(.1), lwd=c(5),...)
   	 							#if(!is.na(ExposureResponse$Response[4*length(probs)])) panel.xYplot(x[subscripts==(4*length(probs))],y[subscripts==(4*length(probs))],subscripts[subscripts==(4*length(probs))],groups,type="p",method="bars",label.curves=FALSE, col=c("red"), lty=c(1),pch=c(16),cex=c(.1), lwd=c(5),...)
								#panel.abline(b=popSlope,a=popInt,col=1,lwd=5)	
								panel.abline(h=10,col="black",lty=4,lwd=2)	
							}, 
							par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("QTlogconcQuantCombined",info$device,height=9,width=12)
		#win.metafile(height=9,width=12,file="QTlogconcQuantCombined.wmf")
		par(oma=c(0,1,5,0))
		print(plot13)
		dev.off()
	}else{
		print(plot13)
	}

	#########################
	###QQ plot of residuals

	no <- length(unique(predblup$trtm[!is.na(predblup$trtm)]))

	if(info$design=="parallel"){
		plot14 <- qqmath(~Resid|as.factor(trtm),data=predblup,
                       layout=c(1,no),
                       aspect="fill",
						  xlab=list("Quantiles of standard normal",cex=1.5),
						ylab=list("Residuals (ms)",cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,y=list(relation="free")),
                       prepanel = prepanel.qqmathline,
							panel = function(x,y,distribution,...) {
    							panel.qqmathline(x, distribution=qnorm,...)
								panel.qqmath(x,type="p",col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)
							},
						 par.strip.text=list(cex=1.5))

		if(!is.na(info$output)){
			outputGraph("residQQtrtm",info$device,height=12,width=8)
			#win.metafile(height=12,width=8,file="residQQtrtm.wmf")
			#par(oma=c(-0,-1,-0,-0),mgp=c(3,1,.5))
			print(plot14)
			dev.off()
		}else{
			print(plot14)
		}
	}

	plot15 <- qqmath(~Resid,data=predblup,
                       layout=c(1,1),
                       aspect="fill",#10/2,
						  xlab=list("Quantiles of standard normal",cex=1.5),
						ylab=list("Residuals (ms)",cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,y=list(relation="free")),
                       prepanel = prepanel.qqmathline,
							panel = function(x,y,distribution,...) {
    							panel.qqmathline(x, distribution=qnorm,...)
								panel.qqmath(x,type="p",col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("residQQall",info$device,height=9,width=9)
		#win.metafile(height=9,width=9,file="residQQall.wmf")
		#par(oma=c(0,1,0,0))
		print(plot15)
		dev.off()
	}else{
		print(plot15)
	}

	###ddQT vs. Conc
	random1 <- random[random$Effect=="Slope" & random$TRT!=-9999,]
	if(info$intercept=="No") random1$Intercept <- rep(0,nrow(random1))
	
	plot16<- xyplot(Intercept~Slope,data=random1,
                       layout=c(1,1),groups=type,
                       aspect="fill",#10/2,
				xlim=c(0,ifelse(info$scale=="log",ceiling(log(max(predblup$CONC))),ceiling(max(predblup$CONC)))),
				ylim=c(floor(min(predblup$DDQTCOBS,na.rm=TRUE)),ceiling(max(predblup$DDQTCOBS,na.rm=TRUE))),
       			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
				xlab=list(paste(if(info$scale=="log"){"Log "},as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
				key=list(x=0,y=1.05,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=2,between.columns=2,text.width.multiplier=1,
               			text = list(c("Individual predictions","Population mean prediction"),cex=c(1)),
				 			lines=list(type=c("l"),lty=c(8,1),pch=c(1),col=c("grey","red"),cex=1.3,lwd=c(4))),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                        #prepanel = prepanel.xyplot,
							panel = function(x,y,groups,subscripts,...) {
									for(i in 1:length(x)){
										if(groups[i]==1)	panel.abline(a=as.numeric(y)[i],b=as.numeric(x)[i],col=c("grey"),cex=c(1.5),lty=c(8),lwd=1)											
									}
									for(i in 1:length(x)){
										if(groups[i]!=1)	panel.abline(a=as.numeric(y)[i],b=as.numeric(x)[i],col=c("red"),cex=c(1.5),lty=c(1),lwd=5)										
									}
									panel.abline(h=10,col=1,lty=4,lwd=5)
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("predqtconc",info$device,height=9,width=9)
		#win.metafile(height=9,width=9,file="predqtconc.wmf")
		print(plot16)
		dev.off()
	}else{
		print(plot16)
	}

	random2 <- random[random$Effect=="Slope" & random$TRT!=-9999,]
	if(info$intercept=="No") random2$Intercept <- rep(0,nrow(random2))
	random2 <- random2[order(random2$ID),]
	#random2 <- sort.col(random2,columns.to.sort="@ALL", columns.to.sort.by=c("ID"),ascending=FALSE)

	if(info$design=="parallel"){
		plot16<- xyplot(Intercept~Slope,data=random2,
                       layout=c(1,1),groups=TRT,
                       aspect="fill",
				xlim=c(0,ifelse(info$scale=="log",ceiling(log(max(predblup$CONC))),ceiling(max(predblup$CONC)))),
				ylim=c(floor(min(predblup$DDQTCOBS,na.rm=TRUE)),ceiling(max(predblup$DDQTCOBS,na.rm=TRUE))),
				key=list(x=0,y=1.0,corner=c(0,1),border=FALSE,transparent=TRUE,columns=1,between=1,between.columns=3,text.width.multiplier=1,
               		text = list(c(unique(unlist(info$label))[-c(which(names(info$label)=="Placebo"),which(names(info$label)=="Moxi"))],"Population mean"),cex=c(1)),
				 		lines=list(type=c("l"),lty=c(1),pch=c(1),col=c(rep(repfactor$color[order(repfactor$order)],repfactor$conc[order(repfactor$order)]),"black"),cex=1.3,lwd=c(4))),
       			ylab=list(paste(QTcorrection,ifelse(info$delta=="single"," change from baseline (ms)"," change from placebo and baseline adjusted (ms)"),sep=""),cex=1.5),
				xlab=list(paste(if(info$scale=="log"){"Log "},as.character(info$pk$drugname)," concentration (",as.character(info$pk$drugunit),")",sep=""),cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5),
                        #prepanel = prepanel.xyplot,
							panel = function(x,y,groups,subscripts,...) {
									for(i in 1:length(x)){
										if(groups[i]==-999){
											panel.abline(a=as.numeric(y)[i],b=as.numeric(x)[i],col=c("black"),cex=c(1.5),lty=c(1),lwd=10)		
										}else{
											panel.abline(a=as.numeric(y)[i],b=as.numeric(x)[i],col=repfactor$color[which(repfactor$trt==names(info$trt)[which(unique(unlist(info$trt))==groups[i])])],cex=c(1.5),lty=c(1),lwd=1)		
										}#if(!is.na(info$trt$Supra) & groups[i]==info$trt$Supra) panel.abline(a=as.numeric(y)[i],b=as.numeric(x)[i],col=c("red"),cex=c(1.5),lty=c(1),lwd=1)						
									}
									panel.abline(h=10,col="black",lty=4,lwd=2)
							},
						 par.strip.text=list(cex=1.5))

		if(!is.na(info$output)){
			outputGraph("predqtconctrt",info$device,height=9,width=9)
			#win.metafile(height=9,width=9,file="predqtconctrt.wmf")
			par(oma=c(0,1,0,0))
			print(plot16)
			dev.off()
		}else{
			print(plot16)
		}
	}
	###QQ plot of random effects
	no <- 1
	if(info$intercept!="No") no <- 2 

	plot17 <- qqmath(~Estimate|as.factor(Effect),data=random,
                       layout=c(1,no),
                       aspect="fill",
						  xlab=list("Quantiles of standard normal",cex=1.5),
						ylab=list("Random-effects",cex=1.5),
                       strip=function(...) strip.default(..., strip.names=c(FALSE,TRUE), style=1),
                       scales=list(cex=1.5,y=list(relation="free")),
                       prepanel = prepanel.qqmathline,
							panel = function(x,y,distribution,...) {
    								panel.qqmathline(x, distribution=qnorm,...)
								panel.qqmath(x,type="p",col=c("black"),cex=c(1),lty=c(1),lwd=1,pch=c(16),...)
							},
						 par.strip.text=list(cex=1.5))

	if(!is.na(info$output)){
		outputGraph("ranefQQ",info$device,height=12,width=8)
		#win.metafile(height=12,width=8,file="ranefQQ.wmf")	
		#par(oma=c(-0,-1,-0,-0),mgp=c(3,1,.5))
		print(plot17)
		dev.off()
	}else{
		print(plot17)
	}

	if(!is.na(info$output)){
		setwd(opath)
	}	


	if(info$report==TRUE){
		wdGoToBookmark("blankConc")
		wdNormal("")

		wdBody(paste("The relationship between dd",QTcorrection," and ",info$pk$drugname," concentrations was investigated by linear mixed-effects modeling.",sep=""))
		wdBody("The following three linear models were considered:")
		wdBody("         Model 1 is a linear model with an intercept")
		wdBody("         Model 2 is a linear model with mean intercept fixed to 0 (with variability)")
		wdBody("         Model 3 is a linear model with no intercept")

		wdBody(paste("Table 3 summarizes the results of the ",as.character(info$pk$drugname),"-dd",QTcorrection," analyses. Model ",tableinfo$Model," was used for further analysis since the model ",tableinfo$ModelTXT," was found to fit the data best.",sep=""))
		wdBody(paste("Table 3: Exposure-response analysis of ",as.character(info$pk$drugname)," associated dd",QTcorrection," prolongation.",sep=""))
		wdTable(format(table))

		wdBody(paste("The relationship between ",info$pk$drugname," concentrations and dd",QTcorrection," is visualized in the Figure 6.",sep=""))
		wdBody(paste("Figure 6: Observed dd",QTcorrection," vs. ",as.character(info$pk$drugname)," concentrations together with the population predictions (solid red line).",sep=""))
		wdPlot(plot12normal,width=12,height=9)
		
		wdBody(paste("The goodness-of-fit plot in Figure 7 shows the observed median-quantile ",info$pk$drugname," concentrations and associated mean (90% CI) dd",QTcorrection," (90% CI) together with the mean (90% CI) predicted dd",QTcorrection,".",sep="")) 
		wdBody(paste("Figure 7: Observed median-quantile ",info$pk$drugname," concentrations and associated mean (90% CI) dd",QTcorrection," (colored dots) together with the mean (90% CI) predicted dd",QTcorrection," (black line with shaded grey area).",sep="")) 
		wdPlot(plot13normal,width=12,height=9)

		wdBody(paste("The predicted dd",QTcorrection," at the geometric mean peak ",info$pk$drugname," concentrations can be found in Table 4 and visualized in Figure 8.",sep=""))
		wdBody(paste("Table 4: Predicted dd",QTcorrection," interval at geometric mean peak ",as.character(info$pk$drugname)," concentration using model ",tableinfo$Model,".",sep=""))
		wdTable(format(tablepred))

		wdBody(paste("Figure 8: Mean (90% CI) predicted dd",QTcorrection," at geometric mean Cmax.",sep=""))
		wdPlot(plot13cmaxnewnormal,width=12,height=9)

		wdGoToBookmark("blankTitle")

		wdNormal("")
		wdTable(format(tableinfo[,1:5]))

		wdSave("QT")
	}


	options(warn = 1)
}
