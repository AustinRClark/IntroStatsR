#
# Statistics 211 R Library
# Created By: Austin Clark
# Made to follow book:
# Intro Stats Fourth Edition De Veaux Velleman Bock 2014 
# ISBN 978-0-321-82527-8
# DISCLAIMER: I do not take credit for the information in this library, I only
# put all of it together so other students could effectively learn and use the
# statistical programming language R in their introductory statistics class.
# Best, Austin
# 					20160721033555
# Any questions can be addressed on the github repository 
# https://github.com/AustinRClark/IntroStatsR
#
# The programming language R is an open-source statistical software package 
# that can be easily installed on any computer. It is similar to the command
# line and is object-oriented.
# 
# It can be downloaded at http:/cran.r-project.org/
#
# Help Feature in R: > ? Command_Name
# Replacing Command_Name with the command that you want more information about
# Intro to R can be found at: http://cran.r-project.org/doc/manuals/R-intro.html

###################
# ESSENTIALS OF R #
###################

# + - * / %/% %% ^ arithmetic (plus,minus,times,divide,integer quotient,modulo,power)
# >= < <= == != relational (greater than, greater than or equals, less than, less than or equals, equals, not equals)
# ! & | logical (not, and, or)
# ~  model formulae ('is modelled as a function of')
# <- -> assignment (gets)
# $	list indexing (the 'element name' operator
# : 	create a sequence
 

#################################
# Chapter 1 - Stats Starts Here #
#################################

# x=c(1,2,3,4)			Creating a vector called x with values
# x=read.table(file.choose(),header=TRUE)	Import data from file and save as a vector

##########################################################
# Chapter 2 - Displaying and Describing Categorical Data #
##########################################################

# table(x)			frequency table for variable x
# barplot(table(x))		bar chart for x
# pie(table(x))			pie chart for x
# barplot(xtabs(~X+Y))		stacked bar charts 2 variable

############################################################
# Chapter 3 - Displaying and Summarizing Quantitative Data #
############################################################

# summary(x)			5-number summary(Min.,1st Qu.,Median,Mean,3rd 					Qu.,Max.)
# mean(x)			mean
# median(x)			median
# sd(x)				standard deviation
# hist(x)			histogram
# bloxplot(x)			boxplot
# min(x)			minimum
# max(x)			maximum

mode=function(x)
{
	out=names(sort(-table(x)))[1]
	cat("Mode: ",out,"\n")
}

#########################################################
# Chapter 4 - Understanding and Comparing Distributions #
#########################################################

# boxplot(x1,x2,x3...)		side-by-side boxplots
# boxplot(X~Y)			"    "  "    "       one for each level of Y

######################################################################
# Chapter 5 - The Standard Deviation as a Ruler and the Normal Model #
######################################################################

# qqnorm(x)			Normal probability (Q-Q) plot, switch axis datax = TRUE within qqnorm

# Standardize variable
standardize=function(x)
{
	z=(x -mean(x))/sd(x)
	return(z)
}
# Standardize z-score
zScore=function(y,ybar,s)
{
	zScore=(y-ybar)/s
	cat("Z-score: ",zScore,"\n")
}
zScore.normal=function(y,mew,sigma)
{
	zScore=(y-mew)/sigma
	cat("Z-score: ",zScore,"\n")
}
# Convert z-scores to percentiles
zScore.percentile=function(z)
{
	percentile=round(pnorm(z)*100,0)
	return(percentile)
}


##########################################################
# Chapter 6 - Scatterplots, Association, and Correlation #
##########################################################

# plot(x,y)			scatterplot 
#				if data is in data frame, will need to attach 
#				data frame, use: with(DATA,plot(X~Y) or plot(Y~X),data=DATA) where DATA is the name of the data frame

# cor(x,y)			correlation [THERE ARE DIFFERENT METHODS OF CALCULATING THIS CAUTION, safer to use scatCor
correlation=function(x,y,n)
{
	# n is the number of objects in arrays x & y
	xmean=mean(x)
	ymean=mean(ty)
	sdx=sd(x)
	sdy=sd(y)
	devfromX=x-xmean
	devfromY=y-ymean
	products=devfromX*devfromY
	su=sum(products)
	denom=(n-1)*sdx*sdy
	r=su/denom
	return(r)
}


#################################
# Chapter 7 - Linear Regression #
#################################

# lm(y~x)			Produces the linear model
# lm(y~x,data=DATA)		Data frame linear model
# Summary(lm(y~x)		Produces more information about the model

#################################
# Chapter 8 - Regression Wisdom #
#################################

# myreg = lm(y~x) or myreg lm(y~x, data=DATA)		save regression moeld object
# plot(residuals(myreg)~predict(myreg))		Plots the residuals against the predicted values
# qqnorm(residuals(myreg))		Normal probability plot of residuals
# plot(myreg)				Similar plots

########################################
# Chapter 9 - Understanding Randomness #
########################################

# To generate k random integers between 1 and n:
# Sample(1:n,k,replace=T)
# Sample(1:n,k)				No repeats and k<=n
# Sample(c(0,1),k,replace=T)		Simulate Bernoulli Trials with p=0.5

###############################
# Chapter 10 - Sample Surveys #
###############################


#####################################################
# Chapter 11 - Experimental & Observational Studies #
#####################################################



###############################################
# Chapter 12 - From Randomness to Probability #
###############################################

# Probability of Event
probability.event = function(outcome,possible)
{
	probability=outcome/possible
	return(probability)
}
# Complement Rule
probability.complement=function(pOfA)
{
	complement=1-pOfA
	return(complement)
}
# Addition Rule
probability.addition=function(a,b)
{
	addition=a+b
	return(addition)
}
# Multiplication Rule
probability.multiplication=function(a,b)
{
	mult=a*b
	return(mult)
}

###################################
# Chapter 13 - Probability Rules! #
###################################

# General Addition Rule
probability.genAdd=function(a,b)
{
	#P(AorB)=P(A)+P(B)-P(AandB)
	genAdd=a+b-(a)
	## page 340
}


########################################################
# Chapter 14 - Random Variables and Probability Models #
########################################################



#############################################
# Chapter 15 - Sampling Distribution Models #
#############################################



#####################################################
# Chapter 16 - Confidence Intervals for Proportions #
#####################################################



#####################################################
# Chapter 17 - Testing Hypotheses About Proportions #
#####################################################



#######################################
# Chapter 18 - Inferences About Means #
#######################################



###############################################
# Chapter 19 - Nore About Tests and Intervals #
###############################################



#################################
# Chapter 20 - Comparing Groups #
#################################
# t.test(XY,alternative=c("two.sided","less","greater"),conf.level=0.95) 

# Standard Error
se=function(phat1,n1,phat2,n2)
{
	q1=1-phat1
	q2=1-phat2
	se=sqrt((phat1*q1)/n1 + (phat2*q2)/n2)
	return(se)
}
se.calcProportion=function(x1,n1,x2,n2)
{
	phat1=x1/n1
	phat2=x2/n2
	q1=1-phat1
	q2=1-phat2
	se=sqrt((phat1*q1)/n1 + (phat2*q2)/n2)
	return(se)
	
}

#Sample Assumptions and conditions
sampleAssumptionConditions=function()
{
	cat("\n1.The data in each group should be drawn at random from a homogeneous population or generated by a randomized comparative experiment.","\n 2.The sample should not exceed​ 10% of the population.", "\n 3.The two groups must be independent of each other.", "\n 4.Both groups must be big enough that at least 10 successes and at least 10 failures are observed in each. \n")
}
# confidence interval
confint=function(x,n,alpha)
{
	phat=x/n
	q=1-phat
	se=sqrt((phat*q)/n)
	zstar1=zstar(alpha)
	left=phat-zstar1*se
	right=phat+zstar1*se
	cat("(",left,",",right,")\n")
}
# confidence interval for difference in proportions
confint.diffproportions=function(x1,n1,x2,n2,alpha)
{
	phat1=x1/n1
	phat2=x2/n2
	q1=1-phat1
	q2=1-phat2
	se=sqrt((phat1*q1)/n1 + (phat2*q2)/n2)
	zstar1=zstar(alpha)
	diffphat=phat1-phat2
	zstar_se=zstar1*se
	cat("(",phat1," - ",phat2,") +-",zstar1,"*",se,"\n",diffphat," +- ",zstar_se,"\n")
	left=diffphat-zstar_se
	right=diffphat+zstar_se
	cat("(",left,",",right,")\n")
	
}
# pooled proportion
proportion.pooled=function(x1,n1,x2,n2)
{
	pooled=(x1+x2)/(n1+n2)
	return(pooled)
}
se.pooled=function(x1,n1,x2,n2)
{
	pooled=(x1+x2)/(n1+n2)
	qpooled=1-pooled
	qp=pooled*qpooled
	sepooled=sqrt(qp/n1 +qp/n2)
	return(sepooled)
}
testStat.pooled=function(x1,n1,x2,n2)
{
	pooled=(x1+x2)/(n1+n2)
	qpooled=1-pooled
	qp=pooled*qpooled
	sepooled=sqrt(qp/n1 +qp/n2)
	phat1=x1/n1
	phat2=x2/n2	
	z=((phat1-phat2)-0)/sepooled
	return(z)
}
#pvalue from Test statistic,z, for diff of two proportions
pvalT.proportion=function(z,npooled)
{
	pvalue=2*pt(abs(z),npooled-1,lower=FALSE)
	return(pvalue)
}
# standard deviation diff proportions
sd.diffproportion=function(p1,n1,p2,n2)
{
	q1=1-p1
	q2=1-p2	
	qp1=p1*q1
	qp2=p2*q2
	stdev=sqrt(qp1/n1 + qp2/n2)
	return(stdev)
}
#alphaLevel=function(conflevel)
#{
#	alphalevel=(100-conflevel)/2
#	return(alphalevel)
#}
wrongConclusionsError=function()
{
	cat("A Type I error occurs when the null hypothesis is​ true, but is mistakenly rejected. A Type II error occurs when the null hypothesis is​ false, but is not rejected.\n")
}
# two-sample t-test 
tTest.twosample=function(ybar1,n1,s1,ybar2,n2,s2,delta_o)
{
	seybardiff=sqrt((s1^2)/n1 + (s2^2)/n2)
	t=((ybar1-ybar2)-delta_o)/seybardiff
	cat("t value: ",t,"\n")
	
}
# find degrees of freedom
df.find=function(s1,n1,s2,n2)
{
	num=(s1^2)/n1 + (s2^2)/n2
	denom1=(1/(n1-1))*((s1^2)/n1)^2
	denom2=(1/(n2-1))*((s2^2)/n2)^2
	denom=denom1+denom2 	
	#df=((s1^2)/n1 + (s2^2)/n2)^2 /((1/n1-1)*((s1^2)/n1)^2 + (1/n2-1)*((s2^2)/n2)^2)
	df=(num^2)/denom
	return(df)
}
# P-value from t-value and df (multiply by 2? absolute value?)
pvalT=function(t,dfval,tail)
{
	ans=pt(t,dfval,lower=FALSE)
	return(ans)
}
# two-sampled t-interval for difference between means confidence interval
confint.diffmean=function(ybar1,n1,s1,ybar2,n2,s2,conf)
{
	b=sqrt((s1^2)/n1 + (s2^2)/n2)
	df=df.find(s1,n1,s2,n2)
	#t=((ybar1-ybar2))/b
	t=criticalTval(conf,df)
	left=(ybar1 - ybar2)-(t*b)
	right=(ybar1 - ybar2)+t*b
	cat("(",left,",",right,")\n")
	
}
se.diffmean=function(s1,n1,s2,n2)
{
	answ=sqrt((s1^2)/n1 + (s2^2)/n2)
	return(answ)
}
confint.fix=function(ybar1,n1,s1,ybar2,n2,s2,conf)
{
	se=se.diffmean(s1,n1,s2,n2)	
	df=df.find(s1,n1,s2,n2)
	t=criticalTval(conf,df)
	me=t*se
	difference=ybar1-ybar2
	left=difference-me
	right=difference+me
	cat("(",left,",",right,")\n")
}
##########################################
# Chapter 21 - Paired Samples and Blocks #
##########################################
# confidence interval paired samples
confint.paired=function(dbar,s_d,n,delta_0,conf)
{
	dfval=n-1
	se.dbar=s_d/sqrt(n)
	#t=(dbar-delta_0)-se.dbar
	t=abs(qt((1-conf)/2,df=dfval))	
	left=dbar-t*se.dbar
	right=dbar+t*se.dbar
	cat("(",left,",",right,")\n")
}


#################################
# Chapter 22 - Comparing Counts #
#################################

#chisq.test(x)		# x is observed values
# critical value from chisq and alpha and df
#qchisq(100-alpha,df)
# degrees of freedom from chart
df.chart=function(r,c)
{
	df=(r-1)*(c-1)
	return(df)
}
# smallest expected count
# rowtotal/total * column total
#a<-matrix(c(10,20,30,40,50,50),ncol=3,byrow=TRUE)
#> a
#     [,1] [,2] [,3]
#[1,]   10   20   30
#[2,]   40   50   50
# trials / outcomes
# import at x <- c(values)
# dataframe = data.frame(alphaprof,betaprof)
# results <- chisq.test(dframe)
# results$expected
#STANDARDIZE RESIDUAL
# c = (obs -exp)/sqrt(exp)

##########################################
# Chapter 23 - Inferences for Regression #
##########################################



######################################
# Chapter 24 - Analysis for Variance #
######################################



####################################
# Chapter 25 - Multiple Regression #
####################################



#########
# Other #
#########
# Critical valueZ*
zstar=function(conf)
{
	alpha=1-conf
	zstar= qnorm(1 - alpha/2)
	return(zstar)
}

criticalTval=function(conf,dfval)
{
	ans=qt((1-conf)/2,df=dfval)
	return(abs(ans))
}
#########################
# DATA ANALYSIS PROJECT #
#########################
# read data in and output number of occurences
occurencesData=function(filename,head)
{
	t1=read.table(file.choose(), header=head)
	return(table(t1))
}

proportion=function(x,n)
{
	# sample proportion is phat
	# x is No. of individuals
	# n is No. of individuals in sample
	phat=x/n
	return(phat)
}

proportionSigEv=function(x1,n1,x2,n2,alpha)
{
	# Provides information to find out if proportions differ at a given
	# alpha level of significance
	# alpha = level of significance
	# phat1 = population proportion 1
	# phat2 = population proportion 2
	# x is No. of individuals with respective sample number
	# n is No. of individuals in sample with respective sample number
	phat1=x1/n1
	phat2=x2/n2
	condition2check1=n1*phat1*(1-phat1)
	condition2check2=n2*phat2*(1-phat2)
	cat("Conditions")
	cat("\n Check that The samples are independently obtained using simple random sampling or through a randomized experiment.")
	cat("\n n1*phat1(1-phat1)>=10, n2*phat2(1-phat2)>=10 \n")
	cat("Sample size is no more than 5% of the population size; ensuring the independence necessary for a binomial experiment \n")
	cat("The null​ hypothesis, denoted H_0​, is a statement to be tested. The null hypothesis is a statement of no​ change, no​ effect, or no difference and is assumed true until evidence indicates otherwise. The alternative​ hypothesis, denoted H_1​, is a statement that the test is trying to find evidence to support. \n")
	phat=(x1+x2)/(n1+n2)
	z_0=(phat1-phat2)/(sqrt(phat*(1-phat))*sqrt((1/n1)+(1/n2)))
	# P-value from z_0
	dfvalue=n1+n2-1
	pvalue=pt(-abs(z_0),dfvalue)*2
	cat("\n----------------------------------------------")
	cat("\n phat1 = ", phat1,"\t phat2 = ", phat2, "\t phat = ", phat, "\n condition2check1 ", condition2check1, "\t condition2check2 ", condition2check2)
	cat("\n\n","z_0 = ", z_0, "\t P-value = ", pvalue)
	cat("\n","Is the P-value less that alpha, reject the null hyp \n Otherwise do not reject null hyp")

}

#Scatter plot
scatterData=function(head)
{
	#head = TRUE or FALSE depending on whether the data file has a header or not	
	tscatter=read.table(file.choose(), header=head, sep = ",")
	tscatData <<- tscatter	
	tscatX <<- tscatter$Weight 
	tscatY <<- tscatter$MilesperGallon
	plot(tscatX,tscatY)
	#correlationcoefficient=cor(x,y)
	#cat("r = ",correlationcoefficient)
	
}
scatCor=function(n)
{
	xmean=mean(tscatX)
	ymean=mean(tscatY)
	sdx=sd(tscatX)
	sdy=sd(tscatY)
	devfromX=tscatX-xmean
	devfromY=tscatY-ymean
	products=devfromX*devfromY
	su=sum(products)
	denom=(n-1)*sdx*sdy
	r=su/denom
	return(r)
}

###
# T test statistic
testStat=function(xbar,u,s,n)
{
	sexbar=s/sqrt(n)
	tn_one=(xbar-u)/sexbar
	pvalue=pt(tn_one,n-1,lower=FALSE)
	cat("t_0 = ",tn_one, "\t P-value = ", pvalue,"\nIf P-value < alpha, reject null hyp\n")
	
}

###
freqTable=function(width,ll,ul)
{
	incomeData <- read.table(file.choose(), header=FALSE, sep=" ")
	incomeData$V2 <- as.numeric(gsub(",", "", as.character(incomeData$V1)))
	incomeValues <- incomeData$V2	
	#ra=income$V1	
	#rangeOfData=range(ra)
	breaks = seq(ll,ul,by=width)
	income.cut = cut(incomeData$V2, breaks, right=FALSE)
	income.freq=table(income.cut)
	income.relfreq= income.freq / nrow(incomeData)	
	final=cbind(income.freq)
	final.relfreq=cbind(income.relfreq)	
	print(final)
	print(final.relfreq)
	hist(incomeData$V2)	
}

freqTable.hist=function(width,ll,ul)
{
	incomeData <- read.table(file.choose(), header=FALSE, sep=" ")
	incomeData$V2 <- as.numeric(gsub(",", "", as.character(incomeData$V1)))
	incomeValues <- incomeData$V2	
	#ra=income$V1	
	#rangeOfData=range(ra)
	breaks = seq(ll,ul,by=width)
	income.cut = cut(incomeData$V2, breaks, right=FALSE)
	income.freq=table(income.cut)
	income.relfreq= income.freq / nrow(incomeData)	
	final=cbind(income.freq)
	final.relfreq=cbind(income.relfreq)	
	#print(final)
	#print(final.relfreq)
	hist(incomeData$V2)
	
}
freqTable.relhist=function()
{
	incomeData <- read.table(file.choose(), header=FALSE, sep=" ")
	incomeData$V2 <- as.numeric(gsub(",", "", as.character(incomeData$V1)))
	incomeValues <- incomeData$V2	
	hist(incomeData$V2, freq=FALSE, right=FALSE)
}

###

dataSummary=function()
{
	table4 <- read.table(file.choose(),header=FALSE)
	out1=summary(table4)
	#mode=function(x) {
	#	ux <- unique(x)
	#	ux[which.max(tabulate(match(x, ux)))]
	#}
	out2=names(sort(-table(x)))[1]
	print(out1)
	cat("\n Mode: ",out2,"\n")
}
dataTransform=function(x,operation)
{
	table4 <- read.table(file.choose(),header=FALSE)
	table4$V2 <- table4$V1(operation(x))	
	out1=summary(table4)
	out2=names(sort(-table(x)))[1]
	print(out1)
	cat("\n Mode: ",out2)
}
