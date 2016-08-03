# Created By Austin Clark
# Github repository: https://github.com/AustinRClark/IntroStatsR
# Made with the help from these resources: 
# Intro Stats Fourth Edition De Veaux Velleman Bock 2014 ISBN 978-0-321-82527-8
# Stackoverflow & other free R resources online
# DISCLAIMER: I do not take credit for the information in this library, I only
# put all of it together so other students could effectively learn and use the
# statistical programming language R in their introductory statistics class.
# 2016.07.20-161401 ARC
# This is for Statistics 211 Online Summer 2016
# While in R, just preform the command: source("libstats211.R") 
# in the same directory that the library is located in.

pconfint=function( pht, n, conf)
	{
	  se=sqrt(pht*(1-pht)/n)
	  al2=1-(1-conf)/2
	  zstar=qnorm(al2)
	  ul=pht+zstar*se
	  ll=pht-(zstar*se)
	  return(c(ll,ul))
	}

zstar=function(conf)
{
	alpha=1-conf
	zstar= qnorm(1 - alpha/2)
	return(zstar)
}
conflevel=function(phat, n, me)
{
	zstar=me/se(phat,n)
	return(1-2*(1-pnorm(zstar)))
}
pvalue=function(zvalue)
{
	pvalue=2*pnorm(-abs(zvalue))
	return(pvalue)
}
zvalue=function(phat,po,n)
{
	qo=1-po
	zvalue=(phat-po)/sqrt((po*qo)/n)
	return(zvalue)
}

me=function(phat, n, conf)
	{
	 qhat=1-phat
	 se=sqrt((phat*qhat)/n)
	  
	 ans=zstar(conf)*se
	 return(ans)
	}
se=function(phat,n)
{
	qhat=1-phat
	se=sqrt((phat*qhat)/n)
	return(se)
}

reqSampleSize=function(phat,me,conf)
{
	qhat=1-phat
	ans=((zstar(conf)*sqrt(phat*qhat))/me)^2
	return(ans)
}
ztest=function(phat, po, n, sdphat)
{
	z=(phat-po)/sdphat(po,n)
	return(z)
}
sdphat=function(po, n)
{
	qo=1-po
	sdphat=sqrt((po*qo)/n)
	return(sdphat)
}

critical.t=function(){
    cat("\n","\bEnter Alpha Level","\n")
        alpha<-scan(n=1,what = double(0),quiet=T)
    cat("\n","\b1 Tailed or 2 Tailed:\nEnter either 1 or 2","\n")
        tt<-scan(n=1,what = double(0),quiet=T)
    cat("\n","\bEnter Number of Observations","\n")
        n<-scan(n=1,what = double(0),quiet=T)
    cat("\n\nCritical Value =",qt(1-(alpha/tt), n-2), "\n")
}
t.alpha=function(alpha,n)
{
	ans=qt(1-alpha, df=n-1)
	return(ans)
}
criticalTval=function(conf,dfval)
{
	ans=qt((1-conf)/2,df=dfval)
	return(abs(ans))
}
pvalT=function(t,dfval,tail)
{
	ans=pt(t,dfval,lower=FALSE)
	return(ans)
}
nconfint=function(sampMean,sampStDev,sampsize,conf)
{
	t=(average-popmean)/seyhat
	seyhat=0
	criticalTval=qt((1-conf)/2,df=dfval)
	me=criticalTval*seyhat
}
confintTdist=function(a,s,n,confint,dfval)
{
	standarderror=s/sqrt(n) 
	#error= qt(confint,df=n)*s/sqrt(n)
	#percentTail=(100-(confint*100))/2
	tval=abs(qt((1-confint)/2,df=dfval))
        #return(abs(ans))
	me=tval*standarderror 
 	left= a-me
	 right=a+me
	# ans=left cat(" ") right
	 print(left);print(right)
	 
}
sampleSize18=function(s,n,me,confint)
{
	dfval=n-1
	tval=abs(qt((1-confint)/2,df=dfval))
	#me=tval*(s/sqrt(n))
	n_new=(tval*(s/me))^2
	return(n_new)
}
testStat18=function(ybar,u,s,n)
{
	seybar=s/sqrt(n)
	tn_one=(ybar-u)/seybar
	pvalue=2*pt(tn_one,n-1,lower=FALSE)
	print(tn_one)
	print(pvalue)

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
	pvalue=pt(z_0,dfvalue)*2
	cat("\n----------------------------------------------")
	cat("\n phat1 = ", phat1,"\t phat2 = ", phat2, "\n condition2check1 ", condition2check1, "\t condition2check2 ", condition2check2)
	cat("\n\n","z_0 = ", z_0, "\t P-value = ", pvalue)
	cat("\n","Is the P-value greater that alpha, reject the null hyp \n Otherwise do not reject null hyp")

}
