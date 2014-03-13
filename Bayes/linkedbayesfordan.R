rm(list=ls(all=TRUE)) 
graphics.off()
windows(record=T)
setwd ("\\\\CCFHR-S-1534090\\popdyn1\\SEDAR\\SEDAR 41\\SEFISIndices\\S41RGIT")
library(MASS)
library(doBy)
library(statmod)
library(Hmisc)
library(pscl)
library(rjags)
# Read in Red Snapper Data
rs=read.csv("\\\\CCFHR-S-1534090\\popdyn1\\SEDAR\\SEDAR 41\\SEFISIndices\\ConfData\\RedSnapGLMData.csv")#;head(rs)

# Full data subsetting: 
rs <- rs[rs$Station_Type!="Recon",]                # remove recon stations
rs <- rs[rs$A.Video.Readable == "Yes",]         # remove invalid videos
rs <- subset(rs, rs$Start_Depth > 0)                  # remove NA in depth
rs <- subset(rs, rs$Start_Depth < 100)              # remove < 100 m deep
rs <- subset(rs, rs$LastOfTemp > 0)                 # remove blank water temps
rs <- subset(rs, rs$Turbidity != "Unknown")    # remove unknown turbidity values
rs <- subset(rs, rs$No.Readable.Frames ==41)    # remove zero readable frames

#Eliminate unnecessary columns
dat=subset(rs,select=c(MC_Lutjanus.campechanus,No.Readable.Frames,Year,Turbidity,Current_Direction,Current_Magnitude,Substrate_Cat,Relief,Size,Biotic_Density_Cat,Biotic_Type,Biotic_Height,Start_Depth,Julian,Start_Latitude,LastOfTemp,TOD))
orgnames=names(dat)

sumcount=round(dat$MC_Lutjanus.campechanus*rs$No.Readable.Frames,0)
dat$MC_Lutjanus.campechanus=sumcount

#rename to short names
names(dat)=c('fishseen','frames','y','wc','cd','cm','sc','sr','ss','bd','bt','bh','d','t','lat','temp','tod')#;head(dat)

#replace NA in the cpue with 0
dat$fishseen[is.na(dat$fishseen)]=0

#rescale frames
#dat$frames=dat$frames/41
#dat$frames=log(dat$frames)
#now categorize the continuous variables
#depth
hist(dat$d,breaks=seq(10,110,by=5))
summary(dat$d)
#temp=cut(dat$d,breaks=c(14,25,41,52,115),labels=FALSE)#;temp;table(temp)
#temp=cut(dat$d,2,breaks=c(0,as.numeric(summary(dat$d))[-c(1,4)]),labels=FALSE);temp;table(temp)
temp=cut(dat$d,2,breaks=quantile(dat$d),labels=FALSE)#;temp;table(temp)
dat$d=temp

#latitude
hist(dat$lat,breaks=seq(27,36,by=0.25))
summary(dat$lat)
#temp=cut(dat$lat,breaks=c(27,29.75,31.25,32.75,34,35.25),labels=FALSE)#;temp;table(temp)
#temp=cut(dat$lat,2,breaks=c(0,as.numeric(summary(dat$lat))[-c(1,4)]),labels=FALSE);temp;table(temp)
temp=cut(dat$lat,2,breaks=quantile(dat$lat),labels=FALSE)#;temp;table(temp)
dat$lat=temp

#day of year (t)
hist(dat$t,breaks=seq(110,305,by=5))
summary(dat$t)
#temp=cut(dat$t,breaks=c(##,##,##,##,##,##),labels=FALSE)#;temp;table(temp)
#temp=cut(dat$t,2,breaks=c(0,as.numeric(summary(dat$t))[-c(1,4)]),labels=FALSE);temp;table(temp)
temp=cut(dat$t,2,breaks=quantile(dat$t),labels=FALSE)#;temp;table(temp)
dat$t=temp

#water temperature (temp)
hist(dat$temp,breaks=seq(12.25,29.25,by=0.25))
summary(dat$temp)
#temp=cut(dat$t,breaks=c(##,##,##,##,##,##),labels=FALSE)#;temp;table(temp)
#temp=cut(dat$temp,2,breaks=c(0,as.numeric(summary(dat$temp))[-c(1,4)]),labels=FALSE);temp;table(temp)
temp=cut(dat$temp,2,breaks=quantile(dat$temp),labels=FALSE)#;temp;table(temp)
dat$temp=temp

#time of day (tod)
hist(dat$tod,breaks=seq(0.4,0.95,by=0.025))
summary(dat$tod)
#temp=cut(dat$tod,breaks=c(##,##,##,##,##,##),labels=FALSE)#;temp;table(temp)
#temp=cut(dat$tod,2,breaks=c(0,as.numeric(summary(dat$tod))[-c(1,4)]),labels=FALSE);temp;table(temp)
temp=cut(dat$tod,2,breaks=quantile(dat$tod),labels=FALSE)#;temp;table(temp)
dat$tod=temp

#Factorize variables
dat$y=factor(dat$y)
dat$d=factor(dat$d)
dat$lat=factor(dat$lat)
dat$t=factor(dat$t)
dat$temp=factor(dat$temp)
dat$tod=factor(dat$tod)

#get rid of unused factors for dat$wc
dat$wc=factor(dat$wc)

plot.design(fishseen~y + wc + cd + cm ,data=dat)
plot.design(fishseen~y + wc + sc + sr + ss ,data=dat)
plot.design(fishseen~y + wc + bd + bt + bh ,data=dat)
plot.design(fishseen~y + wc +  d + t + lat + temp + tod,data=dat)
plot.design(fishseen~y + wc + cd + cm + sc + sr + ss + bd + bt + bh + d + t + lat + temp + tod,data=dat)


#**So this bit allows variable selection of each of the model types**


modelFilename = "BayesZip"
cat("
    model {
    #First set up the prior Weight Parameters
    
    for(i in 1:wgts){
    w[i] ~ dbern(.5)
    }
    
    
    #Priors parameters
    
    # For the non zero infl part
    
    for(i in 1:1){
    alph[i] ~ dt(0,pow(1.566267,-2),7.63179)#dnorm(0,.1)
    }
    
    # For the non zero infl part
    for(i in 1:28){
    bet[i] ~ dt(0,pow(1.566267,-2),7.63179)
    }
    
    
    #model
    for (i in 1:samp){
    
    
    
    log.ypred[i] <- bet[1]+
    w[1]*(bet[2]*y2011[i]+bet[3]*y2012[i])+
    w[2]*(bet[4]*wc.1[i]+bet[5]*wc.2[i])+
    w[3]*(bet[6]*cd.s[i]+bet[7]*cd.t[i]+bet[8]*cd.u[i])+
    w[4]*(bet[9]*sc.l[i]+bet[10]*sc.m[i]+bet[11]*sc.n[i]+bet[12]*sc.u[i])+
    w[5]*(bet[13]*bd.l[i]+bet[14]*bd.m[i]+bet[15]*bd.n[i]+bet[16]*bd.u[i])+
    w[6]*(bet[17]*d.2[i]+bet[18]*d.3[i]+bet[19]*d.4[i])+
    w[7]*(bet[20]*t.2[i]+bet[21]*t.3[i]+bet[22]*t.4[i])+
    w[8]*(bet[23]*lat.2[i]+bet[24]*lat.3[i]+bet[25]*lat.4[i])+
    w[9]*(bet[26]*temp.2[i]+bet[27]*temp.3[i]+bet[28]*temp.4[i])
    
    
    
    logit.psi[i] <- alph[1]
    
    #Use This bit for the psi-lambda link
    #logit.psi[i] <- alph[1]+w[10]*alph[2]*log.ypred[i]
    
    
    ###Use this bit to have the global model for zero inflation
    #w[10]*(alph[2]*y2011[i]+alph[3]*y2012[i])+
    #w[11]*(alph[4]*wc.1[i]+alph[5]*wc.2[i])+
    #w[12]*(alph[6]*cd.s[i]+alph[7]*cd.t[i]+alph[8]*cd.u[i])+
    #w[13]*(alph[9]*sc.l[i]+alph[10]*sc.m[i]+alph[11]*sc.n[i]+alph[12]*sc.u[i])+
    #w[14]*(alph[13]*bd.l[i]+alph[14]*bd.m[i]+alph[15]*bd.n[i]+alph[16]*bd.u[i])+
    #w[15]*(alph[17]*d.2[i]+alph[18]*d.3[i]+alph[19]*d.4[i])+
    #w[16]*(alph[20]*t.2[i]+alph[21]*t.3[i]+alph[22]*t.4[i])+
    #w[17]*(alph[23]*lat.2[i]+alph[24]*lat.3[i]+alph[25]*lat.4[i])+
    #w[18]*(alph[26]*temp.2[i]+alph[27]*temp.3[i]+alph[28]*temp.4[i])
    
    
    
    
    psi[i]<-1/(1+exp(-logit.psi[i]))
    ypred[i]<-exp(log.ypred[i])
    z[i] ~ dbern(psi[i])
    lambda[i] <- z[i]*ypred[i]
    fishseen[i] ~ dpois(lambda[i])
    }
    }
    
    ", fill=TRUE, file=modelFilename)


#use this line to make a covariate with random values
#dat$d=sample(c("1","2","3","4"),dim(dat)[1],replace=T)
#################################


jags.data = list(fishseen=dat$fishseen,y2011=as.numeric(dat$y=='2011'), y2012=as.numeric(dat$y=='2012'),
                 wc.1=as.numeric(dat$wc==1),wc.2=as.numeric(dat$wc==2),
                 cd.s=as.numeric(dat$cd=="Sideways"),cd.t=as.numeric(dat$cd=="Towards"),cd.u=as.numeric(dat$cd=="Unknown"),
                 sc.l=as.numeric(dat$sc=="Low"),sc.m=as.numeric(dat$sc=="Moderate"),sc.n=as.numeric(dat$sc=="None"),sc.u=as.numeric(dat$sc=="Unknown"),
                 bd.l=as.numeric(dat$bd=="Low"),bd.m=as.numeric(dat$bd=="Moderate"),bd.n=as.numeric(dat$bd=="None"),bd.u=as.numeric(dat$bd=="Unknown"),
                 d.2=as.numeric(dat$d=="2"),d.3=as.numeric(dat$d=="3"),d.4=as.numeric(dat$d=="4"),
                 t.2=as.numeric(dat$t=="2"),t.3=as.numeric(dat$t=="3"),t.4=as.numeric(dat$t=="4"),
                 lat.2=as.numeric(dat$lat=="2"),lat.3=as.numeric(dat$lat=="3"),lat.4=as.numeric(dat$lat=="4"),
                 temp.2=as.numeric(dat$temp=="2"),temp.3=as.numeric(dat$temp=="3"),temp.4=as.numeric(dat$temp=="4"),
                 wgts=9,
                 samp=dim(dat)[1])

jags.inits = function(){list(bet = rnorm(28,0,.15),alph=rnorm(1,0,.15))}
jags.parms = c('w','bet','alph')

jinits=jags.inits()
jmod = jags.model(file=modelFilename, data=jags.data, inits = jags.inits(),n.chains=3, n.adapt=100)
#jmod = jags.model(file=modelFilename, data=jags.data,n.chains=3, n.adapt=100)   #inits = jags.inits(),
update(jmod, n.iter=100, by=10, progress.bar='text')
post = coda.samples(jmod, jags.parms, n.iter=5000, thin=10)
#dicsam=dic.samples(jmod, 1000, "popt")
mypost = as.matrix(post, chain=F)
plot(post)
summary(post)

