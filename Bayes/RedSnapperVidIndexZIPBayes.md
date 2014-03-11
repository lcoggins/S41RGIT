
Red Snapper Video Index Using Delta GLM
========






![plot of chunk ConditionData](figure/ConditionData1.png) 

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    15.0    28.0    38.0    40.7    52.0    98.0
```

![plot of chunk ConditionData](figure/ConditionData2.png) 

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    27.2    29.7    31.3    31.3    32.8    35.0
```

![plot of chunk ConditionData](figure/ConditionData3.png) 

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     115     171     209     205     235     300
```

![plot of chunk ConditionData](figure/ConditionData4.png) 

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    12.4    19.6    22.2    22.1    24.8    29.1
```

![plot of chunk ConditionData](figure/ConditionData5.png) 

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.443   0.522   0.647   0.658   0.766   0.924
```

![plot of chunk ConditionData](figure/ConditionData6.png) ![plot of chunk ConditionData](figure/ConditionData7.png) ![plot of chunk ConditionData](figure/ConditionData8.png) ![plot of chunk ConditionData](figure/ConditionData9.png) ![plot of chunk ConditionData](figure/ConditionData10.png) 


**So this bit allows variable selection of each of the model types**

```r


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
    for(i in 1:45){
      bet[i] ~ dt(0,pow(1.566267,-2),7.63179)
    }


#model
   for (i in 1:samp){
    logit.psi[i] <- alph[1]




#      log.ypred[i] <- bet[1]+
# w[1]*(bet[2]*y2011[i]+bet[3]*y2012[i])+
# w[2]*(bet[4]*wc.1[i]+bet[5]*wc.2[i])+
# w[3]*(bet[6]*cd.s[i]+bet[7]*cd.t[i]+bet[8]*cd.u[i])+
# w[4]*(bet[9]*cm.s[i]+bet[10]*cm.u[i])+
# w[5]*(bet[11]*sc.l[i]+bet[12]*sc.m[i]+bet[13]*sc.n[i]+bet[14]*sc.u[i])+
# w[6]*(bet[15]*sr.l[i]+bet[16]*sr.m[i]+bet[17]*sr.u[i])+
# w[7]*(bet[18]*ss.cont[i]+bet[19]*ss.na[i]+bet[20]*ss.u[i])+
# w[8]*(bet[21]*bd.l[i]+bet[22]*bd.m[i]+bet[23]*bd.n[i]+bet[24]*bd.u[i])+
# w[9]*(bet[25]*bt.na[i]+bet[26]*bt.o[i]+bet[27]*bt.u[i])+
# w[10]*(bet[28]*bh.l[i]+bet[29]*bh.na[i]+bet[30]*bh.u[i])+
# w[11]*(bet[31]*d.2[i]+bet[32]*d.3[i]+bet[33]*d.4[i])+
# w[12]*(bet[34]*t.2[i]+bet[35]*t.3[i]+bet[36]*t.4[i])+
# w[13]*(bet[37]*lat.2[i]+bet[38]*lat.3[i]+bet[39]*lat.4[i])+
# w[14]*(bet[40]*temp.2[i]+bet[41]*temp.3[i]+bet[42]*temp.4[i])+
# w[15]*(bet[43]*tod.2[i]+bet[44]*tod.3[i]+bet[45]*tod.4[i])

     log.ypred[i] <- bet[1]+
w[1]*(bet[2]*y2011[i]+bet[3]*y2012[i])+
w[2]*(bet[4]*wc.1[i]+bet[5]*wc.2[i])+
w[3]*(bet[6]*cd.s[i]+bet[7]*cd.t[i]+bet[8]*cd.u[i])+
#w[4]*(bet[9]*cm.s[i]+bet[10]*cm.u[i])+
w[5]*(bet[9]*sc.l[i]+bet[10]*sc.m[i]+bet[11]*sc.n[i]+bet[12]*sc.u[i])+
w[6]*(bet[13]*sr.l[i]+bet[14]*sr.m[i]+bet[15]*sr.u[i])+
w[7]*(bet[16]*ss.cont[i]+bet[17]*ss.na[i]+bet[18]*ss.u[i])+
w[8]*(bet[19]*bd.l[i]+bet[20]*bd.m[i]+bet[21]*bd.n[i]+bet[22]*bd.u[i])+
w[9]*(bet[23]*bt.na[i]+bet[24]*bt.o[i]+bet[25]*bt.u[i])+
w[10]*(bet[26]*bh.l[i]+bet[27]*bh.na[i]+bet[28]*bh.u[i])+
w[11]*(bet[29]*d.2[i]+bet[30]*d.3[i]+bet[31]*d.4[i])+
w[12]*(bet[32]*t.2[i]+bet[33]*t.3[i]+bet[34]*t.4[i])+
w[13]*(bet[35]*lat.2[i]+bet[36]*lat.3[i]+bet[37]*lat.4[i])+
w[14]*(bet[38]*temp.2[i]+bet[39]*temp.3[i]+bet[40]*temp.4[i])+
w[15]*(bet[41]*tod.2[i]+bet[42]*tod.3[i]+bet[43]*tod.4[i])



   ypred[i]<-exp(log.ypred[i])
   z[i] ~ dbern(1/(1+exp(-logit.psi[i])))
   lambda[i] <- z[i]*ypred[i]
   fishseen[i] ~ dpois(lambda[i])
           }
   }
             
", fill=TRUE, file=modelFilename)

jags.data = list(fishseen=dat$fishseen,y2011=as.numeric(dat$y=='2011'), y2012=as.numeric(dat$y=='2012'),
                 wc.1=as.numeric(dat$wc==1),wc.2=as.numeric(dat$wc==2),
                 cd.s=as.numeric(dat$cd=="Sideways"),cd.t=as.numeric(dat$cd=="Towards"),cd.u=as.numeric(dat$cd=="Unknown"),
                 #cm.s=as.numeric(dat$cm=="Strong"),cm.u=as.numeric(dat$cm=="Unknown"),
                 sc.l=as.numeric(dat$sc=="Low"),sc.m=as.numeric(dat$sc=="Moderate"),sc.n=as.numeric(dat$sc=="None"),sc.u=as.numeric(dat$sc=="Unknown"),
                 sr.l=as.numeric(dat$sr=="Low"),sr.m=as.numeric(dat$sr=="Moderate"),sr.u=as.numeric(dat$sr=="Unknown"),
                 ss.cont=as.numeric(dat$ss=="Continuous"),ss.na=as.numeric(dat$ss=="N/A"),ss.u=as.numeric(dat$ss=="Unknown"),
                 bd.l=as.numeric(dat$bd=="Low"),bd.m=as.numeric(dat$bd=="Moderate"),bd.n=as.numeric(dat$bd=="None"),bd.u=as.numeric(dat$bd=="Unknown"),
                 bt.na=as.numeric(dat$bt=="N/A"), bt.o=as.numeric(dat$bt=="Other"), bt.u=as.numeric(dat$bt=="Unknown"),
                 bh.l=as.numeric(dat$bh=="Low"), bh.na=as.numeric(dat$bh=="N/A"), bh.u=as.numeric(dat$bh=="Unknown"),
                 d.2=as.numeric(dat$d=="2"),d.3=as.numeric(dat$d=="3"),d.4=as.numeric(dat$d=="4"),
                 t.2=as.numeric(dat$t=="2"),t.3=as.numeric(dat$t=="3"),t.4=as.numeric(dat$t=="4"),
                 lat.2=as.numeric(dat$lat=="2"),lat.3=as.numeric(dat$lat=="3"),lat.4=as.numeric(dat$lat=="4"),
                 temp.2=as.numeric(dat$temp=="2"),temp.3=as.numeric(dat$temp=="3"),temp.4=as.numeric(dat$temp=="4"),
                 tod.2=as.numeric(dat$tod=="2"),tod.3=as.numeric(dat$tod=="3"),tod.4=as.numeric(dat$tod=="4"),
                 wgts=14,
                 samp=dim(dat)[1])

jags.inits = function(){list(bet = rnorm(43,0,.15),alph=rnorm(1,0,.15))}
jags.parms = c('w','bet','alph')

jinits=jags.inits()
jmod = jags.model(file=modelFilename, data=jags.data, inits = jags.inits(),n.chains=3, n.adapt=100)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Deleting model
```

```
## Error: RUNTIME ERROR:
## Compilation error on line 64.
## Subset out of range: w[15]
```

```r
#jmod = jags.model(file=modelFilename, data=jags.data,n.chains=3, n.adapt=100)   #inits = jags.inits(),
update(jmod, n.iter=100, by=10, progress.bar='text')
```

```
## Error: object 'jmod' not found
```

```r
post = coda.samples(jmod, jags.parms, n.iter=1000, thin=10)
```

```
## Error: object 'jmod' not found
```

```r
#dicsam=dic.samples(jmod, 1000, "popt")
#mypost = as.matrix(post, chain=F)
#plot(post)
#summary(post)

```



```r
# zipform=formula(fishseen~ y + wc + cd + cm + sc + sr + ss + bd + bt + bh +
# d + t + lat + temp + tod +offset(frames)) zipmod=zeroinfl(zipform, dist =
# 'poisson', link = 'logit',data=dat);summary(zipmod) ###Remove biotic
# variables zipform=formula(fishseen~ y + wc + cd + cm + sc + sr + ss + bt +
# bh + d + t + lat + temp + tod +offset(frames)) zipmod=zeroinfl(zipform,
# dist = 'poisson', link = 'logit',data=dat);summary(zipmod)
# zipform=formula(fishseen~ y + wc + cd + cm + sc + sr + ss + bh + d + t +
# lat + temp + tod +offset(frames)) zipmod=zeroinfl(zipform, dist =
# 'poisson', link = 'logit',data=dat);summary(zipmod)
# zipform=formula(fishseen~ y + wc + cd + cm + sc + sr + ss + d + t + lat +
# temp + tod +offset(frames)) zipmod=zeroinfl(zipform, dist = 'poisson',
# link = 'logit',data=dat);summary(zipmod) #### Remove Sebstrate type
# zipform=formula(fishseen~ y + wc + cd + cm + sr + ss + bd + bt + bh + d +
# t + lat + temp + tod +offset(frames)) zipmod=zeroinfl(zipform, dist =
# 'poisson', link = 'logit',data=dat);summary(zipmod)
# zipform=formula(fishseen~ y + wc + cd + cm + ss + bd + bt + bh + d + t +
# lat + temp + tod +offset(frames)) zipmod=zeroinfl(zipform, dist =
# 'poisson', link = 'logit',data=dat);summary(zipmod)
# zipform=formula(fishseen~ y + wc + cd + cm + bd + bt + bh + d + t + lat +
# temp + tod +offset(frames)) zipmod=zeroinfl(zipform, dist = 'poisson',
# link = 'logit',data=dat);summary(zipmod) zipform=formula(fishseen~ y + wc
# + cd + cm + sc + ss + bd + bt + bh + d + t + lat + temp + tod
# +offset(frames)) zipmod=zeroinfl(zipform, dist = 'poisson', link =
# 'logit',data=dat);summary(zipmod) zipform=formula(fishseen~ y + wc + cd +
# cm + sc + bd + bt + bh + d + t + lat + temp + tod +offset(frames))
# zipmod=zeroinfl(zipform, dist = 'poisson', link =
# 'logit',data=dat);summary(zipmod) zipform=formula(fishseen~ y + wc + cd +
# cm + sc + sr + bd + bt + bh + d + t + lat + temp + tod +offset(frames))
# zipmod=zeroinfl(zipform, dist = 'poisson', link =
# 'logit',data=dat);summary(zipmod) zipform=formula(fishseen~ y + wc + cd +
# cm + sr + bd + bt + bh + d + t + lat + temp + tod +offset(frames))
# zipmod=zeroinfl(zipform, dist = 'poisson', link =
# 'logit',data=dat);summary(zipmod) summary(zipmod) #glm.step <-
# stepAIC(zipmod,direction='backward') windows(width=8,height=6,record=T)
# resids=residuals(zipmod)
# cbind(fitted(zipmod),dat$fishseen,fitted(zipmod)-dat$fishseen,resids)
# plot(fitted(zipmod),resids) qr.zipmod=qres.pois(zipmod)
# qqnorm(qr.zipmod,ylim=c(-4,10),main='QQplot residuals (qr.zipmod)',
# cex=0.5) qqline(qr.zipmod) #qqnorm(resids,ylim=c(-4,10),main='QQplot
# residuals ', cex=0.5) #qqline(resids)
# plot(dat$y,resids,xlab='Year',main='Residuals (zipmod)')
# plot(dat$wc,resids,xlab='Water Clarity',main='Residuals (zipmod)')
# plot(dat$cd,resids,xlab='Current Direction',main='Residuals (zipmod)')
# plot(dat$bd,resids,xlab='Biotic Diversity',main='Residuals (zipmod)')
# plot(dat$bt,resids,xlab='Biotic Type',main='Residuals (zipmod)')
# plot(dat$bh,resids,xlab='Biotic Height',main='Residuals (zipmod)')
# plot(dat$lat,resids,xlab='Latitude',main='Residuals (zipmod)')
# hist(dat$fishseen,breaks=0:max(dat$fishseen),freq=T,right=TRUE,xlab='Aggregate
# Fish Counted', main='ZIP')
# d=hist(predict(zipmod),breaks=0:max(dat$fishseen),plot=FALSE)
# lines(seq(0.5,max(dat$fishseen),by=1),d$counts, col='blue',type='l')
# hist(dat$fishseen,breaks=0:max(dat$fishseen),freq=T,right=TRUE,xlab='Aggregate
# Fish Counted', main='ZIP',ylim=c(0,50))
# lines(seq(0.5,max(dat$fishseen),by=1),d$counts, col='blue',type='b')


# new.dat=expand.grid(y=levels(dat$y), wc=levels(dat$wc), cm=levels(dat$cm),
# cd=levels(dat$cd), #sc=levels(dat$sc), #bd=levels(dat$bd),
# bt=levels(dat$bt), #bh=levels(dat$bh), d=levels(dat$d), t=levels(dat$t),
# lat=levels(dat$lat), temp=levels(dat$temp), tod=levels(dat$tod), frames=1)
# new.dat=cbind(new.dat,predict(zipmod,new.dat))
# names(new.dat)[dim(new.dat)[2]]='Predicted'
# resvec=summaryBy(Predicted~y,data=new.dat,FUN=mean)[,2] index =
# resvec/mean(resvec) plot(index,type='b')


# Now plot the simplified model zipmod=glm.step resids=residuals(zipmod)
# plot(fitted(zipmod),resids) qr.zipmod=qres.pois(zipmod)
# qqnorm(qr.zipmod,ylim=c(-4,10),main='QQplot residuals (qr.zipmod)',
# cex=0.5) qqline(qr.zipmod) plot(dat$y,resids,xlab='Year',main='Residuals
# (zipmod)') plot(dat$wc,resids,xlab='Water Clarity',main='Residuals
# (zipmod)') plot(dat$cd,resids,xlab='Current Direction',main='Residuals
# (zipmod)') plot(dat$bd,resids,xlab='Biotic Diversity',main='Residuals
# (zipmod)') plot(dat$bt,resids,xlab='Biotic Type',main='Residuals
# (zipmod)') plot(dat$bh,resids,xlab='Biotic Height',main='Residuals
# (zipmod)') plot(dat$lat,resids,xlab='Latitude',main='Residuals (zipmod)')
# hist(dat$fishseen,breaks=0:39,freq=T,right=TRUE,xlab='Aggregate Fish
# Counted', main='ZIP') d=hist(predict(zipmod),breaks=0:39,plot=FALSE)
# lines(seq(0.5,38.5,by=1),d$counts, col='blue',type='l')
# hist(dat$fishseen,breaks=0:39,freq=T,right=TRUE,xlab='Aggregate Fish
# Counted', main='ZIP',ylim=c(0,50)) lines(seq(0.5,38.5,by=1),d$counts,
# col='blue',type='b')


# new.dat=expand.grid(y=levels(dat$y), wc=levels(dat$wc), cm=levels(dat$cm),
# #cd=levels(dat$cd), #sc=levels(dat$sc), #bd=levels(dat$bd),
# bt=levels(dat$bt), #bh=levels(dat$bh), d=levels(dat$d), t=levels(dat$t),
# lat=levels(dat$lat), temp=levels(dat$temp), tod=levels(dat$tod), frames=1)
# new.dat=cbind(new.dat,predict(zipmod,new.dat))
# names(new.dat)[dim(new.dat)[2]]='Predicted'
# resvec=summaryBy(Predicted~y,data=new.dat,FUN=mean)[,2] index =
# resvec/mean(resvec) plot(index,type='b')

```



```r
# library(glmmADMB) admbzip=glmmadmb(fishseen~ y + wc + cd + cm + sr + bt +
# d + t + lat + temp + tod +offset(frames), data=dat, zeroInflation=TRUE,
# family='poisson');admbzip resids=residuals(admbzip)
# preds=(fitted(admbzip))
# cbind((predict(admbzip)),dat$fishseen,exp(predict(admbzip))-dat$fishseen,resids)
# plot(fitted(admbzip),resids) qr.admbzip=qres.pois(admbzip)
# qqnorm(resids,ylim=c(-4,10),main='QQplot residuals (qr.admbzip)', cex=0.5)
# qqline(resids)
# hist(dat$fishseen,breaks=0:max(dat$fishseen),freq=T,right=TRUE,xlab='Aggregate
# Fish Counted', main='ZIP')
# d=hist(preds,breaks=0:max(dat$fishseen),plot=FALSE)
# lines(seq(0.5,max(dat$fishseen),by=1),d$counts, col='blue',type='l')
# hist(dat$fishseen,breaks=0:max(dat$fishseen),freq=T,right=TRUE,xlab='Aggregate
# Fish Counted', main='ZIP',ylim=c(0,500))
# lines(seq(0.5,max(dat$fishseen),by=1),d$counts, col='blue',type='b')
# library(coefplot2) coefplot2(admbzip)

```


