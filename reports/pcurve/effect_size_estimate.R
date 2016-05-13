
#R Code behind Figure 5C&D in -  MANY LABS REPLICATION PROJECT
#Simonsohn, Simmons and Nelson,  "P-Curve and Effect Size: Correcting for Publication Bias Using Only Significant Results"
#
#Prepared by Uri Simonsohn, uws@wharton.upenn.edu
#
#Last updated: 2014 05 13
##########################################################



#This program uses data collected by the 36 labs participating in the Many Labs replication project
#see Klein et al (2014) https://osf.io/wx7ck/

#1) It loads the data for two of the studies: sunk cost fallacy and asian disease.

#2) It drops the two largest studies, with about 1000 subjects each, so that there will be some effect of selective
#reporting (otherwise those two studies drive the effect size estimate. p-curve still does very well, almost exactly 
#estimating effect size from significant studies alone, but, the naive estimate is not too biased either, so the
#example is not partiularly informative.

#3) Computes a meta-analytical effect size based on all studies (earnest estimate) and based only on the significant ones (naive)
#   Using the RMA procedure it collects the standard error of the naive and earnest estiamte
#4) Computes a p-curve based effect size

#5) Computes the standard error of the p-curve estimate via bootstrapping. IN particular, the same number of significnat
#   studies is drawn with replacement from teh set that exists, a new estimate is performed, and the task is repeated
#   1000 times. The standard deviation of the effect size estimate is used as the standard error for p-curve's estimate

#################################################################

#Load library used for the meta-analysis
  library(metafor)

#Create three functions that will be used in the program below

#Function 1: variance of a cohen d estimate
    vd=function(d,df) 
      {
      n=(df+2)/2
      return(2/n +(d**2)/(2*(n-3.94)))
      }

#Function 2: Loss function for effect size estimation based on Kolmogorov-Smirnov test
    loss=function(t_obs,df_obs,d_est) {
      ncp_est=sqrt((df_obs+2)/4)*d_est                          
      tc=qt(.975,df_obs)                     
      power_est=1-pt(tc,df_obs,ncp_est)        
      p_larger=pt(t_obs,df=df_obs,ncp=ncp_est)
      ppr=(p_larger-(1-power_est))/power_est  
      KSD=ks.test(ppr,punif)$statistic        
      return(KSD)          
    }


#Function 3: Simplified sampling function
  sub = function(df) return(df[sample(nrow(df),nrow(df),replace=TRUE),])  #take a dataframe and generates another of the same size


#######################################################################

#LOAD data with sunk costs and asian disease (available from www.p-curve.com/)
      data=read.csv("http://p-curve.com/datafiles/SunkAsian.csv")
    
    #fix missing values in asian disease
      data$yasian[data$yasian==0]=NA  #substitute 0s in the asian disease as missing
      

    #Drop two big labs ( mturk and PI)
      data.small=data[(data$lab!="mturk" & data$lab!="pi"),]

    #tell R that I dropped two values from the factor.lab, so re-do the factor
      data.small$lab=factor(data.small$lab)

    
    #Split data by lab so that I can run t-test by lab
      data.by=split(data.small,data.small$lab)


  #get t-values and d.f. for each lab
    sunk.t.all=unlist(lapply(data.by,function (x) t.test(ysunk~xsunk,var.equal=TRUE,data=x)$statistic))  #t-values 
    sunk.df.all=unlist(lapply(data.by,function (x) t.test(ysunk~xsunk,var.equal=TRUE,data=x)$parameter)) #df vector

    asian.t.all=unlist(lapply(data.by,function (x) -1*t.test(yasian~xasian,var.equal=TRUE,data=x)$statistic)) #-1 so that an effect in the predicted direction is >0
    asian.df.all=unlist(lapply(data.by,function (x) t.test(yasian~xasian,var.equal=TRUE,data=x)$parameter))

  #Compute p-values 
    sunk.p.all = 2*(1-pt(abs(sunk.t.all),df=sunk.df.all))
    asian.p.all= 2*(1-pt(abs(asian.t.all),df=asian.df.all))
  
  
  #Make table for plotting p=curves (done in excel for paper)
      sunk.p01=ceiling(sunk.p.all*100)/100
      asian.p01=ceiling(asian.p.all*100)/100
      table(sunk.p01)
      table(asian.p01)


  #Compute observed ds using the formula d=t*2/sqrt(DF+2)
    sunk.d.all =2*sunk.t.all/sqrt(sunk.df.all+2)
    asian.d.all =2*asian.t.all/sqrt(asian.df.all+2)
    
  #Significant subset (more efficient creating a dataframe(), but already did it this way
      sunk.d.sig=subset(sunk.d.all, sunk.p.all<.05 & sunk.t.all>0)
      sunk.t.sig=subset(sunk.t.all, sunk.p.all<.05 & sunk.t.all>0)
      sunk.df.sig=subset(sunk.df.all, sunk.p.all<.05 & sunk.t.all>0)
      
      asian.d.sig=subset(asian.d.all, asian.p.all<.05 & asian.t.all>0)
      asian.t.sig=subset(asian.t.all, asian.p.all<.05 & asian.t.all>0)
      asian.df.sig=subset(asian.df.all, asian.p.all<.05 & asian.t.all>0)
    
  #variance of the effect sizes using formula (see Function 1 above)
      sunk.vd.sig=vd(d=sunk.d.sig, df=sunk.df.sig)  #vd: variance of d
      sunk.vd.all=vd(d=sunk.d.all, df=sunk.df.all)
      
      asian.vd.sig=vd(d=asian.d.sig, df=asian.df.sig)
      asian.vd.all=vd(d=asian.d.all, df=asian.df.all)
      
    
  #ESTIMATED EFFECT SIZE 
    #Earnest
     #this needs the METAFOR library
      library(metafor) #just in case, done already upstairs

      sunk.dhat.e= rma(yi=sunk.d.all ,vi=sunk.vd.all ,method="FE")
      asian.dhat.e= rma(yi=asian.d.all ,vi=asian.vd.all ,method="FE")

    #Naive
      sunk.dhat.n= rma(yi=sunk.d.sig ,vi=sunk.vd.sig ,method="FE")
      asian.dhat.n= rma(yi=asian.d.sig ,vi=asian.vd.sig ,method="FE")

    #p-curve
      sunk.dhat.pc=optimize(loss,c(-.3,2),t_obs=sunk.t.sig, df_obs=sunk.df.sig)$minimum
      asian.dhat.pc=optimize(loss,c(-.3,2),t_obs=asian.t.sig, df_obs=asian.df.sig)$minimum
    

  #BOOTSTRAPPED STANDARD ERRRORS
     #Create a dataframe for each example, it contains the t, df, and d together, so just one random draw gets all three values at once
        sunk= data.frame(sunk.t.sig, sunk.df.sig,  sunk.d.sig, sunk.vd.sig)      #puts together all significant result
        asian=data.frame(asian.t.sig, asian.df.sig,  asian.d.sig, asian.vd.sig) 
      
    
    #BOOTSTRAP IT
      #store each simulated effect size estimate in  vectors. 
      #A p-curve and a naive estimate for sunk and for asian: 4 vectors
          asian.dhat.pc.sim=c()  
          asian.dhat.n.sim=c()
          sunk.dhat.pc.sim=c()
          sunk.dhat.n.sim=c()

        options(warn=-1)  #R gives warning for its own noncentral distribution, not my functions
        set.seed(778899)  #arbitrary number to always get same results
        
      for (i in 1:1000)   #Do 1000 bootstraps: note, when bootstrappingn p-values larger number of bootstraps in often a good idea, 
                          #for SE() not really as we want the stable central value, SD, not the 2.5th percentile.
        {
        #Draw a sample of significant studies with replacement (otherwise would get the same studies every time)
          sunk.k=sub(sunk)         #here i am  using the sub(), Function 3 above
          asian.k=sub(asian)
          
        #Estimate effects for bootstrapped sample
          #Sunk
              #p-curve
                  sunk.dhat.pc.k   =optimize(loss,c(-.2,2),t_obs=sunk.k$sunk.t.sig, df_obs=sunk.k$sunk.df.sig)$minimum
              #naive
                  sunk.dhat.n.k=rma(yi=sunk.k$sunk.d.sig ,vi=sunk.k$sunk.vd.sig, method="FE")$b
          #Asian
              #p-curve
                asian.dhat.pc.k   =optimize(loss,c(-.2,2),t_obs=asian.k$asian.t.sig, df_obs=asian.k$asian.df.sig)$minimum
              #naive
                asian.dhat.n.k=rma(yi=asian.k$asian.d.sig ,vi=asian.k$asian.vd.sig, method="FE")$b
        
         #add to saved values
            sunk.dhat.pc.sim=c(sunk.dhat.pc.sim, sunk.dhat.pc.k)
            sunk.dhat.n.sim=c(sunk.dhat.n.sim, sunk.dhat.n.k)
            asian.dhat.pc.sim=c(asian.dhat.pc.sim, asian.dhat.pc.k)
            asian.dhat.n.sim=c(asian.dhat.n.sim, asian.dhat.n.k)
        }
        
  #verify on average I get the right estimate
      mean(sunk.dhat.pc.sim)   #Average bootstrapped mean estiamted with p-curve
      mean(asian.dhat.pc.sim)  #Average bootstrapped mean estiamted with naive RMA()

      mean(sunk.dhat.n.sim)   #Average bootstrapped mean estiamted with p-curve
      mean(asian.dhat.n.sim)  #Average bootstrapped mean estiamted with naive RMA()

  #compute bootstrapped SE for p-curve
      sunk.se.pc=sd(sunk.dhat.pc.sim)
      asian.se.pc=sd(asian.dhat.pc.sim)

  #and for naive, should be ~ same as obtained using the regression (i.e., the SE from the rma() procedure
  #   note: SEs are slightly smaller with the bootsatrap
      sd(sunk.dhat.n.sim)   #SE from the bootstrap
      sunk.dhat.n$se        #SE from the RMA()

      sd(asian.dhat.n.sim)  #SE from the bootstrap
      asian.dhat.n$se       #SE from the RMA()
      
    
#Tabulate REsults for makin the graph
  #betas
    asianB=c(asian.dhat.n$b, asian.dhat.e$b, asian.dhat.pc)
    sunkB=c(sunk.dhat.n$b, sunk.dhat.e$b, sunk.dhat.pc)
  #SE(B)
    asianSE=c(asian.dhat.n$se, asian.dhat.e$se, asian.se.pc)
    sunkSE=c(sunk.dhat.n$se, sunk.dhat.e$se, sunk.se.pc)

  #put together
    results=rbind(asianB, sunkB, asianSE, sunkSE)
    colnames(results)=c("naive","earnest","p-curve")
    
    results  #this was copy-pasted onto Excel to make the figure



