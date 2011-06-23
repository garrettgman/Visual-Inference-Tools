createRandom1Data<-function(n1=35,n2=25,m1=120,m2=90,sd1=18,sd2=12){
  cBP     = rnorm(n1,m1,sd1)
  dBP     = rnorm(n2,m2,sd2)
  cType   = rep("Control",n1)
  dType   = rep("Drug",n2)
  sample.df = data.frame(BP=c(cBP,dBP),type=c(cType,dType))
  index<-sample(1:(n1+n2))
  sample.df<-sample.df[index,]
  rownames(sample.df)<-1:(n1+n2)
  return(list(data=sample.df))
}

createRandom2Data<-function(n1=20,n2=10,p1=runif(1,0,1),p2=runif(1,0,1)){
  # n1: size of control group
  # n2: size of drug group
  # p1: proportion of A in control group
  # p2: proportion of B in control group
  cA = round(n1*p1)
  cB = n1-cA
  dA = round(n2*p2)
  dB = n2-dA
  
  control.D<-c(rep("A",cA),rep("B",cB))
  drug.D<-c(rep("A",dA),rep("B",dB))
  
  sample.df<-data.frame(disease=c(control.D,drug.D),
                        type=c(rep("Control",n1),rep("Drug",n2)))
  
  index<-sample(1:(n1+n2))
  sample.df<-sample.df[index,]
  rownames(sample.df)<-1:(n1+n2)
  freqtable<-table(sample.df)
  #write.csv(sample.df,file="GUI/data/Disease3.csv",row.names=F)
  return(list(data=sample.df,freq=freqtable,prop=round(freqtable/rep(c(n1,n2),each=2),2)))
}



