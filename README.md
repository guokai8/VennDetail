# VennDetail

Extract detail information from venndiagram

## Description

__VennDetail__ is a package can be used for extracting details(shared,unique sets) from venndiagram

## Dependencies

R>2.15  
VennDiagrm

## Installation
``` 
library(devtools)    
install_github("guokai8/VennDetail")
``` 
** Recommand: Use RStudio to run the command.
## Getting started
```  
library(VennDetail)
A <- sample(1:1000, 400, replace = FALSE);
B <- sample(1:1000, 600, replace = FALSE);
C <- sample(1:1000, 350, replace = FALSE);
D <- sample(1:1000, 550, replace = FALSE);
res<-venndetail(list(A=A,B=B,C=C,D=D),plot=TRUE);
result<-result(res);
head(res);
```  
### Some useful commands
```  
get(res,"A"); #get sets unique in A
detail(res); #show detail for groups 
dplot(res); #make a detail barplot
dA=data.frame(A=A,"FC"=rnorm(400))
dB=data.frame(B=B,"FC"=rnorm(600))
dC=data.frame(C=C,"FC"=rnorm(350))
dD=data.frame(D=D,"FC"=rnorm(550))
getFeature(res,group="Shared",rlist=list(dA,dB,dC,dD),userowname=F)
###As all this four dataframe don't have rownames we choose userownames=F 
``` 
** PS: Support up to five-way venndiagram
## Contact information

I still working on this package and will add more functions here. 
For any questions please contact guokai8@gmail.com
