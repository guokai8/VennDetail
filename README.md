# VennDetail

VennDetail: a package for visualization and extract details
![](venn.png)

## Description

Visualizing and extracting unique (disjoint) or overlapping subsets of multiple 
gene sets are a frequently performed task for bioinformatics. Although various 
packages and web applications are available, no R package offerings functions to
extract and combine details of these subsets with user datasets in data frame is
available. Moreover, graphical visualization is usually limited to six or less 
gene sets and a novel method is required to properly show the subset details.
We have developed __VennDetail__, an R package to generate high-quality Venn-Pie
charts and to allow extraction of subset details from input sets.  

## Dependencies

R>2.15  
VennDiagrm

## Installation
``` 
library(devtools)    
install_github("guokai8/VennDetail")
``` 
## Getting started
```  
library(VennDetail)
A <- sample(1:1000, 400, replace = FALSE);
B <- sample(1:1000, 600, replace = FALSE);
C <- sample(1:1000, 350, replace = FALSE);
D <- sample(1:1000, 550, replace = FALSE);
res<-venndetail(list(A=A,B=B,C=C,D=D),plot=TRUE);
result<-result(res);
head(result);
```  
### Some useful commands
```  
get(res,"A"); # get unique elements in A
detail(res); #show overlap 'details' of all groups
dplot(res); #make a bargraph for 'details'
dA=data.frame(A=A,"FC"=rnorm(400))
dB=data.frame(B=B,"FC"=rnorm(600))
dC=data.frame(C=C,"FC"=rnorm(350))
dD=data.frame(D=D,"FC"=rnorm(550))
getFeature(res,group="Shared",rlist=list(dA,dB,dC,dD),userowname=F,gind=c("A","B","C","D"))
###As all these four dataframes don't have row names, we set userowname to be FALSE
``` 
### Support shiny now

[VennDetail Shiny App](https://github.com/guokai8/VennDetail-Shiny)   
[VennDetail Shiny App Website](http://hurlab.med.und.edu:3838/VennDetail/)

## Contact information

For any questions please contact guokai8@gmail.com
