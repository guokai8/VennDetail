# VennDetail

Extract detail information from venndiagram

## Description

VennDetail is a package can be used for Extract detail information from venndiagram

## Dependencies

R>2.15  
VennDiagrm

## Installation

library(devtools)    
install_github("guokai8/VennDetail")
Suggest use RStudio to run the command if you want to use build_vignettes
### Getting started
```  
library(VennDetail)
A <- sample(1:1000, 400, replace = FALSE);
B <- sample(1:1000, 600, replace = FALSE);
C <- sample(1:1000, 350, replace = FALSE);
D <- sample(1:1000, 550, replace = FALSE);
res<-venndetail(list(A=A,B=B,C=C,D=D),plot=TRUE)
result<-result(res)
head(res);
```  
Some useful commands
```  
get(res,"A")
detail(res)
dplot(res)

```  
### Contact information

I still working on this package and will add more functions here. 
For any questions please contact guokai8@gmail.com
