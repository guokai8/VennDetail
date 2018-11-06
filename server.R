library(VennDetail)
library(DT)
library(ggplot2)
library(grid)
library(tidyverse)
library(readxl)
library(export)
library(xlsx)
options(shiny.maxRequestSize=200*1024^2) 
options(digits=3)
#setwd("/home/hurlab/tmp")
A1 <- sample(1:100, 15, replace = FALSE);
B1 <- sample(1:100, 20, replace = FALSE);
A=data.frame("Gene"=paste("Gene",sample(LETTERS[1:26],15,replace=F),sep="."),"Exp"=A1,"FC"=round(rnorm(15),2))
B=data.frame("Gene"=paste("Gene",sample(LETTERS[1:26],20,replace=F),sep="."),"Exp"=B1,"FC"=round(rnorm(20),2))
rownames(A)<-A$Gene
rownames(B)<-B$Gene
#mycol<-unique(c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3",setcolor(30)))
mycol<-c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3")
mycol2<-unique(c("transparent","black",mycol,setcolor(40)))
checkfile <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 

shinyServer(function(input, output,session){
  all_list=list()
  p <- reactiveValues(default=0)
  click<-reactiveValues(default=0)
  datain <- reactive({
    file1<-input$file1
    file2<-input$file2
    file3<-input$file3
    file4<-input$file4
    file5<-input$file5
    file6<-input$file6
    filename1 <- input$filename1; filename2 <- input$filename2; filename3 <- input$filename3; filename4 <- input$filename4; 
    filename5 <- input$filename5; filename6 <- input$filename6;
    if(filename1==""){filename1<-"File1"}; if(filename2==""){filename2<-"File2"}; if(filename3==""){filename3<-"File3"};
    if(filename4==""){filename4<-"File4"}; if(filename5==""){filename5<-"File5"}; if(filename6==""){filename6<-"File6"};
    if (is.null(input$file1)) {
      # User has not uploaded a file yet
      f1<-A
     }else{
      ext1=checkfile(file1$datapath)
      if(ext1=="txt"){
        f1<-read.delim(file1$datapath,sep="\t",header = TRUE)
      }
      if(ext1=="csv"){
        f1<-read.delim(file1$datapath,sep=",",header = TRUE)
      }
      if(ext1=="xls"|ext1=="xlsx"){
        f1<-read_excel(file1$datapath)
        f1<-as.data.frame(f1)
      }
      rownames(f1)<-make.names(f1[,1],unique = T)
    }
    all_list[[filename1]]<-f1
    if (is.null(file2)) {
      # User has not uploaded a file yet
      f2<-B
    }else{
      ext2<-checkfile(file2$datapath)
      if(ext2=="txt"){
        f2<-read.delim(file2$datapath,sep="\t",header = TRUE)
      }
      if(ext2=="csv"){
        f2<-read.delim(file2$datapath,sep=",",header = TRUE)
      }
      if(ext2=="xls"|ext2=="xlsx"){
        f2<-read_excel(file2$datapath)
        f2<-as.data.frame(f2)
      }
      rownames(f2)<-make.names(f2[,1],unique = T)
    }
    all_list[[filename2]]<-f2
    if (is.null(file3)) {
      # User has not uploaded a file yet
      f3<-NULL
    }else{
      ext3=checkfile(file3$datapath)
      if(ext3=="txt"){
        f3<-read.delim(file3$datapath,sep="\t",header = TRUE)
      }
      if(ext3=="csv"){
        f3<-read.delim(file3$datapath,sep=",",header = TRUE)
      }
      if(ext3=="xls"|ext3=="xlsx"){
        f3<-read_excel(file3$datapath)
        f3<-as.data.frame(f3)
      }
      rownames(f3)<-make.names(f3[,1],unique = T)
    }
    all_list[[filename3]]<-f3
    if (is.null(file4)) {
      # User has not uploaded a file yet
      f4<-NULL
    }else{
      ext4<-checkfile(file4$datapath)
      if(ext4=="txt"){
        f4<-read.delim(file4$datapath,sep="\t",header = TRUE)
      }
      if(ext4=="csv"){
        f4<-read.delim(file4$datapath,sep=",",header = TRUE)
      }
      if(ext4=="xls"|ext4=="xlsx"){
        f4<-read_excel(file4$datapath)
        f4<-as.data.frame(f4)
      }
      rownames(f4)<-make.names(f4[,1],unique = T)
    }
    all_list[[filename4]]<-f4
    if (is.null(file5)) {
      # User has not uploaded a file yet
      f5<-NULL
    }else{
      ext5<-checkfile(file5$datapath)
      if(ext5=="txt"){
        f5<-read.delim(file5$datapath,sep="\t",header = TRUE)
      }
      if(ext5=="csv"){
        f5<-read.delim(file5$datapath,sep=",",header = TRUE)
      }
      if(ext5=="xls"|ext5=="xlsx"){
        f5<-read_excel(file5$datapath)
        f5<-as.data.frame(f5)
      }
      rownames(f5)<-make.names(f5[,1],unique = T)
    }
    all_list[[filename5]]<-f5
    if (is.null(file6)) {
      # User has not uploaded a file yet
      f6<-NULL
    }else{
      ext6=checkfile(file6$datapath)
      if(ext6=="txt"){
        f6<-read.delim(file6$datapath,sep="\t",header = TRUE)
      }
      if(ext6=="csv"){
        f6<-read.delim(file6$datapath,sep=",",header = TRUE)
      }
      if(ext6=="xls"|ext6=="xlsx"){
        f6<-read_excel(file6$datapath)
        f6<-as.data.frame(f6)
      }
      rownames(f6)<-make.names(f6[,1],unique = T)
    }
    all_list[[filename6]]<-f6
    return(all_list)
  })
  size<-reactive({
    length(datain())
  })
  observe({
    lapply(1:size(), function(i) {
      local({
        #my_i <- i
        tablename <- names(datain())
        output[[tablename[i]]] <-  DT::renderDT({
          datain()[[tablename[i]]]
        },selection = list(target = 'column',mode="single"),filter = 'top')
      })
    })
  })
  output$mytabs = renderUI({
    nTabs = size()
    myTabs = lapply(names(datain()), function(x){
      tabPanel(x, DT::dataTableOutput(x))
    })
    do.call(tabsetPanel, myTabs)
  })
  
  
  listData <- reactive({
    all<-list()
    name1 <- input$name1; name2 <- input$name2; name3 <- input$name3; name4 <- input$name4; 
    name5 <- input$name5; name6 <- input$name6;
    if(name1==""){name1<-"List 1"}; if(name2==""){name2<-"List 2"}; if(name3==""){name3<-"List 3"};
    if(name4==""){name4<-"List 4"}; if(name5==""){name5<-"List 5"}; if(name6==""){name6<-"List 6"};
    a <- input$list1; 
    a <- as.list(unlist(strsplit(a,",")))
    a <- unlist(lapply(a, function(x){x <- gsub(" ", "", x)})); a <- a[a != ""]
    all[[name1]]<-a
    b <- input$list2
    b <- as.list(unlist(strsplit(b,",")))
    b <- unlist(lapply(b, function(x){x <- gsub(" ", "", x)})); b <- b[b != ""]
    all[[name2]]<-b
    c <- input$list3
    c <- as.list(unlist(strsplit(c,",")))
    c <- unlist(lapply(c, function(x){x <- gsub(" ", "", x)})); c <- c[c != ""]
    all[[name3]]<-c
    d <- input$list4
    d <- as.list(unlist(strsplit(d,",")))
    d <- unlist(lapply(d, function(x){x <- gsub(" ", "", x)})); d <- d[d != ""]
    all[[name4]]<-d
    e <- input$list5
    e <- as.list(unlist(strsplit(e,",")))
    e <- unlist(lapply(e, function(x){x <- gsub(" ", "", x)})); e <- e[e != ""]
    all[[name5]]<-e
    f <- input$list6
    f <- as.list(unlist(strsplit(f,",")))
    f <- unlist(lapply(f, function(x){x <- gsub(" ", "", x)})); f <- f[f != ""]
    all[[name6]]<-f
    return(all)
  })
  observeEvent(input$plotx,{
    p$default<-1
    click$default=1
  })
  observeEvent(input$ploty,{
    p$default<-2
    click$default=1
  })
  #output$text<-renderText({paste("the vakye is:",p$default)}) debug
  data <- reactive({
    if(p$default==1){
      dat <- datain()
      tablename <- names(dat)
      rowtable<-paste(tablename,"rows_all",sep="_")
      tablename<-paste(tablename,"columns_selected",sep="_")
      for(i in 1:length(tablename)){
        tmp<-dat[[i]]
        s = input[[rowtable[[i]]]]
        if(length(s)>0 && length(s) < nrow(tmp)){
          tmp<-tmp[s,,drop=FALSE]
        }
        dat[[i]]<-tmp
        if(is.null(input[[tablename[i]]])){
          dat[[i]]<-as.character(dat[[i]][,1])
        }else{
          dat[[i]]<-dat[[i]][,input[[tablename[i]]]]
        }
      }
      #dat<-lapply(dat,function(x)rownames(x))
    }
    if(p$default==2){
      dat <- listData()
    }
    if(p$default==0){
      filen1=input$filename1
      filen2=input$filename2
      dat<-list(rownames(A),rownames(B))
      names(dat)<-c(filen1,filen2)
    }
    return(dat)
  })
output$select<-renderUI({
    group=names(data())
    selectInput("sel","Group",choices=group,multiple=TRUE,selected=group)
})
setven<-reactive({
  dd<-data()
  dd<-dd[input$sel]
  dd<-lapply(dd,function(x)na.omit(x))
  ven<<-venndetail(dd,plot=F)
})
plot_fig<-function(){
  res<-setven()
  if(input$type=="venn"){
    plot(res,type=input$type,mycol=input$col[1:length(res@input)],cat.cex=input$vencex,cex=input$textsize,alpha=input$alpha,col=input$border) 
  }
  if(input$type=="vennpie"){
    col1<-input$piecol
    len<-length(detail(res))
    if(length(col1)<len){
      col1[len]<-"white"
    }
    names(col1)<-names(detail(res))
    if(input$percent=="Yes"){
      percent=TRUE
    }else{
      percent=FALSE
    }
    if(input$log=="Yes"){
      logg=TRUE
    }else{
      logg=FALSE
    }
    plot(res,type=input$type,piecolor=col1,any=input$any,revcolor=input$revcol,percentage=percent,log = logg,base=2)
  }
  if(input$type=="upset"){
    plot(res,type=input$type,mycol=input$col[1:length(res@input)],
         sets.x.label=input$xlab,mainbar.y.label=input$ylab,
         text.scale=c(input$labs,input$uax,input$labs,input$groups,input$uax,input$uax)) 
  }

}
###dirname(input$file1$datapath)
###
save.plot.name<-reactive(function(){
  paste(input$plotfile,input$type,input$ftype,sep=".")
})
output$content<-renderPlot({
  plot_fig()
})
output$plot<-renderUI({
  plotOutput("content",height = as.numeric(input$height)*50,width = as.numeric(input$width)*50)
})
output$chose<-renderUI({
  selectizeInput("group","Group",choices=names(detail(setven())),multiple=TRUE)
})
output$Download_plot<-downloadHandler(
  filename = save.plot.name(),
  content = function(filename) {
    wid<-as.numeric(input$width)
    hei<-as.numeric(input$height)
    if(input$ftype=="pdf"){
      pdf(filename,width = wid,height = hei)
      plot_fig()
    }
    if(input$ftype=="png"){
      png(filename,width = wid*50,height = hei*50,res=300,units = "px")
      plot_fig()
    }
    if(input$ftype=="jpeg"){
      jpeg(filename,width = wid*50,height = hei*50,res=300,units = "px")
      plot_fig()
    }
    if(input$ftype=="tiff"){
      tiff(filename,width = wid*50,height = hei*50,units = "px")
      plot_fig()
    }
    if(input$ftype=="eps"){
      graph2eps(plot_fig(),file=filename)
    }
    if(input$ftype=="pptx"){
      graph2ppt(plot_fig(),file=filename)
    }
    if(input$ftype=="doxs"){
      graph2doc(plot_fig(),file=filename)
    }
    dev.off()
    }
)
output$dyna<-renderUI({
  switch(input$type,
  "venn"=list(selectInput("col","Color",choices = c(mycol,colors()),multiple = TRUE,selected =c(mycol,colors())[1:length(names(data()))]),
              sliderInput("vencex","Label size",min = 0,max=5,value = 1.5,step=0.1),
              sliderInput("textsize","Text size",min = 0,max=8,value = 2,step=0.1),
              sliderInput("alpha","Alpha",min=0,max=1,value=0.5,step=0.1),
              selectInput("border","Border",choices=mycol2,selected="black")
             ),
  "vennpie"=list(selectInput("piecol","Color",choices = c(rev(mycol),colors()),selected = rev(mycol)[1:length(detail(setven()))],multiple = T),
                 selectInput("any","Selected",choices = 1:(length(setven()@raw)),multiple = T,selected =1:(length(setven()@raw))),
                 selectInput("revcol","Reverse color",choices = c(mycol,colors()),selected="lightgrey"),
                 radioButtons("percent","Percentage",choices = c("Yes","No"),selected = "No",inline = TRUE),
                 radioButtons("log","Log2 Transformed",choices = c("Yes","No"),selected = "No",inline = TRUE)
  ),
  "upset"=list(selectInput("col","Color",choices = c(mycol,colors()),multiple = TRUE,selected = c(mycol,colors())[1:length(names(data()))]),
               textInput("xlab","X lab",value="Set Size"),
               textInput("ylab","Y lab",value="Intersection Size"),
               sliderInput("labs","lab size",min = 0,max=8,value = 1.5,step=0.1),
               sliderInput("groups","Group size",min = 0,max=8,value = 1.5,step=0.1),
               sliderInput("uax","axis size",min = 0,max=8,value = 1.5,step=0.1)
  )
  )
})
getdata<-reactive({
  if(p$default==1){
    dat <- datain()
    tablename <- names(dat)
    rowtable<-paste(tablename,"rows_all",sep="_")
    tablename<-paste(tablename,"columns_selected",sep="_")
    flag=0
    for(i in 1:length(dat)){
     # ind<-input[[tablename[[i]]]]
      tmp<-dat[[i]]
      s = input[[rowtable[[i]]]]
      if(length(s)>0 && length(s) < nrow(tmp)){
        tmp<-tmp[s,,drop=FALSE]
      }
      if(!is.null(input[[tablename[[i]]]])){
          iname<-input[[tablename[[i]]]]
          colnames(tmp)[iname]="RowNxyz"
          tmp$RowNxyz<-as.character(tmp$RowNxyz)
          tmp<-rownames_to_column(tmp)
          tmp<-tmp%>%select(RowNxyz,everything())
          #dat[[i]]<-column_to_rownames(tmp,iname)
          flag=1
      }else{
        colnames(tmp)[1]<-"RowNxyz" ###need to be modified
        tmp[,1]<-as.character(tmp[,1])
      }
      dat[[i]]<-tmp
    }
   # if(flag==1){
      tab<-VennDetail::getFeature(setven(),rlist=dat,group = input$group,userowname = F)
   # }else{
  #    tab<-VennDetail::getFeature(setven(),rlist=dat,group = input$group,userowname = T)
   # }
    flag=0
  }
  if(p$default==2){
    tab<-VennDetail::get(setven(),group = input$group)
  }
  if(p$default==0){
    n1<-input$filename1
    n2<-input$filename2
    rlist=list(A,B)
    names(rlist)<-c(n1,n2)
    flag1=0
    tablename <- names(rlist)
    rowtable<-paste(tablename,"rows_all",sep="_")
    tablename<-paste(tablename,"columns_selected",sep="_")
    for(i in 1:length(rlist)){
      # ind<-input[[tablename[[i]]]]
      tmp1<-rlist[[i]]
      s1 = input[[rowtable[[i]]]]
      if(length(s1)>0 && length(s1) < nrow(tmp1)){
        tmp1<-tmp1[s1,,drop=FALSE]
      }
      if(!is.null(input[[tablename[[i]]]])){
        tmp1<-rlist[[i]]
        coln1<-colnames(tmp1)
        iname1<-input[[tablename[[i]]]]
        colnames(tmp1)[iname1]="RowNxyz"
        tmp1$RowNxyz<-as.character(tmp1$RowNxyz)
        tmp1<-rownames_to_column(tmp1)
        tmp1<-tmp1%>%select(RowNxyz,everything())
        flag1=1
      }else{
        colnames(tmp1)[1]<-"RowNxyz" ###need to be modified
        tmp1[,1]<-as.character(tmp1[,1])
      }
      rlist[[i]]<-tmp1
    }
    #if(flag1==1){
      tab<-VennDetail::getFeature(setven(),rlist=rlist,group = input$group,userowname = F)
  #  }else{
    #  tab<-VennDetail::getFeature(setven(),rlist=rlist,group = input$group,userowname = T)
   # }
    flag=0
  }
  tab
})

output$table<-DT::renderDT({
  datatable(getdata(),caption=htmltools::tags$caption(
    style = 'caption-side: bottom; text-align: center;',
    htmltools::em('Detail of subsets selected.')
  ))
  #vennpie(ven)
})
save.file.name<-reactive(function(){
  paste(input$filename,input$type,input$fftype,sep=".")
})
output$Download_data<-downloadHandler(
  filename=save.file.name(),
  content = function(file){
    if(input$fftype=="txt"){
     write.table(getdata(),file=file,sep="\t",quote=F)
    }
    if(input$fftype=="csv"){
      write.csv(getdata(),file=file,quote=F)
    }
    if(input$fftype=="xls"){
      write.xlsx(getdata(),file=file,sheetName="Sheet 1",append=FALSE)
    }
  }
)
observe({
  if(click$default==1){
    updateTabsetPanel(session,"tabs","Plot")
  }
  click$default=0
})
onSessionEnded(function() {
  cat("Goodbye\n")
}) 
}
)

