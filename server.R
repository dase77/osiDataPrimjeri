library(shiny)
library(ggplot2)
library(stringr)
library(scales)
library(wordcloud)
library(tm)

##################### funkcija za dobivanje kodova; primjer data=vjeronauk[,c(4,5,6,26)]
myfun1=function(data){
  #data=data[,-c(2,27)]
  data$Udžbenik=droplevels(data$Udžbenik) #so we can easily subset data
  huh=levels(data$Udžbenik)
  l=lapply(huh,function(x)data[data$Udžbenik==x,])
  
  
  ls=list()
  for(i in 1:length(huh)){
    asdf1=data.frame(l[i])# unlisting
    asdf=data.frame(l[i])[,4]# unlisting 
    asdf=as.character(asdf) # converting to character
    asdf[asdf==""]=NA # empty to NA
    asdf[asdf==" "]=NA # empty to NA
    asdf=na.omit(asdf) #remove NA
    asdf=as.character(asdf) # converting to character
    asdf=str_split(asdf," ") # breaking the strings
    asdf=unlist(asdf) 
    asdf=str_replace(asdf,",","") #removing comma
    asdf=str_trim(asdf) #removing whitespaces
    asdf[asdf==""]=NA # empty to NA
    asdf=na.omit(asdf) #remove NA
    asdf=as.character(asdf) # converting to character
    len=length(asdf) #so we can set how long the data is going to be
    
    #adding texbook code
    asdf1=asdf1[rep(nrow(asdf1),len),][,-4] #seting the length of the data 
    asdf1$kod=asdf
    
    ls[[i]]=asdf1
  }
  ls=do.call(rbind, ls) ####merging the list of dataframes - rbindlist(ls) in the data.table package seems to be the fastet way but is masks melt from reshape
  ls$KKod=paste(ls[,3],ls[,4],sep=" ") #adding a unique code for merging with the primjeri
  ls
}

################### preparing data
data=read.csv("historija (copy).csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
geografija=read.csv("geografija.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
vjeronauk=read.csv("vjeronauk.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
knjizevnost=read.csv("V6baza_jezik i knjizevnost_cijela.csv",sep=";",na.strings=c(" ","","\"\"","NA","N/A")) # load tha data
levels(knjizevnost$Negativni.oblici.ponašanja)[c(3)]=c("Ne")
knjizevnost=knjizevnost[,-27]

names(vjeronauk)=names(data) # same names in order to merge data
names(geografija)=names(data) # same names in order to merge data
names(knjizevnost)=names(data) # same names in order to merge data

data=rbind(data,vjeronauk,geografija,knjizevnost)

levels(data$Negativni.oblici.ponašanja)[c(3,4)]=c("Da","Ne")
levels(data$Univerzalne.vrijednosti)[c(3,4)]=c("Da","Ne")
levels(data$Predmet)[2]="Vjeronauka"
levels(data[,12])[3]="Ne"


data[,5]=factor(data[,5],labels=c("šesti","sedmi","osmi","deveti"))
data[,4]=as.character(data[,4])
data[,7]=as.character(data[,7])
  
######################## preparing data with primjeri
primjeri=read.csv("primjeri.csv",sep=";")
primjeri[,1]=as.character(primjeri[,1])
primjeri[,2]=as.character(primjeri[,2])
primjeri[,3]=as.character(primjeri[,3])
l=list()
for(i in 1:dim(primjeri)[1]){
  l[[i]]=paste(primjeri[i,1],primjeri[i,2],sep=" ")
  l=unlist(l)
}
primjeri$KKod=l



shinyServer(
  function(input, output) {
    
######################################### plots with values
    output$plotosi <- renderPlot({
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      ### if predmet is selected
      if(input$checkbox == TRUE)
      {
        qplot(Razred,fill=data1[,as.numeric(input$var)],data=data1,facets = .~Predmet)+scale_x_discrete("razred")+
          scale_y_continuous("broj lekcija")+ theme(legend.title=element_blank())
      
      }
      else
      qplot(data1[,5],fill=data1[,as.numeric(input$var)])+scale_x_discrete("razred")+scale_y_continuous("broj lekcija")+
        theme(legend.title=element_blank())
    },height = 400, width = 600)
    
    
######################################### table with values
    output$tableosi <- renderPrint({ 
     
       #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      ### if predmet is selected
      if(input$checkbox == TRUE)
      {
        addmargins(table(data1[,as.numeric(input$var)],droplevels(data1[,5]),data1[,3]))
      }
      else
        addmargins(table(data1[,as.numeric(input$var)],droplevels(data1[,5])))
    })
    
    
    ######################################### plots with percentages
    output$plot1osi <- renderPlot({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      ### if predmet is selected
      if(input$checkbox == TRUE)
      {
        qplot(Razred,fill=data1[,as.numeric(input$var)],position="fill",data=data1,facets = .~Predmet)+scale_x_discrete("razred")+
          scale_y_continuous("procenat lekcija",labels=percent)+ theme(legend.title=element_blank())
        
      }
      else
      qplot(data1[,5],fill=data1[,as.numeric(input$var)],position="fill")+scale_x_discrete("razred")+
        scale_y_continuous("procenat lekcija",labels=percent)+ theme(legend.title=element_blank())
    },height = 400, width = 600)
    
    
    ######################################### tables with percentages
    output$table1osi <- renderPrint({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      ### if predmet is selected
      if(input$checkbox == TRUE)
      {
        prop.table(ftable(table(data1[,3],droplevels(data1[,5]),data1[,as.numeric(input$var)])),margin = 1)*100
      }
      else
      
      
      prop.table(table(data1[,as.numeric(input$var)],droplevels(data1[,5])),margin = 2)*100
    })
    
    ######################################## primjeri
    
    #data=vjeronauk[,c(4,5,6,26)]
    output$primjeriosi <- renderTable({ 
      
      #### changing NPP
      if(input$var1 == "svi")
      {
        data=data
      }
      else
        data=data[data[,4]==input$var1,]
      
      data1=data[data[,5] %in% c(unlist(input$checkGroup)),]
      
      ### if predmet is selected
      if(input$checkbox == TRUE)
      {
        data2=data1[,c(4,5,6,as.numeric(input$var)+1)]
        prim=merge(primjeri,myfun1(data2),by="KKod")[c(5,6,7,4)]
        prim
      }
      else
        data2=data1[,c(4,5,6,as.numeric(input$var)+1)]
      prim=merge(primjeri,myfun1(data2),by="KKod")[c(5,6,7,4)]
      prim
      },include.rownames=FALSE)
    
    
  }
)
