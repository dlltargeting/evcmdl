#########################################################################################################################################
#This function is used for vector analysis of soil                                                                                      #
#carbon, nitrogen and phosphorus acquisition enzymes.                                                                                   #
#This function is based on the following literature.                                                                                    #
#Moorhead, D. L., Sinsabaugh, R. L., Hill, B. H. & Weintraub, M. N.                                                                     #
#Vector analysis of ecoenzyme activities reveal constraints on coupled C, N and P dynamics.                                             #
#Soil Biology and Biochemistry,2016,93,1-7, doi:10.1016/j.soilbio.2015.10.019.                                                          #
#We call this R function mevcdl,Its formula is based on the analysis of Enzyme VeCtor proposed by Moorhead,et al.,                      #
#and its R Code is made by  Ding Leilei (Guizhou Institution of Prataculture, Guizhou Academy of Agricultural Sciences, Guizhou, China) #
#for researchers to use.                                                                                                                #
#########################################################################################################################################
#*****
#****
#***
#**
#*

#####* 
#####**
#####***
###########################################################()#
#####***
#####**
#####*
evcmdl<-function(trans="2",data){ 
  if(trans=="1"){ 
   x<-data$ec/data$ep
   y<-data$ec/data$en
   vl<-sqrt(x^2+y^2)
   va<-atan2(y,x)/pi*180
   results<-as.data.frame(cbind(vl,va))
   return(results)
   }else if (trans=="2") {
   x<-log(data$ec)/log(data$ep)
   y<-log(data$ec)/log(data$en)
   vl<-sqrt(x^2+y^2)
   va<-atan2(y,x)/pi*180
   results<-as.data.frame(cbind(vl,va))
   return(results)
   }else if (trans=="3") {
   x<-data$ec/(data$ec+data$ep)
   y<-data$ec/(data$ec+data$en)
   vl<-sqrt(x^2+y^2)
   va<-atan2(y,x)/pi*180
   results<-as.data.frame(cbind(vl,va))
   return(results)  
   }else if (trans=="4") {
   x<-asin(data$ec/(data$ec+data$ep))
   y<-asin(data$ec/(data$ec+data$en))
   vl<-sqrt(x^2+y^2)
   va<-atan2(y,x)/pi*180
   results<-as.data.frame(cbind(vl,va))
   return(results) 
   }else if (trans=="5") {
   x<-asin(sqrt(data$ec/(data$ec+data$ep)))
   y<-asin(sqrt(data$ec/(data$ec+data$en)))
   vl<-sqrt(x^2+y^2)
   va<-atan2(y,x)/pi*180
   results<-as.data.frame(cbind(vl,va))
   return(results) 
   }else if(trans=="6") {
   Pp<-data$ec/(data$ec+data$ep)
   Pn<-data$ec/(data$ec+data$en)
   x<-log(Pp/(1-Pp))
   y<-log(Pn/(1-Pn))
   vl<-sqrt(x^2+y^2)
   va<-atan2(y,x)/pi*180
   results<-as.data.frame(cbind(vl,va))
   return(results)
   }     
}
#####* 
#####**
#####***
###########################################################()#
#####***
#####**
#####*