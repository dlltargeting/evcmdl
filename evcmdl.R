######################################################################
###############This function is used for vector analysis of soil 
#############carbon, nitrogen and phosphorus acquisition enzymes.
###############This function is based on the following literature.
##############Moorhead, D. L., Sinsabaugh, R. L., Hill, B. H. & Weintraub, M. N. 
##############Vector analysis of ecoenzyme activities reveal constraints on coupled C, N and P dynamics. 
###########################Soil Biology and Biochemistry,2016,93,1-7, doi:10.1016/j.soilbio.2015.10.019.
###We call this R function mevcdl,Its formula is based on the analysis of Enzyme VeCtor proposed by Moorhead,et al., 
###and is made by user Ding Leilei (Guizhou Institution of Prataculture, Guizhou Academy of Agricultural Sciences, Guizhou, China)
###for researchers to use.
##########################
###
###
###
evcmdl<-function(ec,en1,en2,ep){
 en<-en1+en2
 x<-ec/ep
 y<-ec/en
 vl<-sqrt(x^2+y^2)
 va<-atan2(y,x)/pi*180
 return(vl,va)
}
###
###
###
#####################  
  
  

