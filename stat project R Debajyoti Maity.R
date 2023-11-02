###########collect from KAGGLE DATASET##################
getwd()
setwd("/home/sysadm/stat1-R-project/STAT EDA PROJECT sem 1/EDA PROJECT")
tab1<-read.csv("/home/sysadm/stat1-R-project/STAT EDA PROJECT sem 1/EDA PROJECT/NCRB_ADSI-2019_Table_2.13_state-ut.csv")
tab1
#transform data frame#
tab2<-data.frame(tab1)
tab2
tab2$State.UT
as.factor(tab2$State.UT)
##remove 30,38,39 row
tab3<-tab2[-c(30,38,39),]
tab3
tab3$State.UT
tab3[30,c(1,2)]###indicate particular category state UT A and NISLANDS
#QUESTION 1----------------------------
#What is the the ratio of by sleeping pills of Male,Female,Trans?
tab4<-tab3[,c(3,4,5)]
tab4
a<-apply(tab4[c(1,2,3)],2,sum)##sum of the death by consuming sleeping pills categories wise of total state and union territories
a
a_1<-barplot(a,ylab='count',col=c("blue","green","red"),
        ylim=c(0,600),main='Accidental Death And Suicide By Consuming Sleeping Pills',
        sub='Vertical Bar Diagram')
a_1
barplot(a,ylab='count',col=c("blue","green","red"),
        ylim=c(0,600),main=' Accidental Death And Suicide By Consuming Sleeping Pills',
        sub='Vertical Bar Diagram')
text(c(0.7,1.9,3.1),
     as.numeric(a)+60,
     paste0(as.numeric(round(prop.table(a)*100,2)),"%"))

barplot(a,ylab='Frequency',space=0.04,font.axis=2,col=c("blue","green","red"),main='Accidental death and suicide By consuming sleeping pills',ylim=c(0,700),sub="vertical Bardiagram")


pie(a,col=c("orange","blue","violet"),radius=1,main='Accidental death and suicide By consuming sleeping pills',sub="vertical Bardiagram",labels=paste0(round(100*a/sum(a),2),"%"))
  legend("bottomleft",legend=c("Male","Female","Trans"),fill=c("orange","blue","violet"))
barplot(a,xlab='count',col=c("blue","green","red"),xlim=c(0,600),horiz=T,main='Accidental Death And Suicide By consuming sleeping pills',sub="Horrizontal Bardiagram")

#sleeping pills maximum  from which state 
tab4_1<-tab3[,c(2,6)]
tab4_1
which.max(tab4_1$By.Consuming.Sleeping.Pills_Total)
tab4_1[which.max(tab4_1$By.Consuming.Sleeping.Pills_Total),]
#created Divided bar diagram
data=matrix(a,nrow=3)
data
rownames(data)<-c('By.Consuming.Sleeping.Pills_Male','By.Consuming.Sleeping.Pills_Female','By.Consuming.Sleeping.Pills_Trans')
colnames(data)<-c('accidental death and suicide')
data
barplot(data,col=c("blue","green","red"),ylim=c(0,800),main='Accidental Death And Suicide By Consuming Sleeping Pills',sub='Divided Dar Diagram',space=0.04,legend.text = c('Male','Female','Trans'))
#Question2----------------- 
#In which state of India had the maximum number of accidental death and suicide?
tab5<-tab3[,c(1,2,70)]
tab5
which.max(tab5$Total_Total)    #this gives number of row
tab5[which.max(tab5$Total_Total),]
#minimum number of accidental death and suicide in India(category)?
which.min(tab5$Total_Total)
tab5[which.min(tab5$Total_Total),]
###---------

tab3_0<-read.csv("/home/sysadm/stat1-R-project/STAT EDA PROJECT sem 1/EDA PROJECT/NCRB_ADSI-2019_Table_2.13_state-ut.csv")
tab3_0

tab4_0<-tab3_0[-c(30,38,39),c(2,70,71)]
tab4_0

tab5_0<-tab4_0$Total_Total/tab4_0$Total.population
tab5_0
tab6_0<-cbind(tab4_0,tab5_0)
tab6_0
colnames(tab6_0)<-c("State.UT","Total_Total","Total.population","proportion Death")
tab6_0


par(mfrow=c(2,1))
barplot(tab6_0$`proportion Death`,xlab="State_UT",ylab="proportion count",col=rainbow(36),main="Proportion Accidental Death All Over India",names.arg = c("ANDHRA PRADESH","ARUNACHAL PRADESH","ASSAM","BIHAR","CHHATTISGARH","GOA","GUJARAT","HARYANA","HIMACHAL PRADESH","JAMMU & KASHMIR","JHARKHAND","KARNATAKA","KERALA","MADHYA PRADESH","MAHARASHTRA","MANIPUR","MEGHALAYA","MIZORAM","NAGALAND","ODISHA","PUNJAB","RAJASTHAN","SIKKIM","TAMIL NADU","TELANGANA","TRIPURA","UTTAR PRADESH","UTTARAKHAND","WEST BENGAL","A & NISLANDS","CHANDIGARH","D & N HAVELI","DAMAN & DIU","DELHI (UT)","LAKSHADWEEP","PUDUCHERRY"),names.argangle=30)

barplot(tab6_0$Total_Total,xlab="State_UT",ylab="frequency",col=rainbow(36),ylim = c(0,20000),main=" Accidental Death All Over India")
legend("center",fill=rainbow(36),legend=c("ANDHRA PRADESH","ARUNACHAL PRADESH","ASSAM","BIHAR","CHHATTISGARH","GOA","GUJARAT","HARYANA","HIMACHAL PRADESH","JAMMU & KASHMIR","JHARKHAND","KARNATAKA","KERALA","MADHYA PRADESH","MAHARASHTRA","MANIPUR","MEGHALAYA","MIZORAM","NAGALAND","ODISHA","PUNJAB","RAJASTHAN","SIKKIM","TAMIL NADU","TELANGANA","TRIPURA","UTTAR PRADESH","UTTARAKHAND","WEST BENGAL","A & NISLANDS","CHANDIGARH","D & N HAVELI","DAMAN & DIU","DELHI (UT)","LAKSHADWEEP","PUDUCHERRY"),cex=0.3)
par(mfrow=c(1,1))

#Question3-----------------------------
#In which type of jumping the people committed to suicide? 
#Jumping..from.Building._Total #Jumping..from.Other.Sites._Total#Jumping..by.Jumping.off.Moving.Trains.Vehicles._Total
tab6<-tab3[,c(39:54)]
tab6
d<-tab6[,c(8,12,16)]
d
e<-apply(d[c(1,2,3)],2,sum)
e
pie(e,col=c("black","green","red"),radius=0.8,clockwise = T,main=('Accidental Death And Suicide From Jumping'),labels=paste0(round(100*e/sum(e),2),"%"),sub="pie chart")
legend("bottomleft",legend=c("Building","othersite","Trains"),fill=c("black","green","red"))
barplot(e,col=c("violet","green","blue"),ylab='count',ylim=c(0,1200),main=('Accidental Death And Suicide From Jumping'),sub="vertical Bardiagram")
text(c(0.7,1.9,3.1),
     as.numeric(e)+60,
     paste0(as.numeric(round(prop.table(e)*100,2)),"%"))
#from which state the jumping suicide was maximum?--------------------------
tab6_1<-tab3[,c(2,39:54)]
tab6_1
which.max(tab6_1$Jumping..Total._Total)
tab6_1[which.max(tab6_1$Jumping..Total._Total),]

#Question4----------------------------
#Distinguish Gender wise which types of consuming poison the people committed to suicide?

tab7<-tab3[,c(24:34)]
tab7
f<-tab7[,c(4,5,6,8,9,10)]
f
par(mfrow=c(1,1))
g<-apply(f[,c(1,4)],2,sum)
g
barplot(g,ylab='Frequency',space=0.1,col=c("red","green"),ylim=c(0,20000),main=('Accidental death and suicide by poison of male category'),sub='vertical bar diagram',legend.text = c("consuming insecticides","consuming other poison"))


h<-apply(f[,c(2,5)],2,sum)
h
barplot(h,ylab='count',ylim=c(0,17000),col=c("red","green"),main=('by poison_Female_barplot'),sub=('vertical bar plot'))

i<-apply(f[,c(3,6)],2,sum)
i
barplot(i,ylab='count',Col=c("red","green"),ylim=c(0,5),main=('by poison_Trans_barplot'),sub='vertical bar plot')
par(mfrow=c(1,1))
##from which state maximum people committed to take suicide(by poison)?
tab7_1<-tab3[,c(2,24:34)]
tab7_1
tab7_2<-tab7_1[which.max(tab7$By.Poison..Total._Total),]
tab7_2

#Question5------------------------------
#Find the correlation between touching electric wire & by other means?--------------------------
tab13_1<-tab3[,59]
tab13_1
tab13_2<-tab3[,63]
tab13_2
plot(tab13_1,tab13_2,main=('Touching electric wire &By other means male category'),col=c("green"),xlab=" frequency of Touching electric wire",ylab=" frequency other means")
cor(tab13_1,tab13_2)
##
tab14_1<-tab3[,60]
tab14_1
tab14_2<-tab3[,64]
tab14_2
plot(tab14_1,tab14_2,main=('Touching electric wire &By other means _female'),col=("orange"),xlab=" freq. of Touching electric wire",ylab=" freq.other means")
boxplot(tab14_1,tab14_2,main=('Touching electric wire &By other means _female'),col=("orange"),sub="box plot")
cor(tab14_1,tab14_2)
###
tab15_1<-tab3[,61]
tab15_1
tab15_2<-tab3[,65]
tab15_2
plot(tab15_1,tab15_2,main=('Touching electric wire &By other means _trans'))
boxplot(tab15_1,tab15_2,main=('Touching electric wire &By other means _trans'))
cor(tab15_1,tab15_2)
##
tab16_1<-tab3[,62]
tab16_1
tab16_2<-tab3[,66]
tab16_2
plot(tab16_1,tab16_2,main=('Touching electric wire &By other means _total'),col="green")
cor(tab16_1,tab16_2)

#Question6----------------------------- 
##Distinguish Total accidental death and suicide in West Bengal

tab17<-tab3[29,c(2,6,10,14,18,22,26,38,42,58,62,66)]
tab17
tab17_1<-tab17[1,c(2:12)]
tab17_1
tab18<-apply(tab17_1,2,sum)
tab18
tab18_0<-c(round(prop.table(tab18)*100,2),"%")
tab18_0
tab18_1<as.numeric(round(prop.table(tab18)*100,2))
tab18_1
f<-barplot(tab18,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='West bengal total accidental death and suicide',sub="Barplot",ylim=c(0,6000))
f
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab18)+120,
     paste0(as.numeric(round(prop.table(tab18)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Question7---------------------
#Delhi accidental death and suicide bar plot
tab19<-tab3[34,c(2,6,10,14,18,22,26,38,42,58,62,66)]
tab19
tab19_1<-tab19[1,c(2:12)]
tab19_1
tab20<-apply(tab19_1,2,sum)
tab20
tab20_0<-c(round(prop.table(tab20)*100,2),"%")
tab20_0
tab20_1<-as.numeric(round(prop.table(tab20)*100,2),"%")
tab20_1
g<-barplot(tab20,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='Delhi total accidental death and suicide',sub="Bar Plot",ylim=c(0,6000))
g
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab20)+120,
     paste0(as.numeric(round(prop.table(tab20)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))

cor(tab18_1,tab20_1)
### Difference between the state West Bengal and Delhi accidental death & suicide
par(mfrow=c(1,1))
pie(tab18,col=c("violet","green","blue","yellow","orange","red","white","grey","red","purple","black"),radius=1,main=('West Bengal Total Accidental Death And Suicide Total'),sub=('West bengal pie chart'))
pie(tab20,col=c("violet","green","blue","yellow","orange","red","white","grey","red","purple","black"),radius=1,main=('Delhi  Total Accidental Death And Suicide Total pie chart'),sub=('Delhi pie chart'))

#Question8----------------------
##Find the gender wise total accidental death and suicide in India?
tab23<-tab3[,c(67,68,69)]
tab23
tab24<-apply(tab23[,c(1,2,3)],2,sum)
tab24
tab24_0<-c(round(prop.table(tab24)*100,2),"%")
tab24_0
pie(tab24,col=c("green","red","black"),radius=1.0,main=('Gender wise Total Accidental Death And Suicide'),clockwise=F,init.angle = 90,sub="pie chart")
barplot(tab24,ylab='count',ylim=c(0,100000),col=c("red","green","black"),main=(" Gender Wise Total Accidental Death And Suicide"))
#Gender wise accidental death and suicide in West Bengal----
tab3$State.UT
tab24_1<-tab3[29,c(67,68,69)]
tab24_1
tab24_2<-as.numeric(tab24_1)
tab24_2
tab24_3<-c(round(prop.table(tab24_1)*100,2),"%")
tab24_3
pie(tab24_2,col=c("green","red","black"),radius=1.0,main=('Gender wise Total Accidental Death And Suicide In West Bengal'),clockwise=F,init.angle = 90,sub="pie chart")
legend("bottomleft",legend=c("Male","Female","trans"),fill=c("green","red","black"))
#gender wise accidental death and suicide in PUDUCHERY
tab24_4<-tab3[36,c(67,68,69)]
tab24_4
tab24_5<-as.numeric(tab24_4)
tab24_5
tab24_6<-c(round(prop.table(tab24_4)*100,2),"%")
tab24_6
pie(tab24_5,col=c("green","red","black"),radius=1.0,main=('Gender wise Total Accidental Death And Suicide In PUDUCHERY'),clockwise=F,init.angle = 90,sub="pie chart")
legend("bottomleft",legend=c("Male","Female","trans"),fill=c("green","red","black"))
cor(tab24_5,tab24_2)
#question9-------------------
#Distinguish various type of total accidental death and suicide in India?

tab25<-tab3[,c(2,6,10,14,18,22,26,38,42,58,62,66)]
tab25
tab26<-apply(tab25[,c(2:12)],2,sum)
tab26
as.numeric(tab26)
tab26_0<-as.numeric(tab26)
tab26_0
tab26[which.max(tab26)]
tab26[which.min(tab26)]
tab26_1<-c(round(prop.table(tab26)*100,2),"%")
tab26_1
as.factor(tab26_1)
barplot(tab26_0,ylab='Frequency',ylim=c(0,80000),col=c("violet","green","blue","yellow","orange","red","white","grey","brown","green","yellow"),main=(" Various Type of Total Accidental Death And Suicide In India"),sub="vertical Barplot")
legend("topright",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","green","yellow"),cex=0.5,adj=c(0,0.5))

#Question10--------------------------------
#Find the correlation between sleeping pills of male and woman?
tab27<-tab3[,c(1,2,3,4)]
tab27
tab27_1<-tab27[,3]
tab27_1
tab27_2<-tab27[,4]
tab27_2
summary(tab27_1)
summary(tab27_2)
boxplot(tab27_1,main=('Sleeping Pills Man'),sub=('Box plot'),col="green")
boxplot(tab27_2,main=('Sleeping pills Woman'),sub=('Box plot'),col="blue")
plot(tab27_1,tab27_2,main=('Sleeping Pills Man And Woman'),col="red",xlab="Frequency of Sleeping Pills Man ",ylab="Frequency of Sleeping Pills Woman")
cor(tab27_1,tab27_2)
#Question11--------------------------
#Distinguish the various type of jumping of male?

tab28<-tab3[,c(43:52)]
tab28
tab28_1<-apply(tab28[,c(1,5,9)],2,sum)
tab28_1
tab28_0<-c(round(prop.table(tab28_1)*100,2),"%")
tab28_0

pie(tab28_1,col=c("green","red","blue"),main=('Jumping From Various Type Of Male'),clockwise=T,init.angle = 0,sub="pie chart")
labels=paste0(round(prop.table(tab28_1)*100,2),"%")
#Question12----------------------
#Distinguish the various type of jumping of Female?
tab28<-tab3[,c(43:52)]
tab28
tab28_2<-apply(tab28[,c(2,6,10)],2,sum)
tab28_2
tab28_00<-c(round(prop.table(tab28_2)*100,2),"%")
tab28_00
pie(tab28_2,col=c("green","red","yellow"),main=('Jumping From Various Type Female'),clockwise=T,init.angle = 0)
barplot(tab28_2,col=c("green","red","yellow"),main=('Jumping From Various Type Female'),ylim=c(0,300))
#Question13-------------------------------
#Find the number of state & Union territory?
tab29<-tab3[,1]
tab29
tab29_1<-table(tab29)
tab29_1
pie(tab29_1,col=c("red","green"),main=("state &ut"),sub="pie chart")
barplot(tab29_1,col=c("red","green"),main=("state &ut"),ylim=c(0,30))
#Question14----------------------------
#which state had the maximum number of drowning,self immolation,firearms gender wise?
tab30<-tab3[,c(1,2,7:18)]
tab30
which.max(tab30$By.Drowning_Male)
which.max(tab30$By.Drowning_Female)
tab30[which.max(tab30$By.Drowning_Male),c(2,3)]
tab30[which.max(tab30$By.Drowning_Female),c(2,4)]
#Ans:Maharashtra
#####################################################
which.max(tab30$By.Fire.Self.Immolation_Male)
tab30[which.max(tab30$By.Fire.Self.Immolation_Male),c(2,7)]
which.max(tab30$By.Fire.Self.Immolation_Female)
tab30[which.max(tab30$By.Fire.Self.Immolation_Female),c(2,8)]
#Ans:Tamil Nadu
#####################################################
which.max(tab30$By.Firearms_Male)
tab30[which.max(tab30$By.Firearms_Male),c(2,11)]
which.max(tab30$By.Firearms_Female)
tab30[which.max(tab30$By.Firearms_Female),c(2,12)]
#Ans: UTTAR PRADESH
#Question15--------------------
#a# Distinguish Kerala accidental death and suicide?
tab33<-tab3[13,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab33
tab33_1<-tab33[,c(2:12)]
tab33_1
tab33_2<-apply(tab33_1,2,sum)
tab33_2
tab33_3<-c(round(prop.table(tab33_2)*100,2),"%")
tab33_3
tab33_4<-as.numeric(round(prop.table(tab33_2)*100,2))
tab33_4
h<-barplot(tab33_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='Kerala Total Accidental Death And Suicide',sub="Bar plot",ylim=c(0,7000))
h
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab33_2)+120,
     paste0(as.numeric(round(prop.table(tab33_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by hanging total
#b#Distinguish ANDHRA PRADESH ACCIDENTAL DEATH AND SUICIDE 
tab3$State.UT
tab33_5<-tab3[1,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab33_5
tab33_6<-tab33[,c(2:12)]
tab33_6
tab33_7<-apply(tab33_6,2,sum)
tab33_7
tab33_8<-c(round(prop.table(tab33_7)*100,2),"%")
tab33_8
tab33_9<-as.numeric(round(prop.table(tab33_7)*100,2))
tab33_9
h<-barplot(tab33_7,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('ANDHRA PRADESH Total Accidental Death And Suicide'),ylim=c(0,7000),sub="vertical bardiagram")
h
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab33_7)+190,
     paste0(as.numeric(round(prop.table(tab33_7)*100,2)),"%"))
legend("topright",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging total

#c#Distinguish ARUNACHAL PRADESH Accidental Death And Suicide
tab43<-tab3[2,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab43
tab43_1<-tab43[,c(2:12)]
tab43_1
tab43_2<-apply(tab43_1,2,sum)
tab43_2
tab43_3<-c(round(prop.table(tab43_2)*100,2),"%")
tab43_3
tab43_4<-as.numeric(round(prop.table(tab43_2)*100,2))
tab43_4
j<-barplot(tab43_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='Arunachal Pradesh Total Accidental Death And Suicide',sub='barplot',ylim=c(0,500))
j
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab43_2)+50,
     paste0(as.numeric(round(prop.table(tab43_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging Total
#d#Distinguish Assam  Accidental Death And Suicide
tab44<-tab3[3,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab44
tab44_1<-tab44[,c(2:12)]
tab44_1
tab44_2<-apply(tab44_1,2,sum)
tab44_2
tab44_3<-c(round(prop.table(tab44_2)*100,2),"%")
tab44_3
tab44_4<-as.numeric(round(prop.table(tab44_2)*100,2))
tab44_4
j<-barplot(tab44_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='Assam Total Accidental Death And Suicide',sub='Vertical BarDiagram',ylim=c(0,2000))
j
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab44_2)+50,
     paste0(as.numeric(round(prop.table(tab44_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by hanging total
#D#Distinguish CHHATTISGARH Accidental Death and suicide
tab45<-tab3[4,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab45
tab45_1<-tab45[,c(2:12)]
tab45_1
tab45_2<-apply(tab45_1,2,sum)
tab45_2
tab45_3<-c(round(prop.table(tab45_2)*100,2),"%")
tab45_3
tab45_4<-as.numeric(round(prop.table(tab45_2)*100,2))
tab45_4
j<-barplot(tab45_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='CHHATTISGARH Total Acidental Death And Suicide',sub="Vertical bar diagram",ylim=c(0,2000))
j
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab45_2)+50,
     paste0(as.numeric(round(prop.table(tab45_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging total
#e#GOA Accidental Death And Suicide
tab46<-tab3[6,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab46
tab46_1<-tab46[,c(2:12)]
tab46_1
tab46_2<-apply(tab46_1,2,sum)
tab46_2
tab46_3<-c(round(prop.table(tab46_2)*100,2),"%")
tab46_3
tab46_4<-as.numeric(round(prop.table(tab46_2)*100,2))
tab46_4
k<-barplot(tab46_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('GOA total accidental death and suicide'),sub="Vertical Bardiagram",ylim=c(0,2000))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab46_2)+50,
     paste0(as.numeric(round(prop.table(tab46_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging
#e# GUJRAT Accidental death and suicide 
tab47<-tab3[7,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab47
tab47_1<-tab47[,c(2:12)]
tab47_1
tab47_2<-apply(tab47_1,2,sum)
tab47_2
tab47_3<-c(round(prop.table(tab47_2)*100,2),"%")
tab47_3
tab47_4<-as.numeric(round(prop.table(tab47_2)*100,2))
tab47_4
l<-barplot(tab47_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('Gujrat Total Accidental Death And Suicide'),sub="Vertical BarDiagram",ylim=c(0,6000))
l
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab47_2)+100,
     paste0(as.numeric(round(prop.table(tab47_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#ANS:By hanging total
#f# HARYANA Accidental Death And Suicide
tab48<-tab3[8,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab48
tab48_1<-tab48[,c(2:12)]
tab48_1
tab48_2<-apply(tab48_1,2,sum)
tab48_2
tab48_3<-c(round(prop.table(tab48_2)*100,2),"%")
tab48_3
tab48_4<-as.numeric(round(prop.table(tab48_2)*100,2))
tab48_4
k<-barplot(tab48_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('HARYANA Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,2000))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab48_2)+50,
     paste0(as.numeric(round(prop.table(tab48_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by other means
#g#HIMACHAL PRADESH Accidental Death And suicide
tab49<-tab3[9,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab49
tab49_1<-tab49[,c(2:12)]
tab49_1
tab49_2<-apply(tab49_1,2,sum)
tab49_2
tab49_3<-c(round(prop.table(tab49_2)*100,2),"%")
tab49_3
tab49_4<-as.numeric(round(prop.table(tab49_2)*100,2))
tab49_4
M<-barplot(tab49_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('HIMACHAL PRADESH Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,1000))
M
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab49_2)+50,
     paste0(as.numeric(round(prop.table(tab49_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By poison total
#h#JAMMU & KASHMIR Accidental Death And Suicide 

tab46<-tab3[10,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab46
tab46_1<-tab46[,c(2:12)]
tab46_1
tab46_2<-apply(tab46_1,2,sum)
tab46_2
tab46_3<-c(round(prop.table(tab46_2)*100,2),"%")
tab46_3
tab46_4<-as.numeric(round(prop.table(tab46_2)*100,2))
tab46_4
k<-barplot(tab46_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('JAMMU&KASHMIR Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,500))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab46_2)+50,
     paste0(as.numeric(round(prop.table(tab46_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by poison total
#n#JHARKHAND Accidental death and suicide
tab46<-tab3[11,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab46
tab46_1<-tab46[,c(2:12)]
tab46_1
tab46_2<-apply(tab46_1,2,sum)
tab46_2
tab46_3<-c(round(prop.table(tab46_2)*100,2),"%")
tab46_3
tab46_4<-as.numeric(round(prop.table(tab46_2)*100,2))
tab46_4
k<-barplot(tab46_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('JHARKHAND Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,1500))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab46_2)+50,
     paste0(as.numeric(round(prop.table(tab46_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by hanging total
#o# KARNATAK accidental death and suicide
tab46<-tab3[12,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab46
tab46_1<-tab46[,c(2:12)]
tab46_1
tab46_2<-apply(tab46_1,2,sum)
tab46_2
tab46_3<-c(round(prop.table(tab46_2)*100,2),"%")
tab46_3
tab46_4<-as.numeric(round(prop.table(tab46_2)*100,2))
tab46_4
k<-barplot(tab46_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('Karnatak Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,7000))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab46_2)+120,
     paste0(as.numeric(round(prop.table(tab46_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#ans:By hanging total
#q# MADHYA PRADESH Accidental death and suicide
tab49<-tab3[14,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab49
tab49_1<-tab49[,c(2:12)]
tab49_1
tab49_2<-apply(tab49_1,2,sum)
tab49_2
tab49_3<-c(round(prop.table(tab49_2)*100,2),"%")
tab49_3
tab49_4<-as.numeric(round(prop.table(tab49_2)*100,2))
tab49_4
M<-barplot(tab49_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('MADHYA PRADESH Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,8000))
M
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab49_2)+150,
     paste0(as.numeric(round(prop.table(tab49_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging total
 #r#MAHARASTRA Accidental death and suicide
tab46<-tab3[15,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab46
tab46_1<-tab46[,c(2:12)]
tab46_1
tab46_2<-apply(tab46_1,2,sum)
tab46_2
tab46_3<-c(round(prop.table(tab46_2)*100,2),"%")
tab46_3
tab46_4<-as.numeric(round(prop.table(tab46_2)*100,2))
tab46_4
k<-barplot(tab46_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('MAHARASHTRA Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,8000))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab46_2)+150,
     paste0(as.numeric(round(prop.table(tab46_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging total
#######################################################
#a1# Distinguish MANIPUR accidental death and suicide?
tab33<-tab3[16,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab33
tab33_1<-tab33[,c(2:12)]
tab33_1
tab33_2<-apply(tab33_1,2,sum)
tab33_2
tab33_3<-c(round(prop.table(tab33_2)*100,2),"%")
tab33_3
tab33_4<-as.numeric(round(prop.table(tab33_2)*100,2))
tab33_4
h<-barplot(tab33_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='Manipur Total Accidental Death And Suicide',sub="Bar plot",ylim=c(0,100))
h
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab33_2)+10,
     paste0(as.numeric(round(prop.table(tab33_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by hanging total
#b1#Distinguish MEGHALAYA ACCIDENTAL DEATH AND SUICIDE 
tab3$State.UT
tab33_5<-tab3[17,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab33_5
tab33_6<-tab33[,c(2:12)]
tab33_6
tab33_7<-apply(tab33_6,2,sum)
tab33_7
tab33_8<-c(round(prop.table(tab33_7)*100,2),"%")
tab33_8
tab33_9<-as.numeric(round(prop.table(tab33_7)*100,2))
tab33_9
h<-barplot(tab33_7,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('MEGHALAYA Total Accidental Death And Suicide'),ylim=c(0,200),sub="vertical bardiagram")
h
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab33_7)+15,
     paste0(as.numeric(round(prop.table(tab33_7)*100,2)),"%"))
legend("topright",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging total

#c1#Distinguish MIZORAM Accidental Death And Suicide
tab43<-tab3[18,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab43
tab43_1<-tab43[,c(2:12)]
tab43_1
tab43_2<-apply(tab43_1,2,sum)
tab43_2
tab43_3<-c(round(prop.table(tab43_2)*100,2),"%")
tab43_3
tab43_4<-as.numeric(round(prop.table(tab43_2)*100,2))
tab43_4
j<-barplot(tab43_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='MIZORAM Total Accidental Death And suicide',sub='barplot',ylim=c(0,100))
j
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab43_2)+5,
     paste0(as.numeric(round(prop.table(tab43_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging Total
#d1#Distinguish NAGALAND  Accidental Death And Suicide
tab44<-tab3[19,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab44
tab44_1<-tab44[,c(2:12)]
tab44_1
tab44_2<-apply(tab44_1,2,sum)
tab44_2
tab44_3<-c(round(prop.table(tab44_2)*100,2),"%")
tab44_3
tab44_4<-as.numeric(round(prop.table(tab44_2)*100,2))
tab44_4
j<-barplot(tab44_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='NAGALAND Total Accidental Death And Suicide',sub='Vertical BarDiagram',ylim=c(0,1600))
j
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab44_2)+80,
     paste0(as.numeric(round(prop.table(tab44_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by hanging total
#D1#Distinguish ODISHA Accidental Death and suicide
tab45<-tab3[20,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab45
tab45_1<-tab45[,c(2:12)]
tab45_1
tab45_2<-apply(tab45_1,2,sum)
tab45_2
tab45_3<-c(round(prop.table(tab45_2)*100,2),"%")
tab45_3
tab45_4<-as.numeric(round(prop.table(tab45_2)*100,2))
tab45_4
j<-barplot(tab45_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main='ODISHA Total Acidental Death And Suicide',sub="Vertical bar diagram",ylim=c(0,5000))
j
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab45_2)+100,
     paste0(as.numeric(round(prop.table(tab45_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By by poisson total
#e1#PUNJAB Accidental Death And Suicide
tab46<-tab3[21,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab46
tab46_1<-tab46[,c(2:12)]
tab46_1
tab46_2<-apply(tab46_1,2,sum)
tab46_2
tab46_3<-c(round(prop.table(tab46_2)*100,2),"%")
tab46_3
tab46_4<-as.numeric(round(prop.table(tab46_2)*100,2))
tab46_4
k<-barplot(tab46_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('Punjab total accidental death and suicide'),sub="Vertical Bardiagram",ylim=c(0,2000))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab46_2)+50,
     paste0(as.numeric(round(prop.table(tab46_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:By hanging
#e1# RAJASTHAN Accidental death and suicide 
tab47<-tab3[22,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab47
tab47_1<-tab47[,c(2:12)]
tab47_1
tab47_2<-apply(tab47_1,2,sum)
tab47_2
tab47_3<-c(round(prop.table(tab47_2)*100,2),"%")tab47_4<-as.numeric(round(prop.table(tab47_2)*100,2))
tab47_3
tab47_4
l<-barplot(tab47_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('Rajasthan Total Accidental Death And Suicide'),sub="Vertical BarDiagram",ylim=c(0,3000))
l
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab47_2)+100,
     paste0(as.numeric(round(prop.table(tab47_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#ANS:By hanging total

#f1# SIKKIM Accidental Death And Suicide
tab48<-tab3[23,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab48
tab48_1<-tab48[,c(2:12)]
tab48_1
tab48_2<-apply(tab48_1,2,sum)
tab48_2
tab48_3<-c(round(prop.table(tab48_2)*100,2),"%")
tab48_3
tab48_4<-as.numeric(round(prop.table(tab48_2)*100,2))
tab48_4
k<-barplot(tab48_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),main=('Sikim Total Accidental Death And Suicide'),sub="Vertical Bardiagram",ylim=c(0,300))
k
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab48_2)+50,
     paste0(as.numeric(round(prop.table(tab48_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","black"),cex=0.6,adj=c(0,0.5))
#Ans:by hanging total


#Question16-----------------
# situation of BIHAR accidental death and suicide 
tab34<-tab3[4,c(2,6,10,14,18,22,26,38,42,58,62,66)]       
tab34
tab34_1<-tab34[,c(2:12)]
tab34_1
tab34_2<-apply(tab34_1,2,sum)
tab34_2
tab34_3<-c(round(prop.table(tab34_2)*100,2),"%")
tab34_3
tab35_4<-as.numeric(round(prop.table(tab34_2)*100,2))
tab35_4
i<-barplot(tab34_2,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","green","yellow"),main=('Bihar total accidental death and suicide total bar plot'),ylim=c(0,2000))
i
text(c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7),
     as.numeric(tab34_2)+120,
     paste0(as.numeric(round(prop.table(tab34_2)*100,2)),"%"))
legend("topleft",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","green","yellow"),cex=0.6,adj=c(0,0.5))
barplot(tab34_2,ylim=c(0,1000),col=c("violet","green","blue","yellow","orange","red","white","grey","brown","purple","yellow"),main=('Bihar Accidental Death And Suicide'),sub="vertical Bardiagram")

#Ans:by hanging total
############
tab2$State.UT
tab3$State.UT
tab3[30,c(1,2)]###indicate particular category state
####ANDHRA PRADESH:BY POISON IS HIGH
####HARIYANA : BY OTHER MEANS TOTAL
####HIMACHAL PRADESH :BY POISON TOTAL
####JAMMU & KASHMIR :BY POISON TOTAL
#### ODISHA: BY POISON TOTAL
####TELANGANA: BY POISON TOTAL
####UTTARAKHAND:BY POISON TOTAL
####other state had maximum in by hanging total

#Question 17-----------------------------
#last:sorted list of total accidental death and suicide in India state wise ascending order

tab21<-tab3[,c(2,70)]
tab21
tab22<-tapply(tab21$State.UT,tab21$Total_Total,sort)
tab22

tab22_1<-data.frame(tab22)
tab22_1
i<-c(1:36)
i
f<-cbind(tab22_1,i)
f
hist(tab21$Total_Total,breaks=seq(0,20000,by=4000),main='Total Accidental Death And Suicide In India',sub="Histrogram",xlab='total accidental death and suicide range',ylab='No of states and UT',xlim=c(0,20000),ylim=c(0,26),col=c("violet","green","blue","yellow","orange","red","white","grey"),border='red')
tab40<-tab3[,c(2,70)]
tab40
prop.table(tab40$Total_Total)
d1<-prop.table(tab40$Total_Total)*100
d1
#percentage of Accidental death and suicide of every state or union territory
cbind(tab3[,2],d1)
tab22_1
ggplot(tab3,aes(x=State.UT,
                y=Total_Total),
       fill=State.UT)+geom_bar(position = "dodge",stat = "identity")
############################################################################

#Question18---------------------
##By.self.Inflicting.Injury_the maximum people committed to take suicide from which state?  

tab8<-tab3[,c(1,2,35,36,37,38)]
tab8
which.max(tab8$By.Self.Inflicting..Injury_Male)
tab8[which.max(tab8$By.Self.Inflicting..Injury_Male),]

tab8_1<-apply(tab8[,c(3,4,5)],2,sum)
tab8_1
c(round(prop.table(tab8_1)*100,2),"%")
pie(tab8_1,col=c("orange","green","blue"),radius=0.9,main=('Accidental Death By Self Inflecting InJury Gender wise'),sub="Pie chart",labels=paste0(as.numeric(round(prop.table(tab8_1)*100,2)),"%"))
legend("bottomleft",legend=c("Male","Female","Trans"),fill=c("orange","green","blue"))
#Question19---------------------
#Which state had the minimum number of accidental death(By.Coming.under.Running.Vehicles.Trains)?

tab9<-tab3[,c(1,2,55,56,57,58)]
tab9
which.max(tab9$By.Coming.under.Running.Vehicles.Trains_Total)
tab9[which.max(tab9$By.Coming.under.Running.Vehicles.Trains_Total),]
which.min(tab9$By.Coming.under.Running.Vehicles.Trains_Total)
tab9[which.min(tab9$By.Coming.under.Running.Vehicles.Trains_Total),]

tab9_1<-tab9[,c(3,4,5)]
tab9_1
tab9_2<-apply(tab9_1[,c(1,2,3)],2,sum)
tab9_2
barplot(tab9_2,ylab='count',ylim=c(0,3000),col=c("green","red","yellow"),main=(' Accidental Death By Coming under Running Vehicles Trains'))

#Question20---------------------
#Draw the pie chart of By.Drowning_Total, By.Fire.Self.Immolation_Total& By.Firearms_Total?

tab10<-tab3[,c(1,2,10,14,18)]
tab10
tab10_1<-apply(tab10[,c(3,4,5)],2,sum)
tab10_1
pie(tab10_1,col=c("green","red","black"),radius=0.9,main=('Accidental Death And Suicide drowning fire self firearms'),sub="pie chart",lockwise = T)

#Question21-------------------
# sort the state wise By_Hanging_Total in accidental death and suicide in India?

tab11<-tab3[,c(1,2,22)]
tab11
tapply(tab11[,2],tab11[,3],sort)
tapply(tab11$State.UT,tab11$By.Hanging_Total,sort)

#Question22-----------
#In which state are maximum and minimum number of hanging total?


tab12<-tab3[,c(1,2,19,20,21,22)]
tab12
tab12_1<-tab12[,c(1,2,6)]
tab12_1
tab12_2<-tab12_1[which.max(tab12_1$By.Hanging_Total),]
tab12_2

tab12_3<-tab12_1[which.min(tab12_1$By.Hanging_Total),]
tab12_3
##sort the hanging _total
tapply(tab12$State.UT,tab12$By.Hanging_Total,sort)

###bar plot
tab12_3<-apply(tab12[,c(3,4,5)],2,sum)
tab12_3
barplot(tab12_3,ylab='count',ylim=c(0,60000),col=c("red","green","black"),main=('By_Hanging_Total'))
##Question23-----------------------
# Distinguish the various type of accidental death and suicide in India trans? 
tab31<-tab3[,c(2,5,9,13,17,21,25,37,41,57,61,65)]
tab31
tab31_1<-apply(tab31[,c(2:12)],2,sum)
tab31_1
pie(tab31_1,col=c("violet","green","blue","yellow","orange","red","white","grey","brown","green","yellow"),main=('accidental death and suicide in India trans'))
barplot(tab31_1,col=c("red","green","orange"),ylab="count",ylim=c(0,15),main=("Accidental death and suicide in India trans"))
#question24-----------------
#compute region wise accidental death and suicide? 
tab32<-tab3[,c(2,70)]
tab32
apply(tab32[2],2,sum)
NE<-tab32[c(2,3,16,17,18,19,23,26),c(1,2)]
NE
S<-tab32[c(12,13,15,24,25,30,36),c(1,2)]
S
E<-tab32[c(1,4,11,20,29),c(1,2)]
E
NW<-tab32[c(8,21,22,34),c(1,2)]
NW
N<-tab32[c(9,10,27,28),c(1,2)]
N
W<-tab32[c(6,7,31,32,33,35),c(1,2)]
W
M<-tab32[c(5,14),c(1,2)]
M
NE_1<-apply(NE[2],2,sum)
NE_1
S_1<-apply(S[2],2,sum)
S_1
E_1<-apply(E[2],2,sum)
E_1
NW_1<-apply(NW[2],2,sum)
NW_1
N_1<-apply(N[2],2,sum)
N_1
W_1<-apply(W[2],2,sum)
W_1
M_1<-apply(M[2],2,sum)
M_1
sum(NE_1,S_1,E_1,NW_1,N_1,W_1,M_1)
v<-c(NE_1,S_1,E_1,NW_1,N_1,W_1,M_1)
v
barplot(v,xlab=c("NE_1","S_1","E_1","NW_1","N_1","W_1","M_1"),ylab=('Frequency'),col=c("violet","red","blue","green","yellow","red","purple"),main=("Region wise Accidental Death And Suicide In India"))
legend("topright",legend=c("NE_1","S_1","E_1","NW_1","N_1","W_1","M_1"),fill=c("violet","red","blue","green","yellow","red","purple"),cex=0.5)
#Ans:south region had maximum number of accidental death

###
tab35<-tab3$Total_Total
tab35
i<-1:36
i

####which state or UT had range wise of accidental death and suicide?

table(tab35[i]<4000)
table(tab35[i]>4000&tab35[i]<8000)
table(tab35[i]>8000&tab35[i]<12000)
table(tab35[i]>16000)




tab3[which(tab35[i]>16000),2]
as.factor(tab3[which(tab35[i]>16000),2])

tab3[which(tab35[i]>8000&tab35[i]<12000),2]
as.factor(tab3[which(tab35[i]>8000&tab35[i]<12000),2])

tab3[which(tab35[i]>4000&tab35[i]<8000),2]
as.factor(tab3[which(tab35[i]>4000&tab35[i]<8000),2])

tab3[which(tab35[i]<4000),2]
as.factor(tab3[which(tab35[i]<4000),2])


#question25-----------------
#correlation between Male and female by consuming sleeping sleeping pills
plot(tab3[,3],tab3[,4],col="blue",xlab='consuming sleeping pills male',ylab='consuming sleeping pills female',main="scatter plot of consuming pills of male and female")
boxplot(tab3[,3],tab3[,4],col=c("red","blue"))
summary(tab3[,3])
summary(tab3[,4])
cor(tab3[,3],tab3[,4])

##Question26----------------------
#summary total accidental death and suicide in India 
summary(tab3[,70])
boxplot(tab3[,70],main="Accidentaldeath And suicide In India",col="yellow",sub="Box plot",ylab="frequency")

#Question27--------------------
#correlation between BIHAR Accidental death and CHHATTISGARH Accidental death
d<-tab3[4,c(6,10,14,18,22,26,38,42,58,62,66)]
d
d_1<-as.numeric(d)
d_1

e<-tab3[5,c(6,10,14,18,22,26,38,42,58,62,66)]
e
e_1<-as.numeric(e)
e_1
plot(d_1,e_1,col="green",xlab="BIHAR Accidental Death Total",ylab="CHHATTISGARH Accidental death total")
cor(d_1,e_1)
##Question28-----------------------
#check the west Bengal gender wise accidental death and suicide
q<-tab3[,c(2,70)]
q
prop.table(q[,2])
cbind(q,prop.table(q[,2]))
sum(tab3$Total_Total)

q1<-sum(tab3$Total_Total)*0.0910345522
q1
r<-tab3[29,c(67,68,69)]
r
prop.table(r)*q1
#Question 29-------------------
#Distinguish between fire self immolation and firearms
tab51<-tab3[,c(11:18)]
tab51
tab51_1<-tab51[,c(1,2,3)]
tab51_1
tab51_2<-apply(tab51_1[1:3],2,sum)
tab51_2
par(mfrow=c(1,2))
pie(tab51_2,col=c("black","green","red"),radius=0.8,clockwise = T,main=('Accidental Death And Suicide By Fire self Immolation'),labels=paste0(round(100*tab51_2/sum(tab51_2),2),"%"),sub="pie chart")
legend("topright",legend=c("Male","Female","Trans"),fill=c("black","green","red"),cex=0.5)
#Ans:Female accidental death and suicide is maximum in Fire self immolation
tab52<-tab51[,c(5,6,7)]
tab52
tab52_1<-apply(tab52,2,sum)
tab52_1
pie(tab52_1,col=c("black","green","red"),radius=0.8,clockwise = T,main=('Accidental Death And Suicide By Fire Arms'),labels=paste0(round(100*tab51_2/sum(tab51_2),2),"%"),sub="pie chart")
legend("topright",legend=c("Male","Female","Trans"),fill=c("black","green","red"),cex=0.5)
par(mfrow=c(1,1))
#
tab53<-tab3$By.Fire.Self.Immolation_Female
tab53
tab54<-tab3$By.Firearms_Female
tab54
plot(tab53,tab54,col="green",xlab="Frequency of Female By Fire Self Immolation",ylab="Frequency of Female by firearms",main="scatter plot")
cor(tab53,tab54)
#question30-----------------
#total accidental death Male 
tab25<-tab3[,c(2,3,7,11,15,19,23,35,39,55,59,63)]
tab25
tab26<-apply(tab25[,c(2:12)],2,sum)
tab26
as.numeric(tab26)
tab26_0<-as.numeric(tab26)
tab26_0
tab26[which.max(tab26)]
tab26[which.min(tab26)]
tab26_1<-c(round(prop.table(tab26)*100,2),"%")
tab26_1
as.factor(tab26_1)
barplot(tab26_0,ylab='Frequency',ylim=c(0,80000),col=c("violet","green","blue","yellow","orange","red","white","grey","brown","green","yellow"),main=(" Various Type of Total Accidental Death And Suicide Male In India"),sub="vertical Barplot")
legend("topright",legend=c("Pill","Drown.","Fire","Firearm","Hang","poisson","Injury","jump","Trains","Wire","other"),fill=c("violet","green","blue","yellow","orange","red","white","grey","brown","green","yellow"),cex=0.5,adj=c(0,0.5))
#Question31------------------------
tab3_0<-read.csv("C:\\Users\\debaj\\Desktop\\NCRB_ADSI-2019_Table_2.13_state-ut.csv")
tab3_0

tab4_0<-tab3_0[-c(30,38,39),c(2,70,71)]
tab4_0

tab5_0<-tab4_0$Total_Total/tab4_0$Total.population
tab5_0
tab6_0<-cbind(tab4_0,tab5_0)
tab6_0
colnames(tab6_0)<-c("State.UT","Total_Total","Total.population","proportion Death")
tab6_0
cor(tab6_0$`proportion Death`,tab6_0$Total.population)
cor(tab6_0$`proportion Death`,tab6_0$Total_Total)

plot(tab6_0$Total.population,tab6_0$`proportion Death`,col="blue",main="Plot Between Proportion Death vs Total Population")
plot(tab6_0$Total_Total,tab6_0$`proportion Death`,col="red",main="Plot Between Proportion Death vs Total Accidental Death")
     