#Name: Jon (me)
#Purpose: Tommy John Surgery Project
#Last Update: 08/15/20
#-------------------------------------#
setwd("/Users/jonzimmerman/Desktop/Data Projects/Tommy John Surgery Project/app")

#install.packages("readxl")
library(readxl)
library(dplyr)
library(survival)
#install.packages("survminer")
library(survminer)

#LOAD THE DATASETS
#---------------------------------------------------------------------------------#
#Dataset 1: TJ Surgery Data
TJ=as.data.frame(read_excel("TJ.xlsx",sheet=1))
#Dataset 2: Master Pitching Data
p_data=as.data.frame(read_excel("pitching_data.xlsx",sheet=1))
#Dataset 3: Salary Data
s_data=as.data.frame(read_excel("s_data.xlsx",sheet=1))
#Dataset 4: Postseason Pitching Data
post_data=as.data.frame(read_excel("post_data.xlsx",sheet=1))
#Dataset 5: Names
names=read.csv(url("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/People.csv"))

#---------------------------------------------------------------------------------#

dim(TJ)
#1906   24
dim(p_data)
#30815    33
dim(s_data)
#26428    5
dim(post_data)
#5798   30
dim(names)
#20060    24

#Remove Columns from Salary Data
s_data$lgID=NULL
names(s_data)[names(s_data) == "playerID"] <- "PlayerID"


#Pitching Data
#if unique playerID has multiple teams per year, sum statistics, use last team
p_data = as.data.frame(p_data %>% group_by(PlayerID,yearID) %>%
    summarise(teamID=last(teamID),
              W = sum(W, na.rm = T),
              L = sum(L, na.rm = T), 
              G = sum(G, na.rm = T),
              GS = sum(GS, na.rm = T), 
              CG = sum(CG, na.rm = T),
              SHO = sum(SHO, na.rm = T), 
              SV = sum(SV, na.rm = T),
              IPouts = sum(IPouts, na.rm = T), 
              H = sum(H, na.rm = T),
              ER = sum(ER, na.rm = T), 
              HR = sum(HR, na.rm = T),
              BB = sum(BB, na.rm = T), 
              SO = sum(SO, na.rm = T),
              BAOpp = sum(BAOpp, na.rm = T), 
              ERA = sum(ERA, na.rm = T), 
              IBB = sum(IBB, na.rm = T), 
              WP = sum(WP, na.rm = T), 
              HBP = sum(HBP, na.rm = T), 
              BK = sum(BK, na.rm = T), 
              BFP = sum(BFP, na.rm = T), 
              GF = sum(GF, na.rm = T), 
              R = sum(R, na.rm = T),
              SH = sum(SH, na.rm = T), 
              SF = sum(SF, na.rm = T), 
              GIDP = sum(GIDP, na.rm = T)
              
              ))


head(p_data)
dim(p_data)

#Join Salary Data onto Pitching Data
p_data=merge(x=p_data,y=s_data,by=c("PlayerID","yearID","teamID"),all.x=TRUE)
dim(p_data)
#28340    29



#----------------------------------------------#
TJ_data=subset(TJ,TJ$Position=="P")
dim(TJ_data)
#1725   24

TJ_data=subset(TJ_data,TJ_data$Level=="MLB")
dim(TJ_data)
#451    24


#Get rid of columns in TJ data you have no use for
colnames(TJ_data)
TJ_data$FirstName=NULL
TJ_data$LastName=NULL
TJ_data$P_ID=NULL
TJ_data$Team=NULL
TJ_data$High_School=NULL
TJ_data$Age=NULL
TJ_data$mlbamid=NULL
TJ_data$fgid=NULL

TJ_data=TJ_data[!duplicated(TJ_data$PlayerID), ]
dim(TJ_data)
#422  16




#Join the TJ data onto the larger dataset (pitching)
#Join datasets on playerID
#bb_data=merge(x=p_data,y=TJ_data,by="PlayerID",all.x=TRUE)

TJ=TJ[!duplicated(TJ$PlayerID), ]


bb_data=merge(x=p_data,y=TJ,by="PlayerID",all.x=TRUE)
dim(p_data)
#28340  29
dim(TJ_data)
#451  16
dim(bb_data)
#28340  74

#Sort data
bb_data=bb_data[with(bb_data, order(bb_data$PlayerID, bb_data$yearID)),]
head(bb_data)


#---------Clean Up Data---------#
bb_data$IP=bb_data$IPouts/3
bb_data$TJ_ID=ifelse(is.na(bb_data$TJ_Surgery_Date)==FALSE,1,0)
bb_data$Repeat_Surgery=ifelse(is.na(bb_data$Repeat)==FALSE,1,0)
bb_data$TJ_Surgery_Date=as.Date(bb_data$TJ_Surgery_Date)
bb_data$Return_Date_same_level=as.Date(bb_data$Return_Date_same_level)
bb_data$PercGS=bb_data$GS/bb_data$G
bb_data$Starter_Reliever=ifelse(bb_data$PercGS>=0.5,"Starter","Reliever")



bb_data$IPouts=NULL
bb_data$PlayerID_0=NULL
bb_data$Match_from_TJdata=NULL
bb_data$length=NULL
bb_data$Repeat=NULL
bb_data$Post_TJ_MLB_G=NULL
bb_data$Post_TJ_MLB_IP_PA=NULL
bb_data$Ipouts_Post=NULL
bb_data$NoDups_NotTouched=NULL
bb_data$Active=NULL

dim(bb_data)
#28340  45



#Designate Before/After Surgery
bb_data$Year_Surgery=substr(as.Date(bb_data$TJ_Surgery_Date),1,4)
bb_data$Before_After=ifelse(bb_data$yearID<=bb_data$Year_Surgery,0,1)
bb_data$Before_After=ifelse(is.na(bb_data$Before_After)==TRUE,0,bb_data$Before_After)

bb_data$PlayerID_BA_IND=paste0(bb_data$PlayerID,"_",bb_data$Before_After)

bb_data2 = as.data.frame(bb_data %>% group_by(PlayerID,TJ_ID) %>%
                            summarise(W_AvgBA = mean(W, na.rm = T),
                                      L_AvgBA = mean(L, na.rm = T), 
                                      G_AvgBA = mean(G, na.rm = T),
                                      GS_AvgBA = mean(GS, na.rm = T), 
                                      CG_AvgBA = mean(CG, na.rm = T),
                                      SHO_AvgBA = mean(SHO, na.rm = T), 
                                      SV_AvgBA = mean(SV, na.rm = T),
                                      IP_AvgBA = mean(IP, na.rm = T), 
                                      H_AvgBA = mean(H, na.rm = T),
                                      ER_AvgBA = mean(ER, na.rm = T), 
                                      HR_AvgBA = mean(HR, na.rm = T),
                                      BB_AvgBA = mean(BB, na.rm = T), 
                                      SO_AvgBA = mean(SO, na.rm = T),
                                      BAOpp_AvgBA = mean(BAOpp, na.rm = T), 
                                      ERA_AvgBA = mean(ERA, na.rm = T), 
                                      IBB_AvgBA = mean(IBB, na.rm = T), 
                                      WP_AvgBA = mean(WP, na.rm = T), 
                                      HBP_AvgBA = mean(HBP, na.rm = T), 
                                      BK_AvgBA = mean(BK, na.rm = T), 
                                      BFP_AvgBA = mean(BFP, na.rm = T), 
                                      GF_AvgBA = mean(GF, na.rm = T), 
                                      R_AvgBA = mean(R, na.rm = T),
                                      SH_AvgBA = mean(SH, na.rm = T), 
                                      SF_AvgBA = mean(SF, na.rm = T), 
                                      GIDP_AvgBA = mean(GIDP, na.rm = T),
                                      
                                    
                                      Salary_MedBA = median(salary,na.rm=T)
                  

                                      
                            ))

#Join the before and after averages back on to dataset
bb_data=merge(x=bb_data,y=bb_data2,by=c("PlayerID","TJ_ID"),all.x=TRUE)
dim(bb_data)
#28340    74
dim(bb_data2)
#5809     28



table(bb_data$TJ_ID)
#0      1
#25271  3069
#10.8%


test=subset(bb_data,bb_data$PlayerID=="balesco01")
dim(test)
#6  74
test


#------------ How Many Minor Leaguers with TJ surgery made the jump to ML -------------#
dim(bb_data)
table(is.na(bb_data$Return_Date_same_level))

advanced=subset(bb_data,bb_data$Year_Surgery<bb_data$yearID & bb_data$Level!="MLB" & TJ_ID==1)
dim(advanced)
#1272   56

advanced=advanced[!duplicated(advanced$PlayerID), ]
dim(advanced)
#335    56
advanced

#--------------------------------------------------------------------------------------#



modeldata=subset(bb_data,bb_data$Before_After==0)
dim(modeldata)
#27180   74


#Compare Stats Before and After
only_TJs=subset(bb_data,bb_data$TJ_ID==1)
dim(only_TJs)
#3069   125


library(gridExtra)

#Before and After Performance
plot1 <- ggplot(aes(x=factor(Before_After), y=SO_AvgBA,group=factor(Starter_Reliever),color=factor(Starter_Reliever)), data=bb_data) + 
  stat_summary(fun="mean", geom='line', 
               aes(group=Starter_Reliever,
                   color=factor(Starter_Reliever)))+
  scale_x_discrete(breaks=c("0","1"),
                       labels=c("Before", "After"))



plot1

Start=subset(bb_data,bb_data$Starter_Reliever=="Starter")
Relief=subset(bb_data,bb_data$Starter_Reliever=="Reliever")

Start_B=subset(Start,Start$Before_After==0 & Start$TJ_ID==1)
Start_A=subset(Start,Start$Before_After==1 & Start$TJ_ID==1)

Relief_B=subset(Relief,Relief$Before_After==0 & Relief$TJ_ID==1)
Relief_A=subset(Relief,Relief$Before_After==1 & Relief$TJ_ID==1)

Start_ttest=subset(bb_data,bb_data$Starter_Reliever=="Starter" & bb_data$TJ_ID==1)
Relief_ttest=subset(bb_data,bb_data$Starter_Reliever=="Reliever" & bb_data$TJ_ID==1)


require(gridExtra)
a=aggregate(TJ$PlayerID, by=list(Category=TJ$Year_Surgery), FUN=count)
a=as.data.frame(table(TJ$Year_Surgery))



p1 <- ggplot(Start_B, aes(IP_AvgBA)) + 
  geom_histogram(fill = "white", color = "grey30")

p2 <- ggplot(Start_A, aes(IP_AvgBA)) + 
  geom_histogram(fill = "white", color = "grey30")
grid.arrange(p1,p2)

p3 <- ggplot(Relief_B, aes(IP_AvgBA)) + 
  geom_histogram(fill = "white", color = "grey30")
p4 <- ggplot(Relief_A, aes(IP_AvgBA)) + 
  geom_histogram(fill = "white", color = "grey30")
grid.arrange(p3,p4)


tt1=t.test(IP_AvgBA~Before_After,data=Start_ttest)
tt2=t.test(IP_AvgBA~Before_After,data=Relief_ttest)
tt1$p.value
tt2$p.value
a=tt1$estimate

names(a)[names(a) == "mean in group 0"] <- "Mean Before"
names(a)[names(a) == "mean in group 1"] <- "Mean After"
a


bb_data3 = as.data.frame(bb_data %>% group_by(PlayerID,Before_After) %>%
                           summarise(W_AvgBA = mean(W, na.rm = T),
                                     L_AvgBA = mean(L, na.rm = T), 
                                     G_AvgBA = mean(G, na.rm = T),
                                     GS_AvgBA = mean(GS, na.rm = T), 
                                     CG_AvgBA = mean(CG, na.rm = T),
                                     SHO_AvgBA = mean(SHO, na.rm = T), 
                                     SV_AvgBA = mean(SV, na.rm = T),
                                     IP_AvgBA = mean(IP, na.rm = T), 
                                     H_AvgBA = mean(H, na.rm = T),
                                     ER_AvgBA = mean(ER, na.rm = T), 
                                     HR_AvgBA = mean(HR, na.rm = T),
                                     BB_AvgBA = mean(BB, na.rm = T), 
                                     SO_AvgBA = mean(SO, na.rm = T),
                                     BAOpp_AvgBA = mean(BAOpp, na.rm = T), 
                                     ERA_AvgBA = mean(ERA, na.rm = T), 
                                     IBB_AvgBA = mean(IBB, na.rm = T), 
                                     WP_AvgBA = mean(WP, na.rm = T), 
                                     HBP_AvgBA = mean(HBP, na.rm = T), 
                                     BK_AvgBA = mean(BK, na.rm = T), 
                                     BFP_AvgBA = mean(BFP, na.rm = T), 
                                     GF_AvgBA = mean(GF, na.rm = T), 
                                     R_AvgBA = mean(R, na.rm = T),
                                     SH_AvgBA = mean(SH, na.rm = T), 
                                     SF_AvgBA = mean(SF, na.rm = T), 
                                     GIDP_AvgBA = mean(GIDP, na.rm = T),
                                    
                                     Salary_MedBA = median(salary,na.rm=T),
                                     TJ_Status= first(TJ_ID),
                                     MostRecentYear = last(yearID),
                                     MostRecentTeam=last(teamID),
                                     MostRecentSt_Re=last(Starter_Reliever)
    
                           ))
dim(bb_data3)
#6102   32
table(bb_data3$TJ_ID)
#5401   701
table(bb_data3$MostRecentYear==2019)
#831
colnames(bb_data3)
#options("scipen"=-100, "digits"=6)

patients=subset(bb_data,bb_data$TJ_ID==1)
dim(patients)
#3069   127


dim(names)
colnames(names)
names2 = subset(names, select = c(playerID,nameFirst,nameLast,throws,weight,height))
names2$FullName=paste0(names$nameFirst," ",names$nameLast)
dim(names2)
#20060    7
names(names2)[names(names2) == "playerID"] <- "PlayerID"



bb_model_data=merge(x=bb_data3,y=names2,by=c("PlayerID"),all.x=TRUE)
dim(bb_data3)
#6105    32
dim(names2)
#20021    5
dim(bb_model_data)
#6105    36

test=subset(bb_model_data,bb_model_data$PlayerID=="carpech01")


bb_model_data2=subset(bb_model_data,bb_model_data$Before_After==0)

dim(bb_model_data2)

test=subset(bb_model_data2,bb_model_data2$PlayerID=="scherma01")
test=subset(bb_model_data2,bb_model_data2$PlayerID=="carpech01")

dim(test)


#options(digits=4)

library(rpart)
treemod=rpart(TJ_Status~IP_AvgBA+BAOpp_AvgBA
                +I(MostRecentSt_Re),data=bb_model_data,method="class",
              control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
printcp(treemod)
plotcp(treemod)
rsq.rpart(treemod)
print(treemod)
summary(treemod)
plot(treemod)
par(mfrow=c(1,1))
text(treemod)
prune(treemod,cp=0.002)

mod1=glm(TJ_Status~W_AvgBA+G_AvgBA+IP_AvgBA+H_AvgBA+BB_AvgBA+SO_AvgBA+BAOpp_AvgBA+
           HBP_AvgBA+BFP_AvgBA+I(MostRecentSt_Re),data=bb_model_data,family=binomial(link="logit"))
summary(mod1)
#(output <- summary(mod1)$coefficients)
#as.data.frame(apply(output, 2, formatC, format="f", digits=4))
#setNames(data.frame(output[,-4], formatC(output[,4], format="f", digits=4)), colnames(output))


#Get the prediction
predict=predict(mod1,type='response')
bb_model_data$prediction=predict
summary(bb_model_data$prediction)
x=summary(bb_model_data$prediction)


#Force decimals predictions to be either 0 or 1
bb_model_data$prediction.update=ifelse(bb_model_data$prediction>=0.5,1,0)
table(bb_model_data$prediction.update)




#Confusion Matrix
cm=with(bb_model_data,table(TJ_Status,prediction.update))
cm
sum(cm)
#6102
#(1-misclassifcation error)
1-((cm[1,2]+cm[2,1])/sum(cm))
#88.4%

bb_data_2019=subset(bb_model_data,bb_model_data$MostRecentYear==2019)
table(bb_data_2019$TJ_Status,bb_data_2019$prediction.update)

risk=(bb_data_2019[which(bb_data_2019$prediction.update==1 & bb_data_2019$TJ_Status==0),])
risk$FullName
risk$prediction
riskdata=as.data.frame(cbind(as.character(risk$FullName),round(as.numeric(risk$prediction),2)))
riskdata=riskdata[with(riskdata, order(riskdata$V2,decreasing = T)),]
colnames(riskdata)=c("Player Name","Risk Probability")
riskdata


#a=summary(mod1)
#capture.output(print(a, digits=4, cutoff=.3, sort=TRUE), file="temp.txt")
#cat(readLines("temp.txt")[10:34], sep="\n")


#------ Repeat Surgery ------#
dim(TJ)
#1906   26
colnames(TJ)
table(is.na(TJ$Surgeon_s))
#676    1230
Surgeon_Data = subset(TJ, select = c(Player,Team,Level,Position,Throws,TJ_Surgery_Date,
                                     Return_Date_same_level,Recovery_Time_months,Surgeon_s))
dim(Surgeon_Data)
#1906   9
Surgeon_Data2=subset(Surgeon_Data,is.na(Surgeon_Data$Surgeon_s)==FALSE)
dim(Surgeon_Data2)
#676    9

stats=as.data.frame(table(Surgeon_Data2$Surgeon_s))
colnames(stats)=c("Surgeon_s","Num_Surgs")

Surgeon_Data3=merge(x=Surgeon_Data2,y=stats,by=c("Surgeon_s"),all.x=TRUE)
dim(Surgeon_Data2)
#676  9
dim(stats)
#42   2
dim(Surgeon_Data3)
#676  10


Surgeon_Data3=Surgeon_Data3[with(Surgeon_Data3, order(Surgeon_Data3$Player,Surgeon_Data3$TJ_Surgery_Date)),]
head(Surgeon_Data3)


Surgeon_Data3$ones <- 1   # create a vector of 1's
Surgeon_Data3 <- transform(Surgeon_Data3, Count_Surgs_Per_Player = ave(ones, Player, FUN=cumsum)) # get counts
Surgeon_Data3$ones <- NULL # delete vector of 1's previously created
Surgeon_Data3  # check results

SD=Surgeon_Data3
test=subset(SD,substr(as.character(SD$TJ_Surgery_Date),6,11)!="01-01" & 
               substr(as.character(SD$Return_Date_same_level),6,11)!="01-01")
dim(test)
#467  11




SD$KnownRecoveryTime=is.na(SD$Recovery_Time_months)
table(SD$KnownRecoveryTime)
#445    231

KnownTime=subset(SD,SD$KnownRecoveryTime==FALSE)
table(KnownTime$Recovery_Time_months)
plot(table(KnownTime$Recovery_Time_months))

ggplot(KnownTime$Recovery_Time_months) + geom_bar(stat="count",aes(y = KnownTime$Surgeon_s))
class(KnownTime)

dim(SD)
head(SD)
survdata=as.data.frame(cbind(SD$SurgeonName,SD$Recovery_Time_months,SD$KnownRecoveryTime))
survdata$Censored=ifelse(survdata$V3==TRUE,0,1)
colnames(survdata)=c("Surgeon","Recovery_Time","T_F","Censored")

#----- How to measure success rate for Doctors -----#
SD2=Surgeon_Data2

SD2$ReturnCounter=ifelse(is.na(SD2$Return_Date_same_level)==FALSE,1,0)
table(SD2$ReturnCounter,SD2$Surgeon_s)

#----------- Recovery ---------#

dim(TJ)
#1906   26

table(is.na(TJ$Recovery_Time_months))
#FALSE    TRUE
#1483      423

#FALSE - not missing
#TRUE - missing


TJK=subset(TJ,substr(as.character(TJ$TJ_Surgery_Date),6,11)!="01-01" & 
            substr(as.character(TJ$Return_Date_same_level),6,11)!="01-01")

dim(TJK)
#1017   26

table(is.na(TJK$Recovery_Time_months))
#FALSE
#1017


range(TJK$Recovery_Time_months)
summary(TJK$Recovery_Time_months)
plot(TJK$Recovery_Time_months)

TJK$KTrange=ifelse(TJK$Recovery_Time_months<=12,"1 Year or Less",ifelse(
     TJK$Recovery_Time_months>12 & TJK$Recovery_Time_months<=18, "1 Year to 1.5 Years",  ifelse(
     TJK$Recovery_Time_months>18 & TJK$Recovery_Time_months<=24, "1.5 Years to 2 Years", ifelse(
     TJK$Recovery_Time_months>24 & TJK$Recovery_Time_months<=30, "2 Years to 2.5 Years", ifelse(
     TJK$Recovery_Time_months>30 & TJK$Recovery_Time_months<=36, "2.5 Years to 3 Years", "3+ Years"
               )))))

table(TJK$KTrange)

table(TJK$KTrange,TJK$Position)

#table(TJK$KTrange,TJK$Team)
table(TJK$KTrange,TJK$Throws)
table(TJK$KTrange,TJK$Level2)

#--------- Repeat Surgery ----------#

dim(TJ)
#1906   26
length(unique(TJ$RepeatID))

table(TJ$RepeatID)
table(TJ$SurgNum)

length(unique(TJ$PlayerID))

S1=subset(TJ,TJ$SurgNum==1)
S2=subset(TJ,TJ$SurgNum==2)
S3=subset(TJ,TJ$SurgNum==3)

S1_Full=subset(S1,select=c(Player,TJ_Surgery_Date,Team,Level,Position,Throws, Return_Date_same_level,Recovery_Time_months,SurgNum))
S2_Full=subset(S2,select=c(Player,TJ_Surgery_Date,Team,Level,Position,Throws, Return_Date_same_level,Recovery_Time_months,SurgNum))
S3_Full=subset(S3,select=c(Player,TJ_Surgery_Date,Team,Level,Position,Throws, Return_Date_same_level,Recovery_Time_months,SurgNum))

S_Full=subset(TJK,select=c(Player,TJ_Surgery_Date,Team,Level,Position,Throws, Return_Date_same_level,Recovery_Time_months,SurgNum,KTrange))

library(ggplot2)
library(reshape2)
ggplot(data=TJK, aes(x=KTrange)) + geom_bar(stat="count")
datatable(S3_Full[,1:dim(S3_Full)[2]-1],rownames=FALSE, options = list(pageLength=10,lengthChange = FALSE))

dt<-data.frame(
  group= c("Mütter über Söhne", "Mütter über Töchter", "Väter über Söhne", "Väter über Töchter"),
  category=c("category1","category1","category1","category1",    "category2","category2","category2","category2"),
  ratio= c(0.50, 0.53, 0.49, 0.47,  0.51, 0.54, 0.46, 0.49))

head(dt)
dt=as.data.frame(table(S_Full$KTrange,S_Full$SurgNum))
library(ggplot2)

myColors1 <- c("#ff3db7", "#ff3db7", "#4699dd")
myColors2 <- c("#ff3db7", "#4699dd", "#4699dd")
myColors3 <- c("#4699dd", "#4699dd", "#4699dd")

ggplot(data=dt, aes(x=Var1, y=Freq, fill=factor(Surg1))) + geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values=c("#56B4E9", "#E69F00", "#E69F00"))


data(mtcars)
df=as.data.frame(table(mtcars$carb,mtcars$gear))
df$C3=ifelse(df$Var2==3,1,0)
df$C4=ifelse(df$Var2==4,1,0)
df$C5=ifelse(df$Var2==5,1,0)


switch(input$rb,
       
       "3"=ggplot(data=df, aes(x=Var1, y=Freq, fill=factor(C3))) + geom_bar(stat="identity", position=position_dodge())+
         scale_fill_brewer(palette="Paired")+geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25),
       "4"=ggplot(data=df, aes(x=Var1, y=Freq, fill=factor(C4))) + geom_bar(stat="identity", position=position_dodge())+
         scale_fill_brewer(palette="Paired")+geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25),
       "5"=ggplot(data=df, aes(x=Var1, y=Freq, fill=factor(C5))) + geom_bar(stat="identity", position=position_dodge())+
         scale_fill_brewer(palette="Paired")+geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25))
})
