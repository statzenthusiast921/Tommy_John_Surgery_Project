# Name: Jon (me)
# Date: 08.18.2020
# Purpose: Create Tommy John Surgery Analysis App 
#-------------------------------------------------#
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(recipes)
library(survival)
library(survminer)
library(scales)



#Dataset 1: TJ Surgery Data
TJ=as.data.frame(read_excel("TJ.xlsx",sheet=1))
#Dataset 2: Master Pitching Data
p_data=as.data.frame(read_excel("pitching_data.xlsx",sheet=1))
#Dataset 3: Salary Data
s_data=as.data.frame(read_excel("s_data.xlsx",sheet=1))
#Dataset 4: Names
names=read.csv(url("https://raw.githubusercontent.com/chadwickbureau/baseballdatabank/master/core/People.csv"))




TJ$Level2=ifelse(TJ$Level %in% c("A","A-","A+","AA","AAA","Rk"),"MiLB",ifelse(
                 TJ$Level %in% c("MLB"), "MLB",ifelse(
                 TJ$Level %in% c("HS"), "High School", "College")))

LevelChoices=c("A","A-","A+","AA","AAA","Coll","HS", "MLB","Rk")
Level2Choices=c("MLB","MiLB","College","High School")

TJ$Year_Surgery=substr(as.Date(TJ$TJ_Surgery_Date),1,4)

StatsChoices=c("Avg_Wins","Avg_Losses","Avg_Games_Played","Avg_Innings_Pitched",
               "Avg_Hits_Allowed","Avg_Earned_Runs","Avg_Home_Runs_Allowed","Avg_Walks",
               "Avg_Strikeouts","Avg_Opposing_Batting_Avg","Avg_Earned_Run_Avg",
               "Avg_Wild_Pitches","Avg_Batters_Hit_By_Pitch","Avg_Batters_Faced")

SurgeonChoices=c("Dr. James Andrews","Dr. Lewis Yocum","Dr. Neal ElAttrache","Dr. David Altchek",
                 "Dr. Keith Meister","Dr. Timothy Kremchek","Dr. Frank Jobe")



#--------- Preprocess Data ---------#

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

#Join Salary Data onto Pitching Data
p_data=merge(x=p_data,y=s_data,by=c("PlayerID","yearID","teamID"),all.x=TRUE)
dim(p_data)
#28340    29

#----------------------------------------------#
TJ_data=subset(TJ,TJ$Position=="P")
dim(TJ_data)
#1725   26

TJ_data=subset(TJ_data,TJ_data$Level=="MLB")
dim(TJ_data)
#451    26


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
#422  18




#Join the TJ data onto the larger dataset (pitching)
#Join datasets on playerID

bb_data=merge(x=p_data,y=TJ_data,by="PlayerID",all.x=TRUE)


#Sort data
bb_data=bb_data[with(bb_data, order(bb_data$PlayerID, bb_data$yearID)),]




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


#Designate Before/After Surgery
bb_data$Year_Surgery=substr(as.Date(bb_data$TJ_Surgery_Date),1,4)
bb_data$Before_After=ifelse(bb_data$yearID<=bb_data$Year_Surgery,0,1)
bb_data$Before_After=ifelse(is.na(bb_data$Before_After)==TRUE,0,bb_data$Before_After)

bb_data$PlayerID_BA_IND=paste0(bb_data$PlayerID,"_",bb_data$Before_After)

bb_data2 = as.data.frame(bb_data %>% group_by(PlayerID,Before_After) %>%
                                   summarise(Avg_Wins = mean(W, na.rm = T),
                                             Avg_Losses = mean(L, na.rm = T), 
                                             Avg_Games_Played = mean(G, na.rm = T),
                                             Avg_Games_Started = mean(GS, na.rm = T), 
                                             Avg_Complete_Games = mean(CG, na.rm = T),
                                             Avg_Shutouts = mean(SHO, na.rm = T), 
                                             Avg_Saves = mean(SV, na.rm = T),
                                             Avg_Innings_Pitched = mean(IP, na.rm = T), 
                                             Avg_Hits_Allowed = mean(H, na.rm = T),
                                             Avg_Earned_Runs = mean(ER, na.rm = T), 
                                             Avg_Home_Runs_Allowed = mean(HR, na.rm = T),
                                             Avg_Walks = mean(BB, na.rm = T), 
                                             Avg_Strikeouts = mean(SO, na.rm = T),
                                             Avg_Opposing_Batting_Avg = mean(BAOpp, na.rm = T), 
                                             Avg_Earned_Run_Avg = mean(ERA, na.rm = T), 
                                             Avg_Intentional_Walks = mean(IBB, na.rm = T), 
                                             Avg_Wild_Pitches = mean(WP, na.rm = T), 
                                             Avg_Batters_Hit_By_Pitch = mean(HBP, na.rm = T), 
                                             Avg_Balks = mean(BK, na.rm = T), 
                                             Avg_Batters_Faced = mean(BFP, na.rm = T), 
                                             Avg_Games_Finished = mean(GF, na.rm = T), 
                                             Avg_Runs_Allowed = mean(R, na.rm = T),
                                             Avg_Sacrifices_Allowed = mean(SH, na.rm = T), 
                                             Avg_Sacrifice_Flys_Allowed = mean(SF, na.rm = T), 
                                             Avg_Double_Plays_Induced = mean(GIDP, na.rm = T),
                                             
                                             
                                             Salary_MedBA = median(salary,na.rm=T),
                                             TJ_Status= first(TJ_ID),
                                             MostRecentYear = last(yearID),
                                             MostRecentTeam=last(teamID),
                                             Starter_or_Reliever=last(Starter_Reliever)
                                   ))




names2 = subset(names, select = c(playerID,nameFirst,nameLast,throws,height,weight))
names2$FullName=paste0(names$nameFirst," ",names$nameLast)
names(names2)[names(names2) == "playerID"] <- "PlayerID"

#This is the dataset for the model
bb_model_data=merge(x=bb_data2,y=names2,by=c("PlayerID"),all.x=TRUE)
bb_model_data=subset(bb_model_data,bb_model_data$Before_After==0)

#Join the before and after averages back on to dataset
bb_data=merge(x=bb_data,y=bb_data2,by=c("PlayerID","Before_After"),all.x=TRUE)
bb_data$Before_After=as.factor(bb_data$Before_After)
patients=subset(bb_data,bb_data$TJ_ID==1 & bb_data$IP>=25)



Start=subset(bb_data,bb_data$Starter_Reliever=="Starter" & bb_data$IP>=25)
Relief=subset(bb_data,bb_data$Starter_Reliever=="Reliever" & bb_data$IP>=25)

Start_ttest=subset(bb_data,bb_data$Starter_Reliever=="Starter" & bb_data$TJ_ID==1 & bb_data$IP>=25)
Relief_ttest=subset(bb_data,bb_data$Starter_Reliever=="Reliever" & bb_data$TJ_ID==1 & bb_data$IP>=25)


Start_B=subset(Start,Start$Before_After==0 & Start$TJ_ID==1)
Start_A=subset(Start,Start$Before_After==1 & Start$TJ_ID==1)

Relief_B=subset(Relief,Relief$Before_After==0 & Relief$TJ_ID==1)
Relief_A=subset(Relief,Relief$Before_After==1 & Relief$TJ_ID==1)





TJK=subset(TJ,substr(as.character(TJ$TJ_Surgery_Date),6,11)!="01-01" & 
             substr(as.character(TJ$Return_Date_same_level),6,11)!="01-01")

TJK$KTrange=ifelse(TJK$Recovery_Time_months<=12,"1 Year or Less",ifelse(
                   TJK$Recovery_Time_months>12 & TJK$Recovery_Time_months<=18, "1 Year to 1.5 Years",  ifelse(
                   TJK$Recovery_Time_months>18 & TJK$Recovery_Time_months<=24, "1.5 Years to 2 Years", ifelse(
                   TJK$Recovery_Time_months>24 & TJK$Recovery_Time_months<=30, "2 Years to 2.5 Years", ifelse(
                   TJK$Recovery_Time_months>30 & TJK$Recovery_Time_months<=36, "2.5 Years to 3 Years", "3+ Years"
      )))))

S1=subset(TJK,TJK$SurgNum==1)
S2=subset(TJK,TJK$SurgNum>=2)


S1_Full=subset(S1,select=c(Player,TJ_Surgery_Date,Team,Level,Position,Throws,Return_Date_same_level,Recovery_Time_months,KTrange))
S2_Full=subset(S2,select=c(Player,TJ_Surgery_Date,Team,Level,Position,Return_Date_same_level,Throws,Recovery_Time_months,KTrange))

S1_Full$TJ_Surgery_Date=as.Date(S1_Full$TJ_Surgery_Date)
S2_Full$TJ_Surgery_Date=as.Date(S2_Full$TJ_Surgery_Date)

S1_Full$Return_Date_same_level=as.Date(S1_Full$Return_Date_same_level)
S2_Full$Return_Date_same_level=as.Date(S2_Full$Return_Date_same_level)

names(S1_Full)[names(S1_Full) == 'Recovery_Time_months'] <- 'Recovery Time (months)'
names(S2_Full)[names(S2_Full) == 'Recovery_Time_months'] <- 'Recovery Time (months)'


S1_Full=subset(S1_Full,substr(as.character(S1_Full$TJ_Surgery_Date),6,11)!="01-01" & 
                 substr(as.character(S1_Full$Return_Date_same_level),6,11)!="01-01")

S2_Full=subset(S2_Full,substr(as.character(S2_Full$TJ_Surgery_Date),6,11)!="01-01" & 
                 substr(as.character(S2_Full$Return_Date_same_level),6,11)!="01-01")


names(S1_Full)[names(S1_Full) == 'TJ_Surgery_Date'] <- 'Date of TJ Surgery'
names(S2_Full)[names(S2_Full) == 'TJ_Surgery_Date'] <- 'Date of TJ Surgery'

names(S1_Full)[names(S1_Full) == 'Return_Date_same_level'] <- 'Return Date'
names(S2_Full)[names(S2_Full) == 'Return_Date_same_level'] <- 'Return Date'

S_Full=subset(TJK,select=c(Player,TJ_Surgery_Date,Team,Level,Position,Throws, Return_Date_same_level,Recovery_Time_months,SurgNum,KTrange))
S_Full$SurgNum2=ifelse(S_Full$SurgNum==1,1,2)


# Define UI for application
ui = fluidPage(
  navbarPage("Tommy John Surgery Analysis",
             tabPanel("Welcome",
                      tabName = "welcome",
                      icon=icon("door-open"),
                      
                      fluidPage(theme=shinytheme("united"),
                                h1("Welcome to my Tommy John Surgery Shiny Dashboard!"),
                                br(),
                                p(strong(tags$u("What is the Tommy John Surgery?"))),
                                p(a("Tommy John Surgery", href = "https://en.wikipedia.org/wiki/Ulnar_collateral_ligament_reconstruction"), 
                                  "or ulnar collateral ligament (UCL) reconstruction surgery is a surgical graft procedure where the",a("ulnar collateral
                                  ligament", href="https://en.wikipedia.org/wiki/Ulnar_collateral_ligament_of_elbow_joint"),"in the medial elbow is replaced
                                  with either a tendon from elsewhere in the patient's body, or with 
                                  a tendon from a dead donor. The procedure is common among professional athletes, 
                                  particularly pitchers in Major League Baseball."), 
                                p("The UCL can become stretched, frayed, or torn through the repetitive stress of a throwing motion. Thus, Major League 
                                  Baseball players are at a higher risk of injury vs. professional athletes of other sports that involve less repetitive 
                                  throwing."),
                                p("The procedure was first performed for professional baseball player, ", a("Tommy John",href="https://en.wikipedia.org/wiki/Tommy_John"),"in 1974.
                                He went on to have a successful post-surgery career earning 288 total career wins ranking seventh all-time for left-handed pitchers.  Over the past
                                  few decades, Tommy John surgery has become more common for Major League pitchers with generally positive results."),
                                p(strong(tags$u("What is the purpose of this analysis?"))),
                                p("I wanted to utilize",a("historical pitching statistics",href="https://github.com/chadwickbureau/baseballdatabank/tree/master/core"),"coupled with data on",
                                a("Tommy John Surgery patients",href="https://docs.google.com/spreadsheets/d/1gQujXQQGOVNaiuwSN680Hq-FDVsCwvN-3AazykOBON0/edit#gid=0"),"to determine the impacts 
                                of this surgery on the career of a Major League pitcher.  Only data from the 1963 baseball season and on were considered as this was Tommy John's first season 
                                as a MLB pitcher.  The Tommy John surgery data is up to date as of July 22nd, 2020."),
                                br(),
                                p(strong(tags$u("How can I use this dashboard?"))),
                                p("You can click on any of the tabs above to see a different analysis of the data.")
                                )),
             tabPanel("Frequency",
                      tabname="quickfacts",
                      icon=icon("chart-line"),
                      fluidRow(column(width=4,
                                      p(strong(tags$u("How common is this surgery?")),
                                      p("Tommy John Surgery was first performed in 1974 and has become more common each year."),
                                      br(),
                                      p("Of the total number of UCL surgeries recorded, 50% of them have been performed on Minor
                                        League players, 26% on Major League players, and the remaining 24% on College and High
                                        School players. An overwhelming majority of these players were pitchers (91%)."),
                                      br(),
                                      p("It is often believed that UCL surgeries may improve performance even for players without injuries and 
                                        could potentially help a Minor League player advance to the MLB level.  This might explain why the number of reported
                                        UCL surgeries for Minor League players is double that of Major League players.  However, according to",
                                        a("Ahmad et al",href="https://pubmed.ncbi.nlm.nih.gov/22759607/"),", a worrying percentage of players, coaches, 
                                        and parents hold misperceptions about this surgery, as no studies exist that support their beliefs.")
                                      )),
                               column(width=8,
                                      selectInput(inputId = "level_select",label="Make a selection:",choices=Level2Choices,selected=Level2Choices[1]),
                                      plotOutput("surg_freq")
                               ))),
             tabPanel("Performance",
                      tabname="performance",
                      icon=icon("baseball-ball"),
                      p(strong(tags$u("Does having Tommy John surgery affect a pitcher's performance?"))),
                      p("This tab analyzes a pitcher's performance by examining different metrics and then tests whether or not the difference before surgery vs. after surgery is statistically significant."),
                      br(),
                      
                      fluidRow(
                               column(width=4,
                                      fluidRow(selectInput(inputId = "stat_select",label="Make a selection:",choices=StatsChoices,selected=StatsChoices[1]),
                                      plotOutput("before_after")
                                      )),
                               column(width=4,align="center",
                                      p(strong("Pitching Metric Distributions")),
                                      br(),
                                      br(),
                                      plotOutput("Freq1234")),
                               column(width=4, 
                                      p(strong("Determine if Difference in Means is Statistically Significant")),
                                      br(),
                                      br(),
                                      p("The output below displays the means and p-value from the t-test for the selected metric among", strong("starting",style = "color:teal"),"pitchers."),
                                      verbatimTextOutput(outputId = "means1"),
                                      verbatimTextOutput(outputId = "ttest1"),
                                      
                                      p("The output below displays the means and p-value from the t-test for the selected metric among", strong("relief",style = "color:coral"), "pitchers."),
                                      verbatimTextOutput(outputId = "means2"),
                                      verbatimTextOutput(outputId = "ttest2"),
                                      p(strong("A p-value less than 0.05 indicates a statistically significant difference 
                                               between the two means."))
                               )),
                      
                               br(),
                               p(strong("Note: "),"Starters and relievers were identified by taking the number of games started per season and dividing by
                                        the number of games played per season.  If the resulting number was >=0.5, then a player was designated as a starter.
                                        Less than 0.5, a player was designated as a reliever.  Pitchers with less than 25 innings logged for a season were
                                        excluded from this analysis. ")
                      ),
             tabPanel("Risk Calculation",
                      tabname="risk",
                      icon=icon("calculator"),
                      
                            p(strong(tags$u("Is it possible to predict whether or not a pitcher will have Tommy John surgery?"))),
                            p("This tab utilizes regression analysis to determine what factors are more predictive of needing Tommy 
                                    John surgery and predicts which players are most at-risk of having the surgery in the future. "),
                      br(),
                      fluidRow(column(width=3,
                                       p("The model outputs probabilities (values between 0% and 100%).  Choose a threshold 
                                          probability to determine above what value is a player deemed 'at risk' for needing Tommy John surgery.")),
                               column(width=3,
                                       sliderInput(inputId = "threshold", label="Select threshold:",
                                                   min = 10, max = 50, value = 50, post="%")),
                               column(width=3,
                                      p("The value to the right represents the accuracy of the model predictions.  As the 
                                        threshold value decreases, the model accuracy decreases.")),
                               column(width=3,
                                      DTOutput(outputId = "model_accuracy"))),
                      br(),
                      
                  
                      fluidRow(
                         column(width=8,
                                p("Below is the output from a", a("logistic regression" , href="https://en.wikipedia.org/wiki/Logistic_regression"),"model 
                                   using the listed independent variables to predict whether or not a player will have Tommy John surgery.  If a p-value 
                                  (last column) is less than 0.05, that indicates the corresponding independent variable is statistically significant."),
                                    verbatimTextOutput(outputId = "LogRegOut")),
                         column(width=4,
                                p("The table below displays the players with the highest risk of needing Tommy John surgery in the future based on the predictions of the model."),
                                DTOutput(outputId = "Players_at_Risk"))),

                             
                      br(),
                      p(strong("Note: "),"The risk %s in the table are probabilities - not certainties.  For example, Shane Bieber has a 64.67% probability of needing Tommy John surgery, 
                              which means there is still a 35.33% probability he doesn't need the surgery.")
                      #tags$style("#Players_at_Risk {white-space: pre;height:56.5em; overflow:auto;}"),
                      ),
             tabPanel("Surgeons",
                      tabname="surgeons",
                      icon=icon("user-md"),
                      fluidRow(column(width=6,
                                   p(strong(tags$u("Does the specific surgeon have an effect on recovery?"))),
                                   p("This tab analyzes the frequency for which surgeons perform UCL surgery, how quickly or slowly these patients return to play, and the recovery rate 
                                     by level and position.")),
                              column(width=6,
                                   selectInput(inputId = "surgeon_select",label="Make a selection:",choices=SurgeonChoices,selected=SurgeonChoices[1]))),
                                     
                      br(),
                          fluidRow(
                              column(width=6,
                                    p("The plot below shows the number of surgeries performed by each surgeon.  Any doctor in this data performing fewer than 10 surgeries
                                       was grouped into the 'Other' category.  If the doctor was unknown, they were not included in this plot.  All levels and positions are included here."),
                                    plotOutput("surgeon_freq")),
                                 column(width=6,
                                        p("The plot below shows",a("Kaplan-Meier",href="https://en.wikipedia.org/wiki/Kaplan%E2%80%93Meier_estimator"),"curves, which are traditionally used for",a("survival analysis.",href="https://en.wikipedia.org/wiki/Survival_analysis"),"
                                           The two lines represent the probabilities a player will return from surgery at a given point in time for the selected surgeon.  Only surgeons with at least 30 surgeries performed are included here."),
                                        plotOutput("surv_plot"))),
                            br(),
                            fluidRow(column(width=6,
                                            strong(textOutput("table1_surg")),
                                            verbatimTextOutput("Level_Surgeon")),
                                     column(width=6,
                                            strong(textOutput("table2_surg")),
                                            verbatimTextOutput("Position_Surgeon"))),
                                
                                        br(),
                                        p(strong("Note:"),"In the survival plot, if the p-value is less than 0.05, then the difference between the two survival curves is statistically significant.  
                                           To illustrate, the difference between survival curves for Dr. James Andrews vs. all other surgeons is significant and since his curve 
                                          (red) is to the left of the blue curve at most time points, it is safe to conclude his patients return to play faster than patients 
                                          for all other surgeons.")
                      ),
             tabPanel("Recovery",
                      tabname="recovery",
                      icon=icon("thumbs-up"),
                      p(strong(tags$u("How much variability exists for the length of recovery time from Tommy John surgery?"))),
                      p("This tab analyzes the length of recovery time for TJ surgery patients.  Only data with players who returned to play were considered for this analysis."),
                      p("The chart below displays the length of time for a player to recover from TJ surgery and distinguishes between the 1st or 2nd and 3rd surgeries.  The 
                        table below displays descriptive information about each player at the time of their surgery."),
                      prettyRadioButtons(inputId = "rb", 
                                         label = "Make a selection:",
                                         c("1st Surgery","2nd or 3rd Surgery"),
                                         animation = "pulse",
                                         inline=TRUE),               
                      plotOutput("RecoveryRange"),
                      DTOutput("Repeats"),
                      br(),
                      p(strong("Note: "),"Only 2 players in this dataset have had the surgery more than twice.  
                                          Further, teams that have changed names or moved cities have all been 
                                          grouped under the current team city and/or name (eg: Players from the 
                                          Montreal Expos will show as playing for the Washington Nationals).")
                                  ),
  
             tabPanel("Conclusion",
                      tabname="conclusion",
                      icon=icon("info"),
                      fluidRow(column(width=12,
                                      p(strong(tags$u("Background"))),
                                      p("The purpose of this analysis was to evaluate the impacts of having Tommy 
                                        John surgery on MLB pitchers.  Two primary data sources were used: historical
                                        pitching statistics on every MLB pitcher and a comprehensive list of professional
                                        baseball athletes at all levels who have undergone Tommy John surgery.  Fully
                                        observed datasets were used whenever possible.  However, there were instances
                                        where this was not possible and as a result, only available data was utilized."))),
                      fluidRow(
                        column(width=12,
                               p(strong(tags$u("Analysis"))),
                               p("Tommy John surgery has become more common each year for professional baseball players as
                                  a reliable tool for repairing the ulnar collateral ligament in the medial elbow and returning
                                  to play.  A common misconception about this surgery is that it may improve performance or help
                                  a below-MLB athlete advance to the MLB level. Thus, it is important to view this tool not as an elective surgery to
                                 gain a competitive advantage, but as a means to mend and prolong a career."),
                               p("To determine if pitching performance was impacted by having the surgery, data was grouped into the seasons
                                 before surgery vs. after surgery.  Statistics were averaged across both time periods and the data was 
                                 separated to account for the differences in statistics between starting pitchers and relief pitchers.
                                 Welch's t-test was utilized to determine if the difference between averaged statistics from before to 
                                 after surgery were statistically significant.  Pitching metrics that showed a statistically significant decrease
                                 for both starters and relievers included wins, innings pitched,  hits, walks, and batters faced.  Both opposing
                                 batting average and earned run average showed statistically significant increases.  Further research would need
                                 to be taken to determine if the significant decreases were mostly due to the decrease in innings pitched."),
                               p("To predict the probability of a pitcher having Tommy John Surgery in the future, a logistic regression model
                                 was fit to the data.  Once predictions were obtained, data only from the 2019 season was kept as this was
                                 the last full season at the time of this analysis.  This model does not account for the extended layoff
                                 due to the COVID-19 pandemic, but assumes the 2020 season began as originally scheduled.  The model showed
                                 that innings pitched, games played, hits allowed, walks, strikeouts, opposing batting average, batters
                                 faced, and whether or not a pitcher was a starter or a reliever were all statistically significant and predictive
                                 of Tommy John surgery status.  Shane Bieber had the highest predicted probability of needing Tommy John surgery in
                                 the future."),
                               p("To analyze the effectiveness of the doctors performing the surgery, a Kaplan-Meier plot was constructed to evaluate
                                 if their patients' time to recovery was statistically significant as compared to other surgeons.  Dr. James Andrews performed the most Tommy John surgeries and his patients returned to play faster
                                 than patients of other doctors in this analysis.  Further, Dr. Andrews and Dr. Yocum have the highest recovery rates for
                                 patients at the MLB level and for pitchers regardless of level in this study with at least an 80% recovery rate for both groups. "),
                               p("Finally, it was of interest to understand the average length of time of recovery from Tommy John surgery for a
                                 professional baseball athlete.  Generally, most patients recover in 12 to 18 months and the majority recover
                                 in 2 years or fewer.")
                               )),
                      fluidRow(column(width=12,
                                      p(),
                                      p(strong(tags$u("Limitations"))),
                                      p("Limitations of this study include leaving out postseason data.  It is very likely that the extra stress on the elbow of
                                        pitching in several additional games in high pressure situations could be a huge factor in determining whether or not a pitcher
                                        will need Tommy John surgery.  Further, additional regression models could be utilized to better explain the relationships
                                        studied in this analysis.  A logistic regression model was chosen for its ease of interpretation, but a",
                                        a("tree-based model",href="https://en.wikipedia.org/wiki/Decision_tree_learning"),", ",
                                        a("neural network", href="https://en.wikipedia.org/wiki/Neural_network"),
                                        ", or", a("support vector machine",href="https://en.wikipedia.org/wiki/Support_vector_machine")," would all be sufficient alternatives."),
                                      p("Further, only completely observed data was utilized if there was a large percentage of missing data.  Thus, inferences can only be applied to the
                                        available data.  A little more than a third of the surgery data had a known doctor attached to the surgery.  Imputation was not considered as some
                                        doctors included in this analysis have passed away. 
                                        "))))))
                                        

                                      




# Define server logic 
server <- function(input, output) {
  
  
  #--------- Surgeries by Level and Year ----------#
  
  filtered_level=reactive({
    filter=subset(TJ,Level2==input$level_select)
    return(filter)
  })
  
  
  output$surg_freq=renderPlot({
    
    a=as.data.frame(table(filtered_level()$Year_Surgery))
    p=ggplot(a, aes(x=Var1, y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+theme_minimal()+theme(legend.position = "none",
                                                    axis.text.x = element_text(angle=45))+
    xlab("Year")+ylab("Number of Surgeries")+
    ggtitle(paste0("Number of UCL Surgeries Performed by Year at the ",input$level_select," level"))+
    theme(plot.title = element_text(hjust = 0.5,face="bold"))
    p
  
  })
  
  #----------------Performance Before vs. After Surgery----------------#
  
  output$before_after=renderPlot({

    plot1 = ggplot(aes_string(x=factor(patients$Before_After), y=input$stat_select), data=patients) + 
      stat_summary(fun="mean", geom='line', size=2,linetype="longdash",
                   aes_string(group=factor(patients$Starter_Reliever),
                                                             color=factor(patients$Starter_Reliever)))+
      scale_x_discrete(breaks=c("0","1"),
                       labels=c("Before Surgery", "After Surgery"))+
      theme(legend.title = element_blank())+
      ylab(input$stat_select)+xlab("")+
      ggtitle("Average Pitching Performance \n Before vs. After Surgery")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))
    plot1
  })
  
  
  output$Freq1234=renderPlot({ 
    p1 <- ggplot(Start_B, aes_string(input$stat_select)) + 
      geom_histogram(fill = "#00BFC4", color = "grey30")+
      ggtitle("Starters Before")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))+
      ylab("Count")
    
    p2 <- ggplot(Start_A, aes_string(input$stat_select)) + 
      geom_histogram(fill = "#00BFC4", color = "grey30")+
      ggtitle("Starters After")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))+
      scale_color_gradientn(colours = rainbow(5))+
      ylab("Count")
    p3 <- ggplot(Relief_B, aes_string(input$stat_select)) + 
      geom_histogram(fill = "#F8766D", color = "grey30")+
      ggtitle("Relievers Before")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))+
      ylab("Count")
    p4 <- ggplot(Relief_A, aes_string(input$stat_select)) + 
      geom_histogram(fill = "#F8766D", color = "grey30")+
      ggtitle("Relievers After")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"))+
      ylab("Count")
    grid.arrange(arrangeGrob(p1,p2, ncol=2),arrangeGrob(p3,p4, ncol=2))
    
    
  })
  

  output$means1 = renderPrint({ 
    tt1=t.test(as.formula(paste(input$stat_select,'~Start_ttest$Before_After')),data=Start_ttest)
    a=tt1$estimate
    names(a)[names(a) == "mean in group 0"] <- "Mean Before"
    names(a)[names(a) == "mean in group 1"] <- "Mean After"
    a
  })
  
  output$ttest1 = renderPrint({ 
    tt1=t.test(as.formula(paste(input$stat_select,'~Start_ttest$Before_After')),data=Start_ttest)
    tt1$p.value
    })
  
  output$means2 = renderPrint({ 
    tt2=t.test(as.formula(paste(input$stat_select,'~Relief_ttest$Before_After')),data=Relief_ttest)
    a=tt2$estimate
    names(a)[names(a) == "mean in group 0"] <- "Mean Before"
    names(a)[names(a) == "mean in group 1"] <- "Mean After"
    a
  })
    
  output$ttest2 = renderPrint({ 
    tt2=t.test(as.formula(paste(input$stat_select,'~Relief_ttest$Before_After')),data=Relief_ttest)
    tt2$p.value
    })

#---------------Logistic Regression Output---------------#

output$LogRegOut = renderPrint({
  mod1=glm(TJ_Status~Avg_Innings_Pitched+Avg_Games_Played+Avg_Hits_Allowed+
                     Avg_Walks+Avg_Strikeouts+Avg_Opposing_Batting_Avg+
                     Avg_Wild_Pitches+
                     Avg_Batters_Hit_By_Pitch+Avg_Batters_Faced+
                     Starter_or_Reliever+height+weight+(height*weight),
                     data = bb_model_data, family=binomial(link="logit"))
  
 a=summary(mod1)
 capture.output(print(a, digits=4, cutoff=.3, sort=TRUE), file="temp.txt")
 cat(readLines("temp.txt")[12:35], sep="\n")
  
})


output$Players_at_Risk=renderDT({
  mod1=glm(TJ_Status~Avg_Innings_Pitched+Avg_Games_Played+Avg_Hits_Allowed+
             Avg_Walks+Avg_Strikeouts+Avg_Opposing_Batting_Avg+
             Avg_Wild_Pitches+
             Avg_Batters_Hit_By_Pitch+Avg_Batters_Faced+
             Starter_or_Reliever+height+weight+(height*weight),
           data = bb_model_data, family=binomial(link="logit"))
  predict=predict(mod1,type='response')
  bb_model_data$prediction=predict
  bb_model_data$prediction.update=ifelse(bb_model_data$prediction>=(input$threshold)/100,1,0)
  cm=with(bb_model_data,table(TJ_Status,prediction.update))
  bb_data_2019=subset(bb_model_data,bb_model_data$MostRecentYear==2019)
  risk=(bb_data_2019[which(bb_data_2019$prediction.update==1 & bb_data_2019$TJ_Status==0),])
  riskdata=as.data.frame(cbind(as.character(risk$FullName),paste0(round(as.numeric(risk$prediction),4)*100,"%")))
  riskdata=riskdata[with(riskdata, order(riskdata$V2,decreasing = T)),]
  colnames(riskdata)=c("Player Name","Risk %")
  dtable = datatable(riskdata, rownames=FALSE,options=list(pageLength=10))
  dtable
})

output$model_accuracy=renderDT({
  mod1=glm(TJ_Status~Avg_Innings_Pitched+Avg_Games_Played+Avg_Hits_Allowed+
             Avg_Walks+Avg_Strikeouts+Avg_Opposing_Batting_Avg+
             Avg_Wild_Pitches+
             Avg_Batters_Hit_By_Pitch+Avg_Batters_Faced+
             Starter_or_Reliever+height+weight+(height*weight),
           data = bb_model_data, family=binomial(link="logit"))
  predict=predict(mod1,type='response')
  bb_model_data$prediction=predict
  bb_model_data$prediction.update=ifelse(bb_model_data$prediction>=(input$threshold)/100,1,0)
  cm=with(bb_model_data,table(TJ_Status,prediction.update))
  cm_table=as.data.frame(paste0(as.numeric(round(1-((cm[1,2]+cm[2,1])/sum(cm)),4)*100),"%"))
  dtable = datatable(cm_table, colnames = NULL,rownames=FALSE,options = list(pageLength=20,lengthChange = TRUE, dom='t',
                                                                             columnDefs = list(list(className = 'dt-center', targets = 0))))
  dtable
 
})

#------------------ Surgeon Analysis -------------------#

output$surgeon_freq=renderPlot({
  Surgeon_Data = subset(TJ, select = c(Player,Team,Level2,Position,Throws,TJ_Surgery_Date,
                                       Return_Date_same_level,Recovery_Time_months,Surgeon_s))

  Surgeon_Data2=subset(Surgeon_Data,is.na(Surgeon_Data$Surgeon_s)==FALSE)
  
  stats=as.data.frame(table(Surgeon_Data2$Surgeon_s))
  colnames(stats)=c("Surgeon_s","Num_Surgs")
  Surgeon_Data3=merge(x=Surgeon_Data2,y=stats,by=c("Surgeon_s"),all.x=TRUE)
  
  SD=Surgeon_Data3
  

  
  
  SD$SurgeonName=ifelse(SD$Num_Surgs<5,"Other",SD$Surgeon_s)
  stats=as.data.frame(table(SD$SurgeonName))
  
  stats=stats[with(stats, order(stats$Freq,decreasing=T)),]

  stats$Var1 <- factor(stats$Var1, levels = stats$Var1[order(stats$Freq)])

  plot=ggplot(data=stats, aes(x=Freq, y=Var1,fill=Var1)) +
    geom_bar(stat="identity")+theme(legend.position = "none")+
    geom_text(aes(label=Freq),hjust=1.02)+
    xlab("Number of Surgeries Performed (as of July 2020)")+
    ylab("")
  plot
 
})

#------------ Survival Plot -----------#

filter=reactive({
  Surgeon_Data = subset(TJ, select = c(Player,Team,Level2,Position,Throws,TJ_Surgery_Date,
                                       Return_Date_same_level,Recovery_Time_months,Surgeon_s))
  
  Surgeon_Data2=subset(Surgeon_Data,is.na(Surgeon_Data$Surgeon_s)==FALSE)
 
  
  stats=as.data.frame(table(Surgeon_Data2$Surgeon_s))
  colnames(stats)=c("Surgeon_s","Num_Surgs")
  Surgeon_Data3=merge(x=Surgeon_Data2,y=stats,by=c("Surgeon_s"),all.x=TRUE)

  SD=Surgeon_Data3
  
  #get rid of unknown dates
  SD=subset(SD,substr(as.character(SD$TJ_Surgery_Date),6,11)!="01-01" & 
               substr(as.character(SD$Return_Date_same_level),6,11)!="01-01")
  
  
  
  SD$KnownRecoveryTime=is.na(SD$Recovery_Time_months)
  
  survdata=as.data.frame(cbind(SD$Surgeon_s,SD$Recovery_Time_months,SD$KnownRecoveryTime))
  survdata$Censored=ifelse(survdata$V3==TRUE,0,1)
  colnames(survdata)=c("Surgeon","Recovery_Time","T_F","Censored")
  survdata$Recovery_Time=as.numeric(survdata$Recovery_Time)
  
  
  survdata['Surgeon'] = ifelse(survdata$Surgeon==input$surgeon_select,
                                 input$surgeon_select,
                                 "Others")
  return(survdata)
})

output$surv_plot=renderPlot({
  fit=survfit(Surv(Recovery_Time,Censored)~Surgeon,data=filter())  
  ggsurvplot(fit,data=filter(),pval=TRUE,xlim=c(0,max(filter()$Recovery_Time)+1),
             title=paste("Survival Plot for",input$surgeon_select, "vs. All Other Surgeons"),
             xlab="Recovery Time (Months)", ylab="Probability of Post-Surgery Return",
             ggtheme=theme(plot.title=element_text(hjust=0.5,face="bold")))
  

})

count_filter=reactive({
  
  Surgeon_Data = subset(TJ, select = c(Player,Team,Level2,Position,Throws,TJ_Surgery_Date,
                                       Return_Date_same_level,Recovery_Time_months,Surgeon_s))
  
  Surgeon_Data2=subset(Surgeon_Data,is.na(Surgeon_Data$Surgeon_s)==FALSE)
  SD2=Surgeon_Data2

  SD2$ReturnCounter=ifelse(is.na(SD2$Return_Date_same_level)==FALSE,1,0)
  a=as.data.frame(table(SD2$ReturnCounter,SD2$Surgeon_s))
  a2=a %>% 
    group_by(Var2) %>% 
    summarise(Total = sum(Freq, na.rm = TRUE))
  a2=as.data.frame(a2)
  colnames(a2)=c("Surgeon_s","Total")
  
  Recover=subset(a,a$Var1==1)
  Recover$Var1=NULL
  colnames(Recover)=c("Surgeon_s","Recoveries")
  
  Recover2=merge(x=Recover,y=a2,by=c("Surgeon_s"),all.x=TRUE)

  SD3=merge(x=SD2,y=Recover2,by=c("Surgeon_s"),all.x=TRUE)

  
  final=subset(SD3,Surgeon_s==input$surgeon_select)
  
  
  return(final)
})


output$Level_Surgeon=renderPrint({
  
  
  a=table(count_filter()$ReturnCounter,count_filter()$Level2)
  
  a=as.matrix(a)
  
  Perc=label_percent()(a[2,]/(a[2,]+a[1,]))
  a=rbind(a,Perc)
  rownames(a)=c("No Recovery","Recovery","Percent")
  a=noquote(a)
  a

})

output$Position_Surgeon=renderPrint({
  
  a=table(count_filter()$ReturnCounter,count_filter()$Position)
  
  a=as.matrix(a)
  
  Perc=label_percent()(a[2,]/(a[2,]+a[1,]))
  a=rbind(a,Perc)
  rownames(a)=c("No Recovery","Recovery","Percent")
  a=noquote(a)
  a

})

output$table1_surg=renderText({
  paste("Recovery Rate by Level for ",input$surgeon_select)
})

output$table2_surg=renderText({
  paste("Recovery Rate by Position for",input$surgeon_select)
  
})


#---------------- Recovery Time ------------------#
  
  
  output$RecoveryRange=renderPlot({
    
    dt=as.data.frame(table(S_Full$KTrange,S_Full$SurgNum2))

    colors <- switch(
      input$rb,
      "1st Surgery" = c("royalblue1", "gray30","gray30"),
      "2nd or 3rd Surgery" = c("gray30", "royalblue1","gray30")
    )
    
    ggplot(data = dt, aes(x = Var1, y = Freq, fill = factor(Var2))) +
      geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
      scale_fill_manual(values = colors) +
      geom_text(aes(label = Freq),position = position_dodge(width = 0.9),vjust = -0.25)+
      xlab("Recovery Time")+ylab("Frequency")+
      ggtitle("Surgery Frequency by Length of Recovery")+
      theme(plot.title = element_text(hjust = 0.5,face="bold"),legend.position = "none")


  })

  
  output$Repeats=renderDT({
    switch(input$rb,
           "1st Surgery"=datatable(S1_Full[,1:dim(S1_Full)[2]-1],rownames=FALSE, options = list(pageLength=10,lengthChange = FALSE)),
           "2nd or 3rd Surgery"=datatable(S2_Full[,1:dim(S2_Full)[2]-1],rownames=FALSE, options = list(pageLength=10,lengthChange = FALSE)))
  })
  
  

 

}

# Run the application 
shinyApp(ui = ui, server = server)



