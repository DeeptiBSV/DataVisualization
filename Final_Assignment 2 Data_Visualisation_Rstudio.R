#loading the package necesary to run this file
#First check for package required is installed, if not we need to install it

needed_packages <- c("dplyr", "ggplot2", "tidyverse", "readxl","sqldf","reshape2","data.table","treemap","ggplot2",
                     "RColorBrewer","scales","gridExtra","plotly","gganimate","ggthemes")   

# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed)   
library(tidyverse)
library(ggplot2)# for plots
library(dplyr)
library(readxl)# for readings xlsx file
library(sqldf)
library(reshape2)
library(data.table)
library(treemap)
library(plotly)
library(ggthemes)
# Install all necessary packages and load the libraries into R

#Loading IPL Dataset from kaggle
matches <- read.csv('https://raw.githubusercontent.com/DeeptiBSV/WWD-Dataset/main/matches.csv',header = TRUE)
deliveries <- read.csv('https://raw.githubusercontent.com/DeeptiBSV/WWD-Dataset/main/deliveries.csv',header = TRUE)

###Cleaning the dataset
sum(is.na(matches))
sum(is.na(deliveries))
matches_final<-matches
deliveries_final<-deliveries

##In Matches column we have a column names dl_applied with all values and Umpire 3 column is Null, therefore removing both the  columns from the dataset.
matches_final<-subset(matches_final, select = -c(dl_applied,umpire3))
#from deliveries dataset we can remove player_dismissed,dismissal_kind, fielder, as these columns have empty rows
deliveries_final<-subset(deliveries_final,select = -c(player_dismissed,dismissal_kind,fielder))
head(deliveries_final)
###We have now removed the unnecessary column, now we can check for any outliers by checking the IQR range and summary statistics
####Data Visualization

###How is win by runs is distributed for the team based on the decision to first batting or fielding
wdc<-matches_final%>%select(winner,toss_decision,win_by_runs)
win_dec<-wdc%>%group_by(winner,toss_decision)%>%filter(!(win_by_runs==0))%>%summarise(count=max(win_by_runs))
win_dec

##Plot1-Iteration1
q1i1_stack<-ggplot(win_dec, aes(x=reorder(winner,count), y=count,fill=toss_decision)) + 
  geom_bar(stat="identity",position='stack') + 
  labs(y="WIN Count", 
       x="Winner", 
       title="Win Vs Tosswin")+
  theme_gray()+
  theme(plot.title = element_text(size=15),axis.text.x= element_text(size=12),
        axis.text.y= element_text(size=12), axis.title=element_text(size=12))+coord_flip()
q1i1_stack

#Q1 Iteration 2 (Facet bar)

q1i2_facet<-  ggplot(data=win_dec,aes(x=winner,y=count, fill=toss_decision))+geom_bar(stat="identity")+
    scale_fill_manual(values=c("bat"='blue',"field"='orange',"Mumbai Indians" ='green'))+
  geom_text(aes(label=count,vjust = -0.2, hjust = 0.6))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title='Team_Total_WinS')+
    facet_wrap(~toss_decision)+theme(axis.text.x=element_text(size=10))

q1i2_facet


##Q2Insight2 Win percentage each team. 
mp1<-sqldf('select team1, count(team1) as T1count from matches_final group by team1')

mp2<-sqldf('select team2, count(team2) as t2count from matches_final group by team2')

mp <- merge(x = mp1 , y = mp2 ,by.x = 'team1' , by.y = 'team2',all = T ) ;
mpc<-sqldf('select team1 as team ,(T1count+T2count) as total_matchplayed from mp group by team')
mpc
matwincount<-sqldf('select winner,count(winner) as wincount1 from matches group by winner')
matwincount

win_perc<-sqldf('select mpc.team,mpc.total_matchplayed,mwc.winner,mwc.wincount1 
                from mpc left join matwincount mwc on mpc.team=mwc.winner')

win_perc$winp<-round((win_perc$wincount1/win_perc$total_matchplayed),4)*100

win_perc<-na.omit(win_perc)%>%arrange(desc(winp))
win_perc

q2i1<-ggplot(data=win_perc,aes(x=reorder(team,-winp),y=winp))+
  geom_point()+geom_segment(aes(x=team,xend=team,y=0,yend=winp))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x='Team',y='WinPercentage',title='Team Winning Percentage')

q2i1

q2i2<-ggplot(win_perc,aes(x=winner,y=winp))+
  geom_point(aes(size = total_matchplayed,color=winp))+scale_size(range = c(3, 10))+
  labs(x='Team',y='WinPercentage',title='Team Winning Percentage')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
q2i2


##Question 3 how many times Mumbai Indian team won against other teams.

##Mumbai Indians
mi1 <- matches_final %>% 
  select(team1,team2,winner) %>% 
  filter(team1 != 'Mumbai Indians' & winner == 'Mumbai Indians' ) %>% 
  group_by(team1) %>% 
  summarise(m1 = n()) 

mi2 <- matches_final %>% 
  select(team1,team2,winner) %>% 
  filter(team2 != 'Mumbai Indians' & winner == 'Mumbai Indians' ) %>% 
  group_by(team2) %>% 
  summarise(m2 = n()) 

mi <- merge(x = mi1 , y = mi2 ,by.x = 'team1' , by.y = 'team2',all = T ) ;
mi[is.na(mi)] <- 0
mi<- rename(mi , Team = team1)
mi<- mi %>% 
  mutate(mi = m1 + m2 ) %>% 
  select(Team ,mi)
mi
q3i1<-ggplot(data = mi , aes(x = reorder(Team,mi) , y = mi ))+
  geom_bar(stat ="identity",fill="orange") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  geom_text(aes(label = mi),color="darkblue",fontface = "bold",size =2.8,vjust = 0.8, hjust = 1.1)+
  labs(title = "Mumbai Indians Win Against Other teams", x="Team",y="Win count of MI") +
  guides(fill = FALSE) +
  coord_flip()

q3i1
#Question3 Iteration 2 use pie chart

pct <- round(mi$mi/sum(mi$mi)*100)

mi$Team1 <- paste(mi$Team, pct) # add percents to labels
mi$Team2 <- paste(mi$Team1,"%",sep="")
mi$Team2
mi
pie(mi$mi,labels=mi$Team2,main="Pie chart",col=rainbow(length(mi$Team2)), radius=1)

treemap(mi ,                           
        index = "Team2" ,                
        vSize = "mi",                   
        type = "index" ,                 
        palette = "Set1",                     
        title = "MI win treemap against other teams ", 
        fontsize.title=15,               
        fontface.labels="bold",
        bg.labels=c("transparent"),
        border.col=c("white"),           
        border.lwds=c(1,1),              
        fontsize.labels=c(8,0),         
        fontcolor.labels=c("black"),
        aspRatio= 0.8,
        fontfamily.title = "serif")


#Question 4:
#Which Team have more number of 6s,4s and compare with the graphs of 6 and 4s of each team.
runs_teamdel<-deliveries_final%>%select(batting_team ,batsman_runs) %>%filter(batsman_runs %in% c(6,4))

s<-sqldf("select batting_team as Team ,count(batsman_runs) as sixes from 
         runs_teamdel where batsman_runs = 6 group by batting_team")
s
f<-sqldf('select batting_team as Team , count(batsman_runs) as fours from runs_teamdel 
      where batsman_runs = 4 group by batting_team')
f
run_t<-sqldf('select s.Team,s.sixes, f.fours from s
                           inner join f
                           on s.Team = f.Team')
run_t
run_sf<-melt(run_t,id.vars = c('Team'))

q4i1<-ggplot(data=run_sf,aes(x=Team,y=value,fill=as.factor(variable)))+
  geom_bar(stat="identity",position='dodge2')+
  geom_text(aes(label = round(value, 1)), 
            position = position_dodge(0.9),
            color="darkblue",vjust = 0,hjust = 0.5)+
  labs(title = "Boundaries by Team")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

q4i1
##Q4 Iteration 2 with Animation


q4i1_ani<-  run_sf%>%
  plot_ly(
    x = ~variable, 
    y = ~value, 
    size = ~value, 
    color = ~variable, 
    frame = ~Team, 
    text = ~value, 
    textposition = 'outside',
    textfont = list(color = '#000000', size = 16),
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers'
  ) %>%
  layout(
    yaxis = list(
      type = "log"
    )
  )


q4i1_ani<-q4i1_ani%>%animation_slider(currentvalue = list(prefix = "Team ", font = list(color="red")))
q4i1_ani
htmlwidgets::saveWidget(as_widget(q4i1_ani),"animation_boundaries.html")

###win and city Animation Iteration 1

yr<-sqldf("select season,city, winner,count(winner) as winnercount from matches_final group by season, city order by winnercount desc")
yr
match_anime <-  yr%>%
  plot_ly(
    x = ~winnercount, 
    y = ~city, 
    size = ~winnercount, 
    color = ~winner, 
    colors="Dark2",
    frame = ~Season, 
    text = ~winnercount,
    hoverinfo = "text",
    type = 'area',
    mode = 'markers',
    label=~winnercount,
    marker = list(line = list(color = 'rgba(152, 0, 0, .8)',
                             width = 2)
                  )
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )
match_anime<-match_anime%>%add_markers(size=10)
match_anime<-match_anime%>%animation_opts(1000,easing = "elastic")
match_anime
htmlwidgets::saveWidget(as_widget(match_anime),"animation_dot.html")

##Win and City Iteration 2 Animation
bar_ani<-yr%>%
  plot_ly(
    x = ~city, 
    y = ~winnercount, 
    size = ~winnercount, 
    color = ~winner, 
    colors="Dark2",
    frame = ~Season, 
    text = ~winnercount, 
    textposition = 'outside',
    textfont = list(color = '#000000', size = 16),
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers'
  )%>%
  layout(
    yaxis = list(
      type = "log"
    )
  )


bar_ani<-bar_ani%>%
  animation_opts(1000,easing="elastic")
bar_ani<-bar_ani%>%animation_button(currentvalue = list(prefix = "Season", font = list(color="red")))
    
bar_ani
htmlwidgets::saveWidget(as_widget(bar_ani),"animation_bar.html")

###############################END##############################################

