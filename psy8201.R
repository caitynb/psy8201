rm(list=ls())

library(ggplot2)
library(tidyr)
library(Rmisc)
## H1a
x<-rnorm(n=1000,mean=0.3,sd=.1)
y<-rnorm(n=1000,mean=0.8,sd=.1)
t.test(x=x,y=y,alternative="less")
dat<-data.frame(x,y)
datl<-pivot_longer(dat, cols=c("x","y"),names_to=c("group"),values_to="score")
f1<-ggplot(datl,aes(y=score,x=group))+geom_boxplot()+theme_minimal()+
  ggtitle("Neighborhood Quality and Trust in Government")+
  xlab("Neighborhood Quality")+
  ylab("Trust in Government")+
  scale_x_discrete(labels=(c("x"="Low","y"="High")))+
  ylim(0,1)

ggsave("f1.png",f1,dpi=300,height=5,width=5)
## H1b
a<-rnorm(n=1000,mean=0.4,sd=0.1)
b<-rnorm(n=1000,mean=0.7,sd=0.1)
dat2<-data.frame(a,b)
dat2l<-pivot_longer(dat2, cols=c("a","b"),names_to="group",values_to="score")
f2<-ggplot(dat2l,aes(y=score,x=group))+geom_boxplot()+theme_minimal()+
  ggtitle("Neighborhood Quality and Political Engagement")+
  xlab("Neighborhood Quality")+
  ylab("Political Engagement")+
  scale_x_discrete(labels=(c("a"="Low","b"="High")))+
  ylim(0,1)

t.test(x=a,y=b,alternative="less")

ggsave("f2.png",f2,dpi=300,height=5,width=5)
## H2
rm(list=ls())
x<-rnorm(n=200,mean=0.2,sd=0.05)
y<-rnorm(n=200,mean=0.7,sd=0.05)
z<-rep(c("Prime","No Prime"),100)

dat<-data.frame(y,x,z)
datl<-pivot_longer(dat, cols=c("x","y"),names_to=c("group"),values_to="score")
mdatl<-summarySE(datl,measurevar="score",groupvars=c("group","z"))
f3<-ggplot(mdatl, aes(x=group,y=score,fill=z))+
  geom_bar(stat="identity",position="dodge")+
  theme_minimal()+
  scale_x_discrete(labels=c("x"="Low","y"="High"))+
  scale_fill_grey()+
  geom_errorbar(aes(ymin=score-se,ymax=score+se),position="dodge")+
  ylab("Trust in Government")+
  xlab("Neighborhood Condition")+
  ggtitle("Experimental Condition and Trust in Government")+
  labs(fill="Prime Condition")

aov1<-aov(score~group*z,data=datl)
summary(aov1)
TukeyHSD(aov1)

ggsave("f3.png",f3,dpi=300,height=5,width=5)
## H3
rm(list=ls())
x<-rep(c("Prime","No Prime"),100)
y<-rep(c(85,52,90,30),50)
z<-rep(c("High","Low","Low","High"),50)

dat<-data.frame(x,y,z)
f4<-ggplot(dat, aes(x=x,y=y,fill=z))+geom_bar(stat="identity",position="dodge")+
  theme_minimal()+
  ylim(0,100)+
  ylab("Percent of Respondents Expect Government to Fix")+
  xlab("Prime Condition")+
  labs(fill="Neighborhood\nCondition",title="Experimental Condition and Government Responsibility")+
  scale_fill_grey()

ggsave("f4.png",f4,dpi=300,height=5,width=5)

aov3<-aov(y~x*z)
summary(aov3)
## H4a
rm(list=ls())
poleff<-rep(c("High","Low","Low","High"),50)
behav<-rep(c(.6,.5,.30,.90),50)
nqual<-rep(c("Low","High"),100)

dat<-data.frame(poleff,behav,nqual)
dat$nqual<-as.factor(dat$nqual)
f5<-ggplot(dat, aes(nqual,behav, group=poleff,color=poleff))+
  stat_summary(fun=mean,geom="point")+
  stat_summary(fun=mean,geom="line")+
  ylim(0,1)+
  labs(title="Neighborhood Condition, Behavior, Efficacy",x="Neighborhood Condition",y="Behavior",color="Efficacy")+
  scale_color_grey()+
  theme_minimal()

aov2<-aov(behav~nqual*poleff,data=dat)
summary(aov2)

ggsave("f5.png",f5,dpi=300,height=5,width=5)

## H4b
rm(list=ls())
prime<-rep(c("Prime","No Prime","No Prime","Prime"),50)
behav<-rep(c(.65,.5,.45,.82),50)
nqual<-rep(c("Low","High"),100)

dat<-data.frame(prime,behav,nqual)
dat$nqual<-as.factor(dat$nqual)
f6<-ggplot(dat, aes(nqual,behav, group=prime,color=prime))+
  stat_summary(fun=mean,geom="point")+
  stat_summary(fun=mean,geom="line")+
  ylim(0,1)+
  labs(title="Neighborhood Condition, Political Behavior, and Prime Condition",x="Neighborhood Condition",y="Behavior",color="Prime Condition")+
  scale_color_grey()+
  theme_minimal()
aov2<-aov(behav~nqual*poleff,data=dat)
summary(aov2)

ggsave("f6.png",f6,dpi=300,height=5,width=5)

