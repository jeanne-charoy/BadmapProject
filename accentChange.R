library(stringr)
library(reshape2)
library(ggplot2)##plotting
library(plyr)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)
library(modelbased)

pronunciationData<-read.csv("C:/Users/jeann/OneDrive - Stony Brook University/accentChangeScores.csv", header=TRUE)

str(pronunciationData)

pronunciationData$wave<-factor(pronunciationData$wave)
pronunciationData$base<-factor(pronunciationData$base)
names(pronunciationData)<-c("id","wave","base","id2","mint","Versant","Sentence","Vocab","Fluency","Pronunciation")

pronunciationData$id<-factor(pronunciationData$id)

ddply(pronunciationData, .(base
), summarize, NumSubjects = length(unique(id)))


means<-aggregate(Vocab~base,pronunciationData, FUN="mean")


png("vocab.png", units="in", width=12, height=6, res=300)


ggplot(pronunciationData, aes(base, Vocab))+
  geom_line(aes(group=id, color=id))+
  geom_point(data=means, aes(base, Vocab,size=2), color="black")+
  theme_classic(20)+
  guides(color=FALSE, size=FALSE)+
  xlab("Time point")+
  ylab("Vocabulary")+
  ylim(30,80)+
  scale_x_discrete(labels=c("1" = "Fall 1", "2" = "Winter 1",
                            "3" = "Spring 1", "4"="Fall 2", "5"="Spring 2"))

dev.off()
