#Special Studies Drought Synthesis Paper
#Trait based approach to look at community level response to drought
#Leela Dixit
#07/1/2022

#####Package install#####
#install.packages("vegan")
#install.packages("remotes")
#remotes::install_github("gavinsimpson/ggvegan")
#install.packages("ggpubr")
#install.packages("lubridate")
#####package load in#####
library(vegan)
library(ggplot2)
library(ggvegan)
library(ggpubr)
library(dplyr)
library(tidyr)
library(lubridate)

#####creating year type file#####
#Nick's file of 
sach <- read.csv("PHDI_Sacramento_year.csv")
sach_m <- sach %>%
  mutate(year=year(date),
         month=month(date))%>%
  unite(year_month,year:month,sep="-",remove=T)
  


#NMDS analysis for benthic invertebrates#
######Load in data#####
#station/year matrix, and sample x species matrix of annual mean cpue
  #Only includes 3 long term stations, and 42 taxa that occured in 10% of samples
benthic.stations <- read.csv("benthic_nmds_predictors.csv")
benthic.stations1 <- merge(benthic.stations, sach, by="year")
benthic.spp <- read.csv("benthic_nmds_abundance_matrix.csv")
#transform community data with hellinger transformation
benthic.hel <- decostand(benthic.spp,method="hellinger")
nmds1 <- metaMDS(benthic.hel,autotransform = F)

#basic plots
ordiplot(nmds1)
ordiplot(nmds1,type="t")

#ggvegan autoplot
autoplot(nmds1)

#full control with fortified ordination output
fort <- fortify(nmds1)
ggplot() +
  geom_point(data=subset(fort, Score == "sites"),
             mapping = aes(x=NMDS1, y=NMDS2),
             colour="black",
             alpha=0.5)+
  geom_segment(data=subset(fort,Score == "species"),
               mapping = aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgrey",
               size=0.8)+
  geom_text(data=subset(fort,Score == "species"),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept=0, slope=0, linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))

#make a two panel plot to reduce complexity
p1 <- ggplot()+
  geom_point(data=subset(fort, Score=="sites"),
             mapping=aes(x=NMDS1, y=NMDS2),
             colour="black",
             alpha=0.5)+
  geom_segment(data=subset(fort,Score=='species'),
               mapping=aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgray",
               size=0,
               alpha=0)+
  geom_text(data=subset(fort,Score=='species'),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1),
            alpha=0)+
  geom_abline(intercept=0,slope=0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))
p1  

p2 <- ggplot()+
  geom_point(data=subset(fort, Score=="sites"),
             mapping=aes(x=NMDS1, y=NMDS2),
             colour="black",
             alpha=0)+
  geom_segment(data=subset(fort,Score=='species'),
               mapping=aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgray",
               size=0.8)+
  geom_text(data=subset(fort,Score=='species'),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept=0,slope=0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))
p2

#create a multi-panel plot with one column
library(ggpubr)
ggarrange(p1,p2,ncol=1)

######test for differences in benthic community composition across year######
summary(benthic.spp)
adonis(benthic.spp~year,data=benthic.stations)
#then show this in an NMDS plot

p3 <- ggplot()+
  geom_point(data=subset(fort, Score=='sites'),
             mapping=aes(x=NMDS1, y=NMDS2, colour=benthic.stations$year),
             alpha=0.5)+
  geom_segment(data=subset(fort,Score=='species'),
               mapping=aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               size=0,
               alpha=0)+
  geom_text(data=subset(fort,Score=='species'),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1),
            alpha=0)+
  geom_abline(intercept=0,slope=0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"),
        legend.position=c(0.8, 0.2))+
  scale_color_continuous("year", type = "viridis")
p3
p4<- ggplot()+
  geom_point(data=subset(fort, Score=='sites'),
             mapping=aes(x=NMDS1, y=NMDS2, colour=benthic.stations$station_code),
             alpha=0.5)+
  geom_segment(data=subset(fort,Score=='species'),
               mapping=aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               size=0,
               alpha=0)+
  geom_text(data=subset(fort,Score=='species'),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1),
            alpha=0)+
  geom_abline(intercept=0,slope=0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"),
        legend.position=c(0.8, 0.2))+
  scale_color_discrete("fgstation_code")
p4

p5 <- ggplot()+
  geom_point(data=subset(fort, Score=='sites'),
             mapping=aes(x=NMDS1, y=NMDS2, colour=benthic.stations1$category_coarse),
             alpha=0.5)+
  geom_segment(data=subset(fort,Score=='species'),
               mapping=aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               size=0,
               alpha=0)+
  geom_text(data=subset(fort,Score=='species'),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1),
            alpha=0)+
  geom_abline(intercept=0,slope=0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"),
        legend.position=c(0.8, 0.2))+
  scale_color_discrete("category_course", type = "viridis")+
  geom_point(data=subset(fort, Score=="sites"),
           mapping=aes(x=NMDS1, y=NMDS2),
           colour="black",
           alpha=0)+
  geom_segment(data=subset(fort,Score=='species'),
               mapping=aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
               arrow=arrow(length=unit(0.015, "npc"),
                           type="closed"),
               colour="darkgray",
               size=0.8)+
  geom_text(data=subset(fort,Score=='species'),
            mapping=aes(label=Label, x=NMDS1*1.1, y=NMDS2*1.1))+
  geom_abline(intercept=0,slope=0,linetype="dashed", size=0.8,colour="gray")+
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8, colour="gray")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.line=element_line(colour="black"))
p5

ggarrange(p3,p2,ncol=1)
ggarrange(p4,p2,ncol=1)

#add in species of interest from
#plot NMDS with different rare taxa removal categories
  #drop fixed portion (least abundant 25%)
  #Comprise at least 5% of total abundance
  #remove below 5% of abundance of most abundant spp.
  #found in 5% of samples
  #found in 10% samples
