library(ade4)

setwd("C:/xxx")
Traits<-read.csv("R_Disperse.csv",row.names="Genus",header=TRUE,sep=",");
ENVtrait<-Traits[,c(1:3)]

trait<-Traits[,c(4:length(Traits))]
str(trait)


###### selecting genus with complete trait information only (insects only)

CompTrait<-na.omit(trait)
CompTrait$Genus<-rownames(CompTrait)
ENVtrait$Genus<-rownames(ENVtrait)
ENVCompTrait <- merge(CompTrait, ENVtrait, by = 'Genus')
names(ENVCompTrait)

### creating 2 tables with taxonomical and trait informations
Taxonomy<- ENVCompTrait[,c(1,42:44)]
Traits<- ENVCompTrait[,c(2:41)]
rownames(Traits)<-Taxonomy$Genus
str(Traits)

##### checking if some trait categories sum to 0
colSums(Traits)
names(Traits)

## remove columns summing to 0
Traitsno0<- Traits[,c(-7,-29)]

#### list the number of categories per trait 
cat<-c(6,2,3,4,4,8,4,4,3)

#### list trait names
names(cat)<-c("Max size","life cycle duration","Reproduction cycles","Dispersal","life span","wing length", "wing type", "fecundity", "drift")
cat

#Transform the fuzzy coded trait data into % affinity

trait.fuzz<-prep.fuzzy.var (Traitsno0, cat)
ktab1 <- ktab.list.df(list(trait.fuzz))

# calculate gower ditance
dis <- dist.ktab(ktab1, type = "F")

## Calculate the FCA

TraitPC.fca<-dudi.fca(trait.fuzz,scan=F, nf=4)

#To plot Figure 3

par(mfrow=c(1,1))
s.class(TraitPC.fca$li,as.factor(ENVCompTrait$Group),cell=0,cstar=0.8,sub="(a)")
add.scatter.eig(TraitPC.fca$eig,nf=2,1,2,posi=c("bottomleft"))

#To plot Figure 4
scatter(TraitPC.fca) ### Figure 4

#Total variance taken into account by each axis or dimension:
variance<-(100*TraitPC.fca$eig)/sum(TraitPC.fca$eig)
variance

## Trait correlation with each axis
TraitPC.fca$cr

