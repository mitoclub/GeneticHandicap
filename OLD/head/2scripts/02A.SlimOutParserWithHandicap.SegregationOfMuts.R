rm(list=ls(all=TRUE))
library(dplyr)
#### file structure:

# bla-bla
#Mutations:
#379 30 m1 1069 -0.01 0.5 p1 1 141
#11913 146 m1 1407 -0.01 0.5 p1 1 15
#624 559 m1 2269 -0.01 0.5 p1 1 115

#Individuals: (starts from zero and individual id is linked to corresponding two haploid genomes)
#p1:i0 H p1:0 p1:1
#p1:i1 H p1:2 p1:3
#p1:i2 H p1:4 p1:5

#Genomes: (this is the number of individuals multiplied by 2 - diploid)
#p1:0 A 0 => first individuum first genome
#p1:1 A 1 2 3 4 => first individuum second genome
#p1:2 A

pdf("../../body/4figures/02A.SlimOutParserWithHandicap.SegregationOfMuts.R.pdf", height = 14, width = 14)
par(mfrow = c(2,1))
library(data.table)

### read files in a directory and sort them according to generations:
Dir = "../../body/1raw/Ne_100000.GenomeLength_10000.MaxNumbOfGenerations_1000.MutRate_0.000001.SelCoeff1_0.01.DomCoeff1_0.5.MutRatio1_999.SelCoeff2_1.DomCoeff2_0.5.MutRatio2_1/"
Files = list.files(path = Dir)
FilesOrder = gsub("(.*)_",'',Files); FilesOrder = gsub(".out",'',FilesOrder); FilesOrder = as.numeric(FilesOrder)
Files = Files[order(FilesOrder)]

#### WHAT IS THE SEGREGATION PERIOD OF A HANDICAP AND SDV? HANDICAP IS MUCH SHORTER (mean = 1.749 vs 13.16) 
MutAll = data.frame()
for (file in Files)
{ # file = Files[1]
Infile = paste(Dir,file, sep = '')

Mut <- fread(Infile, skip="Mutations:", header = FALSE) # automatically stops when number of columns decreases:
names(Mut)=c('MutIdTemp','MutIdPerm','MutType','BasePos','SelCoeff','DomCoeff','SupPopId','GenerArose','MutPreval')
generation = gsub('sim0_','',file); generation = as.numeric(gsub('.out','',generation));  
Mut = select(Mut,MutIdPerm,MutType,GenerArose,MutPreval)
Mut$Generation = generation
MutAll=rbind(MutAll,Mut)
}

MutAll1 = MutAll[MutAll$MutType == 'm1',]
MutAll1 = MutAll1[order(MutAll1$MutIdPerm,-MutAll1$Generation),]
MutAllNoDup1 = MutAll1[!duplicated(MutAll1$MutIdPerm),]
summary(MutAllNoDup1$MutPreval) # even max is less then < 10000
MutAllNoDup1$TimeOfSegregation = MutAllNoDup1$Generation - MutAllNoDup1$GenerArose
summary(MutAllNoDup1$TimeOfSegregation) # 1.00    1.00    2.00   13.16    7.00  970.00
hist(MutAllNoDup1$TimeOfSegregation,breaks = 100)

MutAll2 = MutAll[MutAll$MutType == 'm2',]
MutAll2 = MutAll2[order(MutAll2$MutIdPerm,-MutAll2$Generation),]
MutAllNoDup2 = MutAll2[!duplicated(MutAll2$MutIdPerm),]
summary(MutAllNoDup2$MutPreval) # max = 4!!!
MutAllNoDup2$TimeOfSegregation = MutAllNoDup2$Generation - MutAllNoDup2$GenerArose
summary(MutAllNoDup2$TimeOfSegregation) # 1.000   1.000   1.000   1.749   2.000   8.000
hist(MutAllNoDup2$TimeOfSegregation,breaks = 100)

dev.off()
