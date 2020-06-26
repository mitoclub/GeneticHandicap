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

pdf("../../body/4figures/01B.SlimOutParser.SegregationOfEachSDV.R.pdf", height = 14, width = 28)
#par(mfrow = c(2,1))
library(data.table)

### read files in a directory and sort them according to generations:
Dir = "../../body/1raw/Ne_100000.GenomeLength_10000.MaxNumbOfGenerations_10000.MutRate_0.000001.SelCoeff1_0.01.DomCoeff1_0.5.MutRatio1_XXX.SelCoeff2_XXX.DomCoeff2_XXX.MutRatio2_XXX/5000_5099_EachGeneration/"

Files = list.files(path = Dir)
FilesOrder = gsub("(.*)_",'',Files); FilesOrder = gsub(".out",'',FilesOrder); FilesOrder = as.numeric(FilesOrder)
Files = Files[order(FilesOrder)]

#Files = list.files(path = Dir)
#FilesOrder = gsub("(.*)_",'',Files); FilesOrder = gsub(".out",'',FilesOrder); 
#Files[order(as.numeric(FilesOrder))]

MutAll = data.frame()
for (file in Files)
{ # file = 'sim0_5000.out'

Infile = paste(Dir,file, sep = '')

Mut <- fread(Infile, skip="Mutations:", header = FALSE) # automatically stops when number of columns decreases:
# each line is one mutation
names(Mut)=c('MutIdTemp','MutIdPerm','MutType','BasePos','SelCoeff','DomCoeff','SupPopId','GenerArose','MutPreval')
generation = gsub('sim0_','',file); generation = as.numeric(gsub('.out','',generation));  
Mut = select(Mut,MutIdPerm,GenerArose,MutPreval)
Mut$Generation = generation
MutAll=rbind(MutAll,Mut)
}

# sort by MutId and Generation
MutAll = MutAll[order(MutAll$MutIdPerm,-MutAll$Generation),]

# what is the distribution of the time of the existence of a mutation (from origin to extinction)
MutAllNoDup = MutAll
MutAllNoDup = MutAllNoDup[!duplicated(MutAllNoDup$MutIdPerm),]
summary(MutAllNoDup$MutPreval) # even max is less then < 10000
MutAllNoDup$TimeOfSegregation = MutAllNoDup$Generation - MutAllNoDup$GenerArose
summary(MutAllNoDup$TimeOfSegregation)
hist(MutAllNoDup$TimeOfSegregation, breaks = 10000, xlim = c(1,500))

# plot it 
MutAll = MutAll[order(MutAll$MutIdPerm,MutAll$Generation),]
VecMut = unique(MutAll$MutIdPerm); length(VecMut)
VecMut = sample(VecMut,10000)
for (j in 1:length(VecMut))
{ # j = 5
plot(MutAll[MutAll$MutIdPerm == VecMut[j],]$Generation,MutAll[MutAll$MutIdPerm == VecMut[j],]$MutPreval, xlim = c(5000,5100), ylim = c(0,500), type = 'l', col = rgb(runif(1),runif(1),runif(1),0.2), ylab = '', xlab = '')
par(new=TRUE)
}

dev.off()
