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

pdf("../../body/4figures/02B.SlimOutParserWithHandicap.BurdenOfSdv.R.pdf", height = 14, width = 14)
par(mfrow = c(2,1))
library(data.table)

### read files in a directory and sort them according to generations:
Dir = "../../body/1raw/Ne_100000.GenomeLength_10000.MaxNumbOfGenerations_1000.MutRate_0.000001.SelCoeff1_0.01.DomCoeff1_0.5.MutRatio1_999.SelCoeff2_1.DomCoeff2_0.5.MutRatio2_1/"
Files = list.files(path = Dir)
FilesOrder = gsub("(.*)_",'',Files); FilesOrder = gsub(".out",'',FilesOrder); FilesOrder = as.numeric(FilesOrder)
Files = Files[order(FilesOrder)]

#### WHAT IS THE BURDEN OF SDV IN HANDICAP CARRIERS AND OTHERS?

MutBurden = data.frame()
for (file in Files)
{ # file = Files[1] file = 'sim0_900.out'
Infile = paste(Dir,file, sep = '')

Mut <- fread(Infile, skip="Mutations:", header = FALSE) # automatically stops when number of columns decreases:
names(Mut)=c('MutIdTemp','MutIdPerm','MutType','BasePos','SelCoeff','DomCoeff','SupPopId','GenerArose','MutPreval')
generation = gsub('sim0_','',file); generation = as.numeric(gsub('.out','',generation));  
Mut = select(Mut,MutIdTemp,MutType,GenerArose,MutPreval)
Mut$Generation = generation
Mut$MutAge = Mut$Generation - Mut$GenerArose

VecOfAllHandicaps = unique(Mut[Mut$MutType == 'm2',]$MutIdTemp)
VecOfRecentHandicaps = unique(Mut[Mut$MutType == 'm2' & Mut$MutAge == 1,]$MutIdTemp)
VecOfOldHandicaps = unique(Mut[Mut$MutType == 'm2' & Mut$MutAge > 1,]$MutIdTemp)

Gen <- fread(Infile, skip="Genomes:", header = FALSE, sep = '\t') # automatically stops when number of columns decreases:
Gen = Gen[Gen$V1 != 'Genomes:',]
row.names(Gen)=seq(1,nrow(Gen),1) #  Gen[row.names(Gen) == 200000,] 

odd = seq(1,(nrow(Gen)),2) # tail(odd)
even = seq(2,(nrow(Gen)),2) # tail(even)
FirstGenome = Gen[row.names(Gen) %in% odd,]
SecondGenome = Gen[row.names(Gen) %in% even,]  # Gen[row.names(Gen) == 200000,] 
Genome = data.frame(FirstGenome$V1,SecondGenome$V1)

Genome$Sdv1 = gsub("(.*) A",'',Genome$FirstGenome.V1)
Genome$Sdv2 = gsub("(.*) A",'',Genome$SecondGenome.V1)
ExtractNumber <- function(x) {SDV = unlist(strsplit(x,' ')); SDV = SDV[SDV != '']; length(SDV)}

Genome$Sdv1Number = apply(as.matrix(Genome$Sdv1),1,FUN = ExtractNumber)
Genome$Sdv2Number = apply(as.matrix(Genome$Sdv2),1,FUN = ExtractNumber) 
Genome$SdvNumber = Genome$Sdv1Number + Genome$Sdv2Number
Genome$Sdv = paste(Genome$Sdv1,Genome$Sdv2)

HandicapCaller <- function(x) {SDV = unlist(strsplit(x,' ')); SDV = SDV[SDV != '']; length(intersect(SDV,VecOfAllHandicaps))}
Genome$CarriersOfAllHandicaps = apply(as.matrix(Genome$Sdv),1,FUN=HandicapCaller)
HandicapCaller <- function(x) {SDV = unlist(strsplit(x,' ')); SDV = SDV[SDV != '']; length(intersect(SDV,VecOfRecentHandicaps))}
Genome$CarriersOfRecentHandicaps = apply(as.matrix(Genome$Sdv),1,FUN=HandicapCaller)
HandicapCaller <- function(x) {SDV = unlist(strsplit(x,' ')); SDV = SDV[SDV != '']; length(intersect(SDV,VecOfOldHandicaps))}
Genome$CarriersOfOldHandicaps = apply(as.matrix(Genome$Sdv),1,FUN=HandicapCaller)

Genome$SdvNumber = Genome$SdvNumber - Genome$CarriersOfAllHandicaps # !!!????

SummaryLine = data.frame(mean(Genome[Genome$CarriersOfAllHandicaps == 0,]$SdvNumber),mean(Genome[Genome$CarriersOfAllHandicaps == 1,]$SdvNumber),mean(Genome[Genome$CarriersOfRecentHandicaps == 1,]$SdvNumber),mean(Genome[Genome$CarriersOfOldHandicaps == 1,]$SdvNumber),nrow(Genome[Genome$CarriersOfAllHandicaps == 1,]),generation)
names(SummaryLine)=c('MeanBurdenControls','MeanBurdenAllCases','MeanBurdenRecentCases','MeanBurdenOldCases','NumberOfAllCases','Generation')
MutBurden = rbind(MutBurden,SummaryLine)
}

write.table(MutBurden,"../../body/2derived/02B.SlimOutParserWithHandicap.BurdenOfSdv.R..txt")

# pause 
MutBurden = read.table("../../body/2derived/02B.SlimOutParserWithHandicap.BurdenOfSdv.R..txt")

# should I subtract one mutation from controls? discuss with Victor probabilities of mutations..
# it night be recent - originated in this generation, or it might be more old - originated before - results should be different!
summary(MutBurden$MeanBurdenControls - 1)
summary(MutBurden$MeanBurdenAllCases)
wilcox.test(MutBurden$MeanBurdenControls - 1,MutBurden$MeanBurdenAllCases, paired = TRUE)
t.test(MutBurden$MeanBurdenControls - 1,MutBurden$MeanBurdenAllCases, paired = TRUE)

summary(MutBurden$MeanBurdenRecentCases)
summary(MutBurden$MeanBurdenOldCases)
wilcox.test(MutBurden$MeanBurdenRecentCases,MutBurden$MeanBurdenOldCases, paired = TRUE) # nothing

dev.off()
