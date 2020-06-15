rm(list=ls(all=TRUE))

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

pdf("../../body/4figures/01.SlimOutParser.R.pdf", height = 14, width = 14)
par(mfrow = c(2,1))
library(data.table)

### read files in a directory and sort them according to generations:
Dir = "../../body/1raw/Ne_100000.GenomeLength_10000.MaxNumbOfGenerations_10000.MutRate_0.000001.SelCoeff1_0.01.DomCoeff1_0.5.MutRatio1_XXX.SelCoeff2_XXX.DomCoeff2_XXX.MutRatio2_XXX/"
Files = list.files(path = Dir)
FilesOrder = gsub("(.*)_",'',Files); FilesOrder = gsub(".out",'',FilesOrder); 
Files[order(as.numeric(FilesOrder))]

for (file in Files)
{ # file = 'sim4_100.out'

Infile = paste(Dir,file, sep = '')

Mut <- fread(Infile, skip="Mutations:", header = FALSE) # automatically stops when number of columns decreases:

Ind <- fread(Infile, skip="Individuals:", header = FALSE) # automatically stops when number of columns decreases:

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

Sdv = paste(Genome$Sdv, collapse = " ")
Sdv = unlist(strsplit(Sdv,' ')); Sdv = Sdv[Sdv != '']

title = paste(Dir,file, sep = '\n')

hist(Genome$SdvNumber, breaks = 100, xlab = 'number of SDV per genome', main = title, cex.main=0.75) # can we compare it with Poisson distribution? 

AllSdv = data.frame(table(Sdv)); AllSdv = AllSdv[order(-AllSdv$Freq),]
hist(AllSdv$Freq, breaks = 100, xlab = 'frequency of each SDV in a population', main = title, cex.main=0.75)
}

dev.off()
