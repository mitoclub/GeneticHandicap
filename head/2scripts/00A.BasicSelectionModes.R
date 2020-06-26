############################## SELECTION SCHEMES:
# see "Multiplicative versus additive selection in relation to genome evolution: a simulation study" 2001
rm(list=ls(all=TRUE))

pdf("../../body/4figures/00A.BasicSelectionModes.R.pdf", height = 14, width = 14)

# additive
NumOfMuts = seq(0,10000, by = 1)
SelCoeff = 0.0001;
AdditiveFitness = 1-NumOfMuts*SelCoeff
MultiplicativeFitness = (1-SelCoeff)^NumOfMuts
plot(NumOfMuts,AdditiveFitness, ylim = c(0.5,1), pch = '.', col = 'blue', ylab = 'FITNESS')
par(new=TRUE)
plot(NumOfMuts,MultiplicativeFitness, ylim = c(0.5,1), pch = '.', col = 'red', ylab = 'FITNESS')

dev.off()