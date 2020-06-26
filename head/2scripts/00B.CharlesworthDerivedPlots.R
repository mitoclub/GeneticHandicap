#########################################
#### plot 5A
#########################################
rm(list=ls(all=TRUE))

pdf("../../body/4figures/00B.CharlesworthDerivedPlots.R.pdf", height = 14, width = 14)

POIS = rpois(1000, 1000)
range(POIS)
hist(POIS,breaks = 100)
var(POIS)
3*sd(POIS)
6*sd(POIS) # ~ 200 - maximal range of contamination
# dev.off()
betta = seq(0,0.1,by=0.001); length(betta) # 11
variance = 1000
handicap_severity = seq(0,200,by=2) # maximum is reasonable to make equal to variance, not more
length(handicap_severity)

for (i in 1:length(betta))
{
  for (j in 1:length(handicap_severity))  
  {
    BETTA = betta[i]; VARIANCE = variance; HANDICAP_SEVERITY = handicap_severity[j]
    HANDICAP_EFFECT = BETTA*HANDICAP_SEVERITY*VARIANCE/(1+BETTA*VARIANCE)
    ONE_LINE = data.frame(HANDICAP_EFFECT,BETTA,HANDICAP_SEVERITY,VARIANCE)
    if (i == 1 & j == 1) {FINAL = ONE_LINE}
    if (i != 1 | j != 1) {FINAL = rbind(FINAL,ONE_LINE)}
    
  }
}
summary(FINAL$HANDICAP_EFFECT) # 0, 198
mat = matrix(data = FINAL$HANDICAP_EFFECT,nrow = 101, ncol = 101, byrow = FALSE)
dim(mat)

# setwd('/data1/BODY/DS_GENOTYPES/2_DS_GC/4_FIGURES/')
# 
colfunc <- colorRampPalette(c("yellow", "brown"))
colfunc_for_legend <- colorRampPalette(c("brown","yellow"))
image(mat, col = colfunc(20),xaxt= "n", yaxt= "n",xlab = 'handicap severity', ylab =  'betta') # x - handicap effect; y - betta ; terrain.colors or heat.colors or topo.colors ; rainbow(20, start=1/6, end = 2/6)
legend(0.02,0.25,c(200,150,100,50,0), fill = colfunc_for_legend(5), bg = 'white')
axis( 1, at=seq(0,1,length.out=5), labels= c(0,50,100,150,200), las= 1)
axis( 2, at=seq(0,1,length.out=5), labels= c(0,0.025,0.05,0.075,0.1), las= 2)

# dev.off()

#### make toy example to understand axises:
#mat=matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3, ncol = 3, byrow = FALSE)
#image(mat, col = colfunc(9))
# so it fills the heatmap like this:
# 7;8;9
# 4;5;6
# 1;2;3

#########################################
#### plot 5B
#########################################
# rm(list=ls(all=TRUE))

POIS = rpois(1000, 1000)
range(POIS)
hist(POIS,breaks = 100)
var(POIS)
3*sd(POIS)
6*sd(POIS) # ~ 200 - maximal range of contamination
# dev.off()
betta = 0.0008
variance = seq(0,10000,by = 100); length(variance)
handicap_severity = seq(0,200,by=2) # maximum is reasonable to make equal to variance, not more
length(handicap_severity)

for (i in 1:length(variance))
{
  for (j in 1:length(handicap_severity))  
  {
    BETTA = betta; VARIANCE = variance[i]; HANDICAP_SEVERITY = handicap_severity[j]
    HANDICAP_EFFECT = BETTA*HANDICAP_SEVERITY*VARIANCE/(1+BETTA*VARIANCE)
    ONE_LINE = data.frame(HANDICAP_EFFECT,BETTA,HANDICAP_SEVERITY,VARIANCE)
    if (i == 1 & j == 1) {FINAL = ONE_LINE}
    if (i != 1 | j != 1) {FINAL = rbind(FINAL,ONE_LINE)}
    
  }
}
summary(FINAL$HANDICAP_EFFECT) # 0, 177
mat = matrix(data = FINAL$HANDICAP_EFFECT,nrow = 101, ncol = 101, byrow = FALSE)
dim(mat)

#setwd('/data1/BODY/DS_GENOTYPES/2_DS_GC/4_FIGURES/')
#pdf('heatmap.Charlesworth.B.pdf')
colfunc <- colorRampPalette(c("yellow", "brown"))
colfunc_for_legend <- colorRampPalette(c("brown","yellow"))
image(mat, col = colfunc(20),xaxt= "n", yaxt= "n",xlab = 'handicap severity', ylab =  'variance') # x - handicap effect; y - betta ; terrain.colors or heat.colors or topo.colors ; rainbow(20, start=1/6, end = 2/6)
legend(0.02,0.3,c(200,150,100,50,0), fill = colfunc_for_legend(5), bg = 'white')
axis( 1, at=seq(0,1,length.out=5), labels= c(0,50,100,150,200), las= 1)
axis( 2, at=seq(0,1,length.out=5), labels= c(0,2500,5000,7500,10000), las= 2)
# dev.off()

#### make toy example to understand axises:
#mat=matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3, ncol = 3, byrow = FALSE)
#image(mat, col = colfunc(9))
# so it fills the heatmap like this:
# 7;8;9
# 4;5;6
# 1;2;3
