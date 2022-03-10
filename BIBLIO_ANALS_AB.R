p <- c("PUBYEAR = 2021","PUBYEAR = 2020","PUBYEAR = 2019","PUBYEAR = 2018","PUBYEAR = 2017")

year <- c("21","20","19","18","17")

#u <- c("AFFIL(LUT University)","AFFIL(Aalto University)","AFFIL(University of Tampere)","AFFIL(University of Oulu)","AFFIL(Massachusetts Institute of Technology)","AFFIL(Karlsruhe Institute of Technology)","AFFIL(Tsinghua University)","AFFIL(Xi'an Jiaotong University)","AFFIL(Islamic Azad University)" ) 

#uni <- c( "LUT","ALL","TAM","OUL","MIT","KIT","TSI","JIO","AZA" )

u <- c("AFFIL(LUT University)","AFFIL(Aalto University)","AFFIL(University of Tampere)","AFFIL(University of Oulu)","AFFIL(Massachusetts Institute of Technology)","AFFIL(Karlsruhe Institute of Technology)","AFFIL(Tsinghua University)","AFFIL(Xi'an Jiaotong University)","AFFIL(Islamic Azad University)","AFFIL(Fraunhofer-Gesellschaft)" ) 

###

uni <- c( "LUT","ALL","TAM","OUL","MIT","KIT","TSI","JIO","AZA","FRA" )


library('bibliometrix')

MM=NULL

#RESULTC=c(7:9)
RESULTC=c(1:10)
max=500
for (c in RESULTC) {

ser_file=sprintf("BIBLIOMETRIX/ME_%s",uni[c])
load(file=ser_file)
M=res2$M
MMM=rbind(MM,M)
MM=MMM
}


####

library("quanteda")
library("bibliometrix")

pdf("biblio_analysis.pdf", paper="a4")


yS=sprintf("ME_")
files=list.files(path="BIBLIOMETRIX2",pattern=yS,full.names=TRUE)
RESULTC=c(1:13)
RESULTC=c(1)
for (c in files) {
plot.new()
load(file=c)
M=res2$M
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)

#text(0,1,c,pos=4)

print(c)

f=S$MostRelKeywords
#text(0,0.9, f,pos=4)

print(f)

f=S$AnnualProduction
#text(0,0.7, f,pos=4)

print(f)

f=S$MostProdCountries
#text(0,0.3, f,pos=4)

print(f)

#print(conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10))


}

dev.off()


#####



load("BIBLIOMETRIX2/ME_BF")
M=res2$M
results <- biblioAnalysis(M, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
S$MostRelKeywords
S$AnnualProduction
S$MostProdCountries
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
plo


options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)
pl=plot(x = results, k = 10, pause = FALSE)
> pl$MostProdAuthors
> pl$$MostProdCountries
Error: unexpected '$' in "pl$$"
> pl$$MostProdCountries
Error: unexpected '$' in "pl$$"
> pl$MostProdCountries
> pl$AverArtCitperYear
> pl$AverTotCitperYear
> S <- summary(object = results, k = 30, pause = FALSE)
names(S)
S$MostRelKeywords
S$AnnualProduction
S$MostProdCountries

CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)

