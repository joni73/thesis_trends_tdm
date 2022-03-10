INST="ON"
if(INST=="ON")
{
install.packages("ggplot")
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages('psych')
install.packages('topic.models')
install.packages('seededlda')
install.packages("quanteda.textplots")
install.packages("Kendall")
}

library('psych')
library('quanteda')
library(quanteda.textstats)
library(ggplot2)
library(scales)
library("quanteda.textplots")
library(Kendall)

require(quanteda)
require(quanteda.corpora)
require(seededlda)

#============
ver=21
word_out="OFF"
Pdf_printing="ON"
Word_printing="OFF"
#============
if(Word_printing=="OFF")
{
library(R2wd)
require(R2wd)
library(RDCOMClient)
require(RDCOMClient)
}


f=1
pau=3		#printing paus seconds microsoft dcom
lookup=FALSE

ajo="8_9_10"
daf="AR"
K=10          # how many topics
lterm_q=5     #how many words
min_termfreq = 0.8
max_docfreq = 0.9

#kaatuu
#ajo="50_3_8_1"
#daf="AR"
#K=50         # how many topics
#lterm_q=3     #how many words
#min_termfreq = 0.8
#max_docfreq = 0.1

#eitoimi
#ajo="8_3_3"
#daf="AR"
#K=3         # how many topics
#lterm_q=3     #how many words
#min_termfreq = 0.8
#max_docfreq = 0.3



GenTopics="TRUE" # do we read lda_tda,... or generate

uni <- c( "LUT","ALL","TAM","OUL","MIT","KIT","TSI","JIO","AZA","FRA","SIT","BF","DIM")
RM=9

year <- c("21","20","19","18","17")


#  frequency rank docfreq - feature -  dictionary - university - year


if(daf=="AB"){
	#load("//192.168.0.42/mpi/DITYO2/CorpXABV2")
	load("CorpXABV2")
        #CorpX=CorpXAB
	}

if(daf=="AR"){
	#load("//192.168.0.42/mpi/DITYO2/CorpXV2")
	load("CorpXV2")
	}

toks_news <- tokens(CorpX)

if(GenTopics==TRUE){
	
	dfmat_news <- dfm(toks_news) %>% 
    	dfm_trim(min_termfreq=min_termfreq, termfreq_type = "quantile",
             max_docfreq=max_docfreq, docfreq_type = "prop")
	tmod_lda <- textmodel_lda(dfmat_news, k = K)
         if(daf=="AB"){
			      save(tmod_lda,file="tmod_lda_AB_V2")
				dfmat_news$topic2 <- topics(tmod_lda)
				save(dfmat_news,file="dfmat_news_AB_V2")
                    }
         if(daf=="AR"){
			      save(tmod_lda,file="tmod_lda_V2")
				dfmat_news$topic2 <- topics(tmod_lda)
				save(dfmat_news,file="dfmat_news_V2")
                    }
	}
	else
	{
        if(daf=="AB"){
		load("tmod_lda_AB_V2")
		load("dfmat_news_AB_V2")
		}
		
        if(daf=="AR"){
		load("tmod_lda_V2")
		load("dfmat_news_V2")
		}

	}

lterms=terms(tmod_lda,lterm_q)
kas=length(lterms)
     
if(Word_printing=="ON")
 WordFile=sprintf("C:/Users/Joni-/LUT/DITYO2/RAAKAANAL/Topic%s_%s_%s.doc",ver,daf,ver)

if(Pdf_printing=="ON")
 PdfFile=sprintf("RAAKAANAL/Topic%s_%s_%s.pdf",ver,ajo,daf)
 
TableFile=sprintf("RAAKAANAL/analysis__topic_%s_%s.csv",ver,daf)

############ RUN TABLE

DIC=NULL
Data=NULL

un=length(uni)
ye=length(year)
Data=array(0,dim=c(K,2,un,ye) )

row.names(Data) <- names(lterms[1,])

RESULTb=c(1:K)

for (b in RESULTb) {


RESULTc=c(1:un)
#RESULTc=c(1)
for (c in RESULTc) {
print("un")
print(c)
RESULTa=c(1:ye)
#RESULTa=c(1)
for (a in RESULTa) {
print("ye:")
print(a)

    DOCS=0
    word=NA
    docs=0

    word1=dfm_subset(dfmat_news,Year == year[a] & University == uni[c] )
    DOCS=length(word1[,1])
    DIC=sprintf("topic%i",b)
    word=dfm_subset(word1,word1$topic2==DIC)
    docs=length(word[,1])
           
    print("DOCS:")
    print(DOCS)

      Data[b,1,c,a]=DOCS
      Data[b,2,c,a]=docs


}  #year a
} #uni c
} #topic b



### TOPIC


year <- c(21,20,19,18,17)

funny1<-function(){
geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <- ggplot(PlotData,  aes(x=Year, y=Presentage, shape=University,color=University), height=2, width=2 )
print(c + geom_point(size=1) + 
    scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +  
ylab("Osuus/Yliopisto") +
xlab("Vuosi") + 
ggtitle(names(DIC[b])) +
theme(text = element_text(size = 8)) +
scale_shape_manual(values = c(0,1,2,3,4,5,6,15,16,17,18,19,20,14)) + 
scale_size_manual(values = c(4,4,4,4,4,4,4,1,1,4,1,4,4,4))
)}


funny2 <-function(){ 
geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <-	ggplot(PlotData,aes(x=University, y=Presentage,color=University ), height=2, width=2) 
print(c + geom_point() +
theme(text = element_text(size = 8)) +
ggtitle(names(DIC[b])) + 
geom_boxplot() + 
ylab("Osuttuja tekstejä") +
xlab("Yliopisto")
 )}

funny3<-function(){
geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <- ggplot(PlotData,  aes(x=Year, y=ScoreTxt, shape=University, color=University), height=2, width=2 )
print(c + geom_point(size=1) + 
    scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) + 
ylab("Osumia/Yliopisto") +
xlab("Vuosi") + 
ggtitle(names(DIC[b])) +
theme(text = element_text(size = 8)) +
scale_shape_manual(values = c(0,1,2,3,4,5,6,15,16,17,18,19,20,14)) + 
scale_size_manual(values = c(4,4,4,4,4,4,4,1,1,4,1,4,4,4)) 
)}


funny4 <-function(){ 
geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <-	ggplot(PlotData,aes(x=University, y=ScoreTxt, color=University ), height=2, width=2) 
print(c + geom_point() +
theme(text = element_text(size = 8)) +
ggtitle(names(DIC[b]) ) +
ylab("Osumia") +
xlab("Yliopisto")
)}

funny5 <-function(){
  pairs.panels(PlotData,pch=21)
}

funny6 <-function(){

  textplot_xray(kwic(tokCL,t_erms))
}


######## print result start
if(Word_printing=="ON"){
wdGet(filename = NULL, path = "", method="RDCOMClient",visible = TRUE)

wdNewDoc(WordFile)    # this creates a new file with "this.doc" name
wdApplyTemplate("C:Users/Joni-/Documents/LUT_Tietokantapohja1.dotx")    # this applies a template
 
#wdTitle("TRENDS - FULLTEXT")    # adds a title to the file

wdBody("\n\nDI-Työn raaka dataa - EI JULKINEN – Joni-Pekka Kurronen\n\n Tekijän oikeudet pidätetään osittain 2021. Tätä työtä saa käyttää ei kaupallisesti, jakaa ja tuottaa uudelleen missä tahansa muodossa, mutta alkuperäisiin tekijöihin ja kustantajiin tulee viittausten näkyä ja säilyä. (Elsevier, 2017)
\n\nSome rights reserved 2021. This work permits non-commercial use, distribution, and re-production in any medium, provided the original author and source are credited.
")



wdBody("\n\nKaikki tekstit:")

 wdBody(length(CorpX))

wdBody("\nAiheet:")

wdBody(f)

} # word print end

if(Pdf_printing=="ON")
{
pdf(PdfFile, paper="a4")
plot.new()
text(0,.90, f,pos=4)
txt <- PdfFile
text(0,.86, txt,pos=4)

txt <- "DI-Työn raaka dataa - EI JULKINEN – Joni-Pekka Kurronen Tekijän\r
oikeudet pidätetään\n osittain 2022. Tätä työtä saa käyttää ei\r
kaupallisesti, jakaa ja tuottaa uudelleen missä tahansa\r
muodossa, mutta alkuperäisiin tekijöihin ja kustantajiin tulee\r
viittausten näkyä ja säilyä.Elsevier, 2017 Some rights reserved 2022.\r
This work permits non-commercial use, distribution, and re\r
production in any medium, provided the original\r
author and source are credited."

text(0,.40, txt,pos=4)

txt <- "Kaikki tekstit:"
text(0,.16,txt,pos=4)
txt <- length(CorpX)
text(0,0.11,txt,pos=4)
txt <- "Aiheet:"
text(0,0.06,txt,pos=4)

} #pdf end


RESULTb=c(1:K) #l
for (b in RESULTb) {

Uni=NULL
Yea=NULL
Pre=NULL
Sco=NULL
ScoTxt=NULL

SiVal=0
SiVal2=0
SiVal2t=0
SiVal3=0
SiVal4=0
Wor=NULL
Dic=NULL
Corp=NULL
Hit=NULL
Ver=NULL

YeaYT   <- NULL
preunpT <- NULL
preuneT <- NULL
preunvT <- NULL
preunmT <- NULL
scouneT <- NULL
scounvT <- NULL
scounpT <- NULL
scounmT <- NULL
txtunpT <- NULL
txtuneT <- NULL
txtunvT <- NULL
txtunmT <- NULL

YeaY=NULL
PreY=NULL
ScoY=NULL
ScoTxtY=NULL

preunp <- NULL
preune <- NULL
preunv <- NULL
preunm <- NULL
scoune <- NULL
scounv <- NULL
scounp <- NULL
scounm <- NULL
txtunp <- NULL
txtune <- NULL
txtunv <- NULL
txtunm <- NULL


YeaYT <- NULL
PreYT <- NULL
ScoYT <- NULL
ScoTxtYT <- NULL


for(an in c(1:ye) ){
YeaYT[an]=0
PreYT[an]=0
ScoYT[an]=0
ScoTxtYT[an]=0
}


RESULTc=c(1:un)
#RESULTc=c(1:1)
for (c in RESULTc) {



RESULTa=c(1:ye)
#RESULTa=c("1")
for (a in RESULTa) {



Wor=append(Wor,DIC[b])
Dic=append(Dic,ajo)
Corp=append(Corp,daf)
Ver=append(Ver,ver)
Uni=append(Uni,uni[c])
Yea=append(Yea,year[a])
YeaY=append(YeaY,year[a])
YeaYT=append(YeaYT,year[a])


Val=Data[b,2,c,a]
if(is.na(Val)==TRUE) Val = 0
if(Val=="NaN") Val = 0
if(Val=="Inf") Val = 0
if(!is.numeric(Val)) Val=0
Sco=append(Sco,Val)
ScoY=append(ScoY,Val)
 if(c<=RM)
  ScoYT[a]=ScoYT[a]+Val
SiVal=SiVal+Val

if(0==Data[b,1,c,a]) Val2 = 0
 else
Val2=Data[b,2,c,a]/Data[b,1,c,a]
if(is.na(Val2)==TRUE) Val2 = 0
if(Val2=="NaN") Val2 = 0
if(Val2=="Inf") Val2 = 0
if(!is.numeric(Val2)) Val2=0
Pre=append(Pre,Val2)
PreY=append(PreY,Val2)
 if(c<=RM)
    PreYT[a]=PreYT[a]+Val2
SiVal2=SiVal2+Val2

Val2t=Data[b,1,c,a]
if(is.na(Val2t)==TRUE) Val2t = 0
if(Val2t=="NaN") Val2t = 0
if(Val2t=="Inf") Val2t = 0
if(!is.numeric(Val2t)) Val2t=0
ScoTxt=append(ScoTxt,Val2t)
ScoTxtY=append(ScoTxtY,Val2t)
 if(c<=RM)
   ScoTxtYT[a]=ScoTxtYT[a]+Val2t
SiVal2t=SiVal2t+Val2t

#Val3=Data[b,3,c,a]
#if(is.na(Val3)==TRUE) Val3 = 0
#if(Val3=="NaN") Val3 = 0
#SiVal3=SiVal3+Val3
#if(Val3!=0) SiVal3=Val3


#Val4=Data[b,4,c,a]
#if(is.na(Val4)==TRUE) Val4 = 0
#if(Val4=="NaN") Val4 = 0
#SiVal4=SiVal4+Val4
#if(Val4!=0) SiVal4=Val4

} # year

scounV=MannKendall(ScoTxtY)
preV=MannKendall(PreY)
scoV=MannKendall(ScoY)

for (a2 in RESULTa)
   {
   
  preunp=append(preunp,preV$sl[1])
   preune=append(preune,preV$tau[1])
   preunv=append(preunv,var(PreY))
   preunm=append(preunm,mean(PreY))

   scounp=append(scounp,scounV$sl[1])
   scoune=append(scoune,scounV$tau[1])
   scounv=append(scounv,var(ScoY))
   scounm=append(scounm,mean(ScoY))

   txtunp=append(txtunp,scoV$sl[1])
   txtune=append(txtune,scoV$tau[1])
   txtunv=append(txtunv,var(ScoY))
   txtunm=append(txtunm,mean(ScoY))

   }

} # uni


#############
#write.table

########

scounV=MannKendall(ScoTxtYT)
preV=MannKendall(PreYT)
scoV=MannKendall(ScoYT)

for(a2 in RESULTa)
  {
for (c2 in RESULTc)
   {
   



   preunpT=append(preunpT,preV$sl[1])
   preuneT=append(preuneT,preV$tau[1])
   preunvT=append(preunvT,var(PreYT))
   preunmT=append(preunmT,mean(PreYT))

   scounpT=append(scounpT,scounV$sl[1])
   scouneT=append(scouneT,scounV$tau[1])
   scounvT=append(scounvT,var(ScoYT))
   scounmT=append(scounmT,mean(ScoYT))

   txtunpT=append(txtunpT,scoV$sl[1])
   txtuneT=append(txtuneT,scoV$tau[1])
   txtunvT=append(txtunvT,var(ScoTxtYT))
   txtunmT=append(txtunmT,mean(ScoTxtYT))

   }

} # uni


TableData=NULL
TableData=data.frame(University=Uni,Year=Yea,Version=Ver,Dictionary=Dic,Wo=Wor,ScoreTxt=ScoTxt,Presentage=Pre,Score=Sco,
PreunpT=preunpT,PreuneT=preuneT,PreunvT=preunvT,Preunm=preunmT,Scounp=scounpT,Scounm=scounmT,Scounv=scounvT,Scoune=scouneT,
Txtunp=txtunpT,Txtune=txtuneT,Txtunv=txtunvT,Txtunm=txtunmT,Preunp=preunp,Preune=preune,Preunv=preunv,Preunm=preunm,
Scounp=scounp,Scounm=scounm,Scounv=scounv,Scoune=scoune,Txtunp=txtunp,Txtune=txtune,Txtunv=txtunv,Txtunm=txtunm)


	if(!is.null(TableData))
				{

			write.table( TableData, file=TableFile, append = TRUE, sep = ",", eol ="\n", col.name = TRUE )
		}


########

if(Word_printing=="ON")
{
DIC=sprintf("topic%i",b)

if(SiVal==0) {
		wdBody(names(DIC))
		wdBody("NO SCORE") 
	}
	else 
	{
wdSection(DIC, newpage = T)


ou=NULL
out=NULL

Sys.sleep(pau)


dlen=length(lterms[,1])
if(dlen > lterm_q)
 {
    dlen=k
   ou=sprintf("%s / %s:",lterm_q,dlen )
   out=paste0(out,ou)
 }

RESULTg=c(1:lterm_q)
#RESULTg=c("1")
for (g in RESULTg) {
ou=sprintf("%s,",lterms[g,b] )
out=paste0(out,ou)
}

wdBody(out)

Sys.sleep(pau)

out=sprintf("\nTekstejä:%s\nTekstejä joissa osumia:%.f\n\n",SiVal, )
wdBody(out)

PlotData=NULL
PlotData=data.frame(University=Uni,Year=Yea,Presentage=Pre,Score=Sco,ScoreTxt=ScoTxt)

wdPlot(plotfun=funny5,method="metafile",height =3.5, width = 3.5,
pointsize = 1 )

Sys.sleep(pau)


wdPlot(plotfun=funny1,method="metafile",height = 3.5, width = 3.5,
pointsize = 1 ) #paragraph = TRUE

Sys.sleep(pau)

wdPlot(plotfun=funny2,method="metafile",height = 3.5, width = 3.5,
pointsize = 1 )

Sys.sleep(pau)

wdPlot(plotfun=funny3,method="metafile",height = 3.5, width = 3.5,
pointsize = 1 )

Sys.sleep(pau)

wdPlot(plotfun=funny4,method="metafile",height =3.5, width = 3.5,
pointsize = 1 )

Sys.sleep(pau)

word=dfm_subset(dfmat_news,dfmat_news$topic2==DIC)




docs=length(word[,1])

if( docs < 20 )
    { 
	E=docs 
	}
   else
    { 
	E=20 
	}

wordCL=NULL
out=NULL
RESULTd=c(1:E)
#RESULTd=c(1:1)

if(E!=0)
 for (d in RESULTd) {

  wordCL=append(wordCL,word@docvars$docname_[[d]])
   ou=NULL
    ou=sprintf("%s\n\n",word$'title'[[d]])
    out=paste0(out,ou)

  } #for    


# xray
#wordC=corpus_subset(CorpX, Name==wordCL )
#tokCL<-tokens(CorpX[wordCL])
#t_erms <- lterms[,b]
#wdPlot(plotfun=funny6,method="metafile",height =3.5, width = 3.5,pointsize = 1)

wdBody(out)
Sys.sleep(pau)
} #WORD OFF

} #siVal


if(Pdf_printing=="ON")
{

DIC=sprintf("topic%i",b)

if(SiVal==0) {
		#print(names(DIC))
		#print("NO SCORE") 
	}
	else 
	{
plot.new()	
txt <- DIC
text(0,.99,txt,pos=4)

ou=NULL
out=NULL


dlen=length(lterms[,b])
if(dlen > 10)
 {
    dlen=10
   ou=sprintf("10 / %s:",dlen )
   out=paste0(out,ou)
 }

RESULTg=c(1:dlen)
#RESULTg=c("1")
for (g in RESULTg) {
ou=sprintf("%s,",lterms[g,b] )
out=paste0(out,ou)
}

text(0,.94,out,pos=4)


out=sprintf("\nTekstejä:%s\nTekstejä joissa osumia:%.f\n\n",SiVal2t,SiVal )
text(0,.8,out,pos=4)

PlotData=NULL
PlotData=data.frame(University=Uni,Year=Yea,Presentage=Pre,Score=Sco,ScoreTxt=ScoTxt)

pairs.panels(PlotData,pch=21)

geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <- ggplot(PlotData,  aes(x=Year, y=Presentage, shape=University,color=University), height=2, width=2 )
print(c + geom_point(size=1) + 
    scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) +  
ylab("Osuus/Yliopisto") +
xlab("Vuosi") + 
ggtitle(names(DIC)) +
theme(text = element_text(size = 8)) +
scale_shape_manual(values = c(0,1,2,3,4,5,6,15,16,17,18,19,20,14)) + 
scale_size_manual(values = c(4,4,4,4,4,4,4,1,1,4,1,4,4,4))
)


geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <-	ggplot(PlotData,aes(x=University, y=Presentage,color=University ), height=2, width=2) 
print(c + geom_point() +
theme(text = element_text(size = 8)) +
ggtitle(names(DIC)) + 
geom_boxplot() + 
ylab("Osuttuja tekstejä") +
xlab("Yliopisto")
 )


geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <- ggplot(PlotData,  aes(x=Year, y=ScoreTxt, shape=University, color=University), height=2, width=2 )
print(c + geom_point(size=1) + 
    scale_y_continuous(trans='log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x))) + 
ylab("Osumia/Yliopisto") +
xlab("Vuosi") + 
ggtitle(names(DIC)) +
theme(text = element_text(size = 8)) +
scale_shape_manual(values = c(0,1,2,3,4,5,6,15,16,17,18,19,20,14)) + 
scale_size_manual(values = c(4,4,4,4,4,4,4,1,1,4,1,4,4,4)) 
)


geom.text.size = 8
theme.size = (14/5) * geom.text.size
c <-	ggplot(PlotData,aes(x=University, y=ScoreTxt, color=University ), height=2, width=2) 
print(c + geom_point() +
theme(text = element_text(size = 8)) +
ggtitle(names(DIC) ) +
ylab("Osumia") +
xlab("Yliopisto")
)

word=dfm_subset(dfmat_news,dfmat_news$topic2==DIC)

docs=length(word[,1])

if( docs < 20 ) E=docs 
if( docs > 20 ) E=20 
 

wordCL=NULL
out=NULL
RESULTd=c(1:E)
#RESULTd=c(1:1)
if(E!=0)
{
for (d in RESULTd) {

wordCL=append(wordCL,word@docvars$docname_[[d]])
ou=NULL
ou=sprintf("%s\n\n",word$'title'[[d]])
out=paste0(out,ou)

} #for    

# xray
wordC=corpus_subset(CorpX, docname_ %in% wordCL )
wordT=tokens(wordC)

print(textplot_xray(kwic(wordT,lterms[,b])))





}


} #siVal


} #b
}
if(Word_printing=="ON")
{
wdSave(WordFile)

Sys.sleep(pau)
Sys.sleep(pau)    
Sys.sleep(pau)

wdQuit()                                                                                     
Sys.sleep(pau)
Sys.sleep(pau)
}


if(Pdf_printing=="ON")
{
	dev.off()
}





