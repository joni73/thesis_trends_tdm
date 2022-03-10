
	#TableData=data.frame(ScoreTxt=sum(ScoT),Sign=sign,University=Uni,Year=Yea,Version=Ver,Dictionary=Dic,Wo=Wor,Presentage=Pre,Score=Sco,
	#PreunpT=as.numeric(tab1[a,"PreunpT"]),PreuneT=0,PreunvT=0,Preunm=0,Scounp=scounpT,Scounm=scounmT,Scounv=scounvT,Scoune=scouneT,
	#Txtunp=txtunpT,Txtune=txtuneT,Txtunv=txtunvT,Txtunm=txtunmT,Preunp=as.numeric(tab1[a,"Preunp"]),Preune=preune,Preunv=preunv,Preunm=preunm,
	#Scounp=scounp,Scounm=scounm,Scounv=scounv,Scoune=scoune,Txtunp=txtunp,Txtune=txtune,Txtunv=txtunv,Txtunm=txtunm)


uniN <- c(
	"AFFIL(LUT University)",
	"AFFIL(Aalto University)",
	"AFFIL(University of Tampere)",
	"AFFIL(University of Oulu)",
	"AFFIL(Massachusetts Institute of Technology)",
	"AFFIL(Karlsruhe Institute of Technology)",
	"AFFIL(Tsinghua University)",
	"AFFIL(Xi'an Jiaotong University)",
	"AFFIL(Islamic Azad University)",
	"AFFIL(LUT University)",
	"AFFIL(Fraunhofer-Gesellschaft)",
	"Fund-All(Finnish Innovation Fund Sitra)",
	"Fund-All(Business Finland)",
	"FUND-ALL(dimecc)" ) 
#"LUT","ALL","TAM","OUL","MIT","KIT","TSI","JIO","AZA",
uni <- c( "LUT2","FRA","SIT","BF","DIM")
RM=9 #institutions removed from total statistic due include duplicates
year <- c("21","20","19","18","17")
yearN <- c(21,20,19,18,17)


dics=c(
       "WORDSTAT/Brand Personality.cat",                   #1
       "WORDSTAT/Corporate Social Responsibility.cat",     #2
       "WORDSTAT/FOREST.cat",                              #3
       "MEGATRENDS/MEGAS1.cat",                            #4
       "MEGATRENDS/SITRA2016.cat",                         #5
       "WORDSTAT/ROGET.cat",                               #6
       "TUTKIMUS/THEORY3.cat",                             #7
       "WORDSTAT/RID.cat",                                 #8
       "TUTKIMUS/deslist3.cat",                             #9
       "TUTKIMUS/etallist3.cat",                            #10
       "TUTKIMUS/des3.cat",                                 #11
       "TUTKIMUS/query.cat",                               #12
       "MEGATRENDS/TeknologiaTeollisuus3.cat"              #13
       )



setwd("//192.168.0.42/mpi/DITYO2")

fil=list.files(path="RAAKAANAL",pattern="*.csv",full.names=TRUE)
fil





#YLEISET

fid="YLEISET"

 tab1=read.table(file=fil[31],sep=",",blank.lines.skip=TRUE,fill=TRUE,row.names=NULL)
 
 for( a in c(23,24) )
 tab1=rbind(tab1,read.table(file=fil[a],sep=",",blank.lines.skip=TRUE,fill=TRUE,row.names=NULL))


#MEGA

fid="MEGA"

tab1=read.table(file=fil[28],sep=",",blank.lines.skip=TRUE,fill=TRUE,row.names=NULL)
 
 for( a in c(29) )
 tab1=rbind(tab1,read.table(file=fil[a],sep=",",blank.lines.skip=TRUE,fill=TRUE,row.names=NULL))



uni <- c( "LUT","ALL","TAM","OUL","MIT","KIT","TSI","JIO","AZA","LUT2","FRA","SIT","BF","DIM")

#koti. u=="ALL"||u=="LUT"||u=="TAM"||u=="OUL"
#kans. u=="AZA"||u=="JIO"||u=="KIT"||u=="TSI"||u=="MIT"

for( run in c("SIT") )
{

print("XXXXXXXXXXXXXX")
print(run)

countP=0
countP2=0
countS=0
all=0
pC=0

TableData=NULL
TableALL=NULL
TableData_Y=NULL
TableData_N=NULL

for(a in c(1:length(tab1[,"Preunp"])))
{
  
p=as.numeric(tab1[a,"Preunp"])
p2=as.numeric(tab1[a,"PreunpT"])
s=as.numeric(tab1[a,"Score"])
u=as.character(tab1[a,"University"])

if(is.na(p)) p=1
if(is.na(p2)) p2=1
if(is.na(s)) s=0
if(is.na(u)) 
  {
     u="" 
     print("error u")
  }

#u=="ALL"||u=="LUT"||u=="TAM"||u=="OUL"
#u=="AZA"||u=="JIO"||u=="KIT"||u=="TSI"||u=="MIT"
#u=run
if(u=="SIT")
 {
  if(p<0.05)
	{
	#print(tab1[a,] )
      countP=countP+1
      #print(tab1[a,6] )
	}
  if(s>100)
	{
	#print(tab1[a,] )
      countS=countS+1
      #print(tab1[a,6] )
	}
  if(p<0.05 && p2<0.05 && s<20)
      {
      all=all+1
      #print(tab1[a,"University"] )
      #print(tab1[a,"Wo"] )
      }

  if(p < 0.05  )
	{
     if(tab1[a, "Year"]=="21") ScoT=NULL

     ScoT=append(ScoT,as.numeric(tab1[a,"ScoreTxt"]))

     if(tab1[a, "Year"]=="17") {
      
      if( length(ScoT)==5 )
        Cv=cor(ScoT,c(length(ScoT):1))
          else 
            Cv=0

      if( is.na(Cv) ) Cv=0
      if(Cv > 0.10 ) sign="+"
      if(Cv < -0.10 ) sign="-"
      if(Cv >= -0.10 && Cv <= 0.10) sign=" "

      out=sprintf("%.0f %s %1.2f %s %s p=%2.2f p2=%2.2f s=%.0f sp=%2.2f %.0f:%s", 
                  sum(ScoT),
			sign,
			Cv,
 			tab1[a,"University"],tab1[a, "Year"],
			as.numeric(tab1[a,"Preunp"]),as.numeric(tab1[a,"PreunpT"]),as.numeric(tab1[a,"ScoreTxt"]),
			as.numeric(tab1[a,"Scounp"]),as.numeric(tab1[a,"Dictionary"]),tab1[a,"Wo"])

      TableData=tab1[a,]
	
	TableData=cbind(CV=Cv,TableData)
      TableData=cbind(Sign=sign,TableData)
      TableData=cbind(ScoreTxtT=sum(ScoT),TableData)
     
      TableALL=rbind(TableALL,TableData)
      
	#write.table( TableData, file=TableFile, append = TRUE, sep = ",", eol ="\n", col.name = TRUE )
	
      
      countP2=countP2+1
      pC=pC+1
      #print(tab1[a,6] )
         
	} #year  
     } #p 
   } #u
 } #for a


print(countP)
print(countP2)
print(countS)
print(all)

TableData_Y=NULL
TableData_N=NULL
for(a in c(1:length(tab1[,"Preunp"]))) 
{
p=as.numeric(tab1[a,"Preunp"])
p2=as.numeric(tab1[a,"PreunpT"])
s=as.numeric(tab1[a,"Score"])
u=as.character(tab1[a,"University"])

if(is.na(p)) p=1
if(is.na(p2)) p2=1
if(is.na(s)) s=0
if(is.na(u)) 
  {
     u="" 
     print("error u")
  }

 if(p < 0.05 && u!="SIT" )
	{

     if(tab1[a, "Year"]=="21") ScoT=NULL

     ScoT=append(ScoT,as.numeric(tab1[a,"ScoreTxt"]))

     if(tab1[a, "Year"]=="17")
	{
      
      if( length(ScoT)==5 )
        Cv=cor(ScoT,c(length(ScoT):1))
          else 
            Cv=0

      if( is.na(Cv) ) Cv=0
      if(Cv > 0.10 ) sign="+"
      if(Cv < -0.10 ) sign="-"

      if(Cv >= -0.10 && Cv <= 0.10) sign=" "

      TableData=tab1[a,]
	TableData=cbind(CV=Cv,TableData)
      TableData=cbind(Sign=sign,TableData)
      TableData=cbind(ScoreTxtT=sum(ScoT),TableData)
      
        #Sort[Sort[,"Wo"]=="R","Sign"]
     WOR=as.character(TableData["Wo"])
     ch=TableALL[TableALL[,"Wo"]==WOR,"Sign"]

 if(!(is.na(sign)||is.null(sign)))
    if(sign!=" ")
   if(length(ch)!=0)
    {
    if( ch==sign) 
      TableData_Y=rbind(TableData_Y,TableData)
    else 
      TableData_N=rbind(TableData_N,TableData)
    }
       } #P
         } #17
	
} #for  a



length(TableData_Y[,1])
length(TableData_N[,1])





PlanOut <- function(){


Sort3=Sort
L=length(Sort3[,1])
if(L>5) L=5
if(L>0)
for( a in c(1:L) )
 {
      out=sprintf("%6.0f %s %1.2f %s %s p=%2.2f p2=%2.2f s=%.0f sp=%2.2f %.0f:%s", 
                  as.numeric(Sort3[a,"ScoreTxtT"]),
			Sort3[a,"Sign"],
			as.numeric(Sort3[a,"CV"]),
 			Sort3[a,"University"],Sort[a, "Year"],
			as.numeric(Sort3[a,"Preunp"]),as.numeric(Sort3[a,"PreunpT"]),as.numeric(Sort3[a,"ScoreTxt"]),
			as.numeric(Sort3[a,"Scounp"]),as.numeric(Sort3[a,"Dictionary"]),Sort3[a,"Wo"])
      out=sprintf("%s %s", 
                 	Sort3[a,"University"],Sort3[a,"Wo"])
       print(out)
   

}

}

PlanOutOw <- function(){


print(out)
Sort3=Sort
L=length(Sort3[,1])
if(L>20) L=20
if(L>0)
for( a in c(1:L) )
 {
      out=sprintf("%6.0f %s %1.2f %s %s p=%2.2f p2=%2.2f s=%.0f sp=%2.2f %.0f:%s", 
                  as.numeric(Sort3[a,"ScoreTxtT"]),
			Sort3[a,"Sign"],
			as.numeric(Sort3[a,"CV"]),
 			Sort3[a,"University"],Sort[a, "Year"],
			as.numeric(Sort3[a,"Preunp"]),as.numeric(Sort3[a,"PreunpT"]),as.numeric(Sort3[a,"ScoreTxt"]),
			as.numeric(Sort3[a,"Scounp"]),as.numeric(Sort3[a,"Dictionary"]),Sort3[a,"Wo"])
      out=sprintf("%s %s", 
                 	Sort3[a,"University"],Sort3[a,"Wo"])
       print(out)
   }
}

##A


PlanOutA <- function(){


Sort3=Sort

L=length(Sort3[,1])
if(L>500) L=500
if(L>0)
for( a in c(1:L) )
 {
      out=sprintf("%6.0f %s %1.2f %s %s p=%2.2f p2=%2.2f s=%.0f sp=%2.2f %.0f:%s", 
                  as.numeric(Sort3[a,"ScoreTxtT"]),
			Sort3[a,"Sign"],
			as.numeric(Sort3[a,"CV"]),
 			Sort3[a,"University"],Sort[a, "Year"],
			as.numeric(Sort3[a,"Preunp"]),as.numeric(Sort3[a,"PreunpT"]),as.numeric(Sort3[a,"ScoreTxt"]),
			as.numeric(Sort3[a,"Scounp"]),as.numeric(Sort3[a,"Dictionary"]),Sort3[a,"Wo"])
      #out=sprintf("%s %s", 
      #           	Sort3[a,"University"],Sort3[a,"Wo"])
       print(out)
   }
}

PlanOutOwA <- function(){


Sort3=Sort

L=length(Sort3[,1])
if(L>1000) L=1000
if(L>0)
for( a in c(1:L) )
 {
      out=sprintf("%6.0f %s %1.2f %s %s p=%2.2f p2=%2.2f s=%.0f sp=%2.2f %.0f:%s", 
                  as.numeric(Sort3[a,"ScoreTxtT"]),
			Sort3[a,"Sign"],
			as.numeric(Sort3[a,"CV"]),
 			Sort3[a,"University"],Sort[a, "Year"],
			as.numeric(Sort3[a,"Preunp"]),as.numeric(Sort3[a,"PreunpT"]),as.numeric(Sort3[a,"ScoreTxt"]),
			as.numeric(Sort3[a,"Scounp"]),as.numeric(Sort3[a,"Dictionary"]),Sort3[a,"Wo"])
      #out=sprintf("%s %s", 
      #           	Sort3[a,"University"],Sort3[a,"Wo"])
       print(out)
   }

}


if( !is.null(TableALL) )
{



Sort<-TableALL[TableALL[,"Sign"]=="+",]
if(!is.null(Sort))
	{
	SortOWP<-Sort[with(Sort,order(Scounv,ScoreTxt,decreasing=TRUE,method="radix")),]
	}
Sort<-TableALL[TableALL[,"Sign"]=="-",]
if(!is.null(Sort))
	{
	SortOWN<-Sort[with(Sort,order(Scounv,ScoreTxt,decreasing=TRUE,method="radix")),]
	}
Sort<-TableData_Y
if(!is.null(Sort))
	{
	SortY<-Sort[with(Sort,order(Scounv,ScoreTxt,decreasing=TRUE,method="radix")),]
	}
Sort<-TableData_N
if(!is.null(Sort))
	{
	SortN<-Sort[with(Sort,order(Scounv,ScoreTxt,decreasing=TRUE,method="radix")),]
	}

SI=sprintf("C:/Users/Joni-/LUT/DITYO2/RAAKAANAL/%s_%s_v2.TXT",run,fid)
sink(SI,append=TRUE)

print(SI)
print(run)
print(countP)
print(countP2)
print(countS)
print(all)

print("Positive trends")
Sort=SortOWP
PlanOutOw()

print("Negative trends")
Sort=SortOWN
PlanOutOw()

print("Same trend")
Sort=SortY
PlanOut()

print("Opposite trend")
Sort=SortN
PlanOut()

sink()


SI=sprintf("C:/Users/Joni-/LUT/DITYO2/RAAKAANAL/%s_TOT_%s_v2.TXT",run,fid)
sink(SI,append=TRUE)

print(SI)
print(run)
print(countP)
print(countP2)
print(countS)
print(all)

print("Positive trends")
Sort=SortOWP
PlanOutOwA()

print("Negative trends")
Sort=SortOWN
PlanOutOwA()

print("Same trend")
Sort=SortY
PlanOutA()

print("Opposite trend")
Sort=SortN
PlanOutA()

sink()
   }


}  #run