
library(quanteda)
library('bibliometrix')
library('rscopus')
library('dplyr')
library(stringi)

inr=inst_token_header("XXXxxxXXX")
options("elsevier_api_key" = "XXXxxxXXX")
api_key=get_api_key
apikey3= "XXXxxxXXX"

load("CorpXV2")
load("CorpXV2etal")
load("CorpXV2UniX")

#resea=cre

          
idByAuthor2 <- function (df,apikey, inr) 
{
    n = dim(df)[1]
    AU_ID = NA
    AU_AFF = NA
    AU_count = NA
    for (j in 1:n) {
        lastname = tolower(df[j, 1])
        firstname = tolower(df[j, 2])
        if (!is.na(df[j, 3])) {
            query1 = paste("affil(", df[j, 3], ")", sep = "")
        }
        else {
            query1 = NULL
        }
        cat("\nSearching author's info: ", toupper(df[j, 1]), 
            toupper(df[j, 2]))
            
            print( firstname )
        Sys.sleep(3)     
        AU_info = get_complete_author_info(last_name = lastname, 
            first_name = firstname, api_key = apikey, query = query1, headers=inr, verbose=TRUE)
            
            
            print(AU_info)
            
        if (AU_info$content$`search-results`$`opensearch:totalResults` != 
            0) {
            AU_ID[j] = AU_info[[2]]$`search-results`$entr[[1]]$`dc:identifier`
            AU_ID[j] = gsub("AUTHOR_ID:", "", AU_ID[j])
            AU_info2 = AU_info[[2]]
            aff = AU_info2$`search-results`$entry[[1]]$`affiliation-current`
            AU_AFF[j] = paste(aff$`affiliation-name`, ", ", aff$`affiliation-city`, 
                ", ", aff$`affiliation-country`, sep = "")
            AU_count[j] = AU_info[[2]]$`search-results`$entr[[1]]$`document-count`
        }
        else {
            AU_ID[j] = NA
            AU_AFF[j] = NA
            AU_count[j] = NA
        }
    }
        print("DONE")
    authorsID = data.frame(lastname = df[, 1], firstname = df[, 
        2], id = AU_ID, affiliation = AU_AFF, count = AU_count, 
        stringsAsFactors = FALSE)
        
        print(authorsID)
        
    return(authorsID)
}

retrievalByAuthorID2 <- function (id, apikey, inr, remove.duplicated = TRUE, country = TRUE) 
{
    id = id[!is.na(id)]
    M_list = list()
    n = length(id)
    nomi = c("au_id", "name", "affil_id", "affilname", "n_auth", 
        "n_affils", "citations", "journal", "description", "title", 
        "pii", "doi", "eid", "cover_date", "cover_display_date", 
        "prism_url", "dc_identifier", "dc_creator", "prism_issn", 
        "prism_eIssn", "prism_pageRange", "dc_description", "prism_aggregationType", 
        "subtype", "authkeywords", "source_id")
    M = data.frame(matrix(NA, 1, length(nomi)))
    names(M) = nomi
    for (j in 1:n) {
        AU_ID = id[j]
        cat("\n Query n. ", j, "   Author ID: ", AU_ID)
        Sys.sleep(3) 
        AU_S <- tryCatch(author_df_orig(au_id = AU_ID, api_key = apikey, headers = inr,
            all_author_info = TRUE, verbose = FALSE), error = function(e) err = 1)
        if (class(AU_S) != "numeric") {
            AU_S$cover_date = substr(as.character(AU_S$cover_date), 
                1, 4)
            for (i in 1:dim(AU_S)[2]) {
                if (is.factor(AU_S[[i]])) {
                  AU_S[[i]] = as.character(AU_S[[i]])
                }
            }
            M_AU = data.frame(AU_S, stringsAsFactors = FALSE)
            if (dim(M_AU)[2] <= dim(M)[2]) {
                M_AU[setdiff(names(M), names(M_AU))] = NA
            }
            M = rbind(M, M_AU[names(M)])
            M_list[[j]] = M_AU
            names(M_list)[j] = id[j]
        }
        else {
            cat("\n Error in id:", AU_ID, "retrieval\n")
        }
    }
    M = M[-1, ]
    names(M) = c("AU_ID", "AU", "C1_ID", "C1", "nAU", "nC1", 
        "TC", "SO", "DT", "TI", "PII", "DI", "EID", "PY", "CDD", 
        "URL", "UT", "AU1", "ISSN", "EISSN", "PAG", "AB", "PT", 
        "SUBTYPE", "DE", "SO_ID")
    if (isTRUE(remove.duplicated)) {
        d = duplicated(gsub("[^[:alnum:] ]", "", M$UT))
        cat("\n", sum(d), "duplicated documents have been removed\n")
        M = M[!d, ]
    }
    M$CR = NA
    M$DB = "SCOPUS"
    M$DE = gsub("\\| ", ";", M$DE)
    M$ID = M$DE
    if (isTRUE(country)) {
        M$AU_CO = paste(M$C1_ID, ";", sep = "")
        cat("\nAuthors' country retrieval\n\n")
        aff_id = sort(unique(unlist(strsplit(M$C1_ID, ";"))))
        aff_id = aff_id[nchar(aff_id) > 1]
        AFF = data.frame(ID = NA, NAME = NA, CO = NA)
        for (i in 1:length(aff_id)) {
            a = affiliation_retrieval(aff_id[i], api_key = apikey, headers = inr,
                verbose = FALSE)
            AFF[i, 1] = aff_id[i]
            if (length(a$content$`affiliation-retrieval-response`$`affiliation-name`) > 
                0) {
                AFF[i, 2] = a$content$`affiliation-retrieval-response`$`affiliation-name`
            }
            if (length(a$content$`affiliation-retrieval-response`$country) > 
                0) {
                AFF[i, 3] = a$content$`affiliation-retrieval-response`$country
            }
            cat("\nAffiliation ID: ", AFF[i, 1], "   Name: ", 
                AFF[i, 2], ",", AFF[i, 3])
            M$AU_CO = gsub(paste(aff_id[i], ";", sep = ""), paste(AFF[i, 
                3], ";", sep = ""), M$AU_CO)
        }
        M$AU_CO = gsub(";;", ";", M$AU_CO)
        M$AU_CO[nchar(M$AU_CO) < 3] = NA
        M$AU1_CO = unlist(lapply(strsplit(M$AU_CO, ";"), function(l) {
            l = l[1]
        }))
        UN = strsplit(M$C1, ";")
        CO = strsplit(M$AU_CO, ";")
        for (i in 1:length(UN)) {
            M$C1[i] = paste(paste(UN[[i]], ", ", CO[[i]], sep = ""), 
                collapse = ";")
        }
    }
    M <- data.frame(lapply(M, toupper), stringsAsFactors = FALSE)
    M$TC = as.numeric(M$TC)
    M$PY = as.numeric(M$PY)
    M$DB = "SCOPUS"
    M$RP = unlist(lapply(strsplit(M$C1, ";"), function(l) {
        l = l[1]
    }))
    M$CR <- NA
    M$J9 <- M$JI <- M$SO
    suppressWarnings(M <- metaTagExtraction(M, Field = "SR"))
    SR = M$SR
    tab = table(SR)
    tab2 = table(tab)
    ind = as.numeric(names(tab2))
    ind = ind[which(ind > 1)]
    if (length(ind) > 0) {
        for (i in ind) {
            indice = names(which(tab == i))
            for (j in indice) {
                indice2 = which(SR == j)
                SR[indice2] = paste(SR[indice2], as.character(1:length(indice2)), 
                  sep = " ")
            }
        }
    }
    row.names(M) <- SR
    results <- list(M = M, authorDocuments = M_list)
    return(results)
}


u <- c(
	#"AFFIL(LUT University)",
	#"AFFIL(Aalto University)",
	#"AFFIL(University of Tampere)",
	#"AFFIL(University of Oulu)",
	#"AFFIL(Massachusetts Institute of Technology)",
	#"AFFIL(Karlsruhe Institute of Technology)",
	#"AFFIL(Tsinghua University)",
	#"AFFIL(Xi'an Jiaotong University)",
	#"AFFIL(Islamic Azad University)"
	#"AFFIL(LUT University)",
	#"AFFIL(Fraunhofer-Gesellschaft)",
	#"Fund-All(Finnish Innovation Fund Sitra)",
	#"Fund-All(Business Finland)",
	"FUND-ALL(dimecc oy)" 
	#"AFFIL(Islamic Azad University)",
	#"AFFIL(Xi'an Jiaotong University)",
	#"AFFIL(Tsinghua University)",
	) 

uni <- c( 
       #"LUT",
       #"ALL",
       #"TAM","OUL",
       #"MIT",
       #"KIT",
       #"TSI",
       ##"JIO","AZA"
       #"LUT2","FRA",
       #"SIT",
       #"BF",
       #"DIM",
       "AZA",
       "JIO",
       "TSI"
        )

afil <- c(
	#"LUT University",
	#"Aalto University",
	#"University of Tampere",
	#"University of Oulu",
	#"Massachusetts Institute of Technology",
	#"Karlsruhe Institute of Technology",
	#"Tsinghua University",
	#"Xi'an Jiaotong University",
	#"Islamic Azad University"
	#"LUT University",
	#"Fraunhofer-Gesellschaft",
	#"Finnish Innovation Fund Sitra",
	#"Business Finland",
	#"dimecc oy",
	"Islamic Azad University",
	"Xi'an Jiaotong University",
	"Tsinghua University"
	 ) 

         

af=data.frame( Uni=uni, UniN=afil )

id=NULL
df=NULL
res2=NULL
reseaa=NULL
UniX2=NULL
#reseaa=CorpX$creator

for( etx in c(1:length(etal)) )
{
if(etal[[etx]][1]!="")
 if(length(etal[[etx]][])==1)
  {
  reseaa=append(reseaa,etal[[etx]][1])
  UniX2=append(UniX2,UniX[etx])
  }
 else
  for( etx2 in c(1:length(etal[[etx]][])) )
  {
    reseaa=append(reseaa,etal[[etx]][etx2])
    UniX2=append(UniX2,UniX[etx])
    }
}

resea=reseaa

inr=inst_token_header("XXXxxxXXX")
options("elsevier_api_key" = "XXXxxxXXX")
api_key=get_api_key
apikey3= "XXXxxxXXX"

for( c in uni )
{

print("University on road:")
print(c)
print("\n################################\n")


#create data frame with 0 rows and 5 columns
df <- data.frame(matrix(ncol = 3, nrow = 0))

#provide column names
colnames(df) <- c('lastname', 'firstname', 'affiliation')



for( a in 1:length(resea) )
{

restN=str_replace_all(resea[a],'([(")])', "")
split=strsplit(restN, split = ", ")
#split2=substring(split[[1]][2],2)


au=which(af[,1] %in% UniX2[a])
if ( UniX2[a] %in% c(c) ) {
 df[nrow(df) + 1,] = c( lastname=split[[1]][1], firstname=split[[1]][2], affiliation=af[[au,2]] )
 #df[nrow(df) + 1,] = c( lastname=split[[1]][1], firstname=split[[1]][2], affiliation="Finland" )
} #dose work at uni

} # Etal

Sys.sleep(1) 
D <- idByAuthor2(df, apikey3, inr)
id <- D[,3]

Sys.sleep(6) 
res2=NULL
res2=retrievalByAuthorID2(id, apikey3, inr, remove.duplicated = TRUE)

if(!is.null(res2))
{
ser_file=sprintf("BIBLIOMETRIX2/ME_%s",c)
save(res2,file=ser_file)
}

} #UNI

#####

M2 <- res2$M

# M <- res$authorDocuments

results <- biblioAnalysis(M, sep = ";")

results <- biblioAnalysis(M, sep = ";")
results
$Articles

M <- metaTagExtraction(B1, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")


