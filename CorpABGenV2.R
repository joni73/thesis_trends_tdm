library("quanteda")
library("rscopus")
library("dplyr")

require(quanteda.corpora)
require(seededlda)
require(lubridate)



f <- NULL
abs <- NULL
YearX <- NULL
UniX <- NULL
sco <- NULL
tit <- NULL
agr <- NULL
pub <- NULL
dol <- NULL
etal <- NULL
cre <- NULL
des <- NULL

resea <- NULL
subje <- NULL

u <- c(
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
  "FUND-ALL(dimecc)"
)

uni <- c("LUT", "ALL", "TAM", "OUL", "MIT", "KIT", "TSI", "JIO", "AZA", "LUT2", "FRA", "SIT", "BF", "DIM")
year <- c("21", "20", "19", "18", "17")


RESULTB <- c(1:5)
for (b in RESULTB) {
  RESULTC <- c(1:14)
  for (c in RESULTC) {
    yS <- sprintf("AB_%s_%s_", uni[c], year[b])

    files <- list.files(path = "AB3", pattern = yS, full.names = TRUE)

    for (a in files) {
      tryCatch(
        {
          load(a)
        },
        warning = function(war) {
          print(paste("MY WARNING: ", war))
        },
        error = function(err) {
          print(paste("MY ERROR: ", err))
        },
        {
          if (!is.atomic(x)) {
            print(a)
            #
            if (length(x$content$`service-error`$status$statusCode) == 0 || all("RESOURCE_NOT_FOUND" != x$content$`service-error`$status$statusCode)) {
              if (all(typeof(x$content$"abstracts-retrieval-response"$coredata$`dc:description`) != "character")) {
                abs <- append(abs, "NC")
              } else {
                abs <- append(abs, x$content$"abstracts-retrieval-response"$coredata$`dc:description`)
              }

              UniX <- append(UniX, uni[c])

              YearX <- append(YearX, year[b])

              # if(length( x$content$'abstracts-retrieval-response'$coredata$`dc:identifier` )==0 )
              sco <- append(sco, a)
              #             else
              #           x$content$'abstracts-retrieval-response'$coredata$`dc:identifier` )
              if (length(x$content$"abstracts-retrieval-response"$coredata$`dc:title`) == 0) {
                tit <- append(tit, "NC")
              } else {
                tit <- append(tit, x$content$"abstracts-retrieval-response"$coredata$`dc:title`)
              }
              if (length(x$content$"abstracts-retrieval-response"$coredata$`prism:aggregationType`) == 0) {
                agr <- append(agr, "NC")
              } else {
                agr <- append(agr, x$content$"abstracts-retrieval-response"$coredata$`prism:aggregationType`)
              }
              if (length(x$content$"abstracts-retrieval-response"$coredata$`prism:publicationName`) == 0) {
                pub <- append(pub, "NC")
              } else {
                pub <- append(pub, x$content$"abstracts-retrieval-response"$coredata$`prism:publicationName`)
              }
              if (length(x$content$"abstracts-retrieval-response"$coredata$`prism:publisher`) == 0) {
                dol <- append(dol, "NC")
              } else {
                dol <- append(dol, x$content$"abstracts-retrieval-response"$coredata$`prism:publisher`)
              }


              if (length(x$content$"abstracts-retrieval-response"$coredata$`dc:creator`) == 0) {
                etal <- append(etal, "NC")
              } else
              if (!is.atomic(x$content$"abstracts-retrieval-response"$coredata$`dc:creator`[[1]])) {
                etal <- append(etal, x$content$"abstracts-retrieval-response"$coredata$`dc:creator`[[1]]$`$`)
              } else {
                etal <- append(etal, x$content$"abstracts-retrieval-response"$coredata$`dc:creator`$`$`)
              }


              for (f in x$content$"abstracts-retrieval-response"$coredata$`dc:creator`) {
                if (!is.atomic(f)) {
                  resea <- append(resea, f$`$`)
                }
                # else
                # resea=append(resea,f )
              }

              if (length(x$content$"abstracts-retrieval-response"$coredata$`dc:creator`) == 0) {
                cre <- append(cre, "NC")
              } else {
                cre <- append(cre, length(x$content$"abstracts-retrieval-response"$coredata$`dc:creator`))
              }


              if (length(x$content$"abstracts-retrieval-response"$coredata$`dcterms:subject`) == 0) {
                des <- append(des, "NC")
              } else
              if (!is.atomic(x$content$"abstracts-retrieval-response"$coredata$`dcterms:subject`[[1]])) {
                des <- append(des, x$content$"abstracts-retrieval-response"$coredata$`dcterms:subject`[[1]]$`$`)
              } else {
                des <- append(des, x$content$"abstracts-retrieval-response"$coredata$`dcterms:subject`$`$`)
              }


              for (f in x$content$"abstracts-retrieval-response"$coredata$`dcterms:subject`) {
                if (!is.atomic(f)) {
                  subje <- append(subje, f$`$`)
                }
                # else
                # resea=append(resea,f )
              }
            } # if
          } # atomic
        }
      )
    }
  } # univer
} # year



dv <- data.frame(University = UniX, Year = YearX, creators = cre, title = tit, aggregationType = agr, publicationName = pub)

CorpXAB <- corpus(abs, docnames = sco, docvars = dv)

save(CorpXAB, file = "CorpXABV5")

save(CorpX, file = "CorpXABV5")
save(CorpX, file = "CorpXABV5")
save(etal, file = "CorpXABV5etal")
save(des, file = "CorpXABV5des")
save(UniX, file = "CorpXABV5UniX")
save(YearX, file = "CorpXABV5YearX")
save(sco, file = "CorpXABV5sco")
save(dv, file = "CorpXABV5dv")
