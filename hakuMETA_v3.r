########
library("quanteda")
library("rscopus")

PLUMX <- "YES"

f <- NULL
abs <- NULL
YearX <- NULL
UniX <- NULL
sco <- NULL
tit <- NULL
cit <- NULL # citations count
doi <- NULL
cre <- NULL
afi <- NULL # affiliations

agr <- NULL
pub <- NULL
dol <- NULL
etal <- NULL
afi <- NULL
des <- NULL

resea <- NULL
subje <- NULL

hin <- NULL # h-index


subje <- NULL

etal_add <- NULL

## plumx

TYPES <- NULL
CAPT <- NULL
CITBY <- NULL
CITA <- NULL
RECO <- NULL
TWEE <- NULL
REFE <- NULL
ABSTR <- NULL
FACE <- NULL
LINK <- NULL
DOWN <- NULL
NEWS <- NULL


inr <- inst_token_header("xxxXXXxxx")
options("elsevier_api_key" = "xxxXXXxxx")
api_key <- get_api_key

base_query <- "( SUBJAREA(ENGI) ) AND ( ({manufacturing} or {production}) AND {engineering}) AND ( {welding} OR  {casting} OR {sheet metal} OR {additive} OR {machining} OR {CNC} OR {drilling} OR {turning} OR {shavings} OR {cutting} OR {surface finishing} OR {polishing} OR {heat treatments} OR {quenching} OR {tempering} OR {surface harndening} ) AND ( {safety} OR {environment} OR {environmental} OR {environment} OR {sustainable} OR {sustainability} OR {labour} OR {economical} OR {green technology} OR {make} OR {technique} OR {machine} )"

p <- c("PUBYEAR = 2021", "PUBYEAR = 2020", "PUBYEAR = 2019", "PUBYEAR = 2018", "PUBYEAR = 2017")
year <- c("21", "20", "19", "18", "17")

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
# RESULTC=c(1)
RESULTC <- c(1:14)
max <- 4000
for (cm in RESULTC) {
  RESULTB <- c(1:5)
  # RESULTB=c(1:1)
  for (bm in RESULTB) {
    ser_file <- sprintf("%s/SEARCH_%s", uni[cm], year[bm])
    load(file = ser_file)


    if (res$total_results > 0) {
      RESULT <- c(1:res$total_results)
      # RESULT=c("1")
      for (am in RESULT) {
        fetch_file <- strsplit(res$entries[[am]]$`dc:identifier`, ":")
        art_file <- sprintf("%s/AR_%s_%s", uni[cm], year[bm], fetch_file[[1]][2])
        print("===========")
        print(fetch_file[[1]][2])
        # print(length(sco))

        # print("Etal lenght:")
        # print(length(etal))
        print(art_file)

        if (file.exists(art_file)) {
          load(art_file)

          print("==")
          print(res$entries[[am]]$`dc:identifier`)
          print(fetch_file[[1]][2])
          print(x$content$`full-text-retrieval-response`$`scopus-id`)
          print("==")
          if (!is.null(x$content$`full-text-retrieval-response`$"originalText")) {
            if (fetch_file[[1]][2] == x$content$`full-text-retrieval-response`$`scopus-id`) {
              if (length(x$content$`service-error`$status$statusCode) == 0 || all("RESOURCE_NOT_FOUND" != x$content$`service-error`$status$statusCode)) {
                sco <- append(sco, fetch_file[[1]][2])
                # poista duplicaatit reportoi
                tit <- append(tit, res$entries[[am]]$"dc:title")
                cit <- append(cit, res$entries[[am]]$"citedby-count")
                doi <- append(doi, res$entries[[am]]$"prism:doi")
                cre <- append(cre, res$entries[[am]]$"dc:creator")
                afi[[length(afi) + 1]] <- list(res$entries[[am]]$"affiliation")
                print("BASIC")


                if (all(typeof(x$content$`full-text-retrieval-response`$"originalText") != "character")) {
                  abs <- append(abs, "")
                } else {
                  abs <- append(abs, x$content$`full-text-retrieval-response`$"originalText")
                }

                UniX <- append(UniX, uni[cm])

                YearX <- append(YearX, year[bm])


                if (length(x$content$`full-text-retrieval-response`$coredata$`prism:aggregationType`) == 0) {
                  agr <- append(agr, "")
                } else {
                  agr <- append(agr, x$content$`full-text-retrieval-response`$coredata$`prism:aggregationType`)
                }

                if (length(x$content$`full-text-retrieval-response`$coredata$`prism:publicationName`) == 0) {
                  pub <- append(pub, "")
                } else {
                  pub <- append(pub, x$content$`full-text-retrieval-response`$coredata$`prism:publicationName`)
                }

                if (length(x$content$`full-text-retrieval-response`$coredata$`prism:publisher`) == 0) {
                  dol <- append(dol, "")
                } else {
                  dol <- append(dol, x$content$`full-text-retrieval-response`$coredata$`prism:publisher`)
                }
                print("PUB")

                if (!is.atomic(x$content$`full-text-retrieval-response`$coredata$`dc:creator`) == 0) {
                  if (length(x$content$`full-text-retrieval-response`$coredata$`dc:creator`) == 0) {
                    etal_add <- append(etal_add, list(""))
                  } else {
                    if (!is.atomic(x$content$`full-text-retrieval-response`$coredata$`dc:creator`)) {
                      RESULTD <- c(1:length(x$content$`full-text-retrieval-response`$coredata$`dc:creator`))
                      etal_add <- NULL
                      for (dm in RESULTD) {
                        if (!is.atomic(x$content$`full-text-retrieval-response`$coredata$`dc:creator`[[dm]]$`$`)) {
                          etal_add <- append(etal_add, x$content$`full-text-retrieval-response`$coredata$`dc:creator`[[dm]]$`$`)
                        } else {
                          etal_add <- list(x$content$`full-text-retrieval-response`$coredata$`dc:creator`$`$`)
                        }
                      } # for
                      etal <- append(etal, list(etal_add))
                    }
                  }
                } else {
                  etal <- append(etal, list(""))
                }

                print("ETAL")
                # resea


                if (!is.atomic(x$content$`full-text-retrieval-response`$coredata$`dcterms:subject`) == 0) {
                  if (length(x$content$`full-text-retrieval-response`$coredata$`dcterms:subject`) == 0) {
                    des[[length(des) + 1]] <- list("")
                  } else {
                    if (!is.atomic(x$content$`full-text-retrieval-response`$coredata$`dcterms:subject`)) {
                      des_add <- NULL
                      RESULTD <- c(1:length(x$content$`full-text-retrieval-response`$coredata$`dcterms:subject`))

                      for (d in RESULTD) {
                        if (!is.atomic(x$content$`full-text-retrieval-response`$coredata$`dcterms:subject`[[d]]$`$`)) {
                          des_add <- append(des_add, x$content$`full-text-retrieval-response`$coredata$`dcterms:subject`[[d]]$`$`)
                        } else {
                          des_add <- append(des, list(x$content$`full-text-retrieval-response`$coredata$`dcterms:subject`[[d]]$`$`))
                        }
                      } # for

                      des[[length(des) + 1]] <- list(des_add)
                    }
                  }
                } else {
                  des <- append(des, list(""))
                }


                print("DES")
                ### SUBJE

                if (!is.atomic(x$content$`full-text-retrieval-response`$coredata$`prism:doi`)) {
                  doi <- append(doi, x$content$`full-text-retrieval-response`$coredata$`prism:doi`)
                }

                ### PLUMX

                if (PLUMX == "YES") {
                  value <- doi[length(doi)]
                  type <- "doi"
                  respl <- plumx_metrics(value, type, headers = inr)

                  if (length(respl$content$count_categories) != 0) {
                    RESULTPM <- c(1:length(respl$content$count_categories))
                    for (px in RESULTPM)
                    {
                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "capture") {
                        CAPT <- append(CAPT, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }

                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "CITED_BY_COUNT") {
                        CITBY <- append(CITBY, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }


                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "citation") {
                        CITA <- append(CITA, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }

                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "READER_COUNT") {
                        RECO <- append(RECO, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }

                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "TWEET_COUNT") {
                        TWEE <- append(TWEE, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }

                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "REFERENCE_COUNT") {
                        REFE <- append(REFE, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }

                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "ABSTRACT_VIEWS") {
                        ABSTR <- append(ABSTR, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }

                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "FACEBOOK_COUNT") {
                        FACE <- append(FACE, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }

                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "NEWS_COUNT") {
                        NEWS <- append(NEWS, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }


                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "DOWNLOAD_COUNT") {
                        DOWN <- append(DOWN, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }


                      if (respl$content$count_categories[[px]]$count_types[[1]]$name == "LINK_OUTS") {
                        LINK <- append(LINK, respl$content$count_categories[[px]]$count_types[[1]]$total)
                      }
                    }

                    if (length(cit) != length(CAPT)) {
                      CAPT <- append(CAPT, 0)
                    }
                    if (length(cit) != length(CITBY)) {
                      CITBY <- append(CITBY, 0)
                    }
                    if (length(cit) != length(CITA)) {
                      CITA <- append(CITA, 0)
                    }
                    if (length(cit) != length(RECO)) {
                      RECO <- append(RECO, 0)
                    }
                    if (length(cit) != length(TWEE)) {
                      TWEE <- append(TWEE, 0)
                    }
                    if (length(cit) != length(REFE)) {
                      REFE <- append(REFE, 0)
                    }
                    if (length(cit) != length(ABSTR)) {
                      ABSTR <- append(ABSTR, 0)
                    }
                    if (length(cit) != length(FACE)) {
                      FACE <- append(FACE, 0)
                    }
                    if (length(cit) != length(NEWS)) {
                      NEWS <- append(NEWS, 0)
                    }
                    if (length(cit) != length(DOWN)) {
                      DOWN <- append(DOWN, 0)
                    }
                    if (length(cit) != length(LINK)) {
                      LINK <- append(LINK, 0)
                    }
                  } else {
                    CAPT <- append(CAPT, 0)
                    CITBY <- append(CITBY, 0)
                    CITA <- append(CITA, 0)
                    RECO <- append(RECO, 0)
                    TWEE <- append(TWEE, 0)
                    REFE <- append(REFE, 0)
                    ABSTR <- append(ABSTR, 0)
                    FACE <- append(FACE, 0)
                    NEWS <- append(NEWS, 0)
                    DOWN <- append(DOWN, 0)
                    LINK <- append(LINK, 0)
                  }
                } # PLUMX

                print("ALL IMPORTED")
                print("==")
              } else {
                print("Error, no content")
              }
            } else {
              print("Scopus ID dose not match")
              print(x$content$`full-text-retrieval-response`$`scopus-id`)
              print(res$entries[[a]]$`dc:identifier`)
            }
          }
        } else {
          print("Can not load fulltext, no file")
        }

        if (PLUMX == "YES") {
          if (!(length(abs) == length(sco) && length(abs) == length(cit) && length(abs) == length(etal) && length(abs) == length(CITBY))) {
            print("DATA LENGHT PROBLEM")
            length(abs)
            length(sco)

            halt()
          }
        }
      } # RESULT EXISTS
    } # ARTICLE
  } # YEAR
}


Count <- c(1:length(sco))
docN <- NULL
for (gh in Count) {
  docN <- append(docN, sprintf("%s:%s", gh, sco[gh]))
}

length(abs)

length(UniX)
length(YearX)
length(sco)
length(cre)
length(tit)
length(agr)
length(pub)
length(dol)
length(etal)
length(des)

length(NEWS)
length(DOWN)
length(LINK)
length(FACE)
length(ABSTR)
length(REFE)
length(CAPT)
length(CITBY)
length(CITA)
length(RECO)
length(TWEE)
length(REFE)




dv <- data.frame(
  University = UniX,
  Year = YearX,
  scopus = sco,
  creators = cre,
  title = tit,
  aggregationType = agr,
  publicationName = pub,
  publisher = dol,
  creator = etal,
  subject = des,
  capture = CAPT,
  cited_by = CITBY,
  citated = CITA,
  reader_count = RECO,
  tweet = TWEE,
  reference = REFE,
  abstract = ABSTR,
  facebook = FACE,
  link = LINK,
  downloade = DOWN,
  news = NEWS
)

CorpX <- corpus(abs, docnames = docN, docvars = dv)

save(CorpX, file = "CorpXV2")

tyhja <- 0
for (f in abs) {
  if (f == "") {
    tyhja <- tyhja + 1
  }
}
tyhja

length(afi[[1]][[1]])
