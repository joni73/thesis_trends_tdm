########

library("rscopus")

doING <- "NO" # IF SEARCH then full article not fetched
whitBase <- "YES" # Whit base query if NO ALL FROM INSTITUTION AT YEAR FETCHED
databa <- "AR" # AB or AR

inr <- inst_token_header("XXXxxxXXX")
options("elsevier_api_key" = "XXXxxxXXX")
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
  "FUND-ALL(dimecc oy)"
)

uni <- c("LUT", "ALL", "TAM", "OUL", "MIT", "KIT", "TSI", "JIO", "AZA", "LUT2", "FRA", "SIT", "BF", "DIM")
RESULTC <- c(1)
# RESULTC=c(13:14)
# RESULTC=c(10:10)
max <- 4000
for (c in RESULTC) {
  RESULTB <- c(3:5)
  # RESULTB=c(5)
  for (b in RESULTB) {
    if (whitBase == "YES") {
      query <- paste("(", u[c], ") AND ", "(", p[b], ") AND ", base_query)
    } else {
      query <- paste("(", u[c], ") AND ", "(", p[b], ")")
    }

    Sys.sleep(10)
    res <- scopus_search(query, verbose = TRUE, general = FALSE, max_count = max, headers = inr, count = 1)

    if (databa == "AR") {
      ser_file <- sprintf("%s/SEARCH_%s", uni[c], year[b])
    }

    if (databa == "AB") {
      ser_file <- sprintf("AB3/SEARCH_%s_%s", uni[c], year[b])
    }

    save(res, file = ser_file)

    if (doING != "SEARCH") {
      if (res$total_results > 0) {
        RESULT <- c(1:res$total_results)
        # RESULT=c("1")
        for (a in RESULT) {
          if (!is.null(res$entries[[a]]$`dc:identifier`)) {
            fetch_file <- strsplit(res$entries[[a]]$`dc:identifier`, ":")

            if (is.null(fetch_file[[1]][2])) {
              print("File Is NULL")
            } else {
              Sys.sleep(5)
              if (!is.null(api_key)) {
                x <- article_retrieval(fetch_file[[1]][2],
                  view = "FULL",
                  identifier = "scopus_id",
                  verbose = TRUE,
                  headers = inr
                )
              } else {
                print("API KEY NULL")
              }

              # if(is.null(x$content$`service-error`$status$statusCode)){
              # 	print("Service error NULL")
              print(x$content$`service-error`$status$statusCode)
              # 	}
              # else
              # {
              # if("RESOURCE_NOT_FOUND"==x$content$`service-error`$status$statusCode )
              # {
              # print("Scopus ID not available:")
              # print(fetch_file[[1]][2])
              # }
              # else

              if (!is.null(x$content$`full-text-retrieval-response`$"originalText")) {
                if (res$entries[[a]]$`dc:identifier` != x$content$`full-text-retrieval-response`$`scopus-id`) {
                  print(x$content$`full-text-retrieval-response`$`scopus-id`)
                  print(res$entries[[a]]$`dc:identifier`)
                }
              }
              Sys.sleep(5)
              if (!is.null(x$content$`full-text-retrieval-response`$"originalText")) {
                # fetch_file =  strsplit(res$entries[[a]]$`dc:identifier`,":")
                art_file <- sprintf("%s/AR_%s_%s", uni[c], year[b], x$content$`full-text-retrieval-response`$`scopus-id`)
                save(x, file = art_file)
              } # file exists
              else {
                print("NO TEXT")
              }



              # } api ok
            }
          } # RESULT EXISTS
        } # Fetch result id ok
      } # ARTICLE
    }
  } # YEAR
} # NOT SEARCH
