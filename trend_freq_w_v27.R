
library("psych")
library("quanteda")
library(quanteda.textstats)
library(ggplot2)
library(scales)
library(quanteda.textplots)
library(Kendall)

# ============
ver <- "27"
word_out <- "OFF"
Pdf_printing <- "ON"
Word_printing <- "OFF"
# ============
pau <- 3
lookup <- TRUE # MATCH == FALSE

if (Word_printing == "ON") {
  library(R2wd)
  require(R2wd)
  library(RDCOMClient)
  require(RDCOMClient)
}

ajo <- "1"
daf <- "AR"



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
RM <- 9 # institutions removed from total statistic due include duplicates
year <- c("21", "20", "19", "18", "17")
yearN <- c(21, 20, 19, 18, 17)

dics <- c(
  "WORDSTAT/Brand Personality.cat", # 1
  "WORDSTAT/Corporate Social Responsibility.cat", # 2
  "WORDSTAT/FOREST.cat", # 3
  "MEGATRENDS/MEGAS1.cat", # 4
  "MEGATRENDS/SITRA2016.cat", # 5
  "WORDSTAT/ROGET.cat", # 6
  "TUTKIMUS/THEORY3.cat", # 7
  "WORDSTAT/RID.cat", # 8
  "TUTKIMUS/deslist3.cat", # 9
  "TUTKIMUS/etallist3.cat", # 10
  "TUTKIMUS/des3.cat", # 11
  "TUTKIMUS/query.cat", # 12
  "MEGATRENDS/TeknologiaTeollisuus3.cat" # 13
)

if (daf == "AB") {
  # load("//192.168.0.42/mpi/DITYO2/CorpXABV2")

  load("CorpXABV2")
  # CorpX=CorpXAB
}

if (daf == "AR") {
  # load("//192.168.0.42/mpi/DITYO2/CorpXV2")

  load("CorpXV2")
}

t2 <- tokens(CorpX)
l2 <- dfm(t2)

RESULTf <- c(9, 8, 6, 3, 2, 1)
# RESULTf=c(1:length(dics))
for (f in RESULTf) {
  if (Word_printing == "ON") {
    WordFile <- sprintf("C:/Users/Joni-/LUT/DITYO2/RAAKAANAL/Trend19v4dic%s.doc", daf)
  }

  if (Pdf_printing == "ON") {
    PdfFile <- sprintf("RAAKAANAL/Trend%sdic%s_%s.pdf", ver, f, daf)
  }

  TableFile <- sprintf("RAAKAANAL/analysis%sdic%s_%s.csv", ver, f, daf)


  if (Word_printing == "ON") {
    WordFile <- sprintf("RAAKAANAL/TrendTopic20v0dic%s.doc", f)
  }

  if (Pdf_printing == "ON") {
    PdfFile <- sprintf("RAAKAANAL/Trendv%svdic%s_%s.pdf", ver, daf, f)
  }

  TableFile <- sprintf("RAAKAANAL/analysis_trend_%s_dic%s_%s.csv", ver, daf, f)

  DIC <- NULL
  Data <- NULL
  DIC <- dictionary(file = dics[f])

  l <- length(dics)
  kas <- length(DIC[])
  di <- length(dics)
  un <- length(uni)
  ye <- length(year)
  Data <- array(0, dim = c(kas, 4, un, ye))
  row.names(Data) <- names(DIC)

  print("dictionary on road:")
  print(f)

  RESULTc <- c(1:un)
  for (c in RESULTc) {
    print("un")
    print(c)
    RESULTa <- c(1:ye)
    # RESULTa=c("1")
    for (a in RESULTa) {
      print("ye:")
      print(a)

      tstat_freq <- NA
      Cor <- NA
      DOCS <- 0
      word <- NA
      docs <- 0
      feat <- 0

      Cor <- corpus_subset(CorpX, Year == year[a] & University == uni[c])
      DOCS <- length(Cor)
      if (DOCS != 0) {
        tl2 <- tokens(Cor)
        Word <- dfm(tl2)

        if (lookup == TRUE) {
          word <- dfm_lookup(Word, DIC, valuetype = "glob", verbose = FALSE, nomatch = "NONE")
        } else {
          word <- dfm_match(Word, DIC)
        }

        # word=dfm_lookup(Word,DIC[b], valuetype = "glob", verbose = FALSE, nomatch = "NONE")
        # word=dfm(Cor,dictionary=DIC)
        docs <- length(word@Dimnames$docs)
        feat <- length(word@Dimnames$features)

        print("DOCS:")
        print(DOCS)
        print("feat:")
        print(feat)

        if (any(!is.na(word) == TRUE)) {
          tstat_freq == NULL
          tstat_freq <- textstat_frequency(word)
          if (!is.null(tstat_freq) == TRUE) {
            Resultd <- c(1:length(tstat_freq[[1]]))

            for (d in Resultd) {
              if (!is.na(tstat_freq[d]) == TRUE) {
                if (any(row.names(Data) == tstat_freq[d]$"feature")) {
                  s <- tstat_freq[d]$"feature"


                  Data[s, 1, c, a] <- (tstat_freq[d]$"frequency")
                  Data[s, 2, c, a] <- (tstat_freq[d]$"docfreq")
                  Data[s, 3, c, a] <- DOCS
                  Data[s, 4, c, a] <- docs
                } # if
                else {
                  for (s in 1:length(row.names(Data))) {
                    Data[s, 1, c, a] <- 0
                    Data[s, 2, c, a] <- 0
                    Data[s, 3, c, a] <- DOCS
                    Data[s, 4, c, a] <- docs
                  }
                } # for
              }
            } # docs = 0
          } # tstat
        } # is word
      } # DOCS0
    } # year a
  } # uni c


  ### TREND


  funny1 <- function() {
    geom.text.size <- 8
    theme.size <- (14 / 5) * geom.text.size
    cprint <- ggplot(PlotData, aes(x = Year, y = Presentage, shape = University, color = University), height = 2, width = 2)
    print(cprint + geom_point(size = 1) +
      scale_y_continuous(
        trans = "log10",
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
      ylab("Osuus/Yliopisto") +
      xlab("Vuosi") +
      ggtitle(names(DIC[b])) +
      theme(text = element_text(size = 8)) +
      scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 15, 16, 17, 18)) +
      scale_size_manual(values = c(4, 4, 4, 4, 4, 4, 4, 1, 1, 4, 1)))
  }


  funny2 <- function() {
    geom.text.size <- 8
    theme.size <- (14 / 5) * geom.text.size
    cprint <- ggplot(PlotData, aes(x = University, y = Presentage, color = University), height = 2, width = 2)
    print(cprint + geom_point() +
      theme(text = element_text(size = 8)) +
      ggtitle(names(DIC[b])) +
      geom_boxplot() +
      ylab("Osuttuja tekstejä") +
      xlab("Yliopisto"))
  }

  funny3 <- function() {
    geom.text.size <- 8
    theme.size <- (14 / 5) * geom.text.size
    cprint <- ggplot(PlotData, aes(x = Year, y = ScoreTxt, shape = University, color = University), height = 2, width = 2)
    print(cprint + geom_point(size = 1) +
      scale_y_continuous(
        trans = "log10",
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
      ylab("Osumia/Yliopisto") +
      xlab("Vuosi") +
      ggtitle(names(DIC[b])) +
      theme(text = element_text(size = 8)) +
      scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 15, 16, 17, 18)) +
      scale_size_manual(values = c(4, 4, 4, 4, 4, 4, 4, 1, 1, 4, 1)))
  }


  funny4 <- function() {
    geom.text.size <- 8
    theme.size <- (14 / 5) * geom.text.size
    cprint <- ggplot(PlotData, aes(x = University, y = ScoreTxt, color = University), height = 2, width = 2)
    print(cprint + geom_point() +
      theme(text = element_text(size = 8)) +
      ggtitle(names(DIC[b])) +
      ylab("Osumia") +
      xlab("Yliopisto"))
  }

  funny5 <- function() {
    pairs.panels(PlotData, pch = 21)
  }

  funny6 <- function() {
    textplot_xray(kwic(CorpX[wordCL], DIC[b]))
  }


  ######## print result start
  if (Word_printing == "ON") {
    wdGet(filename = NULL, path = "", method = "RDCOMClient", visible = TRUE)

    wdNewDoc(WordFile) # this creates a new file with "this.doc" name
    wdApplyTemplate("C:Users/Joni-/Documents/LUT_Tietokantapohja1.dotx") # this applies a template

    # wdTitle("TRENDS - FULLTEXT")    # adds a title to the file

    wdBody("\n\nDI-Työn raaka dataa - EI JULKINEN – Joni-Pekka Kurronen\n\n Tekijän oikeudet pidätetään osittain 2021. Tätä työtä saa käyttää ei kaupallisesti, jakaa ja tuottaa uudelleen missä tahansa muodossa, mutta alkuperäisiin tekijöihin ja kustantajiin tulee viittausten näkyä ja säilyä. (Elsevier, 2017)
\n\nSome rights reserved 2021. This work permits non-commercial use, distribution, and re-production in any medium, provided the original author and source are credited.
")

    wdBody("\n\nKaikki tekstit:")
    wdBody(length(CorpX))

    wdBody("\nSanakirjat:")

    wdBody(dics[f])
  } # word print end

  if (Pdf_printing == "ON") {
    pdf(PdfFile, paper = "a4")
    plot.new()
    text(0, .90, f, pos = 4)
    txt <- PdfFile
    text(0, .86, txt, pos = 4)

    txt <- "DI-Työn raaka dataa - EI JULKINEN – Joni-Pekka Kurronen Tekijän\r
oikeudet pidätetään\n osittain 2022. Tätä työtä saa käyttää ei\r
kaupallisesti, jakaa ja tuottaa uudelleen missä tahansa\r
muodossa, mutta alkuperäisiin tekijöihin ja kustantajiin tulee\r
viittausten näkyä ja säilyä.Elsevier, 2017 Some rights reserved 2022.\r
This work permits non-commercial use, distribution, and re\r
production in any medium, provided the original\r
author and source are credited."

    text(0, .40, txt, pos = 4)

    txt <- "Kaikki tekstit:"
    text(0, .10, txt, pos = 4)
    txt <- length(CorpX)
    text(0, 0.04, txt, pos = 4)
    txt <- "Aiheet:"
    text(0, 0.01, txt, pos = 4)

    plot.new()
  } # pdf end

  RESULTb <- c(1:kas) # l
  for (b in RESULTb) {
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



    Wor <- NULL
    Dic <- NULL
    Ver <- NULL
    YeaYT <- NULL
    PreYT <- NULL
    ScoYT <- NULL
    ScoTxtYT <- NULL
    Uni <- NULL
    Yea <- NULL

    Pre <- NULL
    Sco <- NULL
    ScoTxt <- NULL

    SiValt <- 0
    SiVal2t <- 0
    SiVal3t <- 0


    for (an in c(1:ye)) {
      YeaYT[an] <- 0
      PreYT[an] <- 0
      ScoYT[an] <- 0
      ScoTxtYT[an] <- 0
    }



    RESULTc <- c(1:un)
    # RESULTc=c(1:1)
    for (c in RESULTc) {
      YeaY <- NULL
      PreY <- NULL
      ScoY <- NULL
      ScoTxtY <- NULL

      SiVal <- 0
      SiVal2 <- 0

      SiVal3 <- 0
      SiVal4 <- 0




      RESULTa <- c(1:ye)
      # RESULTa=c("1")
      for (a in RESULTa) {
        Wor <- append(Wor, names(DIC[b]))
        Dic <- append(Dic, f)
        Ver <- append(Ver, ver)

        Uni <- append(Uni, uni[c])
        Yea <- append(Yea, yearN[a])
        YeaY <- append(YeaY, yearN[a])
        YeaYT <- append(YeaYT, yearN[a])

        # frequency
        Val <- Data[b, 1, c, a]
        if (is.na(Val) == TRUE) Val <- 0
        if (Val == "NaN") Val <- 0
        Sco <- append(Sco, Val)
        ScoY <- append(ScoY, Val)
        if (c <= RM) {
          ScoYT[a] <- ScoYT[a] + Val
        }
        SiVal <- SiVal + Val
        SiValt <- SiValt + Val

        # doc freq / all docs
        if (0 == Data[b, 3, c, a]) {
          Val2 <- 0
        } else {
          Val2 <- Data[b, 2, c, a] / Data[b, 3, c, a]
        }
        if (is.na(Val2) == TRUE) Val2 <- 0
        if (Val2 == "NaN") Val2 <- 0
        Pre <- append(Pre, Val2)
        PreY <- append(PreY, Val2)
        if (c <= RM) {
          PreYT[a] <- PreYT[a] + Val2
        }
        SiVal2 <- SiVal2 + Val2


        # doc freq
        Val2t <- Data[b, 2, c, a]
        if (is.na(Val2t) == TRUE) Val2t <- 0
        if (Val2t == "NaN") Val2t <- 0
        ScoTxt <- append(ScoTxt, Val2t)
        ScoTxtY <- append(ScoTxtY, Val2t)
        if (c <= RM) {
          ScoTxtYT[a] <- ScoTxtYT[a] + Val2t
        }
        SiVal2t <- SiVal2t + Val2t

        # ALL DOCS
        Val3 <- Data[b, 3, c, a]
        if (is.na(Val3) == TRUE) Val3 <- 0
        if (Val3 == "NaN") Val3 <- 0
        SiVal3 <- SiVal3 + Val3
        # if(Val3!=0) SiVal3=Val3
        SiVal3t <- SiVal3t + Val3

        # docs whit hit
        Val4 <- Data[b, 4, c, a]
        if (is.na(Val4) == TRUE) Val4 <- 0
        if (Val4 == "NaN") Val4 <- 0
        SiVal4 <- SiVal4 + Val4
        # if(Val4!=0) SiVal4=Val4
      } # a year


      scounV <- MannKendall(ScoTxtY)
      preV <- MannKendall(PreY)
      scoV <- MannKendall(ScoY)

      # scounV=cor.test(YeaY,ScoTxtY)
      # preV=cor.test(YeaY,PreY)
      # scoV=cor.test(YeaY,ScoY)

      for (a1 in RESULTa)
      {
        preunp <- append(preunp, preV$sl[1])
        preune <- append(preune, preV$tau[1])
        preunv <- append(preunv, var(PreY))
        preunm <- append(preunm, mean(PreY))

        scounp <- append(scounp, scounV$sl[1])
        scoune <- append(scoune, scounV$tau[1])
        scounv <- append(scounv, var(ScoY))
        scounm <- append(scounm, mean(ScoY))

        txtunp <- append(txtunp, scoV$sl[1])
        txtune <- append(txtune, scoV$tau[1])
        txtunv <- append(txtunv, var(ScoY))
        txtunm <- append(txtunm, mean(ScoY))
      }
    } # c uni


    #############
    # write.table

    ########

    scounV <- MannKendall(ScoTxtYT)
    preV <- MannKendall(PreYT)
    scoV <- MannKendall(ScoYT)

    for (a1 in RESULTa)
    {
      for (c1 in RESULTc)
      {
        preunpT <- append(preunpT, preV$sl[1])
        preuneT <- append(preuneT, preV$tau[1])
        preunvT <- append(preunvT, var(PreYT))
        preunmT <- append(preunmT, mean(PreYT))

        scounpT <- append(scounpT, scounV$sl[1])
        scouneT <- append(scouneT, scounV$tau[1])
        scounvT <- append(scounvT, var(ScoYT))
        scounmT <- append(scounmT, mean(ScoYT))

        txtunpT <- append(txtunpT, scoV$sl[1])
        txtuneT <- append(txtuneT, scoV$tau[1])
        txtunvT <- append(txtunvT, var(ScoTxtYT))
        txtunmT <- append(txtunmT, mean(ScoTxtYT))
      }
    } # uni


    TableData <- NULL
    TableData <- data.frame(
      University = Uni, Year = Yea, Version = Ver, Dictionary = Dic, Wo = Wor, ScoreTxt = ScoTxt, Presentage = Pre, Score = Sco,
      PreunpT = preunpT, PreuneT = preuneT, PreunvT = preunvT, Preunm = preunmT, Scounp = scounpT, Scounm = scounmT, Scounv = scounvT, Scoune = scouneT,
      Txtunp = txtunpT, Txtune = txtuneT, Txtunv = txtunvT, Txtunm = txtunmT, Preunp = preunp, Preune = preune, Preunv = preunv, Preunm = preunm,
      Scounp = scounp, Scounm = scounm, Scounv = scounv, Scoune = scoune, Txtunp = txtunp, Txtune = txtune, Txtunv = txtunv, Txtunm = txtunm
    )

    if (!is.null(TableData)) {
      write.table(TableData, file = TableFile, append = TRUE, sep = ",", eol = "\n", col.name = TRUE)
    }


    if (Word_printing == "ON") {
      if (SiValt == 0) {
        wdBody(names(DIC[b]))
        wdBody("NO SCORE")
      } else {
        wdSection(names(DIC[b]), newpage = T)


        ou <- NULL
        out <- NULL

        Sys.sleep(pau)

        dlen <- length(DIC[[b]])
        if (dlen > 10) {
          dlen <- 10
          ou <- sprintf("10 / %s:", dlen)
          out <- paste0(out, ou)
        }

        RESULTg <- c(1:dlen)
        # RESULTg=c("1")
        for (g in RESULTg) {
          ou <- sprintf("%s,", DIC[[b]][g])
          out <- paste0(out, ou)
        }

        wdBody(out)

        Sys.sleep(pau)

        out <- sprintf("\nTekstejä:%s\nTekstejä joissa osumia:%.f\nOsumia:%.f\n\n", SiVal3t, SiVal2t, SiValt)
        wdBody(out)

        PlotData <- NULL
        PlotData <- data.frame(University = Uni, Year = Yea, Presentage = Pre, Score = Sco, ScoreTxt = ScoTxt)

        wdPlot(plotfun = funny5, method = "metafile", height = 3.5, width = 3.5, pointsize = 1)

        Sys.sleep(pau)


        wdPlot(
          plotfun = funny1, method = "metafile", height = 3.5, width = 3.5,
          pointsize = 1
        )

        Sys.sleep(pau)

        wdPlot(
          plotfun = funny2, method = "metafile", height = 3.5, width = 3.5,
          pointsize = 1
        )

        Sys.sleep(pau)

        wdPlot(
          plotfun = funny3, method = "metafile", height = 3.5, width = 3.5,
          pointsize = 1
        )

        Sys.sleep(pau)

        wdPlot(
          plotfun = funny4, method = "metafile", height = 3.5, width = 3.5,
          pointsize = 1
        )

        Sys.sleep(pau)

        tl <- dfm_lookup(l2, DIC[b], valuetype = "glob", verbose = FALSE, nomatch = "NONE")
        tlf <- convert(tl, to = "data.frame")
        tlfs <- tlf[order(tlf[, 2]), ]
        tlfsr <- tlfs[dim(tlfs)[1]:1, ]

        out <- NULL
        RESULTd <- c(1:20)
        # RESULTd=c(1:1)
        for (d in RESULTd) {
          ou <- NULL

          if (nrow(tlfsr) >= d) {
            txt2 <- CorpX[[tlfsr[d, 1]]]
            if (is.null(txt2) == FALSE) {
              print(CorpX[tlfsr[d, 1]]$"title")

              tit <- CorpX[tlfsr[d, 1]]$"title"
              txt1 <- tokens(txt2)
              to <- kwic(txt1, pattern = DIC[b], separator = "|", valuetype = "glob", window = 6)
              ou <- sprintf("\n%s: %s || %s || %s   %s", tlfsr[d, 1], to[1]$pre, to[1]$keyword, to[1]$post, tit)
              out <- paste0(out, ou)
            }
          }
        } # for

        wdBody(out)
        Sys.sleep(pau)
      } # siVal
    } # WORD OFF

    if (Pdf_printing == "ON") {
      if (SiValt == 0) {

      } else {
        plot.new()
        txt <- names(DIC[b])
        text(0, 1, txt, pos = 4)

        ou <- NULL
        out <- ""


        dlen <- length(DIC[[b]])
        if (dlen > 10) {
          dlen <- 10
          ou <- sprintf("10 / %s:", dlen)
          out <- paste0(out, ou)
        }

        text(0, 0.9, out, pos = 4)

        out <- ""

        RESULTg <- c(1:dlen)
        # RESULTg=c("1")
        for (g in RESULTg) {
          ou <- sprintf("%s,", DIC[[b]][g])
          out <- paste0(out, ou)
        }

        text(0, 0.8, out, pos = 4)

        ou <- NULL
        out <- ""

        out <- sprintf("\nTekstejä:%s\nTekstejä joissa osumia:%.f\nOsumia:%.f\n\n", SiVal3t, SiVal2t, SiValt)

        text(0, .80, out, pos = 4)

        PlotData <- NULL
        PlotData <- data.frame(University = Uni, Year = Yea, Presentage = Pre, Score = Sco, ScoreTxt = ScoTxt)


        #######################

        pairs.panels(PlotData, pch = 21)

        geom.text.size <- 8
        theme.size <- (14 / 5) * geom.text.size
        cprint <- ggplot(PlotData, aes(x = Year, y = Presentage, shape = University, color = University), height = 2, width = 2)
        print(cprint + geom_point(size = 1) +
          scale_y_continuous(
            trans = "log10",
            breaks = trans_breaks("log10", function(x) 10^x),
            labels = trans_format("log10", math_format(10^.x))
          ) +
          ylab("Osuus/Yliopisto") +
          xlab("Vuosi") +
          ggtitle(names(DIC[b])) +
          theme(text = element_text(size = 8)) +
          scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 15, 16, 17, 18, 19, 20, 14)) +
          scale_size_manual(values = c(4, 4, 4, 4, 4, 4, 4, 1, 1, 4, 1, 4, 4, 4)))



        geom.text.size <- 8
        theme.size <- (14 / 5) * geom.text.size
        cprint <- ggplot(PlotData, aes(x = University, y = Presentage, color = University), height = 2, width = 2)
        print(cprint + geom_point() +
          theme(text = element_text(size = 8)) +
          ggtitle(names(DIC[b])) +
          geom_boxplot() +
          ylab("Osuttuja tekstejä") +
          xlab("Yliopisto"))



        geom.text.size <- 8
        theme.size <- (14 / 5) * geom.text.size
        cprint <- ggplot(PlotData, aes(x = Year, y = ScoreTxt, shape = University, color = University), height = 2, width = 2)
        print(cprint + geom_point(size = 1) +
          scale_y_continuous(
            trans = "log10",
            breaks = trans_breaks("log10", function(x) 10^x),
            labels = trans_format("log10", math_format(10^.x))
          ) +
          ylab("Osumia/Yliopisto") +
          xlab("Vuosi") +
          ggtitle(names(DIC[b])) +
          theme(text = element_text(size = 8)) +
          scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 15, 16, 17, 18, 19, 20, 14)) +
          scale_size_manual(values = c(4, 4, 4, 4, 4, 4, 4, 1, 1, 4, 1, 4, 4, 4)))

        geom.text.size <- 8
        theme.size <- (14 / 5) * geom.text.size
        cprint <- ggplot(PlotData, aes(x = University, y = ScoreTxt, color = University), height = 2, width = 2)
        print(cprint + geom_point() +
          theme(text = element_text(size = 8)) +
          ggtitle(names(DIC[b])) +
          ylab("Osumia") +
          xlab("Yliopisto"))

        ###########################



        ###########################

        tl <- dfm_lookup(l2, DIC, valuetype = "glob", verbose = FALSE, nomatch = "NONE")
        tlf <- convert(tl, to = "data.frame")
        tlfs <- tlf[order(tlf[, 2]), ]
        tlfsr <- tlfs[dim(tlfs)[1]:1, ]

        E <- length(DIC[[b]][])

        out <- NULL
        DICRAY <- NULL

        if (E > 0) {
          if (E == 1) DICRAY <- append(DICRAY, DIC[[b]][1])

          if (E > 5) E <- 5

          if (E > 1) {
            RESULTd <- c(1:E)
            # RESULTd=c(1:1)

            for (d in RESULTd) {
              DICRAY <- append(DICRAY, DIC[[b]][d])
            } # for
          }

          txt2 <- tokens(Cor)
          resM <- kwic(txt2, pattern = DICRAY)

          if (length(resM$docname) > 0 && E != 0) {
            print(textplot_xray(kwic(txt2, pattern = DICRAY)))
          }
        }
        #######################

        plot.new()

        tl <- dfm_lookup(l2, DIC[b], valuetype = "glob", verbose = FALSE, nomatch = "NONE")
        tlf <- convert(tl, to = "data.frame")
        tlfs <- tlf[order(tlf[, 2]), ]
        tlfsr <- tlfs[dim(tlfs)[1]:1, ]

        out <- NULL

        l1 <- length(tl)
        if (l1 < 20) {
          le <- l1
        } else {
          le <- 20
        }

        RESULTd <- c(1:le)
        # RESULTd=c(1:1)
        for (d in RESULTd) {
          ou <- NULL

          if (nrow(tlfsr) >= d) {
            txt <- CorpX[[tlfsr[d, 1]]]
            if (is.null(txt) == FALSE) {
              ou <- sprintf("%s", CorpX[tlfsr[d, 1]]$"title")
              out <- paste0(out, ou)
              tit <- CorpX[tlfsr[d, 1]]$"title"
              txt1 <- tokens(txt)
              to <- kwic(txt1, pattern = DIC[b], separator = "|", valuetype = "glob", window = 6)
              ou <- sprintf("\n%s: %s || %s || %s   %s", tlfsr[d, 1], to[1]$pre, to[1]$keyword, to[1]$post, tit)
              out <- paste0(out, ou)
            }
          }
        } # for

        text(0, 0.95, out, pos = 4)

        #######################
      } # siVal
    } # pdf printing off
  }

  if (Word_printing == "ON") {
    wdSave(WordFile)

    Sys.sleep(pau)
    Sys.sleep(pau)
    Sys.sleep(pau)

    wdQuit()
    Sys.sleep(pau)
    Sys.sleep(pau)
  } # word end

  if (Pdf_printing == "ON") {
    dev.off()
  }
} # f
