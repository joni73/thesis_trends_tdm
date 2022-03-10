WORKING DOCUMENT

Future trends at academic aesrach
a view from product engineering

Master Thesis work, LUT University, Author: Joni-Pekka Kurronen
 
R-code realated at: <p>
	https://github.com/joni73/thesis_trends_tdm <p>
R-code + *.csv and *.pdf raw analysis files: <p>
        https://drive.google.com/drive/folders/1dIBji7311INISWZPtVi1Gp2tz-p4EJf2?usp=sharing <p>
        
<p>
<p>

 -raw analysis file's at /RAAKAANAL at google drive <p>
 -some Dictionaryes at /TUTKIMUS and MEGATRENDS
 -R code to fetch data from scopus <p>
 	hakuar_V4.R for full text <p>
 	hakuMETA_AB_v5.r for abstarct whit metadata <p>
 -R code to generate AR and AB corpus from search results <p>
        CorpABGenV2.R generates abstract database <p>
        hakuMETA_v3.r generates fulltext database whit meta data <p>
        hakuMETA_AB_v5.r generates abstract database whit meta data <p>
 -R code to produce raw analysis<p>
        trend28.R <p>
        trend_freq_w_v27.R <p>
 -R code used to form research results<p>
        CSV_counter_FINAL.R <P>
        
 Code and data is published as is, just for information.
 It's 100% spagethi code to just get work done.
 You can find some glues how to get your work done. Even code
 is fully working it's not done any form production use.
 
 all R code is licenced by term's - FREE CODE TO BE USED AS YOU WANNA USE
 
 Raw analysis files are based Elsevier Scopus database and
 published under below term's and licence:
 
 
APPENDIX A - Elsevier Provisions for Text and Data Mining (TDM)
Access to subscription content for text mining is provided to subscribers for noncommercial research purposes. Please note that for open access content, TDM permissions and reuse are determined by the author's choice of user license. Upon acceptance of these provisions for TDM you will be provided with the API documentation and API key to allow you to do the following:
Secure a unique API key for your own personal use
To retrieve, via your API key, content your institution has subscribed to in order to use it as a corpus for non-commercial text mining.
You can automatically extract semantic entities from the corpus to create TDM output, and prepare research papers or other scholarly publications using the TDM output.
You are able to distribute the findings of your text mining, in line with the following conditions:
Where snippets (which may include a few lines of query-dependent text of individual full text articles or book chapters up to a maximum length of 200 characters surrounding and excluding the text entity matched) and/or bibliographic metadata are distributed, they should be accompanied by a DOI link that points back to the individual full text article or book chapter;
You should also include a proprietary notice in the following form: "Some rights reserved. This work permits non-commercial use, distribution, and reproduction in any medium, provided the original author and source are credited."
You are permitted to distribute or create a link to the list of DOIs used to perform TDM;
Where images are used you should clear the rights for reuse with the relevant rightsholder;

You are not allowed to:
Use snippets of text from individual full text articles or book chapters of more than 200 characters (excluding text entity matches or bibliographic metadata);
Abridge, modify, translate or create any derivative work based on the corpus;
Delete information about authorship or copyright notices from the corpus;
Substantially or systematically reproduce, retain or redistribute the corpus;
Extract, develop or use the corpus in any direct or indirect commercial activity;
Use any robots, spiders or other automated downloading programs, algorithms or devices to search, screen-scrape, extract, or index any Elsevier web site or web application, instead of using the APIs; and
Utilize the TDM output to enhance institutional or subject repositories in a way that would compete with the value of the final peer review journal article, or have the potential to substitute and/or replicate any other existing Elsevier products, services and/or solutions.


You are responsible to keep your contact information as registered on https://dev.elsevier.com up to date and you may not modify or attempt to circumvent the key for secure access to the APIs. We shall have the right to deactivate the API Keys provided to you, if (a) you have not started using the APIs within six months following the delivery of the API Keys, or (b) you have not been accessing the APIs for at least a year since its last access, or (c) the term of the ScienceDirect database subscription to the book and journal content the organization you are affiliated with expires or if (d) you sell, transfer, sublicense, or otherwise disclose the API key to any other party or you use them for purposes not described herein.
Upon termination or expiration of the subscribed content, Elsevier shall disable access to the APIs for your API Key. You must permanently delete all Elsevier content or Elsevier data which you stored pursuant to your use of the APIs except for the TDM output and the Snippets. Notwithstanding the foregoing, you are permitted to retain a private copy of the corpus, or excerpts thereof, for reasons of data archiving requirements and to make this corpus available for internal institutional uses or for peer review, funding or ethics purposes (but not for further external distribution by these agencies or reviewers). You can also maintain the list of DOIs as a data object and provide this externally. Instead of obtaining an API Key for text mining directly from Elsevier through registration on https://dev.elsevier.com, you can also use the CrossRef TDM service (http://tdmsupport.crossref.org/) to obtain a cross-publisher API token for use with our APIs. Obtaining this token is subject to the same conditions as outlined in these provisions.
Last updated: 24-Feb-2017

