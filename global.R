library(shiny)
library(rCharts)
library(reshape2)
library(stringr)
library(ggvis)
library(rAltmetric)
library(XLConnect)
library(dplyr)
library(DT)

####################################
#
#  Import metrics file
#
####################################

# Few lines with double quotes inside fields terminated by quotes
# sed -i 's/""/%%/g' aalto_since2007.csv 
# sed -i 's/\\"/%%/g' altm2014/metrics.csv
# In one DOI this causes errors because
# 10.1145/1571941.1572030 Journal_short empty, and no ISSN


aalto_all_m <- read.table(file = "metrics.csv", 
                          header = TRUE, 
                          sep = ",",
                          col.names = c("DOI", "Title", "Authors", "Journal_short", "Journal_long", "School", "URL", 
                                        "Altmetric.com_URL", "Year",
                                        "WoS", "ISSN", "Altmetric", "Mendeley", "CiteULike",
                                        "Readers_count", "GooglePlus", "Facebook", "Any_type_of_posts", "Twitter", 
                                        "Accounts", "Blog_posts", "Videos", "Delicious_bookmarks", 
                                        "Reddit", "Forums",
                                        "StackExchange", "rh",                                     
                                        "Science_news_outlets",
                                        "Wikipedia"),
                       #   colClasses = c("character", "character", "character", "character", "character", "character", "character", 
                      #                   "character", "character", 
                      #                   "integer", "character", "numeric", "character", "character", 
                      #                   "integer", "integer", "integer", "integer", "integer", 
                      #                   "integer", "integer", "integer", "integer", 
                      #                   "integer", "integer", 
                      #                   "integer", "integer", 
                      #                   "integer"),
                          stringsAsFactors = FALSE)


# Readers_count = Mendeley + CiteULike; rh = ?
aalto_all_m <- aalto_all_m[ ,c("DOI", "Title", "Authors", "Journal_short", "Journal_long", "School",  "URL", 
                               "Altmetric.com_URL", "Year", 
                               "WoS", "ISSN", "Altmetric", "Mendeley", "CiteULike",
                               "GooglePlus", "Facebook", "Twitter", 
                               "Accounts", "Blog_posts", "Videos", "Delicious_bookmarks", 
                               "Reddit", "Forums",
                               "StackExchange", "rh",                                     
                               "Science_news_outlets", "Wikipedia")]

names(aalto_all_m) <- c("DOI", "Title", "Authors", "Journal_short", "Journal",  "School", "URL", 
                        "AltmetricURL", "Year", 
                        "WoS", "ISSN", "Altmetric", "Mendeley", "CiteULike",
                        "GooglePlus", "Facebook", "Twitter", 
                        "Accounts", "Blog posts", "Videos", "Delicious", 
                        "Reddit", "Forums","StackExchange", "rh", "NewsOutlets", "Wikipedia")

aalto_all_m$Journal_short[which(aalto_all_m$DOI=="10.1145/1571941.1572030")] <- ""
aalto_all_m$ISSN[which(aalto_all_m$DOI=="10.1145/1571941.1572030")] <- ""

# Altmetric.com_score value to ceiling 
aalto_all_m$Altmetric <- as.integer(ceiling(aalto_all_m$Altmetric))
# For unknown reason, some columns need to be transformed twice to integer 
# columns <- c(which(colnames(aalto_all_m) == "Mendeley"):ncol(aalto_all_m))
# aalto_all_m[ ,columns] <- apply(aalto_all_m[ ,columns], 2, function(x) as.integer(as.character(x)))

# In original data, we have duplicate DOIs if the article is co-authored by
# multiple Schools. Here, we deliberately delete that information, and
# the DOI is randomnly put under one of the Schools
aalto_all_m <- aalto_all_m[!duplicated(aalto_all_m$DOI),]

# Some Altmetric.com data is NA, some 0
aalto_all_m[is.na(aalto_all_m)] <- 0

# Still need these?
#
# columns <- c(which(colnames(aalto_all_m) == "GooglePlus"):ncol(aalto_all_m))
# aalto_all_m[ ,columns] <- apply(aalto_all_m[ ,columns], 2, function(x) as.integer(as.character(x)))

# See remark on top
aalto_all_m$Title <- with(aalto_all_m, gsub("%%", "'", Title))

# See ?toupper
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

aalto_all_m$Journal <- sapply(aalto_all_m$Journal, tolower)
aalto_all_m$Journal <- sapply(aalto_all_m$Journal, function(x) simpleCap(x))


########################################################
#
# Open Access journals
# http://www.lib4ri.ch/news.html++/year/2014/item/89/
#
# In the Shiny web app, data is already downloaded
########################################################

# url <- "http://www.lib4ri.ch/files/oa_journals_scie.xls"
# file <- "oa.xls"
# download.file(url, file, mode="wb")
wb <- loadWorkbook("oa.xls")
oa <- readWorksheet(wb, sheet = "All_OA_Journals", header = TRUE)

# Might use the subject field information later on, so concatenate them here
oa$field <- do.call(paste, c(oa[,8:ncol(oa)], sep=","))
oa$field <- gsub("NA,", "", oa$field)
oa$field <- gsub("NA", "", oa$field)
oa$field <- gsub(",$", "", oa$field)

oa <- oa[, c("Journal", "ISSN", "E.ISSN", "field")]
names(oa) <- c("Journal", "ISSN", "E-ISSN", "Fields")

# Plos Computational Biology, Plos Medicine and Plos Biology print versions 
# have wrong ISSN's so let's change them
oa$ISSN[which(oa$ISSN=="1553-7358")] <- "1553-734X"
oa$ISSN[which(oa$ISSN=="1549-1676")] <- "1549-1277"
oa$ISSN[which(oa$ISSN=="1545-7885")] <- "1544-9173"

aalto_oa <- merge(aalto_all_m, oa, by.x = "ISSN", by.y = "ISSN", all.x = TRUE)
#aalto_oa <- aalto_oa[-c(28)]
aalto_oa <- aalto_oa[-c(29)]
aalto_oa2 <- merge(aalto_all_m, oa, by.x = "ISSN", by.y = "E-ISSN", all.x = TRUE)
aalto_oa2 <- aalto_oa2[-c(29)]

n <- c("ISSN", "DOI", "Title", "Authors", "Journal_short", "Journal", "School", "URL", 
       "AltmetricURL", "Year", 
       "WoS", "Altmetric", "Mendeley", "CiteULike",
       "GooglePlus", "Facebook", "Twitter", 
       "Accounts", "Blog posts", "Videos", "Delicious", 
       "Reddit", "Forums","StackExchange", "rh", "NewsOutlets", "Wikipedia",
       "Journal", "Fields")

names(aalto_oa) <- n
names(aalto_oa2) <- n

# Take unique rows
aalto_oa_all <- rbind(aalto_oa, aalto_oa2)
aalto_oa_uniq <- aalto_oa_all[!duplicated(aalto_oa_all$DOI), ]

n <- c("ISSN", "DOI", "Title", "Authors", "Journal_short", "Journal", "School", "URL", 
       "AltmetricURL", "Year", 
       "WoS", "Altmetric", "Mendeley", "CiteULike",
       "GooglePlus", "Facebook", "Twitter", 
       "Accounts", "Blog posts", "Videos", "Delicious", 
       "Reddit", "Forums","StackExchange", "rh", "NewsOutlets", "Wikipedia",
       "OAJournal", "Fields")
names(aalto_oa_uniq) <- n

# Make a new OA variable by checking the OAJournal variable
# If NA, not an OA item, else OA:
m_oa_all <- aalto_oa_uniq %>%
  mutate(OA = ifelse(is.na(OAJournal),0,1)) %>%
  select(-starts_with("OAJournal")) %>%
  select(-starts_with("Fields"))


########################################################
#
# SCOPUS
# http://www.journalmetrics.com/values.php
#
# TO-DO: because the procedure is very much like the OA one above,
# try to functionalize
#
########################################################

# url <- "http://www.journalmetrics.com/documents/SNIP_SJR_complete_1999_2013_v1_incl_IPP.xlsx"
# file <- "scopus.xlsx"
# download.file(url, file, mode="wb")

# Java heap space problem.
# http://stackoverflow.com/questions/22145260/xlconnect-java-virtual-machine-out-of-memory-error
# Solution: save as .xls, e.g. via OOffice
#
# If already done, read in here and continue from row 280
aalto_scopus_uniq <- read.table(file = "scopus.csv", 
                                header = TRUE, 
                                sep = ";",
                                stringsAsFactors = FALSE)

#wb <- loadWorkbook("../altm2014oldies/oldies/scopus.xls")
#scopus <- readWorksheet(wb, sheet = "Metrics 1999-2013", header = TRUE)
# 
#scopus$field <- do.call(paste, c(scopus[,53:ncol(scopus)-1], sep=","))
#scopus$field <- gsub("NA,", "", scopus$field)
#scopus$field <- gsub("NA", "", scopus$field)
#scopus$field <- gsub(",$", "", scopus$field)
#
# SNIP2012 values are the same as SNIP2013
#
#scopus <- scopus[ , c("Print.ISSN", "E.ISSN", "X2013.SNIP", "X2012.IPP", "X2013.IPP", "X2012.SJR", "X2013.SJR", "field")]
#names(scopus) <- c("ISSN", "E-ISSN","SNIP2013", "IPP2012", "IPP2013", "SJR2012", "SJR2013", "Fields")
#
# Scopus ISSNs are without dash
#m_oa_all$ISSN <- gsub("-", "", m_oa_all$ISSN)
#
# CyberPsychology & Behavior (1094-9313) is now
# CyberPsychology, Behavior and Social Networking
# 2152-2715 (print); 2152-2723 (E)
#
#doi_scopus$issn[which(doi_scopus$Journal=="Cyberpsychology & Behavior")] <- "21522715"
#
#aalto_scopus <- merge(m_oa_all, scopus, by.x = "ISSN", by.y = "ISSN", all.x = TRUE)
#aalto_scopus <- aalto_scopus[-c(29)]
#aalto_scopus2 <- merge(m_oa_all, scopus, by.x = "ISSN", by.y = "E-ISSN", all.x = TRUE)
#aalto_scopus2 <- aalto_scopus2[-c(29)]

# Take unique rows
# 
#aalto_scopus_all <- rbind(aalto_scopus, aalto_scopus2)
#aalto_scopus_uniq <- aalto_scopus_all[!duplicated(aalto_scopus_all$DOI), ]
#
# Save this for the Shiny web app, the original sheet is too big
#
#write.table(aalto_scopus_uniq, 
#            file = "scopus.csv",
#            sep = ";",
#            row.names = FALSE)


#############################
#
# If scopus.csv already done
#
#############################

# NAs cause trouble in ggvis
aalto_scopus_uniq$SNIP2013 <- with(aalto_scopus_uniq, ifelse(is.na(SNIP2013),0,SNIP2013))
aalto_scopus_uniq$IPP2012 <- with(aalto_scopus_uniq, ifelse(is.na(IPP2012),0,IPP2012))
aalto_scopus_uniq$IPP2013 <- with(aalto_scopus_uniq, ifelse(is.na(IPP2013),0,IPP2013))
aalto_scopus_uniq$SJR2012 <- with(aalto_scopus_uniq, ifelse(is.na(SJR2012),0,SJR2012))
aalto_scopus_uniq$SJR2013 <- with(aalto_scopus_uniq, ifelse(is.na(SJR2013),0,SJR2013))


################################################################
#
# JUFO classification
#
# Search at http://www.tsv.fi/julkaisufoorumi/haku.php?lang=en 
# with 'Search all', and download the csv result file
# 
# TO-DO: because the procedure is very much like the OA one above,
# try to functionalize
#
###############################################################

# cut -d ";" -f 2,4-5 altm2014/jufo.csv >altm2014/jufo2.csv

jufo <- read.csv2(file = "jufo2.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)

names(jufo) <- c("Level", "ISSN1", "ISSN2")
jufo$Level <- gsub("-", NA, jufo$Level)
jufo$Level <- as.integer(as.character(jufo$Level))

jufo$ISSN1 <- gsub("-", "", jufo$ISSN1)
jufo$ISSN2 <- gsub("-", "", jufo$ISSN2)

jufo_doi_issn1 <- merge(aalto_scopus_uniq, jufo, by.x = "ISSN", by.y = "ISSN1", all.x = TRUE)
jufo_doi_issn2 <- merge(aalto_scopus_uniq, jufo, by.x = "ISSN", by.y = "ISSN2", all.x = TRUE)
jufo_doi_issn1 <- jufo_doi_issn1[-c(36)]
jufo_doi_issn2 <- jufo_doi_issn2[-c(36)]

jufo_doi_all <- rbind(jufo_doi_issn1, jufo_doi_issn2)
jufo_doi_uniq <- jufo_doi_all[!duplicated(jufo_doi_all$DOI), ]

oa_jufo_all <- jufo_doi_uniq %>%
  mutate(JuFo = ifelse(is.na(Level),NA,Level)) %>%
  select(-starts_with("Fields")) %>%
  select(-starts_with("Level"))

oa_jufo_all$JuFo <- as.integer(as.character(oa_jufo_all$JuFo))

all_uniq <- oa_jufo_all[!duplicated(oa_jufo_all$DOI), ]

aalto_all <- all_uniq

#aalto_all$WoS <- as.integer(as.character(aalto_all$WoS))
#aalto_all$OA <- as.integer(as.character(aalto_all$OA))

###########################################
#
# Add new variables:
#
# - Acronyms for plot labels and tooltips
# - Number of authors
# - keys for ggvis tooltips
#
###########################################

columns <- c(which(colnames(aalto_all) == "SNIP2013"):ncol(aalto_all)-1)
aalto_all[ ,columns] <- apply(aalto_all[ ,columns], 2, function(x) as.numeric(as.character(x)))
aalto_all$JuFo <- as.integer(as.character(aalto_all$JuFo))

aalto_all$Title5 <- with(aalto_all, paste(substring(Title,1,5), " (", Year, ")", sep=""))
aalto_all$Title10 <- with(aalto_all, paste(substring(Title,1,10), " (", Year, ")", sep=""))
aalto_all$Title20 <- with(aalto_all, paste(substring(Title,1,20), " (", Year, ")", sep=""))
aalto_all$Journal5 <- with(aalto_all, paste(substring(Journal,1,5), " (", Year, ")", sep=""))
aalto_all$Journal10 <- with(aalto_all, paste(substring(Journal,1,10), " (", Year, ")", sep=""))
#aalto_all$Authors5 <- with(aalto_all, paste(substring(Authors,1,5), " (", Year, ")", sep=""))
aalto_all$NrOfAuthors <- with(aalto_all, as.integer(str_count(Authors, ";")+1))
aalto_all$keys <- seq_along(aalto_all[,1])




##########################################
#
#  Texts and other constants
#
##########################################

metrics <- c("Altmetric", "WoS", "Mendeley", "Twitter", "Facebook", "GooglePlus", "CiteULike", 
             "Videos", "Reddit", "NewsOutlets", "Wikipedia", "NrOfAuthors", "SNIP2013", "IPP2012", "IPP2013", "SJR2012", "SJR2013", "JuFo")

dimensions <- c("Title", "Journal")

colsChart <- c("Altmetric", "WoS", "Mendeley", "Twitter", "Facebook", "GooglePlus", "CiteULike", 
               "Videos", "Reddit", "NewsOutlets", "Wikipedia", "id", "NrOfAuthors", "SNIP2013", "IPP2012", "IPP2013", "SJR2012", "SJR2013", "JuFo")

colsChart2 <- c("Altmetric", "Mendeley", "Twitter", "Facebook", "GooglePlus", "CiteULike", 
                "Reddit", "NewsOutlets", "Wikipedia", "id", "SNIP2012", "SNIP2013", "IPP2012", "IPP2013", "SJR2012", "SJR2013", "JuFo")

colsTable <- c("Journal", "Title", "AltmetricURL", "NrOfAuthors", "Altmetric",
               "Authors", "Year", "WoS", "Mendeley", "School",
               "Twitter", "Facebook", "GooglePlus", "CiteULike",
               "Videos", "Reddit", "NewsOutlets", "Wikipedia", "SNIP2013", "IPP2012", "IPP2013", "SJR2012", "SJR2013", "JuFo")

schools <- "ARTS = School of Arts, Design and Architecture
    <br/>BIZ = School of Business<br/>CHEM = School of Chemical Technology<br/>ELEC = School of Electrical Engineering<br/>ENG = School of Engineering
    <br/>SCI = School of Science
    <br/><br/>Items with a golden stroke are Open Access (DOAJ)"

scs <- c("All", "ARTS", "BIZ", "CHEM", "ELEC", "ENG", "SCI")

help <- "Score: Altmetric. Citations: Web of Science (WoS). Rating: Publication Channel (JuFo). 
Scopus: Source Normalized Impact per Paper (SNIP), SCImago Journal Rank (SJR), The Impact per Publication (IPP).
Readers: Mendeley. Mentions: Twitter, Facebook, GooglePlus, YouTube, Wikipedia. Social bookmarking: CiteULike, Reddit. News: Science news outlets"

datadate <- "10 Feb 2015"
code <- "<a href='https://gist.github.com/tts/0df0fd1deae932d6488b' target='_blank'>R code</a>"
