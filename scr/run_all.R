
# load packages
library(plyr) # needed for groupwiseMedian function
library(tidyverse)
library(labelled) # to remove labels from imported data
library(readxl)
library(scales)
library(Hmisc)
library(naniar) # for dealing with missing values
library(ggrepel) # for non-overlapping chart annotation
library(boot) # needed for groupwiseMedian function
library(survey)

source("scr/functions.R")
source("scr/strings.R")
source("scr/colours.R")

person18 <- readRDS("output/person18.rds")
hhold18 <- readRDS("output/hhold18.rds")
adult_1819 <- readRDS("output/adult_1819.rds")
househol_1819 <- readRDS("output/househol_1819.rds")
benefits_1819 <- readRDS("output/benefits_1819.rds")
hbai1819 <- readRDS("output/hbai1819.rds")

# run scripts
source("scr/prepSHSdata.R", encoding = "UTF-8")
source("scr/prepHBAIdata.R", encoding = "UTF-8")

cilif_council <- read_excel("ext/docs/Cilif.xlsx", sheet = "council")
cilif_hhtype <- read_excel("ext/docs/Cilif.xlsx", sheet = "hhtype")
cilif_work <- read_excel("ext/docs/Cilif.xlsx", sheet = "work")
cilif_urbrur <- read_excel("ext/docs/Cilif.xlsx", sheet = "urbrur")

tidyshs <- readRDS("output/tidyshs.rds")
tidyshsbens <- readRDS("output/tidyshsbens.rds")
tidyhbai <- readRDS("output/tidyhbai.rds")
tidyhbaibens <- readRDS("output/tidyhbaibens.rds")

source("scr/prepdata.R", encoding = "UTF-8")
source("scr/prepPovertydata.R", encoding = "UTF-8")
source("scr/tables.R", encoding = "UTF-8")
source("scr/charts.R", encoding = "UTF-8")

# remove no longer needed data
keep <- c("bencontr",
          "incdiff",
          "totdiff",
          "SHSmedian",
          "HBAImedian",
          "ecoshares",
          paste0("ch0", seq(1,9)),
          paste0("ch", seq(10,50)),
          paste0("tb0", seq(1,9)),
          paste0("tb", seq(10,50)),
          "ch16b")

rm(list = setdiff(ls(), keep))

# produce word document - final report
rmarkdown::render(input = "Final report.Rmd",
                  output_format = "word_document",
                  encoding = "UTF-8")

# produce word document - Annex
rmarkdown::render(input = "Final report technical annex.Rmd",
                  output_format = "word_document",
                  encoding = "UTF-8")
