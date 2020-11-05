
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
source("scr/charts_tables.R", encoding = "UTF-8")

# produce word document
rmarkdown::render(input = "Final report.Rmd",
                  output_format = "word_document",
                  encoding = "UTF-8")
