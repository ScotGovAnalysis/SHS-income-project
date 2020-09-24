# Load packages

library(plyr) # needed for groupwiseMedian function
library(tidyverse)
library(naniar) # for dealing with missing values
library(labelled) # to remove labels from imported data
library(scales) # for comma and percent formats
library(ggrepel) # for non-overlapping chart annotation
library(Hmisc) # for weighted quartiles
library(boot) # needed for groupwiseMedian function
library(readxl) # import Excel files
library(survey) # survey data analysis

# Load functions

source("scr/functions.R")

# Load colour scheme and other strings

source("scr/strings.R")
source("scr/colours.R")

# Load datasets

adult_1819 <- readRDS("output/adult_1819.rds")
househol_1819 <- readRDS("output/househol_1819.rds")
benefits_1819 <- readRDS("output/benefits_1819.rds")
hbai1819 <- readRDS("output/hbai1819.rds")
person18 <- readRDS("output/person18.rds")
hhold18 <- readRDS("output/hhold18.rds")

cilif_council <- read_excel("ext/docs/Cilif.xlsx", sheet = "council")
cilif_hhtype <- read_excel("ext/docs/Cilif.xlsx", sheet = "hhtype")
cilif_work <- read_excel("ext/docs/Cilif.xlsx", sheet = "work")
cilif_urbrur <- read_excel("ext/docs/Cilif.xlsx", sheet = "urbrur")

# Create tidy datasets

source("scr/prepSHSdata.R")
source("scr/prepHBAIdata.R")

# Combine all data

source("scr/prepdata.R")

# Change some theme elements (charts)

theme_update(legend.position = "top",
             legend.title = element_blank(),
             panel.background = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             axis.ticks = element_blank(),
             text = element_text(size=16))

# Prepare SHS survey data for poverty analysis

## SHS survey design for child poverty

SHSpov_ch <- tidydata %>%
  filter(pnwgt == 0,
         type == "total",
         survey == "SHS") %>%
  mutate(allchildren = sum(chwgt),
         work = ifelse(HIHemp %in% c("Full-time Employee", "Part-time Employee", "Self-Employed"), 
                       "In working families",
                       "Not in working families"))

SHSpov_ch$urbrur <- decode(SHSpov_ch$urbrur,
                           search = urbrurclasses,
                           replace = urbrurclasses2)

SHSdesign_ch <- svydesign(id = ~ID, strata = ~council, weights = ~chwgt, data = SHSpov_ch, fpc = ~allchildren)

## SHS survey design for working age poverty

SHSpov_wa <- tidydata %>%
  filter(pnwgt == 0,
         type == "total",
         survey == "SHS") %>%
  mutate(alladults = sum(wawgt))

SHSdesign_wa <- svydesign(id = ~ID, strata = ~council, weights = ~wawgt, data = SHSpov_wa, fpc = ~alladults)

## SHS survey design for poverty excluding households with pensioners

SHSpov_pp <- tidydata %>%
  filter(pnwgt == 0,
         type == "total",
         survey == "SHS") %>%
  mutate(allpeople = sum(ppwgt))

SHSdesign_pp <- svydesign(id = ~ID, strata = ~council, weights = ~ppwgt, data = SHSpov_pp, fpc = ~allpeople)