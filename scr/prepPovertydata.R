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

tidydata <- filter(tidydata,
                   type == "total",
                   survey != "Admin") %>%
  select(-hcost, -type, -finman, -md, -n, -equ, -pp, -ch, -wa, -pn)

# Get (original) HBAI poverty flag

hbai_orig <- hbai1819 %>%
  filter(GVTREGN == 12,
         BENUNIT == 1) %>%
  mutate(ID = row_number(),
         survey = "HBAI",
         low60bhc_orig = low60bhc) %>%
  select(ID, survey, low60bhc_orig)

tidydata <- left_join(tidydata, hbai_orig, by = c("ID", "survey")) %>%
  mutate(low60bhc = ifelse(survey == "HBAI", low60bhc_orig, low60bhc))


# Change some theme elements (charts)

theme_update(legend.position = "top",
             legend.title = element_blank(),
             panel.background = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             axis.line.x = element_line(),
             axis.ticks = element_blank(),
             text = element_text(size=18))

# Prepare SHS survey data for poverty analysis

## SHS survey design for ch, wa, pn, pp; including all hhlds

data <- tidydata %>%
  filter(survey == "SHS") %>%
  mutate(allch = sum(chwgt),
         allwa = sum(wawgt),
         allpn = sum(pnwgt),
         allpp = sum(ppwgt),
         chwawgt = chwgt + wawgt,
         allchwa = sum(chwawgt))

## SHS survey design for child poverty

SHSpov_ch <- data %>%
  filter(survey == "SHS") %>%
  mutate(work = ifelse(HIHemp %in% c("Full-time Employee", "Part-time Employee", "Self-Employed"), 
                       "In working families",
                       "Not in working families"))

SHSpov_ch$urbrur <- decode(SHSpov_ch$urbrur,
                           search = urbrurclasses,
                           replace = urbrurclasses2)

SHSdesign_ch <- svydesign(id = ~ID, strata = ~council, weights = ~chwgt, data = SHSpov_ch, fpc = ~allch)

## SHS survey design for working-age adults, children + waa, pensioners, all people

SHSdesign_wa <- svydesign(id = ~ID, strata = ~council, weights = ~wawgt, data = data, fpc = ~allwa)
SHSdesign_chwa <- svydesign(id = ~ID, strata = ~council, weights = ~chwawgt, data = data, fpc = ~allchwa)
SHSdesign_pn <- svydesign(id = ~ID, strata = ~council, weights = ~pnwgt, data = data, fpc = ~allpn)
SHSdesign_pp <- svydesign(id = ~ID, strata = ~council, weights = ~ppwgt, data = data, fpc = ~allpp)

