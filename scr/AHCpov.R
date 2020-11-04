
library(tidyverse)
library(readxl) # import Excel files
library(scales)
library(labelled)
library(Hmisc)
library(naniar) # for dealing with missing values

source("scr/functions.R")
source("scr/strings.R")
adult_1819 <- readRDS("output/adult_1819.rds")
househol_1819 <- readRDS("output/househol_1819.rds")
benefits_1819 <- readRDS("output/benefits_1819.rds")
hbai1819 <- readRDS("output/hbai1819.rds")

# create tidy SHS dataset ----

person19 <- readRDS("output/person19.rds")
hhold19 <- readRDS("output/hhold19.rds")

# Get number of individuals from person dataset for equivalisation and weights

# get number of children under 14, children over 14, adults
# get number of dependent children; working-age adults; pensioners per hhld

personshs19 <- person19 %>%
  # Remove permanently absent hhld members
  select(UNIQID, HA5, HA7, HA10, HA13, HA31:HA310, PENAGE) %>%
  filter(is.na(HA10)|HA10 != 1,
         is.na(HA13)|HA13 != 1) %>%
  mutate(u14 = ifelse(HA5 < 14, 1, 0),
         o64 = ifelse(HA5 >= 65, 1, 0),
         pp = 1,
         kid = ifelse(HA31 %in% c(4, 5) |
                        HA32 %in% c(4, 5) |
                        HA33 %in% c(4, 5) |
                        HA34 %in% c(4, 5) |
                        HA35 %in% c(4, 5) |
                        HA36 %in% c(4, 5) |
                        HA37 %in% c(4, 5) |
                        HA38 %in% c(4, 5) |
                        HA39 %in% c(4, 5) |
                        HA310 %in% c(4, 5) , 1, 0),
         depchld = ifelse(HA5 <= 16, 1, 0),
         p1719 = ifelse(HA5 <= 19 & HA5 >= 17, 1, 0),
         inedu = ifelse(HA7 %in% c(7, 8), 1, 0),
         athome = ifelse(HA10 != 1 & !is.na(HA10), 1, 0),
         depchld = ifelse(kid == 1 & p1719 == 1 & inedu == 1 & athome == 1, 1, depchld)) %>%
  group_by(UNIQID)  %>%
  summarise(u14 = sum(u14),
            pp = sum(pp),
            ch = sum(depchld),
            o64 = sum(o64),
            pn = ifelse(is.na(sum(PENAGE)), o64, sum(PENAGE)),
            wa = pp - ch - pn)


# Note there is one hhld with one dependent child and no (present) adults
# check <- filter(personshs19, wa == 0 & pn == 0)

# Work out council tax from council tax band and single person discount
# Council tax is deducted from household income in the HBAI
# Note that water and sewerage charges are part of housing costs in the HBAI, so I won't include them here


counciltax <- read_excel("ext/docs/CouncilTax1819.xlsx", sheet = "CouncilTax") %>%
  mutate(COUNCIL = as.numeric(COUNCIL)) %>%
  select(-council) %>%
  remove_labels()

# Get manually allocated council tax bands from SHCS

#SHCScounciltaxbands <- read_excel("ext/docs/SHCS counciltaxbands.xlsx") %>%
#  remove_labels()

# Get age group weights, income vars, equivalise

tidyshs19 <- hhold19 %>%
  select(UNIQID, MSCINC01:MSCINC10, EARNINC, BENINC,  
         LA_GRWT, COUNCIL, SHS_6CLA, HIHECON, TENURE, HINCMINC, HH4, HCOST_AMT) %>%
  left_join(personshs19, by = "UNIQID") %>%
  remove_attributes(c("label", "format.sas")) %>%
  # left_join(SHCScounciltaxbands, by = "UNIQID") %>%
  left_join(counciltax, by = "COUNCIL") %>%
  mutate_at(vars(MSCINC01:MSCINC10, EARNINC, BENINC),  ~replace_na(., 0)) %>%
  mutate(band = NA,
         counciltax = ifelse(band == "A", A, 
                             ifelse(band == "B", B,
                                    ifelse(band == "C", C,
                                           ifelse(band == "D", D,
                                                  ifelse(band == "E", E,
                                                         ifelse(band == "F", F,
                                                                ifelse(band == "G", G, 
                                                                       ifelse(band == "H", H, B))))))) ),
         counciltax_disc = ifelse(pp - ch == 1, 0.75 * counciltax, counciltax),
         equ = 0.58 + (pp-u14-1)*0.42 + u14*0.2,
         earn = EARNINC*7/(365*equ),
         ben = BENINC*7/(365*equ),
         privben = (MSCINC02 + MSCINC06)*7/(365*equ),
         occ = MSCINC01*7/(365*equ),
         inv = MSCINC07*7/(365*equ),
         oth = (MSCINC03 + MSCINC04 + MSCINC05 + MSCINC08 + MSCINC09 + MSCINC10)*7/(365*equ),
         ded = ifelse(!is.na(counciltax), -counciltax_disc*7/(365*equ), 0),
         total = (earn + ben + privben + occ + inv + oth + ded),
         hhwgt = LA_GRWT,
         ppwgt = round(LA_GRWT*pp),
         chwgt = round(LA_GRWT*ch),
         wawgt = round(LA_GRWT*wa),
         pnwgt = round(LA_GRWT*pn),
         council = COUNCIL,
         urbrur = SHS_6CLA,
         HIHemp = ifelse(HIHECON != 8, HIHECON, ifelse(is.na(HINCMINC), 8, ifelse(HINCMINC <= 0, 8, ifelse(is.na(HH4), 3, ifelse(HH4 >= 30, 2, 3))))),
         tenure = ifelse(TENURE == 1, 4,
                         ifelse(TENURE == 2, 5,
                                ifelse(TENURE == 3, 1,
                                       ifelse(TENURE == 4, 2, 3)))),
         hcost = HCOST_AMT*7*12/365,
         hhtype = ifelse(pp == 1 & wa == 1, 1, 
                         ifelse(pp == 2 & wa == 2, 2, 
                                ifelse(pp >= 3 & wa == pp & pn == 0, 3, 
                                       ifelse(ch >= 1 & wa == 1 & pn == 0, 4, 
                                              ifelse(ch >= 1 & wa == 2 & pn == 0, 5, 
                                                     ifelse(pp == 1 & pn == 1, 6, 
                                                            ifelse(pp == 2 & pn == 2, 7, 
                                                                   ifelse(pp == 2 & wa == 1 & pn == 1, 8, 9)))))))),
         ID = row_number()) %>%
  select(ID, hhwgt, ppwgt, chwgt, wawgt, pnwgt, 
         total, equ,
         council, urbrur, hhtype, HIHemp, tenure, hcost) %>%
  remove_labels() %>%
  gather(key = type, value = amount, -ID, -hhwgt, -ppwgt, -chwgt, -wawgt, -pnwgt, -equ,
         -council, -urbrur, -hhtype, -HIHemp, -tenure, -hcost) %>%
  mutate(survey = "SHS")


# Recode council area, household type, and economic status vars

tidyshs19$council <- decode(tidyshs19$council,
                            search = SHScodes, 
                            replace = councilnames,  
                            default = "unknown")

tidyshs19$hhtype <- decode(tidyshs19$hhtype,
                           search = hhtypecodes, 
                           replace = hhtypenames,
                           default = "unknown")

tidyshs19$HIHemp <- decode(tidyshs19$HIHemp,
                           search = SHS_HIHECONcodes, 
                           replace = SHS_HIHECONrecode,
                           default = 7)

tidyshs19$tenure <- decode(tidyshs19$tenure,
                           search = tenurecodes, 
                           replace = tenurenames)


# create tidy HBAI dataset ----

# ADULT dataset - Get Highest Income Householder and their economic status

tidyadult <- adult_1819 %>%
  filter(HRPID == 1) %>%
  select(SERNUM, EMPSTATI) %>%
  rename(HIHemp = EMPSTATI)

# HOUSEHOL dataset - Get council and urban/rural class

tidyhousehol <- househol_1819 %>%
  filter(COUNTRY == 3) %>%
  mutate(council = LAC,
         urbrur = URINDS) %>%
  select(SERNUM, council, urbrur)

# Recode urban/rural to 6-tier classification

tidyhousehol$urbrur <- decode(tidyhousehol$urbrur,
                              search = c(1, 2, 3, 4, 5, 6, 7, 8),
                              replace = c(1, 2, 3, 4, 4, 5, 6, 6))

# Get weights from HBAI dataset

HBAIweights <- hbai1819 %>%
  filter(GVTREGN == 12) %>%
  select(SERNUM, gs_newpp, gs_newch, gs_newwa, gs_newpn, gs_newhh) %>%
  group_by(SERNUM) %>%
  summarise(ppwgt = sum(gs_newpp),
            chwgt = sum(gs_newch),
            wawgt = sum(gs_newwa),
            pnwgt = sum(gs_newpn),
            hhwgt = max(gs_newhh))

# Combine all FRS and HBAI datasets

tidyhbai <- hbai1819 %>%
  filter(GVTREGN == 12,
         BENUNIT == 1) %>%
  left_join(HBAIweights, by = "SERNUM") %>%
  left_join(tidyadult, by = "SERNUM") %>%
  mutate(simd = NA,
         equ = eqoahchh,
         earn = enternhh/equ,
         #earn = ifelse(enternhh >= 0, enternhh/equ, 0),
         ben = ebeninhh/equ,
         privben = epribnhh/equ,
         occ = hntocchh/equ,
         inv = hntinvhh/equ,
         #inv = ifelse(hntinvhh >= 0, hntinvhh/equ, 0),
         oth =  (emiscihh + inchilhh)/equ,
         ded =  -eothdehh/equ,
         total = (earn + ben + privben + occ + inv + oth + ded),
         pp = ADULTH + DEPCHLDH,
         ch = DEPCHLDH,
         wa = round(pp*wawgt/ppwgt),
         pn = round(pp*pnwgt/ppwgt),
         hcost = ehcost,
         tenure = ifelse(PTENTYP2 %in% c(1,2,3), PTENTYP2, 
                         ifelse(PTENTYP2 == 4, 3,
                                ifelse(PTENTYP2 == 5, 4, 5))),
         hhtype = ifelse(pp == 1 & wa == 1, 1, 
                         ifelse(pp == 2 & wa == 2, 2, 
                                ifelse(pp >= 3 & wa == pp & pn == 0, 3, 
                                       ifelse(ch >= 1 & wa == 1 & pn == 0, 4, 
                                              ifelse(ch >= 1 & wa == 2 & pn == 0, 5, 
                                                     ifelse(pp == 1 & pn == 1, 6, 
                                                            ifelse(pp == 2 & pn == 2, 7, 
                                                                   ifelse(pp == 2 & wa == 1 & pn == 1, 8, 9)))))))),
         ID = row_number()) %>%
  select(ID, hhwgt, ppwgt, chwgt, wawgt, pnwgt, hhtype, HIHemp,
         total, equ, SERNUM, tenure, hcost, simd) %>%
  left_join(tidyhousehol, by = "SERNUM") %>%
  select(-SERNUM) %>%
  gather(key = type, value = amount, -ID, -hhwgt, -ppwgt, -chwgt, -wawgt, -pnwgt, 
         -council, -urbrur, -hhtype, - HIHemp, -equ, -tenure, -hcost, -simd) %>%
  mutate(survey = "HBAI") %>%
  remove_labels() 

# Recode council area, household type, tenure and economic status vars

tidyhbai$council <- decode(tidyhbai$council, 
                           search = FRScodes, 
                           replace = councilnames, 
                           default = "unknown")

tidyhbai$hhtype <- decode(tidyhbai$hhtype,
                          search = hhtypecodes, 
                          replace = hhtypenames,
                          default = "unknown")

tidyhbai$HIHemp <- decode(tidyhbai$HIHemp,
                          search = FRS_empstatcodes, 
                          replace = FRS_empstatrecode,
                          default = "unknown")

tidyhbai$tenure <- decode(tidyhbai$tenure,
                          search = tenurecodes, 
                          replace = tenurenames,
                          default = "unknown")

# combine and add AHC poverty flag ----

tidyhbai <- tidyhbai %>%
  filter(type == "total") %>%
  select( -type, -simd)

tidyshs19 <- tidyshs19 %>%
  select(-type)


povahc <- rbind(tidyshs19, tidyhbai) %>%
  # Excluding SHS cases with missing housing costs (originally in "Other tenure" category)
  filter(!(survey == "SHS" & tenure == "Rented privately" & is.na(hcost))) %>%
  # Excluding mortgage holders
  filter(tenure %in% c("Rented from Council",
                       "Rented from Housing Association",
                       "Rented privately",
                       "Owned outright")) %>%
  # Excluding outright owners
  filter(tenure != "Owned outright") %>%
  # Setting SHS homeowners' (missing) housing costs to zero
  mutate(hcost = ifelse(survey == "SHS" & tenure == "Owned outright" & is.na(hcost), 0, hcost ),
         hcost_equ = hcost/equ) %>%
  group_by(survey) %>%
  mutate(incahc = amount-hcost_equ,
         mdahc = wtd.quantile(incahc, probs = 0.5, weights = ppwgt),
         low60ahc = ifelse(incahc < 0.6*mdahc, 1, 0),
         urbrur = factor(urbrur))

# povahc %>%
#   group_by(tenure) %>%
#   select(hcost) %>%
#   miss_var_summary()

povahc %>%
  group_by(survey, tenure) %>%
  summarise(hcost = comma(wtd.quantile(hcost_equ, probs = 0.5, weights = ppwgt), 1)) %>%
  spread(survey, hcost) %>%
  knitr::kable()

povahc$HIHemp <- decode(povahc$HIHemp,
                        search = c(seq(1, 10)),
                        replace = empstatnames)

povahc$urbrur <- decode(povahc$urbrur,
                        search = urbrurcodes,
                        replace = urbrurclasses)

# create cross-tabs - median housing costs ----

# tenure
povahc %>%
  rename(groupingvar = tenure) %>%
  group_by(survey, groupingvar) %>%
  summarise(hcost_md = wtd.quantile(hcost_equ, probs = 0.5, weights = ppwgt),
            hcost_mn = wtd.mean(hcost_equ, weights = ppwgt)) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(hcost_md))) %>%
  ggplot(mapping = aes(x = groupingvar, y = hcost_md, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL)

# hhtype
povahc %>%
  rename(groupingvar = hhtype) %>%
  group_by(survey, groupingvar) %>%
  summarise(hcost_md = wtd.quantile(hcost_equ, probs = 0.5, weights = ppwgt),
            hcost_mn = wtd.mean(hcost_equ, weights = ppwgt)) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(hcost_md))) %>%
  ggplot(mapping = aes(x = groupingvar, y = hcost_md, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL)


# ecostatus
povahc %>%
  rename(groupingvar = HIHemp) %>%
  group_by(survey, groupingvar) %>%
  summarise(hcost_md = wtd.quantile(hcost_equ, probs = 0.5, weights = ppwgt),
            hcost_mn = wtd.mean(hcost_equ, weights = ppwgt)) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(hcost_md))) %>%
  ggplot(mapping = aes(x = groupingvar, y = hcost_md, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL) 

# urban rural
povahc %>%
  rename(groupingvar = urbrur) %>%
  group_by(survey, groupingvar) %>%
  summarise(hcost_md = wtd.quantile(hcost_equ, probs = 0.5, weights = ppwgt),
            hcost_mn = wtd.mean(hcost_equ, weights = ppwgt)) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(hcost_md))) %>%
  ggplot(mapping = aes(x = groupingvar, y = hcost_md, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL) 


# create cross-tabs - AHC poverty ----
# excluding pensioners

# tenure
povahc %>%
  rename(groupingvar = tenure) %>%
  group_by(survey, groupingvar) %>%
  summarise(pov = wtd.mean(low60ahc, weights = (chwgt + wawgt))) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(pov))) %>%
  ggplot(mapping = aes(x = groupingvar, y = pov, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL)

# hhtype
povahc %>%
  rename(groupingvar = hhtype) %>%
  group_by(survey, groupingvar) %>%
  summarise(pov = wtd.mean(low60ahc, weights = (chwgt + wawgt))) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(pov))) %>%
  ggplot(mapping = aes(x = groupingvar, y = pov, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL) 

# ecostatus
povahc %>%
  rename(groupingvar = HIHemp) %>%
  group_by(survey, groupingvar) %>%
  summarise(pov = wtd.mean(low60ahc, weights = (chwgt + wawgt))) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(pov))) %>%
  ggplot(mapping = aes(x = groupingvar, y = pov, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL) 

# urban rural
povahc %>%
  rename(groupingvar = urbrur) %>%
  group_by(survey, groupingvar) %>%
  summarise(pov = wtd.mean(low60ahc, weights = (chwgt + wawgt))) %>%
  ungroup() %>%
  mutate(groupingvar = fct_reorder2(groupingvar, desc(survey), desc(pov))) %>%
  ggplot(mapping = aes(x = groupingvar, y = pov, fill = survey)) +
  geom_col(position = "dodge", colour = "white") +
  coord_flip() +
  theme(legend.position = "top") +
  xlab(NULL) 

