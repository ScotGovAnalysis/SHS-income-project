

# import datasets

adult_1819 <- read_sas("../Full SAS Datasets/adult_1819.sas7bdat")
househol_1819 <- read_sas("../Full SAS Datasets/househol_1819.sas7bdat")
benefits_1819 <- read_sas("../Full SAS Datasets/benefits_1819.sas7bdat")
hbai1819 <- read_sas("../Full SAS Datasets/hbai1819.sas7bdat")
person18 <- read_sas("../Full SAS Datasets/person18.sas7bdat")
hhold18 <- read_sas("../Full SAS Datasets/hhold18.sas7bdat")


# Create tidy SHS dataset ----

# Equivalise: 
# get number of children under 14 from person dataset; 
# get number of adults and children over 14

# Age group weights:
# get number of dependent children; working-age adults; pensioners per hhld

personshs <- person18 %>%
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
  group_by(UNIQID) %>%
  summarise(u14 = sum(u14),
            pp = sum(pp),
            ch = sum(depchld),
            o64 = sum(o64),
            pn = ifelse(is.na(sum(PENAGE)), o64, sum(PENAGE)),
            wa = pp - ch - pn)


# Note there is one hhld with one dependent child and no (present) adults
# check <- filter(personshs, wa == 0 & pn == 0)

tidyshs <- hhold18 %>%
  select(UNIQID, MSCINC01:MSCINC10, EARNINC, BENINC, BENINC01:BENINC40, BENINC01_OA1:BENINC40_OA3, LA_GRWT, COUNCIL, SHS_6CLA, HIHECON) %>%
  left_join(personshs, by = "UNIQID") %>%
  mutate_at(vars(MSCINC01:MSCINC10, EARNINC, BENINC, BENINC01:BENINC40, BENINC01_OA1:BENINC40_OA3), ~replace_na(., 0)) %>%
  mutate(equ = 0.67 + (pp-u14-1)*0.33 + u14*0.2,
         earn = EARNINC*7/(365*equ),
         ben = BENINC*7/(365*equ),
         privben = (MSCINC02 + MSCINC06)*7/(365*equ),
         occ = MSCINC01*7/(365*equ),
         inv = MSCINC07*7/(365*equ),
         oth = (MSCINC03 + MSCINC04 + MSCINC05 + MSCINC08 + MSCINC09 + MSCINC10)*7/(365*equ),
         total = (earn + ben + privben + occ + inv + oth),
         weight = round(LA_GRWT*pp),
         chwgt = round(LA_GRWT*ch),
         wawgt = round(LA_GRWT*wa),
         pnwgt = round(LA_GRWT*pn),
         council = COUNCIL,
         urbrur = SHS_6CLA,
         HIHemp = HIHECON,
         hhtype = ifelse(pp == 1 & wa == 1, 1, 
                         ifelse(pp == 2 & wa == 2, 2, 
                                ifelse(pp >= 3 & wa == pp & pn == 0, 3, 
                                       ifelse(ch >= 1 & wa == 1 & pn == 0, 4, 
                                              ifelse(ch >= 1 & wa == 2 & pn == 0, 5, 
                                                     ifelse(pp == 1 & pn == 1, 6, 
                                                            ifelse(pp == 2 & pn == 2, 7, 
                                                                   ifelse(pp == 2 & wa == 1 & pn == 1, 8, 9)))))))),
         ID = row_number()) %>%
  select(ID, weight, chwgt, wawgt, pnwgt, 
         total, earn, ben, privben, occ, inv, oth, equ,
         council, urbrur, hhtype, HIHemp, pp, ch, wa, pn, BENINC01:BENINC40, BENINC01_OA1:BENINC40_OA3) %>%
  remove_labels() %>%
  gather(key = type, value = amount, -ID, -weight, -chwgt, -wawgt, -pnwgt, 
         -council, -urbrur, -hhtype, -HIHemp, -pp, -ch, -wa, -pn, -equ, 
         -(BENINC01:BENINC40), -(BENINC01_OA1:BENINC40_OA3)) %>%
  mutate(survey = "SHS")

tidyshs$council <- decode(tidyshs$council,
                          search = SHScodes, 
                          replace = councilnames,  
                          default = "unknown")

tidyshs$hhtype <- decode(tidyshs$hhtype,
                         search = hhtypecodes, 
                         replace = hhtypenames,
                         default = "unknown")

tidyshs$HIHemp <- decode(tidyshs$HIHemp,
                         search = SHS_HIHECONcodes, 
                         replace = SHS_HIHECONrecode,
                         default = 7)

# Create dataset with detailed benefits

tidyshsbens <- filter(tidyshs, type == "ben")

# Remove extra benefit information

tidyshs <- select(tidyshs, -(BENINC01:BENINC40), -(BENINC01_OA1:BENINC40_OA3))

# Create tidy HBAI dataset ----

# Get Highest Income Householder (= Household Reference Person)
# from person dataset

tidyadult <- adult_1819 %>%
  filter(HRPID == 1) %>%
  select(SERNUM, EMPSTATI) %>%
  rename(HIHemp = EMPSTATI)

# Get council and urban/rural class from househol dataset

tidyhousehol <- househol_1819 %>%
  filter(COUNTRY == 3) %>%
  mutate(council = LAC,
         urbrur = URINDS) %>%
  select(SERNUM, council, urbrur)


# Recode to 6-tier classification

tidyhousehol$urbrur <- decode(tidyhousehol$urbrur,
                              search = c(1, 2, 3, 4, 5, 6, 7, 8),
                              replace = c(1, 2, 3, 4, 4, 5, 6, 6))

# Get (most) benefits from benefits dataset
# Get these from HBAI dataset: ben02 HBENBU, ben10 CTREBAM1, ben16 WINPAYBU
# Get these from FRS adult dataset: ben17 SMPADJ + SPPADJ + SAPADJ, ben24 SSPADJ

tidyfrsbens <- benefits_1819 %>%
  select(SERNUM, BENEFIT, BENAMT, USUAL, NOTUSAMT, PRES, BENPD, VAR2) %>%
  mutate(VAR2 = ifelse(is.na(VAR2), 0, VAR2),
         USUAL = ifelse(is.na(USUAL), 0, USUAL),
         PRES = ifelse(is.na(PRES), 0, PRES),
         ben01 = ifelse(BENEFIT == 5, 
                        ifelse(USUAL == 2 & !is.na(NOTUSAMT), NOTUSAMT, BENAMT), 0), 
         ben03 = ifelse(BENEFIT == 3, BENAMT, 0), 
         ben04 = ifelse(BENEFIT == 91 | BENEFIT == 93, BENAMT, 0), 
         ben05 = ifelse(BENEFIT == 16, BENAMT, 0), 
         ben05 = ifelse(BENEFIT == 66 & BENAMT > 0 & VAR2 != 1, BENAMT, ben05), 
         ben06 = ifelse(BENEFIT == 96 | BENEFIT == 97, BENAMT, 0), 
         ben07 = ifelse(BENEFIT == 1 | BENEFIT == 2, BENAMT, 0),
         ben08 = ifelse(BENEFIT == 95, BENAMT, 0),
         ben08 = ifelse(BENEFIT == 110 & BENAMT > 0 & VAR2 != 1, BENAMT, ben08), 
         ben09 = ifelse(BENEFIT == 90 | BENEFIT == 92, BENAMT, 0), 
         ben11 = ifelse(BENEFIT == 13, BENAMT, 0), 
         ben12 = ifelse(BENEFIT == 19, BENAMT, 0), 
         ben12 = ifelse(BENEFIT == 65 & BENAMT > 0 & VAR2 != 1, BENAMT, ben12), 
         ben13 = ifelse(BENEFIT == 12, BENAMT, 0), 
         ben14 = ifelse(BENEFIT == 4, BENAMT, 0), 
         ben15 = ifelse(BENEFIT == 14, BENAMT, 0),  
         ben18 = ifelse(BENEFIT == 6, 
                        ifelse(USUAL == 2, 
                               ifelse(is.na(NOTUSAMT), BENAMT, NOTUSAMT), BENAMT), 0), 
         ben19 = ifelse(BENEFIT == 21, BENAMT, 0),
         ben20 = ifelse(BENEFIT == 17, BENAMT, 0), 
         ben21 = ifelse(BENEFIT == 10, BENAMT, 0), 
         ben22 = ifelse(BENEFIT == 15, BENAMT, 0), 
         ben23 = NA,
         ben25 = ifelse(BENEFIT == 37, BENAMT, 0), 
         ben26 = NA, 
         ben27 = ifelse(BENEFIT == 98, BENAMT*7/365, 0), 
         ben28 = NA, 
         ben29 = ifelse(BENEFIT == 69 | BENEFIT == 70 | BENEFIT == 111, 
                        ifelse(VAR2 == 2 & BENAMT > 0, BENAMT, 0), 0), 
         ben30 = NA) %>% 
  group_by(SERNUM) %>%
  summarise_at(vars(ben01:ben30), sum) 


# Get person, child, wa and pensioner weights from HBAI dataset

HBAIweights <- hbai1819 %>%
  filter(GVTREGN == 12) %>%
  select(SERNUM, gs_newpp, gs_newch, gs_newwa, gs_newpn) %>%
  group_by(SERNUM) %>%
  summarise(weight = sum(gs_newpp),
            chwgt = sum(gs_newch),
            wawgt = sum(gs_newwa),
            pnwgt = sum(gs_newpn))

# Combine all FRS and HBAI datasets

tidyhbai <- hbai1819 %>%
  filter(GVTREGN == 12,
         BENUNIT == 1) %>%
  left_join(HBAIweights, by = "SERNUM") %>%
  left_join(tidyadult, by = "SERNUM") %>%
  mutate(equ = eqobhchh,
         earn = enternhh/equ,
         ben = ebeninhh/equ,
         privben = epribnhh/equ,
         occ = hntocchh/equ,
         inv = hntinvhh/equ,
         oth =  emiscihh/equ,
         total = (earn + ben + privben + occ + inv + oth),
         pp = ADULTH + DEPCHLDH,
         ch = DEPCHLDH,
         wa = round(pp*wawgt/weight),
         pn = round(pp*pnwgt/weight),
         hhtype = ifelse(pp == 1 & wa == 1, 1, 
                         ifelse(pp == 2 & wa == 2, 2, 
                                ifelse(pp >= 3 & wa == pp & pn == 0, 3, 
                                       ifelse(ch >= 1 & wa == 1 & pn == 0, 4, 
                                              ifelse(ch >= 1 & wa == 2 & pn == 0, 5, 
                                                     ifelse(pp == 1 & pn == 1, 6, 
                                                            ifelse(pp == 2 & pn == 2, 7, 
                                                                   ifelse(pp == 2 & wa == 1 & pn == 1, 8, 9)))))))),
         ID = row_number()) %>%
  select(ID, weight, chwgt, wawgt, pnwgt, hhtype, HIHemp, pp, ch, wa, pn, 
         total, earn, ben, privben, occ, inv, oth, equ, SERNUM) %>%
  left_join(tidyhousehol, by = "SERNUM") %>%
  gather(key = type, value = amount, -ID, -SERNUM, -weight, -chwgt, -wawgt, -pnwgt, -pp, -ch, -wa, -pn, -council, -urbrur, -hhtype, - HIHemp, -equ) %>%
  mutate(survey = "HBAI") %>%
  select(-SERNUM) %>%
  remove_labels() 

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

# Combine tidy SHS and HBAI datasets ----
# create hhld level dataset with different income components (using hhld weight)

tidydata <- rbind(tidyhbai, tidyshs) %>%
  mutate(survey = factor(survey, ordered = TRUE),
         n = 1)

tidydata$HIHemp <- decode(tidydata$HIHemp,
                          search = empstatcodes, 
                          replace = empstatnames,
                          default = "unknown")

tidydata$type <- factor(tidydata$type, levels = inctypes)


# Get decile points

decilepoints <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey) %>% 
  summarise(x=list(enframe(wtd.quantile(amount, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), weights = weight)))) %>%
  unnest(x) %>%
  mutate(name = str_c("dec", str_sub(name, 1L, 2L))) %>%
  spread(name, value)

deciles <- tidydata %>%
  filter(type == "total") %>%
  select(ID, survey, amount) %>%
  left_join(decilepoints, by = "survey") %>%
  mutate(decile = ifelse(amount < dec10, 1,
                         ifelse(amount < dec20, 2,
                                ifelse(amount < dec30, 3,
                                       ifelse(amount < dec40, 4,
                                              ifelse(amount < dec50, 5, 
                                                     ifelse(amount < dec60, 6,
                                                            ifelse(amount < dec70, 7,
                                                                   ifelse(amount < dec80, 8,
                                                                          ifelse(amount < dec90, 9, 10)))))))))) %>%
  select(ID, survey, decile)

tidydata <- tidydata %>%
  left_join(deciles, by = c("survey", "ID"))

tidydata$quintile <- decode(tidydata$decile,
                            search = c(seq(1,10)),
                            replace = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
                            default = NA)

# Check for missing data

# View(miss_var_summary(tidyhbai))
# View(miss_var_summary(tidyshs))
# View(miss_var_summary(tidydata))


