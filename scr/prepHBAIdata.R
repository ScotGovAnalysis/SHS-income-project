
# Create tidy HBAI dataset

# ADULT dataset - Get Highest Income Householder and their economic status  ----

tidyadult <- adult_1819 %>%
  filter(HRPID == 1) %>%
  select(SERNUM, EMPSTATI) %>%
  rename(HIHemp = EMPSTATI)

# HOUSEHOL dataset - Get council and urban/rural class  ----

tidyhousehol <- househol_1819 %>%
  filter(COUNTRY == 3) %>%
  mutate(council = LAC,
         urbrur = URINDS) %>%
  select(SERNUM, council, urbrur)

# Recode urban/rural to 6-tier classification ----

tidyhousehol$urbrur <- decode(tidyhousehol$urbrur,
                              search = c(1, 2, 3, 4, 5, 6, 7, 8),
                              replace = c(1, 2, 3, 4, 4, 5, 6, 6))




# Get weights from HBAI dataset ----

HBAIweights <- hbai1819 %>%
  filter(GVTREGN == 12) %>%
  select(SERNUM, gs_newpp, gs_newch, gs_newwa, gs_newpn, gs_newhh) %>%
  group_by(SERNUM) %>%
  summarise(ppwgt = sum(gs_newpp),
            chwgt = sum(gs_newch),
            wawgt = sum(gs_newwa),
            pnwgt = sum(gs_newpn),
            hhwgt = max(gs_newhh))

# Combine all FRS and HBAI datasets ----

tidyhbai <- hbai1819 %>%
  filter(GVTREGN == 12,
         BENUNIT == 1) %>%
  left_join(HBAIweights, by = "SERNUM") %>%
  left_join(tidyadult, by = "SERNUM") %>%
  mutate(simd = NA,
         equ = eqobhchh,
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
  select(ID, hhwgt, ppwgt, chwgt, wawgt, pnwgt, hhtype, HIHemp, pp, ch, wa, pn, 
         total, earn, ben, privben, occ, inv, oth, ded, equ, SERNUM, tenure, hcost, simd) %>%
  left_join(tidyhousehol, by = "SERNUM") %>%
  gather(key = type, value = amount, -ID, -SERNUM, -hhwgt, -ppwgt, -chwgt, -wawgt, -pnwgt, -pp, -ch, -wa, -pn, 
         -council, -urbrur, -hhtype, - HIHemp, -equ, -tenure, -hcost, -simd) %>%
  mutate(survey = "HBAI") %>%
  remove_labels() 



# Recode council area, household type, tenure and economic status vars ----

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

# Create benefit dataset ----

## BENEFITS dataset - Get (most) benefits ----

# Get these from HBAI dataset: ben02 HBENBU, ben10 CTREBAM1, ben16 WINPAYBU
# Get these from ADULT dataset: ben17 SMPADJ + SPPADJ + SAPADJ, ben24 SSPADJ

frsbens1 <- benefits_1819 %>%
  select(SERNUM, BENEFIT, BENAMT, USUAL, NOTUSAMT, PRES, BENPD, VAR2) %>%
  mutate(VAR2 = ifelse(is.na(VAR2), 0, VAR2),
         USUAL = ifelse(is.na(USUAL), 0, USUAL),
         PRES = ifelse(is.na(PRES), 0, PRES),
         ben01 = ifelse(BENEFIT == 5, 
                        ifelse(USUAL == 2 & !is.na(NOTUSAMT), NOTUSAMT, BENAMT), 0), 
         ben03 = ifelse(BENEFIT == 3, BENAMT, 0), 
         ben04 = ifelse(BENEFIT %in% c(90, 91, 92, 93), BENAMT, 0), 
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
  summarise_at(vars(ben01:ben30), sum) %>%
  remove_labels()

## ADULT dataset: Get ben17 SMPADJ + SPPADJ + SAPADJ, ben24 SSPADJ ----

frsbens2 <- adult_1819 %>%
  select(SERNUM, SMPADJ, SPPADJ, SAPADJ, SSPADJ) %>%
  mutate(ben17 = SMPADJ + SPPADJ + SAPADJ,
         ben24 = SSPADJ) %>%
  select(SERNUM, ben17, ben24) %>%
  group_by(SERNUM) %>%
  summarise_all(sum) %>%
  remove_labels()

## HBAI dataset: Get ben02 HBENBU, ben10 CTREBAM1, ben16 WINPAYBU ----

frsbens3 <- hbai1819 %>%
  mutate(ben02 = hbenbu,
         ben10 = ctrebam1,
         ben16 = winpaybu) %>%
  select(SERNUM, ben02, ben10, ben16) %>%
  group_by(SERNUM) %>%
  summarise_all(sum) %>%
  remove_labels()

## Combine all ----

tidyhbaibens <- filter(tidyhbai, type == "ben") %>%
  left_join(frsbens1, by = "SERNUM") %>%
  left_join(frsbens2, by = "SERNUM") %>%
  left_join(frsbens3, by = "SERNUM") %>%
  select(ID, sprintf("ben%02d", 1:30), everything(), -SERNUM)

## Transpose to long format and equivalise ----

# Note that total household benefit income is slightly higher than sum(ben01:ben30)
# due to some benefits not being included in the list

tidyhbaibens <- tidyhbaibens %>%
  select(-type, -pp, -ch, -wa, -pn, -amount) %>%
  gather(type, amount, -ID, -hhwgt, -ppwgt, -chwgt, -wawgt, -pnwgt, -council, -urbrur,
         -hhtype, -HIHemp, -survey, -equ, -tenure, -hcost, -simd) %>%
  mutate(amount = ifelse(is.na(amount), 0, amount/equ),
         survey = "HBAI") 

## Recode benefit names ----

tidyhbaibens$type <- decode(tidyhbaibens$type,
                           search = bencodesfrs,
                           replace = str_trunc(bennamesfrs, 30))

# Remove extra variable from tidyhbai dataset ----

tidyhbai <- select(tidyhbai, -SERNUM)
