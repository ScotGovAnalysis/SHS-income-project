
# Create tidy SHS dataset

# Get number of individuals from person dataset for equivalisation and weights ----

# get number of children under 14, children over 14, adults
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

# Get age group weights, income vars, equivalise ----

tidyshs <- hhold18 %>%
  select(UNIQID, MSCINC01:MSCINC10, EARNINC, BENINC, BENINC01:BENINC40, BENINC01_OA1:BENINC40_OA3, 
         LA_GRWT, COUNCIL, SHS_6CLA, HIHECON, TENURE) %>%
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
         hhwgt = LA_GRWT,
         ppwgt = round(LA_GRWT*pp),
         chwgt = round(LA_GRWT*ch),
         wawgt = round(LA_GRWT*wa),
         pnwgt = round(LA_GRWT*pn),
         council = COUNCIL,
         urbrur = SHS_6CLA,
         HIHemp = HIHECON,
         tenure = ifelse(TENURE == 1, 4,
                         ifelse(TENURE == 2, 5,
                                ifelse(TENURE == 3, 1,
                                       ifelse(TENURE == 4, 2, 3)))),
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
         total, earn, ben, privben, occ, inv, oth, equ,
         council, urbrur, hhtype, HIHemp, pp, ch, wa, pn, 
         BENINC01:BENINC40, BENINC01_OA1:BENINC40_OA3, tenure) %>%
  remove_labels() %>%
  gather(key = type, value = amount, -ID, -hhwgt, -ppwgt, -chwgt, -wawgt, -pnwgt, 
         -council, -urbrur, -hhtype, -HIHemp, -pp, -ch, -wa, -pn, -equ, 
         -(BENINC01:BENINC40), -(BENINC01_OA1:BENINC40_OA3), -tenure) %>%
  mutate(survey = "SHS")

# Recode council area, household type, and economic status vars ----

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

tidyshs$tenure <- decode(tidyshs$tenure,
                         search = tenurecodes, 
                         replace = tenurenames)

# Create SHS benefit dataset ----

tidyshsbens <- filter(tidyshs, type == "ben")

## Aggregate benefit amounts from all adults

bentypesSHS <- tidyshsbens %>%
  select(ID, bencodes, bencodesoa)

for (i in seq(1, 40)){
  bentypesSHS[, i+1] = bentypesSHS[, i+1] + bentypesSHS[, i+41] + bentypesSHS[, i+81] + bentypesSHS[, i+121] }

bentypesSHS <- bentypesSHS %>%
  select(ID, bencodes)

## Transpose to long format and equivalise ----

tidyshsbens <- tidyshsbens %>%
  select(-bencodesoa, -bencodes) %>%
  left_join(bentypesSHS, by = "ID") %>%
  select(-type, -amount, -pp, -ch, -wa, -pn) %>%
  gather(type, amount, -ID, -hhwgt, -ppwgt, -chwgt, -wawgt, -pnwgt, -council, -urbrur,
         -hhtype, -HIHemp, -survey, -equ, -tenure) %>%
  mutate(amount = amount*7/(365*equ),
         survey = "SHS") 

## Recode benefit names ----

tidyshsbens$type <- decode(tidyshsbens$type,
                            search = bencodes,
                            replace = str_trunc(bennames, 30))

# Remove extra benefit information from tidyshs dataset ----

tidyshs <- select(tidyshs, -(BENINC01:BENINC40), -(BENINC01_OA1:BENINC40_OA3))