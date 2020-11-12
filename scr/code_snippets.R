

# Snippets of code for a range of tasks necessary to calculate poverty estimates

library(tidyverse)
library(Hmisc) # for calculating weighted medians

# Load datasets
# These are the imported SAS SHS datasets available to Scottish Government
# analysts; the datasets available on the UK Data Service will be very similar

person19 <- readRDS("output/person19.rds")
hhold19 <- readRDS("output/hhold19.rds")

# 1. Add equivalence factors

person <- person19 %>%
  select(UNIQID, HA5) %>%
  # count number of under-14 year-olds and number of all people in household
  mutate(u14 = ifelse(HA5 <= 13, 1, 0),
         pp = 1) %>%
  group_by(UNIQID) %>%
  summarise(u14 = sum(u14),
            pp = sum(pp))

hhld <- hhold19 %>%
  left_join(person, by = "UNIQID") %>%
  # calculate equivalence factor using modified OECD scale
  mutate(equ = 0.67 + (pp - u14 - 1)*0.33 + u14*0.2) %>%
  select(-pp, -u14)


# 2. Calculate equivalised net household income before housing costs
#    Note that this requires the equivalence factor and amount of council tax
#    paid

hhld <- hhold19 %>%
  select(UNIQID, ANNETINC_BROAD, equ, counciltax) %>%
  # set NA and negative incomes to 0, then deduct council tax and equivalise
  mutate(ANNETINC_BROAD = case_when(is.na(ANNETINC_BROAD) ~ 0,
                                    ANNETINC_BROAD < 0 ~ 0,
                                    TRUE ~ ANNETINC_BROAD),
         bhcinc = (ANNETINC_BROAD - counciltax)/equ)


# 3. Create poverty flag (BHC)
#    Note that this requires equivalised net household income before housing
#    costs, and number of people in household to create person weight

hhold19 %>%
  select(bhcinc, LA_GRWT, pp) %>%
  mutate(bhcmedian = wtd.quantile(bhcinc, probs = 0.5, weights = LA_GRWT*pp),
         povbhc = ifelse(bhcinc < 0.6*bhcmedian, 1, 0))


