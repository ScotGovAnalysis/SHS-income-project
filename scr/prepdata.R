
# Combine tidy SHS and HBAI datasets


# Create hhld level dataset with different income components (using hhld weight) ----

tidydata <- rbind(tidyhbai, tidyshs) %>%
  mutate(survey = factor(survey, ordered = TRUE),
         n = 1)


tidydata$HIHemp <- decode(tidydata$HIHemp,
                          search = empstatcodes, 
                          replace = empstatnames,
                          default = "unknown")

tidydata$type <- factor(tidydata$type, levels = inctypes)

tidydata$urbrur <- decode(tidydata$urbrur,
                          search = urbrurcodes,
                          replace = urbrurclasses)

tidydata$urbrur <- factor(tidydata$urbrur, levels = urbrurclasses)


# Get decile points ----

decilepoints <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey) %>% 
  summarise(x=list(enframe(wtd.quantile(amount, probs=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), weights = ppwgt)))) %>%
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

# get medians ----

SHSmedian <- filter(tidydata, type == "total", survey == "SHS") %>% 
  mutate(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt)) %>%
  select(median) %>% tail(1L) %>% pull()


HBAImedian <- filter(tidydata, type == "total", survey == "HBAI") %>% 
  mutate(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt)) %>%
  select(median) %>% tail(1L) %>% pull()

# get poverty / low income flags

lowinc <- tidydata %>%
  filter(type == "total") %>%
  select(survey, ID, amount) %>%
  mutate(low60bhc = ifelse(survey == "SHS", ifelse(amount < 0.6*SHSmedian, 1, 0), 
                           ifelse(amount < 0.6*HBAImedian, 1, 0)),
         low70bhc = ifelse(survey == "SHS", ifelse(amount < 0.7*SHSmedian, 1, 0), 
                           ifelse(amount < 0.7*HBAImedian, 1, 0)),
         low50bhc = ifelse(survey == "SHS", ifelse(amount < 0.5*SHSmedian, 1, 0), 
                           ifelse(amount < 0.5*HBAImedian, 1, 0))) %>%
  select(-amount)

tidydata <- tidydata %>%
  left_join(lowinc, by = c("survey", "ID"))




# Check for missing data ----

# View(miss_var_summary(tidyhbai))
# View(miss_var_summary(tidyshs))
# View(miss_var_summary(tidydata))

# Create benefits dataset ----

tidybens <- rbind(tidyhbaibens, tidyshsbens) %>%
  mutate(survey = factor(survey, ordered = TRUE))

tidybens$HIHemp <- decode(tidybens$HIHemp,
                             search = empstatcodes, 
                             replace = empstatnames,
                             default = "unknown")

# Add deciles

tidydeciles <- tidydata %>%
  filter(type == "total") %>%
  select(survey, ID, decile)

tidybens <- tidybens %>%
  left_join(tidydeciles, by = c("survey", "ID"))

# Import administrative data

admin <- read_excel("ext/docs/StatXplore benefit receipt.xlsx", sheet = "Weekly 201819") %>%
  mutate(amount = 1000000*amount,
         survey = "Admin",
         type = str_trunc(type, 30))

# Create aggregated benefits dataset

tidybens_agg <- tidybens %>%
  group_by(survey, type) %>%
  summarise(amount = sum(amount*hhwgt*equ)) %>%
  ungroup() %>%
  rbind(admin) %>%
  mutate(survey = factor(survey, levels = surveys, ordered = TRUE),
         type = factor(type),
         type = fct_reorder2(type, survey, desc(amount))) %>%
  arrange(desc(type), survey)


# Get some summary stats ----

medianearnings <- tidydata %>%
  filter(type == "earn") %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt)) %>%
  select(median) %>% pull()

names(medianearnings) <- c("HBAI", "SHS")

# Get councils with a large enough sample size (households with earnings > 0)

councilsn50 <- filter(tidydata, 
                      type == "total",
                      survey == "HBAI") %>%
  group_by(council) %>%
  count() %>%
  filter(n >= 50) %>%
  select(council) %>% pull()

# Get main benefits 

mainbens <- tidybens_agg %>%
  filter(survey == "HBAI") %>%
  mutate(type = fct_collapse(type, Disability = str_trunc(disbens, 30)),
         type = fct_reorder(type, desc(amount))) %>%
  group_by(type) %>%
  summarise(amount = sum(amount)) %>%
  head(length(cols_bens)) %>%
  select(type) %>% 
  pull()

names(cols_bens) <- mainbens

# Get council areas where hhld pop difference isn't too large

popokcouncils <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey, council) %>%
  summarise(households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  filter(sample >= 50) %>%
  select(-sample) %>%
  spread(survey, households) %>%
  mutate(diff = (HBAI - SHS)/SHS) %>%
  filter(abs(diff) < 0.5) %>%
  select(council) %>%
  pull()

# Get income components that explain difference in total income

incdiff <- tidydata %>%
  filter(type != "total") %>%
  group_by(survey, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt)) %>%
  spread(survey, mean) %>%
  mutate(HBAIshare = percent(HBAI/sum(HBAI), 1),
         reldiff = percent(SHS/HBAI - 1, 1),
         diff = abs(HBAI - SHS),
         diffcontr = percent(diff/sum(diff), 1))

totdiff <- tidydata %>%
  group_by(survey, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt)) %>%
  spread(survey, mean) %>%
  head(1L) %>%
  mutate(diff = percent(1- SHS/HBAI, accuracy = 1)) %>%
  select(diff) %>% 
  pull()

# Get hhld shares by economic status

ecoshares <- tidydata %>%
  filter(type == "total",
         survey == "SHS") %>%
  count(HIHemp, wt = hhwgt) %>%
  mutate(share = percent(n/sum(n), 1)) %>%
  select(-n)

# <!-- # Annex - Survey variables --> ----
#   
#   <!-- Relevant variable names in SHS and FRS datasets.  -->
#   
#   <!-- * SHS variable list (see SHS income project - SHS Household Variables.xlsx) -->
#   <!-- * [SHS questionnaires](https://www2.gov.scot/Topics/Statistics/16002/PublicationQuestionnaire) -->
#   
#   <!-- SHS variables: -->
#   
#   <!-- * ANNETINC_BROAD -->
#   <!-- * EARNINC -->
#   <!-- * BENINC -->
#   <!-- * MSCINC -->
#   <!-- * LA_GRWT -->
#   
#   <!-- BENINC = sum(BENINC_HIHSP, BENINC_OA1, BENINC_OA2, BENINC_OA3) -->
#   
#   <!-- BENINC_HIHSP = sum(BENINC01, ..., BENINC40) -->
#   
#   <!-- BENINC = sum(BENINC01, BENINC01_OA1, BENINC01_OA2, BENINC01_OA3, ..., BENINC40_OA3)  -->
#   
#   <!--       (1) Universal Credit -->
#   <!--       (2) Housing Benefit -->
#   <!--       (3) Council Tax Reduction -->
#   <!--       (4) Working Tax Credit -->
#   <!--       (5) Child Tax Credit -->
#   <!--       (6) Income Support -->
#   <!--       (7) Jobseeker’s Allowance -->
#   <!--       (8) Employment and Support Allowance -->
#   <!--       (9) Carer’s Allowance -->
#   <!--       (10) Child Benefit -->
#   <!--       (11) Guardian’s Allowance -->
#   <!--       (12) Maternity Allowance -->
#   <!--       (13) Statutory Maternity/Paternity pay, Statutory Adoption Pay -->
#   <!--       (14) Statutory sick pay -->
#   <!--       (15) Personal Independence Payments -->
#   <!--       (16) Disability Living Allowance -->
#   <!--       (17) Attendance allowance -->
#   <!--       (18) Severe disablement allowance -->
#   <!--       (19) Incapacity benefit -->
#   <!--       (20) Industrial Injuries Disablement Benefit -->
#   <!--       (21) Pension Credit -->
#   <!--       (22) State Retirement Pension -->
#   <!--       (23) Widow’s Pension, Bereavement Allowance, or Widowed Parent’s Allowance -->
#   <!--       (24) Armed Forces Compensation Scheme -->
#   <!--       (25) War Widow’s/Widower’s Pension -->
#   <!--       (26) Funeral Expenses Payment -->
#   <!--       (27) Sure Start Maternity Grant -->
#   <!--       (28) Best Start Grant -->
#   <!--       (29) Discretionary Housing Payment -->
#   <!--       (30) Loan or grant from DWP -->
#   <!--       (31) Loan or grant from Local Authority -->
#   <!--       (32) Winter Fuel Payments -->
#   <!--       (33) Cold Weather Payments -->
#   <!--       (34) Extended payment of Housing Benefit -->
#   <!--       (35) Bereavement Payment -->
#   <!--       (36) Return to Work Payment -->
#   <!--       (37) Community Care Grant from the Scottish Welfare Fund -->
#   <!--       (38) Crisis Grant from the Scottish Welfare Fund -->
#   <!--       (39) Budgeting Loan from Social Fund/Budgeting Advances from Universal Credit -->
#   <!--       (40) Healthy Start Vouchers -->
#   
#   <!-- MSCINC = sum(MSCINC_HIHSP, MSCINC_OA1, MSCINC_OA2, MSCINC_OA3) for MSCINC01-MSCINC10) -->
#   
#   <!--       (1) Income from Occupational/employer (non-State) pension(s) -->
#   <!--       (2) Income from Benefit from annuity, trust or covenant -->
#   <!--       (3) Income from Maintenance payments -->
#   <!--       (4) Income from Rent from property or subletting, including boarders -->
#   <!--       (5) Income from Dig money from other household members -->
#   <!--       (6) Income from Benefit from accident/sickness scheme etc -->
#   <!--       (7) Income from Investment income (eg Dividends from shares/interest from savings) -->
#   <!--       (8) Income from Student loan -->
#   <!--       (9) Income from Grant -->
#   <!--       (10) Income from Regular non-work income, from any other source (please specify) -->
#   
#   <!-- HBAI variables: -->
#   
#   <!-- * GVTREGN -->
#   <!-- * gs_newhh -->
#   <!-- * ehcost -->
#   <!-- * entinchh -->
#   <!-- * ebeninhh -->
#   <!-- * enternhh -->
#   <!-- * hntinvhh -->
#   <!-- * hntocchh -->
#   <!-- * epribnhh -->
#   <!-- * emiscih -->
#   
#   <!-- * HOUSEHOL	URINDS	HOL_327X	Urban and Rural Indicators for Scotland	 -->
#   
#   <!--       1	Large Urban Area -->
#   <!--     	2	Other Urban Area -->
#   <!--     	3	Accessible Small Town -->
#   <!--     	4	Remote Small Town -->
#   <!--     	5	Very Remote Small Town -->
#   <!--     	6	Accessible Rural -->
#   <!--     	7	Remote Rural -->
#   <!--     	8	Very Remote Rural -->
#   
#   