
# Create combined tidy SHS and HBAI dataset

# Combine SHS and HBAI ----

tidydata <- rbind(tidyhbai, tidyshs) %>%
  mutate(survey = factor(survey, ordered = TRUE),
         n = 1)

# recode some group characteristics
tidydata$HIHemp <- decode(tidydata$HIHemp,
                          search = empstatcodes,
                          replace = empstatnames,
                          default = "unknown")

tidydata$type <- factor(tidydata$type, levels = inctypes)

tidydata$urbrur <- decode(tidydata$urbrur,
                          search = urbrurcodes,
                          replace = urbrurclasses)

tidydata$urbrur <- factor(tidydata$urbrur, levels = urbrurclasses)

tidydata$finman <- factor(tidydata$finman, levels = finmannames, ordered = TRUE)

# Decile points ----

decilepoints <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey) %>%
  summarise(x = list(enframe(wtd.quantile(amount,
                                        probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                        weights = ppwgt)))) %>%
  unnest(x) %>%
  mutate(name = str_c("dec", str_sub(name, 1L, 2L))) %>%
  spread(name, value)

deciles <- tidydata %>%
  filter(type == "total") %>%
  select(ID, survey, amount) %>%
  left_join(decilepoints, by = "survey") %>%
  mutate(decile = case_when(amount < dec10 ~ 1,
                            amount < dec20 ~ 2,
                            amount < dec30 ~ 3,
                            amount < dec40 ~ 4,
                            amount < dec50 ~ 5,
                            amount < dec60 ~ 6,
                            amount < dec70 ~ 7,
                            amount < dec80 ~ 8,
                            amount < dec90 ~ 9,
                            TRUE ~ 10)) %>%
  select(ID, survey, decile)

tidydata <- tidydata %>%
  left_join(deciles, by = c("survey", "ID"))

tidydata$quintile <- decode(tidydata$decile,
                            search = c(seq(1,10)),
                            replace = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
                            default = NA)

# Medians ----
SHSmedian <- filter(tidydata, type == "total", survey == "SHS") %>%
  mutate(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt)) %>%
  select(median) %>% tail(1L) %>% pull()


HBAImedian <- filter(tidydata, type == "total", survey == "HBAI") %>%
  mutate(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt)) %>%
  select(median) %>% tail(1L) %>% pull()

# Poverty flags ----
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

# Combine benefits datasets ----

tidybens <- rbind(tidyhbaibens, tidyshsbens) %>%
  mutate(survey = factor(survey, ordered = TRUE))

tidybens$HIHemp <- decode(tidybens$HIHemp,
                             search = empstatcodes,
                             replace = empstatnames,
                             default = "unknown")

# Deciles ----
tidydeciles <- tidydata %>%
  filter(type == "total") %>%
  select(survey, ID, decile)

tidybens <- tidybens %>%
  left_join(tidydeciles, by = c("survey", "ID"))

# Admin benefit data ----
admin <- read_excel("ext/docs/StatXplore benefit receipt.xlsx", sheet = "Weekly 201819") %>%
  mutate(amount = 1000000*amount,
         survey = "Admin",
         type = str_trunc(type, 30))

# Aggregated benefits dataset ----
tidybens_agg <- tidybens %>%
  group_by(survey, type) %>%
  summarise(amount = sum(amount*hhwgt*equ)) %>%
  ungroup() %>%
  rbind(admin) %>%
  mutate(survey = factor(survey, levels = surveys, ordered = TRUE),
         type = factor(type),
         type = fct_reorder2(type, survey, desc(amount))) %>%
  arrange(desc(type), survey)

# Summary stats ----
medianearnings <- tidydata %>%
  filter(type == "earn") %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt)) %>%
  select(median) %>% pull()

names(medianearnings) <- c("HBAI", "SHS")

# Councils with a large enough sample size (households with earnings > 0) ----
councilsn50 <- filter(tidydata,
                      type == "total",
                      survey == "HBAI") %>%
  group_by(council) %>%
  count() %>%
  filter(n >= 50) %>%
  select(council) %>% pull()

# Main benefits ----
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

# Council areas where hhld pop difference isn't too large ----
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

# Income components that explain difference in total income ----
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
  mutate(diff = percent(1 - SHS/HBAI, accuracy = 1)) %>%
  select(diff) %>%
  pull()

bencontr <- tidybens_agg %>%
  mutate(type = as.character(type),
         type = ifelse(type %in% str_trunc(disbens, 30), "Disability", type)) %>%
  group_by(survey, type) %>%
  summarise(amount = sum(amount)) %>%
  ungroup() %>%
  group_by(survey) %>%
  gather(key, value, -survey, -type) %>%
  unite(measure, c(key, survey)) %>%
  spread(measure, value) %>%
  mutate(diff = abs(amount_HBAI - amount_SHS),
         HBAI_share = percent(amount_HBAI/sum(amount_HBAI, na.rm = TRUE), 1),
         SHS_share = percent(amount_SHS/sum(amount_SHS, na.rm = TRUE), 1),
         HBAI_capt = percent(amount_HBAI/amount_Admin, 1),
         SHS_capt = percent(amount_SHS/amount_Admin, 1),
         totdiff = sum(diff, na.rm = TRUE),
         diffcontr = ifelse(is.na(diff), NA, percent(diff/totdiff, 1))) %>%
  arrange(desc(diff)) %>%
  filter(!is.na(diffcontr))

# Hhld shares by economic status ----
ecoshares <- tidydata %>%
  filter(type == "total",
         survey == "SHS") %>%
  count(HIHemp, wt = hhwgt) %>%
  mutate(share = percent(n/sum(n), 1)) %>%
  select(-n)
