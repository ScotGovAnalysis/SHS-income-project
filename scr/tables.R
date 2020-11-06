
# table 1 ----

# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = NULL,
                            data = filter(tidydata,
                                          type == "earn",
                                          survey == "HBAI") %>%
                              mutate(weight = ppwgt),
                            conf = 0.95,
                            R = 100,
                            normal = TRUE,
                            basic = FALSE,
                            percentile = FALSE,
                            digits = 3)

# SHS bootstrap mechanism includes council strata
SHS_CI <- groupwiseMedian3(var = "amount",
                           group = NULL,
                           data = filter(tidydata,
                                         type == "earn",
                                         survey == "SHS") %>%
                             mutate(weight = ppwgt),
                           conf = 0.95,
                           R = 100,
                           normal = TRUE,
                           basic = FALSE,
                           percentile = FALSE,
                           digits = 3)

SHS_CI$survey <- "SHS"
HBAI_CI$survey <- "HBAI"

range <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(CI = str_c("£", Normal.lower, "-£", Normal.upper),
         survey = ordered(survey)) %>%
  select(survey, CI)

tb01 <- tidydata %>%
  filter(type == "earn") %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000)) %>%
  left_join(range, by = "survey") %>%
  select(survey, median, CI, everything())

# table 2 ----

# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = NULL,
                            data = filter(tidydata,
                                          type == "earn",
                                          survey == "HBAI",
                                          amount != 0) %>%
                              mutate(weight = ppwgt),
                            conf = 0.95,
                            R = 100,
                            normal = TRUE,
                            basic = FALSE,
                            percentile = FALSE,
                            digits = 3)

# SHS bootstrap mechanism includes council strata
SHS_CI <- groupwiseMedian3(var = "amount",
                           group = NULL,
                           data = filter(tidydata,
                                         type == "earn",
                                         survey == "SHS",
                                         amount != 0) %>%
                             mutate(weight = ppwgt),
                           conf = 0.95,
                           R = 100,
                           normal = TRUE,
                           basic = FALSE,
                           percentile = FALSE,
                           digits = 3)

SHS_CI$survey <- "SHS"
HBAI_CI$survey <- "HBAI"

range <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(CI = str_c("£", Normal.lower, "-£", Normal.upper),
         survey = ordered(survey)) %>%
  select(survey, CI)

tb02 <- tidydata %>%
  filter(type == "earn",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000)) %>%
  left_join(range, by = "survey") %>%
  select(survey, median, CI, everything())

# table 3 ----

tb03 <- tidydata %>%
  filter(type == "earn") %>%
  group_by(survey, HIHemp) %>%
  summarise(median = wtd.quantile(amount,
                                  probs = 0.5,
                                  weights = ppwgt),
            mean = wtd.mean(amount,
                            weights = hhwgt),
            total = sum(amount * equ * hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(HIHemp = fct_reorder2(HIHemp, survey, desc(total)),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, prefix = "£", accuracy = 1)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£", accuracy = 1)),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, accuracy = 10000))) %>%
  arrange(desc(HIHemp))

# table 4 ----

tb04 <- tidydata %>%
  filter(type == "earn",
         amount != 0) %>%
  group_by(survey, HIHemp) %>%
  summarise(median = wtd.quantile(amount,
                                  probs = 0.5,
                                  weights = ppwgt),
            mean = wtd.mean(amount,
                            weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(HIHemp = fct_reorder2(HIHemp, survey, desc(total)),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, prefix = "£", accuracy = 1)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£", accuracy = 1)),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, accuracy = 10000))) %>%
  arrange(desc(HIHemp))

# table 5 ----

tb05 <- tidydata %>%
  filter(type == "earn") %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount,
                                  probs = 0.5,
                                  weights = ppwgt),
            mean = wtd.mean(amount,
                            weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  arrange(hhtype) %>%
  mutate(median =  ifelse(survey == "HBAI" & sample < 50, "..",
                          comma(median, prefix = "£", accuracy = 1)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£", accuracy = 1)),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, accuracy = 10000)))

# table 6 ----

tb06 <- tidydata %>%
  filter(type == "earn",
         amount != 0) %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount,
                                  probs = 0.5,
                                  weights = ppwgt),
            mean = wtd.mean(amount,
                            weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  arrange(hhtype) %>%
  mutate(median =  ifelse(survey == "HBAI" & sample < 50, "..",
                          comma(median, prefix = "£", accuracy = 1)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£", accuracy = 1)),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, accuracy = 10000)))


# table 7 ----

tb07 <- tidydata %>%
  filter(type == "earn",
         council %in% popokcouncils) %>%
  group_by(survey, council) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  arrange(council, survey) %>%
  mutate(median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1 , prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)))




# table 8 ----

tb08 <- tidydata %>%
  filter(type == "earn",
         amount != 0,
         council %in% popokcouncils) %>%
  group_by(survey, council) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  arrange(council, survey) %>%
  mutate(median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1 , prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)))

# table 9 ----

all <- tidydata %>%
  filter(type == "ben") %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "All") %>%
  select(group, everything())

waa <- tidydata %>%
  filter(type == "ben",
         pnwgt == 0,
         chwgt == 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "Working-age households with no children") %>%
  select(group, everything())

waakids <- tidydata %>%
  filter(type == "ben",
         pnwgt == 0,
         chwgt > 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "Working-age households with children") %>%
  select(group, everything())

pns <- tidydata %>%
  filter(type == "ben",
         pnwgt > 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "Pensioner households") %>%
  select(group, everything())

tb09 <- rbind(all, waa, waakids, pns)

# table 10 ----

all <- tidydata %>%
  filter(type == "ben",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "All") %>%
  select(group, everything())

waa <- tidydata %>%
  filter(type == "ben",
         pnwgt == 0,
         chwgt == 0,
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "Working-age households with no children") %>%
  select(group, everything())

waakids <- tidydata %>%
  filter(type == "ben",
         pnwgt == 0,
         chwgt > 0,
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "Working-age households with children") %>%
  select(group, everything())

pns <- tidydata %>%
  filter(type == "ben",
         pnwgt > 0,
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*equ*hhwgt),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(median =  comma(median, prefix = "£", accuracy = 1),
         mean = comma(mean, prefix = "£", accuracy = 1),
         total = comma(total, scale = 1E-6, prefix = "£", suffix = " million"),
         households = comma(households, accuracy = 10000),
         group = "Pensioner households") %>%
  select(group, everything())

tb10 <- rbind(all, waa, waakids, pns)

# table 11 ----

tb11 <- tidydata %>%
  filter(type == "ben") %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = sum(n)) %>%
  ungroup() %>%
  mutate(total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         hhtype = factor(hhtype, levels = hhtypenames),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, prefix = "£")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£"))) %>%
  arrange(hhtype, survey)





# table 12 ----

tb12 <- tidydata %>%
  filter(type == "ben",
         amount != 0) %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = sum(n)) %>%
  ungroup() %>%
  mutate(total = ifelse(survey == "HBAI" & sample < 50, "..", comma(total, scale = 1E-6, prefix = "£", suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..", comma(households, 10000)),
         hhtype = factor(hhtype, levels = hhtypenames),
         median = ifelse(survey == "HBAI" & sample < 50, "..", comma(median, prefix = "£")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..", comma(mean, prefix = "£"))) %>%
  arrange(hhtype, survey)



# table 13 ----

tb13 <- tidydata %>%
  filter(type == "ben") %>%
  group_by(survey, HIHemp) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(HIHemp = factor(HIHemp, levels = empstatnames),
         total = ifelse(survey == "HBAI" & sample < 100, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 50, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 100, "..",
                       comma(mean, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 100, "..",
                         comma(median, prefix = "£"))) %>%
  arrange(HIHemp, survey)

# table 14 ----

tb14 <- tidydata %>%
  filter(type == "ben",
         amount != 0) %>%
  group_by(survey, HIHemp) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(HIHemp = factor(HIHemp, levels = empstatnames),
         total = ifelse(survey == "HBAI" & sample < 100, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 50, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 100, "..",
                       comma(mean, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 100, "..",
                         comma(median, prefix = "£"))) %>%
  arrange(HIHemp, survey)


# table 15 ----

tb15 <- tidydata %>%
  filter(type == "ben",
         council %in% popokcouncils) %>%
  group_by(survey, council) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  arrange(council, survey) %>%
  mutate(median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1 , prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)))


# table 16 ----

tb16 <- tidydata %>%
  filter(type == "ben",
         amount != 0,
         council %in% popokcouncils) %>%
  group_by(survey, council) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  arrange(council, survey) %>%
  mutate(median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1 , prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)))

# table 17 ----

tb17 <- tidybens_agg %>%
  group_by(survey) %>%
  summarise(amount = sum(amount),
            type = "total") %>%
  spread(survey, amount) %>%
  mutate(capt_HBAI = percent(HBAI/Admin, 1),
         capt_SHS = percent(SHS/Admin, 1)) %>%
  mutate_at(c("Admin", "HBAI", "SHS"), comma_format(scale = 1E-6, accuracy = 1,
                                                    prefix = "£",
                                                    suffix = " million"))
# table 18 ----

tb18 <- tidybens_agg %>%
  mutate(type = as.character(type),
         type = ifelse(type %in% str_trunc(disbens, 30), "Disability", type)) %>%
  group_by(survey, type) %>%
  summarise(amount = sum(amount)) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(totshare = amount/sum(amount)) %>%
  gather(key, value, -survey, -type) %>%
  unite(measure, c(key, survey)) %>%
  spread(measure, value) %>%
  arrange(desc(amount_Admin)) %>%
  mutate(capt_HBAI = percent(amount_HBAI/amount_Admin, 1),
         capt_SHS = percent(amount_SHS/amount_Admin, 1),
         diff = abs(amount_HBAI - amount_SHS),
         totdiff = sum(diff, na.rm = TRUE),
         diffcontr = percent(diff/totdiff, 1)) %>%
  select(-totdiff, -diff) %>%
  mutate_at(c("amount_Admin", "amount_HBAI", "amount_SHS"),
            comma_format(scale = 1E-6, accuracy = 1, prefix = "£",
                         suffix = " million")) %>%
  mutate_at(c("totshare_Admin", "totshare_HBAI", "totshare_SHS"),
            percent_format(1)) %>%
  head(12L)

# table 19 ----

tb19 <- tidybens %>%
  mutate(type = as.character(type),
         type = ifelse(type %in% str_trunc(disbens, 30), "Disability", type)) %>%
  group_by(survey, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            sample = n()) %>%
  ungroup() %>%
  filter(sample >= 50) %>%
  select(-sample) %>%
  mutate(type = factor(type),
         type = fct_reorder2(type, survey, mean)) %>%
  arrange(type) %>%
  spread(survey, mean) %>%
  mutate_at(c("HBAI", "SHS"), comma_format(prefix = "£", 0.01)) %>%
  head(12L)

# table 20 ----

tb20 <- tidybens %>%
  mutate(type = as.character(type),
         type = ifelse(type %in% str_trunc(disbens, 30), "Disability", type)) %>%
  filter(amount != 0) %>%
  group_by(survey, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            sample = n()) %>%
  ungroup() %>%
  filter(sample >= 50) %>%
  select(-sample) %>%
  mutate(type = factor(type),
         type = fct_reorder2(type, desc(survey), mean)) %>%
  arrange(type) %>%
  spread(survey, mean) %>%
  mutate(HBAI = ifelse(is.na(HBAI), "..", comma(HBAI, prefix = "£")),
         SHS = ifelse(is.na(SHS), "..", comma(SHS, prefix = "£"))) %>%
  head(20L)

# table 21 ----

tb21 <- tidybens %>%
  filter(type %in% str_trunc(disbens, 30),
         amount != 0) %>%
  group_by(survey, type) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(type = factor(type),
         type = fct_reorder(type, total),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, prefix = "£")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£")),
         households = ifelse(sample < 100 & survey == "HBAI", "..",
                             comma(households, 10000))) %>%
  arrange(desc(type))

# table 22 ----

tb22 <- tidybens %>%
  filter(type %in% disbens,
         survey != "Admin",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, prefix = "£")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)))

# table 23 ----

tb23 <- tidybens %>%
  filter(type %in% disbens,
         survey != "Admin",
         amount != 0) %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(hhtype = fct_reorder2(hhtype, survey, total),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, prefix = "£")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000))) %>%
  arrange(hhtype)

# table 24 ----

tb24 <- tidybens %>%
  filter(type %in% disbens,
         survey != "Admin",
         amount != 0) %>%
  group_by(survey, HIHemp) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(HIHemp = fct_reorder2(HIHemp, survey, total),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, prefix = "£")),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000))) %>%
  arrange(HIHemp)

# table 25 ----

tb25 <- tidybens %>%
  filter(type == "Tax Credits",
         amount != 0,
         HIHemp %in% c("Part-time Employee", "Full-time Employee",
                       "Self-Employed")) %>%
  group_by(survey, HIHemp) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million"))) %>%
  arrange(HIHemp)

# table 26 ----

tb26 <- tidybens %>%
  filter(type == "Housing Benefit",
         amount != 0) %>%
  group_by(survey, tenure) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  filter(sample > 30) %>%
  mutate(tenure = factor(tenure),
         tenure = fct_reorder2(tenure, survey, desc(total)),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million"))) %>%
  arrange(desc(tenure))

# table 27 ----

tb27 <- tidybens %>%
  filter(type == "Child Benefit",
         amount != 0,
         chwgt > 0) %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  filter(sample > 30) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million"))) %>%
  arrange(desc(hhtype))

# table 28 ----

tb28 <- tidybens %>%
  filter(type == "Universal Credit",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  filter(sample > 30) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")))

# table 29 ----

tb29 <- tidybens %>%
  filter(type == "Pension Credit",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  filter(sample > 30) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")))

# table 30 ----

tb30 <- tidydata %>%
  filter(hhtype == "Single adult",
         type == "ben") %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  filter(sample > 30) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")))

# table 31 ----

tb31 <- tidydata %>%
  filter(hhtype == "Single adult",
         type == "ben",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  filter(sample > 30) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")))

# table 32 ----

tb32 <- tidybens %>%
  mutate(type = ifelse(type %in% str_trunc(disbens, 30), "Disability", type)) %>%
  filter(hhtype == "Single adult",
         amount != 0) %>%
  group_by(survey, type) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(type = factor(type),
         type = fct_reorder2(type, survey, desc(total)),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million"))) %>%
  filter(sample > 30) %>%
  arrange(desc(type)) %>%
  head(6L)

# table 32 ----

tb32 <- tidydata %>%
  filter(type == "inv") %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")))

# table 33 ----

tb33 <- tidydata %>%
  filter(type == "inv",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")))

# table 34 ----

tb34 <- tidydata %>%
  filter(hhtype %in% c("Single pensioner", "Two pensioners",
                       "One adult, one pensioner"),
         amount != 0,
         type %in% c("earn", "ben", "occ", "inv")) %>%
  group_by(survey, type, hhtype) %>%
  summarise(households = sum(hhwgt),
            sample = n()) %>%
  filter(sample >= 100) %>%
  select(hhtype, type, households, survey) %>%
  spread(survey, households) %>%
  mutate(HBAI = ifelse(is.na(HBAI), "..", comma(HBAI, 10000)),
         SHS = ifelse(is.na(SHS), "..", comma(SHS, 10000))) %>%
  arrange(desc(hhtype))

# table 35 ----

tb35 <- tidybens %>%
  filter(type == "State Pension",
         pnwgt > 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         hhtype = "All pensioner households") %>%
  select(survey, hhtype, everything())

# table 36 ----

tb36 <- tidybens %>%
  filter(type == "State Pension",
         pnwgt > 0) %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(hhtype = fct_reorder2(hhtype, survey, desc(total)),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million"))) %>%
  arrange(desc(hhtype))

# table 37 ----

tb37 <- tidybens %>%
  filter(type == "State Pension",
         pnwgt > 0,
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         hhtype = "All pensioner households") %>%
  select(survey, hhtype, everything())

# table 38 ----

tb38 <- tidybens %>%
  filter(type == "State Pension",
         pnwgt > 0,
         amount != 0) %>%
  group_by(survey, hhtype) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  mutate(hhtype = fct_reorder2(hhtype, survey, desc(total)),
         households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million"))) %>%
  arrange(desc(hhtype))

# table 39 ----

pns <- tidydata %>%
  filter(type == "occ",
         pnwgt > 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6,
                              prefix = "£",
                              suffix = " million")),
         group = "Pensioner households") %>%
  select(group, everything())

waa <- tidydata %>%
  filter(type == "occ",
         pnwgt == 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total,
                              scale = 1E-6,
                              prefix = "£",
                              suffix = " million")),
         group = "Working-age households") %>%
  select(group, everything())

all <- tidydata %>%
  filter(type == "occ") %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total,
                              scale = 1E-6,
                              prefix = "£",
                              suffix = " million")),
         group = "All households") %>%
  select(group, everything())

tb39 <- rbind(all, pns, waa)

# table 40 ----

pns <- tidydata %>%
  filter(type == "occ",
         pnwgt > 0,
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total,
                              scale = 1E-6,
                              prefix = "£",
                              suffix = " million")),
         group = "Pensioner households") %>%
  select(group, everything())

waa <- tidydata %>%
  filter(type == "occ",
         pnwgt == 0,
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total,
                              scale = 1E-6,
                              prefix = "£",
                              suffix = " million")),
         group = "Working-age households") %>%
  select(group, everything())

all <- tidydata %>%
  filter(type == "occ",
         amount != 0) %>%
  group_by(survey) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100, "..",
                             comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50, "..",
                       comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50, "..",
                         comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50, "..",
                        comma(total, scale = 1E-6, prefix = "£",
                              suffix = " million")),
         group = "All households") %>%
  select(group, everything())

tb40 <- rbind(all, pns, waa)

# table 41 ----

tb41 <- tidydata %>%
  filter(HIHemp == "Self-Employed") %>%
  group_by(survey, type) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt),
            total = sum(amount*hhwgt*equ),
            households = sum(hhwgt),
            sample = n()) %>%
  mutate(households = ifelse(survey == "HBAI" & sample < 100 | survey == "SHS"
                             & sample < 30, "..", comma(households, 10000)),
         mean = ifelse(survey == "HBAI" & sample < 50 | survey == "SHS"
                       & sample < 30, "..", comma(mean, 1, prefix = "£")),
         median = ifelse(survey == "HBAI" & sample < 50 | survey == "SHS"
                         & sample < 30, "..", comma(median, 1, prefix = "£")),
         total = ifelse(survey == "HBAI" & sample < 50 | survey == "SHS"
                        & sample < 30, "..", comma(total, scale = 1E-6,
                                                   prefix = "£",
                                                   suffix = " million"))) %>%
  arrange(type)

# table 42 ----

tb42 <- tidydata %>%
  group_by(survey, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt)) %>%
  spread(survey, mean) %>%
  mutate(diff = HBAI - SHS,
         absdiff = abs(diff),
         totdiff = sum(absdiff[2:8]),
         diffcontr = ifelse(type == "total", 1, abs(diff)/totdiff)) %>%
  mutate(HBAI = comma(HBAI, prefix = "£"),
         SHS = comma(SHS, prefix = "£"),
         diff = comma(diff, prefix = "£"),
         diffcontr = percent(diffcontr, 1)) %>%
  select(type, HBAI, SHS, diff, diffcontr) %>%
  mutate(type = factor(type, levels = inctypes))

# table 43 ----

HBAI_CI_all <- groupwiseMedian2(var = "amount",
                                data = filter(tidydata,
                                              type == "total",
                                              survey == "HBAI") %>%
                                  mutate(weight = ppwgt),
                                conf = 0.95,
                                R = 100,
                                normal = TRUE,
                                basic = FALSE,
                                percentile = FALSE,
                                digits = 3) %>%
  mutate(survey = "HBAI") %>%
  select(survey, Median, n, Conf.level, Normal.lower, Normal.upper)

# SHS bootstrap mechanism includes council strata
SHS_CI_all <- groupwiseMedian3(var = "amount",
                               data = filter(tidydata,
                                             type == "total",
                                             survey == "SHS") %>%
                                 mutate(weight = ppwgt),
                               conf = 0.95,
                               R = 100,
                               normal = TRUE,
                               basic = FALSE,
                               percentile = FALSE,
                               digits = 3) %>%
  mutate(survey = "SHS") %>%
  select(survey, Median, Normal.lower, Normal.upper, Conf.level,  n)

tb43 <- rbind(HBAI_CI_all, SHS_CI_all) %>%
  select(survey, Median, Normal.lower, Normal.upper, Conf.level,  n) %>%
  mutate_at(c("Median", "Normal.lower", "Normal.upper"),
            comma_format(1, prefix = "£"))

# table 44 ----

SHS_CI_ch <- confint(svytotal(~low60bhc, SHSdesign_ch))
SHS_CI_wa <- confint(svytotal(~low60bhc, SHSdesign_wa))
SHS_CI_pn <- confint(svytotal(~low60bhc, SHSdesign_pn))
SHS_CI_pp <- confint(svytotal(~low60bhc, SHSdesign_pp))

ch <- tidypovdata %>%
  filter(low60bhc == 1) %>%
  group_by(survey) %>%
  summarise(children = sum(chwgt)) %>%
  spread(survey, children) %>%
  mutate(group = "Children",
         HBAI = comma(HBAI, 10000),
         SHS = str_c(comma(SHS, 10000),
                     " (",
                     comma(SHS_CI_ch[1], 10000),
                     "-",
                     comma(SHS_CI_ch[2], 10000),
                     ")"))

wa <- tidypovdata %>%
  filter(low60bhc == 1) %>%
  group_by(survey) %>%
  summarise(adults = sum(wawgt)) %>%
  spread(survey, adults) %>%
  mutate(group = "Working-age adults",
         HBAI = comma(HBAI, 10000),
         SHS = str_c(comma(SHS, 10000),
                     " (",
                     comma(SHS_CI_wa[1], 10000),
                     "-",
                     comma(SHS_CI_wa[2], 10000),
                     ")"))

pn <- tidypovdata %>%
  filter(low60bhc == 1) %>%
  group_by(survey) %>%
  summarise(pensioners = sum(pnwgt)) %>%
  spread(survey, pensioners) %>%
  mutate(group = "Pensioners",
         HBAI = comma(HBAI, 10000),
         SHS = str_c(comma(SHS, 10000),
                     " (",
                     comma(SHS_CI_pn[1], 10000),
                     "-",
                     comma(SHS_CI_pn[2], 10000),
                     ")"))

pp <- tidypovdata %>%
  filter(low60bhc == 1) %>%
  group_by(survey) %>%
  summarise(total = sum(ppwgt)) %>%
  spread(survey, total) %>%
  mutate(group = "Total",
         HBAI = comma(HBAI, 10000),
         SHS = str_c(comma(SHS, 10000),
                     " (",
                     comma(SHS_CI_pp[1], 10000),
                     "-",
                     comma(SHS_CI_pp[2], 10000),
                     ")"))

tb44 <- rbind(ch, wa, pn, pp) %>%
  select(group, HBAI, SHS)

# table 45 ----

ch <- svymean(~low60bhc, SHSdesign_ch)[[1]]
ch_lower <- confint(svymean(~low60bhc, SHSdesign_ch))[[1]]
ch_upper <- confint(svymean(~low60bhc, SHSdesign_ch))[[2]]

wa <- svymean(~low60bhc, SHSdesign_wa)[[1]]
wa_lower <- confint(svymean(~low60bhc, SHSdesign_wa))[[1]]
wa_upper <- confint(svymean(~low60bhc, SHSdesign_wa))[[2]]

pp <- svymean(~low60bhc, SHSdesign_chwa)[[1]]
pp_lower <- confint(svymean(~low60bhc, SHSdesign_chwa))[[1]]
pp_upper <- confint(svymean(~low60bhc, SHSdesign_chwa))[[2]]

SHS <- tribble(
  ~group, ~rate, ~lower, ~upper,
  "Children", ch, ch_lower, ch_upper,
  "Working-age adults", wa, wa_lower, wa_upper,
  "Total", pp, pp_lower, pp_upper)

SHS <- SHS %>%
  mutate(SHS = str_c(percent(rate, 1), " (", percent(lower, 1), "-",
                     percent(upper, 1), ")" )) %>%
  select(group, SHS)

tb45 <- tidypovdata %>%
  filter(survey == "HBAI") %>%
  group_by(low60bhc) %>%
  summarise(children = sum(chwgt),
            adults = sum(wawgt),
            total = sum(ppwgt)) %>%
  mutate(chrate = children/sum(children),
         warate = adults/sum(adults),
         pprate = total/sum(total))  %>%
  filter(low60bhc == 1) %>%
  select(chrate, warate, pprate) %>%
  gather(group, HBAI) %>%
  mutate(group = ifelse(group == "chrate", "Children",
                        ifelse(group == "warate", "Working-age adults",
                               "Total")),
         HBAI = percent(HBAI, 1)) %>%
  left_join(SHS, by = "group")

# table 46 ----

tb46 <- tidypovdata %>%
  filter(survey != "Admin") %>%
  group_by(survey, low60bhc) %>%
  summarise(children = sum(chwgt),
            adults = sum(wawgt),
            total = sum(chwgt + wawgt)) %>%
  filter(low60bhc == 1) %>%
  mutate(ch = children/total,
         ad = adults/total,
         all = 1) %>%
  select(survey, ch, ad, all) %>%
  gather(group, comp, -survey) %>%
  spread(survey, comp) %>%
  arrange(HBAI) %>%
  mutate(group = ifelse(group == "ch", "Children",
                        ifelse(group == "ad", "Working-age adults", "Total")),
         HBAI = percent(HBAI, 1),
         SHS = percent(SHS, 1))

# table 47 ----

allcilif <- cilif_council %>%
  summarise(CiLIF = sum(Cilif))

SHS_CI <- confint(svytotal(~low60bhc, SHSdesign_ch))

tb47 <- tidypovdata %>%
  filter(low60bhc == 1) %>%
  group_by(survey) %>%
  summarise(SHS = sum(chwgt)) %>%
  spread(survey, SHS) %>%
  cbind(allcilif) %>%
  mutate(CiLIF = comma(CiLIF),
         HBAI = comma(HBAI, 10000),
         SHS = str_c(comma(SHS, 10000),
                     " (",
                     comma(SHS_CI[1], 10000),
                     "-",
                     comma(SHS_CI[2], 10000),
                     ")"))

# table 48 ----

tb48 <- svyby(~low60bhc, ~hhtype, SHSdesign_ch, svytotal, vartype = c("ci","ci")) %>%
  filter(hhtype %in% c("Single adult with children", "Two adults with children")) %>%
  left_join(cilif_hhtype, by = "hhtype") %>%
  mutate(SHS = str_c(comma(low60bhc, 10000),
                     " (",
                     comma(ci_l, 10000),
                     "-",
                     comma(ci_u, 10000),
                     ")"),
         CiLIF = comma(Cilif)) %>%
  select(hhtype, SHS, CiLIF)

# table 49 ----

tb49 <- svyby(~low60bhc, ~work, SHSdesign_ch, svytotal,
              vartype = c("ci","ci")) %>%
  left_join(cilif_work, by = "work") %>%
  mutate(SHS = str_c(comma(low60bhc, 10000),
                     " (",
                     comma(ci_l, 10000),
                     "-",
                     comma(ci_u, 10000),
                     ")"),
         CiLIF = comma(Cilif)) %>%
  select(work, SHS, CiLIF)

# table 50 ----

cilif_urbrur <- cilif_urbrur %>%
  mutate(urbrur = ifelse(urbrur %in% c("Accessible Small Towns",
                                       "Remote Small Towns"), "Small Towns",
                         ifelse(urbrur %in% c("Accessible Rural",
                                              "Remote Rural"), "Rural",
                                urbrur))) %>%
  group_by(urbrur) %>%
  summarise(Cilif = sum(Cilif))

tb50 <- svyby(~low60bhc, ~urbrur, SHSdesign_ch, svytotal,
              vartype = c("ci","ci")) %>%
  left_join(cilif_urbrur, by = "urbrur") %>%
  mutate(SHS = str_c(comma(low60bhc, 10000),
                     " (",
                     comma(ci_l, 10000),
                     "-",
                     comma(ci_u, 10000),
                     ")"),
         CiLIF = comma(Cilif)) %>%
  select(urbrur, SHS, CiLIF)

















