
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

# Check for missing data ----

# View(miss_var_summary(tidyhbai))
# View(miss_var_summary(tidyshs))
# View(miss_var_summary(tidydata))