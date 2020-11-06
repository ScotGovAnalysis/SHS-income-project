
# Change some theme elements (charts)
theme_update(legend.position = "top",
             legend.title = element_blank(),
             panel.background = element_blank(),
             panel.grid.major.x = element_line(colour = "grey95"),
             panel.grid.minor.x = element_line(colour = "grey95"),
             panel.grid.major.y = element_line(colour = "grey95"),
             axis.ticks = element_blank(),
             text = element_text(size = 16))

# chart 1 ----

ch01 <- tidydata %>%
  filter(type == "earn") %>%
  group_by(survey) %>%
  mutate(weight = ppwgt/sum(ppwgt)) %>%
  ggplot(aes(x = amount,
             weight = weight,
             colour = survey, fill = survey)) +
  geom_density(size = 1, alpha = 0.5, bw = 0.02) +
  scale_x_log10(labels = comma_format(prefix = "£"), limits = c(10, 10000)) +
  scale_color_manual(values = cols_survey) +
  scale_fill_manual(values = cols_survey) +
  labs(title = "Net equivalised household earnings density distributions",
       subtitle = "Note log x scale",
       x = "Weekly equivalised net household earnings",
       y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# chart 2 ----

ch02 <- tidydata %>%
  filter(type == "earn") %>%
  group_by(survey) %>%
  ggplot(aes(x = amount, weight = ppwgt, colour = survey, fill = survey )) +
  coord_cartesian(xlim = c(0, 1000)) +
  stat_ecdf(geom = "step", size = 1, alpha = 0.5) +
  scale_x_continuous(labels = comma_format(prefix = "£"),
                     breaks = c(0, 200, 400, 600, 800, 1000)) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = cols_survey) +
  scale_fill_manual(values = cols_survey) +
  labs(title = "Cumulative net equivalised household earnings distributions",
       subtitle = "Note linear x scale",
       x = "Weekly equivalised net household earnings",
       y = "Proportion of people") +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text())

# chart 3 ----

# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "HIHemp",
                            data = filter(tidydata,
                                          type == "earn",
                                          survey == "HBAI",
                                          HIHemp %in% empstatnames[1:3]) %>%
                              mutate(weight = ppwgt),
                            conf = 0.95,
                            R = 100,
                            normal = TRUE,
                            basic = FALSE,
                            percentile = FALSE,
                            digits = 3)

# SHS bootstrap mechanism includes council strata
SHS_CI <- groupwiseMedian3(var = "amount",
                           group = "HIHemp",
                           data = filter(tidydata,
                                         type == "earn",
                                         survey == "SHS",
                                         HIHemp %in% empstatnames[1:3]) %>%
                             mutate(weight = ppwgt),
                           conf = 0.95,
                           R = 100,
                           normal = TRUE,
                           basic = FALSE,
                           percentile = FALSE,
                           digits = 3)

SHS_CI$survey <- "SHS"
HBAI_CI$survey <- "HBAI"

ch03 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(HIHemp = factor(HIHemp),
         HIHemp = fct_reorder2(HIHemp, survey, desc(Median))) %>%
  filter(n >= 50) %>%
  ggplot() +
  geom_point(aes(x = HIHemp,
                 y = Median,
                 colour = survey),
             size = 2) +
  geom_segment(aes(x = HIHemp,
                   xend = HIHemp,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  geom_hline(yintercept = medianearnings["HBAI"],
             colour = cols_survey[1],
             alpha = 0.2,
             size = 1.5) +
  geom_hline(yintercept = medianearnings["SHS"],
             colour = cols_survey[2],
             alpha = 0.2,
             size = 1.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median equivalised household earnings',
       subtitle = "Includes all households; lines show overall medians") +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800)) +
  theme(panel.grid.major.y = element_blank())

# chart 4 ----

# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "hhtype",
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
                           group = "hhtype",
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

ch04 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(hhtype = factor(hhtype),
         hhtype = fct_reorder2(hhtype, survey, desc(Median))) %>%
  filter(n >= 50) %>%
  ggplot() +
  geom_point(aes(x = hhtype,
                 y = Median,
                 colour = survey),
             size = 2) +
  geom_segment(aes(x = hhtype,
                   xend = hhtype,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  geom_hline(yintercept = medianearnings["HBAI"],
             colour = cols_survey[1],
             alpha = 0.2,
             size = 1.5) +
  geom_hline(yintercept = medianearnings["SHS"],
             colour = cols_survey[2],
             alpha = 0.2,
             size = 1.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household earnings',
       subtitle = str_wrap("Includes households with and without earnings; sample >= 50; lines show overall medians",
                           75)) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))


# chart 5 ----

# Earnings by hhld income decile
# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "decile",
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
                           group = "decile",
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

ch05 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(Median = ifelse(n < 50, NA, Median),
         Normal.lower = ifelse(n < 50, NA, Normal.lower),
         Normal.upper = ifelse(n < 50, NA, Normal.upper),
         decile = factor(decile)) %>%
  filter(!is.na(Median)) %>%
  ggplot() +
  geom_point(aes(x = decile, y = Median, colour = survey), size = 2) +
  geom_segment(aes(x = decile,
                   xend = decile,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household earnings by household income decile',
       subtitle = 'Includes all households') +
  scale_y_continuous(labels = comma_format(prefix = "£"), breaks = c(0, 200, 400, 600, 800, 1000, 1200)) +
  coord_flip(ylim = c(-25, 1175))

# chart 6 ----

ch06 <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey, council) %>%
  summarise(households = sum(hhwgt),
            sample = n()) %>%
  ungroup() %>%
  filter(sample >= 50) %>%
  select(-sample) %>%
  spread(survey, households) %>%
  filter(!is.na(HBAI)) %>%
  mutate(diff = (HBAI - SHS)/SHS,
         diffsize = ifelse(abs(diff) < 0.5, "HBAI", "SHS"),
         council = fct_reorder(council, diff)) %>%
  ggplot(aes(x = council, y = diff, fill = diffsize)) +
  geom_col(position = "dodge") +
  annotate("text", x = 18, y = 0.6,
           label = "Difference > 50%",
           hjust = 0, colour = cols_survey[2],
           fontface = "bold") +
  scale_y_continuous(labels = percent_format(1)) +
  scale_fill_manual(values = cols_survey, guide = FALSE) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "Difference in SHS and HBAI household populations by council
       area",
       subtitle = str_wrap("Positive values = HBAI population higher than SHS population; excludes councils with < 50 cases",
                           80))


# chart 7 ----

HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "council",
                            data = filter(tidydata,
                                          type == "earn",
                                          council %in% councilsn50,
                                          council %in% popokcouncils,
                                          survey == "HBAI") %>%
                              mutate(weight = ppwgt),
                            conf = 0.95,
                            R = 100,
                            normal = TRUE,
                            basic = FALSE,
                            percentile = FALSE,
                            digits = 3)

SHS_CI <- groupwiseMedian3(var = "amount",
                           group = "council",
                           data = filter(tidydata,
                                         type == "earn",
                                         council %in% councilsn50,
                                         council %in% popokcouncils,
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

ch07 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(council = fct_reorder2(council, survey, desc(Median))) %>%
  ggplot() +
  geom_point(aes(x = council, y = Median, colour = survey),
             size = 2) +
  geom_segment(aes(x = council,
                   xend = council,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  geom_hline(yintercept = medianearnings["HBAI"],
             colour = cols_survey[1],
             alpha = 0.2,
             size = 1.5) +
  geom_hline(yintercept = medianearnings["SHS"],
             colour = cols_survey[2],
             alpha = 0.2,
             size = 1.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household earnings by council
       area',
       subtitle = str_wrap('Includes all households, councils with >= 50 cases, and councils where household population is off by < 50%; lines show Scotland medians',
                           80)) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-250, 800))

# chart 8 ----

ch08 <- tidybens %>%
  filter(pnwgt == 0,
         chwgt == 0) %>%
  mutate(type = factor(type),
         type = fct_collapse(type, Disability = str_trunc(disbens, 30))) %>%
  group_by(survey, decile, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            total = sum(hhwgt*amount*equ),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey, decile) %>%
  filter(type %in% type[order(-total)[1:5]]) %>%
  ungroup() %>%
  mutate(decile = factor(decile),
         type = fct_reorder(type, mean)) %>%
  ggplot(aes(x = decile, y = mean, fill = type)) +
  geom_col() +
  scale_fill_manual(values = cols_bens) +
  facet_wrap(~survey, nrow = 2) +
  labs(title = "Working-age households with no children - main benefits",
       subtitle = str_wrap("Weekly equivalised mean benefit income of the five largest benefits in each income decile; includes households with and without any benefit income",
                           80),
       x = NULL, y = NULL) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  theme(legend.position = "right",
        panel.grid.major.x = element_blank())

# chart 9 ----

ch09 <- tidybens %>%
  filter(pnwgt == 0,
         chwgt > 0) %>%
  mutate(type = factor(type),
         type = fct_collapse(type, Disability = str_trunc(disbens, 30))) %>%
  group_by(survey, decile, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            total = sum(hhwgt*amount*equ),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey, decile) %>%
  filter(type %in% type[order(-total)[1:5]]) %>%
  ungroup() %>%
  mutate(decile = factor(decile),
         type = fct_reorder(type, total)) %>%
  ggplot(aes(x = decile, y = mean, fill = type)) +
  geom_col() +
  scale_fill_manual(values = cols_bens) +
  facet_wrap(~survey, nrow = 2) +
  labs(title = "Working-age households with children - main benefits",
       subtitle = str_wrap("Weekly equivalised mean benefit income of the five largest benefits in each income decile; includes households with and without any benefit income",
                           80),
       x = NULL, y = NULL) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  theme(legend.position = "right",
        panel.grid.major.x = element_blank())

# chart 10 ----

ch10 <- tidybens %>%
  filter(pnwgt > 0) %>%
  mutate(type = factor(type),
         type = fct_collapse(type, Disability = str_trunc(disbens, 30))) %>%
  group_by(survey, decile, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            total = sum(hhwgt*amount*equ),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey, decile) %>%
  filter(type %in% type[order(-total)[1:5]]) %>%
  ungroup() %>%
  mutate(decile = factor(decile),
         type = fct_reorder(type, total)) %>%
  ggplot(aes(x = decile, y = mean, fill = type)) +
  geom_col() +
  scale_fill_manual(values = cols_bens) +
  facet_wrap(~survey, nrow = 2) +
  labs(title = "Pensioner households - main benefits",
       subtitle = str_wrap("Weekly equivalised mean benefit income of the five largest benefits in each income decile; includes households with and without any benefit income",
                           80),
       x = NULL, y = NULL) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  theme(legend.position = "right",
        panel.grid.major.x = element_blank())

# chart 11 ----

# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "hhtype",
                            data = filter(tidydata,
                                          type == "ben",
                                          survey == "HBAI",
                                          hhtype != "Two adults with children") %>%
                              mutate(weight = ppwgt),
                            conf = 0.95,
                            R = 100,
                            normal = TRUE,
                            basic = FALSE,
                            percentile = FALSE,
                            digits = 3)

# SHS bootstrap mechanism includes council strata
SHS_CI <- groupwiseMedian3(var = "amount",
                           group = "hhtype",
                           data = filter(tidydata,
                                         type == "ben",
                                         survey == "SHS",
                                         hhtype != "Two adults with children") %>%
                             mutate(weight = ppwgt),
                           conf = 0.95,
                           R = 100,
                           normal = TRUE,
                           basic = FALSE,
                           percentile = FALSE,
                           digits = 3)

SHS_CI$survey <- "SHS"
HBAI_CI$survey <- "HBAI"

ch11 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(Median = ifelse(n < 50, NA, Median),
         Normal.lower = ifelse(n < 50, NA, Normal.lower),
         Normal.upper = ifelse(n < 50, NA, Normal.upper),
         hhtype = factor(hhtype),
         hhtype = fct_reorder2(hhtype, survey, Median)) %>%
  filter(!is.na(Median)) %>%
  ggplot() +
  geom_point(aes(x = fct_rev(hhtype), y = Median, colour = survey), size = 2) +
  geom_segment(aes(x = hhtype,
                   xend = hhtype,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Equivalised weekly median household benefit income',
       subtitle = str_wrap("Includes households with and without benefit income; sample >= 50; excludes household type Two Adults with Children",
                           75)) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))


# chart 12 ----

# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "HIHemp",
                            data = filter(tidydata,
                                          type == "ben",
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
                           group = "HIHemp",
                           data = filter(tidydata,
                                         type == "ben",
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

ch12 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(Median = ifelse(n < 50, NA, Median),
         Normal.lower = ifelse(n < 50, NA, Normal.lower),
         Normal.upper = ifelse(n < 50, NA, Normal.upper),
         HIHemp = factor(HIHemp),
         HIHemp = fct_reorder2(HIHemp, survey, Median)) %>%
  filter(!is.na(Median)) %>%
  ggplot() +
  geom_point(aes(x = fct_rev(HIHemp), y = Median, colour = survey), size = 2) +
  geom_segment(aes(x = HIHemp,
                   xend = HIHemp,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Equivalised weekly median household benefit income',
       subtitle = "Includes households with and without benefit income; sample >= 50") +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))

# chart 13 ----

ch13 <- tidybens %>%
  mutate(type = factor(type),
         type = fct_collapse(type, Disability = str_trunc(disbens, 30))) %>%
  group_by(survey, HIHemp, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            total = sum(hhwgt*amount*equ),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey, HIHemp) %>%
  filter(type %in% type[order(-total)[1:5]]) %>%
  ungroup() %>%
  mutate(HIHemp = factor(HIHemp),
         type = fct_reorder(type, total)) %>%
  ggplot(aes(x = HIHemp, y = mean, fill = type)) +
  geom_col() +
  scale_fill_manual(values = cols_bens) +
  facet_wrap(~survey, nrow = 2) +
  labs(title = "Main benefits by economic status",
       subtitle = str_wrap("Weekly equivalised mean benefit income of the five largest benefits for each group; includes households with and without any benefit income",
                           80),
       x = NULL, y = NULL) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip() +
  theme(legend.position = "right",
        panel.grid.major.y = element_blank())

# chart 14 ----

# Get councils with a large enough sample size (households with earnings > 0)
councilsn50 <- filter(tidydata,
                      type == "ben",
                      amount != 0,
                      survey == "HBAI") %>%
  group_by(council) %>%
  count() %>%
  filter(n >= 50) %>%
  select(council) %>% pull()

HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "council",
                            data = filter(tidydata,
                                          type == "ben",
                                          council %in% councilsn50,
                                          council %in% popokcouncils,
                                          survey == "HBAI") %>%
                              mutate(weight = ppwgt),
                            conf = 0.95,
                            R = 100,
                            normal = TRUE,
                            basic = FALSE,
                            percentile = FALSE,
                            digits = 3)

SHS_CI <- groupwiseMedian3(var = "amount",
                           group = "council",
                           data = filter(tidydata,
                                         type == "ben",
                                         council %in% councilsn50,
                                         council %in% popokcouncils,
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

ch14 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(Median = ifelse(n < 50, NA, Median),
         Normal.lower = ifelse(n < 50, NA, Normal.lower),
         Normal.upper = ifelse(n < 50, NA, Normal.upper),
         council = factor(council),
         council = fct_reorder2(council, survey, desc(Median))) %>%
  filter(!is.na(Median)) %>%
  ggplot() +
  geom_point(aes(x = council, y = Median, colour = survey), size = 2) +
  geom_segment(aes(x = council,
                   xend = council,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median benefit household income by council area',
       subtitle = str_wrap('Includes households with and without benefit income; councils with >= 50 cases; councils where household populations are off by < 50%',
                           80)) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))

# chart 15 ----

ch15 <- tidybens_agg %>%
  filter(type %in% tail(levels(type), 14L)) %>%
  mutate(type = as.character(type),
         type = ifelse(type %in% str_trunc(disbens, 30), "Disability", type)) %>%
  group_by(survey, type) %>%
  summarise(amount = sum(amount)) %>%
  ggplot(aes(x = fct_reorder2(type, desc(survey), desc(amount)), y = amount,
             fill = survey)) +
  geom_col(position = "dodge") +
  geom_text(aes(y = amount + 8E5,
                label = str_c("£", comma(amount, scale = 1E-6), " m"),
                colour = survey),
            position =  position_dodge(width = 1),
            hjust = 0) +
  scale_fill_manual(values = cols_survey) +
  scale_colour_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(scale = 1E-6, suffix = " million",
                                           prefix = "£")) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "Aggregated weekly benefit income of the largest benefits") +
  theme(panel.grid.major.y = element_blank())

# chart 16 ----

ch16 <- tidybens %>%
  filter(type %in% disbens,
         survey != "Admin",
         amount != 0) %>%
  group_by(survey, HIHemp) %>%
  summarise(tot = sum(amount*hhwgt*equ),
            mean = wtd.mean(amount, weights = hhwgt),
            median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            sample = n(),
            people = sum(ppwgt)) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(share = tot/sum(tot),
         HIHemp = fct_reorder(HIHemp, share)) %>%
  ggplot(aes(x = HIHemp, y = share, fill = survey)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Share of aggregated disability benefit income by economic status",
                        65)) +
  coord_flip() +
  theme(panel.grid.major.y = element_blank())

# chart 16b ----

ch16b <- tidydata %>%
  filter(type == "inv",
         amount != 0) %>%
  group_by(survey, HIHemp) %>%
  summarise(tot = sum(amount*hhwgt*equ),
            mean = wtd.mean(amount, weights = hhwgt),
            median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            sample = n(),
            people = sum(ppwgt)) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(share = tot/sum(tot),
         HIHemp = fct_reorder(HIHemp, share)) %>%
  ggplot(aes(x = HIHemp, y = share, fill = survey)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = "Share of aggregated investment income across household types") +
  coord_flip() +
  theme(panel.grid.major.y = element_blank())

# chart 17 ----

ch17 <- tidydata %>%
  filter(type == "inv",
         amount != 0) %>%
  group_by(survey, hhtype) %>%
  summarise(tot = sum(amount*hhwgt*equ),
            mean = wtd.mean(amount, weights = hhwgt),
            median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            sample = n(),
            people = sum(ppwgt)) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(share = tot/sum(tot),
         hhtype = fct_reorder(hhtype, share)) %>%
  ggplot(aes(x = hhtype, y = share, fill = survey)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = "Share of aggregated investment income across household types") +
  coord_flip() +
  theme(panel.grid.major.y = element_blank())

# chart 18 ----

ch18 <- tidydata %>%
  filter(type == "inv",
         amount != 0) %>%
  group_by(survey, decile) %>%
  summarise(tot = sum(amount*hhwgt*equ),
            mean = wtd.mean(amount, weights = hhwgt),
            median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            sample = n(),
            people = sum(ppwgt)) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(share = tot/sum(tot),
         decile = factor(decile)) %>%
  ggplot(aes(x = decile, y = share, fill = survey)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = "Share of aggregated investment income across income deciles") +
  coord_flip() +
  theme(panel.grid.major.y = element_blank())

# chart 19 ----

ch19 <- tidydata %>%
  filter(hhtype %in% c("Single pensioner", "Two pensioners", "One adult,
                       one pensioner"),
         type %in% c("total", "earn", "ben", "occ", "inv")) %>%
  group_by(survey, type, hhtype) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt, na.rm = TRUE),
            sample = n()) %>%
  filter(sample >= 50) %>%
  ggplot(aes(x = type, y = mean, fill = survey)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  facet_wrap(vars(hhtype)) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Mean equivalised weekly income and main income sources for pensioner households",
                        70)) +
  theme(panel.grid.major.x = element_blank())

# chart 20 ----

ch20 <- tidybens %>%
  filter(hhtype %in% c("Single pensioner", "Two pensioners",
                       "One adult, one pensioner")) %>%
  mutate(type = factor(type),
         type = fct_collapse(type, Disability = str_trunc(disbens, 30))) %>%
  group_by(survey, hhtype, type) %>%
  summarise(total = sum(hhwgt*amount*equ),
            mean = wtd.mean(amount, weights = hhwgt)) %>%
  ungroup() %>%
  group_by(survey, hhtype) %>%
  filter(type %in% type[order(-total)[1:5]]) %>%
  ungroup() %>%
  mutate(type = fct_reorder(type, mean)) %>%
  ggplot(aes(x = hhtype, y = mean, fill = type)) +
  geom_col() +
  scale_fill_manual(values = cols_bens) +
  facet_wrap(~survey, nrow = 2) +
  labs(title = "What are the main benefits for each type of pensioner
       household?",
       subtitle = str_wrap("Weekly mean benefit income of the five most common benefits in each household type; includes all pensioner households",
                           80),
       x = NULL, y = NULL) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  theme(legend.position = "right") +
  theme(panel.grid.major.x = element_blank())

# chart 21 ----

ch21 <- tidydata %>%
  filter(type == "occ") %>%
  group_by(survey, HIHemp) %>%
  summarise(tot = sum(amount*hhwgt*equ),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(share = tot/sum(tot),
         HIHemp = fct_reorder(HIHemp, share)) %>%
  ggplot(aes(x = HIHemp, y = share, fill = survey)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = "Who gets occupational pensions?",
       subtitle = str_wrap("Share of aggregated occupational pensions income across household types",
                           80)) +
  coord_flip() +
  theme(panel.grid.major.y = element_blank())

# chart 22 ----

ch22 <- tidydata %>%
  filter(type == "occ") %>%
  group_by(survey, hhtype) %>%
  summarise(tot = sum(amount*hhwgt*equ),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(share = tot/sum(tot),
         hhtype = fct_reorder(hhtype, share)) %>%
  ggplot(aes(x = hhtype, y = share, fill = survey)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = "Who gets occupational pensions?",
       subtitle = str_wrap("Share of aggregated occupational pensions income by economic status of household head",
                           75)) +
  coord_flip() +
  theme(panel.grid.major.y = element_blank())

# chart 23 ----

ch23 <- tidydata %>%
  filter(type == "occ") %>%
  group_by(survey, decile) %>%
  summarise(tot = sum(amount*hhwgt*equ),
            sample = n()) %>%
  ungroup() %>%
  group_by(survey) %>%
  mutate(share = tot/sum(tot),
         decile = factor(decile)) %>%
  ggplot(aes(x = decile, y = share, fill = survey)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = percent_format(1)) +
  labs(x = NULL, y = NULL,
       title = "Who gets occupational pensions?",
       subtitle = str_wrap("Share of aggregated occupational pensions income across household income deciles",
                           80)) +
  coord_flip() +
  theme(panel.grid.major.y = element_blank())

# chart 24 ----

ch24 <- tidydata %>%
  group_by(survey, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            median = wtd.quantile(amount, probs = 0.5, weights = ppwgt)) %>%
  ggplot(aes(x = type,
             y = mean,
             fill = survey)) +
  geom_col(position = 'dodge') +
  geom_text(aes(y = ifelse(type == "ded", mean + 50, mean + 20),
                label = comma(mean),
                colour = survey),
            show.legend = FALSE,
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = cols_survey) +
  scale_colour_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  labs(title = str_wrap("Weekly equivalised mean income - total income and income components",
                        70), x = NULL, y = NULL) +
  theme(panel.grid.major.x = element_blank())

# chart 25 ----

ch25 <- tidydata %>%
  filter(type %in% c("earn", "ben")) %>%
  group_by(survey, decile, type) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt)) %>%
  ungroup() %>%
  mutate(decile = factor(decile)) %>%
  ggplot(aes(x = decile, y = mean, fill = survey)) +
  geom_col(position = 'dodge') +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Weekly mean net equivalised household income for the main income types by income decile",
                        70),
       subtitle = "Includes all households") +
  theme(panel.grid.major.x = element_blank()) +
  facet_wrap(~type, ncol = 1)

# chart 26 ----

ch26 <- tidydata %>%
  filter(type != "total",
         type != "ded",
         amount >= 0) %>%
  group_by(survey, decile, type) %>%
  summarise(amount = sum(amount*hhwgt*equ)) %>%
  ungroup() %>%
  mutate(decile = factor(decile)) %>%
  group_by(survey, decile) %>%
  ggplot(aes(x = decile, y = amount, fill = type)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols_types) +
  scale_y_continuous(labels = percent_format()) +
  labs(x  = NULL, y = NULL,
       title = str_wrap("Income sources as a share of aggregated total income by (total) household income decile",
                        70),
       subtitle = "Excludes negative incomes and deductions") +
  facet_wrap(~survey, nrow = 2) +
  theme(legend.position = "right",
        axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank())

# chart 27 ----

ch27 <- tidydata %>%
  filter(type != "total") %>%
  group_by(survey, decile, type) %>%
  summarise(mean = wtd.mean(amount, weights = hhwgt),
            sample =  n()) %>%
  ungroup() %>%
  filter(decile < 5) %>%
  mutate(decile = factor(decile)) %>%

  group_by(survey, decile) %>%
  mutate(total = sum(mean)) %>%
  ggplot(aes(x = survey, y = mean, fill = type)) +
  geom_col(position = "stack") +
  geom_point(aes(y = total), show.legend = FALSE) +
  scale_fill_manual(values = cols_types) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  labs(x  = NULL, y = NULL,
       title = str_wrap("Weekly mean equivalised total income and income components in the bottom four (total) household income deciles",
                        70),
       subtitle = "Includes all income types and all households; dots show mean total income") +
  facet_wrap(~decile) +
  theme(legend.position = "right",
        axis.ticks = element_blank()) +
  theme(panel.grid.major.x = element_blank())

# chart 28 ----

ch28 <- tidydata %>%
  filter(type != "total",
         type != "ded",
         survey == "SHS") %>%
  group_by(survey, type, council) %>%
  summarise(total = sum(amount),
            n = n()) %>%
  group_by(survey, council) %>%
  mutate(share = total/sum(total)) %>%
  ungroup() %>%
  mutate(survey = fct_reorder(survey, share),
         type = fct_shift(type, 2),
         council = fct_reorder2(council, desc(type), desc(share))) %>%
  filter(n >= 50) %>%
  ggplot(aes(x = council, y = share, fill = type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = cols_types) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = str_wrap("Income sources as a share of aggregated total income by council area, SHS",
                        65),
       subtitle = "Excludes deductions") +
  theme(legend.position = "right") +
  theme(panel.grid.major.y = element_blank())

# chart 29 ----

ch29 <- tidydata %>%
  filter(type != "total",
         type != "ded",
         survey == "SHS") %>%
  group_by(type, urbrur) %>%
  summarise(total = sum(amount),
            n = n()) %>%
  group_by(urbrur) %>%
  mutate(share = total/sum(total)) %>%
  ungroup() %>%
  mutate(type = fct_shift(type, 2),
         urbrur = fct_rev(urbrur)) %>%
  ggplot(aes(x = urbrur, y = share, fill = type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = cols_types) +
  scale_y_continuous(labels = percent_format() ) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = str_wrap("Income sources as a share of aggregated total income by urban/rural class, SHS",
                        65),
       subtitle = "Excludes deductions") +
  theme(legend.position = "right") +
  theme(panel.grid.major.y = element_blank())

# chart 30 ----

ch30 <- tidydata %>%
  filter(type == "total",
         survey == "SHS") %>%
  group_by(council) %>%
  summarise(sample = n(),
            people = sum(ppwgt),
            children = sum(chwgt),
            waadults = sum(wawgt),
            pensioners = sum(pnwgt)) %>%
  mutate(children = children/people,
         adults = waadults/people,
         pensioners = pensioners/people) %>%
  select(council, children, adults, pensioners) %>%
  gather(group, share, -council) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = people),
         group = fct_rev(group),
         council = fct_reorder2(council, desc(group), desc(share))) %>%
  ggplot(aes(x = council, y = share, fill = group)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = wes_palettes[["IsleofDogs1"]]) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "Age profile of council areas, SHS",
       subtitle = "Proportion of children, working-age adults and pensioners in each council") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.major.y = element_blank())

# chart 31 ----

ch31 <- tidydata %>%
  filter(type == "total",
         survey == "SHS") %>%
  group_by(urbrur) %>%
  summarise(sample = n(),
            people = sum(ppwgt),
            children = sum(chwgt),
            waadults = sum(wawgt),
            pensioners = sum(pnwgt)) %>%
  mutate(children = children/people,
         adults = waadults/people,
         pensioners = pensioners/people) %>%
  select(urbrur, children, adults, pensioners) %>%
  gather(group, share, -urbrur) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = people),
         group = fct_rev(group),
         urbrur = fct_rev(urbrur)) %>%
  ggplot(aes(x = urbrur, y = share, fill = group)) +
  geom_col(position = "stack") +
  geom_text(aes(y = ifelse(group == "children", 0.05,
                           ifelse(group == "pensioners", 0.85, 0.5)),
                label = percent(share, 1)),
            colour = "white",
            hjust = 0,
            fontface = "bold") +
  scale_fill_manual(values = wes_palettes[["IsleofDogs1"]]) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "Age profile of urban/rural areas, SHS") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())

# chart 32 ----

ch32 <- tidydata %>%
  filter(type != "total",
         type != "ded") %>%
  group_by(survey, hhtype, type) %>%
  summarise(amount = sum(amount*hhwgt*equ),
            sample = n()) %>%
  ungroup() %>%
  mutate(hhtype = factor(hhtype),
         hhtype = fct_reorder2(hhtype, desc(type), desc(amount))) %>%
  ggplot(aes(x = hhtype, y = amount, fill = type)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols_types) +
  scale_y_continuous(labels = percent_format()) +
  labs(x  = NULL, y = NULL,
       title = str_wrap("Income sources as a share of aggregated total income by household type",
                        60),
       subtitle = "Excludes deductions") +
  coord_flip() +
  facet_wrap(~survey, ncol = 2) +
  theme(legend.position = "right") +
  theme(panel.grid.major.y = element_blank())

# chart 33 ----

ch33 <- tidydata %>%
  filter(type != "total",
         type != "ded") %>%
  group_by(survey, HIHemp, type) %>%
  summarise(amount = sum(amount*hhwgt*equ),
            sample = n()) %>%
  ungroup() %>%
  mutate(HIHemp = factor(HIHemp),
         HIHemp = fct_reorder2(HIHemp, desc(type), desc(amount))) %>%
  filter(sample >= 50) %>%
  ggplot(aes(x = HIHemp, y = amount, fill = type)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = cols_types) +
  scale_y_continuous(labels = percent_format()) +
  labs(x  = NULL, y = NULL,
       title = str_wrap("Income sources as a share of aggregated total income by economic status of household head",
                        60),
       subtitle = "Excludes deductions; sample >= 50") +
  coord_flip() +
  facet_wrap(~survey, ncol = 2) +
  theme(legend.position = "right") +
  theme(panel.grid.major.y = element_blank())

# chart 34 ----

ch34 <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey) %>%
  mutate(weight = ppwgt/sum(ppwgt)) %>%
  ggplot(aes(x = amount,
             weight = weight,
             colour = survey, fill = survey )) +
  geom_vline(xintercept = HBAImedian, colour = cols_survey[1]) +
  geom_vline(xintercept = SHSmedian, colour = cols_survey[2]) +
  geom_vline(xintercept = 0.6*HBAImedian, colour = cols_survey[1]) +
  geom_vline(xintercept = 0.6*SHSmedian, colour = cols_survey[2]) +
  geom_density(size = 1, alpha = 0.5, bw = 0.02) +
  scale_x_log10(labels = comma_format(prefix = "£"), limits = c(20, 10000)) +
  scale_color_manual(values = cols_survey) +
  scale_fill_manual(values = cols_survey) +
  labs(title = "Net equivalised household income density distributions",
       subtitle = "Vertical lines show poverty thresholds and medians; note log x scale",
       x = "Weekly equivalised net household income",
       y = NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# chart 35 ----

ch35 <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey) %>%
  ggplot(aes(x = amount, weight = ppwgt, colour = survey, fill = survey )) +
  geom_vline(xintercept = HBAImedian, colour = cols_survey[1]) +
  geom_vline(xintercept = SHSmedian, colour = cols_survey[2]) +
  geom_vline(xintercept = 0.6*HBAImedian, colour = cols_survey[1]) +
  geom_vline(xintercept = 0.6*SHSmedian, colour = cols_survey[2]) +
  coord_cartesian(xlim = c(0, 1000)) +
  stat_ecdf(geom = "step", size = 1, alpha = 0.5) +
  geom_text(data = tail(tidydata, 1L),
            aes(x = HBAImedian + 20, y = 1, label = "Medians"),
            colour = "grey20",
            hjust = 0,
            show.legend = FALSE) +
  geom_text(data = tail(tidydata, 1L),
            aes(x = 0.6*HBAImedian + 10, y = 1, label = "Poverty lines"),
            colour = "grey20",
            hjust = 0,
            show.legend = FALSE) +
  scale_x_continuous(labels = comma_format(prefix = "£"),
                     breaks = c(0, 200, 400, 600, 800, 1000)) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = cols_survey) +
  scale_fill_manual(values = cols_survey) +
  labs(title = "Cumulative net equivalised household income distributions",
       subtitle = "Note linear x scale",
       x = "Weekly equivalised net household income",
       y = "Proportion of people") +
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text())

# chart 36 ----

ch36 <- tidydata %>%
  filter(type == "total") %>%
  group_by(survey, decile) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt)) %>%
  ungroup() %>%
  mutate(decile = factor(decile)) %>%
  ggplot(aes(x = decile, y = median, fill = survey)) +
  geom_col(position = 'dodge') +
  geom_text(aes(y = 40,
                label = comma(median, accuracy = 1, prefix = "£")),
            colour = 'white',
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  labs(x = NULL, y = NULL,
       title = "Weekly median net equivalised household income by income decile",
       subtitle = "All households") +
  theme(panel.grid.major.x = element_blank())

# chart 37 ----

ch37 <- tidydata %>%
  filter(type == "total",
         pnwgt > 0) %>%
  group_by(survey, decile) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt)) %>%
  ungroup() %>%
  mutate(decile = factor(decile)) %>%
  ggplot(aes(x = decile, y = median, fill = survey)) +
  geom_col(position = 'dodge') +
  geom_text(aes(y = 40,
                label = comma(median, accuracy = 1, prefix = "£")),
            colour = 'white',
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  labs(x = NULL, y = NULL,
       title = "Weekly median net equivalised household income by income decile",
       subtitle = "Pensioner households") +
  theme(panel.grid.major.x = element_blank())

# chart 38 ----

ch38 <- tidydata %>%
  filter(type == "total",
         pnwgt == 0,
         chwgt == 0) %>%
  group_by(survey, decile) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt)) %>%
  ungroup() %>%
  mutate(decile = factor(decile)) %>%
  ggplot(aes(x = decile, y = median, fill = survey)) +
  geom_col(position = 'dodge') +
  geom_text(aes(y = 40,
                label = comma(median, accuracy = 1, prefix = "£")),
            colour = 'white',
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  labs(x = NULL, y = NULL,
       title = "Weekly median net equivalised household income by income decile",
       subtitle = "Working-age households without children") +
  theme(panel.grid.major.x = element_blank())

# chart 39 ----

ch39 <- tidydata %>%
  filter(type == "total",
         pnwgt == 0,
         chwgt > 0) %>%
  group_by(survey, decile) %>%
  summarise(median = wtd.quantile(amount, probs = 0.5, weights = ppwgt),
            mean = wtd.mean(amount, weights = hhwgt)) %>%
  ungroup() %>%
  mutate(decile = factor(decile)) %>%
  ggplot(aes(x = decile, y = median, fill = survey)) +
  geom_col(position = 'dodge') +
  geom_text(aes(y = 40,
                label = comma(median, accuracy = 1, prefix = "£")),
            colour = 'white',
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = cols_survey) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  labs(x = NULL, y = NULL,
       title = "Weekly median net equivalised household income by income decile",
       subtitle = "Households with children") +
  theme(panel.grid.major.x = element_blank())

# chart 40 ----

HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "council",
                            data = filter(tidydata,
                                          type == "total",
                                          survey == "HBAI",
                                          council %in% popokcouncils) %>%
                              mutate(weight = ppwgt),
                            conf = 0.95,
                            R = 100,
                            normal = TRUE,
                            basic = FALSE,
                            percentile = FALSE,
                            digits = 3)

SHS_CI <- groupwiseMedian3(var = "amount",
                           group = "council",
                           data = filter(tidydata,
                                         type == "total",
                                         survey == "SHS",
                                         council %in% popokcouncils) %>%
                             mutate(weight = ppwgt),
                           conf = 0.95,
                           R = 100,
                           normal = TRUE,
                           basic = FALSE,
                           percentile = FALSE,
                           digits = 3)

SHS_CI$survey <- "SHS"
HBAI_CI$survey <- "HBAI"

ch40 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(council = fct_reorder2(council, survey, desc(Median))) %>%
  ggplot() +
  geom_hline(yintercept = HBAImedian,
             colour = cols_survey[1],
             alpha = 0.2,
             size = 1.5) +
  geom_hline(yintercept = SHSmedian,
             colour = cols_survey[2],
             alpha = 0.2,
             size = 1.5) +
  geom_point(aes(x = council,
                 y = Median,
                 colour = survey),
             size = 2) +
  geom_segment(aes(x = council,
                   xend = council,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household income',
       subtitle = str_wrap('Excludes council areas with large population discrepancies; lines show Scotland median',
                           80)) +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))

# chart 41 ----

HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "urbrur",
                            data = filter(tidydata, type == "total",
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
                           group = "urbrur",
                           data = filter(tidydata, type == "total",
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

ch41 <- rbind(HBAI_CI, SHS_CI) %>%
  filter(!is.na(Median)) %>%
  ggplot(aes(x = fct_rev(urbrur), y = Median)) +
  geom_hline(yintercept = HBAImedian, colour = cols_survey[1],
             alpha = 0.2, size = 1.5) +
  geom_hline(yintercept = SHSmedian, colour = cols_survey[2],
             alpha = 0.2, size = 1.5) +
  geom_point(aes(colour = survey), size = 2) +
  geom_segment(aes(xend = urbrur,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household income',
       subtitle = 'Lines show Scotland median') +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))

# chart 42 ----

ch_HBAI_CI <- groupwiseMedian2(var = "amount",
                               data = filter(tidydata,
                                             type == "total",
                                             survey == "HBAI") %>%
                                 mutate(weight = chwgt),
                               conf = 0.95,
                               R = 100,
                               normal = TRUE,
                               basic = FALSE,
                               percentile = FALSE,
                               digits = 3)

wa_HBAI_CI <- groupwiseMedian2(var = "amount",
                               data = filter(tidydata,
                                             type == "total",
                                             survey == "HBAI") %>%
                                 mutate(weight = wawgt),
                               conf = 0.95,
                               R = 100,
                               normal = TRUE,
                               basic = FALSE,
                               percentile = FALSE,
                               digits = 3)

pn_HBAI_CI <- groupwiseMedian2(var = "amount",
                               data = filter(tidydata, type == "total",
                                             survey == "HBAI") %>%
                                 mutate(weight = pnwgt),
                               conf = 0.95,
                               R = 100,
                               normal = TRUE,
                               basic = FALSE,
                               percentile = FALSE,
                               digits = 3)

# SHS bootstrap mechanism includes council strata
ch_SHS_CI <- groupwiseMedian3(var = "amount",
                              data = filter(tidydata,
                                            type == "total",
                                            survey == "SHS") %>%
                                mutate(weight = chwgt),
                              conf = 0.95,
                              R = 100,
                              normal = TRUE,
                              basic = FALSE,
                              percentile = FALSE,
                              digits = 3)

wa_SHS_CI <- groupwiseMedian3(var = "amount",
                              data = filter(tidydata,
                                            type == "total",
                                            survey == "SHS") %>%
                                mutate(weight = wawgt),
                              conf = 0.95,
                              R = 100,
                              normal = TRUE,
                              basic = FALSE,
                              percentile = FALSE,
                              digits = 3)

pn_SHS_CI <- groupwiseMedian3(var = "amount",
                              data = filter(tidydata,
                                            type == "total",
                                            survey == "SHS") %>%
                                mutate(weight = pnwgt),
                              conf = 0.95,
                              R = 100,
                              normal = TRUE,
                              basic = FALSE,
                              percentile = FALSE,
                              digits = 3)

ch_HBAI_CI$group <- "Children"
wa_HBAI_CI$group <- "WorkingAgeAdults"
pn_HBAI_CI$group <- "Pensioners"
ch_SHS_CI$group <- "Children"
wa_SHS_CI$group <- "WorkingAgeAdults"
pn_SHS_CI$group <- "Pensioners"

HBAI_CI <- rbind(ch_HBAI_CI, wa_HBAI_CI, pn_HBAI_CI) %>%
  mutate(survey = "HBAI")

SHS_CI <- rbind(ch_SHS_CI, wa_SHS_CI, pn_SHS_CI) %>%
  mutate(survey = "SHS")

ch42 <- rbind(HBAI_CI, SHS_CI) %>%
  select(survey, group, Median, Normal.lower, Normal.upper, n) %>%
  mutate(group = factor(group, levels = agegrouplevels),
         group = fct_rev(group)) %>%
  ggplot() +
  geom_hline(yintercept = HBAImedian, colour = cols_survey[1],
             alpha = 0.2, size = 1.5) +
  geom_hline(yintercept = SHSmedian, colour = cols_survey[2],
             alpha = 0.2, size = 1.5) +
  geom_point(aes(x = group, y = Median, colour = survey), size = 2) +
  geom_segment(aes(x = group,
                   xend = group,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household income',
       subtitle = 'Lines show Scotland median') +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))

# chart 43 ----

HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "hhtype",
                            data = filter(tidydata,
                                          type == "total",
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
                           group = "hhtype",
                           data = filter(tidydata,
                                         type == "total",
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

ch43 <- rbind(HBAI_CI, SHS_CI) %>%
  filter(n >= 50) %>%
  mutate(hhtype = factor(hhtype),
         hhtype = fct_reorder2(hhtype, survey, desc(Median))) %>%
  ggplot(aes(x = hhtype, y = Median)) +
  geom_hline(yintercept = HBAImedian, colour = cols_survey[1], alpha = 0.2,
             size = 1.5) +
  geom_hline(yintercept = SHSmedian, colour = cols_survey[2], alpha = 0.2,
             size = 1.5) +
  geom_point(aes(x = hhtype, y = Median, colour = survey), size = 2) +
  geom_segment(aes(x = hhtype,
                   xend = hhtype,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household income',
       subtitle = 'Lines show Scotland median') +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))

# chart 44 ----

# Note that HBAI bootstrap mechanism is SRS!
HBAI_CI <- groupwiseMedian2(var = "amount",
                            group = "HIHemp",
                            data = filter(tidydata,
                                          type == "total",
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
                           group = "HIHemp",
                           data = filter(tidydata,
                                         type == "total",
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

ch44 <- rbind(HBAI_CI, SHS_CI) %>%
  mutate(HIHemp <- factor(HIHemp, levels = empstatnames),
         HIHemp = fct_reorder2(HIHemp, survey, Median)) %>%
  filter(n >= 50 ) %>%
  ggplot() +
  geom_hline(yintercept = HBAImedian, colour = cols_survey[1], alpha = 0.2,
             size = 1.5) +
  geom_hline(yintercept = SHSmedian, colour = cols_survey[2], alpha = 0.2,
             size = 1.5) +
  geom_point(aes(x = fct_rev(HIHemp), y = Median, colour = survey), size = 2) +
  geom_segment(aes(x = HIHemp,
                   xend = HIHemp,
                   y = Normal.lower,
                   yend = Normal.upper,
                   colour = survey),
               size = 3,
               alpha = 0.5) +
  scale_colour_manual(values = cols_survey) +
  labs(x = NULL,
       y = "Median and 95% C.I",
       title = 'Weekly median net equivalised household income',
       subtitle = "Sample >= 50; lines show Scotland median") +
  scale_y_continuous(labels = comma_format(prefix = "£")) +
  coord_flip(ylim = c(-200, 800))

# chart 45 ----

SHS_pp_rate <- svyby(~low60bhc, ~hhtype, SHSdesign_chwa, svymean,
                     vartype = c("ci","ci")) %>%
  mutate(rate = low60bhc,
         lower = ci_l,
         upper = ci_u,
         survey = "SHS") %>%
  select(survey, hhtype, rate, lower, upper)

ch45 <- tidypovdata %>%
  filter(survey == "HBAI") %>%
  group_by(hhtype) %>%
  mutate(allpeople = sum(chwgt + wawgt),
         sample = n()) %>%
  group_by(hhtype, low60bhc) %>%
  summarise(people = sum(chwgt + wawgt),
            allpeople = max(allpeople),
            sample = max(sample)) %>%
  ungroup() %>%
  filter(low60bhc == 1,
         sample >= 100) %>%
  mutate(survey = "HBAI",
         rate = people/allpeople) %>%
  select(survey, hhtype, rate) %>%
  bind_rows(SHS_pp_rate) %>%
  mutate(hhtype = factor(hhtype),
         hhtype = fct_reorder2(hhtype, survey, desc(rate))) %>%
  filter(hhtype != "Two pensioners",
         hhtype != "Single pensioner") %>%
  ggplot(aes(x = hhtype, y = rate, colour = survey, ymin = lower,
             ymax = upper)) +
  geom_point(aes(size = survey),
             show.legend = FALSE) +
  geom_errorbar() +
  geom_text(aes(y = Inf,
                label = ifelse(survey == "SHS",
                               str_c(percent(rate, 1)," (", percent(lower, 1),
                                     "-", percent(upper, 1), ")"),
                               NA)),
            hjust = 1,
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = cols_survey) +
  scale_size_manual(values = c(3, 2)) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Proportion of people in relative poverty by household type",
                        50),
       subtitle = str_wrap("Excludes pensioners; poverty is BHC; sample >= 100",
                           60))

# chart 46 ----

SHS_pp_rate <- svyby(~low60bhc, ~HIHemp, SHSdesign_chwa, svymean,
                     vartype = c("ci","ci")) %>%
  mutate(rate = low60bhc,
         lower = ci_l,
         upper = ci_u,
         survey = "SHS") %>%
  select(survey, HIHemp, rate, lower, upper)

ch46 <- tidypovdata %>%
  filter(survey == "HBAI") %>%
  group_by(HIHemp) %>%
  mutate(allpeople = sum(chwgt + wawgt),
         sample = n()) %>%
  group_by(HIHemp, low60bhc) %>%
  summarise(people = sum(chwgt + wawgt),
            allpeople = max(allpeople),
            sample = max(sample)) %>%
  ungroup() %>%
  filter(low60bhc == 1,
         sample >= 100) %>%
  mutate(survey = "HBAI",
         rate = people/allpeople) %>%
  select(survey, HIHemp, rate) %>%
  bind_rows(SHS_pp_rate) %>%
  mutate(HIHemp = factor(HIHemp),
         HIHemp = fct_reorder2(HIHemp, survey, desc(rate))) %>%
  ggplot(aes(x = HIHemp, y = rate, colour = survey, ymin = lower,
             ymax = upper)) +
  geom_point(aes(size = survey),
             show.legend = FALSE) +
  geom_errorbar() +
  geom_text(aes(y = Inf,
                label = ifelse(survey == "SHS",
                               str_c(percent(rate, 1)," (", percent(lower, 1),
                                     "-", percent(upper, 1), ")"),
                               NA)),
            hjust = 1,
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = cols_survey) +
  scale_size_manual(values = c(3, 2)) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Proportion of people in relative poverty by economic status of household head",
                        50),
       subtitle = str_wrap("Excludes pensioners; poverty is BHC; sample >= 100",
                           60))

# chart 47 ----

SHS_pp_rate <- svyby(~low60bhc, ~council, SHSdesign_chwa, svymean, vartype = c("ci","ci")) %>%
  mutate(rate = low60bhc,
         lower = ci_l,
         upper = ci_u,
         survey = "SHS") %>%
  select(survey, council, rate, lower, upper)

ch47 <- tidypovdata %>%
  filter(survey == "HBAI") %>%
  group_by(council) %>%
  mutate(allpeople = sum(chwgt + wawgt),
         sample = n()) %>%
  group_by(council, low60bhc) %>%
  summarise(people = sum(chwgt + wawgt),
            allpeople = max(allpeople),
            sample = max(sample)) %>%
  ungroup() %>%
  filter(low60bhc == 1,
         sample >= 100) %>%
  mutate(survey = "HBAI",
         rate = people/allpeople) %>%
  select(survey, council, rate) %>%
  bind_rows(SHS_pp_rate) %>%
  mutate(council = factor(council),
         council = fct_reorder2(council, survey, desc(rate))) %>%
  ggplot(aes(x = council, y = rate, colour = survey, ymin = lower,
             ymax = upper)) +
  geom_point(aes(size = survey),
             show.legend = FALSE) +
  geom_errorbar() +
  geom_text(aes(y = Inf,
                label = ifelse(survey == "SHS",
                               str_c(percent(rate, 1)," (", percent(lower, 1),
                                     "-", percent(upper, 1), ")"),
                               NA)),
            hjust = 1,
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = cols_survey) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  scale_size_manual(values = c(3, 2)) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Proportion of people in relative poverty by council area",
                        50),
       subtitle = str_wrap("Excludes pensioners; poverty is BHC; sample >= 100",
                           60))

# chart 48 ----

SHS_pp_rate <- svyby(~low60bhc, ~urbrur, SHSdesign_chwa, svymean,
                     vartype = c("ci","ci")) %>%
  mutate(rate = low60bhc,
         lower = ci_l,
         upper = ci_u,
         survey = "SHS") %>%
  select(survey, urbrur, rate, lower, upper)

ch48 <- tidypovdata %>%
  filter(survey == "HBAI") %>%
  group_by(urbrur) %>%
  mutate(allpeople = sum(chwgt + wawgt),
         sample = n()) %>%
  group_by(urbrur, low60bhc) %>%
  summarise(people = sum(chwgt + wawgt),
            allpeople = max(allpeople),
            sample = max(sample)) %>%
  ungroup() %>%
  filter(low60bhc == 1,
         sample >= 100) %>%
  mutate(survey = "HBAI",
         rate = people/allpeople) %>%
  select(survey, urbrur, rate) %>%
  bind_rows(SHS_pp_rate) %>%
  mutate(urbrur = factor(urbrur),
         urbrur = fct_reorder2(urbrur, survey, desc(rate))) %>%
  ggplot(aes(x = urbrur, y = rate, colour = survey, ymin = lower,
             ymax = upper)) +
  geom_point(aes(size = survey),
             show.legend = FALSE) +
  geom_errorbar() +
  geom_text(aes(y = Inf,
                label = ifelse(survey == "SHS",
                               str_c(percent(rate, 1)," (", percent(lower, 1),
                                     "-", percent(upper, 1), ")"),
                               NA)),
            hjust = 1,
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = cols_survey) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  scale_size_manual(values = c(3, 2)) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Proportion of people in relative poverty by urban / rural area",
                        50),
       subtitle = str_wrap("Excludes pensioners; poverty is BHC; sample >= 100",
                           60))

# chart 49 ----

SHS_pp_rate <- svyby(~low60bhc, ~tenure, SHSdesign_chwa, svymean,
                     vartype = c("ci","ci")) %>%
  mutate(rate = low60bhc,
         lower = ci_l,
         upper = ci_u,
         survey = "SHS") %>%
  select(survey, tenure, rate, lower, upper)

ch49 <- tidypovdata %>%
  filter(survey == "HBAI") %>%
  group_by(tenure) %>%
  mutate(allpeople = sum(chwgt + wawgt),
         sample = n()) %>%
  group_by(tenure, low60bhc) %>%
  summarise(people = sum(chwgt + wawgt),
            allpeople = max(allpeople),
            sample = max(sample)) %>%
  ungroup() %>%
  filter(low60bhc == 1,
         sample >= 100) %>%
  mutate(survey = "HBAI",
         rate = people/allpeople) %>%
  select(survey, tenure, rate) %>%
  bind_rows(SHS_pp_rate) %>%
  mutate(tenure = factor(tenure),
         tenure = fct_reorder2(tenure, survey, desc(rate))) %>%
  ggplot(aes(x = tenure, y = rate, colour = survey, ymin = lower,
             ymax = upper)) +
  geom_point(aes(size = survey),
             show.legend = FALSE) +
  geom_errorbar() +
  geom_text(aes(y = Inf,
                label = ifelse(survey == "SHS",
                               str_c(percent(rate, 1)," (", percent(lower, 1),
                                     "-", percent(upper, 1), ")"),
                               NA)),
            hjust = 1,
            show.legend = FALSE) +
  coord_flip() +
  scale_colour_manual(values = cols_survey) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  scale_size_manual(values = c(3, 2)) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Proportion of people in relative poverty by tenure",
                        50),
       subtitle = str_wrap("Excludes pensioners; poverty is BHC; sample >= 100",
                           55))

# chart 50 ----

cp <- tidypovdata %>%
  filter(survey == "SHS") %>%
  group_by(council) %>%
  mutate(allchildren = sum(chwgt),
         sample = n()) %>%
  group_by(council, low60bhc) %>%
  mutate(samplepov = ifelse(low60bhc == 1, sum(n()), 0)) %>%
  ungroup() %>%
  filter(samplepov >= 30)

cpnumbers <- svyby(~low60bhc, ~council, SHSdesign_ch, svytotal,
                   vartype = c("ci","ci"))

cilifcomparison <- cpnumbers %>%
  left_join(cilif_council, by = "council") %>%
  mutate(council = fct_reorder(council, Cilif)) %>%
  rename(SHS = low60bhc) %>%
  gather(source, children, -council,  -ci_l, -ci_u) %>%
  arrange(source)

corr <- cor.test(cilifcomparison$children[1:(length(cilifcomparison$children)/2)],
                 cilifcomparison$children[(1 + length(cilifcomparison$children)/2):length(cilifcomparison$children)],
                 method = c("pearson", "kendall", "spearman"))

Pearson <- corr$estimate

ch50 <- cilifcomparison %>%
  mutate(council = factor(council),
         council = fct_reorder2(council, source, desc(children))) %>%
  ggplot(aes(x = council, y = children, colour = source)) +
  geom_point(aes(size = source)) +
  geom_errorbar(data = filter(cilifcomparison, source == "SHS"),
                aes(ymin = ci_l,
                    ymax = ci_u)) +
  coord_flip() +
  scale_colour_manual(values = wes_palettes[["Moonrise2"]]) +
  scale_size_manual(values = c(3,2)) +
  scale_y_continuous(labels = comma_format()) +
  labs(x = NULL, y = NULL,
       title = str_wrap("Number of children in relative poverty BHC by council, SHS and CiLIF",
                        50),
       subtitle = str_wrap("Excludes council areas with SHS samples (in poverty) < 30",
                           60)) +
  annotate("text", x = 3, y = Inf,
           label = str_c("Pearson correlation: ",
                         percent(Pearson, 0.1)), hjust = 1)
