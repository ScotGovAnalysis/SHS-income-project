
# Change some theme elements (charts)
theme_update(legend.position = "top",
             legend.title = element_blank(),
             panel.background = element_blank(),
             panel.grid.major.x = element_line(colour = "grey95"),
             panel.grid.minor.x = element_line(colour = "grey95"),
             panel.grid.major.y = element_line(colour = "grey95"),
             axis.ticks = element_blank(),
             text = element_text(size = 16))

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

