# Poverty analysis using official HBAI income definition
# comparisons with SHS and Cilif

# customise charts ----
theme_update(legend.position = "top",
             legend.title = element_blank(),
             panel.background = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.y = element_blank(),
             axis.line.x = element_line(),
             axis.ticks = element_blank(),
             text = element_text(size = 18))

# combine survey and Cilif data ----
tidypovdata <- filter(tidydata,
                   type == "total",
                   survey != "Admin") %>%
  select(-hcost, -type, -finman, -md, -n, -equ, -pp, -ch, -wa, -pn)

# HBAI poverty flag (original) ----
hbai_orig <- hbai1819 %>%
  filter(GVTREGN == 12,
         BENUNIT == 1) %>%
  mutate(ID = row_number(),
         survey = factor("HBAI", levels = c("HBAI", "SHS"), ordered = TRUE),
         low60bhc_orig = low60bhc) %>%
  select(ID, survey, low60bhc_orig)

tidypovdata <- left_join(tidypovdata, hbai_orig, by = c("ID", "survey")) %>%
  mutate(low60bhc = ifelse(survey == "HBAI", low60bhc_orig, low60bhc))

# SHS survey design ----

data <- tidypovdata %>%
  filter(survey == "SHS") %>%
  mutate(allch = sum(chwgt),
         allwa = sum(wawgt),
         allpn = sum(pnwgt),
         allpp = sum(ppwgt),
         chwawgt = chwgt + wawgt,
         allchwa = sum(chwawgt))

# for child poverty
SHSpov_ch <- data %>%
  filter(survey == "SHS") %>%
  mutate(work = ifelse(HIHemp %in% c("Full-time Employee",
                                     "Part-time Employee",
                                     "Self-Employed"),
                       "In working families",
                       "Not in working families"))

SHSpov_ch$urbrur <- decode(SHSpov_ch$urbrur,
                           search = urbrurclasses,
                           replace = urbrurclasses2)

SHSdesign_ch <- svydesign(id = ~ID,
                          strata = ~council,
                          weights = ~chwgt,
                          data = SHSpov_ch,
                          fpc = ~allch)

# for working-age adults, non-pensioners (children + waa), pensioners, all people
SHSdesign_wa <- svydesign(id = ~ID,
                          strata = ~council,
                          weights = ~wawgt,
                          data = data,
                          fpc = ~allwa)
SHSdesign_chwa <- svydesign(id = ~ID,
                            strata = ~council,
                            weights = ~chwawgt,
                            data = data,
                            fpc = ~allchwa)
SHSdesign_pn <- svydesign(id = ~ID,
                          strata = ~council,
                          weights = ~pnwgt,
                          data = data,
                          fpc = ~allpn)
SHSdesign_pp <- svydesign(id = ~ID,
                          strata = ~council,
                          weights = ~ppwgt,
                          data = data,
                          fpc = ~allpp)
