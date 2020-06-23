
groupwiseMedian(var = "amount",
                group = "survey",
                data = test2,
                conf = 0.95,
                R = 100,
                bca = FALSE,
                normal = FALSE,
                basic = TRUE,
                percentile = FALSE)

groupwiseMedian2(var = "amount",
                group = c("survey", "council"),
                data = filter(tidydata, type == "total"),
                conf = 0.95,
                R = 100,
                normal = TRUE,
                basic = FALSE,
                percentile = FALSE)


