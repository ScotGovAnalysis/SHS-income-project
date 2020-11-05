
decode <- function(x, search, replace, default = NULL){

  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)

  if (length(search) == 0L) {function(x) if (is.null(default)) x else rep(default, length(x))}
                                           else {function(x) ifelse(x == search[1L],
                               replace[1L],
                               decode.fun(tail(search,  -1L),
                                          tail(replace, -1L),
                                          default)(x))}

  return(decode.fun(search, replace, default)(x))
}

groupwiseMedian <- function(formula = NULL,
                            data = NULL,
                            var = NULL,
                            group = NULL,
                            conf = 0.95,
                            R = 5000,
                            boot = FALSE,
                            pseudo = FALSE,
                            basic = FALSE,
                            normal = FALSE,
                            percentile = FALSE,
                            bca = TRUE,
                            wilcox = FALSE,
                            exact = FALSE,
                            digits = 3,
                            ...){
    # groupwiseMedian function from rcompanion package
    # basic	- If TRUE, includes the basic confidence intervals for the group means by bootstrap.
    # normal - If TRUE, includes the normal confidence intervals for the group means by bootstrap.
    # percentile - If TRUE, includes the percentile confidence intervals for the group means by bootstrap.
    # bca - If TRUE, includes the BCa confidence intervals for the group means by bootstrap.
    # Don't use: pseudo, wilcox, exact (these require other packages)
    # Don't use bca (doesn't workfor some reason)

    if (!is.null(formula)) {
      var = all.vars(formula[[2]])[1]
      group = all.vars(formula[[3]])
    }

    DF <- ddply(.data = data,
                .variables = group, var,
                .fun = function(x, idx){sum(!is.na(x[,idx]))})

    fun1 <- function(x, idx){as.numeric(median(x[,idx], na.rm = TRUE))}

    D1 <- ddply(.data = data,
                .variables = group, var,
                .fun = fun1)

    if (boot == TRUE) {
      fun2 <- function(x, idx){mean(boot(x[,idx],
                                        function(y,j) median(y[j]),
                                        R = R, ...)$t[,1])}
      D2 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun2)
    }

    if (pseudo == TRUE) {
      fun3 <- function(x, idx){as.numeric(wilcox.test(x[,idx],
                                                     conf.int = TRUE,
                                                     conf.level = conf,
                                                     exact = FALSE)$estimate)}
      D3 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun3)
    }

    if (basic == TRUE) {
      fun4 <- function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R = R, ...),
                                      conf = conf,
                                      type = "basic", ...)$basic[4]}
      fun5 <- function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R = R, ...),
                                      conf = conf,
                                      type = "basic", ...)$basic[5]}
      D4 <- ddply(.data = data,
                 .variables = group, var,
                 .fun = fun4)
      D5 <- ddply(.data = data,
                 .variables = group, var,
                 .fun = fun5)
    }

    if (normal == TRUE) {
      fun6 <- function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R = R, ...), conf = conf,
                                      type = "norm", ...)$normal[2]}
      fun7 <- function(x, idxt){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R = R, ...), conf = conf,
                                      type = "norm", ...)$normal[3]}
      D6 <- ddply(.data = data,
               .variables = group, var,
               .fun = fun6)
      D7 <- ddply(.data = data,
               .variables = group, var,
               .fun = fun7)
    }

    if (percentile == TRUE) {
      fun8 <- function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R = R, ...), conf = conf,
                                      type = "perc", ...)$percent[4]}
      fun9 <- function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,jt) median(y[j]),
                                           R = R, ...), conf = conf,
                                      type = "perc", ...)$percent[5]}
      D8 <- ddply(.data = data,
                 .variables = group, var,
                 .fun = fun8)
      D9 <- ddply(.data = data,
                 .variables = group, var,
                 .fun = fun9)
    }

    if (bca == TRUE) {
      fun10 <- function(x, idx){boot.ci(boot(x[,idx],
                                            function(y,j) median(y[j]),
                                            R = R, ...), conf = conf,
                                       type = "bca", ...)$bca[4]}
      fun11 <- function(x, idx){boot.ci(boot(x[,idx],
                                            function(y,j) median(y[j]),
                                            R = R, ...), conf = conf,
                                       type = "bca", ...)$bca[5]}
      D10 <- ddply(.data = data,
                   .variables = group, var,
                   .fun = fun10)
      D11 <- ddply(.data = data,
                   .variables = group, var,
                   .fun = fun11)
    }

    if (exact == TRUE) {
      fun12 <- function(x, idx){MedianCI(x[,idx], ...)[2]}
      fun13 <- function(x, idx){MedianCI(x[,idx], ...)[3]}
      D12 <- ddply(.data = data,
                   .variables = group, var,
                   .fun = fun12)
      D13 <- ddply(.data = data,
                   .variables = group, var,
                   .fun = fun13)
    }

    if (wilcox == TRUE) {
      fun14 <- function(x, idx){wilcox.test(x[,idx],
                                           conf.int = TRUE,
                                           conf.level = conf,
                                           exact = FALSE,
                                           ...)$conf.int[1]}
      fun15 <- function(x, idx){wilcox.test(x[,idx],
                                          conf.int = TRUE, conf.level = conf, exact = FALSE,
                                          ...)$conf.int[2]}

      D14 <- ddply(.data = data,
                   .variables = group, var,
                   .fun = fun14)
      D15 <- ddply(.data = data,
                   .variables = group, var,
                   .fun = fun15)
    }

    DF <- plyr::rename(DF,c('V1' = 'n'))
    DF$Median                                 <- signif(D1$V1, digits = digits)
    if (boot == TRUE) {DF$Boot.median         <- signif(D2$V1, digits = digits)}
    if (pseudo == TRUE) {DF$Pseudo.median     <- signif(D3$V1, digits = digits)}
    if (basic | normal | percentile | bca | exact) {DF$Conf.level = conf}
    if (basic == TRUE) {DF$Basic.lower           <- signif(D4$V1, digits = digits)}
    if (basic == TRUE) {DF$Basic.upper           <- signif(D5$V1, digits = digits)}
    if (normal == TRUE) {DF$Normal.lower         <- signif(D6$V1, digits = digits)}
    if (normal == TRUE) {DF$Normal.upper         <- signif(D7$V1, digits = digits)}
    if (percentile == TRUE) {DF$Percentile.lower <- signif(D8$V1, digits = digits)}
    if (percentile == TRUE) {DF$Percentile.upper <- signif(D9$V1, digits = digits)}
    if (bca == TRUE) {DF$Bca.lower               <- signif(D10$V1, digits = digits)}
    if (bca == TRUE) {DF$Bca.upper               <- signif(D11$V1, digits = digits)}
    if (exact == TRUE) {DF$Exact.lower           <- signif(D12$lwr.ci, digits = digits)}
    if (exact == TRUE) {DF$Exact.upper           <- signif(D13$upr.ci, digits = digits)}
    if (wilcox == TRUE) {DF$Wilcox.lower         <- signif(D14$V1, digits = digits)}
    if (wilcox == TRUE) {DF$Wilcox.upper         <- signif(D15$V1, digits = digits)}
    return(DF)
  }

groupwiseMedian2 <- function(data = NULL,
                             var = NULL,
                             group = NULL,
                             conf = 0.95,
                             R = 100,
                             basic = TRUE,
                             normal = FALSE,
                             percentile = FALSE,
                             digits = 3, ...){

    # groupwiseMedian2 function - adapted from groupwiseMedian function to include frequency weights
    # More info on bootstrp methods: https://influentialpoints.com/Training/bootstrap_confidence_intervals-principles-properties-assumptions.htm#type

    DF <- ddply(.data = data,
                .variables = group, var,
                .fun = function(x, idx){
                  sum(!is.na(x[,idx]))})

    fun1 <- function(x, idx){as.numeric(wtd.quantile(x[,idx], probs = c(0.5),
                                                    weights = x$weight,
                                                    na.rm = TRUE))}
    D1 <- ddply(.data = data,
                .variables = group, var,
                .fun = fun1)

    if (basic == TRUE) {
      fun4 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R, ...),
                                      conf = conf,
                                      type = "basic", ...)$basic[4]}

      fun5 <- function(x, idx){
        boot.ci(boot(x[, idx],
                      function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R, ...),
                                      conf = conf,
                                      type = "basic", ...)$basic[5]}
      D4 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun4)
      D5 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun5)
    }

    if (normal == TRUE) {
      fun6 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R, ...), conf = conf,
                                      type = "norm", ...)$normal[2]}
      fun7 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R, ...), conf = conf,
                                      type = "norm", ...)$normal[3]}
      D6 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun6)
      D7 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun7)
    }

    if (percentile == TRUE) {
      fun8 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R, ...), conf = conf,
                                      type = "perc", ...)$percent[4]}
      fun9 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R, ...), conf = conf,
                                      type = "perc", ...)$percent[5]}
      D8 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun8)
      D9 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun9)
    }

    DF <- plyr::rename(DF,c('V1' = 'n'))
    DF$Median                                     <- signif(D1$V1, digits = digits)
    if (basic | normal | percentile) {DF$Conf.level = conf}
    if (basic == TRUE) {DF$Basic.lower            <- signif(D4$V1, digits = digits)}
    if (basic == TRUE) {DF$Basic.upper            <- signif(D5$V1, digits = digits)}
    if (normal == TRUE) {DF$Normal.lower          <- signif(D6$V1, digits = digits)}
    if (normal == TRUE) {DF$Normal.upper          <- signif(D7$V1, digits = digits)}
    if (percentile == TRUE) {DF$Percentile.lower  <- signif(D8$V1, digits = digits)}
    if (percentile == TRUE) {DF$Percentile.upper  <- signif(D9$V1, digits = digits)}

    return(DF)
  }

groupwiseMedian3 <- function(data = NULL,
                             var = NULL,
                             group = NULL,
                             conf = 0.95,
                             R = 100,
                             basic = TRUE,
                             normal = FALSE,
                             percentile = FALSE,
                             digits = 3,
                             ...){

  # groupwiseMedian3 function for SHS - accounts for council stratification
    DF <- ddply(.data = data,
               .variables = group, var,
               .fun = function(x, idx){
              sum(!is.na(x[,idx]))})

    fun1 <- function(x, idx){as.numeric(wtd.quantile(x[,idx],
                                                    probs = c(0.5),
                                                    weights = x$weight, na.rm = TRUE))}
    D1 <- ddply(.data = data,
                .variables = group, var,
                .fun = fun1)

    if (basic == TRUE) {
      fun4 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R,
                                           strata = factor(x$council), ...),
                                      conf = conf,
                                      type = "basic", ...)$basic[4]}

      fun5 <- function(x, idx){
        boot.ci(boot(x[, idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R,
                                           strata = factor(x$council), ...),
                                      conf = conf,
                                      type = "basic", ...)$basic[5]}
      D4 <- ddply(.data = data,
                 .variables = group, var,
                 .fun = fun4)
      D5 <- ddply(.data = data,
                 .variables = group, var,
                 .fun = fun5)
    }

    if (normal == TRUE) {
      fun6 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R,
                                           strata = factor(x$council), ...), conf = conf,
                                      type = "norm", ...)$normal[2]}
      fun7 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R,
                                           strata = factor(x$council), ...), conf = conf,
                                      type = "norm", ...)$normal[3]}
      D6 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun6)
      D7 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun7)
    }

    if (percentile == TRUE) {
      fun8 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R,
                                           strata = factor(x$council), ...), conf = conf,
                                      type = "perc", ...)$percent[4]}
      fun9 <- function(x, idx){
        boot.ci(boot(x[,idx],
                     function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R = R,
                                           strata = factor(x$council), ...), conf = conf,
                                      type = "perc", ...)$percent[5]}
      D8 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun8)
      D9 <- ddply(.data = data,
                  .variables = group, var,
                  .fun = fun9)
    }

    DF <- plyr::rename(DF,c('V1' = 'n'))
    DF$Median                                    <- signif(D1$V1, digits = digits)
    if (basic | normal | percentile) {DF$Conf.level = conf}
    if (basic == TRUE) {DF$Basic.lower           <- signif(D4$V1, digits = digits)}
    if (basic == TRUE) {DF$Basic.upper           <- signif(D5$V1, digits = digits)}
    if (normal == TRUE) {DF$Normal.lower         <- signif(D6$V1, digits = digits)}
    if (normal == TRUE) {DF$Normal.upper         <- signif(D7$V1, digits = digits)}
    if (percentile == TRUE) {DF$Percentile.lower <- signif(D8$V1, digits = digits)}
    if (percentile == TRUE) {DF$Percentile.upper <- signif(D9$V1, digits = digits)}

    return(DF)
  }
