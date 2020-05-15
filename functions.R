
# Functions ----

decode <- function(x, search, replace, default = NULL) {
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

## groupwiseMedian function from rcompanion package ----

# basic	- If TRUE, includes the basic confidence intervals for the group means by bootstrap.
# normal - If TRUE, includes the normal confidence intervals for the group means by bootstrap.
# percentile - If TRUE, includes the percentile confidence intervals for the group means by bootstrap.
# bca - If TRUE, includes the BCa confidence intervals for the group means by bootstrap.
# Don't use: pseudo, wilcox, exact (these require other packages)
# Don't use bca (doesn't workfor some reason)


groupwiseMedian = 
  function(formula=NULL, data=NULL, var=NULL, group=NULL,
           conf=0.95, R=5000,
           boot=FALSE,  pseudo=FALSE,
           basic=FALSE, normal=FALSE,
           percentile=FALSE, bca=TRUE, 
           wilcox=FALSE,  exact=FALSE, 
           digits=3, ...)
  {
    if(!is.null(formula)){
      var   = all.vars(formula[[2]])[1]
      group = all.vars(formula[[3]])
    }
    ####################
    DF=
      ddply(.data=data,
            .variables=group, var, 
            .fun=function(x, idx){
              sum(!is.na(x[,idx]))})
    ####################
    fun1 = function(x, idx){as.numeric(median(x[,idx], na.rm=TRUE))}
    D1=
      ddply(.data=data,
            .variables=group, var,
            .fun=fun1)
    ####################
    if(boot==TRUE){
      fun2 = function(x, idx){mean(boot(x[,idx],
                                        function(y,j) median(y[j]),
                                        R=R, ...)$t[,1])}
      D2=ddply(.data=data,
               .variables=group, var,
               .fun=fun2)
    }
    ####################
    if(pseudo==TRUE){
      fun3 = function(x, idx){as.numeric(wilcox.test(x[,idx],
                                                     conf.int=TRUE, conf.level=conf, exact=FALSE)$estimate)}
      D3=
        ddply(.data=data,
              .variables=group, var,
              .fun=fun3)
    }
    ####################
    if(basic==TRUE){
      fun4 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R=R, ...), 
                                      conf=conf, 
                                      type="basic", ...)$basic[4]}
      fun5 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R=R, ...), 
                                      conf=conf, 
                                      type="basic", ...)$basic[5]}
      D4=ddply(.data=data,
               .variables=group, var,
               .fun=fun4)
      D5=ddply(.data=data,
               .variables=group, var,
               .fun=fun5)
    }
    ####################
    if(normal==TRUE){
      fun6 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R=R, ...), conf=conf, 
                                      type="norm", ...)$normal[2]}
      fun7 = function(x, idxt){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R=R, ...), conf=conf, 
                                      type="norm", ...)$normal[3]}
      D6=ddply(.data=data,
               .variables=group, var,
               .fun=fun6)                    
      D7=ddply(.data=data,
               .variables=group, var,
               .fun=fun7)
    }
    ####################
    if(percentile==TRUE){
      fun8 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(y[j]),
                                           R=R, ...), conf=conf, 
                                      type="perc", ...)$percent[4]}
      fun9 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,jt) median(y[j]),
                                           R=R, ...), conf=conf, 
                                      type="perc", ...)$percent[5]}
      D8=ddply(.data=data,
               .variables=group, var,
               .fun=fun8)                    
      D9=ddply(.data=data,
               .variables=group, var,
               .fun=fun9)
    }
    ####################
    if(bca==TRUE){
      fun10 = function(x, idx){boot.ci(boot(x[,idx],
                                            function(y,j) median(y[j]),
                                            R=R, ...), conf=conf, 
                                       type="bca", ...)$bca[4]}
      fun11 = function(x, idx){boot.ci(boot(x[,idx],
                                            function(y,j) median(y[j]),
                                            R=R, ...), conf=conf, 
                                       type="bca", ...)$bca[5]}
      D10=ddply(.data=data,
                .variables=group, var,
                .fun=fun10)                    
      D11=ddply(.data=data,
                .variables=group, var,
                .fun=fun11)
    }  
    ####################
    if(exact==TRUE){
      fun12 = function(x, idx){MedianCI(x[,idx], ...)[2]}
      fun13 = function(x, idx){MedianCI(x[,idx], ...)[3]}
      D12=ddply(.data=data,
                .variables=group, var,
                .fun=fun12)                    
      D13=ddply(.data=data,
                .variables=group, var,
                .fun=fun13)
    }  
    ####################
    if(wilcox==TRUE){
      fun14 = function(x, idx){wilcox.test(x[,idx],
                                           conf.int=TRUE, conf.level=conf, exact=FALSE,
                                           ...)$conf.int[1]}
      fun15= function(x, idx){wilcox.test(x[,idx],
                                          conf.int=TRUE, conf.level=conf, exact=FALSE,
                                          ...)$conf.int[2]}              
      
      D14=ddply(.data=data,
                .variables=group, var,
                .fun=fun14)
      D15=ddply(.data=data,
                .variables=group, var,
                .fun=fun15)
    }
    ####################
    DF = plyr::rename(DF,c('V1'='n'))
    DF$Median                                 = signif(D1$V1, digits=digits)
    if(boot==TRUE){DF$Boot.median             = signif(D2$V1, digits=digits)}
    if(pseudo==TRUE){DF$Pseudo.median         = signif(D3$V1, digits=digits)}
    if(basic|normal|percentile|bca|exact){DF$Conf.level = conf}
    if(basic==TRUE){DF$Basic.lower            = signif(D4$V1, digits=digits)}
    if(basic==TRUE){DF$Basic.upper            = signif(D5$V1, digits=digits)}
    if(normal==TRUE){DF$Normal.lower          = signif(D6$V1, digits=digits)}
    if(normal==TRUE){DF$Normal.upper          = signif(D7$V1, digits=digits)}
    if(percentile==TRUE){DF$Percentile.lower  = signif(D8$V1, digits=digits)}
    if(percentile==TRUE){DF$Percentile.upper  = signif(D9$V1, digits=digits)}
    if(bca==TRUE){DF$Bca.lower                = signif(D10$V1, digits=digits)}
    if(bca==TRUE){DF$Bca.upper                = signif(D11$V1, digits=digits)}
    if(exact==TRUE){DF$Exact.lower            = signif(D12$lwr.ci, digits=digits)}
    if(exact==TRUE){DF$Exact.upper            = signif(D13$upr.ci, digits=digits)}
    if(wilcox==TRUE){DF$Wilcox.lower          = signif(D14$V1, digits=digits)}
    if(wilcox==TRUE){DF$Wilcox.upper          = signif(D15$V1, digits=digits)}
    return(DF)
  }






## groupwiseMedian2 function - adapted from groupwiseMedian function to include frequency weights ----

# More info on bootstrp methods: https://influentialpoints.com/Training/bootstrap_confidence_intervals-principles-properties-assumptions.htm#type 

groupwiseMedian2 = 
  function(data=NULL, 
           var=NULL, 
           group=NULL,
           conf=0.95, 
           R=100,
           basic=TRUE, 
           normal=FALSE,
           percentile=FALSE, 
           digits=3, ...)
  {
    
    ####################
    
    DF=
      ddply(.data=data,
            .variables=group, var, 
            .fun=function(x, idx){
              sum(!is.na(x[,idx]))})
    
    ####################
    
    fun1 = function(x, idx){as.numeric(wtd.quantile(x[,idx], probs = c(0.5), weights = x$weight, na.rm=TRUE))}
    D1=
      ddply(.data=data,
            .variables=group, var,
            .fun=fun1)
    
    ####################
    
    if(basic==TRUE){
      fun4 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, ...), 
                                      conf=conf, 
                                      type="basic", ...)$basic[4]}
      
      fun5 = function(x, idx){boot.ci(boot(x[, idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, ...),
                                      conf=conf, 
                                      type="basic", ...)$basic[5]}
      D4=ddply(.data=data,
               .variables=group, var,
               .fun=fun4)
      D5=ddply(.data=data,
               .variables=group, var,
               .fun=fun5)
    }
    ####################
    
    if(normal==TRUE){
      fun6 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, ...), conf=conf, 
                                      type="norm", ...)$normal[2]}
      fun7 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, ...), conf=conf, 
                                      type="norm", ...)$normal[3]}
      D6=ddply(.data=data,
               .variables=group, var,
               .fun=fun6)                    
      D7=ddply(.data=data,
               .variables=group, var,
               .fun=fun7)
    }
    ####################
    
    if(percentile==TRUE){
      fun8 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, ...), conf=conf, 
                                      type="perc", ...)$percent[4]}
      fun9 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, ...), conf=conf, 
                                      type="perc", ...)$percent[5]}
      D8=ddply(.data=data,
               .variables=group, var,
               .fun=fun8)                    
      D9=ddply(.data=data,
               .variables=group, var,
               .fun=fun9)
    }
    ####################
    
    DF = plyr::rename(DF,c('V1'='n'))
    DF$Median                                 = signif(D1$V1, digits=digits)
    if(basic|normal|percentile){DF$Conf.level = conf}
    if(basic==TRUE){DF$Basic.lower            = signif(D4$V1, digits=digits)}
    if(basic==TRUE){DF$Basic.upper            = signif(D5$V1, digits=digits)}
    if(normal==TRUE){DF$Normal.lower          = signif(D6$V1, digits=digits)}
    if(normal==TRUE){DF$Normal.upper          = signif(D7$V1, digits=digits)}
    if(percentile==TRUE){DF$Percentile.lower  = signif(D8$V1, digits=digits)}
    if(percentile==TRUE){DF$Percentile.upper  = signif(D9$V1, digits=digits)}
    
    return(DF)
  }

## groupwiseMedian3 function for SHS - accounts for council stratification ----

groupwiseMedian3 = 
  function(data=NULL, 
           var=NULL, 
           group=NULL,
           conf=0.95, 
           R=100,
           basic=TRUE, 
           normal=FALSE,
           percentile=FALSE, 
           digits=3, ...)
  {
    
    ####################
    
    DF=
      ddply(.data=data,
            .variables=group, var, 
            .fun=function(x, idx){
              sum(!is.na(x[,idx]))})
    
    ####################
    
    fun1 = function(x, idx){as.numeric(wtd.quantile(x[,idx], probs = c(0.5), weights = x$weight, na.rm=TRUE))}
    D1=
      ddply(.data=data,
            .variables=group, var,
            .fun=fun1)
    
    ####################
    
    if(basic==TRUE){
      fun4 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, 
                                           strata = factor(x$council), ...), 
                                      conf=conf, 
                                      type="basic", ...)$basic[4]}
      
      fun5 = function(x, idx){boot.ci(boot(x[, idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, 
                                           strata = factor(x$council), ...),
                                      conf=conf, 
                                      type="basic", ...)$basic[5]}
      D4=ddply(.data=data,
               .variables=group, var,
               .fun=fun4)
      D5=ddply(.data=data,
               .variables=group, var,
               .fun=fun5)
    }
    ####################
    
    if(normal==TRUE){
      fun6 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, 
                                           strata = factor(x$council), ...), conf=conf, 
                                      type="norm", ...)$normal[2]}
      fun7 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, 
                                           strata = factor(x$council), ...), conf=conf, 
                                      type="norm", ...)$normal[3]}
      D6=ddply(.data=data,
               .variables=group, var,
               .fun=fun6)                    
      D7=ddply(.data=data,
               .variables=group, var,
               .fun=fun7)
    }
    ####################
    
    if(percentile==TRUE){
      fun8 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, 
                                           strata = factor(x$council), ...), conf=conf, 
                                      type="perc", ...)$percent[4]}
      fun9 = function(x, idx){boot.ci(boot(x[,idx],
                                           function(y,j) median(rep(y[j], times = x$weight[j])),
                                           R=R, 
                                           strata = factor(x$council), ...), conf=conf, 
                                      type="perc", ...)$percent[5]}
      D8=ddply(.data=data,
               .variables=group, var,
               .fun=fun8)                    
      D9=ddply(.data=data,
               .variables=group, var,
               .fun=fun9)
    }
    ####################
    
    DF = plyr::rename(DF,c('V1'='n'))
    DF$Median                                 = signif(D1$V1, digits=digits)
    if(basic|normal|percentile){DF$Conf.level = conf}
    if(basic==TRUE){DF$Basic.lower            = signif(D4$V1, digits=digits)}
    if(basic==TRUE){DF$Basic.upper            = signif(D5$V1, digits=digits)}
    if(normal==TRUE){DF$Normal.lower          = signif(D6$V1, digits=digits)}
    if(normal==TRUE){DF$Normal.upper          = signif(D7$V1, digits=digits)}
    if(percentile==TRUE){DF$Percentile.lower  = signif(D8$V1, digits=digits)}
    if(percentile==TRUE){DF$Percentile.upper  = signif(D9$V1, digits=digits)}
    
    return(DF)
  }


# Strings ----

## Council names

FRScodes <- c(194, 195, 196, 287, 289, 291, 292, 
              293, 294, 295, 296, 387, 495, 388, 
              389, 390, 391, 392, 393, 394, 395, 
              396, 494, 487, 488, 288, 493, 489, 
              490, 491, 290, 492)

SHScodes <- c(100, 110, 120, 130, 150, 170, 180, 
              190, 200, 210, 220, 230, 235, 240, 
              250, 260, 270, 280, 290, 300, 310, 
              320, 330, 340, 350, 355, 360, 370, 
              380, 390, 395, 400)
  
councilnames <- c('Aberdeen City', 'Aberdeenshire', 'Angus', 
              'Argyll & Bute', 'Clackmannanshire', 
              'Dumfries & Galloway', 'Dundee City', 
              'East Ayrshire', 'East Dunbartonshire', 
              'East Lothian', 'East Renfrewshire', 
              'Edinburgh', 'Na h-Eileanan Siar', 
              'Falkirk', 'Fife', 'Glasgow City', 'Highland', 
              'Inverclyde', 'Midlothian', 'Moray', 
              'North Ayrshire', 'North Lanarkshire', 
              'Orkney Islands', 'Perth & Kinross', 
              'Renfrewshire', 'Scottish Borders', 
              'Shetland Islands', 'South Ayrshire', 
              'South Lanarkshire', 'Stirling', 
              'West Dunbartonshire', 'West Lothian')

councilshort <- c('Abdn', 'Abdshr', 'Angus', 
                  'ArgBu', 'Clack', 
                  'DumGal', 'Dundee', 
                  'EAyr', 'EDunb', 
                  'ELoth', 'ERenf', 
                  'Edi', 'WestIsl', 
                  'Falk', 'Fife', 'Gla', 'Highl', 
                  'Inv', 'Mloth', 'Moray', 
                  'NAyr', 'NLana', 
                  'OrkIsl', 'PerthK', 
                  'Renf', 'ScBord', 
                  'ShetIsl', 'SAyr', 
                  'SLana', 'Stir', 
                  'WDunb', 'WLoth')


## Urbrur classes

urbrurcodes <- c(1,2,3,4,5,6)
urbrurclasses <- c("Large Urban Areas", 
                   "Other Urban Areas",
                   "Accessible Small Towns", 
                   "Remote Small Towns",
                   "Accessible Rural", 
                   "Remote Rural")
# Age group

agegrouplevels <- c("People", "Children", "WorkingAgeAdults", "Pensioners")

# Hholdtypes

hhtypecodes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
hhtypenames <- c("Single adult", "Two adults", 
                 "Three+ adults", "Single adult with children", 
                 "Two adults with children", "Three+ adults with children", 
                 "Single pensioner", "Two pensioners",  
                 "One adult, one pensioner", "Other")

FRShhcodes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
FRShhrecode <- c(7, 7, 1, 1, 8, 9, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6)

# HOUSEHOL	HHCOMPS	HHCOMPS	Household comp - revised	
# 1	One male adult, no children over pension age      7
# 2	One female adult, no children over pension age    7
# 3	One male adult, no children, under pension age    1
# 4	One female adult, no children, under pension age  1
# 5	Two adults, no children, both over pension age    8
# 6	Two adults, no children, one over pension age     9
# 7	Two adults, no children, both under pension age   2
# 8	Three or more adults, no children                 3
# 9	One adult, one child                              4
# 10	One adult, two children                         4
# 11	One adult, three or more children               4
# 12	Two adults, one child                           5
# 13	Two adults, two children                        5
# 14	Two adults, three or more children              5
# 15	Three or more adults, one child                 6
# 16	Three or more adults, two children              6
# 17	Three or more adults, three or more children    6



## Colours

### Moonrise 2 = c("#798E87", "#C27D38", "#CCC591", "#29211F")

cols_survey <- c(HBAI = "#798E87", SHS = "#C27D38")

