
# Strings

# Income types ----

inctypes <- c("total", "earn", "ben", "occ", "inv", "oth", "privben")

# Council names ----

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


# Urbrur classes ----

urbrurcodes <- c(1,2,3,4,5,6)
urbrurclasses <- c("Large Urban Areas", 
                   "Other Urban Areas",
                   "Accessible Small Towns", 
                   "Remote Small Towns",
                   "Accessible Rural", 
                   "Remote Rural")
# Age group ----

agegrouplevels <- c("People", "Children", "WorkingAgeAdults", "Pensioners")

# Hholdtypes ----

hhtypecodes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
hhtypenames <- c("Single adult", "Two adults", 
                 "Three+ adults", "Single adult with children", 
                 "Two adults with children", "Single pensioner", 
                 "Two pensioners", "One adult, one pensioner", 
                 "Other")

# Economic status ----

# SHS HA7 codes
# (1) Self employed [1]
# (2) Employed full time [2]
# (3) Employed part time [3]
# (4) Looking after the home or family [4]
# (5) Permanently retired from work [5]
# (6) Unemployed and seeking work [6]
# (7) At school [7]
# (8) In further/higher education [8]
# (9) Government work or training scheme [9]
# (10) Permanently sick or disabled [10]
# (11) Unable to work due to short-term illness or injury [11]
# (12) Pre school/Not yet at school [12]
# (13) Other, please say what [specify] [13] 

SHS_HIHECONcodes <- c(seq(1, 13))
SHS_HIHECONrecode <-c(3, 1, 2, 7, 5, 4, 7, 7, 7, 6, 7, 7, 7)


# ADULT	EMPSTATI	EMPSTATI	Adult - Employment Status - ILO definition	
# 1	Full-time Employee
# 2	Part-time Employee
# 3	Full-time Self-Employed
# 4	Part-time Self-Employed
# 5	Unemployed
# 6	Retired
# 7	Student
# 8	Looking after family/home
# 9	Permanently sick/disabled
# 10	Temporarily sick/injured
# 11	Other Inactive

FRS_empstatcodes <- c(seq(1, 11))
FRS_empstatrecode <- c(1, 2, 3, 3, 4, 5, 7, 7, 6, 7, 7)

empstatcodes <- c(seq(1, 10))
empstatnames <- c("Full-time Employee", 
                  "Part-time Employee",
                  "Self-Employed", 
                  "Unemployed", 
                  "Retired",
                  "Permanently sick/disabled", 
                  "Other Inactive")

# Benefit names SHS ----

bencodes <- sprintf("BENINC%02d", 1:40)
bencodesoa <- c(sprintf("BENINC%02d_OA1", 1:40), 
                sprintf("BENINC%02d_OA2", 1:40), 
                sprintf("BENINC%02d_OA3", 1:40))

bennames <- c("Universal Credit",
              "Housing Benefit",
              "Council Tax Reduction",
              "Working Tax Credit",
              "Child Tax Credit",
              "Income Support",
              "Jobseekers Allowance",
              "Employment and Support Allowance",
              "Carers Allowance",
              "Child Benefit",
              "Guardians Allowance",
              "Maternity Allowance",
              "Statutory Maternity/Paternity pay, Statutory Adoption Pay",
              "Statutory sick pay",
              "Personal Independence Payments",
              "Disability Living Allowance",
              "Attendance allowance",
              "Severe disablement allowance",
              "Incapacity benefit",
              "Industrial Injuries Disablement Benefit",
              "Pension Credit",
              "State Retirement Pension",
              "Widows Pension, Bereavement Allowance, or Widowed Parents Allowance",
              "Armed Forces Compensation Scheme",
              "War Widows/Widowers Pension",
              "Funeral Expenses Payment",
              "Sure Start Maternity Grant",
              "Best Start Grant",
              "Discretionary Housing Payment",
              "Loan or grant from DWP",
              "Loan or grant from Local Authority",
              "Winter Fuel Payments",
              "Cold Weather Payments",
              "Extended payment of Housing Benefit",
              "Bereavement Payment",
              "Return to Work Payment",
              "Community Care Grant from the Scottish Welfare Fund",
              "Crisis Grant from the Scottish Welfare Fund",
              "Budgeting Loan from Social Fund/Budgeting Advances from Universal Credit",
              "Healthy Start Vouchers")

# Benefit names FRS ----

bencodesfrs <- sprintf("ben%02d", 1:40)

bennamesfrs <- c("State Retirement Pension",
                 "Housing Benefit",
                 "Child Benefit",
                 "Child Tax Credit",
                 "Employment and Support Allo...",
                 
                 "Personal Independence Payments",
                 "Disability Living Allowance",
                 "Universal Credit",
                 "Working Tax Credit",
                 "Council Tax Reduction",
                 
                 "Carers Allowance",
                 "Income Support",
                 "Attendance allowance",
                 "Pension Credit",
                 "Jobseekers Allowance",
                 
                 "Winter Fuel Payments",
                 "Statutory Maternity/Paterni...",
                 "Widows Pension, Bereavem...",
                 "Maternity Allowance",
                 "Incapacity benefit",
                 
                 "Severe disablement allowance",
                 "Industrial Injuries Disable...",
                 "Cold Weather Payments",
                 "Statutory sick pay",
                 "Guardians Allowance",
                 
                 "Discretionary Housing Payment",
                 "Loan or grant from DWP",
                 "Bereavement Payment",
                 "Budgeting Loan from Social ...",
                 "Community Care Grant from t...",
                 
                 "Funeral Expenses Payment",
                 "Armed Forces Compensation S...",
                 "Crisis Grant from the Scott...",
                 "Return to Work Payment",
                 "War Widows/Widowers P...",
                 
                 "Healthy Start Vouchers",
                 "Loan or grant from Local Au...",
                 "Sure Start Maternity Grant",
                 "Extended payment of Housing...",
                 "Best Start Grant")