
# Strings

# Surveys

surveys <- c("Admin", "HBAI", "SHS")

# People

people <-c("people", "children", "adults", "pensioners")

# Income types ----

inctypes <- c("total", "earn", "ben", "occ", "inv", "oth", "privben", "ded")

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


urbrurclasses2 <- c("Large Urban Areas", 
                   "Other Urban Areas",
                   "Small Towns", 
                   "Small Towns",
                   "Rural", 
                   "Rural")

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
              "Tax Credits",
              "Tax Credits",
              "Income Support",
              "Jobseekers Allowance",
              "Employment and Support Allowance",
              "Carers Allowance",
              "Child Benefit",
              "Guardians Allowance",
              "Maternity Allowance",
              "Statutory Maternity Pay",
              "Statutory sick pay",
              "Personal Independence Payment",
              "Disability Living Allowance",
              "Attendance Allowance",
              "Severe Disablement Allowance",
              "Incapacity benefit",
              "Industrial Injuries Disablement Benefit",
              "Pension Credit",
              "State Pension",
              "Widows Pension, Bereavement Allowance, or Widowed Parents Allowance",
              "Armed Forces Compensation Scheme",
              "War Widows/Widowers Pension",
              "Funeral Expenses Payment",
              "Sure Start Maternity Grant",
              "Best Start Grant",
              "Discretionary Housing Payment",
              "Loan or grant from DWP",
              "Loan or grant from Local Authority",
              "Winter Fuel Payment",
              "Cold Weather Payment",
              "Extended payment of Housing Benefit",
              "Widows Pension, Bereavement Allowance, or Widowed Parents Allowance",
              "Return to Work Payment",
              "Scottish Welfare Fund",
              "Scottish Welfare Fund",
              "Budgeting Loan from Social Fund/Budgeting Advances from Universal Credit",
              "Healthy Start Vouchers")

# Benefit names FRS ----

bencodesfrs <- sprintf("ben%02d", 1:40)

bennamesfrs <- c("State Pension",
                 "Housing Benefit",
                 "Child Benefit",
                 "Tax Credits",
                 "Employment and Support Allowance",
                 
                 "Personal Independence Payment",
                 "Disability Living Allowance",
                 "Universal Credit",
                 "Tax Credits",
                 "Council Tax Reduction",
                 
                 "Carers Allowance",
                 "Income Support",
                 "Attendance Allowance",
                 "Pension Credit",
                 "Jobseekers Allowance",
                 
                 "Winter Fuel Payment",
                 "Statutory Maternity Pay",
                 "Widows Pension, Bereavement Allowance, or Widowed Parents Allowance",
                 "Maternity Allowance",
                 "Incapacity benefit",
                 
                 "Severe Disablement Allowance",
                 "Industrial Injuries Disablement Benefit",
                 "Cold Weather Payment",
                 "Statutory sick pay",
                 "Guardians Allowance",
                 
                 "Discretionary Housing Payment",
                 "Loan or grant from DWP",
                 "Widows Pension, Bereavement Allowance, or Widowed Parents Allowance",
                 "Budgeting Loan from Social Fund/Budgeting Advances from Universal Credit",
                 "Scottish Welfare Fund",
                 
                 "Funeral Expenses Payment",
                 "Armed Forces Compensation Scheme",
                 "Scottish Welfare Fund",
                 "Return to Work Payment",
                 "War Widows/Widowers Pension",
                 
                 "Healthy Start Vouchers",
                 "Loan or grant from Local Authority",
                 "Sure Start Maternity Grant",
                 "Extended payment of Housing Benefit",
                 "Best Start Grant")

disbens <- c("Employment and Support Allowance",
             "Personal Independence Payment",
             "Disability Living Allowance",
             "Attendance Allowance",
             "Incapacity benefit",
             "Severe Disablement Allowance",
             "Industrial Injuries Disablement Benefit",
             "Carers Allowance")
             
# Tenure ----

# PTENTYP2
# 1 = Rented from Council	
# 2 = Rented from Housing Association	
# 3 = Rented privately unfurnished	
# 4 = Rented privately furnished	
# 5 = Owned outright	
# 6 = Owned with mortgage

# TENURE	
# 1	Owned outright
# 2	Owned with a mortgage
# 3	Rented from council
# 4	Rented from housing association
# 5	Private rented
# 6	Other


tenurecodes <- c(1, 2, 3, 4, 5)

tenurenames <- c("Rented from Council",
                 "Rented from Housing Association",
                 "Rented privately",
                 "Owned outright",
                 "Owned with mortgage")

# Financial management ----

finmancodes <- c(1,2,3,4,5,6)
  
finmannames <- c("Manage very well", 
                 "Manage quite well", 
                 "Get by alright", 
                 "Do not manage very well",
                 "Have some financial difficulties",
                 "Are in deep financial trouble")
