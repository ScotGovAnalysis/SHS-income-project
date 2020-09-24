
# Colours

wes_palettes <- list(
  BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707"),
  BottleRocket2 = c("#FAD510", "#CB2314", "#273046", "#354823", "#1E1E1E"),
  Rushmore1 = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
  Rushmore = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
  Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B"),
  Royal2 = c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089"),
  Zissou1 = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),
  Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
  Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
  Chevalier1 = c("#446455", "#FDD262", "#D3DDDC", "#C7B19C"),
  FantasticFox1 = c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20"),
  Moonrise1 = c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A"),
  Moonrise2 = c("#798E87", "#C27D38", "#CCC591", "#29211F"),
  Moonrise3 = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B"),
  Cavalcanti1 = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"),
  GrandBudapest1 = c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236"),
  GrandBudapest2 = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4"),
  IsleofDogs1 = c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3", "#8D8680"),
  IsleofDogs2 = c("#EAD3BF", "#AA9486", "#B6854D", "#39312F", "#1C1718")
)

cols_survey <- wes_palettes[["Moonrise2"]]
names(cols_survey) = c("HBAI", "SHS", "Admin")

cols_types <- c(wes_palettes[["IsleofDogs1"]], "grey20")
names(cols_types) <- inctypes[-1]

cols_types2 <- c(wes_palettes[["FantasticFox1"]], 
                 wes_palettes[["Rushmore1"]], 
                 wes_palettes[["GrandBudapest1"]],
                 wes_palettes[["Zissou1"]],
                 wes_palettes[["Cavalcanti1"]],
                 wes_palettes[["Moonrise1"]])

# show_col(cols_types2)

cols_bens <- cols_types2[c(3, 5, 2, 8, 10, 
                           6, 16, 12, 1, 21,
                           22, 13, 27, 24, 9)]
# show_col(cols_bens)

 show_col(wes_palettes[[6]])
 
 cols_finman <- c("#0B775E", "#74A089", "#899DA4", "#FAEFD1", "#DC863B", "#C93312")
#show_col(cols_finman)
 