## @knitr loadData
load("../../data/pakistan/pak.rdata")
# Turn some factors into characters
pak$RiceLost <- as.character(pak$RiceLost)
pak$StagnantWater <- as.character(pak$StagnantWater)
pak$Accommodation <- as.character(pak$Accommodation)

# Put missing for blanks in water question
pak$StagnantWater <- ifelse(pak$StagnantWater == "", "Missing", pak$StagnantWater)

# shorten up names in Accomodation and clean empties
pak$Accommodation <- ifelse(pak$Accommodation == "Collective centers (school/Public building)", "Collective Centers", pak$Accommodation)
pak$Accommodation <- ifelse(pak$Accommodation == "On the site of the house (Damaged)", "Site of Damaged House", pak$Accommodation)
