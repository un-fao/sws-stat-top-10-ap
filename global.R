library(shiny)
library(data.table)
library(DT)
# library(formattable)
# library(scales)
library(openxlsx)

# ---- Base year and data folder ----
# The app compares base_year with base_year - 1, using SWS files covering
# base_year - 3 .. base_year (SWS_<type>_data_<y1>_<y2>.csv, from fetch_sws_ap.R).
# Files are read from the shared drive folder if present there, else from this folder.
shared_data_dir <- Sys.getenv("TOP10_DATA_DIR", "/sws-shared-drive/top-10-ap")
data_types <- c("crop", "livestock", "stock")
data_file <- function(type, base) sprintf("SWS_%s_data_%d_%d.csv", type, base - 3L, base)
data_dir_for <- function(base) {
  for (d in c(shared_data_dir, ".")) {
    if (all(file.exists(file.path(d, data_file(data_types, base))))) return(d)
  }
  NA_character_
}
available_base_years <- function() {
  f <- list.files(c(shared_data_dir, "."), pattern = "^SWS_crop_data_[0-9]{4}_[0-9]{4}[.]csv$")
  b <- as.integer(sub(".*_([0-9]{4})[.]csv$", "\\1", f))
  b <- b[as.integer(sub(".*_([0-9]{4})_[0-9]{4}[.]csv$", "\\1", f)) == b - 3L]
  sort(unique(b[!is.na(sapply(b, data_dir_for))]), decreasing = TRUE)
}
if (!exists("base_year")) base_year <- available_base_years()[1]
data_dir <- data_dir_for(base_year)
base_col <- as.character(base_year)        # e.g. "2025"
prev_col <- as.character(base_year - 1L)   # e.g. "2024"
year_cols <- as.character((base_year - 3L):base_year)
# make sure all four year columns exist (a year with no data in SWS becomes NA)
add_year_cols <- function(d) {
  for (y in setdiff(year_cols, names(d))) set(d, j = y, value = NA_real_)
  setcolorder(d, c(setdiff(names(d), year_cols), year_cols))
  d
}
# d_p = read.csv("FAOSTAT_data_en_11-23-2023_crops.csv")
# DOWNLOAD DATA
#read.csv('Faostat_crop_data_2020_2023.csv', sep = ";")
data_crops = read.csv(file.path(data_dir, data_file('crop', base_year)), sep = ",", colClasses = c(Item.Code..CPC. = "character")) # Crops primary
setDT(data_crops)
#data_crops[, X := NULL]

data_crops$Area.Code..M49. <- as.integer(data_crops$Area.Code..M49.)
data_crops$Element.Code <- as.integer(data_crops$Element.Code)
data_crops$Year.Code <- as.integer(data_crops$Year.Code)
data_crops$Year <- as.character(data_crops$Year)
data_crops$Note <- as.character(data_crops$Note)

#data_crops$Value <- gsub(',', '.', data_crops$Value)
data_crops$Value <- as.numeric(data_crops$Value)


data_livestock = read.csv(file.path(data_dir, data_file('livestock', base_year)), sep = ",",colClasses = c(Item.Code..CPC. = "character")) # Livestock primary
setDT(data_livestock)
#data_livestock[, X := NULL]

data_livestock$Area.Code..M49. <- as.integer(data_livestock$Area.Code..M49.)
data_livestock$Element.Code <- as.integer(data_livestock$Element.Code)
data_livestock$Year.Code <- as.integer(data_livestock$Year.Code)
data_livestock$Year <- as.character(data_livestock$Year)
data_livestock$Note <- as.character(data_livestock$Note)

#data_livestock$Value <- gsub(',', '.', data_livestock$Value)
data_livestock$Value <- as.numeric(data_livestock$Value)

data_liveanimals = read.csv(file.path(data_dir, data_file('stock', base_year)), sep = ",",colClasses = c(Item.Code..CPC. = "character")) # Live animals
setDT(data_liveanimals)
#data_liveanimals[, X := NULL]

data_liveanimals$Area.Code..M49. <- as.integer(data_liveanimals$Area.Code..M49.)
data_liveanimals$Element.Code <- as.integer(data_liveanimals$Element.Code)
data_liveanimals$Year.Code <- as.integer(data_liveanimals$Year.Code)
data_liveanimals$Year <- as.character(data_liveanimals$Year)
data_liveanimals$Note <- as.character(data_liveanimals$Note)

#data_liveanimals$Value <- gsub(',', '.', data_liveanimals$Value)
data_liveanimals$Value <- as.numeric(data_liveanimals$Value)

# library(readxl)
static_list = read.xlsx('selected commodities.xlsx') # Selected commodities for static analysis

#static_list <- read.table(file = "selected commodities.txt", 
#                      sep = "\t", header=TRUE)
setDT(static_list)

static_list_liveanimal = read.xlsx('selected live animals.xlsx') # Selected live animals for static analysis
#static_list_liveanimal <- read.table(file = "selected live animals.txt", 
#                          sep = "\t", header=TRUE)
setDT(static_list_liveanimal)

# PREPARE DATA
data_binded = rbind(data_crops,data_livestock) # Crops and Livestock primary merged

# Please note that summing all the countries is not the best practise to calculate the World value.
# This is due to the duplication of values in case of China.
# For this reason, we need to clean the China(total) from the data.
data_binded = data_binded[Area!='China',]
data_liveanimals = data_liveanimals[Area!='China',]
# The primary data should be only the item in tonnes 
data_binded = data_binded[Unit=='t',]

# setnames(data_crops, 'Item.Code..CPC.', 'Item.Code')
# setnames(data_livestock, 'Item.Code..CPC.', 'Item.Code')
setnames(data_liveanimals, 'Item.Code..CPC.', 'Item.Code')
setnames(data_binded, 'Item.Code..CPC.', 'Item.Code')

# COUNTRY PRIMARY ANALYSIS FOR DYNAMIC
country_crop <- data_binded[, lapply(.SD, sum, na.rm=TRUE), by = .(Area,Year,Item.Code,Item), .SDcols = c('Value')]
country_crop2 <- dcast.data.table(country_crop, Area + Item.Code + Item ~ Year, value.var = list('Value'))
country_crop2 <- add_year_cols(country_crop2)
country_crop3 <- country_crop2[, `:=`(Diff = abs(get(base_col)-get(prev_col)), `% change` = ((get(base_col)-get(prev_col))/get(prev_col)))]
# country_crop4 <- country_crop3[,`:=`(`% change` = percent(Change))]
# country_crop4 <- country_crop4[, Change:= NULL]

# WORLD PRIMARY ANALYSIS FOR DYNAMIC
world_crop <- country_crop[, lapply(.SD, sum, na.rm=TRUE), by= .(Year,Item.Code,Item), .SDcols = c('Value')]
world_crop2 <- dcast.data.table(world_crop, Item.Code + Item ~ Year, value.var = list('Value'))
world_crop2 <- add_year_cols(world_crop2)
world_crop2 <- world_crop2[, `:=`(Diff = abs(get(base_col)-get(prev_col)), `% change` = ((get(base_col)-get(prev_col))/get(prev_col)))]

world_crop2 <- world_crop2[,][order(-get(base_col))]
world_crop3 <- world_crop2[, head(.SD, 20)]
#mainItemList <- unique(world_crop3$Item)

# COUNTRY LIVE ANIMAL ANALYSIS FOR DYNAMIC
country_liveanimal <- data_liveanimals[, lapply(.SD, sum, na.rm=TRUE), by = .(Area,Year,Item.Code,Item), .SDcols = c('Value')]
country_liveanimal2 <- dcast.data.table(country_liveanimal, Area + Item.Code + Item ~ Year, value.var = list('Value'))
country_liveanimal2 <- add_year_cols(country_liveanimal2)
country_liveanimal3 <- country_liveanimal2[, `:=`(Diff = abs(get(base_col)-get(prev_col)), `% change` = ((get(base_col)-get(prev_col))/get(prev_col)))]

# WORLD LIVE ANIMAL ANALYSIS FOR DYNAMIC
world_liveanimal <- country_liveanimal[, lapply(.SD, sum, na.rm=TRUE), by= .(Year,Item.Code,Item), .SDcols = c('Value')]
world_liveanimal2 <- dcast.data.table(world_liveanimal, Item.Code + Item ~ Year, value.var = list('Value'))
world_liveanimal2 <- add_year_cols(world_liveanimal2)
world_liveanimal2 <- world_liveanimal2[, `:=`(Diff = abs(get(base_col)-get(prev_col)), `% change` = ((get(base_col)-get(prev_col))/get(prev_col)))]

world_liveanimal2 <- world_liveanimal2[,][order(-get(base_col))]
world_liveanimal3 <- world_liveanimal2[, head(.SD, 20)]
#mainItemList_animal <- unique(world_liveanimal3$Item)

# COUNTRY PRIMARY ANALYSIS FOR STATIC
country_crop3_stat <- country_crop3[Item.Code %in% static_list$Item.Code,]

# WORLD PRIMARY ANALYSIS FOR STATIC
world_crop2_stat <- world_crop2[Item.Code %in% static_list$Item.Code,][order(-get(base_col))]
world_crop3_stat <- world_crop2_stat[, head(.SD, 20)]

# COUNTRY LIVE ANIMAL ANALYSIS FOR STATIC
country_liveanimal3_stat <- country_liveanimal3[Item.Code %in% static_list_liveanimal$Item.Code,]

# WORLD LIVE ANIMAL ANALYSIS FOR STATIC
world_liveanimal2_stat <- world_liveanimal2[Item.Code %in% static_list_liveanimal$Item.Code,][order(-get(base_col))]
world_liveanimal3_stat <- world_liveanimal2_stat[, head(.SD, 20)]

# GOING ON TO COUNTRY ANALYSIS

# createTable <- function(df=country_crop2){
#   listofdfs <- list()
#   
#   for (i in 1:length(mainItemList)){
#     dfs <- country_crop2[Item %in% mainItemList[i]][order(-`2020`)]
#     dfs <- dfs[, head(.SD, 5)]
#     listofdfs[[i]] <- dfs
#   }
#   return(listofdfs)
#   
# }
# 
# country_crop3 <- createTable(country_crop2)
# country_crop3[[1]]
# 
# xx <- country_crop2[Item == 'Sugar cane',][order(-`2020`)]
# a <- xx[, head(.SD, 5)]

# nameOfCountries <- function(ls=country_crop3){
#   names <- list()
#   
#   for (i in 1:length(ls)){
#     names <- append(names,as.list(levels(ls[[1]]$Area)))
#   }
#   return(names)
# }
# 
# countryList <- nameOfCountries(country_crop3)
 
# country_crop2 <- world_crop2[, `:=`(Diff = abs(`2020`-`2017`), `% change` = ((`2020`-`2017`)/`2017`))][order(-`2020`)]
# country_crop2 <- country_crop[Year==2020, head(.SD, 20)]