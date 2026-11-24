# Fetch Agricultural Production data from SWS (agriculture / aproduction) and
# write three FAOSTAT-bulk-shaped CSVs that the Top10_AP app can read:
#   <prefix>_crop_data_<y1>_<y2>.csv       (crops primary,     element 5510)
#   <prefix>_livestock_data_<y1>_<y2>.csv  (livestock primary, 5510 / 5513)
#   <prefix>_stock_data_<y1>_<y2>.csv      (live animals,      5111 / 5112 / 5114)
#
# Run this script to fetch/refresh the data; the app itself never calls SWS,
# it only reads the CSV files in this folder.
#
# Item/element pairs, item names and area names are taken from the reference
# FAOSTAT files, so the SWS output is like-for-like with the FAOSTAT download.
#
# Requires: FAO VPN, and your SWS server/token in sws.yml in this folder.
# Packages: data.table, yaml, faosws (faosws is on the FAO R repository:
#   install.packages("faosws", repos = "https://rstudiopm.fao.org/fao-sws-cran/latest"))
#
# Usage (from this folder):
#   Rscript fetch_sws_ap.R [--years 2021:2024] [--prefix SWS] [--out-dir .]
#                          [--yml sws.yml]
#                          [--ref-crop Faostat_crop_data_2021_2024.csv]
#                          [--ref-livestock Faostat_livestock_data_2021_2024.csv]
#                          [--ref-stock Faostat_stock_data_2021_2024.csv]

suppressPackageStartupMessages({
  library(data.table)
  library(faosws)
})

# ---- Arguments ----
args <- commandArgs(trailingOnly = TRUE)
opt <- list(
  years         = "2021:2024",
  prefix        = "SWS",
  `out-dir`     = ".",
  yml           = "sws.yml",
  `ref-crop`      = "Faostat_crop_data_2021_2024.csv",
  `ref-livestock` = "Faostat_livestock_data_2021_2024.csv",
  `ref-stock`     = "Faostat_stock_data_2021_2024.csv"
)
for (i in seq_len(length(args) %/% 2) * 2 - 1) {
  name <- sub("^--", "", args[i])
  if (!name %in% names(opt)) stop("Unknown argument: ", args[i])
  opt[[name]] <- args[i + 1]
}
years <- as.character(eval(parse(text = opt$years)))
y_lab <- paste(range(as.integer(years)), collapse = "_")

DOMAIN  <- "agriculture"
DATASET <- "aproduction"

UNITS <- c("5510" = "t", "5513" = "1000 No",
           "5111" = "An", "5112" = "1000 An", "5114" = "No")
ELEMENT_NAMES <- c("5510" = "Production", "5513" = "Production",
                   "5111" = "Stocks", "5112" = "Stocks", "5114" = "Stocks")

# Aggregate China codes in SWS: never pulled, to avoid double counting with
# 156 (mainland), 158 (Taiwan), 344 (Hong Kong), 446 (Macao).
CHINA_AGGREGATES <- c("157", "159", "159.03", "1248", "1249")

# ---- Connect ----
settings <- yaml::read_yaml(opt$yml)
GetTestEnvironment(baseUrl = settings[["server"]], token = trimws(settings[["token"]]))

# ---- Reference FAOSTAT files: item/element pairs and names ----
read_ref <- function(f) {
  d <- fread(f, colClasses = list(character = c("Item Code (CPC)", "Area Code (M49)",
                                                "Element Code")),
             encoding = "UTF-8")
  setnames(d, 1, sub("^﻿", "", names(d)[1]))
  d
}
ref <- list(crop      = read_ref(opt$`ref-crop`),
            livestock = read_ref(opt$`ref-livestock`),
            stock     = read_ref(opt$`ref-stock`))

all_ref   <- rbindlist(ref)
item_name <- unique(all_ref[, .(cpc = `Item Code (CPC)`, Item)], by = "cpc")
area_name <- unique(all_ref[, .(m49 = as.character(as.integer(`Area Code (M49)`)), Area)],
                    by = "m49")

# ---- SWS code lists ----
geo <- GetCodeList(DOMAIN, DATASET, "geographicAreaM49")
geo <- geo[type == "country" & !code %in% CHINA_AGGREGATES]
sws_items <- GetCodeList(DOMAIN, DATASET, "measuredItemCPC")[, .(code, description)]

# Keep only areas published in the reference FAOSTAT files, with FAOSTAT names.
# 156 must never be called plain "China" (the app drops Area == "China").
geo <- merge(geo[, .(m49 = code)], area_name, by = "m49")
geo[m49 == "156", Area := "China, mainland"]

# ---- Fetch ----
fetch <- function(pairs) {
  # pairs: data.table(cpc, element) from the reference file
  out <- list()
  for (el in unique(pairs$element)) {
    items <- intersect(pairs[element == el, cpc], sws_items$code)
    missing <- setdiff(pairs[element == el, cpc], sws_items$code)
    if (length(missing)) message("  CPC not in SWS code list (skipped): ",
                                 paste(missing, collapse = ", "))
    for (chunk in split(geo$m49, ceiling(seq_along(geo$m49) / 60))) {
      key <- DatasetKey(domain = DOMAIN, dataset = DATASET, dimensions = list(
        geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = chunk),
        measuredElement   = Dimension(name = "measuredElement",   keys = el),
        measuredItemCPC   = Dimension(name = "measuredItemCPC",   keys = items),
        timePointYears    = Dimension(name = "timePointYears",    keys = years)))
      out[[length(out) + 1]] <- GetData(key, flags = TRUE)
    }
  }
  d <- rbindlist(out, use.names = TRUE)
  # keep only the exact item/element pairs FAOSTAT publishes for this file
  d[pairs, on = .(measuredItemCPC = cpc, measuredElement = element), nomatch = NULL]
}

# SWS (observation status, method) -> FAOSTAT single flag. Derived by
# cross-tabulating SWS flags against the FAOSTAT 2021-2024 download:
#   ""(official, any method) -> A ; T -> X ; E -> E ; I,f -> E ; I,other -> I ; M -> M
faostat_flag <- function(obs, meth) {
  fifelse(obs == "", "A",
  fifelse(obs == "T", "X",
  fifelse(obs == "E", "E",
  fifelse(obs == "I" & meth == "f", "E",
  fifelse(obs == "I", "I",
  fifelse(obs == "M", "M", obs))))))
}
FLAG_DESC <- c(A = "Official figure",
               X = "Figure from external organization",
               E = "Estimated value",
               I = "Value imputed by a receiving agency",
               M = "Missing value; data cannot exist")

to_faostat <- function(d) {
  # keep missing (M) rows with empty Value, as FAOSTAT does; drop other empties
  d <- d[!is.na(Value) | flagObservationStatus == "M"]
  d[flagObservationStatus == "M", Value := NA_real_]
  d[, Flag := faostat_flag(flagObservationStatus, flagMethod)]
  d <- merge(d, geo[, .(m49, Area)], by.x = "geographicAreaM49", by.y = "m49")
  d <- merge(d, item_name, by.x = "measuredItemCPC", by.y = "cpc", all.x = TRUE)
  d[is.na(Item), Item := sws_items$description[match(measuredItemCPC, sws_items$code)]]
  out <- d[, .(
    `Domain Code`      = "QCL",
    Domain             = "Crops and livestock products",
    `Area Code (M49)`  = sprintf("%03d", as.integer(geographicAreaM49)),
    Area,
    `Element Code`     = measuredElement,
    Element            = ELEMENT_NAMES[measuredElement],
    `Item Code (CPC)`  = measuredItemCPC,
    Item,
    `Year Code`        = timePointYears,
    Year               = timePointYears,
    Unit               = UNITS[measuredElement],
    Value,
    Flag,
    `Flag Description` = FLAG_DESC[Flag],
    Note               = ""
  )]
  setorder(out, Area, `Item Code (CPC)`, `Element Code`, Year)
  out
}

files <- c(crop = "crop", livestock = "livestock", stock = "stock")
for (nm in names(files)) {
  pairs <- unique(ref[[nm]][, .(cpc = `Item Code (CPC)`, element = `Element Code`)])
  message(sprintf("Fetching %s: %d items, elements %s, years %s ...", nm,
                  uniqueN(pairs$cpc), paste(unique(pairs$element), collapse = "/"),
                  paste(years, collapse = ",")))
  out <- to_faostat(fetch(pairs))
  f <- file.path(opt$`out-dir`, sprintf("%s_%s_data_%s.csv", opt$prefix, files[[nm]], y_lab))
  fwrite(out, f, quote = TRUE)
  message(sprintf("  wrote %s (%d rows, %d areas, %d items)", f, nrow(out),
                  uniqueN(out$Area), uniqueN(out$Item)))
}
