## options and values ####
theme_set(theme_minimal())
pyear <- Sys.Date() %>% lubridate::year()

# IMPORT DATA ####

# import test data set
df <- readRDS("./data/cleaned_data_w_lat-long.RDS")

Preds <- read.csv('./data/FinalPreds.csv') %>% 
  janitor::clean_names() %>%
  dplyr::select(serno, tasp, scaled_pred_cap, scaled_pred, adjusted_prediction_cap, adjusted_prediction, tot_val_2022, re_res, re_agr, re_com, im_agr, pct_change)

s <- geojson_sf('./data/parcel_Residential.geojson')
x <- st_sf(s)
x$SERIAL <- ifelse(nchar(x$SERIAL) ==8, paste0('0', x$SERIAL), x$SERIAL)

IMAGE <- read.csv('./data/HouseImageLinks.csv')
IMAGE$serno <- ifelse(nchar(IMAGE$serno) ==8, paste0('0', IMAGE$serno), IMAGE$serno)

read_clean <- function(filepath){
  conflict_prefer("year", "lubridate")
  conflict_prefer("month", "lubridate")
  conflict_prefer("day", "lubridate")
  conflict_prefer("quarter", "lubridate")

  ## options and values ####
  theme_set(theme_minimal())
  pyear <- Sys.Date() %>% lubridate::year()

  ## Custom functions ####
  read_w_clean_names <- function(x){
    read_csv(x) %>% janitor::clean_names()
  }

  # IMPORT DATA ####

  # path to data set
  dat <- filepath

  # load the data
  df <- read_w_clean_names(dat)
  df %>% names

  ## combine 'deluxe' and 'luxury' baths ####
  df$fancy_baths = df$deluxe_baths_5 + df$luxury_baths_6

  ## convert to dates ####
  df <- df %>%
    mutate(sold_date = sold_date %>% as.POSIXct(format='%d-%h-%y'),
           sold_year = year(sold_date),
           sold_month = month(sold_date),
           sold_quarter = quarter(sold_date),
    )

  ## broaden categorical variables ####

  # combine quality and condition descriptions into broader categories
  # but shouldn't we model on the numeric value of this?


  # combine style descriptions into broader categories

  df <- df %>%
    mutate(style_descr_broad = case_when(style_descr %in% c("One Story",
                                                            "One and One Half") ~ "Single Story",
                                         style_descr %in% c("Bi-Level",
                                                            "Split Level",
                                                            "Two Story",
                                                            "Two and One Half",
                                                            "Bi Level 2 Story") ~ "Multi Story",
                                         TRUE ~ "Other Story"))

  ## create net sold price, subtracting concessions ####
  df <- df %>%
    mutate(net_sold_price = sold_price - concessions)


  ## determine simplified sale validity based on sale_valid codes ####
  df <- df %>%
    mutate(valid = case_when(sale_valid %in% c("Y","W") ~ TRUE,
                             sale_valid %in% c("X","N","S","B","T") ~ FALSE,
                             TRUE ~ NA))

  ## add total finished area (GLA + Fin_Bsmnt) ####
  df <- df %>%
    mutate(fin_area = gla + fin_bsmt)

  ## add total detached improvement values (less depreciation) ####
  df <- df %>%
    mutate(det_rcnld = df %>%
             select(ends_with("_rcnld")) %>%
             select(starts_with("ds_")) %>%
             rowSums()
    )



  ## add total improvement values (home and detached structures, less depreciation) ####
  df <- df %>%
    mutate(total_rcnld = det_rcnld + res_rcnld)

  # check basement values (looking for difference between total unfin and fin compared to bsmt)


  ## add "real age" ####
  df <- df %>%
    mutate(age = pyear - year_built)



  ## add logical for basement (TRUE if basement over 200 sqft) ####
  df <- df %>%
    mutate(basement = case_when(bsmt > 200 ~ TRUE,
                                TRUE ~ FALSE))

  ## add logical for stories ####
  df <- df %>%
    mutate(otherstory = case_when(style_descr_broad == "Other Story" ~ TRUE,
                                  TRUE ~ FALSE),
           multistory = case_when(style_descr_broad == "Multi Story" ~ TRUE,
                                  TRUE ~ FALSE),
           singlestory = case_when(style_descr_broad == "Single Story" ~ TRUE,
                                   TRUE ~ FALSE)) %>%
    mutate(excellent = case_when(quality_descr == "Excellent" ~ TRUE,
                                 TRUE ~ FALSE),
           verygood = case_when(quality_descr %in% c("Very Good","Very Good Plus") ~ TRUE,
                                TRUE ~ FALSE),
           good = case_when(quality_descr %in% c("Good","Good Plus") ~ TRUE,
                            TRUE ~ FALSE),
           fair = case_when(quality_descr %in% c("Fair","Fair Plus",
                                                 "Poor","Low") ~ TRUE,
                            TRUE ~ FALSE),
           average = case_when(quality_descr %in% c("Average","Average Plus",
                                                    "Average P") ~ TRUE,
                               TRUE ~ FALSE)) %>%
    mutate(bath_3_4 = full_baths_3 + full_baths_4 + three_qtr_baths_3)

  ## add splines ####
  # calculate splines for first 2000 sqft of gla and anything over that
  df <- df %>%
    mutate(spline_first2000_gla = case_when(gla <= 2000 ~ gla,
                                            TRUE ~ 2000),
           spline_above2000_gla = case_when(gla > 2000 ~ gla - 2000,
                                            TRUE ~ 0))

  ## merge low-sales-volume districts ####


  df2 <- df %>%
    # dplyr::filter(sold_year == pyear) %>%
    Valuation::merge_districts(N = 50,
                               district_id_col = "district",
                               sold_price_col = "net_sold_price",
                               res_rcnld_col = "res_rcnld",
                               rl_value_col = "rl_value",
                               acreage_col = "acreage") # function does the work

  # rejoin with full data set
  df <- left_join(df,df2)

  # convert new_district to factor
  df$new_district <- factor(df$new_district)

  ## Add boolean column indicating whether a property was sold in the current data set ####
  df <- df %>%
    mutate(sold = !is.na(net_sold_price))

  ## add transformed response variable (log net sold price) ####
  df <- df %>%
    mutate(log_net_sold_price = log10(net_sold_price))

  df %>%
    select(contains("net_sold")) %>%
    dplyr::filter(!is.na(net_sold_price)) %>%
    arrange(log_net_sold_price) # There are some $0 net sales !!!? Get rid of them

  ## convert Inf log-transformed values to NA ####
  df$log_net_sold_price[is.infinite(df$log_net_sold_price)] <- NA

  ## add time adjustment variables ####

  df <- df %>%
    mutate(syear = sold_date %>% year(),
           smonth = sold_date %>% month(),
           time_adj = (1.005)^((12*(pyear - syear))+(13-smonth)),
           yr_mon = syear + (smonth -.5)/12,
           updated = remodel_year >= (pyear-18) & remodel_pct >= .75)


  # bring in lat/lon, build distance matrix
  XY <- readRDS("./Data/lat_long_elev.RDS")
  # join xy coords and elevation with other property info
  full <- dplyr::left_join(df,XY,by=c('serno' = 'serno'))

  return(full)
}

df <- df %>%
  dplyr::distinct(serno, .keep_all = T)

MapIt <- function(data, x, grad){
  
Preds <- Preds %>%
    dplyr::mutate(serno = as.character(serno),
    serno = ifelse(nchar(serno) == 8, paste0('0', serno), serno))
  
  list_data <- as.list(data$serno)

  PARCELID <- data$serno

  STYLE <- data$style_descr
  QUALITY <- data$quality
  ADJ_SOLD_PRICE <- data$net_sold_price
  YEAR_BUILT <- data$year_built
  GLA <- data$gla
  FLA <- data$fla
  ADJ_LA <- data$gla + .7* data$fin_bsmt + .35 * data$unfin_bsmt


  dftemp <- data.frame(PARCELID, YEAR_BUILT, GLA, FLA, ADJ_LA,
                       ADJ_SOLD_PRICE, STYLE, QUALITY)
  #dftemp <- dplyr::inner_join(dftemp, IMAGE, by = c('PARCELID' = 'serno'))
  dftemp <- merge(x = dftemp, y = IMAGE, by.x = 'PARCELID', by.y = 'serno', all.x = T)
  total <- merge(x = x, dftemp, by.x="SERIAL", by.y = 'PARCELID')
  total <- merge(x= total, y = Preds, by.x = 'SERIAL', by.y = 'serno')
  
  total$psf <- (total$adjusted_prediction_cap - total$re_res - total$re_agr - total$re_com - total$im_agr) / total$ADJ_LA

  total <- total %>%
    dplyr::filter(!is.na(tasp))
  
  w <- st_bbox(total)

  xmin = as.double(w$xmin)
  xmax = as.double(w$xmax)
  xav = (xmin + xmax) /2

  ymin = as.double(w$ymin)
  ymax = as.double(w$ymax)
  yav = (ymin + ymax) /2

  qpal <- colorNumeric(
    palette = "Reds",
    domain = total$value)

  mapRes <- leaflet() %>%
  addTiles() %>%
  setView(lng = xav, lat = yav, zoom = 15) %>%
   addPolygons( data = total, smoothFactor = 0.2, fillOpacity = 1, opacity = 1,
                stroke = T, weight = .5, color = 'black', fillColor = ~qpal(get(grad)),   label = ~SERIAL,
                group = "SERIAL",
                   popup=paste(sep= "<br/>",
                               paste0("<a target='_blank', href='",total$IMAGE,"", "'>Photo Link</a>"),
                               "<b>SERIAL: </b>", total$SERIAL,
                               "<b>YEAR BUILT: </b>", total$YEAR_BUILT,
                               "<b>ADJ_LA: </b>", total$ADJ_LA,
                               "<b>PSF: </b>", total$psf,
                               "<b>SERIAL: </b>", total$SERIAL,
                               "<b>Quality: </b>",total$QUALITY,
                               "<b>Style: </b>",total$STYLE,
                               "<b>Predicted Value: </b>",total$adjusted_prediction_cap,
                               "<b>Percent_change: </b>",total$pct_change,
                               "<b>tasp: </b>",total$tasp,
                               "<b>Noccap Value: </b>",total$adjusted_prediction,
                               "<b>Last Year Value: </b>",total$tot_val_2022),
                

                   popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
  ) %>%
  leaflet.extras::addSearchFeatures(targetGroups = 'SERIAL')

return(mapRes)
}

get_image <- function(SERNO){
  # Build URL from serial number
  WEB <- paste0("https://www.utahcounty.gov/LandRecords/property.asp?av_serial=",SERNO)

  # read in html
  PAGE <- rvest::read_html(WEB)

  # Collect images, excluding the county logo
  IMAGES <-
    PAGE %>%
    rvest::html_elements("img")
  IMAGES <- IMAGES[-1]

  IMAGES <-
    IMAGES %>%
    html_attrs() %>%
    purrr::map_chr(1)

  IMAGES <- unique(IMAGES)

  # build image URL(s)
  IMAGE_URL <- paste0("https://www.utahcounty.gov/",IMAGES)
  data.frame(IMAGE = IMAGE_URL[1], serno = SERNO)
}
