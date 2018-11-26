library(tidyverse)
library(plyr)
library(readr)
library(data.table)
library(ggplot2)
install.packages('Hmisc')
library(Hmisc)
df_train = read_csv("./train_users_2.csv",
                    col_types = cols(
                      timestamp_first_active = col_character()))
df_test = read_csv("./test_users.csv",
                   col_types = cols(
                     timestamp_first_active = col_character()))
labels = df_train[, c('id', 'country_destination')]
df_test$country_destination = NA
age_gender_bkts <- fread("./age_gender_bkts.csv", data.table=F)
countries <- fread("./countries.csv", data.table=F)
sample_submission_NDF <- fread("./sample_submission_NDF.csv", data.table=F)
sessions <- fread("./sessions.csv", data.table=F)

df_train$dataset <- "train"
df_test$dataset <- "test"
df_all = rbind(df_train, df_test)
df_train %>% head()
df_test %>% head()
colnames(df_test)

df_all <- df_all %>%
  mutate(
    age_cln = ifelse(age >= 1920, 2015 - age, age),
    age_cln2 = ifelse(age_cln < 14 | age_cln > 100, -1, age_cln),
    age_bucket = cut(age, breaks = c(min(age_cln), 4, 9, 14, 19, 24,
                                     29, 34, 39, 44, 49, 54,
                                     59, 64, 69, 74, 79, 84,
                                     89, 94, 99, max(age_cln)
    )),
    age_bucket = mapvalues(age_bucket,
                           from=c("(1,4]", "(4,9]", "(9,14]", "(14,19]",
                                  "(19,24]", "(24,29]", "(29,34]", "(34,39]",
                                  "(39,44]", "(44,49]", "(49,54]", "(54,59]",
                                  "(59,64]", "(64,69]", "(69,74]", "(74,79]",
                                  "(79,84]", "(84,89]", "(89,94]", "(94,99]", "(99,150]"),
                           to=c("0-4", "5-9", "10-14", "15-19",
                                "20-24", "25-29", "30-34", "35-39",
                                "40-44", "45-49", "50-54", "55-59",
                                "60-64", "65-69", "70-74", "75-79",
                                "80-84", "85-89", "90-94", "95-99", "100+"))
  )

View(df_all)
df_all %>% colnames()

df_all <- df_all %>%
  separate(date_account_created, into = c("dac_year", "dac_month", "dac_day"), sep = "-", remove=FALSE) %>%
  mutate(
    dac_yearmonth = paste0(dac_year, dac_month),
    dac_yearmonthday = as.numeric(paste0(dac_year, dac_month, dac_day)),
    dac_week = as.numeric(format(date_account_created+3, "%U")),
    dac_yearmonthweek = as.numeric(paste0(dac_year, dac_month, formatC(dac_week, width=2, flag="0"))),
    tfa_year = str_sub(timestamp_first_active, 1, 4),
    tfa_month = str_sub(timestamp_first_active, 5, 6),
    tfa_day = str_sub(timestamp_first_active, 7, 8),
    tfa_yearmonth = str_sub(timestamp_first_active, 1, 6),
    tfa_yearmonthday = as.numeric(str_sub(timestamp_first_active, 1, 8)),
    tfa_date = as.Date(paste(tfa_year, tfa_month, tfa_day, sep="-")),
    tfa_week = as.numeric(format(tfa_date+3, "%U")),
    tfa_yearmonthweek = as.numeric(paste0(tfa_year, tfa_month, formatC(tfa_week, width=2, flag="0"))),
    dac_lag = as.numeric(date_account_created - tfa_date),
    dfb_dac_lag = as.numeric(date_first_booking - date_account_created),
    dfb_dac_lag_cut = as.character(cut2(dfb_dac_lag, c(0, 1))),
    dfb_dac_lag_flg = as.numeric(as.factor(ifelse(is.na(dfb_dac_lag_cut)==T, "NA", dfb_dac_lag_cut))) - 1,
    dfb_tfa_lag = as.numeric(date_first_booking - tfa_date),
    dfb_tfa_lag_cut = as.character(cut2(dfb_tfa_lag, c(0, 1))),
    dfb_tfa_lag_flg = as.numeric(as.factor(ifelse(is.na(dfb_tfa_lag_cut)==T, "NA", dfb_tfa_lag_cut))) - 1
  )

df_all$dfb_dac_lag_cut
df_all$dfb_dac_lag
df_all$dfb_dac_lag_flg
df_all$dfb_tfa_lag
df_all$dfb_tfa_lag_flg
df_all$dfb_tfa_lag_cut


countries <- mutate(countries,
                           language = str_sub(destination_language, 1, 2))
countries %>% head()
df_all <- df_all %>%
  mutate(country_destination = country_destination) %>%
  left_join(., countries[c("language", "country_destination", "distance_km", "destination_km2", "language_levenshtein_distance")],
                   by = c("language", "country_destination"))
df_all %>% head()
View(df_all)
countries$language <- NULL

df_train <- subset(df_all, dataset == "train")
df_train <- df_train %>%
  dplyr::mutate(dataset = ifelse(dac_yearmonth %nin% c("201404", "201405", "201406"), "train", "valid"))

df_test <- subset(df_all, dataset == "test")
df_all = rbind(df_train, df_test)

num_feats <- c(
  "age_cln",
  "age_cln2",
  "dac_year",
  "dac_month",
  "dac_yearmonth",
  "dac_yearmonthday",
  "dac_yearmonthweek",
  "dac_day",
  "dac_week",
  "tfa_year",
  "tfa_month",
  "tfa_yearmonth",
  "tfa_yearmonthday",
  "tfa_yearmonthweek",
  "tfa_day",
  "tfa_week"#,
  # "dac_lag",
  # "dfb_dac_lag",
  # "dfb_tfa_lag"
)
df_all_num_feats <- list()
i <- 1
for(feat in num_feats){
  df_all_num_feats_ <- df_all[c("id", feat)]
  df_all_num_feats_$feature <- feat
  df_all_num_feats_$value <- as.numeric(df_all_num_feats_[[feat]])
  df_all_num_feats_ <- df_all_num_feats_[c("id", "feature", "value")]
  df_all_num_feats[[i]] <- df_all_num_feats_
  i <- i + 1
}
df_all_num_feats
df_all_num_feats <- bind_rows(df_all_num_feats)
print("numeric feature")
print(n_distinct(df_all_num_feats$feature))

ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
df_all_ohe_feats <- list()
i <- 1
n_feats <- 0
for(feat in ohe_feats){
  df_all_ohe_feats_ <- df_all[c("id", feat)]
  df_all_ohe_feats_$feature <- paste(feat, df_all_ohe_feats_[[feat]], sep="_")
  n_feats_ <- n_distinct(df_all_ohe_feats_$feature)
  df_all_ohe_feats_$value <- 1
  df_all_ohe_feats_ <- df_all_ohe_feats_[c("id", "feature", "value")]
  df_all_ohe_feats[[i]] <- df_all_ohe_feats_
  i <- i + 1
  n_feats <- n_feats + n_feats_
}
df_all_ohe_feats <- bind_rows(df_all_ohe_feats)
print("categorical feature")
print(n_feats)
df_all_ohe_feats
unique(df_all_ohe_feats$value)

countries <- dplyr::mutate(countries,
                           country_language = paste0(country_destination, "_", destination_language))
countries_reshape <- data.frame()
for(i in unique(countries$country_language)){
  # i <- "AU_eng"
  countries_ <- subset(countries, country_language == i)
  print(countries_)
  countries_$country_language <- NULL
  countries_ <- reshape(countries_,
                        direction='wide',
                        idvar='destination_language',
                        timevar='country_destination')
  countries_reshape <- bind_rows(
    countries_reshape,
    countries_
  )
}
countries_reshape
