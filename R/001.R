
library(data.table)
library(magrittr)
library(Hmisc)
library(stringr)

# https://www.kaggle.com/bls/american-time-use-survey
# https://www.reddit.com/r/dataisbeautiful/comments/5xodqv/how_we_spend_our_days_oc/

dt_cols <- fread(file.path('data_source', 'atussum.csv'), nrows = 0)

id_cols <- c(
  'tucaseid',
  'teage',
  'tesex',      # 1 - Male
  'trdpftpt',   # Full time (1) or part time (2) employment status of respondent
  'trernwa',    # Weekly earnings at main job (2 implied decimals)
  'tudiaryday', # Day of the week of diary day (day of the week about which the respondent was interviewed), 1 - Sunday
  'tuyear'      # Year of diary day (year of day about which respondent was interviewed) 
)
sel_cols <- names(dt_cols) %>% str_detect('^t[0-9]+') %>% { names(dt_cols)[.] }

dt <- fread(file.path('data_source', 'atussum.csv'), select = c(id_cols, sel_cols))

# Confirm no duplicated tucaseid in 'atussum.csv'
# identical(dt[, tucaseid], dt[, unique(tucaseid)])

# Activity codes here https://www.bls.gov/tus/lexiconnoex0315.pdf
lookup <- tibble::tribble(
  ~key, ~category,
  '^t0101.*',          'Sleep',                                  # Includes Sleeplessness, t010102
  '^t01[0,9][^1].*',   'Personal care',                          # All except t010101/02/99, includes t019999, so t01[0,9][^1]any number
  '^t02.*',            'Household chores',
  '^t03.*',            'Caring for household members',
  '^t04.*',            'Caring for non-hh members',
  '^t05.*',            'Work',
  '^t06.*',            'Education',
  '^t07.*',            'Consumer purchases',
  '^t08.*',            'Professional & Personal Care Services',
  '^t09.*',            'Household services',
  '^t10.*',            'Government services',
  '^t11.*',            'Eating and drinking',
  # '^t12.*',           'Socializing, Relaxing, Leisure',
  '^t12(?!030[3-8])', 'Socializing, Relaxing, Leisure',
  '^t12030[3-8]',      'TV / Radio / Computer',
  '^t13.*',            'Sports, Exercise, Recreation',
  '^t14.*',            'Religious & Spiritual activities',
  '^t15.*',            'Volunteering',
  '^t16.*',            'Telephone',
  '^t18.*',            'Travelling'
) %>% data.table()

lookup

for(i in seq_len(nrow(lookup))) {
  dt[, (lookup[i, category]) := sum(.SD), .SDcols = names(dt) %like% lookup[i, key], by = tucaseid]
}

dt[, age_bins := cut(teage, breaks = c(-Inf, seq(0, 100, by = 5), +Inf))]
dt[, age_bins]

dt_cooked <- dt[, mget(c(id_cols, lookup[, category]))]

fwrite(dt_cooked, file.path('data', 'atussum_cooked.csv'))

dt_cooked[, total := sum(.SD), .SDcols = lookup[, category]]

# Melt
dt_cooked <- melt(dt_cooked, id.vars = c(1:7))

# Remove media leisure from Leisure
dt_cooked

dt_cooked[tucaseid == 20030100013280, sum(value)]
