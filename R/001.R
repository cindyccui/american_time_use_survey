
library(data.table)
library(magrittr)
library(Hmisc)
library(stringr)
library(ggplot2)

# https://www.kaggle.com/bls/american-time-use-survey
# https://www.reddit.com/r/dataisbeautiful/comments/5xodqv/how_we_spend_our_days_oc/



# Check col names ---------------------------------------------------------

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
cat_cols <- names(dt_cols) %>% str_detect('^t[0-4]+') %>% { names(dt_cols)[.] }



# Read data ---------------------------------------------------------------

dt <- fread(file.path('data_source', 'atussum.csv'), select = c(id_cols, cat_cols)) %>%
  setnames(
    c('tucaseid', 'teage', 'tesex', 'trdpftpt', 'trernwa', 'tudiaryday', 'tuyear'),
    c('ID','Age','Gender','Empl._status','Weekly_salary','Day_of_week','Year')
  )

# Confirm no duplicated IDs in 'atussum.csv'
# identical(dt[, ID], dt[, unique(ID)])



# Process activities ------------------------------------------------------

# Activity codes here https://www.bls.gov/tus/lexiconnoex0315.pdf
lookup <- tibble::tribble(
  ~key, ~category,
  '^t0101.*',          'Sleep',                                  # Includes Sleeplessness, t010102
  '^t01(?!01).*',      'Personal care',                          # All except starting with t0101, all t01... included
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
  '^t12(?!030[3-8])',  'Socializing, Relaxing, Leisure',
  '^t12030[3-8]',      'TV / Radio / Computer',
  '^t13.*',            'Sports, Exercise, Recreation',
  '^t14.*',            'Religious & Spiritual activities',
  '^t15.*',            'Volunteering',
  '^t16.*',            'Telephone',
  '^t18.*',            'Travelling'
) %>% data.table()

# Check regexes
lapply(lookup %>% t() %>% data.table, function(x) {
  grep(x[1], names(dt), value = TRUE, perl = TRUE)
})

# Set args for grep
perl_grep <- function(x, y) grep(x, y, value = TRUE, perl = TRUE)

for(i in seq_len(nrow(lookup))) {
  # dt[, .SD, .SDcols = perl_grep(lookup[i, key], names(dt))] %>% print()
  dt[, (lookup[i, category]) := rowSums(.SD), .SDcols = perl_grep(lookup[i, key], names(dt))]
  dt[, (perl_grep(lookup[i, key], names(dt))) := NULL]
}



# Rescale to 24h ----------------------------------------------------------

dt[, Total := rowSums(.SD), .SDcols = lookup[, category]]
ggplot(dt, aes(x = Total)) + geom_histogram(bins = 100) + theme_classic()

# Rescale
dt[, prop.table(table(Total == 1440))]  # 24h
dt[Total != 1440, (lookup[, category]) := .SD * 1440/Total, .SDcols = lookup[, category]]

dt[, Total_rescaled := rowSums(.SD), .SDcols = lookup[, category]]
ggplot(dt, aes(x = Total_rescaled)) + geom_histogram(bins = 100) + theme_classic()

ggplot(dt[Work >= 60, ], aes(x = Work)) + geom_histogram(binwidth = 15) + theme_minimal()

dt[, Working_day := as.numeric(Work >= 60)]
mdmisc::reorder_columns(dt, 'Working_day after Day_of_week')



# Melt --------------------------------------------------------------------

dt_melted <- melt(
  dt[, !c('Total', 'Total_rescaled')], id.vars = c(1:8),
  variable.name = 'Category', value.name = 'Minutes',
  variable.factor = FALSE)

dt_melted_aggr <- dt_melted[, .(Minutes = mean(Minutes)), by = .(Age, Day_of_week, Working_day, Year, Category)]

# By Age ------------------------------------------------------------------

# dt_melted[Age == 15, .(Avg_mins = mean(Minutes)), by = c('Age', 'Category')] %>%
#   .[, Category := str_trim(Category)] %>% setorder(Age, Category) %>% .[, sum(Avg_mins)] %>% print()

#    Age                              Category     Avg_mins
# 1:  15          Caring for household members   6.40444287
# 2:  15             Caring for non-hh members   6.69300096
# 3:  15                    Consumer purchases  19.59461602
# 4:  15                   Eating and drinking  54.52740070
# 5:  15                             Education 181.78777092

# From https://www.kaggle.com/halhen/d/bls/american-time-use-survey/what-we-give-up-for-work
# 1   15          Caring for household members   6.34214391
# 2   15             Caring for non-hh members   6.64757709
# 3   15                    Consumer purchases  19.34459129
# 4   15                   Eating and drinking  54.06265296
# 5   15                             Education 180.80518845



# Save --------------------------------------------------------------------

fwrite(dt,             file.path('data', 'atussum_cooked.csv'))
fwrite(dt_melted,      file.path('data', 'atussum_cooked_melted.csv'))
fwrite(dt_melted_aggr, file.path('data', 'atussum_cooked_melted_aggr.csv'))


