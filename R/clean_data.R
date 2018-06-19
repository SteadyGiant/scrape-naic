library(magrittr)
library(readxl)
library(tidyverse)

paths = str_c('data/raw/', list.files(path = 'data/raw'))

tables = map(.x = paths,
             .f = ~ read_excel(path = .x))

for (i in 1:length(tables)) {

  names(tables[[i]]) =
    str_replace(string = names(tables[[i]]), pattern = 'ACITIVITY',
                replacement = 'ACTIVITY')

  tables[[i]] %<>%
    mutate(years = str_extract_all(`RELATED STATE ACTIVITY`,
                                   '(?<=\\()[[:digit:]].*?(?=\\))') %>%
             str_replace_all(pattern = 'character\\(0\\)|c\\(|\\"|\\)',
                             replacement = ''))

  num_years = ifelse(
    all(is.na(tables[[i]]$years)),
    1,
    max(str_count(string = tables[[i]]$years, pattern = '\\,'),
        na.rm = TRUE) + 1)

  tables[[i]] %<>%
    separate(col = years,
             into = str_c('year', 1:num_years),
             sep = '\\, ') %>%
    mutate_all(.funs = funs(ifelse(is.na(.), '', .)))

}

# Write out tables as Excel spreadsheets.
for (table in tables) {

  doc = table %>%
    select(document) %>%
    unique() %>%
    as.character()

  openxlsx::write.xlsx(
    x = table,
    file = str_c('data/clean/', doc, '.xlsx'))

}
