library(tibble)
library(readr)
library(tidyverse)
download.file('https://royallib.com/get/txt/Townsend_Sue/The_Secret_Diary_of_Adrian_Mole_Aged_13_34.zip',
              destfile = 'adrian.zip')
zipF<-file.choose()
outDir<-getwd()
unzip(zipF,exdir=outDir)
adrian <- read_lines(file.choose(), skip = 11)
adrian <- tibble(text = adrian)
x <- adrian %>% 
  mutate(len = unlist(map(str_extract_all(text, boundary('word')), length)),
         digit = str_detect(text, '[:digit:]$'))
pos <- as.numeric(rownames(x)[which(x$len == 3 & x$digit == TRUE)])
value_date <- x %>% 
  filter(len == 3, digit == TRUE)
date_list <- value_date$text %>% 
  str_subset('[:digit:]$') %>% 
  str_extract_all(boundary('word'))
year_rep <- rep(c(1981, 1982), c(369, 93))
library(lubridate)
value_date <- date_list %>% 
  map(function(x) paste(x[2], x[3])) %>%
  unlist() %>% 
  paste(year_rep) %>% 
  parse_date_time('%b %d %Y')
value_weekday <- date_list %>% 
  map(function(x) x[1]) %>%
  unlist()

x$date <- as.Date(NA)
x$date[pos] <- value_date
x$weekday <- as.character(NA)
x$weekday[pos] <- value_weekday
x$weekday[c(2431, 2433, 2439, 2443)] <- NA
x$text[pos] <- ""
x <-  x %>% 
  select(-len, -digit)

wday_sort <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
df <- x %>%
  fill(date, weekday) %>%
  group_by(date, weekday) %>% 
  summarise(senten = paste(text, collapse = ' ')) %>% 
  ungroup() %>% 
  mutate(weekday = factor(weekday, levels = wday_sort))
df$senten <- df$senten %>% 
  iconv("UTF-8", "ASCII", "'")
head(df)
library(tidytext)
df$senten <- df$senten %>% 
  str_replace_all(pattern = "[[:punct:]]|[[:digit:]]", replacement = "") %>%
  tolower()
df_tidy <- df %>% 
  filter(!is.na(date)) %>% 
  unnest_tokens(word, senten) %>%
  filter(!nchar(word) < 3) %>%
  anti_join(stop_words)

df_tidy %>% 
  group_by(weekday) %>% 
  ggplot(aes(x = weekday)) + geom_bar() + coord_flip()
word_len <- df_tidy %>% 
  group_by(weekday) %>% 
  count(word, sort = T) %>% 
  arrange(desc(n)) %>%
  slice(seq_len(7)) %>%
  ungroup() %>% 
  arrange(weekday, n) %>%
  mutate(row = row_number())

ggplot(word_len, aes(x = row, y = n, fill = weekday)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~weekday, scales = "free_y") + 
  coord_flip() +
  scale_x_continuous(breaks = word_len$row, labels = word_len$word) +
  xlab(NULL) + 
  ylab(NULL)

#sentiments
afinn_pos_words <- get_sentiments('afinn') %>% 
  filter(score > 0) %>%
  select(word) %>%
  unlist %>%
  as.character()

afinn_neg_words <- get_sentiments('afinn') %>% 
  filter(score < 0) %>%
  select(word) %>%
  unlist %>%
  as.character()

bing_pos_words <- get_sentiments('bing') %>%
  filter(sentiment == 'positive') %>%
  select(word) %>%
  unlist %>%
  as.character()

bing_neg_words <- get_sentiments('bing') %>%
  filter(sentiment == 'negative') %>%
  select(word) %>%
  unlist %>%
  as.character()

pos_words <- union(afinn_pos_words, bing_pos_words)
neg_words <- union(afinn_neg_words, bing_neg_words)

df_tidy %>% 
  group_by(date) %>% 
  summarise(value = sum(word %in% pos_words) - sum(word %in% neg_words)) %>% 
  ggplot(aes(x = date, y = value)) + geom_line() + geom_hline(yintercept = 0, col = 'red') + geom_smooth(se = F) +
  scale_x_date(breaks = seq(min(df_tidy$date), max(df_tidy$date), 120))

pos_col <- RColorBrewer::brewer.pal(name = 'Set1', 3)[2]
neg_col <- RColorBrewer::brewer.pal(name = 'Set1', 3)[1]
df_tidy_sentiment <- df_tidy %>% 
  group_by(date, weekday) %>% 
  summarise(value_pos = sum(word %in% pos_words),
            value_neg = sum(word %in% neg_words)) 

p <-  ggplot(df_tidy_sentiment, aes(x = date)) + geom_line(aes(y = value_pos), col = pos_col) + geom_line(aes(y = value_neg), col = neg_col) + 
  geom_smooth(aes(y = value_pos), col = pos_col, se = F) + geom_smooth(aes(y = value_neg), col = neg_col, se = F)
p
p + scale_y_log10()