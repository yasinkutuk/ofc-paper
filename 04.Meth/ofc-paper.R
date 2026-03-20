## -*- coding: utf-8 -*-
#"""
#Created on Wed Jan 28 07:19:04 2026

#####################################################################
#                       _         _            _           _        #
#    _   _   __ _  ___ (_) _ __  | | __ _   _ | |_  _   _ | | __    #
#   | | | | / _  |/ __|| || '_ \ | |/ /| | | || __|| | | || |/ /    #
#   | |_| || (_| |\__ \| || | | ||   < | |_| || |_ | |_| ||   <     #
#    \__, | \__,_||___/|_||_| |_||_|\_\ \__,_| \__| \__,_||_|\_\    #
#    |___/                                                          #
#    ____                            _  _                           #
#   / __ \   __ _  _ __ ___    __ _ (_)| |    ___  ___   _ __ ___   #
#  / / _  | / _  || '_   _ \  / _  || || |   / __|/ _ \ | '_   _ \  #
# | | (_| || (_| || | | | | || (_| || || | _| (__| (_) || | | | | | #
#  \ \__,_| \__, ||_| |_| |_| \__,_||_||_|(_)\___|\___/ |_| |_| |_| #
#   \____/  |___/                                                   #
#####################################################################
#@author: Yasin KÜTÜK          ######################################
#@web   : yasinkutuk.com       ######################################
#@email : yasinkutuk@gmail.com ######################################
#####################################################################
#"""
#



# Leave empty of environment
rm(list=ls())


#Initials#####
options(digits = 4)
if(.Platform$OS.type=="windows"){
  path='d://GDrive//MyResearch//OFC-Paper//03.Data//'
  respath='d://GDrive//MyResearch//OFC-Paper//05.Res//'
  print("Hocam Windows'dasın!")
} else {
  path='/media/DRIVE/GDrive/MyResearch/OFC-Paper/03.Data/'
  respath='/media/DRIVE/GDrive/MyResearch/OFC-Paper/05.Res/'
  print("Abi Linux bu!")
}

# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}






# Usage example ####
packages<-c('tidyverse','readr','lubridate', 'stargazer', 'stopwords', 'tidytext',
            'wordcloud2', 'topicmodels', 'tm', 'zoo', 'wordcloud', 'htmlwidgets',
            'webshot')
check.packages(packages)

# Wordcloud2 Grafigi Dinamik Kayit
webshot::install_phantomjs()


# Read the CSV file
df <- read_csv(paste0(path,'ofc_opinions.csv'))


# Turkish month name lookup table
turkish_months <- c(
  "Ocak" = "01", "Şubat" = "02", "Mart"    = "03",
  "Nisan" = "04", "Mayıs" = "05", "Haziran" = "06",
  "Temmuz" = "07", "Ağustos" = "08", "Eylül"  = "09",
  "Ekim"  = "10", "Kasım"  = "11", "Aralık"  = "12"
)

df <- df %>%
  mutate(
    # Replace Turkish month name with its numeric equivalent
    date_clean = str_replace_all(date, turkish_months),
    # Now parse as a standard date
    date_parsed = as.Date(date_clean, format = "%d %m %Y")
  )

# Check the result
df %>% select(date, date_clean, date_parsed) %>% head()




df <- df %>%
  mutate(
    date_clean  = str_replace_all(date, turkish_months),
    date_parsed = as.Date(date_clean, format = "%d %m %Y"),
    word_count  = str_count(content, "\\S+"),
    year        = year(date_parsed)
  )


# Plot Exports Secenekleri ####
wi <- 1920 
he <- 1280
res <- 300





# Word count over time ####
png(paste0(respath, '01.ArticleLengths.png'), width = wi, height = he, res = res)
ggplot(df, aes(x = date_parsed, y = word_count)) +
  geom_line(color = "#aaaaaa", linewidth = 0.6) +
  geom_point(color = "#2C3E6B", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "#C0392B", linewidth = 1.2) +
  labs(
    title   = "Makale Uzunluğu",
    caption = "Kırmızı çizgi: LOESS eğrisi",
    x       = 'Yıllar',
    y       = "Kelime Sayısı"
  ) +
  theme_minimal(base_size = 12)
dev.off()





# Descriptives ####

# Per year descriptive statistics
yearly_stats <- df %>%
  group_by(year) %>%
  summarise(
    n         = n(),
    mean      = mean(word_count),
    median    = median(word_count),
    sd        = sd(word_count),
    min       = min(word_count),
    max       = max(word_count),
    q25       = quantile(word_count, 0.25),
    q75       = quantile(word_count, 0.75),
    .groups   = "drop"
  ) %>%
  mutate(period = as.character(year)) %>%
  select(-year)

# Overall descriptive statistics
overall_stats <- df %>%
  summarise(
    period    = "Overall",
    n         = n(),
    mean      = mean(word_count),
    median    = median(word_count),
    sd        = sd(word_count),
    min       = min(word_count),
    max       = max(word_count),
    q25       = quantile(word_count, 0.25),
    q75       = quantile(word_count, 0.75)
  )

# Combine into a single table
desc<-bind_rows(yearly_stats, overall_stats) %>%
  mutate(across(where(is.numeric), \(x) round(x, 1))) %>%
  print()


sink(file=paste0(respath,'02.Descriptives.txt'))
print(desc)
sink()



# Word Frequencies ####
# Turkish stop words (you'll need a custom list or use stopwords::stopwords("tr"))
tr_stops <- stopwords::stopwords('tr', source = 'stopwords-iso')

df %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% tr_stops, nchar(word) > 2) %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 30) %>%
  ggplot(aes(n, reorder(word, n))) +
  geom_col() +
  labs(y='Sıklıklarına Göre Sıralı', x = 'Sıklık')






word_freq <- df %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% tr_stops) %>%
  count(word, sort = TRUE)



# Plot Exports Secenekleri ####
wi <- 800 
he <- 600
res <- 300

# Save as HTML first, then screenshot to PNG
wc <- wordcloud2(word_freq)
saveWidget(wc, "temp_wc.html", selfcontained = TRUE)
webshot("temp_wc.html", paste0(respath, "03.UniGramsWC.png"),
        delay = 5, vwidth = wi, vheight = he)



keywords <- c('türkiye', 'enflasyon', 'para', 'abd', 'borç', 'çin', 'dolar', 'faiz', 'merkez')

df %>%
  unnest_tokens(word, content) %>%
  filter(word %in% keywords) %>%
  count(date_parsed, word) %>%
  ggplot(aes(x = date_parsed, y = n, color = word)) +
  geom_line() +
  facet_wrap(~word, scales = "free_y")+
  labs(y='Sıklık', x = 'Yıllar')




# Plot Exports Secenekleri ####
wi <- 640 
he <- 480
res <- 300

# 2-grams (bigrams)
bigrams <- df %>%
  unnest_ngrams(bigram, content, n = 2) %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 30)

bigrams_filtered <- df %>%
  unnest_ngrams(bigram, content, n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% tr_stops, !word2 %in% tr_stops) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 30)

bwc <- bigrams_filtered %>%
  wordcloud2(size = 0.5, color = "random-dark", backgroundColor = "white")
saveWidget(bwc, "temp_wc.html", selfcontained = TRUE)
webshot("temp_wc.html", paste0(respath, "04.BiGramsWC.png"),
        delay = 5, vwidth = wi, vheight = he)


# 3-grams (trigrams)
trigrams <- df %>%
  unnest_ngrams(trigram, content, n = 3) %>%
  count(trigram, sort = TRUE) %>%
  slice_head(n = 30)

# Trigram plot
trigrams %>%
  ggplot(aes(n, reorder(trigram, n))) +
  geom_col(fill = "tomato") +
  labs(title = "En Sık 3-gram", x = "Frekans", y = "")

# Filter trigrams where ANY word is a stop word
trigrams_filtered <- df %>%
  unnest_ngrams(trigram, content, n = 3) %>%
  separate(trigram, into = c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% tr_stops, !word2 %in% tr_stops, !word3 %in% tr_stops) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  count(trigram, sort = TRUE) %>%
  slice_head(n = 30)


# Trigram wordcloud
twc <- trigrams_filtered %>%
  rename(word = trigram, freq = n) %>%
  wordcloud2(size = 0.4, color = "random-dark", backgroundColor = "white")


saveWidget(twc, "temp_wc.html", selfcontained = TRUE)
webshot("temp_wc.html", paste0(respath, "05.TriGramsWC.png"),
        delay = 5, vwidth = wi, vheight = he)




# Latent Dirichlet Allocation for Topics ####

dtm <- df %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% tr_stops) %>%
  count(no, word) %>%
  cast_dtm(no, word, n)

lda_model <- LDA(dtm, k = 5, control = list(seed = 42))
tidy(lda_model, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
  geom_col() +
  facet_wrap(~topic, scales = "free")





# Sentiments per Article of OFC ####
url <- "https://raw.githubusercontent.com/StarlangSoftware/TurkishSentiNet/refs/heads/master/target/classes/turkish_sentiliteralnet.xml"

# Read as raw text
raw <- read_lines(url)
raw_text <- paste(raw, collapse = " ")

# Extract all WORD blocks
word_blocks <- str_extract_all(raw_text, "<WORD>.*?</WORD>")[[1]]

# Parse each block
sentiturk <- tibble(block = word_blocks) %>%
  mutate(
    word  = str_extract(block, "(?<=<NAME>).*?(?=</NAME>)"),
    pos   = as.numeric(str_extract(block, "(?<=<PSCORE>).*?(?=</PSCORE>)")),
    neg   = as.numeric(str_extract(block, "(?<=<NSCORE>).*?(?=</NSCORE>)"))
  ) %>%
  select(-block) %>%
  mutate(score = pos - neg) %>%
  filter(score != 0)

# Check
nrow(sentiturk)
head(sentiturk)


library(tidyverse)
library(tidytext)

# Make sure df has date_parsed
turkish_months <- c(
  "Ocak" = "01", "Şubat" = "02", "Mart"    = "03",
  "Nisan" = "04", "Mayıs" = "05", "Haziran" = "06",
  "Temmuz" = "07", "Ağustos" = "08", "Eylül"  = "09",
  "Ekim"  = "10", "Kasım"  = "11", "Aralık"  = "12"
)

df <- df %>%
  mutate(
    date_clean  = str_replace_all(date, turkish_months),
    date_parsed = as.Date(date_clean, format = "%d %m %Y")
  )

# Sentiment analysis
df %>%
  unnest_tokens(word, content) %>%
  inner_join(sentiturk, by = "word") %>%
  group_by(no, date_parsed) %>%
  summarise(sentiment = sum(score), .groups = "drop") %>%
  ggplot(aes(date_parsed, sentiment)) +
  geom_col(aes(fill = sentiment > 0)) +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "tomato"),
    labels = c("FALSE" = "Negatif", "TRUE" = "Pozitif"),
    name   = ""
  ) +
  labs(title = "Makale Başına Duygu Skoru", x = "Yıllar", y = "Skor")









sentiment_df <- df %>%
  unnest_tokens(word, content) %>%
  inner_join(sentiturk, by = "word") %>%
  group_by(no, date_parsed) %>%
  summarise(sentiment = sum(score), .groups = "drop") %>%
  mutate(
    sentiment_dir = ifelse(sentiment > 0, "Pozitif", "Negatif"),
    # Rolling average for trend line
    trend = zoo::rollmean(sentiment, k = 5, fill = NA, align = "center")
  )

# Plot
ggplot(sentiment_df, aes(x = date_parsed)) +
  
  # Zero reference line
  geom_hline(yintercept = 0, color = "#cccccc", linewidth = 0.5, linetype = "dashed") +
  
  # Shaded area under trend
  geom_area(aes(y = trend), fill = "#2C3E6B", alpha = 0.08, na.rm = TRUE) +
  
  # Sentiment bars
  geom_col(aes(y = sentiment, fill = sentiment_dir), width = 5, alpha = 0.75) +
  
  # Trend line
  geom_line(aes(y = trend), color = "#2C3E6B", linewidth = 1, na.rm = TRUE) +
  
  # Dots on trend line
  geom_point(aes(y = trend), color = "#2C3E6B", size = 1.5, na.rm = TRUE) +
  
  scale_fill_manual(
    values = c("Pozitif" = "#3A7D6B", "Negatif" = "#C0392B"),
    name   = NULL
  ) +
  
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b\n%Y",
    expand = c(0.02, 0)
  ) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 6),
    labels = scales::label_number(style_positive = "plus")
  ) +
  
  labs(
    title    = "Ömer Faruk Çolak — Yazı Duygu Analizi",
    subtitle = "Çubuk: Makale Başına Net Duygu Skoru, Koyu: Beş Makalenin Hareketli Ortalaması",
    caption  = "Kaynak: ekonomim.com | Sözlük: TurkishSentiNet",
    x        = NULL,
    y        = "Duygu Skoru"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    text              = element_text(family = "Georgia"),
    plot.title        = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle     = element_text(color = "#555555", size = 10, margin = margin(b = 12)),
    plot.caption      = element_text(color = "#aaaaaa", size = 8, hjust = 0),
    plot.background   = element_rect(fill = "#FAFAF8", color = NA),
    panel.background  = element_rect(fill = "#FAFAF8", color = NA),
    panel.grid.major  = element_line(color = "#eeeeee"),
    panel.grid.minor  = element_blank(),
    axis.text         = element_text(color = "#666666", size = 9),
    legend.position   = "top",
    legend.justification = "left",
    legend.text       = element_text(size = 10),
    plot.margin       = margin(20, 25, 15, 20)
  )

# Save
ggsave(paste0(respath, '06.SentimentsTimeSeries.png'), width = 11, height = 6, dpi = 180, bg = "#FAFAF8")
