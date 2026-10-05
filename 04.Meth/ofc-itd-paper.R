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

library(tidyverse)
library(tidytext)
library(stopwords)
library(wordcloud)
library(RColorBrewer)
library(scales)

# Initial Setups ####
wi <- 1400; he <- 900; res <- 150   # png boyutları

# ---- 2. Veri Okuma ve Temizlik ----
df <- read_csv("itd_editorden.csv")

# Sayı numarasını başlıktan çıkar (örn. "İTD 180. Sayı" -> 180)
df <- df %>%
  mutate(
    issue_no = as.numeric(str_extract(title, "(?<=İTD )\\d+")),
    word_count = str_count(full_text, "\\S+")
  ) %>%
  filter(!is.na(issue_no), char_count > 0) %>%   # boş / eşleşmeyen satırları çıkar
  arrange(issue_no)

cat("Toplam yazı sayısı:", nrow(df), "\n")
cat("Sayı aralığı:", min(df$issue_no), "-", max(df$issue_no), "\n")

# Türkçe stopwords
tr_stops <- stopwords("tr", source = "stopwords-iso")

# ---- 3. Betimsel İstatistikler ----
overall_stats <- df %>%
  summarise(
    n = n(), mean = mean(word_count), median = median(word_count),
    sd = sd(word_count), min = min(word_count), max = max(word_count),
    q25 = quantile(word_count, .25), q75 = quantile(word_count, .75)
  )
print(overall_stats)

# 10'luk sayı bloklarına göre (dönemsel karşılaştırma için)
df <- df %>% mutate(issue_block = paste0(((issue_no - 1) %/% 20) * 20 + 1,
                                          "-", ((issue_no - 1) %/% 20) * 20 + 20))

block_stats <- df %>%
  group_by(issue_block) %>%
  summarise(n = n(), mean = mean(word_count), median = median(word_count),
            sd = sd(word_count), .groups = "drop")
print(block_stats)

# ---- 4. Yazı Uzunluğu - Sayı Numarasına Göre ----
png(paste0(respath, "01.YaziUzunlugu.png"), width = wi, height = he, res = res)
ggplot(df, aes(x = issue_no, y = word_count)) +
  geom_line(color = "#aaaaaa", linewidth = 0.6) +
  geom_point(color = "#2C3E6B", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "#C0392B", linewidth = 1.2) +
  labs(
    title   = "İTD Editörden Yazıları - Kelime Sayısı",
    caption = "Kırmızı çizgi: LOESS trend",
    x       = "Dergi Sayısı",
    y       = "Kelime Sayısı"
  ) +
  theme_minimal(base_size = 12)
dev.off()

# ---- 5. Unigram Frekans ----
word_freq <- df %>%
  unnest_tokens(word, full_text) %>%
  filter(!word %in% tr_stops, nchar(word) > 2, !str_detect(word, "^[0-9]+$")) %>%
  count(word, sort = TRUE)

png(paste0(respath, "02.UnigramBarChart.png"), width = wi, height = he, res = res)
word_freq %>%
  slice_head(n = 30) %>%
  ggplot(aes(n, reorder(word, n))) +
  geom_col(fill = "#2C3E6B") +
  labs(title = "İTD Editörden - En Sık Kullanılan Kelimeler",
       x = "Frekans", y = NULL) +
  theme_minimal(base_size = 12)
dev.off()

png(paste0(respath, "03.UnigramWC.png"), width = wi, height = he, res = res)
wordcloud(
  words = word_freq$word, freq = word_freq$n,
  max.words = 100, random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)
dev.off()

# ---- 6. Bigram Analizi ----
bigrams_filtered <- df %>%
  unnest_ngrams(bigram, full_text, n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% tr_stops, !word2 %in% tr_stops,
         !str_detect(word1, "^[0-9]+$"), !str_detect(word2, "^[0-9]+$")) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE)

png(paste0(respath, "04.BigramBarChart.png"), width = wi, height = he, res = res)
bigrams_filtered %>%
  slice_head(n = 30) %>%
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(fill = "#1F4E79") +
  labs(title = "İTD Editörden - En Sık 2-gram", x = "Frekans", y = NULL) +
  theme_minimal(base_size = 12)
dev.off()

png(paste0(respath, "05.BigramWC.png"), width = wi, height = he, res = res)
wordcloud(
  words = bigrams_filtered$bigram, freq = bigrams_filtered$n,
  max.words = 100, random.order = FALSE,
  colors = brewer.pal(8, "Dark2"), scale = c(2.5, 0.5)
)
dev.off()

# ---- 7. Trigram Analizi ----
trigrams_filtered <- df %>%
  unnest_ngrams(trigram, full_text, n = 3) %>%
  separate(trigram, into = c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% tr_stops, !word2 %in% tr_stops, !word3 %in% tr_stops,
         !str_detect(word1, "^[0-9]+$"), !str_detect(word2, "^[0-9]+$"), !str_detect(word3, "^[0-9]+$")) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  count(trigram, sort = TRUE)

png(paste0(respath, "06.TrigramBarChart.png"), width = wi, height = he, res = res)
trigrams_filtered %>%
  slice_head(n = 30) %>%
  ggplot(aes(n, reorder(trigram, n))) +
  geom_col(fill = "#7B241C") +
  labs(title = "İTD Editörden - En Sık 3-gram", x = "Frekans", y = NULL) +
  theme_minimal(base_size = 12)
dev.off()

png(paste0(respath, "07.TrigramWC.png"), width = wi, height = he, res = res)
wordcloud(
  words = trigrams_filtered$trigram, freq = trigrams_filtered$n,
  max.words = 80, random.order = FALSE,
  colors = brewer.pal(8, "Dark2"), scale = c(2.2, 0.4)
)
dev.off()

# ---- 8. Duygu Analizi (HisNet / TurkishSentiNet) ----
url <- "https://raw.githubusercontent.com/StarlangSoftware/TurkishSentiNet/refs/heads/master/target/classes/turkish_sentiliteralnet.xml"
raw <- read_lines(url)
raw_text <- paste(raw, collapse = " ")
word_blocks <- str_extract_all(raw_text, "<WORD>.*?</WORD>")[[1]]

sentiturk <- tibble(block = word_blocks) %>%
  mutate(
    word = str_extract(block, "(?<=<NAME>).*?(?=</NAME>)"),
    pos  = as.numeric(str_extract(block, "(?<=<PSCORE>).*?(?=</PSCORE>)")),
    neg  = as.numeric(str_extract(block, "(?<=<NSCORE>).*?(?=</NSCORE>)"))
  ) %>%
  select(-block) %>%
  mutate(score = pos - neg) %>%
  filter(score != 0)

sentiment_df <- df %>%
  unnest_tokens(word, full_text) %>%
  inner_join(sentiturk, by = "word") %>%
  group_by(issue_no) %>%
  summarise(sentiment = sum(score), .groups = "drop") %>%
  arrange(issue_no) %>%
  mutate(
    sentiment_dir = ifelse(sentiment > 0, "Pozitif", "Negatif"),
    trend = zoo::rollmean(sentiment, k = 5, fill = NA, align = "center")
  )

png(paste0(respath, "08.DuyguZamanSerisi.png"), width = wi, height = he, res = res)
ggplot(sentiment_df, aes(x = issue_no)) +
  geom_hline(yintercept = 0, color = "#cccccc", linewidth = 0.5, linetype = "dashed") +
  geom_area(aes(y = trend), fill = "#2C3E6B", alpha = 0.08, na.rm = TRUE) +
  geom_col(aes(y = sentiment, fill = sentiment_dir), width = 0.7, alpha = 0.75) +
  geom_line(aes(y = trend), color = "#2C3E6B", linewidth = 1, na.rm = TRUE) +
  geom_point(aes(y = trend), color = "#2C3E6B", size = 1.2, na.rm = TRUE) +
  scale_fill_manual(values = c("Pozitif" = "#3A7D6B", "Negatif" = "#C0392B"), name = NULL) +
  labs(
    title    = "İTD Editörden Yazıları - Duygu Analizi",
    subtitle = "Çubuklar sayı başına net duygu skorunu, koyu çizgi 5 sayının hareketli ortalamasını gösterir",
    caption  = "Sözlük: HisNet / TurkishSentiNet (Ozcelik ve ark., 2021)",
    x = "Dergi Sayısı", y = "Duygu Skoru"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", legend.justification = "left")
dev.off()

cat("\nTüm grafikler kaydedildi:", normalizePath(respath), "\n")
