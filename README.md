# FOMC-Sentiment-Uncertainty
##Textual Analysis of FOMC Minutes to Gauge Uncertainty

rm(list = ls())

setwd("D:\\TOM CUNNINGHAM\\ECON 685")

##Load libraries

library(extrafont)
library(ggraph)
library(ggridges)
library(pdftools)
library(tidyverse)
library(tidytext)
library(forcats)
library(reshape2)
library(tidyr)
library(igraph)
library(widyr)
library(viridis)
library(dplyr)
library(SentimentAnalysis)
library(tokenizers)
library(sentimentr)
library(stringi) 
library(Rcpp)
library(RcppArmadillo)
library(BH)
library(quanteda)
library(textdata)

##Importing stopwords library from the textdata package

##Stop Words are generally defined as extremely common words which do not have any valuable analytical information which can be derived from them, largely made up of prepositions. Examples include “also”, “am”, “an”, “of”, etc.

data(stop_words)

##Import PDF of FOMC Minutes

##Note - In the essence of brevity, I have limited this code sample to three recent minutes

##In this case, I saved the minutes from the FOMC website by selecting the text and printing to PDF

##The structure of the text in the FOMC-supplied PDF is organized such that the columns are not separated correctly

july2019 <- pdf_text("July2019Minutes.pdf")

june2019 <- pdf_text("June2019Minutes.pdf")

april2019 <- pdf_text("April2019Minutes.pdf")

##Removing linebreaks, special characters, and spaces

july2019_raw <- 
  data.frame(text=unlist(strsplit(july2019, "\r"))) %>%
  mutate(report = "July2019",
         line = row_number(),
         text = gsub("\n", "", text))
head(july2019_raw)


june2019_raw <- 
  data.frame(text=unlist(strsplit(june2019, "\r"))) %>%
  mutate(report = "June2019",
         line = row_number(),
         text = gsub("\n", "", text))
head(june2019_raw)

april2019_raw <- 
  data.frame(text=unlist(strsplit(april2019, "\r"))) %>%
  mutate(report = "April2019",
         line = row_number(),
         text = gsub("\n", "", text))
head(april2019_raw)

##Mining Text

##Restructuring into a sentence by sentence format

###For a word-by-word format, let the unnest_tokens line read "unnest_tokens(word, text)

###mutate(sentenceID = 1:n()) allows you to track the number of separated sentences more easily

textjuly2019 <- july2019_raw$text

sentence_july2019 <- tibble(text = textjuly2019)

sentencejuly2019 <- sentence_july2019 %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentenceID = 1:n())


textjune2019 <- june2019_raw$text

sentence_june2019 <- tibble(text = textjune2019)

sentencejune2019 <- sentence_june2019 %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentenceID = 1:n())

textapril2019 <- april2019_raw$text

sentence_april2019 <- tibble(text = textapril2019)

sentenceapril2019 <- sentence_april2019 %>%
  unnest_tokens(sentence, text, token = "sentences") %>%
  mutate(sentenceID = 1:n())

##Applying Sentiment Analysis from Loughran McDonald Dictionary (2011) & Removing Stop Words

##The Tokenization section further separates each pre separated sentence into its word parts so each word can be analyzed, more easily apply the Stop Words removal, and to more simply provide counts to be used later

##In order to apply different sentiments of the Loughran McDonald Dictionary (i.e. positive, negative, litigious) Replace as follows: filter(sentiment == ‘positive’) %>%

tokenization_july2019 <- sentencejuly2019 %>%
  mutate(sentence_id = 1:n()) %>%
  unnest_tokens(
    output = word, 
    input = sentence, 
    token = 'words', 
    drop = FALSE
  ) %>%
  ungroup()

tokenization_july2019 <- tokenization_july2019 %>%
  anti_join(stop_words)


sentiments_july2019 <- tokenization_july2019 %>%
  inner_join(get_sentiments('loughran')) %>%
  filter(sentiment == "uncertainty") %>%
  group_by(sentence_id, sentence) %>%
  ungroup()

tokenization_june2019 <- sentencejune2019 %>%
  mutate(sentence_id = 1:n()) %>%
  unnest_tokens(
    output = word, 
    input = sentence, 
    token = 'words', 
    drop = FALSE
  ) %>%
  ungroup()

tokenization_june2019 <- tokenization_june2019 %>%
  anti_join(stop_words)


sentiments_june2019 <- tokenization_june2019 %>%
  inner_join(get_sentiments('loughran')) %>%
  filter(sentiment == "uncertainty") %>%
  group_by(sentence_id, sentence) %>%
  ungroup()


tokenization_april2019 <- sentenceapril2019 %>%
  mutate(sentence_id = 1:n()) %>%
  unnest_tokens(
    output = word, 
    input = sentence, 
    token = 'words', 
    drop = FALSE
  ) %>%
  ungroup()

tokenization_april2019 <- tokenization_april2019 %>%
  anti_join(stop_words)


sentiments_april2019 <- tokenization_april2019 %>%
  inner_join(get_sentiments('loughran')) %>%
  filter(sentiment == "uncertainty") %>%
  group_by(sentence_id, sentence) %>%
  ungroup()

sentiments_april2019

##Applying valence shifters to our selected uncertainty observations from the sentimentr package

###Note: As stated in the introduction, the analysis shown in this example does not show the results of the valence-shifted analysis. However, I have included the code to make revisions between analysis types easier(the valence-shifted value can still be counted as a number of total associated terms, even without measuring the adjusted value).

valence_sentence_july2019 <- sentiments_july2019 %>%
  sentimentr::get_sentences() %>%
  sentimentr::sentiment() 
  
valence_sentence_june2019 <- sentiments_june2019 %>%
  sentimentr::get_sentences() %>%
  sentimentr::sentiment() 

valence_sentence_april2019 <- sentiments_april2019 %>%
  sentimentr::get_sentences() %>%
  sentimentr::sentiment() 

##Calculating uncertainty measures (both valence shifted and number of uncertainty associated terms) as a proportion of total words

##In order to measure uncertainty associated terms as a proportion of general sentiment, I did the following: 1. Removed “/nrow(tokenization_monthyear))” from the text below 2. Exported the results of the total count of uncertainty-associated terms to excel 3. Revised the applied sentiment from the LM Dictionary to positive, collected the total counts, and exported to excel 4. Revised the applied sentiment from the LM Dictionary to negative, collected the total counts, and exported to excel 5. Performed the calculation as an excel function

colSums(Filter(is.numeric, valence_sentence_june2019))

value_june2019 <- (colSums(Filter(is.numeric, valence_sentence_june2019))/nrow(tokenization_june2019))


colSums(Filter(is.numeric, valence_sentence_july2019))

value_july2019 <- (colSums(Filter(is.numeric, valence_sentence_july2019))/nrow(tokenization_july2019))


colSums(Filter(is.numeric, valence_sentence_april2019))

value_april2019 <- (colSums(Filter(is.numeric, valence_sentence_april2019))/nrow(tokenization_april2019))

##Since the textual analysis uses two analysis packages in R, there are two ‘sentiment’ values, given. This makes reviewing the information easier by making the columns unique

make.unique(c("sentence", "sentenceid", "sentence_id", "word", "sentiment", "element_id", "word_count", "sentiment"), sep = "_")

##Plotting Index as a time-series

###Note [2] is the summation of the number of associated uncertainty terms divided by the number of total words [5] is the valence-shited uncertainty value as a proportion of total words


ajj <- c(value_april2019[2], value_june2019[2], value_july2019[2])

ajj_ts <- ts(ajj, start = c(2019,1), frequency = 8)

plot(ajj_ts, type = 'l', ylab = 'Uncertainty Index', xlab = 'Time')

##For reference, calculating values of total words over time

token <- c(nrow(tokenization_april2019), nrow(tokenization_june2019), nrow(tokenization_july2019))

token

##Regression Analyses (Forward Looking FOMC Minutes)

rm(list=ls())

setwd("D:\\TOM CUNNINGHAM\\ECON 685")

library('Quandl')
Quandl.api_key("UMo9bYuaDpyAy55mMfGS")
library('lubridate')
library('zoo')
library(xts)

##Importing Combined Data from Initial Code and Benchmarks and viewing the structure of each object

trunc.df = read.csv("TruncatedText.csv", header = TRUE, stringsAsFactors = FALSE)
str(trunc.df)

##Changing the date and unemployment data to be read as dates and numbers, respectively

trunc.df$Date = as.Date(trunc.df$Date)
trunc.df$LogDiffUnemp = as.numeric(trunc.df$LogDiffUnemp)

##Establishing a time series of the truncated data frame variables

##The imported variables I used are as follows:

###UncertaintyPropNoStop = Uncertainty associated terms as a proportion of total words (minus stop words) from the forward-looking FOMC minutes selection (Imported to excel from earlier code) 
###Target.FFR = Target Federal Funds Rate (Imported into excel from FRED) 
###LogDiffUnemp = Unemployment Rate Growth 
###DGS1 = One Year Treasury Bill (Imported into excel from FRED) 
###DGS3MO = Three Month Treasury Bill (Imported into excel from FRED) 
###X10Y3MAVG = Ten Year - Three Month Treasury Spread (Imported into excel from FRED) 
###X10Y2YAVG = Ten Year - Two Year Treasury Spread (Imported into excel from FRED) 
###X10Y2YAVGLAG = Ten year - Two Year Lagged Treasury Spread 
###X10Y1Y = Ten year - One Year Treasury Spread

uncertaintyindicator.zoo <- zoo(trunc.df$UncertaintyPropNoStop, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

TargetFederalFundsRate.zoo <- zoo(trunc.df$Target.FFR, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

InflationRate.zoo <- zoo(trunc.df$inf, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

UnemploymentRate.zoo <- zoo(trunc.df$LogDiffUnemp, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

OneYearTBill.zoo <- zoo(trunc.df$DGS1, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

ThreeMonthTBill.zoo <- zoo(trunc.df$DGS3MO, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

TenYr3MoSpread.zoo <- zoo(trunc.df$X10Y3MAVG, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

TenYr2YrSpread.zoo <- zoo(trunc.df$X10Y2YAVG, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

TenYr2YrSpreadlag.zoo <- zoo(trunc.df$X10Y2YAVGLAG, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

TenYr1YrSpread.zoo <- zoo(trunc.df$X10Y1Y, trunc.df$Date, is.regular(trunc.df, strict = FALSE))

##Merging zoo time series

data = merge(uncertaintyindicator.zoo, TargetFederalFundsRate.zoo, InflationRate.zoo, UnemploymentRate.zoo, OneYearTBill.zoo, ThreeMonthTBill.zoo, TenYr3MoSpread.zoo, TenYr2YrSpread.zoo, TenYr2YrSpreadlag.zoo, TenYr1YrSpread.zoo)

##Regressions

###1yr Tbill vs Counts, Inflation, Unemployment, Targeted Federal Funds Rate

modeltrunc1yrcountsFFR = lm(data$OneYearTBill.zoo~data$uncertaintyindicator.zoo+InflationRate.zoo+UnemploymentRate.zoo+TargetFederalFundsRate.zoo)

summary(modeltrunc1yrcountsFFR)

###3mo Tbill vs Counts, Inflation, Unemployment, Targeted Federal Funds Rate

modeltrunc3mocountsFFR = lm(data$ThreeMonthTBill.zoo~data$uncertaintyindicator.zoo+InflationRate.zoo+UnemploymentRate.zoo+TargetFederalFundsRate.zoo)

summary(modeltrunc3mocountsFFR)

###10Y3mo Spread vs Counts, Inflation, Unemployment, Targeted Federal Funds Rate

modelTenYr3MoSpreadcountsFFR = lm(data$TenYr3MoSpread.zoo~data$uncertaintyindicator.zoo+InflationRate.zoo+UnemploymentRate.zoo+TargetFederalFundsRate.zoo)

summary(modelTenYr3MoSpreadcountsFFR)

###10Y2Y Spread vs Counts, Inflation, Unemployment, Targeted Federal Funds Rate



modelTenYr2YrSpreadcountsFFR = lm(data$TenYr2YrSpread.zoo~data$uncertaintyindicator.zoo+InflationRate.zoo+UnemploymentRate.zoo+TargetFederalFundsRate.zoo)

summary(modelTenYr2YrSpreadcountsFFR)

###10Y2YLag Spread vs Counts, Inflation, Unemployment, Targeted Federal Funds Rate

modelTenYr2YrSpreadcountsFFRlag = lm(data$TenYr2YrSpreadlag.zoo~data$uncertaintyindicator.zoo+InflationRate.zoo+UnemploymentRate.zoo+TargetFederalFundsRate.zoo)

summary(modelTenYr2YrSpreadcountsFFRlag)

###10Y1Y Spread vs Counts, Inflation, Unemployment, Targeted Federal Funds Rate

modelTenYr1YrSpreadcountsFFR = lm(data$TenYr1YrSpread.zoo~data$uncertaintyindicator.zoo+InflationRate.zoo+UnemploymentRate.zoo+TargetFederalFundsRate.zoo)

summary(modelTenYr1YrSpreadcountsFFR)

