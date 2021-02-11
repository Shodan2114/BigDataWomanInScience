library(tidyverse) #includes ggplot2, dplyr, readr, stringr
library(knitr)
library(cowplot)
library(gender)
library(aRxiv)
library(rbiorxiv)
library(lubridate)
library(anytime)

n.2020 <- arxiv_count(query = 'submittedDate:[202001010000 TO 202012312400]')
df.2020.1 <- arxiv_search(query = 'submittedDate:[202001010000 TO 202001152400]', limit=n.2020, batchsize=2000)
df.2020.2 <- arxiv_search(query = 'submittedDate:[202001160000 TO 202001312400]', limit=n.2020, batchsize=2000)
df.2020.3 <- arxiv_search(query = 'submittedDate:[202002010000 TO 202002152400]', limit=n.2020, batchsize=2000)
df.2020.4 <- arxiv_search(query = 'submittedDate:[202002160000 TO 202002292400]', limit=n.2020, batchsize=2000)
df.2020.5 <- arxiv_search(query = 'submittedDate:[202003010000 TO 202003152400]', limit=n.2020, batchsize=2000)
df.2020.6 <- arxiv_search(query = 'submittedDate:[202003160000 TO 202003312400]', limit=n.2020, batchsize=2000)
df.2020.7 <- arxiv_search(query = 'submittedDate:[202004010000 TO 202004152400]', limit=n.2020, batchsize=2000)
df.2020.8 <- arxiv_search(query = 'submittedDate:[202004160000 TO 202004302400]', limit=n.2020, batchsize=2000)
df.2020.9 <- arxiv_search(query = 'submittedDate:[202005010000 TO 202005152400]', limit=n.2020, batchsize=2000)
df.2020.10 <- arxiv_search(query = 'submittedDate:[202005160000 TO 202005312400]', limit=n.2020, batchsize=2000)
df.2020.11 <- arxiv_search(query = 'submittedDate:[202006010000 TO 202006152400]', limit=n.2020, batchsize=2000)
df.2020.12 <- arxiv_search(query = 'submittedDate:[202006160000 TO 202006302400]', limit=n.2020, batchsize=2000)
df.2020.13 <- arxiv_search(query = 'submittedDate:[202007010000 TO 202007152400]', limit=n.2020, batchsize=2000)
df.2020.14 <- arxiv_search(query = 'submittedDate:[202007160000 TO 202007312400]', limit=n.2020, batchsize=2000)
df.2020.15 <- arxiv_search(query = 'submittedDate:[202008010000 TO 202008152400]', limit=n.2020, batchsize=2000)
df.2020.16 <- arxiv_search(query = 'submittedDate:[202008160000 TO 202008312400]', limit=n.2020, batchsize=2000)
df.2020.17 <- arxiv_search(query = 'submittedDate:[202009010000 TO 202009152400]', limit=n.2020, batchsize=2000)
df.2020.18 <- arxiv_search(query = 'submittedDate:[202009160000 TO 202009302400]', limit=n.2020, batchsize=2000)
df.2020.19 <- arxiv_search(query = 'submittedDate:[202010010000 TO 202010152400]', limit=n.2020, batchsize=2000)
df.2020.20 <- arxiv_search(query = 'submittedDate:[202010160000 TO 202010312400]', limit=n.2020, batchsize=2000)
df.2020.21 <- arxiv_search(query = 'submittedDate:[202011010000 TO 202011152400]', limit=n.2020, batchsize=2000)
df.2020.22 <- arxiv_search(query = 'submittedDate:[202011160000 TO 202011302400]', limit=n.2020, batchsize=2000)
df.2020.13 <- arxiv_search(query = 'submittedDate:[202012010000 TO 202012152400]', limit=n.2020, batchsize=2000)
df.2020.24 <- arxiv_search(query = 'submittedDate:[202012160000 TO 202012312400]', limit=n.2020, batchsize=2000)
df.2020.full <- rbind(df.2020.1, df.2020.2, df.2020.3, df.2020.4, df.2020.5, df.2020.6, df.2020.7, df.2020.8, df.2020.9, df.2020.10, df.2020.11, df.2020.12, df.2020.13, df.2020.14, df.2020.15, df.2020.16, df.2020.17, df.2020.18, df.2020.19, df.2020.20, df.2020.21, df.2020.22, df.2020.23, df.2020.24)
n.2020-length(df.2020.full$id) #Check that the number of records matches
write.csv(df.2020.full, file="Data/arxiv_2020_data.csv")

df.2020 <- read.csv("Data/arxiv_2020_data.csv") #Read in data
df.all2020 <- rbind(df.2020) # 2020 data

split.names <- function(x){strsplit(as.character(x), "|", fixed=TRUE)} #Function to split strings of author names

df.all2020$split.names <- lapply(df.all2020$authors, split.names)
tmp <- NULL
all_first_names <- word(unlist(df.all2020$split.names),1)
gender <- gender(all_first_names, method = "ssa")
gender <- unique(gender[ , c(1,2,4)])

for(i in 1:length(df.all2020$authors)){
  tmp <- as.data.frame(word(unlist(df.all2020$split.names[[i]]), 1))
  colnames(tmp) <- "name"
  tmp <- merge(tmp, gender, by="name", all.x=TRUE, all.y=FALSE)
  df.all2020$male.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "male")))), na.rm=TRUE)
  df.all2020$female.n[i] <- sum(as.numeric(str_count(as.character(tmp$gender), pattern = paste(sprintf("\\b%s\\b", "female")))), na.rm=TRUE)
}

df.all2020.output <- as.data.frame(apply(df.all2020,2,as.character))
write.csv(df.all2020.output, "Data/arxiv_all2020_gender.csv") #Save data


