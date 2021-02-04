####

#install.packages("devtools")
#devtools::install_github("bmschmidt/wordVectors")

library(wordVectors)
library(readr)
library(tidyverse)
library(tidyr)
library(kernlab)
library(tsne)
library(Rtsne)

?train_word2vec
?nearest_to
?closest_to

options(digits = 7)

train = data.table::fread('data/train.csv', encoding = 'UTF-8', data.table = F)
#train$tags %>% str_remove_all("[']") %>% str_remove_all("[,]") %>% str_sub(start = 2, end = -2) %>% 
#  write.csv('word2vec/train_w2v.txt', row.names = F)


### 여기부터 ###
#model = train_word2vec(train_file = "word2vec/train_w2v.txt",
#                       threads = 6,                  #cpu 개수
#                       vectors = 100,                #차원의 수
#                       force = T,
#                       min_count = 5,
#                       window = 4,                 #앞뒤에 볼 단어 수
#                       output_file = "word2vec/word2vec_tag_model.bin")

model = wordVectors::read.binary.vectors("word2vec/word2vec_tag_model.bin")
model = model[nchar(rownames(model))>1,] %>% .[-1, ]
model[1:20, ]

row.names(model)

model %>% dim
nearest_to(model, model[['기분전환']], 20) %>% round(3)
nearest_to(model, model[['감성']], 20) %>% round(3)
nearest_to(model, model[['휴식']], 20) %>% round(3)
closest_to(model, model[['휴식']], 10)
closest_to(model, model[['기분전환']], 20)
closest_to(model, model[['사랑']], 20)

closest_to(model, model[['기분전횐']], 20)

gibun_name = nearest_to(model, model[['기분전환']], 50) %>% round(3) %>% names
model[gibun_name, ]


### 곡!

song_meta = data.table::fread('data/song_meta.csv', encoding = 'UTF-8', data.table = F)
train = data.table::fread('data/train.csv', encoding = 'UTF-8', data.table = F)
#train$songs %>% str_remove_all("[']") %>% str_remove_all("[,]") %>% str_sub(start = 2, end = -2) %>% 
#  write.csv('word2vec/train_song_w2v.txt', row.names = F)


#song_model = train_word2vec(train_file = "word2vec/train_song_w2v.txt",
#                           threads = 6,                  #cpu 개수
#                           vectors = 100,                #차원의 수
#                           force = T,
#                           min_count = 5,
#                           window = 6,                 #앞뒤에 볼 단어 수
#                           output_file = "word2vec/word2vec_song_model.bin")

song_model = wordVectors::read.binary.vectors("word2vec/word2vec_song_model.bin")
song_model = song_model[nchar(rownames(song_model))>1,] %>% .[-1, ]
song_model %>% head()

result1 = nearest_to(song_model, song_model[['144663']], 30) %>% round(3)
closest_to(song_model, song_model[['144663']], 30)

result1_meta = song_meta[which(song_meta$id %in% (result1 %>% names)), ] %>% 
  select(song_name, artist_name_basket, id) %>% mutate(id = id %>% factor %>% as.character)

result1_df = result1 %>% as.data.frame %>% rownames_to_column()
result1_df %>% 
  left_join(result1_meta, by = c('rowname' = 'id'))



library(tidytext)
library(janeaustenr)

train_tag = train %>% select(id, tags)
train_tag = train_tag %>% mutate(tags = tags %>% str_remove_all("[']") %>% 
                                    str_remove_all("[,]") %>% str_sub(start = 2, end = -2),
                                 id = as.character(id)) %>% as_tibble()


train_tag %>% unnest_tokens(word, tags) %>% head(20)
count_train_tag = train_tag %>% unnest_tokens(word, tags) %>% count(id, word, sort = TRUE)
count_train_tag %>% arrange(desc(n))
count_train_tag %>% group_by(id) %>% summarise(total = sum(n))
count_train_tag %>% left_join(count_train_tag %>% group_by(id) %>% summarise(total = sum(n)))

total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)

library(methods)
train_tag
data("data_corpus_inaugural", package = "quanteda")
quanteda::dfm(train_tag, verbose = FALSE)

train_tag %>% unnest_tokens(word, tags) %>% 
  count(id, word)


idsong<-train %>% select(id, songs) %>% 
  mutate(songs = songs %>% 
           str_sub(start = 2, end = -2)) %>% 
  separate_rows(songs)

library(superml)
tf = TfIdfVectorizer$new(min_df = 1, smooth_idf = T, remove_stopwords = F)
train_tag_sample = train_tag[1:10000, ]
tf_feature = tf$fit_transform(train_tag_sample$tags)
tf_feature %>% colnames


train_tag_sample %>% separate_rows(tags)


train_tag = a

train_song_sample = train[1:10000, ] %>% select(id, songs) %>% mutate(songs = songs %>% str_remove_all("[']") %>% 
                                                    str_remove_all("[,]") %>% str_sub(start = 3, end = -3))
train_song_sample_id = train_song_sample %>% select(id) %>% unlist %>% as.vector
train_song_sample_gather = train_song_sample %>% separate_rows(songs)
train_song_sample_gather %>% glimpse
train_song_sample_unique = train_song_sample_gather$songs %>% unique

take_song = train_song_sample_gather$songs %>% as.vector

rec_mat = matrix(NA, nrow = 10000, ncol = length(train_song_sample_unique))
colnames(rec_mat) = train_song_sample_unique

for (i in 1:10) {
  index1 = train_song_sample_unique[i]
  index_same1 = which(take_song == index1)
  take_id = train_song_sample_gather[index_same1, 'id'] %>% unlist %>% as.vector()
  index_same2 = which(train_song_sample_id %in% take_id)
  rec_mat[index_same2, i] = 1
}
rec_mat[, 1:10]
sum(train_song_sample_id %in% take_id)





