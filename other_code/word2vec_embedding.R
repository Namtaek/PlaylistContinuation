# word2vec embedding

library(wordVectors)
library(readr)
library(tidyverse)
library(tidyr)
library(recommenderlab)
library(reshape2)

train = data.table::fread('data/train.csv', encoding = 'UTF-8', data.table = F)
#train$tags %>% str_remove_all("[']") %>% str_remove_all("[,]") %>% str_sub(start = 2, end = -2) %>% 
#  write.csv('word2vec/train_w2v.txt', row.names = F)
song_meta = data.table::fread('data/song_meta.csv', encoding = 'UTF-8', data.table = F)
val = data.table::fread('data/val.csv', encoding = 'UTF-8', data.table = F)

#model = train_word2vec(train_file = "word2vec/train_w2v.txt",
#                       threads = 6,                  #cpu 개수
#                       vectors = 100,                #차원의 수
#                       force = T,
#                       min_count = 5,
#                       window = 4,                 #앞뒤에 볼 단어 수
#                       output_file = "word2vec/word2vec_tag_model.bin")
model = wordVectors::read.binary.vectors("word2vec/word2vec_tag_model.bin")
#model = model[nchar(rownames(model))>1,] %>% .[-1, ]
# mincount를 더 늘리면 불필요한 패턴들만 남음
# 4930행 100열

model = model[nchar(rownames(model))>1,] %>% .[-1, ]
twenty = model %>% row.names %>% .[1:20]
closest_to(model, model[['기분전환']], 20)

twenty_similar = matrix('', nrow = 20, ncol = 2) %>% as_tibble
for (i in 1:20) {
  twenty_similar[i, 1] = twenty[i]
  twenty_similar[i, 2] = closest_to(model, model[[twenty[i]]], 11) %>% .[2:11, 1] %>% str_flatten(', ')
}
twenty_similar %>% write.csv('data/twenty_similar.csv', row.names = F, fileEncoding = 'UTF-8')



closest_to(model, model[[twenty[1]]], 11) %>% .[2:11, 1] %>% str_flatten(', ')




#song_model = train_word2vec(train_file = "word2vec/train_song_w2v.txt",
#                           threads = 6,                  #cpu 개수
#                           vectors = 100,                #차원의 수
#                           force = T,
#                           min_count = 5,
#                           window = 6,                 #앞뒤에 볼 단어 수
#                           output_file = "word2vec/word2vec_song_model.bin")
song_model = wordVectors::read.binary.vectors("word2vec/word2vec_song_model.bin")
#song_model = song_model[nchar(rownames(song_model))>1,] %>% .[-1, ]
# 114675행 100열
result1 = closest_to(song_model, song_model[['144663']], 30)

result1_meta = song_meta[(song_meta$id %in% (result1[, 'word'])), ] %>% 
  select(song_name, artist_name_basket, id) %>% mutate(id = id %>% factor %>% as.character)
result1_meta

model = model[-1, ]
tag_model = model
train_tag = train %>% select(tags, id)
val_tag = val %>% select(tags, id)


train_tag$tags = train$tags %>% str_remove_all("[']") %>% str_remove_all("[,]") %>% str_sub(start = 2, end = -2)
val_tag$tags = val_tag$tags %>% str_remove_all("[']") %>% str_remove_all("[,]") %>% str_sub(start = 2, end = -2)
val$songs = val$songs %>% str_remove_all("[']") %>% str_remove_all("[,]") %>% str_sub(start = 2, end = -2)

tags_names = model %>% row.names()
model %>% dim
train %>% dim

train_playlist_tag_word2vec = matrix(0, nrow = 115081, ncol = 100)

#for (i in 1:115081) {
#  each_playlist_tag = train_tag[i, 'tags'] %>% str_split(' ') %>% .[[1]]
#  train_playlist_tag_word2vec[i, ] = model[which(tags_names %in% each_playlist_tag), ] %>% apply(2, mean)
#  if (i %% 1000 == 0) {
#    cat(i, ', ')
#  }
#}
#write.csv(train_playlist_tag_word2vec, 'data/train_playlist_tag_word2vec.csv', row.names = F)
train_playlist_tag_word2vec = data.table::fread('data/train_playlist_tag_word2vec.csv')

#vec_norm = function(x) sqrt(sum(x^2))
#vec_normalize = function(x) x/vec_norm(x)
#cosine_sim = function(x, y) vec_normalize(x) %*% vec_normalize(y)

#val %>% dim
#val_playlist_tag_word2vec = matrix(0, nrow = 23015, ncol = 100)
#for (i in 1:23015) {
#  each_playlist_tag = val_tag[i, 'tags'] %>% str_split(' ') %>% .[[1]]
#  val_playlist_tag_word2vec[i, ] = model[which(tags_names %in% each_playlist_tag), ] %>% apply(2, mean)
#  if (i %% 1000 == 0) {
#    cat(i, ', ')
#  }
#}
#val_playlist_tag_word2vec[val_playlist_tag_word2vec %>% is.nan()] = 1e-08

#write.csv(val_playlist_tag_word2vec, 'data/val_playlist_tag_word2vec.csv', row.names = F)
val_playlist_tag_word2vec = data.table::fread('data/val_playlist_tag_word2vec.csv') %>% as.matrix()

#val_playlist_tag_word2vec[1:5, 1:5]
#cosine_matrix = matrix(0, nrow = 23015, ncol = 4857)
#for (i in 1:23015) {
#  for (j in 1:4857) {
#    if ((val_playlist_tag_word2vec[i, ] %>% vec_norm()) < 1e-08) {
#      cosine_matrix[i, j] = 0
#    } else {
#      cosine_matrix[i, j] = cosine_sim(as.vector(val_playlist_tag_word2vec[i, ]), as.vector(model[j, ]))
#    }
#    
#  }
#  cat(i, ', ')
#}

#write.csv(cosine_matrix, 'data/cosine_matrix.csv', row.names = F)

model %>% closest_to(val_tag$tags[1] %>% str_split(' ') %>% unlist)

song_model %>% closest_to(val$songs[1] %>% str_split(' ') %>% unlist)










