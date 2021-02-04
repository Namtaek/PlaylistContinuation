# TVT 추가적인 비교

library(tidyverse)
library(tidyr)

library(wordVectors)
library(readr)

genre_gn_all = data.table::fread('data/genre_gn_all.csv', encoding = 'UTF-8', data.table = F)
genre_gn_all

song_meta = data.table::fread('data/song_meta.csv', encoding = 'UTF-8', data.table = F)
song_meta %>% glimpse

train = data.table::fread('data/train.csv', encoding = 'UTF-8', data.table = F)
train %>% glimpse

val = data.table::fread('data/val.csv', encoding = 'UTF-8', data.table = F)
val %>% glimpse

test = data.table::fread('data/test.csv', encoding = 'UTF-8', data.table = F)
test %>% glimpse

# TVT song의 형태
idsong <- train %>% select(id, songs) %>% 
  mutate(songs = songs %>% 
           str_sub(start = 2, end = -2)) %>% 
  separate_rows(songs)
train_unique_song = idsong$songs %>% unique
train_unique_song1 %>% length # 615142

idsong <- train[1:10000, ] %>% select(id, songs) %>% 
  mutate(songs = songs %>% 
           str_sub(start = 2, end = -2)) %>% 
  separate_rows(songs)
train_unique_song = idsong$songs %>% unique
train_unique_song %>% length # 157446


val_idsong <- val %>% select(id, songs) %>% 
  mutate(songs = songs %>% 
           str_sub(start = 2, end = -2)) %>% 
  separate_rows(songs)
val_unique_song = val_idsong$songs %>% unique
val_unique_song %>% length # 149880

test_idsong <- test %>% select(id, songs) %>% 
  mutate(songs = songs %>% 
           str_sub(start = 2, end = -2)) %>% 
  separate_rows(songs)
test_unique_song = test_idsong$songs %>% unique
test_unique_song %>% length() # 91352

val_unique_song %>% setdiff(train_unique_song) %>% length # 23195
test_unique_song %>% setdiff(train_unique_song) %>% length # 11311

high_freq_val_song = val_idsong %>% group_by(songs) %>% summarise(number = n()) %>% arrange(desc(number))
high_freq_val_song = high_freq_val_song[-1, ]
high_freq_val_song[which(high_freq_val_song$songs %in% (val_unique_song %>% setdiff(train_unique_song))), ] %>% 
  arrange(desc(number))

high_freq_test_song = test_idsong %>% group_by(songs) %>% summarise(number = n()) %>% arrange(desc(number))
high_freq_test_song = high_freq_test_song[-1, ]
high_freq_test_song[which(high_freq_test_song$songs %in% (test_unique_song %>% setdiff(train_unique_song))), ] %>% 
  arrange(desc(number))

# 주요하게 새로 등장하는 곡들은 없다!

# TVT 태그의 형태
train_tag = data.table::fread('data/train_tag.csv', encoding = 'UTF-8', data.table = F)
val_tag = data.table::fread('data/val_tag.csv', encoding = 'UTF-8', data.table = F)
test_tag = data.table::fread('data/test_tag.csv', encoding = 'UTF-8', data.table = F)

train_tag_unique = train_tag$tags %>% unique
train_tag_unique %>% length # 29160개

val_tag_unique = val_tag$tags %>% unique
val_tag_unique %>% length # 4875개

test_tag_unique = test_tag$tags %>% unique
test_tag_unique %>% length # 2824개

val_tag_unique %>% setdiff(train_tag_unique) %>% length  # 1037
test_tag_unique %>% setdiff(train_tag_unique) %>% length  # 470

high_freq_val_tag = val_tag %>% group_by(tags) %>% summarise(number = n()) %>% arrange(desc(number))
high_freq_val_tag = high_freq_val_tag[-1, ]
high_freq_val_tag[which(high_freq_val_tag$tags %in% (val_tag_unique %>% setdiff(train_tag_unique))), ] %>% 
  arrange(desc(number)) 

high_freq_test_tag = test_tag %>% group_by(tags) %>% summarise(number = n()) %>% arrange(desc(number))
high_freq_test_tag = high_freq_test_tag[-1, ]
high_freq_test_tag[which(high_freq_test_tag$tags %in% (test_tag_unique %>% setdiff(train_tag_unique))), ] %>% 
  arrange(desc(number)) %>% View

# 주요하게 새로 등장하는 태그들은 없다.


## validation/test에도 태그 오타가 있을까~?

#val$tags %>% str_remove_all("[']") %>% str_remove_all("[,]") %>% str_sub(start = 2, end = -2) %>% 
#  write.csv('word2vec/val_w2v.txt', row.names = F)
#val_model = train_word2vec(train_file = "word2vec/val_w2v.txt",
#                           threads = 6,                  #cpu 개
#                           vectors = 100,                #차원의 수
#                           force = T,
#                           window = 4,                 #앞뒤에 볼 단어 수
#                           output_file = "word2vec/word2vec_val_model.bin")
val_model = wordVectors::read.binary.vectors("word2vec/word2vec_val_model.bin")
val_model = val_model %>% .[-1, ]
val_model %>% View

closest_to(val_model, val_model[['휴식']], 5)
closest_to(val_model, val_model[['기분전환']], 10)
closest_to(val_model, val_model[['사랑']], 10)

closest_to(val_model, val_model[['겨울']], 10)
closest_to(val_model, val_model[['캐롤']], 10)

# 오타는 딱히 없다...!!!










