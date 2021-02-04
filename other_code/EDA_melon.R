library(tidyverse)
library(tidyr)

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

## 장르 대분류와 소분류 코드 트리
make_tree_index = genre_gn_all$gnr_code %>% str_detect('0{2}')
make_tree_index
big_genre = genre_gn_all %>% .[make_tree_index, ]
big_genre

genre_gn_all %>% left_join(big_genre)

k = 0
for (i in seq_along(make_tree_index)) {
  if (make_tree_index[i] == 1) {
    k = k+1
    genre_gn_all$gnr_dtl_code[i] = big_genre$gnr_code[k]
    genre_gn_all$gnr_dtl_name[i] = big_genre$gnr_name[k]
  } else {
    genre_gn_all$gnr_dtl_code[i] = big_genre$gnr_code[k]
    genre_gn_all$gnr_dtl_name[i] = big_genre$gnr_name[k]
  }
}
genre_gn_all = genre_gn_all %>%  select(gnr_dtl_code, gnr_dtl_name, gnr_code, gnr_name) %>% as.data.frame %>% .[-make_tree_index, ]
# genre_gn_all %>% write.csv('data/genre_gn_all_tree.csv', row.names = F, fileEncoding = 'UTF-8')
genre_gn_all = data.table::fread('data/genre_gn_all_tree.csv', encoding = 'UTF-8')
genre_gn_all %>% glimpse

genre_gn_all %>% group_by(gnr_dtl_name) %>% summarise(n_of_genre = n()) %>% arrange(desc(n_of_genre)) %>% 
  ggplot(aes(x = reorder(gnr_dtl_name, n_of_genre), y = n_of_genre)) + geom_bar(stat = 'identity') + 
  coord_flip() + 
  labs(x = "장르 대분류", y = "대분류에 포함되는 개수", title = '장르 대분류별 소분류 개수') + 
  theme(axis.title = element_text(face = "bold"),
        axis.text = element_text(face = "bold", color = "grey50", size = 7),
        legend.title = element_text(face ="bold"),
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = 'bold', size = 20))

## 노래별 대분류 확인해보자
none_index = which(song_meta$song_gn_dtl_gnr_basket == '[]')
song_meta = song_meta[-none_index, ]


song_big_genre = 
  song_meta %>% select(id, song_gn_gnr_basket) %>% 
  mutate(song_gn_gnr_basket = song_gn_gnr_basket %>% str_sub(start = 3, end = -3)) %>% 
  separate_rows(song_gn_gnr_basket)
song_big_genre %>% View

song_big_genre_summary = 
  song_big_genre %>% group_by(id) %>% summarise(each_n = n()) %>% arrange(desc(each_n)) %>% 
  group_by(each_n) %>% summarise(n_genre = n(),
                                 prop_genre = n_genre/length(song_big_genre$id %>% unique))
song_big_genre_summary
# 시각화
song_big_genre_summary %>% ggplot(aes(x = reorder(each_n, n_genre), y = n_genre)) + geom_bar(stat = 'identity') + coord_flip()
## 대부분의 곡은 대분류를 1개 가지지만, 2개씩 가지는 경우도 있다.

song_meta %>% head
# 노래별 소분류 
song_small_genre = 
  song_meta %>% select(id, song_gn_dtl_gnr_basket) %>% 
  mutate(song_gn_dtl_gnr_basket = song_gn_dtl_gnr_basket %>% str_sub(start = 3, end = -3)) %>% 
  separate_rows(song_gn_dtl_gnr_basket)
song_small_genre

song_small_genre_summary = 
  song_small_genre %>% group_by(id) %>% summarise(each_n = n()) %>% arrange(desc(each_n)) %>% 
  group_by(each_n) %>% summarise(n_genre = n(),
                                 prop_genre = n_genre/length(song_small_genre$id %>% unique))
song_small_genre_summary

song_small_genre_summary %>% ggplot(aes(x = each_n, y = n_genre)) + 
  geom_bar(stat = 'identity', fill = 'limegreen', alpha = 1) + coord_flip()
song_small_genre_summary %>% ggplot(aes(x = each_n, y = n_genre)) + 
  geom_bar(stat = 'identity', fill = 'azure1', alpha = 1) + coord_flip()
# 보통 

song_meta %>% glimpse



## train 좋아요 횟수
train %>% glimpse

train$like_cnt %>% summary
boxplot(train$like_cnt, horizontal = T)
train %>% mutate(like_level = ifelse(like_cnt < 2, 1, 
                                     ifelse(like_cnt < 7, 2, 
                                            ifelse(like_cnt < 24, 3, 
                                                   ifelse(like_cnt < 96, 4, 
                                                          ifelse(like_cnt < 1000, 5, 6)))))) %>% 
  group_by(like_level) %>% summarise(n_like_level = n())

train %>% select(plylst_title, like_cnt, updt_date) %>% arrange(desc(like_cnt)) %>% head(40) %>% View

train %>% select(plylst_title, like_cnt, updt_date) %>% arrange(like_cnt) %>% head(40) %>% View
# 뭔가 이름부터 트렌디해야 사랑을 받는 느낌?


val %>% select(plylst_title, like_cnt, updt_date, id) %>% arrange(desc(like_cnt)) %>% head(80) %>% View
val %>% select(plylst_title, like_cnt, tags, songs, id) %>% arrange(desc(like_cnt)) %>% head(80) %>% View
val %>% select(plylst_title, like_cnt, tags, songs, id) %>% arrange(like_cnt) %>% head(80) %>% View
test %>% select(plylst_title, like_cnt, updt_date, id) %>% arrange(desc(like_cnt)) %>% head(80) %>% View
test %>% select(plylst_title, like_cnt, tags, songs, id) %>% arrange(desc(like_cnt)) %>% head(80) %>% View


## 곡별로 플레이리스트레 얼마나 있는지?
plylst_id_song = data.table::fread('data/idsong.csv', encoding = 'UTF-8')
plylst_id_song$V1 <- NULL
high_freq_song = plylst_id_song %>% group_by(songs) %>% summarise(number = n()) %>% arrange(desc(number)) %>% head(30)
high_freq_song
low_freq_song = plylst_id_song %>% group_by(songs) %>% summarise(number = n()) %>% arrange(desc(number)) %>% tail(30)

# 어떤 노래가 인기야?
high_freq_song %>% inner_join(song_meta %>% select(song_name, artist_name_basket, id), by = c('songs' = 'id')) 
low_freq_song %>% inner_join(song_meta %>% select(song_name, artist_name_basket, id), by = c('songs' = 'id')) 

# 어떤 아티스트가 인기야? various artist 좌를 거를수 없다...
high_freq_artist =
  plylst_id_song %>% group_by(songs) %>% summarise(number = n()) %>% 
  inner_join(song_meta %>% select(song_name, artist_name_basket, id) , by = c('songs' = 'id')) %>% 
  group_by(artist_name_basket) %>% summarise(total_artist = sum(number)) %>% arrange(desc(total_artist)) %>% head(30)

high_freq_artist %>% mutate(artist_name_basket = artist_name_basket %>% str_sub(start = 3, end = -3)) %>% .[1:15, ] %>% 
  ggplot(aes(x = reorder(artist_name_basket, -total_artist), y = total_artist, fill = total_artist)) + 
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(high = 'limegreen') + 
  theme_classic() + 
  labs(x = '', y = '', title = '플레이리스트 포함 곡 수 상위 15명 가수') + 
  theme(axis.text.x = element_text(face = "bold", color = "grey50", size = 16, angle = 30, vjust = 1, hjust = 1),
        legend.title = element_text(face ="bold"),
        legend.text = element_text(face = "bold"),
        plot.title = element_text(face = 'bold', size = 20),
        axis.text.y = element_text(face = "bold", color = "grey50", size = 15))





#train에서 id와 song, tag 분류

idsong<-train %>% select(id, songs) %>% 
  mutate(songs = songs %>% 
           str_sub(start = 2, end = -2)) %>% 
  separate_rows(songs)

idsong %>% head
#train_unique_song = idsong$songs %>% unique
#tag 떼어내기

ntag = data.table::fread('data/ntag.csv') %>% select(n_tag, tag_index)
ntag %>% head

tags<-data.frame(tags=train$tags)

tags = tags %>% mutate(tags=tags %>% str_remove_all("[']"),
                num_tag = tags %>% apply(1, function(x) str_count(x, ",") + 1))
tags %>% write.csv('data/tags.csv', row.names = F)


tags_v<-as.vector(tags2)
tags4=str_split(tags_v, ',',simplify = TRUE)
new_tag<-gsub("[[:punct:]]","",tags4)
new_tag[1]<-gsub("c","",new_tag)
new_tag
new_tag_1=gsub(pattern = "\\s",   
               replacement = "",
               x = new_tag)

#unique한 tag로 dataframe 만들기
length(unique(as.vector(new_tag_1)))
n_tag=unique(as.vector(new_tag_1))
tag_index=c(0:(length(unique(as.vector(new_tag_1)))-1))
ntag=data.frame(n_tag, tag_index)




#### d
#### TVT Visualization


type_like_df = data.table::fread('data/type_like.csv', encoding = 'UTF-8')
type_like_df = type_like_df %>% mutate(index = ifelse(type == 'train', '1_Train', ifelse(type == 'val', '2_Validation', '3_Test')))
type_like_df %>% ggplot() +  
  geom_bar(aes(x = `like_cnt`, y = `like_cnt의 수`, group = 1, fill = index), stat = 'identity') +
  facet_wrap(~index, nrow = 3, scales = "free_y") +
#+ scale_x_discrete(limits=sorted_columns) + 
  scale_color_hue(l=0.45) + 
  theme_light()  + 
  labs(title = 'Train-Validation-Test의 좋아요 분포',
       x = '좋아요 개수',
       y = '플레이리스트의 개수') 
  

type_year_df = data.table::fread('data/type_year.csv', encoding = 'UTF-8')
type_year_df = type_year_df %>% mutate(index = ifelse(type == 'train', '1_Train', ifelse(type == 'val', '2_Validation', '3_Test')))
type_year_df %>% ggplot() +  
  geom_bar(aes(x = `year`, y = `year의 수`, group = 1, fill = index), stat = 'identity') +
  facet_wrap(~index, nrow = 3, scales = "free_y") +
  #+ scale_x_discrete(limits=sorted_columns) + 
  scale_color_hue(l=0.45) + 
  theme_light()  + 
  labs(title = 'Train-Validation-Test의 연도별 분포',
       x = '연도',
       y = '플레이리스트의 개수') 


train_song = data.table::fread('TVTdata/train_song.csv', encoding = 'UTF-8')
train_song = train_song %>% group_by(id) %>% summarise(nsong =n()) %>% group_by(nsong) %>% summarise(freq = n())
train_song$type = 'train'
val_song = data.table::fread('TVTdata/val_song.csv', encoding = 'UTF-8')
val_song = val_song %>% group_by(id) %>% summarise(nsong =n()) %>% group_by(nsong) %>% summarise(freq = n())
val_song$type = 'val'
test_song = data.table::fread('TVTdata/test_song.csv', encoding = 'UTF-8')
test_song = test_song %>% group_by(id) %>% summarise(nsong =n()) %>% group_by(nsong) %>% summarise(freq = n())
test_song$type = 'test'

song = rbind(train_song, val_song, test_song)
song = song %>% mutate(index = ifelse(type == 'train', '1_Train', ifelse(type == 'val', '2_Validation', '3_Test')))
song %>% ggplot() +  
  geom_bar(aes(x = nsong, y = freq, group = 1, fill = index), stat = 'identity') +
  facet_wrap(~index, nrow = 3, scales = "free_y") +
  #+ scale_x_discrete(limits=sorted_columns) + 
  scale_color_hue(l=0.45) + 
  theme_light()  + 
  labs(title = 'Train-Validation-Test의 곡 개수 분포',
       x = '곡 개수',
       y = '플레이리스트의 개수') 


train_tag = data.table::fread('TVTdata/train_tag.csv', encoding = 'UTF-8')
train_tag = train_tag %>% group_by(id) %>% summarise(ntag =n()) %>% group_by(ntag) %>% summarise(freq = n())
train_tag$type = 'train'

val_tag = data.table::fread('TVTdata/val_tag.csv', encoding = 'UTF-8')
val_tag = val_tag %>% group_by(id) %>% summarise(ntag =n()) %>% group_by(ntag) %>% summarise(freq = n())
val_tag$type = 'val'

test_tag = data.table::fread('TVTdata/test_tag.csv', encoding = 'UTF-8')
test_tag = test_tag %>% group_by(id) %>% summarise(ntag =n()) %>% group_by(ntag) %>% summarise(freq = n())
test_tag$type = 'test'

tag = rbind(train_tag, val_tag, test_tag)
tag = tag %>% mutate(index = ifelse(type == 'train', '1_Train', ifelse(type == 'val', '2_Validation', '3_Test')))
tag %>% ggplot() +  
  geom_bar(aes(x = ntag, y = freq, group = 1, fill = index), stat = 'identity') +
  facet_wrap(~index, nrow = 3, scales = "free_y") +
  #+ scale_x_discrete(limits=sorted_columns) + 
  scale_color_hue(l=0.45) + 
  theme_light()  + 
  labs(title = 'Train-Validation-Test의 태그 개수 분포',
       x = '태그 개수',
       y = '플레이리스트의 개수')




train_song = data.table::fread('TVTdata/train_song.csv', encoding = 'UTF-8')
for_med =train_song %>% group_by(id) %>% summarise(nsong = n()) %>% group_by(id)
for_med$nsong %>% summary
train_song = train_song %>% group_by(id) %>% summarise(nsong =n()) %>% group_by(nsong) %>% summarise(freq = n())

train_song %>% ggplot() +  
  geom_bar(aes(x = nsong, y = freq, group = 1), stat = 'identity') +
  scale_fill_gradient(low = 'green yellow', high = 'limegreen')+
  scale_color_hue(l=0.45) + 
  theme_light()  + 
  labs(title = 'Train의 곡 개수 분포',
       x = '곡 개수',
       y = '플레이리스트의 개수')
  geom_vline(xintercept = sum(train_song$nsong * train_song$freq)/sum(train_song$freq), 
             linetype = 2, size = 1, colour = 'limegreen')
# median 30, mean 46

sum(train_song$nsong * train_song$freq)/sum(train_song$freq)



train_tag = data.table::fread('TVTdata/train_tag.csv', encoding = 'UTF-8')
for_med = train_tag %>% group_by(id) %>% summarise(ntag = n())
for_med$ntag %>% summary
train_tag = train_tag %>% group_by(id) %>% summarise(ntag =n()) %>% group_by(ntag) %>% summarise(freq = n())
train_tag %>% head

train_tag %>% ggplot() +  
  geom_bar(aes(x = ntag, y = freq, group = 1), stat = 'identity') +
  scale_fill_gradient(low = 'green yellow', high = 'limegreen')+
  scale_color_hue(l=0.45) + 
  theme_light()  + 
  labs(title = 'Train의 태그 개수 분포',
       x = '태그 개수',
       y = '플레이리스트의 개수')
  geom_vline(xintercept = sum(train_tag$ntag * train_tag$freq)/sum(train_tag$freq), linetype = 2, size = 1.5, colour = 'limegreen')
# median 3, mean 4.139

sum(train_tag$ntag * train_tag$freq)/sum(train_tag$freq)
