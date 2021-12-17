raw_moon = readLines('speech_moon.txt', encoding = 'UTF-8')
raw_moon

# 문자 처리 library
# install.packages('stringr')
library(stringr)

# 문자 전처리(영어, 한자, 특수문자 제거)

txt = '치킨은!! 맛있다... dkfjd 정말 맛있다.'

str_replace_all(string = txt,   # 문자열 데이터
                pattern = '[^가-힣]',  # 한글 추출 정규표현식
                replacement = ' ')   # 대체문자

moon = raw_moon %>%
        str_replace_all('[^가-힣]', ' ')
moon

txt = '치킨은      맛있따    정말   맛있다'

# 여러칸 공백을 한칸의 공백으로 변경
str_squish(txt)

moon = moon %>% str_squish()
moon

library(dplyr)

# tibble 형태로 데이터 변경경
moon = as_tibble(moon)
moon

moon = raw_moon %>% 
        str_replace_all('[^가-힣]', ' ') %>%
          str_squish() %>%
            as_tibble()
moon                      

# text 다루는 library
# install.packages('tidytext')
library(tidytext)   # 문자열 토큰화

word_space = moon %>% unnest_tokens(input = value, 
                       output = word,
                       token = 'words') # 단어별  / 문장별은 sentence
word_space = word_space %>%
              count(word, sort = T)
word_space

word_space = word_space %>%
              filter(str_count(word) > 1)
word_space

top20 = word_space %>% head(20)

library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), 
                  y = n)) + geom_col() + coord_flip() # 그래프 가로로 눕히기기

# 박근혜 연설문 분석
raw_park = readLines('speech_park.txt', encoding = 'UTF-8')
raw_park

park = raw_park %>% 
  str_replace_all('[^가-힣]', ' ') %>%
  str_squish() %>%
  as_tibble()

word_space_park = park %>% unnest_tokens(input = value, 
                                    output = word,
                                    token = 'words')

word_space_park = word_space_park %>%
  count(word, sort = T)
word_space_park
word_space_park = word_space_park %>%
  filter(str_count(word) > 1)
word_space_park

top20_park = word_space_park %>% head(20)

ggplot(top20_park, aes(x = reorder(word, n),
                       y = n)) + geom_col() + coord_flip()

# install.packages("wordcloud")
library(wordcloud)

# install.packages("RColorBrewer")
library(RColorBrewer)

# install.packages("wordcloud2")
library(wordcloud2)

# wordcloud(words = word_space$word, freq = word_space$n, min.freq = 3, max.words=500, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# wordcloud2(data=word_space, size=1.6, color='random-dark', shape = 'circle')

# install.packages('ggwordcloud')
library(ggwordcloud)

ggplot(word_space, aes(label = word,
                       size = n,
                       col = n)) + geom_text_wordcloud() + scale_radius(limits = c(3, NA), range = c(3, 30)) + scale_color_gradient(low = '#31B404', high = '#0B6138') + theme_minimal()

#--

# 버전이 맞지 않을수 있음.
# install.packages('multilinguer')
library(multilinguer)

# install.packages('remotes')
library(remotes)

# gitlab에 있는 library 불러와 설치하기
# remotes::install_gitlab('mrchypark/multilinguer')
library(multilinguer)
# install_jdk()

# install.packages(c('hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = 'binary')

# remotes::install_github('haven-jeon/KoNLP', upgrade = 'never', INSTALL_opts = c('--no-multiarch'))

# install.packages('rJava')
library(rJava)
library(KoNLP)
SimplePos09('안녕하신가요?')

useNIADic()

word_noun = moon %>%
              unnest_tokens(input = value,
                            output = word,
                            token = extractNoun) # 명사만 추출출
word_noun

word_noun = word_noun %>%
              count(word, sort = T) %>%
                filter(str_count(word) > 1)
word_noun

sentences_moon = raw_moon %>%
                  str_squish() %>%
                    as_tibble() %>%
                      unnest_tokens(input = value,
                                    output = sentence,
                                    token = 'sentences')
sentences_moon

sentences_moon %>% filter(str_detect(sentence, '일자리'))

#--

raw_moon = readLines('speech_moon.txt', encoding = 'UTF-8')
moon = raw_moon %>% as_tibble() %>% mutate(president = 'moon')
moon

raw_park = readLines('speech_park.txt', encoding = 'UTF-8')
park = raw_park %>% as_tibble() %>% mutate(president = 'park')
park

bind_speeches = bind_rows(moon, park) %>% select(president, value)
bind_speeches

speeches = bind_speeches %>% mutate(value = str_replace_all(value, '[^가-힣]', ' '), value = str_squish(value))
speeches

speeches = speeches %>% unnest_tokens(input = value,
                                      output = word,
                                      token = extractNoun)
speeches

frequency = speeches %>% count(president, word) %>% filter(str_count(word) > 1)
frequency


# install.packages('tidyr')
library(tidyr)

frequency_wide = frequency %>%
                    pivot_wider(names_from = president,
                                values_from = n,
                                values_fill = list(n = 0))
frequency_wide  # 오즈비

frequency_wide = frequency_wide %>%
                  mutate(ratio_moon = ((moon+1)/sum(moon+1)),
                         ratio_park = ((park+1)/sum(park+1)))
frequency_wide

frequency_wide = frequency_wide %>%
                  mutate(odds_ratio = ratio_moon/ratio_park)
frequency_wide

frequency_wide %>% arrange(-odds_ratio) # 문
frequency_wide %>% arrange(odds_ratio)  # 박

top10 = frequency_wide %>% filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10

top10 = top10 %>% mutate(president = ifelse(odds_ratio > 1, 'moon', 'park'), n = ifelse(odds_ratio > 1, moon, park))
top10

ggplot(data = top10, aes(x = reorder_within(word, n, president), y = n, fill = president)) + geom_col() + coord_flip() + facet_wrap(~president, scales = 'free') + scale_x_reordered()

#--
# 감성분석
# install.packages('readr')
library(readr)
dic = read_csv('knu_sentiment_lexicon.csv')
dic

df = tibble(sentence = c('디자인 예쁘고 마감도 좋아서 만족스럽다.', '디자인 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다.'))
df

df = df %>% unnest_tokens(input = sentence,
                          output = word,
                          token = 'words',
                          drop = F)
df

df = df %>% 
      left_join(dic, by = 'word') %>%
        mutate(polarity = ifelse(is.na(polarity), 0, polarity)) 
# 단어가 dic에 있으면 점수 넣고 없으면 0점 넣는다
df

score_df = df %>% 
            group_by(sentence) %>%
              summarise(score = sum(polarity))
score_df

# 충격, 소름 등 이중적으로 사용 가능한 내용들은 데이터를 하나씩 보고
# 사전을 하나하나 수정할 필요가 있음
# 감성사전 신뢰기관 등 업데이트 자료 받아서 사용하면 됨됨