a = 1
b <- 2

a + b
a * b
a - b
a / b

a = c(1, 2, 3, 4, 5)
a + 1

a = c(1:5)
a

a = seq(1, 10)
a = seq(1, 10, by = 2)
a

a = 'hello'
a = c('hello', 'world!', 'master')

a = c(1, 2, 3, 4, 5)
mean(a)
max(a)
min(a)

paste(a, collapse = ' ')

install.packages('ggplot2')
library(ggplot2) # R 시각화 패키지

a = c('a', 'a', 'b', 'c')
qplot(a)

mpg2 = mpg

qplot(data = mpg2, x = hwy)
qplot(data = mpg2, x = cty, y = hwy)
qplot(data = mpg2, x = cty, y = hwy, geom = 'line')
qplot(data = mpg2, x = drv, y = hwy, geom = 'boxplot')
qplot(data = mpg2, x = drv, y = hwy, geom = 'boxplot', color = drv)

# 퀴즈 1 : 80, 60, 70, 50, 90
mean(c(80, 60, 70, 50, 90))

#--

english = c(90, 80, 60, 70)
math = c(10, 20, 30, 40)

df = data.frame(english, math)
df

mean(df$english)

#--
# R에서 CSV는 괜찮지만 Excel파일은 추가 라이브러리가 필요하다.
install.packages('readxl')
library(readxl)

df = read_excel('excel_exam.xlsx')
df

mean(df$math)

# 열이름 없는 깔끔하지 않은 데이터 처리
# columns이 없음을 지정하면 1,2,3,4,5로 처리
df = read_excel('excel_exam_novar.xlsx', col_names = F)
df

# 다른 엑셀 시트에 있는 데이터 가져오기
df = read_excel('excel_exam_sheet.xlsx', sheet = 3)
df

#--

df = read.csv('csv_exam.csv')
df

# CSV 파일로 저장
write.csv(df, file = 'df_csv.csv')
# R에서만 호환되는 RDS 파일로 저장
saveRDS(df, file = 'df_RDS.rds')

# RDS 파일 불러오기
df = readRDS('df_RDS.rds')
df

exam = read.csv('csv_exam.csv')
exam

head(exam)
head(exam, 10)

tail(exam)
tail(exam, 10)

# shape 확인
dim(exam)

# 속성 파악
str(exam)

# 요약
summary(exam)

dim(mpg2)

#--

df = data.frame(var1 = c(1,2,3),
                var2 = c(4,5,6))
df

install.packages('dplyr')
library(dplyr)

df2 = rename(df, v2 = var2)
df2

# 퀴즈 : mpg2 cty(국도연비) -> city, hwy(고속도로연비) -> highway 로 바꾸기
mpg3 = rename(mpg2, city = cty, highway = hwy)
mpg3

df = data.frame(var1 = c(1,2,3),
                var2 = c(4,5,6))

# 새로운 열 추가하기
df$sum = df$var1 + df$var2
df

df$mean = df$sum / 2
df

mpg3$total = (mpg3$city + mpg3$highway) / 2
mpg3$test = ifelse(mpg3$total >= 20, 'pass', 'fail')

# 데이터 빈도 확인
table(mpg3$test)

#--

qplot(mpg3$test)

# 퀴즈 : mpg3$grade를 새로 만들고
# total >= 30 'A', 20 >= 'B', 'C'
mpg3$grade = ifelse(mpg3$total >= 30, 'A', ifelse(mpg3$total>=20, 'B', 'C'))
table(mpg3$grade)
qplot(mpg3$grade)  

#--

# 원하는 조건만 출력
exam %>% filter(class == 1)
exam %>% filter(class != 1)
exam %>% filter(math > 50)
exam %>% filter(class == 1 & math >= 80)
exam %>% filter(math >= 90 | english >= 90 | science >= 90)
# 1, 3, 5반 필터링
exam %>% filter(class %in% c(1, 3, 5))

# 퀴즈1 : displ(배기량량)
# 배기량 4이하, 5이상 자동차 중 어떤 hwy가 평균적으로 더 좋은지?
a = mpg3 %>% filter(displ <= 4)
mean(a$highway)
b = mpg3 %>% filter(displ >= 5)
mean(b$highway)

# 퀴즈2 : audi, toyota 중에서 누구의 cty가 평균적으로 더 좋은지?
audi = mpg3 %>% filter(manufacturer == 'audi')
mean(audi$city)
toyota = mpg3 %>% filter(manufacturer == 'toyota')
mean(toyota$city)

# 퀴즈3 : %in% 을 사용해서
# chevrolet, ford, honda 자동차의 고속도로 연비 평균
a = mpg3 %>% filter(manufacturer %in% c('chevrolet', 'ford', 'honda'))
mean(a$highway)

#--
exam %>% select(english)
exam %>% select(english, math)
exam %>% select(-math, -english)

exam %>% 
  filter(class == 1) %>% 
    select(math, english) %>%
      head

#--

# 수학점수로 정렬렬
exam %>% arrange(math)
exam %>% arrange(desc(math))
exam %>% arrange(class, math)

# 퀴즈 : audi에서 생산한 자동차 중에서
# 어떤 자동차 모델의 hwy가 높은지 1위 ~ 5위
mpg3 %>% filter(manufacturer == 'audi') %>%
  select(model, highway) %>% 
    arrange(desc(highway))
      %>% head(5)

#--

exam %>%
  mutate(total = math + english + science,
         mean = total / 3)

exam %>%
  mutate(test = ifelse(science >= 60, 'pass', 'fail'))

#--

# groupby 함수 활용
exam %>% summarise(mean = mean(math))
exam %>% group_by(class) %>%
  summarise(mean_math = mean(math),
            mean_english = mean(english),
            mean_science = mean(science))

mpg3 %>%
  group_by(manufacturer, drv) %>%
    summarise(mean_cty = mean(city))

# 퀴즈 : 회사별로 suv 자동차의 도시 및 고속도로 연비 평균을 구해서 
# 내림차순으로 정렬하고 1~5위까지 출력

# 내 풀이
mpg3 %>%
    group_by(manufacturer, model) %>%
      filter(class == 'suv') %>%
        mutate(total = (city + highway)/2) %>%
          summarise(mean_hwy = mean(total)) %>%
            arrange(desc(mean_hwy)) %>% head(5)

# 풀이
mpg3 %>%
  group_by(manufacturer) %>%
    filter(class == 'suv') %>%
      mutate(total = (city + highway)/2) %>%
        summarise(mean = mean(total)) %>%
          arrange(desc(mean)) %>% head(5)

# 퀴즈 : class별 city의 평균(내림차순)
mpg3 %>%
  group_by(class) %>%
    summarise(mean = mean(city)) %>%
      arrange(desc(mean))

# mutate는 기존 df에 새로운 컬럼 추가
# summarise는 새로운 df 생성

mpg3 %>% filter(class == 'compact') %>%
  group_by(manufacturer) %>%
    summarise(count = n())

# 퀴즈 : 어떤 회사 자동차의 고속도로 연비가 가장 높은지 세곳
mpg3 %>%
  group_by(manufacturer) %>%
    summarise(mean = mean(highway)) %>%
      arrange(desc(mean)) %>% head(3)

#--

a = data.frame(id = c(1,2,3,4,5),
               mid = c(60,80,70,90,85))
a

b = data.frame(id = c(1,2,3,4,5),
               final = c(70,85,65,5,80))

result = left_join(a, b, by = 'id')
result

#--

a = data.frame(id = c(1,2,3,4,5),
               test = c(60,80,45,100,25))
b = data.frame(id = c(6,7,8,9,10),
               test = c(40,20,30,50,70))
result = bind_rows(a, b)
result

#--

df = data.frame(sex = c('M', 'F', NA, 'M', 'F'),
                score = c(5,4,3,4,NA))
df

mean(df$score)

table(is.na(df))
table(is.na(df$sex))
table(is.na(df$score))

# 특정 열 결측치 필터링
df %>% filter(!is.na(score) & !is.na(sex))

# 단 하나라도 빈칸이 있으면 row를 삭제
na.omit(df)

# na.rm argument 사용
mean(df$score, na.rm = T)

exam[c(3, 8, 15), 'math'] = NA
exam

exam$math = ifelse(is.na(exam$math), mean(exam$math, na.rm = T), exam$math)
exam

#--
# 이상치 처리
a = data.frame(sex = c(1,2,1,3,2,1),
               score = c(5,4,3,4,2,6))
a

table(a$sex)
table(a$score)

a$sex = ifelse(a$sex == 3, NA, a$sex)
a$score = ifelse(a$score >5, NA, a$score)
a

boxplot(mpg3$highway)
boxplot(mpg3$highway)$stats

mpg3$highway = ifelse(mpg3$highway <12 | mpg3$highway > 37, NA, mpg3$highway)
boxplot((mpg3$highway))

#--

ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + xlim(3, 6) + ylim(10, 30)

# 퀴즈 : cty와 hwy 산점도
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()


# 퀴즈2
# x축 : poptotal(전체인구)
# y축 : popasian(아시아인 인구)
# 전체인구는 50만명, 아시아인 인구는 1만명
midwest2 = midwest
midwest2
ggplot(data = midwest2, aes(x = poptotal, y = popasian)) + geom_point() + xlim(0, 500000) + ylim(0, 10000)

#--

a = mpg %>% group_by(drv) %>% summarise(mean = mean(hwy))
a

ggplot(data = a, aes(x = drv, y = mean)) + geom_col()

ggplot(data = a, aes(x = reorder(drv, mean), y = mean)) + geom_col()
ggplot(data = a, aes(x = reorder(drv, -mean), y = mean)) + geom_col()

ggplot(data = mpg, aes(x = drv)) + geom_bar()

#--

# 퀴즈1 : suv 차종을 대상으로 평균 cty가 가장 높은 회사 5곳을 막대 그래프로 표현(높은순 정렬)
q1 = mpg %>% 
        filter(class == 'suv') %>%
          group_by(manufacturer) %>% 
            summarise(mean = mean(cty)) %>%
              arrange(desc(mean)) %>%
                
q1
ggplot(data = q1, aes(x = reorder(manufacturer,-mean), y = mean)) + geom_col()

# 퀴즈2 : 어떤 class(자동차 종류)가 가장 많은지 빈도 분석 막대그래프 표현
q2 = mpg %>% count(class)
q2

ggplot(data = q2, aes(x = class, y = n)) + geom_col()

ggplot(data = mpg, aes(x = class)) + geom_bar()

#--

economics2 = economics
economics2

ggplot(data = economics2, aes(x = date, y = unemploy)) + geom_line()
ggplot(data = economics2, aes(x = date, y = psavert)) + geom_line()

#--

ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

# 퀴즈 : class가 compact, subcompact, suv 인 자동차의 cty boxplot
q = mpg %>% filter(class %in% c('compact', 'subcompact', 'suv'))
q
ggplot(data = q, aes(x = class, y = cty)) + geom_boxplot()

#--

install.packages('foreign')
library(foreign)

raw = read.spss(file = 'Koweps_hpc10_2015_beta1.sav', to.data.frame = T)
raw

walfare = raw
walfare <- rename(walfare, 
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,     # 종교
                  income = p1002_8aq1,    # 월급
                  code_job = h10_eco9,    # 직업 코드
                  code_region = h10_reg7) # 지역 코드
walfare

# 성별에 따른 월급 차이
walfare$sex = ifelse(walfare$sex == 1, 'male', 'female')

summary(walfare$income)

table(walfare$sex)
table(is.na(walfare$income))
# walfare$income = ifelse(is.na(walfare$income), mean(walfare$income, na.rm = T), walfare$income)

wal_income = walfare %>% group_by(sex) %>% summarise(mean_income = mean(income, na.rm = T))
wal_income
ggplot(data = wal_income, aes(x = sex, y = mean_income)) + geom_col()

# q 풀이
table(walfare$sex)

walfare$income = ifelse(walfare$income %in% c(0, 9999), NA, walfare$income)
table(is.na(walfare$income))

sex_income = walfare %>% 
              filter(!is.na(income)) %>% 
                group_by(sex) %>%
                  summarise(mean_income = mean(income))
sex_income  

ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

#--

# 나이와 월급의 관계
# 몇살에 월급을 가장 많이 받을까?
table(is.na(walfare$birth))
summary(walfare$birth)

walfare$age = 2015 - walfare$birth + 1
walfare$age

# 나이에 따른 월급 평균표
q_income = walfare %>% group_by(age) %>% summarise(mean_income = mean(income, na.rm = T))
q_income
age_income = walfare %>% filter(!is.na(income)) %>% group_by(age) %>% summarise(mean_income = mean(income))
age_income

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#-- 
walfare = walfare %>%
            mutate(ageg = ifelse(age < 30, 'young', ifelse(age <= 59, 'middle', 'old')))
walfare$ageg

# 퀴즈 : 연령대 및 성별 월급표
age_sex_income = walfare %>% 
                  filter(!is.na(income)) %>% 
                    group_by(ageg, sex) %>% 
                      summarise(mean_income = mean(income))
age_sex_income

ggplot(data = age_sex_income, aes(x = ageg, y = mean_income, fill = sex)) + geom_col(position = 'dodge')

#--

sex_age = walfare %>%
            filter(!is.na(income)) %>%
              group_by(age, sex) %>%
                summarise(mean_income = mean(income))
sex_age

ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

