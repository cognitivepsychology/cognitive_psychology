# 1. 말뭉치 원자료 파일을 분석 가능한 형식으로 변환하기

# 2. 말뭉치 원자료에 대한 전처리 작업 수행하기

## (1) 말뭉치 원자료 파일 내려받기
# 압축된 말뭉치 원자료 파일의 url 주소 축약하기.
raw <- "https://raw.github.com/cognitivepsychology/cognitive_psychology/master/RawData/RawData.zip"
# 압축된 말뭉치 원자료 파일 내려받기.
download.file(raw, "raw.zip")
# 압축된 말뭉치 원자료 파일을 RawData 폴더에 풀기.
unzip("raw.zip", exdir="RawData")

## (2) 말뭉치 원자료 파일 가공하기

# 모든 말뭉치 원자료 파일명에 대한 목록 작성하기.
whole <- list.files(path = "RawData/", pattern = "*CT*")
# 말뭉치 원자료 파일이 저장된 디렉토리를 워킹 디렉토리로 변경하기.
setwd("RawData/")
# 자료 변형 및 가공을 도와주는 패키지 plyr 불러오기.
library(plyr) 
# 목록 내 모든 말뭉치 원자료 파일의 텍스트 라인 읽어들이기.
list.whole <- llply(whole, readLines)
# 파일명이 들어가 있으면서 "<"를 지닌 부분을 ""으로 바꾸기. 
whole.1 <- llply(list.whole, function(x) gsub("[0-9]CT.*<", "", x)) 
# "<"가 없는 라인들만 추리기(해당 말뭉치 파일에 대한 앞부분의 소개글 제거).
whole.2 <- llply(whole.1, function(x) grep("[0-9]CT", x, value=T)) 
# 라인 번호와 형태소 분석 태그 없는 어절을 지우고, 태그된 어절만 남기기.
whole.3 <- llply(whole.2, function(x) gsub("[0-9]CT.*\t.*\t", "", x)) 
# 각종 punctuation 기호 제거(단, 사선 "/"만 남겨둠).
whole.4 <- llply(whole.3, function(x) gsub("[]$*+.?[^{|(\\#%&~_<=>'!,:;`\")}@-]", "", x))
# 말뭉치 원자료 파일에 반복적으로 나타나는 알파벳 "c" 없애기.
whole.5 <- llply(whole.4, function(x) gsub("c", "", x)) 
# 부호를 나타내는 태그(/SF, /SP, /SS, /SE, /SO, ./SW) 제거.
whole.6 <- llply(whole.5, function(x) gsub("/SF|/SP|/SS|/SE|/SO|/SW", "", x))
# 일차 가공이 완료된 200개의 자료 파일을 합쳐 하나의 R 텍스트 벡터로 만들기.
whole.7 <- paste(whole.6, collapse=" ")

# 텍스트 마이닝 패키지 stylo 불러오기.
library(stylo) 
# 텍스트의 단위를 행(line)에서 어절(word)로 바꿔줌.
whole.words <- txt.to.words(whole.7, splitting.rule ="[ \t\n]+", preserve.case = T) 
# 잡다한 punctuation 기호 없애기(단, "/"는 남겨둠).
whole.words.1<- gsub("[]$*+.?[^{|(\\#%&~_<=>'!,:;`\")}@-]", "", whole.words) 
# 일차 가공된 자료를 두 단어 연쇄 단위로 분절함.
whole.2gram <- make.ngrams(whole.words.1, ngram.size = 2)
# 일차 가공된 자료를 세 단어 연쇄로 분절함. 이는 목적어+부사어+서술어 연쇄를 가려내기 위해 사용될 것임.
whole.3gram <- make.ngrams(whole.words.1, ngram.size = 3)

# JKO(목적격 조사 표지)와 VV(일반동사) 또는 JKO와 XSV(파생동사)로 구성된 두 단어 연쇄만 추리기.
whole.2gram.n.v <- grep("JKO[ \t\n]+[^a-zA-Z]+VV|JKO[ \t\n]+.+XSV", whole.2gram, value=T)
# R에 내장된 기본 텍스트 마이닝 명령어인 grep, gsub의 기능이 확장된 텍스트 마이닝 패키지 stringr 불러오기.
library(stringr) 
# 용언+명사형 전성어미/종결어미 구성(총 286개)이 앞에 오는 경우만 제외하기.
whole.2gram.n.v <- grep("^+.+/ETN+.+[ \t\n\r\f\v]|^+.+/EF+.+[ \t\n\r\f\v]", whole.2gram.n.v, value = T, invert = T) 
# VV 어간만 남기고, 분석의 편의를 위해 그 뒤에 마침표를 붙이기.
whole.2gram.n.v.root <- str_replace_all(whole.2gram.n.v, c("VV+.+."), "VV.")
# XSV 어간만 남기고, 분석의 편의를 위해 그 뒤에 마침표를 붙이기.
whole.2gram.n.v.root.1 <- str_replace_all(whole.2gram.n.v.root, c("XSV+.+."), "XSV.")  
# 분석의 편의를 위해 "ㄹ, 을, 를/JKO"를" "을/JKO"로 통일하기.
whole.2gram.n.v.root.2 <- str_replace_all(whole.2gram.n.v.root.1, "를/JKO", "을/JKO")
whole.2gram.n.v.root.2 <- str_replace_all(whole.2gram.n.v.root.2, "ㄹ/JKO", "을/JKO")
# 목적어 + 서술어 연쇄 목록 벡터.
whole.2gram.n.v.root.2

# 세 단어 연쇄 자료에서 목적어(JKO) + 부사어[형용사(vA)+연결어미/부사(MAG)] + 서술어 연쇄 찾아내기.
whole.3gram.n.a.v <- grep("JKO[ \t\n]+[^a-zA-Z]+VA+.+VV|JKO[ \t\n]+[^a-zA-Z]+VA+.+XSV|JKO[ \t\n]+[^a-zA-Z]+MAG+.+VV|JKO[ \t\n]+[^a-zA-Z]+MAG+.+XSV", whole.3gram, value=T) 
# 부사(MAG)+동사 파생 접미사(XSV) 결합된 세 단어 연쇄를 목록에서 제거하기(이 구성은 이미 두 단어 연쇄 목록에 있음).
whole.3gram.n.a.v.1 <- gsub("[^a-zA-Z]/MAG+[^a-zA-Z]/XSV", "", whole.3gram.n.a.v)
# 세 단어 연쇄 자료에서 목+부+술 연쇄만 추리기.
whole.3gram.n.a.v.2 <- grep("JKO[ \t\n]+[^a-zA-Z]/VA+.+/VV|JKO[ \t\n]+[^a-zA-Z]/VA+.+/XSV|JKO[ \t\n]+[^a-zA-Z]/MAG+.+/VV|JKO[ \t\n]+[^a-zA-Z]/MAG+.+/XSV", whole.3gram.n.a.v.1, value=T) 
# 부사(MAG)를 제거하여 목+부+술 연쇄를 목+술 연쇄로 만들기.
whole.3gram.n.a.v.3 <- str_replace_all(whole.3gram.n.a.v.2, "[^a-zA-Z]/MAG", "")
# 부사어(형용사+연결어미)를 제거하여 목+부+술 연쇄를 목+술 연쇄로 만들기.
whole.3gram.n.a.v.4 <- str_replace_all(whole.3gram.n.a.v.3, "[^a-zA-Z]/VA+.+[ \t\n\r\f\v]", " ")  
# 스페이스 두 개를 한 개로 줄이기.
whole.3gram.n.a.v.4 <- str_replace_all(whole.3gram.n.a.v.4, "  ", " ")
# 용언+명사형 전성어미/종결어미 구성(총 12개)이 앞에 오는 경우만 제외하기.
whole.3gram.n.a.v.5 <- grep("^+.+/ETN+.+[ \t\n\r\f\v]|^+.+/EF+.+[ \t\n\r\f\v]", whole.3gram.n.a.v.4, value =T, invert = T) 
# VV 어간만 남기고, 분석의 편의를 위해 그 뒤에 마침표를 붙이기.
whole.3gram.n.a.v.6 <- str_replace_all(whole.3gram.n.a.v.5, c("VV+.+."), "VV.")
# XSV 어간만 남기고, 분석의 편의를 위해 그 뒤에 마침표를 붙이기.
whole.3gram.n.a.v.7 <- str_replace_all(whole.3gram.n.a.v.6, c("XSV+.+."), "XSV.")  
# "ㄹ, 을, 를/JKO"을" "을/JKO"로 통일하기.
whole.3gram.n.a.v.8 <- str_replace_all(whole.3gram.n.a.v.7, "를/JKO", "을/JKO")
whole.3gram.n.a.v.8 <- str_replace_all(whole.3gram.n.a.v.8, "ㄹ/JKO", "을/JKO")
# 목+부+술 연쇄들 가운데서 추린 목+술 연쇄 목록 벡터.
whole.3gram.n.a.v.8 

# 두 단어 연쇄와 세 단어 연쇄 목록 벡터를 합쳐 최종 목+술 연쇄 목록 벡터 완성하기.
whole.nv <- append(whole.2gram.n.v.root.2, whole.3gram.n.a.v.8) 
# 연어구성이 될 수 없는 고유명사(NNP)/수사(NNB)/의존명사(NR)/대명사(NP) + 서술어 구성 등을 제외한 일반명사(NNG) + 동사(VV) 구성만 추출하기.
whole.nv0 <- grep("NNG+.*을", whole.nv, value = T) 
# 분석에 불필요한 "/"(슬래시) 없애기.
whole.nv1 <- str_replace_all(whole.nv0, "[]$*+.?[^{|(\\#%&~_/<=>'!,:;`\")}@-]", "") 
# 입력의 편의를 위해 벡터명을 짧은 것으로 바꾸기.
x <- whole.nv1 
# 목적어(word1), 서술어(word2), 목+술 연쇄(word1+word2)가 하나의 행을 이루는 데이터프레임 만들기.
whole.nv2 <- data.frame(cbind(do.call('rbind', strsplit(x, " ")), x)) 
# 열 제목을 "word1", "word2", "bigram"으로 바꾸기.
colnames(whole.nv2) <- c("word1", "word2", "bigram")

# 3. 가공된 말뭉치 자료에 분석을 위한 각종 양적 변수 추가하기

## (1) 가공된 말뭉치 자료 토대로 빈도 항목 추가하기

# 데이터프레임 자료를 좀더 간편하게 가공할 수 있도록 도와주는 패키지인 data.table 패키지 불러오기.
library(data.table) 
# words & 2-gram 자료 whole.nv3를 데이터테이블 형식으로 변환하기.
whole.nv2.dt <- data.table(whole.nv2)
# 목적어(word1), 서술어(word2), 목+술 연쇄(bigram), 목적어(word1) 빈도, 서술어(word2) 빈도, 목+술 연쇄(bigram) 빈도 열로 구성된 데이터프레임 완성하기.
a <- whole.nv2.dt[, freq.bi := .N, by=bigram] 
b <- whole.nv2.dt[, freq.w1 := .N, by=word1]
c <- whole.nv2.dt[, freq.w2 := .N, by=word2]
d <- data.frame(c)
e <- d[, c(1, 2, 3, 5, 6, 4)] 
# bigram 열 제목 "freq"를" "freq.bi"로 수정하기.
colnames(e)[6] <- "freq.bi"
# 분석의 편의를 위해 데이터프레임 이름을 whole.nv.df로 변경하기.
whole.nv.df <- e 

# 중복되는 행 제거하기.
whole.nv.df.uni <- unique(whole.nv.df)
# 중복자료 제거 전 데이터 행 수(N = 19652).
nrow(whole.nv.df)
# 중복자료 제거 후 데이터 행 수(N = 9342).
nrow(whole.nv.df.uni) 
# 입력의 편의를 위해 데이터프레임 이름 수정하기.
whole.uni <- whole.nv.df.uni 

# 공기빈도가 2회 이상인 자료만 남기고, 나머지는 NA(not avaible)로 처리하기.
test1 <- ifelse(whole.uni$freq.bi > 1, whole.uni$freq.bi, NA) 
whole.uni.test1 <- cbind(whole.uni, test1)
# NA가 있는 행(공기빈도 2회 미만인 목+술 연쇄) 제거하기.
whole.uni.one <- na.omit(whole.uni.test1) 
# 행 수(두 단어 연쇄의 수) 확인(N = 2340).
nrow(whole.uni.one)
# bigram열과 중복되는 test1열 지우기.
whole.one <- whole.uni.one[, 1:6] 

# o11열(특정 목+술 연쇄 빈도) 생성하기.
o11 <- whole.one[, 6] 
# o12열(w1 - o11 빈도) 생성하기.
o12 <- whole.one[, 4] - whole.one[, 6] 
# o21열(w2 - o11 빈도) 생성하기.
o21 <- whole.one[, 5] - whole.one[, 6] 
# o22열(전체 목+술 구조 출현형[token] - w1 - w2 + o11 빈도) 생성하기.
o22 <- nrow(whole.nv.df) - whole.one[, 4] - whole.one[, 5] + whole.one[, 6] 
# o11, o12, o21, o22 열들 하나로 합치기.
whole.one.ct <- cbind(whole.one, o11, o12, o21, o22)
# R1, R2, C1, C2, N 열 생성하기.
whole.one.ct.add <- transform(whole.one.ct, r1=o11+o12, r2=o21+o22, c1=o11+o21, c2=o12+o22, n=o11+o12+o21+o22)

# E11, E12, E21, E22 열 생성하기.
whole.one.ct.add1 <- transform(whole.one.ct.add, e11=(r1*c1)/n, e12=(r1*c2)/n, e21=(r2*c1)/n, e22=(r2*c2)/n)
# 주의: 모든 열의 특성을 numeric으로 바꾸기(큰 값의 integer를 가지고 계산할 경우 integer overflow 현상이 일어남).
as.numeric(whole.one.ct.add1[, 4])
as.numeric(whole.one.ct.add1[, 5])
as.numeric(whole.one.ct.add1[, 6])
as.numeric(whole.one.ct.add1[, 7])
as.numeric(whole.one.ct.add1[, 8])
as.numeric(whole.one.ct.add1[, 9])
as.numeric(whole.one.ct.add1[, 10])
as.numeric(whole.one.ct.add1[, 11])
as.numeric(whole.one.ct.add1[, 12])
as.numeric(whole.one.ct.add1[, 13])
as.numeric(whole.one.ct.add1[, 14])
as.numeric(whole.one.ct.add1[, 15])
as.numeric(whole.one.ct.add1[, 16])
as.numeric(whole.one.ct.add1[, 17])
as.numeric(whole.one.ct.add1[, 18])
as.numeric(whole.one.ct.add1[, 19])

## (2) AM별 측정치 항목 추가하기

### 1) 카이제곱 값 계산 및 측정치 열 생성하기

# R1*R2*C1*C2 값이 커서 integer overflow 메시지가 뜰 수 있으므로, R1*R2와 C1*C2를 나누어 계산할 것.
whole.one.ct.add.chi <- transform(whole.one.ct.add1, chisq = n * 
                                    (abs(o11*o22-o12*o21) - n/2)^2 / 
                                    (r1*r2)) # r1*r2를 먼저 나눠줌.
whole.one.ct.add.chisquare <- transform(whole.one.ct.add.chi, chisquare = chisq / (c1*c2)) # C1*C2를 나중에 나눠줌.
# R1*R2를 분모로 사용한 chisq 열을 지우고, 진짜 카이제곱 공식으로 구한 카이제곱 값 열만 남기기.
whole.one.ct.add.chisq <- whole.one.ct.add.chisquare[names(whole.one.ct.add.chisquare) !="chisq"]
# 분석의 편의를 위해 열 제목을 chisq로 바꾸기.
colnames(whole.one.ct.add.chisq)[20] <- "chisq"

# 본격적 분석에 앞서 E11, E12, E21, E22의 기대빈도를 직접 확인하기.
sum(whole.one.ct.add.chisq$e11 < 5) / 2340 # 0.9465812
sum(whole.one.ct.add.chisq$e12 < 5) / 2340 # 0.4235043
sum(whole.one.ct.add.chisq$e21 < 5) / 2340 # 0.06794872
sum(whole.one.ct.add.chisq$e22 < 5) / 2340 # 0

### 2) 로그 우도비 계산 및 측정치 열 생성하기

whole.one.ct.add.log <- transform(whole.one.ct.add.chisq,
                                  logl = 2 * (
                                    ifelse(o11>0, o11*log(o11/e11), 0) +
                                      ifelse(o12>0, o12*log(o12/e12), 0) +
                                      ifelse(o21>0, o21*log(o21/e21), 0) +
                                      ifelse(o22>0, o22*log(o22/e22), 0)
                                  )) # log 0은 계산불가임. 따라서 관찰빈도가 0인 경우, 로그 우도비 값을 0으로 처리.

### 3) t-값 계산 및 측정치 열 생성하기

whole.one.ct.add.ttest <- transform(whole.one.ct.add.log, ttest = (o11-e11) / sqrt(o11))

### 4) 상호정보 계산 및 측정치 열 생성하기

whole.one.ct.add.mi <- transform(whole.one.ct.add.ttest, MI = log2(o11/e11))

## (3) 전문가 판정 결과 항목 추가하기

# 전문가의 연어관계 판정 결과가 담긴 파일 R로 불러들이기.
whole.one.human <- read.csv(file="https://raw.github.com/cognitivepsychology/cognitive_psychology/master/whole.one.human.csv")
# 2340개의 TRUE 또는 FALSE로 구성된 "human"이라는 제목의 전문가 판정 결과 열을 기존 빈도자료 데이터프레임에 추가하기.
human <- whole.one.human$human.eval 
whole.one.am.human <- transform(whole.one.ct.add.mi, human = human)