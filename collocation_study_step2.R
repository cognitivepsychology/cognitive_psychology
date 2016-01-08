# 1. 연어판별 정확도와 연어 포함률 소개

# 2. 연구자료 소개 

# 3. 연어판별 정확도 계산 및 표 작성하기

## (1) 모든 목+술 연쇄 대상으로 연어판별 정확도 계산 및 표 작성하기

### 1) 5개 AM별 상위 n대 연어판별 정확도 계산 및 표 작성하기 

library(devtools)
source_url('https://raw.github.com/cognitivepsychology/cognitive_psychology/master/collocation_study_step1.R')

# 소스 파일을 직접 내려받고 싶다면 다음을 실행할 것: download.file('https://raw.github.com/cognitivepsychology/cognitive_psychology/master/collocation_study_step1.R'

show.prec <- function(mydata, AM, n.best) {
  stopifnot(AM %in% colnames(mydata)) # 개별 AM을 열 제목으로 갖는 자료만 계산 대상이 됨!
  sort.idx <- order(mydata[[AM]], decreasing=TRUE)
  prec <- cumsum(mydata$human[sort.idx]) / (1:nrow(mydata))
  result <- data.frame(100 * prec[n.best]) # 정확도를 퍼센트로 나타내줌.
  result <- round(result, 2) # 정확도를 소수점 이하 둘째 자리까지만 보고함.
  rownames(result) <- n.best # 행 제목은 n.best로.
  colnames(result) <- AM # 열 제목은 개별 AM의 이름으로.
  result # 상위 n대의 목+술 연쇄에 대한 특정 AM의 연어판별 정확도를 보고해줌.
}

show.prec(whole.one.am.human, "chisq", c(50,100,200,500,1000,2000))
show.prec(whole.one.am.human, "o11", c(50,100,200,500,1000,2000))
show.prec(whole.one.am.human, "logl", c(50,100,200,500,1000,2000))
show.prec(whole.one.am.human, "ttest", c(50,100,200,500,1000,2000))
show.prec(whole.one.am.human, "MI", c(50,100,200,500,1000,2000))

### 2) 5개 AM별 상위 n퍼센트 연어판별 정확도 계산 및 표 작성하기

# AM별 상위 n % 연어판별 정확도 표 만들기.
show.prec.percent <- function(mydata, AM, n.percent) {
  stopifnot(AM %in% colnames(mydata)) # 개별 AM을 열 제목으로 갖는 자료만 계산 대상이 됨!
  sort.idx <- order(mydata[[AM]], decreasing=TRUE)
  prec <- cumsum(mydata$human[sort.idx]) / (1:nrow(mydata))
  result <- data.frame(100 * prec[nrow(mydata)*n.percent/100]) # 정확도를 퍼센트로 나타내줌. 
  result <- round(result, 2)
  rownames(result) <- n.percent # 행 제목은 n.percent로.
  colnames(result) <- AM # 열 제목은 개별 AM의 이름으로.
  result # 상위 n% 목+술 연쇄에 대한 특정 AM의 연어판별 정확도를 보고해줌.
}

show.prec.percent(whole.one.am.human, "chisq", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.am.human, "o11", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.am.human, "logl", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.am.human, "ttest", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.am.human, "MI", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 

### 3) 5개 AM 통합 상위 n대 연어판별 정확도 표 작성하기

# 상위 n대 목+술 연쇄에 대한 5개 AM의 연어판별 정확도를 하나의 표로 나타내기.

n.list <- c(50,100,200,500,1000,1500,2000,2340)
prec.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.prec(whole.one.am.human, AM, n.list)
)
prec.table <- do.call(cbind, prec.list)

### 4) 5개 AM 통합 상위 n퍼센트 연어판별 정확도 표 작성하기

# 상위 n퍼센트 목+술 연쇄에 대한 5개 AM의 연어판별 정확도를 하나의 표로 나타내기.

p.list <- c(1,5,10,20,30,40,50,60,70,80,90,100)
prec.percent.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.prec.percent(whole.one.am.human, AM, p.list)
)
prec.percent.table <- do.call(cbind, prec.percent.list)

### 5) 5개 AM 통합 상위 n퍼센트 연어판별 정확도 표에 평균 연어판별 정확도 추가하기

#### A. 5개 AM의 연어판별 정확도를 하나의 데이터프레임으로 정리하기

##### 가. 5개 AM 측정치를 내림차순으로 나열하기

idx.logl <- order(whole.one.am.human$logl, decreasing=TRUE)
idx.chisq <- order(whole.one.am.human$chisq, decreasing=TRUE)
idx.ttest <- order(whole.one.am.human$ttest, decreasing=TRUE)
idx.mi <- order(whole.one.am.human$MI, decreasing=TRUE)
idx.freq <- order(whole.one.am.human$o11, decreasing=TRUE)

##### 나. 5개 AM의 연어판별 정확도 계산하기

n.vals <- 1:nrow(whole.one.am.human)
prec.ll <- round(cumsum(whole.one.am.human$human[idx.logl]) * 100 / n.vals, 2)
prec.chisq <- round(cumsum(whole.one.am.human$human[idx.chisq]) * 100 / n.vals, 2)
prec.ttest <- round(cumsum(whole.one.am.human$human[idx.ttest]) * 100 / n.vals, 2)
prec.MI <- round(cumsum(whole.one.am.human$human[idx.mi]) * 100 / n.vals, 2)
prec.freq <- round(cumsum(whole.one.am.human$human[idx.freq]) * 100 / n.vals, 2)

##### 다. 계산된 5개 AM의 정확도 벡터를 하나의 데이터프레임으로 통합하기

whole.human.prec <- cbind(prec.ll, prec.chisq, prec.ttest, prec.MI, prec.freq)
whole.human.prec <- data.frame(whole.human.prec)

#### B. 5개 AM의 포함률을 정확도 데이터프레임에 추가하기

##### 가. 5개 AM 측정치를 내림차순으로 나열하기

idx.logl <- order(whole.one.am.human$logl, decreasing=TRUE)
idx.chisq <- order(whole.one.am.human$chisq, decreasing=TRUE)
idx.ttest <- order(whole.one.am.human$ttest, decreasing=TRUE)
idx.mi <- order(whole.one.am.human$MI, decreasing=TRUE)
idx.freq <- order(whole.one.am.human$o11, decreasing=TRUE)

##### 나. 5개 AM의 연어 포함률 계산하기

rec.ll <- round(cumsum(whole.one.am.human$human[idx.logl]) * 100 / sum(whole.one.am.human$human), 2)
rec.chisq <- round(cumsum(whole.one.am.human$human[idx.chisq]) * 100 / sum(whole.one.am.human$human), 2)
rec.ttest <- round(cumsum(whole.one.am.human$human[idx.ttest]) * 100 / sum(whole.one.am.human$human), 2)
rec.MI <- round(cumsum(whole.one.am.human$human[idx.mi]) * 100 / sum(whole.one.am.human$human), 2)
rec.freq <- round(cumsum(whole.one.am.human$human[idx.freq]) * 100 / sum(whole.one.am.human$human), 2)
whole.human.rec <- cbind(rec.ll, rec.chisq, rec.ttest, rec.MI, rec.freq)
whole.human.rec <- data.frame(whole.human.rec)
# 정확도와 포함률 자료 합치기.
whole.human.prec.rec <- cbind(whole.human.prec, whole.human.rec)
# 합쳐진 자료를 데이터프레임 형식으로 변환하기.
whole.human.prec.rec <- data.frame(whole.human.prec.rec)

##### 다. x축을 구성할 자료 열 만들기

n.best <- 1:nrow(whole.human.prec.rec)
whole.human.prec.rec.nbest <- transform(whole.human.prec.rec, n.best=n.best)

n.percent <- n.best * 100 / nrow(whole.human.prec.rec.nbest)
n.percent <- round(n.percent, 2)
whole.human.prec.rec.nbest <- transform(whole.human.prec.rec.nbest, n.percent=n.percent)

##### 라. 5개 AM별 평균 연어판별 정확도 산출하기

# 연어 포함률 0.5-0.95일 때에 해당하는 공기빈도의 평균 연어판별 정확도.
ave.prec.freq.5.95 <- mean(whole.human.prec.rec.nbest$prec.freq[whole.human.prec.rec.nbest$rec.freq >= 5 & whole.human.prec.rec.nbest$rec.freq <= 95]) 
# 연어 포함률 0.5-0.95일 때에 해당하는 로그 우도비의 평균 연어판별 정확도.
ave.prec.ll.5.95 <- mean(whole.human.prec.rec.nbest$prec.ll[whole.human.prec.rec.nbest$rec.ll >= 5 & whole.human.prec.rec.nbest$rec.ll <= 95]) 
# 연어 포함률 0.5-0.95일 때에 해당하는 카이제곱 검정의 평균 연어판별 정확도.
ave.prec.chisq.5.95 <- mean(whole.human.prec.rec.nbest$prec.chisq[whole.human.prec.rec.nbest$rec.chisq >= 5 & whole.human.prec.rec.nbest$rec.chisq <= 95]) 
# 연어 포함률 0.5-0.95일 때에 해당하는 t-검정 평균 연어판별 정확도.
ave.prec.ttest.5.95 <-  mean(whole.human.prec.rec.nbest$prec.ttest[whole.human.prec.rec.nbest$rec.ttest >= 5 & whole.human.prec.rec.nbest$rec.ttest <= 95])
# 연어 포함률 0.5-0.95일 때에 해당하는 상호정보의 평균 연어판별 정확도.
ave.prec.MI.5.95 <- mean(whole.human.prec.rec.nbest$prec.MI[whole.human.prec.rec.nbest$rec.MI >= 5 & whole.human.prec.rec.nbest$rec.MI <= 95])

##### 마. 5개 AM 통합 연어판별 정확도 표에 평균 연어판별 정확도 행 추가하기

# 5개 AM의 평균 연어판별 정확도 행을 위한 벡터 만들기.
ave.AM.vector <- c(ave.prec.freq.5.95, ave.prec.MI.5.95, ave.prec.chisq.5.95, ave.prec.ttest.5.95, ave.prec.ll.5.95)
# 소수점 두 자리까지만 표기됨.
ave.AM.vector <- round(ave.AM.vector, 2)
# 5개 AM의 평균 연어판별 정확도 행 벡터를 5개 AM 통합 연어판별 정확도 표 마지막 행으로 추가하기.
prec.percent.table <- rbind(prec.percent.table, ave.AM.vector)
prec.table <- rbind(prec.table, ave.AM.vector)
# 평균 연어판별 정확도 행 제목을 "평균"으로 변경하기.
rownames(prec.percent.table) <- c(p.list, "average") 
rownames(prec.table) <- c(n.list, "average") 

### 6) 5개 AM 통합 연어판별 정확도 표 문서로 출력하기

# 데이터프레임을 R 뷰어나 html 문서에 표의 형태로 출력해주는 패키지 sjPlot 불러오기.
library(sjPlot)
# 5개 AM의 상위 n대별 연어판별 정확도 표(문서용) 작성하기. 이때 "title =" 논항을 통해 표 제목도 R상에서 추가할 수 있으나, 한글 입력 시 깨짐 현상 발생. 표 제목은 문서상에서 추가할 것.
sjt.df(prec.table, describe = F) 
# 5개 AM의 상위 n%별 연어판별 정확도 표(문서용) 작성하기.
sjt.df(prec.percent.table, describe = F)

### 7) 상위 1600대 이상 목+술 연쇄 대상 후속분석

#### A. AM별 상위 1600대 이상 목+술 연쇄 추출하기

##### 가. 상호정보 상위 1600대 이상 목+술 연쇄 추출하기 

# 상호정보를 내림차순으로 나열하기.
idx.MI <- order(whole.one.am.human$MI, decreasing=TRUE) 
# 상호정보를 기준으로 데이터프레임 내림차순 정렬하기.
whole.one.am.human.MI <- whole.one.am.human[idx.MI, ] 
# 상호정보 상위 1600-2340대 연쇄 목록 추출하기.
inspect.mi <- whole.one.am.human.MI[1600:2340, ]
# 상호정보 상위 1600-2340대 연쇄 목록을 csv 형식으로 저장하기. csv 파일은 엑셀에서 열어볼 수 있음. 해당 목록을 엑셀로 불러와 살펴볼 경우, 훨씬 가독성을 높일 수 있음.
write.csv(inspect.mi, file="inspect.mi.csv") 
inspect.mi[, c(1:6, 24)]

##### 나. 카이제곱 검정 상위 1600대 이상 목+술 연쇄 추출하기 

# 카이제곱 값을 내림차순으로 나열하기.
idx.chisq <- order(whole.one.am.human$chisq, decreasing=TRUE) 
# 카이제곱 값을 기준으로 데이터프레임 내림차순 정렬하기.
whole.one.am.human.chisq <- whole.one.am.human[idx.chisq, ]
# 카이제곱 값 상위 1600-2340대 목+술 연쇄 목록 추출하기. 
inspect.chisq <- whole.one.am.human.chisq[1600:2340, ]
# 카이제곱 값 상위 1600-2340대 목+술 연쇄 목록 csv 형식으로 저장하기.
write.csv(inspect.chisq, file="inspect.chisq.csv") 
inspect.chisq[, c(1:6, 24)]

##### 다. *t*-검정 상위 1600대 이상 목+술 연쇄 추출하기 

# t-값을 내림차순으로 나열하기.
idx.ttest <- order(whole.one.am.human$ttest, decreasing=TRUE)
# t-값을 기준으로 데이터프레임 내림차순 정렬하기.
whole.one.am.human.ttest <- whole.one.am.human[idx.ttest, ]
# t-값 상위 1600-2340대 목+술 연쇄 목록 추출하기.
inspect.ttest <- whole.one.am.human.ttest[1600:2340, ]
# t-값 상위 1600-2340대 목+술 연쇄 목록 csv 형식으로 저장하기.
write.csv(inspect.ttest, file="inspect.ttest.csv")
inspect.ttest[, c(1:6, 24)]

##### 라. 로그 우도비 상위 1600대 이상 목+술 연쇄 추출하기 

# 로그 우도비를 내림차순으로 나열하기.
idx.logl <- order(whole.one.am.human$logl, decreasing=TRUE)
# 로그 우도비를 기준으로 데이터프레임 내림차순 정렬하기.
whole.one.am.human.logl <- whole.one.am.human[idx.logl, ] 
# 로그 우도비 상위 1600-2340대 목+술 연쇄 목록 추출하기.
inspect.logl <- whole.one.am.human.logl[1600:2340, ]
# 로그 우도비 상위 1600-2340대 목+술 연쇄 목록 csv 형식으로 저장하기.
write.csv(inspect.logl, file="inspect.logl.csv")
inspect.logl[, c(1:6, 24)]

##### 마. 공기빈도 상위 1600대 이상 목+술 연쇄 추출하기 

# 공기빈도를 내림차순으로 나열하기.
idx.o11 <- order(whole.one.am.human$o11, decreasing=TRUE) 
# 공기빈도를 기준으로 데이터프레임 내림차순 정렬하기.
whole.one.am.human.o11 <- whole.one.am.human[idx.o11, ] 
# 공기빈도 상위 1600-2340대 목+술 연쇄 목록 추출하기.
inspect.o11 <- whole.one.am.human.o11[1600:2340, ]
# 공기빈도 상위 1600-2340대 목+술 연쇄 목록 csv 형식으로 저장하기.
write.csv(inspect.o11, file="inspect.o11.csv") 
inspect.o11[, c(1:6, 24)]

#### B. 상위 1600대 이상 목+술 연쇄 목록에 대한 AM별 평균 연어판별 정확도 계산하기

sum(inspect.mi$human)/nrow(inspect.mi) # 정확도: 0.7516869.
sum(inspect.chisq$human)/nrow(inspect.chisq) # 정확도: 0.6356275.
sum(inspect.ttest$human)/nrow(inspect.ttest) # 정확도: 0.5641026.
sum(inspect.logl$human)/nrow(inspect.logl) # 정확도: 0.5492578.
sum(inspect.o11$human)/nrow(inspect.o11) # 정확도: 0.294197.

length(grep("NNG을JKO[ \t\n]하VV", inspect.mi$bigram, value=T)) / nrow(inspect.mi) # 611/741 =  0.8245614
length(grep("NNG을JKO[ \t\n]하VV", inspect.chisq$bigram, value=T)) / nrow(inspect.chisq) # 504/741 = 0.6801619
length(grep("NNG을JKO[ \t\n]하VV", inspect.ttest$bigram, value=T)) / nrow(inspect.ttest) # 450/741 = 0.6072874
length(grep("NNG을JKO[ \t\n]하VV", inspect.logl$bigram, value=T)) / nrow(inspect.logl) # 424/741 = 0.5721997
length(grep("NNG을JKO[ \t\n]하VV", inspect.o11$bigram, value=T)) / nrow(inspect.o11) # 137/741 = 0.1848853

## (2) 비(非)하다류 연쇄 대상으로 연어판별 정확도 계산 및 표 작성하기

### 1) 전체 목+술 연쇄 목록에서 하다류 연쇄 제외하기

whole.one.no.ha <- whole.one.am.human[-grep("NNG을JKO[ \t\n]하VV", whole.one.am.human$bigram), ]
nrow(whole.one.no.ha) # 비(非)하다류 연쇄의 수(N = 1729) 확인하기.

### 2) 5개 AM별 상위 n대 연어판별 정확도 계산 및 표 작성하기 

show.prec(whole.one.no.ha, "chisq", c(50,100,200,500,1000,1500,1729))
show.prec(whole.one.no.ha, "o11", c(50,100,200,500,1000,1500,1729))
show.prec(whole.one.no.ha, "logl", c(50,100,200,500,1000,1500,1729))
show.prec(whole.one.no.ha, "ttest", c(50,100,200,500,1000,1500,1729))
show.prec(whole.one.no.ha, "MI", c(50,100,200,500,1000,1500,1729))

### 3) 5개 AM별 상위 n퍼센트 연어판별 정확도 계산 및 표 작성하기 

show.prec.percent(whole.one.no.ha, "chisq", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.no.ha, "o11", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.no.ha, "logl", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.no.ha, "ttest", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) 
show.prec.percent(whole.one.no.ha, "MI", c(1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

### 4) 5개 AM 통합 상위 n대 연어판별 정확도 표 작성하기

n.no.ha.list <- c(50,100,200,500,1000,1500,1729)
prec.no.ha.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.prec(whole.one.no.ha, AM, n.no.ha.list)
)
prec.no.ha.table <- do.call(cbind, prec.no.ha.list)

### 5) 5개 AM 통합 상위 n퍼센트 연어판별 정확도 표 작성하기

p.list <- c(1,5,10,20,30,40,50,60,70,80,90,100)
prec.percent.no.ha.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.prec.percent(whole.one.no.ha, AM, p.list)
)
prec.percent.no.ha.table <- do.call(cbind, prec.percent.no.ha.list)

### 6) 5개 AM 통합 상위 n퍼센트 연어판별 정확도 표에 평균 연어판별 정확도 추가하기

#### A. 5개 AM의 연어판별 정확도를 하나의 데이터프레임으로 정리하기

##### 가. 5개 AM 측정치를 내림차순으로 나열하기

idx.logl.no.ha <- order(whole.one.no.ha$logl, decreasing=TRUE)
idx.chisq.no.ha <- order(whole.one.no.ha$chisq, decreasing=TRUE)
idx.ttest.no.ha <- order(whole.one.no.ha$ttest, decreasing=TRUE)
idx.mi.no.ha <- order(whole.one.no.ha$MI, decreasing=TRUE)
idx.freq.no.ha <- order(whole.one.no.ha$o11, decreasing=TRUE)

##### 나. 5개 AM의 연어판별 정확도 계산하기

n.vals.no.ha <- 1:nrow(whole.one.no.ha)
prec.ll.no.ha <- round(cumsum(whole.one.no.ha$human[idx.logl.no.ha]) * 100 / n.vals.no.ha, 2)
prec.chisq.no.ha <- round(cumsum(whole.one.no.ha$human[idx.chisq.no.ha]) * 100 / n.vals.no.ha, 2)
prec.ttest.no.ha <- round(cumsum(whole.one.no.ha$human[idx.ttest.no.ha]) * 100 / n.vals.no.ha, 2)
prec.MI.no.ha <- round(cumsum(whole.one.no.ha$human[idx.mi.no.ha]) * 100 / n.vals.no.ha, 2)
prec.freq.no.ha <- round(cumsum(whole.one.no.ha$human[idx.freq.no.ha]) * 100 / n.vals.no.ha, 2)

##### 다. 계산된 5개 AM의 정확도 벡터를 하나의 데이터프레임으로 통합하기

whole.human.prec.no.ha <- cbind(prec.ll.no.ha, prec.chisq.no.ha, prec.ttest.no.ha, prec.MI.no.ha, prec.freq.no.ha)
whole.human.prec.no.ha <- data.frame(whole.human.prec.no.ha)

#### B. 5개 AM의 포함률을 정확도 데이터프레임에 추가하기

##### 가. 5개 AM 측정치를 내림차순으로 나열하기

idx.logl.no.ha <- order(whole.one.no.ha$logl, decreasing=TRUE)
idx.chisq.no.ha <- order(whole.one.no.ha$chisq, decreasing=TRUE)
idx.ttest.no.ha <- order(whole.one.no.ha$ttest, decreasing=TRUE)
idx.mi.no.ha <- order(whole.one.no.ha$MI, decreasing=TRUE)
idx.freq.no.ha <- order(whole.one.no.ha$o11, decreasing=TRUE)

##### 나. 5개 AM의 연어 포함률 계산하기

rec.ll.no.ha <- round(cumsum(whole.one.no.ha$human[idx.logl.no.ha]) * 100 / sum(whole.one.no.ha$human), 2)
rec.chisq.no.ha <- round(cumsum(whole.one.no.ha$human[idx.chisq.no.ha]) * 100 / sum(whole.one.no.ha$human), 2)
rec.ttest.no.ha <- round(cumsum(whole.one.no.ha$human[idx.ttest.no.ha]) * 100 / sum(whole.one.no.ha$human), 2)
rec.MI.no.ha <- round(cumsum(whole.one.no.ha$human[idx.mi.no.ha]) * 100 / sum(whole.one.no.ha$human), 2)
rec.freq.no.ha <- round(cumsum(whole.one.no.ha$human[idx.freq.no.ha]) * 100 / sum(whole.one.no.ha$human), 2)
whole.human.rec.no.ha <- cbind(rec.ll.no.ha, rec.chisq.no.ha, rec.ttest.no.ha, rec.MI.no.ha, rec.freq.no.ha)
whole.human.rec.no.ha <- data.frame(whole.human.rec.no.ha)
whole.human.prec.rec.no.ha <- cbind(whole.human.prec.no.ha, whole.human.rec.no.ha) # 정확도, 포함률 자료 합치기.
whole.human.prec.rec.no.ha <- data.frame(whole.human.prec.rec.no.ha) # 합쳐진 자료를 데이터 프레임 형식으로 변환하기.

##### 다. x축을 구성할 자료 열 만들기

n.best.no.ha <- 1:nrow(whole.human.prec.rec.no.ha)
whole.human.prec.rec.nbest.no.ha <- transform(whole.human.prec.rec.no.ha, n.best.no.ha=n.best.no.ha)
n.percent.no.ha <- n.best.no.ha * 100 / nrow(whole.human.prec.rec.nbest.no.ha)
n.percent.no.ha <- round(n.percent.no.ha, 2)
whole.human.prec.rec.nbest.no.ha <- transform(whole.human.prec.rec.nbest.no.ha, n.percent.no.ha=n.percent.no.ha)

##### 라. 5개 AM별 평균 연어판별 정확도 산출하기

# 연어 포함률 0.5-0.95일 때에 해당하는 공기빈도의 평균 연어판별 정확도.
ave.prec.freq.no.ha <- mean(whole.human.prec.rec.nbest.no.ha$prec.freq[whole.human.prec.rec.nbest.no.ha$rec.freq >= 5 & whole.human.prec.rec.nbest.no.ha$rec.freq <= 95]) 
# 연어 포함률 0.5-0.95일 때에 해당하는 로그 우도비의 평균 연어판별 정확도.
ave.prec.ll.no.ha <- mean(whole.human.prec.rec.nbest.no.ha$prec.ll[whole.human.prec.rec.nbest.no.ha$rec.ll >= 5 & whole.human.prec.rec.nbest.no.ha$rec.ll <= 95]) 
# 연어 포함률 0.5-0.95일 때에 해당하는 카이제곱 검정의 평균 연어판별 정확도.
ave.prec.chisq.no.ha <- mean(whole.human.prec.rec.nbest.no.ha$prec.chisq[whole.human.prec.rec.nbest.no.ha$rec.chisq >= 5 & whole.human.prec.rec.nbest.no.ha$rec.chisq <= 95]) 
# 연어 포함률 0.5-0.95일 때에 해당하는 t-검정 평균 연어판별 정확도.
ave.prec.ttest.no.ha <-  mean(whole.human.prec.rec.nbest.no.ha$prec.ttest[whole.human.prec.rec.nbest.no.ha$rec.ttest >= 5 & whole.human.prec.rec.nbest.no.ha$rec.ttest <= 95]) 
# 연어 포함률 0.5-0.95일 때에 해당하는 상호정보의 평균 연어판별 정확도.
ave.prec.MI.no.ha <- mean(whole.human.prec.rec.nbest.no.ha$prec.MI[whole.human.prec.rec.nbest.no.ha$rec.MI >= 5 & whole.human.prec.rec.nbest.no.ha$rec.MI <= 95])

##### 마. 5개 AM 통합 연어판별 정확도 표에 평균 연어판별 정확도 행 추가하기

# 5개 AM의 평균 연어판별 정확도 행을 위한 벡터 만들기.
# 5개 AM의 평균 연어판별 정확도 행을 위한 벡터 만들기.
ave.AM.no.ha.vector <- c(ave.prec.freq.no.ha, ave.prec.MI.no.ha, ave.prec.chisq.no.ha, ave.prec.ttest.no.ha, ave.prec.ll.no.ha) 
# 소수점 두 자리까지만 표기됨.
ave.AM.no.ha.vector <- round(ave.AM.no.ha.vector, 2)
# 5개 AM의 평균 연어판별 정확도 행 벡터를 5개 AM 통합 연어판별 정확도 표 마지막 행으로 추가하기.
prec.percent.no.ha.table <- rbind(prec.percent.no.ha.table, ave.AM.no.ha.vector) 
prec.no.ha.table <- rbind(prec.no.ha.table, ave.AM.no.ha.vector) 
# 평균 연어판별 정확도 행 제목을 "평균"으로 변경하기.
rownames(prec.percent.no.ha.table) <- c(p.list, "average") 
rownames(prec.no.ha.table) <- c(n.no.ha.list, "average") 

### 7) 5개 AM 통합 연어판별 정확도 표 출력하기

# 비(非)하다류 연쇄에 대한 상위 n대별 5개 AM 연어판별 정확도 표(문서용) 출력하기.
sjt.df(prec.no.ha.table, describe = F) 
# 비(非)하다류 연쇄에 대한 상위 n%별 5개 AM 연어판별 정확도 표(문서용) 출력하기.
sjt.df(prec.percent.no.ha.table, describe = F) 

# 4. 연어 포함률 계산 및 표 작성하기

## (1) 모든 목+술 연쇄 대상으로 연어 포함률 계산 및 표 작성하기

### 1) 5개 AM별 상위 n대 연어 포함률 계산 및 표 작성하기 

show.recall <- function(mydata, AM, n.best) {
  stopifnot(AM %in% colnames(mydata)) # 개별 AM을 열 제목으로 갖는 자료만 계산 대상이 됨!
  sort.idx <- order(mydata[[AM]], decreasing=TRUE)
  recall <- cumsum(mydata$human[sort.idx]) / sum(mydata$human)
  result <- data.frame(100 * recall[n.best]) # 포함률을 퍼센트로 나타내줌.
  result <- round(result, 2) # 포함률을 소수점 이하 둘째 자리까지만 보고함.
  rownames(result) <- n.best # 행 이름은 n.best로.
  colnames(result) <- AM # 열 이름은 개별 AM의 이름으로.
  result # 상위 n대의 목+술 연쇄에 대한 특정 AM의 연어 포함률을 결과로 보고해줌.
}

### 2) 5개 AM별 상위 n퍼센트 연어 포함률 계산 및 표 작성하기

show.recall(whole.one.am.human, "chisq", c(50,100,200,500,1000,1500,2000,2340))
show.recall.percent <- function(mydata, AM, n.percent) {
  stopifnot(AM %in% colnames(mydata)) # 개별 AM을 열 제목으로 갖는 자료만 계산 대상이 됨!
  sort.idx <- order(mydata[[AM]], decreasing=TRUE)
  recall <- cumsum(mydata$human[sort.idx]) / sum(mydata$human)
  result <- data.frame(100 * recall[nrow(mydata)*n.percent/100]) # 포함률을 퍼센트로 나타내줌. 
  result <- round(result, 2)
  rownames(result) <- n.percent # 행 이름은 n.percent로.
  colnames(result) <- AM # 열 이름은 개별 AM으로.
  result # n개의 목+술 연쇄에 대한 특정 AM의 연어 포함률을 결과로 보고해줌.
}

### 3) 5개 AM 통합 상위 n대 연어 포함률 표 작성하기

n.list <- c(50,100,200,500,1000,1500,2000,2340)
recall.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.recall(whole.one.am.human, AM, n.list)
)
recall.table <- do.call(cbind, recall.list)

### 4) 5개 AM 통합 상위 n퍼센트 연어 포함률 표 작성하기

p.list <- c(1,5,10,20,30,40,50,60,70,80,90,100)
recall.percent.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.recall.percent(whole.one.am.human, AM, p.list)
)
recall.percent.table <- do.call(cbind, recall.percent.list)

### 6) 5개 AM 통합 연어 포함률 표 출력하기

# 상위 n대별 목+술 연쇄에 대한 5개 측정방법의 연어 포함률 표(문서용) 출력하기.
sjt.df(recall.table, describe = F) 
# 상위 n%별 목+술 연쇄에 대한 5개 측정방법의 연어 포함률 표(문서용) 출력하기.
sjt.df(recall.percent.table, describe = F) 

## (2) 비(非)하다류 연쇄 대상으로 연어 포함률 계산 및 표 작성하기

### 1) 5개 AM 통합 상위 n대 연어 포함률 표 작성하기 

n.no.ha.list <- c(50,100,200,500,1000,1500,1729)
recall.no.ha.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.recall(whole.one.no.ha, AM, n.no.ha.list)
)
recall.no.ha.table <- do.call(cbind, recall.no.ha.list)

### 2) 5개 AM 통합 상위 n퍼센트 연어 포함률 표 작성하기 

p.list <- c(1,5,10,20,30,40,50,60,70,80,90,100)
recall.percent.no.ha.list <- lapply(
  c("o11", "MI", "chisq", "ttest", "logl"),
  function (AM) show.recall.percent(whole.one.no.ha, AM, p.list)
)
recall.percent.no.ha.table <- do.call(cbind, recall.percent.no.ha.list)

### 3) 5개 AM 통합 연어 포함률 표 출력하기

# 비(非)하다류 연쇄에 대한 상위 n대별 5개 AM 연어판별 정확도 표(문서용) 출력하기.
sjt.df(recall.no.ha.table, describe = F)
# 비(非)하다류 연쇄에 대한 상위 n%별 5개 AM의 연어판별 정확도 표(문서용) 출력하기.
sjt.df(recall.percent.no.ha.table, describe = F)
