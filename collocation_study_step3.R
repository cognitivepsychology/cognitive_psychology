# 1. 모든 목+술 연쇄 자료 바탕으로 연어판별 정확도와 연어 포함률 그림 그리기

library(devtools)
source_url('https://raw.github.com/cognitivepsychology/cognitive_psychology/master/collocation_study_step2.R')
# 소스 파일을 직접 내려받고 싶다면 다음을 실행할 것: download.file('https://raw.github.com/cognitivepsychology/cognitive_psychology/master/collocation_study_step2.R')

# 자료의 형태를 바꿔주는 패키지 reshape2 불러오기.
library(reshape2)

## (1) 모든 목+술 연쇄 자료 바탕으로 연어판별 정확도 그림 그리기

### 1) 5개 AM의 상위 n대별 연어판별 정확도와 연어 포함률을 각각 하나의 열로 통합하기

whole.human.pr.melt.prec <- melt(whole.human.prec.rec.nbest, measure.vars=c("prec.freq", "prec.MI", "prec.chisq", "prec.ttest", "prec.ll"))
# 5개 AM 이름이 통합된 "variable" 열 제목을 "prec.AM"으로 바꾸기.
colnames(whole.human.pr.melt.prec)[8] <- c("prec.AM") 
# 5개 AM의 정확도가 통합된 "value" 열 제목을 "prec"으로 바꾸기.
colnames(whole.human.pr.melt.prec)[9] <- c("prec") 
whole.human.pr.melt.rec <- melt(whole.human.prec.rec.nbest, measure.vars=c("rec.freq", "rec.MI", "rec.chisq", "rec.ttest", "rec.ll"))
# 5개 AM의 이름이 통합된 "variable" 열 제목을 "rec.AM"으로 바꾸기.
colnames(whole.human.pr.melt.rec)[8] <- c("rec.AM") 
# 5개 AM의 정확도가 통합된 "value" 열 제목을 "rec"으로 바꾸기.
colnames(whole.human.pr.melt.rec)[9] <- c("rec") 
whole.human.pr.melt <- data.frame(cbind(whole.human.pr.melt.prec[, 6:9], whole.human.pr.melt.rec[, 8:9]))

### 2) 한글 서체 지정 및 한글 깨짐 방지를 위한 R 환경 설정하기

# ggplot2 패키지 불러오기.
library(ggplot2) 
# 나눔고딕 폰트를 R 폰트 시스템에 연동시키기. 
windowsFonts(NanumGothic = windowsFont("나눔 고딕")) 
# R에서 한글 입력 시 깨지는 현상 방지하기.
Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8") 

### 3) 상위 n퍼센트 연어판별 정확도 그림을 위한 ggplot2 코드 작성하기

# 모든 목+술 연쇄 중 진짜 연어의 비율 계산하기. 결과: 0.4034188.
TPall <- sum(whole.one.am.human$human) * 100 / nrow(whole.one.am.human) 
p.prec.per <- ggplot(data=whole.human.pr.melt, aes(x=n.percent, y=prec, linetype=prec.AM, color=prec.AM)) + geom_line(size=0.5)
p.prec.per <- p.prec.per + scale_colour_brewer(palette="Set1", name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.prec.per <- p.prec.per + scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash"), name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.prec.per <- p.prec.per + scale_x_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
p.prec.per <- p.prec.per + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
# 상위 10% 목+술 연쇄 목록 지점 표시.
p.prec.per <- p.prec.per + geom_hline(aes(yintercept=TPall), size=0.5) + geom_vline(aes(xintercept=10), linetype="dashed", size=0.5)
p.prec.per <- p.prec.per + labs(x="상위 n% 목+술 연쇄 목록", y="연어판별 정확도(%)")
p.prec.per <- p.prec.per + theme(title=element_text(family = "NanumGothic", color="black"),
                                 axis.title=element_text(size=13),
                                 legend.title=element_text(size=10), 
                                 legend.text=element_text(size=10),
                                 legend.position = c(.83, .8),
                                 axis.text=element_text(size=10),
                                 legend.background=element_rect(colour = "black"),
                                 legend.key=element_rect(colour="black"),
                                 legend.direction="vertical",
                                 legend.position="right")

### 4) 상위 n퍼센트 연어판별 정확도 그림 저장하기

# png 파일로 저장하기.
png(width = 1100, height = 1100, filename = "p.prec.per.png", type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
p.prec.per 
dev.off()
# pdf 파일로 저장하기.
cairo_pdf(filename = "p.prec.per.pdf", family="NanumGothic", width=7, height=7, antialias = "subpixel")
p.prec.per  
dev.off()

## (2) 모든 목+술 연쇄 자료 바탕으로 연어 포함률 그림 그리기

### 1) 상위 n퍼센트 연어 포함률 그림을 위한 ggplot2 코드 작성하기

p.rec.per <- ggplot(data=whole.human.pr.melt, aes(x=n.percent, y=rec, linetype=rec.AM, color=rec.AM)) + geom_line(size=0.5)
p.rec.per <- p.rec.per + scale_colour_brewer(palette="Set1", name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.rec.per <- p.rec.per + scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash"), name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.rec.per <- p.rec.per + scale_x_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
p.rec.per <- p.rec.per + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
p.rec.per <- p.rec.per + labs(x="상위 n% 목+술 연쇄 목록", y="연어 포함률(%)")
p.rec.per <- p.rec.per + theme(title=element_text(family = "NanumGothic", color="black"),
                               axis.title=element_text(size=13),
                               legend.title=element_text(size=10), 
                               legend.text=element_text(size=10),
                               legend.position = c(.17, .8),
                               axis.text=element_text(size=10),
                               legend.background=element_rect(colour = "black"),
                               legend.key=element_rect(colour="black"),
                               legend.direction="vertical",
                               legend.position="right")


### 2) 상위 n퍼센트 연어 포함률 그림 저장하기

# png 파일로 저장하기.
png(width = 1100, height = 1100, filename = "p.rec.per.png", type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
p.rec.per  
dev.off()
# pdf 파일로 저장하기.
cairo_pdf(filename = "p.rec.per.pdf", family="NanumGothic", width=7, height=7, antialias = "subpixel")
p.rec.per  
dev.off()

# 2. 비(非)하다류 연쇄 자료 바탕으로 연어판별 정확도와 연어 포함률 그림 그리기

## (1) 모든 비(非)하다류 연쇄 자료 바탕으로 연어판별 정확도 그림 그리기

### 1) 5개 AM의 상위 n대별 연어판별 정확도와 연어 포함률을 각각 하나의 열로 통합하기

whole.human.pr.melt.prec.no.ha <- melt(whole.human.prec.rec.nbest.no.ha, measure.vars=c("prec.freq.no.ha", "prec.MI.no.ha", "prec.chisq.no.ha", "prec.ttest.no.ha", "prec.ll.no.ha"))
# 5개 AM 이름이 통합된 "variable" 열 제목을 "prec.AM"으로 바꾸기.
colnames(whole.human.pr.melt.prec.no.ha)[8] <- c("prec.AM")
# 5개 AM의 정확도가 통합된 "value" 열 제목을 "prec"으로 바꾸기.
colnames(whole.human.pr.melt.prec.no.ha)[9] <- c("prec")
whole.human.pr.melt.rec.no.ha <- melt(whole.human.prec.rec.nbest.no.ha, measure.vars=c("rec.freq.no.ha", "rec.MI.no.ha", "rec.chisq.no.ha", "rec.ttest.no.ha", "rec.ll.no.ha"))
# 5개 AM의 이름이 통합된 "variable" 열 제목을 "rec.AM"으로 바꾸기.
colnames(whole.human.pr.melt.rec.no.ha)[8] <- c("rec.AM")
# 5개 AM의 정확도가 통합된 "value" 열 제목을 "rec"으로 바꾸기.
colnames(whole.human.pr.melt.rec.no.ha)[9] <- c("rec")
whole.human.pr.melt.no.ha <- data.frame(cbind(whole.human.pr.melt.prec.no.ha[, 6:9], whole.human.pr.melt.rec.no.ha[, 8:9]))

### 2) 상위 n퍼센트 연어판별 정확도 그림을 위한 ggplot2 코드 작성하기

# 비(非)하다류 연쇄 중 진짜 연어의 비율 계산하기. 결과: 0.2307692.
TP.no.ha <- sum(whole.one.no.ha$human) * 100 / nrow(whole.one.no.ha) 
p.prec.nh.per <- ggplot(data=whole.human.pr.melt.no.ha, aes(x=n.percent.no.ha, y=prec, linetype=prec.AM, color=prec.AM)) + geom_line(size=0.5)
p.prec.nh.per <- p.prec.nh.per + scale_colour_brewer(palette="Set1", name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.prec.nh.per <- p.prec.nh.per + scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash"), name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.prec.nh.per <- p.prec.nh.per + scale_x_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
p.prec.nh.per <- p.prec.nh.per + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
# 상위 10% 목+술 연쇄 목록 지점 표시하기.
p.prec.nh.per <- p.prec.nh.per + geom_hline(aes(yintercept=TP.no.ha), size=0.5) + geom_vline(aes(xintercept=10), linetype="dashed", size=0.5)
p.prec.nh.per <- p.prec.nh.per + labs(x="상위 n% 목+술 연쇄 목록", y="연어판별 정확도(%)")
p.prec.nh.per <- p.prec.nh.per + theme(title=element_text(family = "NanumGothic", color="black"),
                                       axis.title=element_text(size=13),
                                       legend.title=element_text(size=10), 
                                       legend.text=element_text(size=10),
                                       legend.position = c(.83, .8),
                                       axis.text=element_text(size=10),
                                       legend.background=element_rect(colour = "black"),
                                       legend.key=element_rect(colour="black"),
                                       legend.direction="vertical",
                                       legend.position="right")


### 3) 상위 n퍼센트 연어판별 정확도 그림 저장하기

# png 파일로 저장하기.
png(width = 1100, height = 1100, filename = "p.prec.nh.per.png", type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
p.prec.nh.per
dev.off()
# pdf 파일로 저장하기.
cairo_pdf(filename = "p.prec.nh.per.pdf", family="NanumGothic", width=7, height=7, antialias = "subpixel")
p.prec.nh.per
dev.off()

## (2) 상위 500대 비(非)하다류 연쇄 자료 바탕으로 연어판별 정확도 그림 그리기

### 1) 상위 n대 연어판별 정확도 그림을 위한 ggplot2 코드 작성하기

# 비(非)하다류 연쇄 중 진짜 연어의 비율 계산하기. 결과: 0.2307692.
TP.no.ha <- sum(whole.one.no.ha$human) * 100 / nrow(whole.one.no.ha) 
p.prec.nh.500 <- ggplot(data=whole.human.pr.melt.no.ha, aes(x=n.best.no.ha, y=prec, linetype=prec.AM, color=prec.AM)) + geom_line(size=0.5)
p.prec.nh.500 <- p.prec.nh.500 + scale_colour_brewer(palette="Set1", name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.prec.nh.500 <- p.prec.nh.500 + scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash"), name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.prec.nh.500 <- p.prec.nh.500 + scale_x_continuous(limits=c(0,500), breaks=c(0,50,100,150,200,250,300,350,400,450,500)) 
p.prec.nh.500 <- p.prec.nh.500 + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
# 상위 10% 목+술 연쇄 목록 지점 표시하기. 
ten.point <- nrow(whole.one.no.ha) / 10
p.prec.nh.500 <- p.prec.nh.500 + geom_hline(aes(yintercept=TP.no.ha), size=0.5) + geom_vline(aes(xintercept=ten.point), linetype="dashed", size=0.5)
p.prec.nh.500 <- p.prec.nh.500 + labs(x="상위 n개 목+술 연쇄 목록", y="연어판별 정확도(%)")
p.prec.nh.500 <- p.prec.nh.500 + theme(title=element_text(family = "NanumGothic", color="black"),
                                       axis.title=element_text(size=13),
                                       legend.title=element_text(size=10), 
                                       legend.text=element_text(size=10),
                                       legend.position = c(.83, .8),
                                       axis.text=element_text(size=10),
                                       legend.background=element_rect(colour = "black"),
                                       legend.key=element_rect(colour="black"),
                                       legend.direction="vertical",
                                       legend.position="right")


### 2) 상위 n퍼센트 연어판별 정확도 그림 저장하기

# png 파일로 저장하기.
png(width = 1100, height = 1100, filename = "p.prec.nh.500.png", type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
p.prec.nh.500
dev.off()
# pdf 파일로 저장하기.
cairo_pdf(filename = "p.prec.nh.500.pdf", family="NanumGothic", width=7, height=7, antialias = "subpixel")
p.prec.nh.500
dev.off()

## (3) 모든 비(非)하다류 연쇄 자료 바탕으로 연어 포함률 그림 그리기

### 1) 상위 n퍼센트 연어 포함률 그림을 위한 ggplot2 코드 작성하기


p.rec.nh.per <- ggplot(data=whole.human.pr.melt.no.ha, aes(x=n.percent.no.ha, y=rec, linetype=rec.AM, color=rec.AM)) + geom_line(size=0.5)
p.rec.nh.per <- p.rec.nh.per + scale_colour_brewer(palette="Set1", name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.rec.nh.per <- p.rec.nh.per + scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash"), name="측정방법",labels=c("공기빈도", "상호정보", "카이제곱 검정", "t-검정", "로그 우도비"))
p.rec.nh.per <- p.rec.nh.per + scale_x_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
p.rec.nh.per <- p.rec.nh.per + scale_y_continuous(limits=c(0,100), breaks=c(0,10,20,30,40,50,60,70,80,90,100)) 
p.rec.nh.per <- p.rec.nh.per + labs(x="상위 n% 목+술 연쇄 목록", y="연어 포함률(%)")
p.rec.nh.per <- p.rec.nh.per + theme(title=element_text(family = "NanumGothic", color="black"),
                                     axis.title=element_text(size=13),
                                     legend.title=element_text(size=10), 
                                     legend.text=element_text(size=10),
                                     legend.position = c(.17, .8),
                                     axis.text=element_text(size=10),
                                     legend.background=element_rect(colour = "black"),
                                     legend.key=element_rect(colour="black"),
                                     legend.direction="vertical",
                                     legend.position="right")


### 2) 상위 n퍼센트 연어 포함률 그림 저장하기

# png 파일로 저장하기.
png(width = 1100, height = 1100, filename = "p.rec.nh.per.png", type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
p.rec.nh.per  
dev.off()
# pdf 파일로 저장하기.
cairo_pdf(filename = "p.rec.nh.per.pdf", family="NanumGothic", width=7, height=7, antialias = "subpixel")
p.rec.nh.per
dev.off()
