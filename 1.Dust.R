# 지역별 미세먼지 차이가 존재할까?
# 서울특별시 대기환경 정보-대기질 통계-전체/미세먼지/22년 2월
dust_data <- read_excel('Part05_Practice/dust.xlsx')
View(dust_data)
str(dust_data)
  # tibble = data frame

# 마포구와 강남구 미세먼지 차이가 존재하는지 검정
dust_stat <- dust_data[,c('day','강남구','마포구')]
dust_stat

is.na(dust_stat)
sum(is.na(dust_stat))
# 결측치가 있는 경우 원하는 방법으로 결측치 처리

# 기술통계량 구하기
library(dplyr)
library(psych)

describe(dust_stat$강남구)
# describe : 관측값 개수(n), 평균(mean), 표준편차(sd), 중앙값(median), 
# 절삭평균(trimmed)(이상치를 뺀 평균), 중앙값 절대편차(mad), 최소값(min),
# 최대값(max), 범위(range), 왜도(skew), 첨도(kurtosis), 표준오차(SE, Standard Error)

boxplot(dust_stat$강남구, dust_stat$마포구,
        main='미세먼지 비교', xlab='지역', ylab='미세먼지', col=7:8)

# 통계량 -> 추론
# 귀무가설 : 기존에 알려진 사실을 기준으로 설정하는 가설
# ex) 서울의 미세먼지가 경기도보다 높다.
# 대립가설 : 새롭게 주장하려는 가설
# ex) 서울이 미세먼지가 더 낮을 것이다.

# t-검정 : 변수가 정규분포를 따를 때, 두 집단의 평균이 차이가 있는지 검정
# f-검정 : 두 집단의 분산에 차이가 있는 지 검정

# f검정으로 지역별 미세먼지 농도의 분산차이 검정하기
var.test(dust_stat$강남구, dust_stat$마포구)
  # p-value < 0.05 : 통계적으로 유의하다.
# 강남구, 마포구 간의 분산 차이가 없다고 할 수 있다.

# 등분산이 확인되었으므로(f검정을 통해) var.equal=T
t.test(dust_stat$강남구, dust_stat$마포구, var.equal=T)
# p-value가 0.05보다 크므로 통계적으로 무의미하다.
# 즉, 미세먼지 농도의 평균 차이가 없다.

# 세 집단 이상의 평균차이 검정(분산분석; anova(lm()))
library(reshape2)
data_anova <- dust_data[,c('강북구','마포구','동작구')]
anova_data <- melt(data_anova)

boxplot(formula=value ~ variable, data=anova_data)
anova(lm(value ~ variable, data=anova_data))

# iris 모든 feature들 간의 평균차이 검정해보기
data1 <- melt(iris[,1:4])
View(data)
boxplot(formula=value~variable, data=data1)
anova(lm(value~variable, data=data1))
# Pr 값이 굉장히 작다 -> 유의미한 차이가 있음