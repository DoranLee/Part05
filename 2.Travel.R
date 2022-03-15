# 입국 추세 파악하기
# 관광지식정보시스템-관광통계-입국관광통계-엑셀전처리
library(readxl)
travel_data <- read_excel("Part05_Practice/nation_immigration.xls")
View(travel_data)

str(travel_data)

# 컬럼명 정리하기
colnames(travel_data)<-c('country','21.01','21.02','21.03',
                         '21.04','21.05','21.06','21.07',
                         '21.08','21.09','21.10','21.11',
                         '21.12')
# for문으로 컬럼명 넣기
a<-c('country')
for (i in 1:12){
  temp <- paste(21, toString(i), sep='.')
  a<-append(a, temp)
}
# colnames(travel_data) <- a

# 나라 이름에 공백이 있는 경우도 있다
# gsub() : 문자열 대체 함수
# gsub('찾을 문자열','대체할 문자열','데이터')
travel_data$country <- gsub(' ','',travel_data$country)

# 21년 1월 기준 상위 5개 나라 뽑아내기
rank_data <- travel_data[order(-travel_data$`21.01`),] %>% head(n=5)
  # - : decreasing=T(->내림차순)

# melt를 사용하기 위해 reshape2 불러오기
library(reshape2)
rank_data_melt <- melt(rank_data, id.vars='country',
                       variable.name='time')

# 시각화
library(ggplot2)
# 국가별 추이를 비교해야하므로 group을 country로 지정
ggplot(rank_data_melt, aes(x=time,y=value, group=country))+
  geom_line(aes(color=country))

ggplot(rank_data_melt, aes(x=time, y=value, group=country))+
  geom_line(aes(color=country))+
  ggtitle('2021년도 국가별 입국자수')+
  scale_y_continuous(breaks=seq(0,25000,1000))
# scale_y_continuous : y축 범위 조정

# 막대그래프 색상 다르게 표시하려면, color or fill 사용
ggplot(rank_data_melt, aes(x=time, y=value, fill=country))+
  geom_bar(stat='identity')
# y=value 넣으면 stat='identity'를 넣어주어야함

# 국가별로 막대그래프 그리려면 position = 'dodge'
ggplot(rank_data_melt, aes(x=time, y=value, fill=country))+
  geom_bar(stat='identity', position='dodge')

# stat
# 디폴트 값은 count -> x축값 매핑
# identity -> y축값 매핑

# position : 막대 위치 옵션

ggplot(rank_data_melt, aes(x=time, y=value, fill=country))+
  geom_bar(stat='identity', position='stack')
