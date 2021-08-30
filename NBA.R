install.packages("FactoMineR")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("corrplot")
install.packages("car")
install.packages("broom")
install.packages("dplyr")
install.packages("ggplot2")

library(car)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(broom)
library(dplyr)
library(ggplot2)
library(FactoMineR)

salary=read_csv(file.choose())
salary
stats=read_csv(file.choose())
stats

join_t=merge(salary,stats,by.x="Player",by.y="Player")
join_t
join_t$`2019-20`=gsub(",","",join_t$`2019-20`) 
join_t$`2019-20`=gsub('\\$', "",join_t$`2019-20`)
join_t$'2019-20'
join_t$'2019-20'=as.numeric(join_t$'2019-20')
colSums(is.na(join_t))
join_t[is.na(join_t)]=0
join_t$Pos
join_t$Pos=gsub("PF","F",join_t$Pos)
join_t$Pos=gsub("SF","F",join_t$Pos)
join_t$Pos=gsub("SG","G",join_t$Pos)
join_t$Pos=gsub("PG","G",join_t$Pos)
join_t$Pos=gsub("G-F","G",join_t$Pos)
join_t$Pos=gsub("F-F","F",join_t$Pos)
join_t$Pos=gsub("C-F","C",join_t$Pos)
join_t$Pos=gsub("F-G","F",join_t$Pos)

boxplot(join_t$`2019-20`)$stats

salary

join_t2=join_t[,-c(1,3)]
join_t2
join_t3=join_t[,-c(1,2,3)]
join_t3

cor(join_t3)
corr=cor(join_t3)
corrplot(corr,method='color')
colSums(is.na(join_t2))

model=lm(formula=join_t2$`2019-20`~., data = join_t2)
model
vif(model)

join_t3_s=scale(join_t3)
pca=PCA(join_t3,graph=F)
summary(pca)

pca$eig
fviz_screeplot(pca)
pca$var$coord
fviz_pca_var(pca, col.var = "contrib", repel=TRUE)
fviz_pca_var(pca, axes = c(2,3) ,col.var = "contrib", repel = TRUE)
fviz_pca_var(pca, axes = c(3,4) ,col.var = "contrib", repel = TRUE) # pc3와 pc4
fviz_pca_var(pca, axes = c(4,5) ,col.var = "contrib", repel = TRUE) # pc4
join_t_prcomp=prcomp(join_t3_s[,-c(1)])
join_t_prcomp
join_t_prcomp$x
y=scale(join_t2$`2019-20`)
pca.score=join_t_prcomp$x
data=cbind(as.data.frame(pca.score),y)
data
data.pcr=lm(y~PC1+PC2+PC3+PC4,data=data)
summary(data.pcr)

plot(data.pcr)
plot(data$PC1,resid(dada.pcr)) # 요인별잔차 플랏 
plot(data$PC2,resid(dada.pcr))
plot(data$PC3,resid(dada.pcr))
plot(data$PC4,resid(dada.pcr))

outlierTest(data.pcr)
pre=predict(data.pcr,newdata=data,interval="predict")
pre=as.data.frame(pre)
pre
pre=cbind(pre,data$y)

tf=NA
pre=cbind(pre,tf)
pre$tf[pre$`data$y`>=pre$lwr&pre$`data$y`<=pre$upr]=T
pre$tf[is.na(pre$tf)]=F
sum(pre$tf=="TRUE")/dim(pre)[1]
step_model=step(model, direction = "both") #변수제거법활용, 다른 모형 추정(step wise)
back_model=step(model, direction = "backward") # 백워드 모델 (aic 안좋은 영향 미치는 변수 하나씩 제거)
min_model=lm(formula=join_t2$`2019-20`~1., data = join_t2) # 포워드 모델 위해 최소 모형 생성
forward_model=step(min_model, scope = list(lower=min_model, upper=model), direction = "forward") # 포워드 모델 생성
data.pcr.broom <- data.pcr %>% glance %>% gather(통계량, 통계수치) %>% mutate(모형 = "주성분 회귀") %>% select(모형, everything())
step_model.broom<- step_model %>% glance %>% gather(통계량, 통계수치) %>% mutate(모형 = "스텝 회귀") %>% select(모형, everything())
back_model.broom<- back_model %>% glance %>% gather(통계량, 통계수치) %>% mutate(모형 = "백워드 회귀") %>% select(모형, everything())
forward_model.broom<- forward_model %>% glance %>% gather(통계량, 통계수치) %>% mutate(모형 = "포워드 회귀") %>% select(모형, everything())
lm_model=bind_rows(data.pcr.broom, step_model.broom, back_model.broom, forward_model.broom)
lm_model %>% 
  filter(통계량 == "adj.r.squared") %>% 
  ggplot(aes(x=모형, y=통계수치)) +
  geom_bar(stat="identity", width=0.3, fill="lightgreen") +
  coord_flip() +
  labs(y="조정결정계수(Adjusted R Squared)", x="모형") +
  theme_minimal(base_family = "NanumGothic") +
  geom_text(aes(label=round(통계수치,2)), position=position_dodge(width=1), vjust=-0.0, hjust=-0.1)

# 스텝회귀나 백워드 회귀 중 하나 선택, AIC를 통해서 변수선택한 모형을 주택가격 예측에 활용하는 것도 가능하지만 변수가 여전히 많고 각 변수간 상관관계가 여전히 존재하기 때문에, 다중공선성 문제가 존재하기 때문에 PCA기법으로 추출한 주성분을 변수로 해서 회귀모형을 구축하는 것이 다소 조정 설명계수에 있어 하락은 있지만 선택하기.
back_model
forward_model
d_data=transform(data, Pos_G=ifelse(join_t$Pos=="G",1,0), Pos_F=ifelse(join_t$Pos=="F",1,0)) #기준 범주를 C로하는 더미변수 생성, 선택한 회귀모델의 설명력을 높이기 위해
new.dada.pcr=lm(y~PC1+PC2+PC3+PC4+Pos_G+Pos_F, data = d_data) #포지션 더미 추가한 새로운 주성분 회귀
summary(new.dada.pcr)#주성분 회귀의 설명력 0.1 상승. 또한 주성분들의 유의성이 증가.
outlierTest(new.dada.pcr) # 이상치의 검증 =>이상치 없다고 진단.
plot(new.dada.pcr) # 새로운 모델의 회귀 진단 위해 플랏 시각화
pre_2=predict(new.dada.pcr, newdata = d_data, interval = "predict") # 앞에서 불완전 회귀 일때말고 여기서 하면 좋을듯
pre_2=as.data.frame(pre_2)
pre_2=cbind(pre,data$y)
pre_2$tf[pre$`data$y`>=pre_2$lwr&pre_2$`data$y`<=pre_2$upr]=T
pre_2$tf[is.na(pre_2$tf)]=F
sum(pre_2$tf=="TRUE")/dim(pre_2)[1] #예측 성공률 93퍼센트
