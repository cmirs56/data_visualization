setwd("C:/Users/MC/R/dacon")
library(readr)
train_target <- read_csv("DAT/train_target.csv")
train_features <- read_csv("DAT/train_features.csv")

str(train_features)
str(train_target)

summary(train_features)
summary(train_target)


# data distribution -------------------------------------------------------

X <- train_target$X
Y <- train_target$Y
plot(X, Y)          # 충격하중이 가해진 위치는 표면적 전체에 골고루 퍼져있음

M <- train_target$M
V <- train_target$V
plot(M, V)          # 충돌체의 질량과 충돌 속도 간에 특별한 관계 없음 

library(vioplot)

par(mfrow = c(1, 2))
boxplot(M)
vioplot(M, col = "blue")

par(mfrow = c(1, 2))
boxplot(V)
vioplot(V, col = "blue")


# ts 가속도: 센서별 가속도 기록 각 S1,2,3,4 칼럼별

library(tidyverse)

ts_1st <- ts(train_features %>% 
                   filter(id < 1) %>%
                   group_by(id), start = 0.000000, frequency = 0.000004)
plot(ts_1st)

ts_under10 <- ts(train_features %>% 
                   filter(id < 10) %>%
                   group_by(id), start = 0.000000, frequency = 0.000004)
plot(ts_under10)


# 데이터 병합_Outer Join_train_features & train_target 
df_merged <- merge(x = train_features, y = train_target,
                   by = 'id', all = TRUE)
df_merged
str(df_merged)
summary(df_merged)

df_merged$id <- as.character(df_merged$id)
train_features$id <- as.character(train_features$id)
train_target$id <- as.character(train_target$id)

moonBook::mytable(df_merged)
moonBook::mytable(train_features)
moonBook::mytable(train_target)


# 가속도 - 충돌체질량 관계 
# ggplot2 -----------------------------------------------------------------
library(ggplot2)
library(gridExtra)

m1 <- ggplot(df_merged[1:1050000, ], aes(x = Time, y = S1))+
  geom_line(aes(color = ifelse(M >= 100, "heavy", "light")), size = 1)+
  scale_color_discrete(name = "weight")+
  theme_bw()

m2 <- ggplot(df_merged[1:1050000, ], aes(x = Time, y = S2))+
  geom_line(aes(color = ifelse(M >= 100, "heavy", "light")), size = 1)+
  scale_color_discrete(name = "weight")+
  theme_bw()

m3 <- ggplot(df_merged[1:1050000, ], aes(x = Time, y = S3))+
  geom_line(aes(color = ifelse(M >= 100, "heavy", "light")), size = 1)+
  scale_color_discrete(name = "weight")+
  theme_bw()

m4 <- ggplot(df_merged[1:1050000, ], aes(x = Time, y = S4))+
  geom_line(aes(color = ifelse(M >= 100, "heavy", "light")), size = 1)+
  scale_color_discrete(name = "weight")+
  theme_bw()

grid.arrange(m1, m2, m3, m4, nrow = 2, ncol = 2)


# 가속도 - 충돌체 속도 관계 ---------------------------------------------------------

for (i in 1:4) {
  ~~~~~ 
}

v1 <- ggplot(df_merged[1:105000, ], aes(x = Time, y = S1))+
  geom_line(aes(color = ifelse(V >= 0.6, "fast", "slow")), size = 1)+
  scale_color_discrete(name = "velocity")+
  theme_bw()

v2 <- ggplot(df_merged[1:105000, ], aes(x = Time, y = S2))+
  geom_line(aes(color = ifelse(V >= 0.6, "fast", "slow")), size = 1)+
  scale_color_discrete(name = "velocity")+
  theme_bw()

v3 <- ggplot(df_merged[1:105000, ], aes(x = Time, y = S3))+
  geom_line(aes(color = ifelse(V >= 0.6, "fast", "slow")), size = 1)+
  scale_color_discrete(name = "velocity")+
  theme_bw()

v4 <- ggplot(df_merged[1:105000, ], aes(x = Time, y = S4))+
  geom_line(aes(color = ifelse(V >= 0.6, "fast", "slow")), size = 1)+
  scale_color_discrete(name = "velocity")+
  theme_bw()

grid.arrange(v1, v2, v3, v4, nrow = 2, ncol = 2)


# 파생변수 생성: max & min - range or 면적 signal별 또는 한꺼번에...상관관계 살펴보기  
# start point, end point, length, mean ... id별   

# [sample code] 
tmp <- data.frame(train_df %>% 
                    group_by(id) %>% 
                    mutate(s1_mean = mean(S1), s1_median = median(S1)) %>% 
                    select(id,s1_mean,s1_median) %>% 
                    distinct())

# 파생변수 
max <- data.frame(df_merged %>% 
                    group_by(id) %>% 
                    )


# 센서 S1~4 충돌 감지 최초시간 - 좌표 X, Y 까지의 거리 

# 센서 S1~4 충돌 최대 진폭 - 거리 관계 
