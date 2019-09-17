getwd()
setwd("/home/bbobbio/dev/R/Curso R/adtracking-fraud")

df = read.csv("train_sample.csv", header = TRUE, sep = ",", stringsAsFactors = F)

colSums(is.na(df))
df$attributed_time = NULL

colSums(df=="")

library(lubridate)
df$year = year(df$click_time)
df$month = month(df$click_time)
df$day = day(df$click_time)
df$hour = hour(df$click_time)
df$minute = minute(df$click_time)
df$second = second(df$click_time)

df$click_time = NULL

library(dplyr)
df = df %>%
  add_count(ip) %>% rename("n_ip" = n) %>%
  add_count(app) %>% rename("n_app" = n) %>%
  add_count(device) %>% rename("n_device" = n) %>%
  add_count(os) %>% rename("n_os" = n) %>%
  add_count(ip, day, hour) %>% rename("ip_day_hour" = n) %>%
  add_count(app, day, hour) %>% rename("app_day_hour" = n) %>%
  add_count(device, day, hour) %>% rename("device_day_hour" = n) %>%
  add_count(os, day, hour) %>% rename("os_day_hour" = n) %>%
  add_count(ip,app) %>% rename("ip_app" = n) %>%
  add_count(ip,app,os) %>% rename("ip_app_os" = n) %>%
  add_count(app,os) %>% rename("app_os" = n) %>%
  add_count(ip,device) %>% rename("ip_device" = n)

library(ggplot2)
df$is_attributed = as.factor(df$is_attributed)
p1 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=n_ip, fill = is_attributed))
p2 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=n_app, fill = is_attributed))
p3 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=n_device, fill = is_attributed))
p4 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=n_os, fill = is_attributed))

p5 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=ip_day_hour, fill = is_attributed))
p6 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=app_day_hour, fill = is_attributed))
p7 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=device_day_hour, fill = is_attributed))
p8 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=os_day_hour, fill = is_attributed))

p9 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=ip_app, fill = is_attributed))
p10 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=ip_app_os, fill = is_attributed))
p11 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=app_os, fill = is_attributed))
p12 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=ip_device, fill = is_attributed))

p13 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=ip, fill = is_attributed))
p14 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=app, fill = is_attributed))
p15 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=os, fill = is_attributed))
p16 = ggplot(df) + geom_boxplot(aes(x=is_attributed, y=channel, fill = is_attributed))

library(gridExtra)
grid.arrange(p1,p2,p3,p4 , nrow=2,ncol=2)
grid.arrange(p5,p6,p7,p8 , nrow=2,ncol=2)
grid.arrange(p9,p10,p11,p12 , nrow=2,ncol=2)
grid.arrange(p13,p14,p15,p16 , nrow=2,ncol=2)

df %>% filter(is_attributed == 1) %>% ggplot() + geom_bar(aes(x = as.factor(hour)))
df %>% filter(is_attributed == 0) %>% ggplot() + geom_bar(aes(x = as.factor(hour)))

indexes = sample(1:nrow(df), size = 0.6 * nrow(df))
train_data = df[indexes,c("is_attributed", "ip", "app_os", "channel")]
test_data = df[-indexes,c("is_attributed", "ip", "app_os", "channel")]

"----------------------------KNN----------------------------"
"
library(class)
modelo_knn = knn(train = train_data,
                 test = test_data,
                 cl = train_data$is_attributed,
                 k = 20)

library(gmodels)

CrossTable(x= test_data$is_attributed, y = modelo_knn)
"
"---------------------------SVM-----------------------------"
"
library(e1071)
modelo_svm = svm(is_attributed ~ .,
                 data = train_data,
                 type = 'C-classification',
                 kernel = 'radial')

pred_train = predict(modelo_svm, train_data)
mean(pred_train == train_data$is_attributed)

pred_test = predict(modelo_svm, test_data)
mean(pred_test == test_data$is_attributed)

table(pred_test, test_data$is_attributed)
"
"--------------------------RF------------------------------"
library(rpart)
modelo_rf = rpart(is_attributed ~ ., data = train_data, control = rpart.control(cp = .005))

rf_pred = predict(modelo_rf, test_data, type = "class")
mean(rf_pred==test_data$is_attributed)
table(rf_pred, test_data$is_attributed)
