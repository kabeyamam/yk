#yk研究
library(tidyverse)
library(lubridate)
library(gnnlab)
library(magick)
library(showtext)
library(readxl)
library(patchwork)

#溶存酸素データ
df = tibble(co2 = dir("~/Lab_Data/Yamahak/実験データ/202207シリーズ/220723_DO/220723_DOdata/",
                      full = T))  |> 
  mutate(data = map(co2,read_csv,skip = 5)) |> 
  unnest(data) |> 
  mutate(co2 =  as.double(str_remove_all(basename(co2),
                                         pattern = ".+_|\\.csv")),
         datetime = floor_date(ymd_hms(str_c(DATE,TIME,sep = " ")),
                               unit = "minute")) |> 
  rename(ODO = "ODO (mg/L)",
         temp = "Temp (°C)") |> 
  select(c(datetime,co2,ODO,temp)) 

#時間データとの結合
data = read_csv("~/Lab_Data/Yamahak/jikken_time.csv") |>
  mutate(starttime = ymd_hms(str_c(date,starttime, sep = " ")),
         endtime = ymd_hms(str_c(date,endtime, sep = " ")),
         id = as.character(id)) |>
  mutate(datetime = map2(starttime,endtime,\(starttime,endtime){
    seq(starttime, endtime, by = 60)
  })) |> 
  unnest(datetime) |> 
  select(c(id,co2,datetime)) |> 
  left_join(df,by = c("co2","datetime")) |> 
  drop_na() |> 
  group_by(id,co2) |> 
  mutate(minutes = as.double(str_remove((datetime-min(datetime))/60,
                                  pattern = "secs"))) |> 
  ungroup()

#データ可視化
data |> ggplot() +
  geom_point(aes(x = minutes,y = ODO,color = id))+
  facet_wrap(vars(co2))


#モデリング
m0 = glm(ODO ~ minutes+co2,family = gaussian(link = "identity"),data = data)
m1 = glm(ODO ~ minutes*co2,family = gaussian(link = "identity"),data = data)
m2 = glm(ODO ~ minutes+co2,family = Gamma(link = "identity"),data = data)
m3 = glm(ODO ~ minutes*co2,family = Gamma(link = "identity"),data = data)
m4 = glm(ODO ~ minutes+co2,family = Gamma(link = "log"),data = data)
m5 = glm(ODO ~ minutes+co2,family = gaussian(link = "log"),data = data)


AIC(m0,m1,m2,m3,m4,m5)
summary(m4)

hat = predict(m4, interval="confidence", level=0.95)
se = predict(m4, se.fit = T)$se.fit
df1 = data|> mutate(hat, se) |> 
  mutate(l95 = hat - 1.96 * se,
         u95 = hat + 1.96 * se) |> 
  mutate(across(c(hat, l95, u95),exp))

df1 |> ggplot() +
  geom_point(aes(x = minutes,
                 y = ODO,
                 color = as.factor(co2)))+
  geom_line(aes(x = minutes, 
                y = hat,
                group = as.factor(co2),
                color = as.factor(co2)))
  # facet_wrap(vars(co2))
  # geom_ribbon(aes(x = minutes, ymin = l95, ymax = u95), fill = "turquoise4", alpha = 0.3)+

df1 |> ggplot() +
  geom_point(aes(x = minutes,
                 y = ODO,
                 color = co2))+
  geom_line(aes(x = minutes, 
                y = hat,
                group = co2,
                color = co2))+
  scale_color_viridis_b(end = 1)


df1 |> ggplot() + 
  geom_point(aes(x = minutes,
                 y = ODO)) + 
  facet_wrap(vars(id))

df1 |> ggplot() + 
  geom_point(aes(x = minutes,
                 y = ODO)) + 
  facet_wrap(vars(co2))

