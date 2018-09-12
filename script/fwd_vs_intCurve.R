# analisis curvas tasas interes vs forward --
library(tidyverse)
library(xts)
library(data.table)
library(stringr)
library(lubridate)

### 0.0 working directory ----
setwd("C:/Users/gnovoa/Desktop/Santiago/Tresalia/MS/curva_tasas")
### 0.1 cargar base ----
# curves <- read_csv("InterestRateCurves_hist.csv", col_types = list(Date = col_date(format = "%m/%d/%Y")))
# names(curves)[-1] <- c("mo1", "mo3", "mo6", 
#                        "yr1", "yr2", "yr3", "yr5", "yr7", "yr10", "yr20", "yr30")
curves_max <- read_tsv("../../../Bases_generales/treasury_txt/treasury_Daily.txt", 
         na = c("<NA>","."), 
         col_types = list(DATE = col_date(), 
                          DGS1MO = col_double(), DGS3MO = col_double(), DGS6MO = col_double(),
                          DGS2 = col_double(),DGS20 = col_double(),DGS30 = col_double(), DGS7 = col_double()))
names(curves_max) <- c("Date","yr1","yr10","mo1","yr2","yr20","yr3", "yr30","mo3", "yr5","mo6","yr7")
curves <- curves_max %>% 
  dplyr::select(Date, mo1,mo3,mo6,yr1,yr2,yr3,yr5,yr7,yr10,yr20,yr30) %>% 
  dplyr::filter(year(Date)>2006)


### 0.2 graficas iniciales series ----
# ?matplot
# matplot(curves[,-1])
# ?xts
par(mfrow=c(1,1))
curves_xts <- xts(curves[,-1], order.by = curves$Date)
plot(curves_xts)
curves_T <- curves %>% 
  tidyr::gather(key = plazo, value = tasa, -Date) %>% 
  dplyr::mutate(plazo_y = case_when(
    plazo == "mo1" ~ 1/12,
    plazo == "mo3" ~ 3/12,
    plazo == "mo6" ~ 6/12,
    plazo == "yr1" ~ 1,
    plazo == "yr2" ~ 2,
    plazo == "yr3" ~ 3,
    plazo == "yr5" ~ 5,
    plazo == "yr7" ~ 7,
    plazo == "yr10" ~ 10,
    plazo == "yr20" ~ 20,
    plazo == "yr30" ~ 30))
scaleFUN <- function(x) sprintf("%.2f", x)
curves_T %>% 
  dplyr::group_by(Date) %>% 
  ggplot(aes(x = plazo_y, y = tasa, colour = factor(Date))) + 
  geom_line(alpha = 0.1, show.legend = F, size = 1.35) + geom_point(alpha = 0.2, show.legend = F) + 
  # geom_smooth(se=F, show.legend = F)+
  scale_x_continuous(breaks = c(1/12,3/12,6/12,1,2,3,5,7,10,20,30), labels=scaleFUN) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Curvas Yield Históricas") + xlab("plazo")

### 0.3 curvas fwd historicas ----
# tasas en terminos /100
curves_perc <- curves[,-1]/100
curves_perc$Date <- curves$Date
curves_perc
# funcion para pasar de tasa anual a tasa en el plazo descrito ---
i_plazo <- function(p_anual = 1, t_anual = 1, yearly = F){
  if(yearly){
  t_real = (1+t_anual)^(1/p_anual)-1
  }else{ 
    t_real = (1+t_anual)^(p_anual) - 1
    } 
  t_real
}
curves_perc <- curves_perc %>% 
  dplyr::mutate(
    mo1_r = i_plazo(p_anual = 1/12,t_anual = mo1, yearly = F) %>% round(5),
    mo3_r = i_plazo(p_anual = 3/12,t_anual = mo3) %>% round(5),
    mo6_r = i_plazo(p_anual = 6/12,t_anual = mo6) %>% round(5),
    yr1_r = i_plazo(p_anual = 1,t_anual = yr1) %>% round(5),
    yr2_r = i_plazo(p_anual = 2,t_anual = yr2) %>% round(5),
    yr3_r = i_plazo(p_anual = 3,t_anual = yr3) %>% round(5),
    yr5_r = i_plazo(p_anual = 5,t_anual = yr5) %>% round(5),
    yr7_r = i_plazo(p_anual = 7,t_anual = yr7) %>% round(5),
    yr10_r = i_plazo(p_anual = 10,t_anual = yr10) %>% round(5),
    yr20_r = i_plazo(p_anual = 20,t_anual = yr20) %>% round(5),
    yr30_r = i_plazo(p_anual = 30,t_anual = yr30) %>% round(5)
  ) %>% 
  dplyr::select(Date, mo1:yr30,mo1_r:yr30_r)


### funcion para calcular tasas fwd ---
fwd_calc <- function(t_a = 0, t_b = 0){
  fwd_ab = ((1+t_b)/(1+t_a))-1
  fwd_ab
}
curves_perc <- curves_perc %>% 
  dplyr::mutate(
    #mo1
    fwd1m3m_r = fwd_calc(mo1_r,mo3_r),
    fwd1m6m_r = fwd_calc(mo1_r,mo6_r),
    fwd1m1y_r = fwd_calc(mo1_r,yr1_r),
    fwd1m2y_r = fwd_calc(mo1_r,yr2_r),
    fwd1m3y_r = fwd_calc(mo1_r,yr3_r),
    fwd1m5y_r = fwd_calc(mo1_r,yr5_r),
    fwd1m7y_r = fwd_calc(mo1_r,yr7_r),
    fwd1m10y_r = fwd_calc(mo1_r,yr10_r),
    fwd1m20y_r = fwd_calc(mo1_r,yr20_r),
    fwd1m30y_r = fwd_calc(mo1_r,yr30_r),
    #mo3
    fwd3m6m_r = fwd_calc(mo3_r,mo6_r),
    fwd3m1y_r = fwd_calc(mo3_r,yr1_r),
    fwd3m2y_r = fwd_calc(mo3_r,yr2_r),
    fwd3m3y_r = fwd_calc(mo3_r,yr3_r),
    fwd3m5y_r = fwd_calc(mo3_r,yr5_r),
    fwd3m7y_r = fwd_calc(mo3_r,yr7_r),
    fwd3m10y_r = fwd_calc(mo3_r,yr10_r),
    fwd3m20y_r = fwd_calc(mo3_r,yr20_r),
    fwd3m30y_r = fwd_calc(mo3_r,yr30_r),
    #mo6
    fwd6m1y_r = fwd_calc(mo6_r,yr1_r),
    fwd6m2y_r = fwd_calc(mo6_r,yr2_r),
    fwd6m3y_r = fwd_calc(mo6_r,yr3_r),
    fwd6m5y_r = fwd_calc(mo6_r,yr5_r),
    fwd6m7y_r = fwd_calc(mo6_r,yr7_r),
    fwd6m10y_r = fwd_calc(mo6_r,yr10_r),
    fwd6m20y_r = fwd_calc(mo6_r,yr20_r),
    fwd6m30y_r = fwd_calc(mo6_r,yr30_r),
    #yr1
    fwd1y2y_r = fwd_calc(yr1_r,yr2_r),
    fwd1y3y_r = fwd_calc(yr1_r,yr3_r),
    fwd1y5y_r = fwd_calc(yr1_r,yr5_r),
    fwd1y7y_r = fwd_calc(yr1_r,yr7_r),
    fwd1y10y_r = fwd_calc(yr1_r,yr10_r),
    fwd1y20y_r = fwd_calc(yr1_r,yr20_r),
    fwd1y30y_r = fwd_calc(yr1_r,yr30_r),
    #yr2
    fwd2y3y_r = fwd_calc(yr2_r,yr3_r),
    fwd2y5y_r = fwd_calc(yr2_r,yr5_r),
    fwd2y7y_r = fwd_calc(yr2_r,yr7_r),
    fwd2y10y_r = fwd_calc(yr2_r,yr10_r),
    fwd2y20y_r = fwd_calc(yr2_r,yr20_r),
    fwd2y30y_r = fwd_calc(yr2_r,yr30_r),
    #yr3
    fwd3y5y_r = fwd_calc(yr3_r,yr5_r),
    fwd3y7y_r = fwd_calc(yr3_r,yr7_r),
    fwd3y10y_r = fwd_calc(yr3_r,yr10_r),
    fwd3y20y_r = fwd_calc(yr3_r,yr20_r),
    fwd3y30y_r = fwd_calc(yr3_r,yr30_r),
    #yr5
    fwd5y7y_r = fwd_calc(yr5_r,yr7_r),
    fwd5y10y_r = fwd_calc(yr5_r,yr10_r),
    fwd5y20y_r = fwd_calc(yr5_r,yr20_r),
    fwd5y30y_r = fwd_calc(yr5_r,yr30_r),
    #yr7
    fwd7y10y_r = fwd_calc(yr7_r,yr10_r),
    fwd7y20y_r = fwd_calc(yr7_r,yr20_r),
    fwd7y30y_r = fwd_calc(yr7_r,yr30_r),
    #yr10
    fwd10y20y_r = fwd_calc(yr10_r,yr20_r),
    fwd10y30y_r = fwd_calc(yr10_r,yr30_r),
    #yr20
    fwd20y30y_r = fwd_calc(yr20_r,yr30_r)
  ) %>% as.data.table()

curves_perc <- curves_perc %>% 
  dplyr::mutate(
    #mo1
    fwd1m3m = i_plazo(p_anual = (3-1)*1/12, t_anual = fwd1m3m_r, yearly = T),
    fwd1m6m = i_plazo(p_anual = (6-1)*1/12, t_anual = fwd1m6m_r, yearly = T),
    fwd1m1y = i_plazo(p_anual = (12*1-1)*1/12, t_anual = fwd1m1y_r, yearly = T),
    fwd1m2y = i_plazo(p_anual = (12*2-1)*1/12, t_anual = fwd1m2y_r, yearly = T),
    fwd1m3y = i_plazo(p_anual = (12*3-1)*1/12, t_anual = fwd1m3y_r, yearly = T),
    fwd1m5y = i_plazo(p_anual = (12*5-1)*1/12, t_anual = fwd1m5y_r, yearly = T),
    fwd1m7y = i_plazo(p_anual = (12*7-1)*1/12, t_anual = fwd1m7y_r, yearly = T),
    fwd1m10y = i_plazo(p_anual = (12*10-1)*1/12, t_anual = fwd1m10y_r, yearly = T),
    fwd1m20y = i_plazo(p_anual = (12*20-1)*1/12, t_anual = fwd1m20y_r, yearly = T),
    fwd1m30y = i_plazo(p_anual = (12*30-1)*1/12, t_anual = fwd1m30y_r, yearly = T),
    #mo3
    fwd3m6m = i_plazo(p_anual = (6-3)*1/12, t_anual = fwd3m6m_r, yearly = T),
    fwd3m1y = i_plazo(p_anual = (12-3)*1/12, t_anual = fwd3m1y_r, yearly = T),
    fwd3m2y = i_plazo(p_anual = (12*2-3)*1/12, t_anual = fwd3m2y_r, yearly = T),
    fwd3m3y = i_plazo(p_anual = (12*3-3)*1/12, t_anual = fwd3m3y_r, yearly = T),
    fwd3m5y = i_plazo(p_anual = (12*5-3)*1/12, t_anual = fwd3m5y_r, yearly = T),
    fwd3m7y = i_plazo(p_anual = (12*7-1)*1/12, t_anual = fwd3m7y_r, yearly = T),
    fwd3m10y = i_plazo(p_anual = (12*10-1)*1/12, t_anual = fwd3m10y_r, yearly = T),
    fwd3m20y = i_plazo(p_anual = (12*20-1)*1/12, t_anual = fwd3m20y_r, yearly = T),
    fwd3m30y = i_plazo(p_anual = (12*30-1)*1/12, t_anual = fwd3m30y_r, yearly = T),
    #mo6
    fwd6m1y = i_plazo(p_anual = (12-6)*1/12, t_anual = fwd6m1y_r, yearly = T),
    fwd6m2y = i_plazo(p_anual = (12*2-6)*1/12, t_anual = fwd6m2y_r, yearly = T),
    fwd6m3y = i_plazo(p_anual = (12*3-6)*1/12, t_anual = fwd6m3y_r, yearly = T),
    fwd6m5y = i_plazo(p_anual = (12*5-6)*1/12, t_anual = fwd6m5y_r, yearly = T),
    fwd6m7y = i_plazo(p_anual = (12*7-1)*1/12, t_anual = fwd6m7y_r, yearly = T),
    fwd6m10y = i_plazo(p_anual = (12*10-1)*1/12, t_anual = fwd6m10y_r, yearly = T),
    fwd6m20y = i_plazo(p_anual = (12*20-1)*1/12, t_anual = fwd6m20y_r, yearly = T),
    fwd6m30y = i_plazo(p_anual = (12*30-1)*1/12, t_anual = fwd6m30y_r, yearly = T),
    #yr1
    fwd1y2y = i_plazo(p_anual = (2-1), t_anual = fwd1y2y_r, yearly = T),
    fwd1y3y = i_plazo(p_anual = (3-1), t_anual = fwd1y3y_r, yearly = T),
    fwd1y5y = i_plazo(p_anual = (5-1), t_anual = fwd1y5y_r, yearly = T),
    fwd1y7y = i_plazo(p_anual = (7-1), t_anual = fwd1y7y_r, yearly = T),
    fwd1y10y = i_plazo(p_anual = (10-1), t_anual = fwd1y10y_r, yearly = T),
    fwd1y20y = i_plazo(p_anual = (20-1), t_anual = fwd1y20y_r, yearly = T),
    fwd1y30y = i_plazo(p_anual = (30-1), t_anual = fwd1y30y_r, yearly = T),
    #yr2
    fwd2y3y = i_plazo(p_anual = (3-2), t_anual = fwd2y3y_r, yearly = T),
    fwd2y5y = i_plazo(p_anual = (5-2), t_anual = fwd2y5y_r, yearly = T),
    fwd2y7y = i_plazo(p_anual = (7-2), t_anual = fwd2y7y_r, yearly = T),
    fwd2y10y = i_plazo(p_anual = (10-2), t_anual = fwd2y10y_r, yearly = T),
    fwd2y20y = i_plazo(p_anual = (20-2), t_anual = fwd2y20y_r, yearly = T),
    fwd2y30y = i_plazo(p_anual = (30-2), t_anual = fwd2y30y_r, yearly = T),
    #yr3
    fwd3y5y = i_plazo(p_anual = (5-3), t_anual = fwd3y5y_r, yearly = T),
    fwd3y7y = i_plazo(p_anual = (7-3), t_anual = fwd3y7y_r, yearly = T),
    fwd3y10y = i_plazo(p_anual = (10-3), t_anual = fwd3y10y_r, yearly = T),
    fwd3y20y = i_plazo(p_anual = (20-3), t_anual = fwd3y20y_r, yearly = T),
    fwd3y30y = i_plazo(p_anual = (30-3), t_anual = fwd3y30y_r, yearly = T),
    #yr5
    fwd5y7y = i_plazo(p_anual = (7-5), t_anual = fwd5y7y_r, yearly = T),
    fwd5y10y = i_plazo(p_anual = (10-5), t_anual = fwd5y10y_r, yearly = T),
    fwd5y20y = i_plazo(p_anual = (20-5), t_anual = fwd5y20y_r, yearly = T),
    fwd5y30y = i_plazo(p_anual = (30-5), t_anual = fwd5y30y_r, yearly = T),
    #yr7
    fwd7y10y = i_plazo(p_anual = (10-7), t_anual = fwd7y10y_r, yearly = T),
    fwd7y20y = i_plazo(p_anual = (20-7), t_anual = fwd7y20y_r, yearly = T),
    fwd7y30y = i_plazo(p_anual = (30-7), t_anual = fwd7y30y_r, yearly = T),
    #yr10
    fwd10y20y = i_plazo(p_anual = (20-10), t_anual = fwd10y20y_r, yearly = T),
    fwd10y30y = i_plazo(p_anual = (30-10), t_anual = fwd10y30y_r, yearly = T),
    #yr20
    fwd20y30y = i_plazo(p_anual = (30-20), t_anual = fwd20y30y_r, yearly = T)
  ) %>% 
  dplyr::select(Date:yr30_r,fwd1m3m:fwd20y30y, fwd1m3m_r:fwd20y30y_r) %>% 
  as.data.table()

curves_y <-curves_perc %>% 
  dplyr::select(Date, mo1:yr30, fwd1m3m:fwd20y30y)
curves_r <- curves_perc%>% 
  dplyr::select(Date, mo1_r:yr30_r, fwd1m3m_r:fwd20y30y_r)

### 1.0 graficas comparativas ----
par(mfrow = c(2,1))
plot(curves_xts)
fwd_xts <- xts(curves_y[,13:67]*100, order.by = curves_y$Date)
plot(fwd_xts)
par(mfrow = c(1,1))

curves_y %>% glimpse()
curves_y %>% 
  tidyr::gather(key = tasa, value = serie, c(yr1, fwd1y2y)) %>% 
  ggplot(aes(x = Date, y = serie*100, colour = tasa)) + 
  geom_line(alpha = 0.5) + geom_point(alpha = 0.75) + 
  theme_bw() + scale_x_date(date_breaks = "6 months", date_labels = "%B-%y") + 
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust=1, vjust = 0)) +
  ylab("")

library(lubridate)
curves_y %>% 
  dplyr::mutate(
    yearD = lubridate::year(Date),
    monthD = lubridate::month(Date),
    dayD = lubridate::day(Date)) %>% 
  dplyr::group_by(yearD) %>% 
  dplyr::summarise(n = n())

# 3 meses vs fwd 3m,6m
ggplot_3mo <- curves_y %>% 
  dplyr::select(Date, mo3, fwd3m6m) %>% 
  dplyr::mutate(fwd = lag(fwd3m6m,n = 63)) %>%
  tidyr::gather(key = serie, value = tasa, c(mo3,fwd)) %>% 
  ggplot(aes(x = Date, y = tasa*100, colour = serie)) + 
  geom_point(alpha = 0.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%B- %y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0)) + 
  ylab("") + ggtitle("3 months vs fwd3m,6m")
ggplot_3mo

# 6 mo vs fwd 6m,1y
ggplot_6mo <- curves_y %>% 
  dplyr::select(Date, mo6, fwd6m1y) %>% 
  dplyr::mutate(fwd = lag(fwd6m1y,n = 131)) %>%
  tidyr::gather(key = serie, value = tasa, c(mo6,fwd)) %>% 
  ggplot(aes(x = Date, y = tasa*100, colour = serie)) + 
  geom_point(alpha = 0.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%B- %y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0)) + 
  ylab("") + ggtitle("6 months vs fwd6m,1y")
ggplot_6mo

# 1 yr vs fwd 1y,2y
ggplot_1yr <- curves_y %>% 
  dplyr::select(Date, yr1, fwd1y2y) %>% 
  dplyr::mutate(fwd = lag(fwd1y2y,n = 261)) %>%
  tidyr::gather(key = serie, value = tasa, c(yr1,fwd)) %>% 
  ggplot(aes(x = Date, y = tasa*100, colour = serie)) + 
  geom_point(alpha = 0.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%B- %y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0)) + 
  ylab("") + ggtitle("1 yr vs fwd1y,2y")
ggplot_1yr

library(gridExtra)
grid.arrange(ggplot_3mo, ggplot_6mo, ggplot_1yr, ncol = 1)

# 1 yr vs fwd 2y,3y
curves_y %>% 
  dplyr::select(Date, yr1, fwd2y3y) %>% 
  dplyr::mutate(fwd = lag(fwd2y3y,n = 261*2)) %>%
  tidyr::gather(key = serie, value = tasa, c(yr1,fwd)) %>% 
  ggplot(aes(x = Date, y = tasa*100, colour = serie)) + 
  geom_point(alpha = 0.4) +
  scale_x_date(date_breaks = "3 months", date_labels = "%B- %y") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, size = 8, hjust = 1, vjust = 0)) + 
  ylab("") + ggtitle("1 yr vs fwd2y,3y")



### 2.0 crear curva fwd actual ----

#curva yield
curves_y[["Date"]][nrow(curves_y)]
yield_act <- curves_y[nrow(curves_y),2:12]*100
yield_points <- c(1/12,3/12,6/12,1,2,3,5,7,10,20,30)

yield_curve <- spline(x = yield_points, 
       y = yield_act, 
       xmin = 0, xmax = 30, n = 30*12*30)
yield_curve %>% plot(type = "p", main = "Curva yield\n2018-07-25", xlab = "Plazos", ylab = "Tasas")
yield_curve %>% 
  as.data.frame() %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_point(col = "midnightblue") + theme_bw() + 
  xlab("Plazos") + ylab("Tasas") + ggtitle("Curva Yield\n2018-07-25") + 
  scale_x_continuous(breaks = c(1/12,1/6,1:30), labels=scaleFUN) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0, size = 7)) 

# curva fwd
n <- nrow(curves_y)
curves_act <- curves_y[n,] %>% as.data.frame()  
imp_df <- cbind(curves_act$mo1*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd1m") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df, curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd3m") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd6m") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd1y") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd2y") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd3y") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd5y") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd7y") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd10y") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_fwd <- curves_y %>% 
  names() %>% 
  str_detect("fwd20y") %>% 
  which() %>% 
  .[1]
imp_df<- cbind(imp_df,curves_act[,imp_fwd]*100)
imp_df
fwd_act <- data_frame(tasa_prom = c(0,imp_df %>% as.double()), punto_curva = c(0,yield_points))
fwd_act
# fwd_act <- curves_y[nrow(curves_y),13:ncol(curves_y)]*100
# j<-1
# k<-0
# fwd_points <- rep(NA_real_,ncol(fwd_act))
# for(i in 1:(length(yield_points)-1)){
#   k <- length(yield_points) - i + k
#   fwd_points[j:k] <- (yield_points[i:length(yield_points)]-yield_points[i])[-1]
#   j <- k+1
# }
# fwd_duplicados <- which(duplicated(fwd_points))
# names(fwd_act)[fwd_duplicados]
# fwd_names <- names(fwd_act)[-fwd_duplicados]
# fwd_act <- fwd_act %>% 
#   dplyr::select(fwd_names)
# fwd_points <- fwd_points[-fwd_duplicados]
# 
# fwd_df <- data.frame(points = fwd_points %>% as.double(),act = fwd_act %>% as.double())
# fwd_df <- fwd_df %>% 
#   dplyr::arrange(points)
# 
fwd_curve <- spline(x = fwd_act$punto_curva[-c(1,2)],
                      y = fwd_act$tasa_prom[-c(1,2)],
                      xmin = 0, xmax = 30, n = 30*12*30)
fwd_curve %>% plot(type = "p", main = "Curva Fwd\n2018-07-25", xlab = "Plazos", ylab = "Tasas")
fwd_curve %>%
  as.data.frame() %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(col = "midnightblue") + theme_bw() +
  xlab("Plazos") + ylab("Tasas") + ggtitle("Curva Yield\n2018-07-25") +
  scale_x_continuous(breaks = c(1/12,1/6,1:30), labels=scaleFUN) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0, size = 7))
plot_curve <- yield_curve %>% as.data.frame()
plot_curve$fwd_prom <- NA_real_
j <- 1
for(i in 2:nrow(fwd_act)){
  x_point <- which(yield_curve$x>=fwd_act$punto_curva[i])[1]
  plot_curve$fwd_prom[j:x_point] <- fwd_act$tasa_prom[i]
  j <- x_point+1
}
plot_curve$fwd_spline <- fwd_curve$y
plot_curve %>% 
  tidyr::gather(key = tasa, value = valor, c(y,fwd_prom,fwd_spline)) %>% 
  ggplot(aes(x = x, y = valor, colour = tasa)) + 
  geom_point(alpha = 0.5) +
  theme_bw() + theme(axis.text.x = element_text(size = 7)) + 
  scale_x_continuous(breaks = c(1/12,3/12,6/12,1:30), labels = scaleFUN) + 
  ggtitle("Curvas: \nYield = y\nForward Promedio = fwd_prom\nForward Splines = fwd_splines") + 
  xlab("plazo") + ylab("tasa")
