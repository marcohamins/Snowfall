library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(scales)
library(ggpubr)
library(patchwork)
library(ggimage)

snowfalldata <- read_csv("~/Google Drive/My Drive/Fun/USW00014739_2_24_25.csv")
preyear = "2024"
year = "2025"
currentYearRange = paste0(preyear,"-",year)
preYearRange = paste0(as.numeric(preyear)-1,"-",as.numeric(year)-1)

snowfalldata <-snowfalldata %>% mutate(year = year(snowfalldata$DATE)) %>%
  mutate(SNOW = ifelse(is.na(SNOW),0,SNOW)) %>% 
  mutate(doy = yday(snowfalldata$DATE)) %>% 
  mutate(sameyeardate = as.Date("2021-12-31") + doy) %>% 
  mutate(daysinceOct1 = ifelse(sameyeardate < as.Date("2022-09-30"),
                               as.Date(sameyeardate) - as.Date("2021-09-30"),
                               as.Date(sameyeardate) - as.Date("2022-09-30"))) %>% 
  mutate(datesinceOct1 = as.Date("2021-09-30") + daysinceOct1) %>% 
  mutate(yearRange= ifelse(sameyeardate < as.Date("2022-09-30"),
                           paste0(as.character(as.numeric(year)-1),"-",year),
                           paste0(year,"-",as.character(as.numeric(year)+1)))) %>% 
  group_by(yearRange) %>% mutate(cum_sum = cumsum(SNOW)) %>% 
  mutate(cum_sum_prec = cumsum(PRCP)) %>% 
  mutate(impYears = ifelse(yearRange == currentYearRange,currentYearRange,ifelse(yearRange =="2014-2015",
                                                                       "2014-2015",
                                                                       "Historical data"))) %>%
  mutate(impYears2 = ifelse(yearRange == currentYearRange,currentYearRange,ifelse(yearRange ==preYearRange,
                                                                                  preYearRange,
                                                                       "Historical data"))) %>% 
  mutate(iscurrentyear = ifelse(yearRange == currentYearRange,currentYearRange,"Historical data")) %>% 
  mutate(sizevals = ifelse(yearRange == currentYearRange,1.5,.75)) %>% 
  mutate(alphavals = ifelse(yearRange %in% c(currentYearRange,"2014-2015"),1,.5)) %>% 
  mutate(alphavals2 = ifelse(yearRange %in% c(currentYearRange,preYearRange),1,.5)) %>% 
  mutate(snowYear = as.numeric(substr(yearRange,6,9))*1.0)

  
##### plot original cm #####
p <- ggplot(snowfalldata,aes(x=datesinceOct1,y=(cum_sum/10))) 
p <- p + geom_line(aes(group = as.factor(yearRange),col = impYears2),lwd=snowfalldata$sizevals,alpha=snowfalldata$alphavals2) + geom_smooth(aes(col="Average"))
p <- p + xlab("Month") + ylab("Cummulative snowfall (cm)") 
p <- p + scale_x_date(date_labels = "%b",limits = c(as.Date("2021-09-30"),as.Date("2022-05-30")),breaks = "1 months")
p <- p + scale_color_manual(values=c("black","red","blue","grey"))
p <- p + guides(lty = "none",col=guide_legend(""))
p <- p + theme_pubr(base_size = 20)
p <- p + annotate("text",label="Data from NOAA: Boston Logan",x=as.Date("2021-11-10"),y=280)
print(p)
ggsave("cummulativeSnowCM_2_24_25.png",dpi=600)

##### plot original in #####
p <- ggplot(snowfalldata,aes(x=datesinceOct1,y=(cum_sum/10)*0.393701)) 
p <- p + geom_line(aes(group = as.factor(yearRange),col = impYears2),lwd=snowfalldata$sizevals,alpha=snowfalldata$alphavals2)
p <- p + geom_smooth(aes(col="Average"))
p <- p + xlab("Month") + ylab("Cummulative snowfall (in.)") 
p <- p + scale_x_date(date_labels = "%b",limits = c(as.Date("2021-09-30"),as.Date("2022-05-30")),breaks = "1 months")
p <- p + scale_color_manual(values=c("black","red","blue","grey"))
p <- p + guides(lty = "none",col=guide_legend(""))
p <- p + theme_pubr(base_size = 20) + ylim(c(0,115))
p <- p + annotate("text",label="Data from NOAA: Boston Logan",x=as.Date("2021-11-10"),y=110)
print(p)
ggsave("cummulativeSnowIN_2_24_25.png",dpi=600)

p1 <- snowfalldata %>% group_by(snowYear,impYears2) %>% 
  summarise(maxsnow = (max(cum_sum)/10)*0.393701) %>% 
  mutate(imageval = "/Users/mhamins/Library/CloudStorage/GoogleDrive-mghaminspuertola@smcm.edu/My Drive/Fun/snowflakes/redsnowflake.png") %>% 
  ggplot(snowfalldata[1,],mapping=aes(x=snowYear,y=maxsnow,col=impYears2)) + geom_image(aes(image=imageval),size=.1) + 
  xlab("Year") + ylab("End of season cummulative snowfall (in.)") + 
  coord_cartesian(xlim = c(1934,2025),ylim= c(0,115)) +
  theme_pubr(base_size = 20) +
  geom_smooth(method = "lm") +
  guides(col=guide_legend("")) + 
  scale_color_manual(values=c("black","red","grey"))

print(p + p1)
ggsave("cummulativeSnowIN_twopanels_2_24_25.png",dpi=600,width = 18,height=10)


snowfalldata <- snowfalldata %>%mutate(decade = paste0(substr(yearRange,1,3),"0"))

p <- ggplot(snowfalldata,aes(x=datesinceOct1,y=(cum_sum/10)*0.393701)) 
p <- p + geom_smooth(aes(group = decade,col=decade))
p <- p + xlab("Month") + ylab("Cummulative snowfall (in.)") 
p <- p + scale_x_date(date_labels = "%b",limits = c(as.Date("2021-09-30"),as.Date("2022-05-30")),breaks = "1 months")
p <- p + scale_color_viridis_d()
p <- p + guides(col=guide_legend("Decade"))
p <- p + theme_pubr(base_size = 20)
p <- p + annotate("text",label="Data from NOAA: Boston Logan",x=as.Date("2021-11-10"),y=110)
p <- p + coord_cartesian(ylim=c(0,65))
print(p)
ggsave("cummulativeSnowIN_decade_2_24_25.png",dpi=600)


##### new plot cm #####
snowfalldata <- snowfalldata %>%mutate(snowYear = as.numeric(substr(yearRange,6,9))*1.0)

p <- ggplot(snowfalldata,aes(x=datesinceOct1,y=(cum_sum/10))) 
p <- p + geom_line(aes(group = as.factor(yearRange),col = snowYear),lwd=snowfalldata$sizevals) + geom_smooth(aes(col="Average"))
p <- p + xlab("Month") + ylab("Cummulative snowfall (cm)") 
p <- p + scale_x_date(date_labels = "%b",limits = c(as.Date("2021-09-30"),as.Date("2022-05-30")),breaks = "1 months")
#p <- p + scale_color_manual(values=c("green","red","blue","black"))
p <- p + guides(lty = "none",col=guide_legend(""))
p <- p + theme_pubr(base_size = 20)
p <- p + annotate("text",label="Data from NOAA: Boston Logan",x=as.Date("2021-11-10"),y=280)
p <- p + scale_color_viridis_c("A")
print(p)
ggsave("cummulativeSnowCM_2_24_25.png",dpi=600)

##### new plot in #####
p <- ggplot(snowfalldata,aes(x=datesinceOct1,y=(cum_sum/10)*0.393701)) 
p <- p + geom_line(aes(group = as.factor(yearRange),col = impYears,lty=as.factor(iscurrentyear)),lwd=snowfalldata$sizevals) + geom_smooth(aes(col="Average"))
p <- p + xlab("Month") + ylab("Cummulative snowfall (in.)") 
p <- p + scale_x_date(date_labels = "%b",limits = c(as.Date("2021-09-30"),as.Date("2022-05-30")),breaks = "1 months")
p <- p + scale_color_manual(values=c("green","red","blue","black"))
p <- p + guides(lty = "none",col=guide_legend(""))
p <- p + theme_pubr(base_size = 20)
p <- p + annotate("text",label="Data from NOAA: Boston Logan",x=as.Date("2021-11-10"),y=110)
print(p)
ggsave("cummulativeSnowIN_2_24_25.png",dpi=600)

snowfalldata <- snowfalldata %>%mutate(decade = paste0(substr(yearRange,1,3),"0"))

