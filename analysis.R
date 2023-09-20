library(tidyverse)
library(ggplot2)
library(lubridate)

activities <-read.csv('../fitbase_dataset/dailyActivity_merged.csv')
View(activities)

calories <- read.csv('../fitbase_dataset/dailyCalories_merged.csv')
View(calories)

intensities <- read.csv('../fitbase_dataset/dailyIntensities_merged.csv')
View(intensities)

steps <- read.csv('../fitbase_dataset/dailySteps_merged.csv')
View(steps)

heart_rate <- read.csv('../fitbase_dataset/heartrate_seconds_merged.csv')
View(heart_rate)

sleep <- read.csv('../fitbase_dataset/sleepDay_merged.csv')
View(sleep)

weight <- read.csv('../fitbase_dataset/weightLogInfo_merged.csv')
View(weight)

summary(weight)
is.na.data.frame(weight)
n_distinct(weight$Id)

summary(heart_rate)
is.na.data.frame(heart_rate)
n_distinct(heart_rate$Id)

summary(steps)
is.na.data.frame(steps)
n_distinct(steps$Id)

summary(intensities)
is.na.data.frame(intensities)
n_distinct(intensities$Id)

summary(calories)
is.na.data.frame(calories)
n_distinct(calories$Id)

summary(activities)
is.na.data.frame(activities)
n_distinct(activities$Id)

summary(sleep)
is.na.data.frame(sleep)
n_distinct(sleep$Id)

merge<-merge.data.frame(activities,sleep, by='Id')
View(merge)
summary(merge)

activities_mean<- activities%>%
  select(Id,VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes,SedentaryMinutes,Calories)%>%
  group_by(Id)%>%
  drop_na()%>%
  summarize(mean_VeryActiveMinutes = mean(VeryActiveMinutes),mean_FairlyActiveMinutes = mean(FairlyActiveMinutes),mean_LightlyActiveMinutes = mean(LightlyActiveMinutes),mean_SedentaryMinutes = mean(SedentaryMinutes), mean_Calories=mean(Calories))
View(activities_mean)

sleep_mean <- sleep%>%
  select(Id,TotalMinutesAsleep,TotalTimeInBed)%>%
  group_by(Id)%>%
  drop_na()%>%
  summarize(mean_TotalMinutesAsleep = mean(TotalMinutesAsleep), mean_TotalTimeInBed=mean(TotalTimeInBed))
View(sleep_mean)
 
merge_means<-merge.data.frame(activities_mean ,sleep_mean, by='Id')
View(merge_means)

ggplot(data=merge)+
  geom_smooth(mapping = aes(x=TotalMinutesAsleep, y=(LightlyActiveMinutes + FairlyActiveMinutes+ VeryActiveMinutes)))+
  labs(title = 'Tempo de sono x Tempo atividade total realizada')+
  scale_x_continuous(name='Tempo (minutos)',breaks=seq(0,900,100)) +
  scale_y_continuous(name="Tempo total de atividade registrado")+
  theme_light()
#quem dormiu >550min fez menos tempo de atividade física

ggplot(data=merge, aes(x=SedentaryMinutes,y=Calories))+
 geom_point() +
  geom_smooth()+
  scale_x_continuous(name='Tempo sedentario (minutos)') +
  scale_y_continuous(name='Calorias perdidas')+
  theme_light()+
  labs(title = 'Tempo sedentario x Gasto calorico')

ggplot(data=merge)+
  geom_point(mapping = aes(x=TotalSteps, y=Calories))+
  labs(title = 'Total de Passos x Calorias')+
  theme_light()

labels <- c('Intensa','Moderada','Leve')

ggplot(data=merge_means)+
  geom_density(mapping = aes(mean_VeryActiveMinutes,color='red'))+
  geom_density(mapping = aes(mean_FairlyActiveMinutes, color='yellow'))+
  geom_density(mapping = aes(mean_LightlyActiveMinutes, color='green'))+
  scale_color_discrete(name='Atividade média realizada:', labels=labels)+
  scale_x_continuous(name='Tempo (minutos)',breaks=seq(0,500,50)) +
  scale_y_continuous(name=" ")+
  theme_light()+
  theme(legend.position = 'bottom')

ggplot(data = merge)+
  geom_histogram(mapping = aes(SedentaryMinutes), bins=20, binwidth = 100, fill='lightblue')+
  scale_x_continuous(name='Tempo (minutos)',breaks=seq(0,1500,100)) +
  scale_y_continuous(name=" ")+
  theme_light()
 
ggplot(data=merge)+
   geom_jitter(mapping = aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+
   geom_smooth(mapping = aes(x=TotalMinutesAsleep, y=TotalTimeInBed))+
   scale_x_continuous(name='Tempo dormindo(minutos)',breaks=seq(0,900,100)) +
   scale_y_continuous(name="Tempo deitado (minutos)",breaks=seq(0,1500,100))+
   theme_light()

cor.test(merge$TotalMinutesAsleep,merge$TotalTimeInBed)

max(merge$TotalMinutesAsleep)
min(merge$TotalMinutesAsleep)
max(merge$TotalTimeInBed)
min(merge$TotalTimeInBed)
mean(merge$TotalMinutesAsleep)
mean(merge$TotalTimeInBed)

cor.test(merge$SedentaryMinutes, merge$Calories)

cor.test(merge$TotalSteps, merge$Calories)

cor.test(merge$TotalSteps, merge$SedentaryMinutes)

ggplot(data=merge)+
  geom_jitter(mapping = aes(x=TotalSteps, y=SedentaryMinutes))+
  geom_smooth(mapping = aes(x=TotalSteps, y=SedentaryMinutes))+
  labs(title='Total de Passos x Tempo parado', x='Total de passos',y="Tempo parado")
  theme_light()

facet_labels <-c('Tempo parado menor que 700min', 'Tempo parado maior que 700min')
names(facet_labels)<-c('FALSE','TRUE')

ggplot(data=merge)+
  geom_histogram(mapping = aes(TotalSteps, fill=Calories>2000), bins=10)+
  facet_wrap(~SedentaryMinutes >=700, labeller = as_labeller(facet_labels))+
  scale_fill_discrete(name='Perda calorica:', labels=c("Menor que 2000",'Maior que 2000'))+
  scale_x_continuous(name='Passos') +
  scale_y_continuous(name="")+
  theme(legend.position = 'bottom')+
  theme_light()

cor.test(merge$SedentaryMinutes, merge$TotalMinutesAsleep)

ggplot(data=merge_means)+
  geom_point(mapping = aes(x=mean_SedentaryMinutes,y=mean_TotalMinutesAsleep,)) +
  geom_smooth(mapping = aes(x=mean_SedentaryMinutes,y=mean_TotalMinutesAsleep))+
  theme_light()+
  labs(title = 'Tempo sedentário x Tempo de sono',
       x='Sedentarismo (minutos)',y='Sono (minutos)')

colors<-c('Intensa'="#CC79A7",'Moderada'="#D55E00", 'Leve'="#0072B2")

ggplot(data=merge_means, aes(y=mean_Calories))+
  geom_point(mapping = aes(x=mean_VeryActiveMinutes,color='Intensa'),size =2 )+
  geom_point(mapping = aes(x=mean_FairlyActiveMinutes,color='Moderada'), size =2)+
  geom_point(mapping = aes(x=mean_LightlyActiveMinutes,color='Leve'), size =2)+
  labs(title = 'Média de calorias perdidas com relação as atividade realizadas', 
       x='Atividade realizada (minutos)',
       y="Média de calorias perdidas")+
  scale_color_manual(name='Atividade realizada:',values=colors)+
  theme_light()


