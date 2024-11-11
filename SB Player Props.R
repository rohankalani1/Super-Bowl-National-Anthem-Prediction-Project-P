library(tidyverse)
library(ggfortify)
library(ggplot2)
library(car)

lmSB = lm(Cmp ~Cmp.Allowed + Bltz. + Defensive.DVOA + Prss. + Rate.1 + PA + Hrry., data = Rsportsref_download)
summary(lmSB)
lmSBsimplified = lm(Cmp ~ Bltz. + Defensive.DVOA + Prss. + Hrry., data = Rsportsref_download)
summary(lmSBsimplified)


summary(lmSB)
rsquared(lmSB)

predict(lmSB, list(Cmp.Allowed = 61.2, Bltz. = .329,Defensive.DVOA = -.05, Prss. = .229, Rate.1 = 83.6, PA = 33.55, Hrry. = 0.098), interval = "prediction")



ggplot(Rsportsref_download) + 
  geom_point(aes(x= Bltz., y = Cmp))

ggplot(Rsportsref_download) +
  geom_point(aes(x=PY, y = Yds))

bootSB = do(1000)*mean(~Yds, data = resample(Rsportsref_download))

ggplot(bootSB) +
  geom_histogram(aes(x=mean), color = "white")
confint(bootSB, level = 0.95)

#-------------------------------------------------------------------------------

lmCMC = lm(Rec ~ + Defensive.DVOA + Bltz. + Prss. + YC, data= CMCSB)


summary(lmCMC)

predict(lmCMC, list( PY = 181.5, Defensive.DVOA = -.05, YP = 8.94, Y.PL = 4.76), interval = "prediction")


CMCSB %>% 
  count(Yds.1 > mean(Yds.1))
bootCMCSB = do(10000)*mean(~Rec, data= resample(CMCSB))

ggplot(bootCMCSB)+
  geom_histogram(aes(x=mean))

ggplot(CMCSB) +
  geom_point(aes(x= Defensive.DVOA, y= Yds))

confint(bootCMCSB, level = 0.95)

#----------------------------------------------------------------------------
lmAnthem = lm(Length ~ Age+Gender+Type, data = National.Anthem)
summary(lmAnthem)
autoplot(lmAnthem)
predict(lmAnthem, list(Age=68, Gender = 'F', Type = 'Country'), interval= 'prediction')

ggplot(National.Anthem)+
  geom_boxplot(aes(x=Type, y= Length))

National.Anthem %>% 
  filter(Type=='Country') %>% 
  summarize(IQR.Time = IQR(Length),
            median.Time = median(Length),
            min.Time = min(Length))

National.Anthem %>% 
  summarize(min.Time = min(Length))


ggplot(National.Anthem) +
  geom_point(aes(x=Age, y= Length)) +
  facet_wrap(~Type)

Oldsingers= National.Anthem %>% 
  filter(Age > 50)

ggplot(National.Anthem) +
  geom_boxplot(aes(y= Length))+
  facet_wrap(~Type)
