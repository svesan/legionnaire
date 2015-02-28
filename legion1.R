library(dplyr)
jul=read.csv("/home/svesan/ki/slask/julien/julien.csv")
summary(jul)
jul$cmonth=factor(jul$month)
jul$cYear=factor(jul$Year)
jul=na.omit(jul)

f1=glm(Cases_NUTS2 ~ NUTS2 + cYear + MeanTemp_NUTS2, offset=log(Pop_NUTS2), family=poisson(), data=jul)
f2=update(f1, ~ . + cmonth)
f3=update(f2, ~ . + cmonth*MeanTemp_NUTS2)

f4=glm(Cases_NUTS2 ~ NUTS2 + cYear + MeanTemp_NUTS2_1, offset=log(Pop_NUTS2), family=poisson(), data=jul)
f5=glm(Cases_NUTS2 ~ NUTS2 + cYear + MeanTemp_NUTS2_1 + cmonth + cmonth*MeanTemp_NUTS2_1, offset=log(Pop_NUTS2), family=poisson(), data=jul)


summary(f1)
summary(f2)
summary(f3)

summary(f4$fitted.values)
chk=jul[f4$fitted.values<-10,]

jul$fit=f4$fitted.values
jul$pred=predict(f4)

chk=jul[jul$pred< -10,]

head(chk)

#summary by NUTS2 region
nutsg=group_by(jul, NUTS2)
a=summarise(nutsg, tot_cases=sum(Cases_NUTS2))
plot(a)

a[a$tot_cases > 250,]

quantile(a$tot_cases, probs = seq(0, 1, by=.01))
plot(quantile(a$tot_cases, probs = seq(0, 1, by=.01)), seq(0, 1, by=.01))

xjul=jul[jul$pred> -10,]
g1=update(f1, data=xjul)
g2=update(f2, data=xjul)
g3=update(f3, data=xjul)
g4=update(f4, data=xjul)

h4=glm.nb(Cases_NUTS2 ~ NUTS2 + cYear + MeanTemp_NUTS2_1, offset=log(Pop_NUTS2), data=xjul)
