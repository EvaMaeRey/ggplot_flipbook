
library(readxl)
library(ggplot2)
library(tidyverse)
library(stringr)
library(lubridate)


library(vcd)
mosaic(HairEyeColor, shade=TRUE, legend=TRUE)

data0=read_xlsx("MyersBriggsTypes.xlsx")

data=data0 %>% 
  rename(EI=`(E)xtroversion/(I)ntroversion`,
         SI=`(S)ensing/I(N)tuition`,
         TF=`(T)hinking/(F)eeling`,
         JP=`(J)udging/(P)erceiving`
  )  %>% 
  mutate(`Perthousand in Population`=1000*`Ratio in Population`)

`(E)xtroversion/(I)ntroversion`=rep(data$EI, data$`Perthousand in Population`)
`(S)ensing/I(N)tuition`=rep(data$SI, data$`Perthousand in Population`)
`(T)hinking/(F)eeling`=rep(data$TF, data$`Perthousand in Population`)
`(J)udging/(P)erceiving`=rep(data$JP, data$`Perthousand in Population`)

table(`(E)xtroversion/(I)ntroversion`)
table(`(S)ensing/I(N)tuition`)
table(`(T)hinking/(F)eeling`)
table(`(J)udging/(P)erceiving`)
4*3*2*1


A2=table(`(S)ensing/I(N)tuition`,`(T)hinking/(F)eeling`)
A3=table(`(S)ensing/I(N)tuition`,`(J)udging/(P)erceiving`)
A4=table(`(S)ensing/I(N)tuition`,`(E)xtroversion/(I)ntroversion`)

B3=table(`(T)hinking/(F)eeling`,`(J)udging/(P)erceiving`)
B4=table(`(T)hinking/(F)eeling`,`(E)xtroversion/(I)ntroversion`)

C4=table(`(J)udging/(P)erceiving`,`(E)xtroversion/(I)ntroversion`)

par(mfrow=c(4,4))

p1=plot(1,2,pch="", axes=F,xlab="",ylab="")
p2=mosaic(A2)
p3=mosaic(A3)
p2=mosaic(A4)
p2=plot(1,2,pch="", axes=F,xlab="",ylab="")
p2=plot(1,2,pch="", axes=F,xlab="",ylab="")
p2=mosaic(B3)
p2=mosaic(B4)
plot(1,2,pch="", axes=F,xlab="",ylab="")
plot(1,2,pch="", axes=F,xlab="",ylab="")
plot(1,2,pch="", axes=F,xlab="",ylab="")
mosaic(C4)

par(mfrow=c(1,1))
library(gridGraphics)
library(gridExtra)
grid.newpage()
grid.arrange(p1,p2, ncol=2)

t=table(`(S)ensing/I(N)tuition`,`(T)hinking/(F)eeling`,`(J)udging/(P)erceiving`,
        `(E)xtroversion/(I)ntroversion`)


mosaic(table(`(E)xtroversion/(I)ntroversion`), shade=F, legend=TRUE,main="Myers-Briggs Frequencies", sub="Data from the Myers-Briggs Foundation")


mosaic(t, shade=F, legend=TRUE,main="Myers-Briggs Frequencies", sub="@evamaerey | Source: http://www.myersbriggs.org/")

mosaic(t, shade=TRUE, legend=TRUE)


col=c("paleturquoise3", "palegreen3")

mosaic(t, highlighting = "(E)xtroversion/(I)ntroversion", highlighting_fill = col)
mosaic(t, highlighting = "(S)ensing/I(N)tuition", highlighting_fill = col)
mosaic(t, highlighting = "(T)hinking/(F)eeling", highlighting_fill = col)
mosaic(t, highlighting = "(J)udging/(P)erceiving", highlighting_fill = col)


# mosaic(xtabs(~Variant+Region + PrecededByPrep   +  Time, data=ttt) 
# ,highlighting="Variant", highlighting_fill=col)

d=data_frame(`(E)xtroversion/(I)ntroversion`,
             `(S)ensing/I(N)tuition`,
             `(T)hinking/(F)eeling`,
             `(J)udging/(P)erceiving`, count=1)

d=d %>% 
  mutate(`(E)xtroversion/(I)ntroversion`=recode(`(E)xtroversion/(I)ntroversion`, E="Extroversion", I="Introversion"),
         `(S)ensing/I(N)tuition`=recode(`(S)ensing/I(N)tuition`, S="Sensing", N="Intuition"),
         `(T)hinking/(F)eeling`=recode(`(T)hinking/(F)eeling`, T="Thinking", F="Feeling"),
         `(J)udging/(P)erceiving`=recode(`(J)udging/(P)erceiving`, J="Judging", P="Perceiving"))

background=data_frame(mins=c(.5,1.5),max=c(1.5,2.5),cats=c(" Judging"," Perceiving"))

ggplot(d, aes(`(J)udging/(P)erceiving`,fill=`(T)hinking/(F)eeling`))+facet_grid(`(E)xtroversion/(I)ntroversion`~`(S)ensing/I(N)tuition`)+geom_rect(aes(NULL, NULL, xmin = mins, xmax = max, fill = cats),
                                                                                                                                                   ymin = 0, ymax = 700, data = background) +
  geom_bar(position = "dodge") + scale_fill_manual(values = alpha(c("lightgrey", "darkgrey", "blue","violet"), c(.3,.3 , .6,.6)))+theme_bw()+ggtitle("Myers-Briggs Type Frequency", sub="Expected among 1000 individuals | @evamaerey | Source: http://www.myersbriggs.org/")+xlab("")+ylab("")+ guides(fill=guide_legend(title=""))+
  theme(plot.title=element_text(family="Times", face="bold", size=20)) 

# ggplot(d, aes(`(J)udging/(P)erceiving`,fill=`(T)hinking/(F)eeling`))+facet_grid(`(E)xtroversion/(I)ntroversion`~`(S)ensing/I(N)tuition`)+geom_rect(aes(NULL, NULL, xmin = 1.5, xmax = 2.5, fill = party),
# ymin = 0, ymax = 700, data = presidential) +geom_bar(position = "dodge")+ scale_fill_manual(values = alpha(c("blue", "red"), .3))

# 
# b <- ggplot(economics, aes(x = date, y = unemploy))
# yrng <- range(economics$unemploy)
# j <- b + geom_line()
# j <- j + geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party),
# ymin = yrng[1], ymax = yrng[2], data = presidential)
# 
# 
# 
# last_plot()+geom_rect(aes(NULL, NULL, xmin = start, xmax = end, fill = party),
# ymin = yrng[1], ymax = yrng[2], data = presidential)+ scale_fill_manual(values = alpha(c("blue", "red"), .3))



