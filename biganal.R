install.packages("readxl")
library("readxl")
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")

economic <- read_excel("/Users/kjy/Downloads/ecoindex.xlsx")

simpleeconomic <- economic %>% select(-futureindex2015100,-futureindexformermonth,-presentindexformermonth,
                                      -formerindexformermonth)


write.csv(simpleeconomic, file = "simple.csv")
monthlypeople <- read_excel("~/Downloads/monthchina.xlsx")
dailypeople <- read_excel("~/Downloads/what.xlsx")
dailypeople <- dailypeople %>% filter(date!="시점")
realeco <- read.csv("~/Desktop/BigAnal/simple.csv")

realeco <- realeco %>% select(date|inventorycirculationp|kospip|presentindex2015100|serviceproducep|
                                retailsoldp|formerindex2015100|workercount)

traditionalmarket <- read_excel("~/Downloads/traditionalmarket.xlsx")
littlecompany <- read_excel("~/Downloads/littlecompany.xlsx")


dailypeople <- data.frame(dailypeople %>% mutate(total=infected+death))
monthlypeople <- data.frame(monthlypeople %>% mutate(total=infected+death))

data_a <- data.frame(simpleeconomic %>% filter(date=="2020. 01"|date=="2020. 02"|date=="2020. 03"|date=="2020. 04"
                                               |date=="2020. 05"|date=="2020. 06"|date=="2020. 07"|date=="2020. 08"
                                               |date=="2020. 09"|date=="2020. 10"))
data_b <- data.frame(monthlypeople %>% filter(month!="2020-09-01"&month!="2020-10-01") %>% select(infected,death))
data_b <- data_b[-c(9,10),]

combining <- data.frame(data_a,data_b)
combining2 <- combining %>% filter(!is.na(death))

workersee <- ggplot(data = combining2, aes(x=date, y=workercount))+geom_col()+scale_y_continuous(name="Percentage of emplyment")
ggsave("/Users/kjy/Desktop/BigAnal/images/1.png")

kospisee <- ggplot(data = combining2, aes(x=date, y=kospip))+geom_col()+scale_y_continuous(name="KOSPI(%p)")
ggsave("/Users/kjy/Desktop/BigAnal/images/2.png")

infectedsee <- ggplot(data = combining2)+geom_line(aes(x=date, y=infected,group=1))
infectedsee <- infectedsee+geom_line(aes(x=date, y=death*10,group=1,colour="red"))
infectedsee <- infectedsee+scale_y_continuous(breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000),sec.axis = sec_axis(~./10, name = "died"))
infectedsee <- infectedsee+theme(legend.position = "none")
infectedsee <- infectedsee+ggtitle("infected and died by COVID-19 cases")
ggsave("/Users/kjy/Desktop/BigAnal/images/3.png")

comparetwolittle <- data.frame(index=c("econmy","profit","fund state"),BSI=c(65,65,66),twenty=c(65,66,65))
comparelittlesee <- ggplot(data = comparetwolittle)+geom_line(aes(x=index,y=BSI,group=1,colour="blue"))
comparelittlesee <- comparelittlesee+geom_line(aes(x=index,y=twenty,group=1))
comparelittlesee <- comparelittlesee+scale_y_continuous(name = "2019, 2020(red)", sec.axis = sec_axis(~./1, name = "2019, 2020(red)"))+ylim(62,68)
comparelittlesee <- comparelittlesee+theme(legend.position = "none")
comparelittlesee <- comparelittlesee+ggtitle("Net profit and prospection of small business in 2019, 2020(average)")
ggsave("/Users/kjy/Desktop/BigAnal/images/4.png")

littlecompanytwen <- littlecompany %>% tail(7)
traditionalmarkettwen <- traditionalmarket %>% tail(7)

combining2 <- data.frame(combining2, littlecompanytwen %>% select(-date), traditionalmarkettwen %>% select(-date))

seewithlittle <- ggplot(data = combining2)
seewithlittle <- seewithlittle+geom_line(aes(x=date,y=infected,group=1))+geom_line(aes(x=date, y=economy*60,group=1,colour="red"))
seewithlittle <- seewithlittle+geom_line(aes(x=date,y=fund.state*60,group=1,colour="blue"))
seewithlittle <- seewithlittle+scale_y_continuous(name = "infected(black)", breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000), sec.axis = sec_axis(~./60, name = "economy(blue, red)", breaks = c(0,20,40,60,80,100)))
seewithlittle <- seewithlittle+theme(legend.position = "none")
seewithlittle <- seewithlittle+ggtitle("Psychological feeling of economy with infected (Small business)")
ggsave("/Users/kjy/Desktop/BigAnal/images/5.png")

seewithtraditional <- ggplot(data = combining2)+geom_line(aes(x=date, y=infected, group=1))
seewithtraditional <- seewithtraditional+geom_line(aes(x=date, y=economy.1*60, group=1, colour="blue"))
seewithtraditional <- seewithtraditional+geom_line(aes(x=date, y=fund.state.1*60, group=1, colour="red"))
seewithtraditional <- seewithtraditional+scale_y_continuous(name = "infected(black)", breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000), sec.axis = sec_axis(~./60, name = "economy(blue, red)", breaks = c(0,20,40,60,80,100)))
seewithtraditional <- seewithtraditional+theme(legend.position = "none")
seewithtraditional <- seewithtraditional+ggtitle("Psychological feeling of economy with infected (Traditional market)")
ggsave("/Users/kjy/Desktop/BigAnal/images/6.png")