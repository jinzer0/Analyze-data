import tkinter as tk
import tkinter.font as tkfont
import tkinter.filedialog as tkfile
import os as os



class BigAnal(tk.Tk):

    def __init__(self, *args, **kwargs):
        tk.Tk.__init__(self, *args, **kwargs)

        self.geometry("1000x800+400+120")
        self.resizable(True, True)
        self.title("BigData Analyze")

        title = tkfont.Font(family="맑은 고딕", size=40, weight="bold")
        explain = tkfont.Font(family="맑은 고딕", size=20)
        buttonfont = tkfont.Font(family="맑은 고딕",)
        textfont = tkfont.Font(family="맑은 고딕", size=17, weight="bold")

        self.biganalmain = tk.Frame().pack(side="top", pady=15)

        self.fileload = tk.Frame()
        self.fileload.pack(side="top")

        self.filename = tk.StringVar()
        self.filename.set("불러온 파일 : ")

        def Fileload():
            self.filedir = tkfile.askopenfilename(
                initialdir="/", title="파일 불러오기", filetypes=(("EXCEL", "*.xlsx"), ("All Files", "*.*")))
            self.filename.set("불러온 파일 : "+self.filedir)
            print(self.filedir)
            newr = open("/Users/kjy/Desktop/testing/test.R", "+w")
            newr.write(f"""
install.packages("readxl", repos="https://cran.seoul.go.kr/")
library("readxl")
install.packages("dplyr", repos="https://cran.seoul.go.kr/")
library("dplyr")
install.packages("ggplot2", repos="https://cran.seoul.go.kr/")
library("ggplot2")

economic <- read_excel({self.filedir})

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
combining3 <- combining2 %>% mutate(month=c("02Feb", "03Mar", "04Apr", "05May", "06Jun", "07Jul", "08Aug"))

workersee <- ggplot(data = combining3, aes(x=month, y=workercount))+geom_col()+scale_y_continuous(name="Percentage of emplyment")
ggsave("/Users/kjy/Desktop/BigAnal/1.png", width=9, height=9, unit="cm")

kospisee <- ggplot(data = combining3, aes(x=month, y=kospip))+geom_col()+scale_y_continuous(name="KOSPI(%p)")
ggsave("/Users/kjy/Desktop/BigAnal/images/2.png", width=9, height=9, unit="cm")

infectedsee <- ggplot(data = combining3)+geom_line(aes(x=month, y=infected,group=1))
infectedsee <- infectedsee+geom_line(aes(x=month, y=death*10,group=1,colour="red"))
infectedsee <- infectedsee+scale_y_continuous(breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000),sec.axis = sec_axis(~./10, name = "died"))
infectedsee <- infectedsee+theme(legend.position = "none")
infectedsee <- infectedsee+ggtitle("infected and died by COVID-19 cases")
ggsave("/Users/kjy/Desktop/BigAnal/images/3.png", width=9, height=9, unit="cm")

comparetwolittle <- data.frame(index=c("econmy","profit","fund state"),BSI=c(65,65,66),twenty=c(65,66,65))
comparelittlesee <- ggplot(data = comparetwolittle)+geom_line(aes(x=index,y=BSI,group=1,colour="blue"))
comparelittlesee <- comparelittlesee+geom_line(aes(x=index,y=twenty,group=1))
comparelittlesee <- comparelittlesee+scale_y_continuous(name = "2019, 2020(red)", sec.axis = sec_axis(~./1, name = "2019, 2020(red)"))+ylim(62,68)
comparelittlesee <- comparelittlesee+theme(legend.position = "none")
comparelittlesee <- comparelittlesee+ggtitle("Net profit and prospection of small business in 2019, 2020(average)")
ggsave("/Users/kjy/Desktop/BigAnal/images/4.png", width=9, height=9, unit="cm")

littlecompanytwen <- littlecompany %>% tail(7)
traditionalmarkettwen <- traditionalmarket %>% tail(7)

combining2 <- data.frame(combining2, littlecompanytwen %>% select(-date), traditionalmarkettwen %>% select(-date))
combining2 <- combining2 %>% mutate(month=c("02Feb", "03Mar", "04Apr", "05May", "06Jun", "07Jul", "08Aug"))
combining3 <- combining2

seewithlittle <- ggplot(data = combining3)
seewithlittle <- seewithlittle+geom_line(aes(x=month,y=infected,group=1))+geom_line(aes(x=month, y=economy*60,group=1,colour="red"))
seewithlittle <- seewithlittle+geom_line(aes(x=month,y=fund.state*60,group=1,colour="blue"))
seewithlittle <- seewithlittle+scale_y_continuous(name = "infected(black)", breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000), sec.axis = sec_axis(~./60, name = "economy(blue, red)", breaks = c(0,20,40,60,80,100)))
seewithlittle <- seewithlittle+theme(legend.position = "none")
seewithlittle <- seewithlittle+ggtitle("Psychological feeling of economy with infected (Small business)")
ggsave("/Users/kjy/Desktop/BigAnal/images/5.png", width=9, height=9, unit="cm")

seewithtraditional <- ggplot(data = combining3)+geom_line(aes(x=month, y=infected, group=1))
seewithtraditional <- seewithtraditional+geom_line(aes(x=month, y=economy.1*60, group=1, colour="blue"))
seewithtraditional <- seewithtraditional+geom_line(aes(x=month, y=fund.state.1*60, group=1, colour="red"))
seewithtraditional <- seewithtraditional+scale_y_continuous(name = "infected(black)", breaks = c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000,5500,6000), sec.axis = sec_axis(~./60, name = "economy(blue, red)", breaks = c(0,20,40,60,80,100)))
seewithtraditional <- seewithtraditional+theme(legend.position = "none")
seewithtraditional <- seewithtraditional+ggtitle("Psychological feeling of economy with infected (Traditional market)")
ggsave("/Users/kjy/Desktop/BigAnal/images/6.png", width=9, height=9, unit="cm")
""")

            newr.close()


        def Fileanalyze():
            os.system(
                "/Library/Frameworks/R.framework/Versions/4.0/Resources/bin/Rscript /Users/kjy/Desktop/testing/test.R")

            def Newwinodw():
                window = tk.Tk()
                window.geometry("1000x800+200+180")
                window.resizable(True, True)
                window.title("Analyze Result")

                img = tk.PhotoImage(file="1.png")
                imglabel = tk.Label(window, image=img)
                imglabel.grid(row=1, column=1)
                window.mainloop()
            SecondMain = Newwinodw()


        biganaltitle = tk.Label(
            self.fileload, text="경제 지표 분석", font=title).pack(side="top", pady=50)

        filenametext = tk.Label(
            self.fileload, textvariable=self.filename, font=textfont)
        filenametext.pack(side="bottom")
        fileloadbutton = tk.Button(
            self.fileload, text="불러오기", font=buttonfont, command=Fileload)
        fileloadbutton.pack(side="bottom", ipadx=40, pady=10)
        fileloadtext = tk.Label(
            self.fileload, text="분석할 파일을 선택하세요", font=explain)
        fileloadtext.pack(side="bottom")

        self.analyze = tk.Frame()
        self.analyze.pack(side="bottom", ipady=150)
        analyzebutton = tk.Button(
            self.analyze, text="분석하기", font=buttonfont, command=Fileanalyze)
        analyzebutton.pack(ipadx=40)

        startanal = tk.Frame(self)
        startanal.pack(side="top")


Mainapp = BigAnal()
Mainapp.mainloop()
