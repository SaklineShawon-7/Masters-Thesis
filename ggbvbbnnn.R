nconc = read.csv("../analysis_15min/N_GGL_15min.csv")

mydata2 <- read_csv("Data/mydata2.csv")

mydata2$Date<-as.Date(mydata2$Time)

pltn = ggplot(mydata2) + 
  geom_line(aes(Date,No3,color=No3)) +
  scale_x_date(date_breaks = "6 month",expand = c(0,0),date_labels = "%Y/%m",
               limits = c(as.Date("2020-01-01"),as.Date("2023-10-14"))) +
  scale_y_continuous(name=expression("NO"[3]^"-"*"-N ["*mg~l^-1*"]"),
                     expand=c(0,0)) +
  scale_color_continuous() +
  theme_bw() +
  theme() +
  annotate("text",x= as.Date("2020-01-01"),y=4.5,fontface="plain",
           label="(b)",color= "black")

pltn


pltwasp = plot_grid(pltq,pltn, ncol = 1,align = "hv")
ggsave("./FigS1_WASPsimulation.jpg",pltwasp,width = 13,height = 6,dpi = 300)
