#####Packages and Data Loading#####
#Require Packages
##setwd('C:\\...') #Add working directory here if saving images



packs<-c('RJSONIO','dplyr','reshape2','tidyr','ggplot2','psych','stringr','jsonlite','gdata',
         'gdata','gganimate','ggrepel','grid','plotly','gapminder','transformr','gifski')

sapply(packs,require,character=T)


#Load JSON file from the source (this is a very large file, may take a few minutes)
a<-jsonlite::fromJSON("https://www.mtgjson.com/files/AllCards.json")

#####Data Cleaning and Processing#####
#Create a dummy data frame and load relevant attributes of each card
b<-as.data.frame(matrix(ncol=9,nrow=length(a)))
for(i in 1:length(a)){
  b[i,1]<-a[[i]]$name
  b[i,2]<-a[[i]]$convertedManaCost
  b[i,3]<-ifelse(any(a[[i]]$colors=="G"),1,0)
  b[i,4]<-ifelse(any(a[[i]]$colors=="R"),1,0)
  b[i,5]<-ifelse(any(a[[i]]$colors=="U"),1,0)
  b[i,6]<-ifelse(any(a[[i]]$colors=="W"),1,0)
  b[i,7]<-ifelse(any(a[[i]]$colors=="B"),1,0)
  b[i,8]<-a[[i]]$type
  b[i,9]<-paste(a[[i]]$printings[1])
}

#Rename columns
colnames(b)<-c("name",'cmc','g','r','u','w','b','type','printing')
b<-subset(b,!is.na(b))
b$printing<-as.character(b$printing)

#Record what year each card was made based on the earliest set made
for(i in 1:nrow(b)){
  d<-as.character(unlist(strsplit(b[i,9],split=" ")));
  if(any(d[1:length(d)]=="LEA"|d[1:length(d)]=="LEB"|d[1:length(d)]=='2ED'|d[1:length(d)]=='ARN')==T){
     b[i,9]<-paste("1993")
     next()
  }
  if(any(d[1:length(d)]=="3ED"|d[1:length(d)]=="ATQ"|d[1:length(d)]=='LEG'|d[1:length(d)]=='DRK'|d=='FEM')==T){
     b[i,9]<-paste('1994')
     next()
  }
  if(any(d[1:length(d)]=="4ED"|d=='CHR'|d[1:length(d)]=='HML'|d[1:length(d)]=='ICE'|d=='CHR')==T){
     b[i,9]<-paste('1995')
     next()
  }
  if(any(d[1:length(d)]=="ALL"|d[1:length(d)]=='MIR')==T){
     b[i,9]<-paste('1996')
     next()
  }
  if(any(d[1:length(d)]=="5ED"|d=='WC97'|d=='POR'|d[1:length(d)]=='VIS'|d[1:length(d)]=='WTH'|d[1:length(d)]=='TMP'|d=='POR')==T){
     b[i,9]<-paste('1997')
     next()
  }
  if(any(d[1:length(d)]=="USG"|d=='WC98'|d=='P02'|d=='STH'|d=='EXO'|d=='P02'|d=='UGL')){
     b[i,9]<-paste('1998')
     next()
  }
  if(any(d[1:length(d)]=="6ED"|d=='PTK'|d=='PAL99'|d=='P3K'|d=='S99'|d[1:length(d)]=='ULG'|d[1:length(d)]=='UDS'|d[1:length(d)]=='MMQ'|d=='S99'|d=='BRB')==T){
     b[i,9]<-paste('1999')
     next()
  }
  if(any(d[1:length(d)]=="NEM"|d[1:length(d)]=='PCY'|d[1:length(d)]=='INV'|d=='S00'|d=='BTD')==T){
     b[i,9]<-paste('2000')
     next()
  }
  if(any(d[1:length(d)]=="7ED"|d=='F01'|d=='PAL01'|d[1:length(d)]=='PLS'|d[1:length(d)]=='ODY'|d=='APC'|d=='DKM')==T){
     b[i,9]<-paste('2001')
     next()
  }
  if(any(d[1:length(d)]=='TOR'|d=='G02'|d=='F02'|d=='PAL02'|d[1:length(d)]=='JUD'|d[1:length(d)]=='ONS')==T){
     b[i,9]<-paste('2002')
     next()
  }
  if(any(d[1:length(d)]=="8ED"|d=='G03'|d=='F03'|d[1:length(d)]=='LGN'|d[1:length(d)]=='SCG'|d[1:length(d)]=='MRD')==T){
     b[i,9]<-paste('2003')
     next()
  }
  if(any(d[1:length(d)]=='DST'|d=='P04'|d=='G04'|d=='F04'|d=='PAL04'|d[1:length(d)]=='5DN'|d[1:length(d)]=='CHK'|d=='UNH')==T){
     b[i,9]<-paste('2004')
     next()
  }
  if(any(d[1:length(d)]=="9ED"|d=='PSAL'|d=='G05'|d=='PAL05'|d[1:length(d)]=="BOK"|d[1:length(d)]=='SOK'|d[1:length(d)]=='RAV')==T){
     b[i,9]<-paste('2005')
     next()
  }
  if(any(d[1:length(d)]=='GPT'|d=='G06'|d=='F06'|d[1:length(d)]=="DIS"|d[1:length(d)]=="CSP"|d[1:length(d)]=="TSP"|d[1:length(d)]=="TSB")==T){
     b[i,9]<-paste('2006')
     next()
  }
  if(any(d[1:length(d)]=="10E"|d=='G07'|d=='F07'|d[1:length(d)]=="PLC"|d[1:length(d)]=="FUT"|d[1:length(d)]=="LRW"|d=='MED'|d=='EVG')==T){
     b[i,9]<-paste('2007')
     next()
  }
  if(any(d[1:length(d)]=='MOR'|d=='P08'|d=='G08'|d=='F08'|d[1:length(d)]=='SHM'|d[1:length(d)]=='EVE'|d[1:length(d)]=='ALA'|d=='DRB'|d=='ME2'|d=='DD2')==T){
     b[i,9]<-paste('2008')
     next()
  }  
  if(any(d[1:length(d)]=="M10"|d=='DDD'|d=='F09'|d=='H09'|d=='V09'|d=='CON'|d=='DDC'|d=='ARB'|d=='V09'|d=='HOP'|d=='ME3'|d[1:length(d)]=='CON'|d[1:length(d)]=='ARB'|d[1:length(d)]=='ZEN')==T){
     b[i,9]<-paste('2009')
     next()
  }
  if(any(d[1:length(d)]=="M11"|d=='PWP10'|d=='DDE'|d=='DDF'|d=='G10'|d=='F10'|d=='PD2'|d=='V10'|d[1:length(d)]=='WWK'|d=='DDE'|d=='DPA'|d=='ARC'|d=='V10'|d=='PD2'|d=='DDF'|d=='SOM'|d=='TD0'|d[1:length(d)]=='ROE'|d[1:length(d)]=='SOM')==T){
     b[i,9]<-paste('2010')
     next()
  }
  if(any(d[1:length(d)]=="M12"|d=='PS11'|d=='DDG'|d=='G11'|d=='F11'|d=='CMD'|d=='PD3'|d=='ME4'|d=='V11'|d=='DDG'|d=='CMD'|d=='DDH'|d=='ISD'|d=='PD3'|d[1:length(d)]=='MBS'|d[1:length(d)]=='NPH'|d[1:length(d)]=='ISD')==T){
     b[i,9]<-paste('2011')
     next()
  }
  if(any(d[1:length(d)]=="M13"|d=='J12'|d=='F12'|d=='CM1'|d=='PC2'|d=='DDI'|d=='V12'|d=='PC2'|d=='V12'|d=='DDJ'|d=='RTR'|d=='CM1'|d[1:length(d)]=="DKA"|d[1:length(d)]=="AVR"|d[1:length(d)]=="RTR")==T){
     b[i,9]<-paste('2012')
     next()
  }
  if(any(d[1:length(d)]=='GTC'|d=='M14'|d=='J13'|d=='F13'|d=='C13'|d=='TD2'|d=='GTC'|d=='DDK'|d=='DGM'|d=='MMA'|d=='V13'|d=='DDL'|d=='C13'|d[1:length(d)]=='DGM'|d[1:length(d)]=='THS')==T){
     b[i,9]<-paste('2013')
     next()
  }
  if(any(d[1:length(d)]=="M15"|d=='DDM'|d=='J14'|d=='F14'|d=='CNS'|d=='C14'|d=='MD1'|d=='CNS'|d=='VMA'|d=='DDN'|d=='KTK'|d=='C14'|d=='DD3'|d[1:length(d)]=='BNG'|d[1:length(d)]=='JOU')==T){
     b[i,9]<-paste('2014')
     next()
  }
  if(any(d[1:length(d)]=="ORI"|d=='CP1'|d=='J15'|d=='F15'|d=='C15'|d=='FRF'|d=='DDO'|d=='DTK'|d=='TPR'|d=='MM2'|d=='DDP'|d=='PZ1'|d[1:length(d)]=='KTK'|d[1:length(d)]=='FRF'|d[1:length(d)]=='DTK'|d[1:length(d)]=='BFZ')==T){
     b[i,9]<-paste('2015')
     next()
  }
  if(any(d[1:length(d)]=="OGW"|d=='PSOI'|d=='PS16'|d=='CP2'|d=='J16'|d=='F16'|d=='CN2'|d=='C16'|d=='PCA'|d=='DDQ'|d=='W16'|d=='SOI'|d=='EMA'|d=='EMN'|d=='V16'|d=='CN2'|d=='DDR'|d=='KLD'|d=='MPS'|d=='PZ2'|d=='C16'|d=='PCA'|d[1:length(d)]=="SOI"|d[1:length(d)]=="EMN"|d[1:length(d)]=="KLD")==T){
     b[i,9]<-paste('2016')
     next()
  }
  if(any(d[1:length(d)]=="AER"|d=='CP3'|d=='J17'|d=='F17'|d=='C17'|d=='MM3'|d=='DDS'|d=='W17'|d=='AKH'|d=='MPS'|d=='CMA'|d=='E01'|d=='C17'|d=='XLN'|d=='DDT'|d=='IMA'|d=='E02'|d=='UST'|d[1:length(d)]=="HOU"|d[1:length(d)]=="XLN")==T){
     b[i,9]<-paste('2017')
     next()
  }
  if(any(d[1:length(d)]=="RIX"|d=='J18'|d=='G18'|d=='BBD'|d=='C18'|d=='CM2'|d=='ARC'|d=='SS1'|d=='GS1'|d=='A25'|d=='DDU'|d=='Q01'|d=='DOM'|d=='CM2'|d=='BBD'|d=='SS1'|d=='GS1'|d=='M19'|d=='C18'|d=='MED'|d=='GRN'|d=='SK1'|d=='GK1'|d=='GNT'|d=='UMA'|d=='MED'|d[1:length(d)]=="DOM"|d[1:length(d)]=="GRN")==T){
     b[i,9]<-paste('2018')
     next()
  }
  if(any(d[1:length(d)]=="RNA"|d=='PS19'|d=='J19'|d=='C19'|d=='SS2'|d=='GK2'|d=='Q02'|d=='MED'|d=='MH1'|d=='SS2'|d=='M20'|d=='C19'|d=='ELD'|d[1:length(d)]=="WAR"|d[1:length(d)]=="ELD")==T){
     b[i,9]<-paste('2019')
     next()
  }
}

levels(factor(b$printing))

b1<-b[startsWith(b$printing,"19")|startsWith(b$printing,"20"),]

b1$printing<-as.numeric(b1$printing)
b1<-b1[!b1$cmc==max(b1$cmc),]

by(b1$cmc,b1$printing,FUN=shapiro.test)

bg<-as.numeric(by(b1$g,b1$printing,FUN=sum))
br<-as.numeric(by(b1$r,b1$printing,FUN=sum))
bu<-as.numeric(by(b1$u,b1$printing,FUN=sum))
bw<-as.numeric(by(b1$w,b1$printing,FUN=sum))
bb<-as.numeric(by(b1$b,b1$printing,FUN=sum))
bc<-as.numeric(by(b1$cmc,b1$printing,FUN=mean))
yb<-unique(b1$printing[order(b1$printing)])
yc<-table(b1$printing)
yd<-as.data.frame(yc)

bd<-data.frame(bg,br,bu,bw,bb,bc,yb,yd$Freq)
colnames(bd)<-c('Green','Red','Blue','White','Black','Avg.Mana.Cost','Year','Total')
cu<-cumsum(bd$`Total`)
bd<-data.frame(bd,cu)
colnames(bd)[9]<-'Cumulative Total'

bd$`Cumulative Total`
bd$`Avg.Mana.Cost`<-bd$`Avg.Mana.Cost`*10
bd$Total<-bd$Total
colnames(bd)[8]<-"Total Cards"
bd$`Cumulative Total`<-bd$`Cumulative Total`/20

bd$`Cumulative Total`
bd$`Total Cards`

dm<-melt(bd,id.vars='Year')
dm$Year<-as.character(dm$Year)
dm$Year<-as.numeric(dm$Year)

st<-c('Total Cards')
cm<-c('Avg.Mana.Cost')
cu<-c('Cumulative Total')
dm$variable<-as.factor(dm$variable)
co<-c('green','red','blue','white','black','purple','gold')
levels(dm$variable)
head(cu)

nyears<-nlevels(as.factor(dm$Year))

#####Gganimate#####
grob <- grobTree(textGrob("Jwychor", x=0.1,  y=0.95, hjust=0,
    gp=gpar(col="darkblue", fontsize = 13, fontface = "italic")))
nlevels(dm$variable)
dm[dm$variable %in% st,]

gb1<-ggplot(dm,aes(x = Year,y = value,group = factor(variable),color = factor(variable)))
l<-gb1+geom_line(size = 1.5,color = c(rep('green',nyears),rep('red',nyears),rep('blue',nyears),rep('grey85',nyears),rep('black',nyears),rep('gold',nyears),rep('gold',nyears),rep('purple',nyears)))+
  geom_text(aes(label = paste("Cards/Year",value,sep = " "),size = 10,hjust = 1,vjust = -.3),data = dm[dm$variable %in% st,],position = position_dodge(width = 3),color = 'black')+
  geom_text(aes(label = paste(variable,round(value/15,1),sep = ' '),size = 10,hjust = 1,vjust = -.3),data = dm[dm$variable %in% cm,],color = 'black')+
  geom_text(aes(label = paste("Total",round(value*20,1),sep = ' '),size = 10,hjust = 1,vjust = -.3),data = dm[dm$variable %in% cu,],color = 'black')+
  scale_y_log10()+theme_light()+theme(legend.position = 'none')+theme(legend.position = 'none',axis.text.x = element_text(size = 12),
                                                                      axis.title = element_text(size=18),title = element_text(size = 20))

l1<-l + transition_reveal(Year) + view_follow(fixed_y = T) + 
  labs(title = 'Magic Card Frequencies by year',x = "Year",y = "Number of Cards")+
  annotation_custom(grob)

animate(l1,fps=3,renderer = gifski_renderer(loop=T)) ##Generates a Gif plot of the data


gb1<-ggplot(dm,aes(x=Year,y=value,group=factor(variable),color=factor(variable)))
l2<-gb1+geom_line(size=1.5,color=c(rep('green',27),rep('red',27),rep('blue',27),rep('grey85',27),rep('black',27),rep('gold',27),rep('gold',27),rep('purple',27)))+
  scale_y_log10()+theme_light()+theme(legend.position='none')+theme(legend.position='none',axis.text.x=element_text(size = 12),axis.title=element_text(size = 18),title=element_text(size = 20))+
  geom_text_repel(aes(label = ifelse(Year == 1993,paste0(as.character(variable)),paste(""))),color="black")+annotation_custom(grob)+
  labs(y="log(y)")+scale_x_continuous(breaks=seq(1993,2019,1))

l2 ##Generates a static plot of the gif plot above


colnames(bd)[c(6,8,9)]<-c("Avg.Mana.Cost*10",'Yearly Total/5','Cumulative Total/50')

bd$`Yearly Total/5`<-bd$`Yearly Total/5`/5
bd$`Cumulative Total/50`<-bd$`Cumulative Total/50`*20
bd$`Cumulative Total/50`<-bd$`Cumulative Total/50`/50

bd$`Avg.Mana.Cost*10`<-round(bd$`Avg.Mana.Cost*10`,1)

dm<-melt(bd,id.vars='Year')
bd

dm$value<-as.numeric(dm$value)
dm$Year<-as.numeric(dm$Year)

#####Plotly#####
levels(dm$variable)
head(dm)

p<- plot_ly(data=dm, x=~variable,
           y=~value,
           frame=~Year,
           marker=list(color=c('green','red','blue','pink',
              'black','gold','grey25','gold')),
           text=~value,
           type= 'bar',
           hoverinfo='variable') %>%
  animation_opts(1000,easing='cubic-in-out')


p #Create an interactive plotly barchart in the viewer

##htmlwidgets::saveWidget(as_widget(p), "PlotlyMagicChart.html") #Save the image as an html widget
