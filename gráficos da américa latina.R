#projeções gráficas é possível verificar os países subdivididos, 
#a partir de suas médias de tolerância, sendo que a variável está organizada em uma escala de 10 pontos. 
library(tidyverse)


Obj1 <- data.frame(Anos = c(2004, 2006, 2008,	2010,	2012, 2014,
                            2016,	2018, "Médiaf."), 
                   Haiti = c(NA,	1.85,	2.27,	1.66,	1.76, 1.94,	1.88,	1.88, 1.92), 
                   El_Salvador =	c(2.86,	2.99,	3.51,	3.28,	3.38,	3.85,	4.08,	4.4, 3.12),
                   Guatemala =	c(3.07,	4.08,	3.53,	3.79,	3.7,	2.65,	3.88,	4.07, 3.59),
                   Honduras = c(3.18,	2.68,	4.15,	4.07,	3.19,	4.64,	4.41,	4.21, 3.82)) 


Obj1.1 <- data.frame(Anos = time(EuStockMarkets),
                     Haiti = EuStockMarkets[, 1],
                     El_Salvador = EuStockMarkets[, 2],
                     Guatemala = EuStockMarkets[, 3],
                     Honduras = EuStockMarkets[, 4])

Obj1.2 <- Obj1 %>% gather(Países, Médias_tolerância, -Anos)

Gra <- ggplot(data = Obj1.2, aes(x= Anos, y= Médias_tolerância, 
                                 group = Países))+
  geom_line(aes(colour = Países), size = 2) + 
  geom_point(aes(shape = Países)) 


uniao1 <- Gra + theme_classic() + theme(legend.position = "bottom")


Objb.1 <- data.frame(Anos = c(2004, 2006, 2008,	2010,	2012, 2014,
                              2016,	2018, "Médiaf."), 
                     República_Dominicana= c(4.15,	3.37,	3.27,	3.38,	3.79,	4.1, 4.4, 4.13,	3.83),
                     Paraguai=	c( NA, 3.57,	3.57,	3.44,	4.01,	4.09,	4.17,	3.93,	3.83),
                     Panamá=	c(3.9,	4.53,	4.74,	4.1,	3.48,	3.8,	4.39,	3.85,	4.1),
                     Nicarágua=	c(3.5,	3.92,	4.45,	4.53,	4.53,	4.13,	4.74,	4.67,	4.31)) 


Objb.2 <- Objb.1 %>% gather(Países, Médias_tolerância, -Anos)

Gra2 <- ggplot(data = Objb.2, aes(x= Anos, y= Médias_tolerância, 
                                  group = Países))+
  geom_line(aes(colour = Países), size = 2) + geom_point(aes(shape = Países))



uniao2 <- Gra2 + theme_classic() + theme(legend.position = "bottom")


## 3ºCorreto ####


Objc <- data.frame (Anos = c(2004, 2006, 2008,	2010,	2012, 2014,
                             2016,	2018, "Médiaf."), 
                    Peru =	c(NA, 4.15,	4.25,	4.5,	4.22,	4.23,	4.37,	4.84,	4.36),
                    Bolívia	= c(3.7,	3.99,	4.25,	4.19,	4.49,	4.17,	4.91,	4.75,	4.47),
                    Equador	= c(1.54,	3.84,	4.32,	4.51,	4.46,	4.33,	4.71,	5.07,	4.68),
                    Venezuela	=	c(NA, 5.1,	4.4,	4.78,	4.43,	5.14,	5.42,	5.42,	4.88),
                    Costa_Rica =	c(NA, 4.73,	5.09,	5.72,	5.22,	5.52,	6.08,	6,	5.48)) 

Objc.1 <- Objc %>% gather(Países, Médias_tolerância, -Anos)

Gra3 <- ggplot(data = Objc.1, aes(x= Anos, y= Médias_tolerância, 
                                  group = Países))+
  geom_line(aes(colour = Países), size = 2) + geom_point(aes(shape = Países))

uniao3 <- Gra3 + theme_classic() + 
  theme(legend.position = "bottom")

## 4º ####
Objd <- data.frame (Anos = c(2004, 2006, 2008,	2010,	2012, 2014,
                             2016,	2018, "Médiaf."), 
                    Costa_Rica = c(NA,4.73,	5.09,	5.72,	5.22,	5.52,	6.08,	6,	5.48),
                    México = c(5.24,	5.54, 5.17,	5.13,	5.3,	5.43,	5.96,	6.34,	5.51),
                    Chile	=	c(NA,5.39,	5.49,	5.87,	6.55,	6.83,	6.98,	6.99,	6.3),
                    Brasil=	c(NA,5.87,	5.77,	6.55,	6.55,	6.37,	6.67,	6.89,	6.39),
                    Argentina	= c(NA,NA, 7.29, 6.9,	6.41,	6.47,	6.73,	7.05,	6.81),
                    Uruguai	=	c(NA, 6.98,6.94,6.94,7.98,8.03,8.1,7.65,7.52)) 

Objd.1 <- Objd %>% gather(Países, Médias_tolerância, -Anos)

Gra4 <- ggplot(data = Objd.1, aes(x= Anos, y= Médias_tolerância, 
                                  group = Países))+
  geom_line(aes(colour = Países), size = 2) + geom_point(aes(shape = Países))


uniao4 <- Gra4 + theme_classic() + 
  theme(legend.position = "bottom")


## 5º ####
Obje <- data.frame (Anos = c(2004, 2006, 2008,	2010,	2012, 2014,
                             2016,	2018, "Médiaf."),
                    Total_América_Latina = c(3.755,4.289,4.45,4.65,4.75,4.78,
                                             5.11,5.3,4.75)) 

Obje.1 <- Obje %>% gather(Países, Médias_tolerância, -Anos)

Gra5 <- ggplot(data = Obje.1, aes(x= Anos, y= Médias_tolerância, 
                                  group = Países))+
  geom_line(aes(colour = Países), size = 2) + 
  geom_point(aes(shape = Países))


uniao5 <- Gra5 + theme_classic() + 
  theme(legend.position = "bottom")



Obj1 <- data.frame(Anos = c(2004, 2006, 2008,	2010,	2012, 2014,
                            2016,	2018, "Médiaf."), 
                   Haiti = c(NA,	1.85,	2.27,	1.66,	1.76, 1.94,	1.88,	1.88, 1.92), 
                   El_Salvador =	c(2.86,	2.99,	3.51,	3.28,	3.38,	3.85,	4.08,	4.4, 3.12),
                   Guatemala =	c(3.07,	4.08,	3.53,	3.79,	3.7,	2.65,	3.88,	4.07, 3.59),
                   Honduras = c(3.18,	2.68,	4.15,	4.07,	3.19,	4.64,	4.41,	4.21, 3.82),
                   República_Dominicana= c(4.15,	3.37,	3.27,	3.38,	3.79,	4.1, 4.4, 4.13,	3.83),
                   Paraguai=	c( NA, 3.57,	3.57,	3.44,	4.01,	4.09,	4.17,	3.93,	3.83),
                   Panamá=	c(3.9,	4.53,	4.74,	4.1,	3.48,	3.8,	4.39,	3.85,	4.1),
                   Nicarágua=	c(3.5,	3.92,	4.45,	4.53,	4.53,	4.13,	4.74,	4.67,	4.31),
                   Peru =	c(NA, 4.15,	4.25,	4.5,	4.22,	4.23,	4.37,	4.84,	4.36),
                   Bolívia	= c(3.7,	3.99,	4.25,	4.19,	4.49,	4.17,	4.91,	4.75,	4.47),
                   Equador	= c(1.54,	3.84,	4.32,	4.51,	4.46,	4.33,	4.71,	5.07,	4.68),
                   Venezuela	=	c(NA, 5.1,	4.4,	4.78,	4.43,	5.14,	5.42,	5.42,	4.88),
                   Costa_Rica =	c(NA, 4.73,	5.09,	5.72,	5.22,	5.52,	6.08,	6,	5.48),
                   México = c(5.24,	5.54, 5.17,	5.13,	5.3,	5.43,	5.96,	6.34,	5.51),
                   Chile	=	c(NA,5.39,	5.49,	5.87,	6.55,	6.83,	6.98,	6.99,	6.3),
                   Brasil=	c(NA,5.87,	5.77,	6.55,	6.55,	6.37,	6.67,	6.89,	6.39),
                   Argentina	= c(NA,NA, 7.29, 6.9,	6.41,	6.47,	6.73,	7.05,	6.81),
                   Uruguai	=	c(NA, 6.98,6.94,6.94,7.98,8.03,8.1,7.65,7.52),
                   Z.Total.Amér.Latina = c(3.755,4.289,4.45,4.65,4.75,4.78,
                                            5.11,5.3,4.75)) 


###Exemplo

####
Obj1.2 <- Obj1 %>% gather(Países, Médias_tolerância, -Anos)


Gra <- ggplot(data = Obj1.2, aes(x= Anos, y= Médias_tolerância, 
                                 group = Países))+
  geom_line(size = 1, color = "Gray")+
  stat_summary(fun.y=mean, geom="point")


#Para colocar cor
library(RColorBrewer) # Tem um site 

#scale_fill_brewer(palette = 2, type = "seq")

#Dividindo em vários gráficos pequenos



#Escolhi esse tema :) 
  GraFinal <- Gra + facet_wrap( ~ Países, nrow = 4) +
  scale_fill_brewer(palette = 2, type = "seq")


GraFinal+ 
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(axis.text.x =  element_text(angle = 90, 
                                    vjust = 0, size= 8,  hjust = 1, 
                                    color = "#616161", family = "Times New Roman")) +
  theme(legend.position = "none")+
  geom_point( alpha=0.3, color = "steelblue", size = 1) + 
  Fonte 
  
  
  
Fonte <-  theme(text = element_text(family = "serif", size = 11),
                title = element_text(color = "#616161"),
                axis.line = element_line(color = "#616161"), 
                axis.text = element_text(colour = "#616161", size = rel(0.5))) 

#+ theme_bw() 
  

################outro

# média em todos os pontos 
# +
####
Obj1.2 <- Obj1 %>% gather(Países, Médias_tolerância, -Anos)


options(digits = 1)
Gra <- ggplot(data = Obj1.2, aes(x= Anos, y= Médias_tolerância, 
                                 group = Países))+
  geom_line(size = 1, color = "#F0E202")+
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size = 2) 
  


#Para colocar cor
library(RColorBrewer) # Tem um site 

#scale_fill_brewer(palette = 2, type = "seq")

#Dividindo em vários gráficos pequenos



#Escolhi esse tema :) 
GraFinal <- Gra + facet_wrap( ~ Países, nrow = 4) +
  scale_fill_brewer(palette = 2, type = "seq")


GraFinal+ 
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(axis.text.x =  element_text(angle = 90, 
                                    vjust = 0, size= 8,  hjust = 1, 
                                    color = "#616161", family = "Times New Roman")) +
  theme(legend.position = "none")+ 
  Fonte 



Fonte <-  theme(text = element_text(family = "serif", size = 11),
                title = element_text(color = "#616161"),
                axis.line = element_line(color = "#616161"), 
                axis.text = element_text(colour = "#616161", size = rel(0.5))) 

#+ theme_bw() 

