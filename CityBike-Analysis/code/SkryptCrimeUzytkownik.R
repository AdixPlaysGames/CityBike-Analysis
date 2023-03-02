library(ggplot2)
library(dplyr)
library(data.table)
library(ggmap)
library(lubridate) #miedzy innymi do odczytywania dni tygodnia
library(rstudioapi)
library(leaflet)
library(leaflet.minicharts)
library(manipulateWidget)
library(ggthemes)
library(scales) #do przecinkow na wykresie

#pobieramy dane z czerwca 2019 
tempjune <- tempfile()
download.file("https://s3.amazonaws.com/tripdata/201906-citibike-tripdata.csv.zip", tempjune, mode="wb")
unzip(tempjune, "201906-citibike-tripdata.csv")
unlink(tempjune)

datajune <- read.csv("201906-citibike-tripdata.csv", sep = ','
                 ,stringsAsFactors = TRUE, header = TRUE, na.strings =c(""," "))

#pobieramy dane z lipca 2019 
tempjuly <- tempfile()
download.file("https://s3.amazonaws.com/tripdata/201907-citibike-tripdata.csv.zip",tempjuly, mode="wb")
unzip(tempjuly, "201907-citibike-tripdata.csv")
unlink(tempjuly)

datajuly <- read.csv("201907-citibike-tripdata.csv", sep = ','
                     ,stringsAsFactors = TRUE, header = TRUE, na.strings =c(""," "))

#pobieramy dane z sierpnia 2019

tempaug <- tempfile()
download.file("https://s3.amazonaws.com/tripdata/201908-citibike-tripdata.csv.zip",tempaug, mode="wb")
unzip(tempaug, "201908-citibike-tripdata.csv")
unlink(tempaug)

dataaug <- read.csv("201908-citibike-tripdata.csv", sep = ','
                     ,stringsAsFactors = TRUE, header = TRUE, na.strings =c(""," "))

#polaczmy nasze ramki, by posiadac jedna ramke z wynikami z 3 miesiecy
data <- rbind(datajune, datajuly, dataaug)

#zmienmy lekko strukture danych aby lepiej nam sie je analizowalo
#nie potrzebujemy setnych czesci czasu rozpoczenia i zakonczenia jazdy
data$starttime <- as.POSIXct(data$starttime, format = "%Y-%m-%d %H:%M:%S")
data$stoptime <- as.POSIXct(data$stoptime, format = "%Y-%m-%d %H:%M:%S")


#sprawdzmy czy mamy jakies NA
sapply(data, function(x) sum(is.na(x)))
#nie mamy nigdzie NA

#sprawdzmy czy mamy jakies anomalie odnosnie dlugosci jazdy
summary(data$tripduration)
#jak widzimy wystepuja

#usunmy te wiersze w ktorych czas uzywania roweru byl dluzszy niz godzina
#i mnijeszy niz 1 min, pozbedziemy sie wtedy danych z potencjalnie 
#skradzionych rowerow(jazda przez 940h nie ma sensu) lub tymi z awariami(w ciagu 
#minuty nie zajedziemy daleko)
data <- data[data$tripduration < 3600 & data$tripduration > 60,]


#stworzmy sobie nowe kolumny ktore pomoga nam przy analizie danych
#tworzymy nowa kolumne z sama data
data$tripdate <- as.Date(factor(data$starttime), format = "%Y-%m-%d")
#storzmy kolumne z czasem o ktorej rozpoczela sie przejazdzka
data$time <- as.POSIXct(strptime(data$starttime, "%Y-%m-%d %H:%M"))
data$time <- strftime(data$time, format = "%H:%M")
#stworzmy kolumne z dniami tygodnia i zmienmy nazwe zawartosci
data$wday <- wday(data$tripdate) # 1 oznacza niedziele
data$wday <- ifelse(data$wday == 1, "Niedziela", data$wday)
data$wday <- ifelse(data$wday == 2, "Poniedzialek", data$wday)
data$wday <- ifelse(data$wday == 3, "Wtorek", data$wday)
data$wday <- ifelse(data$wday == 4, "Sroda", data$wday)
data$wday <- ifelse(data$wday == 5, "Czwartek", data$wday)
data$wday <- ifelse(data$wday == 6, "Piatek", data$wday)
data$wday <- ifelse(data$wday == 7, "Sobota", data$wday)
data$wday <- factor(data$wday, levels = c("Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela"))
#stworzmy kolumne z godzina rozpoczecia
data$hour <- as.numeric(substr(data$time, 1, 2))
#by dane byly bardziej czytelne zamienmy czas trwania przejazdzki z sekund na minuty
data$tripminuty <- data$tripduration / 60
#znaljdzmy rowniez wiek uzytkownikow
data$age <- year(today()) - data$birth.year
data$age_group <- cut(data$age,breaks = c(0,30,40,50,60,70,80,100),labels=c("≥1","≥30","≥40","≥50","≥60","≥70","≥80"))
#zmienmy kolumne z plcia na bardziej czytelna
data$gender <- ifelse(data$gender==0, "Unknown", ifelse(data$gender==1, "Male", "Female"))
head(data)

#---------------Ramka danych crimes ---------------
datacrime <- read.csv("NYC_crime.csv", sep = ','
                     ,stringsAsFactors = TRUE, header = TRUE, na.strings =c(""," "))
datacrime$arrest_date <- as.Date(datacrime$arrest_date)
class(datacrime$arrest_date) 
datacrime <- datacrime[datacrime$arrest_date > "2019-06-01" & datacrime$arrest_date < "2019-09-01" ,]

#________________________STACJE I ICH POPULARNOSC___________________________

#sprawdzmy jakie stacje startowe sa najbardziej i najmniej popularne
stacjestart <- data %>%
  select(start.station.name, start.station.latitude, start.station.longitude)%>%
  group_by(start.station.name, start.station.latitude, start.station.longitude)%>%
  summarise(liczba = n())%>%
  ungroup()%>%
  mutate(popupinfo = paste("Nazwa stacji:", start.station.name,"<br/>", "Liczba rowerow:",liczba))
stacjestart_top10 <- stacjestart%>% arrange(desc(liczba)) %>% head(10)
stacjestart_worst20 <- stacjestart %>% filter(liczba >5) %>% arrange(liczba)%>% head(20)

stacjestart_top10_tabela <- stacjestart_top10 %>%
  select(start.station.name, start.station.latitude, start.station.longitude, liczba)%>%
  as.data.frame()
stacjestart_top10_tabela

write.csv(stacjestart_top10_tabela, "stacjestart_top10_tabela1.csv")



#sprawdzmy jakie stacje koncowe sa najbardziej i najmniej popularne
stacjeend <- data %>%
  select(end.station.name, end.station.latitude, end.station.longitude)%>%
  group_by(end.station.name, end.station.latitude, end.station.longitude)%>%
  summarise(liczba = n())%>%
  ungroup()%>%
  mutate(popupinfo = paste("Nazwa stacji:", end.station.name,"<br/>", "Liczba rowerow:",liczba))
  
stacjeend_top10 <- stacjeend %>% arrange(desc(liczba)) %>% head(10)
stacjeend_worst20 <- stacjeend %>% filter(liczba >5) %>% arrange(liczba)%>% head(20)
stacjeend_worst20

stacjesend_top10_tabela <- stacjeend_top10 %>%
  select(end.station.name, liczba)%>%
  as.data.frame()

write.csv(stacjesend_top10_tabela, "stacjesend_top10_tabela.csv")





#pokazemy wszystkie stacje
stacjeall_funkcja <- function(stacjestart, stacjeend){
  names <- c("stationname", "stationLat", "stationLon", "liczba", "popupinfo")
  colnames(stacjestart) <- names
  colnames(stacjeend) <- names
  stacjeall <- rbind(stacjestart, stacjeend)
  stacjeall_top <- unique(stacjeall)
  return (stacjeall_top)
}
stacjeall <- stacjeall_funkcja(stacjestart, stacjeend)


#stworzmy mape interaktywna
initial_lat = median(stacje$start.station.latitude)
initial_lon = median(stacje$start.station.longitude)

mapa <- leaflet()%>%addTiles()%>%
  setView(lat = initial_lat, lng = initial_lon, zoom = 12)%>%
  addCircles(data = stacjeall, lng = ~stationLon, lat = ~stationLat, weight = 1, 
             radius = ~sqrt(liczba), popup = ~popupinfo, group = "wszystkie stacje", color = "purple")%>%
  addCircles(data = stacjestart_top10, lng = ~start.station.longitude, lat = ~start.station.latitude, weight = 3,
             radius = ~sqrt(liczba), popup = ~popupinfo, group = "top 10 stacje poczatkowe")%>%
  addCircles(data = stacjeend_top10, lng = ~end.station.longitude, lat = ~end.station.latitude, weight = 3,
             radius = ~sqrt(liczba), popup = ~popupinfo, color = "red", group = "top 10 stacje koncowe" )%>%
  addCircles(data = stacjestart_worst20, lng = ~start.station.longitude, lat = ~start.station.latitude, weight = 3,
             radius = 90, popup = ~popupinfo, color = "red", group = "worst 20 stacje poczatkowe" )%>%
  addCircles(data = stacjeend_worst20, lng = ~end.station.longitude, lat = ~end.station.latitude, weight = 3,
             radius = 90, popup = ~popupinfo, color = "red", group = "worst 20 stacje koncowe" )%>%
  addCircles(data = datacrime_filtr, lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = 20, popup = ~popupinfo, color = "green", group = "przestepstwa")%>%
  addLayersControl(baseGroups = c("wszystkie stacje", "top 10 stacje poczatkowe", "top 10 stacje koncowe",
                                  "worst 20 stacje poczatkowe","worst 20 stacje koncowe"),
                   overlayGroups = c("przestepstwa"))
mapa

#---------------ramka crimes do mapy-------------
#sprawdzimy gdzie ma miejsce wiekszosc przestepstw
initial_lat = median(stacje$start.station.latitude)
initial_lon = median(stacje$start.station.longitude)
datacrime_filtr <- datacrime %>%
  select(ofns_desc, longitude, latitude)%>%
  mutate(popupinfo = paste("rodzaj przesępstwa:", ofns_desc))

mapacrime <- leaflet()%>%addTiles()%>%
  setView(lat = initial_lat, lng = initial_lon, zoom = 12)%>%
  addCircles(data = datacrime_filtr, lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = 10, popup = ~popupinfo, color = "green", group = "przestepstwa")
mapacrime

#__________KTO JEST PRZECIETNYM UZYTKOWNIKIEM CITIBIKES?______________

#sprawdzmy plec 
plec <- data %>% select(gender)%>%
  group_by(gender)%>%
  summarise(liczba = n())%>%
  as.data.frame()
#wykres rozlozenia plci
plec_wykres <- ggplot(plec, aes(x="", y = liczba, fill = gender)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +  
  theme_void() + 
  ggtitle(expression("Rozłożenie płci")) +
  geom_text(aes(label = paste0(round(liczba/sum(liczba)*100), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "YlOrRd")
plec_wykres 


#jakie grupy wiekowe 
grupa_wiekowa <- data %>% 
  select(age_group)%>% 
  as.data.frame()

grupa_wiekowa_wykres <- grupa_wiekowa %>% 
  ggplot(aes(x = age_group, fill=age_group)) +
  geom_bar(alpha = .9) + #przejrzystosc slupkow
  theme_fivethirtyeight() +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(legend.position="false")+
  ggtitle(expression(atop("Rozkład grup wiekowych")))+
  scale_y_continuous(labels = comma)
grupa_wiekowa_wykres
#wekszosc uzytkownikow jest z zakresu wiekowego 30 - 40

#rozklad wieku w zaleznosci od plci
plec_a_wiek <- data %>% 
  select(gender, age) %>% 
  filter(gender %in% c("Male","Female"))

#szukamy mediany wieku w zaleznosci od plci
plec_a_wiek_mediana <- plec_a_wiek %>% 
  group_by(gender) %>% 
  summarise(mediana = median(age)) 


plec_a_wiek_wykres <- plec_a_wiek %>% 
  ggplot(aes(x = gender, y = age, fill = gender))+
  geom_boxplot(alpha=.9) +
  ylim(15,60)+
  theme_fivethirtyeight()+
  theme(legend.position="none")+
  scale_fill_brewer(palette="YlOrRd")+
  ggtitle("Rozkład wieku w zależności od płci")+
  annotate("text",x="Female",y=36,label="Mediana = 36")+
  annotate("text",x="Male",y=38,label="Mediana = 38")
plec_a_wiek_wykres


#czy jest subskrybentem
subskrybent <- data %>% 
  select(usertype) %>% 
  group_by(usertype)%>%
  summarise(liczba = n())%>%
  as.data.frame()

subskrybent_wykres <- ggplot(subskrybent, aes(x="", y = liczba, fill = usertype)) + geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +  
  theme_void() + 
  geom_text(aes(label = paste0(round(liczba/sum(liczba)*100), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "YlOrRd")
subskrybent_wykres

#czy subskrybentami sa glownie mezczyzni czy kobiety
subskrybent_a_plec <- data %>% 
  select(gender, usertype) %>% 
  filter(gender %in% c("Male","Female"))%>% 
  group_by(gender, usertype) %>% 
  summarise(liczba = n()) %>% 
  mutate(procent = round(liczba/sum(liczba)* 100))%>%
  as.data.frame()
subskrybent_a_plec_kobieta <- subskrybent_a_plec[subskrybent_a_plec$gender == 'Female',]
subskrybent_a_plec_mezczyzna <- subskrybent_a_plec[subskrybent_a_plec$gender == 'Male',]

subskrybent_a_plec__kobieta_wykres <- ggplot(subskrybent_a_plec_kobieta, aes(x="", y = liczba, fill = usertype)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +  
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Rozkład subskrybentów dla kobiet") + 
  geom_text(aes(label = paste0(round(liczba/sum(liczba)*100), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "YlOrRd")
subskrybent_a_plec__kobieta_wykres

subskrybent_a_plec_mezczyzna_wykres <-ggplot(subskrybent_a_plec_mezczyzna, aes(x="", y = liczba, fill = usertype)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +  
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Rozkład subskrybentów dla mężczyzn") + 
  geom_text(aes(label = paste0(round(liczba/sum(liczba)*100), "%")), position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = "YlOrRd")
subskrybent_a_plec_mezczyzna_wykres








