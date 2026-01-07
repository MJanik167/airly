#######Mapa zanieczyszczenia Krakowa#######
#Najpierw utworzymy sobie strukturę danych, 
#do której ostatecznie dołączymy dane dotyczące zanieczyszczenia z czujników

#install.packages("httr") # do obsługi połączeń internetowych 
library(httr)

#install.packages("jsonlite")#dane są w formacie JSON, pakiet do pracy w tym formacie
library(jsonlite) 

#pobranie danych o czujnikach w odległości 15km od ratusza 
#w argumencie "apikey", w miejsce "xxx" wpisz klucz dostępu

r <- GET("https://airapi.airly.eu/v2/installations/nearest?lat=50.0617022&lng=19.9373569&maxDistanceKM=15&maxResults=-1", 
         add_headers(apikey = "Tu wpisz swój klucz", Accept = "application/json")
)
#przejście do listy
jsonRespText<-content(r,as="text")
test15<-fromJSON(jsonRespText)

#tworzymy ramkę data15 - z danymi o lokalizacji, wysokości i id czjników
longitude<-test15$location$longitude
latitude<-test15$location$latitude
data15<-data.frame(longitude,latitude)
data15$elevation<-test15$elev #wysokość nie bedzie potrzebna, ale niech będzie
data15$id<-test15$id
head(data15)

#tak jak poprzednim razem, tworzymy obiekt przestrzenny (ppp)
#install.packages("sp")
library(sp)
#install.packages("spatstat")
library(spatstat)
data_spat<-data.frame(lon=data15$longitude,lat=data15$latitude,elev=data15$elev,id=data15$id)
coordinates(data_spat) <- ~lon+lat #określamy, które elementy to koordynaty (potrzebne do ppp)
proj4string(data_spat) <- CRS("+proj=longlat +datum=WGS84") #określamy, jaki mamy układ
head(data_spat) # mamy już obiekt w układzie sferycznym, który można automatycznie konwertować

#konwersja do UTM (bo tworzymy ppp, a to jego układ)
data_UTM <- spTransform(data_spat, CRS("+proj=utm +zone=34 +datum=WGS84"))

#aby utworzyć obiekt ppp, ale tylko z czujnikami w Krakowie, musimy mieć kontur Krakowa, którym przytniemy data_UTM 
#tworzymy więc odpowiedni obiekt w ukłądzie UTM (zmienna krakowUTM)
#install.packages("sf")
library(sf)
dzielnice<-st_read("dzielnice_Krakowa.shp") #układ odniesienia(CRS) to ETRS89 (Poland CS92)
# konwertujemy do WGS84
dzielniceWGS84<-st_transform(dzielnice,crs = 4326) # "4326" to kod dla WGS84
# zostawiamy tylko kontur miasta 
krakowWGS84<-st_union(dzielniceWGS84)
#konwertujemy go na UTM
krakowUTM<-st_transform(krakowWGS84,CRS("+proj=utm +zone=34 +datum=WGS84"))

#teraz możemy już utworzyć obiekt ppp z danymi ("marks") w punktach (uwaga: dane to id), przycięcie oknem krakowUTM
#jeszcze zmienna pomocnicza z koordynatami (bo spTransform wprowadza swoje nazwy kolumn z koordynatami)
XY<-coordinates(data_UTM)
#i obiekt ppp 2D:
data15_ppp_id<-ppp(x=XY[,1],y=XY[,2],marks=data.frame(elev=data_UTM$elev,id=data_UTM$id),window=as.owin(krakowUTM))
data15_ppp_id$marks$id #mamy od razu tylko te id które są w Krakowie

#####pobieranie danych z AIRLY dla wybranych czujników (czyli tych z data15_ppp_id)#####

#najpierw musimy utworzyć:
##1) dwa obiekty zawierające:
###liczbę czujników
n_id<-length(data15_ppp_id$marks$id)
n_id
### id czujników
id<-data15_ppp_id$marks$id
id
##2) pustą listę do odczytów z czujników (installations) AIRLY 
list_inst2 <- vector(mode = "list", length = n_id) #funkcja do stworzenia struktury danych

#pętla do pobrania danych z czujników AIRLY
#w argumencie "apikey", w miejsce "xxx" wpisz klucz dostępu
#!!!!!!UWAGA - proszę puścić tylko raz (bo mamy 100 zapytań na dzień, a czujników jest 63, więc wykorzystamy 63 zapytania)

for (i in seq(1,n_id)) {
  
  print(i) #to tylko pomocniczo, żeby wiedzieć, który obrót pętli
  #tworzymy ciąg znaków określajacy adres, pod kótrym znajdują się pomiary z czujnika
  str<-paste("https://airapi.airly.eu/v2/measurements/installation?installationId=",id[i],sep="")
  #pobieramy dane z adresu
  r <- GET(url=str,add_headers(apikey = "xxx", Accept = "application/json"))
  #przechodzimy z formatu r na json i z json na tekst
  jsonRespText<-content(r,as="text")
  inst<-fromJSON(jsonRespText)
  
  list_inst2[[i]]<-inst #tutaj będą wszystkie odczyty
  
}
#koniec pętli

#zapis pełnej listy do pliku (na wszelki wypadek, bo mamy tylko 100 zapytań dziennie do AIRLY
save(list_inst2,file="list_inst2.Rdata") #wczytanie: load()

#load("list_inst2.Rdata")

#wyświetlmy informacje o drugim czujniku (pierwszy czujnik jest uszkodzony (brak danych) - sprawdź sam - może już naprawili ;))
list_inst2[[2]] #koszmarna sturktura danych :( ale nas interesuje tylko pole "current" dla PM2.5
#są tam jeszcze inne dane z ostatnich 24 godzin (history) i prognoza (forecast) na następne 24 godziny

#problematyczne może być to, że czujniki mogą sie różnić względem rejestrowanych parametrów
#przykład różnic
list_inst2[[2]]$current$values
list_inst2[[6]]$current$values

#Wybieramy potrzebne odczyty aktualnego (”current”) stężenia PM2.5 zarejestrowanego dla wybranych czujników.
## 1) tworzymy pusty wektor dla danych "current"
current<-rep(NA,n_id)

## 2) "wyciągnamy" wartości "current" PM2.5 za pomocą pętli
for (i in seq(1,n_id)) {
  
  print(i)
  
  logic<-list_inst2[[i]]$current$values$name=="PM25" #zmienna logiczna do wyszukania pól o nazwie "PM25"
  
  if (sum(logic)==1) #testujemy, czy istnieje jedno i tylko jedno takie pole (zdarzają się błędne odczyty - tych nie chcemy zapisać)
    current[i]<-list_inst2[[i]]$current$values[logic,2] 
} #od for


#####!!!wyjaśnienie co robi powyższa pętla!!!#####
i<-2
logic<-list_inst2[[i]]$current$values$name=="PM25"
logic
# [1] FALSE  TRUE FALSE FALSE FALSE FALSE
list_inst2[[i]]$current$values
#         name   value
#1         PM1    8.00
#2        PM25   12.38
#3        PM10   13.81
#4    PRESSURE 1006.06
#5    HUMIDITY   91.33
#6 TEMPERATURE    3.94
list_inst2[[i]]$current$values[logic,]
#  name value
#2 PM25   12.38
list_inst2[[i]]$current$values[logic,2]
#[1] 12.38

# zobaczmy utworzony wektor
current

#####tworzenie mapy#####
# teraz przekształcamy obiekt data15_ppp_id do obiektu spdf (aby móc narysować mapę)
library(automap)
#przekształcamy obiekt ppp do spdf (na tym działa autokrige)
data15_spdf <- as.data.frame(data15_ppp_id)
coordinates(data15_spdf) <- ~x+y # koordynaty
proj4string(data15_spdf) <- CRS("+proj=utm +zone=34 +datum=WGS84") # układ odniesienia
# dodajemy kolumnę current
data15_spdf$current<-current
head(data15_spdf)
dev.off() #bo RStudio może wariować ;)
plot(data15_spdf)

#zobaczmy jeszcze raz nasze dane z czujników
current
#ponieważ mamy wartości NA, musimy je oznaczyć (aby zadziałało autoKrige)
miss <- is.na(data15_spdf$current)

#teraz już mozemy wykonać mapę
pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,])
# "!" odwraca wartości logiczne - czyli input data to dane nie będące NA
plot(pm25_auto$krige_output[1],main="PM 2.5")
points(data15_ppp_id[!miss,],pch="*",col="White")
plot(Window(data15_ppp_id),add=TRUE)

#jeszcze parę słów odnośnie wyboru metod krigingu, wyswietlmy mapę z błędami i wariogramem 
plot(pm25_auto)
# zwróćmy uwagę na wariogramy

#zmieńmy model i porównajmy wyniki, popatrzmy na wariogram
pm25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,], model="Gau")
plot(pm25_auto$krige_output[1],main="PM 2.5")
points(data15_ppp_id[!miss,],pch="*",col="White")
plot(Window(data15_ppp_id),add=TRUE)
plot(pm25_auto)


############ ŁADNA MAPA############
dev.off()
#i kriging na wypasie, jeszcze raz to co już było tylko "marks" jest zmienione na "current"
#zarys krakowa w odpowiednim formacie
bound<-st_as_sf(krakowUTM)
plot(bound)
##pobierzmy współrzędne punktów konturu w formie macierzy
coord<-as.data.frame(st_coordinates(krakowUTM))
#tworzymy siatkę - prostokąt okalajacy kontur Krakowa
##określamy współrzędne naroży
left_down<-c( min(coord$X), min(coord$Y))
right_up<-c( max(coord$X), max(coord$Y))
##ustalamy rozmiar oczka siatki (100x100 metrów)
size<-c(100,100)
##przeliczamy: ile oczek siatki przypada na długość i szerokość naszego prostokąta 
points<- (right_up-left_down)/size
num_points<-ceiling(points) #zaokrąglenie w górę 
#wreszcie siatka
grid <- GridTopology(left_down, size,num_points)
# kowersja siatki do odpowiedniego formatu, w odpowiednim układzie (tu: WGS84)
gridpoints <- SpatialPoints(grid, proj4string = CRS("+proj=utm +zone=34 +datum=WGS84"))
plot(gridpoints) #czekamy cierpliwie

#przycinamy siatkę konturem funkcją crop_shape z pakietu tmaptools
#install.packages("tmaptools")
library(tmaptools)
g<-st_as_sf(gridpoints)#konwersja do formatu na którym działa crop_shape
cg<-crop_shape(g,bound,polygon = TRUE)
spgrid <- SpatialPixels(as_Spatial(cg))#konwersja z powrotem do st i następnie do SpatialPixels
plot(spgrid)
#kriging i mapa
##tu uwaga na current zamiast marks!
PM25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,],new_data=spgrid)
plot(PM25_auto$krige_output[1],main="PM 2.5")
points(data15_ppp_id[!miss,],pch="*",col="White")

#Zobaczmy też błędy i semiwariongram
plot(PM25_auto)

###poeksperymentujmy z innymi metodami:
PM25_auto <- autoKrige(current ~ 1, input_data = data15_spdf[!miss,],new_data=spgrid, model="Exp")
plot(PM25_auto$krige_output[1],main="PM 2.5")
points(data15_ppp_id[!miss,],pch="*",col="White")
plot(PM25_auto)



