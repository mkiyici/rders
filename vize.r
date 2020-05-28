# 1. https://raw.githubusercontent.com/ulklc/covid19-timeseries/master/countryReport/raw/rawReport.csv
# Adresinde bulunan veri setini yükleyiniz.
# 
# ---------------------------------------------------------------------------------------------------
#   Veri Seti Hakkında Genel Bilgi;
# 
# day -> Tarih
# countryCode -> Ülke Kodu ( 2 Haneli)
# countryName -> Ülke Adı ( Ingilizce)
# region -> Kıta bilgisi 
# lat ->  Latitude - Enlem Bilgisi 
# lon -> Longitude - Boylam Bilgisi 
# confirmed -> Vaka Sayısı - kümülatif toplamdır
# recovered -> İyileşen Hasta Sayısı - kümülatif toplamdır
# death -> Vefat Sayısı  - kümülatif toplamdır
# 
# ---------------------------------------------------------------------------------------------------
#   


df <- read.csv('https://raw.githubusercontent.com/ulklc/covid19-timeseries/master/countryReport/raw/rawReport.csv')
# 
############################################################

# 2. Her bir ülke için 

# A. Yeni Vaka Sayısını Hesaplayınız
df$NewConfirmedforCountry <- ave(df$confirmed, df$countryName, FUN = function(x) c(NA,diff(x)))

# B. Yeni İyileşen Sayısını Hesaplayınız
df$NewRecoveredforCountry <- ave(df$recovered, df$countryName, FUN = function(x) c(NA,diff(x)))

# C. Yeni Ölüm Sayısını Hesapalayınız
df$NewDeathforCountry <- ave(df$death, df$countryName, FUN = function(x) c(NA,diff(x)))


############################################################
# 3. Her Bir Kıta için  

# A. Kümülatif Vaka Sayısını Hesaplayınız
df$CsumConfirmedforContinent <- ave(df$NewConfirmedforCountry, df$region, df$day , FUN = sum)

# B. Kümülatif İyileşen Sayısını Hesaplayınız
df$CsumRecoveredforContinent <- ave(df$NewRecoveredforCountry, df$region, df$day , FUN = sum)

# C. Kümülatif Ölüm Sayısını Hesapalayınız
df$CsumDeathforContinent <- ave(df$NewDeathforCountry, df$region, df$day, FUN = sum)

############################################################
# 4.  Ülkelerin nüfus bilgisini bul ve eşleştir (2019 son çeyrek (Q4) ya da 2020 ilk çeyrek (Q1) verisi istiyorum)
# ülke nüfusları için Birleşmiş Milletler verisi kullanılıyor 
dfulke <- read.csv('https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv')

# print(colnames(dfulke2020))

#UN verisinde birden fazla indikatör ile ve ileri dönük tahminler olmasından dolayı 
# 2020 yılında ve indikatörü 9 olan alan filtreleniyor ve veri seti olarak burası alınıyor

dfulke2020 <- subset(dfulke, Time=='2020')
dfulke2020 <- subset(dfulke, Time=='2020' & VarID=='9')

# print(colnames(dfulke2020))

#ülke nüfusları 1000 e bölündükten sonra verildiği için nüfus verisi 1000 ile çarpılıyor
dfulke2020$PopMale <- dfulke2020$PopMale*1000
dfulke2020$PopFemale <- dfulke2020$PopFemale*1000
dfulke2020$PopTotal <- dfulke2020$PopTotal * 1000

# print(colnames(dfulke2020))

#ülke verisi olarak işimize yarayacak olan yalnızca Location ve PopTotal olduğu için bu veriler süzülüyor
dfulke2020suzulmus <- dfulke2020[c("Location","PopTotal")]

# daha sonra orjinal veri setine dokunmamak adına df4 isminde bir df ye veri yazdırılıyor 
# ancak veri yazdırılırken de nüfus verisi ile eşleştiriliyor ve day veri alanı tip dönüşünüme uğruyor

df4 <- merge(df, dfulke2020suzulmus, by.x= "countryName", by.y="Location")
df4$day <- as.Date(df4$day, format="%Y-%m-%d")

#df4 dataframe içinde bulunan veriler sıralanıyor
df4sirali <- df4[order(df4$countryName, df4$day), ]

#df4 sirali içindeki veriler duplicatelerinden arındırılıyor
df4suzulmus <- df4sirali[ !duplicated(df4sirali[, c("countryName")], fromLast=T),]

# df4 suzulmus df sine bakıldığı zaman ülke nüfus verilerini de içeren veri setimiz hazır

############################################################

# 5.  Kıta nüfuslarını hesapla

df5 <- df4suzulmus


df5$ContinentPop <- ave(df5$PopTotal, df$region, FUN = sum)
df5birlesik <- aggregate(df5$PopTotal, by=list(Category=df5$region), FUN=sum)


#6. Her bir ülke için 
df6 <- df4suzulmus 
#A. Yeni Vaka / nüfus oranını hesaplayınız 
df6$newcomfirmed_pop <- df6$NewConfirmedforCountry / df6$PopTotal
#B. Yeni iyileşen / nüfus oranını hesaplayınız 
df6$newrecoverd_pop <- df6$NewRecoveredforCountry /  df6$PopTotal
#C. Yeni ölüm / nüfus oranını hesaplayınız 
df6$newdeath_pop <- df6$NewDeathforCountry / df6$PopTotal


#7. Her bir kıta için 
df7 <- df4suzulmus
df7 <- df7[,-2]
df7[complete.cases(df7), ]
#A. Yeni Vaka / nüfus oranını hesaplayınız 
newconfirmedcontinent = df7 %>% group_by(region) %>% summarise(df7 = sum(NewConfirmedforCountry))
#B. Yeni iyileşen / nüfus oranını hesaplayınız 
newrecoveredcontinet = df7 %>% group_by(region) %>% summarise(df7 = sum(NewRecoveredforCountry))
#C. Yeni ölüm / nüfus oranını hesaplayınız 
newdeathcontinet = df7 %>% group_by(region) %>% summarise(df7 = sum(NewDeathforCountry))


#8. Her bir ülke için 
df8 <- df4suzulmus 

#A. Kümülatif Vaka / nüfus oranını hesaplayınız 
df8$comfirmed_pop <- df8$confirmed / df8$PopTotal

#B. Kümülatif iyileşen / nüfus oranını hesaplayınız 
df8$recoverd_pop <- df8$recovered /  df8$PopTotal

#C. Kümülatif ölüm / nüfus oranını hesaplayınız 
df8$death_pop <- df8$death / df8$PopTotal


#9. Her bir kıta için 
df9 <- df4suzulmus
df9 <- df9[,-2]
df9[complete.cases(df9), ]
#A. Kümülatif Vaka / nüfus oranını hesaplayınız
confirmedcontinent = df9 %>% group_by(region) %>% summarise(df9 = sum(confirmed))

#B. Kümülatif iyileşen / nüfus oranını hesaplayınız 
recoveredcontinet = df9 %>% group_by(region) %>% summarise(df9 = sum(recovered))

#C. Kümülatif ölüm / nüfus oranını hesaplayınız 
deathcontinet = df9 %>% group_by(region) %>% summarise(df9 = sum(death))
