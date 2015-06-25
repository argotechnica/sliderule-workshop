# 01 - An Analytical Detective
rm(list=ls())

mvt <- read.csv("mvtWeek1.csv")

View(mvt)

nrow(mvt)

ncol(mvt)

max(mvt$ID)

min(mvt$Beat)

length(mvt$Arrest[mvt$Arrest == TRUE])

length(mvt$LocationDescription[mvt$LocationDescription == "ALLEY"])

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

summary(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

which.min(table(mvt$Month))

which.max(table(mvt$Weekday))

which.max(table(mvt$Month[mvt$Arrest == TRUE]))

hist(mvt$Date, breaks=100)

boxplot(mvt$Date ~ mvt$Arrest)

table(mvt$Year, mvt$Arrest)

2152 / (2152 + 18517)
1212 / (1212 + 13068)
550 / (550 + 13542)

Top5 <- subset(mvt, mvt$LocationDescription == "STREET" |
      mvt$LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
      mvt$LocationDescription == "ALLEY" |
      mvt$LocationDescription == "GAS STATION" |
      mvt$LocationDescription == "DRIVEWAY - RESIDENTIAL")

nrow(Top5)

Top5$LocationDescription = factor(Top5$LocationDescription)

prop.table(table(Top5$LocationDescription, Top5$Arrest), margin = 1)

table(Top5$Weekday, Top5$LocationDescription)

# 02 - Stock Dynamics
rm(list=ls())

IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
Boeing <- read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

max(IBM$Date, GE$Date, CocaCola$Date, ProcterGamble$Date, Boeing$Date)

plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")

lines(ProcterGamble$Date, ProcterGamble$StockPrice, type = "l", col = "blue")

abline(v=as.Date(c("1983-01-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="black", ylim=c(0,210))

lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))

lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="green", ylim=c(0,210))

lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="orange", ylim=c(0,210))

abline(v=as.Date(c("2004-01-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)

tapply(IBM$StockPrice, months(IBM$Date), mean)

which.max(tapply(GE$StockPrice, months(GE$Date), mean))
which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))

# 3 - Demographics and Unemployement in the United States
rm(list=ls())

CPS <- read.csv("CPSData.csv")

summary(CPS$Industry)

sort(table(CPS$State))
table(CPS$Citizenship)

(116639 + 7073) / (116639 + 7073 + 7590)

table(CPS$Race[CPS$Hispanic == 1])
apply(CPS$Race, is.na())

colSums(is.na(CPS)) # find which columns contain NAs by counting

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table <- table(CPS$Region, is.na(CPS$MetroAreaCode))
prop.table(table, margin = 1)

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

length(CPS$MetroArea[CPS$MetroArea == "Atlanta-Sandy Springs-Marietta, GA"])
length(CPS$MetroArea[CPS$MetroArea == "Baltimore-Towson, MD"])
length(CPS$MetroArea[CPS$MetroArea == "Boston-Cambridge-Quincy, MA-NH"])
length(CPS$MetroArea[CPS$MetroArea == "San Francisco-Oakland-Fremont, CA"])

# Remembering that CPS$Race == "Asian" returns a TRUE/FALSE vector of
# whether an interviewee is Asian, determine the number of metropolitan
# areas in the United States from which at least 20% of interviewees
# are Asian.
sum(tapply(CPS$Race == "Asian", CPS$MetroArea, mean) > 0.2, na.rm = TRUE)

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

str(CPS)

table <- table(CPS$Region, is.na(CPS$MetroAreaCode))
prop.table(table, margin = 1)

table(CPS$Country[CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" & is.na(CPS$Country) == FALSE] == "United States")
table(CPS$Country[CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA"] == "United States")


na.omit(CPS$Country[CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" & is.na(CPS$Country) == FALSE] == "United States")

# What proportion of the interviewees from the "New York-Northern New
# Jersey-Long Island, NY-NJ-PA" metropolitan area have a country of
# birth that is not the United States? For this computation, don't
# include people from this metropolitan area who have a missing country
# of birth.
mean(CPS$Country[CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA" & is.na(CPS$Country) == FALSE] != "United States", na.rm = TRUE)

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm = TRUE))
sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm = TRUE))

# 04 - Internet Privacy
rm(list=ls())

poll  <- read.csv("AnonymityPoll.csv")

table(poll$Smartphone)
1002-487-472