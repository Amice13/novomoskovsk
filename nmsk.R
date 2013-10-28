# Обработка данных по новомосковским СМИ

# Чтение базы данных

data <- read.csv("nmsk.csv", sep=";", header=T,dec=",",  colClasses=c("factor","factor","factor","factor","character",rep("numeric",50)))

# Обшее количество публикаций

dim(data)[1]

# Количество публикаций по различным СМИ

table(data[,1])

# Количество публикаций по месяцам за период исследования

data1 = subset(data,data[2]=='2012')
data2 = subset(data,data[2]=='2013')
table(data1[,c(1,3)])
table(data2[,c(1,3)])
rm(data1,data2)

# Среднее количество слов в публикациях каждого СМИ

aggregate(data[54], by=data[1], FUN=mean)

# Семантическая насыщенность публикаций

aggregate(data[55], by=data[1], FUN=mean)

# Процентное соотношение статей, в которых упомянуты политические лидеры

nmsk = data[data[42]==1,]
print(cbind(aggregate(nmsk[6:13], by=nmsk[1], FUN=sum)[1],aggregate(nmsk[6:13], by=nmsk[1], FUN=sum)[2:9]*100/table(nmsk[,1])),digits=2)

# Процентное соотношение статей в Новомосковской правде

np <- read.csv("np.csv", sep=";", header=T)

colSums(np[3:6])/dim(np)[1]

rm(np)

# Процентное соотношение публикаций о Новомосковске

print(table(nmsk[1])/table(data[1])*100,digits=2)

# Процент политических публикаций

politics = cbind(nmsk[1],rowSums(nmsk[6:17]))
for (i in 1:length(politics[,2])) {
  if (politics[i,2] == 0) { politics[i,2] = 0 } else {politics[i,2]=1}
}

table(politics)[,2]/table(nmsk[1])
rm(i,politics)

# Количество публикаций о политических партиях

aggregate(data[c(14:17)], by=data[1], FUN=sum)

# Процентное соотношение публикаций по темам "Исполком", "ЖКХ", "Спорт", "Медицина" и "Образование"

a = cbind(aggregate(nmsk[18:53], by=nmsk[1], FUN=sum)[1],aggregate(nmsk[18:53], by=nmsk[1], FUN=sum)[2:37]*100/table(nmsk[,1]))
print(a[c(1:2,4,8,17,36)],digits=3)
rm(a)
rm(nmsk)
rm(data)
