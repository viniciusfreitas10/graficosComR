#formação cientisda de dados
#Grafico

trees
hist(trees$Height)
hist(trees$Height, main = 'Arvores', ylab = 'Frenquencia', xlab = 'Altura', col = 'blue')
hist(trees$Height, main = 'Arvores', ylab = 'Frenquencia', xlab = 'Altura', col = 'blue',
     breaks = 15)

densidade = density(trees$Height)
plot(densidade)

hist(trees$Height, main = NULL, xlab = NULL, ylab = NULL, col = 'blue')
par(new = TRUE)
plot(densidade)

#Dispersão

plot(trees$Girth, trees$Volume)
plot(trees$Girth, trees$Volume, main = 'Arvores')
plot(trees$Girth, trees$Volume, ylab ='Cincuferencia', xlab = 'Volume', col = 'blue', main = 'Arvores')
plot(trees$Girth, trees$Volume, ylab ='Cincuferencia', xlab = 'Volume', col = 'blue', main = 'Arvores', pch = 20)
#linha
plot(trees$Girth, trees$Volume, ylab ='Cincuferencia', xlab = 'Volume', col = 'blue', main = 'Arvores', pch = 20, type = 'l')

CO2

plot(CO2$conc, CO2$uptake, pch = 20, col = CO2$Treatment)
legend('bottomright', legend = c('Nonchilled', 'Chilled'), cex = 1, fill = c('black', 'red'))

plot(trees)
plot(CO2)

split.screen(figs = c(2,2))
screen(1)
plot(trees$Girth, trees$Volume)
screen(2)
plot(trees$Girth, trees$Height)
screen(3)
plot(trees$Height, trees$Volume)
screen(4)
plot(trees$Volume)
close.screen(all=TRUE)


boxplot(trees$Volume)
boxplot(trees$Volume, main = 'Arvores', xlab = 'Volume', col = 'blue', outline = F)
boxplot(trees$Volume, main = 'Arvores', xlab = 'Volume', col = 'blue', outline = F, notch = T)
boxplot.stats(trees$Height)
boxplot.stats(trees$Height)$stats
boxplot(trees, col = 'red')

InsectSprays
spray = aggregate(. ~ spray, data = InsectSprays, sum)
spray
barplot(spray$count, col = gray.colors(6), xlab = 'Spray', ylab = 'Total', names.arg = spray$spray)
box()

pie(spray$count, labels = spray$spray, main = 'Spray', col = c(2:7))
pie(spray$count, labels = NA, main = 'Spray', col = c(2:7))
legend('bottomright', legend = spray$spray, cex = 1, fill = c(2:7))


library(lattice)
bwplot(trees$Volume)
bwplot(trees$Volume, xlab = 'Arvores')

histogram(trees$Volume, main = 'Arvores', xlab = 'Volume', aspect=0.5, nint = 20)
chickwts
histogram(chickwts$weight, aspect = 0.5)
aggregate(chickwts$weight, by=list(chickwts$feed), FUN=sum)
histogram(~weight | feed, data=chickwts)

CO2
xyplot(CO2$conc ~ CO2$uptake)

xyplot(CO2$conc ~ CO2$uptake | CO2$Type)
xyplot(CO2$conc ~CO2$uptake | CO2$Treatment)

esoph
dotplot(esoph$alcgp ~ esoph$ncontrols, data = esoph)
dotplot(esoph$alcgp ~ esoph$ncontrols | esoph$tobgp)

#matriz de disperçao

splom(~CO2[4:5] | CO2$Type, CO2)

#densidade condicional

densityplot(~CO2$conc | CO2$Treatment)
densityplot(CO2$conc)
densityplot(~CO2$Plant | CO2$Treatment)

OrchardSprays

#grafico 3D

cloud(decrease ~ rowpos * colpos, data = OrchardSprays)
cloud(decrease ~rowpos * colpos, data = OrchardSprays, groups = treatment )
trees

#grafico e level plot

levelplot(Girth ~ Height * Volume, data = trees)


x = sample(1:6,6,replace = T)
mean(x)
 
