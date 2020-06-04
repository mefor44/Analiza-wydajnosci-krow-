



#biblioteki 
library(readxl)#wczytanie danych
#library(ggplot2)
library(corrplot)#heatmap
library(MASS)#general to specific
library(xtable)#zapisywanie tablic (latex) 
library(sur)#funkcja leverage
library(faraway)# funkcja vif
library(lmtest)# test breusha pagana
library(caret)#walidacja krzyzowa
library(glmnet)#lasso

#wczytywanie surowych danych
path = "choose your path"#sciezka do pliku
DATA<- read_excel(path,sheet=1,range='A1:GV16280',col_names=TRUE)
df_full <- as.data.frame(DATA)


#wybieramy interesujacy nas rok (2017) i farmy zajmujace sie produkcja mleka
df = subset(df_full, subset=df_full$YEAR == 2017& df_full$TF8 == "(5) Milk" &
              df_full$REGION != ".",
            select = c("SE125", "SE615", "SE195", "SE216", "SE220", "SE436", "SE310",
                       "SE284", "SE315", "SE200", "SE330","SE010", "SE011", "SE025"
                       , "SE005", "SE085", "SE309", "SE090", "SE080", "SE071"))





colnames(df) = c("wydajnosc_krow", "doplaty", "SE195", "mleko_przetwory", "SE220", "SE436", "SE310",
                 "koszty_prod_roslinnej", "SE315", "SE200", "koszty_prod_zwierzecej","naklady_pracy", "SE011", "SE025"
                 , "SE005", "SE085", "SE309", "SE090", "SE080", "uprawy_pastewne")
#View(df)

#usuwamy wiersze z brakami danych
df <- na.omit(df)

#Zmienna: wielkosc gospodarstwa, tworzymy zmienna kategorialna na podstawie wielkosci 
#ekonomicznej gospodarstwa

df$gospodarstwo = df$SE005
df$gospodarstwo[df$SE005 < 8] = "b_male"
df$gospodarstwo[df$SE005 >= 8 & df$SE005 <25] = "male"
df$gospodarstwo[df$SE005 >= 25 & df$SE005 <50] = "sr_male"
df$gospodarstwo[df$SE005 >= 50 & df$SE005 <100] = "sr_duze"
df$gospodarstwo[df$SE005 >= 100 & df$SE005 <500] = "duze"
df$gospodarstwo[df$SE005 >= 500] = "b_duze"
df$gospodarstwo = factor(df$gospodarstwo, levels = c("b_male", "male", "sr_male",
                                                     "sr_duze", "duze", "b_duze"),
                         labels =c("b_male", "male", "sr_male",
                                   "sr_duze", "duze", "b_duze") )

#zmienna pasze_wlasne
odsetek_mlecznych = df$SE085/df$SE080#odsetek krow mlecznych wsord wszystkiech zwierzat

df$pasze_wlasne = odsetek_mlecznych * df$SE315 / df$SE085

#zmienna pasze_obce
df$pasze_obce = (df$SE310-df$SE315)*odsetek_mlecznych / df$SE085



#2.1 Wizualizacja

#############zmienna objasniana####################
par(mfrow=c(1,2))
plot(df$wydajnosc_krow,  main = "Wydajnoœæ krów mlecznych", pch = 16)
abline(h = mean(df$wydajnosc_krow), lwd = 2, col = "red", lty=5)
#abline(h = median(df$wydajnosc_krow), lwd = 2, col = "green", lty = 5)
#legend(x = "topright", legend=c("srednia", 
#                               "mediana"),
#       col=c("red", "green"), lty=c(5,5), cex=0.5)
hist(df$wydajnosc_krow, main = "Histogram")

#srednia dla wydajnosci krow
mean(df$wydajnosc_krow)



############Zmienna gospodarstwo##########
par(mfrow = c(1,2))
plot(df$gospodarstwo, ylab = "liczba gospodarstw", main = "Rozmiar gospodarstw")
plot( df$gospodarstwo,df$wydajnosc_krow, ylab = "wydajnoœæ króW",
      main = "Rozmiar gospodarst a wydajnoœæ")

#zmniejszam liczbe poziomów factora gospodarstwo do "0" i "1" które oznaczaja odpowiednio male i duze gospodarstwa

df$gospodarstwo_2 <- df$SE005
df$gospodarstwo_2[df$SE005 <=100 ] = "male"#male gospodarstwa
df$gospodarstwo_2[df$SE005 >100 ] = "duze"#duze gospodarstwa
df$gospodarstwo_2 = factor(df$gospodarstwo_2)

df$gospodarstwo_2
plot(df$gospodarstwo_2, df$wydajnosc_krow)
################pozostale zmienne##########################
dev.off()

par(mfrow = c(2,4))
#Dop³ata do produkcji
plot(df$doplaty, main = "Dop³aty do produkcji zwierz.", pch = 16)
abline(h = mean(df$doplaty), lwd = 2, col = "red", lty=5)

#uprawy pastewne
plot(df$uprawy_pastewne, main = "Uprawy pastewne", pch = 16)
abline(h = mean(df$uprawy_pastewne), lwd = 2, col = "red", lty=5)

#Mleko i przetwory z mleka krowiego

plot(df$mleko_przetwory, main = "Mleko i przetwory z mleka", pch = 16)
abline(h = mean(df$mleko_przetwory), lwd = 2, col = "red", lty=5)

#Pasze w³asne

plot(df$pasze_wlasne, main = "Pasze w³asne", pch = 16)
abline(h = mean(df$pasze_wlasne), lwd = 2, col = "red", lty=5)

#Pasze obce

plot(df$pasze_obce, main = "Pasze obce", pch = 16)
abline(h = mean(df$pasze_obce), lwd = 2, col = "red", lty=5)

#Koszty bezpoœrednie produkcji roœlinnej

plot(df$koszty_prod_roslinnej, main = "Koszty produkcji roœlinnej", pch = 16)
abline(h = mean(df$koszty_prod_roslinnej), lwd = 2, col = "red", lty=5)

#Nak³ady pracy

plot(df$naklady_pracy, main = "Nak³ady pracy", pch = 16)
abline(h = mean(df$naklady_pracy), lwd = 2, col = "red", lty=5)

#Pozosta³e koszty bezpoœrednie produkcji zwierzêce

plot(df$koszty_prod_zwierzecej, main = "Pozosta³e koszty prod. zwierz.", pch = 16)
abline(h = mean(df$koszty_prod_zwierzecej), lwd = 2, col = "red", lty=5)



####################################################################
###### Wp³yw poszczególnych zmiennych na zmienn¹ objaœnian¹ #####
plot(df$doplaty,df$wydajnosc_krow,main = "Dop³aty do produkcji zwierz.")
plot(df$uprawy_pastewne,df$wydajnosc_krow,main = "Uprawy pastewne")
plot(df$mleko_przetwory,df$wydajnosc_krow,main = "Mleko i przetwory z mleka")
plot(df$pasze_wlasne,df$wydajnosc_krow,main = "Pasze w³asne")
plot(df$pasze_obce,df$wydajnosc_krow,main = "Pasze obce")
plot(df$koszty_prod_roslinnej,df$wydajnosc_krow, main = "Koszty produkcji roœlinnej")
plot(df$naklady_pracy,df$wydajnosc_krow, main = "Nak³ady pracy")
plot(df$koszty_prod_zwierzecej,df$wydajnosc_krow, main = "Pozosta³e koszty prod. zwierz.")



##########czy logarytmy sa okej?#############
par(mfrow = c(1,4))
plot(log(df$mleko_przetwory ), df$wydajnosc_krow, main = "Mleko i przetwory z mleka")
plot(log(df$koszty_prod_zwierzecej), df$wydajnosc_krow, main = "Pozosta³e koszty prod. zwierz.")
plot(log(df$uprawy_pastewne), df$wydajnosc_krow, main = "Uprawy pastewne")
plot(log(df$naklady_pracy), df$wydajnosc_krow, main = "Naklady pracy")
plot(log(df$koszty_prod_roslinnej), df$wydajnosc_krow, main = "Koszty prod. roœlinnej")

###########DOKLADAMY LOGARYTMY########

df$log_mleko = log(df$mleko_przetwory)
df$log_koszty_p_zwierzecej = log(df$koszty_prod_zwierzecej)
df$log_koszty_p_roslinnej = log(df$koszty_prod_roslinnej)
df$log_naklady_pracy = log(df$naklady_pracy)
df$log_uprawy_pastewne = log(df$uprawy_pastewne)


par(mfrow=c(1,1))
######### HEATMAP #########
cor_data = subset(df,select=c( "wydajnosc_krow", "koszty_prod_roslinnej","doplaty",
                               "pasze_wlasne","pasze_obce",
                               "log_naklady_pracy","log_koszty_p_zwierzecej"
                               ,"log_mleko","log_uprawy_pastewne") )


corr_m = cor(cor_data,method="pearson")
corrplot(corr_m, method = "square")


########### SEKWENCYJNA BUDOWA MODELU #########
#od ogolu do szczegolu



dane = subset(df, select = c( "wydajnosc_krow","log_naklady_pracy",
                              "pasze_wlasne","pasze_obce",
                              "log_koszty_p_zwierzecej", "gospodarstwo_2",
                              "doplaty","log_uprawy_pastewne", "log_mleko",
                              "log_koszty_p_roslinnej" ) )



# Fit the full model 
model1 <- lm(wydajnosc_krow ~. + gospodarstwo_2:log_koszty_p_roslinnej+
               +gospodarstwo_2:pasze_wlasne + gospodarstwo_2:log_uprawy_pastewne, data = dane)
summary(model1)


# Stepwise regression model
model_backward <- stepAIC(model1, direction = "backward", steps = 2000, 
                          trace = T)


#hispotria zmian
model_backward$anova

summary(model_backward)



######TEST F MODEL1 VS MODEL_BACKWARD ########
n = dim(model.matrix(model1))[1]
rss1 = sum(model1$residuals^2)
k = model1$rank
rss2 = sum(model_backward$residuals^2)
liczba_restrykcji = k - model_backward$rank
liczba_restrykcji
F = (n-k)/liczba_restrykcji*(rss2-rss1)/rss1
F
qf(0.95, liczba_restrykcji, n-k)
1-pf(F, liczba_restrykcji, n-k)#pvalue
#Wartosc statystyki testowej jest mniejsza od wartosci krytycznej wiec nie odrzucamty
# H0 mowiacej o tym ze b1=b2=b3=0. 

########## Sprawdzamy czy faktycznie logarytmy poprawiaj¹ dopasowanie modelu #############

dane_proste <- subset(df,select = c("wydajnosc_krow","naklady_pracy",
                                    "pasze_wlasne","pasze_obce",
                                    "koszty_prod_zwierzecej", "gospodarstwo_2",
                                    "doplaty","uprawy_pastewne", "mleko_przetwory",
                                    "koszty_prod_roslinnej"))

model0 <- lm(wydajnosc_krow~. + gospodarstwo_2:koszty_prod_roslinnej +
               + gospodarstwo_2:pasze_wlasne + gospodarstwo_2:uprawy_pastewne, data = dane_proste)
summary(model0)
#print(xtable(model1, type = "latex"), file = "D:\\Studia\\UJ - Matematyka w ekonomii\\Ekonometria\\Projekt\\model0_summary.tex")

model0_backward <- stepAIC(model0, direction = "backward", steps = 2000, 
                           trace = T)
summary(model0_backward)



######### IDENTYFIKACJA OBSERWACJI WPLYWOWYCH  DZWIGNIA#########
# X <- model.matrix(model_backward)
# dzwignia = leverage(model_backward)#dzwignia
# plot(dzwignia)#wykresik
# treshold = 2*(dim(X)[2]-1)/dim(X)[1] #nie wliczamy interceptu do puli zmiennych objaœniaj¹cych
# abline(h=treshold,col="red")
# y = df$wydajnosc_krow
# dane2 =  as.data.frame(X[dzwignia<treshold,-1])#usuwamy wartoœci wp³ywowe i kolumne intercept
# y2 = y[dzwignia<treshold]#zmienna objasniana, tez bez obserwacji wplywowych
# model_backward2 = lm(y2~.,data = dane2 ) 
# summary(model_backward2) #widzimy spadek adjusted R^2
# bptest(model_backward)
# qchisq(0.95,8)
# bptest(model_backward2)
# qchisq(0.95,8) #nie wpadamy w zbiór, ale sprawdzimy jeszcze test odleg³oœci Cooka

#################################################
#########OBSERWACJE WPLYWOWE COOK#########
plot(cooks.distance(model_backward), xlab = "numer obserwacji",
     ylab = "wartosc statystyki Cooka",pch=16)
#usuwamy jeden region
dane_cook = dane[max(cooks.distance(model_backward)) != cooks.distance(model_backward),]
#dane_cook - macierz bez jednej zmiennej wplywowej
#reestymacja modelu
model_cook = update(model_backward, ~. , data=dane_cook)
View(dane_cook)

summary(model_cook)




######RESIDUA############

#analiza graficza
residua = model_cook$residuals
mean(residua)#srednia bledow jest bardzo bliska 0
y_hat = model_cook$fitted.values

#reszty vs wielkosc gospodadartwa
plot(dane_cook$gospodarstwo_2,residua, ylab = "wartoœæ residuów")

###TEST goldfelda quandta, h0: takie same wariancje, dla malych i duzych gospodarstw
#View(dane_cook)
K=7#liczba zm w modelu
X1 = as.data.frame(dane_cook[dane_cook$gospodarstwo_2 == "male",])
y1 = dane_cook$wydajnosc_krow[dane_cook$gospodarstwo_2 == "male"]
n1 = dim(X1)[1]
#View(X1)
X1 = X1[,-c(1,3,6,7)]
model_male = lm(y1~., data=X1)
summary(model_male)$sigma
summary(model_male)
X2 = as.data.frame(dane_cook[dane_cook$gospodarstwo_2 == "duze",])
y2 = dane_cook$wydajnosc_krow[dane_cook$gospodarstwo_2 == "duze"]
n2 = dim(X2)[1]
#View(X2)
X2 = X2[,-c(1,3,6,7)]
model_duze = lm(y2~., data=X2)
summary(model_duze)$sigma

GQ = summary(model_male)$sigma^2/summary(model_duze)$sigma^2#statystyka testowa
w_kryt = qf(0.95,n1-K, n2-K )
w_kryt  > GQ
GQ
1-pf(GQ,n1-K,n2-K)#pvalue
#brak podstaw do odrzucenia H0 o rownych wariancjach

par(mfrow=c(1,4))
#reszty vs y_hat
plot(y_hat, residua, main = "Wartoœci dopasowane")
abline(h=0)
#reszty vs pasze_obce
plot(dane_cook$pasze_obce, residua, main = "pasze_obce")
abline(h=0)
#reszty vs log_koszty_p_roslinnej
plot(dane_cook$log_koszty_p_roslinnej, residua, main = "log_koszty_p_zwierzecej")
abline(h=0)
#reszty vs log_uprawy_pastewne
plot(dane_cook$log_uprawy_pastewne, residua, main = "log_uprawy_pastewne")
abline(h=0)

#homoskedastycznosc
bptest(model_cook)


######  RESIDUA CD ############

#analiza graficza
residua = model_cook$residuals
mean(residua)#srednia bledow jest bardzo bliska 0
y_hat = model_cook$fitted.values
mb2 <- as.data.frame(model.matrix(model_cook)[,-1])

#wykres dopasowane wartoœci vs zaobserwowane wartoœci
y3 <- dane_cook$wydajnosc_krow
plot(y_hat,y3)
z <- c(1,2,3,4)
u <- c(1,2,3,4)
g <- lm(z~u)
abline(g,col="blue", lwd=2)
legend(2500, 9800, legend=c("f(x)=x"),
       col=c("blue"), lwd=2, cex=2)

#View(mb2)
#plot(residua)
#abline(h=0)
#plot(y_hat, residua)
#abline(h=0)
dev.off()


######  SPRAWDZANIE LINIOWEJ ZALE¯NOŒCI (Za³o¿enie A.2)  #########

#skorzystam z odwrotnosci vif czyli tolerance vector
tv1 = vif(model_cook)^-1
tv1
plot(tv1,pch=16,col=c("darkorange"), cex=1, xlab="Indeks zmiennej"
     ,ylab="Wartoœæ wsp³.tolerancji",ylim=c(0,1), main = "Model_cook")
abline(h=c(0.01,0.1),col="red")
#bardzo duzo zmiennych otrzymalo wartosc tv<0.1, ale ¿adna nie spad³a poni¿ej krytycznej #wartoœci 0.01

############NORMALNOSC BLEDOW################

par(mfrow=c(1,2))
residua = model_cook$residuals
qqnorm(residua)
qqline(residua,col="blue",lwd=2) # wydaje siê w porz¹dku
hist(residua,probability = TRUE)
curve(dnorm(x,0,sd(residua)),add = TRUE,col="blue",lwd=2) #pokrywa siê
sd(residua)
##library(fBasics)
##dagoTest(residua)

shapiro.test(residua) # nie mamy podstaw do odrzucenia H0 



######MOC PROGNOSTYCZNA MODELU##########


#####WALIDACJA KRZYZOWA###############
dane_cv = as.data.frame(cbind(dane_cook$wydajnosc_krow, model.matrix(model_cook)[,-1]))
View(dane_cv)
# tworze jakis obiekt do cross walidacji
train_control <- trainControl(method="repeatedcv", number=6, repeats = 3)
# trenuje model
model_cv <- train(V1~., data=dane_cv, trControl=train_control, method="lm")#V1 to nazwa pierwszej kolumny dane_cv
# roznego rodzaju wyniki
print(model_cv)

sd(model_cv$resample$Rsquared)
mean(model_cv$resample$Rsquared)


#######OCENA STABILNOSCI MODELU################3

#od tego momentu traktuje X jako macierz danych
View(dane_cook)
y = dane_cook$wydajnosc_krow
X = model.matrix(model_cook)[,-1]
X = as.data.frame(cbind(y, X))
View(X)
#odsetek obserwacji ktore zostawiamy
sample_size = as.integer(0.8*dim(X)[1])
#macierz na wyniki estymacji
beta_matrix = matrix(data = NA, ncol = 8, nrow = 1000)#bez wyrazu wolnego

#petla do obliczania wartosci parametrow
for (i in 1:1000){
  dane = X[sample(1:dim(X)[1], replace=FALSE, size = sample_size),]
  bety = lm(y~., data = dane)$coefficients[2:9]
  beta_matrix[i,] = bety
}
View(beta_matrix)
#tworze df z wynikami
wyniki = data.frame(matrix(NA, nrow = 8, ncol = 5),
                    row.names = colnames(X)[2:9])
colnames(wyniki) = c("wartosc_rzeczywista", "srednia","odchylenie",
                     "poczatek_przedzialu","koniec_przedzialu")
plot(beta_matrix[,2])
for (i in 1:8){
  wyniki[i,1] = model_cook$coefficients[i+1]
  wektor = beta_matrix[,i]
  odchylenie = sd(wektor)
  srednia = mean(wektor)
  wyniki[i,2] = srednia
  wyniki[i,3] = odchylenie
  wyniki[i,4] = quantile(wektor,0.025)
  wyniki[i,5] = quantile(wektor,0.975)
}
View(wyniki)
#print(xtable(wyniki, type = "latex"), file = another_path)

################LASSO###################
#przogotowanie danych
y = dane_cook$wydajnosc_krow
dane_lasso = dane_cook[,c(-1,-6)]
dane_lasso$gospodarstwo_2 = as.character(dane_cook$gospodarstwo_2)
dane_lasso$gospodarstwo_2[dane_cook$gospodarstwo_2 == "male" ] = 1#male gospodarstwa
dane_lasso$gospodarstwo_2[dane_cook$gospodarstwo_2 == "duze" ] = 0#duze gospodarstwa
dane_lasso$gospodarstwo_2 = as.numeric(dane_lasso$gospodarstwo_2)
dane_lasso$iloczyn_koszty_p_zwierzecej = dane_lasso$gospodarstwo_2*dane_lasso$log_koszty_p_roslinnej
dane_lasso$iloczyn_pasze_wlasne = dane_lasso$gospodarstwo_2*dane_lasso$pasze_wlasne
dane_lasso$iloczyn_log_uprawy_pastewne = dane_lasso$gospodarstwo_2*dane_lasso$log_uprawy_pastewne
View(dane_lasso)

#potrzebujemy macierz
X = as.matrix(dane_lasso)
View(X)

#tworze wektor parametrow lambda z ktorych bede wybieral najlepsza wartosc
lambdas <- 10^seq(-2, 2, by = 0.1)
lambdas
#uzywam cv aby wybrac najlepsza wartosc dla parametru lambda
lasso_reg <- cv.glmnet(X, y, alpha = 1, standardize = T,lambda = lambdas, nfolds = 6)


#najlepsza lambda
lambda_best <- lasso_reg$lambda.min#parametr minimalizujacy mean(cv error)
lambda_best

lasso_model <- glmnet(X, y, alpha = 1, lambda = lambda_best, standardize = TRUE)
summary(lasso_model)
as.matrix(lasso_model$beta)
lasso_model$a0#intercept
xtable(as.matrix(lasso_model$beta))#bety, odstandaryzowane



#####cross-validation dla regresji LASSO,analogicza metoda jak w rodziale CV

dane_cv_lasso = cbind(y,X)#dane

# trenuje model
model_cv_lasso <- train(y~., data=dane_cv_lasso, trControl=train_control, method="glmnet",
                        tuneGrid = expand.grid(alpha = 1,lambda = lambda_best), lambda = lambda_best)
# roznego rodzaju wyniki
sd(model_cv_lasso$resample$Rsquared)
mean(model_cv_lasso$resample$Rsquared)



