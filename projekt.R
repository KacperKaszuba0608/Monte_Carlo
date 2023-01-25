set.seed(123)

alpha = 0.05
n = 30

data = rnorm(n)

iterations = seq(1, 1000)
p_values = c()
powers = c()
blad_II_r = c()

#Metoda MC dla Shpairo-Wilka
for (iter in iterations) {
  test1 = numeric(iter)
  test2 = numeric(iter)
  for (j in 1:iter) {
    x = rnorm(n,0,10)
    y = rexp(n)
    test1[j] = shapiro.test(x)$p.value
    test2[j] = as.integer(shapiro.test(y)$p.value <= alpha)
  }
  p_values = append(p_values, mean(test1))
  powers = append(powers, mean(test2))
  blad_II_r = append(blad_II_r, 1-mean(test2))
}

dane1 = data.frame(cbind(iterations, p_values))
dane1$alpha_status <- ifelse(dane1$p_values > alpha, "p-value > 0.05", "p-value < 0.05")
dane2 = data.frame(cbind(iterations, powers))
dane3 = data.frame(cbind(iterations, blad_II_r))

library(ggplot2)

# Wykres p-value w zależności do iteracji
ggplot(data = dane1, aes(x = iterations, y = p_values)) +
  geom_point(aes(color = alpha_status)) +
  ggtitle('p-value testu w zależności od ilości iteracji') +
  xlab('iteracja') +
  ylab('p-value') +
  ylim(-0.1, 1) +
  scale_color_manual(name = "", values = c("p-value > 0.05" = "darkgreen", "p-value < 0.05" = "red")) +
  theme(legend.position = 'right')

# Wykres błędu II rodzaju i mocy testu w zależności od iteracji

ggplot(data = dane2, aes(x = iterations, y = powers, color = 'Moc testu')) +
  geom_line()+
  geom_line(data = dane3, aes(x = iterations, y = blad_II_r, color = 'Błąd II rodzaju'))+
  ggtitle('Błąd II rodzaju i moc testu w zależności od ilości iteracji') +
  xlab('iteracja') +
  ylab('') +
  ylim(-0.01, 1) +
  scale_color_manual(name = "", values = c("Moc testu" = "red", "Błąd II rodzaju" = "orange"))+
  theme(legend.position = 'right')

# Wykres błędu II rodzaju
ggplot (data = dane2, aes(x = iterations, y = blad_II_r))+
  geom_line(col = 'orange')+
  ggtitle('Błąd II rodzaju testu w zależności od ilości iteracji') +
  xlab('iteracja') +
  ylab('') +
  ylim(-0.01, 1)

# Wykres mocy testu
ggplot (data = dane3, aes(x = iterations, y = powers))+
  geom_line(col = 'red')+
  ggtitle('Moc testu w zależności od ilości iteracji') +
  xlab('iteracja') +
  ylab('') +
  ylim(-0.01, 1)

# Wykres błędu bezwzględnego
gen_mc <- function(n_iter){
  # Przechowuj statystyki testowe
  pv = numeric(n_iter)
  # Przeprowadź test Shapiro-Wilka wielokrotnie
  for (i in 1:n_iter){
    random_data = rnorm(30)
    p = shapiro.test(random_data)$p.value
    pv[i] = p
    
  result = abs(shapiro.test(data)$p.value - mean(pv))
  return(result)
    }
}

it = 1:500#seq(1, 100001, 500)
bbez_lst = c()

for (n in it){
  bezwzgledny = gen_mc(n)
  bbez_lst = append(bbez_lst, bezwzgledny)
}

data_bbez = data.frame('iteracje'=it, 'bbez_lst'=bbez_lst)

ggplot(data = data_bbez, aes(x = iteracje, y = bbez_lst))+
  geom_point(col = 'red', alpha = 0.8)+
  geom_line(col = 'grey', alpha = 0.8)+
  ggtitle('Wykres błędu bezwzględnego')+
  xlab('Iteracja')+
  ylab('Błąd bezwzględny')+
  ylim(-0.01, 1)

#Wykres czasu potrzebnego na daną iterację
gen_mc_time <- function(n_iter){
  # Przechowuj statystyki testowe
  pv = numeric(n_iter)
  # Przeprowadź test Shapiro-Wilka wielokrotnie
  for (i in 1:n_iter){
    random_data = rnorm(100)
    p = shapiro.test(random_data)$p.value
    pv[i] = p
  }
}

it = seq(1, 1100000, 100000)
time_lst = c()
for (n in it){
  start = Sys.time()
  gen_mc_time(n)
  stop = Sys.time()
  time_lst = append(time_lst, stop-start)
}

data_time = data.frame('iteracje'=it, 'czas'=time_lst)

ggplot(data = data_time, aes(x = iteracje, y = czas))+
  geom_point(col = 'red', alpha = 0.8)+
  geom_line(col = 'blue', alpha = 0.5)+
  ylab('czas [s]')+
  ggtitle('Złożoność obliczeniowa metody Monte Carlo zobrazowana w czasie')








