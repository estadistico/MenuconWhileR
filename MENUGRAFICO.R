download.file( url = "https://raw.githubusercontent.com/jboscomendoza/r-principiantes-bookdown/master/datos/bank.csv", destfile = "bank.csv")

#I
banco <- read.csv(file = "bank.csv", sep = ";", header = TRUE)

#II Transformar los datos char a factor
str(banco)
banco$job<-as.factor(banco$job)
banco$marital<-as.factor(banco$marital)
banco$education<-as.factor(banco$education)
banco$default<-as.factor(banco$default)
banco$housing<-as.factor(banco$housing)
banco$loan<-as.factor(banco$loan)
banco$contact<-as.factor(banco$contact)
banco$month<-as.factor(banco$month)
banco$poutcome<-as.factor(banco$poutcome)
banco$y<-as.factor(banco$y)
#ordenar factor una sola vez
banco$poutcome<-factor(banco$poutcome,levels(banco$poutcome)[c(4,1,2,3)])
str(banco)

### menu

Opcion=0
while (Opcion!=7) {
  
  
  cat("1.Realizar un resumen de las variables de la base de datos.
2. Mostrar un gr?fico de tipo Histograma (especificar qu? se muestra)
3. Mostrar un gr?fico de tipo barra (especificar qu? se muestra)
4. Mostrar un gr?fico de tipo dispersi?n (especificar qu? se muestra)
5. Mostrar un gr?fico de bigote (especificar qu? se muestra)
6. Mostrar un gr?fico de mosaico (especificar qu? se muestra)
7. Salir
      ")
  Opcion=readline(prompt="Seleccionar un numero del menu (1 al 7): ")
  Opcion<-as.numeric(Opcion)
  if(Opcion==1){
     print(summary(banco))
    
  } else if(Opcion==2){
    str(banco)
    print(hist(banco$duration, main="Histograma de la duraci?n",col="blue1",
               xlab = "Duraci?n", ylab="Frecuencia"), xlim=c(100,2600))
  
      
  } else if(Opcion==3) {
    
    
    print(barplot(table(banco$poutcome), col = c("dodgerblue4","firebrick","darkgray","gold4"),
                  ylab="Frecuencia",main="Gr?fico de barras de poutcome",
                  xlab="poutcome", ylim=c(0,4000) ))
    print(legend("topright",legend = row.names(table(banco$poutcome)),title = "poutcome",
           fill = c("dodgerblue4","firebrick","darkgray","gold4"), cex=0.55,box.lty=0 ))
    
    #legend=row.names(table(banco$poutcome)
  } else if(Opcion==4) {
    
        print(plot(banco$balance, banco$duration,col=banco$loan, main="Dispersion: Balance vs Duraci?n seg?n Loan", 
                   xlab="Balance", ylab="Durac?on", ))
    print(legend("topright",legend = c("No","Si"), title = "Loan",
                 fill = c("Black", "Red"),inset=0.02 , cex=0.55,box.lty=0 ))
    
  } else if(Opcion==5) {
    
    print(boxplot(banco$age~banco$contact , main="Gr?fico de bigote: Edad",
                  xlab="Contact", ylab="Edades",col=c("chocolate","chartreuse4","coral4") ))
      
    print(legend("topright",legend = row.names(table(banco$contact)),
                 fill = c("chocolate","chartreuse4","coral4"), 
                 cex=0.50,box.lty=0,bg="transparent",horiz=FALSE ))
    
  } else if(Opcion==6){
    print( plot(x = banco$marital, y = banco$education,
                col = c("chocolate","chartreuse4","coral4","cyan3"),
                xlab="Estado Marital",ylab="Educacion", main="Mosaico: Educacion vs Estado marital",
                legend = row.names(banco$education)))
    print(legend("center",legend = row.names(table(banco$education)),
                 fill = c("chocolate","chartreuse4","coral4","cyan3"), 
                 cex=0.50,box.lty=0,bg="transparent",horiz=FALSE ))
    } else if (Opcion==7) {
    
    print("salir")
    } else {
    
      print("Por favor ingrese un numero entre 1 al 7")
  }
  
} #fin del while


