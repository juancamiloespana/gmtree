#' Split sample
#'
#' @description
#' Split sample in test and train.
#'
#' @param datos	a data.frame
#' @param perc percentile of training sample.
#'
#' @return
#' \code{split_sample} gives two samples
#' 
#' @examples
#' require(COUNT)
#' data(azpro)
#' sample <- split_sample(datos=azpro, perc=0.8)
#' 
#' @export
split_sample<-function(datos,perc)
{
  id_muestra<-sample(2,length(datos[,1]),replace = T,prob=c(perc,1-perc))
  fila<-seq(1:length(datos[,1]))
  muestra<-data.frame(id_muestra,fila)
  sub<-subset(muestra[,2],muestra$id_muestra==1)
  test<-subset(muestra$fila,muestra$id_muestra==2)
  datos_train<-datos[sub,]
  datos_test<-datos[test,]
  return(list(train=datos_train,test=datos_test))
}