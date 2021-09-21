
#organizar os dados a partir da planilha do excel para executar a análise de sobrevivência
#m: data.set com os dados
#ini: coluna em que estão os tratamentos
#mid: Colunas em que estão as repetições e o número de indivíduos
#end: colunas em que estão as datas
#cen: coluna em que está a censura

m=data1

data.sobrev <- function(m, ini, mid, end, cen) {
  t.ini <- m[, ini]
  t.mid <- m[, mid]
  t.end <- m[, end]
  t.cen <- m[, cen]
  datas <- as.numeric(colnames(t.end))
  d0 <- datas[1]
  dmax <- rev(datas)[1]
  dt <- data.frame()
  for (i in 1:dim(t.end)[1]) {
    # i=16
    t <- t.ini[i, ]
    r <- t.mid[i, ]
    t.data <- t.ocor <- c()
    for (j in 1:(length(datas) - 1)) {
      t.data[j] <- datas[j + 1] - d0
      t.ocor[j] <- as.numeric(t.end[i, j + 1] - t.end[i, j])
    }
    ocor <- rep(t.data[-which(t.ocor == 0)],
                t.ocor[-which(t.ocor == 0)]) #tirar os dias que não ocorreu (==0) e repetir o vetor
    #censuras
    if (t.cen[i, 1] == 0 & length(ocor) == 0) {
      #ocorreu total
      cens <- rep(1, length(ocor))
    } else if (t.cen[i, 1] == 0 | length(ocor) != 0) {
      #ocorreu parcial
      cens <- rep(1, length(ocor))
      cens <- c(cens, rep(0, t.cen[i, 1]))
    } else if (t.cen[i, 1] != 0 & length(ocor) == 0) {
      #não ocorreu total
      cens <- rep(0, t.cen[i, 1])
    }

   
   #apend novas linhas com as informações
   if (t.cen[i, 1] == 0 & length(ocor) == 0) { #ocorreu total
     for (k in 1:length(cens)) {
       dt <- rbind.data.frame(dt,
                              cbind(t,  r, tm = ocor[k], cs = cens[k]))
     }    
   } else if (t.cen[i, 1] == 0 | length(ocor) != 0) { #ocorreu parcial
     for (k in which(cens != 0)) {
       dt <- rbind.data.frame(dt,
                              cbind(t,  r, tm = ocor[k], cs = cens[k]))
     }
     for (k in which(cens == 0)) {
       dt <- rbind.data.frame(dt,
                              cbind(t, r, tm = dmax - d0, cs = cens[k]))
     }
   } else if (t.cen[i, 1] != 0 & length(ocor) == 0) { #não ocorreu total
     for (k in 1:length(cens)) {
       dt <- rbind.data.frame(dt,
                              cbind(t, r, tm = dmax - d0, cs = cens[k]))
     }
   }
   
   
   
    #  #apend novas linhas com as informações
    # if (length(ocor) != 0) { #ocorreu total
    #   for (k in 1:length(cens)) {
    #     dt <- rbind.data.frame(dt,
    #                            cbind(t,  r, tm = ocor[k], cs = cens[k]))
    #   }
    # } else {
    #   for (k in 1:length(cens)) {
    #     dt <- rbind.data.frame(dt,
    #                            cbind(t, r, tm = dmax - d0, cs = cens[k]))
    #   }
    # }
  }
  return(dt)
}


