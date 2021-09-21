
#calcula a aaccm e retorna um data.frame com a aaccm na última coluna
#datb: data.frame com os valores médios do crescimento micelial
#os nomes das colunas devem ser o tempo em que ocorreu cada medição
#t.end: até que dia calcular a aaccm. Geramlmente, o momento em que um tratamento preencheu a placa
#colini: colunas iniciais (normalmente 2: trat+rep)

aaccm.calc <-
  function(datb,
           name = "aaccm",
           t.end = colnames(datb)[length(datb)],
           colini = 2) {
    aaccm.d <- rep(0, 1, dim(datb)[1])
    for (i in (colini + 1):(which(colnames(datb) == t.end) - 1)) {
      aaccm.d <- cbind(aaccm.d,
                       (datb[i + 1] + datb[i]) / 2 *
                         (as.numeric(colnames(datb)[i + 1]) - as.numeric(colnames(datb)[i]))
      )
    }
    
    aaccm.sum <- data.frame(apply(aaccm.d, 1, FUN = "sum"))
    colnames(aaccm.sum) <- name
    datb <- cbind(datb[, 1:colini], aaccm.sum)
    
    return(datb)
    
  }
