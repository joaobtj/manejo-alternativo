

#salvar excel para fazer os gráficos
save_survival <- function(fits, file) {
  g <-
    cbind(summary(fits, c = T)$strata,
          summary(fits, c = T)$time,
          summary(fits, c = T)$surv)
  
  colnames(g) <- c("trat", "time", "surv")
  surv.s <- data.frame(rep(seq(0, max(g[, 2])), e = 2))
  colnames(surv.s) <- "tempo"
  for (i in unique(g[, 1])) {
    h <- g[which(g[, 1] == i),]
    surv.s[1, i + 1] <- 1
    if (is.null(dim(h))) {
      surv.s[n * 2 + 2, i + 1] <- h[3]
    } else {
      for (n in h[, 2]) {
        surv.s[n * 2 + 2, i + 1] <- h[which(h[, 2] == n), 3]
      }
    }
    for (h in 1:dim(surv.s)[1]) {
      if (!is.na(surv.s[h, i + 1])) {
        tmp <- surv.s[h, i + 1]
      } else{
        surv.s[h, i + 1] <- tmp
      }
    }
    surv.s
  }
  colnames(surv.s) <-
    c("tempo", unique(unlist(
      strsplit(
        levels(summary(fits, c = T)$strata),
        split = '=',
        fixed = TRUE
      )
    ))[-1])
  openxlsx::write.xlsx(surv.s, paste0("save-", file))
}



