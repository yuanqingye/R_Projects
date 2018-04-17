new_knn_imputation = function (data, k = 10, scale = T, meth = "weighAvg", distData = NULL)
{
  n <- nrow(data)
  if (!is.null(distData)) {
    distInit <- n + 1
    data <- rbind(data, distData)
  }
  else distInit <- 1
  N <- nrow(data)
  ncol <- ncol(data)
  nomAttrs <- rep(F, ncol)
  for (i in seq(ncol)) nomAttrs[i] <- is.factor(data[, i])
  nomAttrs <- which(nomAttrs)
  hasNom <- length(nomAttrs)
  contAttrs <- setdiff(seq(ncol), nomAttrs)
  dm <- data
  if (scale)
    dm[, contAttrs] <- scale(dm[, contAttrs])
  if (hasNom)
    for (i in nomAttrs) dm[, i] <- as.integer(dm[, i])
  dm <- as.matrix(dm)
  nas <- which(!complete.cases(dm))
  if (!is.null(distData))
    tgt.nas <- nas[nas <= n]
  else tgt.nas <- nas
  if (length(tgt.nas) == 0)
    warning("No case has missing values. Stopping as there is nothing to do.")
  xcomplete <- dm[setdiff(distInit:N, nas), ]
  if (nrow(xcomplete) < k)
    stop("Not sufficient complete cases for computing neighbors.")
  for (i in tgt.nas) {
    tgtAs <- which(is.na(dm[i, ]))
    dist <- scale(xcomplete, dm[i, ], FALSE)
    xnom <- setdiff(nomAttrs, tgtAs)
    if (length(xnom))
      dist[, xnom] <- ifelse(dist[, xnom] > 0, 1, dist[,
                                                       xnom])
    dist <- dist[, -tgtAs]
    dist <- sqrt(drop(dist^2 %*% rep(1, ncol(dist))))
    ks <- order(dist)[seq(k)]
    for (j in tgtAs) if (meth == "median")
      data[i, j] <- centralValue(data[setdiff(distInit:N,
                                              nas), j][ks])
    else data[i, j] <- centralValue(data[setdiff(distInit:N,
                                                 nas), j][ks], exp(-dist[ks]))
  }
  data[1:n, ]
}