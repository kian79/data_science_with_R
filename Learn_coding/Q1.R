# algorithm for doubly even numbers (4,8,12,...,4*n)
doubly_enen_magic_square <- function(n){
  magic_sq = t(matrix(1:n**2,n,n))
  for (i in 1:(n/4))
    for (j in 1:(n/4))
      magic_sq[i,j] = n*n+1 - magic_sq[i,j]
  for (i in 1:(n/4))
    for (j in ((3*(n/4))+1):n)
      magic_sq[i,j] = n*n+1 - magic_sq[i,j]
  for (i in (3*(n/4)+1):n)
    for (j in 1:(n/4))
      magic_sq[i,j] = n*n+1 - magic_sq[i,j]
  for (i in (3*(n/4)+1):n)
    for (j in (3*(n/4)+1):n)
      magic_sq[i,j] = n*n+1 - magic_sq[i,j]
  for (i in ((n/4)+1):(3*(n/4)))
    for (j in ((n/4)+1):(3*(n/4)))
      magic_sq[i,j] = n*n+1 - magic_sq[i,j]
  return(magic_sq)
}
magic_square <- function(n) {
  if (n %% 2 == 1) { #for odd numbers
    p <- (n + 1) %/% 2 - 2
    ii <- seq(n)
    outer(ii, ii, function(i, j) n * ((i + j + p) %% n) + (i + 2 * (j - 1)) %% n + 1)
  } else if (n %% 4 == 0) { #for doubly even numbers 
    doubly_enen_magic_square(n)
  } else { #for even numbers
    p <- n %/% 2
    q <- p * p
    k <- (n - 2) %/% 4 + 1
    a <- Recall(p)
    a <- rbind(cbind(a, a + 2 * q), cbind(a + 3 * q, a + q))
    ii <- seq(p)
    jj <- c(seq(k - 1), seq(length.out=k - 2, to=n))
    m <- a[ii, jj]; a[ii, jj] <- a[ii + p, jj]; a[ii + p, jj] <- m
    jj <- c(1, k)
    m <- a[k, jj]; a[k, jj] <- a[k + p, jj]; a[k + p, jj] <- m
    a
  }
}
ans_1 = magic_square(4)
ans_2 = magic_square(5)
ans_3 = magic_square(6)
save(ans_1,ans_2,ans_3, file='1273168143_1.RData')
ans_1
ans_2
ans_3
