str(HBAT)
designMat <- HBAT[,c(2, 4, 14:18)]
str(designMat)

Mult.Twoway.MANOVA <-function(A, B, R, Data, alpha){
  A.n <- table(A)  #A 요인 처리별 표본의 수
  A.n
  B.n <- table(B)  #B 요인 처리별 표본의 수
  B.n
  g <- length(A.n) #A 요인 처리수
  g
  b <- length(B.n) #B 요인 처리수
  b
  n <- max(R)  #케이스
  n
  N <- dim(Data)[1]  #전체 자료의 수, Data는 종속변수
  N
  p <- dim(Data)[2]  #변수의 수
  p
  X <- as.matrix(Data)  #행렬계산을 위해 행렬로 변환
  #처리별 평균벡터
  bar.A <- aggregate(X~A, FUN = mean)[,-1]
  bar.B <- aggregate(X~B, FUN = mean)[,-1]
  bar.AB <- aggregate(X~B+A, FUN = mean)[,c(-1,-2)]
  bar <- colMeans(X)  #전체평균
  #행렬계산을 위해 평균행렬 행렬변환
  bar.A <- as.matrix(bar.A)
  bar.B <- as.matrix(bar.B)
  bar.AB <- as.matrix(bar.AB)
  
  #제곱합과 교차곱행렬
  SSPA <- matrix(rep(0,p^2), ncol=p)
  for(i in 1:g){
    EQ <- bar.A[,i]-bar
    SSPA <- SSPA + b*n*(EQ%*%t(EQ))
  }
  SSPB <- matrix(rep(0,p^2), ncol=p)
  for(i in 1:b){
    EQ <- bar.B[i,]-bar
    SSPB <- SSPB + g*n*(EQ%*%t(EQ))
  }
  SSPint <- matrix(rep(0,p^2), ncol=p)
  for(i in 1:g){
    for(j in 1:b){
      EQ <- bar.AB[(i-1)*b+j,]-bar.A[i,]-bar.B[j,]+bar
      SSPint <- SSPint + n*(EQ%*%t(EQ))
    }
  }
  SSPE <- matrix(rep(0,p^2), ncol=p)
  for(i in 1:N){
    EQ <- X[i,]-bar.AB[((i-1)%/%R)+1,]
    SSPE <- SSPE+(EQ%*%t(EQ))
  }
  SSPcor <- matrix(rep(0,p^2), ncol=p)
  for(i in 1:N){
    EQ <- X[i,]-bar
    SSPcor <- SSPcor+(EQ%*%t(EQ))
  }
  SSPdf <- c(g-1,b-1,(g-1)*(b-1),g*b*(n-1),g*b*n-1)
  names(SSPdf) <- c("A","B","AxB","E","T")
  
  #검정통계량 람다
  Lambda1 <- det(SSPE)/(det(SSPA+SSPE))
  Lambda2 <- det(SSPE)/(det(SSPB+SSPE))
  Lambda12 <- det(SSPE)/(det(SSPint+SSPE))
  
  #근사 카이제곱 검정통계량
  Lambda1P <- (-((g*b*(n-1))-(p+1-(g-1))/2))*log(Lambda1)
  Lambda2P <- (-((g*b*(n-1))-(p+1-(b-1))/2))*log(Lambda2)
  Lambda12P <- (-((g*b*(n-1))-(p+1-(g-1)*(b-1))/2))*log(Lambda12)
  
  #기각값
  chi1 <- qchisq(1-alpha, (g-1)*p)
  chi2 <- qchisq(1-alpha, (b-1)*p)
  chi12 <- qchisq(1-alpha, (g-1)*(b-1)*p)
  
  #p-값
  pvalue1 <- pchisq(Lambda1P, (g-1)*p, lower.tail = FALSE)
  pvalue2 <- pchisq(Lambda2P, (b-1)*p, lower.tail = FALSE)
  pvalue12 <- pchisq(Lambda12P, (g-1)*(b-1)*p, lower.tail = FALSE)
  
  #검정통계량표
  Lambda <- rbind(Lambda1, Lambda2, Lambda12)
  LambdaP <- rbind(Lambda1P, Lambda2P, Lambda12P)
  df <- rbind((g-1)*p, (b-1)*p, (g-1)*(b-1)*p)
  CR <- rbind(chi1, chi2, chi12)
  pvalue <- rbind(pvalue1, pvalue2, pvalue12)
  Lambda.Table <- cbind(Lambda, LambdaP, df, CR, pvalue)
  colnames(Lambda.Table) <- c("람다", "람다*", "df", "critical value", "p-value")
  rownames(Lambda.Table) <- c("A", "B", "AxB")
  
  return(list(SSPA=SSPA, SSPB=SSPB,
              SSPint=SSPint, SSPE=SSPE,
              SSPcor=SSPcor, SSPdf=SSPdf,
              Stat=Lambda.Table))
}

