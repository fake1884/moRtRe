# In dieser Datei sind die Funktionen um eine Sterbetafel wie die DAV es vorschlägt zu
# bestimmen.

##########################################################################################
Whittaker = function(DataIn){
  # Beschreibung : Diese Funktion glättet die vorgegebenen Daten (DataIn) mittels des
  #                Whittaker-Henderson-Verfahrens

  u = DataIn
  x = 0:(length(DataIn)-1)
  n = length(DataIn)
  p = 3 # is this correct?
  lambdas = 0:10 # this is where the possible values of lambda are checked
  lambdas = 2^lambdas
  GCV = rep(NA, length(lambdas))
  vCandidates = matrix(rep(NA, length(lambdas) * length(u)), ncol = length(lambdas))
  for(k in 1:length(lambdas)){
    I = diag(n)
    K = matrix(rep(0,n*(n-p)), nrow = (n-p), ncol = n)
    for(i in 1:(n-p)){
      for(j in 1:n){
        if(i <= p || j >= i){K[i,j] = (-1)^(p+j-i)*choose(p, j-i)}
      }
    }
    B = I + lambdas[k]*t(K)%*%K
    vCandidates[,k] = solve(B) %*% u

    GCV[k] =  GCV_function(u, vCandidates[,k], n, B)
  }

  return(vCandidates[,which.min(GCV)])
}

GCV_function = function(u, v, n, B){
  # this function has to be minimized to find the best value of lambda
  return( (1/n) * t(u-v)%*%(u-v) * (1-sum(diag(solve(B)))/n)^{-2} )
}

##########################################################################################
Lee = function(Geburtsjahre, Alter, Deathrates){
  # Beschreibung : Diese Funktion schätzt die Parameter im Lee-Carter Modell, wie in dem
  #                Paper von Girosi und King vorgeschlagen.

  # initialize constants
  M = matrix(rep(NA, length(Geburtsjahre) * length(Alter)), nrow = length(Alter),
                 ncol = length(Geburtsjahre))
  m_bar_a = rep(NA, length(Alter))

  # calculate the M matrix
  Deathrates[Deathrates < 0] = 0
  for(i in 1:length(Alter)){
    m_at = log(subset(Deathrates[,3], Deathrates[,2] == -1+i))
    # setting -Inf to zero might not be the best idea TODO
    m_at[m_at == "-Inf"] = 0
    m_bar_a[i] = mean(m_at)
    M[i,] = m_at - m_bar_a[i]
  }
  alpha_a = m_bar_a

  # estimate gamma
  gamma_t = rep(NA, length(Geburtsjahre))
  for(i in 1:length(Geburtsjahre)){
    m_at = log(subset(Deathrates[,3], Deathrates[,1] == min(Geburtsjahre)-1+i))
    # setting -Inf to zero might not be the best idea TODO
    m_at[m_at == "-Inf"] = 0
    gamma_t[i] = sum(m_at - m_bar_a)
  }

  # estimate beta
  svd_M = svd(M)
  B = svd_M$u
  beta_a_1 = B[,which.max(svd_M$d)]/ sum(B[,which.max(svd_M$d)])

  prod = rep(NA, length(Alter))
  for(i in 1:length(Alter)){
    m_at = log(subset(Deathrates[,3], Deathrates[,2] == -1+i))
    # setting -Inf to zero might not be the best idea TODO
    m_at[m_at == "-Inf"] = 0
    prod[i] = sum(m_at * gamma_t)
  }
  beta_a = prod * sum(gamma_t^2)^(-1)
  # plot(Alter, beta_a_1, type = "l")
  # lines(Alter, beta_a, lty = "dashed")
  # lines(Alter, beta_data, lty = "dotted")

  # estimate nu
  nu = (gamma_t[length(gamma_t)]-gamma_t[1])/(max(Geburtsjahre)-min(Geburtsjahre))

  return(list(alpha_a = alpha_a, beta_a = beta_a, gamma_t = gamma_t, nu = nu))
}


##########################################################################################
MakePlotDAV = function (){
  # Beschreibung : This function calls the other functions in this file to generate plots

  ######################################
  # Whittaker
  v = Whittaker(simple_data[,4]/sum(simple_data[,4]))
  X = 0:95
  mu = mu_data
  sigma = sigma_data
  Y = (2*pi*sigma^2)^(-1/2) * exp(- (X-mu)^2/(2*sigma^2))

  pdf("../../1 Doku/graphics/Whittaker_SampleData.pdf", width = 10, height = 8)
  plot(X, simple_data[,4]/sum(simple_data[,4]), type = "l",
       xlab = "Alter", ylab = "Sterbewahrscheinlichkeit")
  lines(X, v, lty = 2)
  lines(X, Y, lty = 3)
  legend("topleft", legend=c("Daten", "Whittaker", "Model"),
         col=c("black", "black", "black"), lty=1:3, cex=0.8)
  dev.off()

  #######################################################################
  # Lee-Carter
  Geburtsjahre = min(deathrates1965west[,1]):max(deathrates1965west[,1])
  Alter = min(deathrates1965west[,2]):max(deathrates1965west[,2])
  Deathrates = deathrates1965west
  erg = Lee(Geburtsjahre, Alter, Deathrates)
  alpha_a = erg$alpha_a
  beta_a = erg$beta_a
  gamma_t = erg$gamma_t
  nu = erg$nu

  # make a plot of alpha
  pdf("../../1 Doku/graphics/Lee-Carter-alpha.pdf", width = 10, height = 8)
  plot(0:95, exp(alpha_a), type = "l", xlab = "Alter", ylab = "Sterblichkeit")
  lines(0:95, exp(alpha_a - 1), lty = "dashed")
  lines(0:95, exp(alpha_a + 1), lty = "dotted")
  legend("topleft", legend=c("exp(alpha)", "exp(alpha - 1)", "exp(alpha + 1)"),
         col=c("black", "black", "black"), lty=1:3, cex=1.5)
  dev.off()

  # make a plot of beta
  pdf("../../1 Doku/graphics/Lee-Carter-beta.pdf", width = 10, height = 8)
  plot(0:95, beta_a, type = "l", xlab = "Alter", ylab = "Beta")
  abline(v = which.min(beta_a[1:20])-1)
  abline(v = which.min(beta_a[21:70])-1+20)
  abline(v = which.min(beta_a[61:99])-1+60)
  segments(x0=which.max(beta_a[60:90])+60, y0=-0.05,
           x1=which.max(beta_a[60:90])+60, y1=max(beta_a[60:90]), lty="dashed")
  dev.off()

  # make a plot of gamma
  pdf("../../1 Doku/graphics/Lee-Carter-gamma.pdf", width = 10, height = 8)
  plot(1956:2017, gamma_t, type = "l",
       xlab = "Geburtsjahre", ylab = "gamma_t")
  lines(1956:2017, rep(0, length(1956:2017)))
  dev.off()

  # make a heatmap plot
  pdf("../../1 Doku/graphics/beta-gamma-heatmap.pdf", width = 10, height = 8)
  gamma_vec = matrix(gamma_t, ncol = 1)
  beta_vec = matrix(beta_a, nrow = 1)
  data = matrix(data = t(beta_vec) %*% t(gamma_vec) , nrow = length(0:95),
                ncol = length(1956:2017), dimnames = list(0:95, 1956:2017))
  data = data[,62:1]
  library(lattice)
  levelplot(data, xlim = c(0:95), ylim = c(0:61),
            xlab = "Alter", ylab = "Geburtsjahr nach 1956",
            col.regions = grey((110*61):0/(110*61)),
            at=seq(min(data), max(data), abs(max(data)-min(data))/20))
  dev.off()

  # make the beta-gamma plot
  pdf("../../1 Doku/graphics/beta-gamma.pdf", width = 10, height = 8)
  plot(Alter, data[,1], type = "l", ylim = c(min(data[,1]), max(data[,62])),
       ylab = "beta * gamma")
  abline(v = which.min(beta_a[1:20])-1)
  abline(v = which.min(beta_a[21:70])-1+20)
  abline(v = which.min(beta_a[61:99])-1+60)
  lines(Alter, data[,62])
  lines(Alter, rep(0, length(Alter)), lty = "dotted")
  segments(x0=which.max(beta_a[60:90])+60, y0=max(beta_a[60:90])*gamma_t[1],
           x1=which.max(beta_a[60:90])+60, y1=max(beta_a[60:90])*gamma_t[62], lty="dashed")
  dev.off()
}
