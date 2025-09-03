# Simple SIR model with base R
sir_model <- function(beta=0.3, gamma=0.1, S0=999, I0=1, R0=0, days=50) {
  S <- numeric(days)
  I <- numeric(days)
  R <- numeric(days)
  
  S[1] <- S0; I[1] <- I0; R[1] <- R0
  
  for (t in 2:days) {
    new_infections <- beta * S[t-1] * I[t-1] / (S0+I0+R0)
    new_recoveries <- gamma * I[t-1]
    
    S[t] <- S[t-1] - new_infections
    I[t] <- I[t-1] + new_infections - new_recoveries
    R[t] <- R[t-1] + new_recoveries
  }
  
  data.frame(day=1:days, S=S, I=I, R=R)
}

result <- sir_model()
print(head(result))
