#Data frame that includes information for all simulation scenarios
#N: sample size
#p: prevalence
#korr: correlation
#pm: proportion missing
#mech: missingness mechanism
#c: cut-off
#mu: mean of "sick" population distribution

grid = expand.grid(
  N = c(400, 800, 1600)
  , p = c(0.1, 0.2, 0.4)
  , korr = c(sin(pi*0.2))
  , pm = c(0.1, 0.3, 0.5)
  , mech = c("MCAR", "MAR", "MNAR")
  , stringsAsFactors = FALSE
)

c <- c(rep(qnorm(0.7), length(grid$N)), 
       rep(qnorm(0.7), length(grid$N)), 
       rep(qnorm(0.7), length(grid$N)), 
       rep(qnorm(0.8), length(grid$N)), 
       rep(qnorm(0.8), length(grid$N)), 
       rep(qnorm(0.8), length(grid$N)), 
       rep(qnorm(0.9), length(grid$N)), 
       rep(qnorm(0.9), length(grid$N)), 
       rep(qnorm(0.9), length(grid$N)))


mu <- c(rep(qnorm(0.7) - qnorm(0.1), length(grid$N)), 
        rep(qnorm(0.7) - qnorm(0.2), length(grid$N)), 
        rep(qnorm(0.7) - qnorm(0.3), length(grid$N)), 
        rep(qnorm(0.8) - qnorm(0.1), length(grid$N)), 
        rep(qnorm(0.8) - qnorm(0.2), length(grid$N)), 
        rep(qnorm(0.8) - qnorm(0.3), length(grid$N)), 
        rep(qnorm(0.9) - qnorm(0.1), length(grid$N)), 
        rep(qnorm(0.9) - qnorm(0.2), length(grid$N)), 
        rep(qnorm(0.9) - qnorm(0.3), length(grid$N)))

grid <- rbind(grid, grid, grid,
              grid, grid, grid,
              grid, grid, grid)

grid$c <- c
grid$mu <- mu





