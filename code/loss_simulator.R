simulation_run = function(simulator, n_iter = 100)
{
  data <- simulator()
  dfloss <- test_all(data)
  for(i in 2:n_iter)
  {
    print(paste("Iteration: ", i, "/", n_iter, sep = ""))
    dfloss <- rbind(dfloss, test_all(data))
  }
}