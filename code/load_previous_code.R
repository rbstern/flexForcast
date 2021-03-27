t_1 = function(x) x
t_2 = function(x) x^2
methods = list(t_1, t_2)
nomes = c("1", "2")
for(t_method in methods)
{
  aux = t_method(...)
  pb_losses = cbind(pb_losses, aux$pbloss)
  cde_losses = cbind(cde_losses, aux$cdeloss)
}
