# require(QAR)
qar_loss = function(ytrain, ytest, alpha_seq)
{
  qar_output = QAR.run(ytrain, ytest, alpha_seq, L = 10)
  qar_loss = c()
  for (i in 1:length(alpha_seq))
  {
    qar_loss = c(qar_loss, qar_output[[i]][[2]])
  }
  list(cdeloss = NULL, pbloss = qar_loss)
}

# source(NNKCDE.R)
nnkcde_loss = function(train_valid_test_sets, alpha_seq)
{
  nnkcde_output= NNKCDE.train(train_valid_test_sets, alpha_seq)
  z_grid=nnkcde_output$output$z_grid
  cdes=nnkcde_output$output$cdes
  this_pbloss = NNKCDE.pbloss(nnkcde_output$output)
  this_cdeloss = cdeloss(ytest, z_grid, cdes)
  list(cdeloss = this_cdeloss, pbloss = this_pbloss)
}
