p_test=0.15
p_valid=0.1
p_train=1-p_test-p_valid
#lags=10
n_cores_flexcode=5
alpha_seq=c(0.05,0.20,0.50,0.80,0.95)

params = list(p_test=p_test,p_valid=p_valid,p_train=1-p_test-p_valid,
              n_cores_flexcode=n_cores_flexcode)

create_train_valid_test_sets = partial(create_train_valid_test_sets,p_valid=p_valid,p_test=p_test  )
test_methods = partial(test_methods,params=params,alpha_seq=alpha_seq)
final_loss_comparations = partial(final_loss_comparations,alpha_seq=alpha_seq)
