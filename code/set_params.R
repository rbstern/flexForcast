p_test=0.15
p_valid=0.1
p_train=1-p_test-p_valid
lags=10
L=10
n_cores_flexcode=8

params = list(p_test=p_test,p_valid=p_valid,p_train=1-p_test-p_valid,
              lags=lags,L=L,n_cores_flexcode=n_cores_flexcode)

create_train_valid_test_sets = partial(create_train_valid_test_sets,p_valid,p_test,lags)
test_methods = partial(test_methods,params=params,alpha_seq=alpha_seq)
