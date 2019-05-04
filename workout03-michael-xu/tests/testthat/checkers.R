package("binomial")

check_prob(.5)
check_prob(1.4)
check_prob(-32)

check_trials(-3)
check_trials(5)
check_trials(0)

check_success(3, 10)
check_success(10, 4)
check_success(-1, 10)