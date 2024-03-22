echo "----- N = 1 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(zero,x)) == f(p(x,zero))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(zero,x)) == f(p(x,zero))]"

echo "----- N = 2 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(zero,x))) == f(p(p(x,zero),p(x,zero)))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(zero,x))) == f(p(p(x,zero),p(x,zero)))]"

echo "----- N = 3 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(zero,x)))) == f(p(p(x,zero),p(p(x,zero),p(x,zero))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(zero,x)))) == f(p(p(x,zero),p(p(x,zero),p(x,zero))))]"

echo "----- N = 4 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))]"

echo "----- N = 5 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x)))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero))))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x)))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero))))))]"

echo "----- N = 6 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))))]"

echo "----- N = 7 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x)))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero))))))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x)))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero))))))))]"

echo "----- N = 8 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))))))]"

echo "----- N = 9 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x)))))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero))))))))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x)))))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero))))))))))]"

echo "----- N = 10 -----"

time acvps A95_ex4_2_15b_llacc.trs -s R --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))))))))]"

time acvps A95_ex4_2_15b_gacc.trs -s R/AC --vp "[VAR x][f(p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(p(zero,x),p(zero,x))))))))))) == f(p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(p(x,zero),p(x,zero)))))))))))]"

