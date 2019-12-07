echo "EAGER:" &&
echo "Pair:" && ./lambda.native eager test/pair.lam | egrep "^(FALSE|TRUE)$" &&
echo "Match empty:" && ./lambda.native eager test/match_empty.lam | egrep "^(FALSE|TRUE)$" &&
echo "Match cons:" && ./lambda.native eager test/match_cons.lam | egrep "^(FALSE|TRUE)$" &&
echo "Cons fst snd:" && ./lambda.native eager test/cons_fst_snd.lam | egrep "^(FALSE|TRUE)$" &&
echo "=================================" &&
echo "LAZY:" &&
echo "Pair:" && ./lambda.native lazy test/pair.lam | egrep "^(FALSE|TRUE)$" &&
echo "Match empty:" && ./lambda.native lazy test/match_empty.lam | egrep "^(FALSE|TRUE)$" &&
echo "Match cons:" && ./lambda.native lazy test/match_cons.lam | egrep "^(FALSE|TRUE)$" &&
echo "Cons fst snd:" && ./lambda.native lazy test/cons_fst_snd.lam | egrep "^(FALSE|TRUE)$" &&
true
