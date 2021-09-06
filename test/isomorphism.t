Test isomorphism on contents
  $ export SEED=1D+DsWvqsdfDWxDzppx1Rm8mcgtsPwQyZSl6Hzj0muI=
  $ ./test_generate.exe $SEED > contents
  $ ./test_stream.exe < contents
  $ ./test_generate.exe $SEED 0 > part-0
  $ diff stdout-0 part-0
  $ ./test_generate.exe $SEED 1 > part-1
  $ diff stdout-1 part-1
  $ ./test_generate.exe $SEED 2 > part-2
  $ diff stdout-2 part-2
