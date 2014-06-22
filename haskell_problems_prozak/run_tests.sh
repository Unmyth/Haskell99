#!/bin/sh

for test_name in *.hs
do
echo $test_name
test_function=`grep -e '^[^ ]*Test' $test_name | tail -1 | awk '{print $1}'`
cat > test_input.hs <<END
import Data.Char
import Test.QuickCheck
 
-- Arbitrary Char is now part of the library
-- {-
-- instance Arbitrary [Char] where
--     arbitrary     = choose ('\32', '\128')
--     coarbitrary c = variant (ord c \`rem\` 4)
-- -}

`cat problem1.hs`

END
echo quickCheck $test_function | ghci -v0 test_input.hs 2>/dev/null
rm test_input.hs
#echo $test_str $test_function
done
