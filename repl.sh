stack ghci --ghci-options "-fobject-code -O2" | ./pretty.sh

# to print Core
# stack -v build poet:exe:poet-exe --ghc-options '-ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes'