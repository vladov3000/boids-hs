mkdir -p build && stack ghc --package gloss-1.13.2.1 --package random-1.2.1 -- -outputdir build -o build/boids main.hs
