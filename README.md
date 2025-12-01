#finish the setup
cd dq-checkers/rocq

# Build Rocq files + Extract.v, which produces Haskell extraction in out/Generated.hs
make -j

cd out

# Compile Main.hs and Generated.hs into the executable `run`
ghc -O2 Main.hs Generated.hs -o run

# example for cli query
./run --dataset german-credit --range "13,18,60"

#open gui
cd dq-checkers/rocq/gui
python3 app.py
