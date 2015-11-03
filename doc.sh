if [ ! -d doc ]; then
    mkdir doc
fi

haddock CellularAutomata2D.hs -h -o doc
haddock GUI.hs -h -o doc
haddock Term.hs -h -o doc


