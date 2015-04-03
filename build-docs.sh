cabal haddock --html-location='http://hackage.haskell.org/package/$pkg/docs'

git stash
git checkout gh-pages
mv dist/doc/html/edit-distance-vector/* .

