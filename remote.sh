sed  's/URL/https:\/\/arcane-cliffs-95237.herokuapp.com\//' ./robot/files/config.txt  > src/Configuration.elm
elm make --optimize src/Main.elm --output=Main.js