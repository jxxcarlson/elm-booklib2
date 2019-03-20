sed  's/URL/http:\/\/localhost:4000/' ./robot/files/config.txt  > src/Configuration.elm
elm make --optimize src/Main.elm --output=Main.js