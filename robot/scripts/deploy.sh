# red=`tput setaf 1`
# green=`tput setaf 2`
# reset=`tput sgr0`
# echo "${red}red text ${green}green text${reset}"

color=`tput setaf 48`
red=`tput setaf 1`
reset=`tput setaf 7`

echo
echo "${red}----------------------${reset}"
echo "${red}DEPLOY BookLib App${reset}"
echo "${red}----------------------${reset}"

echo
echo "${color}Compiling .src/Main.elm with --optimize${reset}"
start=`date +%s`
elm make  --optimize ./src/Main.elm --output=Main.js
end=`date +%s`
runtime=$((end-start))
echo
echo "${magenta}Compile time: " $runtime " seconds${reset}"

echo
echo "${color}Uglifying ... ${reset}"
uglifyjs Main.js -mc 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9"' -o Main.min.js

echo
echo "${color}Uploading to cloud ...${reset}"
scp -r ./index-remote.html  root@206.189.184.194:/var/www/html/index.html
scp -r ./Main.min.js root@206.189.184.194:/var/www/html/


echo
tput setaf 2; echo "${color}Done${reset}"