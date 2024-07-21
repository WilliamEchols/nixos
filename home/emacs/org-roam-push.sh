source ~/Desktop/nixos/home/emacs/.env

printf "Preparing to PUSH orgfiles to server\n"
printf "Server files may be deleted to match local config\n\n"

rsync -avz ~/Desktop/orgfiles/ ${REMOTE_USER}@${REMOTE_IP}:~/Desktop/orgfiles 
