#!/bin/bash
# hacky mess
source ~/logicblox/etc/profile.d/logicblox.sh
source ~/logicblox/etc/bash_completion.d/logicblox.sh
lb compile project ~/imp/lb/imp.project && lb delete imp && lb create imp && lb addproject imp ~/imp/lb
sudo pkill -9 nginx
sudo nginx -c ~/imp/lb/nginx.conf -p ~/imp/lb
