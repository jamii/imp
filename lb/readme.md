sudo pkill -9 nginx && sudo nginx -p ./ -c ./nginx.conf

lb compile project ~/imp/lb/imp.project && lb delete imp && lb create imp && lb addproject imp ~/imp/lb && lb web-server load-services && lb exec imp '
  ^minesweeper:minesweeper:num_x[] = 30.
  ^minesweeper:minesweeper:num_y[] = 30.
  ^minesweeper:minesweeper:num_mines[] = 90.
  ^minesweeper:minesweeper:seed[] = 123.
  -minesweeper:minesweeper:cleared(x,y) <-
    minesweeper:minesweeper:cleared@prev(x,y).
'
