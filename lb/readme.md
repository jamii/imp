sudo pkill -9 nginx && sudo nginx -p ./ -c ./nginx.conf

lb compile project ~/imp/lb/imp.project && lb delete imp && lb create imp && lb addproject imp ~/imp/lb && lb web-server load-services && lb exec imp '
  ^minesweeper:minesweeper:num_x[] = 30.
  ^minesweeper:minesweeper:num_y[] = 30.
  ^minesweeper:minesweeper:num_mines[] = 90.
  ^minesweeper:minesweeper:seed[] = 123.
  -minesweeper:minesweeper:cleared(x,y) <-
    minesweeper:minesweeper:cleared@prev(x,y).
'

lb compile project ~/imp/lb/imp.project && lb delete imp && lb create imp && lb addproject imp ~/imp/lb && lb web-server load-services && lb exec imp '
  ^todomvc:todomvc:current_filter[] = "All".
'

diff is a mess + currently no way to handle non-client initiated changes
definitely want ordered choice, especially for style cascades
random gen of mines is ugly
stratification was problematic with state[] = _
want live coding
need some way to serialize events
setting initial values is a pain
need to handle edge events like focus, clear
