sudo pkill -9 nginx && sudo nginx -p ./ -c ./nginx.conf

lb compile project ~/imp/lb/imp.project && lb delete imp && lb create imp && lb web-server load --config ~/imp/lb/branch-services.config && lb web-server load-services -w imp && lb addproject imp ~/imp/lb && lb web-server load-services && lb exec imp '
  ^minesweeper:minesweeper:num_x[] = 30.
  ^minesweeper:minesweeper:num_y[] = 30.
  ^minesweeper:minesweeper:num_mines[] = 90.
  ^minesweeper:minesweeper:seed[] = 123.
  -minesweeper:minesweeper:cleared(x,y) <-
    minesweeper:minesweeper:cleared@prev(x,y).
'

lb compile project ~/imp/lb/imp.project && lb delete imp && lb create imp && lb web-server load --config ~/imp/lb/branch-services.config && lb web-server load-services -w imp && lb addproject imp ~/imp/lb && lb web-server load-services && lb exec imp '
  ^todomvc:todomvc:current_filter[] = "All".
'

lb compile project ~/imp/lb/imp.project && lb delete imp && lb create imp && lb addproject imp ~/imp/lb

diff is a mess + currently no way to handle non-client initiated changes
definitely want ordered choice, especially for style cascades
random gen of mines is ugly
stratification was problematic with state[] = _
want live coding
need some way to serialize events
setting initial values is a pain
need to handle edge events like focus, clear
sorting is a mess, especially with optional nodes
state machines are awkward - enums? init?

echo '{ "request": { "workspace": "imp", "branch": "q1", "from_branch": "master", "overwrite": false } }' | lb web-client call http://localhost:55183/imp/create-branch
