# Utility for showing a diff of assert_eq! failure betweeen data structures.

require 'pathname'

out = `RUST_BACKTRACE=1 cargo test #{ARGV[0]}`

out =~ /left: `([^`]*)/
left = $1
exit unless left

out =~ /right: `([^`]*)/
right = $1

$desktop = Pathname.new("~/Desktop").expand_path

def save(file_name, data)
  data.gsub!(/[\[\{]/, "\\0\n")
  path = $desktop / file_name
  path.write(data)
  path
end

left_path = save("left.rs", left)
right_path = save("right.rs", right)

`opendiff #{left_path} #{right_path}`