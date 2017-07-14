module Util

macro showtime(expr)
  quote
    @time $(esc(expr))
    println($(string("^ ", expr)))
    println()
  end
end

export @showtime

end
