module Util

macro showtime(expr)
  quote
    result = @time $(esc(expr))
    println($(string("^ ", expr)))
    println()
    result
  end
end

export @showtime

end
