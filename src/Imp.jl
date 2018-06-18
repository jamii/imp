module Imp

import MacroTools
import MacroTools: @capture
using Rematch

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Base.Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end

# include("columns.jl")
include("first.jl")
# include("higher.jl")

# export imp, @imp, @imp!, @lib

end
