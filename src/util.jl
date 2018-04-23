macro showtime(expr)
  quote
    result = @time $(esc(expr))
    println($(string("^ ", expr)))
    println()
    result
  end
end

macro splice(iterator, body)
  @assert iterator.head == :call
  @assert iterator.args[1] == :in
  Expr(:..., :(($(esc(body)) for $(esc(iterator.args[2])) in $(esc(iterator.args[3])))))
end
