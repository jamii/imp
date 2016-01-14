module Hamt

@inline is_marked(val_pointer::Ptr{Void}) = begin
  (unsafe_load(convert(Ptr{UInt}, val_pointer), 0) & UInt(1)) == UInt(1)
end

@inline gc_write_barrier(parent_pointer::Ptr{Void}, child_pointer::Ptr{Void}) = begin
  # println("parent is")
  # println("parent is", unsafe_pointer_to_objref(parent_pointer))
  # println((is_marked(parent_pointer), is_marked(child_pointer)))
  if (is_marked(parent_pointer) && !is_marked(child_pointer))
    ccall((:jl_gc_queue_root, :libjulia), Void, (Ptr{Void},), parent_pointer)
  end
end

# equivalent to:
# type Node{N}
#   bitmap::UInt32
#   nodes::NTuple{N, Node} # <- this needs to be stored inline
# end

abstract Node

macro node(n)
  :(begin
  type $(symbol("Node", n)) <: Node
    bitmap::UInt32
    $([symbol("node", i) for i in 1:n]...)
  end
end)
end

# aims to fit into pool sizes - see https://github.com/JuliaLang/julia/blob/6cc48dcd24322976bdc193b3c578acb924f0b8e9/src/gc.c#L1308-L1336
@node(2)
@node(4)
@node(8)
@node(16)
@node(32)

# TODO assert that offsets are correct, fields are in order etc
const bitmap_offset = 0
const nodes_offset = sizeof(Ptr)

Base.call(::Type{Node2}) = Node2(0, unsafe_pointer_to_objref(C_NULL), unsafe_pointer_to_objref(C_NULL))

grow(parent_pointer::Ptr{Void}, node_pointer::Ptr{Node}, size_before::Integer, size_after::Integer, type_after::Type) = begin
  node_before = unsafe_load(convert(Ptr{Ptr{UInt8}}, node_pointer))
  node_after = ccall((:jl_gc_allocobj, :libjulia), Ptr{UInt8}, (Csize_t,), size_after)
  unsafe_store!(convert(Ptr{Ptr{Void}}, node_after), pointer_from_objref(type_after), 0)
  for i in 1:size_before
    unsafe_store!(node_after, unsafe_load(node_before, i), i)
  end
  for i in (size_before+1):size_after
    unsafe_store!(node_after, 0, i)
  end
  unsafe_store!(convert(Ptr{Ptr{UInt8}}, node_pointer), node_after)
  gc_write_barrier(parent_pointer, convert(Ptr{Void}, node_after))
  return
end

# TODO figure out how to get a computed goto out of this
maybe_grow(parent_pointer::Ptr{Void}, node_pointer::Ptr{Node}, length::Integer) = begin
  if length == 2
    grow(parent_pointer, node_pointer, sizeof(Node2), sizeof(Node4), Node4)
  elseif length == 4
    grow(parent_pointer, node_pointer, sizeof(Node4), sizeof(Node8), Node8)
  elseif length == 8
    grow(parent_pointer, node_pointer, sizeof(Node8), sizeof(Node16), Node16)
  elseif length == 16
    grow(parent_pointer, node_pointer, sizeof(Node16), sizeof(Node32), Node32)
  end
  return
end

@inline get_bitmap(node::Ptr{Void}) = begin
  unsafe_load(convert(Ptr{UInt32}, node + bitmap_offset))
end

@inline set_bitmap!(node::Ptr{Void}, bitmap::Integer) = begin
  unsafe_store!(convert(Ptr{UInt32}, node + bitmap_offset), UInt32(bitmap))
end

@inline get_node(node::Ptr{Void}, pos::Integer) = begin
  convert(Ptr{Node}, node + nodes_offset + ((pos-1) * sizeof(Ptr)))
end

@inline set_node!(node::Ptr{Void}, val::Ptr{Void}, pos::Integer) = begin
  unsafe_store!(convert(Ptr{Ptr{Void}}, get_node(node, pos)), val)
end

@inline get_pos(bitmap::UInt32, ix::Integer) = begin
  1 + count_ones(bitmap << (32 - ix))
end

get_child(node_pointer::Ptr{Node}, ix::Integer) = begin
  node = unsafe_load(convert(Ptr{Ptr{Void}}, node_pointer))
  bitmap = get_bitmap(node)
  if (bitmap & (UInt32(1) << ix)) == 0
    convert(Ptr{Node}, 0)
  else
    get_node(node, get_pos(bitmap, ix))
  end
end

set_child!(parent_pointer::Ptr{Void}, node_pointer::Ptr{Node}, val::ANY, ix::Integer) = begin
  old_gc = gc_enable(false)
  node::Ptr{Void} = unsafe_load(convert(Ptr{Ptr{Void}}, node_pointer))
  bitmap = get_bitmap(node)
  pos = get_pos(bitmap, ix)
  if (bitmap & (UInt32(1) << ix)) == 0
    length = count_ones(bitmap)
    maybe_grow(parent_pointer, node_pointer, length)
    node = unsafe_load(convert(Ptr{Ptr{Void}}, node_pointer))
    set_bitmap!(node, bitmap | (UInt32(1) << ix))
    for i in length:-1:pos
      set_node!(node, unsafe_load(convert(Ptr{Ptr{Void}}, get_node(node, i))), i+1)
    end
  end
  val_pointer = pointer_from_objref(val)
  set_node!(node, val_pointer, pos)
  gc_write_barrier(node, val_pointer)
  gc_enable(old_gc)
  return
end

type Tree{T}
  root::Node
end

const root_offset = 0

call{T}(::Type{Tree{T}}) = Tree{T}(Node2())

const key_length = Int64(ceil(sizeof(hash(0)) * 8.0 / 5.0))

chunk_at(key, ix) = (key >> (ix*5)) & 0b11111

@noinline out_of_bits() = error("Out of bits!")

reinsert{T}(parent_pointer::Ptr{Void}, node_pointer::Ptr{Node}, row::T, column::Integer, ix::Integer) = begin
  if ix >= key_length
    column += 1
    ix = 0
    if column > length(row)
      out_of_bits()
    end
  end
  value = row[column]
  key = hash(value)
  chunk = chunk_at(key, ix)
  set_child!(parent_pointer, node_pointer, row, chunk)
end

Base.push!{T}(tree::Tree{T}, row::T) = begin
  parent_pointer = pointer_from_objref(tree)
  node_pointer = convert(Ptr{Node}, parent_pointer + root_offset)
  for column in 1:length(row)
    value = row[column]
    key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      child_pointer = get_child(node_pointer, chunk)
      if convert(UInt, child_pointer) === UInt(0)
        # empty slot
        set_child!(parent_pointer, node_pointer, row, chunk)
        return tree
      else
        child = unsafe_pointer_to_objref(unsafe_load(convert(Ptr{Ptr{Any}}, child_pointer)))
        if typeof(child) !== T
          parent_pointer = unsafe_load(convert(Ptr{Ptr{Void}}, node_pointer))
          node_pointer = child_pointer
          # continue loop
        elseif child::T == row # TODO extra typeassert is wasteful
          # dupe
          return tree
        else
          # collision
          # TODO this calls popcnt more than necessary
          set_child!(parent_pointer, node_pointer, Node2(), chunk)
          parent_pointer = unsafe_load(convert(Ptr{Ptr{Void}}, node_pointer))
          node_pointer = get_child(node_pointer, chunk)
          reinsert(parent_pointer, node_pointer, child::T, column, ix+1) # TODO extra typeassert is wasteful
          # continue loop
        end
      end
    end
  end
  out_of_bits()
end

Base.in{T}(row::T, tree::Tree{T}) = begin
  node_pointer = convert(Ptr{Node}, pointer_from_objref(tree) + root_offset)
  for column in 1:length(row)
    value = row[column]
    key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      child_pointer = get_child(node_pointer, chunk)
      if convert(UInt, child_pointer) == 0
        return false
      else
        child = unsafe_pointer_to_objref(unsafe_load(convert(Ptr{Ptr{Any}}, child_pointer)))
        if typeof(child) !== T
          node_pointer = child_pointer
          # continue loop
        else
          return child::T == row # TODO extra typeassert is wasteful
        end
      end
    end
  end
  out_of_bits()
end

ids() = ids(1000000)
ids(n) = rand(UInt64, n)

type A
  a::UInt
end

Base.length(::A) = 1
Base.getindex(a::A, _) = a.a

f(ids) = begin
  # tree = Tree{A}()
  # push!(tree, (UInt64(1),))
  rows = [A(a) for a in ids]
  gc_enable(false)
  for _ in 1:10
    tree = Tree{A}()
    @time begin
      for row in rows
        # gc()
        # println(row)
        push!(tree, row)
      end
    end
    @time begin
      for row in rows
        if !(row in tree)
          error("missing")
        end
      end
    end
    tree = Tree{A}()
    @time begin
      gc_enable(true)
      gc()
      gc_enable(false)
    end
    println("")
  end
end

srand(999)
@time f(ids(10000000))
# Profile.clear_malloc_data()
# srand(999)
# f(ids(1000000))

end
