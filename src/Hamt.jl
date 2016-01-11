module Hamt

abstract Node

macro node(n)
  :(begin
  type $(symbol("Node", n)) <: Node
    bitmap::UInt32
    $([symbol("node", i) for i in 1:n]...)
  end
end)
end

# aims to fit into pool sizes at https://github.com/JuliaLang/julia/blob/6cc48dcd24322976bdc193b3c578acb924f0b8e9/src/gc.c#L1308-L1336
@node(0)
@node(3)
@node(7)
@node(15)
@node(31)
@node(32)

# TODO assert that offsets are correct, fields are in order etc
const bitmap_offset = 0
const nodes_offset = sizeof(Ptr)

const not_found = "not_found" # arbitrary pointer to compare to

grow(node_before::Ptr{Node}, size_before::Integer, size_after::Integer, type_after::Type) = begin
  pointer_before = convert(Ptr{UInt8}, node_before)
  pointer_after = ccall((:jl_gc_allocobj, :libjulia), Ptr{UInt8}, (Csize_t,), size_after)
  unsafe_store!(convert(Ptr{Ptr{Void}}, pointer_after), pointer_from_objref(type_after), 0)
  for i in 1:size_before
    unsafe_store!(pointer_after, unsafe_load(pointer_before, i), i)
  end
  for i in (size_before+1):size_after
    unsafe_store!(pointer_after, 0, i)
  end
  convert(Ptr{Node}, pointer_after)
end

# TODO figure out how to get a computed goto out of this
maybe_grow(node_before::Ptr{Node}, length::Integer) = begin
  if length == 0
    grow(node_before, sizeof(Node0), sizeof(Node3), Node3)
  elseif length == 3
    grow(node_before, sizeof(Node3), sizeof(Node7), Node7)
  elseif length == 7
    grow(node_before, sizeof(Node7), sizeof(Node15), Node7)
  elseif length == 15
    grow(node_before, sizeof(Node15), sizeof(Node31), Node7)
  elseif length == 31
    grow(node_before, sizeof(Node31), sizeof(Node32), Node7)
  else
    node_before
  end
end

get_bitmap(node::Ptr{Node}) = begin
  unsafe_load(convert(Ptr{UInt32}, node + bitmap_offset))
end

set_bitmap!(node::Ptr{Node}, bitmap::Integer) = begin
  unsafe_store!(convert(Ptr{UInt32}, node + bitmap_offset), UInt32(bitmap))
end

get_node(node::Ptr{Node}, pos::Integer) = begin
  unsafe_load(convert(Ptr{Any}, node + nodes_offset), pos)
end

set_node!(node::Ptr{Node}, val, pos::Integer) = begin
  unsafe_store!(convert(Ptr{Any}, node + nodes_offset), val, pos)
end

get_pos(bitmap::UInt32, ix::Integer) = begin
  1 + count_ones(bitmap << (32 - ix))
end

get_child_inner(node::Ptr{Node}, ix::Integer) = begin
  bitmap = get_bitmap(node)
  if (bitmap & (1 << ix)) == 0
    not_found
  else
    get_node(node, get_pos(bitmap, ix))
  end
end

set_child_inner!(node::Ptr{Node}, val::ANY, ix::Integer) = begin
  val_pointer = pointer_from_objref(val)
  bitmap = get_bitmap(node)
  pos = get_pos(bitmap, ix)
  if (bitmap & (1 << ix)) == 0
    length = count_ones(bitmap)
    node = maybe_grow(node, length)
    set_bitmap!(node, bitmap | (1 << ix))
    for i in length:-1:pos
      set_node!(node, get_node(node, i), i+1)
    end
  end
  set_node!(node, val, pos)
  # TODO figure out how to link this
  # ccall((:jl_gc_wb, :libjulia), Void, (Ptr{Void}, Ptr{Void}), node, val_pointer)
  ccall((:jl_gc_queue_root, :libjulia), Void, (Ptr{Void},), node)
  node
end

@inline get_child(node::Node, ix::Integer) = begin
  get_child_inner(convert(Ptr{Node}, pointer_from_objref(node)), ix)
end

@inline set_child!(node::Node, val::ANY, ix::Integer) = begin
  unsafe_pointer_to_objref(set_child_inner!(convert(Ptr{Node}, pointer_from_objref(node)), val, ix))::Node
end

### NO UNSAFE BELOW THIS LINE ###

n = Node0(0)
n = set_child!(n, 3, 3)
n = set_child!(n, 5, 5)
n = set_child!(n, 1, 1)
n = set_child!(n, 0, 0)
n = set_child!(n, 21, 21)
n = set_child!(n, 99, 31)
n = set_child!(n, 31, 31)

type Tree{T}
  root::Node
end

call{T}(::Type{Tree{T}}) = Tree{T}(Node0(0))

const key_length = Int64(ceil(sizeof(hash(0)) * 8.0 / 5.0))

chunk_at(key, ix) = (key >> (ix*5)) & 0b11111

singleton(row, column, ix) = begin
  if ix >= key_length
    column += 1
    ix = 0
    if column > length(row)
      error("Out of bits")
    end
  end
  value = row[column]
  key = value # key = hash(value)
  chunk = chunk_at(key, ix)
  node = Node0(0)
  set_child!(node, row, ix)
end

Base.push!{T}(tree::Tree{T}, row::T) = begin
  node::Node = tree.root
  for column in 1:length(row)
    value = row[column]
    key = value # TODO key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      child = get_child(node, chunk)
      if child === not_found
        # empty slot
        set_child!(node, row, chunk)
        return tree
      elseif typeof(child) !== t
        node = child
        # continue loop
      elseif child == row
        # dupe
        return tree
      else
        # collision
        new_node = singleton(leaf, column, ix+1)
        set_child!(node, new_node, chunk) # TODO this calls popcnt again
        node = new_node
        # continue loop
      end
    end
  end
  error("Out of bits!")
end

Base.in{T}(row::T, tree::Tree{T}) = begin
  node = tree.root
  for column in 1:length(row)
    value = row[column]
    key = value # TODO key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      child = get_child(node, chunk)
      if child === not_found
        return false
      elseif typeof(child) !== t
        node = child
        # continue loop
      else
        return child == row
      end
    end
  end
  error("Out of bits!")
end

ids() = ids(1000000)
ids(n) = rand(UInt64, n)

f(ids) = begin
  tree = Tree{Tuple{UInt64}}()
  push!(tree, (UInt64(1),))
  (UInt64(1),) in tree
  rows = Any[(a,) for a in ids] # forces boxing
  gc_enable(false)
  for _ in 1:10
    tree = Tree{Tuple{UInt64}}()
    @time begin
      for row in rows
        push!(tree, row)
      end
    end
    @time begin
      println(all([(row in tree) for row in rows]))
    end
    tree = Tree{Tuple{UInt64}}()
    @time begin
      gc_enable(true)
      gc()
      gc_enable(false)
    end
    println("")
  end
end

f(ids(1))

end
