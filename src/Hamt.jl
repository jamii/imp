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

for i in 1:32
  @eval @node($i)
end

const nodes = tuple([eval(symbol("Node", i)) for i in 1:32]...)
const sizes = tuple([sizeof(node) for node in nodes]...)

# TODO assert that offsets are correct, fields are in order etc
const bitmap_offset = 0
const nodes_offset = sizeof(Ptr)

const not_found = "not_found" # arbitrary pointer to compare to

call(::Type{Node}, n) = begin
  node_type = nodes[n]
  node_size = sizes[n]
  node_pointer = ccall((:jl_gc_allocobj, :libjulia), Ptr{UInt8}, (Csize_t,), node_size)
  unsafe_store!(convert(Ptr{Ptr{Void}}, node_pointer), pointer_from_objref(node_type), 0)
  for i in 1:div(node_size,sizeof(Int))
    unsafe_store!(convert(Ptr{Int}, node_pointer), 0, i)
  end
  unsafe_pointer_to_objref(node_pointer)
end

Base.getindex(node::Ptr{Node}, ix::Integer) = begin
  bitmap = unsafe_load(convert(Ptr{UInt32}, node + bitmap_offset))
  if (bitmap & (1 << ix)) == 0
    not_found
  else
    pos = count_ones(bitmap << (32 - ix))
    unsafe_load(convert(Ptr{Any}, node + nodes_offset), pos+1)
  end
end

# # TODO does unsafe_store know about alignment?
# Base.setindex!{T}(narray::NArray{T}, t::T, ix::Integer) = begin
#   buffer = narray.buffer
#   bitmap = narray.bitmap | (1 << ix)
#   length = count_ones(bitmap)
#   capacity = nextpow2(length) # invariant, enforced by setindex and deleteat
#   if length > capacity
#     narray = grow(narray)
#   end
#   pos = count_ones(bitmap << (32 - ix))
#   if (narray.bitmap & (1 << ix)) == 0
#     unsafe_store!(convert(Ptr{T}, pointer_from_objref(buffer)), t, ix)
#     if !isbits(T)
#       ccall((:jl_gc_wb, :libjulia), Void, (Ptr{Void}, Ptr{Void}), buffer, t)
#     end
#     NArray(bitmap, buffer)
#   end

  #
  # grow{T}(before::NArray{T}, size_before, size_after) = begin
  #   old_gc = gc_enable(false) # going to temporarily break gc guarantees
  #   after = make_narray(size_after, T)
  #   for i in 1:size_before
  #     after[i] = before[i]
  #   end
  #   for i in (size_before+1):size_after
  #     after[i] = convert(T, C_NULL) # dont want random pointers making the gc sad
  #   end
  #   gc_enable(old_gc)
  #   after
  # end

  # type Tree{T}
  #   root::Node{T}
  # end
  #
  # Tree(T) = Tree(Node{T}(0, 0, T[], Node{T}[]))
  #
  # const key_length = Int64(ceil(sizeof(hash(0)) * 8.0 / 5.0))
  #
  # chunk_at(key, ix) = (key >> (ix*5)) & 0b11111
  #
  # singleton{T}(row::T, column, ix) = begin
  #   if ix >= key_length
  #     column += 1
  #     ix = 0
  #     if column > length(row)
  #       error("Out of bits")
  #     end
  #   end
  #   value = row[column]
  #   key = value # key = hash(value)
  #   chunk = chunk_at(key, ix)
  #   Node{T}(1 << chunk, 0, T[row], Node{T}[])
  # end
  #
  # Base.push!{T}(tree::Tree{T}, row::T) = begin
  #   node = tree.root
  #   for column in 1:length(row)
  #     value = row[column]
  #     key = value # key = hash(value)
  #     for ix in 0:(key_length-1)
  #       chunk = chunk_at(key, ix)
  #       mask = 1 << chunk
  #       if (node.node_bitmap & mask) > 0
  #         node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
  #         node = node.nodes[node_ix]
  #         # continue loop
  #       elseif (node.leaf_bitmap & mask) > 0
  #         leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
  #         leaf = node.leaves[leaf_ix]
  #         if row == leaf
  #           return tree # was a dupe
  #         else
  #           deleteat!(node.leaves, leaf_ix)
  #           child = singleton(leaf, column, ix+1)
  #           node.leaf_bitmap $= mask
  #           node.node_bitmap |= mask
  #           node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
  #           insert!(node.nodes, node_ix, child)
  #           node = child
  #           # continue loop
  #         end
  #       else
  #         node.leaf_bitmap |= mask
  #         leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
  #         insert!(node.leaves, leaf_ix, row)
  #         return tree # inserted
  #       end
  #     end
  #   end
  #   error("Out of bits!")
  # end
  #
  # Base.in{T}(row::T, tree::Tree{T}) = begin
  #   node = tree.root
  #   for column in 1:length(row)
  #     value = row[column]
  #     key = value # hash(value)
  #     for ix in 0:(key_length-1)
  #       chunk = chunk_at(key, ix)
  #       mask = 1 << chunk
  #       if (node.node_bitmap & mask) > 0
  #         node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
  #         node = node.nodes[node_ix]
  #         # continue loop
  #       elseif (node.leaf_bitmap & mask) > 0
  #         leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
  #         leaf = node.leaves[leaf_ix]
  #         return row == leaf
  #       else
  #         return false
  #       end
  #     end
  #   end
  #   error("Out of bits!")
  # end
  #
  # ids() = ids(1000000)
  # ids(n) = rand(UInt64, n)
  #
  # f(ids) = begin
  #   tree = Tree(Tuple{UInt64})
  #   push!(tree, (UInt64(1),))
  #   (UInt64(1),) in tree
  #   rows = [(a,) for a in ids]
  #   gc_enable(false)
  #   for _ in 1:10
  #     tree = Tree(Tuple{UInt64})
  #     @time begin
  #       for row in rows
  #         push!(tree, row)
  #       end
  #     end
  #     @time begin
  #       all([(row in tree) for row in rows])
  #     end
  #     tree = Tree(Tuple{UInt64})
  #     @time begin
  #       gc_enable(true)
  #       gc()
  #       gc_enable(false)
  #     end
  #     println("")
  #   end
  # end
  #
  # f(ids(10000000))

end
