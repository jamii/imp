# module Hamt

type Node{T}
  leaf_bitmap::UInt32
  node_bitmap::UInt32
  leaves::Vector{T}
  nodes::Vector{Node{T}}
end

type Tree{T}
  root::Node{T}
end

Tree(T) = Tree(Node{T}(0, 0, T[], Node{T}[]))

const key_length = Int64(ceil(sizeof(hash(0)) * 8.0 / 5.0))

chunk_at(key, ix) = (key >> (ix*5)) & 0b11111

singleton{T}(row::T, column, ix) = begin
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
  Node{T}(1 << chunk, 0, T[row], Node{T}[])
end

Base.push!{T}(tree::Tree{T}, row::T) = begin
  node = tree.root
  for column in 1:length(row)
    value = row[column]
    key = value # key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      mask = 1 << chunk
      if (node.node_bitmap & mask) > 0
        node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
        node = node.nodes[node_ix]
        # continue loop
      elseif (node.leaf_bitmap & mask) > 0
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        leaf = node.leaves[leaf_ix]
        if row == leaf
          return tree # was a dupe
        else
          deleteat!(node.leaves, leaf_ix)
          child = singleton(leaf, column, ix+1)
          node.leaf_bitmap $= mask
          node.node_bitmap |= mask
          node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
          insert!(node.nodes, node_ix, child)
          node = child
          # continue loop
        end
      else
        node.leaf_bitmap |= mask
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        insert!(node.leaves, leaf_ix, row)
        return tree # inserted
      end
    end
  end
  error("Out of bits!")
end

Base.in{T}(row::T, tree::Tree{T}) = begin
  node = tree.root
  for column in 1:length(row)
    value = row[column]
    key = value # hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      mask = 1 << chunk
      if (node.node_bitmap & mask) > 0
        node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
        node = node.nodes[node_ix]
        # continue loop
      elseif (node.leaf_bitmap & mask) > 0
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        leaf = node.leaves[leaf_ix]
        return row == leaf
      else
        return false
      end
    end
  end
  error("Out of bits!")
end

ids() = ids(1000000)
ids(n) = rand(UInt64, n)

f() = begin
  tree = Tree(Tuple{UInt64})
  push!(tree, (UInt64(1),))
  (UInt64(1),) in tree
  rows = [(a,) for a in ids(10_000_000)]
  gc_enable(false)
  for _ in 1:10
    tree = Tree(Tuple{UInt64})
    @time begin
      for row in rows
        push!(tree, row)
      end
    end
    @time begin
      all([(row in tree) for row in rows])
    end
    tree = Tree(Tuple{UInt64})
    @time begin
      gc_enable(true)
      gc()
      gc_enable(false)
    end
    println("")
  end
end

f()

# end
