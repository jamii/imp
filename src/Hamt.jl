module Hamt

immutable Node{T}
  leaf_bitmap::UInt32
  node_bitmap::UInt32
  leaves::Vector{T}
  nodes::Vector{Node{T}}
end

type Tree{T}
  root::Vector{Node{T}}
end

type Node{T, L, N}
  leaf_bitmap::UInt32
  node_bitmap::UInt32
  leaves::NTuple{L, Nullable{T}}
  nodes::NTuple{N, Nullable{Any}}
end

sizeof(MNNode{Int, 0, 0})

Tree(T) = Tree(Node{T}[Node{T}(0, 0, T[], Node{T}[])])

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
  key = hash(value)
  chunk = chunk_at(key, ix)
  Node{T}(UInt32(1) << chunk, 0, T[row], Node{T}[])
end

Base.push!{T}(tree::Tree{T}, row::T) = begin
  nodes = tree.root
  node_ix = 1
  node = tree.root[node_ix]
  for column in 1:length(row)
    value = row[column]
    key = hash(value)
    for ix in 0:(key_length-1)
      chunk = chunk_at(key, ix)
      mask = UInt32(1) << chunk
      if (node.node_bitmap & mask) > 0
        nodes = node.nodes
        node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
        node = nodes[node_ix]
        # continue loop
      elseif (node.leaf_bitmap & mask) > 0
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        leaf = node.leaves[leaf_ix]
        if row == leaf
          return tree # was a dupe
        else
          leaf_bitmap = node.leaf_bitmap $ mask
          node_bitmap = node.node_bitmap | mask
          nodes[node_ix] = Node(leaf_bitmap, node_bitmap, node.leaves, node.nodes)
          nodes = node.nodes
          node_ix = 1 + count_ones(node.node_bitmap << (32 - chunk))
          child = singleton(leaf, column, ix+1)
          deleteat!(node.leaves, leaf_ix)
          insert!(nodes, node_ix, child)
          node = child
          # continue loop
        end
      else
        leaf_ix = 1 + count_ones(node.leaf_bitmap << (32 - chunk))
        leaf_bitmap = node.leaf_bitmap | mask
        nodes[node_ix] = Node(leaf_bitmap, node.node_bitmap, node.leaves, node.nodes)
        insert!(node.leaves, leaf_ix, row)
        return tree # inserted
      end
    end
  end
  error("Out of bits!")
end

Base.in{T}(row::T, tree::Tree{T}) = begin
  node = tree.root[1]
  for column in 1:length(row)
    value = row[column]
    key = hash(value)
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
ids(n) = rand(1:n, n)

using Benchmark

f() = begin
  rows = [(a,) for a in ids(1000000)]
  make_tree() = begin
    tree = Tree(Tuple{Int64})
    for row in rows
      push!(tree, row)
    end
    tree
  end
  tree = make_tree()
  (benchmark(()->sort(rows, alg=QuickSort), "", 100),
   benchmark(make_tree, "", 100),
   benchmark(()->all([(row in tree) for row in rows]), "", 100))
end

g() = begin
  n = 1000000
  rows = [(a,) for (a,b) in zip(ids(n), ids(n))]
  for _ in 1:100
    tree = Tree(Tuple{Int64})
    for row in rows
      push!(tree, row)
    end
  end
end

end
