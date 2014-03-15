#!/usr/bin/env ruby

class Tree

  attr_accessor :children, :node_name

  def initialize(name, children=[])
    @children = children
    @node_name = name
  end

  def visit_all(&block)
    visit &block
    children.each {|c| c.visit_all &block}
  end

  def visit(&block)
    block.call self
  end

  def self.from_object(o)
    l = o.collect { |name,childrens| Tree.new(name, Tree.from_object(childrens)) }
    if l.length == 1
      return l[0]
    end
    l
  end

  def to_s
    "<Tree #{self.node_name}: {#{children.join(",")}}>"
  end

end

ruby_tree = Tree.new( "Ruby",
  [Tree.new("Reia" ),
  Tree.new("MacRuby")]
)

puts "Visiting a node"
ruby_tree.visit {|node| puts ">> #{node.node_name}"}
puts

puts "visiting entire tree"
ruby_tree.visit_all {|node| puts ">> #{node.node_name}"}
puts

tree = Tree.from_object(
  {'grandpa' => {
      'dad' => {
        'child 1' => {}, 'child 2' => {}
      },
      'uncle' => {
        'child 3' => {}, 'child 4' => {}
      }
    }
  }
)
puts "Display tree with from_object constructor"
tree.visit_all { |node| puts node }
