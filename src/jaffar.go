package main

import "fmt"
import "os"
import "unsafe"

type PrintCyclicer interface {
	PrintCyclic(prefix string, seen map[unsafe.Pointer]bool)
}

func genNode(format string, objs ...interface{}) {
	fmt.Printf("%s%p [label=\"" + format + "\"]\n", objs...)
}

func genLink(seen map[unsafe.Pointer]bool, format string, objs ...interface{}) {
	fmt.Printf("%s%p -> %[1]s%[3]p [label=\"" + format + "\"]\n", objs...)
	objs[2].(PrintCyclicer).PrintCyclic(objs[0].(string), seen)
}

// immutable
type TaggedTreeList struct {
	left, right *TaggedTreeList
	size, termCount int
	leaf *TermNode
}

func NewTaggedTreeList(term *TermNode) *TaggedTreeList {
	t := 0
	if term.varNode == nil {
		t = 1
	}
	return &TaggedTreeList{
		nil,
		nil,
		1,
		t,
		term,
	}
}

func (l *TaggedTreeList) Append(term *TermNode) *TaggedTreeList {
	return l.Concat(NewTaggedTreeList(term))
}

func (l *TaggedTreeList) Concat(r *TaggedTreeList) *TaggedTreeList {
	return &TaggedTreeList{
		l,
		r,
		l.size + r.size,
		l.termCount + r.termCount,
		nil,
	}
}

func (l *TaggedTreeList) Slice() []*TermNode {
	s := make([]*TermNode, 0, l.size)
	return l.slice(s)
}

func (l *TaggedTreeList) slice(s []*TermNode) []*TermNode {
	if l.leaf == nil {
		s = l.left.slice(s)
		s = l.right.slice(s)
	} else {
		s = append(s, l.leaf)
	}
	return s
}

func (l *TaggedTreeList) TermCount() int {
	if l == nil {
		return 0
	}
	return l.termCount
}

func (l *TaggedTreeList) PrintCyclicRoot(prefix string) {
	fmt.Println("subgraph {")
	l.PrintCyclic(prefix, make(map[unsafe.Pointer]bool))
	fmt.Println("}")
}

func (l *TaggedTreeList) PrintCyclic(prefix string, seen map[unsafe.Pointer]bool) {
	ptr := unsafe.Pointer(l)
	if !seen[ptr] {
		seen[ptr] = true
		genNode("%d/%d", prefix, l, l.termCount, l.size)
		if l.leaf != nil {
			genLink(seen, "leaf", prefix, l, l.leaf)
		} else {
			genLink(seen, "left", prefix, l, l.left)
			genLink(seen, "right", prefix, l, l.right)
		}
	}
}

// immutable
type Symbol struct {
	name string
	arity int
}

func (s *Symbol) PrintCyclic(prefix string, seen map[unsafe.Pointer]bool) {
	ptr := unsafe.Pointer(s)
	if !seen[ptr] {
		seen[ptr] = true
		genNode("%s/%d", prefix, s, s.name, s.arity)
	}
}

type TermNode struct {
	symbol *Symbol // immutable
	varNode *VarNode // const
	child []*TermNode // const (slice object only)
	done bool // mutable
}

func (t *TermNode) Substitute() {
	if !t.done {
		t.done = true
		for i, c := range t.child {
			if c.varNode == nil {
				c.Substitute()
			} else {
				ts := rep(c.varNode).terms.Slice()
				if len(ts) != 1 {
					panic("Attempted to substitute variable with multiple terms")
				}
				t.child[i] = ts[0]
			}
		}
	}
}

func (t *TermNode) PrintCyclic(prefix string, seen map[unsafe.Pointer]bool) {
	ptr := unsafe.Pointer(t)
	if !seen[ptr] {
		seen[ptr] = true
		suffix := ""
		if t.done {
			suffix = " ✓"
		}
		if t.symbol != nil {
			genNode("%s%s", prefix, t, t.symbol.name, suffix)
		} else if t.varNode != nil {
			genNode("%s%s", prefix, t, t.varNode.symbol, suffix)
			genLink(seen, "var", prefix, t, t.varNode)
		}
		for i, c := range t.child {
			genLink(seen, "#%d", prefix, t, c, i)
		}
	}
}

type VarNode struct {
	symbol string // immutable
	rep *VarNode // mutable
	terms *TaggedTreeList // mutable
	varCount int // mutable
}

func (v *VarNode) PrintCyclic(prefix string, seen map[unsafe.Pointer]bool) {
	ptr := unsafe.Pointer(v)
	if !seen[ptr] {
		seen[ptr] = true
		genNode("%s (%d)", prefix, v, v.symbol, v.varCount)
		if v.rep != nil {
			genLink(seen, "rep", prefix, v, v.rep)
		}
		if v.terms != nil {
			genLink(seen, "terms", prefix, v, v.terms)
		}
	}
}

type UnificationError struct {
	left, right *Symbol
}

func add(queue []*VarNode, v *VarNode, t *TermNode) []*VarNode {
	if v.terms == nil {
		v.terms = NewTaggedTreeList(t)
	} else {
		v.terms = v.terms.Append(t)
	}
	if v.terms.TermCount() == 2 {
		return append(queue, v)
	}
	return queue
}

func merge(queue []*VarNode, v1, v2 *VarNode) []*VarNode {
	r1, r2 := v1.varCount, v2.varCount
	var bigV, v *VarNode
	if r1 >= r2 {
		bigV, v = v1, v2
	} else {
		bigV, v = v2, v1
	}
	small := bigV.terms.TermCount() <= 1
	if bigV.terms == nil {
		bigV.terms = v.terms
	} else if v.terms != nil {
		bigV.terms = bigV.terms.Concat(v.terms)
	}
	v.rep, v.terms, v.varCount = bigV, nil, 0
	bigV.varCount = r1 + r1
	if small && bigV.terms.TermCount() >= 2 {
		return append(queue, bigV)
	}
	return queue
}

func rep(v *VarNode) *VarNode {
	if v.rep == nil {
		return v
	}
	v0 := v.rep
	for v0.rep != nil {
		v0 = v0.rep
	}
	for v.rep != v0 {
		v.rep, v = v0, v.rep
	}
	return v0
}

func commonFrontier(queue []*VarNode, index int, parents, t_list []*TermNode) ([]*VarNode, *UnificationError) {
	sym := t_list[0].symbol
	for _, term := range t_list {
		if term.symbol != sym {
			return nil, &UnificationError{
				left:  sym,
				right: term.symbol,
			}
		}
	}
	a := sym.arity
	t0_list := make([]*TermNode, len(t_list))
	for i := 0; i < a; i++ {
		for j := range t_list {
			t0_list[j] = t_list[j].child[i]
		}
		j := -1
		for k, term := range t0_list {
			if term.varNode != nil {
				j = k
				break
			}
		}
		if j != -1 {
			// In the original this unconditionally swaps them in memory
			if parents != nil {
				tmp := parents[0].child[index]
				parents[0].child[index] = parents[j].child[index]
				parents[j].child[index] = tmp
			}
			v := rep(t0_list[j].varNode)
			for k, term := range t0_list {
				if k == j || term.varNode == nil {
					continue
				}
				v2 := rep(term.varNode)
				if v != v2 {
					queue = merge(queue, v, v2)
				}
			}
			for _, term := range t0_list {
				if term.varNode != nil {
					continue
				}
				queue = add(queue, v, term)
			}
		} else {
			return commonFrontier(queue, i, t_list, t0_list)
		}
	}
	return queue, nil
}

func unify(t_list []*TermNode) *UnificationError {
	queue := make([]*VarNode, 0)
	queue, err := commonFrontier(queue, 0, nil, t_list)
	if err != nil {
		return err
	}
	for len(queue) != 0 {
		v := queue[0]
		queue = queue[1:]
		k := v.terms.TermCount()
		if k >= 2 {
			t := v.terms.Slice()
			v.terms = NewTaggedTreeList(t[0])
			queue, err = commonFrontier(queue, 0, nil, t)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func main() {
	sym := func(name string, arity int) *Symbol {
		return &Symbol{ name, arity }
	}
	varNode := func(name string) *VarNode {
		return &VarNode{ name, nil, nil, 1 }
	}
	varTerm := func(node *VarNode) *TermNode {
		return &TermNode{ nil, node, nil, false }
	}
	funcTerm := func(symbol *Symbol, children ...*TermNode) *TermNode {
		return &TermNode{ symbol, nil, children, false }
	}
	f := sym("f", 2)
	x := varTerm(varNode("x"))
	z := varTerm(varNode("z"))
	expr1 := funcTerm(f, x, x)
	expr2 := funcTerm(f, funcTerm(f, z, z), funcTerm(f, z, x))
	root := NewTaggedTreeList(expr1).Append(expr2)
	fmt.Println("digraph {")
	root.PrintCyclicRoot("initial")
	err := unify(root.Slice())
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
		os.Exit(1)
	}
	root.PrintCyclicRoot("unify")
	expr1.Substitute()
	expr2.Substitute()
	root.PrintCyclicRoot("substitute")
	fmt.Println("}")
}
