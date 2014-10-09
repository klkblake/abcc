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
	symbol *Symbol
	varNode *VarNode
	child []*TermNode
}

func (t *TermNode) PrintCyclic(prefix string, seen map[unsafe.Pointer]bool) {
	ptr := unsafe.Pointer(t)
	if !seen[ptr] {
		seen[ptr] = true
		if t.symbol != nil {
			genNode("%s", prefix, t, t.symbol.name)
		} else if t.varNode != nil {
			genNode("%s", prefix, t, t.varNode.symbol)
			genLink(seen, "var", prefix, t, t.varNode)
		}
		for i, c := range t.child {
			genLink(seen, "#%d", prefix, t, c, i)
		}
	}
}

type VarNode struct {
	symbol string
	rep *VarNode
	terms *TaggedTreeList
	varCount int
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

var queue []*VarNode

func add(v *VarNode, t *TermNode) {
	if v.terms.TermCount() == 1 {
		queue = append(queue, v)
	}
	if v.terms == nil {
		v.terms = NewTaggedTreeList(t)
	} else {
		v.terms = v.terms.Append(t)
	}
}

func merge(v1, v2 *VarNode) {
	r1, r2 := v1.varCount, v2.varCount
	var bigV, v *VarNode
	if r1 >= r2 {
		bigV, v = v1, v2
	} else {
		bigV, v = v2, v1
	}
	k1, k2 := bigV.terms.TermCount(), v.terms.TermCount()
	if k1 <= 1 && k1 + k2 > 1 {
		queue = append(queue, bigV)
	}
	if bigV.terms == nil {
		bigV.terms = v.terms
	} else if v.terms != nil {
		bigV.terms = bigV.terms.Concat(v.terms)
	}
	v.rep, v.terms, v.varCount = bigV, nil, 0
	bigV.varCount = r1 + r1
}

func rep(v *VarNode) *VarNode {
	v0 := v.rep
	for v0 != v0.rep {
		v0 = v0.rep
	}
	for v.rep != v0 {
		v.rep, v = v0, v.rep
	}
	return v0
}

func commonFrontier(t_list []*TermNode) *UnificationError {
	sym := t_list[0].symbol
	for _, term := range t_list {
		if term.symbol != sym {
			return &UnificationError{
				left:  sym,
				right: term.symbol,
			}
		}
	}
	a := sym.arity
	t0_list := make([]*TermNode, len(t_list))
	s0Backing := make([]int, len(t_list))
	s1Backing := make([]int, len(t_list))
	for i := 0; i < a; i++ {
		for j := range t_list {
			t0_list[j] = t_list[j].child[i]
		}
		s0 := s0Backing[0:0]
		s1 := s1Backing[0:0]
		for j, term := range t0_list {
			if term.varNode != nil {
				s0 = append(s0, j)
			} else {
				s1 = append(s1, j)
			}
		}
		if len(s0) != 0 {
			j := s0[0]
			s0 := s0[1:]
			tmp := *t_list[0]
			*t_list[0] = *t_list[j]
			*t_list[j] = tmp
			v := rep(t0_list[j].varNode)
			for _, k := range s0 {
				v2 := rep(t0_list[k].varNode)
				if v != v2 {
					merge(v, v2)
				}
			}
			for _, k := range s1 {
				add(v, t0_list[k])
			}
		} else {
			return commonFrontier(t0_list)
		}
	}
	return nil
}

func unify(t_list []*TermNode) *UnificationError {
	queue = make([]*VarNode, 0)
	err := commonFrontier(t_list)
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
			err = commonFrontier(t)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func main() {
	f := &Symbol{ "f", 2 }
	x := &VarNode{ "x", nil, nil, 1 }
	x.rep = x
	z := &VarNode{ "z", nil, nil, 1 }
	z.rep = z
	x1 := &TermNode{ nil, x, nil }
	x2 := &TermNode{ nil, x, nil }
	z2 := &TermNode{ nil, z, nil }
	expr1 := &TermNode{ f, nil, []*TermNode{ x1, x1 } }
	expr2 := &TermNode{ f, nil, []*TermNode{ &TermNode{ f, nil, []*TermNode{ z2, z2 } }, &TermNode{ f, nil, []*TermNode{ z2, x2 } } } }
	root := NewTaggedTreeList(expr1).Append(expr2)
	fmt.Println("digraph {")
	root.PrintCyclicRoot("before")
	err := unify(root.Slice())
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
		os.Exit(1)
	}
	root.PrintCyclicRoot("after")
	fmt.Println("}")
}
