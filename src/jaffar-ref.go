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
	term *TermNode
	child []*TermNode
}

func (t *TermNode) PrintCyclicRoot(prefix string) {
	fmt.Println("subgraph {")
	t.PrintCyclic(prefix, make(map[unsafe.Pointer]bool))
	fmt.Println("}")
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
		if t.term != nil {
			genLink(seen, "term", prefix, t, t.term)
		}
		for i, c := range t.child {
			genLink(seen, "#%d", prefix, t, c, i)
		}
	}
}

type VarNode struct {
	symbol string
	rep *VarNode
	term *TermNode
	varCount int
	termCount int
}

func (v *VarNode) PrintCyclic(prefix string, seen map[unsafe.Pointer]bool) {
	ptr := unsafe.Pointer(v)
	if !seen[ptr] {
		seen[ptr] = true
		genNode("%s (%d, %d)", prefix, v, v.symbol, v.varCount, v.termCount)
		if v.rep != nil {
			genLink(seen, "rep", prefix, v, v.rep)
		}
		if v.term != nil {
			genLink(seen, "term", prefix, v, v.term)
		}
	}
}

type UnificationError struct {
	left, right *Symbol
}

var queue []*VarNode

func add(v *VarNode, t *TermNode) {
	if v.termCount == 1 {
		queue = append(queue, v)
	}
	t0 := v.term
	if t0 == nil {
		v.term, t.term = t, t
	} else {
		t0.term, t.term = t, t0.term
	}
	v.termCount++
}

func merge(v1, v2 *VarNode) {
	r1, r2 := v1.varCount, v2.varCount
	var bigV, v *VarNode
	if r1 >= r2 {
		bigV, v = v1, v2
	} else {
		bigV, v = v2, v1
	}
	k1, k2 := bigV.termCount, v.termCount
	if k1 <= 1 && k1 + k2 > 1 {
		queue = append(queue, bigV)
	}
	t0, t1 := v.term, bigV.term
	if t1 == nil {
		bigV.term = t0
	} else if t0 != nil {
		t1.term, t0.term = t0.term, t1.term
	}
	v.rep, v.term, v.varCount, v.termCount = bigV, nil, 0, 0
	bigV.varCount, bigV.termCount = r1 + r1, k1 + k2
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
		k := v.termCount
		if k >= 2 {
			t := make([]*TermNode, k)
			t0 := v.term
			for i := 0; i < k; i++ {
				t[i], t0 = t0, t0.term
			}
			t[0].term = t[0]
			v.termCount = 1
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
	x := &VarNode{ "x", nil, nil, 1, 0 }
	x.rep = x
	z := &VarNode{ "z", nil, nil, 1, 0 }
	z.rep = z
	x1 := &TermNode{ nil, x, nil, nil }
	x2 := &TermNode{ nil, x, nil, nil }
	z2 := &TermNode{ nil, z, nil, nil }
	expr1 := &TermNode{ f, nil, nil, []*TermNode{ x1, x1 } }
	expr2 := &TermNode{ f, nil, expr1, []*TermNode{ &TermNode{ f, nil, nil, []*TermNode{ z2, z2 } }, &TermNode{ f, nil, nil, []*TermNode{ z2, x2 } } } }
	expr1.term = expr2
	fmt.Println("digraph {")
	expr1.PrintCyclicRoot("before")
	err := unify([]*TermNode{expr1, expr2})
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
		os.Exit(1)
	}
	expr1.PrintCyclicRoot("after")
	fmt.Println("}")
}
