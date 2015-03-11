package main

import "fmt"
import "os"

const (
	SYMBOL_PRODUCT = uint64(iota)
	SYMBOL_NUMBER
)

var names = [...]string{
	"*",
	"N",
}

var arities = [...]uint{
	2,
	0,
}

type TermNode struct {
	symbol uint64
	varNode *VarNode
	term *TermNode
	child []*TermNode
	seen uint64
}

type VarNode struct {
	symbol string
	rep *VarNode
	term *TermNode
	varCount int
	termCount int
	seen uint64
}

func PrintCyclicTerm(t *TermNode, id uint64) {
	if t.seen != id {
		t.seen = id
		if t.varNode != nil {
			fmt.Printf("node_%d_%p [label=\"%s\"]\n", id, t, t.varNode.symbol)
			fmt.Printf("node_%d_%p -> node_%d_%p [label=\"var\"]\n", id, t, id, t.varNode)
			PrintCyclicVar(t.varNode, id)
		} else {
			fmt.Printf("node_%d_%p [label=\"%s\"]\n", id, t, names[t.symbol])
		}
		if t.term != nil {
			fmt.Printf("node_%d_%p -> node_%d_%p [label=\"term\"]\n", id, t, id, t.term)
			PrintCyclicTerm(t.term, id)
		}
		for i, c := range t.child {
			fmt.Printf("node_%d_%p -> node_%d_%p [label=\"#%d\"]\n", id, t, id, c, i)
			PrintCyclicTerm(c, id)
		}
	}
}

func PrintCyclicVar(v *VarNode, id uint64) {
	if v.seen != id {
		v.seen = id
		fmt.Printf("node_%d_%p [label=\"%s (%d, %d)\"]\n", id, v, v.symbol, v.varCount, v.termCount)
		if v.rep != nil {
			fmt.Printf("node_%d_%p -> node_%d_%p [label=\"rep\"]\n", id, v, id, v.rep)
			PrintCyclicVar(v.rep, id)
		}
		if v.term != nil {
			fmt.Printf("node_%d_%p -> node_%d_%p [label=\"term\"]\n", id, v, id, v.term)
			PrintCyclicTerm(v.term, id)
		}
	}
}

func PrintCyclicRoot(t *TermNode,  id uint64) {
	fmt.Printf("subgraph cluster_%d {\n", id)
	PrintCyclicTerm(t, id)
	fmt.Println("}")
}

type UnificationError struct {
	left, right uint64
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
	a := arities[sym]
	t0_list := make([]*TermNode, len(t_list))
	s0Backing := make([]int, len(t_list))
	s1Backing := make([]int, len(t_list))
	for i := uint(0); i < a; i++ {
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
	f := SYMBOL_PRODUCT
	x := &VarNode{ "x", nil, nil, 1, 0, 0 }
	x.rep = x
	z := &VarNode{ "z", nil, nil, 1, 0, 0 }
	z.rep = z
	x1 := &TermNode{ 0, x, nil, nil, 0 }
	x2 := &TermNode{ 0, x, nil, nil, 0 }
	z2 := &TermNode{ 0, z, nil, nil, 0 }
	expr1 := &TermNode{ f, nil, nil, []*TermNode{ x1, x1 }, 0 }
	expr2 := &TermNode{ f, nil, expr1, []*TermNode{ &TermNode{ f, nil, nil, []*TermNode{ z2, z2 }, 0 }, &TermNode{ f, nil, nil, []*TermNode{ z2, x2 }, 0 } }, 0 }
	expr1.term = expr2
	//fmt.Printf("Expr 1: %+v\n", expr1)
	//fmt.Printf("Expr 2: %+v\n", expr2)
	fmt.Println("digraph {")
	PrintCyclicRoot(expr1, 1)
	err := unify([]*TermNode{expr1, expr2})
	if err != nil {
		fmt.Printf("Error: %+v\n", err)
		os.Exit(1)
	}
	PrintCyclicRoot(expr1, 2)
	fmt.Println("}")
}
