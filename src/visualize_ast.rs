use std::io::Write;

use crate::parser::{Ast, Expr, ExprType, Statement, StatementType};

#[derive(Debug, Clone, PartialEq, Eq)]
enum NodeType {
    Statement { name: String },

    If,

    BinaryOp { name: String },
    UnaryOp { name: String },
    Literal { value: String },

    Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Node {
    node_type: NodeType,
    id: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EdgeType {
    Lhs,
    Rhs,
    Cond,
    Body,
    Expr,

    ContainedStatement,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Edge {
    from: usize,
    to: usize,
    edge_type: EdgeType,
}

struct Graph {
    nodes: Vec<Node>,
    edges: Vec<Edge>,
}
impl<'a> dot::Labeller<'a, Node, Edge> for Graph {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("ast").unwrap()
    }

    fn node_id(&'a self, n: &Node) -> dot::Id<'a> {
        dot::Id::new(format!("Node{}", n.id)).unwrap()
    }

    fn node_label(&'a self, n: &Node) -> dot::LabelText<'a> {
        match n.node_type {
            NodeType::Statement { ref name } => dot::LabelText::label(name.clone()),
            NodeType::If => dot::LabelText::label("If".to_string()),
            NodeType::BinaryOp { ref name } => dot::LabelText::label(name.clone()),
            NodeType::UnaryOp { ref name } => dot::LabelText::label(name.clone()),
            NodeType::Literal { ref value } => dot::LabelText::label(value.clone()),
            NodeType::Block => dot::LabelText::label("Block".to_string()),
        }
    }
    fn edge_label(&'a self, e: &Edge) -> dot::LabelText<'a> {
        match e.edge_type {
            EdgeType::Lhs => dot::LabelText::label("lhs"),
            EdgeType::Rhs => dot::LabelText::label("rhs"),
            EdgeType::Cond => dot::LabelText::label("cond"),
            EdgeType::Body => dot::LabelText::label("body"),
            EdgeType::Expr => dot::LabelText::label("expr"),
            EdgeType::ContainedStatement => dot::LabelText::label("contained"),
        }
    }

    fn node_shape(&'a self, node: &Node) -> Option<dot::LabelText<'a>> {
        match node.node_type {
            NodeType::Statement { .. } => Some(dot::LabelText::label("oval")),
            _ => Some(dot::LabelText::label("box")),
        }
    }
}

impl<'a> dot::GraphWalk<'a, Node, Edge> for Graph {
    fn nodes(&'a self) -> dot::Nodes<'a, Node> {
        (&self.nodes).into()
    }
    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        (&self.edges).into()
    }
    fn source(&'a self, edge: &Edge) -> Node {
        self.nodes[edge.from].clone()
    }
    fn target(&'a self, edge: &Edge) -> Node {
        self.nodes[edge.to].clone()
    }
}

fn build_graph(ast: Ast) -> Graph {
    let mut graph = Graph {
        nodes: vec![],
        edges: vec![],
    };

    fn push_node(graph: &mut Graph, node_type: NodeType) -> usize {
        let id = graph.nodes.len();
        graph.nodes.push(Node { node_type, id });
        id
    }

    fn build_from_expr(graph: &mut Graph, expr: Expr) -> usize {
        match expr.value {
            ExprType::If { condition, body } => {
                let if_id = push_node(graph, NodeType::If);
                let cond_id = build_from_expr(graph, *condition);
                let body_id = build_from_expr(graph, *body);

                graph.edges.push(Edge {
                    from: if_id,
                    to: cond_id,
                    edge_type: EdgeType::Cond,
                });
                graph.edges.push(Edge {
                    from: if_id,
                    to: body_id,
                    edge_type: EdgeType::Body,
                });

                if_id
            }
            ExprType::Equality { lhs, rhs, .. }
            | ExprType::Comparison { lhs, rhs, .. }
            | ExprType::Term { lhs, rhs, .. }
            | ExprType::Factor { lhs, rhs, .. } => {
                let eq_id = push_node(graph, NodeType::BinaryOp {
                    name: "BinaryOp".to_string(),
                });
                let lhs_id = build_from_expr(graph, *lhs);
                let rhs_id = build_from_expr(graph, *rhs);

                graph.edges.push(Edge {
                    from: eq_id,
                    to: lhs_id,
                    edge_type: EdgeType::Lhs,
                });
                graph.edges.push(Edge {
                    from: eq_id,
                    to: rhs_id,
                    edge_type: EdgeType::Rhs,
                });

                eq_id
            }
            ExprType::Unary { rhs, .. } => {
                let unary_id = push_node(graph, NodeType::UnaryOp {
                    name: "Unary".to_string(),
                });
                let rhs_id = build_from_expr(graph, *rhs);

                graph.edges.push(Edge {
                    from: unary_id,
                    to: rhs_id,
                    edge_type: EdgeType::Rhs,
                });

                unary_id
            }
            ExprType::Terminal(terminal) => {
                push_node(graph, NodeType::Literal {
                    value: format!("{:?}", terminal),
                })
            }
            ExprType::Block { statements, ret } => {
                let block_id = push_node(graph, NodeType::Block);
                for statement in statements {
                    let contained_id = build_from_statement(graph, statement);
                    graph.edges.push(Edge {
                        from: block_id,
                        to: contained_id,
                        edge_type: EdgeType::ContainedStatement,
                    });
                }

                if let Some(ret) = ret {
                    let ret_id = build_from_expr(graph, *ret);
                    graph.edges.push(Edge {
                        from: block_id,
                        to: ret_id,
                        edge_type: EdgeType::Expr,
                    });
                }

                block_id
            },
        }
    }

    fn build_from_statement(graph: &mut Graph, statement: Statement) -> usize {
        match statement.value {
            StatementType::Return { expr } => {
                let stmt_id = push_node(graph, NodeType::Statement {
                    name: "Return".to_string(),
                });
                let expr_id = build_from_expr(graph, expr);

                graph.edges.push(Edge {
                    from: stmt_id,
                    to: expr_id,
                    edge_type: EdgeType::Expr,
                });

                stmt_id
            }
            StatementType::Decleration { ident, value } => {
                let stmt_id = push_node(graph, NodeType::Statement { name: format!("Assign {}", ident.value.ident) });
                let expr_id = build_from_expr(graph, value);

                graph.edges.push(Edge {
                    from: stmt_id,
                    to: expr_id,
                    edge_type: EdgeType::Expr
                });

                stmt_id
            }
            StatementType::DevaluedExpr { expr } => {
                let stmt_id = push_node(graph, NodeType::Statement {
                    name: "DevaluedExpr".to_string(),
                });

                let expr_id = build_from_expr(graph, expr);

                graph.edges.push(Edge {
                    from: stmt_id,
                    to: expr_id,
                    edge_type: EdgeType::Expr,
                });

                stmt_id
            }
        }
    }

    for statement in ast {
        build_from_statement(&mut graph, statement);
    }

    graph
}

pub fn render_to<W: Write>(ast: Ast, output: &mut W) -> Result<(), std::io::Error> {
    let graph = build_graph(ast);
    dot::render(&graph, output)
}