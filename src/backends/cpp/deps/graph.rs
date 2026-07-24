//! Graph primitives shared by the dependency walker: the `ModuleDeps`
//! aggregate, the intra-module topological sort, and Tarjan's SCC algorithm
//! used both for intra-module and cross-module cycle detection.

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    backends::{
        BackendError, Result,
        cpp::extern_bindings::CppExternBinding,
        error::{CppBackendError, CppLayoutCycleScope},
    },
    grammar::ItemPath,
    semantic::TypeRegistry,
    span::ItemLocation,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EdgeKind {
    FullDef,
    FwdOnly,
}

/// All cross-module dependencies a module's items collectively pull in.
#[derive(Debug, Default)]
pub struct ModuleDeps {
    /// Modules whose `.hpp` must be `#include`d.
    pub include_modules: BTreeSet<ItemPath>,
    /// Items that only need a forward declaration, grouped by defining module.
    pub forward_decls: BTreeMap<ItemPath, BTreeSet<ItemPath>>,
    /// External `#include` directives (e.g. `<atomic>`, `\"windows.h\"`)
    /// pulled in by referenced extern types via `#[cpp_header]`.
    pub include_headers: BTreeSet<String>,
}

/// Topologically sort a module's items so that any FullDef intra-module
/// reference (by-value field, base, array element of a same-module type)
/// produces an item ordered after its dependency.
///
/// If the FullDef graph contains a strongly-connected component (a real
/// value-cycle that no forward declaration can resolve), returns a
/// `BackendError::CppLayoutCycle` reporting the cycle. Otherwise the
/// returned ordering is deterministic: templates first, then alphabetical
/// within each topological level.
pub fn topo_sort_module_items<'a>(
    module_path: &'a ItemPath,
    items: Vec<&'a crate::semantic::types::ItemDefinition>,
    registry: &TypeRegistry,
    bindings: &BTreeMap<ItemPath, CppExternBinding>,
) -> Result<Vec<&'a crate::semantic::types::ItemDefinition>> {
    use std::collections::{BTreeMap as Map, BTreeSet as Set};

    let item_paths: Set<ItemPath> = items.iter().map(|i| i.path.clone()).collect();
    let mut deps: Map<ItemPath, Set<ItemPath>> = Map::new();
    for item in &items {
        let mut item_full_deps = Set::new();
        if let Some(resolved) = item.resolved() {
            super::collect_intra_module_full_deps(
                &resolved.inner,
                module_path,
                &item_paths,
                registry,
                bindings,
                &mut item_full_deps,
            );
        }
        // No self-edges.
        item_full_deps.remove(&item.path);
        deps.insert(item.path.clone(), item_full_deps);
    }

    let mut by_path: Map<ItemPath, &crate::semantic::types::ItemDefinition> = Map::new();
    for item in &items {
        by_path.insert(item.path.clone(), *item);
    }

    // Detect SCCs in the FullDef graph. Any SCC with size > 1 is a cycle.
    // We also flag self-loops, but `item_full_deps.remove(&item.path)` above
    // means we never have self-edges at this point.
    if let Some(cycle) = first_scc_cycle(&deps) {
        let location = cycle
            .first()
            .and_then(|p| by_path.get(p))
            .map(|i| i.location)
            .unwrap_or_else(ItemLocation::internal);
        return Err(BackendError::Cpp(CppBackendError::LayoutCycle {
            scope: CppLayoutCycleScope::IntraModule,
            cycle,
            location,
        }));
    }

    // No cycles — produce a deterministic topological ordering.
    let mut output: Vec<&crate::semantic::types::ItemDefinition> = Vec::with_capacity(items.len());
    let mut visited: Set<ItemPath> = Set::new();

    fn visit<'a>(
        path: &ItemPath,
        deps: &std::collections::BTreeMap<ItemPath, std::collections::BTreeSet<ItemPath>>,
        by_path: &std::collections::BTreeMap<ItemPath, &'a crate::semantic::types::ItemDefinition>,
        visited: &mut std::collections::BTreeSet<ItemPath>,
        output: &mut Vec<&'a crate::semantic::types::ItemDefinition>,
    ) {
        if visited.contains(path) {
            return;
        }
        visited.insert(path.clone());
        if let Some(children) = deps.get(path) {
            // Visit deps in (templates-first, then alphabetical) order so
            // tied independent siblings stay deterministic.
            let mut children: Vec<&ItemPath> = children.iter().collect();
            children.sort_by(|a, b| {
                let ag = by_path.get(*a).is_some_and(|i| i.is_generic());
                let bg = by_path.get(*b).is_some_and(|i| i.is_generic());
                bg.cmp(&ag).then_with(|| a.cmp(b))
            });
            for child in children {
                visit(child, deps, by_path, visited, output);
            }
        }
        if let Some(item) = by_path.get(path) {
            output.push(item);
        }
    }

    let mut roots: Vec<ItemPath> = items.iter().map(|i| i.path.clone()).collect();
    roots.sort_by(|a, b| {
        let ag = by_path.get(a).is_some_and(|i| i.is_generic());
        let bg = by_path.get(b).is_some_and(|i| i.is_generic());
        bg.cmp(&ag).then_with(|| a.cmp(b))
    });
    for path in &roots {
        visit(path, &deps, &by_path, &mut visited, &mut output);
    }
    Ok(output)
}

/// Run Tarjan's SCC algorithm over the given adjacency map; return the
/// first non-trivial SCC found (size > 1), as a path through the cycle
/// in deterministic order. Returns `None` if the graph is acyclic.
pub fn first_scc_cycle<K>(adj: &BTreeMap<K, BTreeSet<K>>) -> Option<Vec<K>>
where
    K: Ord + Clone,
{
    use std::collections::BTreeMap as Map;

    struct State<'a, K: Ord + Clone> {
        adj: &'a Map<K, BTreeSet<K>>,
        index: usize,
        stack: Vec<K>,
        on_stack: BTreeSet<K>,
        indices: Map<K, usize>,
        lowlinks: Map<K, usize>,
        cycle: Option<Vec<K>>,
    }

    fn strongconnect<K: Ord + Clone>(node: &K, st: &mut State<'_, K>) {
        if st.cycle.is_some() {
            return;
        }
        st.indices.insert(node.clone(), st.index);
        st.lowlinks.insert(node.clone(), st.index);
        st.index += 1;
        st.stack.push(node.clone());
        st.on_stack.insert(node.clone());

        if let Some(succs) = st.adj.get(node) {
            for w in succs {
                if !st.indices.contains_key(w) {
                    strongconnect(w, st);
                    if st.cycle.is_some() {
                        return;
                    }
                    let w_low = *st.lowlinks.get(w).unwrap();
                    let v_low = st.lowlinks.get_mut(node).unwrap();
                    *v_low = (*v_low).min(w_low);
                } else if st.on_stack.contains(w) {
                    let w_idx = *st.indices.get(w).unwrap();
                    let v_low = st.lowlinks.get_mut(node).unwrap();
                    *v_low = (*v_low).min(w_idx);
                }
            }
        }

        if st.lowlinks.get(node) == st.indices.get(node) {
            // Pop an SCC off the stack.
            let mut scc = Vec::new();
            loop {
                let w = st.stack.pop().expect("non-empty stack at SCC root");
                st.on_stack.remove(&w);
                let done = w == *node;
                scc.push(w);
                if done {
                    break;
                }
            }
            if scc.len() > 1 {
                scc.reverse();
                st.cycle = Some(scc);
            }
        }
    }

    let mut st = State {
        adj,
        index: 0,
        stack: Vec::new(),
        on_stack: BTreeSet::new(),
        indices: Map::new(),
        lowlinks: Map::new(),
        cycle: None,
    };
    for node in adj.keys() {
        if !st.indices.contains_key(node) {
            strongconnect(node, &mut st);
            if st.cycle.is_some() {
                break;
            }
        }
    }
    st.cycle
}

#[cfg(test)]
mod tests {
    use super::*;

    fn graph(edges: &[(&str, &[&str])]) -> BTreeMap<String, BTreeSet<String>> {
        edges
            .iter()
            .map(|(k, vs)| {
                (
                    (*k).to_string(),
                    vs.iter().map(|v| (*v).to_string()).collect(),
                )
            })
            .collect()
    }

    #[test]
    fn first_scc_cycle_detects_two_node_cycle() {
        let g = graph(&[("a", &["b"]), ("b", &["a"])]);
        let cycle = first_scc_cycle(&g).expect("cycle expected");
        let cycle: BTreeSet<_> = cycle.into_iter().collect();
        assert_eq!(cycle, ["a", "b"].iter().map(|s| s.to_string()).collect());
    }

    #[test]
    fn first_scc_cycle_detects_three_node_cycle() {
        let g = graph(&[
            ("a", &["b"]),
            ("b", &["c"]),
            ("c", &["a"]),
            ("d", &[]), // a disjoint acyclic node
        ]);
        let cycle = first_scc_cycle(&g).expect("cycle expected");
        let cycle: BTreeSet<_> = cycle.into_iter().collect();
        assert_eq!(
            cycle,
            ["a", "b", "c"].iter().map(|s| s.to_string()).collect()
        );
    }

    #[test]
    fn first_scc_cycle_returns_none_for_dag() {
        let g = graph(&[("a", &["b", "c"]), ("b", &["d"]), ("c", &["d"]), ("d", &[])]);
        assert!(first_scc_cycle(&g).is_none());
    }

    #[test]
    fn first_scc_cycle_ignores_disjoint_acyclic_components() {
        // Cycle on the right side, disjoint DAG on the left.
        let g = graph(&[("a", &["b"]), ("b", &[]), ("x", &["y"]), ("y", &["x"])]);
        let cycle = first_scc_cycle(&g).expect("cycle expected");
        let cycle: BTreeSet<_> = cycle.into_iter().collect();
        assert_eq!(cycle, ["x", "y"].iter().map(|s| s.to_string()).collect());
    }
}
