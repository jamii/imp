use std::rc::Rc;

use chunk::Chunk;
use query::Query;
use relation::{ViewId, FieldId, Kind};
use plan::*;

pub struct Flow {
    pub ids: Vec<ViewId>,
    pub fields: Vec<Vec<FieldId>>,
    pub kinds: Vec<Vec<Kind>>,
    pub states: Vec<Rc<Chunk>>,
    pub nodes: Vec<Node>,
    pub downstreams: Vec<Vec<usize>>,
    pub dirty: Vec<bool>, // should be BitSet but that has been removed from std :(

    pub strings: Vec<String>, // to be replaced by gc
}

pub enum Node {
    Input,
    View{
        query: Query,
        upstream: Vec<usize>,
        plan: Plan,
    }
}

impl Flow {
    pub fn get_state(&self, id: ViewId) -> &Chunk {
        let &Flow{ref ids, ref states, ..} = self;
        let ix = ids.iter().position(|&other_id| other_id == id).unwrap();
        &states[ix]
    }

    // TODO require Relation?
    pub fn set_state(&mut self, id: ViewId, state: Chunk) {
        let &mut Flow{ref ids, ref mut states, ref downstreams, ref mut dirty, ..} = self;
        let ix = ids.iter().position(|&other_id| other_id == id).unwrap();
        states[ix] = Rc::new(state);
        for &downstream_ix in downstreams[ix].iter() {
            dirty[downstream_ix] = true;
        }
    }

    pub fn clean(&mut self) {
        let &mut Flow{ref mut states, ref nodes, ref downstreams, ref mut dirty, ref strings, ..} = self;
        while let Some(ix) = dirty.iter().position(|&is_dirty| is_dirty) {
            match nodes[ix] {
                Node::Input => panic!("How did an input get dirtied?"),
                Node::View{ref upstream, ref plan, ..} => {
                    let chunks = upstream.iter().map(|&upstream_ix| (*states[upstream_ix]).clone()).collect();
                    let new_chunk = plan.execute(strings, chunks);
                    // TODO using != assumes both will have the same sort order. is that safe?
                    if *states[ix] != new_chunk {
                        states[ix] = Rc::new(new_chunk);
                        for &downstream_ix in downstreams[ix].iter() {
                            dirty[downstream_ix] = true;
                        }
                    }
                }
            }
        }
    }
}