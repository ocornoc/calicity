#![deny(broken_intra_doc_links)]
#![deny(private_intra_doc_links)]
#![warn(missing_debug_implementations)]

use std::collections::BinaryHeap;
use std::borrow::BorrowMut;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Pointer, Result as FmtResult};
use std::sync::atomic::{AtomicBool, Ordering};
use calicity_world::*;

#[repr(transparent)]
pub struct Frame<Spec: WorldSpec>(
    pub fn(&World<Spec>, &Character<Spec>, &Character<Spec>, &mut ConvState<Spec>),
);

impl<Spec: WorldSpec> Debug for Frame<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.write_str(&format!("Frame({:p})", &self.0))
    }
}

impl<Spec: WorldSpec> Pointer for Frame<Spec> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:?}", self)
    }
}

pub struct Obligation<Spec: WorldSpec> {
    pub urgency: Urgency,
    pub initiator_performs: bool,
    pub update_conversers:
        Box<dyn FnOnce(&mut Character<Spec>, &mut Character<Spec>, bool) + Send + Sync>,
    pub generate_data: Box<
        dyn FnOnce(&World<Spec>, PastActionIdx, bool) -> ObligationData<Spec> + Send + Sync
    >,
}

impl<Spec: WorldSpec> Debug for Obligation<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("Obligation")
            .field("urgency", &self.urgency)
            .field("update_conversers", &format!("{:p}", &self.update_conversers))
            .field("generate_data", &format!("{:p}", &self.generate_data))
            .finish()
    }
}

pub struct ObligationData<Spec: WorldSpec> {
    pub description: String,
    pub causes: Box<[PastActionIdx]>,
    pub data: Spec::PastActionData,
}

impl<Spec: WorldSpec> Debug for ObligationData<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("ObligationData")
            .field("causes", &self.causes)
            .field("data", &self.data)
            .finish()
    }
}

impl<Spec: WorldSpec> PartialEq for Obligation<Spec> {
    fn eq(&self, other: &Self) -> bool {
        self.urgency == other.urgency
    }
}

impl<Spec: WorldSpec> Eq for Obligation<Spec> {}

impl<Spec: WorldSpec> PartialOrd for Obligation<Spec> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.urgency.partial_cmp(&other.urgency)
    }
}

impl<Spec: WorldSpec> Ord for Obligation<Spec> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.urgency.cmp(&other.urgency)
    }
}

pub struct ConvState<Spec: WorldSpec> {
    pub obligations: BinaryHeap<Obligation<Spec>>,
    pub goals: Vec<Box<dyn Goal<Spec>>>,
}

impl<Spec: WorldSpec> ConvState<Spec> {
    pub fn new_with_frames(
        world: &World<Spec>,
        initiator: &Character<Spec>,
        recipient: &Character<Spec>,
        frames: &[Frame<Spec>],
    ) -> Self {
        let mut new = Default::default();

        for frame in frames {
            (frame.0)(world, initiator, recipient, &mut new);
        }

        new
    }
}

impl<Spec: WorldSpec> Default for ConvState<Spec> {
    fn default() -> Self {
        ConvState {
            obligations: BinaryHeap::new(),
            goals: Vec::new(),
        }
    }
}

impl<Spec: WorldSpec> Debug for ConvState<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("ConvState")
            .field("obligations", &self.obligations)
            .field("goals", &self.goals)
            .finish()
    }
}

pub trait Goal<Spec: WorldSpec>: Debug + Send + Sync + 'static {
    fn update_state(
        &mut self,
        popped: &Obligation<Spec>,
        obligations: &mut BinaryHeap<Obligation<Spec>>,
    ) -> bool;
}

#[repr(transparent)]
pub struct ConvStates<Spec: WorldSpec>(pub HashMap<CharIdx, ConvState<Spec>>);

impl<Spec: WorldSpec> Debug for ConvStates<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        self.0.fmt(f)
    }
}

impl<Spec: WorldSpec> Default for ConvStates<Spec> {
    fn default() -> Self {
        ConvStates(HashMap::default())
    }
}

pub fn update_state<Spec: WorldSpec>(
    initiator: &mut Character<Spec>,
    recipient: &mut Character<Spec>,
    delete: &AtomicBool,
) -> Option<Box<dyn FnOnce(&World<Spec>, PastActionIdx) -> PastActionRet<Spec>>>
where
    Spec::CharData: BorrowMut<ConvStates<Spec>>,
{
    let state = initiator.data
        .borrow_mut().0
        .get_mut(&recipient.get_id())
        .expect("tried to update state between two entities with no conversation state");

    let obligation = if let Some(next) = state.obligations.pop() {
        next
    } else {
        delete.store(true, Ordering::Release);
        return None;
    };

    let mut goals_to_remove = Vec::new();

    for (i, goal) in state.goals.iter_mut().enumerate() {
        if goal.update_state(&obligation, &mut state.obligations) {
            goals_to_remove.push(i);
        }
    }

    for i in goals_to_remove {
        state.goals.swap_remove(i);
    }

    (obligation.update_conversers)(initiator, recipient, obligation.initiator_performs);
    let generate_data = obligation.generate_data;
    let initiator = initiator.get_id().into();
    let recipients = Box::new([recipient.get_id().into()]);
    let initiator_performs = obligation.initiator_performs;

    Some(Box::new(move |world, id| {
        let data = generate_data(world, id, initiator_performs);

        PastActionRet {
            description: data.description,
            causes: data.causes,
            initiator,
            recipients,
            bystanders: Box::new([]), // TODO: add eavesdropping
            data: data.data,
        }
    }))
}
