use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::DerefMut;
use std::error::Error;
use std::collections::{HashSet, HashMap};
use parking_lot::Mutex;
use super::*;

#[derive(Debug, Default)]
pub struct Reserved {
    pub(super) exclusive: Vec<ThingIdx>,
    pub shared: Vec<ThingIdx>,
}

impl Reserved {
    pub fn is_empty(&self) -> bool {
        self.exclusive.is_empty() && self.shared.is_empty()
    }

    pub fn get_exclusives(&self) -> HashSet<ThingIdx> {
        self.exclusive.clone().into_iter().collect()
    }

    pub(super) fn is_compatible(&self, other: &Self) -> bool {
        fn aux(left: &Reserved, right: &Reserved) -> bool {
            left.exclusive.iter().all(|index|
                    !right.exclusive.contains(index) && !right.shared.contains(index)
            )
        }

        aux(self, other) && aux(other, self)
    }
}

#[derive(Debug, Clone)]
pub enum ReserveErr {
    AlreadyReserved(ThingIdx),
    Nonexistent(ThingIdx)
}

impl Display for ReserveErr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ReserveErr::AlreadyReserved(id) => write!(f, "{} already reserved", id),
            ReserveErr::Nonexistent(id) =>
                write!(f, "{} can't perform actions or doesn't exist", id),
        }
    }
}

impl Error for ReserveErr {}

#[derive(Debug, Clone, Copy)]
pub struct Reservations<'a>(pub(super) &'a Mutex<HashMap<ThingIdx, bool>>);

impl<'a> Reservations<'a> {
    #[must_use="The reservation may not be successful"]
    pub fn try_reserve(&self, exclusive: Vec<ThingIdx>) -> Result<Reserved, ReserveErr> {
        let iter = exclusive.iter();

        {
            let mut map = self.0.lock();
            let map = map.deref_mut();

            for thing in iter {
                if let Some(reserved) = map.get_mut(thing) {
                    if *reserved {
                        return Err(ReserveErr::AlreadyReserved(*thing));
                    } else {
                        *reserved = true;
                    }
                } else {
                    return Err(ReserveErr::Nonexistent(*thing));
                }
            }
        }

        Ok(Reserved {
            exclusive,
            shared: Vec::new(),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ActionActRet {
    pub action: PastActionRet,
}

#[derive(Debug, Clone)]
pub enum LocalActionActRet {
    Executed(Option<ActionActRet>),
    PerformWorld,
}

pub trait ProspectiveAction<Spec: WorldSpec>: Debug + Send + Sync {
    /// Pick all the things relevant to the action.
    ///
    /// If the return is [`None`], then the action isn't performed at all, used
    /// to represent the preconditions failing.
    fn pick_things(
        &self,
        world: &World<Spec>,
        thing: ThingIdx,
        reservations: Reservations,
    ) -> Option<Reserved>;

    /// Perform the action upon the relevant entities.
    ///
    /// If this returns [`PerformWorld`](LocalActionActRet::PerformWorld), then
    /// the [world act](ProspectiveAction::world_act) function is called.
    fn local_act(
        &mut self,
        exclusive: Vec<MutThing<Spec>>,
        shared: Vec<RefThing<Spec>>,
    ) -> LocalActionActRet;

    /// Perform an action mutable on the world.
    #[allow(unused_variables)]
    fn world_act(&mut self, world: &mut World<Spec>) -> Option<ActionActRet> {
        None
    }
}

pub type Urgency = i32;

pub mod urgencies {
    use super::Urgency;

    pub const NONURGENT: Urgency = 0;
    pub const SEMIURGENT: Urgency = 1_000;
    pub const URGENT: Urgency = 1_000_000;
}

pub(super) struct QueuedAction<Spec: WorldSpec> {
    urgency: Urgency,
    pub(super) act: Box<dyn ProspectiveAction<Spec>>,
}

impl<Spec: WorldSpec> Debug for QueuedAction<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("QueuedAction")
            .field("urgency", &self.urgency)
            .field("act", &self.act)
            .finish()
    }
}

impl<Spec: WorldSpec> PartialEq for QueuedAction<Spec> {
    fn eq(&self, other: &Self) -> bool {
        self.urgency == other.urgency
    }
}

impl<Spec: WorldSpec> Eq for QueuedAction<Spec> {}

impl<Spec: WorldSpec> PartialOrd for QueuedAction<Spec> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.urgency.partial_cmp(&other.urgency)
    }
}

impl<Spec: WorldSpec> Ord for QueuedAction<Spec> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.urgency.cmp(&other.urgency)
    }
}

pub struct QueuedActions<Spec: WorldSpec>(pub(super) Vec<QueuedAction<Spec>>);

impl<Spec: WorldSpec> QueuedActions<Spec> {
    pub fn push(&mut self, urgency: Urgency, act: Box<dyn ProspectiveAction<Spec>>) {
        let act = QueuedAction { urgency, act };

        for (i, other) in self.0.iter().enumerate() {
            if act > *other {
                self.0.insert(i, act);
                return;
            }
        }

        self.0.push(act);
    }

    pub fn push_owned(&mut self, urgency: Urgency, act: impl ProspectiveAction<Spec> + 'static) {
        self.push(urgency, Box::new(act))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<Spec: WorldSpec> Default for QueuedActions<Spec> {
    fn default() -> Self {
        QueuedActions(Vec::default())
    }
}

impl<Spec: WorldSpec> Debug for QueuedActions<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_list()
            .entries(self.0.iter())
            .finish()
    }
}

#[derive(Debug)]
pub struct PastAction {
    pub entity_data: EntityData<PastActionIdx>,
    pub causes: Box<[PastActionIdx]>,
    pub caused: Vec<PastActionIdx>,
    pub initiator: ThingIdx,
    pub recipients: Box<[ThingIdx]>,
    pub bystanders: Box<[ThingIdx]>,
}

impl Display for PastAction {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self.entity_data.name, f)
    }
}

#[derive(Debug, Clone)]
pub struct PastActionRet {
    pub name: String,
    pub causes: Box<[PastActionIdx]>,
    pub initiator: ThingIdx,
    pub recipients: Box<[ThingIdx]>,
    pub bystanders: Box<[ThingIdx]>,
}

pub type LockCode = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotStatus {
    Cooldown(RelativeTime),
    Locked(LockCode),
    Open
}

impl Default for SlotStatus {
    fn default() -> Self {
        SlotStatus::Open
    }
}

pub type ActionSlot = u32;

pub struct ActionState<Spec: WorldSpec> {
    pub slots: HashMap<ActionSlot, SlotStatus>,
    pub queue: QueuedActions<Spec>,
    pub active: bool,
}

impl<Spec: WorldSpec> ActionState<Spec> {
    pub(super) fn progress_time(&mut self, dt: RelativeTime) {
        for slot in self.slots.values_mut() {
            if let SlotStatus::Cooldown(time) = slot {
                *time = *time - dt;

                if *time <= RelativeTime::zero() {
                    *slot = SlotStatus::Open;
                }
            }
        }
    }

    pub fn get_slot_status(&self, slot: ActionSlot) -> Option<SlotStatus> {
        self.slots.get(&slot).copied()
    }

    pub fn get_slot_status_mut(&mut self, slot: ActionSlot) -> Option<&mut SlotStatus> {
        self.slots.get_mut(&slot)
    }

    pub fn is_slot_open(&self, slot: ActionSlot) -> bool {
        self.get_slot_status(slot) == Some(SlotStatus::Open)
    }

    pub fn create_slot(&mut self, slot: ActionSlot) -> Option<SlotStatus> {
        self.set_slot(slot, SlotStatus::Open)
    }

    pub fn set_slot(&mut self, slot: ActionSlot, status: SlotStatus) -> Option<SlotStatus> {
        self.slots.insert(slot, status)
    }

    pub fn is_active(&self) -> bool {
        self.active
    }
}

impl<Spec: WorldSpec> Default for ActionState<Spec> {
    fn default() -> Self {
        ActionState {
            slots: HashMap::default(),
            queue: QueuedActions::default(),
            active: true,
        }
    }
}

impl<Spec: WorldSpec> Debug for ActionState<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("ActionState")
            .field("slots", &self.slots)
            .field("queue", &self.queue)
            .field("active", &self.active)
            .finish()
    }
}
