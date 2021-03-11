use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::collections::HashMap;
use super::*;

/// The set of [IDs](ThingIdx) to reserve in the [world](World) for an
/// [action](ProspectiveAction).
///
/// If the [`Reserved`] returned from [`ProspectiveAction::pick_things`] is
/// valid, then they'll be converted to the `exclusive` and `shared` arguments
/// of [`ProspectiveAction::local_act`] respectively _in the same order as in
/// the [`Reserved`]_.
///
/// # Safety
///
/// The `exclusive` field must not have any duplicates nor any entries shared
/// with the `shared` field. This can be checked with the `.is_valid()` method.
///
/// When performing [actions](ProspectiveAction), if there are any invalid
/// [reservations](Reserved), the library *may* panic.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Reserved {
    /// The [IDs](ThingIdx) which are being exclusively/mutably accessed.
    pub exclusive: Vec<ThingIdx>,
    /// The [IDs](ThingIdx) which are being only immutably accessed.
    pub shared: Vec<ThingIdx>,
}

impl Reserved {
    /// Returns `true` if there are no reserved entities.
    pub fn is_empty(&self) -> bool {
        self.exclusive.is_empty() && self.shared.is_empty()
    }

    /// Get an immutable reference to the list of exclusively reserved
    /// [IDs](ThingIdx).
    pub fn get_exclusives(&self) -> &Vec<ThingIdx> {
        &self.exclusive
    }

    pub(super) fn is_compatible(&self, other: &Self) -> bool {
        fn aux(left: &Reserved, right: &Reserved) -> bool {
            left.exclusive.iter().all(|index|
                !right.exclusive.contains(index) && !right.shared.contains(index)
            )
        }

        aux(self, other) && aux(other, self)
    }

    /// Checks whether `self` is "valid".
    ///
    /// For exactly what "valid" means, read the documentation of [`Reserved`].
    pub fn is_valid(&self) -> bool {
        for (i, exl) in self.exclusive.iter().enumerate() {
            if self.exclusive[(i + 1)..].contains(exl) || self.shared.contains(exl) {
                return false;
            }
        }

        true
    }
}

/// The return action after performing [`ProspectiveAction::local_act`].
#[derive(Debug, Clone)]
pub enum LocalActionActRet {
    /// The `local_act` function completed and a new
    /// [historical action](PastAction) is potentially returned to be added
    /// to the history of the [world](World).
    Completed(Option<PastActionRet>),
    /// While the `local_act` function completed, the action requests that its
    /// [`world_act`](ProspectiveAction::world_act) function be executed.
    PerformWorld,
}

/// The requested [action](ProspectiveAction) to be performed by the
/// [world](World) if the [action](ProspectiveAction) passes its
/// [precondition](ProspectiveAction::pick_things).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PreconditionOut {
    /// The [action](ProspectiveAction) successfully
    /// [picked](ProspectiveAction::pick_things) what it needs to
    /// [reserve](Reserved).
    Success(Reserved),
    /// The [action](ProspectiveAction) will be deleted.
    Delete,
}

/// A queued action to be executed for some [acting entity](ActingEntity).
pub trait ProspectiveAction<Spec: WorldSpec>: Debug + Send + Sync {
    /// Pick all the things relevant to the action.
    ///
    /// If the return is [`None`], then the action isn't performed at all, used
    /// to represent the preconditions failing.
    fn pick_things(&self, world: &World<Spec>, thing: ThingIdx) -> Option<PreconditionOut>;

    /// Perform the action upon the relevant entities.
    ///
    /// If this returns [`PerformWorld`](LocalActionActRet::PerformWorld), then
    /// the [world act](ProspectiveAction::world_act) function is called.
    fn local_act(
        &mut self,
        exclusive: Vec<MutThing<Spec>>,
        shared: Vec<RefThing<Spec>>,
    ) -> LocalActionActRet;

    /// Perform an action mutable on the [world](World).
    #[allow(unused_variables)]
    fn world_act(&mut self, world: &mut World<Spec>) -> Option<PastActionRet> {
        None
    }
}

/// An urgency of an [action](ProspectiveAction).
///
/// Different [actions](ProspectiveAction) can have different "urgencies" or
/// "priorities". [Actions](ProspectiveAction) are tested in *descending* order
/// of urgency, so the higher the urgency, the more quickly it will be
/// attempted.
pub type Urgency = i32;

/// A set of [urgency](Urgency) constants.
pub mod urgencies {
    use super::Urgency;

    /// A non-urgent [action](super::ProspectiveAction).
    pub const NONURGENT: Urgency = 0;

    /// A semi-urgent [action](super::ProspectiveAction).
    pub const SEMIURGENT: Urgency = 1_000;

    /// An urgent [action](super::ProspectiveAction).
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

/// The queued actions of an [acting entity](ActingEntity).
pub struct QueuedActions<Spec: WorldSpec>(pub(super) Vec<QueuedAction<Spec>>);

impl<Spec: WorldSpec> QueuedActions<Spec> {
    /// Push a queued [act](ProspectiveAction) with a given [urgency](Urgency).
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

    /// Push a queued [act](ProspectiveAction), without the process of
    /// [`Box`]ing.
    pub fn push_owned(&mut self, urgency: Urgency, act: impl ProspectiveAction<Spec> + 'static) {
        self.push(urgency, Box::new(act))
    }

    /// Get the number of queued [acts](ProspectiveAction).
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Return whether this set of queued [acts](ProspectiveAction) is empty.
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

/// The data recorded about executed [actions](ProspectiveAction).
#[derive(Debug)]
pub struct PastAction {
    /// The data associated with this entity.
    pub entity_data: EntityData<PastActionIdx>,
    /// The actions directly causing this action to occur.
    pub causes: Box<[PastActionIdx]>,
    /// The actions this action directly caused.
    pub caused: Vec<PastActionIdx>,
    /// The [entity](Entity) that performed this action.
    pub initiator: ThingIdx,
    /// The recipients of this action.
    ///
    /// Recipients are directly involved in the action somehow.
    pub recipients: Box<[ThingIdx]>,
    /// The bystanders of this action.
    ///
    /// Bystanders aren't directly involved in the action at all.
    pub bystanders: Box<[ThingIdx]>,
}

impl Display for PastAction {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self.entity_data.name, f)
    }
}

/// The information returned by a finished [action](ProspectiveAction) that
/// will be turned into a [`PastAction`] by the [world](World).
#[derive(Debug, Clone)]
pub struct PastActionRet {
    /// The description of the event.
    pub description: String,
    /// The [past actions](PastAction) that *directly* caused this.
    pub causes: Box<[PastActionIdx]>,
    /// The [entity](ActingEntity) that initiated this event.
    pub initiator: ThingIdx,
    /// The recipients of this action.
    ///
    /// Recipients are directly involved in the action somehow.
    pub recipients: Box<[ThingIdx]>,
    /// The bystanders of this action.
    ///
    /// Bystanders aren't directly involved in the action at all.
    pub bystanders: Box<[ThingIdx]>,
}

/// An arbitrary integer used to encode some user-defined reason for which the
/// [action slot](ActionSlot) it's associated with is locked.
pub type LockCode = u64;

/// The status of an [action slot](ActionSlot) in an
/// [action state](ActionState).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotStatus {
    /// The length of time remaining until the associated [slot](ActionSlot) is
    /// re-opened.
    Cooldown(RelativeTime),
    /// The [slot](ActionSlot) is locked using a [user-defined lock
    /// code](LockCode).
    Locked(LockCode),
    /// The [slot](ActionSlot) isn't locked whatsoever.
    Open,
}

impl Default for SlotStatus {
    fn default() -> Self {
        SlotStatus::Open
    }
}

/// A user-defined ID associated with every action slot.
pub type ActionSlot = u32;

/// The state of all the actions in the [acting entity](ActingEntity).
pub struct ActionState<Spec: WorldSpec> {
    /// The [action slots](ActionSlot) and their [statuses](SlotStatus).
    pub slots: HashMap<ActionSlot, SlotStatus>,
    /// All of the [queued actions](QueuedActions).
    pub queue: QueuedActions<Spec>,
    /// Whether the [acting entity](ActingEntity) has the slots and queue
    /// updated whatsoever.
    ///
    /// Even when inactive, [slot cooldowns](SlotStatus::Cooldown) are updated
    /// when time is progressed.
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

    /// Get a reference to the [status](SlotStatus) of a particular
    /// [slot](ActionSlot).
    pub fn get_slot_status(&self, slot: ActionSlot) -> Option<SlotStatus> {
        self.slots.get(&slot).copied()
    }

    /// Get a mutable reference to the [status](SlotStatus) of a particular
    /// [slot](ActionSlot).
    pub fn get_slot_status_mut(&mut self, slot: ActionSlot) -> Option<&mut SlotStatus> {
        self.slots.get_mut(&slot)
    }

    /// Return where a [slot](ActionSlot) is [open](SlotStatus::Open).
    ///
    /// If the [slot](ActionSlot) isn't in the [action state](ActionState), this
    /// returns `false`.
    pub fn is_slot_open(&self, slot: ActionSlot) -> bool {
        self.get_slot_status(slot) == Some(SlotStatus::Open)
    }

    /// Initialize a new [slot](ActionSlot), returning the previous
    /// [status](SlotStatus) if there was one.
    pub fn create_slot(&mut self, slot: ActionSlot) -> Option<SlotStatus> {
        self.set_slot(slot, SlotStatus::Open)
    }

    /// Set (or insert) a new [slot](ActionSlot), returning the previous
    /// [status](SlotStatus) if there was one.
    pub fn set_slot(&mut self, slot: ActionSlot, status: SlotStatus) -> Option<SlotStatus> {
        self.slots.insert(slot, status)
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
