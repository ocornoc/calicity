use std::fmt::Debug;
use std::hash::Hash;
use super::*;

pub type Unique = u64;

pub(super) fn get_unique() -> Unique {
    use std::sync::atomic::{AtomicU64, Ordering};
    static UNIQUE: AtomicU64 = AtomicU64::new(0);
    UNIQUE.fetch_add(1, Ordering::Relaxed)
}

#[derive(Debug)]
pub struct EntityData<Id: Copy + Debug + Eq + Ord> {
    pub(super) id: Id,
    pub(super) unique: Unique,
    pub name: String,
    pub(super) participated: Vec<PastActionIdx>,
    pub(super) created: Time,
}

impl<Id: Copy + Debug + Eq + Ord + Hash> PartialEq for EntityData<Id> {
    fn eq(&self, other: &Self) -> bool {
        self.unique == other.unique
    }
}

impl<Id: Copy + Debug + Eq + Ord + Hash> Eq for EntityData<Id> {}

impl<Id: Copy + Debug + Eq + Ord + Hash> Hash for EntityData<Id> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.unique.hash(state);
    }
}

impl<Id: Copy + Debug + Eq + Ord> EntityData<Id> {
    pub fn get_id(&self) -> Id {
        self.id
    }

    pub fn get_participated(&self) -> &Vec<PastActionIdx> {
        &self.participated
    }

    pub fn get_creation_time(&self) -> Time {
        self.created
    }

    pub fn get_unique_id(&self) -> Unique {
        self.unique
    }
}

pub trait Entity<Spec, T, Id>: Eq + Hash + AsRef<EntityData<Id>> + AsMut<EntityData<Id>>
where
    Id: Copy + Debug + Eq + Ord + Hash,
    ThingIdx: From<Id>,
    Spec: WorldSpec,
    T: Debug + Send + Sync,
{
    fn get_entity_data(&self) -> &EntityData<Id> {
        self.as_ref()
    }

    fn get_entity_data_mut(&mut self) -> &mut EntityData<Id> {
        self.as_mut()
    }

    fn progress_time(&mut self, dt: RelativeTime);

    fn get_id(&self) -> Id {
        self.get_entity_data().id
    }
}

macro_rules! impl_refs_for_entities {
    ($idx:ty, $t:ty $( ,)?) => {
        impl AsRef<EntityData<$idx>> for $t {
            fn as_ref(&self) -> &EntityData<$idx> {
                &self.entity_data
            }
        }

        impl AsMut<EntityData<$idx>> for $t {
            fn as_mut(&mut self) -> &mut EntityData<$idx> {
                &mut self.entity_data
            }
        }

        impl PartialEq for $t {
            fn eq(&self, other: &Self) -> bool {
                self.entity_data.id == other.entity_data.id
            }
        }

        impl Eq for $t {}

        impl Hash for $t {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.entity_data.hash(state);
            }
        }
    };
    ($idx:ty, $t:tt, "data" $( ,)?) => {
        impl<Spec: WorldSpec> AsRef<EntityData<$idx>> for $t<Spec> {
            fn as_ref(&self) -> &EntityData<$idx> {
                &self.entity_data
            }
        }

        impl<Spec: WorldSpec> AsMut<EntityData<$idx>> for $t<Spec> {
            fn as_mut(&mut self) -> &mut EntityData<$idx> {
                &mut self.entity_data
            }
        }

        impl<Spec: WorldSpec> PartialEq for $t<Spec> {
            fn eq(&self, other: &Self) -> bool {
                self.entity_data.id == other.entity_data.id
            }
        }

        impl<Spec: WorldSpec> Eq for $t<Spec> {}

        impl<Spec: WorldSpec> Hash for $t<Spec> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.entity_data.hash(state);
            }
        }
    };
}

macro_rules! impl_acts_for_entities {
    ($t:tt $( ,)?) => {
        impl<Spec: WorldSpec> AsRef<ActionState<Spec>> for $t<Spec> {
            fn as_ref(&self) -> &ActionState<Spec> {
                &self.action_state
            }
        }

        impl<Spec: WorldSpec> AsMut<ActionState<Spec>> for $t<Spec> {
            fn as_mut(&mut self) -> &mut ActionState<Spec> {
                &mut self.action_state
            }
        }
    };
}

pub trait ActingEntity<Spec, T, Id>:
    Entity<Spec, T, Id> + AsRef<ActionState<Spec>> + AsMut<ActionState<Spec>>
where
    Id: Copy + Debug + Eq + Ord + Hash,
    ThingIdx: From<Id>,
    Spec: WorldSpec,
    T: Debug + Send + Sync,
{
    fn get_action_state(&self) -> &ActionState<Spec> {
        self.as_ref()
    }

    fn get_action_state_mut(&mut self) -> &mut ActionState<Spec> {
        self.as_mut()
    }

    fn get_slot_status(&self, slot: ActionSlot) -> Option<SlotStatus> {
        self.get_action_state().get_slot_status(slot)
    }

    fn is_slot_open(&self, slot: ActionSlot) -> bool {
        self.get_action_state().is_slot_open(slot)
    }

    fn pick_next_queued_action(
        &self,
        world: &World<Spec>,
        reservations: Reservations,
    ) -> Option<(usize, Reserved)> {
        let thing = self.get_entity_data().id.into();
        let action_state = self.get_action_state();

        if action_state.is_active() {
            action_state.queue.0
                .iter()
                .enumerate()
                .find_map(move |(i, act)|
                    act.act
                        .pick_things(world, thing, reservations)
                        .map(|reserved| (i, reserved))
                )
        } else {
            None
        }
    }

    fn push_action(&mut self, urgency: Urgency, act: Box<dyn ProspectiveAction<Spec>>) {
        self.get_action_state_mut().queue.push(urgency, act);
    }
}

pub struct Character<Spec: WorldSpec> {
    pub entity_data: EntityData<CharIdx>,
    pub action_state: ActionState<Spec>,
    pub location: PlaceIdx,
    pub data: Spec::CharData,
}

impl<Spec: WorldSpec> Character<Spec> {
    pub fn new_in_world(
        world: &mut World<Spec>,
        location: PlaceIdx,
        data: Spec::CharData,
    ) -> &mut Self {
        let len = world.chars.len();
        let character = Self {
            entity_data: EntityData {
                id: CharIdx(len),
                unique: get_unique(),
                name: format!("New Character #{}", len),
                participated: Vec::new(),
                created: world.get_time(),
            },
            action_state: ActionState::default(),
            location,
            data,
        };
        let id = world.push_char(character);
        &mut world[id]
    }
}

impl<Spec: WorldSpec> Debug for Character<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("Character")
            .field("entity_data", &self.entity_data)
            .field("action_state", &self.action_state)
            .field("location", &self.location)
            .field("data", &self.data)
            .finish()
    }
}

impl_refs_for_entities!(CharIdx, Character, "data");
impl_acts_for_entities!(Character);

impl<Spec: WorldSpec> Entity<Spec, Spec::CharData, CharIdx> for Character<Spec> {
    fn progress_time(&mut self, dt: RelativeTime) {
        self.action_state.progress_time(dt);
    }
}

impl<Spec: WorldSpec> ActingEntity<Spec, Spec::CharData, CharIdx> for Character<Spec> {}

pub struct Artifact<Spec: WorldSpec> {
    pub entity_data: EntityData<ArtIdx>,
    pub action_state: ActionState<Spec>,
    pub location: PlaceIdx,
    pub data: Spec::ArtifactData,
}

impl<Spec: WorldSpec> Artifact<Spec> {
    pub fn new_in_world(
        world: &mut World<Spec>,
        location: PlaceIdx,
        data: Spec::ArtifactData,
    ) -> &mut Self {
        let len = world.artifacts.len();
        let artifact = Self {
            entity_data: EntityData {
                id: ArtIdx(len),
                unique: get_unique(),
                name: format!("New Artifact #{}", len),
                participated: Vec::new(),
                created: world.get_time(),
            },
            action_state: ActionState::default(),
            location,
            data,
        };
        let id = world.push_artifact(artifact);
        &mut world[id]
    }
}

impl_refs_for_entities!(ArtIdx, Artifact, "data");
impl_acts_for_entities!(Artifact);

impl<Spec: WorldSpec> Debug for Artifact<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("Artifact")
            .field("entity_data", &self.entity_data)
            .field("action_state", &self.action_state)
            .field("location", &self.location)
            .field("data", &self.data)
            .finish()
    }
}

impl<Spec: WorldSpec> Entity<Spec, Spec::ArtifactData, ArtIdx> for Artifact<Spec> {
    fn progress_time(&mut self, dt: RelativeTime) {
        self.action_state.progress_time(dt);
    }
}

impl<Spec: WorldSpec> ActingEntity<Spec, Spec::ArtifactData, ArtIdx> for Artifact<Spec> {}

pub struct Place<Spec: WorldSpec> {
    pub entity_data: EntityData<PlaceIdx>,
    pub action_state: ActionState<Spec>,
    pub data: Spec::PlaceData,
}

impl<Spec: WorldSpec> Place<Spec> {
    pub fn new_in_world(world: &mut World<Spec>, data: Spec::PlaceData) -> &mut Self {
        let len = world.places.len();
        let place = Self {
            entity_data: EntityData {
                id: PlaceIdx(len),
                unique: get_unique(),
                name: format!("New Place #{}", len),
                participated: Vec::new(),
                created: world.get_time(),
            },
            action_state: ActionState::default(),
            data,
        };
        let id = world.push_place(place);
        &mut world[id]
    }
}

impl_refs_for_entities!(PlaceIdx, Place, "data");
impl_acts_for_entities!(Place);

impl<Spec: WorldSpec> Debug for Place<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("Place")
            .field("entity_data", &self.entity_data)
            .field("action_state", &self.action_state)
            .field("data", &self.data)
            .finish()
    }
}

impl<Spec: WorldSpec> Entity<Spec, Spec::PlaceData, PlaceIdx> for Place<Spec> {
    fn progress_time(&mut self, dt: RelativeTime) {
        self.action_state.progress_time(dt);
    }
}

impl<Spec: WorldSpec> ActingEntity<Spec, Spec::PlaceData, PlaceIdx> for Place<Spec> {}

impl_refs_for_entities!(PastActionIdx, PastAction);
