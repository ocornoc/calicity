use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::ops::{Index, IndexMut};
use rand::prelude::*;
use std::collections::BTreeMap;
use std::convert::TryFrom;
use std::collections::HashMap;
use parking_lot::Mutex;
use chrono::prelude::*;
use rayon::prelude::*;
use firestorm::profile_method;
pub use entity::*;
pub use action::*;

/// An absolute time.
pub type Time = NaiveDateTime;
/// A "relative time" or duration of time.
pub type RelativeTime = chrono::Duration;

pub mod entity;
pub mod action;

/// The specification of the types used in the [world](World).
pub trait WorldSpec {
    /// The [character](Character) data.
    type CharData: Debug + Send + Sync;
    /// The [character](Character) data.
    type ArtifactData: Debug + Send + Sync;
    /// The [character](Character) data.
    type PlaceData: Debug + Send + Sync;
}

#[derive(Debug)]
pub struct DefaultSpec;

impl WorldSpec for DefaultSpec {
    type CharData = ();
    type ArtifactData = ();
    type PlaceData = ();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ThingIdx {
    Char(CharIdx),
    Artifact(ArtIdx),
    Place(PlaceIdx),
    Action(PastActionIdx),
}

impl Display for ThingIdx {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ThingIdx::Char(CharIdx(id)) => write!(f, "Character #{}", id),
            ThingIdx::Artifact(ArtIdx(id)) => write!(f, "Artifact #{}", id),
            ThingIdx::Place(PlaceIdx(id)) => write!(f, "Place #{}", id),
            ThingIdx::Action(PastActionIdx(id)) => write!(f, "Past action #{}", id),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum RefThing<'a, Spec: WorldSpec + 'a> {
    Char(&'a Character<Spec>),
    Artifact(&'a Artifact<Spec>),
    Place(&'a Place<Spec>),
    Action(&'a PastAction),
}

impl<'a, Spec: WorldSpec + 'a> Debug for RefThing<'a, Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            RefThing::Char(c) =>
                f.debug_tuple("RefThing::Char").field(c).finish(),
            RefThing::Artifact(a) =>
                f.debug_tuple("RefThing::Artifact").field(a).finish(),
            RefThing::Place(p) =>
                f.debug_tuple("RefThing::Place").field(p).finish(),
            RefThing::Action(a) =>
                f.debug_tuple("RefThing::Action").field(a).finish(),
        }
    }
}

impl<'a, Spec: WorldSpec + 'a> From<MutThing<'a, Spec>> for RefThing<'a, Spec> {
    fn from(index: MutThing<'a, Spec>) -> Self {
        match index {
            MutThing::Char(c) => RefThing::Char(c),
            MutThing::Artifact(a) => RefThing::Artifact(a),
            MutThing::Place(p) => RefThing::Place(p),
            MutThing::Action(a) => RefThing::Action(a),
        }
    }
}

macro_rules! try_from_for_ref {
    ($thing:tt, $t:ty, $id:ident $( ,)?) => {
        impl<'a, Spec: WorldSpec + 'a> TryFrom<$thing<'a, Spec>> for &'a $t {
            type Error = $thing<'a, Spec>;
        
            fn try_from(value: $thing<'a, Spec>) -> Result<Self, Self::Error> {
                if let $thing::$id(value) = value {
                    Ok(value)
                } else {
                    Err(value)
                }
            }
        }
    };
    ($thing:tt, $t:tt, $id:ident $( ,)?) => {
        impl<'a, Spec: WorldSpec + 'a> TryFrom<$thing<'a, Spec>> for &'a $t<Spec> {
            type Error = $thing<'a, Spec>;
        
            fn try_from(value: $thing<Spec>) -> Result<Self, Self::Error> {
                if let $thing::$id(value) = value {
                    Ok(value)
                } else {
                    Err(value)
                }
            }
        }
    }
}

macro_rules! try_from_for_mut {
    ($thing:tt, $t:ty, $id:ident $( ,)?) => {
        impl<'a, Spec: WorldSpec + 'a> TryFrom<$thing<'a, Spec>> for &'a mut $t {
            type Error = $thing<'a, Spec>;
        
            fn try_from(value: $thing<'a, Spec>) -> Result<Self, Self::Error> {
                if let $thing::$id(value) = value {
                    Ok(value)
                } else {
                    Err(value)
                }
            }
        }
    };
    ($thing:ident, $t:tt, $spect:ident, $id:ident $( ,)?) => {
        impl<'a, Spec: WorldSpec + 'a> TryFrom<$thing<'a, Spec>> for &'a mut $t<Spec> {
            type Error = $thing<'a, Spec>;
        
            fn try_from(value: $thing<'a, Spec>) -> Result<Self, Self::Error> {
                if let $thing::$id(value) = value {
                    Ok(value)
                } else {
                    Err(value)
                }
            }
        }
    }
}

try_from_for_ref!(RefThing, Character<Spec>, Char);
try_from_for_ref!(RefThing, Artifact<Spec>, Artifact);
try_from_for_ref!(RefThing, Place<Spec>, Place);
try_from_for_ref!(RefThing, PastAction, Action);

#[derive(PartialEq, Eq)]
pub enum MutThing<'a, Spec: WorldSpec + 'a> {
    Char(&'a mut Character<Spec>),
    Artifact(&'a mut Artifact<Spec>),
    Place(&'a mut Place<Spec>),
    Action(&'a mut PastAction),
}

impl<'a, Spec: WorldSpec + 'a> Debug for MutThing<'a, Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            MutThing::Char(c) =>
                f.debug_tuple("MutThing::Char").field(c).finish(),
            MutThing::Artifact(a) =>
                f.debug_tuple("MutThing::Artifact").field(a).finish(),
            MutThing::Place(p) =>
                f.debug_tuple("MutThing::Place").field(p).finish(),
            MutThing::Action(a) =>
                f.debug_tuple("MutThing::Action").field(a).finish(),
        }
    }
}

try_from_for_ref!(MutThing, Character<Spec>, Char);
try_from_for_ref!(MutThing, Artifact<Spec>, Artifact);
try_from_for_ref!(MutThing, Place<Spec>, Place);
try_from_for_ref!(MutThing, PastAction, Action);
try_from_for_mut!(MutThing, Character, CharData, Char);
try_from_for_mut!(MutThing, Artifact, ArtifactData, Artifact);
try_from_for_mut!(MutThing, Place, PlaceData, Place);
try_from_for_mut!(MutThing, PastAction, Action);

pub struct World<Spec: WorldSpec = DefaultSpec> {
    chars: Vec<Character<Spec>>,
    artifacts: Vec<Artifact<Spec>>,
    places: Vec<Place<Spec>>,
    history: Vec<PastAction>,
    time: Time,
    uniques: BTreeMap<Unique, ThingIdx>,
}

impl<Spec: WorldSpec> World<Spec> {
    pub fn new(start_time: Time) -> Self {
        World {
            chars: Vec::new(),
            artifacts: Vec::new(),
            places: Vec::new(),
            history: Vec::new(),
            time: start_time,
            uniques: BTreeMap::new(),
        }
    }

    pub fn get_time(&self) -> Time {
        self.time
    }

    fn push_char(&mut self, mut character: Character<Spec>) -> CharIdx {
        profile_method!(push_char);
        if character.entity_data.id.0 != 0 {
            panic!("Trying to insert a character already inserted!");
        } else {
            character.entity_data.id.0 = self.chars.len();
            let id = character.entity_data.id;
            self.uniques.insert(character.entity_data.unique, id.into());
            self.chars.push(character);
            id
        }
    }

    fn push_artifact(&mut self, mut artifact: Artifact<Spec>) -> ArtIdx {
        profile_method!(push_artifact);
        if artifact.entity_data.id.0 != 0 {
            panic!("Trying to insert an artifact already inserted!");
        } else {
            artifact.entity_data.id.0 = self.artifacts.len();
            let id = artifact.entity_data.id;
            self.uniques.insert(artifact.entity_data.unique, id.into());
            self.artifacts.push(artifact);
            id
        }
    }

    fn push_place(&mut self, mut place: Place<Spec>) -> PlaceIdx {
        profile_method!(push_place);
        if place.entity_data.id.0 != 0 {
            panic!("Trying to insert a place already inserted!");
        } else {
            place.entity_data.id.0 = self.places.len();
            let id = place.entity_data.id;
            self.uniques.insert(place.entity_data.unique, id.into());
            self.places.push(place);
            id
        }
    }

    pub fn get_thing(&self, index: ThingIdx) -> RefThing<Spec> {
        match index {
            ThingIdx::Char(id) => RefThing::Char(&self[id]),
            ThingIdx::Artifact(id) => RefThing::Artifact(&self[id]),
            ThingIdx::Place(id) => RefThing::Place(&self[id]),
            ThingIdx::Action(id) => RefThing::Action(&self[id]),
        }
    }

    pub fn get_thing_mut(&mut self, index: ThingIdx) -> MutThing<Spec> {
        match index {
            ThingIdx::Char(id) => MutThing::Char(&mut self[id]),
            ThingIdx::Artifact(id) => MutThing::Artifact(&mut self[id]),
            ThingIdx::Place(id) => MutThing::Place(&mut self[id]),
            ThingIdx::Action(id) => MutThing::Action(&mut self[id]),
        }
    }

    unsafe fn get_thing_unsafe(&self, index: ThingIdx) -> MutThing<Spec> {
        fn aux<T>(t: &T) -> &mut T {
            use std::cell::UnsafeCell;
            use std::mem::transmute;

            unsafe { transmute::<_, &UnsafeCell<T>>(t).get().as_mut() }.unwrap()
        }

        match index {
            ThingIdx::Char(id) => MutThing::Char(aux(&self[id])),
            ThingIdx::Artifact(id) => MutThing::Artifact(aux(&self[id])),
            ThingIdx::Place(id) => MutThing::Place(aux(&self[id])),
            ThingIdx::Action(id) => MutThing::Action(aux(&self[id])),
        }
    }

    pub fn lookup_unique(&self, unique: Unique) -> Option<ThingIdx> {
        self.uniques.get(&unique).copied()
    }

    fn progress_time(&mut self, dt: RelativeTime) {
        profile_method!(progress_time);
        for character in &mut self.chars {
            character.progress_time(dt);
        }

        for artifact in &mut self.artifacts {
            artifact.progress_time(dt);
        }

        for place in &mut self.places {
            place.progress_time(dt);
        }

        self.time += dt;
    }

    fn get_accepted_actions_queued(
        &mut self,
    ) -> Vec<(Box<dyn ProspectiveAction<Spec>>, Reserved)> {
        profile_method!(get_accepted_actions_queued);
        let map: HashMap<ThingIdx, bool> = self.chars.iter().map(|c| c.get_id().into())
            .chain(self.artifacts.iter().map(|a| a.get_id().into()))
            .chain(self.places.iter().map(|p| p.get_id().into()))
            .zip(std::iter::repeat(false))
            .collect();
        let map = Mutex::new(map);
        let reserves = Reservations(&map);
        let mut rng = thread_rng();
        let mut chars = self.chars.iter().collect::<Vec<_>>();
        let mut artifacts = self.artifacts.iter().collect::<Vec<_>>();
        let mut places = self.places.iter().collect::<Vec<_>>();
        chars.shuffle(&mut rng);
        artifacts.shuffle(&mut rng);
        places.shuffle(&mut rng);

        chars
            .into_par_iter()
            .filter_map(|c| if let Some(data) = c.pick_next_queued_action(self, reserves) {
                Some((c.get_id().into(), data))
            } else {
                None
            })
            .chain(artifacts.into_par_iter().filter_map(|a|
                if let Some(data) = a.pick_next_queued_action(self, reserves) {
                    Some((a.get_id().into(), data))
                } else {
                    None
                }
            ))
            .chain(places.into_par_iter().filter_map(|p|
                if let Some(data) = p.pick_next_queued_action(self, reserves) {
                    Some((p.get_id().into(), data))
                } else {
                    None
                }
            ))
            .collect::<Vec<(ThingIdx, _)>>()
            .into_iter()
            .map(|(id, (act_id, res))| (
                match self.get_thing_mut(id) {
                    MutThing::Char(character) => character.get_action_state_mut(),
                    MutThing::Artifact(artifact) => artifact.get_action_state_mut(),
                    MutThing::Place(place) => place.get_action_state_mut(),
                    MutThing::Action(_) => unreachable!("past actions aren't ActionEntity"),
                }.queue.0.remove(act_id).act,
                res,
            ))
            .collect()
    }

    fn perform_local_acts(
        &mut self,
        mut acts: Vec<(Box<dyn ProspectiveAction<Spec>>, Reserved)>,
    ) -> Vec<(Box<dyn ProspectiveAction<Spec>>, LocalActionActRet)> {
        profile_method!(perform_local_acts);
        let mut act_groups = Vec::with_capacity(10);

        while let Some(act) = acts.pop() {
            let mut group = Vec::with_capacity(10);
            group.push(act);
            let mut i = acts.len() as isize - 1;

            while i >= 0 {
                let (_, potential) = &acts[i as usize];

                if group.iter().all(|(_, res)| res.is_compatible(potential)) {
                    group.push(acts.remove(i as usize));
                    i -= 2;
                } else {
                    i -= 1;
                }
            }

            act_groups.push(group);
        }

        let rets = Mutex::new(Vec::with_capacity(act_groups.len()));
        let retsr = &rets;
        let me = &*self;

        for group in act_groups {
            rayon::scope(|s| {
                for (mut act, reserved) in group {
                    s.spawn(move |_| {
                        let exclusive = reserved.exclusive
                            .iter()
                            .map(|&index| unsafe { me.get_thing_unsafe(index) })
                            .collect();
                        let shared = reserved.shared
                            .iter()
                            .map(|&index| me.get_thing(index))
                            .collect();
                        let x = act.local_act(exclusive, shared);
                        retsr.lock().push((act, x));
                    });
                }
            });
        }

        rets.into_inner()
    }

    fn perform_world_acts(
        &mut self,
        lrets: Vec<(Box<dyn ProspectiveAction<Spec>>, LocalActionActRet)>,
    ) {
        profile_method!(perform_world_acts);
        let rets = lrets
            .into_iter()
            .filter_map(|(mut act, lret)| match lret {
                LocalActionActRet::Executed(ret) => ret,
                LocalActionActRet::PerformWorld => act.world_act(self),
            })
            .collect::<Vec<_>>();

        for ret in rets {
            let act = ret.action;
            let act = PastAction {
                entity_data: EntityData {
                    id: PastActionIdx(self.history.len()),
                    unique: get_unique(),
                    name: act.name,
                    participated: Vec::new(),
                    created: self.get_time(),
                },
                causes: act.causes,
                caused: Vec::new(),
                initiator: act.initiator,
                recipients: act.recipients,
                bystanders: act.bystanders,
            };

            for &cause in act.causes.iter() {
                self[cause].caused.push(act.entity_data.id);
            }

            let iter = std::iter::once(act.initiator)
                .chain(act.recipients.iter().copied())
                .chain(act.bystanders.iter().copied());

            for recipient in iter {
                match self.get_thing_mut(recipient) {
                    MutThing::Char(c) => &mut c.get_entity_data_mut().participated,
                    MutThing::Artifact(a) => &mut a.get_entity_data_mut().participated,
                    MutThing::Place(p) => &mut p.get_entity_data_mut().participated,
                    MutThing::Action(_) => unreachable!("Actions cannot be recipients!"),
                }.push(act.entity_data.id);
            }

            self.uniques.insert(act.entity_data.unique, act.entity_data.id.into());
            self.history.push(act);
        }
    }

    fn apply_queued_acts(&mut self) {
        profile_method!(apply_queued_acts);
        let acts = self.get_accepted_actions_queued();
        let lrets = self.perform_local_acts(acts);
        self.perform_world_acts(lrets);
    }

    pub fn perform_queued_actions(&mut self, dt: RelativeTime) {
        profile_method!(perform_queued_actions);
        self.progress_time(dt);
        self.apply_queued_acts();
    }

    pub fn queue_new_actions(
        &mut self,
        rng: &mut (impl Rng + ?Sized),
        actions: impl ExactSizeIterator<Item=(Urgency, Box<dyn ProspectiveAction<Spec>>)>,
    ) {
        profile_method!(queue_new_actions);
        let mut action_states = self.chars.iter_mut().map(|c| c.get_action_state_mut())
            .chain(self.artifacts.iter_mut().map(|a| a.get_action_state_mut()))
            .chain(self.places.iter_mut().map(|p| p.get_action_state_mut()))
            .collect::<Vec<_>>();
        let action_states = action_states.partial_shuffle(rng, actions.len()).0;
        let aslen = action_states.len();

        if actions.len() <= aslen {
            for (i, (urgency, action)) in actions.enumerate() {
                action_states[i].queue.push(urgency, action);
            }
        } else {
            for (i, (urgency, action)) in actions.enumerate() {
                action_states[i % aslen].queue.push(urgency, action);
            }
        }
    }
}

impl<Spec: WorldSpec> Debug for World<Spec> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_struct("World")
            .field("time", &self.time.to_string())
            .field("chars", &self.chars)
            .field("artifacts", &self.artifacts)
            .field("places", &self.places)
            .field("history", &self.history)
            .finish()
    }
}

macro_rules! get_ref_mut_index {
    (
        $nameref:ident,
        $namemut:ident,
        $allref:ident,
        $field:ident,
        $idx:tt,
        $t:ty,
        $namet:ident $( ,)?
    ) => {
        #[repr(transparent)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $idx(pub(crate) usize);

        impl<Spec: WorldSpec> World<Spec> {
            pub fn $nameref(&self, index: $idx) -> Option<&$t> {
                self.$field.get(index.0)
            }

            pub fn $namemut(&mut self, index: $idx) -> Option<&mut $t> {
                self.$field.get_mut(index.0)
            }

            pub fn $allref(&self) -> &Vec<$t> {
                &self.$field
            }
        }

        impl<Spec: WorldSpec> Index<$idx> for World<Spec> {
            type Output = $t;

            fn index(&self, index: $idx) -> &Self::Output {
                &self.$field[index.0]
            }
        }

        impl<Spec: WorldSpec> IndexMut<$idx> for World<Spec> {
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index.0]
            }
        }

        impl From<$idx> for ThingIdx {
            fn from(index: $idx) -> Self {
                ThingIdx::$namet(index)
            }
        }

        impl TryFrom<ThingIdx> for $idx {
            type Error = ();

            fn try_from(index: ThingIdx) -> Result<Self, Self::Error> {
                if let ThingIdx::$namet(x) = index {
                    Ok(x)
                } else {
                    Err(())
                }
            }
        }
    };
}

get_ref_mut_index!(
    get_char,
    get_char_mut,
    get_all_chars,
    chars,
    CharIdx,
    Character<Spec>,
    Char,
);
get_ref_mut_index!(
    get_art,
    get_art_mut,
    get_all_artifacts,
    artifacts,
    ArtIdx,
    Artifact<Spec>,
    Artifact,
);
get_ref_mut_index!(
    get_place,
    get_place_mut,
    get_all_places,
    places,
    PlaceIdx,
    Place<Spec>,
    Place,
);
get_ref_mut_index!(
    get_past_action,
    get_past_action_mut,
    get_all_past_actions,
    history,
    PastActionIdx,
    PastAction,
    Action,
);
