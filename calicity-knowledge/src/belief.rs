use std::fmt::Debug;
use std::hash::Hash;
use std::collections::HashMap;
use ordered_float::NotNan;
use calicity_world::{ThingIdx, PastActionIdx, RelativeTime};
use rand::prelude::*;

pub type RNG = ThreadRng;

type Nf32 = NotNan<f32>;
pub type Strength = Nf32;
pub type Memorability = Nf32;
pub type Salience = Nf32;
pub type ObsScore = Nf32;

const NF32_ZERO: Nf32 = unsafe { Nf32::unchecked_new(0.0) };

fn forgetting_curve(x: f32, k: f32, c: f32) -> f32 {
    k * (x.ln_1p().powf(c) + k).recip()
}

pub trait BeliefFacet: Debug + Hash + Eq + Clone {
    type Value: BeliefValue<Facet = Self>;
}

pub trait BeliefValue: Debug + Hash + Eq + Sized + Clone {
    type Facet: BeliefFacet<Value = Self>;

    fn facet(&self) -> Self::Facet;

    fn mutate_value(
        &self,
        vdata: &ValueData,
        rng: &mut (impl Rng + ?Sized),
    ) -> Option<(Self, ValueData)>;

    fn mutate_evidence(
        &self,
        vdata: &ValueData,
        evidence: usize,
        rng: &mut (impl Rng + ?Sized),
    ) -> Option<(Self, OccasionalEvidence)>;

    fn remember(evidence: &mut OccasionalEvidence);
}

/// A kind of (occasional) evidence.
///
/// There are different kinds of evidence, and each different kind of evidence
/// has distinct properties. For example, for
/// [transference](EvidenceKind::Transference), one has to be reminded of
/// something, so that entity that the belief owner was reminded of is saved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvidenceKind {
    Transference { subject: ThingIdx, reminded_of: ThingIdx },
    Confabulation,
    Lie { spoken_to: ThingIdx },
    Statement { speaker: ThingIdx },
    Eavesdropping { speaker: ThingIdx, spoken_to: ThingIdx },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrengthData {
    pub strength: Strength,
    pub peak_strength: Strength,
    pub k: Nf32,
    pub c: Nf32,
    pub offset: Nf32,
}

impl StrengthData {
    pub const DEFAULT: Self = unsafe { StrengthData {
        strength: NotNan::unchecked_new(1000.0),
        peak_strength: NotNan::unchecked_new(1000.0),
        k: NotNan::unchecked_new(30.0),
        c: NotNan::unchecked_new(2.0),
        offset: NF32_ZERO,
    } };

    pub fn deteriorate(&mut self, dt: RelativeTime) {
        self.offset += dt.num_minutes() as f32;
        self.update_strength();
    }

    pub fn update_strength(&mut self) {
        self.strength = self.peak_strength * forgetting_curve(*self.offset, *self.k, *self.c);
    }

    pub fn reset(&mut self) {
        self.strength = self.peak_strength;
        self.offset = NF32_ZERO;
    }
}

impl Default for StrengthData {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MemorabilityData {
    pub memorability: Memorability,
    pub k: Nf32,
    pub c: Nf32,
    pub minutes_since_last_recall: Nf32,
    pub original_c: Nf32,
}

impl MemorabilityData {
    pub const DEFAULT: Self = unsafe { MemorabilityData {
        memorability: NotNan::unchecked_new(1.0),
        k: NotNan::unchecked_new(7.3),
        c: NotNan::unchecked_new(3.7),
        minutes_since_last_recall: NF32_ZERO,
        original_c: NotNan::unchecked_new(7.3),
    } };

    pub fn deteriorate(&mut self, dt: RelativeTime) {
        self.minutes_since_last_recall += dt.num_minutes() as f32;
        self.update_memorability();
    }

    pub fn update_memorability(&mut self) {
        self.memorability = Nf32::new(
            forgetting_curve(*self.minutes_since_last_recall, *self.k, *self.c),
        ).unwrap();
    }

    pub fn reset(&mut self) {
        self.c = self.original_c;
        self.minutes_since_last_recall = NF32_ZERO;
    }

    pub fn recall(&mut self) {
        self.minutes_since_last_recall = NF32_ZERO;
        self.memorability = unsafe { NotNan::unchecked_new(1.0) };
        self.c *= 0.9;
    }
}

impl Default for MemorabilityData {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Debug, Clone)]
pub struct OccasionalEvidence {
    pub kind: EvidenceKind,
    pub action: PastActionIdx,
    pub strength: StrengthData,
    pub memorability: MemorabilityData,
}

#[derive(Debug, Clone, Default)]
pub struct ValueData {
    pub occasional_evidence: Vec<OccasionalEvidence>,
    pub reflective: bool,
    pub observation_score: ObsScore,
    pub strength: Strength,
}

impl ValueData {
    pub fn deteriorate(&mut self, dt: RelativeTime) {
        for evidence in &mut self.occasional_evidence {
            evidence.strength.deteriorate(dt);
        }

        self.update_strength();
    }

    pub fn update_strength(&mut self) {
        if self.reflective {
            self.strength = unsafe { NotNan::unchecked_new(std::f32::INFINITY) };
        } else {
            self.strength = self.observation_score;

            for evidence in &mut self.occasional_evidence {
                self.strength += *evidence.strength.strength;
            }
        }
    }

    pub fn merge(&mut self, vdata: ValueData) {
        self.reflective |= vdata.reflective;
        self.observation_score += vdata.observation_score;
        self.occasional_evidence.extend(vdata.occasional_evidence);
        self.update_strength();
    }
}

#[derive(Debug)]
pub struct BeliefSet<V: BeliefValue>(pub HashMap<V, ValueData>);

impl<V: BeliefValue> BeliefSet<V> {
    pub fn get_strongest_belief(&self) -> Option<(&V, &ValueData)> {
        self.0
            .iter()
            .max_by(|(_, l), (_, r)| l.strength.cmp(&r.strength))
    }

    pub fn deteriorate(&mut self, dt: RelativeTime) {
        for value in self.0.values_mut() {
            value.deteriorate(dt);
        }
    }

    pub fn insert_or_merge(&mut self, value: V, vdata: ValueData) {
        if let Some(old) = self.0.get_mut(&value) {
            old.merge(vdata);
        } else {
            self.0.insert(value, vdata);
        }
    }
}

impl<V: BeliefValue> Default for BeliefSet<V> {
    fn default() -> Self {
        BeliefSet(HashMap::new())
    }
}

#[derive(Debug)]
pub struct FacetBeliefs<F: BeliefFacet>(pub HashMap<F, BeliefSet<F::Value>>);

impl<F: BeliefFacet> FacetBeliefs<F> {
    pub fn deteriorate(&mut self, dt: RelativeTime) {
        for value in self.0.values_mut() {
            value.deteriorate(dt);
        }
    }

    pub fn mutate_value(&mut self, value: &F::Value, rng: &mut (impl Rng + ?Sized)) {
        let facet = value.facet();
        let vdata = self.0
            .entry(facet.clone())
            .or_default().0
            .entry(value.clone())
            .or_default();
        if let Some((new_val, new_data)) = value.mutate_value(vdata, rng) {
            self.0
                .get_mut(&facet)
                .unwrap().0
                .remove(value);
            self.0
                .entry(new_val.facet())
                .or_default()
                .insert_or_merge(new_val, new_data);
        }
    }

    pub fn mutate_evidence(
        &mut self,
        value: &F::Value,
        evidence: usize,
        rng: &mut (impl Rng + ?Sized),
    ) {
        let facet = value.facet();
        let vdata = self.0
            .entry(facet.clone())
            .or_default().0
            .entry(value.clone())
            .or_default();
        if let Some((new_val, new_evidence)) = value.mutate_evidence(vdata, evidence, rng) {
            vdata.occasional_evidence.remove(evidence);
            vdata.update_strength();
            self.0
                .entry(new_val.facet())
                .or_default()
                .0
                .entry(new_val)
                .or_default().occasional_evidence
                .push(new_evidence);
        }
    }
}

impl<F: BeliefFacet> Default for FacetBeliefs<F> {
    fn default() -> Self {
        FacetBeliefs(HashMap::new())
    }
}

#[derive(Debug)]
pub struct Beliefs<F: BeliefFacet> {
    pub entity_beliefs: HashMap<ThingIdx, FacetBeliefs<F>>,
    pub other_beliefs: FacetBeliefs<F>,
}

impl<F: BeliefFacet> Beliefs<F> {
    pub fn deteriorate(&mut self, dt: RelativeTime) {
        for value in self.entity_beliefs.values_mut() {
            value.deteriorate(dt);
        }

        self.other_beliefs.deteriorate(dt);
    }
}

impl<F: BeliefFacet> Default for Beliefs<F> {
    fn default() -> Self {
        Beliefs {
            entity_beliefs: HashMap::new(),
            other_beliefs: Default::default(),
        }
    }
}
