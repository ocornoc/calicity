#![deny(broken_intra_doc_links)]
#![deny(private_intra_doc_links)]
#![warn(missing_debug_implementations)]

use std::fmt::Debug;
use std::hash::Hash;
use std::collections::HashMap;
use ordered_float::NotNan;
use calicity_world::{ThingIdx, PastActionIdx, RelativeTime};
use rand::prelude::*;

type Nf32 = NotNan<f32>;
pub type Strength = Nf32;
pub type Memorability = Nf32;
pub type Salience = Nf32;
pub type ObsScore = Nf32;

const NF32_ZERO: Nf32 = unsafe { Nf32::unchecked_new(0.0) };

/// The curve defining the memorability of something.
///
/// Based on the summed exponential formula used in
/// [`10.1371/journal.pone.0120644`](https://doi.org/10.1371/journal.pone.0120644).
///
/// # Differences from paper
///
/// The only difference from the summed exponential formula used in the paper
/// is that `a1` and `a2` are stored *negated*. So, when the paper says it uses
/// `a1 = 0.000319`, we store it as `a1 = -0.000319`. This is **not** the case
/// for `mu1` and `mu2`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SavingsCurve {
    pub mu1: Nf32,
    pub mu2: Nf32,
    pub a1: Nf32,
    pub a2: Nf32,
}

impl SavingsCurve {
    /// The curve fit to Ebbinghaus's data, from
    /// [Table 5](https://doi.org/10.1371/journal.pone.0120644.t005).
    pub const EBBINGHAUS: Self = unsafe { SavingsCurve {
        mu1: Nf32::unchecked_new(0.383),
        mu2: Nf32::unchecked_new(0.321),
        a1: Nf32::unchecked_new(-0.000319),
        a2: Nf32::unchecked_new(-1.79E-07),
    } };
    /// The curve fit to Mack's data, from
    /// [Table 5](https://doi.org/10.1371/journal.pone.0120644.t005).
    pub const MACK: Self = unsafe { SavingsCurve {
        mu1: Nf32::unchecked_new( 0.315),
        mu2: Nf32::unchecked_new(0.323),
        a1: Nf32::unchecked_new(-0.000296),
        a2: Nf32::unchecked_new( -7.99E-08),
    } };
    /// The curve fit to Seitz's data, from
    /// [Table 5](https://doi.org/10.1371/journal.pone.0120644.t005).
    pub const SEITZ: Self = unsafe { SavingsCurve {
        mu1: Nf32::unchecked_new(0.304),
        mu2: Nf32::unchecked_new(0.266),
        a1: Nf32::unchecked_new(-0.000457),
        a2: Nf32::unchecked_new(-1.22E-07),
    } };
    /// The curve fit to Dros's data, from
    /// [Table 5](https://doi.org/10.1371/journal.pone.0120644.t005).
    pub const DROS: Self = unsafe { SavingsCurve {
        mu1: Nf32::unchecked_new(0.262),
        mu2: Nf32::unchecked_new(0.3),
        a1: Nf32::unchecked_new(-0.000353),
        a2: Nf32::unchecked_new( -1.00E-06),
    } };

    /// The "savings", or in our use case, memorability of something, given
    /// seconds from last ideal recall `t`.
    pub fn savings(&self, t: Nf32) -> f32 {
        let rhs = self.mu2 * (self.a1 * t).exp();
        (self.a1 * t).exp().mul_add(*self.mu1, *rhs)
    }

    /// Update the constants to decay slower, modelling how repeated recalling
    /// and repeated.
    pub fn recall(&mut self) {
        self.a1 *= 0.6666666666;
        self.a2 *= 0.6666666666;
    }
}

/// A belief "facet", or something an entity could have beliefs about.
///
/// Given an example "I believe your eyes are blue", the facet is "your eye
/// color" and the [belief value](BeliefValue) is "blue".
pub trait BeliefFacet: Debug + Hash + Eq + Clone {
    type Value: BeliefValue<Facet = Self>;
}

pub trait BeliefValue: Debug + Hash + Eq + Sized + Clone {
    type Facet: BeliefFacet<Value = Self>;

    fn facet(&self) -> Self::Facet;

    fn mutate_value(
        &self,
        vdata: &mut ValueData,
        rng: &mut (impl Rng + ?Sized),
    ) -> Option<Self>;

    fn mutate_evidence(
        &self,
        vdata: &mut ValueData,
        evidence: usize,
        rng: &mut (impl Rng + ?Sized),
    ) -> Option<(Self, OccasionalEvidence)> {
        Some((self.mutate_value(vdata, rng)?, vdata.occasional_evidence.swap_remove(evidence)))
    }
}

/// A kind of [(occasional) evidence](OccasionalEvidence).
///
/// There are different kinds of evidence, and each different kind of evidence
/// has distinct properties. For example, for
/// [transference](EvidenceKind::Transference), one has to be reminded of
/// something, so that entity that the belief owner was reminded of is saved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvidenceKind {
    /// Knowledge that was conjured due to being reminded of something else and
    /// conflating the two.
    ///
    /// For example, if Adam saw another girl similar in appearance to Eve, he
    /// may accidentally "conjure" (transfer) some evidence regarding Eve to
    /// that girl.
    Transference { subject: ThingIdx, reminded_of: ThingIdx },
    /// Knowledge that was conjured due to just assuming something applied to
    /// someone due to the commonality of that [belief](BeliefValue) within the
    /// relevant area.
    Confabulation,
    /// A lie spoken to someone else.
    Lie { spoken_to: ThingIdx },
    /// A statement that was spoken to me.
    Statement { speaker: ThingIdx },
    /// A statement I made to someone else.
    Declaration { spoken_to: ThingIdx },
    /// A statement overheard between two other entities.
    Eavesdropping { speaker: ThingIdx, spoken_to: ThingIdx },
}

/// Data regarding the strength or weight assigned to a piece of
/// [evidence](OccasionalEvidence).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StrengthData {
    /// The strength as of last update.
    pub strength: Strength,
    /// The scale applied to the output of the curve.
    pub scale: Strength,
    /// The curve used to deteriorate the strength of the evidence.
    pub curve: SavingsCurve,
    /// The time offset in seconds, or how old the piece of evidence is.
    pub offset: Nf32,
}

impl StrengthData {
    pub const DEFAULT: Self = unsafe { StrengthData {
        strength: NotNan::unchecked_new(1000.0),
        scale: NotNan::unchecked_new(1000.0),
        curve: SavingsCurve::DROS,
        offset: NF32_ZERO,
    } };

    /// Deteriorate the strength.
    pub fn deteriorate(&mut self, dt: RelativeTime) {
        self.offset += dt.num_minutes() as f32;
        self.update_strength();
    }

    /// Update the strength to be the calculated at this time.
    pub fn update_strength(&mut self) {
        self.strength = self.scale * self.curve.savings(self.offset);
    }

    /// Reset the strength to be zero and update the strength.
    pub fn reset(&mut self) {
        self.offset = NF32_ZERO;
        self.update_strength();
    }
}

impl Default for StrengthData {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// Data regarding just how memorable a piece of [evidence](OccasionalEvidence)
/// is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MemorabilityData {
    /// How memorable the [evidence](OccasionalEvidence) is.
    ///
    /// This is used for calcuting whether to forget some evidence, and can be
    /// used naturally to calculate whether to mutate some evidence.
    pub memorability: Memorability,
    /// The curve used for updating the memorability.
    pub curve: SavingsCurve,
    /// The number of seconds since this evidence was last successfully
    /// remembered.
    pub seconds_since_last_recall: Nf32,
    /// `seconds_since_last_recall` when the evidence was last attempted to be
    /// forgotten.
    pub last_forget_attempt: Nf32,
    /// The memorability last time this evidence was attempted to be forgotten.
    pub mem_at_forget_attempt: Memorability,
    /// The curve prior to any recalls.
    ///
    /// Used for resetting the evidence.
    pub original_curve: SavingsCurve,
}

impl MemorabilityData {
    pub const DEFAULT: Self = unsafe { MemorabilityData {
        memorability: NotNan::unchecked_new(1.0),
        curve: SavingsCurve::DROS,
        seconds_since_last_recall: NF32_ZERO,
        last_forget_attempt: NF32_ZERO,
        mem_at_forget_attempt: NotNan::unchecked_new(1.0),
        original_curve: SavingsCurve::DROS,
    } };

    /// Deteriorate the memorability.
    pub fn deteriorate(&mut self, dt: RelativeTime) {
        let seconds = dt.num_seconds() as f32;
        self.seconds_since_last_recall += seconds;
        self.update_memorability();
    }

    /// Update the memorability with the current curve.
    pub fn update_memorability(&mut self) {
        self.memorability = Nf32::new(self.curve.savings(self.seconds_since_last_recall)).unwrap();
    }

    /// Update the memorability at the last attempted forget.
    pub fn update_mem_at_forget(&mut self) {
        self.mem_at_forget_attempt = Nf32::new(
            self.curve.savings(self.last_forget_attempt),
        ).unwrap();
    }

    /// Reset the memorability data, including the curve.
    pub fn reset(&mut self) {
        self.curve = self.original_curve;
        self.seconds_since_last_recall = NF32_ZERO;
        self.mem_at_forget_attempt = NF32_ZERO;
        self.update_memorability();
        self.update_mem_at_forget();
    }

    /// Recall the data, resetting some fields and updating memorability.
    pub fn recall(&mut self) {
        self.seconds_since_last_recall = NF32_ZERO;
        self.mem_at_forget_attempt = NF32_ZERO;
        self.curve.recall();
        self.update_memorability();
        self.update_mem_at_forget();
    }

    /// Returns whether the evidence has been forgotten since the last time this
    /// was called, assuming it was still remembered when last called.
    pub fn should_forget(&mut self, rng: &mut (impl Rng + ?Sized)) -> bool {
        let have_forgotten = rng.gen_bool(
            (*self.mem_at_forget_attempt - *self.memorability) as f64,
        );
        self.last_forget_attempt = self.seconds_since_last_recall;
        self.update_mem_at_forget();
        have_forgotten
    }
}

impl Default for MemorabilityData {
    fn default() -> Self {
        Self::DEFAULT
    }
}

/// A piece of evidence that isn't observational or reflexive.
#[derive(Debug, Clone)]
pub struct OccasionalEvidence {
    /// The kind of evidence.
    pub kind: EvidenceKind,
    /// The historical action associated with the evidence.
    pub action: PastActionIdx,
    /// The strength of the evidence.
    pub strength: StrengthData,
    /// The memorability data of the evidence.
    pub memorability: MemorabilityData,
}

/// All the data regarding the value of a [belief value](BeliefValue).
#[derive(Debug, Clone, Default)]
pub struct ValueData {
    /// All of the associated [occasional evidence](OccasionalEvidence).
    pub occasional_evidence: Vec<OccasionalEvidence>,
    /// Represents whether this value is a belief about the holder of the
    /// belief.
    ///
    /// For example, "I know my eyes are blue."
    pub reflective: bool,
    /// A "score" added to the strength to represent the sum of observational
    /// evidence.
    pub observation_score: ObsScore,
    /// Just how strongly this [belief value](BeliefValue) is believed.
    pub strength: Strength,
}

impl ValueData {
    /// Deteriorate the [strength](StrengthData) and
    /// [memorability](MemorabilityData).
    pub fn deteriorate(&mut self, dt: RelativeTime) {
        for evidence in &mut self.occasional_evidence {
            evidence.strength.deteriorate(dt);
            evidence.memorability.deteriorate(dt);
        }

        self.update_strength();
    }

    pub fn update_strength(&mut self) {
        if self.reflective {
            self.strength = unsafe { NotNan::unchecked_new(std::f32::INFINITY) };
        } else {
            self.strength = self.observation_score;

            for evidence in &mut self.occasional_evidence {
                if matches!(evidence.kind, EvidenceKind::Lie {..}) {
                    self.strength = unsafe { NotNan::unchecked_new(-std::f32::INFINITY) };
                    return;
                } else {
                    self.strength += *evidence.strength.strength;
                }
            }
        }
    }

    /// Merge evidence, reflexivity, and observation score.
    pub fn merge(&mut self, vdata: ValueData) {
        self.reflective |= vdata.reflective;
        self.observation_score += vdata.observation_score;
        self.occasional_evidence.extend(vdata.occasional_evidence);
        self.update_strength();
    }

    /// Forget a piece of [occasional evidence](OccasionalEvidence).
    pub fn forget(&mut self, evidence: usize) {
        self.occasional_evidence.swap_remove(evidence);
    }

    /// *Potentially* forget a piece of [evidence](OccasionalEvidence) based on
    /// its [memorability](MemorabilityData).
    pub fn maybe_forget(&mut self, evidence: usize, rng: &mut (impl Rng + ?Sized)) {
        if self.occasional_evidence[evidence].memorability.should_forget(rng) {
            self.forget(evidence);
        }
    }

    /// Try forgetting all [evidence](OccasionalEvidence).
    pub fn maybe_forget_all(&mut self, rng: &mut (impl Rng + ?Sized)) {
        for evidence in (0..self.occasional_evidence.len()).into_iter().rev() {
            self.maybe_forget(evidence, rng);
        }
    }
}

/// A set of [beliefs](BeliefValue).
///
/// A map between [belief values](BeliefValue) and their [stats](ValueData).
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

    pub fn maybe_forget_all(&mut self, rng: &mut (impl Rng + ?Sized)) {
        for value in self.0.values_mut() {
            value.maybe_forget_all(rng);
        }
    }

    pub fn mark_reflective(&mut self) {
        for value in self.0.values_mut() {
            value.reflective = true;
            value.update_strength();
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
        if let Some(new_val) = value.mutate_value(vdata, rng) {
            let new_data = self.0
                .get_mut(&facet)
                .unwrap().0
                .remove(value)
                .unwrap();
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
            vdata.occasional_evidence.swap_remove(evidence);
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

    pub fn maybe_forget_all(&mut self, rng: &mut (impl Rng + ?Sized)) {
        for value in self.0.values_mut() {
            value.maybe_forget_all(rng);
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
        firestorm::profile_method!(deteriorate);

        for value in self.entity_beliefs.values_mut() {
            value.deteriorate(dt);
        }

        self.other_beliefs.deteriorate(dt);
    }

    pub fn maybe_forget_all(&mut self, rng: &mut (impl Rng + ?Sized)) {
        firestorm::profile_method!(maybe_forget_all);

        for value in self.entity_beliefs.values_mut() {
            value.maybe_forget_all(rng);
        }

        self.other_beliefs.maybe_forget_all(rng);
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

