use std::borrow::{Borrow, BorrowMut};
use std::collections::HashSet;
use calicity_world::*;
use calicity_knowledge::*;
use calicity_dialog::*;
use rand::prelude::*;

mod dialog;
mod ontology;

#[derive(Debug, Default)]
pub struct CharData {
    conv_states: ConvStates<Spec>,
    beliefs: Beliefs<ontology::OntFacet>,
    facts: HashSet<ontology::OntValue>,
}

impl Borrow<Beliefs<ontology::OntFacet>> for CharData {
    fn borrow(&self) -> &Beliefs<ontology::OntFacet> {
        &self.beliefs
    }
}

impl BorrowMut<Beliefs<ontology::OntFacet>> for CharData {
    fn borrow_mut(&mut self) -> &mut Beliefs<ontology::OntFacet> {
        &mut self.beliefs
    }
}

#[derive(Debug)]
pub enum PlaceData {
    House,
    Business,
}

pub struct Spec;

impl WorldSpec for Spec {
    type CharData = CharData;

    type ArtifactData = ();

    type PlaceData = PlaceData;

    type PastActionData = ();
}

fn main() {
       
}
