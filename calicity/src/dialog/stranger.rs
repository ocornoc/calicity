use std::collections::BinaryHeap;
use const_random::const_random;
use super::*;

pub const SMALLTALK: Tag = const_random!(u64);
pub const SAY_NAME: Tag = const_random!(u64);
pub const ASK_NAME: Tag = const_random!(u64);

#[derive(Debug, Clone, Copy)]
pub struct IntroduceSelf {
    smalltalks: u8,
    target_smalltalks: u8,
    say_name: Option<SayName>,
    ask_for_name: Option<AskForName>,
}

impl Goal<Spec> for IntroduceSelf {
    fn update_state(
        &mut self,
        popped: &QueuedObligation<Spec>,
        obligations: &mut BinaryHeap<QueuedObligation<Spec>>,
    ) -> bool {
        if popped.obligation.fulfills_tag(SMALLTALK) {
            self.smalltalks += 1;
            // TODO: push dialog
        }

        if let Some(say_name) = &mut self.say_name {
            if say_name.update_state(popped, obligations) {
                self.say_name = None;
            }
        }

        if let Some(ask_for_name) = &mut self.ask_for_name {
            if ask_for_name.update_state(popped, obligations) {
                self.ask_for_name = None;
            }
        }

        self.smalltalks >= self.target_smalltalks
        && self.say_name.is_none()
        && self.ask_for_name.is_none()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SayName;

impl Goal<Spec> for SayName {
    fn update_state(
        &mut self,
        popped: &QueuedObligation<Spec>,
        _obligations: &mut BinaryHeap<QueuedObligation<Spec>>,
    ) -> bool {
        popped.obligation.fulfills_tag(SAY_NAME)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AskForName;

impl Goal<Spec> for AskForName {
    fn update_state(
        &mut self,
        popped: &QueuedObligation<Spec>,
        _obligations: &mut BinaryHeap<QueuedObligation<Spec>>,
    ) -> bool {
        popped.obligation.fulfills_tag(ASK_NAME)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HaveAlreadyMet;

static STRANGER: Frame<Spec> = Frame(
    |world, initiator, recipient, state| {
        let recid = recipient.get_id().into();

        if let Some(beliefs) = initiator.data.beliefs.entity_beliefs.get(&recid) {
            if beliefs.0.is_empty() {
                return;
            }
        } else {
            return;
        }

        state.goals.push(Box::new(IntroduceSelf {
            smalltalks: 0,
            target_smalltalks: 4,
            ask_for_name: Some(AskForName),
            say_name: Some(SayName),
        }));
    }
);
