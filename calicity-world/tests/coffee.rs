use std::convert::TryInto;
use calicity_world::*;
use chrono::prelude::*;

const CAFFEINE_FULFILLMENT_SLOT: ActionSlot = 0xDEADBEEF;
const COFFEE_SLOT: ActionSlot = 0xC0FFEE;

#[derive(Debug, Clone)]
struct DrinkCoffee;

impl ProspectiveAction<DefaultSpec> for DrinkCoffee {
    fn pick_things(
        &self,
        world: &World,
        thing: RefThing<DefaultSpec>,
    ) -> Option<PreconditionOut<DefaultSpec>> {
        let me: &Character<_> = thing.try_into().ok()?;

        if me.entity_data.name != "Grayson" {
            return None;
        }

        if !me.action_state.is_slot_open(CAFFEINE_FULFILLMENT_SLOT) {
            return None;
        }

        let coffee_cup = world.get_all_artifacts().first()?;

        if coffee_cup.entity_data.name != "Grayson's Coffee Cup" {
            return None;
        }

        if !coffee_cup.action_state.is_slot_open(COFFEE_SLOT) {
            return None;
        }

        PreconditionOut::SuccessOnce(Reserved {
            exclusive: vec![me.get_id().into(), coffee_cup.get_id().into()],
            shared: Vec::new(),
        }).into()
    }

    fn local_act(
        &mut self,
        mut exclusive: Vec<MutThing<DefaultSpec>>,
        _: Vec<RefThing<DefaultSpec>>,
    ) -> LocalActionActRet<DefaultSpec> {
        let (me, coffee_cup) = match exclusive.as_mut_slice() {
            [MutThing::Char(me), MutThing::Artifact(cup)] => (me, cup),
            s => panic!("Failed to match things: {:?}", s),
        };

        me.action_state.slots.insert(
            CAFFEINE_FULFILLMENT_SLOT,
            SlotStatus::Cooldown(RelativeTime::minutes(2)),
        );

        me.action_state.queue.push_owned(urgencies::NONURGENT, self.clone());

        coffee_cup.action_state.slots.insert(
            COFFEE_SLOT,
            SlotStatus::Locked(0xC0FFEE_BABE),
        );
        let act = PastActionRet {
            description: "Grayson drank coffee from his coffee cup".to_string(),
            causes: Box::new([]),
            initiator: me.entity_data.get_id().into(),
            recipients: Box::new([coffee_cup.entity_data.get_id().into()]),
            bystanders: Box::new([]),
            data: (),
        };

        LocalActionActRet::Completed(vec![Box::new(move |_, _| act)])
    }
}

#[test]
fn main() {
    let mut world = World::new(NaiveDate::from_ymd(2000, 1, 1).and_hms(0, 0, 0));
    let home = world.new_place(()).get_id();
    let grayson = world.new_character(home, ());
    grayson.entity_data.name = "Grayson".to_string();
    grayson.action_state.create_slot(CAFFEINE_FULFILLMENT_SLOT);
    grayson.action_state.queue.push_owned(urgencies::URGENT, DrinkCoffee);
    let coffee_cup = world.new_artifact(home, ());
    coffee_cup.entity_data.name = "Grayson's Coffee Cup".to_string();
    coffee_cup.action_state.create_slot(COFFEE_SLOT);
    let coffee_id = coffee_cup.get_id();
    let dt = RelativeTime::minutes(2);

    debug_assert_eq!(world.get_all_past_actions().len(), 0);
    world.perform_queued_actions(dt);
    debug_assert_eq!(world.get_all_past_actions().len(), 1);
    world.perform_queued_actions(dt);
    debug_assert_eq!(world.get_all_past_actions().len(), 1);
    world.perform_queued_actions(dt);
    debug_assert_eq!(world.get_all_past_actions().len(), 1);
    world.perform_queued_actions(dt);
    debug_assert_eq!(world.get_all_past_actions().len(), 1);
    let coffee_cup = world[coffee_id].action_state.get_slot_status_mut(COFFEE_SLOT).unwrap();
    *coffee_cup = SlotStatus::Open;
    world.perform_queued_actions(dt);
    debug_assert_eq!(world.get_all_past_actions().len(), 2);
    let coffee_cup = world[coffee_id].action_state.get_slot_status_mut(COFFEE_SLOT).unwrap();
    *coffee_cup = SlotStatus::Cooldown(dt);
    world.perform_queued_actions(dt);
    debug_assert_eq!(world.get_all_past_actions().len(), 3);
}
