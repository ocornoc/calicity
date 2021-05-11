use rand::distributions::WeightedIndex;
use super::*;

macro_rules! make_mutate_value {
    ($id:ident, $rng:expr, $me:ty, $($old:path => Self = $nothing:expr, $(=> $new:path = $p:expr),* $(, )?),*) => {
        match $id {
            $($old => {
                const NEW: &[$me] = &[$($new),*];

                let id = WeightedIndex::new([$nothing, $($p),*].iter().copied())
                    .unwrap()
                    .sample($rng);

                if id == 0 {
                    None
                } else {
                    Some(NEW[id - 1].clone())
                }
            }),*
        }
    };
}

macro_rules! make_mutate_value_strength {
    ($id:ident, $rng:expr, $me:ty, $str: expr, $($old:path $(=> $new:path = $p:expr),* $(, )?),*) => {
        make_mutate_value!($id, $rng, $me, $($old => Self = *$str.strength, $(=> $new = $p),*),*)
    };
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum HairColor {
    Brown,
    Red,
    Blond,
    Green,
}

impl HairColor {
    fn mutate_value(&self, vdata: &ValueData, rng: &mut (impl Rng + ?Sized)) -> Option<Self> {
        make_mutate_value_strength! {
            self, rng, HairColor, vdata,
            HairColor::Brown
                => HairColor::Red = 50.0,
                => HairColor::Blond = 20.0,
                => HairColor::Green = 1.0,
            HairColor::Red
                => HairColor::Brown = 50.0,
                => HairColor::Blond = 30.0,
                => HairColor::Green = 1.0,
            HairColor::Blond
                => HairColor::Brown = 20.0,
                => HairColor::Red = 30.0,
                => HairColor::Green = 1.0,
            HairColor::Green
                => HairColor::Brown = 1.0,
                => HairColor::Red = 1.0,
                => HairColor::Blond = 1.0,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum EyeColor {
    Brown,
    Hazel,
    Blue,
    Green,
    Red,
}

impl EyeColor {
    fn mutate_value(&self, vdata: &ValueData, rng: &mut (impl Rng + ?Sized)) -> Option<Self> {
        make_mutate_value_strength! {
            self, rng, EyeColor, vdata,
            EyeColor::Brown
                => EyeColor::Hazel = 100.0,
                => EyeColor::Blue = 2.0,
                => EyeColor::Green = 1.0,
                => EyeColor::Red = 20.0,
            EyeColor::Hazel
                => EyeColor::Brown = 100.0,
                => EyeColor::Blue = 7.0,
                => EyeColor::Green = 7.0,
                => EyeColor::Red = 20.0,
            EyeColor::Blue
                => EyeColor::Brown = 2.0,
                => EyeColor::Hazel = 5.0,
                => EyeColor::Green = 75.0,
                => EyeColor::Red = 1.0,
            EyeColor::Green
                => EyeColor::Brown = 5.0,
                => EyeColor::Hazel = 5.0,
                => EyeColor::Blue = 75.0,
                => EyeColor::Red = 2.0,
            EyeColor::Red
                => EyeColor::Brown = 50.0,
                => EyeColor::Hazel = 20.0,
                => EyeColor::Blue = 1.0,
                => EyeColor::Green = 1.0,
        }
    }
}

/*
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Height {
    VeryShort,
    Short,
    Typical,
    Tall,
    VeryTall,
}

impl Height {
    fn mutate_value(&self, vdata: &ValueData, rng: &mut (impl Rng + ?Sized)) -> Option<Self> {
        todo!()

    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum HairLength {
    Bald,
    VeryShort,
    Short,
    Shoulder,
    MidBack,
}

impl HairLength {
    fn mutate_value(&self, vdata: &ValueData, rng: &mut (impl Rng + ?Sized)) -> Option<Self> {
        todo!()

    }
}
*/

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum HasMet {
    Known,
    Stranger,
}

impl HasMet {
    fn mutate_value(&self, _vdata: &ValueData, rng: &mut (impl Rng + ?Sized)) -> Option<Self> {
        if rng.gen_ratio(1, 100000) {
            if *self == HasMet::Known {
                Some(HasMet::Stranger)
            } else {
                Some(HasMet::Known)
            }
        } else {
            None
        }
    }
}

macro_rules! make_facets_values {
    ($($value:ident),*) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub enum OntFacet {
            $($value),*
        }

        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub enum OntValue {
            $($value($value)),*
        }

        impl BeliefFacet for OntFacet {
            type Value = OntValue;
        }

        impl BeliefValue for OntValue {
            type Facet = OntFacet;

            fn facet(&self) -> OntFacet {
                match self {
                    $(OntValue::$value(_) => OntFacet::$value),*
                }
            }

            fn mutate_value(
                &self,
                vdata: &mut ValueData,
                rng: &mut (impl Rng + ?Sized),
            ) -> Option<Self> {
                match self {
                    $(OntValue::$value(v) => Some(v.mutate_value(vdata, rng)?.into())),*
                }
            }
        }

        $(impl From<$value> for OntValue {
            fn from(v: $value) -> Self {
                OntValue::$value(v)
            }
        })*
    };
}

//make_facets_values!(HairColor, EyeColor, Height, HairLength);
make_facets_values!(HairColor, EyeColor, HasMet);
