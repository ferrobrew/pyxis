#![allow(clippy::upper_case_acronyms)]
use super::*;

#[derive(Clone, Debug)]
pub enum InheritancePrimitiveType {
    I32,
    U32,
    U64,
    F32,
    Unknown(usize),
    Named(String, usize),
}
pub type IPT = InheritancePrimitiveType;
impl IPT {
    fn size(&self) -> usize {
        match self {
            IPT::I32 => 4,
            IPT::U32 => 4,
            IPT::U64 => 8,
            IPT::F32 => 4,
            IPT::Unknown(size) => *size,
            IPT::Named(_, size) => *size,
        }
    }
    fn to_grammar(&self) -> T {
        match self {
            IPT::I32 => T::ident("i32"),
            IPT::U32 => T::ident("u32"),
            IPT::U64 => T::ident("u64"),
            IPT::F32 => T::ident("f32"),
            IPT::Unknown(size) => T::unknown(*size),
            IPT::Named(name, _) => T::ident(name.as_str()),
        }
    }
    fn to_semantic(&self) -> ST {
        match self {
            IPT::I32 => ST::raw("i32"),
            IPT::U32 => ST::raw("u32"),
            IPT::U64 => ST::raw("u64"),
            IPT::F32 => ST::raw("f32"),
            IPT::Unknown(size) => unknown(*size),
            IPT::Named(name, _) => ST::raw(format!("test::{name}").as_str()),
        }
    }
}

pub struct InheritanceType {
    name: String,
    alignment: usize,
    fields: Vec<(Option<usize>, String, IPT, Vec<A>)>,
    vftable: Option<IV>,
}
pub type IT = InheritanceType;
impl IT {
    pub fn new(
        name: impl Into<String>,
        alignment: usize,
        fields: impl IntoIterator<Item = (Option<usize>, String, IPT, Vec<A>)>,
    ) -> Self {
        Self {
            name: name.into(),
            alignment,
            fields: fields.into_iter().collect(),
            vftable: None,
        }
    }
    pub fn with_vftable(mut self, vftable: IV) -> Self {
        self.vftable = Some(vftable);
        self
    }
    pub fn to_grammar(&self) -> ID {
        ID::new(
            V::Public,
            &self.name,
            TD::new(
                Iterator::chain(
                    self.vftable.as_ref().iter().map(|v| {
                        TS::vftable(
                            v.functions
                                .iter()
                                .map(|func| func.to_grammar())
                                .collect::<Vec<_>>(),
                            [],
                        )
                    }),
                    self.fields.iter().map(|(address, name, ty, attrs)| {
                        let underscore_start = name.starts_with('_');
                        let mut attrs = attrs.clone();
                        if let Some(address) = address {
                            attrs.push(A::address(*address));
                        }
                        TS::field(
                            if underscore_start {
                                V::Private
                            } else {
                                V::Public
                            },
                            &if underscore_start {
                                "_".to_string()
                            } else {
                                name.to_string()
                            },
                            ty.to_grammar(),
                        )
                        .with_attributes(attrs)
                    }),
                )
                .collect::<Vec<_>>(),
            )
            .with_attributes([A::align(self.alignment)]),
        )
    }
    pub fn to_semantic(&self, vftable: Option<STV>, include_vftable_field: bool) -> SID {
        let f = &self.fields;

        let mut regions = vec![];
        let mut last_address = 0;

        let vftable_type = self
            .vftable
            .as_ref()
            .map(|vftable| vftable.to_semantic_pointer_type());
        if let Some(vftable_type) = vftable_type.as_ref().filter(|_| include_vftable_field) {
            regions.insert(0, SR::field(SV::Private, "vftable", vftable_type.clone()));
            last_address += pointer_size();
        }

        for (address, name, ty, attrs) in f {
            let visibility = if name.starts_with('_') {
                SV::Private
            } else {
                SV::Public
            };
            if let Some(address) = address {
                let delta = address - last_address;
                if delta > 0 {
                    regions.push(SR::field(
                        SV::Private,
                        format!("_field_{:x}", last_address),
                        IPT::Unknown(delta).to_semantic(),
                    ));
                }
                last_address = *address;
            }

            let mut region = SR::field(visibility, name.clone(), ty.to_semantic());
            region.is_base = attrs.iter().any(|attr| *attr == A::base());
            regions.push(region);
            last_address += ty.size();
        }

        let type_definition = STD::new().with_regions(regions);
        let type_definition = if let Some(vftable) = vftable {
            type_definition.with_vftable(vftable)
        } else if let Some((vftable, vftable_type)) = self.vftable.as_ref().zip(vftable_type) {
            type_definition.with_vftable_functions(vftable_type, vftable.to_semantic_functions())
        } else {
            type_definition
        };

        SID::defined_resolved(
            SV::Public,
            format!("test::{}", self.name).as_str(),
            SISR {
                size: last_address,
                alignment: self.alignment,
                inner: type_definition.into(),
            },
        )
    }
}

#[derive(Clone, Debug)]
pub struct InheritanceVftable {
    name: String,
    name_vftable: String,
    functions: Vec<IF>,
}
pub type IV = InheritanceVftable;
impl IV {
    pub fn new(
        name: impl Into<String>,
        name_vftable: impl Into<String>,
        functions: impl IntoIterator<Item = IF>,
    ) -> Self {
        Self {
            name: name.into(),
            name_vftable: name_vftable.into(),
            functions: functions.into_iter().collect(),
        }
    }
    pub fn to_semantic(&self) -> SID {
        SID::defined_resolved(
            SV::Public,
            format!("test::{}", self.name_vftable).as_str(),
            SISR {
                size: self.functions.len() * pointer_size(),
                alignment: pointer_size(),
                inner: STD::new()
                    .with_regions(
                        self.functions
                            .iter()
                            .map(|func| func.to_semantic_region(self.name.as_str()))
                            .collect::<Vec<_>>(),
                    )
                    .into(),
            },
        )
    }
    pub fn to_semantic_functions(&self) -> Vec<SF> {
        self.functions
            .iter()
            .map(|func| func.to_semantic())
            .collect::<Vec<_>>()
    }
    pub fn to_semantic_pointer_type(&self) -> ST {
        ST::raw(format!("test::{}", self.name_vftable).as_str()).const_pointer()
    }
}

#[derive(Clone, Debug)]
pub struct InheritanceFunction {
    name: String,
    arguments: Vec<IFA>,
    return_type: IPT,
}
pub type IF = InheritanceFunction;
#[derive(Clone, Debug)]
pub enum InheritanceFunctionArgument {
    MutSelf,
    Field(String, IPT),
}
pub type IFA = InheritanceFunctionArgument;
impl IFA {
    pub fn field(name: impl Into<String>, ty: IPT) -> Self {
        Self::Field(name.into(), ty)
    }
}
impl IF {
    pub fn new(
        name: impl Into<String>,
        arguments: impl IntoIterator<Item = IFA>,
        return_type: IPT,
    ) -> Self {
        Self {
            name: name.into(),
            arguments: arguments.into_iter().collect(),
            return_type,
        }
    }

    fn to_grammar(&self) -> F {
        F::new(
            V::Public,
            &self.name,
            self.arguments
                .iter()
                .map(|arg| match arg {
                    IFA::MutSelf => Ar::MutSelf,
                    IFA::Field(name, ty) => Ar::named(name.as_str(), ty.to_grammar()),
                })
                .collect::<Vec<_>>(),
        )
        .with_return_type(self.return_type.to_grammar())
    }

    fn to_semantic(&self) -> SF {
        SF::new(SV::Public, &self.name)
            .with_arguments(
                self.arguments
                    .iter()
                    .map(|arg| match arg {
                        IFA::MutSelf => SAr::MutSelf,
                        IFA::Field(name, ty) => SAr::field(name.clone(), ty.to_semantic()),
                    })
                    .collect::<Vec<_>>(),
            )
            .with_return_type(self.return_type.to_semantic())
    }

    fn to_semantic_region(&self, type_name: &str) -> SR {
        SR::field(
            SV::Public,
            &self.name,
            ST::function(
                SCC::Thiscall,
                self.arguments
                    .iter()
                    .map(|arg| match arg {
                        IFA::MutSelf => (
                            "this",
                            ST::raw(format!("test::{type_name}").as_str()).mut_pointer(),
                        ),
                        IFA::Field(name, ty) => (name.as_str(), ty.to_semantic()),
                    })
                    .collect::<Vec<_>>(),
                self.return_type.to_semantic(),
            ),
        )
    }
}
