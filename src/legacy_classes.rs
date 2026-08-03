use std::fmt;

use crate::{Airspace, AirspaceType, Class};

/// Conflict between a legacy `AC` value and an existing `AY` airspace type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LegacyClassConflict {
    /// Legacy `AC` token that implies an airspace type.
    pub legacy_class: Box<str>,
    /// Airspace type implied by the legacy class token.
    pub resolved_type: AirspaceType,
    /// Different airspace type already present in the `AY` record.
    pub existing_type: AirspaceType,
}

impl fmt::Display for LegacyClassConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Legacy class {} resolves to AY {} but airspace already has AY {}",
            self.legacy_class,
            self.resolved_type.as_str(),
            self.existing_type.as_str(),
        )
    }
}

impl std::error::Error for LegacyClassConflict {}

impl Airspace {
    /// Moves a recognized legacy `AC` type into `AY` and assigns class `UNC`.
    pub fn normalize_legacy_class(&mut self) -> Result<(), LegacyClassConflict> {
        let Some(resolved_type) = legacy_class_type(&self.class) else {
            return Ok(());
        };

        if let Some(existing_type) = &self.type_
            && existing_type != &resolved_type
        {
            return Err(LegacyClassConflict {
                legacy_class: self.class.as_str().into(),
                resolved_type,
                existing_type: existing_type.clone(),
            });
        }

        self.class = Class::Unclassified;
        self.type_ = Some(resolved_type);
        Ok(())
    }
}

fn legacy_class_type(class: &Class) -> Option<AirspaceType> {
    let Class::Unknown(value) = class else {
        return None;
    };

    let airspace_type = match value.as_ref() {
        "CTR" => AirspaceType::ControlZone,
        "R" => AirspaceType::RestrictedArea,
        "Q" => AirspaceType::DangerArea,
        "P" => AirspaceType::ProhibitedArea,
        "GP" => AirspaceType::OverflightRestriction,
        "W" => AirspaceType::GlidingSector,
        "RMZ" => AirspaceType::RadioMandatoryZone,
        "TMZ" => AirspaceType::TransponderMandatoryZone,
        _ => return None,
    };
    Some(airspace_type)
}
