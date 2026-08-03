use openair::{Airspace, AirspaceType, Altitude, Class, Coord, Geometry, LegacyClassConflict};

fn airspace(class: Class, airspace_type: Option<AirspaceType>) -> Airspace {
    Airspace {
        name: None,
        class,
        type_: airspace_type,
        lower_bound: Altitude::Gnd,
        upper_bound: Altitude::FlightLevel(100),
        geom: Geometry::Circle {
            centerpoint: Coord {
                lat: 47.0,
                lng: 8.0,
            },
            radius: 5.0,
        },
        frequency: None,
        call_sign: None,
        transponder_code: None,
        activation_times: None,
    }
}

#[test]
fn normalizes_legacy_classes() {
    for (legacy_class, expected_type) in [
        ("CTR", AirspaceType::ControlZone),
        ("R", AirspaceType::RestrictedArea),
        ("Q", AirspaceType::DangerArea),
        ("P", AirspaceType::ProhibitedArea),
        ("GP", AirspaceType::OverflightRestriction),
        ("W", AirspaceType::GlidingSector),
        ("RMZ", AirspaceType::RadioMandatoryZone),
        ("TMZ", AirspaceType::TransponderMandatoryZone),
    ] {
        let mut airspace = airspace(Class::Unknown(legacy_class.into()), None);

        airspace.normalize_legacy_class().unwrap();

        assert_eq!(airspace.class, Class::Unclassified);
        assert_eq!(airspace.type_, Some(expected_type));
    }
}

#[test]
fn accepts_matching_existing_type() {
    let mut airspace = airspace(
        Class::Unknown("R".into()),
        Some(AirspaceType::RestrictedArea),
    );

    airspace.normalize_legacy_class().unwrap();

    assert_eq!(airspace.class, Class::Unclassified);
    assert_eq!(airspace.type_, Some(AirspaceType::RestrictedArea));
}

#[test]
fn rejects_conflicting_existing_type_without_mutation() {
    let mut airspace = airspace(
        Class::Unknown("R".into()),
        Some(AirspaceType::RadioMandatoryZone),
    );

    let error = airspace.normalize_legacy_class().unwrap_err();

    assert_eq!(
        error,
        LegacyClassConflict {
            legacy_class: "R".into(),
            resolved_type: AirspaceType::RestrictedArea,
            existing_type: AirspaceType::RadioMandatoryZone,
        }
    );
    assert_eq!(
        error.to_string(),
        "Legacy class R resolves to AY R but airspace already has AY RMZ"
    );
    assert_eq!(airspace.class, Class::Unknown("R".into()));
    assert_eq!(airspace.type_, Some(AirspaceType::RadioMandatoryZone));
}

#[test]
fn leaves_other_classes_unchanged() {
    for class in [Class::D, Class::Unknown("FUTURE".into())] {
        let mut airspace = airspace(class.clone(), None);

        airspace.normalize_legacy_class().unwrap();

        assert_eq!(airspace.class, class);
        assert_eq!(airspace.type_, None);
    }
}

#[test]
fn normalization_is_idempotent() {
    let mut airspace = airspace(Class::Unknown("W".into()), None);

    airspace.normalize_legacy_class().unwrap();
    airspace.normalize_legacy_class().unwrap();

    assert_eq!(airspace.class, Class::Unclassified);
    assert_eq!(airspace.type_, Some(AirspaceType::GlidingSector));
}
